//! Unified highlighting engine over syntect (TextMate grammars) and
//! tree-sitter. Syntect is the default; tree-sitter `Language` is still
//! detected for non-highlighting features (indentation, semantic highlighting).
//!
//! # TextMate cache design
//!
//! Syntect's parser is a sequential state machine — it must process bytes
//! in order from a known parse state to track multi-line constructs and
//! embedded language transitions. To make scrolling cheap, the engine keeps
//! a span cache, a composite `ParseSnapshot` (host parse state plus any
//! embedded-language child state, see `EMBEDDING_SPECS`) at the cache tail,
//! and periodic checkpoint anchors to support resume-from-anywhere.
//!
//! Three render-time paths, gated by what the cache covers:
//!
//! - **Cache hit** — cache fully covers the parse range and there's no
//!   pending edit; filter cached spans for the viewport. Zero parse work.
//! - **Forward extension** — cache covers the start of the parse range but
//!   not its end; resume from `tail_state` and parse only the uncovered
//!   tail bytes. Steady-state scroll path.
//! - **Partial update** — there's a pending edit; resume from the nearest
//!   line-start checkpoint before the dirty point and parse forward looking
//!   for convergence (state matches an existing checkpoint at a line end past
//!   the last edit), bounded by a per-pass byte budget so pathological edits
//!   can't degenerate into whole-file reparses.
//! - **Cold start / fallback** — no cache, or none of the above applies;
//!   parse the appropriate range from a fresh state or nearest checkpoint.
//!
//! For files at or below `MAX_PARSE_BYTES` the parse range starts at byte 0,
//! so every state is exact, and ends `context_bytes` past the viewport; the
//! forward-extension path grows it as the viewport moves down. Larger files
//! use a viewport-centred window of `±context_bytes`.
//!
//! Edits go through `notify_insert` / `notify_delete`, which shift cached
//! span byte offsets in place, widen the dirty range (`dirty_from` ..
//! `dirty_end`), and invalidate `tail_state` when the edit lies inside the
//! cached range.

use crate::model::buffer::Buffer;
use crate::model::marker::{MarkerId, MarkerList};
use crate::primitives::grammar::GrammarRegistry;
use crate::primitives::highlighter::{
    highlight_bg, highlight_color, HighlightCategory, HighlightSpan, Highlighter, Language,
};
use crate::theme::Theme;
use fresh_core::hooks::{RegionLine, TableLine, TableRole};
use std::collections::HashMap;
use std::ops::Range;
use std::path::Path;
use std::sync::Arc;
use syntect::parsing::SyntaxSet;

/// Map TextMate scope to highlight category
fn scope_to_category(scope: &str) -> Option<HighlightCategory> {
    let scope_lower = scope.to_lowercase();

    // Comments - highest priority
    if scope_lower.starts_with("comment") {
        return Some(HighlightCategory::Comment);
    }

    // Strings
    if scope_lower.starts_with("string") {
        return Some(HighlightCategory::String);
    }

    // Markdown/markup scopes - handle before generic keyword/punctuation checks
    // See: https://macromates.com/manual/en/language_grammars (TextMate scope naming)
    // Headings: markup.heading and entity.name.section (used by syntect's markdown grammar)
    if scope_lower.starts_with("markup.heading") || scope_lower.starts_with("entity.name.section") {
        return Some(HighlightCategory::Keyword); // Headers styled like keywords (bold, prominent)
    }
    // Bold: markup.bold
    if scope_lower.starts_with("markup.bold") {
        return Some(HighlightCategory::Constant); // Bold styled like constants (bright)
    }
    // Italic: markup.italic
    if scope_lower.starts_with("markup.italic") {
        return Some(HighlightCategory::Variable); // Italic styled like variables
    }
    // Inline code and code blocks: markup.raw, markup.inline.raw
    if scope_lower.starts_with("markup.raw") || scope_lower.starts_with("markup.inline.raw") {
        return Some(HighlightCategory::String); // Code styled like strings
    }
    // Links: markup.underline.link
    if scope_lower.starts_with("markup.underline.link") {
        return Some(HighlightCategory::Function); // Links styled like functions (distinct color)
    }
    // Generic underline (often links)
    if scope_lower.starts_with("markup.underline") {
        return Some(HighlightCategory::Function);
    }
    // Block quotes: markup.quote
    if scope_lower.starts_with("markup.quote") {
        return Some(HighlightCategory::Comment); // Quotes styled like comments (subdued)
    }
    // Lists: markup.list
    if scope_lower.starts_with("markup.list") {
        return Some(HighlightCategory::Operator); // List markers styled like operators
    }
    // Strikethrough: markup.strikethrough
    if scope_lower.starts_with("markup.strikethrough") {
        return Some(HighlightCategory::Comment); // Strikethrough styled subdued
    }

    // Diff scopes (syntect's bundled `Diff` grammar). These scope the
    // entire row, not just the leading +/-/@@ marker, so the renderer
    // can paint a whole-line background by reading the span's bg.
    //
    //   markup.inserted.diff      — `+` line
    //   markup.deleted.diff       — `-` line
    //   meta.diff.range.unified   — `@@ ... @@` hunk header
    //   markup.changed.*          — generic "changed" marker (rare)
    //   meta.diff.header.*        — `diff --git`, `index ...`, file
    //                               headers; render like Type so they
    //                               stand out without a bg wash.
    if scope_lower.starts_with("markup.inserted") {
        return Some(HighlightCategory::Inserted);
    }
    if scope_lower.starts_with("markup.deleted") {
        return Some(HighlightCategory::Deleted);
    }
    if scope_lower.starts_with("markup.changed") || scope_lower.starts_with("meta.diff.range") {
        return Some(HighlightCategory::Changed);
    }
    if scope_lower.starts_with("meta.diff.header") {
        return Some(HighlightCategory::Type);
    }

    // Word operators (`new`, `typeof`, `instanceof`, `in`, `of`) read as keywords.
    //
    // `keyword.operator.word` is the scope Sublime grammars give operators that
    // are spelled as words rather than symbols, so it belongs here too: C/C++
    // `sizeof`, `alignof`, `typeid`, `static_assert`, `and`/`or`/`not`, Java
    // `instanceof`, Fortran `.and.`, VHDL `mod`/`rem`, Typst `and`/`or`/`not`.
    // Without it they fell through to Operator, whose colour equals plain text
    // in most themes, so they read as unhighlighted identifiers (issue #3644)
    // — and Java's `instanceof` was coloured differently from JavaScript's,
    // which already lands on `keyword.operator.expression` just above.
    if scope_lower.starts_with("keyword.operator.new")
        || scope_lower.starts_with("keyword.operator.expression")
        || scope_lower.starts_with("keyword.operator.word")
    {
        return Some(HighlightCategory::Keyword);
    }

    // Keywords
    if scope_lower.starts_with("keyword.control")
        || scope_lower.starts_with("keyword.other")
        || scope_lower.starts_with("keyword.declaration")
        || scope_lower.starts_with("keyword")
    {
        // keyword.operator should map to Operator, not Keyword
        if !scope_lower.starts_with("keyword.operator") {
            return Some(HighlightCategory::Keyword);
        }
    }

    // Punctuation that belongs to a parent construct (comment/string delimiters)
    // These must be checked before the generic punctuation rule below.
    // TextMate grammars assign e.g. `punctuation.definition.comment` to # // /* etc.
    if scope_lower.starts_with("punctuation.definition.comment") {
        return Some(HighlightCategory::Comment);
    }
    if scope_lower.starts_with("punctuation.definition.string") {
        return Some(HighlightCategory::String);
    }

    // Operators (keyword.operator only)
    if scope_lower.starts_with("keyword.operator") {
        return Some(HighlightCategory::Operator);
    }

    // Punctuation brackets ({, }, (, ), [, ], <, >)
    // Covers punctuation.section.*, punctuation.bracket.*,
    // and punctuation.definition.{array,block,brackets,group,inline-table,section,table,tag}
    if scope_lower.starts_with("punctuation.section")
        || scope_lower.starts_with("punctuation.bracket")
        || scope_lower.starts_with("punctuation.definition.array")
        || scope_lower.starts_with("punctuation.definition.block")
        || scope_lower.starts_with("punctuation.definition.brackets")
        || scope_lower.starts_with("punctuation.definition.group")
        || scope_lower.starts_with("punctuation.definition.inline-table")
        || scope_lower.starts_with("punctuation.definition.section")
        || scope_lower.starts_with("punctuation.definition.table")
        || scope_lower.starts_with("punctuation.definition.tag")
        || scope_lower.starts_with("punctuation.definition.typeparameters")
        || scope_lower.starts_with("punctuation.definition.parameters")
        || scope_lower.starts_with("meta.brace")
    {
        return Some(HighlightCategory::PunctuationBracket);
    }

    // Punctuation delimiters (;, ,, .)
    if scope_lower.starts_with("punctuation.separator")
        || scope_lower.starts_with("punctuation.terminator")
        || scope_lower.starts_with("punctuation.accessor")
    {
        return Some(HighlightCategory::PunctuationDelimiter);
    }

    // Functions
    if scope_lower.starts_with("entity.name.function")
        || scope_lower.starts_with("support.function")
        || scope_lower.starts_with("meta.function-call")
        || scope_lower.starts_with("variable.function")
    {
        return Some(HighlightCategory::Function);
    }

    // Types
    if scope_lower.starts_with("entity.name.type")
        || scope_lower.starts_with("entity.name.class")
        || scope_lower.starts_with("entity.name.struct")
        || scope_lower.starts_with("entity.name.enum")
        || scope_lower.starts_with("entity.name.interface")
        || scope_lower.starts_with("entity.name.trait")
        || scope_lower.starts_with("support.type")
        || scope_lower.starts_with("support.class")
    {
        return Some(HighlightCategory::Type);
    }

    // SPIKE (not for merge): builtin types take the Type colour instead of the
    // keyword colour. `storage.modifier` (const, static, virtual, final, …)
    // stays a keyword.
    if scope_lower.starts_with("storage.type") {
        return Some(HighlightCategory::Type);
    }
    if scope_lower.starts_with("storage.modifier") {
        return Some(HighlightCategory::Keyword);
    }

    // Constants and numbers
    if scope_lower.starts_with("constant.numeric")
        || scope_lower.starts_with("constant.language.boolean")
    {
        return Some(HighlightCategory::Number);
    }
    if scope_lower.starts_with("constant") {
        return Some(HighlightCategory::Constant);
    }

    // Member access (`obj.prop`, `.length`); checked before `variable.other`.
    if scope_lower.starts_with("variable.other.property")
        || scope_lower.starts_with("variable.other.object.property")
        || scope_lower.starts_with("support.variable.property")
    {
        return Some(HighlightCategory::Property);
    }

    // Host globals (`document`, `window`) and library constants (`Math.PI`).
    if scope_lower.starts_with("support.variable") {
        return Some(HighlightCategory::VariableBuiltin);
    }
    if scope_lower.starts_with("support.constant") {
        return Some(HighlightCategory::Constant);
    }

    // Variables
    if scope_lower.starts_with("variable.language") {
        return Some(HighlightCategory::VariableBuiltin);
    }
    if scope_lower.starts_with("variable.parameter") || scope_lower.starts_with("variable.other") {
        return Some(HighlightCategory::Variable);
    }

    // Properties / object keys
    if scope_lower.starts_with("entity.name.tag")
        || scope_lower.starts_with("support.other.property")
        || scope_lower.starts_with("meta.object-literal.key")
    {
        return Some(HighlightCategory::Property);
    }

    // Attributes (decorators, annotations)
    if scope_lower.starts_with("entity.other.attribute")
        || scope_lower.starts_with("meta.attribute")
        || scope_lower.starts_with("entity.name.decorator")
    {
        return Some(HighlightCategory::Attribute);
    }

    // Generic variable fallback
    if scope_lower.starts_with("variable") {
        return Some(HighlightCategory::Variable);
    }

    None
}

/// Unified highlighting engine supporting multiple backends
#[derive(Default)]
pub enum HighlightEngine {
    /// Tree-sitter based highlighting (built-in languages)
    TreeSitter(Box<Highlighter>),
    /// TextMate grammar based highlighting
    TextMate(Box<TextMateEngine>),
    /// No highlighting available
    #[default]
    None,
}

/// TextMate highlighting engine. See module docs for the cache design.
pub struct TextMateEngine {
    syntax_set: Arc<SyntaxSet>,
    syntax_index: usize,
    checkpoint_markers: MarkerList,
    checkpoint_states: HashMap<MarkerId, ParseSnapshot>,
    dirty_from: Option<usize>,
    // Furthest end of any edit since the cache was last clean (the largest
    // edit end, in current buffer offsets: later edits before it shift it);
    // only meaningful while `dirty_from` is set. A bulk edit touches many sites,
    // and a stored state matching past the first one says nothing about the
    // spans of the later ones, so convergence must not be accepted before it.
    dirty_end: usize,
    cache: Option<TextMateCache>,
    last_buffer_len: usize,
    ts_language: Option<Language>,
    stats: HighlightStats,
    // Region kinds this syntax hosts (see `EMBEDDING_SPECS`); empty for
    // the vast majority of syntaxes.
    embedding: Vec<EmbeddingSpec>,
    // Body scope of this syntax's tables (see `TABLE_SPECS`); `None` for
    // every syntax that doesn't scope them.
    table_body_scope: Option<syntect::parsing::Scope>,
    // Fence-language-token → syntax index memo (None = unrecognized).
    embedded_syntax_memo: HashMap<String, Option<usize>>,
    // Reusable per-line span buffers for embedding hosts, so region
    // classification in `parse_snapshot_line` doesn't allocate per line.
    host_span_scratch: Vec<(usize, usize, HighlightCategory)>,
    child_span_scratch: Vec<(usize, usize, HighlightCategory)>,
    // Scope→Category memo. Syntect Scope atoms are append-only-interned
    // globally, so entries never need invalidation.
    scope_category_cache: HashMap<syntect::parsing::Scope, Option<HighlightCategory>>,
    // A host for declared regions highlights a buffer a plugin composed,
    // whose code rows the plugin declared (`set_declared_regions`) rather
    // than a grammar found. Its own syntax is plain text; every colour
    // comes from a region's child parser.
    declared_host: bool,
    // One span marker per declared region, in the engine's own marker
    // list — the same list, and the same `notify_insert` / `notify_delete`
    // hooks, that carry its parse checkpoints through an edit. `declared`
    // holds what each marker's region parses as.
    region_markers: MarkerList,
    declared: HashMap<MarkerId, DeclaredRegion>,
    // The region the last row fell in. Regions are runs of rows, so the
    // marker tree is asked once per run rather than once per row; a row
    // outside this range re-queries. Dropped whenever the markers move,
    // since it caches their positions.
    active_region: Option<(MarkerId, usize, usize)>,
}

/// A run of rows a plugin declared as code, as the host resolved it. The
/// contract the plugin sees is `SyntaxRegion` in the plugin API; this is
/// its language already looked up in the syntax set.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclaredRegion {
    /// Byte offset of the first row's first byte, as declared. The live
    /// bounds are the region's marker's: an edit moves them.
    pub start: usize,
    /// Byte offset one past the last row's newline, as declared.
    pub end: usize,
    /// Index into the syntax set of the language the rows are written
    /// in. `None` when nothing in the set claims it, in which case the
    /// rows keep the styling they have.
    pub syntax_index: Option<usize>,
    /// Bytes at the start of every row that are not code.
    pub prefix: usize,
    /// Parsers the rows feed, in order; the first colours the row. Empty
    /// means [`DEFAULT_REGION_STREAM`].
    pub streams: Vec<u32>,
}

/// Stream a region that names none feeds.
pub const DEFAULT_REGION_STREAM: u32 = 0;

/// Parsers a declared-region host carries between rows. The diff panels
/// interleave the two sides of one hunk and never need more than that;
/// anything older is dropped rather than cloned into every checkpoint.
pub const MAX_REGION_STREAMS: usize = 4;

/// Counters for monitoring highlighting performance in tests.
#[derive(Debug, Default, Clone)]
pub struct HighlightStats {
    /// Number of bytes parsed by syntect (total across all highlight_viewport calls).
    pub bytes_parsed: usize,
    /// Bytes of line text actually handed to syntect. Unlike `bytes_parsed`,
    /// which counts the range a pass walked, a long line contributes only its
    /// highlighted prefix here.
    pub bytes_tokenized: usize,
    /// Number of highlight_viewport calls that hit the span cache.
    pub cache_hits: usize,
    /// Number of highlight_viewport calls that missed the cache and re-parsed.
    pub cache_misses: usize,
    /// Number of checkpoint states updated during convergence.
    pub checkpoints_updated: usize,
    /// Number of times convergence was detected (state matched existing checkpoint).
    pub convergences: usize,
}

#[derive(Debug, Clone)]
struct TextMateCache {
    range: Range<usize>,
    spans: Vec<CachedSpan>,
    // Parse state at `range.end`; powers forward extension. None when the
    // last mutation didn't end at `range.end`.
    tail_state: Option<ParseSnapshot>,
    // What the parse said about each whole line it walked, ascending by
    // start; empty for syntaxes that host no structure (`hosts_structure`).
    // Kept by the same three paths, edit shifts and rewind as `spans`, so
    // `structure_lines_in` reads it rather than parsing again. A line the
    // parse did not commit (a streaming tail, a host parse error, the
    // unparsed rest of a cut line) is simply absent, and a lookup that
    // needs it answers "unknown".
    lines: Vec<LineStructure>,
    // Whether the parse that built this cache started from a state it can
    // vouch for — byte 0 or a checkpoint — rather than a fresh state in the
    // middle of a large file. A fresh state reads as "outside every region",
    // which is a confident wrong answer, so an unanchored cache answers no
    // structure question at all. Highlighting from it is still fine: there a
    // wrong guess only mis-colours, and only until the parse is anchored.
    anchored: bool,
}

/// The structure state the host grammar's scope stack was in around one
/// line: whether an embedded-language region (`EMBEDDING_SPECS`) and a
/// table body (`TABLE_SPECS`) were open before the line and after it.
/// Every role `structure_lines_in` reports is a function of these
/// transitions, read across a line and its neighbours.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct LineStructure {
    start: usize,
    /// One past the line's terminator.
    end: usize,
    region_before: bool,
    region_after: bool,
    table_before: bool,
    table_after: bool,
}

#[derive(Debug, Clone)]
struct CachedSpan {
    range: Range<usize>,
    category: crate::primitives::highlighter::HighlightCategory,
    /// Optional independent background category. Embedded source tokens in
    /// unified diffs keep their language category for the foreground while
    /// inheriting Inserted/Deleted from the host diff row.
    background_category: Option<crate::primitives::highlighter::HighlightCategory>,
}

/// Declares how a host syntax delimits embedded-language regions whose
/// language is named *by the document* (e.g. Markdown code fences). The
/// host grammar itself must scope both the region and the language name;
/// the engine then runs a child parser over region content lines. Hosts
/// whose embedded language is known statically (HTML `<style>` → CSS)
/// don't belong here — the grammar embeds those directly via the
/// `SyntaxSet`. See `docs/internal/embedded-language-highlighting.md`.
struct EmbeddingSpecDef {
    host_syntax: &'static str,
    /// Scope (prefix) the host grammar keeps on the stack for the whole
    /// region, including its delimiter lines.
    region_scope: &'static str,
    /// Scope (prefix) the host grammar assigns to the language token on
    /// the region's opening line.
    language_scope: &'static str,
    /// Language token used when the opening line names no language, or
    /// names one that doesn't resolve to a syntax (e.g. `lang="ts"` in a
    /// Vue `<script>` — there is no TextMate TS grammar in the set, and
    /// JS is the standard approximation). `None` = keep the host's own
    /// styling for such regions (Markdown's raw-code look).
    default_language: Option<&'static str>,
    /// How content rows are presented to the embedded parser.
    content_mode: EmbeddedContentMode,
    /// Whether one region of this kind can end and the next begin on a
    /// single line, so the language token has to be watched on every
    /// line rather than only where a region could open. A unified diff
    /// does that at each `diff --git`; fences and tags never do.
    retargets_on_same_line: bool,
}

#[derive(Debug, Clone, Copy)]
enum EmbeddedContentMode {
    /// Parse the complete region-content line (Markdown, Vue).
    WholeLine,
    /// Parse unified-diff source rows after removing their one-byte patch
    /// marker. Diff metadata stays host-styled and does not advance the
    /// source parser.
    UnifiedDiff,
}

/// A host may declare several region kinds (Vue: `<script>` and
/// `<style>`), each with its own default language. Region scopes of one
/// host must not be prefixes of each other.
const EMBEDDING_SPECS: &[EmbeddingSpecDef] = &[
    EmbeddingSpecDef {
        host_syntax: "Markdown",
        region_scope: "markup.raw.code-fence",
        language_scope: "constant.other.language-name",
        default_language: None,
        content_mode: EmbeddedContentMode::WholeLine,
        retargets_on_same_line: false,
    },
    EmbeddingSpecDef {
        host_syntax: "Vue",
        region_scope: "meta.embedded.block.script",
        language_scope: "constant.other.language-name",
        default_language: Some("js"),
        content_mode: EmbeddedContentMode::WholeLine,
        retargets_on_same_line: false,
    },
    EmbeddingSpecDef {
        host_syntax: "Vue",
        region_scope: "meta.embedded.block.style",
        language_scope: "constant.other.language-name",
        default_language: Some("css"),
        content_mode: EmbeddedContentMode::WholeLine,
        retargets_on_same_line: false,
    },
    EmbeddingSpecDef {
        host_syntax: "Fresh Diff",
        region_scope: "meta.embedded.block.diff",
        language_scope: "constant.other.language-name",
        default_language: None,
        content_mode: EmbeddedContentMode::UnifiedDiff,
        retargets_on_same_line: true,
    },
];

/// Cap on watched language scopes per host; far above any real spec
/// table (Vue, the largest host, has one distinct language scope).
const MAX_WATCH_SCOPES: usize = 8;

/// Declares how a host syntax scopes a *table*: a block whose rows the
/// grammar recognizes, but which embeds no second language.
///
/// Deliberately **not** an `EMBEDDING_SPECS` entry. That table means
/// something specific — a region whose language the document names, which
/// the engine runs a child parser over — and a table is neither. What the
/// two share is the *pattern*: a per-line structural classification the
/// host grammar already makes while parsing, exported so a decoration
/// plugin needs no block model of its own.
///
/// Only `body_scope` is read from the scope *stack*. The header and
/// delimiter rows are identified positionally from that stack's
/// transitions (see `structure_lines_in`), which is why they need no
/// scope of their own here: a grammar that closes over the body is enough
/// to place every edge of the frame.
struct TableSpecDef {
    host_syntax: &'static str,
    /// Scope (prefix) the host grammar keeps on the stack from the end of
    /// the delimiter row through the table's last row.
    body_scope: &'static str,
}

/// One entry per host syntax that scopes tables. Markdown is the only
/// one today; the bundled grammar pushes `meta.table.markdown` on the
/// delimiter row and pops it after the table's last row.
const TABLE_SPECS: &[TableSpecDef] = &[TableSpecDef {
    host_syntax: "Markdown",
    body_scope: "meta.table",
}];

/// How far past the requested range `structure_lines_in` looks for the end
/// of the line that closes a table. One line is enough; this is the byte
/// budget for finding it, generous enough for any realistic line and
/// small enough that it never turns a viewport query into a scan.
const TABLE_LOOKAHEAD_BYTES: usize = 8192;

/// What the host grammar says about each line in a range: its role in an
/// embedded-language region, and its place in a table. Both lists are
/// keyed by line start byte, ascending. See
/// [`TextMateEngine::structure_lines_in`].
#[derive(Debug, Default, Clone)]
pub struct StructureLines {
    pub regions: Vec<(usize, RegionLine)>,
    pub tables: Vec<(usize, TableLine)>,
}

impl StructureLines {
    /// Record a table line, dropping it if it falls outside the caller's
    /// range — the lookup deliberately reads more lines than the caller
    /// asked about (the one before `range.start`, one line of lookahead
    /// past `range.end`), and those extra lines exist only to classify the
    /// ones inside it.
    fn push_table(
        &mut self,
        line_start: usize,
        role: TableRole,
        first_row: bool,
        range: &Range<usize>,
    ) {
        if line_start >= range.start && line_start < range.end {
            self.tables.push((
                line_start,
                TableLine {
                    role,
                    first_row,
                    last: false,
                },
            ));
        }
    }

    /// Mark the table line starting at `line_start` — the one the walk just
    /// found the end of the table below — as its table's last.
    ///
    /// Keyed by position rather than "whatever was recorded last", because
    /// the two part company exactly when it matters: a range that stops
    /// mid-table drops the rows after it, so the last *recorded* line can be
    /// several rows above the one the table actually ends on. Marking that
    /// one would close the frame in the middle of the table. When the real
    /// last row was dropped, this is a no-op — correct, since the caller is
    /// not rendering it.
    fn mark_table_line_last(&mut self, line_start: Option<usize>) {
        let Some(line_start) = line_start else { return };
        if let Some((start, line)) = self.tables.last_mut() {
            if *start == line_start {
                line.last = true;
            }
        }
    }
}

/// `EmbeddingSpecDef` with its scope selectors interned as syntect `Scope`
/// atoms for cheap prefix matching in the per-line parse loop.
#[derive(Debug, Clone, Copy)]
struct EmbeddingSpec {
    region_scope: syntect::parsing::Scope,
    language_scope: syntect::parsing::Scope,
    default_language: Option<&'static str>,
    content_mode: EmbeddedContentMode,
    retargets_on_same_line: bool,
}

impl EmbeddingSpec {
    fn compile_for_syntax(name: &str) -> Vec<Self> {
        EMBEDDING_SPECS
            .iter()
            .filter(|d| d.host_syntax == name)
            .filter_map(|d| {
                Some(Self {
                    region_scope: syntect::parsing::Scope::new(d.region_scope).ok()?,
                    language_scope: syntect::parsing::Scope::new(d.language_scope).ok()?,
                    default_language: d.default_language,
                    content_mode: d.content_mode,
                    retargets_on_same_line: d.retargets_on_same_line,
                })
            })
            .collect()
    }
}

/// True when any scope on the stack has `scope` as a prefix.
fn scopes_contain(stack: &syntect::parsing::ScopeStack, scope: syntect::parsing::Scope) -> bool {
    stack.as_slice().iter().any(|s| scope.is_prefix_of(*s))
}

/// Parse state of an embedded-language region's child parser.
#[derive(Debug, Clone, PartialEq)]
struct EmbeddedParseState {
    /// Index of the embedded syntax in the shared `SyntaxSet`. Part of the
    /// state identity: two parses that entered the same region with
    /// different fence languages must not be considered converged.
    syntax_index: usize,
    state: syntect::parsing::ParseState,
    scopes: syntect::parsing::ScopeStack,
}

/// Complete resumable parse state: the host parser plus, while inside an
/// embedded-language region with a recognized language, the child parser.
/// This is what checkpoints, the cache tail, and convergence comparison
/// carry, so all incremental paths (resume-from-checkpoint, forward
/// extension, partial update) work identically inside embedded regions.
#[derive(Debug, Clone, PartialEq)]
struct ParseSnapshot {
    state: syntect::parsing::ParseState,
    scopes: syntect::parsing::ScopeStack,
    embedded: Option<EmbeddedParseState>,
    /// A declared-region host's live parsers, most recently fed last;
    /// empty for every grammar host. See `DeclaredRegion::streams`.
    streams: Vec<(u32, EmbeddedParseState)>,
}

impl ParseSnapshot {
    fn new(syntax: &syntect::parsing::SyntaxReference) -> Self {
        Self {
            state: syntect::parsing::ParseState::new(syntax),
            scopes: syntect::parsing::ScopeStack::new(),
            embedded: None,
            streams: Vec::new(),
        }
    }

    /// The parser for `stream`, taken out of the snapshot for the
    /// duration of a row, or a fresh one when the stream is new — or was
    /// last parsing a different language, which cannot continue.
    fn take_stream(
        &mut self,
        stream: u32,
        syntax_index: usize,
        syntax_set: &SyntaxSet,
    ) -> EmbeddedParseState {
        if let Some(pos) = self.streams.iter().position(|(id, _)| *id == stream) {
            let (_, parser) = self.streams.remove(pos);
            if parser.syntax_index == syntax_index {
                return parser;
            }
        }
        EmbeddedParseState {
            syntax_index,
            state: syntect::parsing::ParseState::new(&syntax_set.syntaxes()[syntax_index]),
            scopes: syntect::parsing::ScopeStack::new(),
        }
    }

    /// Put a stream's parser back, as the most recent; the oldest goes
    /// once there are more than a hunk's two sides could need.
    fn keep_stream(&mut self, stream: u32, parser: EmbeddedParseState) {
        self.streams.push((stream, parser));
        if self.streams.len() > MAX_REGION_STREAMS {
            self.streams.remove(0);
        }
    }
}

/// Small/large file threshold (whole-file cache vs viewport window).
const MAX_PARSE_BYTES: usize = 1024 * 1024;

/// Distance between checkpoint anchors. Smaller = faster convergence on edit.
const CHECKPOINT_INTERVAL: usize = 256;

/// Per-pass cap on partial-update parsing past `dirty_pos`. Bounds work for
/// pathological edits whose effect doesn't converge.
const CONVERGENCE_BUDGET: usize = 64 * 1024;

/// Whether `pos` starts a line under `find_line_end`'s rules: after `\n`,
/// or after a `\r` that no `\n` follows.
fn is_line_start(buffer: &Buffer, pos: usize) -> bool {
    if pos == 0 {
        return true;
    }
    match buffer
        .slice_bytes(pos - 1..(pos + 1).min(buffer.len()))
        .as_slice()
    {
        [b'\n', ..] => true,
        [b'\r', next @ ..] => next.first() != Some(&b'\n'),
        _ => false,
    }
}

/// Byte position one past the end of the line that starts at `pos`.
/// Accepts `\n` and `\r\n` terminators; returns `content_bytes.len()`
/// when the buffer ends without a terminator (the streaming tail).
fn find_line_end(content_bytes: &[u8], pos: usize) -> usize {
    let mut line_end = pos;
    while line_end < content_bytes.len() {
        if content_bytes[line_end] == b'\n' {
            line_end += 1;
            break;
        } else if content_bytes[line_end] == b'\r' {
            if line_end + 1 < content_bytes.len() && content_bytes[line_end + 1] == b'\n' {
                line_end += 2;
            } else {
                line_end += 1;
            }
            break;
        }
        line_end += 1;
    }
    line_end
}

/// UTF-8-decoded line ready to feed `state.parse_line`.
struct PreparedLine {
    /// What `parse_line` sees: line content always terminated by `\n`,
    /// EXCEPT for the buffer's final partial line where no `\n` has
    /// arrived yet (caller must not commit cache state past such a
    /// line — see `extend_cache_forward`).
    line_for_syntect: String,
    /// Byte length of the line excluding `\r` / `\n` terminator.
    line_content_len: usize,
    /// Whether the original line ended with `\n` (true for every line
    /// except the streaming tail).
    ends_with_newline: bool,
}

/// A row as its embedded language parser should see it: the first
/// `prefix` bytes — a gutter, a diff marker — removed and nothing else,
/// so the parser's offsets land on the code. `None` for a row too short
/// to hold the prefix (a filler row) or whose prefix ends inside a
/// character; such a row keeps the styling it has and advances no parser.
fn prepare_row_content(prepared: &PreparedLine, prefix: usize) -> Option<PreparedLine> {
    if prepared.line_content_len < prefix {
        return None;
    }
    let rest = prepared.line_for_syntect.get(prefix..)?;
    Some(PreparedLine {
        line_for_syntect: rest.to_string(),
        line_content_len: prepared.line_content_len - prefix,
        ends_with_newline: prepared.ends_with_newline,
    })
}

/// Prepare one unified-diff source row for its embedded language parser.
///
/// Added, removed, and context rows carry a one-byte patch marker that is
/// not part of the source language. Which rows those are is the diff
/// grammar's call, read off the row's host spans: a row it scoped as an
/// insertion or deletion is source, and so is one that begins with the
/// context marker. File headers (`+++` / `---`), hunk headers and the
/// rest of the metadata return `None` so they retain host-grammar
/// styling without advancing the embedded parser — a removed line that
/// happens to begin `--` is source, not a header, and the grammar knows
/// the difference because it knows whether it is inside a hunk.
fn prepare_unified_diff_content(
    prepared: &PreparedLine,
    host_spans: &[(usize, usize, HighlightCategory)],
) -> Option<PreparedLine> {
    let marker = *prepared.line_for_syntect.as_bytes().first()?;
    let is_change = host_spans.iter().any(|&(_, _, category)| {
        matches!(
            category,
            HighlightCategory::Inserted | HighlightCategory::Deleted
        )
    });
    if !(is_change || marker == b' ') {
        return None;
    }
    prepare_row_content(prepared, 1)
}

/// Resolve a cached span's colours against a theme.
fn color_span(span: CachedSpan, theme: &Theme) -> HighlightSpan {
    let bg = span
        .background_category
        .and_then(|category| highlight_bg(category, theme))
        .or_else(|| highlight_bg(span.category, theme));
    HighlightSpan {
        range: span.range,
        color: highlight_color(span.category, theme),
        bg,
        category: Some(span.category),
    }
}

/// How much of a line is highlighted: the rest of a longer one is plain text.
///
/// Syntect parses a line in one shot, and an edit re-parses the line it lands
/// in, so without a bound an edit in a long line costs the whole line up to
/// the viewport. On a file that is one long line (minified JSON or JS,
/// routinely hundreds of KB to tens of MB) that was every keystroke: ~270 ms
/// near the end of the first megabyte of a 50 MB array (issue #3631).
///
/// Only the line's first this-many bytes reach syntect. The parse after the
/// line carries on from the state those bytes leave, as if the line ended
/// there. Because the cut is at a fixed distance into the line, it falls in
/// the same place however much of the line a pass has in hand — the colours
/// do not depend on where the viewport is, there is nothing about a long line
/// for the cache to track, and no scan for the line's end is needed. VS Code
/// makes the same call at the same size (`editor.maxTokenizationLineLength`),
/// leaving such lines untokenized altogether.
const MAX_HIGHLIGHT_LINE_BYTES: usize = 20_000;

/// The part of a line's content that is highlighted: all of it, or its first
/// [`MAX_HIGHLIGHT_LINE_BYTES`] cut back to a character boundary.
fn highlighted_prefix(line: &str) -> &str {
    if line.len() <= MAX_HIGHLIGHT_LINE_BYTES {
        return line;
    }
    let mut cut = MAX_HIGHLIGHT_LINE_BYTES;
    while !line.is_char_boundary(cut) {
        cut -= 1;
    }
    &line[..cut]
}

/// Slice the line starting at `pos` from `content_bytes` and prepare
/// it for `parse_line`. Returns `(line_end, line_byte_len, prepared)`:
/// callers always advance by `line_byte_len` to `line_end`; `prepared`
/// is `None` only when the line wasn't valid UTF-8 (skip & continue).
fn prepare_line_at(content_bytes: &[u8], pos: usize) -> (usize, usize, Option<PreparedLine>) {
    let line_end = find_line_end(content_bytes, pos);
    let line_bytes = &content_bytes[pos..line_end];
    let line_byte_len = line_bytes.len();
    let prepared = std::str::from_utf8(line_bytes).ok().map(|line_str| {
        let whole = line_str.trim_end_matches(&['\r', '\n'][..]);
        let line_content = highlighted_prefix(whole);
        let ends_with_newline = line_str.ends_with('\n');
        // A cut line is parsed as ending at the cut, whether or not this
        // pass holds the rest of it — the same parse either way.
        let cut = line_content.len() < whole.len();
        let is_streaming_tail = !cut && line_end == content_bytes.len() && !ends_with_newline;
        let line_for_syntect = if is_streaming_tail {
            line_content.to_string()
        } else {
            format!("{}\n", line_content)
        };
        PreparedLine {
            line_for_syntect,
            line_content_len: line_content.len(),
            ends_with_newline,
        }
    });
    (line_end, line_byte_len, prepared)
}

/// Streaming span sink that fills the *unclaimed* gaps of a washed row
/// with that row's wash.
///
/// The diff categories (`Inserted` / `Deleted` / `Changed`) are the
/// ones whose `bg_extends_to_line_end()` is set: the renderer treats
/// them as a whole-row wash rather than as styling of the scoped
/// text. Syntect's `Diff` grammar mostly cooperates — a `+`/`-` row is
/// one span covering the row — but a hunk header is not: it scopes
/// `@@ -1,2 +3,4 @@` under `meta.diff.range` and leaves the enclosing
/// section name `git diff` appends after the closing `@@` outside that
/// scope entirely, so the bar stopped at the `@@` and resumed only in
/// the trailing fill past end-of-line, with the section name sitting in
/// a hole.
///
/// So the rule is applied per row rather than per scope: when the span
/// covering the row start carries a wash, every byte range of the row
/// that *no span claims* is emitted with that same category. Rows whose
/// covering span at the row start is not a wash — every row of ordinary
/// source, and the non-wash rows of a diff — pass through untouched.
/// Filler spans merge with their neighbours in `merge_adjacent_spans`,
/// and the wash categories resolve to the editor foreground, so filling
/// changes background only.
///
/// The guarantee stops at "unclaimed": a span that *does* claim a range
/// inside a washed row keeps its own category, background or none, so
/// the bar can still break across it. Fresh's diff grammar anchors its
/// non-wash row rules, so supported diff output does not currently
/// trigger that limitation; the sink is nevertheless deliberately not
/// scope-aware.
struct RowWash<F: FnMut(usize, usize, HighlightCategory, Option<HighlightCategory>)> {
    row_start: usize,
    row_end: usize,
    /// The wash to fill gaps with; `None` until the span covering
    /// `row_start` decides, and stays `None` for rows carrying no wash.
    wash: Option<HighlightCategory>,
    /// Whether the span covering `row_start` has been seen (so `wash`
    /// is decided).
    decided: bool,
    /// End of the highest byte range emitted so far.
    covered: usize,
    inner: F,
}

impl<F: FnMut(usize, usize, HighlightCategory, Option<HighlightCategory>)> RowWash<F> {
    fn new(row_start: usize, row_end: usize, inner: F) -> Self {
        Self {
            row_start,
            row_end,
            wash: None,
            decided: false,
            covered: row_start,
            inner,
        }
    }

    fn emit(
        &mut self,
        start: usize,
        end: usize,
        category: HighlightCategory,
        background_category: Option<HighlightCategory>,
    ) {
        // Decide from the span that *covers* `row_start`, not from
        // whichever span happens to arrive first: an empty leading
        // segment (`start == end == row_start`) must not decide the
        // row, so the rule does not depend on the producer skipping
        // zero-length ranges.
        if !self.decided && end > self.row_start {
            self.decided = true;
            if start <= self.row_start && category.bg_extends_to_line_end() {
                self.wash = Some(category);
            }
        }
        if let Some(wash) = self.wash {
            if start > self.covered {
                (self.inner)(self.covered, start, wash, None);
            }
        }
        self.covered = self.covered.max(end);
        (self.inner)(start, end, category, background_category);
    }

    /// Emit the trailing filler, if any. Must be called once the row's
    /// spans have all been emitted.
    fn finish(mut self) {
        if let Some(wash) = self.wash {
            if self.covered < self.row_end {
                (self.inner)(self.covered, self.row_end, wash, None);
            }
        }
    }
}

impl TextMateEngine {
    /// Create a new TextMate engine for the given syntax
    pub fn new(syntax_set: Arc<SyntaxSet>, syntax_index: usize) -> Self {
        Self::with_language(syntax_set, syntax_index, None)
    }

    /// Create a new TextMate engine with a tree-sitter language for non-highlighting features
    pub fn with_language(
        syntax_set: Arc<SyntaxSet>,
        syntax_index: usize,
        ts_language: Option<Language>,
    ) -> Self {
        let syntax_name = syntax_set.syntaxes()[syntax_index].name.clone();
        let embedding = EmbeddingSpec::compile_for_syntax(&syntax_name);
        let table_body_scope = TABLE_SPECS
            .iter()
            .find(|d| d.host_syntax == syntax_name)
            .and_then(|d| syntect::parsing::Scope::new(d.body_scope).ok());
        Self {
            syntax_set,
            syntax_index,
            checkpoint_markers: MarkerList::new(),
            checkpoint_states: HashMap::new(),
            dirty_from: None,
            dirty_end: 0,
            cache: None,
            last_buffer_len: 0,
            ts_language,
            stats: HighlightStats::default(),
            embedding,
            table_body_scope,
            embedded_syntax_memo: HashMap::new(),
            host_span_scratch: Vec::new(),
            child_span_scratch: Vec::new(),
            scope_category_cache: HashMap::new(),
            declared_host: false,
            region_markers: MarkerList::new(),
            declared: HashMap::new(),
            active_region: None,
        }
    }

    /// An engine for a buffer a plugin composed: plain text of its own,
    /// coloured only where the declared regions say there is code.
    pub fn declared_region_host(syntax_set: Arc<SyntaxSet>, regions: Vec<DeclaredRegion>) -> Self {
        let plain = syntax_set.find_syntax_plain_text();
        let index = syntax_set
            .syntaxes()
            .iter()
            .position(|s| std::ptr::eq(s, plain))
            .unwrap_or(0);
        let mut engine = Self::new(syntax_set, index);
        engine.declared_host = true;
        engine.set_declared_regions(regions);
        engine
    }

    /// Whether this engine takes its regions from a plugin rather than a
    /// grammar (see [`Self::declared_region_host`]).
    pub fn hosts_declared_regions(&self) -> bool {
        self.declared_host
    }

    /// Replace the declared regions. They are kept sorted and disjoint:
    /// an empty region, or one that overlaps the region before it, is
    /// dropped with a warning rather than left to fight over rows. Every
    /// cached span is dropped, since any of them may now be wrong.
    pub fn set_declared_regions(&mut self, mut regions: Vec<DeclaredRegion>) {
        let stale: Vec<MarkerId> = self.declared.keys().copied().collect();
        for id in stale {
            self.region_markers.delete(id);
        }
        self.declared.clear();
        self.active_region = None;
        regions.sort_by_key(|region| region.start);
        let mut previous_end = 0;
        for mut region in regions {
            if region.end <= region.start {
                continue;
            }
            if region.start < previous_end {
                tracing::warn!(
                    "declared region {}..{} overlaps the one ending at {}; dropped",
                    region.start,
                    region.end,
                    previous_end
                );
                continue;
            }
            previous_end = region.end;
            if region.streams.is_empty() {
                region.streams.push(DEFAULT_REGION_STREAM);
            }
            let id = self.region_markers.create_span(region.start, region.end);
            self.declared.insert(id, region);
        }
        self.invalidate_all();
    }

    /// The declared regions and the syntax set they index, for a caller
    /// that is replacing this engine and must not lose them. The regions
    /// come back at their live bounds, so an engine rebuilt from them
    /// picks up where this one left off.
    pub fn take_declared_regions(&mut self) -> (Arc<SyntaxSet>, Vec<DeclaredRegion>) {
        let mut regions: Vec<DeclaredRegion> = std::mem::take(&mut self.declared)
            .into_iter()
            .filter_map(|(id, mut region)| {
                let (start, end) = self.region_bounds(id)?;
                region.start = start;
                region.end = end;
                Some(region)
            })
            .collect();
        regions.sort_by_key(|region| region.start);
        self.region_markers = MarkerList::new();
        self.active_region = None;
        (Arc::clone(&self.syntax_set), regions)
    }

    /// A region's live bounds, or `None` once an edit has swallowed it.
    fn region_bounds(&self, id: MarkerId) -> Option<(usize, usize)> {
        let start = self.region_markers.get_position(id)?;
        let (_, start, end) = self
            .region_markers
            .query_range(start, start + 1)
            .into_iter()
            .find(|&(found, _, _)| found == id)?;
        (end > start).then_some((start, end))
    }

    /// The declared region a row belongs to, by the row's first byte.
    ///
    /// Regions come in runs of rows, so the answer is remembered and the
    /// marker tree is consulted once per run rather than once per row.
    fn declared_region_at(&mut self, row_start: usize) -> Option<MarkerId> {
        if let Some((id, start, end)) = self.active_region {
            if start <= row_start && row_start < end {
                return Some(id);
            }
        }
        let found = self
            .region_markers
            .query_range(row_start, row_start + 1)
            .into_iter()
            .filter(|&(id, start, end)| {
                start <= row_start && row_start < end && self.declared.contains_key(&id)
            })
            .max_by_key(|&(_, start, _)| start);
        match found {
            Some((id, start, end)) => {
                self.active_region = Some((id, start, end));
                Some(id)
            }
            None => {
                self.active_region = None;
                None
            }
        }
    }

    /// Get performance stats for testing and diagnostics.
    pub fn stats(&self) -> &HighlightStats {
        &self.stats
    }

    /// Reset performance counters.
    pub fn reset_stats(&mut self) {
        self.stats = HighlightStats::default();
    }

    /// Get the tree-sitter language (for indentation, semantic highlighting, etc.)
    pub fn language(&self) -> Option<&Language> {
        self.ts_language.as_ref()
    }

    /// Buffer-insert notification. Shifts span offsets in place and marks
    /// the cache dirty so the partial-update path runs on next render.
    pub fn notify_insert(&mut self, position: usize, length: usize) {
        self.checkpoint_markers.adjust_for_insert(position, length);
        self.region_markers.adjust_for_insert(position, length);
        self.active_region = None;
        self.dirty_end = match self.dirty_from {
            Some(_) if self.dirty_end >= position => self.dirty_end + length,
            Some(_) => self.dirty_end,
            None => 0,
        }
        .max(position + length);
        self.dirty_from = Some(self.dirty_from.map_or(position, |d| d.min(position)));
        if let Some(cache) = &mut self.cache {
            for span in &mut cache.spans {
                if span.range.start >= position {
                    span.range.start += length;
                    span.range.end += length;
                } else if span.range.end > position {
                    span.range.end += length;
                }
            }
            // A line the insert lands in is stale until the partial update
            // re-parses it (it starts before `dirty_from`); only positions
            // move here, exactly as for spans.
            for line in &mut cache.lines {
                if line.start >= position {
                    line.start += length;
                    line.end += length;
                } else if line.end > position {
                    line.end += length;
                }
            }
            if cache.range.end >= position {
                cache.range.end += length;
                if position < cache.range.end {
                    cache.tail_state = None;
                }
            }
        }
    }

    /// Buffer-delete notification. Mirror of `notify_insert`.
    pub fn notify_delete(&mut self, position: usize, length: usize) {
        self.checkpoint_markers.adjust_for_delete(position, length);
        self.region_markers.adjust_for_delete(position, length);
        self.active_region = None;
        let delete_end = position + length;
        self.dirty_end = match self.dirty_from {
            Some(_) if self.dirty_end >= delete_end => self.dirty_end - length,
            Some(_) => self.dirty_end.min(position),
            None => 0,
        }
        .max(position);
        self.dirty_from = Some(self.dirty_from.map_or(position, |d| d.min(position)));
        if let Some(cache) = &mut self.cache {
            cache.spans.retain_mut(|span| {
                if span.range.start >= delete_end {
                    span.range.start -= length;
                    span.range.end -= length;
                    true
                } else if span.range.end <= position {
                    true
                } else if span.range.start >= position && span.range.end <= delete_end {
                    false
                } else {
                    if span.range.start < position {
                        span.range.end = position.min(span.range.end);
                    } else {
                        span.range.start = position;
                        span.range.end = position + span.range.end.saturating_sub(delete_end);
                    }
                    span.range.start < span.range.end
                }
            });
            // Lines the delete touches are dropped rather than clamped: the
            // partial update that repairs this edit re-parses from before
            // `position` and lays down fresh entries for them.
            cache.lines.retain_mut(|line| {
                if line.start >= delete_end {
                    line.start -= length;
                    line.end -= length;
                    true
                } else {
                    line.end <= position
                }
            });
            if cache.range.end > delete_end {
                cache.range.end -= length;
            } else if cache.range.end > position {
                cache.range.end = position;
            }
            if position < cache.range.end {
                cache.tail_state = None;
            }
        }
    }

    /// Create a checkpoint at `current_offset` carrying the supplied
    /// parse state, unless one already exists within half an interval
    /// (which would shadow it). Callers gate on
    /// `bytes_since_checkpoint >= CHECKPOINT_INTERVAL` to control
    /// spacing.
    fn maybe_create_checkpoint(&mut self, current_offset: usize, snapshot: &ParseSnapshot) {
        let nearby = self.checkpoint_markers.query_range(
            current_offset.saturating_sub(CHECKPOINT_INTERVAL / 2),
            current_offset + CHECKPOINT_INTERVAL / 2,
        );
        if nearby.is_empty() {
            let marker_id = self.checkpoint_markers.create(current_offset);
            self.checkpoint_states.insert(marker_id, snapshot.clone());
        }
    }

    /// Forget every checkpoint at or past `from`.
    ///
    /// The engine's invariant is that **with no edit pending (`dirty_from` is
    /// `None`), every checkpoint's state is the true parse state at its
    /// position**. `notify_insert`/`notify_delete` only move checkpoints, so
    /// those past an edit hold pre-edit states until a pass re-parses through
    /// them or converges onto them; `dirty_from` marks that they are
    /// unverified. A pass that must clear `dirty_from` without having verified
    /// them drops them here instead — re-creating a checkpoint costs a parse
    /// of the text before it, resuming from a stale one colours and frames the
    /// text as it was before the edit.
    fn drop_checkpoints_from(&mut self, from: usize) {
        for (id, _, _) in self.checkpoint_markers.query_range(from, usize::MAX) {
            self.checkpoint_markers.delete(id);
            self.checkpoint_states.remove(&id);
        }
    }

    /// Whether this syntax hosts any structure `structure_lines_in` reports
    /// — embedded-language regions or tables. Currently only Markdown and
    /// Vue; every other syntax records nothing per line.
    fn hosts_structure(&self) -> bool {
        !self.embedding.is_empty() || self.table_body_scope.is_some()
    }

    /// (inside a region, inside a table body) for a scope stack.
    fn structure_state(&self, scopes: &syntect::parsing::ScopeStack) -> (bool, bool) {
        (
            self.active_region_spec(scopes).is_some(),
            self.table_body_scope
                .is_some_and(|scope| scopes_contain(scopes, scope)),
        )
    }

    /// `parse_snapshot_line` for the three cache paths: the same parse, plus
    /// the line's structure appended to `lines` when the syntax hosts any and
    /// the line is whole — terminated, or the buffer's last. A partial
    /// streaming tail is not recorded, for the reason its spans are not
    /// committed (see `extend_cache_forward`), and neither is a line the host
    /// failed to parse, whose scope transition would be a guess.
    #[allow(clippy::too_many_arguments)]
    fn parse_cached_line(
        &mut self,
        snapshot: &mut ParseSnapshot,
        prepared: &PreparedLine,
        line_start: usize,
        line_end: usize,
        buffer_len: usize,
        lines: &mut Vec<LineStructure>,
        on_span: impl FnMut(usize, usize, HighlightCategory, Option<HighlightCategory>),
    ) -> bool {
        let before = self
            .hosts_structure()
            .then(|| self.structure_state(&snapshot.scopes));
        let parsed = self.parse_snapshot_line(snapshot, prepared, line_start, on_span);
        if let Some((region_before, table_before)) = before {
            if parsed && (prepared.ends_with_newline || line_end >= buffer_len) {
                let (region_after, table_after) = self.structure_state(&snapshot.scopes);
                lines.push(LineStructure {
                    start: line_start,
                    end: line_end,
                    region_before,
                    region_after,
                    table_before,
                    table_after,
                });
            }
        }
        parsed
    }

    /// Drive `state.parse_line(prepared.line_for_syntect)` and emit one
    /// span per category-carrying byte range via `on_span(start, end,
    /// category)`. Returns `(false, None)` when `parse_line` errored —
    /// caller should advance past the line and continue (state may have
    /// been mutated mid-parse but is left as-is, matching prior
    /// behaviour).
    ///
    /// When `watch_scopes` is non-empty, additionally returns the
    /// absolute byte range of the first contiguous run where any of
    /// those scopes was on the stack (used to locate the language token
    /// on a region-opening line).
    ///
    /// Span emission is in two passes: the op iterator emits the
    /// segment between consecutive ops with the scope-stack-active
    /// category, and a trailing segment covers `[syntect_offset,
    /// line_content_len)` when the final op didn't reach end-of-line.
    fn parse_line_into_spans(
        &mut self,
        state: &mut syntect::parsing::ParseState,
        current_scopes: &mut syntect::parsing::ScopeStack,
        prepared: &PreparedLine,
        current_offset: usize,
        watch_scopes: &[syntect::parsing::Scope],
        mut on_span: impl FnMut(usize, usize, HighlightCategory),
    ) -> (bool, Option<Range<usize>>) {
        let ops = match state.parse_line(&prepared.line_for_syntect, &self.syntax_set) {
            Ok(ops) => ops,
            Err(_) => return (false, None),
        };

        let line_content_len = prepared.line_content_len;
        let mut syntect_offset = 0;
        let mut watch_run_start: Option<usize> = None;
        let mut watched_range: Option<Range<usize>> = None;

        for (op_offset, op) in ops {
            let clamped_op_offset = op_offset.min(line_content_len);
            if clamped_op_offset > syntect_offset {
                if let Some(category) = self.scope_stack_to_category(current_scopes) {
                    on_span(
                        current_offset + syntect_offset,
                        current_offset + clamped_op_offset,
                        category,
                    );
                }
            }
            syntect_offset = clamped_op_offset;
            #[allow(clippy::let_underscore_must_use)]
            let _ = current_scopes.apply(&op);
            if !watch_scopes.is_empty() && watched_range.is_none() {
                let active = watch_scopes
                    .iter()
                    .any(|w| scopes_contain(current_scopes, *w));
                match (watch_run_start, active) {
                    (None, true) => watch_run_start = Some(clamped_op_offset),
                    (Some(start), false) => {
                        watched_range =
                            Some(current_offset + start..current_offset + clamped_op_offset);
                    }
                    _ => {}
                }
            }
        }

        if syntect_offset < line_content_len {
            if let Some(category) = self.scope_stack_to_category(current_scopes) {
                on_span(
                    current_offset + syntect_offset,
                    current_offset + line_content_len,
                    category,
                );
            }
        }
        if let (Some(start), None) = (watch_run_start, watched_range.as_ref()) {
            watched_range = Some(current_offset + start..current_offset + line_content_len);
        }
        (true, watched_range)
    }

    /// Parse one line advancing the composite `snapshot` (host parser +
    /// optional embedded-language child parser). This is the single
    /// per-line entry point for all parse paths, so embedded regions
    /// behave identically under cold start, forward extension and
    /// partial update — and the region/table structure those paths record
    /// for `structure_lines_in` is read off the same snapshot.
    ///
    /// Line classification is driven purely by the host grammar's scope
    /// stack (no second lexer that could disagree with what the host
    /// grammar recognizes as a region):
    /// - no region scope before or after → plain host line;
    /// - none before, one after → region opened: host-styled, and the
    ///   language token (located by `language_scope`), or the spec's
    ///   `default_language` when absent/unresolvable, selects the child
    ///   syntax for subsequent lines;
    /// - same region before and after → region content: parsed by the
    ///   child (host spans for the line are suppressed); with no child
    ///   (no default and no recognizable token) the host's own styling
    ///   (e.g. `markup.raw`) is kept;
    /// - present before, absent after → closing delimiter: host-styled.
    ///
    /// The host parser always consumes the line first — inside a region
    /// it is in a cheap "raw" context — because only it knows where the
    /// region ends. Content lines therefore cost one extra (trivial)
    /// host `parse_line` on top of the child parse.
    ///
    /// Spans leave this function through `RowWash`, which fills the
    /// unclaimed gaps of rows the grammar washes (see its docs) — a
    /// `@@` hunk header whose trailing section name is left outside
    /// `meta.diff.range` would otherwise punch a hole in the bar.
    fn parse_snapshot_line(
        &mut self,
        snapshot: &mut ParseSnapshot,
        prepared: &PreparedLine,
        current_offset: usize,
        on_span: impl FnMut(usize, usize, HighlightCategory, Option<HighlightCategory>),
    ) -> bool {
        self.stats.bytes_tokenized += prepared.line_content_len;
        let mut wash = RowWash::new(
            current_offset,
            current_offset + prepared.line_content_len,
            on_span,
        );
        let parsed =
            self.parse_snapshot_line_spans(snapshot, prepared, current_offset, |s, e, c, bg| {
                wash.emit(s, e, c, bg)
            });
        wash.finish();
        parsed
    }

    /// `parse_snapshot_line` without the row-wash gap filling; see there.
    fn parse_snapshot_line_spans(
        &mut self,
        snapshot: &mut ParseSnapshot,
        prepared: &PreparedLine,
        current_offset: usize,
        mut on_span: impl FnMut(usize, usize, HighlightCategory, Option<HighlightCategory>),
    ) -> bool {
        if self.declared_host {
            return self.parse_declared_row(snapshot, prepared, current_offset, on_span);
        }
        if self.embedding.is_empty() {
            return self
                .parse_line_into_spans(
                    &mut snapshot.state,
                    &mut snapshot.scopes,
                    prepared,
                    current_offset,
                    &[],
                    |start, end, category| on_span(start, end, category, None),
                )
                .0;
        }

        let was_in = self.active_region_spec(&snapshot.scopes);
        // Watch for a language token on lines that could open a region —
        // and, for a host whose regions can hand over on a single line (a
        // unified diff at each `diff --git`), on every line, since the
        // region scope is active at both ends of that line and only the
        // new token says the child parser must be retargeted. Copy the
        // (deduplicated) language scopes to the stack so no borrow of
        // self outlives the parse call below.
        let watch_everywhere = self
            .embedding
            .iter()
            .any(|spec| spec.retargets_on_same_line);
        let mut watch_buf = [self.embedding[0].language_scope; MAX_WATCH_SCOPES];
        let mut watch_len = 0;
        if was_in.is_none() || watch_everywhere {
            for spec in &self.embedding {
                if watch_len < MAX_WATCH_SCOPES
                    && !watch_buf[..watch_len].contains(&spec.language_scope)
                {
                    watch_buf[watch_len] = spec.language_scope;
                    watch_len += 1;
                }
            }
        }

        let mut host_spans = std::mem::take(&mut self.host_span_scratch);
        host_spans.clear();
        let (parse_ok, language_range) = self.parse_line_into_spans(
            &mut snapshot.state,
            &mut snapshot.scopes,
            prepared,
            current_offset,
            &watch_buf[..watch_len],
            |start, end, category| host_spans.push((start, end, category)),
        );
        if !parse_ok {
            // Transient host parse error (a regex-engine failure): emit
            // nothing for this line, matching the single-layer path, and
            // leave region membership untouched. The host scope stack is
            // left as-is on error, so treating the error as a region
            // close would silently drop the child parser and
            // de-highlight the rest of the region.
            self.host_span_scratch = host_spans;
            return false;
        }
        let now_in = self.active_region_spec(&snapshot.scopes);

        // NOTE for future `EMBEDDING_SPECS` hosts: classification is
        // line-granular, so a host whose regions can close AND reopen
        // within one line reads as continuous region content (same kind)
        // or as a default-language restart (different kind). Markdown
        // fences and Vue script/style tags occupy whole lines, so
        // neither arises for the current hosts.
        if let (Some(i), Some(j)) = (was_in, now_in) {
            if i == j {
                // A host may replace one same-kind region with another on a
                // single line (unified diff file headers). A newly scoped
                // language token restarts the child for the new region; the
                // transition line itself remains host-styled.
                if language_range.is_some() && self.embedding[i].retargets_on_same_line {
                    snapshot.embedded =
                        self.embedded_state_for_region(prepared, current_offset, language_range, j);
                    for &(start, end, category) in &host_spans {
                        on_span(start, end, category, None);
                    }
                    self.host_span_scratch = host_spans;
                    return true;
                }
                // Region content line.
                if snapshot.embedded.is_some() {
                    let mut embedded = snapshot
                        .embedded
                        .take()
                        .expect("checked embedded.is_some() above");
                    let stripped;
                    let (child_line, child_offset) = match self.embedding[i].content_mode {
                        EmbeddedContentMode::WholeLine => (Some(prepared), current_offset),
                        EmbeddedContentMode::UnifiedDiff => {
                            stripped = prepare_unified_diff_content(prepared, &host_spans);
                            (stripped.as_ref(), current_offset + 1)
                        }
                    };
                    // Metadata and hunk-header rows inside a diff region stay
                    // host-styled and do not disturb the source parser state.
                    let Some(child_line) = child_line else {
                        for &(start, end, category) in &host_spans {
                            on_span(start, end, category, None);
                        }
                        snapshot.embedded = Some(embedded);
                        self.host_span_scratch = host_spans;
                        return true;
                    };
                    let mut child_spans = std::mem::take(&mut self.child_span_scratch);
                    child_spans.clear();
                    let (child_ok, _) = self.parse_line_into_spans(
                        &mut embedded.state,
                        &mut embedded.scopes,
                        child_line,
                        child_offset,
                        &[],
                        |start, end, category| child_spans.push((start, end, category)),
                    );
                    // On a child parse error, fall back to the host's
                    // raw styling for the line rather than leaving it
                    // unstyled.
                    if child_ok {
                        let background_category = match self.embedding[i].content_mode {
                            EmbeddedContentMode::UnifiedDiff => host_spans
                                .iter()
                                .map(|&(_, _, category)| category)
                                .find(|category| {
                                    matches!(
                                        category,
                                        HighlightCategory::Inserted | HighlightCategory::Deleted
                                    )
                                }),
                            EmbeddedContentMode::WholeLine => None,
                        };
                        // Preserve the diff marker's host category while
                        // source syntax owns the remainder of the row.
                        for &(start, end, category) in &host_spans {
                            let clipped_end = end.min(child_offset);
                            if start < clipped_end {
                                on_span(start, clipped_end, category, None);
                            }
                        }
                        // The sink is a `RowWash`, and the marker span above
                        // carries the row's Inserted/Deleted category from
                        // its first byte, so the wash fills every gap the
                        // source grammar leaves unscoped and the run after
                        // the last token. The child spans only need the
                        // background to ride along on the ranges they claim.
                        for &(start, end, category) in &child_spans {
                            on_span(start, end, category, background_category);
                        }
                    } else {
                        for &(start, end, category) in &host_spans {
                            on_span(start, end, category, None);
                        }
                    }
                    snapshot.embedded = Some(embedded);
                    self.child_span_scratch = child_spans;
                    self.host_span_scratch = host_spans;
                    return true;
                }
                // No child (no default and no recognizable token): the
                // line keeps the host's own styling below.
            }
        }

        for &(start, end, category) in &host_spans {
            on_span(start, end, category, None);
        }
        match (was_in, now_in) {
            (None, Some(j)) => {
                // Region opened: language token if the grammar scoped
                // one, else the spec default.
                snapshot.embedded =
                    self.embedded_state_for_region(prepared, current_offset, language_range, j);
            }
            (Some(i), Some(j)) if i != j => {
                // One region kind closed and another opened on the same
                // line; the opening token wasn't watched, so start the
                // new region with its default language.
                snapshot.embedded =
                    self.embedded_state_for_region(prepared, current_offset, None, j);
            }
            (Some(_), None) => snapshot.embedded = None,
            _ => {}
        }
        self.host_span_scratch = host_spans;
        true
    }

    /// One row of a declared-region host (see
    /// [`Self::declared_region_host`]). A row inside a declared region is
    /// handed, prefix removed, to the parser of each stream the region
    /// names, and coloured by the first; a row outside every region is
    /// left as the plugin styled it and touches no parser, so a header or
    /// a comment box between two runs of one stream does not break the
    /// construct they are in the middle of.
    fn parse_declared_row(
        &mut self,
        snapshot: &mut ParseSnapshot,
        prepared: &PreparedLine,
        current_offset: usize,
        mut on_span: impl FnMut(usize, usize, HighlightCategory, Option<HighlightCategory>),
    ) -> bool {
        let Some(id) = self.declared_region_at(current_offset) else {
            return true;
        };
        let (syntax_index, prefix, stream_count) = {
            let region = &self.declared[&id];
            (region.syntax_index, region.prefix, region.streams.len())
        };
        let Some(syntax_index) = syntax_index else {
            return true;
        };
        let stripped;
        let line = if prefix == 0 {
            prepared
        } else {
            match prepare_row_content(prepared, prefix) {
                Some(rest) => {
                    stripped = rest;
                    &stripped
                }
                None => return true,
            }
        };
        let child_offset = current_offset + prefix;
        let mut child_spans = std::mem::take(&mut self.child_span_scratch);
        child_spans.clear();
        let mut ok = true;
        for k in 0..stream_count {
            let stream = self.declared[&id].streams[k];
            let mut parser = snapshot.take_stream(stream, syntax_index, &self.syntax_set);
            let (child_ok, _) = self.parse_line_into_spans(
                &mut parser.state,
                &mut parser.scopes,
                line,
                child_offset,
                &[],
                |start, end, category| {
                    if k == 0 {
                        child_spans.push((start, end, category));
                    }
                },
            );
            ok &= child_ok;
            snapshot.keep_stream(stream, parser);
        }
        if ok {
            for &(start, end, category) in &child_spans {
                on_span(start, end, category, None);
            }
        }
        self.child_span_scratch = child_spans;
        ok
    }

    /// Index of the spec whose region scope is on the host stack, if any.
    fn active_region_spec(&self, scopes: &syntect::parsing::ScopeStack) -> Option<usize> {
        self.embedding
            .iter()
            .position(|spec| scopes_contain(scopes, spec.region_scope))
    }

    /// Build the child parser for a region that just opened, from the
    /// language token the host grammar scoped on the opening line, or
    /// the spec's `default_language` when the token is absent or doesn't
    /// resolve to a syntax.
    fn embedded_state_for_region(
        &mut self,
        prepared: &PreparedLine,
        current_offset: usize,
        language_range: Option<Range<usize>>,
        spec_index: usize,
    ) -> Option<EmbeddedParseState> {
        let explicit = language_range
            .and_then(|range| {
                let rel = range.start.checked_sub(current_offset)?
                    ..range.end.checked_sub(current_offset)?;
                prepared.line_for_syntect.get(rel)
            })
            .and_then(|token| self.resolve_embedded_syntax(token));
        let syntax_index = match explicit {
            Some(index) => index,
            None => {
                let default = self.embedding[spec_index].default_language?;
                self.resolve_embedded_syntax(default)?
            }
        };
        Some(EmbeddedParseState {
            syntax_index,
            state: syntect::parsing::ParseState::new(&self.syntax_set.syntaxes()[syntax_index]),
            scopes: syntect::parsing::ScopeStack::new(),
        })
    }

    /// Resolve a fence-style language token ("rust", "py",
    /// "language-c++", "{.python}") to a syntax index, memoised. Plain
    /// text resolves to None so unrecognized fences keep the host's raw
    /// styling instead of losing it to a no-op child parse.
    fn resolve_embedded_syntax(&mut self, raw_token: &str) -> Option<usize> {
        let token = raw_token.trim();
        let token = token
            .strip_prefix("{.")
            .and_then(|t| t.strip_suffix('}'))
            .unwrap_or(token);
        let token = token.strip_prefix("language-").unwrap_or(token);
        if token.is_empty() {
            return None;
        }
        if let Some(cached) = self.embedded_syntax_memo.get(token) {
            return *cached;
        }
        let plain = self.syntax_set.find_syntax_plain_text();
        // Region tokens are usually short language names (`rust`, `ts`).
        // Diff regions instead provide a target path (`src/main.rs`), so
        // also try its basename and extension. This covers extensionless
        // grammar tokens such as Makefile as well as ordinary source files.
        let basename = token.rsplit(['/', '\\']).next().unwrap_or(token);
        let extension = basename.rsplit_once('.').map(|(_, ext)| ext);
        let resolved = std::iter::once(token)
            .chain((basename != token).then_some(basename))
            .chain(extension)
            .find_map(|candidate| self.syntax_set.find_syntax_by_token(candidate))
            .filter(|found| !std::ptr::eq(*found, plain))
            .and_then(|found| {
                self.syntax_set
                    .syntaxes()
                    .iter()
                    .position(|s| std::ptr::eq(s, found))
            });
        self.embedded_syntax_memo
            .insert(token.to_string(), resolved);
        resolved
    }

    /// Highlight the visible viewport. Path selection is documented in the
    /// module-level docs ("TextMate cache design").
    /// Test-only: inspect the cache commit point (range.end) and
    /// whether tail_state is populated. The cache must commit at a
    /// newline boundary — anything past that risks streaming
    /// forward-extension picking up where partial-line state poisoned
    /// the parser, see `test_partial_trailing_line_not_committed_to_cache`.
    #[cfg(test)]
    pub fn cache_commit_for_test(&self) -> (usize, bool) {
        match &self.cache {
            Some(c) => (c.range.end, c.tail_state.is_some()),
            None => (0, false),
        }
    }

    pub fn highlight_viewport(
        &mut self,
        buffer: &Buffer,
        viewport_start: usize,
        viewport_end: usize,
        theme: &Theme,
        context_bytes: usize,
    ) -> Vec<HighlightSpan> {
        self.sync_cache(buffer, viewport_start, viewport_end, context_bytes)
            .into_iter()
            .map(|span| color_span(span, theme))
            .collect()
    }

    /// Bring the cache up to date for a viewport and return the viewport's
    /// spans, uncoloured. This is the one dispatcher over the cache paths —
    /// hit, forward extension, partial update, cold parse — shared by
    /// `highlight_viewport` and `structure_lines_in`, so both read one parse.
    fn sync_cache(
        &mut self,
        buffer: &Buffer,
        viewport_start: usize,
        viewport_end: usize,
        context_bytes: usize,
    ) -> Vec<CachedSpan> {
        let buf_len = buffer.len();
        let (desired_parse_start, parse_end) = if buf_len <= MAX_PARSE_BYTES {
            (0, (viewport_end + context_bytes).min(buf_len))
        } else {
            let s = viewport_start.saturating_sub(context_bytes);
            let e = (viewport_end + context_bytes).min(buf_len);
            (s, e)
        };

        let dirty = self.dirty_from.take();
        let cache_covers_viewport = self.cache.as_ref().is_some_and(|c| {
            c.range.start <= desired_parse_start && c.range.end >= desired_parse_start
        });
        let exact_cache_hit = cache_covers_viewport
            && dirty.is_none()
            && self.last_buffer_len == buffer.len()
            && self
                .cache
                .as_ref()
                .is_some_and(|c| c.range.end >= parse_end);

        if cache_covers_viewport
            && dirty.is_none()
            && self.last_buffer_len == buffer.len()
            && self
                .cache
                .as_ref()
                .is_some_and(|c| c.range.end < parse_end && c.tail_state.is_none())
        {
            self.rewind_cache_to_checkpoint();
        }

        // Cache hit.
        if exact_cache_hit {
            self.stats.cache_hits += 1;
            return self.cached_spans_in(viewport_start, viewport_end);
        }

        // Forward extension.
        if dirty.is_none()
            && cache_covers_viewport
            && self.last_buffer_len == buffer.len()
            && self
                .cache
                .as_ref()
                .is_some_and(|c| c.range.end < parse_end && c.tail_state.is_some())
        {
            return self.extend_cache_forward(buffer, parse_end, viewport_start, viewport_end);
        }

        // Partial update.
        if cache_covers_viewport && dirty.is_some() {
            if let Some(dirty_pos) = dirty {
                if dirty_pos < parse_end {
                    if let Some(result) = self.try_partial_update(
                        buffer,
                        dirty_pos,
                        desired_parse_start,
                        parse_end,
                        viewport_start,
                        viewport_end,
                    ) {
                        return result;
                    }
                } else {
                    // Dirty region past viewport: cached spans are still valid.
                    self.dirty_from = Some(dirty_pos);
                    self.stats.cache_hits += 1;
                    return self.cached_spans_in(viewport_start, viewport_end);
                }
            }
        } else if let Some(d) = dirty {
            self.dirty_from = Some(d);
        }

        // Cold start / fallback.
        self.full_parse(
            buffer,
            desired_parse_start,
            parse_end,
            viewport_start,
            viewport_end,
            dirty,
        )
    }

    /// The cached spans overlapping the viewport.
    fn cached_spans_in(&self, viewport_start: usize, viewport_end: usize) -> Vec<CachedSpan> {
        let cache = self.cache.as_ref().unwrap();
        // Sorted and disjoint, so the viewport's spans are one contiguous run.
        let first = cache
            .spans
            .partition_point(|span| span.range.end <= viewport_start);
        let last = cache
            .spans
            .partition_point(|span| span.range.start < viewport_end);
        cache.spans[first..last.max(first)].to_vec()
    }

    /// Partial update path. Returns `Some` whenever an anchor was available,
    /// even on budget hit or EOF (see post-loop classification). `None` only
    /// when no checkpoint anchor reaches the dirty point.
    #[allow(clippy::too_many_arguments)]
    fn try_partial_update(
        &mut self,
        buffer: &Buffer,
        dirty_pos: usize,
        desired_parse_start: usize,
        parse_end: usize,
        viewport_start: usize,
        viewport_end: usize,
    ) -> Option<Vec<CachedSpan>> {
        let syntax = &self.syntax_set.syntaxes()[self.syntax_index];

        // Resume from the nearest checkpoint before the dirty point (bounded
        // search). It must lie strictly before the dirty point — a delete
        // collapses the checkpoints it covered onto its position, each still
        // holding the state of text that is gone — and on a line start: an
        // insert at a checkpoint's position pushes it into the middle of that
        // line while its state still belongs to the line start.
        let (actual_start, mut snapshot) = {
            let search_start = dirty_pos.saturating_sub(MAX_PARSE_BYTES);
            let mut markers = self.checkpoint_markers.query_range(search_start, dirty_pos);
            markers.sort_by_key(|(_, start, _)| std::cmp::Reverse(*start));
            let nearest = markers
                .into_iter()
                .find(|(_, start, _)| *start < dirty_pos && is_line_start(buffer, *start));
            if let Some((id, cp_pos, _)) = nearest {
                if let Some(snap) = self.checkpoint_states.get(&id) {
                    (cp_pos, snap.clone())
                } else {
                    return None; // orphan, fall back
                }
            } else if parse_end <= MAX_PARSE_BYTES {
                (0, ParseSnapshot::new(syntax))
            } else {
                return None; // large file, no nearby checkpoint, fall back
            }
        };

        // Every checkpoint the walk passes, for convergence checking and
        // repair. Any between the resume point and the dirty point is a
        // displaced one the search above skipped; the walk drops it.
        let mut markers_ahead: Vec<(MarkerId, usize)> = self
            .checkpoint_markers
            .query_range(actual_start, parse_end)
            .into_iter()
            .map(|(id, start, _)| (id, start))
            .collect();
        markers_ahead.sort_by_key(|(_, pos)| *pos);
        let mut marker_idx = 0;
        let dirty_end = self.dirty_end;

        // Parse from actual_start to parse_end, looking for convergence
        let content_end = parse_end.min(buffer.len());
        if actual_start >= content_end {
            return None;
        }
        let content = buffer.slice_bytes(actual_start..content_end);
        let content_str = match std::str::from_utf8(&content) {
            Ok(s) => s,
            Err(_) => return None,
        };

        let mut new_spans = Vec::new();
        let mut new_lines = Vec::new();
        let buffer_len = buffer.len();
        let content_bytes = content_str.as_bytes();
        let mut pos = 0;
        let mut current_offset = actual_start;
        let mut converged_at: Option<usize> = None;
        let mut budget_hit_at: Option<usize> = None;
        let must_reach = if buffer.len() <= MAX_PARSE_BYTES {
            viewport_end
        } else {
            0
        };
        let mut bytes_since_checkpoint: usize = 0;
        // Spans are collected for everything the splice below will replace:
        // the cached spans from `actual_start` on, within the cache. Not from
        // `desired_parse_start` — when a caller asks to start below the
        // cache's own start (a viewport scrolled down since the cache was
        // built, a structure query at the viewport top), the lines between
        // the anchor and that start would lose their cached spans to the
        // splice and get none back: a hole of plain text above the viewport,
        // served as a cache hit once it scrolls into view.
        let collect_from = self
            .cache
            .as_ref()
            .map_or(desired_parse_start, |cache| cache.range.start)
            .max(actual_start);

        while pos < content_bytes.len() {
            // Create checkpoints in new territory
            if bytes_since_checkpoint >= CHECKPOINT_INTERVAL {
                self.maybe_create_checkpoint(current_offset, &snapshot);
                bytes_since_checkpoint = 0;
            }

            let (line_end, line_byte_len, prepared) = prepare_line_at(content_bytes, pos);
            let collect_spans = current_offset + line_byte_len > collect_from;
            if let Some(prepared) = prepared {
                let _ = self.parse_cached_line(
                    &mut snapshot,
                    &prepared,
                    current_offset,
                    current_offset + line_byte_len,
                    buffer_len,
                    &mut new_lines,
                    |byte_start, byte_end, category, background_category| {
                        if !collect_spans {
                            return;
                        }
                        let clamped_start = byte_start.max(collect_from);
                        if clamped_start < byte_end {
                            new_spans.push(CachedSpan {
                                range: clamped_start..byte_end,
                                category,
                                background_category,
                            });
                        }
                    },
                );
            }

            pos = line_end;
            current_offset += line_byte_len;
            bytes_since_checkpoint += line_byte_len;

            // Check convergence at checkpoint markers. `snapshot` is now the
            // state *after* this line, which is the state *before* the next
            // one — so it is the right state for a checkpoint on this line's
            // end only. A checkpoint sitting on this line's start was already
            // settled by the previous line's pass with the state that belongs
            // to it (see `full_parse` for the same rule); walk past it rather
            // than comparing — or storing — a state one line late. A
            // checkpoint strictly inside this line was pushed there by an
            // insert at its line start, and no state belongs to a mid-line
            // position: drop it rather than compare or store it, so no later
            // pass resumes mid-line. Only a checkpoint past the last edit may
            // end the walk: before it, a matching state says nothing about the
            // cached spans of the edits still ahead.
            let line_start = current_offset - line_byte_len;
            while marker_idx < markers_ahead.len() && markers_ahead[marker_idx].1 <= current_offset
            {
                let (marker_id, marker_pos) = markers_ahead[marker_idx];
                marker_idx += 1;
                if marker_pos <= line_start {
                    continue;
                }
                if marker_pos < current_offset {
                    self.checkpoint_markers.delete(marker_id);
                    self.checkpoint_states.remove(&marker_id);
                    continue;
                }
                if current_offset > dirty_end
                    && self.checkpoint_states.get(&marker_id) == Some(&snapshot)
                {
                    self.stats.convergences += 1;
                    converged_at = Some(current_offset);
                    break;
                }
                self.stats.checkpoints_updated += 1;
                self.checkpoint_states.insert(marker_id, snapshot.clone());
            }

            if converged_at.is_some() {
                break;
            }

            // Bound work per pass: pathological edits (e.g. unclosed `/*`
            // re-scoping the rest of the file) can never converge. Stop here
            // and resume from `current_offset` on the next render; a small
            // file's viewport is always parsed first.
            if current_offset >= must_reach
                && current_offset.saturating_sub(dirty_pos) >= CONVERGENCE_BUDGET
            {
                budget_hit_at = Some(current_offset);
                break;
            }
        }

        self.stats.bytes_parsed += current_offset.saturating_sub(actual_start);

        // Splice classification: converged → clear dirty; budget hit → keep
        // dirty for next pass; EOF → clear dirty.
        let (splice_end, dirty_after) = if let Some(c) = converged_at {
            (c, None)
        } else if let Some(b) = budget_hit_at {
            (b, Some(b))
        } else if self
            .cache
            .as_ref()
            .is_some_and(|c| current_offset < c.range.end)
        {
            // Stopped at `parse_end`; the cached spans past it predate the edit.
            (current_offset, Some(current_offset))
        } else {
            // Reached the cache's end without converging: nothing past here
            // is cached, but checkpoints from an earlier visit may be, and the
            // walk never verified them. Clearing `dirty_from` must not leave
            // them behind (see `drop_checkpoints_from`). A checkpoint exactly
            // at `current_offset` was just given this walk's state.
            self.drop_checkpoints_from(current_offset.saturating_add(1));
            (current_offset, None)
        };

        self.stats.cache_misses += 1; // partial update counts as a miss

        Self::merge_adjacent_spans(&mut new_spans);

        if let Some(cache) = &mut self.cache {
            let splice_start = actual_start;
            let in_order = new_spans
                .windows(2)
                .all(|w| w[0].range.end <= w[1].range.start)
                && new_spans
                    .first()
                    .is_none_or(|s| s.range.start >= splice_start)
                && new_spans.last().is_none_or(|s| s.range.end <= splice_end);
            if in_order {
                // The cache is sorted and disjoint: swap the re-parsed run in
                // where it belongs and merge only at its two seams.
                let lo = cache
                    .spans
                    .partition_point(|span| span.range.end <= splice_start);
                let hi = cache
                    .spans
                    .partition_point(|span| span.range.start < splice_end)
                    .max(lo);
                let inserted = new_spans.len();
                cache.spans.splice(lo..hi, new_spans);
                let seams = lo.saturating_sub(1)..(lo + inserted + 1).min(cache.spans.len());
                let mut window: Vec<CachedSpan> = cache.spans.drain(seams.clone()).collect();
                Self::merge_adjacent_spans(&mut window);
                cache.spans.splice(seams.start..seams.start, window);
            } else {
                cache.spans.retain(|span| {
                    span.range.end <= splice_start || span.range.start >= splice_end
                });
                cache.spans.extend(new_spans);
                cache.spans.sort_by_key(|s| s.range.start);
                Self::merge_adjacent_spans(&mut cache.spans);
            }
            // The re-parsed lines replace every recorded line they overlap.
            // Both runs ascend, so this is one splice; a line the walk did not
            // record (a host parse error) is left absent, never stale.
            let lo = cache.lines.partition_point(|line| line.end <= actual_start);
            let hi = cache
                .lines
                .partition_point(|line| line.start < splice_end)
                .max(lo);
            cache.lines.splice(lo..hi, new_lines);
            if splice_end > cache.range.end {
                cache.range.end = splice_end;
            }
            cache.tail_state = None;
        }

        self.last_buffer_len = buffer.len();
        self.dirty_from = dirty_after;

        Some(self.cached_spans_in(viewport_start, viewport_end))
    }

    /// Give a cache whose end state an edit cleared a new end at its last
    /// checkpoint, so it extends forward instead of re-parsing from the start.
    fn rewind_cache_to_checkpoint(&mut self) {
        let Some(cache) = self.cache.as_mut() else {
            return;
        };
        let last = self
            .checkpoint_markers
            .query_range(cache.range.start, cache.range.end + 1)
            .into_iter()
            .max_by_key(|(_, pos, _)| *pos);
        let Some((id, pos, _)) = last else {
            return;
        };
        let Some(state) = self.checkpoint_states.get(&id) else {
            return;
        };
        cache.spans.retain_mut(|span| {
            span.range.end = span.range.end.min(pos);
            span.range.start < span.range.end
        });
        cache.lines.retain(|line| line.end <= pos);
        cache.range.end = pos;
        cache.tail_state = Some(state.clone());
    }

    /// Forward extension path (see module docs). Caller checks the cache
    /// exists, has a `tail_state`, has no dirty edits, and `cache.range.end
    /// < parse_end`.
    fn extend_cache_forward(
        &mut self,
        buffer: &Buffer,
        parse_end: usize,
        viewport_start: usize,
        viewport_end: usize,
    ) -> Vec<CachedSpan> {
        self.stats.cache_misses += 1;
        let buf_len = buffer.len();
        let parse_end = parse_end.min(buf_len);

        let (extension_start, mut snapshot) = {
            let cache = self
                .cache
                .as_ref()
                .expect("extend_cache_forward: cache must exist");
            let snap = cache
                .tail_state
                .as_ref()
                .expect("extend_cache_forward: tail_state must exist")
                .clone();
            (cache.range.end, snap)
        };

        if parse_end <= extension_start {
            return self.cached_spans_in(viewport_start, viewport_end);
        }

        let content = buffer.slice_bytes(extension_start..parse_end);
        let content_str = match std::str::from_utf8(&content) {
            Ok(s) => s,
            Err(_) => return self.cached_spans_in(viewport_start, viewport_end),
        };

        let mut new_spans = Vec::new();
        let mut new_lines = Vec::new();
        let content_bytes = content_str.as_bytes();
        let mut pos = 0;
        let mut current_offset = extension_start;
        let mut bytes_since_checkpoint: usize = 0;
        // Snapshot of the last newline-aligned cache commit point. We never
        // commit parse state for a partial trailing line: with a streaming
        // grammar like syntect's `Diff` (line-anchored `^\+.*` etc.) the
        // state at end-of-input has already popped `markup.inserted`, so
        // resuming from there parses the rest of the same line in
        // `source.diff` with no scope — bytes of the line streamed in
        // later get default editor bg, producing the dark-bar artifact
        // inside `+` lines. Re-parsing the trailing partial line on every
        // refresh costs at most one extra `parse_line` and is correct.
        let mut safe_offset = extension_start;
        let mut safe_snapshot = snapshot.clone();

        while pos < content_bytes.len() {
            if bytes_since_checkpoint >= CHECKPOINT_INTERVAL {
                self.maybe_create_checkpoint(current_offset, &snapshot);
                bytes_since_checkpoint = 0;
            }

            let (line_end, line_byte_len, prepared) = prepare_line_at(content_bytes, pos);
            let mut newline_terminated = false;
            if let Some(prepared) = prepared {
                let parse_ok = self.parse_cached_line(
                    &mut snapshot,
                    &prepared,
                    current_offset,
                    current_offset + line_byte_len,
                    buf_len,
                    &mut new_lines,
                    |byte_start, byte_end, category, background_category| {
                        new_spans.push(CachedSpan {
                            range: byte_start..byte_end,
                            category,
                            background_category,
                        });
                    },
                );
                if parse_ok {
                    newline_terminated = prepared.ends_with_newline;
                }
            }

            pos = line_end;
            current_offset += line_byte_len;
            bytes_since_checkpoint += line_byte_len;

            if newline_terminated {
                safe_offset = current_offset;
                safe_snapshot = snapshot.clone();
            }
        }

        // The trailing partial line is committable when the parse
        // reached the buffer end: those bytes are final unless the
        // buffer grows, and both cache-consuming paths in
        // `highlight_viewport` require `last_buffer_len == buffer.len()`
        // (plus no pending edit), so a grown buffer can never resume
        // from this state. Without this, a file that is one huge line
        // with no trailing newline (a minified JSON one-liner) can
        // never commit anything and re-parses in full on every frame.
        if parse_end >= buf_len {
            safe_offset = current_offset;
            safe_snapshot = snapshot.clone();
        }

        self.stats.bytes_parsed += parse_end - extension_start;

        Self::merge_adjacent_spans(&mut new_spans);

        // Split spans into safe (fully before the trailing partial line,
        // cacheable) and unsafe (overlap the partial line, render-only).
        // Unsafe spans are returned in this pass so the partial line is
        // still highlighted, but won't be cached — they'll be recomputed
        // on the next refresh once more bytes (and a newline) stream in.
        let (safe_spans, unsafe_spans): (Vec<_>, Vec<_>) = new_spans
            .into_iter()
            .partition(|s| s.range.end <= safe_offset);

        let cache = self
            .cache
            .as_mut()
            .expect("extend_cache_forward: cache must still exist");
        cache.spans.extend(safe_spans);
        Self::merge_adjacent_spans(&mut cache.spans);
        cache
            .lines
            .extend(new_lines.into_iter().filter(|line| line.end <= safe_offset));
        cache.range.end = safe_offset;
        cache.tail_state = Some(safe_snapshot);
        self.last_buffer_len = buf_len;

        let mut result = self.cached_spans_in(viewport_start, viewport_end);
        result.extend(
            unsafe_spans
                .into_iter()
                .filter(|s| s.range.start < viewport_end && s.range.end > viewport_start),
        );
        result
    }

    /// Full re-parse from desired_parse_start to parse_end. Used on cold start
    /// or when partial update fails (no convergence).
    #[allow(clippy::too_many_arguments)]
    fn full_parse(
        &mut self,
        buffer: &Buffer,
        desired_parse_start: usize,
        parse_end: usize,
        viewport_start: usize,
        viewport_end: usize,
        pending_edit: Option<usize>,
    ) -> Vec<CachedSpan> {
        self.stats.cache_misses += 1;
        // This pass clears `dirty_from`, so the checkpoints an unrepaired
        // edit left stale go first (see `drop_checkpoints_from`): resuming
        // from one would parse the text as it was before the edit. The resume
        // point is then before the edit — and the parse runs through it,
        // re-laying checkpoints — or, in a large file with none in reach, a
        // fresh start, which reports itself unanchored. One exactly at the
        // edit goes too: the same "strictly before the edit" rule
        // `try_partial_update` resumes by.
        if let Some(edit) = pending_edit {
            self.drop_checkpoints_from(edit);
        }
        self.dirty_from = None; // consumed

        if parse_end <= desired_parse_start {
            return Vec::new();
        }

        let syntax = &self.syntax_set.syntaxes()[self.syntax_index];
        let (actual_start, mut snapshot, anchored) =
            self.find_parse_resume_point(desired_parse_start, parse_end, syntax);

        let content = buffer.slice_bytes(actual_start..parse_end);
        let content_str = match std::str::from_utf8(&content) {
            Ok(s) => s,
            Err(_) => return Vec::new(),
        };

        // A fresh start inside a line — a large file with no anchor near the
        // viewport — does not know how far into the line it is. With a line
        // start within the highlighted prefix's length behind it, the line is
        // highlighted from here (from a fresh state, so approximately); with
        // none, this is past the line's cut, and the line is left plain like
        // the rest of every long line. A bounded look back, not a scan.
        let starts_line = is_line_start(buffer, actual_start);
        let mut past_cut = !starts_line
            && !buffer
                .slice_bytes(actual_start.saturating_sub(MAX_HIGHLIGHT_LINE_BYTES)..actual_start)
                .contains(&b'\n');

        let mut spans = Vec::new();
        let mut lines = Vec::new();
        let buffer_len = buffer.len();
        let content_bytes = content_str.as_bytes();
        let mut pos = 0;
        let mut current_offset = actual_start;
        let mut bytes_since_checkpoint: usize = 0;
        // See `extend_cache_forward` for rationale: never commit cache
        // state past the last newline. `safe_offset` ends up == parse_end
        // for buffers that end on `\n` (no behaviour change), and at the
        // start of the trailing partial line otherwise so the next
        // refresh re-parses it from scratch.
        let mut safe_offset = actual_start;
        let mut safe_snapshot = snapshot.clone();
        // Whether `safe_snapshot` is a state the parse can resume from: one at
        // a line start. A fresh start inside a line is not — its state is a
        // guess for that point, and resuming from it skips the look back above.
        let mut safe_resumable = starts_line;

        while pos < content_bytes.len() {
            if bytes_since_checkpoint >= CHECKPOINT_INTERVAL {
                self.maybe_create_checkpoint(current_offset, &snapshot);
                bytes_since_checkpoint = 0;
            }

            let (line_end, line_byte_len, prepared) = prepare_line_at(content_bytes, pos);
            // Skip span collection for lines that ended before the viewport's
            // desired_parse_start — we still need to drive `parse_line` for
            // state continuity, but their spans wouldn't be returned anyway.
            let collect_spans = current_offset + line_byte_len > desired_parse_start;
            let mut newline_terminated = false;
            if std::mem::take(&mut past_cut) {
                newline_terminated = prepared.is_some_and(|p| p.ends_with_newline);
            } else if let Some(prepared) = prepared {
                let parse_ok = self.parse_cached_line(
                    &mut snapshot,
                    &prepared,
                    current_offset,
                    current_offset + line_byte_len,
                    buffer_len,
                    &mut lines,
                    |byte_start, byte_end, category, background_category| {
                        if !collect_spans {
                            return;
                        }
                        let clamped_start = byte_start.max(desired_parse_start);
                        if clamped_start < byte_end {
                            spans.push(CachedSpan {
                                range: clamped_start..byte_end,
                                category,
                                background_category,
                            });
                        }
                    },
                );
                if parse_ok {
                    newline_terminated = prepared.ends_with_newline;
                }
            }

            pos = line_end;
            current_offset += line_byte_len;
            bytes_since_checkpoint += line_byte_len;

            if newline_terminated {
                safe_offset = current_offset;
                safe_snapshot = snapshot.clone();
                safe_resumable = true;
            }

            // Update checkpoint states as we pass them. A checkpoint at
            // position `p` stores the state *before* `p` — that is the
            // contract every resume path relies on
            // (`find_parse_resume_point`, `try_partial_update` both parse
            // the line at `p` starting from it), so the state to store is the
            // one that reflects
            // everything up to but not including `p`.
            //
            // The range is therefore half-open at the *low* end
            // (`query_range` is inclusive both ways): a checkpoint sitting on
            // this line's start must keep the state it was given at the end of
            // the previous line, not the state that includes this line. Giving
            // it the end-of-line state shifts it one line late, and resuming
            // from it re-parses the line with the state that follows it — for
            // Markdown that inverts fence parity at a delimiter line, so every
            // prose line below a code block reads as fence body until the next
            // delimiter (issue: prose framed as code after scrolling into a
            // file mid-document). A checkpoint strictly inside this line was
            // displaced there by an insert at its line start; no state belongs
            // to it, so drop it (see `try_partial_update`).
            let markers_here: Vec<(MarkerId, usize)> = self
                .checkpoint_markers
                .query_range(
                    current_offset
                        .saturating_sub(line_byte_len)
                        .saturating_add(1),
                    current_offset,
                )
                .into_iter()
                .map(|(id, start, _)| (id, start))
                .collect();
            for (marker_id, marker_pos) in markers_here {
                if marker_pos < current_offset {
                    self.checkpoint_markers.delete(marker_id);
                    self.checkpoint_states.remove(&marker_id);
                } else {
                    self.checkpoint_states.insert(marker_id, snapshot.clone());
                }
            }
        }

        // Same EOF rule as `extend_cache_forward`: a trailing partial
        // line that ends at the buffer end is committable, because the
        // cache-consuming paths require an unchanged buffer length.
        if parse_end >= buffer.len() {
            safe_offset = current_offset;
            safe_snapshot = snapshot.clone();
            safe_resumable = true;
        }

        self.stats.bytes_parsed += parse_end.saturating_sub(actual_start);

        Self::merge_adjacent_spans(&mut spans);

        // Cache only the prefix up to the last newline. Spans straddling
        // or past the trailing partial line are returned for THIS render
        // pass (so the partial line is highlighted now), but excluded
        // from the cache — the next refresh re-parses them from
        // `safe_state` once more bytes have streamed in.
        let cache_range_end = safe_offset.max(desired_parse_start);
        let cached_spans: Vec<CachedSpan> = spans
            .iter()
            .filter(|s| s.range.end <= cache_range_end)
            .cloned()
            .collect();

        // Lines are kept from the parse's own start, not the cache's: a line
        // above `desired_parse_start` has no cached spans but its structure is
        // as sound as the rest, and the neighbour a lookup at the cache's top
        // line needs.
        lines.retain(|line| line.end <= cache_range_end);

        self.cache = Some(TextMateCache {
            range: desired_parse_start..cache_range_end,
            spans: cached_spans,
            lines,
            anchored,
            // The state belongs to `safe_offset`. A pass that ended inside a
            // line has none for the cache's end, and one that started inside a
            // line has only a guess: extending from either would resume the
            // line mid-way and count its cut from the wrong place.
            tail_state: (safe_resumable && safe_offset >= desired_parse_start)
                .then_some(safe_snapshot),
        });
        self.last_buffer_len = buffer.len();

        spans
            .into_iter()
            .filter(|span| span.range.start < viewport_end && span.range.end > viewport_start)
            .collect()
    }

    /// Find the best point to resume parsing from for the viewport.
    fn find_parse_resume_point(
        &self,
        desired_start: usize,
        parse_end: usize,
        syntax: &syntect::parsing::SyntaxReference,
    ) -> (usize, ParseSnapshot, bool) {
        // Returns (start, state, anchored): anchored when the state is one
        // the parse can vouch for — a checkpoint's, or the initial state at
        // byte 0 — rather than a fresh state guessed mid-file.
        // Look for a checkpoint near the desired start. For large files, only
        // consider checkpoints that are within MAX_PARSE_BYTES of desired_start
        // to avoid parsing hundreds of MB from a distant checkpoint.
        let search_start = desired_start.saturating_sub(MAX_PARSE_BYTES);
        let markers = self
            .checkpoint_markers
            .query_range(search_start, desired_start + 1);
        let nearest = markers.into_iter().max_by_key(|(_, start, _)| *start);

        if let Some((id, cp_pos, _)) = nearest {
            if let Some(snap) = self.checkpoint_states.get(&id) {
                return (cp_pos, snap.clone(), true);
            }
        }

        if parse_end <= MAX_PARSE_BYTES {
            // File is small enough to parse from byte 0
            (0, ParseSnapshot::new(syntax), true)
        } else {
            // Large file, no nearby checkpoint — start fresh from desired_start.
            // Still create checkpoints so future visits to this region can resume.
            (
                desired_start,
                ParseSnapshot::new(syntax),
                desired_start == 0,
            )
        }
    }

    /// Map scope stack to highlight category, memoising per-scope lookups.
    /// `scope.build_string()` is the costly step; the cache hides it after
    /// each scope atom has been seen once.
    fn scope_stack_to_category(
        &mut self,
        scopes: &syntect::parsing::ScopeStack,
    ) -> Option<HighlightCategory> {
        for scope in scopes.as_slice().iter().rev() {
            let cat = match self.scope_category_cache.get(scope) {
                Some(c) => *c,
                None => {
                    let computed = scope_to_category(&scope.build_string());
                    self.scope_category_cache.insert(*scope, computed);
                    computed
                }
            };
            if let Some(c) = cat {
                return Some(c);
            }
        }
        None
    }

    /// Classify every line start in `range` by its role in an
    /// embedded-language region — `(line_start_byte, RegionLine)` pairs, in
    /// increasing order. Lines outside any region are omitted.
    ///
    /// Thin projection of [`Self::structure_lines_in`]; see that method for
    /// the mechanism, cost and the meaning of an empty answer.
    pub fn region_lines_in(
        &mut self,
        buffer: &Buffer,
        range: Range<usize>,
    ) -> Vec<(usize, RegionLine)> {
        self.structure_lines_in(buffer, range).regions
    }

    /// Classify every line start in `range` by its role in the structures
    /// the host grammar recognizes: embedded-language regions
    /// (`markup.raw.code-fence`, Vue `<script>`) and tables. Both lists are
    /// in increasing order of line start; lines with no role are omitted.
    ///
    /// This exposes, read-only, classifications the parse already makes per
    /// line — for regions the `region scope before → after` table in
    /// `docs/internal/embedded-language-highlighting.md`, for tables the same
    /// before/after test against the table body scope. It exists because
    /// those are the properties of a Markdown line that a decoration plugin
    /// cannot work out for itself: region membership is not derivable from a
    /// line's own text at all (a bare ``` opens or closes depending on every
    /// fence above it), and while "is this a table row" *is* line-local,
    /// "is this the table's first/last row" is not — the plugin thread sees
    /// one `lines_changed` batch and cannot read the buffer around it
    /// synchronously.
    ///
    /// A lookup, not a parse of its own. The highlight cache records every
    /// line's scope transitions as it parses (`TextMateCache::lines`), so this
    /// brings that cache up to date through the same dispatcher highlighting
    /// uses (`sync_cache`) — to one line past `range.end`, the line that tells
    /// whether the range's last table row is the table's last — and reads the
    /// answer off it. Whatever that costs, the frame's highlight pass would
    /// have paid anyway, and then finds a cache hit; on a scroll it is the few
    /// lines that came into view. It used to be a separate, uncached walk from
    /// the nearest checkpoint through the range plus 8 KB, repeated on every
    /// frame that offered new lines, and the bundled Markdown grammar
    /// backtracks exponentially on a line of several `code` spans (~100 ms
    /// for one such line) — so a prose-heavy document re-paid that on every
    /// scroll step.
    ///
    /// Cost: zero for the overwhelming majority of buffers (neither
    /// `embedding` nor `table_body_scope` is set unless the syntax hosts
    /// these — currently only Markdown and Vue).
    ///
    /// Returns **empty** — not "no structures" — whenever the cache cannot
    /// vouch for every line the answer depends on: the parse that built it
    /// started from a guessed state in the middle of a large file, a line in
    /// the range was not committed (a host parse error, a streaming tail), or
    /// an edit past the range has not been repaired yet. That is the same
    /// cold-start trade-off the engine already documents for styling inside a
    /// huge region, and it is why callers must treat an absent answer as
    /// "unknown".
    pub fn structure_lines_in(&mut self, buffer: &Buffer, range: Range<usize>) -> StructureLines {
        if !self.hosts_structure() || range.start >= range.end {
            return StructureLines::default();
        }
        let buffer_len = buffer.len();
        let range = range.start..range.end.min(buffer_len);
        if range.start >= range.end {
            return StructureLines::default();
        }
        // One line past `range.end`, so the table's last row inside the range
        // can be recognized as last: that is the one fact about a table line
        // which needs the line *after* it. Found within a bounded look ahead;
        // a line longer than that is left out, and the row above it simply
        // isn't called last.
        let need_end = if self.table_body_scope.is_some() {
            let probe_end = range
                .end
                .saturating_add(TABLE_LOOKAHEAD_BYTES)
                .min(buffer_len);
            match buffer
                .slice_bytes(range.end..probe_end)
                .iter()
                .position(|&b| b == b'\n')
            {
                Some(i) => range.end + i + 1,
                None if probe_end == buffer_len => buffer_len,
                None => range.end,
            }
        } else {
            range.end
        };

        // The spans this returns are the highlighter's business; only the
        // cache it leaves behind matters here.
        let _ = self.sync_cache(buffer, range.start, need_end, 0);
        self.structure_from_cache(&range, need_end, buffer_len)
            .unwrap_or_default()
    }

    /// Read `range`'s structure off the cache's recorded lines, or `None` when
    /// the cache cannot vouch for every line it needs: the lines of `range`
    /// and those up to `need_end` (whether its last row closes the table).
    /// The lines must be recorded contiguously — a missing one is unknown,
    /// not "no structure" — and none may sit at or past an unrepaired edit.
    ///
    /// The line *before* `range` is read when it is recorded, and only
    /// settles whether the range's first line is a table's first row. When it
    /// is not — the parse began right at `range.start`, as one does at a
    /// checkpoint after a jump in a large file — that one flag stays false,
    /// which is what the old probe resuming from the same checkpoint said.
    fn structure_from_cache(
        &self,
        range: &Range<usize>,
        need_end: usize,
        buffer_len: usize,
    ) -> Option<StructureLines> {
        let cache = self.cache.as_ref()?;
        if !cache.anchored || cache.range.end < need_end {
            return None;
        }
        if self.dirty_from.is_some_and(|dirty| dirty < need_end) {
            return None;
        }
        let lines = &cache.lines;
        // The line holding `range.start`, and the one before it when it is
        // recorded (see above for when it is not).
        let at = lines.partition_point(|line| line.end <= range.start);
        let first = lines.get(at).filter(|line| line.start <= range.start)?;
        let from = match at.checked_sub(1) {
            Some(prev) if lines[prev].end == first.start => prev,
            _ => at,
        };

        let table_hosted = self.table_body_scope.is_some();
        let mut out = StructureLines::default();
        // The table walk is one line behind the region walk: the *header* row
        // is the line before the delimiter row, and the *last* row is the line
        // before the table's body scope drops. Neither is knowable when that
        // line is read, so both are settled retroactively — the header by
        // holding on to the previous line's start, the last row by patching the
        // entry already pushed.
        let mut prev: Option<&LineStructure> = None;
        // Start of the most recent line the walk saw *inside* a table, whether
        // or not it fell in the caller's range. `mark_table_line_last` matches
        // on it, so a table continuing past `range.end` never closes early.
        let mut last_table_line_start: Option<usize> = None;
        let mut after_delimiter = false;
        for line in &lines[from..] {
            if prev.is_some_and(|p| p.end != line.start) {
                return None;
            }
            let in_range = line.start >= range.start && line.start < range.end;
            let role = match (line.region_before, line.region_after) {
                (false, true) => Some(RegionLine::Open),
                (true, true) => Some(RegionLine::Body),
                (true, false) => Some(RegionLine::Close),
                (false, false) => None,
            };
            if let (Some(role), true) = (role, in_range) {
                out.regions.push((line.start, role));
            }
            if table_hosted {
                match (line.table_before, line.table_after) {
                    // The body scope opens on the delimiter row, so the line
                    // above it is the header — emitted now, and still in
                    // ascending order because it precedes this one.
                    (false, true) => {
                        if let Some(header) = prev {
                            out.push_table(header.start, TableRole::Header, false, range);
                        }
                        out.push_table(line.start, TableRole::Delimiter, false, range);
                        last_table_line_start = Some(line.start);
                        after_delimiter = true;
                    }
                    (true, true) => {
                        out.push_table(line.start, TableRole::Row, after_delimiter, range);
                        last_table_line_start = Some(line.start);
                        after_delimiter = false;
                    }
                    // The table ended above this line, so the previous one
                    // was its last. Every recorded line is whole, so the
                    // transition is final.
                    (true, false) => {
                        out.mark_table_line_last(last_table_line_start);
                        after_delimiter = false;
                    }
                    (false, false) => after_delimiter = false,
                }
            }
            prev = Some(line);
            if line.end >= need_end {
                break;
            }
        }
        let last = prev?;
        if last.end < need_end {
            return None;
        }
        // A table that runs to the end of the buffer has no line below it to
        // close it, so the transition above never fires. The buffer's own end
        // is that closing edge.
        if last.table_after && last.end >= buffer_len {
            out.mark_table_line_last(last_table_line_start);
        }
        Some(out)
    }

    /// Merge adjacent spans with the same foreground and background categories.
    fn merge_adjacent_spans(spans: &mut Vec<CachedSpan>) {
        if spans.len() < 2 {
            return;
        }

        let mut write_idx = 0;
        for read_idx in 1..spans.len() {
            if spans[write_idx].category == spans[read_idx].category
                && spans[write_idx].background_category == spans[read_idx].background_category
                && spans[write_idx].range.end == spans[read_idx].range.start
            {
                spans[write_idx].range.end = spans[read_idx].range.end;
            } else {
                write_idx += 1;
                if write_idx != read_idx {
                    spans[write_idx] = spans[read_idx].clone();
                }
            }
        }
        spans.truncate(write_idx + 1);
    }

    /// Invalidate span cache for an edited range.
    /// Checkpoint positions are handled by notify_insert/notify_delete.
    /// The span cache is NOT cleared here — it will be patched (partial update)
    /// during the next highlight_viewport call using convergence. Only dirty_from
    /// (set by notify_insert/notify_delete) controls re-parsing scope.
    pub fn invalidate_range(&mut self, _edit_range: Range<usize>) {
        // Intentionally does NOT clear self.cache.
        // The cache will be partially updated in highlight_viewport when
        // dirty_from is set. This avoids full re-parses for small edits.
    }

    /// Invalidate all cache and checkpoints (file reload, language change, etc.)
    pub fn invalidate_all(&mut self) {
        self.cache = None;
        let ids: Vec<MarkerId> = self.checkpoint_states.keys().copied().collect();
        for id in ids {
            self.checkpoint_markers.delete(id);
        }
        self.checkpoint_states.clear();
        self.dirty_from = None;
    }

    /// Get the highlight category at a byte position from the cache.
    ///
    /// Returns the category if the position falls within a cached highlight span.
    /// The position must be within the last highlighted viewport range for a result.
    pub fn category_at_position(&self, position: usize) -> Option<HighlightCategory> {
        let cache = self.cache.as_ref()?;
        // Spans are sorted by start and disjoint; the cache can hold a whole
        // file's worth, and the indent rules ask on every keystroke.
        let after = cache.spans.partition_point(|s| s.range.start <= position);
        let span = cache.spans.get(after.checked_sub(1)?)?;
        (position < span.range.end).then_some(span.category)
    }

    /// Get syntax name
    pub fn syntax_name(&self) -> &str {
        &self.syntax_set.syntaxes()[self.syntax_index].name
    }
}

impl HighlightEngine {
    /// Build a highlighting engine for a catalog entry.
    ///
    /// Single chokepoint for the "prefer syntect, fall back to tree-sitter"
    /// logic. Callers that start from a path or a syntax name should resolve
    /// the entry through `GrammarRegistry::find_by_path` / `find_by_name` and
    /// then call this.
    pub fn from_entry(
        entry: &crate::primitives::grammar::GrammarEntry,
        registry: &GrammarRegistry,
    ) -> Self {
        Self::build(entry, entry.engines.syntect, registry)
    }

    /// Like [`Self::from_entry`], but picks the entry's dialect grammar for
    /// `path` when it has one (e.g. TypeScriptReact for `.tsx`).
    pub fn from_entry_for_path(
        entry: &crate::primitives::grammar::GrammarEntry,
        path: &Path,
        registry: &GrammarRegistry,
    ) -> Self {
        Self::build(entry, entry.syntect_for_path(path), registry)
    }

    fn build(
        entry: &crate::primitives::grammar::GrammarEntry,
        syntect: Option<usize>,
        registry: &GrammarRegistry,
    ) -> Self {
        let syntax_set = registry.syntax_set_arc();
        if let Some(index) = syntect {
            return Self::TextMate(Box::new(TextMateEngine::with_language(
                syntax_set,
                index,
                entry.engines.tree_sitter,
            )));
        }
        if let Some(lang) = entry.engines.tree_sitter {
            if let Ok(highlighter) = Highlighter::new(lang) {
                return Self::TreeSitter(Box::new(highlighter));
            }
        }
        Self::None
    }

    /// Create a highlighting engine for a file.
    ///
    /// Thin wrapper around `from_entry` that resolves the path via the catalog.
    /// User-config-declared filename/extension mappings are honoured as long as
    /// `GrammarRegistry::apply_language_config` has been called on the registry.
    /// `first_line` is used for shebang / first-line regex fallback — pass
    /// `None` when no content is available.
    pub fn for_file(path: &Path, first_line: Option<&str>, registry: &GrammarRegistry) -> Self {
        if let Some(entry) = registry.find_by_path(path, first_line) {
            return Self::from_entry_for_path(entry, path, registry);
        }
        Self::None
    }

    /// Highlight the visible viewport
    ///
    /// `context_bytes` controls how far before/after the viewport to parse for accurate
    /// highlighting of multi-line constructs (strings, comments, nested blocks).
    pub fn highlight_viewport(
        &mut self,
        buffer: &Buffer,
        viewport_start: usize,
        viewport_end: usize,
        theme: &Theme,
        context_bytes: usize,
    ) -> Vec<HighlightSpan> {
        match self {
            Self::TreeSitter(h) => {
                h.highlight_viewport(buffer, viewport_start, viewport_end, theme, context_bytes)
            }
            Self::TextMate(h) => {
                h.highlight_viewport(buffer, viewport_start, viewport_end, theme, context_bytes)
            }
            Self::None => Vec::new(),
        }
    }

    /// Embedded-language region roles for the line starts in `range`; see
    /// `TextMateEngine::region_lines_in`. Empty for engines that have no
    /// notion of embedded regions — the tree-sitter backend has injections
    /// disabled, so it hosts none.
    pub fn region_lines_in(
        &mut self,
        buffer: &Buffer,
        range: Range<usize>,
    ) -> Vec<(usize, RegionLine)> {
        match self {
            Self::TextMate(h) => h.region_lines_in(buffer, range),
            Self::TreeSitter(_) | Self::None => Vec::new(),
        }
    }

    /// Region *and* table roles for the line starts in `range`, from one
    /// walk; see `TextMateEngine::structure_lines_in`. Empty for engines
    /// with no notion of either.
    pub fn structure_lines_in(&mut self, buffer: &Buffer, range: Range<usize>) -> StructureLines {
        match self {
            Self::TextMate(h) => h.structure_lines_in(buffer, range),
            Self::TreeSitter(_) | Self::None => StructureLines::default(),
        }
    }

    /// Notify the highlighting engine of a buffer insert (for checkpoint position tracking).
    pub fn notify_insert(&mut self, position: usize, length: usize) {
        if let Self::TextMate(h) = self {
            h.notify_insert(position, length);
        }
    }

    /// Notify the highlighting engine of a buffer delete (for checkpoint position tracking).
    pub fn notify_delete(&mut self, position: usize, length: usize) {
        if let Self::TextMate(h) = self {
            h.notify_delete(position, length);
        }
    }

    /// Invalidate cache for an edited range
    pub fn invalidate_range(&mut self, edit_range: Range<usize>) {
        match self {
            Self::TreeSitter(h) => h.invalidate_range(edit_range),
            Self::TextMate(h) => h.invalidate_range(edit_range),
            Self::None => {}
        }
    }

    /// Invalidate entire cache
    pub fn invalidate_all(&mut self) {
        match self {
            Self::TreeSitter(h) => h.invalidate_all(),
            Self::TextMate(h) => h.invalidate_all(),
            Self::None => {}
        }
    }

    /// Track a sequence of bulk edits in the cache.
    ///
    /// Each edit is `(pos, del_len, ins_len)`. The slice must be sorted in
    /// descending position order — the same order `apply_bulk_edits` uses to
    /// mutate the buffer — so positions remain valid as the buffer changes.
    ///
    /// This mirrors the `notify_*` + `invalidate_range` pattern used by
    /// single-edit paths. It preserves the TextMate engine's checkpoints and
    /// dirty-from anchor (so the next render uses the partial-update path
    /// rather than a cold reparse from byte zero) and drops the tree-sitter
    /// viewport cache only when an edit overlaps it.
    pub fn notify_edits(&mut self, edits: &[(usize, usize, usize)]) {
        for &(pos, del_len, ins_len) in edits {
            if del_len > 0 {
                self.notify_delete(pos, del_len);
            }
            if ins_len > 0 {
                self.notify_insert(pos, ins_len);
            }
            let edit_end = pos + del_len.max(ins_len);
            self.invalidate_range(pos..edit_end);
        }
    }

    /// Check if this engine has highlighting available
    pub fn has_highlighting(&self) -> bool {
        !matches!(self, Self::None)
    }

    /// An engine for a buffer a plugin composed; see
    /// [`TextMateEngine::declared_region_host`].
    pub fn declared_region_host(syntax_set: Arc<SyntaxSet>, regions: Vec<DeclaredRegion>) -> Self {
        Self::TextMate(Box::new(TextMateEngine::declared_region_host(
            syntax_set, regions,
        )))
    }

    /// Whether this engine takes its regions from a plugin rather than a
    /// grammar.
    pub fn hosts_declared_regions(&self) -> bool {
        matches!(self, Self::TextMate(h) if h.hosts_declared_regions())
    }

    /// Replace the regions a plugin declared. A no-op for engines that
    /// take their regions from a grammar.
    pub fn set_declared_regions(&mut self, regions: Vec<DeclaredRegion>) {
        if let Self::TextMate(h) = self {
            if h.hosts_declared_regions() {
                h.set_declared_regions(regions);
            }
        }
    }

    /// The declared regions and their syntax set, taken out of a host
    /// that is about to be replaced so the replacement can carry them;
    /// `None` for every other engine.
    pub fn take_declared_regions(&mut self) -> Option<(Arc<SyntaxSet>, Vec<DeclaredRegion>)> {
        match self {
            Self::TextMate(h) if h.hosts_declared_regions() => Some(h.take_declared_regions()),
            _ => None,
        }
    }

    /// Get a description of the active backend
    pub fn backend_name(&self) -> &str {
        match self {
            Self::TreeSitter(_) => "tree-sitter",
            Self::TextMate(_) => "textmate",
            Self::None => "none",
        }
    }

    /// Get performance stats (TextMate engine only).
    pub fn highlight_stats(&self) -> Option<&HighlightStats> {
        if let Self::TextMate(h) = self {
            Some(h.stats())
        } else {
            None
        }
    }

    /// Reset performance counters.
    pub fn reset_highlight_stats(&mut self) {
        if let Self::TextMate(h) = self {
            h.reset_stats();
        }
    }

    /// Get the language/syntax name if available
    pub fn syntax_name(&self) -> Option<&str> {
        match self {
            Self::TreeSitter(_) => None, // Tree-sitter doesn't expose name easily
            Self::TextMate(h) => Some(h.syntax_name()),
            Self::None => None,
        }
    }

    /// Get the highlight category at a byte position from the cache.
    ///
    /// Returns the category if the position falls within a cached highlight span.
    /// Useful for detecting whether the cursor is inside a string, comment, etc.
    pub fn category_at_position(&self, position: usize) -> Option<HighlightCategory> {
        match self {
            Self::TreeSitter(h) => h.category_at_position(position),
            Self::TextMate(h) => h.category_at_position(position),
            Self::None => None,
        }
    }

    /// Get the tree-sitter Language for non-highlighting features
    /// Returns the language even when using TextMate for highlighting
    pub fn language(&self) -> Option<&Language> {
        match self {
            Self::TreeSitter(h) => Some(h.language()),
            Self::TextMate(h) => h.language(),
            Self::None => None,
        }
    }
}

/// Highlight a code string using syntect (for markdown code blocks, hover popups, etc.)
/// Returns spans with byte ranges relative to the input string.
///
/// This uses TextMate grammars via syntect which provides broader language coverage
/// than tree-sitter (~150+ languages vs ~17).
pub fn highlight_string(
    code: &str,
    lang_hint: &str,
    registry: &GrammarRegistry,
    theme: &Theme,
) -> Vec<HighlightSpan> {
    use syntect::parsing::{ParseState, ScopeStack};

    // Find syntax by language token (handles aliases like "py" -> Python)
    let syntax = match registry.syntax_set().find_syntax_by_token(lang_hint) {
        Some(s) => s,
        None => return Vec::new(),
    };

    let syntax_set = registry.syntax_set();
    let mut state = ParseState::new(syntax);
    let mut spans = Vec::new();
    let mut current_scopes = ScopeStack::new();
    let mut current_offset = 0;

    // Parse line by line
    for line in code.split_inclusive('\n') {
        let line_start = current_offset;
        let line_len = line.len();

        // Remove trailing newline for syntect, then add it back
        let line_content = line.trim_end_matches(&['\r', '\n'][..]);
        let line_for_syntect = if line.ends_with('\n') {
            format!("{}\n", line_content)
        } else {
            line_content.to_string()
        };

        let ops = match state.parse_line(&line_for_syntect, syntax_set) {
            Ok(ops) => ops,
            Err(_) => {
                current_offset += line_len;
                continue;
            }
        };

        let mut syntect_offset = 0;
        let line_content_len = line_content.len();

        for (op_offset, op) in ops {
            let clamped_op_offset = op_offset.min(line_content_len);
            if clamped_op_offset > syntect_offset {
                if let Some(category) = scope_stack_to_category(&current_scopes) {
                    let byte_start = line_start + syntect_offset;
                    let byte_end = line_start + clamped_op_offset;
                    if byte_start < byte_end {
                        spans.push(HighlightSpan {
                            range: byte_start..byte_end,
                            color: highlight_color(category, theme),
                            bg: highlight_bg(category, theme),
                            category: Some(category),
                        });
                    }
                }
            }
            syntect_offset = clamped_op_offset;
            // Scope stack errors are non-fatal for highlighting
            #[allow(clippy::let_underscore_must_use)]
            let _ = current_scopes.apply(&op);
        }

        // Handle remaining text on line
        if syntect_offset < line_content_len {
            if let Some(category) = scope_stack_to_category(&current_scopes) {
                let byte_start = line_start + syntect_offset;
                let byte_end = line_start + line_content_len;
                if byte_start < byte_end {
                    spans.push(HighlightSpan {
                        range: byte_start..byte_end,
                        color: highlight_color(category, theme),
                        bg: highlight_bg(category, theme),
                        category: Some(category),
                    });
                }
            }
        }

        current_offset += line_len;
    }

    // Merge adjacent spans with same color
    merge_adjacent_highlight_spans(&mut spans);

    spans
}

/// Map scope stack to highlight category (for highlight_string)
fn scope_stack_to_category(scopes: &syntect::parsing::ScopeStack) -> Option<HighlightCategory> {
    for scope in scopes.as_slice().iter().rev() {
        let scope_str = scope.build_string();
        if let Some(cat) = scope_to_category(&scope_str) {
            return Some(cat);
        }
    }
    None
}

/// Merge adjacent spans with same color
fn merge_adjacent_highlight_spans(spans: &mut Vec<HighlightSpan>) {
    if spans.len() < 2 {
        return;
    }

    let mut write_idx = 0;
    for read_idx in 1..spans.len() {
        if spans[write_idx].color == spans[read_idx].color
            && spans[write_idx].range.end == spans[read_idx].range.start
        {
            spans[write_idx].range.end = spans[read_idx].range.end;
        } else {
            write_idx += 1;
            if write_idx != read_idx {
                spans[write_idx] = spans[read_idx].clone();
            }
        }
    }
    spans.truncate(write_idx + 1);
}

#[cfg(test)]
mod tests {
    use crate::model::filesystem::StdFileSystem;
    use std::sync::Arc;

    fn test_fs() -> Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> {
        Arc::new(StdFileSystem)
    }
    use super::*;
    use crate::theme;

    #[test]
    fn test_highlight_engine_default() {
        let engine = HighlightEngine::default();
        assert!(!engine.has_highlighting());
        assert_eq!(engine.backend_name(), "none");
    }

    #[test]
    fn test_textmate_backend_selection() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // Languages with TextMate grammars use TextMate for highlighting
        let engine = HighlightEngine::for_file(Path::new("test.rs"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        // Tree-sitter language should still be detected for other features
        assert!(engine.language().is_some());

        let engine = HighlightEngine::for_file(Path::new("test.py"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.language().is_some());

        // JavaScript and TypeScript use Fresh's TypeScript grammar and its
        // derived variants; `.tsx` picks the TypeScriptReact dialect.
        for (file, syntax) in [
            ("test.js", "JavaScript"),
            ("test.jsx", "JavaScript"),
            ("test.ts", "TypeScript"),
            ("test.tsx", "TypeScriptReact"),
        ] {
            let engine = HighlightEngine::for_file(Path::new(file), None, &registry);
            assert_eq!(engine.backend_name(), "textmate", "{file}");
            assert_eq!(engine.syntax_name(), Some(syntax), "{file}");
            assert!(engine.language().is_some(), "{file}");
        }
    }

    #[test]
    fn test_fish_indented_control_keywords_stay_keywords() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("config.fish"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");

        let content =
            "function greet\n    if test -n \"$name\"\n        echo $name\n    end\nend\n";
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        let indented_if = content.find("if test").unwrap();
        let indented_end = content.find("    end").unwrap() + 4;
        let column_zero_end = content.rfind("end").unwrap();

        assert_eq!(
            engine.category_at_position(indented_if),
            Some(HighlightCategory::Keyword)
        );
        assert_eq!(
            engine.category_at_position(indented_end),
            Some(HighlightCategory::Keyword)
        );
        assert_eq!(
            engine.category_at_position(column_zero_end),
            Some(HighlightCategory::Keyword)
        );
    }

    #[test]
    fn test_toml_multiline_array_strings_stay_strings() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("config.toml"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");

        let content = concat!(
            "cmdline = [\n",
            "  \"USER=root\",\n",
            "  \"PATH=/bin:/benchmark\",\n",
            "  \"init=/init\",\n",
            "]\n",
            "initramfs = \"test/initramfs/build/initramfs.cpio.gz\"\n",
            "args = \"$(./tools/qemu_args.sh test)\"\n",
            "package.metadata = { enabled = true, label = \"v1.2\" }\n",
            "released = 2026-08-02\n",
        );
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        for needle in [
            "USER=root",
            "/benchmark",
            "init=/init",
            ".cpio.gz",
            "./tools",
        ] {
            let position = content.find(needle).unwrap();
            assert_eq!(
                engine.category_at_position(position),
                Some(HighlightCategory::String),
                "{needle:?} should be highlighted as part of its TOML string"
            );
        }

        for needle in ["package.metadata", "enabled", "label"] {
            let position = content.find(needle).unwrap();
            assert_eq!(
                engine.category_at_position(position),
                Some(HighlightCategory::Property),
                "{needle:?} should be highlighted as a TOML key"
            );
        }
        assert_eq!(
            engine.category_at_position(content.find("true").unwrap()),
            Some(HighlightCategory::Number)
        );
        assert_eq!(
            engine.category_at_position(content.find("2026-08-02").unwrap()),
            Some(HighlightCategory::Constant)
        );
    }

    /// A comment after a value (`name = "x" # Umbriel's ...`) wasn't
    /// recognised as a comment, so its apostrophe opened a literal string
    /// that never closed and painted the rest of the file as a string.
    /// Regression test for issue #3358.
    #[test]
    fn test_toml_trailing_comment_with_quote_does_not_open_string() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        for first_line in [
            "name = \"umbriel\" # Prefer Umbriel's border-only decoration\n",
            "list = [\"a\", \"b\"] # Umbriel's list\n",
            "a = 1 # say \"hi\n",
            "t = { k = 1 } # Umbriel's table\n",
            // Single-line strings can't span lines: an unterminated one
            // must end at its line, not swallow the rest of the file.
            "broken = 'unterminated\n",
            "broken = \"unterminated\n",
        ] {
            let mut engine = HighlightEngine::for_file(Path::new("config.toml"), None, &registry);
            let content = format!("{first_line}border = true\n\n[colors]\nfg = \"#ffffff\"\n");
            let buffer = Buffer::from_str(&content, 0, test_fs());
            engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

            if let Some(hash) = first_line.find(" # ") {
                assert_eq!(
                    engine.category_at_position(hash + 3),
                    Some(HighlightCategory::Comment),
                    "trailing comment in {first_line:?} should be a comment"
                );
            }
            for (needle, category) in [
                ("\nborder", HighlightCategory::Property),
                ("\nfg", HighlightCategory::Property),
                ("true\n", HighlightCategory::Number),
            ] {
                let position =
                    content.find(needle).unwrap() + usize::from(needle.starts_with('\n'));
                assert_eq!(
                    engine.category_at_position(position),
                    Some(category),
                    "{needle:?} after {first_line:?} should not be inside a string"
                );
            }
            assert_eq!(
                engine.category_at_position(content.find("#ffffff").unwrap()),
                Some(HighlightCategory::String),
                "the string on the last line after {first_line:?} should still be a string"
            );
        }

        // Multi-line strings still span lines.
        let mut engine = HighlightEngine::for_file(Path::new("config.toml"), None, &registry);
        let content = "a = \"\"\"\nfirst # not a comment\n\"\"\"\nb = '''\nsecond\n'''\nc = 1\n";
        let buffer = Buffer::from_str(content, 0, test_fs());
        engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);
        for needle in ["first", "# not", "second"] {
            assert_eq!(
                engine.category_at_position(content.find(needle).unwrap()),
                Some(HighlightCategory::String),
                "{needle:?} is inside a multi-line string"
            );
        }
        assert_eq!(
            engine.category_at_position(content.find("c = 1").unwrap()),
            Some(HighlightCategory::Property)
        );
    }

    /// An unspaced `<` before a string literal used to be read as the start of
    /// a generic argument list, and the recovery rule ate the string's opening
    /// quote on the way out. That inverted quote parity: the string body
    /// rendered as code, the closing `");` opened a string, and every following
    /// line was painted as a string literal until the next `"` flipped it back.
    /// Regression test for issue #3325.
    #[test]
    fn test_rust_unspaced_less_than_before_string_literal() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // `<` and `<=` with no surrounding spaces, plus the `>`/`>=`/spaced
        // forms that always worked and must keep working.
        for line in [
            "fn a() { ensure!(x<M,\"one\"); }\n",
            "fn a() { ensure!(x<=M,\"one\"); }\n",
            "fn d() { plain(x<=M,\"four\"); }\n",
            "fn a() { ensure!(x>M,\"one\"); }\n",
            "fn a() { ensure!(x>=M,\"one\"); }\n",
            "fn a() { ensure!(x <= M,\"one\"); }\n",
        ] {
            let mut engine = HighlightEngine::for_file(Path::new("lib.rs"), None, &registry);
            assert_eq!(engine.backend_name(), "textmate");
            let buffer = Buffer::from_str(line, 0, test_fs());
            let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
            engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

            // The quoted body is a string...
            let body = line.find("one").or_else(|| line.find("four")).unwrap();
            assert_eq!(
                engine.category_at_position(body),
                Some(HighlightCategory::String),
                "string body should be a string literal in {line:?}"
            );
            // ...and the code after the closing quote is not.
            let after = line.rfind("); }").unwrap() + 1;
            assert_ne!(
                engine.category_at_position(after),
                Some(HighlightCategory::String),
                "code after the closing quote must not be a string in {line:?}"
            );
            // The comparison is an operator, not the opening punctuation of a
            // generic argument list. syntect's older bundled grammar scoped the
            // `<` forms as `punctuation.definition.generic.begin`, which maps to
            // no category and rendered as plain text.
            let operator = line.find(['<', '>']).unwrap();
            assert_eq!(
                engine.category_at_position(operator),
                Some(HighlightCategory::Operator),
                "the comparison operator should be an operator in {line:?}"
            );
        }
    }

    /// Issue #3509: `match` and `case` are soft keywords that the Python
    /// grammar syntect bundles predates, so they used to render as plain
    /// identifiers. At the head of a compound statement they must read as
    /// keywords, like the `if`/`else` on the same screen already did.
    #[test]
    fn test_python_match_case_are_keywords() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("m.py"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");

        let content = concat!(
            "def f(x):\n",
            "    match x:\n",
            "        case 1:\n",
            "            return \"one\"\n",
            "        case _:\n",
            "            return \"other\"\n",
            "match command.split():\n",
            "    case [\"go\", direction]:\n",
            "        pass\n",
        );
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        for token in [
            "match x",
            "case 1",
            "case _",
            "match command",
            "case [",
            // Controls: these already worked and must keep working.
            "def",
            "return",
        ] {
            let at = content.find(token).unwrap();
            assert_eq!(
                engine.category_at_position(at),
                Some(HighlightCategory::Keyword),
                "{token:?} should be highlighted as a keyword"
            );
        }
    }

    /// The other half of issue #3509: because `match` and `case` are *soft*
    /// keywords, they are ordinary identifiers everywhere else and must not be
    /// highlighted there.
    #[test]
    fn test_python_match_case_as_identifiers_are_not_keywords() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("m.py"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");

        let content = concat!(
            "import re\n",
            "match = re.match(\"a\", \"b\")\n",
            "case = match\n",
            "match: int = 0\n",
            "if match[0]:\n",
            "    pass\n",
            "def g(match, case):\n",
            "    return match\n",
            // `elif` scopes its keyword without pushing a sub-context, so the
            // compound-statement context is still live for the rest of the
            // line. This shape is real: CPython's `fractions.py` has it.
            "if x:\n",
            "    pass\n",
            "elif match[\"align\"] is not None:\n",
            "    pass\n",
            "elif case != 1:\n",
            "    pass\n",
        );
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        for (token, what) in [
            ("match = ", "assignment target"),
            ("case = ", "assignment target"),
            ("match: int", "annotated name"),
            ("match[0]", "subscripted name"),
            ("match, case", "parameter name"),
            ("match[\"align\"]", "subscript in an elif condition"),
            ("case != 1", "comparison in an elif condition"),
        ] {
            let at = content.find(token).unwrap();
            assert_ne!(
                engine.category_at_position(at),
                Some(HighlightCategory::Keyword),
                "`match`/`case` as a {what} ({token:?}) must not be a keyword"
            );
        }
    }

    /// Issue #3644: the C++ grammar syntect bundles predates `final`,
    /// `requires` and the coroutine keywords, and scoped `static_assert` as an
    /// operator. All four rendered as plain text next to keywords like `class`
    /// and `virtual`.
    #[test]
    fn test_cpp_contextual_keywords_are_keywords() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("a.cpp"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");

        let content = concat!(
            "class Foo final {\n",
            "    virtual void bar() noexcept;\n",
            "};\n",
            "struct Shape final : Base {};\n",
            "template <typename T>\n",
            "requires std::integral<T>\n",
            "T add(T a, T b) { return a + b; }\n",
            "Task g() {\n",
            "    static_assert(sizeof(int) > 0);\n",
            "    co_return 1;\n",
            "    co_await x;\n",
            "    co_yield y;\n",
            "}\n",
        );
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        for token in [
            "final",
            "requires",
            "static_assert",
            "co_return",
            "co_await",
            "co_yield",
            // Controls from the issue: these already worked.
            "class",
            "virtual",
            "noexcept",
            "template",
            "typename",
            "return",
        ] {
            let at = content.find(token).unwrap();
            assert_eq!(
                engine.category_at_position(at),
                Some(HighlightCategory::Keyword),
                "{token:?} should be highlighted as a keyword"
            );
        }
    }

    /// `final` is a contextual keyword, legal as an identifier, so it must stay
    /// plain outside a class or struct head (issue #3644). `requires` is not in
    /// this list: it became a reserved word in C++20, so it is a keyword
    /// wherever it appears.
    #[test]
    fn test_cpp_final_as_identifier_is_not_a_keyword() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("a.cpp"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");

        let content = concat!(
            "int main() {\n",
            "    int final = 0;\n",
            "    return final;\n",
            "}\n",
        );
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        for token in ["final = 0", "final;"] {
            let at = content.find(token).unwrap();
            assert_ne!(
                engine.category_at_position(at),
                Some(HighlightCategory::Keyword),
                "a variable named `final` ({token:?}) must not be a keyword"
            );
        }
    }

    /// The reporter's snippet from issue #3325, end to end: the two
    /// `<=` comparisons must not leak string state into the lines below them.
    #[test]
    fn test_rust_unspaced_less_than_does_not_bleed_across_lines() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("warc.rs"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");

        let content = concat!(
            "use anyhow::{Result, bail, ensure};\n",
            "use std::io::Read;\n",
            "pub const MAX_OUTPUT:usize=256*1024*1024;\n",
            "pub fn zlib(input:&[u8],limit:usize)->Result<Vec<u8>> {\n",
            "    ensure!(limit<=MAX_OUTPUT,\"decompression limit exceeds safety cap\");\n",
            "    let mut output=Vec::new();\n",
            "    flate2::read::ZlibDecoder::new(input).take(limit as u64+1).read_to_end(&mut output)?;\n",
            "    ensure!(output.len()<=limit,\"zlib output exceeds limit {limit}\"); Ok(output)\n",
            "}\n",
        );
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        // Both string bodies are strings.
        for needle in ["decompression limit", "zlib output exceeds"] {
            let position = content.find(needle).unwrap();
            assert_eq!(
                engine.category_at_position(position),
                Some(HighlightCategory::String),
                "{needle:?} should be highlighted as a string literal"
            );
        }

        // The ordinary statements between and after them are not.
        for needle in [
            "let mut output",
            "Vec::new",
            "ZlibDecoder",
            "read_to_end",
            "Ok(output)",
        ] {
            let position = content.find(needle).unwrap();
            assert_ne!(
                engine.category_at_position(position),
                Some(HighlightCategory::String),
                "{needle:?} is ordinary code and must not be highlighted as a string"
            );
        }
    }

    /// A recognized Markdown fence language must be highlighted with that
    /// language's grammar instead of the uniform `markup.raw` string color.
    #[test]
    fn test_markdown_fence_embedded_language_highlighting() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");

        let content = "# Example\n\n```rust\nfn answer() -> u32 { 42 }\n```\n\ntext\n";
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let spans = engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        let category_at = |needle: &str| {
            let position = content.find(needle).unwrap();
            spans
                .iter()
                .find(|span| span.range.start <= position && position < span.range.end)
                .and_then(|span| span.category)
        };

        assert_eq!(
            category_at("fn answer"),
            Some(HighlightCategory::Keyword),
            "rust keyword inside the fence must use the rust grammar"
        );
        assert_eq!(category_at("42"), Some(HighlightCategory::Number));
        // The fence delimiter lines still belong to the host grammar.
        assert_eq!(category_at("```rust"), Some(HighlightCategory::String));
        assert_eq!(
            engine.category_at_position(content.find("fn answer").unwrap()),
            Some(HighlightCategory::Keyword),
            "category lookups must agree with the returned spans"
        );
    }

    /// `region_lines_in` classifies each fence line the way the parse does:
    /// delimiters as open/close, content as body, everything else omitted —
    /// including for a *bare* fence, whose role is not derivable from the
    /// line's own text.
    #[test]
    fn test_region_lines_classify_markdown_fences() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);

        let content = "# T\n\n```rust\nlet x = 1;\n```\n\nprose\n\n```\nbare\n```\n\nend\n";
        let buffer = Buffer::from_str(content, 0, test_fs());
        let at = |needle: &str| content.find(needle).unwrap();

        let roles = engine.region_lines_in(&buffer, 0..buffer.len());
        assert_eq!(
            roles,
            vec![
                (at("```rust"), RegionLine::Open),
                (at("let x = 1;"), RegionLine::Body),
                (at("```\n\nprose"), RegionLine::Close),
                (at("```\nbare"), RegionLine::Open),
                (at("bare"), RegionLine::Body),
                (at("```\n\nend"), RegionLine::Close),
            ],
            "prose lines are omitted; both delimiters of the bare fence are \
             classified from the parse, not from their own text"
        );
    }

    /// Table classification, from the same walk and the same grammar scopes.
    ///
    /// The header row is the interesting one: the body scope opens on the
    /// *delimiter* row, so the header is identified as the line above it —
    /// which is exactly the fact a consumer holding one `lines_changed` batch
    /// cannot get for itself when that batch starts at the header.
    #[test]
    fn test_structure_lines_classify_markdown_tables() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);

        let content = "intro\n\n| K | V |\n|---|---|\n| a | b |\n| c | d |\n\nafter\n";
        let buffer = Buffer::from_str(content, 0, test_fs());
        let at = |needle: &str| content.find(needle).unwrap();
        let line = |role, first_row, last| TableLine {
            role,
            first_row,
            last,
        };

        let tables = engine.structure_lines_in(&buffer, 0..buffer.len()).tables;
        assert_eq!(
            tables,
            vec![
                (at("| K | V |"), line(TableRole::Header, false, false)),
                (at("|---|---|"), line(TableRole::Delimiter, false, false)),
                (at("| a | b |"), line(TableRole::Row, true, false)),
                (at("| c | d |"), line(TableRole::Row, false, true)),
            ],
            "prose is omitted; the header is the line above the delimiter, and \
             the last row is the one above the line that closes the table"
        );
    }

    /// A table running to the end of the buffer has no line below it, so the
    /// transition that closes one never fires. The buffer's end is that edge.
    #[test]
    fn test_structure_lines_close_a_table_at_end_of_buffer() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);

        let content = "intro\n\n| K | V |\n|---|---|\n| a | b |\n";
        let buffer = Buffer::from_str(content, 0, test_fs());

        let tables = engine.structure_lines_in(&buffer, 0..buffer.len()).tables;
        assert_eq!(
            tables.last().map(|(_, t)| (t.role, t.last)),
            Some((TableRole::Row, true)),
            "the final row of a table at end-of-buffer is its last"
        );
    }

    /// Asked about a range that stops mid-table, the probe must not call the
    /// range's final row the table's last: there is more table below it, and a
    /// consumer would close the frame in the middle. It sees this only because
    /// it walks one line past the range it was asked about.
    #[test]
    fn test_structure_lines_do_not_close_a_table_at_the_range_edge() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);

        let content = "| K | V |\n|---|---|\n| a | b |\n| c | d |\n| e | f |\n\nafter\n";
        let buffer = Buffer::from_str(content, 0, test_fs());
        let cut = content.find("| e | f |").unwrap();

        let tables = engine.structure_lines_in(&buffer, 0..cut).tables;
        assert_eq!(
            tables.last().map(|(p, t)| (*p, t.last)),
            Some((content.find("| c | d |").unwrap(), false)),
            "the last row *inside the range* is not the table's last row"
        );
        assert!(
            tables.iter().all(|(p, _)| *p < cut),
            "the lookahead line is classified but never reported"
        );
    }

    /// Region and table classification come from one walk, and neither
    /// disturbs the other: a fence and a table in the same document are each
    /// reported on their own lines only.
    #[test]
    fn test_structure_lines_report_regions_and_tables_together() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);

        let content = "| K |\n|---|\n| a |\n\n```rust\nlet x = 1;\n```\n\nend\n";
        let buffer = Buffer::from_str(content, 0, test_fs());

        let structure = engine.structure_lines_in(&buffer, 0..buffer.len());
        assert_eq!(structure.tables.len(), 3, "header, delimiter, one row");
        assert_eq!(structure.regions.len(), 3, "open, body, close");
        let table_starts: Vec<usize> = structure.tables.iter().map(|(p, _)| *p).collect();
        assert!(
            structure
                .regions
                .iter()
                .all(|(p, _)| !table_starts.contains(p)),
            "no line is reported as both"
        );
    }

    /// The structure answer is read off the highlight cache's per-line
    /// records, which edits shift and the partial update re-parses alongside
    /// the spans. This holds that bookkeeping to the only standard that
    /// matters: after any mix of edits, highlight passes and queries, the
    /// answer equals what a fresh engine parsing the same text from byte 0
    /// says. Edits are drawn to hit the transitions the records encode —
    /// fences and table rows opened, closed and broken, lines split and
    /// joined — and queries run both before the frame's highlight pass (as
    /// `lines_changed` does) and after it.
    #[test]
    fn test_structure_lines_match_a_cold_parse_through_edits() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);

        let mut content = String::from("# Title\n\nintro with `a`, `b` and **bold**\n\n");
        for i in 0..6 {
            content.push_str(&format!(
                "| K{i} | V |\n|---|---|\n| a | b |\n| c | d |\n\nprose {i}\n\n```rust\nlet x{i} = 1;\nlet y = 2;\n```\n\n"
            ));
        }
        content.push_str("tail\n");
        let mut buffer = Buffer::from_str(&content, 0, test_fs());

        let snippets = [
            "\n",
            "\n\n",
            "```",
            "```js\n",
            "| x |",
            "|---|---|\n",
            "| e | f |\n",
            "`c`, ",
            "words ",
            "|",
            "`",
        ];
        // Deterministic LCG: the sequence is the same on every run, so a
        // failure reproduces.
        let mut seed: u64 = 0x5eed_1234_abcd;
        let mut next = |bound: usize| -> usize {
            seed = seed
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            ((seed >> 33) as usize) % bound.max(1)
        };
        let line_start_at_or_before = |buffer: &Buffer, pos: usize| -> usize {
            let text = buffer.slice_bytes(0..pos);
            text.iter().rposition(|&b| b == b'\n').map_or(0, |i| i + 1)
        };

        for step in 0..300 {
            let len = buffer.len();
            match next(3) {
                0 | 1 => {
                    let at = next(len + 1);
                    let text = snippets[next(snippets.len())];
                    buffer.insert(at, text);
                    engine.notify_insert(at, text.len());
                }
                _ if len > 0 => {
                    let at = next(len);
                    let n = (1 + next(24)).min(len - at);
                    buffer.delete(at..at + n);
                    engine.notify_delete(at, n);
                }
                _ => {}
            }
            let len = buffer.len();
            if len == 0 {
                continue;
            }

            // A query range shaped like a `lines_changed` batch: line-aligned
            // start, a viewport's worth of bytes.
            let start = line_start_at_or_before(&buffer, next(len));
            let end = (start + 1 + next(600)).min(len);
            let query = start..end;

            // Half the time the frame's highlight pass has already run (with
            // the renderer's one-viewport context either side); half the time
            // the query comes first, against an unrepaired edit.
            if next(2) == 0 {
                let vs = start.saturating_sub(end - start);
                let ve = (end + (end - start)).min(len);
                engine.highlight_viewport(&buffer, vs, ve, &theme, 1024);
            }

            let got = engine.structure_lines_in(&buffer, query.clone());
            let mut cold = HighlightEngine::for_file(Path::new("README.md"), None, &registry);
            let want = cold.structure_lines_in(&buffer, query.clone());
            assert_eq!(
                (&got.regions, &got.tables),
                (&want.regions, &want.tables),
                "step {step}: cached structure for {query:?} disagrees with a cold parse \
                 of the same text"
            );
        }
    }

    /// A large file's cache is a window. The `lines_changed` pass asks for
    /// structure from the viewport top *before* the frame's highlight pass,
    /// whose window starts a viewport and some context above it. When that
    /// query repairs a pending edit, the partial update resumes from a
    /// checkpoint above the viewport top and replaces the cached spans from
    /// there — so it must also re-collect spans from there, not from where
    /// the query asked to start. Otherwise the lines just above the viewport
    /// keep no spans, and scrolling up onto them is a cache hit that draws
    /// them as plain text.
    #[test]
    fn test_structure_query_after_edit_keeps_spans_above_the_viewport_in_a_large_file() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);

        let mut content = String::new();
        let mut i = 0;
        while content.len() <= MAX_PARSE_BYTES + 64 * 1024 {
            content.push_str(&format!("Line {i} has **bold** and `code` in it.\n"));
            i += 1;
        }
        let mut buffer = Buffer::from_str(&content, 0, test_fs());
        let line_starts: Vec<usize> = std::iter::once(0)
            .chain(content.match_indices('\n').map(|(i, _)| i + 1))
            .collect();
        // What the renderer asks for: a viewport, widened by its own size on
        // either side, plus the context bytes.
        let frame = |engine: &mut HighlightEngine, buffer: &Buffer, top: usize, bottom: usize| {
            let size = bottom - top;
            engine.highlight_viewport(
                buffer,
                top.saturating_sub(size),
                (bottom + size).min(buffer.len()),
                &theme,
                1024,
            );
        };

        // Scroll to a viewport deep in the file, one frame at a time.
        let rows = 30;
        let mut top_line = 20_000;
        for _ in 0..30 {
            frame(
                &mut engine,
                &buffer,
                line_starts[top_line],
                line_starts[top_line + rows],
            );
            top_line += 1;
        }
        // Type on the viewport's second line. The next frame's structure
        // query runs before its highlight pass, as `lines_changed` does.
        let at = line_starts[top_line + 1] + 4;
        buffer.insert(at, "x");
        engine.notify_insert(at, 1);
        let top = line_starts[top_line];
        let bottom = line_starts[top_line + rows] + 1;
        engine.structure_lines_in(&buffer, top..bottom);
        frame(&mut engine, &buffer, top, bottom);

        // Scroll up five lines: those lines must still be highlighted.
        let up = line_starts[top_line - 5];
        let spans = engine.highlight_viewport(&buffer, up, top, &theme, 1024);
        let mut cold = HighlightEngine::for_file(Path::new("README.md"), None, &registry);
        let want = cold.highlight_viewport(&buffer, up, top, &theme, 1024);
        let ranges = |spans: &[HighlightSpan]| -> Vec<Range<usize>> {
            spans
                .iter()
                .filter(|s| s.range.start >= up && s.range.end <= top)
                .map(|s| s.range.clone())
                .collect()
        };
        assert!(
            !ranges(&want).is_empty(),
            "precondition: those lines have spans"
        );
        assert_eq!(
            ranges(&spans),
            ranges(&want),
            "the lines just above the edited viewport lost their highlighting"
        );
    }

    /// After a jump in a large file the structure query is the first to
    /// parse, and it resumes from the checkpoint at or before the viewport
    /// top — often exactly on it, every `CHECKPOINT_INTERVAL` bytes. The line
    /// above the viewport is then never parsed, which must not turn the whole
    /// viewport's answer into "unknown": that line only settles whether the
    /// first line is a table's first row.
    #[test]
    fn test_structure_lines_answer_after_a_jump_wherever_the_checkpoint_lands() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);

        let mut content = String::from("intro\n\n```text\n");
        while content.len() <= MAX_PARSE_BYTES + 128 * 1024 {
            content.push_str("fenced body line\n");
        }
        content.push_str("```\n\nafter\n");
        let buffer = Buffer::from_str(&content, 0, test_fs());

        // Read down the file from the top, so checkpoints exist all along it
        // and every one of them was parsed from byte 0.
        let step = 32 * 1024;
        let mut top = 0;
        while top + step < MAX_PARSE_BYTES + 64 * 1024 {
            engine.highlight_viewport(&buffer, top, top + step, &theme, 1024);
            top += step;
        }
        let target = content[..MAX_PARSE_BYTES / 2].rfind('\n').unwrap() + 1;

        // Whichever line the viewport lands on — on a checkpoint or not —
        // its lines are fence body. Each query follows a jump away, so it
        // is the first parse there.
        for (offset, _) in content[target..target + 600].match_indices('\n') {
            let start = target + offset + 1;
            // Away to the end of the file: outside the window the walk down
            // cached, so the cache moves there and the query below finds
            // nothing cached at `start`.
            let len = buffer.len();
            engine.highlight_viewport(&buffer, len - 4096, len, &theme, 1024);
            let regions = engine.region_lines_in(&buffer, start..start + 2000);
            assert!(
                !regions.is_empty() && regions.iter().all(|(_, r)| *r == RegionLine::Body),
                "viewport at byte {start} inside the fence: got {} regions",
                regions.len()
            );
        }
    }

    /// An edit whose effect never converges — deleting the fence that every
    /// line below it was inside — repaired by a pass that stops at the cache's
    /// end. Checkpoints past that end, laid down by an earlier visit, still
    /// hold the pre-edit "inside the fence" state. With the edit then counted
    /// as repaired, a later visit resumed from one and coloured and framed
    /// the prose below as fence body. Both must match a parse of the text as
    /// it now is.
    #[test]
    fn test_checkpoints_past_a_repaired_edit_never_carry_pre_edit_state() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);

        // A Rust fence that is never closed: every line below it is Rust.
        let mut content = String::from(
            "```rust
",
        );
        // Long enough that its end is out of a checkpoint's reach from where the
        // read below stops, so going there is a cheap fresh start.
        while content.len() <= MAX_PARSE_BYTES / 4 + MAX_PARSE_BYTES + 128 * 1024 {
            content.push_str("let value = call(1, \"two\");\n");
        }
        let mut buffer = Buffer::from_str(&content, 0, test_fs());
        // Read a quarter of the way down, laying checkpoints all along.
        let step = 32 * 1024;
        let mut top = 0;
        while top < MAX_PARSE_BYTES / 4 + step {
            engine.highlight_viewport(&buffer, top, top + step, &theme, 1024);
            top += step;
        }
        // Back to the top by way of the end, so the cache there is a small
        // window rather than the one the read built.
        let len = buffer.len();
        engine.highlight_viewport(&buffer, len - 4096, len, &theme, 1024);
        engine.highlight_viewport(&buffer, 0, 4096, &theme, 1024);

        // Delete the fence and let a pass repair the edit in that window.
        buffer.delete(0..8);
        engine.notify_delete(0, 8);
        engine.highlight_viewport(&buffer, 0, 4096, &theme, 1024);

        // Jump back down there: no fence, so no region, and the lines are
        // coloured as the prose they now are.
        let middle = content[..MAX_PARSE_BYTES / 4].rfind('\n').unwrap() + 1 - 8;
        let window = middle..middle + 2000;
        let regions = engine.region_lines_in(&buffer, window.clone());
        assert!(
            regions.is_empty(),
            "the fence is gone; got {} lines of {:?}",
            regions.len(),
            regions.first().map(|r| r.1)
        );
        let spans = engine.highlight_viewport(&buffer, window.start, window.end, &theme, 0);
        let mut cold = HighlightEngine::for_file(Path::new("README.md"), None, &registry);
        let want = cold.highlight_viewport(&buffer, window.start, window.end, &theme, 0);
        let cats = |spans: &[HighlightSpan]| -> Vec<(Range<usize>, Option<HighlightCategory>)> {
            spans
                .iter()
                .map(|s| (s.range.clone(), s.category))
                .collect()
        };
        assert_eq!(
            cats(&spans),
            cats(&want),
            "the lines are coloured as they were inside the deleted fence"
        );
    }

    /// A jump while an edit is still unrepaired: the structure query is the
    /// first parse at the destination, and the checkpoint it resumes from
    /// sits past the edit, still holding the state from before it. Here the
    /// edit deletes the fence that every line below was inside, so that
    /// state says "fence body" for what is now prose. Such a cache must
    /// answer nothing rather than frame the prose as code.
    #[test]
    fn test_structure_lines_after_a_jump_ignore_checkpoints_past_a_pending_edit() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);

        // A fence that is never closed: every line below it is its body.
        let mut content = String::from("```text\n");
        while content.len() <= MAX_PARSE_BYTES + 128 * 1024 {
            content.push_str("a line of text\n");
        }
        let mut buffer = Buffer::from_str(&content, 0, test_fs());
        let step = 32 * 1024;
        let mut top = 0;
        while top + step < MAX_PARSE_BYTES + 64 * 1024 {
            engine.highlight_viewport(&buffer, top, top + step, &theme, 1024);
            top += step;
        }
        let len = buffer.len();
        engine.highlight_viewport(&buffer, len - 4096, len, &theme, 1024);

        // Delete the fence, then jump before any pass repairs the edit.
        buffer.delete(0..8);
        engine.notify_delete(0, 8);
        let start = content[..MAX_PARSE_BYTES / 2].rfind('\n').unwrap() + 1 - 8;
        let regions = engine.region_lines_in(&buffer, start..start + 2000);
        assert!(
            regions.is_empty(),
            "with the fence deleted there is no region; got {} lines of {:?}",
            regions.len(),
            regions.first().map(|r| r.1)
        );
    }

    /// Once the frame's highlight pass has parsed the viewport, asking for
    /// its structure parses nothing: the answer is already in the cache.
    /// This is what keeps scrolling a prose-heavy Markdown file cheap — the
    /// grammar backtracks exponentially on a line of several `code` spans,
    /// and a query that re-parsed paid that again on every scroll step.
    #[test]
    fn test_structure_lines_after_a_highlight_pass_parse_nothing() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);

        let mut content = String::new();
        for i in 0..40 {
            content.push_str(&format!(
                "Line {i} names `a`, `b`, `c`, `d`, `e` and `f`.\n\n| K | V |\n|---|---|\n| a | b |\n\n"
            ));
        }
        let buffer = Buffer::from_str(&content, 0, test_fs());
        engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 1024);
        engine.reset_highlight_stats();

        let starts: Vec<usize> = std::iter::once(0)
            .chain(content.match_indices('\n').map(|(i, _)| i + 1))
            .filter(|&s| s < buffer.len())
            .collect();
        let mut tables = 0;
        for &s in &starts {
            let structure = engine.structure_lines_in(&buffer, s..(s + 400).min(buffer.len()));
            tables += structure.tables.len();
        }
        assert!(tables > 0, "precondition: the queries see the tables");
        assert_eq!(
            engine.highlight_stats().map(|s| s.bytes_parsed),
            Some(0),
            "a structure query over already-highlighted lines must not re-parse them"
        );
    }

    /// The engine is what makes a *scrolled-into* fence resolvable: asked only
    /// about lines deep inside a block, it still answers, because its parse
    /// carries the state from before them rather than starting at the range.
    /// A batch-local reading of those same lines has no way to know.
    #[test]
    fn test_region_lines_resolve_a_range_starting_inside_a_fence() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);

        let mut content = String::from("intro\n\n```rust\n");
        for i in 0..200 {
            content.push_str(&format!("let v{i} = {i};\n"));
        }
        content.push_str("```\n\ntail\n");
        let buffer = Buffer::from_str(&content, 0, test_fs());

        // A window well past the opening fence — the shape of a scroll batch.
        let start = content.find("let v150").unwrap();
        let end = content.find("tail").unwrap();
        let roles = engine.region_lines_in(&buffer, start..end);

        assert_eq!(roles.first().map(|r| r.1), Some(RegionLine::Body));
        assert_eq!(
            roles
                .iter()
                .find(|(_, role)| *role == RegionLine::Close)
                .map(|(byte, _)| *byte),
            Some(content.rfind("```").unwrap()),
            "the closing delimiter must be reported as a close even though the \
             range never saw the opening fence"
        );
        assert!(
            roles.iter().all(|(byte, _)| *byte >= start),
            "nothing before the requested range is reported"
        );
    }

    /// An edit that has not yet been repaired by a highlight pass must not be
    /// answered from a checkpoint captured before it. Checkpoint *states* are
    /// left stale by `notify_insert` (only their positions shift), and so are
    /// the per-line structure records; a lookup that trusted either would
    /// keep reporting the pre-edit region structure — and the caller would
    /// frame lines that are no longer code.
    #[test]
    fn test_region_lines_ignore_checkpoints_stale_from_an_unrepaired_edit() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        // Long enough that checkpoints (every CHECKPOINT_INTERVAL bytes) land
        // between the fence and the lines below it.
        let mut content = String::from("```js\n");
        for i in 0..40 {
            content.push_str(&format!("const v{i} = {i};\n"));
        }
        content.push_str("```\n");
        for i in 0..40 {
            content.push_str(&format!("prose line {i}\n"));
        }
        let mut buffer = Buffer::from_str(&content, 0, test_fs());
        engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        let close_at = content.find("```\nprose").unwrap();
        let probe_from = content.find("prose line 20").unwrap();
        assert!(
            engine
                .region_lines_in(&buffer, probe_from..buffer.len())
                .is_empty(),
            "prose after the closed fence is outside any region"
        );

        // Break the closing fence *in place* (no line-count change, so nothing
        // forces a re-parse) — every line below is now fence body.
        let insert_at = close_at + 3;
        buffer.insert(insert_at, " x");
        engine.notify_insert(insert_at, 2);

        let roles = engine.region_lines_in(&buffer, probe_from + 2..buffer.len());
        assert!(
            !roles.is_empty() && roles.iter().all(|(_, r)| *r == RegionLine::Body),
            "with the fence no longer closed, the lines below must read as \
             region body, not as the pre-edit prose; got {roles:?}"
        );
    }

    /// A checkpoint stores the state *before* its position — every resume path
    /// (`find_parse_resume_point`, `try_partial_update`) parses the line at
    /// that position starting from it. `full_parse` used to
    /// overwrite a checkpoint sitting on a line start with the state *after*
    /// that line, so resuming from it re-parsed the line under the state that
    /// follows it.
    ///
    /// On a Markdown fence delimiter that inverts parity: the closing ```
    /// re-read from "outside" *opens* a region, and every prose line below the
    /// code block reads as fence body. Compose mode then frames that prose as
    /// code — visible when a file is opened mid-document, because the viewport
    /// starts below the poisoned checkpoint and only recovers once it scrolls
    /// above it.
    #[test]
    fn test_checkpoint_state_belongs_to_its_own_position() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        // Sized so the closing fence starts at exactly CHECKPOINT_INTERVAL:
        // the opening line is 8 bytes and each code line 31, so eight of them
        // put the walk at 256 and the checkpoint lands on the delimiter — the
        // one line where a state one line late changes what the parse means.
        let mut content = String::from("```bash\n");
        for _ in 0..8 {
            content.push_str(&format!("echo {}\n", "x".repeat(25)));
        }
        assert_eq!(content.len(), CHECKPOINT_INTERVAL);
        let close_fence = content.len();
        content.push_str("```\n\n");
        let prose_start = content.len();
        for i in 0..12 {
            content.push_str(&format!("prose line {i} with a `code` span in it\n"));
        }
        let buffer = Buffer::from_str(&content, 0, test_fs());

        // The cold probe walks from byte 0, so it is the ground truth.
        let cold = engine.region_lines_in(&buffer, 0..buffer.len());
        assert_eq!(
            cold.last().map(|(byte, role)| (*byte, *role)),
            Some((close_fence, RegionLine::Close)),
            "precondition: the fence closes and nothing below it is a region"
        );

        // The highlight pass that runs on the same frame lays down checkpoints
        // — one of them on the closing delimiter, by the sizing above.
        engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        // Every viewport the file can be opened at must read the document the
        // same way, whichever checkpoint it resumes from.
        for (line_start, _) in content.match_indices('\n').map(|(i, _)| (i + 1, ())) {
            if line_start >= buffer.len() {
                break;
            }
            let expected: Vec<_> = cold
                .iter()
                .filter(|(byte, _)| *byte >= line_start)
                .copied()
                .collect();
            assert_eq!(
                engine.region_lines_in(&buffer, line_start..buffer.len()),
                expected,
                "probe from byte {line_start} disagrees with the cold walk"
            );
        }

        // The symptom itself: prose below the block is not code.
        assert!(
            engine
                .region_lines_in(&buffer, prose_start..buffer.len())
                .is_empty(),
            "prose after the closed fence must not read as fence body"
        );
    }

    /// Buffers whose syntax hosts no embedded regions pay nothing.
    #[test]
    fn test_region_lines_empty_for_non_hosting_syntax() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("main.rs"), None, &registry);
        let buffer = Buffer::from_str("fn main() {\n    let x = 1;\n}\n", 0, test_fs());
        assert!(engine.region_lines_in(&buffer, 0..buffer.len()).is_empty());
    }

    /// Unified diffs select a source grammar from each target path and parse
    /// hunk rows without their one-byte patch marker.
    #[test]
    fn test_diff_embeds_target_file_syntax() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("commit.diff"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert_eq!(engine.syntax_name(), Some("Fresh Diff"));

        let content = "\
diff --git a/src/lib.rs b/src/lib.rs
index 1111111..2222222 100644
--- a/src/lib.rs
+++ b/src/lib.rs
@@ -1 +1 @@
-pub fn old() -> u32 { 1 }
+pub fn new() -> u32 { 2 }
diff --git a/tools/check.py b/tools/check.py
--- a/tools/check.py
+++ b/tools/check.py
@@ -1 +1 @@
-def old():
+def new():
";
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let spans = engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        let category_at = |needle: &str| {
            let position = content.find(needle).unwrap();
            spans
                .iter()
                .find(|span| span.range.start <= position && position < span.range.end)
                .and_then(|span| span.category)
        };

        assert_eq!(
            category_at("pub fn new"),
            Some(HighlightCategory::Keyword),
            "Rust added lines must retain Rust token colors"
        );
        assert_eq!(
            category_at("def new"),
            Some(HighlightCategory::Keyword),
            "the next file header must switch the embedded parser to Python"
        );
        let added_marker = content.find("+pub fn new").unwrap();
        assert_eq!(
            spans
                .iter()
                .find(|span| span.range.start <= added_marker && added_marker < span.range.end)
                .and_then(|span| span.category),
            Some(HighlightCategory::Inserted),
            "the patch marker itself must retain diff styling"
        );
        assert_eq!(
            category_at("@@ -1"),
            Some(HighlightCategory::Changed),
            "hunk headers stay styled by the host diff grammar"
        );
    }

    /// Fences naming an unknown language keep the host's raw-code styling.
    #[test]
    fn test_markdown_unknown_fence_language_keeps_raw_style() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);
        let content = "```definitely-not-a-language\nfn answer() { 42 }\n```\n";
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let spans = engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);
        let position = content.find("fn answer").unwrap();

        assert_eq!(
            spans
                .iter()
                .find(|span| span.range.start <= position && position < span.range.end)
                .and_then(|span| span.category),
            Some(HighlightCategory::String)
        );
    }

    /// CRLF fences must produce correctly-offset embedded spans.
    #[test]
    fn test_markdown_fence_crlf_offsets() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);
        let content = "```rust\r\nfn answer() -> u32 { 42 }\r\n```\r\n";
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let spans = engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        let position = content.find("fn answer").unwrap();
        assert_eq!(
            spans
                .iter()
                .find(|span| span.range.start <= position && position < span.range.end)
                .and_then(|span| span.category),
            Some(HighlightCategory::Keyword)
        );
    }

    /// A fence left open at EOF (streaming, or mid-typing) still gets
    /// embedded highlighting for the content streamed in so far.
    #[test]
    fn test_markdown_fence_unclosed_at_eof() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);
        let content = "```rust\nfn answer() -> u32 { 42 }\n";
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let spans = engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        let position = content.find("fn answer").unwrap();
        assert_eq!(
            spans
                .iter()
                .find(|span| span.range.start <= position && position < span.range.end)
                .and_then(|span| span.category),
            Some(HighlightCategory::Keyword)
        );
    }

    /// Editing the fence's language token must re-style the whole region.
    ///
    /// This pins the reason checkpoints and convergence carry the
    /// *composite* snapshot (host + embedded child state): inside a fence
    /// the host parser's state is identical no matter which language the
    /// fence names, so host-only convergence could falsely converge at
    /// the first checkpoint inside the region and leave stale spans from
    /// the old language beyond it. The fence here is longer than
    /// CHECKPOINT_INTERVAL to guarantee such a checkpoint exists.
    #[test]
    fn test_markdown_fence_language_edit_restyles_whole_region() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        let code_line = "fn deep_in_the_fence() -> u32 { 42 }\n";
        let lines = 2 * CHECKPOINT_INTERVAL / code_line.len() + 2;
        let fence_body = code_line.repeat(lines);
        let before = format!("```rust\n{fence_body}```\n\nafter\n");
        let buffer = Buffer::from_str(&before, 0, test_fs());
        let _ = engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        // A position in the fence's *last* code line — past any checkpoint
        // created inside the region.
        let last_line_pos = before.rfind("fn deep_in_the_fence").unwrap();
        assert_eq!(
            engine.category_at_position(last_line_pos),
            Some(HighlightCategory::Keyword),
            "precondition: rust highlighting active deep in the fence"
        );

        // Delete "rust" from the opening fence line.
        let lang_pos = before.find("rust").unwrap();
        let after = before.replacen("rust", "", 1);
        let buffer = Buffer::from_str(&after, 0, test_fs());
        engine.notify_delete(lang_pos, "rust".len());
        let spans = engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        let position = after.rfind("fn deep_in_the_fence").unwrap();
        assert_eq!(
            spans
                .iter()
                .find(|span| span.range.start <= position && position < span.range.end)
                .and_then(|span| span.category),
            Some(HighlightCategory::String),
            "after removing the language token the whole region must fall \
             back to raw-code styling — stale rust spans past a checkpoint \
             inside the fence mean convergence ignored the embedded state"
        );
    }

    /// Typing inside a fence must stay on the incremental partial-update
    /// path — no whole-file rescan per edit (the flaw that motivated
    /// replacing full-buffer fence detection).
    ///
    /// Convergence granularity note: syntect parse states carrying regex
    /// captures (which the fence context does, for its close-marker
    /// backreference) never compare equal across parses — `onig::Region`
    /// equality is allocation identity, a pre-existing syntect/onig
    /// limitation that master's tuple comparison had too. So an edit
    /// inside a fence re-parses to the end of the *region* and converges
    /// at the first checkpoint after it, rather than 256 bytes after the
    /// edit. This test pins exactly that contract: the long tail after
    /// the fence must not be re-parsed.
    #[test]
    fn test_markdown_fence_edit_reparses_region_not_file() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        let mut content = String::from("# Doc\n\n```rust\n");
        for i in 0..40 {
            content.push_str(&format!("fn f_{i}() -> u32 {{ {i} }}\n"));
        }
        content.push_str("```\n\n");
        let fence_end = content.len();
        for i in 0..400 {
            content.push_str(&format!("plain prose line {i}\n"));
        }
        let buffer = Buffer::from_str(&content, 0, test_fs());

        let HighlightEngine::TextMate(ref mut tm) = engine else {
            panic!("expected TextMate engine for .md");
        };
        let _ = tm.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);
        let bytes_before = tm.stats().bytes_parsed;

        // Type one character in the middle of the fence.
        let edit_pos = fence_end / 2;
        content.insert(edit_pos, 'x');
        let buffer = Buffer::from_str(&content, 0, test_fs());
        tm.notify_insert(edit_pos, 1);
        let _ = tm.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        let stats = tm.stats();
        let parsed = stats.bytes_parsed - bytes_before;
        let slack = 2 * CHECKPOINT_INTERVAL;
        assert!(
            parsed < fence_end + slack,
            "an edit inside a fence must converge shortly after the region \
             ends instead of re-parsing the rest of the file \
             (parsed {parsed}, fence ends at {fence_end}, file {} bytes)",
            content.len()
        );
        assert!(
            stats.convergences >= 1,
            "the edit must converge at a checkpoint after the fence"
        );
    }

    /// Embedded highlighting must keep working past MAX_PARSE_BYTES — the
    /// whole point of integrating regions into the windowed/checkpointed
    /// engine instead of a full-buffer scan with a size cutoff.
    #[test]
    fn test_markdown_fence_beyond_max_parse_bytes_no_size_cliff() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        let mut content = "plain prose line\n".repeat(MAX_PARSE_BYTES / 16);
        assert!(content.len() > MAX_PARSE_BYTES);
        let fence_pos = content.len();
        content.push_str("```rust\nfn deep() -> u32 { 42 }\n```\n");
        let buffer = Buffer::from_str(&content, 0, test_fs());

        let HighlightEngine::TextMate(ref mut tm) = engine else {
            panic!("expected TextMate engine for .md");
        };

        // Scroll from the top to the fence in steps small enough for the
        // forward-extension path (mirrors a user paging through the file,
        // building checkpoints along the way).
        let context = 10_000;
        let step = 16_000;
        let mut viewport_start = 0;
        loop {
            let viewport_end = (viewport_start + 1_000).min(buffer.len());
            let _ = tm.highlight_viewport(&buffer, viewport_start, viewport_end, &theme, context);
            if viewport_end == buffer.len() {
                break;
            }
            viewport_start += step;
        }

        let position = fence_pos + "```rust\n".len();
        assert_eq!(
            tm.category_at_position(position),
            Some(HighlightCategory::Keyword),
            "a fence starting past MAX_PARSE_BYTES must still get embedded \
             highlighting once reached via scrolling — a size-based cutoff \
             here means the mechanism bypassed the windowed engine"
        );
    }

    /// Vue `<script>`/`<style>` blocks with no `lang` attribute are
    /// parsed with the spec's default languages (js / css) — proving the
    /// embedding mechanism generalizes beyond Markdown with nothing but
    /// spec-table entries and grammar scopes.
    #[test]
    fn test_vue_script_and_style_default_languages() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("App.vue"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");

        let content = "<template>\n  <p>hi</p>\n</template>\n\n<script>\nconst answer = 42;\n</script>\n\n<style>\n.foo { color: red; }\n</style>\n";
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let spans = engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        let category_at = |needle: &str| {
            let position = content.find(needle).unwrap();
            spans
                .iter()
                .find(|span| span.range.start <= position && position < span.range.end)
                .and_then(|span| span.category)
        };

        assert_eq!(
            category_at("const"),
            Some(HighlightCategory::Keyword),
            "script block without lang must be parsed as JavaScript"
        );
        assert_eq!(category_at("42"), Some(HighlightCategory::Number));
        assert_eq!(
            category_at("color"),
            Some(HighlightCategory::Type),
            "style block without lang must be parsed as CSS"
        );
        // Delimiter lines stay host-styled (tag names).
        assert_eq!(category_at("script>"), Some(HighlightCategory::Property));
    }

    /// A `lang` attribute dynamically selects the embedded language.
    #[test]
    fn test_vue_lang_attribute_selects_language() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("App.vue"), None, &registry);
        let content = "<style lang=\"scss\">\n$width: 10px;\n.a { width: $width; }\n</style>\n";
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let spans = engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        let position = content.find("$width").unwrap();
        assert_eq!(
            spans
                .iter()
                .find(|span| span.range.start <= position && position < span.range.end)
                .and_then(|span| span.category),
            Some(HighlightCategory::Variable),
            "lang=\"scss\" must switch the style block to the SCSS grammar \
             ($variables are not CSS syntax)"
        );
    }

    /// A `lang` naming a language with no grammar in the set falls back
    /// to the region's default language rather than dropping
    /// highlighting entirely.
    #[test]
    fn test_vue_unresolvable_lang_falls_back_to_default() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("App.vue"), None, &registry);
        let content = "<script setup lang=\"mysterylang\">\nconst answer = 42;\n</script>\n";
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let spans = engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        let position = content.find("const").unwrap();
        assert_eq!(
            spans
                .iter()
                .find(|span| span.range.start <= position && position < span.range.end)
                .and_then(|span| span.category),
            Some(HighlightCategory::Keyword),
            "unresolvable lang token must fall back to the spec default (js)"
        );
    }

    /// `lang="ts"` resolves to the bundled TypeScript grammar — the
    /// dominant real-world Vue pattern gets TS-aware highlighting
    /// (`interface` is not a keyword to the JS default it previously
    /// fell back to).
    #[test]
    fn test_vue_lang_ts_uses_typescript_grammar() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("App.vue"), None, &registry);
        let content =
            "<script setup lang=\"ts\">\ninterface Greeting {\n  message: string;\n}\n</script>\n";
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let spans = engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        let category_at = |needle: &str| {
            let position = content.find(needle).unwrap();
            spans
                .iter()
                .find(|span| span.range.start <= position && position < span.range.end)
                .and_then(|span| span.category)
        };
        assert_eq!(
            category_at("interface"),
            Some(HighlightCategory::Keyword),
            "TS-only keyword must be styled by the TypeScript grammar"
        );
        assert_eq!(
            category_at("string"),
            Some(HighlightCategory::Type),
            "TS primitive type must be styled by the TypeScript grammar"
        );
    }

    /// Markdown ```ts fences resolve to the same TypeScript grammar.
    #[test]
    fn test_markdown_ts_fence_uses_typescript_grammar() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("README.md"), None, &registry);
        let content = "```ts\ntype Answer = number;\n```\n";
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let spans = engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);

        let position = content.find("type").unwrap();
        assert_eq!(
            spans
                .iter()
                .find(|span| span.range.start <= position && position < span.range.end)
                .and_then(|span| span.category),
            Some(HighlightCategory::Keyword),
            "```ts fences must be parsed with the TypeScript grammar"
        );
    }

    #[test]
    fn test_tree_sitter_direct() {
        // Verify a tree-sitter highlighter can be created directly for a
        // bundled grammar (TypeScript — most grammars were dropped and are now
        // highlighted by syntect instead).
        let highlighter = Highlighter::new(Language::TypeScript);
        assert!(highlighter.is_ok());
    }

    #[test]
    fn test_unknown_extension() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // Unknown extension
        let engine = HighlightEngine::for_file(Path::new("test.unknown_xyz_123"), None, &registry);
        // Might be none or might find something via syntect
        // Just verify it doesn't panic
        let _ = engine.backend_name();
    }

    #[test]
    fn test_highlight_viewport_empty_buffer_no_panic() {
        // Regression test: calling highlight_viewport with an empty buffer
        // and non-zero viewport range previously caused subtraction overflow panic.
        //
        // The bug occurred when:
        // - buffer is empty (len = 0)
        // - viewport_start > context_bytes (so parse_start > 0 after saturating_sub)
        // - parse_end = min(viewport_end + context_bytes, buffer.len()) = 0
        // - parse_end - parse_start would underflow (0 - positive = overflow)
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        let mut engine = HighlightEngine::for_file(Path::new("test.rs"), None, &registry);

        // Create empty buffer
        let buffer = Buffer::from_str("", 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        // Test the specific case that triggered the overflow:
        // viewport_start=100, context_bytes=10 => parse_start=90, parse_end=0
        // 0 - 90 = overflow!
        if let HighlightEngine::TextMate(ref mut tm) = engine {
            // Small context_bytes so parse_start remains > 0
            let spans = tm.highlight_viewport(&buffer, 100, 200, &theme, 10);
            assert!(spans.is_empty());
        }
    }

    /// Test that TextMateEngine produces correct byte offsets for CRLF content.
    /// This is a regression test for a bug where using str::lines() caused 1-byte
    /// offset drift per line because it strips line terminators.
    #[test]
    fn test_textmate_engine_crlf_byte_offsets() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        let mut engine = HighlightEngine::for_file(Path::new("test.java"), None, &registry);

        // Create CRLF content with keywords on each line
        // Each "public" keyword should be highlighted at byte positions:
        // Line 1: "public" at bytes 0-5
        // Line 2: "public" at bytes 8-13 (after "public\r\n" = 8 bytes)
        // Line 3: "public" at bytes 16-21 (after two "public\r\n" = 16 bytes)
        let content = b"public\r\npublic\r\npublic\r\n";
        let buffer = Buffer::from_bytes(content.to_vec(), test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        if let HighlightEngine::TextMate(ref mut tm) = engine {
            // Highlight the entire content
            let spans = tm.highlight_viewport(&buffer, 0, content.len(), &theme, 0);

            // Find spans that cover keyword positions
            // The keyword "public" should have spans at these byte ranges:
            // Line 1: 0..6
            // Line 2: 8..14 (NOT 7..13 which would be the buggy offset)
            // Line 3: 16..22 (NOT 14..20 which would be the buggy offset)

            eprintln!(
                "Spans: {:?}",
                spans.iter().map(|s| &s.range).collect::<Vec<_>>()
            );

            // Check that we have spans covering the correct positions
            let has_span_at = |start: usize, end: usize| -> bool {
                spans
                    .iter()
                    .any(|s| s.range.start <= start && s.range.end >= end)
            };

            // Line 1: "public" at bytes 0-6
            assert!(
                has_span_at(0, 6),
                "Should have span covering bytes 0-6 (line 1 'public'). Spans: {:?}",
                spans.iter().map(|s| &s.range).collect::<Vec<_>>()
            );

            // Line 2: "public" at bytes 8-14 (after "public\r\n")
            // If buggy, would be at 7-13
            assert!(
                has_span_at(8, 14),
                "Should have span covering bytes 8-14 (line 2 'public'). \
                 If this fails, CRLF offset drift is occurring. Spans: {:?}",
                spans.iter().map(|s| &s.range).collect::<Vec<_>>()
            );

            // Line 3: "public" at bytes 16-22 (after two "public\r\n")
            // If buggy, would be at 14-20
            assert!(
                has_span_at(16, 22),
                "Should have span covering bytes 16-22 (line 3 'public'). \
                 If this fails, CRLF offset drift is occurring. Spans: {:?}",
                spans.iter().map(|s| &s.range).collect::<Vec<_>>()
            );
        } else {
            panic!("Expected TextMate engine for .java file");
        }
    }

    /// A buffer parsed to its end with no trailing newline (the
    /// streaming case for `git show` output between writes) DOES commit
    /// its trailing partial line to the cache.
    ///
    /// The bytes are final unless the buffer grows, and both
    /// cache-consuming paths in `highlight_viewport` require
    /// `last_buffer_len == buffer.len()` and no pending edit — so a
    /// grown buffer can never resume from this state. That length gate,
    /// not the commit boundary, is what prevents the streaming artefact;
    /// `test_streaming_partial_diff_line_keeps_bg_when_rest_arrives`
    /// pins the rendered outcome.
    ///
    /// Committing here is what lets a file that is one huge line with no
    /// trailing newline (a minified JSON one-liner, fresh#2838) be
    /// parsed once instead of re-parsed on every frame.
    #[test]
    fn test_partial_trailing_line_committed_when_parse_reaches_eof() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("commit.diff"), None, &registry);
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();

        // A complete `+` line followed by a partial `+` line (no \n).
        let content = "+complete\n+partial";
        let buffer = Buffer::from_str(content, 0, test_fs());

        if let HighlightEngine::TextMate(ref mut tm) = engine {
            let _ = tm.highlight_viewport(&buffer, 0, content.len(), &theme, 0);
            let (cache_end, has_tail) = tm.cache_commit_for_test();
            assert_eq!(
                cache_end,
                content.len(),
                "a parse that reached the buffer end should commit the whole \
                 range, so the next frame is a cache hit rather than a re-parse"
            );
            assert!(
                has_tail,
                "tail state should be saved at the commit boundary"
            );
        }
    }

    /// Behavioural guard for the streaming case: when more bytes of a
    /// partially-streamed `+` line arrive, every byte of the completed
    /// line must still carry the diff-add background.
    ///
    /// This is the artefact the "never commit past the last newline"
    /// rule exists to prevent (see `extend_cache_forward`): with
    /// syntect's line-anchored `Diff` grammar, resuming from a state
    /// captured mid-line parses the continuation in `source.diff` with
    /// no scope, leaving a bg-less dark bar inside an otherwise green
    /// line.  Unlike `test_partial_trailing_line_not_committed_to_cache`
    /// (which pins where the cache commits), this asserts the rendered
    /// outcome, so it stays valid if the commit strategy changes.
    #[test]
    fn test_streaming_partial_diff_line_keeps_bg_when_rest_arrives() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("commit.diff"), None, &registry);
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();

        // Refresh 1: the second `+` line has only partially arrived.
        let first = "+complete\n+partial";
        let buf1 = Buffer::from_str(first, 0, test_fs());
        if let HighlightEngine::TextMate(ref mut tm) = engine {
            let _ = tm.highlight_viewport(&buf1, 0, first.len(), &theme, 0);
        }

        // Refresh 2: the rest of that same line (and its newline) arrived.
        let second = "+complete\n+partial line continues to here\n";
        let buf2 = Buffer::from_str(second, 0, test_fs());
        if let HighlightEngine::TextMate(ref mut tm) = engine {
            let spans = tm.highlight_viewport(&buf2, 0, second.len(), &theme, 0);
            let line_start = "+complete\n".len();
            let line_end = second.len() - 1; // exclude the trailing newline
            for byte_pos in line_start..line_end {
                let span = spans
                    .iter()
                    .find(|s| s.range.start <= byte_pos && s.range.end > byte_pos);
                assert_eq!(
                    span.and_then(|s| s.bg),
                    Some(theme.diff_add_bg),
                    "byte {} (`{}`) of the completed `+` line lost its diff bg — \
                     the streaming continuation was parsed from a mid-line state",
                    byte_pos,
                    second[byte_pos..byte_pos + 1].escape_debug(),
                );
            }
        }
    }

    /// Reproduce: artifacts inside `+` lines whose content contains
    /// JS template literals — `\`...\`` with `${}` interpolation.
    /// The whole `+` line should be one contiguous Inserted span
    /// carrying `theme.diff_add_bg`, with no bg-less holes mid-line.
    #[test]
    fn test_diff_inserted_line_is_fully_covered() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("commit.diff"), None, &registry);
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();

        let content =
            "diff --git a/file.ts b/file.ts\n\
             index aaa..bbb 100644\n\
             --- a/file.ts\n\
             +++ b/file.ts\n\
             @@ -1,3 +1,5 @@\n\
             +${seen[g.subtree] > 1 ? `**Seen ${seen[g.subtree]}× — likely cross-subtree type seam.**` : \"\"}\n\
             +              const k = `${b.fn}::${(b.what || \"\").slice(0, 80)}`;\n";
        let buffer = Buffer::from_str(content, 0, test_fs());

        if let HighlightEngine::TextMate(ref mut tm) = engine {
            let spans = tm.highlight_viewport(&buffer, 0, content.len(), &theme, 0);

            let bytes = content.as_bytes();
            let mut line_start = 0;
            while line_start < bytes.len() {
                let mut line_end = line_start;
                while line_end < bytes.len() && bytes[line_end] != b'\n' {
                    line_end += 1;
                }
                if bytes[line_start] == b'+' && !content[line_start..line_end].starts_with("+++") {
                    for byte_pos in line_start..line_end {
                        let span = spans
                            .iter()
                            .find(|s| s.range.start <= byte_pos && s.range.end > byte_pos);
                        let bg = span.and_then(|s| s.bg);
                        assert_eq!(
                            bg,
                            Some(theme.diff_add_bg),
                            "byte {} (`{}`) of `+` line starting at {} should carry diff_add_bg; \
                             got span={:?}",
                            byte_pos,
                            content[byte_pos..byte_pos + 1].escape_debug(),
                            line_start,
                            span,
                        );
                    }
                }
                line_start = line_end + 1;
            }
        } else {
            panic!("Expected TextMate engine for .diff file");
        }
    }

    /// Reproduce <https://github.com/sinelaw/fresh/issues/3021>: the
    /// enclosing-section name `git diff` appends after a hunk header's
    /// closing `@@` is scoped outside `meta.diff.range`, so the
    /// `diff_modify_bg` bar stopped at the `@@` and left the section
    /// name in a bg-less hole. The whole `@@` row must carry
    /// `theme.diff_modify_bg`, exactly as
    /// `test_diff_inserted_line_is_fully_covered` requires of `+` rows.
    #[test]
    fn test_diff_hunk_header_line_is_fully_covered() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("commit.diff"), None, &registry);
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();

        // Two hunk headers: one with a section name (the reported case)
        // and one bare, so the bare form is covered against regressions
        // from the gap fill.
        let content = concat!(
            "diff --git a/file.rs b/file.rs\n",
            "index aaa..bbb 100644\n",
            "--- a/file.rs\n",
            "+++ b/file.rs\n",
            "@@ -2,6 +2,8 @@ fn keep_one(&self) -> usize {\n",
            " context\n",
            "+added\n",
            "@@ -40,3 +42,4 @@\n",
            " more context\n",
        );
        let buffer = Buffer::from_str(content, 0, test_fs());

        if let HighlightEngine::TextMate(ref mut tm) = engine {
            let spans = tm.highlight_viewport(&buffer, 0, content.len(), &theme, 0);

            let bytes = content.as_bytes();
            let mut line_start = 0;
            let mut headers_checked = 0;
            while line_start < bytes.len() {
                let mut line_end = line_start;
                while line_end < bytes.len() && bytes[line_end] != b'\n' {
                    line_end += 1;
                }
                if content[line_start..line_end].starts_with("@@ ") {
                    headers_checked += 1;
                    for byte_pos in line_start..line_end {
                        let span = spans
                            .iter()
                            .find(|s| s.range.start <= byte_pos && s.range.end > byte_pos);
                        let bg = span.and_then(|s| s.bg);
                        assert_eq!(
                            bg,
                            Some(theme.diff_modify_bg),
                            "byte {} (`{}`) of `@@` line starting at {} should carry \
                             diff_modify_bg; got span={:?}",
                            byte_pos,
                            content[byte_pos..byte_pos + 1].escape_debug(),
                            line_start,
                            span,
                        );
                    }
                }
                line_start = line_end + 1;
            }
            assert_eq!(headers_checked, 2, "both hunk headers should be examined");
        } else {
            panic!("Expected TextMate engine for .diff file");
        }
    }

    /// Fresh's diff grammar anchors file-header rules, so text resembling
    /// a file header inside a hunk-header section remains unclaimed and
    /// receives the row wash instead of punching a hole in it.
    #[test]
    fn test_diff_hunk_header_unclaimed_section_is_washed() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("commit.diff"), None, &registry);
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();

        const RANGE: &str = "@@ -2,6 +2,8 @@";
        const SECTION: &str = " - foo ====";
        let content = format!(
            "--- a/file.rs\n\
             +++ b/file.rs\n\
             {RANGE}{SECTION}\n\
             context\n"
        );
        let buffer = Buffer::from_str(&content, 0, test_fs());

        let HighlightEngine::TextMate(ref mut tm) = engine else {
            panic!("Expected TextMate engine for .diff file");
        };
        let spans = tm.highlight_viewport(&buffer, 0, content.len(), &theme, 0);
        let span_at = |byte_pos: usize| {
            spans
                .iter()
                .find(|s| s.range.start <= byte_pos && s.range.end > byte_pos)
        };
        let bg_at = |byte_pos: usize| span_at(byte_pos).and_then(|s| s.bg);

        let header_start = content.find(RANGE).expect("header line present");
        // The `@@ … @@` run is washed, as issue #3021 requires.
        for byte_pos in header_start..header_start + RANGE.len() {
            assert_eq!(
                bg_at(byte_pos),
                Some(theme.diff_modify_bg),
                "byte {byte_pos} of `{RANGE}` should carry diff_modify_bg",
            );
        }
        // The section resembles a file header, but the corresponding grammar
        // rule is line-anchored. It therefore remains unclaimed and inherits
        // the hunk header's wash.
        for byte_pos in header_start + RANGE.len()..header_start + RANGE.len() + SECTION.len() {
            assert_eq!(
                bg_at(byte_pos),
                Some(theme.diff_modify_bg),
                "byte {byte_pos} of `{SECTION}` should inherit the hunk-header wash",
            );
        }
    }

    #[test]
    fn test_git_rebase_todo_highlighting() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // git-rebase-todo files should use the Git Rebase Todo grammar
        let engine = HighlightEngine::for_file(Path::new("git-rebase-todo"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());
    }

    #[test]
    fn test_git_commit_message_highlighting() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // COMMIT_EDITMSG should use the Git Commit Message grammar
        let engine = HighlightEngine::for_file(Path::new("COMMIT_EDITMSG"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());

        // MERGE_MSG should also work
        let engine = HighlightEngine::for_file(Path::new("MERGE_MSG"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());
    }

    #[test]
    fn test_gitignore_highlighting() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // .gitignore should use the Gitignore grammar
        let engine = HighlightEngine::for_file(Path::new(".gitignore"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());

        // .dockerignore should also work
        let engine = HighlightEngine::for_file(Path::new(".dockerignore"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());
    }

    #[test]
    fn test_gitconfig_highlighting() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // .gitconfig should use the Git Config grammar
        let engine = HighlightEngine::for_file(Path::new(".gitconfig"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());

        // .gitmodules should also work
        let engine = HighlightEngine::for_file(Path::new(".gitmodules"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());
    }

    #[test]
    fn test_gitattributes_highlighting() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // .gitattributes should use the Git Attributes grammar
        let engine = HighlightEngine::for_file(Path::new(".gitattributes"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());
    }

    #[test]
    fn test_comment_delimiter_uses_comment_color() {
        // Comment delimiters (#, //, /*) should use comment color, not operator
        assert_eq!(
            scope_to_category("punctuation.definition.comment"),
            Some(HighlightCategory::Comment)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.comment.python"),
            Some(HighlightCategory::Comment)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.comment.begin"),
            Some(HighlightCategory::Comment)
        );
    }

    #[test]
    fn test_word_operators_use_keyword_color() {
        // Operators spelled as words are scoped `keyword.operator.word` by
        // Sublime grammars. They must read as keywords, not operators: the
        // operator colour equals plain text in most themes, so C++
        // `static_assert` looked unhighlighted (issue #3644).
        assert_eq!(
            scope_to_category("keyword.operator.word.c++"),
            Some(HighlightCategory::Keyword)
        );
        assert_eq!(
            scope_to_category("keyword.operator.word.instanceof.java"),
            Some(HighlightCategory::Keyword)
        );
        // Symbolic operators keep the operator colour.
        assert_eq!(
            scope_to_category("keyword.operator.arithmetic.c"),
            Some(HighlightCategory::Operator)
        );
        assert_eq!(
            scope_to_category("keyword.operator.assignment.python"),
            Some(HighlightCategory::Operator)
        );
    }

    #[test]
    fn test_variable_builtin_category() {
        assert_eq!(
            scope_to_category("variable.language.this"),
            Some(HighlightCategory::VariableBuiltin)
        );
        assert_eq!(
            scope_to_category("variable.language.super"),
            Some(HighlightCategory::VariableBuiltin)
        );
    }

    #[test]
    fn test_string_delimiter_uses_string_color() {
        // String delimiters (", ', `) should use string color, not operator
        assert_eq!(
            scope_to_category("punctuation.definition.string.begin"),
            Some(HighlightCategory::String)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.string.end"),
            Some(HighlightCategory::String)
        );
    }

    #[test]
    fn test_punctuation_bracket() {
        // punctuation.section (TextMate standard for block delimiters)
        assert_eq!(
            scope_to_category("punctuation.section"),
            Some(HighlightCategory::PunctuationBracket)
        );
        assert_eq!(
            scope_to_category("punctuation.section.block.begin.c"),
            Some(HighlightCategory::PunctuationBracket)
        );
        assert_eq!(
            scope_to_category("punctuation.bracket"),
            Some(HighlightCategory::PunctuationBracket)
        );
        // punctuation.definition.* bracket-like scopes from sublime-syntax grammars
        assert_eq!(
            scope_to_category("punctuation.definition.array.begin.toml"),
            Some(HighlightCategory::PunctuationBracket)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.block.code.typst"),
            Some(HighlightCategory::PunctuationBracket)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.group.typst"),
            Some(HighlightCategory::PunctuationBracket)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.inline-table.begin.toml"),
            Some(HighlightCategory::PunctuationBracket)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.tag.end.svelte"),
            Some(HighlightCategory::PunctuationBracket)
        );
    }

    #[test]
    fn test_punctuation_delimiter() {
        assert_eq!(
            scope_to_category("punctuation.separator"),
            Some(HighlightCategory::PunctuationDelimiter)
        );
        assert_eq!(
            scope_to_category("punctuation.terminator.statement.c"),
            Some(HighlightCategory::PunctuationDelimiter)
        );
        assert_eq!(
            scope_to_category("punctuation.accessor"),
            Some(HighlightCategory::PunctuationDelimiter)
        );
    }

    #[test]
    fn test_storage_type_keyword() {
        assert_eq!(
            scope_to_category("storage.type"),
            Some(HighlightCategory::Keyword)
        );
        assert_eq!(
            scope_to_category("storage.type.class"),
            Some(HighlightCategory::Keyword)
        );
        assert_ne!(
            scope_to_category("storage.type"),
            Some(HighlightCategory::Type)
        );
    }

    /// First parse of a small file populates a whole-file cache; subsequent
    /// scrolls anywhere in the file are exact cache hits with no extra parse
    /// work.
    #[test]
    fn test_small_file_scroll_is_cache_hit() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("test.rs"), None, &registry);

        let mut content = String::new();
        for i in 0..200 {
            content.push_str(&format!("fn f_{i}() {{ let x = {i}; }}\n"));
        }
        let buffer = Buffer::from_str(&content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        let HighlightEngine::TextMate(ref mut tm) = engine else {
            panic!("expected TextMate engine for .rs");
        };

        // First call: cold start, full parse.
        let _ = tm.highlight_viewport(&buffer, 0, 200, &theme, 10_000);
        let stats_after_first = tm.stats().clone();
        assert_eq!(
            stats_after_first.cache_hits, 0,
            "first call cannot hit cache"
        );
        assert_eq!(
            stats_after_first.cache_misses, 1,
            "first call must be a miss"
        );

        // Scroll anywhere — top, middle, end. All must be cache hits.
        let mid = buffer.len() / 2;
        let near_end = buffer.len().saturating_sub(200);
        let probes = [(0, 200), (mid, mid + 200), (near_end, buffer.len())];
        for (vs, ve) in probes {
            let _ = tm.highlight_viewport(&buffer, vs, ve, &theme, 10_000);
        }

        let stats_after_scroll = tm.stats().clone();
        assert_eq!(
            stats_after_scroll.cache_misses,
            1,
            "scrolling must not add cache misses (got extra: {})",
            stats_after_scroll.cache_misses - 1
        );
        assert_eq!(
            stats_after_scroll.cache_hits, 3,
            "all three scroll probes must hit the cache"
        );
        assert_eq!(
            stats_after_scroll.bytes_parsed, stats_after_first.bytes_parsed,
            "scrolling must not parse any new bytes"
        );
    }

    /// After a small edit, the next render takes the partial-update path
    /// (convergence) and continues to serve cache hits afterwards. Crucially:
    /// the partial update parses far fewer bytes than the file is long.
    #[test]
    fn test_small_file_edit_uses_partial_update() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("test.rs"), None, &registry);

        let mut content = String::new();
        for i in 0..200 {
            content.push_str(&format!("fn f_{i}() {{ let x = {i}; }}\n"));
        }
        let buffer = Buffer::from_str(&content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        let HighlightEngine::TextMate(ref mut tm) = engine else {
            panic!("expected TextMate engine for .rs");
        };

        // Warm cache.
        let _ = tm.highlight_viewport(&buffer, 0, 100, &theme, 10_000);
        let bytes_before_edit = tm.stats().bytes_parsed;
        let buf_len = buffer.len();
        assert!(
            buf_len > 4000,
            "test needs a buffer larger than the partial-update region"
        );

        // Simulate an edit deep in the file.
        let edit_pos = buf_len / 2;
        tm.notify_insert(edit_pos, 1);
        // The buffer itself doesn't change here (we test the engine in isolation),
        // but notify_insert sets dirty_from and shifts spans, which is what the
        // partial-update path consumes.

        let _ = tm.highlight_viewport(&buffer, 0, 100, &theme, 10_000);
        let bytes_after_edit = tm.stats().bytes_parsed;
        let parsed = bytes_after_edit - bytes_before_edit;

        assert!(
            parsed < buf_len,
            "edit must not trigger a whole-file reparse (parsed {parsed}, file {buf_len})"
        );
    }

    /// Bulk edits (multi-cursor typing, "select word + type letter" replace,
    /// search-replace, etc.) must take the same partial-update path as single
    /// edits. Regression for #1958: the previous code called `invalidate_all()`
    /// after a bulk edit, wiping every checkpoint and forcing a cold reparse
    /// from byte zero on the next keystroke.
    #[test]
    fn test_bulk_edit_uses_partial_update() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("test.rs"), None, &registry);

        let mut content = String::new();
        for i in 0..200 {
            content.push_str(&format!("fn f_{i}() {{ let x = {i}; }}\n"));
        }
        let buffer = Buffer::from_str(&content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        // Warm cache.
        let _ = engine.highlight_viewport(&buffer, 0, 100, &theme, 10_000);
        let bytes_before_edit = match &engine {
            HighlightEngine::TextMate(h) => h.stats().bytes_parsed,
            _ => panic!("expected TextMate engine for .rs"),
        };
        let buf_len = buffer.len();
        assert!(
            buf_len > 4000,
            "test needs a buffer larger than the partial-update region"
        );

        // Simulate "select a word, type a letter" deep in the file: a single
        // bulk edit that deletes 8 bytes and inserts 1 byte at the same
        // position. This is exactly the user-facing scenario in #1958.
        let edit_pos = buf_len / 2;
        let edits = vec![(edit_pos, 8usize, 1usize)];
        engine.notify_edits(&edits);

        let _ = engine.highlight_viewport(&buffer, 0, 100, &theme, 10_000);
        let bytes_after_edit = match &engine {
            HighlightEngine::TextMate(h) => h.stats().bytes_parsed,
            _ => unreachable!(),
        };
        let parsed = bytes_after_edit - bytes_before_edit;

        assert!(
            parsed < buf_len,
            "bulk edit must not trigger a whole-file reparse \
             (parsed {parsed}, file {buf_len})"
        );
    }

    /// A bulk edit that touches many lines (comment toggle: `# ` at each
    /// line start) must re-highlight every edited line, not stop at the first
    /// checkpoint whose stored state happens to match before the last edit.
    /// Each insert also pushes the checkpoint sitting on its line start into
    /// mid-line, where its state belongs to no parse position, so the partial
    /// update must not converge on it either. The result is checked against a
    /// fresh full parse, after the inserts and again after the inverse deletes.
    #[test]
    fn test_bulk_line_start_comment_edits_match_full_parse() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let path = Path::new("config.yaml");

        // 32-byte lines alternating the two halves of a quoted string that
        // spans them, so the parse state differs from line to line and
        // checkpoints land on every eighth line start (an opening half).
        let lines: Vec<String> = (0..48)
            .map(|i| {
                if i % 2 == 0 {
                    format!("key_{i:02}: \"one long quoted string\n")
                } else {
                    format!("  spans two lines; no {i:02} pads.\"\n")
                }
            })
            .collect();
        assert!(lines.iter().all(|line| line.len() == 32));
        let original: String = lines.concat();
        assert!(original.len() >= 3 * CHECKPOINT_INTERVAL);
        let line_starts: Vec<usize> = (0..lines.len()).map(|i| 32 * i).collect();
        // Ten openers: a run through the line-8 checkpoint, then, past the
        // untouched line-16 checkpoint, a run ending on the line-24 one.
        let edited = [2, 4, 6, 8, 10, 12, 18, 20, 22, 24];

        let spans_of = |engine: &mut HighlightEngine, text: &str| {
            let buffer = Buffer::from_str(text, 0, test_fs());
            engine
                .highlight_viewport(&buffer, 0, text.len(), &theme, text.len())
                .into_iter()
                .map(|span| (span.range, span.category))
                .collect::<Vec<_>>()
        };
        let full_parse = |text: &str| {
            let mut fresh = HighlightEngine::for_file(path, None, &registry);
            spans_of(&mut fresh, text)
        };

        let mut engine = HighlightEngine::for_file(path, None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        let _ = spans_of(&mut engine, &original);

        // Comment: insert `# ` at each line start, descending like a bulk edit.
        let commented: String = lines
            .iter()
            .enumerate()
            .map(|(i, line)| {
                if edited.contains(&i) {
                    format!("# {line}")
                } else {
                    line.clone()
                }
            })
            .collect();
        let inserts: Vec<(usize, usize, usize)> = edited
            .iter()
            .rev()
            .map(|&i| (line_starts[i], 0, 2))
            .collect();
        engine.notify_edits(&inserts);
        assert_eq!(
            spans_of(&mut engine, &commented),
            full_parse(&commented),
            "partial re-highlight after commenting must match a full parse"
        );

        // Uncomment: delete each `# ` again, at its post-insert position.
        let deletes: Vec<(usize, usize, usize)> = edited
            .iter()
            .enumerate()
            .rev()
            .map(|(n, &i)| (line_starts[i] + 2 * n, 2, 0))
            .collect();
        engine.notify_edits(&deletes);
        assert_eq!(
            spans_of(&mut engine, &original),
            full_parse(&original),
            "partial re-highlight after uncommenting must match a full parse"
        );
    }

    /// `notify_edits` applies a batch in order, each position in the text as
    /// the edits before it left it, so the end of the dirty range must follow
    /// every later shift — whatever order the sites come in. An insert near
    /// the top that is longer than a checkpoint interval moves an earlier,
    /// lower edit past the checkpoint between them; a dirty end left behind
    /// lets the walk converge on that checkpoint before it re-parses the
    /// lower edit. Also covers ascending batches, a newline replaced by a
    /// space (which pulls a checkpoint off its line start) and its inverse,
    /// each on LF and CRLF text, against a fresh full parse after every batch.
    #[test]
    fn test_bulk_edits_in_any_order_match_full_parse() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let path = Path::new("config.yaml");

        // Apply `(position, deleted bytes, inserted text)` edits in order and
        // return them in the form `notify_edits` takes.
        fn apply(
            text: &mut String,
            batch: &[(usize, usize, String)],
        ) -> Vec<(usize, usize, usize)> {
            batch
                .iter()
                .map(|(pos, del, ins)| {
                    text.replace_range(*pos..*pos + *del, ins);
                    (*pos, *del, ins.len())
                })
                .collect()
        }
        let spans_of = |engine: &mut HighlightEngine, text: &str| {
            let buffer = Buffer::from_str(text, 0, test_fs());
            engine
                .highlight_viewport(&buffer, 0, text.len(), &theme, text.len())
                .into_iter()
                .map(|span| (span.range, span.category))
                .collect::<Vec<_>>()
        };
        let full_parse = |text: &str| {
            let mut fresh = HighlightEngine::for_file(path, None, &registry);
            spans_of(&mut fresh, text)
        };

        for nl in ["\n", "\r\n"] {
            // Same shape as `test_bulk_line_start_comment_edits_match_full_parse`:
            // a quoted string opened on every even line and closed on the
            // next, so the state differs from line to line.
            let lines: Vec<String> = (0..48)
                .map(|i| {
                    if i % 2 == 0 {
                        format!("key_{i:02}: \"one long quoted string{nl}")
                    } else {
                        format!("  spans two lines; no {i:02} pads.\"{nl}")
                    }
                })
                .collect();
            let line_len = lines[0].len();
            assert!(lines.iter().all(|line| line.len() == line_len));
            let original: String = lines.concat();
            let line_start = |i: usize| i * line_len;
            // Complete top-level lines, so the state after them is the state
            // before them, and more than a checkpoint interval of them.
            let pad: String = (0..30).map(|k| format!("pad_{k:02}: 1{nl}")).collect();
            assert!(pad.len() > CHECKPOINT_INTERVAL + line_len);
            let edited = [2, 4, 6, 8, 10, 12, 18, 20, 22, 24];
            let s = |text: &str| text.to_string();

            let batches: Vec<Vec<(usize, usize, String)>> = vec![
                // Comment, ascending: each site past the earlier inserts.
                edited
                    .iter()
                    .enumerate()
                    .map(|(n, &i)| (line_start(i) + 2 * n, 0, s("# ")))
                    .collect(),
                // Uncomment, ascending: each site past the earlier deletes.
                edited
                    .iter()
                    .map(|&i| (line_start(i), 2, String::new()))
                    .collect(),
                // A lower edit, then a long insert above it.
                vec![
                    (line_start(24), 0, s("# ")),
                    (line_start(2), 0, pad.clone()),
                ],
                // The inverse, lower site first.
                vec![
                    (line_start(24) + pad.len(), 2, String::new()),
                    (line_start(2), pad.len(), String::new()),
                ],
                // Join line 15 to the checkpointed line 16, then split it again.
                vec![(line_start(16) - nl.len(), nl.len(), s(" "))],
                vec![(line_start(16) - nl.len(), 1, s(nl))],
            ];

            let mut engine = HighlightEngine::for_file(path, None, &registry);
            assert_eq!(engine.backend_name(), "textmate");
            let mut text = original.clone();
            let _ = spans_of(&mut engine, &text);
            for (n, batch) in batches.iter().enumerate() {
                let edits = apply(&mut text, batch);
                engine.notify_edits(&edits);
                assert_eq!(
                    spans_of(&mut engine, &text),
                    full_parse(&text),
                    "partial re-highlight after batch {n} ({nl:?} lines) must match a full parse"
                );
            }
            assert_eq!(text, original, "the batches undo each other");
        }
    }

    /// Bulk edits whose positions are all outside the cached viewport must
    /// not invalidate the cache at all on the tree-sitter / `Highlighter`
    /// path. (TextMate has a richer convergence model, but for both engines
    /// the regression to guard against is: "any bulk edit, even a tiny one,
    /// destroys the cache and forces a full reparse.")
    #[test]
    fn test_bulk_edit_outside_cache_keeps_textmate_partial_update() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("test.rs"), None, &registry);

        let mut content = String::new();
        for i in 0..400 {
            content.push_str(&format!("fn f_{i}() {{ let x = {i}; }}\n"));
        }
        let buffer = Buffer::from_str(&content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        // Warm a viewport near the start.
        let _ = engine.highlight_viewport(&buffer, 0, 200, &theme, 1_000);
        let bytes_before = match &engine {
            HighlightEngine::TextMate(h) => h.stats().bytes_parsed,
            _ => panic!("expected TextMate engine for .rs"),
        };

        // Apply a bulk edit far past the warmed viewport.
        let far_pos = buffer.len() - 100;
        engine.notify_edits(&[(far_pos, 3, 1)]);

        // Re-render the original viewport. The partial-update path must keep
        // parsed bytes well below a whole-file reparse.
        let _ = engine.highlight_viewport(&buffer, 0, 200, &theme, 1_000);
        let bytes_after = match &engine {
            HighlightEngine::TextMate(h) => h.stats().bytes_parsed,
            _ => unreachable!(),
        };
        let parsed = bytes_after - bytes_before;
        let buf_len = buffer.len();
        assert!(
            parsed < buf_len,
            "bulk edit outside the viewport must not force a whole-file \
             reparse (parsed {parsed}, file {buf_len})"
        );
    }

    /// Convergence budget caps per-pass work even when the parse state never
    /// agrees with any existing checkpoint. Without the cap, a non-converging
    /// edit would parse the rest of the file on every keystroke.
    #[test]
    fn test_partial_update_budget_caps_work() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("test.rs"), None, &registry);

        // Build a buffer comfortably larger than CONVERGENCE_BUDGET.
        let mut content = String::new();
        while content.len() < (CONVERGENCE_BUDGET * 4) {
            content.push_str("fn name() { let mut v = 0; v += 1; }\n");
        }
        let buffer = Buffer::from_str(&content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        let HighlightEngine::TextMate(ref mut tm) = engine else {
            panic!("expected TextMate engine for .rs");
        };

        // Warm cache (whole-file parse: the context covers the file).
        let _ = tm.highlight_viewport(&buffer, 0, 200, &theme, buffer.len());
        // Simulate an edit and force every checkpoint to disagree by clearing
        // their stored states. The convergence loop will look at each marker,
        // find the slot empty, and never converge.
        tm.notify_insert(100, 0);
        tm.checkpoint_states.clear();

        let bytes_before = tm.stats().bytes_parsed;
        let _ = tm.highlight_viewport(&buffer, 0, 200, &theme, buffer.len());
        let parsed = tm.stats().bytes_parsed - bytes_before;

        // Budget bounds the work to roughly CONVERGENCE_BUDGET past the dirty
        // point (plus the prefix back to the resume checkpoint). Allow a small
        // overshoot for the line that crossed the budget threshold.
        assert!(
            parsed <= CONVERGENCE_BUDGET + 4096,
            "partial update parsed {parsed}, expected <= {} \
             (budget {CONVERGENCE_BUDGET} + slack)",
            CONVERGENCE_BUDGET + 4096
        );

        // Budget hit must leave dirty_from set for follow-up passes.
        assert!(
            tm.dirty_from.is_some(),
            "budget exit must keep dirty_from set"
        );
    }

    /// Large files (above MAX_PARSE_BYTES) keep the existing windowed
    /// behaviour: parse range is bounded by ±context_bytes around the
    /// viewport, not the whole file.
    ///
    /// The viewport is placed past `MAX_PARSE_BYTES` so we exercise the
    /// "large file, no nearby checkpoint" branch in `find_parse_resume_point`
    /// — the symmetric branch that fires when `parse_end <= MAX_PARSE_BYTES`
    /// still parses from byte 0 even on big files (pre-existing behaviour,
    /// addressed in a later phase).
    #[test]
    fn test_large_file_uses_windowed_parse() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("test.rs"), None, &registry);

        // Build content well past MAX_PARSE_BYTES so we can put the viewport
        // beyond it.
        let line = "fn long_name_for_padding() { let v = 1; v + 1; }\n";
        let bytes_needed = MAX_PARSE_BYTES * 2;
        let lines_needed = bytes_needed / line.len() + 100;
        let mut content = String::with_capacity(lines_needed * line.len());
        for _ in 0..lines_needed {
            content.push_str(line);
        }
        assert!(content.len() > MAX_PARSE_BYTES * 2);
        let buffer = Buffer::from_str(&content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        let HighlightEngine::TextMate(ref mut tm) = engine else {
            panic!("expected TextMate engine for .rs");
        };

        // Viewport past MAX_PARSE_BYTES: parse_end > MAX_PARSE_BYTES, so the
        // resume-from-byte-0 fallback in find_parse_resume_point doesn't fire.
        let context_bytes = 10_000usize;
        let viewport_start = MAX_PARSE_BYTES + 200_000;
        let viewport_end = viewport_start + 1000;
        let _ = tm.highlight_viewport(&buffer, viewport_start, viewport_end, &theme, context_bytes);
        let parsed = tm.stats().bytes_parsed;

        // Windowed parse covers viewport ± context_bytes plus a tiny prefix
        // for the resume anchor. Allow generous slack (4×) but reject
        // anything close to whole-file.
        let window = (viewport_end - viewport_start) + 2 * context_bytes;
        assert!(
            parsed <= window * 4,
            "large file windowed parse should be ~{window} bytes, got {parsed} \
             (file {})",
            buffer.len()
        );
    }

    /// Regression for issue #899: a class field initialised with an arrow
    /// function that returns a template literal must not bleed string
    /// highlighting onto the rest of the class body. The user-reported
    /// repro pinned the syntect JavaScript grammar to a string state from
    /// the trailing `;` until EOF; the constructor keyword, comments, and
    /// the closing `}` were all painted as a string.
    #[test]
    fn test_javascript_template_literal_does_not_bleed() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("repro.js"), None, &registry);

        // Reproduction code from issue #899.
        let source = "class ExampleClass {\n\
                      \texampleFunction = exampleArg => `${exampleArg}`;\n\
                      \n\
                      \tconstructor() {\n\
                      \t\t// constructor body\n\
                      \t}\n\
                      \n\
                      \t/* multiline comment */\n\
                      }\n";
        let buffer = Buffer::from_str(source, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        let _ = engine.highlight_viewport(&buffer, 0, source.len(), &theme, 0);

        // The `constructor` keyword sits well after the template literal.
        // If string state bleeds, this position is reported as String.
        let ctor_pos = source.find("constructor").expect("locate constructor");
        let ctor_cat = engine.category_at_position(ctor_pos);
        assert_ne!(
            ctor_cat,
            Some(HighlightCategory::String),
            "constructor keyword must not inherit string state from earlier \
             template literal (got {:?})",
            ctor_cat,
        );

        // The closing brace of the class — the very last non-whitespace char
        // — also lives outside any string in correct JS.
        let last_brace = source.rfind('}').expect("locate closing brace");
        let brace_cat = engine.category_at_position(last_brace);
        assert_ne!(
            brace_cat,
            Some(HighlightCategory::String),
            "closing class brace must not be highlighted as string \
             (got {:?})",
            brace_cat,
        );
    }

    /// The closing `}` of a `${…}` template substitution and the closing
    /// backtick of the surrounding template literal must keep template
    /// string colouring — not inherit the `@variable` highlight from the
    /// substitution's expression. Tree-sitter-highlight emits one
    /// HighlightEnd event per started highlight; if the editor's
    /// span-flattening logic doesn't pop the inner `@variable` correctly
    /// when the substitution closes, the variable colour bleeds across
    /// `}` and the trailing `\`` until the next sibling capture (here,
    /// the `;` operator).
    #[test]
    fn test_javascript_template_substitution_closing_tokens_are_string() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("tmpl.js"), None, &registry);

        // Minimal template literal: `${name}` — wrapped in a statement so
        // the parser sees a complete program.
        let source = "const x = `${name}`;\n";
        let buffer = Buffer::from_str(source, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        let _ = engine.highlight_viewport(&buffer, 0, source.len(), &theme, 0);

        // Locate the closing `}` of the substitution and the closing
        // backtick of the template literal.
        let close_brace = source
            .find("}`")
            .expect("locate substitution closing brace");
        let close_backtick = close_brace + 1;

        // Sanity: the inner identifier `name` is correctly tagged as a
        // variable (this guards us against an unrelated regression where
        // the entire template gets typed wrong).
        let name_pos = source.find("name").expect("locate identifier");
        let name_cat = engine.category_at_position(name_pos);
        assert_eq!(
            name_cat,
            Some(HighlightCategory::Variable),
            "substitution identifier should be Variable (got {:?})",
            name_cat,
        );

        // The closing `}` and `` ` `` live inside the surrounding
        // `template_string` node, so tree-sitter assigns them the
        // `@string` capture. They must surface as String here — not
        // as Variable (the previous symptom of the bleed) and not as
        // None (which would make the editor render them with the
        // default foreground colour, equally wrong).
        let brace_cat = engine.category_at_position(close_brace);
        assert_eq!(
            brace_cat,
            Some(HighlightCategory::String),
            "closing }} of ${{…}} must be String (got {:?})",
            brace_cat,
        );
        let backtick_cat = engine.category_at_position(close_backtick);
        assert_eq!(
            backtick_cat,
            Some(HighlightCategory::String),
            "closing backtick of template literal must be String \
             (got {:?})",
            backtick_cat,
        );
    }
    fn declared_host_for_test() -> (GrammarRegistry, Theme) {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        (registry, theme)
    }

    fn category_at(
        spans: &[HighlightSpan],
        content: &str,
        needle: &str,
    ) -> Option<HighlightCategory> {
        let pos = content.find(needle).expect("needle in content");
        spans
            .iter()
            .find(|span| span.range.contains(&pos))
            .and_then(|span| span.category)
    }

    fn python_region(
        registry: &GrammarRegistry,
        start: usize,
        end: usize,
        prefix: usize,
        streams: &[u32],
    ) -> DeclaredRegion {
        DeclaredRegion {
            start,
            end,
            syntax_index: registry.embedded_syntax_index("calc.py"),
            prefix,
            streams: streams.to_vec(),
        }
    }

    /// A declared-region host colours the rows a plugin declared, past
    /// their prefix, and nothing else: the header above them and the
    /// comment box between them stay uncoloured, and the docstring the
    /// box interrupts is still one docstring on the row after it.
    #[test]
    fn test_declared_regions_colour_declared_rows_across_a_gap() {
        let (registry, theme) = declared_host_for_test();
        let header = "UNSTAGED\n";
        let code_a = "   1 +def f():\n   2 +    \"\"\"doc\n";
        let note = "  [ a note ]\n";
        let code_b = "   3 +    more\"\"\"\n   4 +    return 1\n";
        let content = format!("{header}{code_a}{note}{code_b}");
        let a = header.len()..header.len() + code_a.len();
        let b = a.end + note.len()..content.len();
        let mut engine = HighlightEngine::declared_region_host(
            registry.syntax_set_arc(),
            vec![
                python_region(&registry, a.start, a.end, 6, &[1]),
                python_region(&registry, b.start, b.end, 6, &[1]),
            ],
        );
        let buffer = Buffer::from_str(&content, 0, test_fs());
        let spans = engine.highlight_viewport(&buffer, 0, content.len(), &theme, 0);

        assert_eq!(
            category_at(&spans, &content, "def"),
            Some(HighlightCategory::Keyword)
        );
        assert_eq!(
            category_at(&spans, &content, "more"),
            Some(HighlightCategory::Comment)
        );
        assert_eq!(
            category_at(&spans, &content, "return"),
            Some(HighlightCategory::Keyword)
        );
        assert_eq!(category_at(&spans, &content, "UNSTAGED"), None);
        assert_eq!(category_at(&spans, &content, "a note"), None);
        // The gutter is not code: nothing is painted before the marker.
        let gutter = content.find("   1 +").unwrap();
        assert!(
            spans
                .iter()
                .all(|s| s.range.end <= gutter || s.range.start >= gutter + 6),
            "a span reached into the gutter: {spans:?}"
        );
    }

    /// The two sides of a hunk interleave row by row, and each is the
    /// contiguous text it came from: a docstring opened on an old-side
    /// row is still open on the next old-side row, while the new-side
    /// rows between them are code.
    #[test]
    fn test_declared_regions_keep_one_parser_per_stream() {
        let (registry, theme) = declared_host_for_test();
        let rows = ["-\"\"\"old\n", "+x = 1\n", "-still old\"\"\"\n", "+y = 2\n"];
        let content: String = rows.concat();
        let mut regions = Vec::new();
        let mut at = 0;
        for row in &rows {
            let stream = if row.starts_with('-') { 0 } else { 1 };
            regions.push(python_region(&registry, at, at + row.len(), 1, &[stream]));
            at += row.len();
        }
        let mut engine = HighlightEngine::declared_region_host(registry.syntax_set_arc(), regions);
        let buffer = Buffer::from_str(&content, 0, test_fs());
        let spans = engine.highlight_viewport(&buffer, 0, content.len(), &theme, 0);

        assert_eq!(
            category_at(&spans, &content, "still old"),
            Some(HighlightCategory::Comment)
        );
        assert_eq!(
            category_at(&spans, &content, "1\n"),
            Some(HighlightCategory::Number)
        );
        assert_eq!(
            category_at(&spans, &content, "2\n"),
            Some(HighlightCategory::Number)
        );
    }

    /// Regions ride the text on the engine's own marker list, the one
    /// that carries its parse checkpoints: an insertion before a region
    /// moves it, an insertion inside it grows it, and either way the rows
    /// it meant are still the ones coloured.
    #[test]
    fn test_declared_regions_follow_an_edit() {
        let (registry, theme) = declared_host_for_test();
        let code = "+def f():\n";
        let mut engine = HighlightEngine::declared_region_host(
            registry.syntax_set_arc(),
            vec![python_region(&registry, 0, code.len(), 1, &[])],
        );
        let buffer = Buffer::from_str(code, 0, test_fs());
        let spans = engine.highlight_viewport(&buffer, 0, code.len(), &theme, 0);
        assert_eq!(
            category_at(&spans, code, "def"),
            Some(HighlightCategory::Keyword)
        );

        let inserted = "HEADER\n";
        let content = format!("{inserted}{code}");
        engine.notify_insert(0, inserted.len());
        engine.invalidate_range(0..inserted.len());
        let buffer = Buffer::from_str(&content, 0, test_fs());
        let spans = engine.highlight_viewport(&buffer, 0, content.len(), &theme, 0);
        assert_eq!(
            category_at(&spans, &content, "def"),
            Some(HighlightCategory::Keyword)
        );
        assert_eq!(category_at(&spans, &content, "HEADER"), None);

        // A row inserted *inside* the region grows it, so the new row is
        // code too — the marker span's own semantics, not arithmetic here.
        let at = content.find("+def").unwrap() + "+def f():\n".len();
        let added = "+    return 1\n";
        let content = format!("{}{}{}", &content[..at], added, &content[at..]);
        engine.notify_insert(at, added.len());
        engine.invalidate_range(at..at + added.len());
        let buffer = Buffer::from_str(&content, 0, test_fs());
        let spans = engine.highlight_viewport(&buffer, 0, content.len(), &theme, 0);
        assert_eq!(
            category_at(&spans, &content, "return"),
            Some(HighlightCategory::Keyword),
            "a row inserted inside a region is still inside it afterwards"
        );
    }

    /// A language nothing in the set claims leaves the rows as they are;
    /// an overlapping region is dropped rather than fought over; and
    /// replacing the regions with none paints nothing.
    #[test]
    fn test_declared_regions_unknown_language_overlap_and_clearing() {
        let (registry, theme) = declared_host_for_test();
        assert_eq!(registry.embedded_syntax_index("notes.nosuchlanguage"), None);
        let code = "+def f():\n";
        let mut engine = HighlightEngine::declared_region_host(
            registry.syntax_set_arc(),
            vec![DeclaredRegion {
                start: 0,
                end: code.len(),
                syntax_index: None,
                prefix: 1,
                streams: Vec::new(),
            }],
        );
        let buffer = Buffer::from_str(code, 0, test_fs());
        let spans = engine.highlight_viewport(&buffer, 0, code.len(), &theme, 0);
        assert!(spans.is_empty(), "{spans:?}");

        // A second region starting inside the first is dropped; the first
        // still paints.
        engine.set_declared_regions(vec![
            python_region(&registry, 0, code.len(), 1, &[]),
            DeclaredRegion {
                start: 2,
                end: code.len(),
                syntax_index: None,
                prefix: 0,
                streams: Vec::new(),
            },
        ]);
        let spans = engine.highlight_viewport(&buffer, 0, code.len(), &theme, 0);
        assert_eq!(
            category_at(&spans, code, "def"),
            Some(HighlightCategory::Keyword)
        );

        engine.set_declared_regions(Vec::new());
        let spans = engine.highlight_viewport(&buffer, 0, code.len(), &theme, 0);
        assert!(spans.is_empty(), "{spans:?}");
    }

    /// The host resolves a region's language through the catalog first,
    /// so a path picks the same grammar the editor would open it with,
    /// and falls back to the grammar a bare token or extension names.
    #[test]
    fn test_embedded_syntax_index_resolves_paths_and_tokens() {
        let (registry, _) = declared_host_for_test();
        let names =
            |index: Option<usize>| index.map(|i| registry.syntax_set().syntaxes()[i].name.clone());
        assert_eq!(
            names(registry.embedded_syntax_index("src/main.rs")).as_deref(),
            Some("Rust")
        );
        assert_eq!(
            names(registry.embedded_syntax_index("py")).as_deref(),
            Some("Python")
        );
        // A region named by a path gets the grammar that file opens with.
        assert_eq!(
            names(registry.embedded_syntax_index("app.ts")).as_deref(),
            Some("TypeScript")
        );
        assert_eq!(
            names(registry.embedded_syntax_index("app.tsx")).as_deref(),
            Some("TypeScriptReact")
        );
        assert_eq!(registry.embedded_syntax_index("README"), None);
    }

    /// Inside a hunk, a removed line beginning `--` or an added one
    /// beginning `++` is source, not a file header: it is fed to the
    /// file's parser, so a construct it closes is closed for the rows
    /// after it.
    #[test]
    fn test_diff_hunk_rows_that_look_like_headers_are_source() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("commit.diff"), None, &registry);
        let content = "\
diff --git a/src/lib.rs b/src/lib.rs
--- a/src/lib.rs
+++ b/src/lib.rs
@@ -1,3 +1,3 @@
 /* open
--- close */
+let x = 1;
";
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let spans = engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);
        assert_eq!(
            category_at(&spans, content, "let x"),
            Some(HighlightCategory::Keyword),
            "the removed `-- close */` row must close the comment for the row after it"
        );
        // The real `+++` header, before any hunk, is still a header: it
        // keeps the metadata category rather than the insertion wash a
        // hunk row would get.
        assert_eq!(
            category_at(&spans, content, "+++ b/src"),
            Some(HighlightCategory::Type),
            "a file header outside a hunk keeps its header styling"
        );
    }

    /// A `diff --git` header the path rules cannot parse still hands the
    /// next file its own parser — resolved from the header's tail when
    /// an extension is in it — rather than running the previous file's.
    #[test]
    fn test_diff_unparsed_header_does_not_leak_the_previous_parser() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("commit.diff"), None, &registry);
        let content = "\
diff --git a/src/lib.rs b/src/lib.rs
@@ -1 +1 @@
+/* open
diff --git a/has space.py b/has space.py
@@ -1 +1 @@
+def f():
";
        let buffer = Buffer::from_str(content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();
        let spans = engine.highlight_viewport(&buffer, 0, buffer.len(), &theme, 0);
        assert_eq!(
            category_at(&spans, content, "def f"),
            Some(HighlightCategory::Keyword),
            "the second file must not be parsed by the first file's Rust parser"
        );
    }

    /// A JSON array of numbers on one line, `len` bytes long (issue #3631's
    /// reproduction: a minified data file is one enormous line).
    fn one_line_number_array(len: usize) -> String {
        let mut content = String::with_capacity(len + 16);
        content.push('[');
        let mut n = 1u64;
        while content.len() < len {
            content.push_str(&n.to_string());
            content.push(',');
            n += 1;
        }
        content.push_str("0]");
        content
    }

    /// Typing deep inside a long line re-parses its highlighted prefix, not
    /// the line up to the cursor. Before the cut, each keystroke re-parsed
    /// from the line's start: ~270 ms near the end of the first megabyte of a
    /// 50 MB array. Both parse modes: a file small enough to parse whole, and
    /// one parsed a viewport window at a time.
    #[test]
    fn typing_deep_in_a_long_line_parses_only_its_highlighted_prefix() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        for len in [900_000, 3 * MAX_PARSE_BYTES] {
            let mut engine = HighlightEngine::for_file(Path::new("data.json"), None, &registry);
            let HighlightEngine::TextMate(ref mut tm) = engine else {
                panic!("expected TextMate engine for .json");
            };
            let at = 600_000;
            let mut content = one_line_number_array(len);
            let viewport = at..at + 3_000;
            let buffer = Buffer::from_str(&content, 0, test_fs());
            let _ = tm.highlight_viewport(&buffer, viewport.start, viewport.end, &theme, 10_000);

            for i in 0..3 {
                let before = tm.stats().bytes_tokenized;
                content.insert(at + i, '7');
                let buffer = Buffer::from_str(&content, 0, test_fs());
                tm.notify_insert(at + i, 1);
                let _ =
                    tm.highlight_viewport(&buffer, viewport.start, viewport.end, &theme, 10_000);
                let tokenized = tm.stats().bytes_tokenized - before;
                assert!(
                    tokenized <= MAX_HIGHLIGHT_LINE_BYTES + 1,
                    "a keystroke {at} bytes into a {len}-byte line tokenized {tokenized} bytes"
                );
            }
        }
    }

    /// A long line is highlighted up to the cut and plain after it, whichever
    /// part of it a pass happened to read: the cut is a fixed distance into
    /// the line, so neither the viewport nor what the cache holds moves it.
    #[test]
    fn a_long_line_is_highlighted_up_to_the_cut_wherever_it_is_viewed() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let long = one_line_number_array(10 * MAX_HIGHLIGHT_LINE_BYTES);
        let content = format!("[\n{},\n2\n]\n", &long[1..long.len() - 1]);
        let line_start = 2;
        let cut = line_start + MAX_HIGHLIGHT_LINE_BYTES;
        let number_at = |spans: &[HighlightSpan], pos: usize| {
            spans.iter().any(|s| {
                s.range.start <= pos
                    && s.range.end > pos
                    && s.category == Some(HighlightCategory::Number)
            })
        };
        let digit_near =
            |pos: usize| pos + content[pos..].find(|c: char| c.is_ascii_digit()).unwrap();
        let before_cut = digit_near(cut - 100);
        let after_cut = digit_near(cut + 100);
        let deep = digit_near(line_start + 9 * MAX_HIGHLIGHT_LINE_BYTES);
        let last = content.rfind('2').unwrap();

        // Read three ways: a viewport at the cut, with only part of the line
        // in hand; the whole file in one pass; and from the cache after it.
        for (label, whole_file) in [("partial", false), ("whole", true), ("cached", true)] {
            let mut engine = HighlightEngine::for_file(Path::new("data.json"), None, &registry);
            let HighlightEngine::TextMate(ref mut tm) = engine else {
                panic!("expected TextMate engine for .json");
            };
            let buffer = Buffer::from_str(&content, 0, test_fs());
            let context = if whole_file { buffer.len() } else { 500 };
            let mut spans =
                tm.highlight_viewport(&buffer, cut - 1_000, cut + 1_000, &theme, context);
            if label == "cached" {
                spans = tm.highlight_viewport(&buffer, cut - 1_000, cut + 1_000, &theme, context);
            }
            assert!(
                number_at(&spans, before_cut),
                "{label}: a number before the cut is plain"
            );
            assert!(
                !number_at(&spans, after_cut),
                "{label}: a number past the cut is highlighted"
            );
            if whole_file {
                let spans = tm.highlight_viewport(&buffer, deep - 50, last + 1, &theme, 0);
                assert!(
                    !number_at(&spans, deep),
                    "{label}: deep in the line is highlighted"
                );
                assert!(
                    number_at(&spans, last),
                    "{label}: the line after the long one is plain"
                );
            }
        }
    }

    /// Wherever a long line is shown — scrolled to, typed in, viewed again —
    /// it is highlighted up to the cut and plain after it. Replays an editor
    /// session: opened at the top, scrolled down, a key typed.
    ///
    /// Two passes used to disagree. A pass ending inside a long line cached its
    /// end at the viewport with the state of the line's start, and the next
    /// frame extended from there, counting the cut from the viewport: the
    /// screen was coloured while scrolling, went plain on a keystroke (which
    /// re-parses from the line's start) and came back on the next scroll. And
    /// past the first megabyte, where a pass starts fresh above the viewport,
    /// everything was coloured, cut or no cut.
    #[test]
    fn a_long_line_is_plain_past_the_cut_however_it_is_reached() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let theme = Theme::load_builtin(theme::THEME_DARK).unwrap();
        let mut engine = HighlightEngine::for_file(Path::new("data.json"), None, &registry);
        let HighlightEngine::TextMate(ref mut tm) = engine else {
            panic!("expected TextMate engine for .json");
        };
        let content = one_line_number_array(3 * MAX_PARSE_BYTES);
        let buffer = Buffer::from_str(&content, 0, test_fs());
        let rows = 2_400;
        // As the editor asks: the viewport, a viewport's worth either side.
        let view = |tm: &mut TextMateEngine, buffer: &Buffer, top: usize| {
            let spans = tm.highlight_viewport(
                buffer,
                top.saturating_sub(rows),
                top + 2 * rows,
                &theme,
                10_000,
            );
            spans
                .iter()
                .filter(|s| {
                    s.category == Some(HighlightCategory::Number)
                        && s.range.start < top + rows
                        && s.range.end > top
                })
                .count()
        };

        for top in (0..MAX_HIGHLIGHT_LINE_BYTES - rows).step_by(rows) {
            assert!(
                view(tm, &buffer, top) > 0,
                "{top}: inside the prefix, plain"
            );
        }
        for top in [MAX_HIGHLIGHT_LINE_BYTES + 900, 600_000, 2 * MAX_PARSE_BYTES] {
            assert_eq!(
                view(tm, &buffer, top),
                0,
                "{top}: scrolled past the cut, highlighted"
            );
            let mut typed = content.clone();
            typed.insert(top + 10, '9');
            let typed = Buffer::from_str(&typed, 0, test_fs());
            tm.notify_insert(top + 10, 1);
            assert_eq!(
                view(tm, &typed, top),
                0,
                "{top}: typed past the cut, highlighted"
            );
            assert_eq!(view(tm, &typed, top), 0, "{top}: viewed again, highlighted");
            tm.notify_delete(top + 10, 1);
            assert_eq!(view(tm, &buffer, top), 0, "{top}: after undo, highlighted");
        }
    }
}
