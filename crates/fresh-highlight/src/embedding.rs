//! Embedded-language regions: blocks of a host document written in a
//! language the document names, such as a Markdown fence (```` ```rust ````),
//! a Vue `<script lang="ts">` block, or a file's hunks in a unified diff.
//!
//! The host grammar finds each region and scopes it, and the token naming
//! its language: [`EMBEDDING_SPECS`] says which scopes those are for each
//! host. A highlighter runs a child parser, in the region's language, over
//! the region's content lines, while the host parser goes on consuming
//! every line (only it knows where the region ends). Hosts whose second
//! language is fixed (HTML's `<style>` is CSS) embed it in the grammar
//! instead and need nothing here.
//!
//! How a line is classified, from the host's scope stack before and after
//! it (see `docs/internal/embedded-language-highlighting.md`):
//!
//! - in no region either side: a host line;
//! - in none before, in one after: the region opens. The line is the
//!   host's; the language token on it, or the kind's default language when
//!   there is none or it names no syntax, picks the child syntax;
//! - in the same region both sides: a content line, coloured by the child
//!   (by the host when the region has no child syntax);
//! - in one before, none after: the region's closing line, the host's.

use syntect::parsing::{Scope, ScopeStack, SyntaxSet};

use crate::Category;

/// How one host syntax delimits a kind of embedded region.
struct EmbeddingSpecDef {
    host_syntax: &'static str,
    /// Scope (prefix) the host keeps on the stack for the whole region,
    /// its delimiter lines included.
    region_scope: &'static str,
    /// Scope (prefix) the host gives the language token on the region's
    /// opening line.
    language_scope: &'static str,
    default_language: Option<&'static str>,
    content: ContentMode,
    retargets_on_same_line: bool,
}

/// A host may declare several region kinds (Vue: `<script>` and `<style>`),
/// each with its own default language. Region scopes of one host must not
/// be prefixes of each other.
const EMBEDDING_SPECS: &[EmbeddingSpecDef] = &[
    EmbeddingSpecDef {
        host_syntax: "Markdown",
        region_scope: "markup.raw.code-fence",
        language_scope: "constant.other.language-name",
        default_language: None,
        content: ContentMode::WholeLine,
        retargets_on_same_line: false,
    },
    EmbeddingSpecDef {
        host_syntax: "Vue",
        region_scope: "meta.embedded.block.script",
        language_scope: "constant.other.language-name",
        default_language: Some("js"),
        content: ContentMode::WholeLine,
        retargets_on_same_line: false,
    },
    EmbeddingSpecDef {
        host_syntax: "Vue",
        region_scope: "meta.embedded.block.style",
        language_scope: "constant.other.language-name",
        default_language: Some("css"),
        content: ContentMode::WholeLine,
        retargets_on_same_line: false,
    },
    EmbeddingSpecDef {
        host_syntax: "Fresh Diff",
        region_scope: "meta.embedded.block.diff",
        language_scope: "constant.other.language-name",
        default_language: None,
        content: ContentMode::UnifiedDiff,
        retargets_on_same_line: true,
    },
];

/// How a region's content lines are handed to its child parser.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContentMode {
    /// The whole line is the embedded language's (Markdown, Vue).
    WholeLine,
    /// A unified diff's rows: a source row (added, removed or context) goes
    /// to the child without its one-byte patch marker, and its source
    /// tokens keep the row's Inserted/Deleted wash as their background.
    /// Every other row (file and hunk headers, metadata) stays the host's
    /// and does not advance the child. See [`diff_change`].
    UnifiedDiff,
}

/// One kind of embedded region a host syntax has.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EmbeddingSpec {
    /// Scope (prefix) on the host's stack for the whole region, its
    /// delimiter lines included.
    pub region_scope: Scope,
    /// Scope (prefix) of the language token on the region's opening line.
    pub language_scope: Scope,
    /// The language token for a region whose opening line names none, or
    /// one that resolves to no syntax (Vue's `lang="ts"`, with no TypeScript
    /// TextMate grammar in the set: JavaScript approximates it). `None`: the
    /// region keeps the host's own styling (Markdown's raw code).
    pub default_language: Option<&'static str>,
    pub content: ContentMode,
    /// Whether one region of this kind can end and the next begin on a
    /// single line, so the language token is watched on every line rather
    /// than only where a region could open. A unified diff does that at
    /// each `diff --git`; fences and tags never do.
    pub retargets_on_same_line: bool,
}

/// The embedded-region kinds of one host syntax; empty for nearly all.
#[derive(Debug, Clone, Default)]
pub struct Embedding {
    specs: Vec<EmbeddingSpec>,
    /// The specs' language scopes, each once.
    language_scopes: Vec<Scope>,
    /// Whether any spec retargets on a single line.
    watch_everywhere: bool,
}

impl Embedding {
    /// The region kinds of the syntax named `host_syntax`.
    pub fn for_host(host_syntax: &str) -> Self {
        let specs: Vec<EmbeddingSpec> = EMBEDDING_SPECS
            .iter()
            .filter(|d| d.host_syntax == host_syntax)
            .map(|d| EmbeddingSpec {
                region_scope: static_scope(d.region_scope),
                language_scope: static_scope(d.language_scope),
                default_language: d.default_language,
                content: d.content,
                retargets_on_same_line: d.retargets_on_same_line,
            })
            .collect();
        let mut language_scopes: Vec<Scope> = Vec::new();
        for spec in &specs {
            if !language_scopes.contains(&spec.language_scope) {
                language_scopes.push(spec.language_scope);
            }
        }
        Embedding {
            watch_everywhere: specs.iter().any(|s| s.retargets_on_same_line),
            specs,
            language_scopes,
        }
    }

    /// Whether the host has no embedded regions.
    pub fn is_empty(&self) -> bool {
        self.specs.is_empty()
    }

    pub fn specs(&self) -> &[EmbeddingSpec] {
        &self.specs
    }

    /// The index of the spec whose region `scopes` is in, if any.
    pub fn active_region(&self, scopes: &ScopeStack) -> Option<usize> {
        self.specs
            .iter()
            .position(|spec| scopes_contain(scopes, spec.region_scope))
    }

    /// The language scopes to watch for on a line that starts in region
    /// `was_in`: on a line that could open a region, and on every line of a
    /// host whose regions hand over on one line — there the region scope is
    /// on the stack at both ends of the line, and only the new token says
    /// the child must be retargeted. None otherwise.
    pub fn watched(&self, was_in: Option<usize>) -> &[Scope] {
        if was_in.is_none() || self.watch_everywhere {
            &self.language_scopes
        } else {
            &[]
        }
    }
}

/// A scope from a selector written in this file.
fn static_scope(selector: &str) -> Scope {
    Scope::new(selector).expect("the embedding specs' scope selectors are valid scopes")
}

/// Whether a scope on `stack` has `scope` as a prefix.
pub fn scopes_contain(stack: &ScopeStack, scope: Scope) -> bool {
    stack.as_slice().iter().any(|s| scope.is_prefix_of(*s))
}

/// The language a region's language token names, as a token to look up:
/// trimmed, and without Pandoc's `{.…}` or the `language-` prefix of a fence
/// info string (`{.python}`, `language-c++`). `None` when nothing is left.
pub fn fence_language(raw: &str) -> Option<&str> {
    let token = raw.trim();
    let token = token
        .strip_prefix("{.")
        .and_then(|t| t.strip_suffix('}'))
        .unwrap_or(token);
    let token = token.strip_prefix("language-").unwrap_or(token);
    (!token.is_empty()).then_some(token)
}

/// The index in `syntaxes` of the syntax a language token (normalised by
/// [`fence_language`]) names: by extension or name (`rust`, `py`), else —
/// for a diff's token, which is the target file's path — by the path's
/// basename (`Makefile`) or extension. Plain text names none, so a region
/// in an unknown language keeps the host's styling rather than losing it to
/// a child parse that colours nothing.
pub fn find_embedded_syntax(syntaxes: &SyntaxSet, token: &str) -> Option<usize> {
    let plain = syntaxes.find_syntax_plain_text();
    let basename = token.rsplit(['/', '\\']).next().unwrap_or(token);
    let extension = basename.rsplit_once('.').map(|(_, ext)| ext);
    std::iter::once(token)
        .chain((basename != token).then_some(basename))
        .chain(extension)
        .find_map(|candidate| syntaxes.find_syntax_by_token(candidate))
        .filter(|found| !std::ptr::eq(*found, plain))
        .and_then(|found| {
            syntaxes
                .syntaxes()
                .iter()
                .position(|s| std::ptr::eq(s, found))
        })
}

/// Of a unified diff row's host categories, the change it marks: the first
/// Inserted or Deleted. A row the diff grammar scoped as a change is source
/// whatever it begins with — a removed line beginning `--` is code, not a
/// file header, and the grammar knows because it knows it is in a hunk.
pub fn diff_change(host: impl IntoIterator<Item = Category>) -> Option<Category> {
    host.into_iter()
        .find(|category| matches!(category, Category::Inserted | Category::Deleted))
}

/// Whether a unified diff row whose host categories mark `change` (see
/// [`diff_change`]) is source for the child parser: a change row, or a
/// context row (one beginning with a space).
pub fn is_diff_source_row(row: &str, change: Option<Category>) -> bool {
    change.is_some() || row.starts_with(' ')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fence_info_strings_are_normalised() {
        assert_eq!(fence_language(" rust "), Some("rust"));
        assert_eq!(fence_language("{.python}"), Some("python"));
        assert_eq!(fence_language("language-c++"), Some("c++"));
        assert_eq!(fence_language("  "), None);
        assert_eq!(fence_language("{.}"), None);
    }

    #[test]
    fn a_language_is_found_by_name_extension_or_path() {
        let syntaxes = SyntaxSet::load_defaults_newlines();
        let name =
            |token| find_embedded_syntax(&syntaxes, token).map(|i| &syntaxes.syntaxes()[i].name);
        assert_eq!(name("rust").map(String::as_str), Some("Rust"));
        assert_eq!(name("py").map(String::as_str), Some("Python"));
        assert_eq!(
            name("src/tools/check.py").map(String::as_str),
            Some("Python")
        );
        assert_eq!(name("build/Makefile").map(String::as_str), Some("Makefile"));
        assert_eq!(name("txt"), None, "plain text is no embedded language");
        assert_eq!(name("definitely-not-a-language"), None);
    }

    #[test]
    fn only_markdown_vue_and_diff_host_regions() {
        assert_eq!(Embedding::for_host("Markdown").specs().len(), 1);
        assert_eq!(Embedding::for_host("Vue").specs().len(), 2);
        assert_eq!(Embedding::for_host("Vue").watched(None).len(), 1);
        assert!(Embedding::for_host("Rust").is_empty());
        let diff = Embedding::for_host("Fresh Diff");
        assert_eq!(
            diff.watched(Some(0)).len(),
            1,
            "a diff retargets mid-region"
        );
        let markdown = Embedding::for_host("Markdown");
        assert!(markdown.watched(Some(0)).is_empty());
    }
}
