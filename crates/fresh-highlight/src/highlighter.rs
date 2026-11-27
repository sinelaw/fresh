//! Incremental highlighting of whole documents, worked in budgeted slices.
//!
//! A [`Highlighter`] holds documents by id, each as its lines (every line but
//! the last ends with its `\n`). Highlighting is a sequential parse from the
//! top: each line is parsed from the state the line before it leaves, and
//! comes out as category [`Run`]s. [`Highlighter::work`] advances it a byte
//! budget at a time, and [`Highlighter::take`] hands over the lines whose
//! runs changed.
//!
//! What a document keeps between slices:
//!
//! - `exact_until`: lines before it hold *exact* runs — those a parse of the
//!   whole current text from the top would give.
//! - Checkpoints: the parse state at a line's start, kept at least every
//!   [`CHECKPOINT_INTERVAL`] parsed bytes. Every checkpoint at or before
//!   `exact_until` is exact, so a parse resumes from the nearest one.
//! - `stale` ranges: after an edit, the lines past it that were exact keep
//!   their runs and checkpoints. They are right again as soon as the parse,
//!   coming down from the edit, reaches one of their checkpoints in the state
//!   stored there (it *converges*): from there to the end of the range
//!   nothing changed, so the reparse stops and `exact_until` jumps the range.
//!   Each range holds lines of one exact parse of an earlier text, none
//!   changed since; a parse passing through one without converging trims it.
//! - Provisional runs: a wanted range far past `exact_until` is parsed from a
//!   fresh state rather than waiting for the exact parse to get there, and
//!   marked not exact until it does.
//!
//! A host whose grammar marks embedded-language regions ([`crate::embedding`]:
//! Markdown fences, Vue `<script>`/`<style>` blocks, a diff's file hunks)
//! has their content lines coloured by a child parser in the region's
//! language, as the editor's native engine does. The child's state is part
//! of the parse state, so checkpoints carry it and a parse converges only
//! where the child (its language included) is the same too.
//!
//! Each line exactly parsed also records whether a region and a table body
//! were open before and after it ([`crate::structure`]); its [`Roles`] are
//! read off those records — its own and, for a table, its neighbours' —
//! once every line they depend on is exact, and are reported with its runs.
//!
//! A document opened in declared-regions mode ([`crate::declared`]) has no
//! grammar: its rows are coloured by the parsers of the regions declared
//! over it. Those parsers are part of the parse state too, and the regions
//! move with edits, so the rows past an edit keep the regions they were in
//! and a parse converges there as it does elsewhere.

use crate::declared::{self, DeclaredRegion, Region, RegionStatus, MAX_STREAMS};
use crate::embedding::{
    diff_change, fence_language, find_embedded_syntax, is_diff_source_row, scopes_contain,
    ContentMode, Embedding, EmbeddingSpec,
};
use crate::structure::{self, Edge, Roles, Transitions};
use crate::{highlighted_prefix, scope_to_category, Category, RowWash};
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt;
use std::ops::Range;
use syntect::parsing::{
    ParseState, ParsingError, Scope, ScopeError, ScopeStack, SyntaxReference, SyntaxSet,
};

/// Parsed bytes between checkpoints, at most (a line is not split).
const CHECKPOINT_INTERVAL: usize = 256;

/// How far past the first line without exact runs a wanted range may start
/// and still be parsed exactly, from there; one further away is parsed
/// provisionally.
const PROVISIONAL_DISTANCE: usize = 256 * 1024;

/// A run of a line's text in one category. Text outside every run is plain.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Run {
    /// Byte offset of the run within its line.
    pub start: u32,
    /// Length in bytes.
    pub len: u32,
    pub cat: Category,
    /// The category whose background the run sits on, when it is not
    /// `cat`'s own: a source token an embedded parser coloured in a diff
    /// row keeps the row's Inserted/Deleted wash under it.
    pub background: Option<Category>,
}

/// A line's runs, as [`Highlighter::take`] reports them.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineRuns {
    pub line: u32,
    /// Whether the runs are those of a parse from the top of the current
    /// text; a provisional parse's are not.
    pub exact: bool,
    pub runs: Vec<Run>,
    /// The line's role in a region and in a table; unknown for provisional
    /// runs, and until the lines the roles depend on are parsed exactly.
    pub roles: Roles,
}

#[derive(Debug)]
pub enum Error {
    /// No syntax of this name is in the syntax set.
    UnknownSyntax(String),
    UnknownDocument(u32),
    /// A document of this id is open already.
    DocumentOpen(u32),
    /// Regions were declared for a document opened with a grammar.
    NotDeclared(u32),
    /// A declared region reaches past the end of the text.
    RegionOutOfRange {
        doc: u32,
        /// The region's index in the list declared.
        index: usize,
        end: usize,
        len: usize,
    },
    /// The text would not fit `u32` byte offsets.
    TooLarge {
        doc: u32,
        len: usize,
    },
    /// An edit reaches past the end of the text.
    OutOfRange {
        doc: u32,
        end: usize,
        len: usize,
    },
    /// An edit's offset is inside a character.
    NotCharBoundary {
        doc: u32,
        offset: usize,
    },
    /// The grammar failed on a line; the document was closed.
    Parse {
        doc: u32,
        line: usize,
        error: ParsingError,
    },
    /// The grammar produced an impossible scope change on a line; the
    /// document was closed.
    Scope {
        doc: u32,
        line: usize,
        error: ScopeError,
    },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UnknownSyntax(name) => write!(f, "no syntax named {name:?}"),
            Error::UnknownDocument(doc) => write!(f, "no document {doc}"),
            Error::DocumentOpen(doc) => write!(f, "document {doc} is open already"),
            Error::NotDeclared(doc) => write!(
                f,
                "document {doc} has a grammar; only a document opened for declared regions takes them"
            ),
            Error::RegionOutOfRange {
                doc,
                index,
                end,
                len,
            } => write!(
                f,
                "document {doc}: declared region {index} ends at byte {end}, past the text's end ({len})"
            ),
            Error::TooLarge { doc, len } => write!(
                f,
                "document {doc} would be {len} bytes, more than {} can be highlighted",
                u32::MAX
            ),
            Error::OutOfRange { doc, end, len } => write!(
                f,
                "document {doc}: an edit ending at byte {end} is past its end ({len})"
            ),
            Error::NotCharBoundary { doc, offset } => write!(
                f,
                "document {doc}: byte {offset} is not on a character boundary"
            ),
            Error::Parse { doc, line, error } => write!(
                f,
                "document {doc}, line {line}: {error}; the document was closed"
            ),
            Error::Scope { doc, line, error } => write!(
                f,
                "document {doc}, line {line}: {error}; the document was closed"
            ),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Parse { error, .. } => Some(error),
            Error::Scope { error, .. } => Some(error),
            _ => None,
        }
    }
}

/// Highlights documents with the grammars of one syntax set.
pub struct Highlighter {
    syntaxes: SyntaxSet,
    /// Each syntax's embedded regions, by its index in `syntaxes`.
    embeddings: Vec<Embedding>,
    /// Each syntax's table body scope, if it scopes tables.
    table_scopes: Vec<Option<Scope>>,
    documents: BTreeMap<u32, Document>,
    categories: CategoryCache,
    languages: LanguageCache,
}

impl Highlighter {
    /// A highlighter over `syntaxes`, which must be a "newlines" syntax set
    /// (lines are parsed with their `\n`).
    pub fn new(syntaxes: SyntaxSet) -> Self {
        Highlighter {
            embeddings: syntaxes
                .syntaxes()
                .iter()
                .map(|syntax| Embedding::for_host(&syntax.name))
                .collect(),
            table_scopes: syntaxes
                .syntaxes()
                .iter()
                .map(|syntax| structure::table_body_scope(&syntax.name))
                .collect(),
            syntaxes,
            documents: BTreeMap::new(),
            categories: CategoryCache::default(),
            languages: LanguageCache::default(),
        }
    }

    /// Open document `doc` with `text`, highlighted with the syntax named
    /// `syntax`.
    pub fn open(&mut self, doc: u32, syntax: &str, text: &str) -> Result<(), Error> {
        if self.documents.contains_key(&doc) {
            return Err(Error::DocumentOpen(doc));
        }
        // The last of a name, as syntect's own lookups resolve it: grammars
        // added later shadow the defaults they replace.
        let syntax_index = self
            .syntaxes
            .syntaxes()
            .iter()
            .rposition(|s| s.name == syntax)
            .ok_or_else(|| Error::UnknownSyntax(syntax.to_string()))?;
        check_size(doc, text.len())?;
        self.documents
            .insert(doc, Document::new(syntax_index, Host::Grammar, text));
        Ok(())
    }

    /// Open document `doc` with `text` in declared-regions mode
    /// ([`crate::declared`]): plain text, coloured only in the regions
    /// [`Highlighter::set_regions`] declares. It has none yet.
    pub fn open_declared(&mut self, doc: u32, text: &str) -> Result<(), Error> {
        if self.documents.contains_key(&doc) {
            return Err(Error::DocumentOpen(doc));
        }
        let plain = self.syntaxes.find_syntax_plain_text();
        let syntax_index = self
            .syntaxes
            .syntaxes()
            .iter()
            .position(|s| std::ptr::eq(s, plain))
            .expect("the plain text syntax is in its set");
        check_size(doc, text.len())?;
        self.documents.insert(
            doc,
            Document::new(syntax_index, Host::Declared(Vec::new()), text),
        );
        Ok(())
    }

    /// Replace the regions declared over document `doc`, opened with
    /// [`Highlighter::open_declared`], with `regions` (byte ranges in its
    /// current text). Returns each region's [`RegionStatus`], in the order
    /// given: one that overlaps the region before it, or is empty, is
    /// dropped, and one in a language no syntax claims leaves its rows as
    /// they are. Every line is highlighted anew.
    pub fn set_regions(
        &mut self,
        doc: u32,
        regions: &[DeclaredRegion],
    ) -> Result<Vec<RegionStatus>, Error> {
        let d = self
            .documents
            .get_mut(&doc)
            .ok_or(Error::UnknownDocument(doc))?;
        if !matches!(d.host, Host::Declared(_)) {
            return Err(Error::NotDeclared(doc));
        }
        let len = d.len();
        if let Some((index, region)) = regions
            .iter()
            .enumerate()
            .find(|(_, region)| region.start.max(region.end) > len)
        {
            return Err(Error::RegionOutOfRange {
                doc,
                index,
                end: region.start.max(region.end),
                len,
            });
        }
        let (kept, statuses) = declared::resolve(regions, |language| {
            find_embedded_syntax(&self.syntaxes, language)
        });
        *d = Document {
            wanted: d.wanted.clone(),
            ..Document::new(d.syntax, Host::Declared(kept), &d.text())
        };
        Ok(statuses)
    }

    /// Replace document `doc`'s whole text. Its syntax and wanted range
    /// stay; the regions declared over it, which were ranges of the text
    /// replaced, do not.
    pub fn replace(&mut self, doc: u32, text: &str) -> Result<(), Error> {
        check_size(doc, text.len())?;
        let d = self.document(doc)?;
        let host = match d.host {
            Host::Grammar => Host::Grammar,
            Host::Declared(_) => Host::Declared(Vec::new()),
        };
        *d = Document {
            wanted: d.wanted.clone(),
            ..Document::new(d.syntax, host, text)
        };
        Ok(())
    }

    pub fn close(&mut self, doc: u32) -> Result<(), Error> {
        self.documents
            .remove(&doc)
            .map(drop)
            .ok_or(Error::UnknownDocument(doc))
    }

    /// Replace `delete_len` bytes at byte `start` of document `doc` with
    /// `insert`. Both ends must be character boundaries of the current text.
    pub fn edit(
        &mut self,
        doc: u32,
        start: usize,
        delete_len: usize,
        insert: &str,
    ) -> Result<(), Error> {
        let d = self.document(doc)?;
        let len = d.len();
        let end = start
            .checked_add(delete_len)
            .filter(|&end| end <= len)
            .ok_or(Error::OutOfRange {
                doc,
                end: start.saturating_add(delete_len),
                len,
            })?;
        check_size(doc, len - delete_len + insert.len())?;
        let (first, first_at) = d.locate(start);
        let (last, last_at) = d.locate(end);
        if !d.lines[first].text.is_char_boundary(first_at) {
            return Err(Error::NotCharBoundary { doc, offset: start });
        }
        if !d.lines[last].text.is_char_boundary(last_at) {
            return Err(Error::NotCharBoundary { doc, offset: end });
        }
        d.splice(first, first_at, last, last_at, insert);
        if let Host::Declared(regions) = &mut d.host {
            declared::shift(regions, start, delete_len, insert.len());
        }
        Ok(())
    }

    /// Have [`Highlighter::work`] parse document `doc`'s lines `from..to`
    /// (end exclusive) before anything else. Replaces the range wanted
    /// before.
    pub fn want(&mut self, doc: u32, from: usize, to: usize) -> Result<(), Error> {
        self.document(doc)?.wanted = from..to;
        Ok(())
    }

    /// Parse about `budget` bytes: the wanted ranges first, then on towards
    /// each document's end. Returns whether work remains.
    ///
    /// A document the grammar fails on is closed, and the error names it.
    pub fn work(&mut self, budget: usize) -> Result<bool, Error> {
        let mut spent = 0;
        while spent < budget {
            let Some((doc, task)) = self.next_task() else {
                break;
            };
            let Highlighter {
                syntaxes,
                embeddings,
                table_scopes,
                documents,
                categories,
                languages,
            } = self;
            let d = documents
                .get_mut(&doc)
                .expect("a task is for an open document");
            let mut parser = Parser {
                syntax: &syntaxes.syntaxes()[d.syntax],
                embedding: &embeddings[d.syntax],
                table_scope: table_scopes[d.syntax],
                syntaxes,
                categories,
                languages,
            };
            let done = match task {
                Task::Exact { until } => d.parse_exact(until, budget - spent, &mut parser),
                Task::Provisional(lines) => d.parse_provisional(lines, &mut parser),
            };
            match done {
                Ok(bytes) => spent += bytes,
                Err((line, error)) => {
                    self.documents.remove(&doc);
                    return Err(match error {
                        LineError::Parse(error) => Error::Parse { doc, line, error },
                        LineError::Scope(error) => Error::Scope { doc, line, error },
                    });
                }
            }
        }
        Ok(self.documents.values().any(Document::unfinished))
    }

    /// The lines of document `doc` whose runs changed since the last take,
    /// in line order.
    pub fn take(&mut self, doc: u32) -> Result<Vec<LineRuns>, Error> {
        let d = self.document(doc)?;
        let changed = std::mem::take(&mut d.changed);
        Ok(changed
            .into_iter()
            .map(|line| {
                let Line {
                    highlight, roles, ..
                } = &d.lines[line];
                let highlight = highlight
                    .as_ref()
                    .expect("a line is marked changed when it is highlighted");
                LineRuns {
                    line: to_u32(line),
                    exact: matches!(highlight.parse, Parse::Exact(_)),
                    runs: highlight.runs.clone(),
                    roles: *roles,
                }
            })
            .collect())
    }

    fn document(&mut self, doc: u32) -> Result<&mut Document, Error> {
        self.documents
            .get_mut(&doc)
            .ok_or(Error::UnknownDocument(doc))
    }

    /// The next thing to parse: a document's wanted range, else the rest of
    /// a document.
    fn next_task(&self) -> Option<(u32, Task)> {
        let wanted = self
            .documents
            .iter()
            .find_map(|(&doc, d)| d.wanted_task().map(|task| (doc, task)));
        wanted.or_else(|| {
            self.documents
                .iter()
                .find(|(_, d)| d.unfinished())
                .map(|(&doc, d)| {
                    (
                        doc,
                        Task::Exact {
                            until: d.lines.len(),
                        },
                    )
                })
        })
    }
}

fn check_size(doc: u32, len: usize) -> Result<(), Error> {
    u32::try_from(len)
        .map(drop)
        .map_err(|_| Error::TooLarge { doc, len })
}

/// A line number or in-line offset as reported. Documents are smaller than
/// `u32::MAX` bytes ([`check_size`]), so these fit.
fn to_u32(n: usize) -> u32 {
    u32::try_from(n).expect("documents are under 4 GiB")
}

enum Task {
    /// Parse exactly, from the nearest checkpoint, up to this line.
    Exact { until: usize },
    /// Parse these lines from a fresh state.
    Provisional(Range<usize>),
}

struct Document {
    /// Index of the syntax in the syntax set: plain text for a declared
    /// host.
    syntax: usize,
    host: Host,
    /// Never empty: an empty text is one empty line.
    lines: Vec<Line>,
    exact_until: usize,
    /// Sorted, disjoint, and all past `exact_until`; see the module docs.
    stale: Vec<Range<usize>>,
    wanted: Range<usize>,
    /// Lines whose runs changed since the last take.
    changed: BTreeSet<usize>,
}

/// Where a document's colours come from.
enum Host {
    /// Its syntax's grammar, and the embedded regions that finds.
    Grammar,
    /// The regions declared over it, sorted and disjoint ([`declared`]).
    Declared(Vec<Region>),
}

/// How one line is coloured.
#[derive(Clone, Copy)]
enum Row<'a> {
    /// By the document's grammar.
    Grammar,
    /// By the declared region the line's first byte lies in, if any.
    Declared(Option<&'a Region>),
}

struct Line {
    text: String,
    /// `None` until the line is parsed after its text last changed.
    highlight: Option<Highlight>,
    /// Settled from the exact parse's transitions once every line they
    /// depend on is exact (see [`Document::settle_roles`]).
    roles: Roles,
    /// The parse state at the line's start.
    checkpoint: Option<Box<State>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Highlight {
    runs: Vec<Run>,
    parse: Parse,
}

/// Which parse a line's highlight is from.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Parse {
    /// A parse from the top of the current text, with the structure
    /// transitions it saw around the line.
    Exact(Transitions),
    /// A parse from a fresh state in mid-document.
    Provisional,
}

/// The parse state at a line boundary: the host parser's, the child
/// parser's while in an embedded region that has one, and a declared host's
/// stream parsers.
#[derive(Debug, Clone, PartialEq, Eq)]
struct State {
    parse: ParseState,
    scopes: ScopeStack,
    embedded: Option<Embedded>,
    /// A declared host's live parsers, by stream, the most recently fed
    /// last; at most [`MAX_STREAMS`]. Empty for a grammar host.
    streams: Vec<(u32, Embedded)>,
}

/// An embedded region's child parser.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Embedded {
    /// Index of the child's syntax in the syntax set. Part of the state's
    /// identity: two parses that entered a region in different languages
    /// have not converged, whatever their parsers' states.
    syntax: usize,
    parse: ParseState,
    scopes: ScopeStack,
}

impl Embedded {
    /// A parser at the start of a text in syntax `syntax`.
    fn start(syntax: usize, syntaxes: &SyntaxSet) -> Self {
        Embedded {
            syntax,
            parse: ParseState::new(&syntaxes.syntaxes()[syntax]),
            scopes: ScopeStack::new(),
        }
    }
}

impl State {
    fn start(syntax: &SyntaxReference) -> Self {
        State {
            parse: ParseState::new(syntax),
            scopes: ScopeStack::new(),
            embedded: None,
            streams: Vec::new(),
        }
    }

    /// The parser of `stream`, taken out for a row: the one it left off
    /// with, unless that one parses another syntax, which cannot go on.
    /// Else a fresh one.
    fn take_stream(&mut self, stream: u32, syntax: usize, syntaxes: &SyntaxSet) -> Embedded {
        self.streams
            .iter()
            .position(|(id, _)| *id == stream)
            .map(|at| self.streams.remove(at).1)
            .filter(|parser| parser.syntax == syntax)
            .unwrap_or_else(|| Embedded::start(syntax, syntaxes))
    }

    /// Put `stream`'s parser back, as the most recently fed; the least
    /// recently fed goes once there are more than [`MAX_STREAMS`].
    fn keep_stream(&mut self, stream: u32, parser: Embedded) {
        self.streams.push((stream, parser));
        if self.streams.len() > MAX_STREAMS {
            self.streams.remove(0);
        }
    }
}

impl Line {
    fn new(text: String) -> Self {
        Line {
            text,
            highlight: None,
            roles: Roles::Unknown,
            checkpoint: None,
        }
    }
}

impl Highlight {
    /// The structure transitions around the line, if it is parsed exactly.
    fn transitions(&self) -> Option<Transitions> {
        match self.parse {
            Parse::Exact(transitions) => Some(transitions),
            Parse::Provisional => None,
        }
    }
}

/// `text` as lines, each with its `\n`. `ends_document`: the text is the end
/// of the document, so it ends with a line of its own without one (empty
/// when the text ends with `\n`, or is empty).
fn split_lines(text: &str, ends_document: bool) -> Vec<String> {
    let mut lines: Vec<String> = text.split_inclusive('\n').map(String::from).collect();
    if ends_document && (text.is_empty() || text.ends_with('\n')) {
        lines.push(String::new());
    }
    lines
}

/// `ranges` without the lines of `removed`, the lines after it moved to
/// follow the `inserted` lines put in its place.
fn splice_ranges(
    ranges: Vec<Range<usize>>,
    removed: &Range<usize>,
    inserted: usize,
) -> Vec<Range<usize>> {
    let moved = |line: usize| line - removed.end + removed.start + inserted;
    ranges
        .into_iter()
        .flat_map(|r| {
            [
                r.start..r.end.min(removed.start),
                moved(r.start.max(removed.end))..moved(r.end.max(removed.end)),
            ]
        })
        .filter(|r| !r.is_empty())
        .collect()
}

impl Document {
    fn new(syntax: usize, host: Host, text: &str) -> Self {
        Document {
            syntax,
            host,
            lines: split_lines(text, true).into_iter().map(Line::new).collect(),
            exact_until: 0,
            stale: Vec::new(),
            wanted: 0..0,
            changed: BTreeSet::new(),
        }
    }

    fn len(&self) -> usize {
        self.lines.iter().map(|l| l.text.len()).sum()
    }

    fn text(&self) -> String {
        self.lines.iter().map(|l| l.text.as_str()).collect()
    }

    /// The byte offset of `line`'s start, where a declared host needs it to
    /// find the line's region; `None` for a grammar host, which does not.
    fn row_offset(&self, line: usize) -> Option<usize> {
        match self.host {
            Host::Grammar => None,
            Host::Declared(_) => Some(self.lines[..line].iter().map(|l| l.text.len()).sum()),
        }
    }

    /// How the line at `offset` (from [`Document::row_offset`]) is
    /// coloured.
    fn row(&self, offset: Option<usize>) -> Row<'_> {
        match (&self.host, offset) {
            (Host::Grammar, None) => Row::Grammar,
            (Host::Declared(regions), Some(offset)) => {
                Row::Declared(declared::region_at(regions, offset))
            }
            _ => unreachable!("row_offset has an offset exactly for a declared host"),
        }
    }

    fn unfinished(&self) -> bool {
        self.exact_until < self.lines.len()
    }

    /// The line holding byte `pos` (at most the text's length), and the
    /// offset in it. A line's end, its `\n` passed, is the next line's start.
    fn locate(&self, pos: usize) -> (usize, usize) {
        let last = self.lines.len() - 1;
        self.lines
            .iter()
            .enumerate()
            .scan(0, |start, (i, line)| {
                let line_start = *start;
                *start += line.text.len();
                Some((i, line_start, *start))
            })
            .find(|&(i, _, end)| pos < end || i == last)
            .map(|(i, line_start, _)| (i, pos - line_start))
            .expect("a document has a line")
    }

    /// Replace the text from `first_at` in line `first` to `last_at` in line
    /// `last` with `insert`.
    fn splice(&mut self, first: usize, first_at: usize, last: usize, last_at: usize, insert: &str) {
        let joined = [
            &self.lines[first].text[..first_at],
            insert,
            &self.lines[last].text[last_at..],
        ]
        .concat();
        let mut lines: Vec<Line> = split_lines(&joined, last + 1 == self.lines.len())
            .into_iter()
            .map(Line::new)
            .collect();
        // The state at the first line's start does not depend on the line.
        lines[0].checkpoint = self.lines[first].checkpoint.take();
        let inserted = lines.len();
        let removed = first..last + 1;
        self.lines.splice(removed.clone(), lines);

        let moved = |line: usize| line - removed.end + removed.start + inserted;
        // The exact lines past the edit may be exact again once the parse
        // converges on them: they become the first stale range.
        let exact_after =
            (self.exact_until > removed.end).then(|| moved(removed.end)..moved(self.exact_until));
        self.stale = exact_after
            .into_iter()
            .chain(splice_ranges(
                std::mem::take(&mut self.stale),
                &removed,
                inserted,
            ))
            .collect();
        self.exact_until = self.exact_until.min(first);
        self.changed = std::mem::take(&mut self.changed)
            .into_iter()
            .filter(|line| !removed.contains(line))
            .map(|line| {
                if line < removed.start {
                    line
                } else {
                    moved(line)
                }
            })
            .collect();
    }

    /// What parsing the wanted range takes, if it is not parsed already.
    fn wanted_task(&self) -> Option<Task> {
        let end = self.wanted.end.min(self.lines.len());
        let start = self.wanted.start.max(self.exact_until);
        if start >= end {
            return None;
        }
        let far = self.lines[self.exact_until..start]
            .iter()
            .try_fold(0usize, |distance, line| {
                let distance = distance + line.text.len();
                (distance <= PROVISIONAL_DISTANCE).then_some(distance)
            })
            .is_none();
        if !far {
            return Some(Task::Exact { until: end });
        }
        (start..end)
            .find(|&line| self.lines[line].highlight.is_none())
            .map(|first| Task::Provisional(first..end))
    }

    /// Parse exactly towards line `until`, from the nearest checkpoint at or
    /// before `exact_until`, for about `budget` bytes but at least one line
    /// past `exact_until`. Stops early when the parse converges on a stale
    /// range. Settles the roles the parse may have changed. Returns the bytes
    /// parsed.
    fn parse_exact(
        &mut self,
        until: usize,
        budget: usize,
        parser: &mut Parser,
    ) -> Result<usize, (usize, LineError)> {
        let (resume, mut state) = (0..=self.exact_until)
            .rev()
            .find_map(|line| {
                self.lines[line]
                    .checkpoint
                    .as_deref()
                    .map(|state| (line, state.clone()))
            })
            .unwrap_or_else(|| (0, State::start(parser.syntax)));
        let progress_from = self.exact_until;
        let mut offset = self.row_offset(resume);
        let mut line = resume;
        let mut spent = 0;
        let mut since_checkpoint = 0;
        let stop = loop {
            if line > resume
                && line < self.lines.len()
                && self.arrive(line, &state, &mut since_checkpoint)
            {
                break line;
            }
            self.exact_until = self.exact_until.max(line);
            if line >= until
                || line >= self.lines.len()
                || (spent >= budget && line > progress_from)
            {
                break line;
            }
            let text = &self.lines[line].text;
            let parsed = parser
                .line(&mut state, text, self.row(offset))
                .map_err(|e| (line, e))?;
            offset = offset.map(|at| at + text.len());
            spent += parsed.bytes;
            since_checkpoint += parsed.bytes;
            self.set_highlight(
                line,
                Highlight {
                    runs: parsed.runs,
                    parse: Parse::Exact(parsed.transitions),
                },
            );
            line += 1;
        };
        // The roles that can have changed: the parsed lines', their
        // neighbours' (whose table roles read theirs), and that of the last
        // exact line, whose next line may have become exact or stopped
        // being so when a converged stale range ended.
        let tables = parser.table_scope.is_some();
        let lines = self.lines.len();
        (resume.saturating_sub(1)..=stop)
            .chain(self.exact_until.checked_sub(1))
            .filter(|&line| line < lines)
            .for_each(|line| self.settle_roles(line, tables));
        Ok(spent)
    }

    /// Settle `line`'s roles from the exact parse: known when it and the
    /// lines they depend on are exact, unknown otherwise. `tables`: the
    /// syntax scopes tables, so a line's roles also depend on the line after
    /// it.
    fn settle_roles(&mut self, line: usize, tables: bool) {
        if self.lines[line].highlight.is_some() {
            let roles = self.exact_roles(line, tables).unwrap_or(Roles::Unknown);
            self.set_roles(line, roles);
        }
    }

    /// `line`'s roles, if it and the lines they depend on are exact: its
    /// region role is its own, its table role also reads the lines either
    /// side of it (see [`crate::structure`]).
    fn exact_roles(&self, line: usize, tables: bool) -> Option<Roles> {
        let this = self.exact_transitions(line)?;
        let table = if tables {
            let next = match line + 1 {
                end if end == self.lines.len() => None,
                next => Some(self.exact_transitions(next)?.table),
            };
            let prev = line.checked_sub(1).map(|prev| {
                self.exact_transitions(prev)
                    .expect("the lines before an exact line are exact")
                    .table
            });
            structure::table_line(prev, this.table, next)
        } else {
            None
        };
        Some(Roles::Known {
            region: structure::region_role(this.region),
            table,
        })
    }

    /// The structure transitions around `line`, if it is exact.
    fn exact_transitions(&self, line: usize) -> Option<Transitions> {
        (line < self.exact_until).then(|| {
            self.lines[line]
                .highlight
                .as_ref()
                .and_then(Highlight::transitions)
                .expect("a line before exact_until is parsed exactly")
        })
    }

    /// The exact parse reached the start of `line` in `state`: check it
    /// against the line's checkpoint, or keep it as one. Returns whether the
    /// parse converged, which makes the stale range it is in exact.
    fn arrive(&mut self, line: usize, state: &State, since_checkpoint: &mut usize) -> bool {
        match &mut self.lines[line].checkpoint {
            Some(stored) if **stored == *state => {
                *since_checkpoint = 0;
                if let Some(range) = self.stale.first().filter(|r| r.contains(&line)) {
                    self.exact_until = range.end;
                    self.stale.remove(0);
                    return true;
                }
            }
            Some(stored) => {
                **stored = state.clone();
                *since_checkpoint = 0;
            }
            slot @ None if *since_checkpoint >= CHECKPOINT_INTERVAL => {
                *slot = Some(Box::new(state.clone()));
                *since_checkpoint = 0;
            }
            None => {}
        }
        // The line's checkpoint is now this parse's: a stale range starts
        // past it.
        while let Some(first) = self.stale.first_mut() {
            if first.start > line {
                break;
            }
            if first.end > line + 1 {
                first.start = line + 1;
                break;
            }
            self.stale.remove(0);
        }
        false
    }

    /// Parse `lines` from a fresh state, as provisional runs. Returns the
    /// bytes parsed.
    fn parse_provisional(
        &mut self,
        lines: Range<usize>,
        parser: &mut Parser,
    ) -> Result<usize, (usize, LineError)> {
        let mut state = State::start(parser.syntax);
        let mut offset = self.row_offset(lines.start);
        let mut spent = 0;
        for line in lines.clone() {
            let text = &self.lines[line].text;
            let parsed = parser
                .line(&mut state, text, self.row(offset))
                .map_err(|e| (line, e))?;
            offset = offset.map(|at| at + text.len());
            spent += parsed.bytes;
            self.set_highlight(
                line,
                Highlight {
                    runs: parsed.runs,
                    parse: Parse::Provisional,
                },
            );
            self.set_roles(line, Roles::Unknown);
        }
        // Their runs are no longer the stale ranges' parse.
        self.stale = splice_ranges(std::mem::take(&mut self.stale), &lines, lines.len());
        Ok(spent)
    }

    fn set_highlight(&mut self, line: usize, highlight: Highlight) {
        let slot = &mut self.lines[line].highlight;
        if slot.as_ref() != Some(&highlight) {
            *slot = Some(highlight);
            self.changed.insert(line);
        }
    }

    fn set_roles(&mut self, line: usize, roles: Roles) {
        let slot = &mut self.lines[line].roles;
        if *slot != roles {
            *slot = roles;
            self.changed.insert(line);
        }
    }
}

enum LineError {
    Parse(ParsingError),
    Scope(ScopeError),
}

struct Parsed {
    runs: Vec<Run>,
    /// The line's bytes, as the work budget counts them.
    bytes: usize,
    transitions: Transitions,
}

/// What parsing a line needs besides its document.
struct Parser<'a> {
    syntaxes: &'a SyntaxSet,
    syntax: &'a SyntaxReference,
    /// The embedded regions of `syntax`.
    embedding: &'a Embedding,
    /// The table body scope of `syntax`, if it scopes tables.
    table_scope: Option<Scope>,
    categories: &'a mut CategoryCache,
    languages: &'a mut LanguageCache,
}

/// Part of a line in a category, as a parser reports it.
type Span = (Range<usize>, Category);

impl Parser<'_> {
    /// Parse one line (`text`, with its `\n` unless it is the document's
    /// last) on from `state`, coloured as `row` says.
    ///
    /// Only the line's [`highlighted_prefix`] is parsed, as ending there; the
    /// rest of a longer line has no runs, and the next line goes on from the
    /// state the cut leaves — as the native engine does. A row a diff
    /// category washes is filled to its end ([`RowWash`]).
    fn line(&mut self, state: &mut State, text: &str, row: Row) -> Result<Parsed, LineError> {
        let content = text.trim_end_matches(['\r', '\n']);
        let prefix = highlighted_prefix(content);
        let cut = prefix.len() < content.len();
        let input: Cow<str> = match (cut, text.ends_with('\n')) {
            (false, false) => Cow::Borrowed(prefix),
            (false, true) if text.len() == prefix.len() + 1 => Cow::Borrowed(text),
            _ => Cow::Owned(format!("{prefix}\n")),
        };
        let end = prefix.len();
        let mut runs = Vec::new();
        let mut wash = RowWash::new(0..end, |range, cat, background| {
            push_run(&mut runs, range, cat, background)
        });
        let before = self.open_blocks(&state.scopes);
        match row {
            Row::Grammar if self.embedding.is_empty() => {
                self.spans(
                    &mut state.parse,
                    &mut state.scopes,
                    &input,
                    end,
                    &[],
                    |range, cat| wash.emit(range, cat, None),
                )?;
            }
            Row::Grammar => self.host_line(state, &input, end, &mut wash)?,
            Row::Declared(region) => self.declared_row(state, region, &input, end, &mut wash)?,
        }
        wash.finish();
        let after = self.open_blocks(&state.scopes);
        Ok(Parsed {
            runs,
            bytes: input.len(),
            transitions: Transitions {
                region: Edge {
                    before: before.region,
                    after: after.region,
                },
                table: Edge {
                    before: before.table,
                    after: after.table,
                },
            },
        })
    }

    /// Whether an embedded region and a table body are open in the host's
    /// `scopes`.
    fn open_blocks(&self, scopes: &ScopeStack) -> OpenBlocks {
        OpenBlocks {
            region: self.embedding.active_region(scopes).is_some(),
            table: self
                .table_scope
                .is_some_and(|scope| scopes_contain(scopes, scope)),
        }
    }

    /// [`Parser::line`] for a declared host's row, as the native engine does
    /// it: a row in a `region` with a language is handed, its prefix
    /// skipped, to the parser of each stream the region names, and coloured
    /// by the first; any other row keeps the styling it has and advances no
    /// parser — nor does a row too short for the prefix, or whose prefix
    /// ends inside a character.
    fn declared_row<F: FnMut(Range<usize>, Category, Option<Category>)>(
        &mut self,
        state: &mut State,
        region: Option<&Region>,
        input: &str,
        end: usize,
        wash: &mut RowWash<F>,
    ) -> Result<(), LineError> {
        let Some((region, syntax)) = region.and_then(|r| Some((r, r.syntax?))) else {
            return Ok(());
        };
        let at = region.prefix;
        if end < at || !input.is_char_boundary(at) {
            return Ok(());
        }
        for (k, &stream) in region.streams.iter().enumerate() {
            let mut parser = state.take_stream(stream, syntax, self.syntaxes);
            let parsed = self.spans(
                &mut parser.parse,
                &mut parser.scopes,
                &input[at..],
                end - at,
                &[],
                |range, cat| {
                    if k == 0 {
                        wash.emit(range.start + at..range.end + at, cat, None)
                    }
                },
            );
            state.keep_stream(stream, parser);
            parsed?;
        }
        Ok(())
    }

    /// [`Parser::line`] for a host with embedded regions, as the native
    /// engine does it. The host parses every line, since only it knows
    /// where a region ends, and its scope stack before and after the line
    /// says whether the line opens a region (and in what language), is a
    /// region's content, which the region's child parser colours when it
    /// has one, or closes it; see [`crate::embedding`].
    fn host_line<F: FnMut(Range<usize>, Category, Option<Category>)>(
        &mut self,
        state: &mut State,
        input: &str,
        end: usize,
        wash: &mut RowWash<F>,
    ) -> Result<(), LineError> {
        let embedding = self.embedding;
        let was_in = embedding.active_region(&state.scopes);
        let mut host = Vec::new();
        let language = self.spans(
            &mut state.parse,
            &mut state.scopes,
            input,
            end,
            embedding.watched(was_in),
            |range, cat| host.push((range, cat)),
        )?;
        let now_in = embedding.active_region(&state.scopes);
        match (was_in, now_in) {
            (Some(i), Some(j)) if i == j => {
                let spec = &embedding.specs()[i];
                if language.is_some() && spec.retargets_on_same_line {
                    // One region handed over to the next on this line (a
                    // diff's next file): its token restarts the child, and
                    // the line itself is the host's.
                    state.embedded = self.region_parser(input, language, spec);
                } else if let Some(child) = &mut state.embedded {
                    return self.content_line(child, spec, input, end, &host, wash);
                }
                // Else the region has no child: the line keeps the host's
                // styling.
            }
            (None, Some(j)) => {
                state.embedded = self.region_parser(input, language, &embedding.specs()[j]);
            }
            // One kind of region closed and another opened on the line.
            // The line's language token was not watched, so the new region
            // is in its default language.
            (Some(_), Some(j)) => {
                state.embedded = self.region_parser(input, None, &embedding.specs()[j]);
            }
            (Some(_), None) => state.embedded = None,
            (None, None) => {}
        }
        emit_all(wash, &host);
        Ok(())
    }

    /// A region's content line, coloured by its child parser `child`. The
    /// host's spans `host` colour what the child does not see: a diff row's
    /// patch marker, and a diff row that is not source (a file or hunk
    /// header), which leaves the child as it was.
    fn content_line<F: FnMut(Range<usize>, Category, Option<Category>)>(
        &mut self,
        child: &mut Embedded,
        spec: &EmbeddingSpec,
        input: &str,
        end: usize,
        host: &[Span],
        wash: &mut RowWash<F>,
    ) -> Result<(), LineError> {
        let (at, background) = match spec.content {
            ContentMode::WholeLine => (0, None),
            ContentMode::UnifiedDiff => {
                let change = diff_change(host.iter().map(|&(_, cat)| cat));
                // The child sees the row past its one-byte marker.
                let source = is_diff_source_row(input, change) && end >= 1;
                if !(source && input.is_char_boundary(1)) {
                    emit_all(wash, host);
                    return Ok(());
                }
                (1, change)
            }
        };
        for (range, cat) in host {
            if range.start < at {
                wash.emit(range.start..range.end.min(at), *cat, None);
            }
        }
        self.spans(
            &mut child.parse,
            &mut child.scopes,
            &input[at..],
            end - at,
            &[],
            |range, cat| wash.emit(range.start + at..range.end + at, cat, background),
        )?;
        Ok(())
    }

    /// The child parser of a region opening on this line: in the language
    /// its token (`language`, a range of `input`) names, else in the
    /// region kind's default language. None when neither names a syntax.
    fn region_parser(
        &mut self,
        input: &str,
        language: Option<Range<usize>>,
        spec: &EmbeddingSpec,
    ) -> Option<Embedded> {
        let syntax = language
            .and_then(|range| self.languages.find(self.syntaxes, &input[range]))
            .or_else(|| self.languages.find(self.syntaxes, spec.default_language?))?;
        Some(Embedded::start(syntax, self.syntaxes))
    }

    /// Parse `input` on from `parse` and `scopes`, passing `on_span` each
    /// part of the line's first `end` bytes that is in a category, in order.
    /// Returns the first stretch of the line during which one of `watch` was
    /// on the stack: where a region's language token is.
    fn spans(
        &mut self,
        parse: &mut ParseState,
        scopes: &mut ScopeStack,
        input: &str,
        end: usize,
        watch: &[Scope],
        mut on_span: impl FnMut(Range<usize>, Category),
    ) -> Result<Option<Range<usize>>, LineError> {
        let ops = parse
            .parse_line(input, self.syntaxes)
            .map_err(LineError::Parse)?;
        let mut at = 0;
        let mut watch_start = None;
        let mut watched = None;
        for (offset, op) in ops {
            let offset = offset.min(end);
            if offset > at {
                if let Some(cat) = self.categories.of(scopes) {
                    on_span(at..offset, cat);
                }
            }
            at = offset;
            scopes.apply(&op).map_err(LineError::Scope)?;
            if watched.is_none() {
                let active = watch.iter().any(|&scope| scopes_contain(scopes, scope));
                match (watch_start, active) {
                    (None, true) => watch_start = Some(offset),
                    (Some(start), false) => watched = Some(start..offset),
                    _ => {}
                }
            }
        }
        if end > at {
            if let Some(cat) = self.categories.of(scopes) {
                on_span(at..end, cat);
            }
        }
        Ok(watched.or_else(|| watch_start.map(|start| start..end)))
    }
}

/// Which blocks are open at a line boundary.
struct OpenBlocks {
    region: bool,
    table: bool,
}

/// Emit `spans` as they are.
fn emit_all<F: FnMut(Range<usize>, Category, Option<Category>)>(
    wash: &mut RowWash<F>,
    spans: &[Span],
) {
    for (range, cat) in spans {
        wash.emit(range.clone(), *cat, None);
    }
}

/// Add `range` in `cat` on `background` to a line's runs, merged into the
/// last run when it continues it in the same categories.
fn push_run(runs: &mut Vec<Run>, range: Range<usize>, cat: Category, background: Option<Category>) {
    let (start, len) = (to_u32(range.start), to_u32(range.len()));
    match runs.last_mut() {
        Some(last)
            if last.cat == cat
                && last.background == background
                && last.start + last.len == start =>
        {
            last.len += len
        }
        _ => runs.push(Run {
            start,
            len,
            cat,
            background,
        }),
    }
}

/// Language token → the syntax it names, memoised
/// ([`find_embedded_syntax`]). The syntax set is the highlighter's for its
/// whole life, so an answer never goes stale.
#[derive(Default)]
struct LanguageCache(HashMap<String, Option<usize>>);

impl LanguageCache {
    /// The syntax a region's raw language token names, if any.
    fn find(&mut self, syntaxes: &SyntaxSet, raw: &str) -> Option<usize> {
        let token = fence_language(raw)?;
        if let Some(&found) = self.0.get(token) {
            return found;
        }
        let found = find_embedded_syntax(syntaxes, token);
        self.0.insert(token.to_string(), found);
        found
    }
}

/// Scope → category, memoised: building a scope's string is the costly part
/// of [`scope_to_category`], and scopes are interned for the process's life.
#[derive(Default)]
struct CategoryCache(HashMap<Scope, Option<Category>>);

impl CategoryCache {
    /// The category of a scope stack: its innermost scope's that has one.
    fn of(&mut self, scopes: &ScopeStack) -> Option<Category> {
        scopes.as_slice().iter().rev().find_map(|scope| {
            *self
                .0
                .entry(*scope)
                .or_insert_with(|| scope_to_category(&scope.build_string()))
        })
    }
}

#[cfg(test)]
mod tests;
