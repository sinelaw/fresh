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

use crate::{highlighted_prefix, scope_to_category, Category};
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
}

/// A line's runs, as [`Highlighter::take`] reports them.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineRuns {
    pub line: u32,
    /// Whether the runs are those of a parse from the top of the current
    /// text; a provisional parse's are not.
    pub exact: bool,
    pub runs: Vec<Run>,
}

#[derive(Debug)]
pub enum Error {
    /// No syntax of this name is in the syntax set.
    UnknownSyntax(String),
    UnknownDocument(u32),
    /// A document of this id is open already.
    DocumentOpen(u32),
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
    documents: BTreeMap<u32, Document>,
    categories: CategoryCache,
}

impl Highlighter {
    /// A highlighter over `syntaxes`, which must be a "newlines" syntax set
    /// (lines are parsed with their `\n`).
    pub fn new(syntaxes: SyntaxSet) -> Self {
        Highlighter {
            syntaxes,
            documents: BTreeMap::new(),
            categories: CategoryCache::default(),
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
            .insert(doc, Document::new(syntax_index, text));
        Ok(())
    }

    /// Replace document `doc`'s whole text. Its syntax and wanted range stay.
    pub fn replace(&mut self, doc: u32, text: &str) -> Result<(), Error> {
        check_size(doc, text.len())?;
        let d = self.document(doc)?;
        *d = Document {
            wanted: d.wanted.clone(),
            ..Document::new(d.syntax, text)
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
                documents,
                categories,
            } = self;
            let d = documents
                .get_mut(&doc)
                .expect("a task is for an open document");
            let mut parser = Parser {
                syntax: &syntaxes.syntaxes()[d.syntax],
                syntaxes,
                categories,
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
                let highlight = d.lines[line]
                    .highlight
                    .as_ref()
                    .expect("a line is marked changed when it is highlighted");
                LineRuns {
                    line: to_u32(line),
                    exact: highlight.exact,
                    runs: highlight.runs.clone(),
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
    /// Index of the syntax in the syntax set.
    syntax: usize,
    /// Never empty: an empty text is one empty line.
    lines: Vec<Line>,
    exact_until: usize,
    /// Sorted, disjoint, and all past `exact_until`; see the module docs.
    stale: Vec<Range<usize>>,
    wanted: Range<usize>,
    /// Lines whose runs changed since the last take.
    changed: BTreeSet<usize>,
}

struct Line {
    text: String,
    /// `None` until the line is parsed after its text last changed.
    highlight: Option<Highlight>,
    /// The parse state at the line's start.
    checkpoint: Option<Box<State>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Highlight {
    exact: bool,
    runs: Vec<Run>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct State {
    parse: ParseState,
    scopes: ScopeStack,
}

impl State {
    fn start(syntax: &SyntaxReference) -> Self {
        State {
            parse: ParseState::new(syntax),
            scopes: ScopeStack::new(),
        }
    }
}

impl Line {
    fn new(text: String) -> Self {
        Line {
            text,
            highlight: None,
            checkpoint: None,
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
    fn new(syntax: usize, text: &str) -> Self {
        Document {
            syntax,
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
    /// range. Returns the bytes parsed.
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
        let mut line = resume;
        let mut spent = 0;
        let mut since_checkpoint = 0;
        loop {
            if line > resume
                && line < self.lines.len()
                && self.arrive(line, &state, &mut since_checkpoint)
            {
                return Ok(spent);
            }
            self.exact_until = self.exact_until.max(line);
            if line >= until
                || line >= self.lines.len()
                || (spent >= budget && line > progress_from)
            {
                return Ok(spent);
            }
            let parsed = parser
                .line(&mut state, &self.lines[line].text)
                .map_err(|e| (line, e))?;
            spent += parsed.bytes;
            since_checkpoint += parsed.bytes;
            self.set_highlight(
                line,
                Highlight {
                    exact: true,
                    runs: parsed.runs,
                },
            );
            line += 1;
        }
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
        let mut spent = 0;
        for line in lines.clone() {
            let parsed = parser
                .line(&mut state, &self.lines[line].text)
                .map_err(|e| (line, e))?;
            spent += parsed.bytes;
            self.set_highlight(
                line,
                Highlight {
                    exact: false,
                    runs: parsed.runs,
                },
            );
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
}

enum LineError {
    Parse(ParsingError),
    Scope(ScopeError),
}

struct Parsed {
    runs: Vec<Run>,
    /// Bytes handed to the grammar.
    bytes: usize,
}

/// What parsing a line needs besides its document.
struct Parser<'a> {
    syntaxes: &'a SyntaxSet,
    syntax: &'a SyntaxReference,
    categories: &'a mut CategoryCache,
}

impl Parser<'_> {
    /// Parse one line (`text`, with its `\n` unless it is the document's
    /// last) on from `state`.
    ///
    /// Only the line's [`highlighted_prefix`] is parsed, as ending there; the
    /// rest of a longer line has no runs, and the next line goes on from the
    /// state the cut leaves — as the native engine does.
    fn line(&mut self, state: &mut State, text: &str) -> Result<Parsed, LineError> {
        let content = text.trim_end_matches(['\r', '\n']);
        let prefix = highlighted_prefix(content);
        let cut = prefix.len() < content.len();
        let input: Cow<str> = match (cut, text.ends_with('\n')) {
            (false, false) => Cow::Borrowed(prefix),
            (false, true) if text.len() == prefix.len() + 1 => Cow::Borrowed(text),
            _ => Cow::Owned(format!("{prefix}\n")),
        };
        let ops = state
            .parse
            .parse_line(&input, self.syntaxes)
            .map_err(LineError::Parse)?;
        let end = prefix.len();
        let mut runs = Vec::new();
        let mut at = 0;
        for (offset, op) in ops {
            let offset = offset.min(end);
            if offset > at {
                push_run(&mut runs, at..offset, self.categories.of(&state.scopes));
            }
            at = offset;
            state.scopes.apply(&op).map_err(LineError::Scope)?;
        }
        if end > at {
            push_run(&mut runs, at..end, self.categories.of(&state.scopes));
        }
        Ok(Parsed {
            runs,
            bytes: input.len(),
        })
    }
}

/// Add `range` in `category` to a line's runs, merged into the last run when
/// it continues it in the same category. Text in no category gets no run.
fn push_run(runs: &mut Vec<Run>, range: Range<usize>, category: Option<Category>) {
    let Some(cat) = category else {
        return;
    };
    let (start, len) = (to_u32(range.start), to_u32(range.len()));
    match runs.last_mut() {
        Some(last) if last.cat == cat && last.start + last.len == start => last.len += len,
        _ => runs.push(Run { start, len, cat }),
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
