//! The web client's WebAssembly module, with a raw C ABI (no bindings
//! generator): its syntax highlighter ([`fresh_highlight::Highlighter`],
//! the `hl_*` calls), its buffers' marker lists ([`markers`], the `mk_*`
//! calls) and the grapheme clusters it lays text out by ([`clusters`], the
//! `uc_*` call).
//!
//! One highlighter per module instance, and any number of marker lists.
//! Text crosses as UTF-8 in memory the caller gets from [`hl_alloc`] and
//! gives back with [`hl_free`] (calls only read it); every integer is a
//! `u32`. Calls that can fail return a code, [`OK`] or [`ERROR`], or a
//! result pointer that is null on failure; after a failure [`hl_error`]
//! describes it. A list argument ([`hl_regions`], [`hl_build`]) is a
//! compact byte encoding: little-endian `u32` words and UTF-8 texts, each
//! text behind its byte length (`[len u32][utf8]`), each list behind its
//! item count (`[n u32]` then the items), nothing padded.
//!
//! Results ([`hl_take`], [`hl_error`], [`hl_categories`], [`hl_build`], the
//! `hl_regions`, `mk_*` and `uc_*` answers) live in the module's own
//! buffers: valid until the next call.

mod clusters;
mod encoding;
mod markers;

use encoding::Reader;
use fresh_highlight::{
    Category, DeclaredRegion, Grammar, Highlighter, LineRuns, RegionRole, RegionStatus, Roles,
    TableLine, TableRole,
};
use markers::{Markers, Placed};
use std::alloc::{alloc, dealloc, Layout};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ptr::{self, NonNull};
use syntect::parsing::SyntaxSet;

/// The call succeeded.
pub const OK: u32 = 0;
/// The call failed; [`hl_error`] says why.
pub const ERROR: u32 = 1;

/// [`hl_work`]: everything is highlighted.
pub const WORK_DONE: u32 = 0;
/// [`hl_work`]: work remains.
pub const WORK_MORE: u32 = 1;
/// [`hl_work`]: a document failed and was closed; [`hl_error`] says which.
pub const WORK_ERROR: u32 = 2;

/// A marker end's gravity: text inserted exactly there goes after it.
pub const LEFT: u32 = 0;
/// A marker end's gravity: text inserted exactly there goes before it.
pub const RIGHT: u32 = 1;

/// [`hl_take`] line flag: the runs are exact (else provisional).
pub const LINE_EXACT: u32 = 1 << 0;
/// [`hl_take`] line flag: the line's roles are known. Without it the line's
/// role bits are all clear and say nothing: its runs are provisional, or a
/// line its roles depend on is not parsed exactly yet. With it, clear role
/// bits mean the line has none.
pub const LINE_ROLES_KNOWN: u32 = 1 << 1;
/// [`hl_take`] line flag: an embedded region's opening delimiter.
pub const LINE_REGION_OPEN: u32 = 1 << 2;
/// [`hl_take`] line flag: a content line strictly inside a region.
pub const LINE_REGION_BODY: u32 = 1 << 3;
/// [`hl_take`] line flag: a region's closing delimiter.
pub const LINE_REGION_CLOSE: u32 = 1 << 4;
/// [`hl_take`] line flag: a table's header row.
pub const LINE_TABLE_HEADER: u32 = 1 << 5;
/// [`hl_take`] line flag: a table's delimiter row.
pub const LINE_TABLE_DELIMITER: u32 = 1 << 6;
/// [`hl_take`] line flag: a table's data row.
pub const LINE_TABLE_ROW: u32 = 1 << 7;
/// [`hl_take`] line flag: the table's first data row (with
/// [`LINE_TABLE_ROW`]).
pub const LINE_TABLE_FIRST_ROW: u32 = 1 << 8;
/// [`hl_take`] line flag: the table's last line (with
/// [`LINE_TABLE_DELIMITER`] or [`LINE_TABLE_ROW`]).
pub const LINE_TABLE_LAST: u32 = 1 << 9;

/// [`hl_regions`]: the region is kept.
pub const REGION_KEPT: u32 = 0;
/// [`hl_regions`]: the region is kept, but no syntax claims its language:
/// its rows keep the styling they have.
pub const REGION_UNKNOWN_LANGUAGE: u32 = 1;
/// [`hl_regions`]: the region is dropped: it overlaps the region before it
/// in byte order.
pub const REGION_OVERLAPS: u32 = 2;
/// [`hl_regions`]: the region is dropped: it ends at or before its start.
pub const REGION_EMPTY: u32 = 3;

struct Module {
    highlighter: Option<Highlighter>,
    /// The last error: `[len u32][utf8]`.
    error: Vec<u8>,
    /// The last take's words.
    taken: Vec<u32>,
    /// The category theme keys: `[len u32][utf8 JSON]`.
    categories: Vec<u8>,
    /// The last syntax set built: `[len u32][packdump]`.
    built: Vec<u8>,
    /// The marker lists, by handle, and the handle the next one gets.
    lists: HashMap<u32, Markers>,
    next_list: u32,
    /// The last `mk_*` answer: `[n u32]` then `n` words.
    answer: Vec<u32>,
}

impl Default for Module {
    fn default() -> Self {
        Module {
            highlighter: None,
            error: framed(&[]),
            taken: Vec::new(),
            categories: Vec::new(),
            built: Vec::new(),
            lists: HashMap::new(),
            next_list: 0,
            answer: Vec::new(),
        }
    }
}

thread_local! {
    static MODULE: RefCell<Module> = RefCell::default();
}

fn with<R>(f: impl FnOnce(&mut Module) -> R) -> R {
    MODULE.with_borrow_mut(f)
}

impl Module {
    fn highlighter(&mut self) -> Result<&mut Highlighter, String> {
        self.highlighter
            .as_mut()
            .ok_or_else(|| "hl_init has not been called".to_string())
    }

    fn list(&mut self, list: u32) -> Result<&mut Markers, String> {
        self.lists
            .get_mut(&list)
            .ok_or_else(|| format!("no marker list {list}"))
    }

    /// An answer's words as the answer buffer, or the failure: the buffer's
    /// address, null on failure.
    fn answer(&mut self, words: Result<Vec<u32>, String>) -> *const u32 {
        match words {
            Ok(words) => {
                self.answer.clear();
                self.answer.push(word(words.len()));
                self.answer.extend(words);
                self.answer.as_ptr()
            }
            Err(message) => {
                self.fail(&message);
                ptr::null()
            }
        }
    }

    fn fail(&mut self, message: &str) {
        self.error = framed(message.as_bytes());
    }

    /// A call's result as its return code.
    fn code(&mut self, result: Result<(), String>) -> u32 {
        match result {
            Ok(()) => OK,
            Err(message) => {
                self.fail(&message);
                ERROR
            }
        }
    }
}

/// A count or offset as a word. Everything counted lives in wasm32 memory.
fn word(n: usize) -> u32 {
    u32::try_from(n).expect("counts fit the wasm32 address space")
}

/// `bytes` behind their length: `[len u32][bytes]`.
fn framed(bytes: &[u8]) -> Vec<u8> {
    word(bytes.len())
        .to_le_bytes()
        .into_iter()
        .chain(bytes.iter().copied())
        .collect()
}

/// # Safety
/// `ptr` points to `len` readable bytes, or `len` is 0.
unsafe fn bytes<'a>(ptr: *const u8, len: u32) -> &'a [u8] {
    if len == 0 {
        &[]
    } else {
        // SAFETY: the caller's contract.
        unsafe { std::slice::from_raw_parts(ptr, len as usize) }
    }
}

/// # Safety
/// As [`bytes`].
unsafe fn text<'a>(ptr: *const u8, len: u32) -> Result<&'a str, String> {
    // SAFETY: the caller's contract.
    std::str::from_utf8(unsafe { bytes(ptr, len) }).map_err(|e| format!("text is not UTF-8: {e}"))
}

fn layout(len: u32) -> Result<Layout, String> {
    Layout::array::<u8>(len as usize).map_err(|e| format!("cannot allocate {len} bytes: {e}"))
}

/// Allocate `len` bytes for the caller to write into. Null when they cannot
/// be had (see [`hl_error`]).
#[no_mangle]
pub extern "C" fn hl_alloc(len: u32) -> *mut u8 {
    if len == 0 {
        return NonNull::dangling().as_ptr();
    }
    let allocated = layout(len).and_then(|layout| {
        // SAFETY: the layout's size is not zero.
        NonNull::new(unsafe { alloc(layout) })
            .map(NonNull::as_ptr)
            .ok_or_else(|| format!("out of memory allocating {len} bytes"))
    });
    allocated.unwrap_or_else(|message| {
        with(|m| m.fail(&message));
        ptr::null_mut()
    })
}

/// Free what [`hl_alloc`] gave for `len` bytes.
///
/// # Safety
/// `ptr` came from `hl_alloc(len)` and is not used again.
#[no_mangle]
pub unsafe extern "C" fn hl_free(ptr: *mut u8, len: u32) {
    if len == 0 {
        return;
    }
    let layout = layout(len).expect("hl_alloc allocated this layout");
    // SAFETY: the caller's contract.
    unsafe { dealloc(ptr, layout) }
}

/// Load the syntax set from an uncompressed packdump (the default set, or
/// one [`hl_build`] made). Replaces any earlier highlighter: every document
/// open in it is closed, its id free again, and the caller reopens the ones
/// it still wants (their wanted ranges, regions and highlights go with
/// them). When the packdump does not load, the earlier highlighter and its
/// documents stay as they were.
///
/// # Safety
/// `ptr` points to `len` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hl_init(ptr: *const u8, len: u32) -> u32 {
    // SAFETY: the caller's contract.
    let packdump = unsafe { bytes(ptr, len) };
    with(|m| {
        let loaded = syntect::dumps::from_uncompressed_data::<SyntaxSet>(packdump)
            .map(|syntaxes| m.highlighter = Some(Highlighter::new(syntaxes)))
            .map_err(|e| format!("cannot load the syntax set: {e}"));
        m.code(loaded)
    })
}

/// Open document `doc` with a text, highlighted with the syntax of the given
/// name.
///
/// # Safety
/// Each pointer points to its length's readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hl_open(
    doc: u32,
    syntax_ptr: *const u8,
    syntax_len: u32,
    text_ptr: *const u8,
    text_len: u32,
) -> u32 {
    with(|m| {
        let opened = (|| {
            // SAFETY: the caller's contract.
            let (syntax, text) =
                unsafe { (text(syntax_ptr, syntax_len)?, text(text_ptr, text_len)?) };
            m.highlighter()?
                .open(doc, syntax, text)
                .map_err(|e| e.to_string())
        })();
        m.code(opened)
    })
}

/// Open document `doc` with a text in declared-regions mode: plain text,
/// coloured only in the regions [`hl_regions`] declares (it has none yet).
///
/// # Safety
/// `text_ptr` points to `text_len` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hl_open_declared(doc: u32, text_ptr: *const u8, text_len: u32) -> u32 {
    with(|m| {
        let opened = (|| {
            // SAFETY: the caller's contract.
            let text = unsafe { text(text_ptr, text_len)? };
            m.highlighter()?
                .open_declared(doc, text)
                .map_err(|e| e.to_string())
        })();
        m.code(opened)
    })
}

/// Replace the regions declared over document `doc` (opened with
/// [`hl_open_declared`]). The regions are a list (see the module docs) of
///
/// `[start u32][end u32][prefix u32][streams: n u32, n × stream u32]`
/// `[language: len u32, utf8]`
///
/// — the byte range of whole rows in the current text, the bytes skipped at
/// each row's start, the parse streams the rows feed (none: stream 0), and
/// the language token or path that picks the grammar. Answers `[status]`
/// per region, in the order given: [`REGION_KEPT`],
/// [`REGION_UNKNOWN_LANGUAGE`], [`REGION_OVERLAPS`] or [`REGION_EMPTY`]
/// (the last two dropped). A region past the text's end, or a malformed
/// list, is an error, and the regions stay as they were. Every line is
/// highlighted anew; [`hl_replace`] clears the regions.
///
/// # Safety
/// `ptr` points to `len` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hl_regions(doc: u32, ptr: *const u8, len: u32) -> *const u32 {
    // SAFETY: the caller's contract.
    let encoded = unsafe { bytes(ptr, len) };
    with(|m| {
        let set = (|| {
            let regions = declared_regions(encoded)?;
            let statuses = m
                .highlighter()?
                .set_regions(doc, &regions)
                .map_err(|e| e.to_string())?;
            Ok(statuses.into_iter().map(region_status).collect())
        })();
        m.answer(set)
    })
}

fn declared_regions(encoded: &[u8]) -> Result<Vec<DeclaredRegion>, String> {
    let mut reader = Reader::new(encoded, "the region list");
    let regions = reader.list(|r| {
        Ok(DeclaredRegion {
            start: r.size()?,
            end: r.size()?,
            prefix: r.size()?,
            streams: r.list(Reader::word)?,
            language: r.text()?.to_string(),
        })
    })?;
    reader.finish()?;
    Ok(regions)
}

fn region_status(status: RegionStatus) -> u32 {
    match status {
        RegionStatus::Kept => REGION_KEPT,
        RegionStatus::UnknownLanguage => REGION_UNKNOWN_LANGUAGE,
        RegionStatus::Overlaps => REGION_OVERLAPS,
        RegionStatus::Empty => REGION_EMPTY,
    }
}

/// Build a syntax set: the uncompressed packdump given (the one [`hl_init`]
/// loads) with `.sublime-syntax` grammars added after its own syntaxes, so
/// that one of a name already in the set shadows it. The sources are a list
/// (see the module docs) of `[name: len u32, utf8][source: len u32, utf8]`:
/// the name the grammar is reported by, and is named when its YAML names
/// none, and its YAML. Answers `[len u32][packdump]`, uncompressed, for
/// [`hl_init`]; null when the packdump does not load or a grammar does not
/// parse — every grammar that does not is named in [`hl_error`].
///
/// Touches no highlighter: a module instance can build sets while another
/// highlights.
///
/// # Safety
/// Each pointer points to its length's readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hl_build(
    packdump_ptr: *const u8,
    packdump_len: u32,
    sources_ptr: *const u8,
    sources_len: u32,
) -> *const u8 {
    // SAFETY: the caller's contract.
    let (packdump, sources) = unsafe {
        (
            bytes(packdump_ptr, packdump_len),
            bytes(sources_ptr, sources_len),
        )
    };
    with(|m| {
        let built = (|| {
            let mut reader = Reader::new(sources, "the grammar list");
            let grammars = reader.list(|r| {
                Ok(Grammar {
                    name: r.text()?,
                    source: r.text()?,
                })
            })?;
            reader.finish()?;
            fresh_highlight::build_syntax_set(packdump, &grammars).map_err(|e| e.to_string())
        })();
        match built {
            Ok(set) => {
                m.built = framed(&set);
                m.built.as_ptr()
            }
            Err(message) => {
                m.fail(&message);
                ptr::null()
            }
        }
    })
}

/// Replace document `doc`'s whole text. A document opened for declared
/// regions loses them: they were ranges of the text replaced.
///
/// # Safety
/// `text_ptr` points to `text_len` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hl_replace(doc: u32, text_ptr: *const u8, text_len: u32) -> u32 {
    with(|m| {
        let replaced = (|| {
            // SAFETY: the caller's contract.
            let text = unsafe { text(text_ptr, text_len)? };
            m.highlighter()?
                .replace(doc, text)
                .map_err(|e| e.to_string())
        })();
        m.code(replaced)
    })
}

#[no_mangle]
pub extern "C" fn hl_close(doc: u32) -> u32 {
    with(|m| {
        let closed = (|| m.highlighter()?.close(doc).map_err(|e| e.to_string()))();
        m.code(closed)
    })
}

/// Replace `delete_len` bytes at byte `start` of document `doc` with a text.
///
/// # Safety
/// `text_ptr` points to `text_len` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hl_edit(
    doc: u32,
    start: u32,
    delete_len: u32,
    text_ptr: *const u8,
    text_len: u32,
) -> u32 {
    with(|m| {
        let edited = (|| {
            // SAFETY: the caller's contract.
            let insert = unsafe { text(text_ptr, text_len)? };
            m.highlighter()?
                .edit(doc, start as usize, delete_len as usize, insert)
                .map_err(|e| e.to_string())
        })();
        m.code(edited)
    })
}

/// Have [`hl_work`] parse lines `from_line..to_line` (end exclusive) of
/// document `doc` first.
#[no_mangle]
pub extern "C" fn hl_want(doc: u32, from_line: u32, to_line: u32) -> u32 {
    with(|m| {
        let wanted = (|| {
            m.highlighter()?
                .want(doc, from_line as usize, to_line as usize)
                .map_err(|e| e.to_string())
        })();
        m.code(wanted)
    })
}

/// Parse about `budget_bytes`: [`WORK_DONE`], [`WORK_MORE`] or
/// [`WORK_ERROR`].
#[no_mangle]
pub extern "C" fn hl_work(budget_bytes: u32) -> u32 {
    with(|m| {
        let worked = (|| {
            m.highlighter()?
                .work(budget_bytes as usize)
                .map_err(|e| e.to_string())
        })();
        match worked {
            Ok(true) => WORK_MORE,
            Ok(false) => WORK_DONE,
            Err(message) => {
                m.fail(&message);
                WORK_ERROR
            }
        }
    })
}

/// The lines of document `doc` whose runs or roles changed since the last
/// take, as words: `[total_words, n_lines]`, then per line `[line, flags,
/// n_runs]` and `n_runs` × `[start, len, category_index, background]`,
/// where `background` is `1 << 31 | category_index` of the category the run
/// sits on, or 0 for none. Null on error (see [`hl_error`]).
///
/// `flags` is a set of bits: [`LINE_EXACT`] when the runs are exact, and
/// the line's roles. [`LINE_ROLES_KNOWN`] says whether they are known;
/// when they are, at most one of [`LINE_REGION_OPEN`], [`LINE_REGION_BODY`]
/// and [`LINE_REGION_CLOSE`] is set (none: the line is in no embedded
/// region), and at most one of [`LINE_TABLE_HEADER`],
/// [`LINE_TABLE_DELIMITER`] and [`LINE_TABLE_ROW`] (none: in no table),
/// with [`LINE_TABLE_FIRST_ROW`] and [`LINE_TABLE_LAST`] qualifying it.
#[no_mangle]
pub extern "C" fn hl_take(doc: u32) -> *const u32 {
    with(|m| {
        let taken = (|| m.highlighter()?.take(doc).map_err(|e| e.to_string()))();
        match taken {
            Ok(lines) => {
                encode(&lines, &mut m.taken);
                m.taken.as_ptr()
            }
            Err(message) => {
                m.fail(&message);
                ptr::null()
            }
        }
    })
}

/// A run's background category as one word: `BACKGROUND | index`, or 0
/// for none.
const BACKGROUND: u32 = 1 << 31;
fn background(category: Option<Category>) -> u32 {
    category.map_or(0, |c| BACKGROUND | u32::from(c.index()))
}

/// A line's [`hl_take`] flags.
fn flags(line: &LineRuns) -> u32 {
    let exact = if line.exact { LINE_EXACT } else { 0 };
    let roles = match line.roles {
        Roles::Unknown => 0,
        Roles::Known { region, table } => {
            LINE_ROLES_KNOWN | region.map_or(0, region_flag) | table.map_or(0, table_flags)
        }
    };
    exact | roles
}

fn region_flag(role: RegionRole) -> u32 {
    match role {
        RegionRole::Open => LINE_REGION_OPEN,
        RegionRole::Body => LINE_REGION_BODY,
        RegionRole::Close => LINE_REGION_CLOSE,
    }
}

fn table_flags(table: TableLine) -> u32 {
    let role = match table.role {
        TableRole::Header => LINE_TABLE_HEADER,
        TableRole::Delimiter => LINE_TABLE_DELIMITER,
        TableRole::Row => LINE_TABLE_ROW,
    };
    let first_row = if table.first_row {
        LINE_TABLE_FIRST_ROW
    } else {
        0
    };
    let last = if table.last { LINE_TABLE_LAST } else { 0 };
    role | first_row | last
}

fn encode(lines: &[LineRuns], out: &mut Vec<u32>) {
    out.clear();
    out.extend([0, word(lines.len())]);
    for line in lines {
        out.extend([line.line, flags(line), word(line.runs.len())]);
        out.extend(line.runs.iter().flat_map(|run| {
            [
                run.start,
                run.len,
                u32::from(run.cat.index()),
                background(run.background),
            ]
        }));
    }
    out[0] = word(out.len());
}

/// The last error: `[len u32][utf8]` (empty before any).
#[no_mangle]
pub extern "C" fn hl_error() -> *const u8 {
    with(|m| m.error.as_ptr())
}

/// The categories' theme keys, by category index: `[len u32][utf8 JSON
/// array of strings]`.
#[no_mangle]
pub extern "C" fn hl_categories() -> *const u8 {
    with(|m| {
        if m.categories.is_empty() {
            // Theme keys are plain identifiers and dots: nothing to escape.
            let keys: Vec<String> = Category::ALL
                .iter()
                .map(|c| format!("\"{}\"", c.theme_key()))
                .collect();
            m.categories = framed(format!("[{}]", keys.join(",")).as_bytes());
        }
        m.categories.as_ptr()
    })
}

// ---------------------------------------------------------------- markers

/// A gravity argument: [`LEFT`] or [`RIGHT`], as whether it is right.
fn right(gravity: u32) -> Result<bool, String> {
    match gravity {
        LEFT => Ok(false),
        RIGHT => Ok(true),
        other => Err(format!("gravity {other} is neither LEFT (0) nor RIGHT (1)")),
    }
}

fn placed_words(m: Placed) -> [u32; 3] {
    [m.id, m.start, m.end]
}

/// A new, empty marker list: answers `[list]`.
#[no_mangle]
pub extern "C" fn mk_new() -> *const u32 {
    with(|m| {
        let made = (|| {
            let list = m.next_list;
            m.next_list = list
                .checked_add(1)
                .ok_or_else(|| "this module has made 2^32 marker lists".to_string())?;
            m.lists.insert(list, Markers::default());
            Ok(vec![list])
        })();
        m.answer(made)
    })
}

/// Let marker list `list` go, with its markers.
#[no_mangle]
pub extern "C" fn mk_drop(list: u32) -> u32 {
    with(|m| {
        let dropped = m
            .lists
            .remove(&list)
            .map(|_| ())
            .ok_or_else(|| format!("no marker list {list}"));
        m.code(dropped)
    })
}

/// A point marker at `pos` with `gravity` ([`LEFT`] or [`RIGHT`]): answers
/// `[id]`.
#[no_mangle]
pub extern "C" fn mk_point(list: u32, pos: u32, gravity: u32) -> *const u32 {
    with(|m| {
        let made = (|| Ok(vec![m.list(list)?.point(pos, right(gravity)?)?]))();
        m.answer(made)
    })
}

/// A span marker over `start..end`, each end with its gravity ([`LEFT`] or
/// [`RIGHT`]): answers `[id]`.
#[no_mangle]
pub extern "C" fn mk_span(
    list: u32,
    start: u32,
    end: u32,
    start_gravity: u32,
    end_gravity: u32,
) -> *const u32 {
    with(|m| {
        let made = (|| {
            let (start_right, end_right) = (right(start_gravity)?, right(end_gravity)?);
            Ok(vec![m.list(list)?.span(
                start,
                end,
                start_right,
                end_right,
            )?])
        })();
        m.answer(made)
    })
}

/// Remove marker `id`.
#[no_mangle]
pub extern "C" fn mk_delete(list: u32, id: u32) -> u32 {
    with(|m| {
        let deleted = (|| m.list(list)?.delete(id))();
        m.code(deleted)
    })
}

/// Move every marker through an edit: `removed` bytes deleted at `at`, then
/// `inserted` bytes inserted there.
#[no_mangle]
pub extern "C" fn mk_edit(list: u32, at: u32, removed: u32, inserted: u32) -> u32 {
    with(|m| {
        let edited = (|| {
            m.list(list)?.edit(at, removed, inserted);
            Ok(())
        })();
        m.code(edited)
    })
}

/// Where marker `id` is: answers `[id, start, end]`.
#[no_mangle]
pub extern "C" fn mk_get(list: u32, id: u32) -> *const u32 {
    with(|m| {
        let got = (|| Ok(placed_words(m.list(list)?.get(id)?).to_vec()))();
        m.answer(got)
    })
}

/// The markers touching `start..end` (see [`Markers::query`]): answers
/// `[id, start, end]` each, by start, then by creation.
#[no_mangle]
pub extern "C" fn mk_query(list: u32, start: u32, end: u32) -> *const u32 {
    with(|m| {
        let found = (|| {
            let found = m.list(list)?.query(start, end)?;
            Ok(found.into_iter().flat_map(placed_words).collect())
        })();
        m.answer(found)
    })
}

// ---------------------------------------------------------------- clusters

/// The grapheme clusters of a UTF-8 text: answers `[bytes, units, width]`
/// each, in order (UTF-8 bytes, UTF-16 code units, columns). The text's last
/// cluster may continue past its end.
///
/// # Safety
/// `ptr` points to `len` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn uc_clusters(ptr: *const u8, len: u32) -> *const u32 {
    with(|m| {
        // SAFETY: the caller's contract.
        let measured = unsafe { text(ptr, len) }.map(|t| {
            clusters::clusters(t)
                .into_iter()
                .flat_map(|c| [word(c.bytes), word(c.units), word(c.width)])
                .collect()
        });
        m.answer(measured)
    })
}

#[cfg(test)]
mod tests;
