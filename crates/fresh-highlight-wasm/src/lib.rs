//! The web client's syntax highlighter: [`fresh_highlight::Highlighter`] as a
//! WebAssembly module with a raw C ABI (no bindings generator).
//!
//! One highlighter per module instance. Text crosses as UTF-8 in memory the
//! caller gets from [`hl_alloc`] and gives back with [`hl_free`] (calls only
//! read it); every integer is a `u32`. Calls that can fail return a code,
//! [`OK`] or [`ERROR`], and after an error [`hl_error`] describes it.
//!
//! Results ([`hl_take`], [`hl_error`], [`hl_categories`]) live in the
//! module's own buffers: valid until the next `hl_*` call.

use fresh_highlight::{Category, Highlighter, LineRuns};
use std::alloc::{alloc, dealloc, Layout};
use std::cell::RefCell;
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

struct Module {
    highlighter: Option<Highlighter>,
    /// The last error: `[len u32][utf8]`.
    error: Vec<u8>,
    /// The last take's words.
    taken: Vec<u32>,
    /// The category theme keys: `[len u32][utf8 JSON]`.
    categories: Vec<u8>,
}

impl Default for Module {
    fn default() -> Self {
        Module {
            highlighter: None,
            error: framed(&[]),
            taken: Vec::new(),
            categories: Vec::new(),
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

/// Load the syntax set from an uncompressed packdump. Replaces any earlier
/// highlighter, with its documents.
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

/// Replace document `doc`'s whole text.
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

/// The lines of document `doc` whose runs changed since the last take, as
/// words: `[total_words, n_lines]`, then per line `[line, flags (bit 0:
/// exact), n_runs]` and `n_runs` × `[start, len, category_index]`. Null on
/// error (see [`hl_error`]).
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

fn encode(lines: &[LineRuns], out: &mut Vec<u32>) {
    out.clear();
    out.extend([0, word(lines.len())]);
    for line in lines {
        out.extend([line.line, u32::from(line.exact), word(line.runs.len())]);
        out.extend(
            line.runs
                .iter()
                .flat_map(|run| [run.start, run.len, u32::from(run.cat.index())]),
        );
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
