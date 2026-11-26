//! Open documents: the core's lazy piece-tree buffer behind a windowed API.
//!
//! A client never receives a whole file. It asks for runs of lines starting
//! at a byte offset (forward or backward), so a 10 GB file costs what the
//! window costs. Line numbers come back when the buffer has a line index (an
//! ordinary file) and are absent for a huge one, exactly as in the editor.
//!
//! Edits arrive as operations against a stated log version. They are
//! transformed past whatever was sequenced since ([`crate::ot`]), applied to
//! the buffer, appended to the log and broadcast — each primitive carrying the
//! newline count it removed, so a client can move line numbers past an edit
//! in text it never fetched.

use crate::error::{err, Code};
use crate::ot::{transform_seqs, Prim};
use crate::workspace::{host, signature, Fs};
use anyhow::Result;
pub use fresh_editor_core::model::buffer::Buffer;
pub use fresh_editor_core::model::buffer::{BufferConfig, Encoding, LineEnding};

use serde_json::{json, Value};
use std::collections::{HashMap, HashSet, VecDeque};
use std::path::{Path, PathBuf};

/// Files at or above this size load lazily, with no line index: the
/// editor's own default.
pub const LARGE_FILE_THRESHOLD: usize = 10 * 1024 * 1024;

/// How much of the log is kept for transforming late edits. A client based
/// further back than this must reload the document.
const LOG_KEEP: usize = 10_000;

/// The most one read returns, whatever line count was asked for: a file that
/// is one enormous line must not turn a window into the file.
const READ_CAP: usize = 4 * 1024 * 1024;

const CHUNK: usize = 64 * 1024;

pub struct LogEntry {
    pub version: u64,
    pub prims: Vec<Prim>,
}

/// What a document is of: a file (its path), or an untitled document (its
/// number, `Untitled-N`, made by `doc.new`) until it is saved as a file
/// (`doc.save_as`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Origin {
    File(PathBuf),
    Untitled(u64),
}

impl Origin {
    /// Its file's path; none while untitled.
    pub fn path(&self) -> Option<&Path> {
        match self {
            Origin::File(p) => Some(p),
            Origin::Untitled(_) => None,
        }
    }

    /// Its number while untitled.
    pub fn untitled(&self) -> Option<u64> {
        match self {
            Origin::File(_) => None,
            Origin::Untitled(n) => Some(*n),
        }
    }
}

pub struct Doc {
    pub id: u64,
    pub origin: Origin,
    pub buffer: Buffer,
    /// Log version: the number of edit batches sequenced so far.
    pub version: u64,
    pub log: VecDeque<LogEntry>,
    pub language: Option<String>,
    pub clients: HashSet<u64>,
    pub readonly: bool,
    /// The file's (size, mtime) when it was loaded or last saved; `None`
    /// while it does not exist. A save finding anything else refuses unless
    /// forced, so another writer's change is never overwritten unseen.
    pub disk: Option<(u64, u64)>,
    /// The signature of an external change already reported to the
    /// document's clients as conflicting with its unsaved edits, so it is
    /// reported once.
    pub reported: Option<Option<(u64, u64)>>,
}

/// A document that followed a rename, with where it was.
pub struct Moved {
    pub id: u64,
    pub path: PathBuf,
    pub language: Option<String>,
}

/// A change as `didChange` states it: start and end (line, UTF-16 column),
/// and the new text.
pub type LspChange = ((usize, usize), (usize, usize), String);

/// One primitive as applied, for the broadcast and for the language server.
pub struct Applied {
    pub prim: Prim,
    /// Newlines the primitive removed (a delete) or added (an insert).
    pub newlines: usize,
    /// The primitive's range in LSP coordinates in the document *before* it,
    /// and the inserted text: what `didChange` sends.
    pub lsp: Option<LspChange>,
}

#[derive(Default)]
pub struct Docs {
    pub docs: HashMap<u64, Doc>,
    by_path: HashMap<PathBuf, u64>,
    next_id: u64,
}

/// The languages a document can be set to (`doc.set`), as `language_of`
/// names them.
pub const LANGUAGES: &[&str] = &[
    "c",
    "cpp",
    "css",
    "go",
    "html",
    "javascript",
    "json",
    "markdown",
    "python",
    "rust",
    "shell",
    "toml",
    "typescript",
    "yaml",
];

/// A line ending by the name `doc.info` gives it.
pub fn line_ending_named(name: &str) -> Option<LineEnding> {
    [LineEnding::LF, LineEnding::CRLF, LineEnding::CR]
        .into_iter()
        .find(|l| l.display_name() == name)
}

/// An encoding by the name `doc.info` gives it.
pub fn encoding_named(name: &str) -> Option<Encoding> {
    Encoding::all()
        .iter()
        .copied()
        .find(|e| e.display_name() == name)
}

pub fn language_of(path: &Path) -> Option<String> {
    let ext = path.extension()?.to_str()?.to_ascii_lowercase();
    let lang = match ext.as_str() {
        "rs" => "rust",
        "py" => "python",
        "js" | "mjs" | "cjs" => "javascript",
        "ts" | "tsx" => "typescript",
        "json" => "json",
        "md" | "markdown" => "markdown",
        "toml" => "toml",
        "sh" | "bash" => "shell",
        "c" | "h" => "c",
        "cc" | "cpp" | "hpp" => "cpp",
        "go" => "go",
        "html" => "html",
        "css" => "css",
        "yml" | "yaml" => "yaml",
        _ => return None,
    };
    Some(lang.to_string())
}

/// Primitives that turn `old` into `new`, for following a change made on
/// disk through the edit log: line hunks (Myers), each trimmed to the
/// characters that differ, last hunk first so every position is one in the
/// text the earlier primitives leave.
pub fn diff_prims(old: &str, new: &str) -> Vec<Prim> {
    fn starts(t: &str) -> Vec<usize> {
        let mut v = vec![0];
        for l in t.split_inclusive('\n') {
            v.push(v[v.len() - 1] + l.len());
        }
        v
    }
    let (os, ns) = (starts(old), starts(new));
    let diff = similar::TextDiff::from_lines(old, new);
    let mut out = Vec::new();
    for op in diff.ops().iter().rev() {
        let (tag, or, nr) = op.as_tag_tuple();
        if tag == similar::DiffTag::Equal {
            continue;
        }
        let a = &old[os[or.start]..os[or.end]];
        let b = &new[ns[nr.start]..ns[nr.end]];
        let pre: usize = a
            .chars()
            .zip(b.chars())
            .take_while(|(x, y)| x == y)
            .map(|(x, _)| x.len_utf8())
            .sum();
        let suf: usize = a[pre..]
            .chars()
            .rev()
            .zip(b[pre..].chars().rev())
            .take_while(|(x, y)| x == y)
            .map(|(x, _)| x.len_utf8())
            .sum();
        let pos = os[or.start] + pre;
        let del = a.len() - pre - suf;
        let ins = &b[pre..b.len() - suf];
        if del > 0 {
            out.push(Prim::Del { pos, len: del });
        }
        if !ins.is_empty() {
            out.push(Prim::Ins {
                pos,
                text: ins.to_string(),
            });
        }
    }
    out
}

fn count_newlines(b: &[u8]) -> usize {
    b.iter().filter(|&&c| c == b'\n').count()
}

/// Largest `n <= b.len()` such that `b[..n]` ends on a UTF-8 boundary.
fn utf8_floor(b: &[u8]) -> usize {
    match std::str::from_utf8(b) {
        Ok(_) => b.len(),
        Err(e) => e.valid_up_to(),
    }
}

impl Doc {
    pub fn len(&self) -> usize {
        self.buffer.len()
    }

    pub fn indexed(&self) -> bool {
        self.buffer.line_count().is_some()
    }

    /// How messages name it: its path, or `Untitled-N`.
    pub fn display(&self) -> String {
        match &self.origin {
            Origin::File(p) => p.display().to_string(),
            Origin::Untitled(n) => format!("Untitled-{n}"),
        }
    }

    /// The document's URI for a language server: its file's, or an
    /// `untitled:` one.
    pub fn uri(&self) -> String {
        match &self.origin {
            Origin::File(p) => crate::lsp::uri_of(p),
            Origin::Untitled(n) => format!("untitled:Untitled-{n}"),
        }
    }

    pub fn info(&self) -> Value {
        json!({
            "doc": self.id,
            "path": self.origin.path().map(|p| p.to_string_lossy()),
            "untitled": self.origin.untitled(),
            "len": self.len(),
            "version": self.version,
            "lines": self.buffer.line_count(),
            "large": self.buffer.is_large_file(),
            "modified": self.buffer.is_modified(),
            "language": self.language,
            "encoding": self.buffer.encoding().display_name(),
            "line_ending": self.buffer.line_ending().display_name(),
            "readonly": self.readonly,
        })
    }

    fn read(&mut self, start: usize, len: usize) -> Result<Vec<u8>> {
        let start = start.min(self.len());
        let len = len.min(self.len() - start);
        self.buffer.get_text_range_mut(start, len)
    }

    /// Start of the line holding `pos`. Looks back at most [`READ_CAP`]; a
    /// line longer than that is entered mid-way, on a character boundary.
    pub fn line_start(&mut self, pos: usize) -> Result<usize> {
        let pos = pos.min(self.len());
        let mut end = pos;
        while end > 0 && pos - end < READ_CAP {
            let s = end.saturating_sub(CHUNK);
            let bytes = self.read(s, end - s)?;
            if let Some(i) = bytes.iter().rposition(|&c| c == b'\n') {
                return Ok(s + i + 1);
            }
            end = s;
        }
        if end == 0 {
            return Ok(0);
        }
        // A very long line: start inside it, at a character boundary.
        let probe = self.read(end, 4)?;
        let skip = probe.iter().take_while(|&&c| (c & 0xC0) == 0x80).count();
        Ok(end + skip)
    }

    fn first_line(&self, start: usize) -> Option<usize> {
        self.indexed().then(|| self.buffer.get_line_number(start))
    }

    /// `count` lines starting at the line holding `from`.
    pub fn lines_forward(&mut self, from: usize, count: usize) -> Result<Value> {
        let start = self.line_start(from)?;
        let mut out: Vec<u8> = Vec::new();
        let mut found = 0;
        let mut pos = start;
        let len = self.len();
        while pos < len && found < count && out.len() < READ_CAP {
            let chunk = self.read(pos, CHUNK)?;
            if chunk.is_empty() {
                break;
            }
            let mut take = chunk.len();
            for (i, &c) in chunk.iter().enumerate() {
                if c == b'\n' {
                    found += 1;
                    if found == count {
                        take = i + 1;
                        break;
                    }
                }
            }
            out.extend_from_slice(&chunk[..take]);
            pos += take;
        }
        if out.len() > READ_CAP {
            out.truncate(READ_CAP);
        }
        // Cut on a character boundary if the cap or a chunk ended mid-way.
        if pos < len {
            let keep = utf8_floor(&out);
            out.truncate(keep);
        }
        let end = start + out.len();
        Ok(json!({
            "start": start,
            "end": end,
            "text": String::from_utf8_lossy(&out),
            "first_line": self.first_line(start),
            "bof": start == 0,
            "eof": end >= len,
            "len": len,
            "version": self.version,
        }))
    }

    /// The `count` lines that end at `before` (a line start, or the end).
    pub fn lines_backward(&mut self, before: usize, count: usize) -> Result<Value> {
        let before = before.min(self.len());
        let mut start = before;
        let mut found = 0;
        // The first newline found looking back is the one ending the line
        // before `before`; it does not start a line of the result.
        let mut skip_first = before > 0;
        while start > 0 && before - start < READ_CAP {
            let s = start.saturating_sub(CHUNK);
            let bytes = self.read(s, start - s)?;
            let mut cut = None;
            for i in (0..bytes.len()).rev() {
                if bytes[i] == b'\n' {
                    if skip_first && s + i + 1 == before {
                        skip_first = false;
                        continue;
                    }
                    found += 1;
                    if found == count {
                        cut = Some(s + i + 1);
                        break;
                    }
                }
            }
            skip_first = false;
            match cut {
                Some(c) => {
                    start = c;
                    break;
                }
                None => start = s,
            }
        }
        let start = if start > 0 && before - start >= READ_CAP {
            // A very long line: start inside it, on a character boundary.
            let s = before - READ_CAP;
            let probe = self.read(s, 4)?;
            s + probe.iter().take_while(|&&c| (c & 0xC0) == 0x80).count()
        } else {
            start
        };
        let bytes = self.read(start, before - start)?;
        Ok(json!({
            "start": start,
            "end": before,
            "text": String::from_utf8_lossy(&bytes),
            "first_line": self.first_line(start),
            "bof": start == 0,
            "eof": before >= self.len(),
            "len": self.len(),
            "version": self.version,
        }))
    }

    pub fn text(&mut self, start: usize, end: usize) -> Result<String> {
        let end = end.min(self.len());
        let start = start.min(end);
        if end - start > READ_CAP {
            fail!(TooLarge, "range too large");
        }
        let b = self.read(start, end - start)?;
        Ok(String::from_utf8_lossy(&b).into_owned())
    }

    /// Next occurrence of `query` at or after `from` (wrapping), or before
    /// `from` when `backward`. Scans the file in chunks, so it works on a
    /// file of any size.
    pub fn search(&mut self, query: &str, from: usize, backward: bool) -> Result<Option<usize>> {
        let q = query.as_bytes();
        if q.is_empty() {
            return Ok(None);
        }
        let len = self.len();
        let big = 1024 * 1024;
        let find_in = |hay: &[u8], last: bool| -> Option<usize> {
            if hay.len() < q.len() {
                return None;
            }
            let mut it = (0..=hay.len() - q.len()).filter(|&i| &hay[i..i + q.len()] == q);
            if last {
                it.next_back()
            } else {
                it.next()
            }
        };
        if !backward {
            for (lo, hi) in [(from.min(len), len), (0, (from + q.len()).min(len))] {
                let mut pos = lo;
                while pos < hi {
                    let end = (pos + big + q.len()).min(hi);
                    let hay = self.read(pos, end - pos)?;
                    if let Some(i) = find_in(&hay, false) {
                        return Ok(Some(pos + i));
                    }
                    if end == hi {
                        break;
                    }
                    pos += big;
                }
            }
        } else {
            for (lo, hi) in [(0, from.min(len)), (from.saturating_sub(q.len()), len)] {
                let mut end = hi;
                while end > lo {
                    let start = end.saturating_sub(big + q.len()).max(lo);
                    let hay = self.read(start, end - start)?;
                    if let Some(i) = find_in(&hay, true) {
                        return Ok(Some(start + i));
                    }
                    if start == lo {
                        break;
                    }
                    end = start + q.len();
                }
            }
        }
        Ok(None)
    }

    /// Sequence an edit made by `client` against log version `base`.
    /// Returns what was applied, at the version it produced.
    pub fn edit(
        &mut self,
        base: u64,
        prims: Vec<Prim>,
        lsp_positions: bool,
    ) -> Result<Vec<Applied>> {
        if base > self.version {
            fail!(
                BadRequest,
                "base version {base} is ahead of the document ({})",
                self.version
            );
        }
        let oldest = self
            .log
            .front()
            .map(|e| e.version - 1)
            .unwrap_or(self.version);
        if base < oldest {
            fail!(
                Resync,
                "resync: base version {base} is older than the kept log"
            );
        }
        // Everything sequenced after `base` was sequenced before this edit,
        // so it goes first at a tie.
        let mut prims = prims;
        for e in self.log.iter().filter(|e| e.version > base) {
            prims = transform_seqs(&prims, &e.prims, false).0;
        }
        let mut applied = Vec::new();
        for p in &prims {
            let len = self.len();
            match p {
                Prim::Ins { pos, text } => {
                    if *pos > len {
                        fail!(BadRequest, "insert at {pos} past the end ({len})");
                    }
                    let lsp = lsp_positions.then(|| {
                        let at = self.buffer.position_to_lsp_position(*pos);
                        (at, at, text.clone())
                    });
                    self.buffer.insert(*pos, text);
                    applied.push(Applied {
                        prim: p.clone(),
                        newlines: count_newlines(text.as_bytes()),
                        lsp,
                    });
                }
                Prim::Del { pos, len: n } => {
                    if pos + n > len {
                        fail!(BadRequest, "delete {pos}+{n} past the end ({len})");
                    }
                    let gone = self.read(*pos, *n)?;
                    let lsp = lsp_positions.then(|| {
                        (
                            self.buffer.position_to_lsp_position(*pos),
                            self.buffer.position_to_lsp_position(pos + n),
                            String::new(),
                        )
                    });
                    self.buffer.delete_bytes(*pos, *n);
                    applied.push(Applied {
                        prim: p.clone(),
                        newlines: count_newlines(&gone),
                        lsp,
                    });
                }
            }
        }
        self.version += 1;
        self.log.push_back(LogEntry {
            version: self.version,
            prims,
        });
        while self.log.len() > LOG_KEEP {
            self.log.pop_front();
        }
        Ok(applied)
    }

    /// Save through the document's filesystem; `recovery` is the host's
    /// recovery folder. Refused when the file changed on disk since it was
    /// loaded or last saved, unless `force`.
    pub fn save(&mut self, recovery: &Path, force: bool) -> Result<()> {
        let Some(path) = self.origin.path().map(Path::to_path_buf) else {
            fail!(
                BadRequest,
                "{} has no file: save it as one (doc.save_as)",
                self.display()
            );
        };
        let fs = self.buffer.filesystem().clone();
        if !force && signature(&fs, &path) != self.disk {
            fail!(
                ChangedOnDisk,
                "{} changed on disk since it was opened; save with force to overwrite it",
                path.display()
            );
        }
        if self.disk.is_none() {
            // A new file, or one deleted while open: its folder may be gone.
            if let Some(parent) = path.parent() {
                fs.create_dir_all(host(parent).as_host_path())?;
            }
        }
        self.buffer.save(recovery)?;
        self.disk = signature(&fs, &path);
        self.reported = None;
        Ok(())
    }

    /// The whole text, for a language server's `didOpen`. Only for an
    /// indexed (ordinary-sized) file.
    pub fn full_text(&mut self) -> Result<String> {
        let n = self.len();
        Ok(String::from_utf8_lossy(&self.read(0, n)?).into_owned())
    }
}

impl Docs {
    /// Open `path` for `client`, or join the document already open for it.
    /// Returns the id and whether it was newly opened.
    pub fn open(&mut self, client: u64, path: &Path, fs: Fs) -> Result<(u64, bool)> {
        let path = fs
            .canonicalize(host(path).as_host_path())
            .map(PathBuf::from)
            .unwrap_or_else(|_| path.to_path_buf());
        if let Some(&id) = self.by_path.get(&path) {
            if let Some(d) = self.docs.get_mut(&id) {
                d.clients.insert(client);
                return Ok((id, false));
            }
        }
        // No such file yet: a new one, written on save. Any other failure
        // to look (permissions, a dropped connection) is an error, never
        // taken for a new, empty file.
        let meta = match fs.metadata(host(&path).as_host_path()) {
            Ok(m) => Some(m),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
            Err(e) => return Err(anyhow::anyhow!("{}: {e}", path.display())),
        };
        let readonly = meta.as_ref().is_some_and(|m| m.is_readonly);
        let disk = signature(&fs, &path);
        let buffer = if meta.is_some() {
            Buffer::load_from_file(&path, LARGE_FILE_THRESHOLD, fs)?
        } else {
            let mut b = Buffer::empty(fs);
            b.set_file_path(path.clone());
            b
        };
        if buffer.is_binary() {
            fail!(Binary, "{} is a binary file", path.display());
        }
        self.next_id += 1;
        let id = self.next_id;
        let mut clients = HashSet::new();
        clients.insert(client);
        self.docs.insert(
            id,
            Doc {
                id,
                language: language_of(&path),
                origin: Origin::File(path.clone()),
                buffer,
                version: 0,
                log: VecDeque::new(),
                clients,
                readonly,
                disk,
                reported: None,
            },
        );
        self.by_path.insert(path, id);
        Ok((id, true))
    }

    /// A new untitled document for `client`: empty, bound to no file,
    /// numbered with the smallest number no open untitled document has.
    pub fn create(&mut self, client: u64, fs: Fs) -> u64 {
        let taken: HashSet<u64> = self
            .docs
            .values()
            .filter_map(|d| d.origin.untitled())
            .collect();
        let n = (1..).find(|n| !taken.contains(n)).unwrap_or(1);
        self.next_id += 1;
        let id = self.next_id;
        self.docs.insert(
            id,
            Doc {
                id,
                language: None,
                origin: Origin::Untitled(n),
                buffer: Buffer::empty(fs),
                version: 0,
                log: VecDeque::new(),
                clients: HashSet::from([client]),
                readonly: false,
                disk: None,
                reported: None,
            },
        );
        id
    }

    /// `client` joins untitled document `n` (one kept for its unsaved text
    /// after its last client went: a page reloaded).
    pub fn join_untitled(&mut self, client: u64, n: u64) -> Result<u64> {
        let Some(d) = self
            .docs
            .values_mut()
            .find(|d| d.origin == Origin::Untitled(n))
        else {
            fail!(NotFound, "no document Untitled-{n}");
        };
        d.clients.insert(client);
        Ok(d.id)
    }

    /// Save document `id` as the file `path` and bind it there: the text is
    /// written through the document's filesystem (unloaded parts of a huge
    /// file copied from its old file), and the document's path, language
    /// (detected from the new name, else kept) and disk signature follow.
    /// Refused when another document is open on `path`, or when a file is
    /// there and `overwrite` is not given. Returns the path it had.
    pub fn save_as(
        &mut self,
        id: u64,
        path: &Path,
        recovery: &Path,
        overwrite: bool,
    ) -> Result<Option<PathBuf>> {
        if self.by_path.get(path).is_some_and(|&other| other != id) {
            fail!(Exists, "{} is open as another document", path.display());
        }
        let d = self.get(id)?;
        let fs = d.buffer.filesystem().clone();
        if d.origin.path() != Some(path) && signature(&fs, path).is_some() && !overwrite {
            fail!(Exists, "{} exists", path.display());
        }
        if let Some(parent) = path.parent() {
            fs.create_dir_all(host(parent).as_host_path())?;
        }
        d.buffer.save_to_file(path, recovery)?;
        let path = fs
            .canonicalize(host(path).as_host_path())
            .map(PathBuf::from)
            .unwrap_or_else(|_| path.to_path_buf());
        d.disk = signature(&fs, &path);
        d.reported = None;
        d.language = language_of(&path).or(d.language.take());
        let old = std::mem::replace(&mut d.origin, Origin::File(path.clone()))
            .path()
            .map(Path::to_path_buf);
        if let Some(o) = &old {
            self.by_path.remove(o);
        }
        self.by_path.insert(path, id);
        Ok(old)
    }

    pub fn get(&mut self, id: u64) -> Result<&mut Doc> {
        self.docs
            .get_mut(&id)
            .ok_or_else(|| err(Code::NotFound, format!("no document {id}")))
    }

    /// `client` closes `id`. The document goes when no client has it open
    /// and it has nothing unsaved, or nothing unsaved worth keeping:
    /// `discard` gives up its unsaved changes, so the next to open the file
    /// reads it from disk. While other clients hold the document its
    /// changes are theirs too and stay. Returns the document when it went.
    pub fn close(&mut self, client: u64, id: u64, discard: bool) -> Option<Doc> {
        let d = self.docs.get_mut(&id)?;
        d.clients.remove(&client);
        if d.clients.is_empty() && (discard || !d.buffer.is_modified()) {
            let d = self.docs.remove(&id)?;
            if let Some(p) = d.origin.path() {
                self.by_path.remove(p);
            }
            return Some(d);
        }
        None
    }

    /// The documents of `path` or of files under it, in id order.
    pub fn under(&self, path: &Path) -> Vec<u64> {
        let mut ids: Vec<u64> = self
            .by_path
            .iter()
            .filter(|(p, _)| p.starts_with(path))
            .map(|(_, id)| *id)
            .collect();
        ids.sort_unstable();
        ids
    }

    /// A file (or a folder of files) was renamed: its open documents follow
    /// it, and their languages are detected again from the new names.
    /// Returns each moved document with its old path and language, in id
    /// order.
    pub fn renamed(&mut self, from: &Path, to: &Path) -> Vec<Moved> {
        let mut out = Vec::new();
        for id in self.under(from) {
            let Some(d) = self.docs.get_mut(&id) else {
                continue;
            };
            let Some(old) = d.origin.path().map(Path::to_path_buf) else {
                continue;
            };
            let np = match old.strip_prefix(from) {
                Ok(rel) if !rel.as_os_str().is_empty() => to.join(rel),
                _ => to.to_path_buf(),
            };
            self.by_path.remove(&old);
            self.by_path.insert(np.clone(), id);
            d.buffer.rename_file_path(np.clone());
            d.origin = Origin::File(np.clone());
            out.push(Moved {
                id,
                path: old,
                language: std::mem::replace(&mut d.language, language_of(&np)),
            });
        }
        out
    }

    /// The file of each document at or under `path` that no longer exists
    /// was deleted: the document keeps its text as unsaved, so it stays
    /// until closed, and saving writes the file again. Returns their ids.
    pub fn deleted(&mut self, path: &Path) -> Vec<u64> {
        let mut out = Vec::new();
        for id in self.under(path) {
            let Some(d) = self.docs.get_mut(&id) else {
                continue;
            };
            let fs = d.buffer.filesystem().clone();
            if d.origin.path().is_some_and(|p| signature(&fs, p).is_none()) {
                d.disk = None;
                d.buffer.set_modified(true);
                out.push(id);
            }
        }
        out
    }

    pub fn by_path(&self, path: &Path) -> Option<u64> {
        self.by_path.get(path).copied()
    }

    /// The document a language server names by `uri`: a file's by its
    /// path, an untitled one by its `untitled:` URI.
    pub fn by_uri(&self, uri: &str) -> Option<u64> {
        match crate::lsp::path_of(uri) {
            Some(path) => self.by_path(&path),
            None => self.docs.values().find(|d| d.uri() == uri).map(|d| d.id),
        }
    }

    /// A client disconnected: it closes everything.
    pub fn drop_client(&mut self, client: u64) -> Vec<Doc> {
        let ids: Vec<u64> = self.docs.keys().copied().collect();
        ids.into_iter()
            .filter_map(|id| self.close(client, id, false))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    fn doc_with(text: &str) -> (tempfile::TempDir, Docs, u64) {
        let dir = tempfile::tempdir().unwrap();
        let p = dir.path().join("a.txt");
        std::fs::write(&p, text).unwrap();
        let mut docs = Docs::default();
        let (id, _) = docs
            .open(
                1,
                &p,
                std::sync::Arc::new(fresh_editor_core::model::filesystem::StdFileSystem),
            )
            .unwrap();
        (dir, docs, id)
    }

    #[test]
    fn reads_lines_forward_and_backward() {
        let text: String = (0..100).map(|i| format!("line {i}\n")).collect();
        let (_d, mut docs, id) = doc_with(&text);
        let d = docs.get(id).unwrap();
        let w = d
            .lines_forward(text.find("line 10\n").unwrap() + 3, 3)
            .unwrap();
        assert_eq!(w["text"], "line 10\nline 11\nline 12\n");
        assert_eq!(w["first_line"], 10);
        let start = w["start"].as_u64().unwrap() as usize;
        let b = d.lines_backward(start, 2).unwrap();
        assert_eq!(b["text"], "line 8\nline 9\n");
        let b = d.lines_backward(d.len(), 1).unwrap();
        assert_eq!(b["text"], "line 99\n");
    }

    #[test]
    fn save_refuses_over_an_external_change_unless_forced() {
        let (dir, mut docs, id) = doc_with("one\n");
        let p = dir.path().join("a.txt");
        let recovery = dir.path().join("recovery");
        let d = docs.get(id).unwrap();
        let ins = |pos, text: &str| {
            vec![Prim::Ins {
                pos,
                text: text.into(),
            }]
        };
        d.edit(0, ins(0, "zero\n"), false).unwrap();
        d.save(&recovery, false).unwrap();
        assert_eq!(std::fs::read_to_string(&p).unwrap(), "zero\none\n");
        assert!(!d.buffer.is_modified());
        // Its own save is not an external change.
        d.edit(1, ins(0, "a\n"), false).unwrap();
        d.save(&recovery, false).unwrap();

        std::fs::write(&p, "another writer\n").unwrap();
        d.edit(2, ins(0, "b\n"), false).unwrap();
        let e = d.save(&recovery, false).unwrap_err();
        assert_eq!(crate::error::code_of(&e), Code::ChangedOnDisk, "{e}");
        assert_eq!(std::fs::read_to_string(&p).unwrap(), "another writer\n");
        d.save(&recovery, true).unwrap();
        assert_eq!(std::fs::read_to_string(&p).unwrap(), "b\na\nzero\none\n");

        // Deleted under it: also a change.
        std::fs::remove_file(&p).unwrap();
        d.edit(3, ins(0, "c\n"), false).unwrap();
        assert!(d.save(&recovery, false).is_err());
    }

    #[test]
    fn a_document_goes_when_its_last_client_closes_it_unmodified() {
        let (_d, mut docs, id) = doc_with("text\n");
        let p = docs.get(id).unwrap().origin.path().unwrap().to_path_buf();
        let fs: Fs = std::sync::Arc::new(fresh_editor_core::model::filesystem::StdFileSystem);
        let (again, new) = docs.open(2, &p, fs.clone()).unwrap();
        assert_eq!((again, new), (id, false), "the second client joins it");
        assert!(docs.close(1, id, false).is_none());
        assert!(docs.close(2, id, false).is_some());
        assert!(docs.get(id).is_err());
        assert_eq!(docs.by_path(&p), None);

        // A modified one stays for the next client, with its edits.
        let (id, _) = docs.open(1, &p, fs.clone()).unwrap();
        docs.get(id)
            .unwrap()
            .edit(
                0,
                vec![Prim::Ins {
                    pos: 0,
                    text: "kept ".into(),
                }],
                false,
            )
            .unwrap();
        assert!(docs.drop_client(1).is_empty());
        let (back, new) = docs.open(3, &p, fs).unwrap();
        assert_eq!((back, new), (id, false));
        assert_eq!(docs.get(id).unwrap().text(0, 100).unwrap(), "kept text\n");
    }

    #[test]
    fn discarding_drops_unsaved_changes_only_from_the_last_holder() {
        let (dir, mut docs, id) = doc_with("disk\n");
        let p = dir.path().join("a.txt");
        let fs: Fs = std::sync::Arc::new(fresh_editor_core::model::filesystem::StdFileSystem);
        let ins = vec![Prim::Ins {
            pos: 0,
            text: "edit ".into(),
        }];
        assert_eq!(docs.open(2, &p, fs.clone()).unwrap(), (id, false));
        docs.get(id).unwrap().edit(0, ins.clone(), false).unwrap();
        // Another client holds it: its changes are theirs too, and stay.
        assert!(docs.close(1, id, true).is_none());
        assert_eq!(docs.get(id).unwrap().text(0, 100).unwrap(), "edit disk\n");
        // The last holder discards: the document goes, and the file opens
        // again as it is on disk.
        assert!(docs.close(2, id, true).is_some());
        let (again, new) = docs.open(1, &p, fs).unwrap();
        assert!(new);
        assert_eq!(docs.get(again).unwrap().text(0, 100).unwrap(), "disk\n");
        assert_eq!(std::fs::read_to_string(&p).unwrap(), "disk\n");
    }

    #[test]
    fn a_diff_applied_as_edits_gives_the_new_text() {
        for (old, new) in [
            ("a\nb\nc\n", "a\nB\nc\n"),
            ("one\ntwo\nthree\n", "zero\none\nthree\nfour"),
            ("", "new\n"),
            ("gone\n", ""),
            ("héllo wörld\nx\n", "héllo wörld!\nx\ny\n"),
            ("same\n", "same\n"),
        ] {
            let (_d, mut docs, id) = doc_with(old);
            let d = docs.get(id).unwrap();
            let prims = diff_prims(old, new);
            d.edit(0, prims.clone(), false).unwrap();
            assert_eq!(d.text(0, 1000).unwrap(), new, "{old:?} -> {new:?}");
            if old == new {
                assert!(prims.is_empty());
            }
        }
        // Only what differs is touched: one character in a line.
        assert_eq!(
            diff_prims("abc\ndef\n", "abc\ndXf\n"),
            [
                Prim::Del { pos: 5, len: 1 },
                Prim::Ins {
                    pos: 5,
                    text: "X".into()
                }
            ]
        );
    }

    #[test]
    fn concurrent_edits_converge() {
        let (_d, mut docs, id) = doc_with("hello world\n");
        let d = docs.get(id).unwrap();
        // Client 1 inserts at 5 against version 0; client 2, also at
        // version 0, deletes "world".
        d.edit(
            0,
            vec![Prim::Ins {
                pos: 5,
                text: ",".into(),
            }],
            false,
        )
        .unwrap();
        d.edit(0, vec![Prim::Del { pos: 6, len: 5 }], false)
            .unwrap();
        assert_eq!(d.text(0, 100).unwrap(), "hello, \n");
        assert_eq!(d.version, 2);
    }

    #[test]
    fn a_language_server_names_a_file_or_an_untitled_document_by_uri() {
        let (_d, mut docs, id) = doc_with("x\n");
        let fs: Fs = std::sync::Arc::new(fresh_editor_core::model::filesystem::StdFileSystem);
        let u = docs.create(1, fs);
        let file_uri = docs.get(id).unwrap().uri();
        assert!(file_uri.starts_with("file://"));
        assert_eq!(docs.by_uri(&file_uri), Some(id));
        assert_eq!(docs.by_uri("untitled:Untitled-1"), Some(u));
        assert_eq!(docs.by_uri("untitled:Untitled-2"), None);
    }

    #[test]
    fn huge_file_is_lazy_and_unindexed() {
        let dir = tempfile::tempdir().unwrap();
        let p = dir.path().join("big.log");
        let mut f = std::fs::File::create(&p).unwrap();
        let line = "a fairly ordinary log line of text, number ";
        let mut n = 0usize;
        let mut written = 0usize;
        while written < LARGE_FILE_THRESHOLD + 1_000_000 {
            let l = format!("{line}{n}\n");
            written += l.len();
            f.write_all(l.as_bytes()).unwrap();
            n += 1;
        }
        drop(f);
        let mut docs = Docs::default();
        let (id, _) = docs
            .open(
                1,
                &p,
                std::sync::Arc::new(fresh_editor_core::model::filesystem::StdFileSystem),
            )
            .unwrap();
        let d = docs.get(id).unwrap();
        assert!(d.buffer.is_large_file());
        assert_eq!(d.buffer.line_count(), None);
        let mid = d.len() / 2;
        let w = d.lines_forward(mid, 5).unwrap();
        let t = w["text"].as_str().unwrap();
        assert_eq!(t.lines().count(), 5);
        assert!(t.starts_with(line));
        assert!(w["first_line"].is_null());
        // Edit in the middle and read it back.
        let s = w["start"].as_u64().unwrap() as usize;
        d.edit(
            0,
            vec![Prim::Ins {
                pos: s,
                text: "EDITED ".into(),
            }],
            false,
        )
        .unwrap();
        let w = d.lines_forward(s, 1).unwrap();
        assert!(w["text"].as_str().unwrap().starts_with("EDITED a fairly"));
        // Search finds the last line from the start.
        let last = format!("{line}{}\n", n - 1);
        let at = d.search(&last, 0, false).unwrap().unwrap();
        assert_eq!(at + last.len(), d.len());
    }
}
