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

use crate::ot::{transform_seqs, Prim};
use anyhow::{anyhow, bail, Result};
use fresh_editor_core::model::buffer::Buffer;
use fresh_editor_core::model::filesystem::StdFileSystem;
use serde_json::{json, Value};
use std::collections::{HashMap, HashSet, VecDeque};
use std::path::{Path, PathBuf};
use std::sync::Arc;

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
    pub client: u64,
    pub prims: Vec<Prim>,
}

pub struct Doc {
    pub id: u64,
    pub path: PathBuf,
    pub buffer: Buffer,
    /// Log version: the number of edit batches sequenced so far.
    pub version: u64,
    pub log: VecDeque<LogEntry>,
    pub language: Option<String>,
    pub clients: HashSet<u64>,
}

/// One primitive as applied, for the broadcast and for the language server.
pub struct Applied {
    pub prim: Prim,
    /// Newlines the primitive removed (a delete) or added (an insert).
    pub newlines: usize,
    /// The primitive's range in LSP coordinates in the document *before* it,
    /// and the inserted text: what `didChange` sends.
    pub lsp: Option<((usize, usize), (usize, usize), String)>,
}

#[derive(Default)]
pub struct Docs {
    pub docs: HashMap<u64, Doc>,
    by_path: HashMap<PathBuf, u64>,
    next_id: u64,
}

pub fn language_of(path: &Path) -> Option<String> {
    let ext = path.extension()?.to_str()?.to_ascii_lowercase();
    let lang = match ext.as_str() {
        "rs" => "rust",
        "py" => "python",
        "js" | "mjs" | "cjs" => "javascript",
        "ts" | "tsx" => "typescript",
        "json" => "json",
        "md" => "markdown",
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

    pub fn info(&self) -> Value {
        json!({
            "doc": self.id,
            "path": self.path.to_string_lossy(),
            "len": self.len(),
            "version": self.version,
            "lines": self.buffer.line_count(),
            "large": self.buffer.is_large_file(),
            "modified": self.buffer.is_modified(),
            "language": self.language,
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
            bail!("range too large");
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
                it.last()
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
        client: u64,
        base: u64,
        prims: Vec<Prim>,
        lsp_positions: bool,
    ) -> Result<Vec<Applied>> {
        if base > self.version {
            bail!("base version {base} is ahead of the document ({})", self.version);
        }
        let oldest = self.log.front().map(|e| e.version - 1).unwrap_or(self.version);
        if base < oldest {
            bail!("resync: base version {base} is older than the kept log");
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
                        bail!("insert at {pos} past the end ({len})");
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
                        bail!("delete {pos}+{n} past the end ({len})");
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
        self.log.push_back(LogEntry { version: self.version, client, prims });
        while self.log.len() > LOG_KEEP {
            self.log.pop_front();
        }
        Ok(applied)
    }

    pub fn save(&mut self) -> Result<()> {
        let dir = std::env::temp_dir().join("fresh-poc-recovery");
        std::fs::create_dir_all(&dir)?;
        self.buffer.save(&dir)
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
    pub fn open(&mut self, client: u64, path: &Path) -> Result<(u64, bool)> {
        let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        if let Some(&id) = self.by_path.get(&path) {
            if let Some(d) = self.docs.get_mut(&id) {
                d.clients.insert(client);
                return Ok((id, false));
            }
        }
        let fs = Arc::new(StdFileSystem);
        let buffer = if path.exists() {
            Buffer::load_from_file(&path, LARGE_FILE_THRESHOLD, fs)?
        } else {
            let mut b = Buffer::empty(fs);
            b.set_file_path(path.clone());
            b
        };
        if buffer.is_binary() {
            return Err(anyhow!("binary file"));
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
                path: path.clone(),
                buffer,
                version: 0,
                log: VecDeque::new(),
                clients,
            },
        );
        self.by_path.insert(path, id);
        Ok((id, true))
    }

    pub fn get(&mut self, id: u64) -> Result<&mut Doc> {
        self.docs.get_mut(&id).ok_or_else(|| anyhow!("no document {id}"))
    }

    /// `client` closes `id`. The document goes when no client has it open
    /// and it has nothing unsaved; returns it then.
    pub fn close(&mut self, client: u64, id: u64) -> Option<Doc> {
        let d = self.docs.get_mut(&id)?;
        d.clients.remove(&client);
        if d.clients.is_empty() && !d.buffer.is_modified() {
            let d = self.docs.remove(&id)?;
            self.by_path.remove(&d.path);
            return Some(d);
        }
        None
    }

    pub fn by_path(&self, path: &Path) -> Option<u64> {
        let p = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
        self.by_path.get(&p).copied()
    }

    /// A client disconnected: it closes everything.
    pub fn drop_client(&mut self, client: u64) -> Vec<Doc> {
        let ids: Vec<u64> = self.docs.keys().copied().collect();
        ids.into_iter().filter_map(|id| self.close(client, id)).collect()
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
        let (id, _) = docs.open(1, &p).unwrap();
        (dir, docs, id)
    }

    #[test]
    fn reads_lines_forward_and_backward() {
        let text: String = (0..100).map(|i| format!("line {i}\n")).collect();
        let (_d, mut docs, id) = doc_with(&text);
        let d = docs.get(id).unwrap();
        let w = d.lines_forward(text.find("line 10\n").unwrap() + 3, 3).unwrap();
        assert_eq!(w["text"], "line 10\nline 11\nline 12\n");
        assert_eq!(w["first_line"], 10);
        let start = w["start"].as_u64().unwrap() as usize;
        let b = d.lines_backward(start, 2).unwrap();
        assert_eq!(b["text"], "line 8\nline 9\n");
        let b = d.lines_backward(d.len(), 1).unwrap();
        assert_eq!(b["text"], "line 99\n");
    }

    #[test]
    fn concurrent_edits_converge() {
        let (_d, mut docs, id) = doc_with("hello world\n");
        let d = docs.get(id).unwrap();
        // Client 1 inserts at 5 against version 0; client 2, also at
        // version 0, deletes "world".
        d.edit(1, 0, vec![Prim::Ins { pos: 5, text: ",".into() }], false).unwrap();
        d.edit(2, 0, vec![Prim::Del { pos: 6, len: 5 }], false).unwrap();
        assert_eq!(d.text(0, 100).unwrap(), "hello, \n");
        assert_eq!(d.version, 2);
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
        let (id, _) = docs.open(1, &p).unwrap();
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
        d.edit(1, 0, vec![Prim::Ins { pos: s, text: "EDITED ".into() }], false)
            .unwrap();
        let w = d.lines_forward(s, 1).unwrap();
        assert!(w["text"].as_str().unwrap().starts_with("EDITED a fairly"));
        // Search finds the last line from the start.
        let last = format!("{line}{}\n", n - 1);
        let at = d.search(&last, 0, false).unwrap().unwrap();
        assert_eq!(at + last.len(), d.len());
    }
}
