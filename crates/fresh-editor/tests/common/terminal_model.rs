//! A terminal emulator model that renders the bytes Fresh actually writes.
//!
//! The harness's `TestBackend` shows what ratatui *meant* to draw; it cannot
//! show what a terminal ends up displaying when the stream of cell writes is
//! replayed with real cursor semantics. The `vt100` parser can, but it sizes
//! every codepoint on its own, so an emoji presentation sequence like `❤️` is
//! one column to it — exactly the terminals where Fresh's output happened to
//! be correct.
//!
//! [`TerminalModel`] is fed the frames the harness renders through the same
//! backend stack the `fresh` binary uses (`ClusterSafeBackend` over
//! `CrosstermBackend`) and interprets the resulting bytes with a chosen
//! [`WidthPolicy`]:
//!
//! * cursor positioning (`CSI row ; col H`) and erases (`CSI 2 J`, `CSI K`),
//! * printable text, segmented into grapheme clusters, each advancing the
//!   cursor by the policy's width, and erasing any wide character it
//!   partially overwrites (as xterm-family terminals do),
//! * OSC 8 hyperlinks, recorded per cell,
//! * everything else (SGR, mode switches) is accepted and ignored.

use fresh::view::ui::terminal_flush::ClusterSafeBackend;
use ratatui::backend::CrosstermBackend;
use ratatui::buffer::Buffer;
use ratatui::Terminal;
use std::io::{self, Write};
use std::sync::{Arc, Mutex};
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

/// How wide the modelled terminal draws a grapheme cluster.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WidthPolicy {
    /// The cluster's Unicode width: `❤️`, keycaps and flags are two columns.
    /// tmux, kitty, WezTerm, iTerm2, foot.
    ClusterWidth,
    /// The sum of its codepoints' widths, the way terminals without grapheme
    /// clustering advance: `❤️` (U+2764 U+FE0F) is one column, a flag two, a
    /// ZWJ family of four eight. xterm, older VTE, the `vt100` crate.
    CodepointSum,
}

impl WidthPolicy {
    fn width(self, cluster: &str) -> usize {
        match self {
            WidthPolicy::ClusterWidth => cluster.width(),
            WidthPolicy::CodepointSum => cluster.chars().filter_map(|c| c.width()).sum(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ModelCell {
    /// Nothing drawn (or erased).
    Blank,
    /// A drawn cluster, `width` columns wide, with the hyperlink active when
    /// it was written.
    Glyph {
        text: String,
        width: usize,
        link: Option<String>,
    },
    /// A column covered by the wide glyph to its left.
    Covered,
}

/// A `Write` sink shared between the backend and the model.
#[derive(Clone, Default)]
struct SharedSink(Arc<Mutex<Vec<u8>>>);

impl Write for SharedSink {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.lock().unwrap().extend_from_slice(buf);
        Ok(buf.len())
    }
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

/// The modelled terminal plus the production backend stack that feeds it.
pub struct TerminalModel {
    policy: WidthPolicy,
    cols: usize,
    rows: usize,
    grid: Vec<Vec<ModelCell>>,
    cursor: (usize, usize),
    link: Option<String>,
    sink: SharedSink,
    terminal: Terminal<ClusterSafeBackend<CrosstermBackend<SharedSink>>>,
    /// Bytes not yet interpreted because they end mid-sequence.
    pending: Vec<u8>,
    /// Every byte the backend wrote, for assertions on the raw stream.
    pub transcript: Vec<u8>,
}

impl TerminalModel {
    pub fn new(cols: u16, rows: u16, policy: WidthPolicy) -> Self {
        let sink = SharedSink::default();
        let backend = ClusterSafeBackend::new(CrosstermBackend::new(sink.clone()));
        let terminal = Terminal::with_options(
            backend,
            ratatui::TerminalOptions {
                viewport: ratatui::Viewport::Fixed(ratatui::layout::Rect::new(0, 0, cols, rows)),
            },
        )
        .expect("model terminal");
        Self {
            policy,
            cols: cols as usize,
            rows: rows as usize,
            grid: vec![vec![ModelCell::Blank; cols as usize]; rows as usize],
            cursor: (0, 0),
            link: None,
            sink,
            terminal,
            pending: Vec::new(),
            transcript: Vec::new(),
        }
    }

    /// Flush `frame` through the backend stack (ratatui's diff against the
    /// previous frame included) and interpret what it wrote.
    pub fn present(&mut self, frame: &Buffer) {
        self.terminal
            .draw(|f| {
                let area = f.area();
                let buf = f.buffer_mut();
                for y in area.top()..area.bottom() {
                    for x in area.left()..area.right() {
                        buf[(x, y)] = frame[(x, y)].clone();
                    }
                }
            })
            .expect("model draw");
        let bytes = std::mem::take(&mut *self.sink.0.lock().unwrap());
        self.transcript.extend_from_slice(&bytes);
        self.pending.extend_from_slice(&bytes);
        let pending = std::mem::take(&mut self.pending);
        let consumed = self.interpret(&pending);
        self.pending = pending[consumed..].to_vec();
    }

    /// Interpret as much of `bytes` as forms complete sequences; return how
    /// many bytes were consumed.
    fn interpret(&mut self, bytes: &[u8]) -> usize {
        let mut i = 0;
        let mut text = String::new();
        while i < bytes.len() {
            match bytes[i] {
                0x1b => {
                    let Some(len) = escape_len(&bytes[i..]) else {
                        break;
                    };
                    self.print(&std::mem::take(&mut text));
                    self.escape(&bytes[i..i + len]);
                    i += len;
                }
                b'\r' => {
                    self.print(&std::mem::take(&mut text));
                    self.cursor.0 = 0;
                    i += 1;
                }
                b'\n' => {
                    self.print(&std::mem::take(&mut text));
                    self.cursor.1 = (self.cursor.1 + 1).min(self.rows - 1);
                    i += 1;
                }
                b if b < 0x20 || b == 0x7f => i += 1,
                _ => {
                    let len = utf8_len(bytes[i]);
                    if i + len > bytes.len() {
                        break;
                    }
                    text.push_str(std::str::from_utf8(&bytes[i..i + len]).expect("utf-8"));
                    i += len;
                }
            }
        }
        self.print(&text);
        i
    }

    fn escape(&mut self, seq: &[u8]) {
        let seq = std::str::from_utf8(seq).expect("utf-8 escape");
        if let Some(body) = seq.strip_prefix("\x1b[") {
            let final_byte = body.chars().last().unwrap_or(' ');
            let params = &body[..body.len() - final_byte.len_utf8()];
            let nums: Vec<usize> = params.split(';').map(|p| p.parse().unwrap_or(0)).collect();
            match final_byte {
                'H' | 'f' => {
                    let row = nums.first().copied().unwrap_or(1).max(1) - 1;
                    let col = nums.get(1).copied().unwrap_or(1).max(1) - 1;
                    self.cursor = (col.min(self.cols - 1), row.min(self.rows - 1));
                }
                'J' if nums.first() == Some(&2) => {
                    for row in &mut self.grid {
                        row.fill(ModelCell::Blank);
                    }
                }
                'K' => {
                    let (x, y) = self.cursor;
                    self.erase_overlaps(x, y, self.cols - x);
                    for cell in &mut self.grid[y][x..] {
                        *cell = ModelCell::Blank;
                    }
                }
                _ => {}
            }
        } else if let Some(body) = seq.strip_prefix("\x1b]") {
            let body = body
                .strip_suffix('\x07')
                .or_else(|| body.strip_suffix("\x1b\\"))
                .unwrap_or(body);
            if let Some(rest) = body.strip_prefix("8;") {
                let url = rest.split_once(';').map(|(_, url)| url).unwrap_or("");
                self.link = (!url.is_empty()).then(|| url.to_string());
            }
        }
    }

    /// Blank any wide glyph that overlaps columns `x .. x + width` on row `y`
    /// without starting inside it, the way terminals erase a wide character
    /// whose half is overwritten.
    fn erase_overlaps(&mut self, x: usize, y: usize, width: usize) {
        let row = &mut self.grid[y];
        let len = row.len();
        let end = (x + width).min(len);
        // A glyph starting left of `x` that reaches into the range.
        if matches!(row.get(x), Some(ModelCell::Covered)) {
            let mut head = x;
            while head > 0 && matches!(row[head], ModelCell::Covered) {
                head -= 1;
            }
            let span = match &row[head] {
                ModelCell::Glyph { width, .. } => *width,
                _ => 1,
            };
            for cell in &mut row[head..(head + span).min(len)] {
                *cell = ModelCell::Blank;
            }
        }
        // A glyph starting inside the range that reaches past it.
        for col in x..end {
            if let ModelCell::Glyph { width, .. } = row[col] {
                for cell in &mut row[col..(col + width).min(len)] {
                    *cell = ModelCell::Blank;
                }
            }
        }
    }

    fn print(&mut self, text: &str) {
        for cluster in text.graphemes(true) {
            let width = self.policy.width(cluster).max(1);
            let (x, y) = self.cursor;
            if x + width > self.cols {
                // Fresh never writes past the right edge; clip like a
                // terminal in pending-wrap state would.
                continue;
            }
            self.erase_overlaps(x, y, width);
            self.grid[y][x] = ModelCell::Glyph {
                text: cluster.to_string(),
                width,
                link: self.link.clone(),
            };
            for col in x + 1..x + width {
                self.grid[y][col] = ModelCell::Covered;
            }
            self.cursor.0 = (x + width).min(self.cols - 1);
        }
    }

    /// The screen as columns: each column holds the cluster that starts there,
    /// or nothing for a blank or a column a wide cluster covers.
    pub fn columns(&self, row: usize) -> Vec<String> {
        self.grid[row]
            .iter()
            .map(|cell| match cell {
                ModelCell::Glyph { text, .. } if text != " " => text.clone(),
                _ => String::new(),
            })
            .collect()
    }

    /// The hyperlink target of each column, `None` where there is none.
    pub fn links(&self, row: usize) -> Vec<Option<String>> {
        let mut out = Vec::with_capacity(self.cols);
        let mut current: Option<String> = None;
        for cell in &self.grid[row] {
            match cell {
                ModelCell::Glyph { link, .. } => {
                    current = link.clone();
                    out.push(link.clone());
                }
                ModelCell::Covered => out.push(current.clone()),
                ModelCell::Blank => {
                    current = None;
                    out.push(None);
                }
            }
        }
        out
    }

    pub fn row_text(&self, row: usize) -> String {
        self.columns(row)
            .iter()
            .map(|c| if c.is_empty() { " " } else { c.as_str() })
            .collect::<String>()
            .trim_end()
            .to_string()
    }

    pub fn rows(&self) -> usize {
        self.rows
    }
}

/// The columns `frame` means to show on `row`, in the same shape as
/// [`TerminalModel::columns`]: the cluster starting at each column, nothing
/// for blanks and for the columns a wide cluster covers.
pub fn expected_columns(frame: &Buffer, row: u16) -> Vec<String> {
    let area = frame.area;
    let mut out = vec![String::new(); area.width as usize];
    let mut x = area.left();
    while x < area.right() {
        let symbol = crate::common::harness::strip_osc8(frame[(x, row)].symbol());
        let width = symbol.width().max(1) as u16;
        if symbol != " " {
            out[(x - area.left()) as usize] = symbol;
        }
        x += width;
    }
    out
}

/// The hyperlink target `frame` gives each column of `row`, in the shape of
/// [`TerminalModel::links`]: a wide cluster's link covers its columns.
pub fn expected_links(frame: &Buffer, row: u16) -> Vec<Option<String>> {
    use fresh::view::ui::terminal_flush::{cell_hyperlink, visible_symbol};
    let area = frame.area;
    let mut out = vec![None; area.width as usize];
    let mut x = area.left();
    while x < area.right() {
        let symbol = frame[(x, row)].symbol();
        let width = visible_symbol(symbol).width().max(1) as u16;
        let link = cell_hyperlink(symbol).map(str::to_string);
        for col in x..(x + width).min(area.right()) {
            out[(col - area.left()) as usize] = link.clone();
        }
        x += width;
    }
    out
}

fn utf8_len(first: u8) -> usize {
    match first {
        0x00..=0x7f => 1,
        0xc0..=0xdf => 2,
        0xe0..=0xef => 3,
        _ => 4,
    }
}

/// Length of the escape sequence at the start of `bytes`, or `None` if it is
/// not complete yet.
fn escape_len(bytes: &[u8]) -> Option<usize> {
    match bytes.get(1)? {
        b'[' => bytes[2..]
            .iter()
            .position(|b| (0x40..=0x7e).contains(b))
            .map(|p| p + 3),
        b']' => {
            let mut i = 2;
            while i < bytes.len() {
                if bytes[i] == 0x07 {
                    return Some(i + 1);
                }
                if bytes[i] == 0x1b && bytes.get(i + 1) == Some(&b'\\') {
                    return Some(i + 2);
                }
                i += 1;
            }
            None
        }
        _ => Some(2),
    }
}
