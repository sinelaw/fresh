//! A reference backend: fold a display list into a grid of characters.
//!
//! Backends are folds over `LayoutSpec::items`. This is the smallest useful
//! one — it exists so that a test can assert on what a frame *looks like*
//! rather than on a list of rectangles, and so that the demo has something to
//! print. A real terminal backend does the same fold into cells with colours.

use fresh_ui::desc::Scrim;
use fresh_ui::glyph::glyphs_in;
use fresh_ui::render::geom::Rect;
use fresh_ui::render::spec::{Draw, Item, LayoutSpec};

/// A grid of cells, each holding the symbol painted into it.
///
/// A symbol is a grapheme cluster — one `char` for most text, a base and its
/// marks for a composed one — and a wide cluster occupies its first cell and
/// leaves the cells after it *empty* (`""`), which is how `line` comes out the
/// right length in columns and how a test can tell a continuation cell from a
/// blank one. See `fresh_ui::glyph` for the policy.
#[derive(Clone, PartialEq, Eq)]
pub struct Screen {
    pub w: u16,
    pub h: u16,
    cells: Vec<String>,
}

impl Screen {
    pub fn new(w: u16, h: u16) -> Self {
        Screen {
            w,
            h,
            cells: vec![" ".to_string(); w as usize * h as usize],
        }
    }

    fn put(&mut self, x: i32, y: i32, c: char, clip: Rect) {
        let mut b = [0u8; 4];
        self.put_symbol(x, y, c.encode_utf8(&mut b), 1, clip);
    }

    /// Paint `sym` at `(x, y)` and blank the `w - 1` continuation cells after
    /// it. The caller has already clipped the columns (`glyph::glyphs_in`).
    fn put_symbol(&mut self, x: i32, y: i32, sym: &str, w: u16, clip: Rect) {
        if x < 0 || y < 0 || x >= self.w as i32 || y >= self.h as i32 {
            return;
        }
        if !clip.contains(fresh_ui::render::geom::Point::new(x, y)) {
            return;
        }
        let row = y as usize * self.w as usize;
        let end = row + self.w as usize;
        let i = row + x as usize;
        // Painting over the continuation of a wide glyph cuts that glyph in
        // half; what is left of it shows as a blank, as it would on a
        // terminal that cannot draw half of `你`.
        if self.cells[i].is_empty() {
            let mut j = i;
            while j > row && self.cells[j].is_empty() {
                j -= 1;
            }
            self.cells[j] = " ".to_string();
        }
        self.cells[i] = sym.to_string();
        for k in 1..w as usize {
            if i + k < end {
                self.cells[i + k].clear();
            }
        }
        // And a narrower glyph over the head of a wide one orphans the wide
        // one's continuation cells, which then belong to nothing.
        let mut j = i + w as usize;
        while j < end && self.cells[j].is_empty() {
            self.cells[j] = " ".to_string();
            j += 1;
        }
    }

    /// The symbol in a cell: `" "` when blank, `""` when the cell is the
    /// continuation of a wide glyph to its left.
    pub fn symbol(&self, x: u16, y: u16) -> &str {
        &self.cells[y as usize * self.w as usize + x as usize]
    }

    /// The first `char` of the cell's symbol; a blank for a continuation
    /// cell, which shows nothing of its own.
    pub fn at(&self, x: u16, y: u16) -> char {
        self.symbol(x, y).chars().next().unwrap_or(' ')
    }

    /// Whether the cell is the second (or later) column of a wide glyph.
    pub fn is_continuation(&self, x: u16, y: u16) -> bool {
        self.symbol(x, y).is_empty()
    }

    pub fn line(&self, y: u16) -> String {
        (0..self.w).map(|x| self.symbol(x, y)).collect()
    }

    /// Every row, trailing blanks trimmed, one per line.
    pub fn text(&self) -> String {
        (0..self.h)
            .map(|y| self.line(y).trim_end().to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Whether any row contains `needle`.
    pub fn contains(&self, needle: &str) -> bool {
        (0..self.h).any(|y| self.line(y).contains(needle))
    }

    /// The first row containing `needle`.
    pub fn row_of(&self, needle: &str) -> Option<u16> {
        (0..self.h).find(|y| self.line(*y).contains(needle))
    }
}

impl std::fmt::Display for Screen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text())
    }
}

impl std::fmt::Debug for Screen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\n{}\n", self.text())
    }
}

/// Draw a display list. Paint order is list order, so later items overwrite
/// earlier ones — which is all "on top" means here.
pub fn render(spec: &LayoutSpec) -> Screen {
    render_with(spec, |_| None)
}

/// As `render`, with a mapping from theme provenance to the character a filled
/// region is drawn with. This is what per-item `ThemeKey` is for: the backend
/// decides how a region looks, and the description only says what it is.
pub fn render_with(spec: &LayoutSpec, fill_char: impl Fn(&str) -> Option<char>) -> Screen {
    let mut s = Screen::new(spec.frame.w, spec.frame.h);
    let frame = Rect::from_size(spec.frame);
    for item in &spec.items {
        draw(&mut s, item, frame, &fill_char);
    }
    s
}

fn draw(s: &mut Screen, item: &Item, frame: Rect, fill_char: &impl Fn(&str) -> Option<char>) {
    let clip = item.clip.intersect(frame);
    let r = item.rect;
    match &item.draw {
        Draw::Fill => fill(s, r, fill_char(item.theme.as_str()).unwrap_or(' '), clip),
        // A wash keeps the text under it; the screen model has no ground to
        // recolour, so it paints nothing.
        Draw::Wash => {}
        Draw::Scrim(Scrim::Opaque) => fill(s, frame, ' ', frame),
        Draw::Scrim(Scrim::Dim) => fill(s, frame, '·', frame),
        Draw::Border(bs) => border(s, r, clip, *bs),
        Draw::Lines(lines) => {
            // Clipped to the item's own rect as well as its inherited one: an
            // item declares how much room it has, and a run longer than that
            // would otherwise paint straight through whatever encloses it.
            let clip = clip.intersect(r);
            for (i, line) in lines.iter().enumerate() {
                let y = r.y + i as i32;
                // By display width, not by char: the library says which
                // columns each cluster takes (`glyph`), and a backend that
                // stepped one cell per char would paint `你好` into two cells
                // of the four layout reserved.
                for g in glyphs_in(line, r.x, clip.x, clip.right()) {
                    s.put_symbol(g.x, y, g.text, g.width, clip);
                }
            }
        }
        Draw::Scrollbar {
            offset,
            content,
            window,
            axis,
            marks,
        } => {
            let track = match axis {
                fresh_ui::Axis::Vertical => r.h.max(1),
                fresh_ui::Axis::Horizontal => r.w.max(1),
            };
            let (top, len) = Draw::scrollbar_thumb(*offset, *content, u32::from(*window), track);
            for i in 0..track {
                let ch = if marks.iter().any(|m| m.at == i) {
                    '▌'
                } else if i >= top && i < top + len {
                    '█'
                } else {
                    '│'
                };
                match axis {
                    fresh_ui::Axis::Vertical => s.put(r.x, r.y + i as i32, ch, clip),
                    fresh_ui::Axis::Horizontal => s.put(r.x + i as i32, r.y, ch, clip),
                }
            }
        }
        // A marker for a backend that supports selection; it draws nothing.
        Draw::Selectable => {}
        Draw::Host(_) => fill(s, r, '▒', clip),
    }
}

fn fill(s: &mut Screen, r: Rect, ch: char, clip: Rect) {
    for y in 0..r.h {
        for x in 0..r.w {
            s.put(r.x + x as i32, r.y + y as i32, ch, clip);
        }
    }
}

fn border(s: &mut Screen, r: Rect, clip: Rect, bs: fresh_ui::BorderStyle) {
    if r.w < 2 || r.h < 2 {
        return;
    }
    let (l, t) = (r.x, r.y);
    let (rr, b) = (r.right() - 1, r.bottom() - 1);
    // The reference backend honours the style, so a golden file records which
    // corner set the description asked for rather than one this file picked.
    let (h, v, tl, tr, br, bl) = bs.glyphs();
    for x in l..=rr {
        s.put(x, t, h, clip);
        s.put(x, b, h, clip);
    }
    for y in t..=b {
        s.put(l, y, v, clip);
        s.put(rr, y, v, clip);
    }
    s.put(l, t, tl, clip);
    s.put(rr, t, tr, clip);
    s.put(l, b, bl, clip);
    s.put(rr, b, br, clip);
}
