//! A reference backend: fold a display list into a grid of characters.
//!
//! Backends are folds over `LayoutSpec::items`. This is the smallest useful
//! one — it exists so that a test can assert on what a frame *looks like*
//! rather than on a list of rectangles, and so that the demo has something to
//! print. A real terminal backend does the same fold into cells with colours.

use fresh_ui::desc::Scrim;
use fresh_ui::render::geom::Rect;
use fresh_ui::render::spec::{Draw, Item, LayoutSpec};

/// A character grid.
#[derive(Clone, PartialEq, Eq)]
pub struct Screen {
    pub w: u16,
    pub h: u16,
    cells: Vec<char>,
}

impl Screen {
    pub fn new(w: u16, h: u16) -> Self {
        Screen {
            w,
            h,
            cells: vec![' '; w as usize * h as usize],
        }
    }

    fn put(&mut self, x: i32, y: i32, c: char, clip: Rect) {
        if x < 0 || y < 0 || x >= self.w as i32 || y >= self.h as i32 {
            return;
        }
        if !clip.contains(fresh_ui::render::geom::Point::new(x, y)) {
            return;
        }
        self.cells[y as usize * self.w as usize + x as usize] = c;
    }

    pub fn at(&self, x: u16, y: u16) -> char {
        self.cells[y as usize * self.w as usize + x as usize]
    }

    pub fn line(&self, y: u16) -> String {
        (0..self.w).map(|x| self.at(x, y)).collect()
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
        Draw::Scrim(Scrim::Opaque) => fill(s, frame, ' ', frame),
        Draw::Scrim(Scrim::Dim) => fill(s, frame, '·', frame),
        Draw::Border => border(s, r, clip),
        Draw::Lines(lines) => {
            for (i, line) in lines.iter().enumerate() {
                let y = r.y + i as i32;
                for (j, ch) in line.chars().enumerate() {
                    s.put(r.x + j as i32, y, ch, clip);
                }
            }
        }
        Draw::Scrollbar {
            offset,
            content,
            window,
        } => {
            let _ = window;
            let track = r.h.max(1);
            let (top, len) = Draw::scrollbar_thumb(*offset, *content, track);
            for i in 0..track {
                let ch = if i >= top && i < top + len {
                    '█'
                } else {
                    '│'
                };
                s.put(r.x, r.y + i as i32, ch, clip);
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

fn border(s: &mut Screen, r: Rect, clip: Rect) {
    if r.w < 2 || r.h < 2 {
        return;
    }
    let (l, t) = (r.x, r.y);
    let (rr, b) = (r.right() - 1, r.bottom() - 1);
    for x in l..=rr {
        s.put(x, t, '─', clip);
        s.put(x, b, '─', clip);
    }
    for y in t..=b {
        s.put(l, y, '│', clip);
        s.put(rr, y, '│', clip);
    }
    s.put(l, t, '┌', clip);
    s.put(rr, t, '┐', clip);
    s.put(l, b, '└', clip);
    s.put(rr, b, '┘', clip);
}
