//! Integer-cell geometry.
//!
//! Everything is in whole terminal cells. There is no sub-cell arithmetic and
//! no rounding stage, so a layout run is reproducible: the same inputs give the
//! same rectangles, on every run and every platform.

/// Box constraints: what a parent permits a child to be.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct Constraints {
    pub min_w: u16,
    pub max_w: u16,
    pub min_h: u16,
    pub max_h: u16,
}

impl Constraints {
    pub const fn new(min_w: u16, max_w: u16, min_h: u16, max_h: u16) -> Self {
        Constraints {
            min_w,
            max_w,
            min_h,
            max_h,
        }
    }

    /// Exactly this size, no choice.
    pub const fn tight(s: Size) -> Self {
        Constraints {
            min_w: s.w,
            max_w: s.w,
            min_h: s.h,
            max_h: s.h,
        }
    }

    /// Anything up to this size.
    pub const fn loose(s: Size) -> Self {
        Constraints {
            min_w: 0,
            max_w: s.w,
            min_h: 0,
            max_h: s.h,
        }
    }

    pub const fn tight_w(self, w: u16) -> Self {
        Constraints {
            min_w: w,
            max_w: w,
            ..self
        }
    }

    pub const fn tight_h(self, h: u16) -> Self {
        Constraints {
            min_h: h,
            max_h: h,
            ..self
        }
    }

    pub const fn loose_w(self, max: u16) -> Self {
        Constraints {
            min_w: 0,
            max_w: max,
            ..self
        }
    }

    pub const fn loose_h(self, max: u16) -> Self {
        Constraints {
            min_h: 0,
            max_h: max,
            ..self
        }
    }

    /// A node whose size cannot change as a result of anything below it is a
    /// relayout boundary: a change inside it stops here instead of walking to
    /// the root.
    pub const fn is_tight(&self) -> bool {
        self.min_w == self.max_w && self.min_h == self.max_h
    }

    /// The nearest size this constraint permits.
    pub fn constrain(&self, s: Size) -> Size {
        Size {
            w: s.w.clamp(self.min_w, self.max_w),
            h: s.h.clamp(self.min_h, self.max_h),
        }
    }

    pub const fn max(&self) -> Size {
        Size {
            w: self.max_w,
            h: self.max_h,
        }
    }

    pub const fn min(&self) -> Size {
        Size {
            w: self.min_w,
            h: self.min_h,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default, Hash)]
pub struct Size {
    pub w: u16,
    pub h: u16,
}

impl Size {
    pub const ZERO: Size = Size { w: 0, h: 0 };

    pub const fn new(w: u16, h: u16) -> Self {
        Size { w, h }
    }

    pub const fn is_empty(&self) -> bool {
        self.w == 0 || self.h == 0
    }
}

/// A position. Signed, because content scrolled above or left of its viewport
/// has a negative origin; clipping, not saturation, is what removes it.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default, Hash)]
pub struct Point {
    pub x: i32,
    pub y: i32,
}

impl Point {
    pub const ZERO: Point = Point { x: 0, y: 0 };

    pub const fn new(x: i32, y: i32) -> Self {
        Point { x, y }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default, Hash)]
pub struct Rect {
    pub x: i32,
    pub y: i32,
    pub w: u16,
    pub h: u16,
}

impl Rect {
    pub const ZERO: Rect = Rect {
        x: 0,
        y: 0,
        w: 0,
        h: 0,
    };

    pub const fn new(x: i32, y: i32, w: u16, h: u16) -> Self {
        Rect { x, y, w, h }
    }

    pub const fn from_size(s: Size) -> Self {
        Rect {
            x: 0,
            y: 0,
            w: s.w,
            h: s.h,
        }
    }

    pub const fn at(p: Point, s: Size) -> Self {
        Rect {
            x: p.x,
            y: p.y,
            w: s.w,
            h: s.h,
        }
    }

    pub const fn size(&self) -> Size {
        Size {
            w: self.w,
            h: self.h,
        }
    }

    pub const fn origin(&self) -> Point {
        Point {
            x: self.x,
            y: self.y,
        }
    }

    pub const fn is_empty(&self) -> bool {
        self.w == 0 || self.h == 0
    }

    pub const fn right(&self) -> i32 {
        self.x + self.w as i32
    }

    pub const fn bottom(&self) -> i32 {
        self.y + self.h as i32
    }

    pub fn contains(&self, p: Point) -> bool {
        p.x >= self.x && p.x < self.right() && p.y >= self.y && p.y < self.bottom()
    }

    pub fn intersect(&self, o: Rect) -> Rect {
        let x = self.x.max(o.x);
        let y = self.y.max(o.y);
        let r = self.right().min(o.right());
        let b = self.bottom().min(o.bottom());
        if r <= x || b <= y {
            Rect::ZERO
        } else {
            Rect {
                x,
                y,
                w: (r - x) as u16,
                h: (b - y) as u16,
            }
        }
    }

    pub fn translate(&self, dx: i32, dy: i32) -> Rect {
        Rect {
            x: self.x + dx,
            y: self.y + dy,
            ..*self
        }
    }
}

/// Divide `total` cells among `weights`, deterministically.
///
/// Integer division leaves a remainder. It is handed out by largest fractional
/// part, ties resolved toward earlier entries — so the result depends only on
/// the inputs. An unspecified rule here produces one-cell gaps and overlaps
/// that move between runs.
pub fn distribute(total: u16, weights: &[u16]) -> Vec<u16> {
    let sum: u32 = weights.iter().map(|w| *w as u32).sum();
    if sum == 0 {
        return vec![0; weights.len()];
    }
    let total = total as u32;
    let mut out: Vec<u16> = Vec::with_capacity(weights.len());
    let mut remainders: Vec<(u32, usize)> = Vec::with_capacity(weights.len());
    let mut given: u32 = 0;
    for (i, &w) in weights.iter().enumerate() {
        let exact = total * w as u32;
        let base = exact / sum;
        out.push(base as u16);
        given += base;
        remainders.push((exact - base * sum, i));
    }
    // Largest remainder first; equal remainders go to the earlier index.
    remainders.sort_by(|a, b| b.0.cmp(&a.0).then(a.1.cmp(&b.1)));
    let mut left = total - given;
    for (_, i) in remainders {
        if left == 0 {
            break;
        }
        out[i] += 1;
        left -= 1;
    }
    out
}
