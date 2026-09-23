//! **Spatial focus movement**: the control an arrow key reaches, by where
//! things are on screen rather than by reading order.
//!
//! Tab walks the focus ring in reading order, and that is right for Tab. An
//! arrow is a statement about direction: ↓ from a field in the left column of
//! a two-column form means the field under it, not the one to its right that
//! happens to come next in the ring. So an arrow the focused control did not
//! use moves to the nearest focusable *in that direction*, measured from the
//! rectangles layout gave them.
//!
//! The rule, in one place:
//!
//! 1. Only candidates wholly past the current control's edge in the arrow's
//!    direction are considered (a control below starts at or after this one's
//!    bottom edge).
//! 2. A candidate whose span across the arrow's axis overlaps this one's (it is
//!    "in the beam") beats one that does not — ↓ from a field lands on the
//!    field under it, not on a nearer button off to the side.
//! 3. Among those, the smaller gap along the axis wins, then the smaller
//!    distance between centres across it, then reading order.
//!
//! Pure: rectangles in, an index out.

use crate::render::geom::Rect;

/// An arrow direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

/// The index into `candidates` an arrow in `dir` reaches from `from`, or
/// `None` when nothing lies that way. `candidates` in reading order; an entry
/// equal to `from` (the focused control itself) is never chosen.
pub fn nearest(from: Rect, candidates: &[Rect], dir: Direction) -> Option<usize> {
    let (f_lo, f_hi) = span_across(from, dir);
    candidates
        .iter()
        .enumerate()
        .filter(|(_, c)| **c != from && c.w > 0 && c.h > 0)
        .filter_map(|(i, c)| {
            let gap = gap_along(from, *c, dir)?;
            let (c_lo, c_hi) = span_across(*c, dir);
            let in_beam = c_lo < f_hi && f_lo < c_hi;
            let across = (centre(c_lo, c_hi) - centre(f_lo, f_hi)).abs();
            Some(((!in_beam) as u8, gap, across, i))
        })
        .min()
        .map(|(_, _, _, i)| i)
}

/// The distance from `from`'s edge to `c`'s near edge along `dir`, when `c`
/// lies wholly past that edge; `None` otherwise.
fn gap_along(from: Rect, c: Rect, dir: Direction) -> Option<i32> {
    let (f_top, f_bottom) = (from.y, from.y + from.h as i32);
    let (f_left, f_right) = (from.x, from.x + from.w as i32);
    let (c_top, c_bottom) = (c.y, c.y + c.h as i32);
    let (c_left, c_right) = (c.x, c.x + c.w as i32);
    let gap = match dir {
        Direction::Down => c_top - f_bottom,
        Direction::Up => f_top - c_bottom,
        Direction::Right => c_left - f_right,
        Direction::Left => f_left - c_right,
    };
    (gap >= 0).then_some(gap)
}

/// The span of `r` across `dir`'s axis: its columns for ↑/↓, its rows for
/// ←/→, as `[lo, hi)`.
fn span_across(r: Rect, dir: Direction) -> (i32, i32) {
    match dir {
        Direction::Up | Direction::Down => (r.x, r.x + r.w as i32),
        Direction::Left | Direction::Right => (r.y, r.y + r.h as i32),
    }
}

/// Twice the centre of `[lo, hi)`, so it stays an integer.
fn centre(lo: i32, hi: i32) -> i32 {
    lo + hi
}

#[cfg(test)]
mod tests {
    use super::*;

    fn r(x: i32, y: i32, w: u16, h: u16) -> Rect {
        Rect { x, y, w, h }
    }

    /// A two-column form: ↓ from the left column stays in it, though the
    /// right-hand field comes next in reading order.
    #[test]
    fn down_stays_in_its_column() {
        let left_a = r(0, 0, 20, 1);
        let right_a = r(30, 0, 20, 1);
        let left_b = r(0, 2, 20, 1);
        let right_b = r(30, 2, 20, 1);
        let ring = [left_a, right_a, left_b, right_b];
        assert_eq!(nearest(left_a, &ring, Direction::Down), Some(2));
        assert_eq!(nearest(right_a, &ring, Direction::Down), Some(3));
        assert_eq!(nearest(left_b, &ring, Direction::Up), Some(0));
        assert_eq!(nearest(left_a, &ring, Direction::Right), Some(1));
        assert_eq!(nearest(right_b, &ring, Direction::Left), Some(2));
    }

    #[test]
    fn nothing_that_way_is_none() {
        let a = r(0, 0, 10, 1);
        let b = r(0, 2, 10, 1);
        assert_eq!(nearest(a, &[a, b], Direction::Up), None);
        assert_eq!(nearest(b, &[a, b], Direction::Down), None);
        assert_eq!(nearest(a, &[a, b], Direction::Left), None);
    }

    /// In the beam beats nearer-but-aside: ↓ from a wide field reaches the
    /// field under it, not a button that starts sooner off to one side.
    #[test]
    fn the_beam_beats_a_nearer_control_off_to_the_side() {
        let field = r(10, 0, 20, 1);
        let aside = r(40, 1, 6, 1);
        let under = r(10, 3, 20, 1);
        assert_eq!(
            nearest(field, &[field, aside, under], Direction::Down),
            Some(2)
        );
    }

    /// With nothing in the beam, the nearest row wins, then the nearest
    /// centre across it — a footer's buttons from a field above them.
    #[test]
    fn out_of_the_beam_the_nearest_row_then_the_nearest_centre_wins() {
        let field = r(0, 0, 8, 1);
        let ok = r(20, 3, 6, 1);
        let cancel = r(30, 3, 8, 1);
        let far = r(10, 6, 6, 1);
        assert_eq!(
            nearest(field, &[field, ok, cancel, far], Direction::Down),
            Some(1)
        );
    }
}
