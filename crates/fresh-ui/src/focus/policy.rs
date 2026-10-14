//! Traversal policies.
//!
//! Which element Tab reaches next is a property of the surface, not of the
//! framework: forms want reading order, a split grid wants directional
//! movement, a hand-ordered dialog wants explicit ordinals.

use crate::element::ElementId;
use crate::render::geom::Rect;

/// Traversal addresses focusables by the element that owns them. The
/// registration handle itself is `focus::FocusTarget`.
pub type FocusTarget = ElementId;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FocusDir {
    Next,
    Prev,
    Up,
    Down,
    Left,
    Right,
}

#[derive(Clone, Debug)]
pub struct FocusEntry {
    pub id: FocusTarget,
    pub ordinal: Option<i32>,
    pub rect: Rect,
    /// The innermost group this stop is in — the element that declared the
    /// group, and the stop it is entered at (`Node::enters_at`) — or `None`
    /// for a stop in no group.
    pub group: Option<(ElementId, Option<FocusTarget>)>,
}

/// The focusables traversal may reach, in tree order.
#[derive(Clone, Debug, Default)]
pub struct FocusScope {
    pub nodes: Vec<FocusEntry>,
}

impl FocusScope {
    pub fn index_of(&self, id: FocusTarget) -> Option<usize> {
        self.nodes.iter().position(|n| n.id == id)
    }

    fn group_of(&self, id: FocusTarget) -> Option<(ElementId, Option<FocusTarget>)> {
        self.nodes.iter().find(|n| n.id == id)?.group
    }

    /// Where a step that would land on `to`, coming from `from`, actually
    /// lands: a group entered from outside is entered at its own entry stop,
    /// when that stop is on the ring. A step within a group, or into a group
    /// with no usable entry, lands where it would have.
    pub fn entered_at(&self, from: Option<FocusTarget>, to: FocusTarget) -> FocusTarget {
        let Some((g, entry)) = self.group_of(to) else {
            return to;
        };
        let already_inside = from
            .and_then(|f| self.group_of(f))
            .is_some_and(|(fg, _)| fg == g);
        if already_inside {
            return to;
        }
        match entry {
            Some(e) if self.index_of(e).is_some() => e,
            _ => to,
        }
    }

    /// Reading order, with explicit ordinals taking precedence over position.
    pub fn ordered(&self) -> Vec<FocusTarget> {
        let mut idx: Vec<usize> = (0..self.nodes.len()).collect();
        idx.sort_by_key(|&i| (self.nodes[i].ordinal.unwrap_or(i32::MAX), i));
        idx.into_iter().map(|i| self.nodes[i].id).collect()
    }
}

pub trait TraversalPolicy {
    fn next(
        &self,
        scope: &FocusScope,
        from: Option<FocusTarget>,
        dir: FocusDir,
    ) -> Option<FocusTarget>;
}

/// The default: reading order, wrapping at the ends, with directional moves
/// falling back to next/previous.
#[derive(Debug, Default, Clone, Copy)]
pub struct ReadingOrder;

impl TraversalPolicy for ReadingOrder {
    fn next(
        &self,
        scope: &FocusScope,
        from: Option<FocusTarget>,
        dir: FocusDir,
    ) -> Option<FocusTarget> {
        let order = scope.ordered();
        if order.is_empty() {
            return None;
        }
        let forward = matches!(dir, FocusDir::Next | FocusDir::Down | FocusDir::Right);
        let Some(cur) = from.and_then(|f| order.iter().position(|x| *x == f)) else {
            let edge = if forward {
                order[0]
            } else {
                order[order.len() - 1]
            };
            return Some(scope.entered_at(None, edge));
        };
        let n = order.len();
        let to = if forward {
            order[(cur + 1) % n]
        } else {
            order[(cur + n - 1) % n]
        };
        Some(scope.entered_at(from, to))
    }
}

/// Geometric traversal: an arrow reaches the nearest focusable *in its
/// direction*, measured from the rectangles layout gave them. Next/Prev
/// (Tab, Shift+Tab) still use reading order.
///
/// The rule, in one place:
///
/// 1. Only stops wholly past the current one's edge in the arrow's direction
///    are considered (↓ considers what starts at or below this one's bottom
///    edge).
/// 2. A stop whose span across the arrow's axis overlaps this one's — it is
///    *in the beam* — beats one that does not: ↓ from a field lands on the
///    field under it, not on a nearer button off to one side, and in a
///    two-column form ↓ stays in its column.
/// 3. Among those, the smaller gap along the axis wins, then the smaller
///    distance between centres across it, then reading order.
///
/// Nothing that way is `None`: an arrow at the edge goes nowhere, rather than
/// wrapping, so a surface around the scope can still answer it.
#[derive(Debug, Default, Clone, Copy)]
pub struct Directional;

impl TraversalPolicy for Directional {
    fn next(
        &self,
        scope: &FocusScope,
        from: Option<FocusTarget>,
        dir: FocusDir,
    ) -> Option<FocusTarget> {
        if matches!(dir, FocusDir::Next | FocusDir::Prev) {
            return ReadingOrder.next(scope, from, dir);
        }
        let Some(from) = from else {
            return ReadingOrder.next(scope, None, dir);
        };
        let here = scope.nodes.iter().find(|n| n.id == from)?.rect;
        let (f_lo, f_hi) = span_across(here, dir);
        let best = scope
            .nodes
            .iter()
            .enumerate()
            .filter(|(_, n)| n.id != from && n.rect.w > 0 && n.rect.h > 0)
            .filter_map(|(i, n)| {
                let gap = gap_along(here, n.rect, dir)?;
                let (c_lo, c_hi) = span_across(n.rect, dir);
                let in_beam = c_lo < f_hi && f_lo < c_hi;
                // Twice each centre, so the distance stays an integer.
                let across = ((c_lo + c_hi) - (f_lo + f_hi)).abs();
                Some(((!in_beam) as u8, gap, across, i))
            })
            .min()?;
        Some(scope.entered_at(Some(from), scope.nodes[best.3].id))
    }
}

/// The distance from `from`'s edge to `c`'s near edge along `dir`, when `c`
/// lies wholly past that edge; `None` otherwise.
fn gap_along(from: Rect, c: Rect, dir: FocusDir) -> Option<i32> {
    let gap = match dir {
        FocusDir::Down => c.y - (from.y + from.h as i32),
        FocusDir::Up => from.y - (c.y + c.h as i32),
        FocusDir::Right => c.x - (from.x + from.w as i32),
        FocusDir::Left => from.x - (c.x + c.w as i32),
        FocusDir::Next | FocusDir::Prev => return None,
    };
    (gap >= 0).then_some(gap)
}

/// The span of `r` across `dir`'s axis — its columns for ↑/↓, its rows for
/// ←/→ — as `[lo, hi)`.
fn span_across(r: Rect, dir: FocusDir) -> (i32, i32) {
    match dir {
        FocusDir::Up | FocusDir::Down => (r.x, r.x + r.w as i32),
        _ => (r.y, r.y + r.h as i32),
    }
}
