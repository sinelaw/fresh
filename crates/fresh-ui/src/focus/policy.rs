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
            return Some(if forward {
                order[0]
            } else {
                order[order.len() - 1]
            });
        };
        let n = order.len();
        Some(if forward {
            order[(cur + 1) % n]
        } else {
            order[(cur + n - 1) % n]
        })
    }
}

/// Geometric traversal: the nearest focusable whose rectangle lies in the
/// requested direction. Next/Prev still use reading order.
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
        let center = |r: Rect| (r.x + r.w as i32 / 2, r.y + r.h as i32 / 2);
        let (cx, cy) = center(here);

        let mut best: Option<(i64, FocusTarget)> = None;
        for n in &scope.nodes {
            if n.id == from {
                continue;
            }
            let (nx, ny) = center(n.rect);
            let (dx, dy) = (nx - cx, ny - cy);
            let ok = match dir {
                FocusDir::Up => dy < 0,
                FocusDir::Down => dy > 0,
                FocusDir::Left => dx < 0,
                FocusDir::Right => dx > 0,
                _ => false,
            };
            if !ok {
                continue;
            }
            // Along the axis of travel first, then off-axis drift: the nearest
            // thing in the requested direction, not merely the nearest thing.
            let (along, across) = match dir {
                FocusDir::Up | FocusDir::Down => (dy.abs() as i64, dx.abs() as i64),
                _ => (dx.abs() as i64, dy.abs() as i64),
            };
            let score = along * 1000 + across;
            if best.is_none_or(|(b, _)| score < b) {
                best = Some((score, n.id));
            }
        }
        best.map(|(_, id)| id)
    }
}
