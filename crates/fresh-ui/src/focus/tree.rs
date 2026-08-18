//! The focus tree.
//!
//! It mirrors the render tree but is not identical to it: only focusables and
//! the scopes that group them appear, so traversal reads a structure the size
//! of the focusable set rather than walking every element.
//!
//! Registration is held by the render object, which is why focus survives
//! reconciliation — a matched element keeps its render object, and the render
//! object keeps its registration.

use crate::element::ElementId;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FocusId(pub(crate) u32);

impl std::fmt::Debug for FocusId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "F{}", self.0)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct FocusNodeData {
    pub element: ElementId,
    pub parent: Option<FocusId>,
    pub children: Vec<FocusId>,
    /// Explicit traversal position; `None` means reading order.
    pub ordinal: Option<i32>,
    /// Reachable by pointer, skipped by traversal.
    pub skip: bool,
    /// Groups the focusables below it; traversal is confined here while it is
    /// the active scope.
    pub scope: bool,
}

#[derive(Default)]
pub(crate) struct FocusTree {
    slots: Vec<Option<FocusNodeData>>,
    free: Vec<u32>,
}

impl FocusTree {
    pub fn alloc(&mut self, n: FocusNodeData) -> FocusId {
        match self.free.pop() {
            Some(i) => {
                self.slots[i as usize] = Some(n);
                FocusId(i)
            }
            None => {
                self.slots.push(Some(n));
                FocusId(self.slots.len() as u32 - 1)
            }
        }
    }

    pub fn release(&mut self, id: FocusId) -> Option<FocusNodeData> {
        let n = self.slots.get_mut(id.0 as usize)?.take();
        if n.is_some() {
            self.free.push(id.0);
        }
        n
    }

    pub fn get(&self, id: FocusId) -> Option<&FocusNodeData> {
        self.slots.get(id.0 as usize).and_then(|s| s.as_ref())
    }

    pub fn get_mut(&mut self, id: FocusId) -> Option<&mut FocusNodeData> {
        self.slots.get_mut(id.0 as usize).and_then(|s| s.as_mut())
    }

}
