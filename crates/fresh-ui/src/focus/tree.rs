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
use crate::render::object::FocusReg;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FocusId {
    pub(crate) idx: u32,
    /// The slot generation this id was minted at; see [`crate::element::ElementId`].
    pub(crate) gen: u32,
}

impl std::fmt::Debug for FocusId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "F{}", self.idx)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct FocusNodeData {
    pub element: ElementId,
    pub parent: Option<FocusId>,
    pub children: Vec<FocusId>,
    /// Ordinal, skip, scope, focus-within and autofocus, exactly as the render
    /// object or the behavior declared them. Traversal reads this and never the
    /// description.
    pub reg: FocusReg,
}

impl FocusNodeData {
    pub fn new(element: ElementId, reg: FocusReg) -> Self {
        FocusNodeData {
            element,
            parent: None,
            children: Vec::new(),
            reg,
        }
    }
}

struct FocusSlot {
    gen: u32,
    data: Option<FocusNodeData>,
}

#[derive(Default)]
pub(crate) struct FocusTree {
    slots: Vec<FocusSlot>,
    free: Vec<u32>,
}

impl FocusTree {
    pub fn alloc(&mut self, n: FocusNodeData) -> FocusId {
        match self.free.pop() {
            Some(i) => {
                let slot = &mut self.slots[i as usize];
                slot.data = Some(n);
                FocusId {
                    idx: i,
                    gen: slot.gen,
                }
            }
            None => {
                self.slots.push(FocusSlot {
                    gen: 0,
                    data: Some(n),
                });
                FocusId {
                    idx: self.slots.len() as u32 - 1,
                    gen: 0,
                }
            }
        }
    }

    pub fn release(&mut self, id: FocusId) -> Option<FocusNodeData> {
        let slot = self.slots.get_mut(id.idx as usize)?;
        if slot.gen != id.gen {
            return None;
        }
        let n = slot.data.take();
        if n.is_some() {
            slot.gen = slot.gen.wrapping_add(1);
            self.free.push(id.idx);
        }
        n
    }

    pub fn get(&self, id: FocusId) -> Option<&FocusNodeData> {
        let slot = self.slots.get(id.idx as usize)?;
        if slot.gen != id.gen {
            return None;
        }
        slot.data.as_ref()
    }

    pub fn get_mut(&mut self, id: FocusId) -> Option<&mut FocusNodeData> {
        let slot = self.slots.get_mut(id.idx as usize)?;
        if slot.gen != id.gen {
            return None;
        }
        slot.data.as_mut()
    }
}
