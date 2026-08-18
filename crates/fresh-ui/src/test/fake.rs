//! A recording renderer: logs `create` / `update` / `dispose` in order.

use std::cell::RefCell;
use std::rc::Rc;

use crate::desc::ElemType;
use crate::element::ElementId;
use crate::schedule::Renderer;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Op {
    Create(ElementId, &'static str),
    Update(ElementId, &'static str),
    Dispose(ElementId, &'static str),
}

impl Op {
    pub fn id(&self) -> ElementId {
        match self {
            Op::Create(i, _) | Op::Update(i, _) | Op::Dispose(i, _) => *i,
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Op::Create(_, n) | Op::Update(_, n) | Op::Dispose(_, n) => n,
        }
    }
}

/// Clone it, hand one clone to [`crate::Ui::with_renderer`], and read the other.
#[derive(Clone, Default)]
pub struct Recorder {
    ops: Rc<RefCell<Vec<Op>>>,
}

impl Recorder {
    pub fn new() -> Self {
        Recorder::default()
    }

    pub fn ops(&self) -> Vec<Op> {
        self.ops.borrow().clone()
    }

    pub fn clear(&self) {
        self.ops.borrow_mut().clear();
    }

    pub fn creates(&self) -> Vec<Op> {
        self.ops()
            .into_iter()
            .filter(|o| matches!(o, Op::Create(..)))
            .collect()
    }

    pub fn updates(&self) -> Vec<Op> {
        self.ops()
            .into_iter()
            .filter(|o| matches!(o, Op::Update(..)))
            .collect()
    }

    pub fn disposes(&self) -> Vec<Op> {
        self.ops()
            .into_iter()
            .filter(|o| matches!(o, Op::Dispose(..)))
            .collect()
    }

    /// `(creates, updates, disposes)`.
    pub fn counts(&self) -> (usize, usize, usize) {
        let ops = self.ops();
        (
            ops.iter().filter(|o| matches!(o, Op::Create(..))).count(),
            ops.iter().filter(|o| matches!(o, Op::Update(..))).count(),
            ops.iter().filter(|o| matches!(o, Op::Dispose(..))).count(),
        )
    }
}

impl Renderer for Recorder {
    fn create(&mut self, id: ElementId, _ty: ElemType, name: &'static str) {
        self.ops.borrow_mut().push(Op::Create(id, name));
    }

    fn update(&mut self, id: ElementId, _ty: ElemType, name: &'static str) {
        self.ops.borrow_mut().push(Op::Update(id, name));
    }

    fn dispose(&mut self, id: ElementId, _ty: ElemType, name: &'static str) {
        self.ops.borrow_mut().push(Op::Dispose(id, name));
    }
}
