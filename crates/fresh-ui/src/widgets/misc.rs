//! Filler and separators.

use crate::desc::{col, text, Node, Sizing};

/// A horizontal rule of `width` cells.
pub fn divider<M>(width: u16) -> Node<M> {
    text("─".repeat(width as usize))
        .h(Sizing::Cells(1))
        .theme("divider")
}

/// Empty space that takes a share of the main axis.
pub fn spacer<M>(flex: u16) -> Node<M> {
    col().w(Sizing::Flex(flex)).h(Sizing::Flex(flex))
}
