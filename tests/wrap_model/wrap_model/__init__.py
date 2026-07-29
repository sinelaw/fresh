"""Executable model of the fresh editor's end-state wrap architecture.

See ../README.md. The package mirrors the module layout the Rust code will have
once the migration lands:

    tokens.py       fresh_core::api::ViewTokenWire
    width.py        primitives::{display_width, visual_layout, ansi}
    buffer.py       model::buffer + primitives::line_iterator
    decorations.py  soft breaks / conceals / virtual text / folds
    base_tokens.py  split_rendering::base_tokens
    wrap_machine.py view::wrap_machine        (NEW — the one wrap rule)
    transforms.py   split_rendering::transforms (drivers over the machine)
    row_layout.py   view_pipeline::{ViewLineIterator, ViewLine}
    wrap_index.py   view::wrap_index          (NEW — replaces visual_row_index
                                               and both row-count caches)
    viewport.py     view::viewport            (anchored)
    editor.py       split_rendering::view_data (the frame)
"""

from .buffer import EditRecord, LineIterator, TextBuffer
from .decorations import (
    Conceal,
    Decorations,
    Fold,
    InlineVirtualText,
    Scope,
    SoftBreak,
    VirtualLine,
    VirtualLinePos,
)
from .editor import EditorModel, Frame, LazyDecoration
from .row_layout import LineStart, ViewLine
from .tokens import Kind, Token
from .viewport import ScrollbarState, ViewAnchor, Viewport
from .wrap_index import LineWrap, RowAddr, WrapGeometry, WrapIndex
from .wrap_machine import MAX_SAFE_LINE_WIDTH, RowCarry, RuleKind, WrapMachine, WrapRule

__all__ = [
    "MAX_SAFE_LINE_WIDTH",
    "Conceal",
    "Decorations",
    "EditRecord",
    "EditorModel",
    "Fold",
    "Frame",
    "InlineVirtualText",
    "Kind",
    "LazyDecoration",
    "LineIterator",
    "LineStart",
    "LineWrap",
    "RowAddr",
    "RowCarry",
    "RuleKind",
    "Scope",
    "ScrollbarState",
    "SoftBreak",
    "TextBuffer",
    "Token",
    "ViewAnchor",
    "ViewLine",
    "Viewport",
    "VirtualLine",
    "VirtualLinePos",
    "WrapGeometry",
    "WrapIndex",
    "WrapMachine",
    "WrapRule",
]
