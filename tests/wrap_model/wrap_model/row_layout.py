"""Wrapped tokens → drawable rows.

Mirrors `view_pipeline::ViewLineIterator` / `LineAccumulator` and `ViewLine`.

In the end state this replaces `LineWrapCache` entirely. The cache existed so
consumers that needed per-character coordinate mappings (cursor up/down column
memory, mouse clicks) could avoid recomputing a whole logical line. With the
wrap index supplying row boundaries, those consumers ask for a *row range*
instead, and materialising a handful of rows is O(width) each — cheap enough
that there is nothing left to cache, and no `cursor_sig` two-key dance.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto

from .metrics import bump
from .tokens import Kind, Token
from .width import DEFAULT_TABS, AnsiParser, TabPolicy, char_width, grapheme_clusters


class LineStart(Enum):
    """What preceded a row — decides whether the gutter shows a line number."""

    BEGINNING = auto()
    AFTER_SOURCE_NEWLINE = auto()
    AFTER_INJECTED_NEWLINE = auto()
    AFTER_BREAK = auto()

    @property
    def is_continuation(self) -> bool:
        return self is LineStart.AFTER_BREAK


@dataclass(slots=True)
class ViewLine:
    """One drawable row with exact byte↔column mappings."""

    text: str = ""
    source_start_byte: int | None = None
    char_source_bytes: list[int | None] = field(default_factory=list)
    char_visual_cols: list[int] = field(default_factory=list)
    visual_to_char: list[int] = field(default_factory=list)
    tab_starts: set[int] = field(default_factory=set)
    line_start: LineStart = LineStart.BEGINNING
    ends_with_newline: bool = False
    virtual_gutter_glyph: str | None = None
    is_virtual: bool = False

    @property
    def visual_width(self) -> int:
        return len(self.visual_to_char)

    def source_byte_at_visual_col(self, col: int) -> int | None:
        if not self.visual_to_char:
            return self.source_start_byte
        idx = (
            self.visual_to_char[col] if col < len(self.visual_to_char) else self.visual_to_char[-1]
        )
        return self.char_source_bytes[idx]

    def visual_col_of_byte(self, byte: int) -> int | None:
        for i, src in enumerate(self.char_source_bytes):
            if src == byte:
                return self.char_visual_cols[i]
        return None


class _Accumulator:
    """Builds one row's parallel mappings, one display character at a time.

    Capacity is reserved up front from the pane width. In the Rust code these
    are five `Vec`s grown one push at a time, and the resulting reallocation
    memcpy was a measurable share of a frame on a long line — reserving is
    worth keeping regardless of the rest of the redesign.
    """

    __slots__ = ("col", "line")

    def __init__(self, width_hint: int = 0) -> None:
        self.line = ViewLine()
        self.col = 0
        if width_hint:
            self.line.visual_to_char = []

    def push_char(self, ch: str, source: int | None, width: int) -> None:
        idx = len(self.line.char_source_bytes)
        self.line.text += ch
        self.line.char_source_bytes.append(source)
        self.line.char_visual_cols.append(self.col)
        self.line.visual_to_char.extend([idx] * width)
        self.col += width
        if source is not None and self.line.source_start_byte is None:
            self.line.source_start_byte = source

    def push_escape(self, s: str, source: int | None) -> None:
        for ch in s:
            self.push_char(ch, source, 1)

    def push_tab(self, source: int | None, spaces: int) -> None:
        idx = len(self.line.char_source_bytes)
        self.line.tab_starts.add(idx)
        self.line.text += " "
        self.line.char_source_bytes.append(source)
        self.line.char_visual_cols.append(self.col)
        self.line.visual_to_char.extend([idx] * spaces)
        base = self.col
        self.col += spaces
        for i in range(1, spaces):
            self.line.text += " "
            self.line.char_source_bytes.append(source)
            self.line.char_visual_cols.append(base + i)
        if source is not None and self.line.source_start_byte is None:
            self.line.source_start_byte = source

    def finish(self, line_start: LineStart, ends_with_newline: bool) -> ViewLine:
        self.line.line_start = line_start
        self.line.ends_with_newline = ends_with_newline
        return self.line


def rows_from_tokens(
    tokens: list[Token],
    *,
    tabs: TabPolicy = DEFAULT_TABS,
    ansi_aware: bool = True,
    binary_mode: bool = False,
    first_line_start: LineStart = LineStart.BEGINNING,
    at_buffer_end: bool = False,
) -> list[ViewLine]:
    """Split a wrapped token stream into rows.

    `first_line_start` is `AFTER_BREAK` when the caller resumed mid-line — the
    gutter must not print a line number against a continuation row.
    """
    rows: list[ViewLine] = []
    acc = _Accumulator()
    next_start = first_line_start
    parser = AnsiParser()

    def flush(ends_with_newline: bool) -> None:
        nonlocal acc
        bump("rows_materialized")
        rows.append(acc.finish(next_start, ends_with_newline))
        acc = _Accumulator()

    for tok in tokens:
        if tok.kind is Kind.NEWLINE:
            flush(True)
            next_start = (
                LineStart.AFTER_SOURCE_NEWLINE
                if tok.source_offset is not None
                else LineStart.AFTER_INJECTED_NEWLINE
            )
            parser.reset()
            continue
        if tok.kind is Kind.BREAK:
            flush(False)
            next_start = LineStart.AFTER_BREAK
            continue
        if tok.kind is Kind.SPACE:
            acc.push_char(" ", tok.source_offset, 1)
            continue
        if tok.kind is Kind.BINARY_BYTE:
            acc.push_escape(f"<{tok.byte:02X}>", tok.source_offset)
            continue

        byte = tok.source_offset
        for g in grapheme_clusters(tok.text):
            if g == "\t":
                acc.push_tab(byte, tabs.expansion_at(acc.col))
            elif ansi_aware and len(g) == 1 and not parser.feed(g):
                acc.push_char(g, byte, 0)
            elif binary_mode and len(g) == 1 and ord(g) < 0x20:
                acc.push_escape(f"<{ord(g):02X}>", byte)
            else:
                first = True
                for c in g:
                    acc.push_char(c, byte if first else None, char_width(c) if first else 0)
                    first = False
            if byte is not None:
                byte += len(g.encode("utf-8"))

    if acc.line.text or not rows or at_buffer_end:
        flush(False)
    return rows
