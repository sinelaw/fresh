"""End-to-end wiring: the model of a rendered frame.

`EditorModel.render` is the shape `build_view_data` takes in the end state:

    anchor byte  →  tokenize forward (mid-line, no backward scan)
                 →  soft breaks / conceals / inline virtual (cursor-AWARE here)
                 →  wrap, resuming from the anchor row's carry
                 →  rows, first one marked AFTER_BREAK if mid-line
                 →  stop at `height` rows

Nothing is built to be discarded, so there is no character budget and no cursor
clause — both existed only to bound a build that started at byte 0.

**The two layouts.** The index is canonical (cursor-blind); the renderer is
cursor-aware. They can disagree only inside an active cursor scope, and only in
how many rows that scope occupies. `_stable_anchor` handles the one case where
that matters — a cursor scope starting *above* the anchor within the same
logical line, where resuming at the anchor would use a carry computed under
different text. It walks back to the scope's row and skips forward, bounded so a
pathological scope can't cost more than a couple of viewports.
"""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass, field

from .base_tokens import LineEnding, build_base_tokens
from .buffer import TextBuffer
from .decorations import Decorations, VirtualLine, VirtualLinePos
from .row_layout import LineStart, ViewLine, rows_from_tokens
from .tokens import Token
from .transforms import (
    apply_conceal_ranges,
    apply_fold_skip,
    apply_soft_breaks,
    splice_inline_virtual_text,
)
from .viewport import ScrollbarState, ViewAnchor, Viewport
from .wrap_index import WrapGeometry, WrapIndex
from .wrap_machine import RowCarry, WrapMachine, WrapRule

#: Cap on how far `_stable_anchor` will walk back for a cursor scope.
MAX_STABLE_ANCHOR_SKIP = 2


@dataclass(slots=True)
class Frame:
    """One rendered frame."""

    rows: list[ViewLine]
    top_row: int
    scrollbar: ScrollbarState
    anchor: ViewAnchor
    #: Rows the pipeline built, including any skipped to reach a stable anchor.
    rows_built: int = 0


@dataclass(slots=True)
class LazyDecoration:
    """One decoration a plugin will add after its line has been drawn.

    Models the `lines_changed` contract markdown compose is built on: the hook
    is fire-and-forget and viewport-driven, so a line's soft breaks, conceals
    and virtual lines exist only after the editor has rendered that line and
    the plugin's round-trip has come back. Until then the canonical index
    counts the line undecorated — and anything placed against those counts is
    placed against rows that are about to change.
    """

    line: int
    apply: Callable[[Decorations], None]


class EditorModel:
    """Buffer + decorations + geometry + index + viewport, wired together."""

    def __init__(
        self,
        text: str = "",
        *,
        rule: WrapRule | None = None,
        height: int = 10,
        line_ending: LineEnding = LineEnding.LF,
        decorations: Decorations | None = None,
        line_wrap_enabled: bool = True,
        view_mode: str = "source",
    ) -> None:
        self.buffer = TextBuffer(text)
        self.decorations = decorations or Decorations()
        self.line_ending = line_ending
        self.geometry = WrapGeometry(
            rule=rule or WrapRule.word(40),
            view_mode=view_mode,
            line_wrap_enabled=line_wrap_enabled,
        )
        self.index = WrapIndex(self.buffer, self.decorations, self.geometry, line_ending)
        self.viewport = Viewport(self.index, height)
        # Derive the anchor from row 0 rather than assuming byte 0 is it: a
        # plugin virtual line drawn above the first source row *is* row 0, and
        # owns no byte.
        self.viewport.set_top_row(0)
        self.cursors: tuple[int, ...] = (0,)
        # -- lazy decoration arrival (the lines_changed model) ---------------
        self.lazy_decorations: list[LazyDecoration] = []
        self._lines_seen: set[int] = set()

    # -- editing -------------------------------------------------------------

    def edit(self, start: int, removed: int, inserted: str) -> None:
        """Apply an edit and repair the index — the hot path this design targets."""
        line_before = self.buffer.get_line_number(start)
        line_start_before = self.buffer.line_start_offset(line_before)
        line_end_before = self.buffer.get_line_number(start + removed)
        record = self.buffer.edit(start, removed, inserted)
        self.decorations.shift_for_edit(start, record.delta)
        self.index.damage_bytes(record, line_before, line_start_before, line_end_before)
        self.cursors = tuple(c + record.delta if c >= start else c for c in self.cursors)

    def insert(self, at: int, text: str) -> None:
        self.edit(at, 0, text)

    def delete(self, start: int, end: int) -> None:
        self.edit(start, end - start, "")

    def set_geometry(self, rule: WrapRule) -> None:
        """Geometry changed — the index for the old geometry is simply dropped.

        The viewport keeps its *row*, not its anchor: a different width means a
        different row structure, so the old anchor's row number is the only thing
        worth carrying across.
        """
        top = self.viewport.top_row()
        self.geometry = WrapGeometry(rule, self.geometry.view_mode, self.geometry.line_wrap_enabled)
        self.index = WrapIndex(self.buffer, self.decorations, self.geometry, self.line_ending)
        self.viewport = Viewport(self.index, self.viewport.height)
        self.viewport.set_top_row(top)

    # -- rendering -----------------------------------------------------------

    def _decorate(
        self, tokens: list[Token], cursors: tuple[int, ...], from_byte: int
    ) -> list[Token]:
        deco = self.decorations
        folds = deco.fold_skip()
        if folds:
            tokens = apply_fold_skip(tokens, folds)
        if deco.soft_breaks:
            tokens = apply_soft_breaks(tokens, deco.active_soft_breaks(cursors))
        if deco.conceals:
            exclude = None if self.geometry.view_mode == "compose" else "md-syntax"
            tokens = apply_conceal_ranges(tokens, deco.active_conceals(cursors, exclude))
        if deco.inline_virtual:
            # Only hints inside the window. `splice_inline_virtual_text` places
            # any hint whose position precedes the first token at that token, so
            # an unfiltered list would drag every earlier hint onto the first
            # rendered row.
            hints = [h for h in deco.inline_virtual if h.position >= from_byte]
            tokens = splice_inline_virtual_text(tokens, hints)
        return tokens

    def pump_lines_changed(self) -> int:
        """Deliver pending decorations for every line a frame has shown.

        One call is one plugin round-trip. Returns how many decorations
        arrived; each arrival bumps the owning version, so the index sees the
        change exactly the way the Rust's `damage_all` does. Placement is NOT
        re-run here — that is the point. The caller decides when to re-place,
        because that is the decision the editor has to get right.
        """
        ready = [d for d in self.lazy_decorations if d.line in self._lines_seen]
        if not ready:
            return 0
        self.lazy_decorations = [d for d in self.lazy_decorations if d.line not in self._lines_seen]
        for d in ready:
            d.apply(self.decorations)
        return len(ready)

    def _stable_anchor(self, anchor: ViewAnchor) -> tuple[ViewAnchor, int]:
        """Back the anchor up out of an active cursor scope; return rows to skip.

        Only fires when a cursor scope begins above the anchor inside the same
        logical line — the one situation where the canonical carry at the anchor
        would not match what the cursor-aware stream produces there.
        """
        addr = self.index.byte_of_row(self.index.row_of_byte(anchor.byte))
        line_start = self.buffer.line_start_offset(addr.line)
        scopes = [
            c.scope
            for c in self.decorations.conceals
            if c.scope is not None and any(c.scope.contains(cur) for cur in self.cursors)
        ] + [
            b.scope
            for b in self.decorations.soft_breaks
            if b.scope is not None and any(b.scope.contains(cur) for cur in self.cursors)
        ]
        earliest = min(
            (s.start for s in scopes if line_start <= s.start < anchor.byte),
            default=None,
        )
        if earliest is None:
            return anchor, 0
        target_row = self.index.row_of_byte(earliest)
        skip = self.index.row_of_byte(anchor.byte) - target_row
        if skip <= 0 or skip > MAX_STABLE_ANCHOR_SKIP * self.viewport.height:
            return anchor, 0
        return ViewAnchor(self.index.byte_of_row(target_row).byte, 0), skip

    def render(self) -> Frame:
        """Build exactly the rows the viewport shows."""
        height = self.viewport.height
        top_row = self.viewport.top_row()

        anchor, skip = self._stable_anchor(self.viewport.anchor)
        # Back up to a row the wrap machine can actually resume at. Zero-cost on
        # an undecorated long line — every row there is resumable.
        start_row, resume_skip = self.index.resumable_row_at_or_before(
            self.index.row_of_byte(anchor.byte)
        )
        skip += resume_skip
        anchor = ViewAnchor(self.index.byte_of_row(start_row).byte, anchor.row_offset)
        addr = self.index.byte_of_row(start_row)
        carry: RowCarry = addr.carry
        mid_line = addr.row_in_line > 0

        want = height + skip + abs(self.viewport.anchor.row_offset)
        # Bytes the window can possibly need: one row holds at most `width`
        # columns, a column at most one character, a character at most 4 bytes.
        byte_budget = (want + 2) * (self.geometry.rule.width + 1) * 4 + 256
        tokens = build_base_tokens(
            self.buffer,
            anchor.byte,
            byte_budget=byte_budget,
            line_ending=self.line_ending,
            fold_skip=self.decorations.fold_skip(),
            mid_line=True,
        )
        tokens = self._decorate(tokens, self.cursors, anchor.byte)

        machine = WrapMachine(self.geometry.rule, carry if mid_line else None)
        for tok in tokens:
            machine.feed(tok)
            if len(machine.rows_so_far()) > want:
                break
        machine.finish()
        out = machine.rows_so_far()
        end_token = out[min(want, len(out)) - 1].token_end if out else 0
        wrapped_tokens = machine.tokens_so_far()[:end_token]

        rows = rows_from_tokens(
            wrapped_tokens,
            tabs=self.geometry.rule.tabs,
            first_line_start=LineStart.AFTER_BREAK if mid_line else LineStart.BEGINNING,
            at_buffer_end=_reaches_buffer_end(wrapped_tokens, len(self.buffer)),
        )
        rows = self._inject_virtual_lines(rows, anchor.byte)
        built = len(rows)
        # Virtual rows injected above the anchor row shift the window: the anchor
        # row sits at `leading_virtual`, and `row_offset` displaces from there.
        leading_virtual = 0
        for row in rows:
            if not row.is_virtual:
                break
            leading_virtual += 1
        start = max(skip + leading_virtual + self.viewport.anchor.row_offset, 0)
        window = rows[start:][:height]

        for row in window:
            for b in row.char_source_bytes:
                if b is not None:
                    self._lines_seen.add(self.buffer.get_line_number(b))
        return Frame(
            rows=window,
            top_row=top_row,
            scrollbar=self.viewport.scrollbar(),
            anchor=self.viewport.anchor,
            rows_built=built,
        )

    def _inject_virtual_lines(self, rows: list[ViewLine], from_byte: int) -> list[ViewLine]:
        """Splice plugin virtual lines above/below their anchor row."""
        if not self.decorations.virtual_lines:
            return rows
        out: list[ViewLine] = []
        for row in rows:
            src = row.source_start_byte
            above = [
                v
                for v in self.decorations.virtual_lines
                if v.where is VirtualLinePos.ABOVE and src is not None and v.position == src
            ]
            below = [
                v
                for v in self.decorations.virtual_lines
                if v.where is VirtualLinePos.BELOW and src is not None and v.position == src
            ]
            out.extend(_virtual_row(v) for v in above)
            out.append(row)
            out.extend(_virtual_row(v) for v in below)
        _ = from_byte
        return out


def _reaches_buffer_end(tokens: list[Token], buffer_len: int) -> bool:
    """Does this window cover the buffer's last byte?

    Decides whether the trailing empty row after a final newline belongs to this
    window. A window that stops short must not emit it — the row belongs to the
    window that actually reaches the end.
    """
    end = 0
    for tok in tokens:
        if tok.source_offset is None:
            continue
        end = max(end, tok.source_offset + tok.source_len())
    return end >= buffer_len


def _virtual_row(v: VirtualLine) -> ViewLine:
    row = ViewLine(
        text=v.text,
        line_start=LineStart.AFTER_INJECTED_NEWLINE,
        virtual_gutter_glyph=v.gutter_glyph,
        is_virtual=True,
    )
    row.char_source_bytes = [None] * len(v.text)
    row.char_visual_cols = list(range(len(v.text)))
    row.visual_to_char = list(range(len(v.text)))
    return row


__all__ = [
    "EditorModel",
    "Frame",
    "LazyDecoration",
    "ViewAnchor",
    "Viewport",
    "WrapGeometry",
    "WrapIndex",
    "WrapRule",
    "field",
]
