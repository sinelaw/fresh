"""The anchored viewport.

Replaces the `(top_byte, top_view_line_offset)` pair with a single byte.

Today `top_byte` must be a *logical line start*, so on a file with one enormous
line it is pinned at 0 and the entire scroll position lives in
`top_view_line_offset` — a row count the renderer satisfies by building every
row from byte 0 and then discarding the first N. Every scroll operation and
every visibility check therefore costs O(scroll depth).

`ViewAnchor.byte` is the byte of the first visible row, wherever that row sits
inside its logical line. Rendering starts there, so it is O(viewport) at any
scroll position, and scrolling is arithmetic on row numbers.

This also deletes a class of bug rather than fixing instances of it: with two
coordinates there was a reconciliation step between them (`calculate_view_anchor`
re-skipping rows, patched by `snap_to_logical_line_start` and `scrolled_up_in_wrap`
for fresh#1574). One coordinate has nothing to reconcile.
"""

from __future__ import annotations

from dataclasses import dataclass

from .wrap_index import RowAddr, WrapIndex


@dataclass(slots=True)
class ViewAnchor:
    """First visible row, as a buffer byte.

    `row_offset` is a *signed* displacement from the row that `byte` addresses.
    It is zero for ordinary rows. It goes negative when the viewport starts on an
    injected row — a plugin virtual line drawn above its anchor — because such a
    row owns no byte of its own and can only be described relative to one.
    """

    byte: int = 0
    row_offset: int = 0


@dataclass(frozen=True, slots=True)
class ScrollbarState:
    """What the vertical scrollbar draws.

    Exact under the redesign: `total` and `top` are O(1) and O(log n) reads off
    the index, so there is no reason left for the approximate logical-line mode
    or for the `MAX_WRAP_SCROLLBAR_LINES` / `MAX_WRAP_SCROLLBAR_BYTES` guards
    that decide between them.
    """

    total_rows: int
    top_row: int
    height: int
    exact: bool = True

    def thumb(self) -> tuple[int, int]:
        if self.total_rows <= self.height:
            return (0, self.height)
        start = self.top_row * self.height // self.total_rows
        size = max(1, self.height * self.height // self.total_rows)
        return (start, min(start + size, self.height))


class Viewport:
    """Scroll state for one split, expressed in visual rows."""

    def __init__(self, index: WrapIndex, height: int, anchor: ViewAnchor | None = None) -> None:
        self.index = index
        self.height = height
        self.anchor = anchor or ViewAnchor()

    # -- row coordinates -----------------------------------------------------

    def top_row(self) -> int:
        return self.index.row_of_byte(self.anchor.byte) + self.anchor.row_offset

    def max_top_row(self) -> int:
        return max(self.index.total_rows() - self.height, 0)

    def set_top_row(self, row: int) -> None:
        row = max(0, min(row, self.max_top_row()))
        addr = self.index.byte_of_row(row)
        base = self.index.row_of_byte(addr.byte)
        self.anchor = ViewAnchor(byte=addr.byte, row_offset=row - base)

    def scroll_by_rows(self, delta: int) -> None:
        """The whole of wheel scrolling.

        Replaces `scroll_up_visual` / `scroll_down_visual` /
        `apply_visual_scroll_limit` / `find_max_visual_scroll_position` /
        `clamp_top_byte_wrapped`, none of which can any longer read a 500 KB
        line twice per event, because none of them read the line at all.
        """
        self.set_top_row(self.top_row() + delta)

    def page_down(self) -> None:
        self.scroll_by_rows(self.height)

    def page_up(self) -> None:
        self.scroll_by_rows(-self.height)

    # -- cursor visibility ---------------------------------------------------

    def ensure_visible(self, cursor_byte: int, margin: int = 0) -> bool:
        """Scroll the minimum amount to bring the cursor's row into view.

        Runs in row space *before* anything is built, which is what collapses
        `compute_buffer_layout`'s up-to-three `build_view_data` calls per frame
        to one: the old code had to build rows to find out whether it needed to
        scroll, then rebuild because it had.
        """
        row = self.index.row_of_byte(cursor_byte)
        top = self.top_row()
        bottom = top + self.height - 1
        if row < top + margin:
            self.set_top_row(row - margin)
            return True
        if row > bottom - margin:
            self.set_top_row(row - self.height + 1 + margin)
            return True
        return False

    def cursor_visible(self, cursor_byte: int) -> bool:
        row = self.index.row_of_byte(cursor_byte)
        top = self.top_row()
        return top <= row < top + self.height

    # -- scrollbar -----------------------------------------------------------

    def scrollbar(self) -> ScrollbarState:
        return ScrollbarState(
            total_rows=self.index.total_rows(),
            top_row=self.top_row(),
            height=self.height,
        )

    # -- addressing ----------------------------------------------------------

    def visible_rows(self) -> list[RowAddr]:
        top = self.top_row()
        total = self.index.total_rows()
        return [self.index.byte_of_row(r) for r in range(top, min(top + self.height, total))]
