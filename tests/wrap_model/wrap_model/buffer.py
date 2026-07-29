"""Text buffer and line iteration.

Mirrors `model::buffer::Buffer` and `primitives::line_iterator::LineIterator`,
including the constraints that survive the redesign:

* `MAX_LINE_BYTES` — a line longer than this is yielded in consecutive pieces,
  so no single read materialises an unbounded string.
* the backward scan to a line start, in blocks rather than per byte.

and the one constructor the redesign adds:

* `LineIterator.from_mid_line` — the caller certifies that `byte` is a row
  start, so no backward scan runs at all. This is what lets the renderer begin
  at the viewport's anchor instead of at byte 0 of the logical line.
"""

from __future__ import annotations

from dataclasses import dataclass

from .metrics import bump

#: `line_iterator::MAX_LINE_BYTES`.
MAX_LINE_BYTES = 100_000

#: Block size for the backward line-start scan (`LINE_SCAN_CHUNK`).
LINE_SCAN_CHUNK = 4096


@dataclass(frozen=True, slots=True)
class EditRecord:
    """One buffer mutation, as the damage contract sees it.

    `start` is the byte the edit begins at, `removed`/`inserted` its byte
    counts. `delta` is what every offset after the edit shifts by.
    """

    start: int
    removed: int
    inserted: int

    @property
    def delta(self) -> int:
        return self.inserted - self.removed

    @property
    def end_old(self) -> int:
        return self.start + self.removed


class TextBuffer:
    """A byte-oriented text buffer with a line index.

    The line index is authoritative for byte↔line queries — the wrap index
    deliberately does *not* duplicate it, since it already shifts correctly on
    every edit and duplicating it would mean two structures to repair.
    """

    def __init__(self, text: str = "") -> None:
        self._data = bytearray(text.encode("utf-8"))
        self._version = 0
        self._line_starts: list[int] | None = None

    # -- basics --------------------------------------------------------------

    def __len__(self) -> int:
        return len(self._data)

    @property
    def version(self) -> int:
        return self._version

    def text(self) -> str:
        return self._data.decode("utf-8", errors="replace")

    def slice_bytes(self, start: int, end: int) -> bytes:
        out = bytes(self._data[max(start, 0) : min(end, len(self._data))])
        bump("bytes_read", len(out))
        return out

    # -- line index ----------------------------------------------------------

    def _starts(self) -> list[int]:
        if self._line_starts is None:
            starts = [0]
            for i, b in enumerate(self._data):
                if b == 0x0A:
                    starts.append(i + 1)
            self._line_starts = starts
        return self._line_starts

    def line_count(self) -> int:
        return len(self._starts())

    def line_start_offset(self, line: int) -> int:
        starts = self._starts()
        if line < 0:
            return 0
        if line >= len(starts):
            return len(self._data)
        return starts[line]

    def line_end_offset(self, line: int) -> int:
        """Exclusive end of `line`, including its terminator if present."""
        starts = self._starts()
        if line + 1 < len(starts):
            return starts[line + 1]
        return len(self._data)

    def get_line_number(self, byte: int) -> int:
        starts = self._starts()
        lo, hi = 0, len(starts) - 1
        while lo < hi:
            mid = (lo + hi + 1) // 2
            if starts[mid] <= byte:
                lo = mid
            else:
                hi = mid - 1
        return lo

    def get_line(self, line: int) -> bytes:
        return self.slice_bytes(self.line_start_offset(line), self.line_end_offset(line))

    def find_line_start_backward(self, byte: int) -> int:
        """Block-scan back to the start of the line containing `byte`."""
        end = min(byte, len(self._data))
        while end > 0:
            start = max(end - LINE_SCAN_CHUNK, 0)
            block = self._data[start:end]
            idx = block.rfind(0x0A)
            if idx >= 0:
                return start + idx + 1
            end = start
        return 0

    # -- mutation ------------------------------------------------------------

    def edit(self, start: int, removed: int, inserted_text: str) -> EditRecord:
        ins = inserted_text.encode("utf-8")
        self._data[start : start + removed] = ins
        self._version += 1
        self._line_starts = None
        return EditRecord(start=start, removed=removed, inserted=len(ins))

    def insert(self, at: int, text: str) -> EditRecord:
        return self.edit(at, 0, text)

    def delete(self, start: int, end: int) -> EditRecord:
        return self.edit(start, end - start, "")


class LineIterator:
    """Forward iteration over lines, in `MAX_LINE_BYTES` pieces.

    A line longer than the cap is returned as several consecutive pieces; each
    piece is treated as its own unit by callers, exactly as today. The redesign
    keeps this because it is the memory bound, not a performance workaround —
    what it removes is the *need* to iterate from the line start at all.
    """

    def __init__(
        self,
        buffer: TextBuffer,
        byte: int,
        *,
        mid_line: bool = False,
        max_line_bytes: int = MAX_LINE_BYTES,
    ) -> None:
        self._buf = buffer
        self._max = max(1, min(max_line_bytes, MAX_LINE_BYTES))
        self._pos = byte if mid_line else buffer.find_line_start_backward(byte)
        self._pending_trailing_empty = (
            len(buffer) > 0
            and byte == len(buffer)
            and buffer.slice_bytes(len(buffer) - 1, len(buffer)) == b"\n"
        )

    @staticmethod
    def from_mid_line(
        buffer: TextBuffer, byte: int, max_line_bytes: int = MAX_LINE_BYTES
    ) -> LineIterator:
        """Start at `byte` with no backward scan.

        The caller certifies `byte` is a visual-row start (it came from
        `WrapIndex.byte_of_row`), which is what makes skipping the scan sound.
        """
        return LineIterator(buffer, byte, mid_line=True, max_line_bytes=max_line_bytes)

    @property
    def position(self) -> int:
        return self._pos

    def next_line(self) -> tuple[int, str] | None:
        """`(start_byte, text_including_terminator)` or None at end of buffer."""
        bump("lines_read")
        if self._pending_trailing_empty:
            self._pending_trailing_empty = False
            return (len(self._buf), "")
        if self._pos >= len(self._buf):
            return None
        start = self._pos
        limit = min(start + self._max, len(self._buf))
        # One read, then slice in memory — reading the block and then re-reading
        # the line out of it doubles the bytes touched for no benefit.
        block = self._buf.slice_bytes(start, limit)
        idx = block.find(0x0A)
        end = start + idx + 1 if idx >= 0 else limit
        self._pos = end
        raw = block[: end - start]
        if raw.endswith(b"\n") and end == len(self._buf):
            self._pending_trailing_empty = True
        return (start, raw.decode("utf-8", errors="replace"))
