"""The coordinate service: byte ↔ visual row.

Replaces `view::visual_row_index::VisualRowIndex` and both row-count caches
(`LineWrapCache`, `Viewport::wrap_row_cache`).

Two things make this different from what it replaces, and both are the point:

1. **It is not keyed on the buffer version.** Every cache in this area today
   folds `buffer.version()` into its key, so an edit invalidates it and the next
   frame rebuilds from scratch — which is why a 500 KB single-line file re-wraps
   entirely on each keystroke despite two caches sitting right there. This
   structure is *repaired* by `damage_bytes` instead: an edit at byte N leaves
   every row boundary before N's row untouched, and rewrapping forward from
   there resynchronises within a row or two.

2. **Totals are a Fenwick tree, not a prefix-sum array.** A flat array makes
   every edit O(lines) just to re-shift the sums.

Byte↔line is deliberately *not* stored here — the buffer's own line index
already answers it and already shifts on edit, and duplicating it would mean two
structures to keep repaired.

The index is **canonical**: built with no cursors, so cursor movement never
damages it. Cursor-aware layout exists only inside the renderer's materialised
window (see `viewport.render`).
"""

from __future__ import annotations

from bisect import bisect_right
from dataclasses import dataclass, field

from .base_tokens import LineEnding, line_tokens, strip_trailing_newline
from .buffer import EditRecord, TextBuffer
from .decorations import Decorations, VirtualLinePos
from .metrics import bump
from .tokens import Kind, Token
from .transforms import (
    apply_conceal_ranges,
    apply_fold_skip,
    apply_soft_breaks,
    splice_inline_virtual_text,
)
from .wrap_machine import RowCarry, RowInfo, WrapMachine, WrapRule


@dataclass(frozen=True, slots=True)
class WrapGeometry:
    """Everything that changes where rows break, other than the text itself."""

    rule: WrapRule
    view_mode: str = "source"
    line_wrap_enabled: bool = True


@dataclass(slots=True)
class LineWrap:
    """Row structure of one logical line.

    `row_starts` are line-relative byte offsets, always beginning with 0.
    `carries[i]` is the wrap state to resume row `i` with — what makes both
    mid-line rendering and incremental repair possible.
    """

    row_starts: list[int] = field(default_factory=lambda: [0])
    carries: list[RowCarry] = field(default_factory=lambda: [RowCarry.fresh()])
    #: Whether each row can be resumed at by byte alone.
    #:
    #: False when the row opens with injected content the machine cannot
    #: reconstruct from its carry — a soft break's newline, an inline hint that
    #: wrapped onto this row, a conceal replacement. Whether such content sits on
    #: this row or the previous one is a decision the wrap made; a byte offset
    #: does not record it. Repair walks back to the nearest resumable row, and
    #: rebuilds the line if there is none. (A hanging indent *is* reconstructible
    #: — it is in the carry — so indented continuations stay resumable.)
    resumable: list[bool] = field(default_factory=lambda: [True])
    virtual_rows: int = 0
    #: Every byte of this line is inside a collapsed fold, so it draws nothing.
    #:
    #: A visual row is a row on screen, so a coordinate system counting rows has
    #: to leave these out — otherwise the scrollbar sizes itself to rows nobody
    #: can scroll to, the wheel moves a different distance than it says, and
    #: page-down lands short. Keeping folds out of the index instead means every
    #: consumer needs its own fold correction, which is where the fallbacks come
    #: from.
    hidden: bool = False

    @property
    def wrap_rows(self) -> int:
        return len(self.row_starts)

    @property
    def total_rows(self) -> int:
        if self.hidden:
            return 0
        return self.wrap_rows + self.virtual_rows

    def virtual_rows_before(self) -> int:
        """Virtual rows drawn ahead of this line's own first wrap row.

        Modelled as all of them: `ABOVE` lines precede the source row and
        `BELOW` lines follow it, a distinction that matters for draw order, not
        for how many rows the logical line occupies.
        """
        return self.virtual_rows


@dataclass(frozen=True, slots=True)
class RowAddr:
    """Where a visual row lives."""

    row: int
    byte: int
    line: int
    row_in_line: int
    carry: RowCarry
    is_virtual: bool = False


class Fenwick:
    """Point-update / prefix-query over per-line row counts."""

    __slots__ = ("_n", "_t")

    def __init__(self, values: list[int] | None = None) -> None:
        vals = values or []
        self._n = len(vals)
        self._t = [0] * (self._n + 1)
        for i, v in enumerate(vals):
            self._add(i, v)

    def _add(self, i: int, delta: int) -> None:
        i += 1
        while i <= self._n:
            self._t[i] += delta
            i += i & -i

    def set(self, i: int, old: int, new: int) -> None:
        if new != old:
            self._add(i, new - old)

    def prefix(self, i: int) -> int:
        """Sum of entries `[0, i)`."""
        i = min(i, self._n)
        total = 0
        while i > 0:
            total += self._t[i]
            i -= i & -i
        return total

    def total(self) -> int:
        return self.prefix(self._n)

    def find(self, row: int) -> int:
        """Largest index whose prefix sum is <= `row` — i.e. the owning line."""
        idx = 0
        bit = 1
        while bit * 2 <= self._n:
            bit *= 2
        remaining = row
        while bit > 0:
            nxt = idx + bit
            if nxt <= self._n and self._t[nxt] <= remaining:
                idx = nxt
                remaining -= self._t[nxt]
            bit //= 2
        return idx

    def rebuild(self, values: list[int]) -> None:
        self._n = len(values)
        self._t = [0] * (self._n + 1)
        for i, v in enumerate(values):
            self._add(i, v)


class WrapIndex:
    """Row boundaries for one buffer under one geometry."""

    def __init__(
        self,
        buffer: TextBuffer,
        decorations: Decorations,
        geometry: WrapGeometry,
        line_ending: LineEnding = LineEnding.LF,
    ) -> None:
        self.buffer = buffer
        self.decorations = decorations
        self.geometry = geometry
        self.line_ending = line_ending
        self.lines: list[LineWrap] = []
        self.rows = Fenwick()
        self._built = False
        self._deco_version = decorations.pipeline_version()
        #: Diagnostics the tests assert on — the whole claim is about *how much*
        #: work an edit causes, so the model counts it.
        self.stats_lines_wrapped = 0
        self.stats_rows_wrapped = 0

    # -- build ---------------------------------------------------------------

    def ensure_built(self) -> None:
        if self._built and self._deco_version == self.decorations.pipeline_version():
            return
        if self._deco_version != self.decorations.pipeline_version():
            self._built = False
            self._deco_version = self.decorations.pipeline_version()
        if self._built:
            return
        bump("index_builds")
        self.lines = [self._build_line(i) for i in range(self.buffer.line_count())]
        self.rows.rebuild([lw.total_rows for lw in self.lines])
        self._built = True

    def _line_token_stream(self, line: int) -> list[Token]:
        """Canonical (cursor-blind) token stream for one logical line."""
        toks = strip_trailing_newline(line_tokens(self.buffer, line, self.line_ending))
        deco = self.decorations
        if deco.soft_breaks:
            toks = apply_soft_breaks(toks, deco.active_soft_breaks(()))
        if deco.conceals:
            toks = apply_conceal_ranges(toks, deco.active_conceals(()))
        if deco.inline_virtual:
            start = self.buffer.line_start_offset(line)
            end = self.buffer.line_end_offset(line)
            hints = [h for h in deco.inline_virtual if start <= h.position < end]
            toks = splice_inline_virtual_text(toks, hints)
        folds = deco.fold_skip()
        if folds:
            toks = apply_fold_skip(toks, folds)
        return toks

    def _line_is_hidden(self, line: int) -> bool:
        """Is every byte of `line` inside a collapsed range?

        A fold's header line only *partly* overlaps its range — its own text is
        still drawn, with the folded tail skipped — so it is not hidden. The
        lines after it are, and those are the ones that must stop occupying
        rows.
        """
        folds = self.decorations.fold_skip()
        if not folds:
            return False
        start = self.buffer.line_start_offset(line)
        end = self.buffer.line_end_offset(line)
        if start >= end:
            return any(lo <= start < hi for lo, hi in folds)
        return any(lo <= start and end <= hi for lo, hi in folds)

    def _virtual_rows(self, line: int) -> int:
        start = self.buffer.line_start_offset(line)
        end = self.buffer.line_end_offset(line)
        return sum(
            1
            for v in self.decorations.virtual_lines
            if start <= v.position < end and v.where in (VirtualLinePos.ABOVE, VirtualLinePos.BELOW)
        )

    def _build_line(self, line: int) -> LineWrap:
        bump("line_builds")
        toks = self._line_token_stream(line)
        line_start = self.buffer.line_start_offset(line)
        out = WrapMachine.run(toks, self.geometry.rule)
        self.stats_lines_wrapped += 1
        self.stats_rows_wrapped += len(out.rows)
        starts, carries, resumable = self._rows_to_starts(out.rows, out.tokens, line_start, 0)
        return LineWrap(
            starts,
            carries,
            resumable,
            self._virtual_rows(line),
            hidden=self._line_is_hidden(line),
        )

    @staticmethod
    def _row_is_resumable(row: RowInfo, out: list[Token]) -> bool:
        """Can this row be rebuilt from its source byte and carry alone?

        Yes when its first token — after any hanging indent, which the carry
        reconstructs — is the source token the row starts at. No when injected
        content opens the row, because nothing in `(byte, carry)` says whether
        that content belonged here or to the row before.
        """
        if row.source_byte is None:
            return False
        idx = row.token_start
        if (
            idx < len(out)
            and out[idx].source_offset is None
            and out[idx].kind is Kind.TEXT
            and set(out[idx].text) <= {" "}
            and len(out[idx].text) == row.carry.line_indent
        ):
            idx += 1
        return idx < len(out) and out[idx].source_offset == row.source_byte

    @staticmethod
    def _rows_to_starts(
        rows: list[RowInfo], out: list[Token], line_start: int, first_rel: int
    ) -> tuple[list[int], list[RowCarry], list[bool]]:
        """Row infos → line-relative starts, filling in rows with no source byte.

        A row can be all-injected (a hanging indent, or a soft-break newline
        followed by indent spaces) and carry no source byte of its own; it still
        occupies a row, and its start is taken to be the previous row's, which is
        what keeps `byte_of_row` monotonic.
        """
        starts: list[int] = []
        carries: list[RowCarry] = []
        resumable: list[bool] = []
        prev = first_rel
        for i, row in enumerate(rows):
            if i == 0:
                rel = first_rel
            elif row.source_byte is None:
                rel = prev
            else:
                rel = row.source_byte - line_start
            is_resumable = WrapIndex._row_is_resumable(row, out)
            starts.append(rel)
            carries.append(row.carry)
            resumable.append(is_resumable)
            prev = rel
        if not starts:
            starts = [first_rel]
            carries = [RowCarry.fresh()]
            resumable = [True]
        return starts, carries, resumable

    # -- queries -------------------------------------------------------------

    def total_rows(self) -> int:
        self.ensure_built()
        return max(self.rows.total(), 1)

    def rows_in_line(self, line: int) -> int:
        self.ensure_built()
        return self.lines[line].total_rows if line < len(self.lines) else 1

    def line_first_row(self, line: int) -> int:
        self.ensure_built()
        return self.rows.prefix(line)

    def row_of_byte(self, byte: int) -> int:
        """Absolute visual row containing `byte`. O(log lines + log rows)."""
        self.ensure_built()
        line = self.buffer.get_line_number(byte)
        if line >= len(self.lines):
            return max(self.total_rows() - 1, 0)
        lw = self.lines[line]
        rel = byte - self.buffer.line_start_offset(line)
        row_in_line = max(bisect_right(lw.row_starts, rel) - 1, 0)
        return self.rows.prefix(line) + lw.virtual_rows_before() + row_in_line

    def byte_of_row(self, row: int) -> RowAddr:
        """Address of absolute visual row `row`. O(log lines)."""
        self.ensure_built()
        total = self.total_rows()
        row = max(0, min(row, total - 1))
        line = min(self.rows.find(row), max(len(self.lines) - 1, 0))
        base = self.rows.prefix(line)
        lw = self.lines[line]
        row_in_line = row - base
        vbefore = lw.virtual_rows_before()
        if row_in_line < vbefore:
            return RowAddr(
                row=row,
                byte=self.buffer.line_start_offset(line),
                line=line,
                row_in_line=0,
                carry=RowCarry.fresh(),
                is_virtual=True,
            )
        idx = min(row_in_line - vbefore, len(lw.row_starts) - 1)
        return RowAddr(
            row=row,
            byte=self.buffer.line_start_offset(line) + lw.row_starts[idx],
            line=line,
            row_in_line=idx,
            carry=lw.carries[idx],
        )

    def resumable_row_at_or_before(self, row: int) -> tuple[int, int]:
        """Nearest row at or before `row` that a render can start at.

        Returns `(start_row, skip)`: build from `start_row` and discard `skip`
        rows. A row that opens with injected content the carry cannot
        reconstruct — a soft break's indent, a hint that wrapped onto it — is not
        a valid entry point, so the renderer walks back to one that is, falling
        back to the line's own first row (always valid, since a logical line
        start needs no carry at all).

        `skip` is bounded by the rows in one logical line, and is zero for the
        case this design exists for: a long line with no decorations, where every
        row is resumable.
        """
        self.ensure_built()
        addr = self.byte_of_row(row)
        if addr.is_virtual:
            return row, 0
        lw = self.lines[addr.line]
        idx = addr.row_in_line
        while idx > 0 and not lw.resumable[idx]:
            idx -= 1
        return row - (addr.row_in_line - idx), addr.row_in_line - idx

    # -- damage contract -----------------------------------------------------

    def damage_all(self) -> None:
        """A plugin input changed; rebuild lazily on the next query."""
        self._built = False

    def damage_bytes(
        self,
        edit: EditRecord,
        line_before: int,
        line_start_before: int,
        line_end_before: int,
    ) -> None:
        """Repair after a buffer edit.

        The three `*_before` arguments describe the edit in *pre-edit* line
        coordinates: the line its start fell in, that line's start byte, and the
        line its removed span ended in. The caller has the pre-edit buffer, so it
        can supply them for free; reconstructing them here would mean keeping a
        shadow copy of the old line index.

        Two shapes of damage:

        * the edit stays inside one logical line and leaves the line count alone
          → incremental `_repair_line`, the hot path (every keystroke);
        * the edit spans lines, or adds or removes some → rebuild just the lines
          it covers and re-tree. Note this is *not* only about the line count
          changing: replacing `"\na"` with `"\n"` spans two lines while leaving
          the count identical, and repairing only the first would silently keep a
          stale layout for the second.
        """
        if not self._built:
            return
        if self.decorations.pipeline_version() != self._deco_version:
            self.damage_all()
            return

        new_last = self.buffer.get_line_number(edit.start + edit.inserted)
        spans_lines = line_end_before != line_before or new_last != line_before
        if spans_lines or self.buffer.line_count() != len(self.lines):
            self._repair_span(line_before, line_end_before, new_last)
            return

        self._repair_line(
            line_before,
            edit.start - line_start_before,
            edit.delta,
            rel_end_old=edit.end_old - line_start_before,
        )

    def _decoration_limit(self, line: int) -> int:
        """Line-relative offset past every decoration anchored in this line.

        Zero when the line carries none, which is what keeps resync available on
        the plain long lines this design targets.
        """
        start = self.buffer.line_start_offset(line)
        end = self.buffer.line_end_offset(line)
        limit = 0
        deco = self.decorations
        for sb in deco.soft_breaks:
            if start <= sb.position < end:
                limit = max(limit, sb.position - start + 1)
        for c in deco.conceals:
            if start <= c.start < end:
                limit = max(limit, c.end - start)
        for v in deco.inline_virtual:
            if start <= v.position < end:
                limit = max(limit, v.position - start + 1)
        return limit

    def _repair_span(self, first: int, old_last: int, new_last: int) -> None:
        """Rebuild old lines `[first, old_last]` as new lines `[first, new_last]`.

        Lines before `first` are untouched. Lines after keep their `LineWrap`
        unchanged — `row_starts` are line-relative, so a line that merely shifted
        in the buffer needs no work at all.
        """
        first = max(first, 0)
        rebuilt = [self._build_line(i) for i in range(first, new_last + 1)]
        self.lines[first : old_last + 1] = rebuilt
        self.rows.rebuild([lw.total_rows for lw in self.lines])

    def _repair_line(self, line: int, rel_start: int, delta: int, rel_end_old: int) -> None:
        """Rewrap forward from the damaged row until the layout resynchronises.

        Correctness rests on `RowCarry` being the complete resume state: once a
        newly computed boundary lands on an old boundary shifted by `delta` *and*
        the carry matches, every later boundary must match too, so the tail can
        be spliced instead of recomputed.

        `rel_end_old` bounds where resync may happen: only old boundaries at or
        past the end of the replaced span describe text the edit did not touch.
        Without that bound a boundary inside the edited region can coincidentally
        land on `old + delta` and splice a tail that no longer corresponds to
        anything — the layout then silently loses rows.

        Resync also requires the tail to be *pure shifted source*: past the last
        decoration in the line. A conceal or hint beyond the resync point means
        the token stream there is not simply the old one moved by `delta` — the
        overlay reattaches to different tokens — and "same byte, same carry" no
        longer implies "same continuation". Lines whose decorations all sit
        before the resync point (including every undecorated line, which is the
        case this design exists for) still resync.
        """
        bump("repairs")
        old = self.lines[line]
        old_total_before = old.total_rows

        # Resume one row *before* the row containing the edit. The break that
        # ends a row is decided by the token that overflows it — which lives on
        # the next row — so the last still-trustworthy boundary is the one before
        # that. Resuming at the damaged row itself would miss two cases: text
        # reflowing backwards into the previous row when the edit shrinks it, and
        # a row disappearing entirely when its content is deleted.
        damaged = max(bisect_right(old.row_starts, rel_start) - 1, 0)
        resume_idx = max(damaged - 1, 0)
        # Only an resumable row can be resumed at — an injected-only row has no
        # byte to slice the token stream at. Walk back to the nearest one; if the
        # line has none (every row is injected content, e.g. a conceal swallowed
        # the whole line), rebuild it. Such lines are short by construction.
        while resume_idx > 0 and not old.resumable[resume_idx]:
            resume_idx -= 1
        if not old.resumable[resume_idx]:
            self.lines[line] = self._build_line(line)
            self.rows.set(line, old_total_before, self.lines[line].total_rows)
            return
        resume_rel = old.row_starts[resume_idx]
        resume_carry = old.carries[resume_idx]

        line_start = self.buffer.line_start_offset(line)
        toks = self._line_token_stream(line)
        tail = tokens_from(toks, line_start + resume_rel)

        if not _resume_is_safe(toks, line_start + resume_rel) or (not tail and toks):
            # Either the *new* stream opens this row with injected content the
            # resume cannot reconstruct (an edit can move a hint or a soft break
            # onto a row that previously started with plain source), or the
            # resume byte no longer addresses anything while the line still has
            # content. Both mean the byte is not a valid resume point any more.
            self.lines[line] = self._build_line(line)
            self.rows.set(line, old_total_before, self.lines[line].total_rows)
            return

        machine = WrapMachine(self.geometry.rule, resume_carry)
        new_starts: list[int] = []
        new_carries: list[RowCarry] = []
        new_resumable: list[bool] = []
        prev_rel = resume_rel
        resync_at: int | None = None
        sealed = 0
        resync_floor = max(rel_end_old, self._decoration_limit(line))

        def absorb() -> int | None:
            """Seal any newly completed rows; return the old index we resynced to."""
            nonlocal prev_rel, sealed
            while len(machine.rows_so_far()) > sealed:
                row = machine.rows_so_far()[sealed]
                sealed += 1
                if sealed == 1:
                    rel = resume_rel
                    is_resumable = True
                elif row.source_byte is None:
                    rel = prev_rel
                    is_resumable = False
                else:
                    rel = row.source_byte - line_start
                    is_resumable = WrapIndex._row_is_resumable(row, machine._out)
                new_starts.append(rel)
                new_carries.append(row.carry)
                new_resumable.append(is_resumable)
                prev_rel = rel
                if len(new_starts) > 1:
                    for k in range(resume_idx + 1, len(old.row_starts)):
                        if old.row_starts[k] < resync_floor:
                            continue
                        if old.row_starts[k] + delta == rel and old.carries[k] == row.carry:
                            return k
            return None

        for tok in tail:
            machine.feed(tok)
            hit = absorb()
            if hit is not None:
                resync_at = hit
                break
        if resync_at is None and tail:
            machine.finish()
            absorb()

        self.stats_lines_wrapped += 1
        self.stats_rows_wrapped += len(new_starts)

        if resync_at is not None:
            bump("resyncs")
            # Drop the provisional row that matched — the spliced tail supplies it.
            merged_starts = old.row_starts[:resume_idx] + new_starts[:-1]
            merged_carries = old.carries[:resume_idx] + new_carries[:-1]
            merged_resumable = old.resumable[:resume_idx] + new_resumable[:-1]
            merged_starts += [s + delta for s in old.row_starts[resync_at:]]
            merged_carries += old.carries[resync_at:]
            merged_resumable += old.resumable[resync_at:]
        else:
            merged_starts = old.row_starts[:resume_idx] + new_starts
            merged_carries = old.carries[:resume_idx] + new_carries
            merged_resumable = old.resumable[:resume_idx] + new_resumable

        if not merged_starts:
            # The resumed region held every row of the line and the edit emptied
            # it. A logical line always occupies at least one row.
            merged_starts = [0]
            merged_carries = [RowCarry.fresh()]
            merged_resumable = [True]

        old.row_starts = merged_starts
        old.carries = merged_carries
        old.resumable = merged_resumable
        old.virtual_rows = self._virtual_rows(line)
        self.rows.set(line, old_total_before, old.total_rows)


def _resume_is_safe(tokens: list[Token], byte: int) -> bool:
    """Is `byte` a valid resume point in this token stream?

    Only when the first source token at or after `byte` is not preceded by
    injected tokens. Decorations move with the text on an edit, so a hint or a
    soft break can land on a row that previously started with plain source; the
    row's old `resumable` flag describes the *old* stream, and this checks the
    new one.
    """
    prev_injected = False
    for tok in tokens:
        if tok.source_offset is None:
            prev_injected = True
            continue
        if tok.source_offset >= byte:
            return not prev_injected
        end = tok.source_offset + (len(tok.text.encode("utf-8")) if tok.kind is Kind.TEXT else 1)
        if end > byte:
            return not prev_injected
        prev_injected = False
    return True


def tokens_from(tokens: list[Token], byte: int) -> list[Token]:
    """Sub-stream beginning at absolute `byte`, splitting a token if needed.

    Strictly source-addressed: injected tokens before the first source token at
    or after `byte` are *not* included. Whether such a token sat on the previous
    row or opened this one is a wrap-time decision, and a byte offset does not
    record it — so rows that open with injected content are marked
    non-resumable (`LineWrap.resumable`) and never reach this function.
    """
    out: list[Token] = []
    started = False
    for tok in tokens:
        if started:
            out.append(tok)
            continue
        off = tok.source_offset
        if off is None:
            continue
        if off >= byte:
            started = True
            out.append(tok)
            continue
        if tok.kind is Kind.TEXT:
            raw = tok.text.encode("utf-8")
            if off + len(raw) > byte:
                cut = byte - off
                started = True
                out.append(Token.text_tok(raw[cut:].decode("utf-8"), byte, tok.style))
    return out
