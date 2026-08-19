"""The one wrap rule.

Today the same row-boundary decision is re-implemented in at least seven
places (`apply_wrapping_transform`, `apply_grid_wrapping_transform`,
`wrap_str_to_width`, `count_visual_rows_for_text`,
`count_visual_rows_for_text_with_soft_breaks`, `count_visual_rows_for_text_grid`
/ `for_each_grid_row_start`, `wrap_segment_source_bytes`) and kept in agreement
by convention plus a couple of cross-checking tests. This module is the
end-state replacement: *one* machine that decides where rows end, with two
drivers reading its output —

* the renderer wants the token stream with `Break` tokens spliced in
  (`transforms.apply_wrapping_transform`);
* the index wants only the row boundaries (`wrap_index.WrapIndex`).

Because both read the same run, "the scrollbar disagrees with the renderer"
stops being a class of bug that can exist.

`RowCarry` is the complete resume state at a row boundary. That completeness is
what makes both mid-line rendering (start at any row, not at byte 0) and
incremental repair (rewrap from the damaged row, not the line start) correct;
`tests/test_wrap_machine.py` pins it as a property.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto

from .metrics import bump
from .tokens import Kind, Token
from .width import (
    DEFAULT_TABS,
    WRAP_MAX_LOOKBACK,
    AnsiParser,
    TabPolicy,
    cluster_width,
    contains_ansi,
    grapheme_indices,
    str_width,
    word_bound_indices,
)

#: Minimum usable columns on a continuation row before hanging indent is dropped.
MIN_CONTINUATION_CONTENT_WIDTH = 10

#: Stand-in for "never wrap", used when the pane is too narrow to wrap in.
_NO_WRAP = 1 << 30

#: `split_rendering::MAX_SAFE_LINE_WIDTH` — the forced-chop width that bounds
#: memory on pathological lines even when soft wrap is off.
MAX_SAFE_LINE_WIDTH = 10_000


class RuleKind(Enum):
    WORD = auto()
    GRID = auto()
    CHOP = auto()


@dataclass(frozen=True, slots=True)
class WrapRule:
    """How rows end.

    * `WORD` — soft wrap: word boundaries, hanging indent, gutter on row 0.
    * `GRID` — terminal scroll-back (fresh#2649): exact column boundaries, no
      gutter, no indent, ANSI-aware, parser reset per logical line.
    * `CHOP` — soft wrap off: rows end every `MAX_SAFE_LINE_WIDTH` *characters*,
      matching the forced `Break` that `build_base_tokens` injects today.
    """

    kind: RuleKind
    width: int
    gutter: int = 0
    hanging_indent: bool = False
    tabs: TabPolicy = DEFAULT_TABS

    @staticmethod
    def word(
        content_width: int,
        gutter: int = 0,
        hanging_indent: bool = False,
        tabs: TabPolicy = DEFAULT_TABS,
    ) -> WrapRule:
        return WrapRule(RuleKind.WORD, content_width, gutter, hanging_indent, tabs)

    @staticmethod
    def grid(cols: int, tabs: TabPolicy = DEFAULT_TABS) -> WrapRule:
        return WrapRule(RuleKind.GRID, cols, tabs=tabs)

    @staticmethod
    def chop(cols: int = MAX_SAFE_LINE_WIDTH, tabs: TabPolicy = DEFAULT_TABS) -> WrapRule:
        return WrapRule(RuleKind.CHOP, cols, tabs=tabs)

    @property
    def available_width(self) -> int:
        return max(self.width - self.gutter, 0)

    @property
    def degenerate(self) -> bool:
        """Below this width the transform bails out and emits no breaks at all.

        Mirrors the `available_width < 2` guard, which exists so a 1-column
        pane cannot produce one `Break` per character.
        """
        return self.kind is RuleKind.WORD and self.available_width < 2


@dataclass(frozen=True, slots=True)
class RowCarry:
    """Everything that crosses a row boundary.

    Completeness argument for `WORD`: tab widths restart from the row's own
    starting column, the word-boundary lookback never reaches behind the row
    start, and the back-up-to-prior-space path is bounded by the current row —
    so `line_indent` and `on_continuation` are the only state a resumed run
    needs. `GRID` additionally carries the ANSI parser state, since a split
    escape sequence would otherwise become visible. `CHOP` carries nothing but
    its character counter.
    """

    line_indent: int = 0
    on_continuation: bool = False
    ansi: tuple[bool, bool] = (False, False)
    chars_in_row: int = 0

    @staticmethod
    def fresh() -> RowCarry:
        return RowCarry()


@dataclass(frozen=True, slots=True)
class RowInfo:
    """One visual row produced by a run.

    `source_byte` is the first source-bearing byte on the row, i.e. exactly what
    `WrapIndex` stores as a row start. It is None only for rows made entirely of
    injected content (a virtual line, or a hanging indent with nothing after it).
    """

    source_byte: int | None
    carry: RowCarry
    token_start: int
    token_end: int


@dataclass(slots=True)
class WrapOutput:
    tokens: list[Token]
    rows: list[RowInfo]

    def row_source_bytes(self) -> list[int | None]:
        return [r.source_byte for r in self.rows]


@dataclass(slots=True)
class _RowAccum:
    """Tokens of the row currently being built.

    Held rather than emitted immediately because the Space-overflow path
    (issue #1363) retroactively moves the row's trailing word onto the next row.
    Bounded by one row's worth of tokens.
    """

    tokens: list[Token] = field(default_factory=list)
    source_byte: int | None = None

    def push(self, tok: Token) -> None:
        if self.source_byte is None and tok.source_offset is not None:
            self.source_byte = tok.source_offset
        self.tokens.append(tok)


class WrapMachine:
    """Decides row boundaries for a token stream under one `WrapRule`."""

    def __init__(self, rule: WrapRule, carry: RowCarry | None = None) -> None:
        self.rule = rule
        start = carry or RowCarry.fresh()
        self.line_indent = start.line_indent
        self.on_continuation = start.on_continuation
        self.chars_in_row = start.chars_in_row
        self._ansi = AnsiParser.restore(start.ansi)
        # A resumed run starts mid-line, so the indent has already been
        # measured; only a run starting at a logical line start measures.
        self.measuring_indent = rule.hanging_indent and not start.on_continuation
        self.col = start.line_indent if start.on_continuation else 0
        self._out: list[Token] = []
        self._rows: list[RowInfo] = []
        self._row = _RowAccum()
        self._row_token_start = 0
        self._row_carry = start
        if start.on_continuation and start.line_indent > 0:
            # A continuation row opens with its hanging indent. The carry knows
            # the width, so a resumed run reconstructs the indent rather than
            # needing it fed back in — which is what keeps a resume addressable
            # by a plain source byte.
            self._row.push(Token.text_tok(" " * start.line_indent, None))

    # -- public API ----------------------------------------------------------

    @staticmethod
    def run(tokens: list[Token], rule: WrapRule, carry: RowCarry | None = None) -> WrapOutput:
        """Wrap `tokens`, optionally resuming from a previous row boundary."""
        machine = WrapMachine(rule, carry)
        for tok in tokens:
            machine.feed(tok)
        machine.finish()
        return WrapOutput(machine._out, machine._rows)

    def feed(self, token: Token) -> None:
        bump("tokens_fed")
        if self.rule.kind is RuleKind.GRID:
            self._feed_grid(token)
        elif self.rule.kind is RuleKind.CHOP:
            self._feed_chop(token)
        else:
            self._feed_word(token)

    def finish(self) -> None:
        self._close_row(final=True)

    def rows_so_far(self) -> list[RowInfo]:
        """Rows already sealed. Lets a caller stop feeding once it has enough."""
        return self._rows

    def tokens_so_far(self) -> list[Token]:
        """Output tokens of the sealed rows.

        The renderer reads this instead of re-running the machine over the same
        stream: one pass produces both the row boundaries it stops on and the
        tokens it draws.
        """
        return self._out

    def carry(self) -> RowCarry:
        return RowCarry(
            line_indent=self.line_indent,
            on_continuation=self.on_continuation,
            ansi=self._ansi.snapshot(),
            chars_in_row=self.chars_in_row,
        )

    # -- row bookkeeping -----------------------------------------------------

    def _close_row(self, final: bool = False) -> None:
        """Seal the pending row into a `RowInfo`.

        An empty trailing accumulator at end-of-stream is not a row: a stream
        ending exactly at a break has already had its last row recorded.
        """
        if final and not self._row.tokens and self._rows:
            return
        bump("rows_emitted")
        self._out.extend(self._row.tokens)
        self._rows.append(
            RowInfo(
                source_byte=self._row.source_byte,
                carry=self._row_carry,
                token_start=self._row_token_start,
                token_end=len(self._out),
            )
        )
        self._row = _RowAccum()
        self._row_token_start = len(self._out)
        self._row_carry = self.carry()

    def _emit_break(self, indent: bool = True) -> None:
        """End the current row with a `Break`, then re-emit the hanging indent."""
        self._row.push(Token.brk())
        self.on_continuation = True
        self.chars_in_row = 0
        self._close_row()
        self.col = 0
        if indent and self.line_indent > 0:
            self._row.push(Token.text_tok(" " * self.line_indent, None))
            self.col = self.line_indent

    # -- WORD rule -----------------------------------------------------------

    def _feed_word(self, token: Token) -> None:
        rule = self.rule
        # A degenerate pane emits no wrap breaks at all — one `Break` per
        # character would be pathological. Rows are still delimited by
        # `Newline` / pre-existing `Break`, so the row model stays consistent
        # with what the renderer draws.
        eff = _NO_WRAP if rule.degenerate else rule.available_width

        if token.kind is Kind.NEWLINE:
            self._row.push(token)
            self._close_row()
            self.col = 0
            self.line_indent = 0
            self.measuring_indent = rule.hanging_indent
            self.on_continuation = False
            self._row_carry = self.carry()
            return

        if token.kind is Kind.BREAK:
            self._emit_break()
            return

        if token.kind is Kind.SPACE:
            if self.measuring_indent:
                self.line_indent += 1
                if self.line_indent + MIN_CONTINUATION_CONTENT_WIDTH > eff:
                    self.line_indent = 0
            if self.col + 1 > eff:
                self._space_overflow(eff)
            self._row.push(token)
            self.col += 1
            return

        if token.kind is Kind.BINARY_BYTE:
            self.measuring_indent = False
            # `col > line_indent` — i.e. the row has content. The Rust path omits
            # this guard, so a `<XX>` escape (4 columns) on a pane narrower than 4
            # breaks at column 0 and leaves an empty leading row. The grid path
            # already guards with `col > 0`; this makes the word path agree.
            if self.col > self.line_indent and self.col + 4 > eff:
                self._emit_break()
            self._row.push(token)
            self.col += 4
            return

        self._feed_word_text(token, eff)

    def _feed_word_text(self, token: Token, eff: int) -> None:
        text = token.text
        if self.measuring_indent:
            self._measure_indent(text, eff)

        text_w = str_width(text, self.col, self.rule.tabs)

        # Break before a token that overflows, when either it fits on a fresh
        # row (classic word wrap) or the row already carries enough content that
        # ending here beats pushing one straggler grapheme to reach `eff`.
        fresh_capacity = max(eff - self.line_indent, 0)
        row_floor = max(eff - WRAP_MAX_LOOKBACK, eff // 2)
        # `col > line_indent` rather than `col > 0`: on a continuation row that
        # holds only its hanging indent, breaking would emit an identical empty
        # row and make no progress.
        if (
            self.col > self.line_indent
            and self.col + text_w > eff
            and (text_w <= fresh_capacity or self.col >= row_floor)
        ):
            self._emit_break()
            text_w = str_width(text, self.col, self.rule.tabs)

        if self.col + text_w > eff and not contains_ansi(text):
            self._split_text(token, eff)
        else:
            self._row.push(token)
            self.col += text_w

    def _measure_indent(self, text: str, eff: int) -> None:
        """Accumulate the logical line's leading whitespace into `line_indent`.

        Dropped to zero when it would leave a continuation row unusably narrow —
        the clamp that also guarantees the char-split loop makes progress.
        """
        ws_chars = 0
        ws_width = 0
        for c in text:
            if c == " ":
                ws_width += 1
                ws_chars += 1
            elif c == "\t":
                ws_width += self.rule.tabs.expansion_at(self.line_indent + ws_width)
                ws_chars += 1
            else:
                break
        self.line_indent += ws_width
        if ws_chars != len(text):
            self.measuring_indent = False
        if self.line_indent + MIN_CONTINUATION_CONTENT_WIDTH > eff:
            self.line_indent = 0

    def _split_text(self, token: Token, eff: int) -> None:
        """Grapheme-split a token too wide for the current row.

        Each chunk prefers to end at a word boundary within the lookback window,
        falling back to the hard column cap so progress is guaranteed and no row
        is ever emitted wider than `eff`.
        """
        text = token.text
        graphemes = grapheme_indices(text)
        bounds = word_bound_indices(text)
        idx = 0
        while idx < len(graphemes):
            remaining = eff - self.col
            if remaining <= 0:
                self._emit_break()
                continue

            chunk_w = 0
            chunk_n = 0
            col = self.col
            row_has_content = self.col > self.line_indent
            for _, g in graphemes[idx:]:
                gw = cluster_width(g, col, self.rule.tabs)
                # `chunk_n > 0 or row_has_content`: the Rust guard is `chunk_n > 0`
                # alone, which lets the first cluster of a chunk through even when
                # it does not fit — the overflow described below.
                if chunk_w + gw > remaining and (chunk_n > 0 or row_has_content):
                    break
                chunk_w += gw
                chunk_n += 1
                col += gw
            if chunk_n == 0:
                # Nothing fits in what is left of the row.
                #
                # The Rust code force-emits one grapheme here, which writes past
                # `eff` whenever the remaining gap is narrower than the next
                # cluster — a double-width glyph with one column left overflows
                # by one and the renderer clips it. (The `remaining == 0` guard
                # above catches only the exactly-zero case.) The end state ends
                # the row instead, and forces a cluster only when the row is
                # empty, i.e. when the cluster is wider than an entire row and
                # has nowhere else to go.
                if self.col > self.line_indent:
                    self._emit_break()
                    continue
                chunk_n = 1
                chunk_w = cluster_width(graphemes[idx][1], self.col, self.rule.tabs)

            force_break = False
            if chunk_n > 1:
                shrunk = self._prefer_word_boundary(graphemes, bounds, idx, chunk_n, eff)
                if shrunk is not None:
                    chunk_n = shrunk
                    col = self.col
                    chunk_w = 0
                    for _, g in graphemes[idx : idx + chunk_n]:
                        gw = cluster_width(g, col, self.rule.tabs)
                        chunk_w += gw
                        col += gw
                    force_break = True

            start_byte = graphemes[idx][0]
            end_byte = (
                graphemes[idx + chunk_n][0]
                if idx + chunk_n < len(graphemes)
                else len(text.encode("utf-8"))
            )
            chunk = text.encode("utf-8")[start_byte:end_byte].decode("utf-8")
            src = None if token.source_offset is None else token.source_offset + start_byte
            self._row.push(Token.text_tok(chunk, src, token.style))
            self.col += chunk_w
            idx += chunk_n

            # Break only when the boundary preference demands it. The Rust code
            # also breaks eagerly on `col >= eff` here, which the non-split path
            # does not — so a row filled exactly by a split ends immediately
            # while one filled exactly by a whole token does not, and a trailing
            # newline lands on a different row in the two cases. Breaking lazily
            # (the next token's own overflow check, or `remaining <= 0` on the
            # next pass of this loop) makes the rule uniform, which is what lets
            # a resumed run reproduce the original boundaries.
            if force_break:
                self._emit_break()

    def _prefer_word_boundary(
        self,
        graphemes: list[tuple[int, str]],
        bounds: list[int],
        idx: int,
        chunk_n: int,
        eff: int,
    ) -> int | None:
        """Shrink a chunk so it ends at a word boundary, or None to keep it."""
        slice_start = graphemes[idx][0]
        slice_end = (
            graphemes[idx + chunk_n][0]
            if idx + chunk_n < len(graphemes)
            else graphemes[-1][0] + len(graphemes[-1][1].encode("utf-8"))
        )
        row_floor = max(eff - WRAP_MAX_LOOKBACK, eff // 2)
        floor_from_cursor = max(row_floor - self.col, 0)
        floor_byte = (
            graphemes[idx + floor_from_cursor][0] if floor_from_cursor < chunk_n else slice_end
        )
        candidates = [b for b in bounds if slice_start < b <= slice_end and b >= floor_byte]
        end_byte = graphemes[-1][0] + len(graphemes[-1][1].encode("utf-8"))
        if floor_byte <= end_byte <= slice_end and end_byte > slice_start:
            candidates.append(end_byte)
        if not candidates:
            return None
        target = max(candidates)
        for offset, (b, _) in enumerate(graphemes[idx:]):
            if b == target:
                return offset if 0 < offset < chunk_n else None
        return None

    def _space_overflow(self, eff: int) -> None:
        """Issue #1363: back up over the trailing word instead of stranding a space.

        Emitting a plain break here would leave the overflowing space as a
        leading space on the continuation row. Moving the row's last word down
        instead puts the break at the prior inter-word space, which sits well
        inside `eff`.
        """
        plan = self._back_up_plan(eff)
        if plan is None:
            self._emit_break()
            return
        tail_start, tail_width = plan
        tail = self._row.tokens[tail_start:]
        del self._row.tokens[tail_start:]
        # The accumulator's cached source byte may have belonged to the tail.
        self._row.source_byte = next(
            (t.source_offset for t in self._row.tokens if t.source_offset is not None),
            None,
        )
        self._emit_break()
        for tok in tail:
            self._row.push(tok)
        self.col += tail_width

    def _back_up_plan(self, eff: int) -> tuple[int, int] | None:
        toks = self._row.tokens
        space_idx: int | None = None
        for i in range(len(toks) - 1, -1, -1):
            if toks[i].kind in (Kind.BREAK, Kind.NEWLINE):
                return None
            if toks[i].kind is Kind.SPACE:
                space_idx = i
                break
        if space_idx is None:
            return None
        tail_start = space_idx + 1
        if tail_start >= len(toks):
            return None
        if not any(t.kind is not Kind.SPACE for t in toks[:space_idx]):
            return None
        col = self.line_indent
        for t in toks[tail_start:]:
            if t.kind is Kind.TEXT:
                col += str_width(t.text, col, self.rule.tabs)
            elif t.kind is Kind.SPACE:
                col += 1
            elif t.kind is Kind.BINARY_BYTE:
                col += 4
            else:
                return None
        tail_width = col - self.line_indent
        if self.line_indent + tail_width > eff:
            return None
        return tail_start, tail_width

    # -- GRID rule -----------------------------------------------------------

    def _feed_grid(self, token: Token) -> None:
        cols = self.rule.width
        if cols == 0:
            self._row.push(token)
            return

        if token.kind is Kind.NEWLINE:
            self._row.push(token)
            self._close_row()
            self.col = 0
            # Escapes never span logical lines in captured scroll-back.
            self._ansi.reset()
            self._row_carry = self.carry()
            return

        if token.kind is Kind.BREAK:
            self._row.push(token)
            self._close_row()
            self.col = 0
            return

        if token.kind is Kind.SPACE:
            if self.col > 0 and self.col + 1 > cols:
                self._emit_break(indent=False)
            self._row.push(token)
            self.col += 1
            return

        if token.kind is Kind.BINARY_BYTE:
            if self.col > 0 and self.col + 4 > cols:
                self._emit_break(indent=False)
            self._row.push(token)
            self.col += 4
            return

        text = token.text
        raw = text.encode("utf-8")
        seg_start = 0
        for byte_offset, g in grapheme_indices(text):
            if len(g) == 1 and not self._ansi.feed(g):
                continue
            for c in g[1:]:
                self._ansi.feed(c)
            width = cluster_width(g, self.col, self.rule.tabs)
            if self.col > 0 and self.col + width > cols:
                # A tab's width depends on the column it starts at, so it must be
                # re-measured once the break moves it to column 0. The Rust grid
                # path measures before the break and applies after it, so a tab at
                # a row boundary is accounted one width by the wrap and drawn at
                # another by the renderer.
                if byte_offset > seg_start:
                    src = None if token.source_offset is None else token.source_offset + seg_start
                    self._row.push(
                        Token.text_tok(raw[seg_start:byte_offset].decode("utf-8"), src, token.style)
                    )
                    seg_start = byte_offset
                self._emit_break(indent=False)
                width = cluster_width(g, self.col, self.rule.tabs)
            self.col += width
        if seg_start == 0:
            self._row.push(token)
        elif seg_start < len(raw):
            src = None if token.source_offset is None else token.source_offset + seg_start
            self._row.push(Token.text_tok(raw[seg_start:].decode("utf-8"), src, token.style))

    # -- CHOP rule -----------------------------------------------------------

    def _feed_chop(self, token: Token) -> None:
        """Soft wrap off: a safety chop every `width` characters.

        This is the `MAX_SAFE_LINE_WIDTH` break that `build_base_tokens` injects
        inline today. Folding it into the machine is what lets the index describe
        wrap-off row structure with the same code path as wrap-on.
        """
        if token.kind is Kind.NEWLINE:
            self._row.push(token)
            self._close_row()
            self.chars_in_row = 0
            self._row_carry = self.carry()
            return
        if token.kind is Kind.BREAK:
            self._row.push(token)
            self._close_row()
            self.chars_in_row = 0
            return
        if token.kind is not Kind.TEXT:
            if self.chars_in_row >= self.rule.width:
                self._emit_break(indent=False)
            self._row.push(token)
            self.chars_in_row += 1
            return

        raw = token.text.encode("utf-8")
        emitted = 0
        buf: list[str] = []
        buf_start = 0
        for ch in token.text:
            if self.chars_in_row >= self.rule.width:
                if buf:
                    src = None if token.source_offset is None else token.source_offset + buf_start
                    self._row.push(Token.text_tok("".join(buf), src, token.style))
                    buf = []
                self._emit_break(indent=False)
                buf_start = emitted
            buf.append(ch)
            emitted += len(ch.encode("utf-8"))
            self.chars_in_row += 1
        if buf:
            src = None if token.source_offset is None else token.source_offset + buf_start
            self._row.push(Token.text_tok("".join(buf), src, token.style))
        assert emitted == len(raw)
