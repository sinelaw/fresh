"""Source bytes → base token stream.

Mirrors `split_rendering::base_tokens::build_base_tokens`, minus the character
budget. The budget existed only because the renderer always started at byte 0 of
a logical line and had to guess how much of the prefix it would throw away; with
an anchored viewport there is nothing to throw away, so it is deleted here —
`row_budget` replaces it and is exact.

The `MAX_SAFE_LINE_WIDTH` forced break also moves out: it is now
`WrapRule.chop`, applied by the wrap machine like any other rule, so wrap-off
row structure is described by the same code path as wrap-on.
"""

from __future__ import annotations

from enum import Enum, auto

from .buffer import MAX_LINE_BYTES, LineIterator, TextBuffer
from .tokens import Kind, Token


class LineEnding(Enum):
    LF = auto()
    CRLF = auto()


def is_control_char(ch: str) -> bool:
    """Control characters rendered as `<XX>` — ESC, tab, and newline excepted."""
    code = ord(ch)
    if code >= 128:
        return False
    if code in (0x09, 0x0A, 0x1B):
        return False
    return code < 0x20 or code == 0x7F


def tokenize_text(text: str, base_offset: int, line_ending: LineEnding) -> list[Token]:
    """Tokenize one run of source text, coalescing runs of ordinary characters.

    CRLF is emitted as a single `Newline` at the `\\r`, with the `\\n` skipped —
    so a CRLF line break is one logical break carrying two source bytes.
    """
    tokens: list[Token] = []
    raw = text.encode("utf-8")
    byte = 0
    skip_lf = False
    pending: list[str] = []
    pending_start = 0

    def flush() -> None:
        nonlocal pending
        if pending:
            tokens.append(Token.text_tok("".join(pending), base_offset + pending_start))
            pending = []

    for ch in text:
        ch_len = len(ch.encode("utf-8"))
        off = base_offset + byte
        if ch == "\r":
            nxt = raw[byte + ch_len : byte + ch_len + 1]
            if line_ending is LineEnding.CRLF and nxt == b"\n":
                flush()
                tokens.append(Token.newline(off))
                skip_lf = True
                byte += ch_len
                continue
            flush()
            tokens.append(Token.binary(0x0D, off))
        elif ch == "\n" and skip_lf:
            skip_lf = False
            byte += ch_len
            continue
        elif ch == "\n":
            flush()
            tokens.append(Token.newline(off))
        elif ch == " ":
            flush()
            tokens.append(Token.space(off))
        elif ch == "\t":
            flush()
            tokens.append(Token.text_tok("\t", off))
        elif is_control_char(ch):
            flush()
            tokens.append(Token.binary(ord(ch), off))
        else:
            if not pending:
                pending_start = byte
            pending.append(ch)
        byte += ch_len
    flush()
    return tokens


def build_base_tokens(
    buffer: TextBuffer,
    start_byte: int,
    *,
    byte_budget: int | None = None,
    line_ending: LineEnding = LineEnding.LF,
    fold_skip: list[tuple[int, int]] | None = None,
    mid_line: bool = True,
) -> list[Token]:
    """Tokenize forward from `start_byte`.

    `start_byte` may be *mid-line* — that is the whole point of the redesign.
    The caller passes a byte obtained from `WrapIndex.byte_of_row`, so no
    backward scan to the line start runs.

    `byte_budget` bounds the read. The caller derives it from the rows it needs
    and the pane width, and also caps `LineIterator`'s per-line read with it — so
    a single enormous line is not slurped in `MAX_LINE_BYTES` chunks to produce
    text the wrap machine stops consuming after a few rows.
    """
    folds = fold_skip or []
    tokens: list[Token] = []
    cursor = start_byte

    fold_idx = 0
    while fold_idx < len(folds) and folds[fold_idx][1] <= cursor:
        fold_idx += 1
    if fold_idx < len(folds) and folds[fold_idx][0] <= cursor < folds[fold_idx][1]:
        cursor = folds[fold_idx][1]
        fold_idx += 1

    budget = None if byte_budget is None else max(byte_budget, 1)

    consumed = 0
    while True:
        segment_end = folds[fold_idx][0] if fold_idx < len(folds) else len(buffer)
        if cursor >= len(buffer):
            break
        cap = MAX_LINE_BYTES if budget is None else min(budget + 1, MAX_LINE_BYTES)
        it = (
            LineIterator.from_mid_line(buffer, cursor, cap)
            if mid_line
            else LineIterator(buffer, cursor, max_line_bytes=cap)
        )
        while True:
            nxt = it.next_line()
            if nxt is None:
                return tokens
            line_start, text = nxt
            if fold_idx < len(folds) and line_start >= segment_end:
                break
            tokens.extend(tokenize_text(text, line_start, line_ending))
            consumed += len(text.encode("utf-8"))
            if budget is not None and consumed >= budget:
                return tokens
        if fold_idx < len(folds):
            cursor = folds[fold_idx][1]
            fold_idx += 1
        else:
            break
    return tokens


def line_tokens(
    buffer: TextBuffer,
    line: int,
    line_ending: LineEnding = LineEnding.LF,
) -> list[Token]:
    """Base tokens for exactly one logical line.

    This is what feeds the wrap index. It matters that the index is built from
    the *real* tokenizer rather than from raw line text: today's count-only
    mirrors wrap a single synthetic `Text` token, so the space-overflow back-up
    (issue #1363) — which only fires on `Space` tokens — never runs, and their
    counts can differ from what the renderer draws. Sharing this function makes
    that class of drift impossible.
    """
    start = buffer.line_start_offset(line)
    raw = buffer.get_line(line)
    return tokenize_text(raw.decode("utf-8", errors="replace"), start, line_ending)


def strip_trailing_newline(tokens: list[Token]) -> list[Token]:
    """Drop a trailing `Newline`, which belongs to the line break, not a row."""
    if tokens and tokens[-1].kind is Kind.NEWLINE:
        return tokens[:-1]
    return tokens
