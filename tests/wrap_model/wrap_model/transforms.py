"""Token-stream passes that run before wrapping, plus the wrap driver.

Order matters and matches the renderer:

    base tokens
      → soft breaks      (plugin line breaks; Compose mode only)
      → conceals         (hide/replace byte ranges)
      → inline virtual   (inlay hints; width participates in wrapping)
      → WRAP             ← the wrap machine, the single row-boundary authority
      → rows

`apply_wrapping_transform` is now a thin driver over `WrapMachine`: it keeps the
public shape the renderer expects (tokens with `Break` spliced in) while the
decision logic lives in one place.
"""

from __future__ import annotations

from .decorations import Conceal, InlineVirtualText
from .tokens import Kind, Token
from .wrap_machine import RowCarry, WrapMachine, WrapOutput, WrapRule


def apply_wrapping_transform(
    tokens: list[Token], rule: WrapRule, carry: RowCarry | None = None
) -> list[Token]:
    """Renderer-facing driver: the wrapped token stream."""
    return WrapMachine.run(tokens, rule, carry).tokens


def wrap_rows(tokens: list[Token], rule: WrapRule, carry: RowCarry | None = None) -> WrapOutput:
    """Index-facing driver: the same run, read for its row boundaries."""
    return WrapMachine.run(tokens, rule, carry)


def apply_soft_breaks(tokens: list[Token], breaks: list[tuple[int, int]]) -> list[Token]:
    """Insert plugin-requested breaks, consuming a `Space` that sits on one.

    A break landing on a Space replaces it (the space would otherwise be
    stranded at the start of the continuation row); anywhere else the break is
    inserted before the token.
    """
    if not breaks:
        return tokens
    out: list[Token] = []
    idx = 0
    for tok in tokens:
        if tok.source_offset is None:
            out.append(tok)
            continue
        offset = tok.source_offset
        while idx < len(breaks) and breaks[idx][0] < offset:
            idx += 1
        if idx < len(breaks) and breaks[idx][0] == offset:
            indent = breaks[idx][1]
            idx += 1
            out.append(Token.newline(None))
            out.extend(Token.space(None) for _ in range(indent))
            if tok.kind is not Kind.SPACE:
                out.append(tok)
        else:
            out.append(tok)
    return out


def apply_conceal_ranges(tokens: list[Token], conceals: list[Conceal]) -> list[Token]:
    """Hide concealed bytes, emitting each range's replacement at most once.

    Text tokens are split at conceal boundaries; single-byte tokens (Space,
    Newline) are dropped wholesale. The replacement's first character keeps a
    source offset so a click or cursor can still land on the concealed range.
    """
    if not conceals:
        return tokens
    ordered = sorted(conceals, key=lambda c: c.start)
    emitted: set[int] = set()

    def concealing(byte: int) -> int | None:
        for i, c in enumerate(ordered):
            if c.start <= byte < c.end:
                return i
            if c.start > byte:
                break
        return None

    def emit_replacement(out: list[Token], idx: int) -> None:
        c = ordered[idx]
        if c.replacement is None or idx in emitted:
            return
        emitted.add(idx)
        if not c.replacement:
            return
        first, rest = c.replacement[0], c.replacement[1:]
        out.append(Token.text_tok(first, c.start))
        if rest:
            out.append(Token.text_tok(rest, None))

    out: list[Token] = []
    for tok in tokens:
        if tok.source_offset is None:
            out.append(tok)
            continue
        if tok.kind is Kind.TEXT:
            byte = tok.source_offset
            visible: list[str] = []
            visible_start: int | None = None
            for ch in tok.text:
                idx = concealing(byte)
                if idx is not None:
                    if visible:
                        out.append(Token.text_tok("".join(visible), visible_start, tok.style))
                        visible = []
                        visible_start = None
                    emit_replacement(out, idx)
                else:
                    if visible_start is None:
                        visible_start = byte
                    visible.append(ch)
                byte += len(ch.encode("utf-8"))
            if visible:
                out.append(Token.text_tok("".join(visible), visible_start, tok.style))
        else:
            idx = concealing(tok.source_offset)
            if idx is None:
                out.append(tok)
            else:
                emit_replacement(out, idx)
    return out


def splice_inline_virtual_text(tokens: list[Token], hints: list[InlineVirtualText]) -> list[Token]:
    """Insert inlay hints before wrapping so their width shifts row boundaries."""
    if not hints:
        return tokens
    ordered = sorted(hints, key=lambda h: h.position)
    out: list[Token] = []
    idx = 0
    for tok in tokens:
        if tok.source_offset is not None:
            while idx < len(ordered) and ordered[idx].position <= tok.source_offset:
                out.append(Token.text_tok(ordered[idx].text, None, ordered[idx].style))
                idx += 1
        out.append(tok)
    while idx < len(ordered):
        out.append(Token.text_tok(ordered[idx].text, None, ordered[idx].style))
        idx += 1
    return out


def apply_fold_skip(tokens: list[Token], folds: list[tuple[int, int]]) -> list[Token]:
    """Drop tokens whose source byte lies inside a collapsed range.

    Defence in depth: `build_base_tokens` already refuses to read folded bytes,
    but a plugin view transform's stream never went through it.
    """
    if not folds:
        return tokens
    out: list[Token] = []
    cursor = 0
    for tok in tokens:
        if tok.source_offset is None:
            out.append(tok)
            continue
        off = tok.source_offset
        while cursor < len(folds) and folds[cursor][1] <= off:
            cursor += 1
        if cursor < len(folds) and folds[cursor][0] <= off < folds[cursor][1]:
            continue
        out.append(tok)
    return out
