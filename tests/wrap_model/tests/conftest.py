"""Shared strategies and helpers.

The generators deliberately produce the inputs that have historically broken
wrapping: unbreakable words wider than the pane, tabs (column-dependent width),
CJK double-width cells, combining marks and ZWJ clusters (multi-codepoint, some
zero-width), control bytes (rendered `<XX>`, width 4), and runs of spaces.
"""

from __future__ import annotations

import sys
from pathlib import Path

from hypothesis import strategies as st

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from wrap_model.decorations import (
    Conceal,
    Decorations,
    Fold,
    InlineVirtualText,
    Scope,
    SoftBreak,
    VirtualLine,
    VirtualLinePos,
)
from wrap_model.wrap_machine import WrapRule

#: Characters chosen so every width class and cluster shape appears.
INTERESTING_CHARS = [
    "a",
    "b",
    "Z",
    "_",
    ".",
    "(",
    ")",
    "-",
    " ",
    "\t",
    "日",  # double width
    "가",  # double width
    "é",
    "é",  # combining mark: 2 codepoints, width 1
    "‍",  # ZWJ
    "\x07",  # control -> BinaryByte, width 4
]

WORDS = ["the", "quick", "brown", "fox", "supercalifragilistic", "a", "x" * 40, "日本語"]


#: `sampled_from` + join rather than `st.text(alphabet=...)`: some entries are
#: multi-codepoint clusters (a combining mark, a ZWJ), which an alphabet forbids
#: — and those clusters are exactly what the segmenter needs exercised.
_chunks = st.lists(st.sampled_from(INTERESTING_CHARS), max_size=60).map("".join)


@st.composite
def texts(draw: st.DrawFn, max_lines: int = 4) -> str:
    """Multi-line text built from interesting characters."""
    lines = draw(st.lists(_chunks, min_size=1, max_size=max_lines))
    return "\n".join(lines)


@st.composite
def word_texts(draw: st.DrawFn, max_words: int = 40) -> str:
    """Prose-shaped text — exercises the word-boundary and space-overflow paths."""
    words = draw(st.lists(st.sampled_from(WORDS), min_size=1, max_size=max_words))
    return " ".join(words)


@st.composite
def single_long_line(draw: st.DrawFn, min_words: int = 30) -> str:
    """The pathological case this whole design exists for: one enormous line."""
    n = draw(st.integers(min_value=min_words, max_value=min_words + 60))
    return " ".join(f"w{i}" for i in range(n))


@st.composite
def rules(draw: st.DrawFn) -> WrapRule:
    kind = draw(st.sampled_from(["word", "grid", "chop"]))
    width = draw(st.integers(min_value=1, max_value=40))
    if kind == "word":
        return WrapRule.word(
            width,
            gutter=draw(st.integers(min_value=0, max_value=4)),
            hanging_indent=draw(st.booleans()),
        )
    if kind == "grid":
        return WrapRule.grid(width)
    return WrapRule.chop(width)


def char_boundaries(text: str) -> list[int]:
    """Byte offsets that sit on a character boundary, plus end-of-buffer.

    Edits are only ever generated here — a real editor never splits a codepoint,
    because every edit is anchored to a cursor and cursors live on character
    boundaries. Generating mid-codepoint edits would test a precondition the
    system does not have to satisfy.
    """
    offsets = [0]
    byte = 0
    for ch in text:
        byte += len(ch.encode("utf-8"))
        offsets.append(byte)
    return offsets


@st.composite
def edits(draw: st.DrawFn, text: str) -> tuple[int, int, str]:
    """`(start, removed, inserted)`, aligned to character boundaries."""
    bounds = char_boundaries(text)
    i = draw(st.integers(min_value=0, max_value=len(bounds) - 1))
    j = draw(st.integers(min_value=i, max_value=len(bounds) - 1))
    inserted = draw(st.lists(st.sampled_from([*INTERESTING_CHARS, "\n"]), max_size=8).map("".join))
    return (bounds[i], bounds[j] - bounds[i], inserted)


def decorations_for(text: str, *, kinds: frozenset[str]) -> Decorations:
    """Deterministic decorations placed at stable offsets in `text`.

    Deterministic rather than random so a matrix cell is reproducible: the point
    of the matrix is coverage of *combinations*, with randomness supplied by the
    property tests.
    """
    deco = Decorations()
    n = len(text.encode("utf-8"))
    if n == 0:
        return deco
    if "soft_break" in kinds:
        deco.add_soft_break(SoftBreak(position=min(10, n - 1), indent=2))
    if "soft_break_cursor" in kinds:
        # Anchored at a token start so the break actually fires — a soft break
        # only applies where a token begins.
        pos = _token_start_near(text, 11)
        deco.add_soft_break(SoftBreak(position=pos, indent=1, scope=Scope(pos - 1, pos + 5)))
    if "conceal" in kinds:
        deco.add_conceal(Conceal(start=min(4, n - 1), end=min(8, n)))
    if "conceal_replace" in kinds:
        deco.add_conceal(
            Conceal(start=min(16, n - 1), end=min(22, n), replacement="→", namespace="md-syntax")
        )
    if "conceal_cursor" in kinds:
        deco.add_conceal(
            Conceal(
                start=min(24, n - 1),
                end=min(30, n),
                replacement="*",
                scope=Scope(min(24, n - 1), min(30, n)),
            )
        )
    if "inline_virtual" in kinds:
        deco.add_inline_virtual(InlineVirtualText(position=min(6, n - 1), text=": int"))
    if "virtual_line" in kinds:
        deco.add_virtual_line(VirtualLine(position=0, text="— hint —", where=VirtualLinePos.ABOVE))
    if "fold" in kinds:
        # Folds are line-granular in the editor (`FoldManager::resolved_ranges`
        # yields whole-line ranges), so fold a whole line or nothing.
        span = _line_span(text, 1)
        if span is not None:
            deco.add_fold(Fold(*span))
    return deco


def _token_start_near(text: str, target: int) -> int:
    """Byte offset of the word start at or after `target`."""
    raw = text.encode("utf-8")
    i = min(target, max(len(raw) - 1, 0))
    while 0 < i < len(raw) and raw[i - 1 : i] != b" ":
        i += 1
        if i >= len(raw):
            return max(len(raw) - 1, 0)
    return i


def _line_span(text: str, line: int) -> tuple[int, int] | None:
    """`(start, end)` bytes of `line`, or None if the text has no such line."""
    raw = text.encode("utf-8")
    starts = [0] + [i + 1 for i, b in enumerate(raw) if b == 0x0A]
    if line >= len(starts):
        return None
    start = starts[line]
    end = starts[line + 1] if line + 1 < len(starts) else len(raw)
    return (start, end)
