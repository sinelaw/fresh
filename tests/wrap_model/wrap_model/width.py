"""Display-width and segmentation primitives.

Mirrors `primitives::display_width`, `primitives::visual_layout`, and
`primitives::ansi`.

**Deliberate simplification.** The real editor uses the `unicode-segmentation`
crate (full UAX #29). Python has no stdlib grapheme segmenter, so this module
implements a reduced clusterer: combining marks, ZWJ sequences, and regional
indicator pairs. That is enough to exercise every code path in the wrap machine
(zero-width continuations, multi-codepoint clusters, double-width cells) while
staying readable. The model's contract is *structural* — that one rule decides
row boundaries and that resume/repair agree with a full rebuild — and that
contract is independent of which segmenter is plugged in.

**A real inconsistency this model deliberately unifies.** The Rust code uses
three different tab stops today: `visual_layout::TAB_WIDTH` (8) inside the wrap
transform's grapheme path, a hardcoded `tab_stop = 4` in the same function's
hanging-indent measurement, and the configurable `tab_size` (default 4) in
`ViewLineIterator`. A tab therefore measures one width when deciding where a row
breaks and another when the row is drawn. The end-state design has exactly one
`TabPolicy`, threaded everywhere; `LEGACY_TAB_QUIRK` reproduces the old split so
a migration test can pin the difference.
"""

from __future__ import annotations

import unicodedata
from dataclasses import dataclass

from .metrics import bump

ESC = "\x1b"

#: Set by the migration test to reproduce the pre-unification tab behaviour.
LEGACY_TAB_QUIRK = False

#: `visual_layout::TAB_WIDTH`.
LEGACY_TAB_WIDTH_WRAP = 8
#: The hardcoded `tab_stop` in `apply_wrapping_transform`'s indent measuring.
LEGACY_TAB_WIDTH_INDENT = 4


@dataclass(frozen=True, slots=True)
class TabPolicy:
    """One tab stop for the whole pipeline — wrap decisions and rendering.

    Replaces the three-way split described in the module docstring.
    """

    width: int = 4

    def expansion_at(self, col: int) -> int:
        """Columns a tab occupies when it starts at visual column `col`."""
        if self.width <= 0:
            return 1
        return self.width - (col % self.width)


DEFAULT_TABS = TabPolicy(4)


def char_width(ch: str) -> int:
    """Display columns for a single codepoint.

    Zero for combining marks, ZWJ, and other non-spacing codepoints; two for
    East Asian Wide/Fullwidth; one otherwise. Control characters are zero here —
    the tokenizer turns them into `BinaryByte` tokens (width 4) before they ever
    reach this function.
    """
    if ch == "\t":
        raise ValueError("tab width is column-dependent; use TabPolicy.expansion_at")
    if unicodedata.combining(ch):
        return 0
    cat = unicodedata.category(ch)
    if cat in ("Mn", "Me", "Cf"):
        return 0
    if cat == "Cc":
        return 0
    if unicodedata.east_asian_width(ch) in ("W", "F"):
        return 2
    return 1


def _is_regional_indicator(ch: str) -> bool:
    return "\U0001f1e6" <= ch <= "\U0001f1ff"


def grapheme_clusters(s: str) -> list[str]:
    """Reduced UAX #29 clustering (see module docstring)."""
    out: list[str] = []
    i = 0
    n = len(s)
    while i < n:
        start = i
        ch = s[i]
        i += 1
        if _is_regional_indicator(ch) and i < n and _is_regional_indicator(s[i]):
            i += 1
        while i < n:
            nxt = s[i]
            if unicodedata.combining(nxt) or unicodedata.category(nxt) in ("Mn", "Me"):
                i += 1
                continue
            if nxt == "‍":  # ZWJ binds the following codepoint into the cluster
                i += 1
                if i < n:
                    i += 1
                continue
            break
        out.append(s[start:i])
    return out


def grapheme_indices(s: str) -> list[tuple[int, str]]:
    """`(byte_offset, cluster)` pairs — the shape `grapheme_indices(true)` returns."""
    out: list[tuple[int, str]] = []
    byte = 0
    for g in grapheme_clusters(s):
        out.append((byte, g))
        byte += len(g.encode("utf-8"))
    return out


def cluster_width(g: str, col: int, tabs: TabPolicy) -> int:
    """Display columns for one grapheme cluster starting at column `col`."""
    bump("width_measurements")
    if g == "\t":
        if LEGACY_TAB_QUIRK:
            return LEGACY_TAB_WIDTH_WRAP - (col % LEGACY_TAB_WIDTH_WRAP)
        return tabs.expansion_at(col)
    return sum(char_width(c) for c in g)


def str_width(s: str, start_col: int = 0, tabs: TabPolicy = DEFAULT_TABS) -> int:
    """Total display columns of `s` when laid out starting at `start_col`.

    ANSI escape sequences are consumed with zero width, mirroring
    `visual_layout::visual_width`.
    """
    col = start_col
    parser = AnsiParser()
    for g in grapheme_clusters(s):
        if len(g) == 1 and not parser.feed(g):
            continue
        col += cluster_width(g, col, tabs)
    return col - start_col


def contains_ansi(s: str) -> bool:
    return ESC in s


class AnsiParser:
    """Consumes SGR escape sequences, reporting their codepoints as zero-width.

    Mirrors `primitives::ansi::AnsiParser`: `feed` returns False while inside an
    escape sequence (the codepoint is invisible) and True for a printable one.
    """

    __slots__ = ("in_csi", "in_escape")

    def __init__(self, in_escape: bool = False, in_csi: bool = False) -> None:
        self.in_escape = in_escape
        self.in_csi = in_csi

    def feed(self, ch: str) -> bool:
        if self.in_escape:
            if self.in_csi:
                if "@" <= ch <= "~":
                    self.in_escape = False
                    self.in_csi = False
            elif ch == "[":
                self.in_csi = True
            else:
                self.in_escape = False
            return False
        if ch == ESC:
            self.in_escape = True
            return False
        return True

    def reset(self) -> None:
        self.in_escape = False
        self.in_csi = False

    def snapshot(self) -> tuple[bool, bool]:
        return (self.in_escape, self.in_csi)

    @staticmethod
    def restore(state: tuple[bool, bool]) -> AnsiParser:
        return AnsiParser(state[0], state[1])


#: `visual_layout::WRAP_MAX_LOOKBACK`.
WRAP_MAX_LOOKBACK = 16


def word_bound_indices(s: str) -> list[int]:
    """Byte offsets of word boundaries — a reduced stand-in for UAX #29.

    Boundaries land at transitions between alphanumeric runs and everything
    else, which is enough to reproduce the behaviour the lookback exists for:
    breaking `dialog.getButton(...).setOnClickListener` at `)` rather than
    mid-identifier.
    """
    bounds: list[int] = []
    byte = 0
    prev_alnum: bool | None = None
    for g in grapheme_clusters(s):
        alnum = g[0].isalnum() or g[0] == "_"
        if (prev_alnum is not None and alnum != prev_alnum) or (
            prev_alnum is not None and not alnum
        ):
            bounds.append(byte)
        prev_alnum = alnum
        byte += len(g.encode("utf-8"))
    return bounds
