"""Plugin-owned decorations that change what gets wrapped.

Everything here is anchored to buffer bytes and versioned by its own manager,
which is why the wrap index cannot be keyed on geometry alone: a soft break
appearing or a conceal activating changes the effective text without the buffer
changing at all.

**Cursor-dependent activation** is the subtle one. `markdown_compose` conceals
its markup unless the cursor is inside the concealed span — so the *rendered*
row layout near the cursor differs from the cursor-blind layout. The end-state
rule (see `wrap_index`): the index is canonical (`cursors=()`), and the renderer
materialises cursor-aware rows only inside its own window. Scroll math and the
scrollbar already query cursor-blind today (`cursor_sig: 0`), so this codifies an
existing convention rather than inventing one.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto


@dataclass(frozen=True, slots=True)
class Scope:
    """Byte span that suppresses a decoration while a cursor is inside it."""

    start: int
    end: int

    def contains(self, byte: int) -> bool:
        return self.start <= byte <= self.end


def _active(scope: Scope | None, cursors: tuple[int, ...]) -> bool:
    if scope is None:
        return True
    return not any(scope.contains(c) for c in cursors)


@dataclass(frozen=True, slots=True)
class SoftBreak:
    """A plugin-requested line break at `position`, with continuation `indent`."""

    position: int
    indent: int = 0
    scope: Scope | None = None


@dataclass(frozen=True, slots=True)
class Conceal:
    """Hide `[start, end)`, optionally replacing it with `replacement`."""

    start: int
    end: int
    replacement: str | None = None
    namespace: str = "default"
    scope: Scope | None = None


@dataclass(frozen=True, slots=True)
class InlineVirtualText:
    """An inlay hint spliced in *before* wrapping, so its width affects rows."""

    position: int
    text: str
    style: str | None = None


class VirtualLinePos(Enum):
    ABOVE = auto()
    BELOW = auto()


@dataclass(frozen=True, slots=True)
class VirtualLine:
    """A whole injected row above or below the line containing `position`."""

    position: int
    text: str
    where: VirtualLinePos = VirtualLinePos.ABOVE
    gutter_glyph: str | None = None


@dataclass(frozen=True, slots=True)
class Fold:
    """A collapsed source range. Folded bytes are never read or tokenized."""

    start: int
    end: int


@dataclass(slots=True)
class Decorations:
    """All decoration state for one buffer, with per-manager versions.

    Each manager bumps its own version; the wrap index's damage contract keys
    off the tuple, so an unrelated manager's churn is distinguishable from a
    buffer edit.
    """

    soft_breaks: list[SoftBreak] = field(default_factory=list)
    conceals: list[Conceal] = field(default_factory=list)
    inline_virtual: list[InlineVirtualText] = field(default_factory=list)
    virtual_lines: list[VirtualLine] = field(default_factory=list)
    folds: list[Fold] = field(default_factory=list)

    soft_break_version: int = 0
    conceal_version: int = 0
    inline_virtual_version: int = 0
    virtual_line_version: int = 0

    def pipeline_version(self) -> tuple[int, int, int, int]:
        return (
            self.soft_break_version,
            self.conceal_version,
            self.inline_virtual_version,
            self.virtual_line_version,
        )

    # -- activation-filtered views ------------------------------------------

    def active_soft_breaks(self, cursors: tuple[int, ...] = ()) -> list[tuple[int, int]]:
        return sorted(
            (sb.position, sb.indent) for sb in self.soft_breaks if _active(sb.scope, cursors)
        )

    def active_conceals(
        self, cursors: tuple[int, ...] = (), exclude_namespace: str | None = None
    ) -> list[Conceal]:
        return sorted(
            (
                c
                for c in self.conceals
                if _active(c.scope, cursors) and c.namespace != exclude_namespace
            ),
            key=lambda c: c.start,
        )

    def fold_skip(self) -> list[tuple[int, int]]:
        """Sorted, non-overlapping skip ranges, as `ViewLineIterator` wants."""
        if not self.folds:
            return []
        ordered = sorted((f.start, f.end) for f in self.folds)
        merged: list[tuple[int, int]] = [ordered[0]]
        for start, end in ordered[1:]:
            last_start, last_end = merged[-1]
            if start <= last_end:
                merged[-1] = (last_start, max(last_end, end))
            else:
                merged.append((start, end))
        return merged

    def virtual_lines_in(self, start: int, end: int) -> list[VirtualLine]:
        return sorted(
            (v for v in self.virtual_lines if start <= v.position < end),
            key=lambda v: v.position,
        )

    # -- mutation (each bumps exactly one version) ---------------------------

    def add_soft_break(self, sb: SoftBreak) -> None:
        self.soft_breaks.append(sb)
        self.soft_break_version += 1

    def add_conceal(self, c: Conceal) -> None:
        self.conceals.append(c)
        self.conceal_version += 1

    def add_inline_virtual(self, v: InlineVirtualText) -> None:
        self.inline_virtual.append(v)
        self.inline_virtual_version += 1

    def add_virtual_line(self, v: VirtualLine) -> None:
        self.virtual_lines.append(v)
        self.virtual_line_version += 1

    def add_fold(self, f: Fold) -> None:
        self.folds.append(f)

    def shift_for_edit(self, start: int, delta: int) -> None:
        """Move anchors after an edit, the way marker-anchored decorations do."""

        def sh(pos: int) -> int:
            return pos + delta if pos >= start else pos

        def sh_scope(s: Scope | None) -> Scope | None:
            return None if s is None else Scope(sh(s.start), sh(s.end))

        self.soft_breaks = [
            SoftBreak(sh(b.position), b.indent, sh_scope(b.scope)) for b in self.soft_breaks
        ]
        self.conceals = [
            Conceal(sh(c.start), sh(c.end), c.replacement, c.namespace, sh_scope(c.scope))
            for c in self.conceals
        ]
        self.inline_virtual = [
            InlineVirtualText(sh(v.position), v.text, v.style) for v in self.inline_virtual
        ]
        self.virtual_lines = [
            VirtualLine(sh(v.position), v.text, v.where, v.gutter_glyph) for v in self.virtual_lines
        ]
        self.folds = [Fold(sh(f.start), sh(f.end)) for f in self.folds]
