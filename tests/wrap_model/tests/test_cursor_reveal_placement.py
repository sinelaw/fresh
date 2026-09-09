"""Placement when revealing the cursor's line changes its row count a lot.

The matrix's activation cells use one small conceal and one soft break, whose
reveal shifts the cursor by at most a row — too little to distinguish "place
against the canonical row" from "place against the drawn row". Markdown is not
that gentle: a paragraph dense with concealed syntax gains several rows the
moment the cursor lands in it. This file pins the distinction with a line whose
reveal roughly triples its rows; the canonical-row strategy parks the drawn
cursor far outside the band (fresh#1574's stall), the drawn-row strategy keeps
it pinned.
"""

from __future__ import annotations

from wrap_model.decorations import Conceal, Decorations, Scope
from wrap_model.editor import EditorModel
from wrap_model.wrap_machine import WrapRule

WIDTH = 20
HEIGHT = 6
MARGIN = 2


def dense_conceal_model() -> EditorModel:
    """One paragraph line where most syntax is concealed to a single glyph.

    Ten runs of 24 'x's, each concealed to "*", each scoped to the whole line —
    the way markdown reveals a whole construct while the cursor is anywhere in
    it. Canonical: ~2 rows. Revealed: ~13. A short line above and below give
    the viewport somewhere to scroll from and to.
    """
    filler = "above the paragraph\n"
    runs = " ".join("x" * 24 for _ in range(10))
    text = filler + runs + "\nbelow the paragraph\n"
    deco = Decorations()
    line_start = len(filler)
    line_end = line_start + len(runs)
    pos = line_start
    for _ in range(10):
        deco.add_conceal(
            Conceal(
                start=pos,
                end=pos + 24,
                replacement="*",
                scope=Scope(line_start, line_end),
            )
        )
        pos += 25
    return EditorModel(text, rule=WrapRule.word(WIDTH), height=HEIGHT, decorations=deco)


def rendered_cursor_row(model: EditorModel, byte: int) -> int | None:
    for i, row in enumerate(model.render().rows):
        if byte in {b for b in row.char_source_bytes if b is not None}:
            return i
    return None


def test_reveal_shifts_many_rows() -> None:
    """Fixture sanity: the reveal must be large or this file pins nothing."""
    model = dense_conceal_model()
    line_start = len("above the paragraph\n")
    model.cursors = ()
    blind_total = model.index.total_rows()
    model.cursors = (line_start + 100,)
    drawn = model.cursor_visual_row(model.cursors[0])
    canonical = model.index.row_of_byte(model.cursors[0])
    assert drawn - canonical >= 4, (
        f"reveal delta only {drawn - canonical} rows (canonical {canonical}, "
        f"drawn {drawn}, blind total {blind_total}) — fixture too tame to pin anything"
    )


def test_drawn_cursor_stays_in_band_under_heavy_reveal() -> None:
    """The property the canonical-row strategy fails: walk the cursor through
    the revealed paragraph; after every placement the drawn cursor sits inside
    the effective band (or at a document edge)."""
    model = dense_conceal_model()
    vp = model.viewport
    m = vp.effective_margin(MARGIN)
    line_start = len("above the paragraph\n")
    for offset in range(0, 248, 7):
        byte = line_start + offset
        model.cursors = (byte,)
        model.ensure_cursor_visible(MARGIN)
        row = rendered_cursor_row(model, byte)
        assert row is not None, f"cursor byte {byte} not drawn"
        in_band = m <= row <= HEIGHT - 1 - m
        at_top = vp.top_row() == 0
        at_bottom = vp.top_row() >= vp.max_top_row()
        assert in_band or (row < m and at_top) or (row > HEIGHT - 1 - m and at_bottom), (
            f"byte {byte}: drawn row {row} outside band [{m}, {HEIGHT - 1 - m}], "
            f"top={vp.top_row()} (edges: top={at_top}, bottom={at_bottom})"
        )
