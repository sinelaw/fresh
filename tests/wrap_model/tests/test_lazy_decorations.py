"""Decoration lag — the `lines_changed` contract markdown compose is built on.

The plugin decorates a line only after the editor has drawn it and the async
round-trip has come back. Two consequences, each pinned here:

* a placement decided before the arrival is placed against rows that are about
  to change, and nothing else re-runs it — so the editor MUST re-place when the
  pipeline version moves (the trigger the Rust's phase E-B implements);
* with that trigger, the place → render → arrive loop converges: the cursor's
  neighbourhood becomes fully decorated and the cursor is on the screen the
  user actually sees. Regions never rendered stay undecorated — that is
  faithful to compose, not a defect — so totals for unvisited spans remain
  approximate while everything near the cursor is exact.
"""

from __future__ import annotations

from wrap_model.decorations import Decorations, SoftBreak
from wrap_model.editor import EditorModel, LazyDecoration
from wrap_model.wrap_machine import WrapRule

LINES = 30
MARGIN = 2


def compose_like_model(height: int = 6) -> tuple[EditorModel, int]:
    """A paragraph-per-line document whose soft breaks arrive lazily.

    Undecorated, every line is one row; once a line has been drawn, its break
    arrives and the line becomes two. That is compose's shape in miniature:
    the canonical index undercounts exactly the region the user is scrolling
    into.
    """
    text = "".join(f"paragraph {i:02d} filler filler filler xx\n" for i in range(LINES))
    model = EditorModel(text, rule=WrapRule.word(40), height=height)
    for line in range(LINES):
        # +19 is the space after the first "filler" — a token start, which is
        # the only place a soft break applies (see conftest's same note).
        pos = model.buffer.line_start_offset(line) + 19

        def arrive(deco: Decorations, pos: int = pos) -> None:
            deco.add_soft_break(SoftBreak(position=pos, indent=0))

        model.lazy_decorations.append(LazyDecoration(line=line, apply=arrive))
    cursor = model.buffer.line_start_offset(LINES - 1)
    return model, cursor


def shown_bytes(model: EditorModel) -> set[int]:
    return {
        b for row in model.render().rows for b in row.char_source_bytes if b is not None
    }


def test_arrival_without_replacement_loses_the_cursor() -> None:
    """Pins that the re-place trigger is REQUIRED, not defensive.

    After the first frame's lines are decorated, the same viewport draws more
    rows above the cursor than placement assumed, and the cursor falls off the
    bottom. The old post-render pass hid this by re-correcting every frame; the
    single-pass design must re-place on version change instead.
    """
    model, cursor = compose_like_model()
    model.viewport.ensure_visible(cursor, MARGIN)
    assert cursor in shown_bytes(model)

    assert model.pump_lines_changed() > 0
    assert cursor not in shown_bytes(model), (
        "arrival did not move the drawn rows — the staleness this file exists "
        "to pin has silently stopped happening; the lag model is broken"
    )

    model.viewport.ensure_visible(cursor, MARGIN)
    assert cursor in shown_bytes(model)


def test_place_render_arrive_loop_converges() -> None:
    """The compose scroll loop reaches a fixed point with the cursor on screen."""
    model, cursor = compose_like_model()
    for _ in range(LINES + 10):
        moved = model.viewport.ensure_visible(cursor, MARGIN)
        seen = shown_bytes(model)
        arrived = model.pump_lines_changed()
        if not moved and not arrived:
            break
    else:
        raise AssertionError("no fixed point: placement and arrival kept chasing each other")
    assert cursor in seen
    # Quiescent means the cursor's neighbourhood is fully decorated; lines the
    # user never scrolled through legitimately keep their pending entries.
    assert all(d.line not in model._lines_seen for d in model.lazy_decorations)


def test_arrival_is_a_rebuild_not_a_repair() -> None:
    """Each arrival bumps the pipeline version, forcing a rebuild.

    This is what makes the repair path's decoration snapshot sound (a repair
    only ever sees a pure-text edit), and it is also compose's per-batch cost —
    the P4 concern. The model states the contract; the cost is P4's to fix.
    """
    model, cursor = compose_like_model()
    before = model.decorations.pipeline_version()
    model.viewport.ensure_visible(cursor, MARGIN)
    model.render()
    assert model.pump_lines_changed() > 0
    assert model.decorations.pipeline_version() != before
