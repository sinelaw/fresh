"""Scaling laws, asserted with operation counts rather than wall clock.

Every claim made for this architecture is a claim about growth rate. These tests
state them as inequalities over `wrap_model.metrics` counters, so a change that
reintroduces an O(line) or O(scroll-depth) path fails here instead of showing up
in a profile months later.

The pathological input throughout is the one from the original report: a single
logical line with no newlines, edited and scrolled at various depths.
"""

from __future__ import annotations

import pytest

from wrap_model.editor import EditorModel
from wrap_model.metrics import measure
from wrap_model.wrap_machine import WrapRule

WIDTH = 20
SIZES = [100, 200, 400, 800]


def one_line(words: int) -> str:
    """A single logical line of `words` words — no newlines anywhere."""
    return " ".join(f"w{i}" for i in range(words))


def model_for(words: int, height: int = 10) -> EditorModel:
    model = EditorModel(one_line(words), rule=WrapRule.word(WIDTH), height=height)
    model.index.ensure_built()  # pay the one-time build outside the measurement
    return model


# -- keystrokes ---------------------------------------------------------------


@pytest.mark.parametrize("words", SIZES)
def test_keystroke_at_end_is_constant(words: int) -> None:
    """Appending to a 500 KB line must not rewrap the line."""
    model = model_for(words)
    with measure() as m:
        model.insert(len(model.buffer), "x")
    assert m.rows_emitted <= 3, m.snapshot()
    assert m.line_builds == 0, m.snapshot()


@pytest.mark.parametrize("words", SIZES)
def test_keystroke_at_start_is_constant(words: int) -> None:
    """Editing at the start resyncs instead of reflowing the whole line."""
    model = model_for(words)
    with measure() as m:
        model.insert(0, "x")
    assert m.resyncs == 1, m.snapshot()
    assert m.rows_emitted <= 4, m.snapshot()


def test_keystroke_cost_does_not_grow_with_line_length() -> None:
    """The headline law, stated directly: cost(n) is flat in n."""
    costs = []
    for words in SIZES:
        model = model_for(words)
        with measure() as m:
            model.insert(len(model.buffer), "x")
        costs.append(m.rows_emitted)
    assert max(costs) - min(costs) <= 1, dict(zip(SIZES, costs, strict=True))


def test_typing_a_word_stays_flat() -> None:
    """Sustained typing, not just one keystroke."""
    model = model_for(400)
    per_key = []
    for ch in "hello world":
        with measure() as m:
            model.insert(len(model.buffer), ch)
        per_key.append(m.rows_emitted)
    assert max(per_key) <= 3, per_key


# -- rendering ----------------------------------------------------------------


@pytest.mark.parametrize("words", SIZES)
def test_render_is_proportional_to_viewport_not_document(words: int) -> None:
    height = 10
    model = model_for(words, height=height)
    with measure() as m:
        model.render()
    assert m.rows_materialized <= height + 6, m.snapshot()


def test_render_cost_is_flat_in_scroll_depth() -> None:
    """The 59% of the profile: rows above the viewport are never built.

    Under the old two-coordinate viewport this count grew linearly with the
    scroll position, because `top_byte` was pinned to the logical line start.
    """
    model = model_for(800, height=10)
    total = model.index.total_rows()
    costs = []
    for row in (0, total // 4, total // 2, max(total - 10, 0)):
        model.viewport.set_top_row(row)
        with measure() as m:
            model.render()
        costs.append(m.rows_materialized)
    assert max(costs) - min(costs) <= 4, costs


def test_render_cost_is_flat_in_document_size() -> None:
    costs = []
    for words in SIZES:
        model = model_for(words, height=8)
        model.viewport.set_top_row(model.index.total_rows() // 2)
        with measure() as m:
            model.render()
        costs.append(m.rows_materialized)
    assert max(costs) - min(costs) <= 4, dict(zip(SIZES, costs, strict=True))


def test_render_read_is_flat_in_document_size() -> None:
    """Not just rows: the *read* is bounded too, so no whole-line slurp.

    The absolute byte count is uninteresting (it depends on the read budget's
    slack); what matters is that it does not grow with the document. A read
    proportional to the line — `LineIterator`'s `MAX_LINE_BYTES` behaviour, or
    starting from byte 0 — would show up here immediately.
    """
    sizes = [800, 1600, 3200]
    reads = []
    for words in sizes:
        model = model_for(words, height=10)
        model.viewport.set_top_row(model.index.total_rows() // 2)
        with measure() as m:
            model.render()
        reads.append(m.bytes_read)
    assert max(reads) - min(reads) <= 16, dict(zip(sizes, reads, strict=True))
    assert max(reads) < len(one_line(sizes[0])) // 2, reads


# -- scrolling ----------------------------------------------------------------


@pytest.mark.parametrize("words", SIZES)
def test_scrolling_is_arithmetic(words: int) -> None:
    """A wheel event does no wrapping and reads no text at all."""
    model = model_for(words)
    with measure() as m:
        model.viewport.scroll_by_rows(3)
    assert m.rows_emitted == 0, m.snapshot()
    assert m.tokens_fed == 0, m.snapshot()


def test_page_down_through_the_whole_document_is_linear_in_pages() -> None:
    """Paging costs the same per page wherever you are in the document."""
    model = model_for(800, height=10)
    per_page = []
    while model.viewport.top_row() < model.viewport.max_top_row():
        with measure() as m:
            model.viewport.page_down()
        per_page.append(m.rows_emitted + m.tokens_fed)
    assert set(per_page) == {0}, per_page


# -- scrollbar ----------------------------------------------------------------


@pytest.mark.parametrize("words", SIZES)
def test_scrollbar_is_free(words: int) -> None:
    """Exact row totals with no per-frame wrapping — the 16.9% path, gone."""
    model = model_for(words)
    with measure() as m:
        for _ in range(10):
            model.viewport.scrollbar()
    assert m.rows_emitted == 0, m.snapshot()
    assert m.width_measurements == 0, m.snapshot()


def test_scrollbar_after_an_edit_costs_only_the_repair() -> None:
    """The old index re-walked every line per keystroke; this one repairs one."""
    model = model_for(800)
    with measure() as m:
        model.insert(len(model.buffer), "x")
        model.viewport.scrollbar()
    assert m.index_builds == 0, m.snapshot()
    assert m.line_builds == 0, m.snapshot()


# -- cursor movement ----------------------------------------------------------


@pytest.mark.parametrize("words", SIZES)
def test_cursor_movement_costs_nothing_in_the_index(words: int) -> None:
    """Cursor moves cannot damage a canonical index, so they cost zero."""
    model = model_for(words)
    with measure() as m:
        for pos in range(0, len(model.buffer), 37):
            model.cursors = (pos,)
            model.index.total_rows()
    assert m.rows_emitted == 0, m.snapshot()
    assert m.line_builds == 0, m.snapshot()


def test_ensure_visible_does_not_build_rows() -> None:
    """Scroll decisions happen in row space, before anything is built.

    This is what collapses the old up-to-three `build_view_data` calls per frame.
    """
    model = model_for(400, height=10)
    with measure() as m:
        model.viewport.ensure_visible(len(model.buffer) - 1)
    assert m.rows_materialized == 0, m.snapshot()
    assert m.tokens_fed == 0, m.snapshot()


# -- the whole frame ----------------------------------------------------------


def test_a_full_keystroke_frame_is_bounded() -> None:
    """Edit → ensure cursor visible → render → scrollbar, end to end.

    The complete per-keystroke budget on a document 800 words long in one line,
    which is the shape that pinned a core.
    """
    model = model_for(800, height=10)
    model.viewport.set_top_row(model.index.total_rows() // 2)
    with measure() as m:
        model.insert(len(model.buffer), "x")
        model.viewport.ensure_visible(len(model.buffer))
        model.render()
        model.viewport.scrollbar()
    assert m.rows_materialized <= 20, m.snapshot()
    assert m.rows_emitted <= 40, m.snapshot()
    assert m.line_builds == 0, m.snapshot()


def test_frame_budget_does_not_grow_with_the_document() -> None:
    budgets = []
    for words in SIZES:
        model = model_for(words, height=10)
        model.viewport.set_top_row(model.index.total_rows() // 2)
        with measure() as m:
            model.insert(len(model.buffer), "x")
            model.viewport.ensure_visible(len(model.buffer))
            model.render()
            model.viewport.scrollbar()
        budgets.append(m.rows_materialized)
    assert max(budgets) - min(budgets) <= 4, dict(zip(SIZES, budgets, strict=True))
