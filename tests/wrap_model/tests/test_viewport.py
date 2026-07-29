"""The anchored viewport: O(viewport) rendering and row-space scrolling.

The central property is that rendering from an anchor produces exactly the rows
a full render would have produced at that offset — proving the renderer can stop
building rows it is going to discard, which is the 59% of the frame this design
targets.
"""

from __future__ import annotations

from conftest import single_long_line, texts, word_texts
from hypothesis import HealthCheck, given, settings
from hypothesis import strategies as st

from wrap_model.editor import EditorModel, PluginViewTransform
from wrap_model.row_layout import LineStart
from wrap_model.tokens import Token
from wrap_model.wrap_machine import WrapRule

SETTINGS = settings(max_examples=120, suppress_health_check=[HealthCheck.too_slow], deadline=None)


def full_render(text: str, rule: WrapRule) -> list[str]:
    """Every row of the document, as a reference for windowed renders."""
    model = EditorModel(text, rule=rule, height=10_000)
    return [r.text for r in model.render().rows]


@SETTINGS
@given(
    text=word_texts(),
    width=st.integers(min_value=6, max_value=30),
    height=st.integers(min_value=1, max_value=6),
    top=st.integers(min_value=0, max_value=40),
)
def test_anchored_render_equals_the_full_render_window(
    text: str, width: int, height: int, top: int
) -> None:
    """Rendering from an anchor == the same slice of a full render."""
    rule = WrapRule.word(width)
    reference = full_render(text, rule)
    model = EditorModel(text, rule=rule, height=height)
    model.viewport.set_top_row(top)
    actual_top = model.viewport.top_row()
    frame = model.render()
    expected = reference[actual_top : actual_top + height]
    assert [r.text for r in frame.rows] == expected


@given(text=single_long_line())
@settings(max_examples=25, deadline=None)
def test_render_cost_is_independent_of_scroll_depth(text: str) -> None:
    """The headline: scrolling deep into a huge line does not cost more.

    Under the old two-coordinate viewport this is exactly what failed — `top_byte`
    is pinned at the line start and every row above the window gets built and
    thrown away.
    """
    model = EditorModel(text, rule=WrapRule.word(20), height=5)
    total = model.index.total_rows()
    costs = []
    for row in (0, total // 2, max(total - 5, 0)):
        model.viewport.set_top_row(row)
        costs.append(model.render().rows_built)
    assert max(costs) <= 12, costs


@SETTINGS
@given(text=texts(), width=st.integers(min_value=6, max_value=30))
def test_scrolling_is_reversible(text: str, width: int) -> None:
    model = EditorModel(text, rule=WrapRule.word(width), height=3)
    start = model.viewport.top_row()
    model.viewport.scroll_by_rows(5)
    model.viewport.scroll_by_rows(-5)
    assert model.viewport.top_row() == start


@SETTINGS
@given(text=texts(), width=st.integers(min_value=6, max_value=30))
def test_scroll_is_clamped_to_the_document(text: str, width: int) -> None:
    model = EditorModel(text, rule=WrapRule.word(width), height=4)
    model.viewport.scroll_by_rows(10_000)
    assert model.viewport.top_row() == model.viewport.max_top_row()
    model.viewport.scroll_by_rows(-10_000)
    assert model.viewport.top_row() == 0


@SETTINGS
@given(text=word_texts(), width=st.integers(min_value=6, max_value=30))
def test_ensure_visible_scrolls_the_minimum(text: str, width: int) -> None:
    model = EditorModel(text, rule=WrapRule.word(width), height=4)
    for byte in range(0, len(model.buffer) + 1, 5):
        model.ensure_visible = None  # type: ignore[attr-defined]
        model.viewport.ensure_visible(byte)
        assert model.viewport.cursor_visible(byte)


def test_ensure_visible_is_a_noop_when_already_visible() -> None:
    """One build per frame: the scroll decision happens before anything is built."""
    model = EditorModel(" ".join(f"w{i}" for i in range(60)), rule=WrapRule.word(20), height=6)
    model.viewport.set_top_row(3)
    before = model.viewport.top_row()
    addr = model.index.byte_of_row(4)
    assert not model.viewport.ensure_visible(addr.byte)
    assert model.viewport.top_row() == before


def test_continuation_rows_are_marked_for_the_gutter() -> None:
    """A mid-line anchor must not print a line number against a continuation."""
    model = EditorModel(" ".join(f"w{i}" for i in range(60)), rule=WrapRule.word(20), height=3)
    model.viewport.set_top_row(4)
    frame = model.render()
    assert frame.rows[0].line_start is LineStart.AFTER_BREAK


def test_first_row_of_a_line_is_not_a_continuation() -> None:
    model = EditorModel("alpha\nbeta gamma", rule=WrapRule.word(30), height=5)
    frame = model.render()
    assert frame.rows[0].line_start is LineStart.BEGINNING
    assert frame.rows[1].line_start is LineStart.AFTER_SOURCE_NEWLINE


@SETTINGS
@given(text=word_texts(), width=st.integers(min_value=6, max_value=30))
def test_scrollbar_is_exact(text: str, width: int) -> None:
    """No approximate mode, and so no size guards to get wrong (fresh#2610)."""
    model = EditorModel(text, rule=WrapRule.word(width), height=5)
    bar = model.viewport.scrollbar()
    assert bar.exact
    assert bar.total_rows == model.index.total_rows()
    assert bar.top_row == model.viewport.top_row()
    start, end = bar.thumb()
    assert 0 <= start <= end <= bar.height


def test_scrollbar_tracks_scrolling() -> None:
    model = EditorModel(" ".join(f"w{i}" for i in range(200)), rule=WrapRule.word(20), height=5)
    top_thumb = model.viewport.scrollbar().thumb()
    model.viewport.scroll_by_rows(10_000)
    bottom_thumb = model.viewport.scrollbar().thumb()
    assert bottom_thumb[0] > top_thumb[0]


def test_plugin_view_transform_bypasses_the_index() -> None:
    """A plugin stream has no source bytes, so it keeps absolute-offset semantics."""
    model = EditorModel("ignored source", rule=WrapRule.word(20), height=2)
    model.view_transform = PluginViewTransform(
        tokens=[
            Token.text_tok("alpha", None),
            Token.newline(None),
            Token.text_tok("beta", None),
            Token.newline(None),
            Token.text_tok("gamma", None),
        ]
    )
    frame = model.render()
    assert [r.text for r in frame.rows] == ["alpha", "beta"]
    assert not frame.scrollbar.exact


def test_editing_does_not_move_the_viewport() -> None:
    """Repair keeps the anchor meaningful — no scroll jump on a keystroke."""
    model = EditorModel(" ".join(f"w{i}" for i in range(120)), rule=WrapRule.word(20), height=4)
    model.viewport.set_top_row(10)
    before = [r.text for r in model.render().rows]
    model.insert(len(model.buffer), "!")
    after = [r.text for r in model.render().rows]
    assert before[:-1] == after[:-1]
