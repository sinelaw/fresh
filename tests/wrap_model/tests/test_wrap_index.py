"""The coordinate service: repair correctness and repair *cost*.

Two claims are under test, and the second matters as much as the first:

1. `repair(edit) ≡ rebuild()` — an incrementally repaired index is
   indistinguishable from one built from scratch.
2. repair touches O(affected rows), not O(line). This is the entire performance
   argument, so the model counts rewrapped rows and asserts on the count.
"""

from __future__ import annotations

from conftest import decorations_for, edits, single_long_line, texts, word_texts
from hypothesis import HealthCheck, given, settings
from hypothesis import strategies as st

from wrap_model.base_tokens import LineEnding
from wrap_model.buffer import TextBuffer
from wrap_model.decorations import Decorations, SoftBreak
from wrap_model.editor import EditorModel
from wrap_model.wrap_index import Fenwick, WrapGeometry, WrapIndex
from wrap_model.wrap_machine import WrapRule

SETTINGS = settings(max_examples=150, suppress_health_check=[HealthCheck.too_slow], deadline=None)


def rebuilt_like(model: EditorModel) -> WrapIndex:
    fresh = WrapIndex(model.buffer, model.decorations, model.geometry, model.line_ending)
    fresh.ensure_built()
    return fresh


def same_structure(a: WrapIndex, b: WrapIndex) -> bool:
    return [lw.row_starts for lw in a.lines] == [lw.row_starts for lw in b.lines] and [
        lw.carries for lw in a.lines
    ] == [lw.carries for lw in b.lines]


# -- correctness -------------------------------------------------------------


@SETTINGS
@given(text=texts(), data=st.data(), width=st.integers(min_value=4, max_value=24))
def test_repair_equals_rebuild(text: str, data: st.DataObject, width: int) -> None:
    """The merge gate for incremental repair."""
    model = EditorModel(text, rule=WrapRule.word(width))
    model.index.ensure_built()
    for _ in range(3):
        start, removed, inserted = data.draw(edits(model.buffer.text()))
        model.edit(start, removed, inserted)
        assert same_structure(model.index, rebuilt_like(model))


@SETTINGS
@given(text=word_texts(), data=st.data())
def test_repair_equals_rebuild_with_decorations(text: str, data: st.DataObject) -> None:
    """Repair holds with soft breaks, conceals, and inline hints in play."""
    deco = decorations_for(text, kinds=frozenset({"soft_break", "conceal", "inline_virtual"}))
    model = EditorModel(text, rule=WrapRule.word(16), decorations=deco)
    model.index.ensure_built()
    for _ in range(2):
        start, removed, inserted = data.draw(edits(model.buffer.text()))
        model.edit(start, removed, inserted)
        assert same_structure(model.index, rebuilt_like(model))


@SETTINGS
@given(text=texts(), width=st.integers(min_value=4, max_value=24))
def test_row_byte_roundtrip(text: str, width: int) -> None:
    """`row_of_byte(byte_of_row(r)) == r` for every row that owns a byte."""
    model = EditorModel(text, rule=WrapRule.word(width))
    total = model.index.total_rows()
    for row in range(total):
        addr = model.index.byte_of_row(row)
        if addr.is_virtual:
            continue
        back = model.index.row_of_byte(addr.byte)
        assert back <= row
        assert model.index.byte_of_row(back).byte == addr.byte


@SETTINGS
@given(text=texts(), width=st.integers(min_value=4, max_value=24))
def test_row_starts_are_monotonic_and_begin_at_zero(text: str, width: int) -> None:
    model = EditorModel(text, rule=WrapRule.word(width))
    model.index.ensure_built()
    for lw in model.index.lines:
        assert lw.row_starts[0] == 0
        assert lw.row_starts == sorted(lw.row_starts)


@SETTINGS
@given(text=texts(), width=st.integers(min_value=4, max_value=24))
def test_total_rows_matches_sum_of_lines(text: str, width: int) -> None:
    model = EditorModel(text, rule=WrapRule.word(width))
    model.index.ensure_built()
    assert model.index.total_rows() == max(sum(lw.total_rows for lw in model.index.lines), 1)


def test_newline_insertion_splits_a_line() -> None:
    """Structural edits re-tree without disturbing untouched lines."""
    model = EditorModel("alpha beta gamma\ndelta", rule=WrapRule.word(30))
    model.index.ensure_built()
    assert len(model.index.lines) == 2
    model.insert(5, "\n")
    assert len(model.index.lines) == 3
    assert same_structure(model.index, rebuilt_like(model))


def test_newline_deletion_merges_lines() -> None:
    model = EditorModel("alpha\nbeta\ngamma", rule=WrapRule.word(30))
    model.index.ensure_built()
    assert len(model.index.lines) == 3
    model.delete(5, 6)
    assert len(model.index.lines) == 2
    assert same_structure(model.index, rebuilt_like(model))


def test_plugin_version_bump_damages_everything() -> None:
    """A decoration change is not a byte edit — it cannot be repaired locally."""
    model = EditorModel("alpha beta gamma delta", rule=WrapRule.word(12))
    before = model.index.total_rows()
    model.decorations.add_soft_break(SoftBreak(position=6, indent=0))
    after = model.index.total_rows()
    assert after > before
    assert same_structure(model.index, rebuilt_like(model))


def test_cursor_movement_never_damages_the_index() -> None:
    """The index is canonical: cursors do not participate in it at all."""
    model = EditorModel("alpha beta gamma delta epsilon", rule=WrapRule.word(12))
    model.index.ensure_built()
    snapshot = [lw.row_starts[:] for lw in model.index.lines]
    work_before = model.index.stats_rows_wrapped
    for pos in range(0, len(model.buffer), 3):
        model.cursors = (pos,)
        model.index.total_rows()
    assert [lw.row_starts for lw in model.index.lines] == snapshot
    assert model.index.stats_rows_wrapped == work_before


# -- cost --------------------------------------------------------------------


@given(n=st.integers(min_value=60, max_value=140))
@settings(max_examples=20, deadline=None)
def test_typing_at_end_of_a_huge_line_rewraps_one_row(n: int) -> None:
    """The headline claim, as a test rather than an argument.

    Appending to a single enormous line must not re-wrap the line.
    """
    text = " ".join(f"w{i}" for i in range(n))
    model = EditorModel(text, rule=WrapRule.word(20))
    total = model.index.total_rows()
    assert total > 10
    before = model.index.stats_rows_wrapped
    model.insert(len(model.buffer), "X")
    assert model.index.stats_rows_wrapped - before <= 2


@given(text=single_long_line())
@settings(max_examples=25, deadline=None)
def test_typing_at_start_of_a_huge_line_resyncs_quickly(text: str) -> None:
    """Editing at the *start* still costs a couple of rows, thanks to resync.

    Without resync this would be O(line): every row boundary after the edit
    shifts, and a naive implementation recomputes them all.
    """
    model = EditorModel(text, rule=WrapRule.word(20))
    total = model.index.total_rows()
    before = model.index.stats_rows_wrapped
    model.insert(0, "Z")
    rewrapped = model.index.stats_rows_wrapped - before
    assert rewrapped < total / 2, f"rewrapped {rewrapped} of {total} rows"
    assert same_structure(model.index, rebuilt_like(model))


def test_repair_cost_is_independent_of_line_length() -> None:
    """Doubling the line must not double the cost of one keystroke."""
    costs = []
    for n in (100, 200, 400):
        text = " ".join(f"w{i}" for i in range(n))
        model = EditorModel(text, rule=WrapRule.word(20))
        model.index.ensure_built()
        before = model.index.stats_rows_wrapped
        model.insert(len(model.buffer), "q")
        costs.append(model.index.stats_rows_wrapped - before)
    assert max(costs) <= 2, costs


# -- Fenwick -----------------------------------------------------------------


@given(values=st.lists(st.integers(min_value=1, max_value=9), min_size=1, max_size=40))
@settings(max_examples=100, deadline=None)
def test_fenwick_prefix_and_find(values: list[int]) -> None:
    tree = Fenwick(values)
    running = 0
    for i, v in enumerate(values):
        assert tree.prefix(i) == running
        running += v
    assert tree.total() == sum(values)
    for row in range(sum(values)):
        line = tree.find(row)
        assert tree.prefix(line) <= row < tree.prefix(line) + values[line]


def test_fenwick_point_update() -> None:
    tree = Fenwick([1, 2, 3])
    tree.set(1, 2, 7)
    assert tree.total() == 11
    assert tree.prefix(2) == 8


def test_index_is_lazy() -> None:
    """Nothing is wrapped until something asks a question."""
    buf = TextBuffer("alpha\nbeta")
    index = WrapIndex(buf, Decorations(), WrapGeometry(WrapRule.word(10)), LineEnding.LF)
    assert index.stats_rows_wrapped == 0
    index.total_rows()
    assert index.stats_rows_wrapped > 0
