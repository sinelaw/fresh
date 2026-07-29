"""Combination matrix over every feature that can change row structure.

The property tests randomise *inputs*; this randomises *configuration*. Each
cell is a full editor with one combination of wrap rule, gutter, hanging indent,
view mode, cursor placement, and decoration set, and every cell is held to the
same invariants:

* the index's row count agrees with what the renderer actually draws;
* an anchored render equals the corresponding window of a full render;
* repair after an edit equals a rebuild;
* `row_of_byte` / `byte_of_row` round-trip;
* no row exceeds the rule's width;
* scrolling stays clamped to the document.

Decoration sets are the full powerset of the four that interact most (soft
breaks, conceals, inline hints, virtual lines), crossed with the rules and view
modes, plus dedicated cells for cursor-dependent activation and folds.
"""

from __future__ import annotations

import itertools

import pytest
from conftest import decorations_for

from wrap_model.decorations import Decorations
from wrap_model.editor import EditorModel
from wrap_model.wrap_index import WrapIndex
from wrap_model.wrap_machine import RuleKind, WrapRule

TEXT = (
    "alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu "
    "supercalifragilisticexpialidocious nu xi omicron pi rho sigma tau"
)
MULTILINE = "alpha beta gamma delta\n    indented continuation line here\nshort\n"

CORE_DECORATIONS = ["soft_break", "conceal", "inline_virtual", "virtual_line"]

RULES = {
    "wrap": WrapRule.word(24),
    "wrap+gutter": WrapRule.word(24, gutter=4),
    "wrap+indent": WrapRule.word(24, hanging_indent=True),
    "grid": WrapRule.grid(24),
    "nowrap": WrapRule.chop(24),
}

VIEW_MODES = ["source", "compose"]


def decoration_sets() -> list[frozenset[str]]:
    out: list[frozenset[str]] = []
    for r in range(len(CORE_DECORATIONS) + 1):
        out.extend(frozenset(c) for c in itertools.combinations(CORE_DECORATIONS, r))
    return out


def build(
    text: str,
    rule_name: str,
    kinds: frozenset[str],
    view_mode: str,
    height: int = 5,
) -> EditorModel:
    deco = decorations_for(text, kinds=kinds)
    return EditorModel(
        text,
        rule=RULES[rule_name],
        height=height,
        decorations=deco,
        view_mode=view_mode,
        line_wrap_enabled=rule_name != "nowrap",
    )


def full_rows(model: EditorModel) -> list[str]:
    tall = EditorModel(
        model.buffer.text(),
        rule=model.geometry.rule,
        height=10_000,
        decorations=model.decorations,
        view_mode=model.geometry.view_mode,
    )
    tall.cursors = model.cursors
    return [r.text for r in tall.render().rows]


MATRIX = [
    pytest.param(rule, kinds, mode, id=f"{rule}-{'+'.join(sorted(kinds)) or 'plain'}-{mode}")
    for rule, kinds, mode in itertools.product(RULES, decoration_sets(), VIEW_MODES)
]


@pytest.mark.parametrize(("rule_name", "kinds", "view_mode"), MATRIX)
def test_anchored_render_matches_full_render(
    rule_name: str, kinds: frozenset[str], view_mode: str
) -> None:
    """Every combination: a window equals the same slice of the whole."""
    model = build(TEXT, rule_name, kinds, view_mode)
    reference = full_rows(model)
    for top in (0, 1, 3, max(model.index.total_rows() - 2, 0)):
        model.viewport.set_top_row(top)
        actual_top = model.viewport.top_row()
        rows = [r.text for r in model.render().rows]
        assert rows == reference[actual_top : actual_top + model.viewport.height]


@pytest.mark.parametrize(("rule_name", "kinds", "view_mode"), MATRIX)
def test_repair_matches_rebuild(rule_name: str, kinds: frozenset[str], view_mode: str) -> None:
    """Every combination survives edits at the start, middle, and end."""
    model = build(TEXT, rule_name, kinds, view_mode)
    model.index.ensure_built()
    for pos in (len(model.buffer), len(model.buffer) // 2, 0):
        model.insert(pos, "X")
        fresh = WrapIndex(model.buffer, model.decorations, model.geometry, model.line_ending)
        fresh.ensure_built()
        assert [lw.row_starts for lw in model.index.lines] == [
            lw.row_starts for lw in fresh.lines
        ], f"row starts diverged after insert at {pos}"


@pytest.mark.parametrize(("rule_name", "kinds", "view_mode"), MATRIX)
def test_row_addressing_roundtrips(rule_name: str, kinds: frozenset[str], view_mode: str) -> None:
    model = build(TEXT, rule_name, kinds, view_mode)
    for row in range(model.index.total_rows()):
        addr = model.index.byte_of_row(row)
        if addr.is_virtual:
            continue
        back = model.index.row_of_byte(addr.byte)
        assert model.index.byte_of_row(back).byte == addr.byte


@pytest.mark.parametrize(("rule_name", "kinds", "view_mode"), MATRIX)
def test_scroll_stays_clamped(rule_name: str, kinds: frozenset[str], view_mode: str) -> None:
    model = build(TEXT, rule_name, kinds, view_mode)
    model.viewport.scroll_by_rows(10_000)
    assert 0 <= model.viewport.top_row() <= model.viewport.max_top_row()
    model.viewport.scroll_by_rows(-10_000)
    assert model.viewport.top_row() == 0


@pytest.mark.parametrize(("rule_name", "kinds", "view_mode"), MATRIX)
def test_no_row_exceeds_the_width(rule_name: str, kinds: frozenset[str], view_mode: str) -> None:
    rule = RULES[rule_name]
    if rule.kind is RuleKind.CHOP:
        return  # chops on characters, not columns
    model = build(TEXT, rule_name, kinds, view_mode, height=10_000)
    limit = rule.available_width if rule.kind is RuleKind.WORD else rule.width
    for row in model.render().rows:
        if row.is_virtual:
            continue
        assert row.visual_width <= limit, repr(row.text)


# -- viewport placement ------------------------------------------------------
#
# `ensure_visible` and `recenter` are the operations the Rust has three
# competing implementations of, and the disagreements between them were never
# about wrapping — they were about *where the viewport lands*. So these run the
# same placement contract over every configuration that changes row structure,
# because a decoration that shifts rows is exactly what made two passes that
# looked equivalent stop being equivalent.

PLACEMENT_MARGINS = [0, 1, 3]
PLACEMENT_HEIGHTS = [1, 2, 5, 8]


def placement_probe_bytes(model: EditorModel) -> list[int]:
    """A cursor byte on every row, plus the two ends of the buffer."""
    probes = {0, len(model.buffer)}
    for row in range(model.index.total_rows()):
        addr = model.index.byte_of_row(row)
        if not addr.is_virtual:
            probes.add(addr.byte)
    return sorted(probes)


@pytest.mark.parametrize("margin", PLACEMENT_MARGINS)
@pytest.mark.parametrize(("rule_name", "kinds", "view_mode"), MATRIX)
def test_ensure_visible_is_minimal_and_idempotent(
    rule_name: str, kinds: frozenset[str], view_mode: str, margin: int
) -> None:
    """The contract the Rust must satisfy, in every configuration.

    Three claims, and the middle one is the one that has actually broken:

    1. the cursor ends up visible;
    2. no top strictly nearer the old one satisfies the margin — an overshoot
       leaves the cursor outside the band it was moved into, and the *next*
       press then finds nothing to do (fresh#1574's stall);
    3. calling again moves nothing, so two passes both running it cannot
       compound into a double scroll.
    """
    for height in PLACEMENT_HEIGHTS:
        model = build(TEXT, rule_name, kinds, view_mode, height=height)
        vp = model.viewport
        for start_top in (0, 2, vp.max_top_row()):
            for byte in placement_probe_bytes(model):
                vp.set_top_row(start_top)
                old = vp.top_row()
                vp.ensure_visible(byte, margin)
                new = vp.top_row()

                assert vp.cursor_visible(byte)
                for candidate in range(min(old, new), max(old, new)):
                    nearer = candidate if new > old else candidate + 1
                    assert not vp.satisfies_margin(nearer, byte, margin), (
                        f"{rule_name}/{view_mode} h={height} m={margin}: top {nearer} "
                        f"already satisfied the margin; {old}->{new} overshot"
                    )

                assert not vp.ensure_visible(byte, margin)
                assert vp.top_row() == new


@pytest.mark.parametrize("margin", PLACEMENT_MARGINS)
@pytest.mark.parametrize(("rule_name", "kinds", "view_mode"), MATRIX)
def test_ensure_visible_misses_the_margin_only_when_unreachable(
    rule_name: str, kinds: frozenset[str], view_mode: str, margin: int
) -> None:
    """Margin unmet only when no top in range would have met it.

    Distinguishing "clamped at the document edge" from "scrolled too little"
    matters because they demand opposite responses, and a caller that cannot
    tell them apart adds a second scroll to fix the first — which is how the
    layout pass and the row pass ended up fighting.
    """
    for height in PLACEMENT_HEIGHTS:
        model = build(TEXT, rule_name, kinds, view_mode, height=height)
        vp = model.viewport
        for byte in placement_probe_bytes(model):
            vp.ensure_visible(byte, margin)
            if vp.satisfies_margin(vp.top_row(), byte, margin):
                continue
            assert not any(
                vp.satisfies_margin(t, byte, margin) for t in range(vp.max_top_row() + 1)
            ), f"{rule_name}/{view_mode} h={height} m={margin}: a reachable top did satisfy it"


@pytest.mark.parametrize(("rule_name", "kinds", "view_mode"), MATRIX)
def test_recenter_centres_or_clamps(rule_name: str, kinds: frozenset[str], view_mode: str) -> None:
    """Centred, or clamped to an end — and never anywhere else.

    Recenter is `ensure_visible` against a different target row. Giving it its
    own scroll path is how `Action::Recenter` drifted from ordinary scrolling in
    the first place, so the matrix pins them to the same arithmetic.
    """
    for height in PLACEMENT_HEIGHTS:
        model = build(TEXT, rule_name, kinds, view_mode, height=height)
        vp = model.viewport
        for byte in placement_probe_bytes(model):
            vp.recenter(byte)
            ideal = model.index.row_of_byte(byte) - (height - 1) // 2
            assert vp.top_row() == max(0, min(ideal, vp.max_top_row()))
            assert vp.cursor_visible(byte)


@pytest.mark.parametrize(("rule_name", "kinds", "view_mode"), MATRIX)
def test_recenter_survives_the_visibility_pass(
    rule_name: str, kinds: frozenset[str], view_mode: str
) -> None:
    """The frame's own pass must not undo what the user just asked for."""
    for height in (3, 5, 8):
        model = build(TEXT, rule_name, kinds, view_mode, height=height)
        vp = model.viewport
        for byte in placement_probe_bytes(model):
            vp.recenter(byte)
            centred = vp.top_row()
            assert not vp.ensure_visible(byte, margin=(height - 1) // 2)
            assert vp.top_row() == centred


@pytest.mark.parametrize(("rule_name", "kinds", "view_mode"), MATRIX)
def test_placement_reads_no_text(rule_name: str, kinds: frozenset[str], view_mode: str) -> None:
    """Deciding where to scroll builds no rows and reads no bytes.

    The property that collapses the frame's build-scroll-rebuild cycle: if
    placement needed materialised rows it would have to build them first, which
    is the O(scroll depth) cost the whole design exists to remove.
    """
    from wrap_model.metrics import measure

    model = build(TEXT, rule_name, kinds, view_mode, height=5)
    model.index.ensure_built()
    probes = placement_probe_bytes(model)
    with measure() as m:
        for byte in probes:
            model.viewport.ensure_visible(byte, margin=2)
            model.viewport.recenter(byte)
    assert m.rows_materialized == 0
    assert m.line_builds == 0


@pytest.mark.parametrize("margin", PLACEMENT_MARGINS)
@pytest.mark.parametrize("rule_name", ["wrap", "wrap+gutter", "grid", "nowrap"])
def test_placement_under_folds(rule_name: str, margin: int) -> None:
    """Placement holds with a fold collapsed, without a fold correction anywhere.

    The case the Rust currently refuses to handle — `compute_buffer_layout`
    steps aside entirely when any fold is collapsed, because the index counted
    rows that were not drawn. With folded lines occupying no rows, the same
    contract applies unchanged, which is the point of putting them in the
    coordinate system rather than correcting for them at each consumer.
    """
    for height in (1, 3, 6):
        model = build(MULTILINE, rule_name, frozenset({"fold"}), "source", height=height)
        vp = model.viewport
        assert model.index.total_rows() == len(full_rows(model))
        for byte in placement_probe_bytes(model):
            vp.ensure_visible(byte, margin)
            assert vp.cursor_visible(byte)
            assert not vp.ensure_visible(byte, margin)
            vp.recenter(byte)
            assert vp.cursor_visible(byte)


# -- multi-line documents ----------------------------------------------------


@pytest.mark.parametrize(("rule_name", "kinds"), list(itertools.product(RULES, decoration_sets())))
def test_multiline_index_matches_render(rule_name: str, kinds: frozenset[str]) -> None:
    """Index totals and drawn rows agree on a document with several lines."""
    model = build(MULTILINE, rule_name, kinds, "source", height=10_000)
    drawn = len(model.render().rows)
    assert model.index.total_rows() == drawn


# -- cursor-dependent activation ---------------------------------------------

CURSOR_KINDS = [
    frozenset({"conceal_cursor"}),
    frozenset({"soft_break_cursor"}),
    frozenset({"conceal_cursor", "soft_break_cursor"}),
    frozenset({"conceal_cursor", "conceal_replace", "inline_virtual"}),
]


@pytest.mark.parametrize("kinds", CURSOR_KINDS)
@pytest.mark.parametrize("rule_name", ["wrap", "wrap+indent"])
def test_cursor_activation_never_damages_the_index(rule_name: str, kinds: frozenset[str]) -> None:
    """Moving the cursor changes what is *drawn*, never the coordinate system."""
    model = build(TEXT, rule_name, kinds, "compose")
    model.index.ensure_built()
    snapshot = [lw.row_starts[:] for lw in model.index.lines]
    work = model.index.stats_rows_wrapped
    for pos in range(0, len(model.buffer), 7):
        model.cursors = (pos,)
        model.render()
        assert [lw.row_starts for lw in model.index.lines] == snapshot
    assert model.index.stats_rows_wrapped == work


@pytest.mark.parametrize("kinds", CURSOR_KINDS)
def test_cursor_inside_a_scope_reveals_it(kinds: frozenset[str]) -> None:
    """The rendered layout is cursor-aware even though the index is not."""
    model = build(TEXT, "wrap", kinds, "compose", height=10_000)
    model.cursors = ()
    blind = [r.text for r in model.render().rows]
    # conftest scopes sit at bytes 12-20 (soft break) and 24-30 (conceal); some
    # cursor inside one of them must change what is drawn.
    revealed = False
    for pos in range(10, 32):
        model.cursors = (pos,)
        if [r.text for r in model.render().rows] != blind:
            revealed = True
            break
    assert revealed


# -- folds -------------------------------------------------------------------


@pytest.mark.parametrize("rule_name", ["wrap", "wrap+gutter", "grid", "nowrap"])
def test_folded_bytes_are_never_drawn(rule_name: str) -> None:
    """A collapsed range contributes no cells to any row."""
    model = build(MULTILINE, rule_name, frozenset({"fold"}), "source", height=10_000)
    folded = model.decorations.fold_skip()
    assert folded
    for row in model.render().rows:
        for src in row.char_source_bytes:
            if src is None:
                continue
            assert not any(lo <= src < hi for lo, hi in folded)


def test_folded_lines_occupy_no_rows() -> None:
    """A collapsed line is not a visual row, so the index must not count it.

    This reverses an earlier stance — the index used to be deliberately
    fold-blind, on the grounds that folds are a drawing concern and keeping them
    out meant a fold toggle invalidated nothing. That is true but it buys the
    wrong thing: a coordinate that counts rows nobody can scroll to makes the
    scrollbar size itself wrong, the wheel move a different distance than it
    reports, and page-down land short. Every consumer then needs its own fold
    correction, which is exactly the family of fallbacks this work is removing.

    A toggle now bumps `fold_version` and rebuilds, which is cheap because
    folding is a deliberate, rare action — unlike typing, which is what the
    repair path actually exists for.
    """
    plain = build(MULTILINE, "wrap", frozenset(), "source")
    folded = build(MULTILINE, "wrap", frozenset({"fold"}), "source")
    assert folded.index.total_rows() < plain.index.total_rows()


@pytest.mark.parametrize("rule_name", ["wrap", "wrap+gutter", "grid", "nowrap"])
def test_index_row_count_matches_drawn_rows_under_folds(rule_name: str) -> None:
    """The index's total is what a full render actually draws.

    The property that lets placement, the scrollbar and the wheel all read the
    same number instead of each correcting it.
    """
    model = build(MULTILINE, rule_name, frozenset({"fold"}), "source", height=10_000)
    assert model.index.total_rows() == len(model.render().rows)


def test_fold_toggle_invalidates_the_index() -> None:
    """Adding a fold changes `pipeline_version`, so the next build is a rebuild.

    Without this the index keeps reporting rows for lines that stopped being
    drawn, and nothing detects it — the failure is silent and looks like a
    scrolling bug much later.
    """
    model = build(MULTILINE, "wrap", frozenset(), "source")
    model.index.ensure_built()
    before = model.index.total_rows()
    version_before = model.decorations.pipeline_version()

    from wrap_model.decorations import Fold

    start = model.buffer.line_start_offset(1)
    model.decorations.add_fold(Fold(start, model.buffer.line_end_offset(2)))
    assert model.decorations.pipeline_version() != version_before
    model.index.ensure_built()
    assert model.index.total_rows() < before


# -- geometry changes --------------------------------------------------------


@pytest.mark.parametrize("kinds", decoration_sets())
def test_geometry_change_rebuilds_consistently(kinds: frozenset[str]) -> None:
    """Resizing the pane drops the old index; the new one must be self-consistent."""
    model = build(TEXT, "wrap", kinds, "source", height=10_000)
    for width in (12, 40, 24):
        model.set_geometry(WrapRule.word(width))
        assert model.index.total_rows() == len(model.render().rows)


def test_empty_buffer_has_one_row() -> None:
    for rule_name in RULES:
        model = build("", rule_name, frozenset(), "source")
        assert model.index.total_rows() == 1
        assert len(model.render().rows) == 1


def test_decorations_on_an_empty_buffer_do_not_crash() -> None:
    model = EditorModel("", rule=WrapRule.word(20), decorations=Decorations())
    assert model.render().rows
