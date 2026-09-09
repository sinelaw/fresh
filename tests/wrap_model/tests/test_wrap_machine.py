"""The single wrap rule, and the property that makes everything else possible.

`RowCarry` completeness — resuming at a row boundary reproduces the rest of the
run exactly — is what licenses both mid-line rendering and incremental repair.
If this file fails, the architecture does not hold.
"""

from __future__ import annotations

import pytest
from conftest import rules, texts, word_texts
from hypothesis import HealthCheck, given, settings
from hypothesis import strategies as st

from wrap_model.base_tokens import LineEnding, tokenize_text
from wrap_model.row_layout import rows_from_tokens
from wrap_model.tokens import Kind, Token
from wrap_model.wrap_index import tokens_from
from wrap_model.wrap_machine import RuleKind, WrapMachine, WrapRule

SETTINGS = settings(max_examples=200, suppress_health_check=[HealthCheck.too_slow], deadline=None)


def toks(text: str) -> list[Token]:
    return tokenize_text(text, 0, LineEnding.LF)


@SETTINGS
@given(text=texts(), rule=rules())
def test_resume_at_any_boundary_reproduces_the_run(text: str, rule: WrapRule) -> None:
    """The property the whole design rests on.

    For every row boundary, a machine resumed there with that row's carry
    produces exactly the boundaries the full run produced from there on.

    Resume is defined over the *source* stream, not over already-wrapped output:
    wrapping is not idempotent (re-feeding an emitted `Break` to a token that
    overflows a fresh row would break twice). Both callers that resume — the
    renderer starting at an anchor and `WrapIndex._repair_line` — slice the
    source stream, which is what this mirrors.
    """
    source = toks(text)
    full = WrapMachine.run(source, rule)
    if len(full.rows) < 2:
        return
    for i, row in enumerate(full.rows):
        if row.source_byte is None:
            continue  # all-injected row: no source byte to resume from
        resumed = WrapMachine.run(tokens_from(source, row.source_byte), rule, row.carry)
        assert [r.source_byte for r in resumed.rows] == [r.source_byte for r in full.rows[i:]], (
            f"resume at row {i} diverged"
        )


@SETTINGS
@given(text=texts(), rule=rules())
def test_no_row_exceeds_the_rule_width(text: str, rule: WrapRule) -> None:
    """Post-condition of every rule: rows never overflow their width.

    Exceptions are structural, not accidental: a degenerate pane emits no breaks
    at all, and a single grapheme wider than the pane must still be drawn.
    """
    if rule.degenerate:
        return
    out = WrapMachine.run(toks(text), rule)
    limit = rule.available_width if rule.kind is RuleKind.WORD else rule.width
    if rule.kind is RuleKind.CHOP:
        return  # CHOP counts characters, not columns
    for row in rows_from_tokens(out.tokens, tabs=rule.tabs):
        if row.visual_width > limit:
            # Only legal when one indivisible cluster is itself too wide.
            assert len(row.char_source_bytes) <= 1 or limit < 4


@SETTINGS
@given(text=texts(), rule=rules())
def test_tokens_are_preserved(text: str, rule: WrapRule) -> None:
    """Wrapping splices `Break`s and indents; it never loses or reorders source."""
    src = toks(text)
    out = WrapMachine.run(src, rule)

    def source_text(tokens: list[Token]) -> str:
        parts = []
        for t in tokens:
            if t.source_offset is None:
                continue
            if t.kind is Kind.TEXT:
                parts.append(t.text)
            elif t.kind is Kind.SPACE:
                parts.append(" ")
            elif t.kind is Kind.NEWLINE:
                parts.append("\n")
            elif t.kind is Kind.BINARY_BYTE:
                parts.append(chr(t.byte))
        return "".join(parts)

    assert source_text(out.tokens) == source_text(src)


@SETTINGS
@given(text=texts(), rule=rules())
def test_row_count_matches_materialized_rows(text: str, rule: WrapRule) -> None:
    """The index's row count and the renderer's row list cannot disagree.

    Both come from the same run — this test pins that they stay that way, and
    replaces the pair of hand-written agreement tests the Rust code needs today
    (`wrap_str_to_width_matches_apply_wrapping_transform`,
    `grid_layout_count_and_segments_agree`).
    """
    out = WrapMachine.run(toks(text), rule)
    rendered = rows_from_tokens(out.tokens, tabs=rule.tabs)
    assert len(out.rows) == len(rendered)


@SETTINGS
@given(text=word_texts(), width=st.integers(min_value=8, max_value=30))
def test_source_bytes_are_monotonic(text: str, width: int) -> None:
    """Row starts advance monotonically — `byte_of_row` depends on it."""
    out = WrapMachine.run(toks(text), WrapRule.word(width))
    seen = [r.source_byte for r in out.rows if r.source_byte is not None]
    assert seen == sorted(seen)


def test_space_overflow_backs_up_over_the_word() -> None:
    """Issue #1363: a continuation row starts with content, not a stranded space."""
    out = WrapMachine.run(toks("aaaa bbbb cccc"), WrapRule.word(12))
    rows = rows_from_tokens(out.tokens)
    assert len(rows) >= 2
    assert not rows[1].text.startswith(" ")


def test_unbreakable_word_wider_than_pane_still_progresses() -> None:
    """A word wider than the pane is char-split rather than looping forever."""
    out = WrapMachine.run(toks("x" * 50), WrapRule.word(10))
    rows = rows_from_tokens(out.tokens)
    assert len(rows) >= 5
    assert "".join(r.text for r in rows) == "x" * 50


def test_hanging_indent_repeats_on_continuations() -> None:
    out = WrapMachine.run(
        toks("    alpha beta gamma delta epsilon"), WrapRule.word(20, hanging_indent=True)
    )
    rows = rows_from_tokens(out.tokens)
    assert len(rows) >= 2
    assert rows[1].text.startswith("    ")


def test_hanging_indent_dropped_when_it_would_starve_the_row() -> None:
    """The clamp that guarantees the char-split loop terminates."""
    out = WrapMachine.run(
        toks(" " * 15 + "alpha beta gamma"), WrapRule.word(20, hanging_indent=True)
    )
    rows = rows_from_tokens(out.tokens)
    assert len(rows) >= 2
    assert not rows[1].text.startswith(" " * 15)


def test_grid_breaks_at_exact_columns() -> None:
    """fresh#2649: the terminal rule breaks mid-word, at the column, always."""
    out = WrapMachine.run(toks("abcdefghij"), WrapRule.grid(4))
    rows = rows_from_tokens(out.tokens)
    assert [r.text for r in rows] == ["abcd", "efgh", "ij"]


def test_grid_treats_ansi_as_zero_width() -> None:
    out = WrapMachine.run([Token.text_tok("\x1b[31mabcd\x1b[0mefgh", 0)], WrapRule.grid(4))
    rows = rows_from_tokens(out.tokens, ansi_aware=True)
    widths = [r.visual_width for r in rows]
    assert all(w <= 4 for w in widths)


def test_chop_breaks_every_n_characters() -> None:
    """Wrap-off safety chop — `MAX_SAFE_LINE_WIDTH` as a rule, not an inline hack."""
    out = WrapMachine.run(toks("y" * 25), WrapRule.chop(10))
    rows = rows_from_tokens(out.tokens)
    assert [len(r.text) for r in rows] == [10, 10, 5]


def test_degenerate_width_emits_no_breaks() -> None:
    """A 1-column pane must not produce one break per character."""
    rule = WrapRule.word(1)
    assert rule.degenerate
    out = WrapMachine.run(toks("hello world"), rule)
    assert not any(t.kind is Kind.BREAK for t in out.tokens)


@pytest.mark.parametrize("width", [4, 7, 13, 20])
def test_double_width_glyphs_never_straddle_a_boundary(width: int) -> None:
    out = WrapMachine.run(toks("日本語テキスト" * 3), WrapRule.word(width))
    for row in rows_from_tokens(out.tokens):
        assert row.visual_width <= max(width, 2)
