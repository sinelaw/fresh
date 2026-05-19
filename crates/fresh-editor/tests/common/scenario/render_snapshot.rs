//! `RenderSnapshot` — typed, theme-free layout observable.
//!
//! Produced by extracting layout state from a live editor *after* a
//! single render pass settles the viewport. Asserted on by
//! [`super::layout_scenario::LayoutScenario`].
//!
//! Today's implementation pulls fields from `EditorTestApi` —
//! `viewport_top_byte`, `hardware_cursor_position`, `gutter_width`,
//! `visible_byte_range`. The doc's longer-term `RenderSnapshot`
//! includes per-row segments, decorations, popup placement; those
//! get added incrementally as layout scenarios demand them. Adding
//! a field here means adding the corresponding accessor on
//! `EditorTestApi`.

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::observable::Observable;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct RenderSnapshot {
    pub width: u16,
    pub height: u16,
    pub viewport: ViewportSnapshot,
    pub hardware_cursor: Option<(u16, u16)>,
    pub gutter_width: u16,
    /// Primary cursor byte offset into the buffer. Useful for
    /// "the cell under the hardware cursor matches the buffer
    /// char at this byte" parity checks (see
    /// `RenderSnapshotExpect::cursor_cell_matches_buffer_char`).
    #[serde(default)]
    pub cursor_byte: usize,
    /// Full buffer text. Populated by `extract_with_rendered_rows`
    /// because the cursor-parity matcher needs to read the byte
    /// under the cursor. Empty for the cheap default `extract`.
    #[serde(default)]
    pub buffer_text: String,
    /// Terminal-absolute hardware cursor position as the vt100
    /// backend observes it after `render_real()`. This is the
    /// cell coordinate the cursor would land on in the user's
    /// real terminal — distinct from `hardware_cursor` above,
    /// which is the editor's viewport-relative reading of the
    /// same cursor (used by overlay-obscuration checks). The
    /// `cursor_cell_matches_buffer_char` matcher indexes
    /// `rendered_rows` by this row, so the two coordinate
    /// systems agree. None when the cursor is hidden or the
    /// snapshot was built with the cheap `extract`.
    #[serde(default)]
    pub terminal_cursor: Option<(u16, u16)>,
    /// One string per visible terminal row, populated by
    /// `extract_with_rendered_rows`. Empty for the default
    /// `extract` (which uses the cheaper abstract render path).
    /// Use the `extract_with_rendered_rows` constructor when a
    /// test needs per-row text inspection (e.g. asserting that
    /// a specific glyph or content lands on a specific row).
    #[serde(default)]
    pub rendered_rows: Vec<String>,
    /// Active status-bar message text, if any. Populated by
    /// `extract` from `EditorTestApi::status_message`. Used by
    /// `RenderSnapshotExpect::status_message` to assert specific
    /// status messages after an action (e.g. theme-load errors).
    #[serde(default)]
    pub status_message: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct ViewportSnapshot {
    pub top_byte: usize,
    /// Byte range currently visible. None ⇒ unknown (extension not
    /// yet wired through `EditorTestApi`).
    #[serde(default)]
    pub visible_byte_range: Option<(usize, usize)>,
}

impl Observable for RenderSnapshot {
    fn extract(harness: &mut EditorTestHarness) -> Self {
        let _ = harness.render();
        let api = harness.api_mut();
        let cursor_byte = api.primary_caret().position;
        let status_message = api.status_message();
        RenderSnapshot {
            width: api.terminal_width(),
            height: api.terminal_height(),
            viewport: ViewportSnapshot {
                top_byte: api.viewport_top_byte(),
                visible_byte_range: api.visible_byte_range(),
            },
            hardware_cursor: api.hardware_cursor_position(),
            gutter_width: api.gutter_width(),
            cursor_byte,
            buffer_text: String::new(),
            terminal_cursor: None,
            rendered_rows: Vec::new(),
            status_message,
        }
    }
}

impl RenderSnapshot {
    /// Like `Observable::extract`, but runs the full
    /// CrosstermBackend → ANSI → vt100 pipeline (via
    /// `harness.render_real()`) and populates `rendered_rows`
    /// with the per-row text the terminal would actually
    /// display. Slower than the default `extract`; use only
    /// when per-row text assertions are needed.
    ///
    /// Resolves the long-standing framework gap tracked in
    /// #2058 (per-row screen-text inspection blocking
    /// ~50 e2e files). Tests in those clusters can now use
    /// `extract_with_rendered_rows` + `RenderSnapshotExpect`'s
    /// `row_contains` / `row_equals` matchers to assert on
    /// specific row content.
    pub fn extract_with_rendered_rows(harness: &mut EditorTestHarness) -> Self {
        // Run a sentinel-tracking render BEFORE the real-ANSI
        // render so we can detect whether the production
        // cursor-obscured-by-overlay check fired and suppressed
        // `Frame::set_cursor_position`.
        //
        // `render_observing_cursor` parks the backend cursor at
        // `(0, 0)` as a sentinel and runs `Terminal::draw`; if
        // the editor populated `Frame::cursor_position`, the
        // cursor moves and the call returns `Some((x, y))`. If
        // the editor left it `None` (because a popup covers the
        // cursor cell), the sentinel survives and the call
        // returns `None`. We project that through into
        // `hardware_cursor: None` and `terminal_cursor: None` so
        // scenarios observe the same "cursor was hidden" outcome
        // the real terminal would display.
        //
        // The vt100 hidden flag (`vt100_cursor_hidden`) is
        // unreliable for this signal because `render_real` writes
        // plain cell payloads without going through
        // `Terminal::draw`, so vt100 never sees the
        // cursor-visibility command. The sentinel trick captures
        // the real `Terminal::draw` decision.
        let observed_cursor = harness.render_observing_cursor().ok().flatten();
        let _ = harness.render_real();
        let screen = harness.vt100_screen_to_string();
        let rendered_rows: Vec<String> =
            screen.split('\n').map(|s| s.to_string()).collect();
<<<<<<< Updated upstream
        let terminal_cursor = observed_cursor;
=======
        // The production cursor-obscured-by-overlay check fires
        // inside `render_real`: when a popup covers the cell the
        // editor would otherwise call `Frame::set_cursor_position`
        // on, the call is omitted and ratatui ends the frame with
        // `hide_cursor`. vt100 reflects that as
        // `screen().hide_cursor() == true`. Project it through as
        // `hardware_cursor: None` so scenarios observe the same
        // "cursor was hidden" outcome the real terminal would
        // display — `EditorTestApi::hardware_cursor_position()`
        // alone returns the cursor's viewport-derived location
        // regardless of overlay state, which would silently fail
        // any cursor-under-popup assertion.
        let cursor_hidden_by_render = harness.vt100_cursor_hidden();
        // Terminal-absolute cursor position from the TestBackend
        // (what ratatui's `Terminal::draw` left the backend at).
        // Distinct from `hardware_cursor_position` on `EditorTestApi`,
        // which is the editor's viewport-relative reading. Used by
        // `cursor_cell_matches_buffer_char` so the cursor row indexes
        // into `rendered_rows` correctly.
        let terminal_cursor_raw = Some(harness.screen_cursor_position());
>>>>>>> Stashed changes
        let api = harness.api_mut();
        let cursor_byte = api.primary_caret().position;
        let buffer_text = api.buffer_text();
        let raw_cursor = api.hardware_cursor_position();
        // hardware_cursor mirrors the editor's viewport-relative
        // coordinates (matchers like `hardware_cursor_row_in`
        // compare against these), but is set to `None` when the
        // renderer hid the cursor — the obscured-by-overlay path
        // the cursor-under-popup tests guard against.
        let hardware_cursor = if observed_cursor.is_some() {
            raw_cursor
<<<<<<< Updated upstream
        } else {
            None
        };
        let status_message = api.status_message();
=======
        };
        // `terminal_cursor` is always populated when the snapshot is
        // built with `extract_with_rendered_rows`; the vt100 hidden
        // flag is unreliable here because `render_real()` doesn't
        // emit cursor-show/-hide commands. Tests that care about the
        // "cursor hidden by overlay" outcome should consult
        // `hardware_cursor` instead, which IS gated on the vt100
        // hidden flag.
        let terminal_cursor = terminal_cursor_raw;
>>>>>>> Stashed changes
        RenderSnapshot {
            width: api.terminal_width(),
            height: api.terminal_height(),
            viewport: ViewportSnapshot {
                top_byte: api.viewport_top_byte(),
                visible_byte_range: api.visible_byte_range(),
            },
            hardware_cursor,
            gutter_width: api.gutter_width(),
            cursor_byte,
            buffer_text,
            terminal_cursor,
            rendered_rows,
            status_message,
        }
    }
}

/// Per-row text matcher. Used by `RenderSnapshotExpect.row_checks`.
///
/// The matcher is intentionally permissive on trailing
/// whitespace (vt100 pads rows to the terminal width with
/// spaces); `Contains` and `Equals` both compare against the
/// row's trimmed text.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RowMatch {
    /// Row at index `row` must contain the given substring
    /// (after trimming trailing spaces).
    Contains { row: u16, substring: String },
    /// Row at index `row` must equal the given text (after
    /// trimming trailing spaces).
    Equals { row: u16, text: String },
    /// Some row anywhere in the snapshot must contain the
    /// given substring (for tests that don't pin the exact
    /// row index, e.g. "the file's first line is somewhere
    /// on screen").
    AnyRowContains(String),
    /// No row may contain the given substring. Useful for
    /// regressions like "after Ctrl+End the empty final line
    /// must be visible, NOT obscured by Entry 140's content".
    NoRowContains(String),
    /// At least one row must contain at least one of the listed
    /// substrings. Disjunctive variant of `AnyRowContains` — used
    /// when a test's load-bearing claim is "some later content is
    /// visible" without pinning the exact line index (e.g. e2e
    /// scroll tests that allow `Line 80 | 90 | 95 | 100 | modified
    /// content`).
    AnyRowContainsAny(Vec<String>),
    /// Pick the Nth row (0-indexed) of those that contain the
    /// gutter separator `│`, take the content area after the
    /// FIRST `│`, count leading spaces. The count must satisfy
    /// the supplied bounds. Used by hanging-wrap-indent tests to
    /// assert that continuation rows inherit (or do not inherit)
    /// a hanging indent matching the source's leading whitespace.
    ///
    /// `min` / `max` are inclusive bounds; `None` means no
    /// bound on that side.
    ContentRowLeadingSpaces {
        nth_content_row: usize,
        min: Option<usize>,
        max: Option<usize>,
    },
    /// For every row that contains the gutter separator `│`, if
    /// the gutter area (text BEFORE the last `│`) has no ASCII
    /// digit (i.e. it's a wrapped-continuation row, no line
    /// number is shown), the trimmed content after the last `│`
    /// must contain at least `min` characters. If `skip_last`
    /// is true the final continuation row is excluded from the
    /// check (the remainder after the last wrap can be
    /// arbitrarily short).
    ///
    /// Drives the issue #1502 ("word wrap squished") regression
    /// guard: with a 10-space hanging indent and a 35-col
    /// terminal, every full continuation must hold >= 10 chars,
    /// not the buggy ~7.
    ContinuationRowsMinContentWidth {
        min: usize,
        skip_last: bool,
    },
    /// For every row that contains the gutter separator `│`, the
    /// gutter area (text BEFORE the last `│`) must have no ASCII
    /// digit. Used in anti-tests to assert that NO continuation
    /// rows exist at all (i.e. with `line_wrap=false`, only
    /// numbered rows are produced).
    ///
    /// Equivalently: no row may be a wrapped-continuation row.
    NoContinuationRows,
}

/// Hanging-indent check for popup line wrapping. Find the row
/// containing `anchor_substring` (the original indented source
/// line) and find a different row containing `continuation_substring`
/// (the wrapped continuation). Inside the popup border (split on
/// '│'), the continuation must start with at least
/// `min_leading_spaces` of horizontal whitespace — that's the
/// hanging-indent property the renderer guarantees so wrapped
/// signature-help descriptions stay grouped with their parameter.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PopupHangingIndent {
    pub anchor_substring: String,
    pub continuation_substring: String,
    pub min_leading_spaces: usize,
}

/// Partial expectation: only fields set on the expectation are
/// asserted. Unspecified fields wildcard-match the editor.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RenderSnapshotExpect {
    #[serde(default)]
    pub viewport_top_byte: Option<usize>,
    #[serde(default)]
    pub hardware_cursor: Option<(u16, u16)>,
    /// Cursor row must be in this inclusive range. Used when the
    /// exact row depends on layout details a test doesn't want to
    /// pin (e.g. "after Recenter the cursor lands somewhere in
    /// the middle band of the viewport"). Compared against
    /// `actual.hardware_cursor.map(|(_, row)| row)`.
    #[serde(default)]
    pub hardware_cursor_row_in: Option<(u16, u16)>,
    /// Cursor column must be in this inclusive range. Used for
    /// "cursor at column 0 on a tab-prefixed line lands at the
    /// start of content (~gutter width), NOT 7 columns further
    /// into the first expanded tab" — the load-bearing claim of
    /// `scroll_clearing.rs::test_cursor_before_first_tab`.
    /// Compared against `actual.hardware_cursor.map(|(col, _)| col)`.
    #[serde(default)]
    pub hardware_cursor_col_in: Option<(u16, u16)>,
    #[serde(default)]
    pub gutter_width: Option<u16>,
    #[serde(default)]
    pub visible_byte_range: Option<(usize, usize)>,
    /// The cursor's logical byte position (the snapshot's
    /// `viewport.top_byte`-anchored window via `visible_byte_range`)
    /// must include this byte. Used for "after Ctrl+End the doc
    /// end is visible" style claims.
    #[serde(default)]
    pub viewport_includes_byte: Option<usize>,
    /// `viewport_top_byte` must be within `delta` bytes of `byte`.
    /// Used when the exact top depends on wrap geometry but a
    /// bound exists ("after Ctrl+End on a long buffer, top is
    /// within max_visible_bytes of doc_end").
    #[serde(default)]
    pub viewport_top_within_delta_of: Option<(usize, usize)>,
    /// `viewport_top_byte` must be strictly greater than this
    /// value. Used for "viewport scrolled past the start".
    #[serde(default)]
    pub viewport_top_byte_greater_than: Option<usize>,
    /// Per-row text matchers. Each entry is checked against the
    /// snapshot's `rendered_rows`. Empty list = no row checks.
    /// Requires the snapshot to have been built with
    /// `RenderSnapshot::extract_with_rendered_rows`; against a
    /// snapshot from the cheaper default `extract` (where
    /// `rendered_rows` is empty), any non-empty row matcher
    /// will fail with "rendered_rows not populated".
    #[serde(default)]
    pub row_checks: Vec<RowMatch>,
    /// Cursor-on-screen parity: assert that the printable char in
    /// the rendered cell under the hardware cursor matches the
    /// buffer byte at `cursor_byte`. Whitespace bytes / newline /
    /// EOL positions are skipped (the original e2e contract: the
    /// check only fires on printable, non-whitespace chars where
    /// the pre-refactor wrap drift was visible). Requires a
    /// snapshot built with `extract_with_rendered_rows`.
    #[serde(default)]
    pub cursor_cell_matches_buffer_char: bool,
    /// Popup wrap hanging-indent check. See [`PopupHangingIndent`].
    /// Requires a snapshot built with `extract_with_rendered_rows`.
    #[serde(default)]
    pub popup_hanging_indent: Option<PopupHangingIndent>,
    /// The hardware cursor must EITHER be hidden (`None`) OR sit
    /// outside the rectangle `[x, x+w) × [y, y+h)`. Used by the
    /// cursor-under-popup regression: when a popup covers the
    /// cell where the cursor would be drawn, the renderer must
    /// omit `Frame::set_cursor_position` so the terminal's
    /// hardware cursor does not show through the popup.
    #[serde(default)]
    pub hardware_cursor_hidden_or_outside_rect: Option<HardwareCursorRect>,
    /// The hardware cursor must be visible at the given `(col, row)`.
    /// Same as `hardware_cursor`, but explicit — used in anti-tests
    /// that pin the cursor stays put when the load-bearing step is
    /// dropped.
    #[serde(default)]
    pub hardware_cursor_at: Option<(u16, u16)>,
    /// Active status-bar message text. Some(s) ⇒ assert the snapshot's
    /// `status_message` field exactly equals `s`. Populated by
    /// snapshots built with `extract_with_rendered_rows`.
    #[serde(default)]
    pub status_message: Option<String>,
}

/// Rectangle used by `hardware_cursor_hidden_or_outside_rect`.
/// Inclusive on `x` / `y`, exclusive on the far edge — the cursor
/// is "inside" iff `x <= cx < x+w AND y <= cy < y+h`.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct HardwareCursorRect {
    pub x: u16,
    pub y: u16,
    pub w: u16,
    pub h: u16,
}

impl RenderSnapshotExpect {
    /// Returns `Some((field, expected, actual))` on the first
    /// mismatch.
    pub fn check_against(&self, actual: &RenderSnapshot) -> Option<(&'static str, String, String)> {
        if let Some(want) = self.viewport_top_byte {
            if want != actual.viewport.top_byte {
                return Some((
                    "viewport_top_byte",
                    want.to_string(),
                    actual.viewport.top_byte.to_string(),
                ));
            }
        }
        if let Some(want) = self.hardware_cursor {
            if Some(want) != actual.hardware_cursor {
                return Some((
                    "hardware_cursor",
                    format!("{want:?}"),
                    format!("{:?}", actual.hardware_cursor),
                ));
            }
        }
        if let Some((lo, hi)) = self.hardware_cursor_col_in {
            match actual.hardware_cursor {
                None => {
                    return Some((
                        "hardware_cursor_col_in",
                        format!("[{lo},{hi}]"),
                        "None".into(),
                    ));
                }
                Some((col, _)) if col < lo || col > hi => {
                    return Some((
                        "hardware_cursor_col_in",
                        format!("[{lo},{hi}]"),
                        format!("col {col}"),
                    ));
                }
                Some(_) => {}
            }
        }
        if let Some((lo, hi)) = self.hardware_cursor_row_in {
            match actual.hardware_cursor {
                None => {
                    return Some((
                        "hardware_cursor_row_in",
                        format!("[{lo},{hi}]"),
                        "None".into(),
                    ));
                }
                Some((_, row)) if row < lo || row > hi => {
                    return Some((
                        "hardware_cursor_row_in",
                        format!("[{lo},{hi}]"),
                        format!("row {row}"),
                    ));
                }
                Some(_) => {}
            }
        }
        if let Some(byte) = self.viewport_includes_byte {
            match actual.viewport.visible_byte_range {
                Some((lo, hi)) if byte < lo || byte > hi => {
                    return Some((
                        "viewport_includes_byte",
                        format!("byte {byte}"),
                        format!("visible {lo}..={hi}"),
                    ));
                }
                None => {
                    return Some((
                        "viewport_includes_byte",
                        format!("byte {byte}"),
                        "visible_byte_range = None".into(),
                    ));
                }
                Some(_) => {}
            }
        }
        if let Some((byte, delta)) = self.viewport_top_within_delta_of {
            let top = actual.viewport.top_byte;
            let gap = if top > byte { top - byte } else { byte - top };
            if gap > delta {
                return Some((
                    "viewport_top_within_delta_of",
                    format!("top within {delta} of {byte}"),
                    format!("top={top}, gap={gap}"),
                ));
            }
        }
        if let Some(min) = self.viewport_top_byte_greater_than {
            if actual.viewport.top_byte <= min {
                return Some((
                    "viewport_top_byte_greater_than",
                    format!("> {min}"),
                    actual.viewport.top_byte.to_string(),
                ));
            }
        }
        if let Some(want) = self.gutter_width {
            if want != actual.gutter_width {
                return Some((
                    "gutter_width",
                    want.to_string(),
                    actual.gutter_width.to_string(),
                ));
            }
        }
        if let Some(want) = self.visible_byte_range {
            if Some(want) != actual.viewport.visible_byte_range {
                return Some((
                    "visible_byte_range",
                    format!("{want:?}"),
                    format!("{:?}", actual.viewport.visible_byte_range),
                ));
            }
        }
        if !self.row_checks.is_empty() && actual.rendered_rows.is_empty() {
            return Some((
                "rendered_rows",
                format!("{} row check(s)", self.row_checks.len()),
                "empty (snapshot built with extract, not extract_with_rendered_rows)".into(),
            ));
        }
        for check in &self.row_checks {
            match check {
                RowMatch::Contains { row, substring } => {
                    let idx = *row as usize;
                    let actual_row = actual.rendered_rows.get(idx).map(|s| s.trim_end());
                    if actual_row.is_none_or(|r| !r.contains(substring.as_str())) {
                        return Some((
                            "rendered_rows[Contains]",
                            format!("row {row} contains {substring:?}"),
                            format!("row {row} = {actual_row:?}"),
                        ));
                    }
                }
                RowMatch::Equals { row, text } => {
                    let idx = *row as usize;
                    let actual_row = actual.rendered_rows.get(idx).map(|s| s.trim_end());
                    if actual_row != Some(text.as_str()) {
                        return Some((
                            "rendered_rows[Equals]",
                            format!("row {row} equals {text:?}"),
                            format!("row {row} = {actual_row:?}"),
                        ));
                    }
                }
                RowMatch::AnyRowContains(substring) => {
                    if !actual
                        .rendered_rows
                        .iter()
                        .any(|r| r.trim_end().contains(substring.as_str()))
                    {
                        return Some((
                            "rendered_rows[AnyRowContains]",
                            format!("some row contains {substring:?}"),
                            format!("none of {} rows contained it", actual.rendered_rows.len()),
                        ));
                    }
                }
                RowMatch::NoRowContains(substring) => {
                    if let Some((i, r)) = actual
                        .rendered_rows
                        .iter()
                        .enumerate()
                        .find(|(_, r)| r.trim_end().contains(substring.as_str()))
                    {
                        return Some((
                            "rendered_rows[NoRowContains]",
                            format!("no row contains {substring:?}"),
                            format!("row {i} contains it: {:?}", r.trim_end()),
                        ));
                    }
                }
                RowMatch::AnyRowContainsAny(substrings) => {
                    let found = actual.rendered_rows.iter().any(|r| {
                        let t = r.trim_end();
                        substrings.iter().any(|s| t.contains(s.as_str()))
                    });
                    if !found {
                        return Some((
                            "rendered_rows[AnyRowContainsAny]",
                            format!("some row contains any of {substrings:?}"),
                            format!(
                                "none of {} rows contained any",
                                actual.rendered_rows.len()
                            ),
                        ));
                    }
                }
                RowMatch::ContentRowLeadingSpaces {
                    nth_content_row,
                    min,
                    max,
                } => {
                    // Collect rows that have the gutter separator
                    // '│'. Take the content area after the FIRST '│'
                    // and count leading spaces. The Nth such row's
                    // leading-space count must be within [min, max].
                    let content_rows: Vec<&str> = actual
                        .rendered_rows
                        .iter()
                        .filter(|r| r.contains('│'))
                        .map(|s| s.as_str())
                        .collect();
                    let Some(line) = content_rows.get(*nth_content_row) else {
                        return Some((
                            "rendered_rows[ContentRowLeadingSpaces]",
                            format!("content row index {nth_content_row} exists"),
                            format!("only {} content rows in snapshot", content_rows.len()),
                        ));
                    };
                    let bar = line
                        .find('│')
                        .expect("content row was filtered to contain '│'");
                    let after = &line[bar + '│'.len_utf8()..];
                    let leading = after.chars().take_while(|c| *c == ' ').count();
                    if let Some(lo) = min {
                        if leading < *lo {
                            return Some((
                                "rendered_rows[ContentRowLeadingSpaces.min]",
                                format!(
                                    "content row {nth_content_row} leading spaces >= {lo}"
                                ),
                                format!("got {leading} leading spaces; content={after:?}"),
                            ));
                        }
                    }
                    if let Some(hi) = max {
                        if leading > *hi {
                            return Some((
                                "rendered_rows[ContentRowLeadingSpaces.max]",
                                format!(
                                    "content row {nth_content_row} leading spaces <= {hi}"
                                ),
                                format!("got {leading} leading spaces; content={after:?}"),
                            ));
                        }
                    }
                }
                RowMatch::ContinuationRowsMinContentWidth { min, skip_last } => {
                    // Build the list of (row_index, trimmed_content_width)
                    // for every continuation row in the snapshot — a row
                    // is a continuation iff it has '│' AND its gutter
                    // (text before the last '│') has no ASCII digit.
                    let mut widths: Vec<(usize, usize, String)> = Vec::new();
                    for (i, r) in actual.rendered_rows.iter().enumerate() {
                        let Some(bar_byte) = r.rfind('│') else {
                            continue;
                        };
                        let gutter_area = &r[..bar_byte];
                        if gutter_area.chars().any(|c| c.is_ascii_digit()) {
                            continue;
                        }
                        let content = &r[bar_byte + '│'.len_utf8()..];
                        let trimmed = content.trim();
                        if trimmed.is_empty() {
                            continue;
                        }
                        widths.push((
                            i,
                            trimmed.chars().count(),
                            trimmed.to_string(),
                        ));
                    }
                    if widths.is_empty() {
                        return Some((
                            "rendered_rows[ContinuationRowsMinContentWidth]",
                            format!("at least one continuation row with width >= {min}"),
                            "no continuation rows found".into(),
                        ));
                    }
                    let check_up_to = if *skip_last {
                        widths.len().saturating_sub(1)
                    } else {
                        widths.len()
                    };
                    for (idx, (row_i, width, sample)) in
                        widths[..check_up_to].iter().enumerate()
                    {
                        if *width < *min {
                            return Some((
                                "rendered_rows[ContinuationRowsMinContentWidth]",
                                format!(
                                    "every continuation row width >= {min} (skip_last={skip_last})"
                                ),
                                format!(
                                    "continuation #{idx} (snapshot row {row_i}) width={width}; sample={sample:?}"
                                ),
                            ));
                        }
                    }
                }
                RowMatch::NoContinuationRows => {
                    for (i, r) in actual.rendered_rows.iter().enumerate() {
                        let Some(bar_byte) = r.rfind('│') else {
                            continue;
                        };
                        let gutter_area = &r[..bar_byte];
                        if gutter_area.chars().any(|c| c.is_ascii_digit()) {
                            continue;
                        }
                        let content = &r[bar_byte + '│'.len_utf8()..];
                        if content.trim().is_empty() {
                            continue;
                        }
                        return Some((
                            "rendered_rows[NoContinuationRows]",
                            "no continuation rows".into(),
                            format!(
                                "row {i} is a continuation: {:?}",
                                content.trim()
                            ),
                        ));
                    }
                }
            }
        }
        if self.cursor_cell_matches_buffer_char {
            if actual.rendered_rows.is_empty() {
                return Some((
                    "cursor_cell_matches_buffer_char",
                    "snapshot built with extract_with_rendered_rows".into(),
                    "rendered_rows empty (built with cheap extract)".into(),
                ));
            }
            let expected_byte = actual.buffer_text.as_bytes().get(actual.cursor_byte).copied();
            let expected_char = expected_byte.map(|b| b as char);
            // Only enforce on printable, non-whitespace chars — the
            // original e2e parity contract: at EOL / on whitespace,
            // the renderer may legitimately paint a blank cell.
            if let Some(exp) = expected_char {
                if !exp.is_ascii_whitespace() && exp != '\n' {
                    // Use terminal-absolute cursor coords so the row
                    // index aligns with `rendered_rows` (which is
                    // indexed from the top of the terminal, not the
                    // top of the viewport).
                    match actual.terminal_cursor {
                        None => {
                            return Some((
                                "cursor_cell_matches_buffer_char",
                                format!("cell == {exp:?}"),
                                "terminal_cursor = None".into(),
                            ));
                        }
                        Some((hw_col, hw_row)) => {
                            let row_text = actual
                                .rendered_rows
                                .get(hw_row as usize)
                                .map(|s| s.as_str())
                                .unwrap_or("");
                            let row_chars: Vec<char> = row_text.chars().collect();
                            let at_cursor = row_chars.get(hw_col as usize).copied();
                            if at_cursor != Some(exp) {
                                return Some((
                                    "cursor_cell_matches_buffer_char",
                                    format!(
                                        "cell({hw_col},{hw_row}) == {exp:?} (buffer byte {})",
                                        actual.cursor_byte
                                    ),
                                    format!("cell == {at_cursor:?}; row = {row_text:?}"),
                                ));
                            }
                        }
                    }
                }
            }
        }
        if let Some(check) = &self.popup_hanging_indent {
            if actual.rendered_rows.is_empty() {
                return Some((
                    "popup_hanging_indent",
                    "snapshot built with extract_with_rendered_rows".into(),
                    "rendered_rows empty (built with cheap extract)".into(),
                ));
            }
            let anchor_row = actual
                .rendered_rows
                .iter()
                .find(|r| r.contains(&check.anchor_substring));
            if anchor_row.is_none() {
                return Some((
                    "popup_hanging_indent",
                    format!("row containing {:?}", check.anchor_substring),
                    "no row matched".into(),
                ));
            }
            let continuation_row = actual.rendered_rows.iter().find(|r| {
                r.contains(&check.continuation_substring)
                    && !r.contains(&check.anchor_substring)
            });
            let Some(cont) = continuation_row else {
                return Some((
                    "popup_hanging_indent",
                    format!(
                        "wrapped continuation row containing {:?} but not {:?}",
                        check.continuation_substring, check.anchor_substring
                    ),
                    "no continuation row matched".into(),
                ));
            };
            // Split on the popup border '│' to isolate cells inside
            // the popup. The continuation's leading spaces inside
            // the border are the hanging indent.
            let Some(inside) = cont.split('│').nth(1) else {
                return Some((
                    "popup_hanging_indent",
                    "continuation row inside popup border ('│')".into(),
                    format!("no border split: {cont:?}"),
                ));
            };
            let leading = inside.chars().take_while(|c| *c == ' ').count();
            if leading < check.min_leading_spaces {
                return Some((
                    "popup_hanging_indent",
                    format!(
                        "continuation has >= {} leading spaces (anchor={:?})",
                        check.min_leading_spaces, check.anchor_substring
                    ),
                    format!(
                        "got {leading} leading spaces; continuation = {cont:?}"
                    ),
                ));
            }
        }
        if let Some(rect) = &self.hardware_cursor_hidden_or_outside_rect {
            if let Some((cx, cy)) = actual.hardware_cursor {
                let inside = cx >= rect.x
                    && cx < rect.x + rect.w
                    && cy >= rect.y
                    && cy < rect.y + rect.h;
                if inside {
                    return Some((
                        "hardware_cursor_hidden_or_outside_rect",
                        format!(
                            "cursor hidden OR outside [{},{})×[{},{})",
                            rect.x,
                            rect.x + rect.w,
                            rect.y,
                            rect.y + rect.h
                        ),
                        format!("cursor at ({cx},{cy}) is inside the rect"),
                    ));
                }
            }
        }
        if let Some(want) = self.hardware_cursor_at {
            if Some(want) != actual.hardware_cursor {
                return Some((
                    "hardware_cursor_at",
                    format!("{want:?}"),
                    format!("{:?}", actual.hardware_cursor),
                ));
            }
        }
        if let Some(want) = &self.status_message {
            if actual.status_message.as_deref() != Some(want.as_str()) {
                return Some((
                    "status_message",
                    format!("{want:?}"),
                    format!("{:?}", actual.status_message),
                ));
            }
        }
        None
    }
}
