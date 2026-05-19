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
    /// One string per visible terminal row, populated by
    /// `extract_with_rendered_rows`. Empty for the default
    /// `extract` (which uses the cheaper abstract render path).
    /// Use the `extract_with_rendered_rows` constructor when a
    /// test needs per-row text inspection (e.g. asserting that
    /// a specific glyph or content lands on a specific row).
    #[serde(default)]
    pub rendered_rows: Vec<String>,
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
        RenderSnapshot {
            width: api.terminal_width(),
            height: api.terminal_height(),
            viewport: ViewportSnapshot {
                top_byte: api.viewport_top_byte(),
                visible_byte_range: api.visible_byte_range(),
            },
            hardware_cursor: api.hardware_cursor_position(),
            gutter_width: api.gutter_width(),
            rendered_rows: Vec::new(),
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
        let _ = harness.render_real();
        let screen = harness.vt100_screen_to_string();
        let rendered_rows: Vec<String> =
            screen.split('\n').map(|s| s.to_string()).collect();
        let api = harness.api_mut();
        RenderSnapshot {
            width: api.terminal_width(),
            height: api.terminal_height(),
            viewport: ViewportSnapshot {
                top_byte: api.viewport_top_byte(),
                visible_byte_range: api.visible_byte_range(),
            },
            hardware_cursor: api.hardware_cursor_position(),
            gutter_width: api.gutter_width(),
            rendered_rows,
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
            }
        }
        None
    }
}
