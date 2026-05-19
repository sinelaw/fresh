//! `LayoutShadow` — alternate, hand-written wrap algorithm.
//!
//! Produces a [`RenderSnapshot`] from `(initial_text, width, height,
//! actions)` using a naive line-by-line wrap. Differential against
//! the live editor's snapshot catches wrap-table regressions on
//! double-width chars, virtual lines, and other surprise inputs the
//! editor's actual layout handles.
//!
//! Today's shadow only computes `viewport.top_byte` (always 0 — no
//! scroll math), `gutter_width` (line-count-driven), and leaves
//! `hardware_cursor` and `visible_byte_range` as None. Expanding the
//! shadow to model wrap math + scroll math is straightforward but
//! left to the LayoutScenario expansion phase that needs it.

use crate::common::scenario::render_snapshot::{RenderSnapshot, ViewportSnapshot};

pub struct LayoutShadow;

impl LayoutShadow {
    /// Compute a snapshot for a buffer of `text` displayed at
    /// `width × height` with no scroll. The action sequence is
    /// ignored; layout shadows model the *post-action* viewport,
    /// which today is just the initial geometry.
    pub fn snapshot(text: &str, width: u16, height: u16) -> RenderSnapshot {
        let line_count = if text.is_empty() {
            1
        } else {
            text.bytes().filter(|&b| b == b'\n').count() + if text.ends_with('\n') { 0 } else { 1 }
        };
        // Mirrors `Viewport::gutter_width` in
        // `src/view/viewport.rs`: `1 + max(digits, 2) + 3` —
        // 1-cell left padding, line-number digits (minimum 2), and
        // 3 cells of trailing separator/marker.
        let mut digits = 1u16;
        let mut n = line_count;
        while n >= 10 {
            digits += 1;
            n /= 10;
        }
        let gutter_width = 1 + digits.max(2) + 3;
        RenderSnapshot {
            width,
            height,
            viewport: ViewportSnapshot {
                top_byte: 0,
                visible_byte_range: None,
            },
            hardware_cursor: None,
            terminal_cursor: None,
            gutter_width,
            cursor_byte: 0,
            buffer_text: String::new(),
            rendered_rows: Vec::new(),
            status_message: None,
        }
    }
}
