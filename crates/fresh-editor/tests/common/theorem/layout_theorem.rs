//! Class B theorems: layout-dependent observables.
//!
//! Layout state (viewport scroll, hardware cursor screen position,
//! scrollbar geometry) is reconciled by the render pipeline, not by
//! action dispatch alone — see `Viewport::ensure_visible_in_layout`.
//! `LayoutTheorem` runs a single render pass at the end of the action
//! sequence so layout state settles before assertion. Tests still
//! avoid `for { send_key; render; }` style imperative transcripts.
//!
//! Phase 3 surface is intentionally narrow: just `viewport_top_byte`.
//! Richer layout observables (cursor screen position, gutter widths,
//! scrollbar thumb extent) belong in a future `RenderSnapshot` per
//! §9.1 of the migration doc, and should land alongside the first
//! theorem that demonstrably needs them.

use crate::common::harness::EditorTestHarness;
use crate::common::theorem::failure::TheoremFailure;
use fresh::test_api::{Action, EditorTestApi};

pub struct LayoutTheorem {
    pub description: &'static str,
    pub initial_text: &'static str,
    pub width: u16,
    pub height: u16,
    pub actions: Vec<Action>,
    pub expected_top_byte: usize,
}

pub fn check_layout_theorem(t: LayoutTheorem) -> Result<(), TheoremFailure> {
    let mut harness = EditorTestHarness::with_temp_project(t.width, t.height)
        .expect("EditorTestHarness::with_temp_project failed");
    let _fixture = harness
        .load_buffer_from_text(t.initial_text)
        .expect("load_buffer_from_text failed");

    // Render once after load so the initial viewport reconciles to the
    // buffer geometry — without this, the editor's first layout pass
    // hasn't computed view lines yet and `top_byte` reads 0 even when
    // ensure_visible would otherwise scroll.
    harness.render().expect("initial render failed");

    {
        let api: &mut dyn EditorTestApi = harness.api_mut();
        api.dispatch_seq(&t.actions);
    }

    // Single layout pass *after* the full action sequence. This is the
    // only structural difference from `BufferTheorem`.
    harness.render().expect("final render failed");

    let actual = harness.api_mut().viewport_top_byte();
    if actual != t.expected_top_byte {
        return Err(TheoremFailure::ViewportTopByteMismatch {
            description: t.description.to_string(),
            expected: t.expected_top_byte,
            actual,
        });
    }
    Ok(())
}

pub fn assert_layout_theorem(t: LayoutTheorem) {
    if let Err(f) = check_layout_theorem(t) {
        panic!("{f}");
    }
}
