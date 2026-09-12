//! `Popup` — a popup layer as a first-class tree node.
//!
//! The plugin-facing popup vocabulary (`docs/internal/retained-mode-ui.md`
//! §3.5). Two modes: panel-clipped (`screen_space: false`) renders
//! through the same promoted-overlay path `Overlay` uses;
//! `screen_space: true` projects the child's fully-rendered rows
//! through the generalized [`PanelPopup`] channel — the same channel
//! the Dropdown pop-over rides — so the box escapes the panel/modal
//! border and is painted (bordered, anchored, flipped and clamped on
//! screen) by the host, which knows nothing about the content.
//!
//! On the audited fate of the `overlays` row channel (phase 7's
//! "side-channel deletion"): it is NOT an event side channel anymore.
//! Which surface covers a press is the tree's decision (a popup is a
//! layer with `pointer_opaque` on its `box_meta`). What remains of
//! `overlays` is the PAINT wire for panel-clipped popup rows (this
//! node's non-screen-space mode, `Overlay`, the Text completion list),
//! the same standing as `entries` for base rows. Migrating the
//! completion popup to screen-space would be a deliberate visual change
//! (escaping panel clipping), not an architecture requirement — recorded
//! as optional follow-up, not debt.

use fresh_core::api::WidgetSpec;

use super::WidgetImpl;

pub struct Popup;

impl WidgetImpl for Popup {
    fn box_meta(&self, spec: &WidgetSpec) -> super::BoxMeta {
        let mut m = super::BoxMeta::plain("popup");
        // A popup is an opaque surface: a click inside it that nothing
        // consumes must not fall through to the rows beneath.
        m.pointer_opaque = true;
        if let WidgetSpec::Popup { key: Some(k), .. } = spec {
            if !k.is_empty() {
                m.key = Some(k.clone());
            }
        }
        m
    }
}
