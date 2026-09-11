//! `Toggle` — checkbox chip, chip-first (`[v] label`) or form layout
//! (`label: [v]` via `label_first`).

use fresh_core::api::WidgetSpec;

use super::WidgetImpl;

pub struct Toggle;

impl WidgetImpl for Toggle {
    fn on_key(
        &self,
        spec: &WidgetSpec,
        _widget_key: &str,
        _panel: &mut crate::widgets::WidgetPanelState,
        _viewport: super::Viewport,
        key: &str,
        fx: &mut super::KeyFx,
    ) -> super::KeyDisposition {
        if !matches!(key, "Enter" | "Space") {
            return super::KeyDisposition::Pass;
        }
        if let WidgetSpec::Toggle { checked, .. } = spec {
            fx.events
                .push(("toggle".into(), serde_json::json!({ "checked": !checked })));
        }
        super::KeyDisposition::Consumed
    }

    /// `WidgetAction::Activate` (a plugin mode binding's Enter) on a
    /// focused Toggle fires `toggle` with the flipped value — the
    /// spec's `checked` is the plugin's pushed truth, so the new
    /// value is computed here, never trusted from the caller.
    fn activate_event(&self, spec: &WidgetSpec) -> Option<(&'static str, serde_json::Value)> {
        if let WidgetSpec::Toggle { checked, .. } = spec {
            Some(("toggle", serde_json::json!({ "checked": !checked })))
        } else {
            None
        }
    }

    fn arrows_advance_focus(&self) -> bool {
        // No vertical axis of its own: panel arrows walk the controls
        // like Tab (button-only popups such as the dock context menu).
        true
    }

    fn box_meta(&self, spec: &WidgetSpec) -> super::BoxMeta {
        let mut m = super::BoxMeta::plain("toggle");
        if let WidgetSpec::Toggle { key: Some(k), .. } = spec {
            if !k.is_empty() {
                m.key = Some(k.clone());
                m.focusable = true;
            }
        }
        m
    }
}
