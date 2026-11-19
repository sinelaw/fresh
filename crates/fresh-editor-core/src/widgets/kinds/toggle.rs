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
        key: &crate::keys::KeySeq,
        fx: &mut super::KeyFx,
    ) -> super::KeyDisposition {
        if !key.single().is_some_and(super::activates) {
            return super::KeyDisposition::Pass;
        }
        if let WidgetSpec::Toggle {
            checked, disabled, ..
        } = spec
        {
            if *disabled {
                return super::KeyDisposition::Pass;
            }
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
        if let WidgetSpec::Toggle {
            checked,
            disabled: false,
            ..
        } = spec
        {
            Some(("toggle", serde_json::json!({ "checked": !checked })))
        } else {
            None
        }
    }

    fn box_meta(&self, spec: &WidgetSpec) -> super::BoxMeta {
        let mut m = super::BoxMeta::plain("toggle");
        if let WidgetSpec::Toggle {
            key: Some(k),
            disabled: false,
            ..
        } = spec
        {
            if !k.is_empty() {
                m.key = Some(k.clone());
                m.focusable = true;
            }
        }
        m
    }
}
