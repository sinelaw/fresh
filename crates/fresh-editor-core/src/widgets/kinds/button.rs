//! `Button` — framed or bare action button with intent styling.

use fresh_core::api::WidgetSpec;

use super::WidgetImpl;

pub struct Button;

impl WidgetImpl for Button {
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
        // Disabled buttons don't fire activate. The renderer already
        // excludes them from the tab cycle; a focus key still pointing
        // at one is a stale-focus race — drop the key.
        if let WidgetSpec::Button { disabled, .. } = spec {
            if !disabled {
                fx.events.push(("activate".into(), serde_json::json!({})));
            }
        }
        super::KeyDisposition::Consumed
    }

    /// `WidgetAction::Activate` (a plugin mode binding's Enter) on a
    /// focused Button fires `activate` — unless disabled: the
    /// renderer excludes disabled buttons from the tab cycle, so a
    /// focus key still pointing at one is a stale-focus race and the
    /// event is dropped.
    fn activate_event(&self, spec: &WidgetSpec) -> Option<(&'static str, serde_json::Value)> {
        match spec {
            WidgetSpec::Button { disabled: true, .. } => None,
            WidgetSpec::Button { .. } => Some(("activate", serde_json::json!({}))),
            _ => None,
        }
    }

    fn arrows_advance_focus(&self) -> bool {
        // No vertical axis of its own: panel arrows walk the controls
        // like Tab (button-only popups such as the dock context menu).
        true
    }

    fn box_meta(&self, spec: &WidgetSpec) -> super::BoxMeta {
        let mut m = super::BoxMeta::plain("button");
        if let WidgetSpec::Button {
            key: Some(k),
            disabled,
            focusable,
            ..
        } = spec
        {
            if !k.is_empty() {
                m.key = Some(k.clone());
                m.focusable = !*disabled && *focusable;
            }
        }
        m
    }
}
