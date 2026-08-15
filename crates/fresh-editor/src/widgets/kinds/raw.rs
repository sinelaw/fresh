//! `Raw` — the pre-rendered text-property escape hatch.

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;

use super::WidgetImpl;
use crate::widgets::registry::WidgetInstanceState;
use crate::widgets::render::{ensure_trailing_newline, CollectedOutput, RenderContext};

pub(crate) struct Raw;

impl WidgetImpl for Raw {
    fn box_meta(&self, _spec: &WidgetSpec) -> super::BoxMeta {
        super::BoxMeta::plain("raw")
    }
    fn collect(
        &self,
        spec: &WidgetSpec,
        _prev: &HashMap<String, WidgetInstanceState>,
        _next_state: &mut HashMap<String, WidgetInstanceState>,
        _ctx: RenderContext<'_>,
        _panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::Raw { entries, .. } = spec else {
            return CollectedOutput::default();
        };
        let mut out = CollectedOutput::default();
        // Raw is the migration escape hatch: the plugin's own
        // bytes flow through unchanged. The plugin still owns
        // mouse clicks within Raw regions (via the existing
        // `mouse_click` hook); the widget runtime intentionally
        // emits no hit areas here. We *do* ensure each Raw
        // entry ends with a newline so it occupies its own
        // buffer line — plugins that already include `\n` are
        // unaffected.
        for raw_entry in entries {
            let mut e = raw_entry.clone();
            e.normalize_widths();
            ensure_trailing_newline(&mut e);
            out.entries.push(e);
        }
        out
    }
}
