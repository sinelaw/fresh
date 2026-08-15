//! `HintBar` — the keyboard-hint footer row.

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;

use super::WidgetImpl;
use crate::widgets::registry::WidgetInstanceState;
use crate::widgets::render::{
    ensure_trailing_newline, render_hint_bar, CollectedOutput, RenderContext,
};

pub(crate) struct HintBar;

impl WidgetImpl for HintBar {
    fn box_meta(&self, _spec: &WidgetSpec) -> super::BoxMeta {
        super::BoxMeta::plain("hint_bar")
    }
    fn collect(
        &self,
        spec: &WidgetSpec,
        _prev: &HashMap<String, WidgetInstanceState>,
        _next_state: &mut HashMap<String, WidgetInstanceState>,
        _ctx: RenderContext<'_>,
        _panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::HintBar { entries, .. } = spec else {
            return CollectedOutput::default();
        };
        let mut out = CollectedOutput::default();
        let mut entry = render_hint_bar(entries);
        ensure_trailing_newline(&mut entry);
        out.entries.push(entry);
        // No hits — HintBar is read-only in v1. (When the
        // keymap layer arrives, individual entries become
        // clickable command targets.)
        out
    }
}
