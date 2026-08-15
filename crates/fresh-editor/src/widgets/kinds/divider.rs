//! `Divider` — a host-width horizontal rule.

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;
use fresh_core::text_property::TextPropertyEntry;

use super::WidgetImpl;
use crate::widgets::registry::WidgetInstanceState;
use crate::widgets::render::{ensure_trailing_newline, CollectedOutput, RenderContext};

pub(crate) struct Divider;

impl WidgetImpl for Divider {
    fn collect(
        &self,
        spec: &WidgetSpec,
        _prev: &HashMap<String, WidgetInstanceState>,
        _next_state: &mut HashMap<String, WidgetInstanceState>,
        _ctx: RenderContext<'_>,
        panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::Divider { ch, style, .. } = spec else {
            return CollectedOutput::default();
        };
        let mut out = CollectedOutput::default();
        // Draw the rule at the host's authoritative inner width, so it
        // always spans the panel exactly — no plugin-side width guess.
        // One column per glyph (the default `─` is a single cell); an
        // empty `ch` falls back to a space so a stray empty divider
        // still occupies its row instead of collapsing.
        let glyph = if ch.is_empty() { " " } else { ch.as_str() };
        let cols = (panel_width as usize).min(4096);
        let mut text = String::with_capacity(cols * glyph.len() + 1);
        for _ in 0..cols {
            text.push_str(glyph);
        }
        let mut entry = TextPropertyEntry {
            text,
            properties: Default::default(),
            style: style.clone(),
            inline_overlays: Vec::new(),
            segments: Vec::new(),
            pad_to_chars: None,
            truncate_to_chars: None,
        };
        ensure_trailing_newline(&mut entry);
        out.entries.push(entry);
        out
    }
}
