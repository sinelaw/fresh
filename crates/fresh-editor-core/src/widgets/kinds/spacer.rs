//! `Spacer` — fixed-width padding (flex sizing is a `Row` concern).

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;
use fresh_core::text_property::TextPropertyEntry;

use super::WidgetImpl;
use crate::widgets::registry::WidgetInstanceState;
use crate::widgets::render::{ensure_trailing_newline, CollectedOutput, RenderContext};

pub struct Spacer;

impl WidgetImpl for Spacer {
    fn box_meta(&self, _spec: &WidgetSpec) -> super::BoxMeta {
        super::BoxMeta::plain("spacer")
    }
    fn collect(
        &self,
        spec: &WidgetSpec,
        _prev: &HashMap<String, WidgetInstanceState>,
        _next_state: &mut HashMap<String, WidgetInstanceState>,
        _ctx: RenderContext<'_>,
        _panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::Spacer { cols, .. } = spec else {
            return CollectedOutput::default();
        };
        let mut out = CollectedOutput::default();
        // Top-level / Col context: flex Spacers don't fill at
        // this level (no Row to absorb their flexibility), so
        // they fall back to `cols`. Row uses a separate code
        // path that sees the Spacer spec directly and handles
        // flex sizing — see RowPiece::Flex.
        let cols = (*cols).min(4096) as usize;
        let mut text = String::with_capacity(cols + 1);
        for _ in 0..cols {
            text.push(' ');
        }
        let mut entry = TextPropertyEntry {
            text,
            properties: Default::default(),
            style: None,
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
