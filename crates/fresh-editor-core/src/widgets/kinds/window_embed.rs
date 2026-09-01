//! `WindowEmbed` — reserves a rectangle for a native window render.

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;
use fresh_core::text_property::TextPropertyEntry;

use super::WidgetImpl;
use crate::widgets::registry::WidgetInstanceState;
use crate::widgets::render::{CollectedOutput, EmbedRect, RenderContext};

pub struct WindowEmbed;

impl WidgetImpl for WindowEmbed {
    fn box_meta(&self, _spec: &WidgetSpec) -> super::BoxMeta {
        super::BoxMeta::plain("window_embed")
    }
    fn collect(
        &self,
        spec: &WidgetSpec,
        _prev: &HashMap<String, WidgetInstanceState>,
        _next_state: &mut HashMap<String, WidgetInstanceState>,
        _ctx: RenderContext<'_>,
        panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::WindowEmbed {
            window_id, rows, ..
        } = spec
        else {
            return CollectedOutput::default();
        };
        let mut out = CollectedOutput::default();
        // Emit `rows` blank lines of `panel_width` width so
        // layout reserves the rectangle. The host paint
        // path overlays the native window render on top of
        // these blanks after the rest of the panel paints.
        let cols = panel_width.max(1) as usize;
        for _ in 0..*rows {
            let mut text = String::with_capacity(cols + 1);
            for _ in 0..cols {
                text.push(' ');
            }
            text.push('\n');
            out.entries.push(TextPropertyEntry {
                text,
                properties: Default::default(),
                style: None,
                inline_overlays: Vec::new(),
                segments: Vec::new(),
                pad_to_chars: None,
                truncate_to_chars: None,
            });
        }
        out.embeds.push(EmbedRect {
            window_id: *window_id,
            buffer_row: 0,
            col_in_row: 0,
            width_cols: panel_width,
            height_rows: *rows,
        });
        out
    }
}
