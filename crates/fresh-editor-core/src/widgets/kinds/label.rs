//! `Label` — one row of static text: the hint under a field, a status
//! line, a read-only summary. Not focusable, no hits, no state.
//!
//! This is what a plugin reached for `Raw` to say — one styled line — and
//! a form full of them is a form the migration cannot finish. As a kind it
//! also knows the form's grid: `label_width` indents the text into the
//! field column, so a field's hint sits under its value rather than under
//! its label, with no column arithmetic in the plugin.

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;

use super::WidgetImpl;
use crate::widgets::registry::WidgetInstanceState;
use crate::widgets::render::{
    ensure_trailing_newline, render_label, CollectedOutput, RenderContext,
};

pub struct Label;

impl WidgetImpl for Label {
    fn box_meta(&self, _spec: &WidgetSpec) -> super::BoxMeta {
        super::BoxMeta::plain("label")
    }
    fn collect(
        &self,
        spec: &WidgetSpec,
        _prev: &HashMap<String, WidgetInstanceState>,
        _next_state: &mut HashMap<String, WidgetInstanceState>,
        ctx: RenderContext<'_>,
        panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::Label {
            text,
            style,
            label_width,
            ..
        } = spec
        else {
            return CollectedOutput::default();
        };
        let mut out = CollectedOutput::default();
        let mut entry = render_label(
            text,
            style.as_ref(),
            *label_width,
            ctx.marker_gutter,
            panel_width,
        );
        ensure_trailing_newline(&mut entry);
        out.entries.push(entry);
        out
    }
}

#[cfg(test)]
mod tests {
    use crate::widgets::render::render_spec;
    use fresh_core::api::WidgetSpec;
    use std::collections::HashMap;

    fn label(text: &str, label_width: u32) -> WidgetSpec {
        WidgetSpec::Label {
            text: text.into(),
            style: None,
            label_width,
            key: None,
        }
    }

    #[test]
    fn a_label_is_its_text_with_no_hits() {
        let out = render_spec(&label("↳ blank = remote home", 0), &HashMap::new(), "", 60);
        assert_eq!(out.entries[0].text.trim_end(), "↳ blank = remote home");
        assert!(out.hits.is_empty());
        assert!(out.focus_key.is_empty(), "a label is never a tab stop");
    }

    #[test]
    fn label_width_indents_into_the_field_column() {
        // A `Text` with the same `label_width` opens its value cell at
        // `label_width + 2` (label, then `: `); the label's text lands
        // there too, so a hint sits under the value it describes.
        let out = render_spec(&label("↳ hint", 8), &HashMap::new(), "", 60);
        assert_eq!(out.entries[0].text.trim_end(), "          ↳ hint");
    }
}
