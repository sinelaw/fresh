//! `Label` — one row of static text: the hint under a field, a status
//! line, a read-only summary. Not focusable, no hits, no state.
//!
//! This is what a plugin reached for `Raw` to say — one styled line — and
//! a form full of them is a form the migration cannot finish. As a kind it
//! also knows the form's grid: `label_width` indents the text into the
//! field column, so a field's hint sits under its value rather than under
//! its label, with no column arithmetic in the plugin.

use fresh_core::api::WidgetSpec;

use super::WidgetImpl;

pub struct Label;

impl WidgetImpl for Label {
    fn box_meta(&self, _spec: &WidgetSpec) -> super::BoxMeta {
        super::BoxMeta::plain("label")
    }
}

#[cfg(test)]
mod tests {
    use crate::widgets::render::render_label;
    use fresh_core::api::{OverlayColorSpec, OverlayOptions};
    use fresh_core::text_property::StyledSegment;

    fn seg(text: &str, fg: &str) -> StyledSegment {
        StyledSegment {
            text: text.into(),
            style: Some(OverlayOptions {
                fg: Some(OverlayColorSpec::ThemeKey(fg.into())),
                ..Default::default()
            }),
            overlays: Vec::new(),
        }
    }

    #[test]
    fn a_label_is_its_text() {
        let e = render_label("\u{21b3} blank = remote home", None, 0, false, 60, &[]);
        assert_eq!(e.text.trim_end(), "\u{21b3} blank = remote home");
    }

    #[test]
    fn label_width_indents_into_the_field_column() {
        // A `Text` with the same `label_width` opens its value cell at
        // `label_width + 2` (label, then `: `); the label's text lands
        // there too, so a hint sits under the value it describes.
        let e = render_label("\u{21b3} hint", None, 8, false, 60, &[]);
        assert_eq!(e.text.trim_end(), "          \u{21b3} hint");
    }

    #[test]
    fn styled_runs_keep_their_own_inks_on_one_row() {
        // The Explain popup's head row: a state glyph in the state's
        // colour, then the workspace name. One row, two inks, which is
        // the whole reason the field exists.
        let mut e = render_label(
            "ignored",
            None,
            0,
            false,
            60,
            &[seg("\u{25cf} blocked", "attention"), seg("   ux-3", "fg")],
        );
        e.normalize_widths();
        assert_eq!(e.text, "\u{25cf} blocked   ux-3");
        let inks: Vec<_> = e
            .inline_overlays
            .iter()
            .map(|o| o.style.fg.clone())
            .collect();
        assert_eq!(
            inks,
            vec![
                Some(OverlayColorSpec::ThemeKey("attention".into())),
                Some(OverlayColorSpec::ThemeKey("fg".into())),
            ]
        );
    }

    #[test]
    fn an_indented_run_is_shifted_past_the_field_column() {
        // The runs are handed on as segments, so their offsets are
        // resolved against the row the gutter and indent already lead.
        let mut e = render_label("", None, 8, false, 60, &[seg("done", "ok")]);
        e.normalize_widths();
        assert_eq!(e.text, "          done");
        let o = &e.inline_overlays[0];
        assert_eq!((o.start, o.end), (10, 14));
    }
}
