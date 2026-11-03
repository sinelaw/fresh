//! Semantic-token colours must not depend on the order the overlay set keeps
//! its entries in.
//!
//! `OverlayManager` removes entries with `swap_remove`, so removing any
//! overlay moves the last entry into the freed slot. The renderer collects
//! the semantic tokens on screen from `query_viewport`, in storage order, and
//! looks each cell's colour up with a forward-only cursor (`span_at`), which
//! needs them sorted by position. Once a later token sat in front of earlier
//! ones, every token on screen before it lost its colour, and stayed that way
//! until something moved the viewport off it.
//!
//! A double-click is enough to get there: selecting a word clears the
//! occurrence highlight of the word under the cursor, which was added before
//! the server's tokens arrived. In a file that fits on screen the token moved
//! forward is on screen too, so the whole file falls back to grammar colours.

use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use fresh::services::lsp::semantic_tokens::{
    apply_semantic_tokens_to_state, lsp_semantic_tokens_namespace,
};
use fresh::state::SemanticTokenSpan;
use fresh::view::overlay::{Overlay, OverlayFace, OverlayNamespace};
use ratatui::style::Color;

fn token(range: std::ops::Range<usize>, token_type: &str) -> SemanticTokenSpan {
    SemanticTokenSpan {
        range,
        token_type: token_type.to_string(),
        modifiers: Vec::new(),
    }
}

#[test]
fn semantic_tokens_keep_their_colours_after_another_overlay_is_removed() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    // Plain text has no grammar colours: a cell shows its token's colour or
    // the default foreground.
    let fixture = TestFixture::new("tokens.txt", "alpha beta gamma\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    let theme = harness.editor().theme().clone();
    let other = OverlayNamespace::from_string("test-other-producer".to_string());
    {
        let state = harness.editor_mut().active_state_mut();
        // Another producer's overlay, in place before the tokens arrive.
        let overlay = Overlay::with_namespace(
            &mut state.marker_list,
            6..10,
            OverlayFace::Background {
                color: Color::Rgb(1, 2, 3),
            },
            other.clone(),
        );
        state.overlays.add(overlay);

        apply_semantic_tokens_to_state(
            state,
            &[
                token(0..5, "function"),
                token(6..10, "class"),
                token(11..16, "keyword"),
            ],
            &theme,
        );

        // Removing it swaps the last token into its slot.
        state
            .overlays
            .clear_namespace(&other, &mut state.marker_list);

        let ns = lsp_semantic_tokens_namespace();
        let starts: Vec<usize> = state
            .overlays
            .all()
            .iter()
            .filter(|o| o.namespace.as_ref() == Some(&ns))
            .map(|o| o.range(&state.marker_list).start)
            .collect();
        assert!(
            !starts.is_sorted(),
            "precondition: the removal should leave the tokens out of position \
             order in storage, or this test checks nothing (starts: {starts:?})"
        );
    }
    harness.render().unwrap();

    let (col, row) = harness
        .find_text_on_screen("alpha beta gamma")
        .expect("line should be on screen");
    for (offset, word, expected) in [
        (0, "alpha", theme.syntax_function),
        (6, "beta", theme.syntax_type),
        (11, "gamma", theme.syntax_keyword),
    ] {
        let fg = harness
            .get_cell_style(col + offset, row)
            .and_then(|style| style.fg);
        assert_eq!(fg, Some(expected), "`{word}` should keep its token colour");
    }
}
