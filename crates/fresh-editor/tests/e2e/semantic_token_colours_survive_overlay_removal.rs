//! Issue #3705: semantic-token colours must not depend on the order the
//! overlay store happens to keep its entries in.
//!
//! `OverlayManager` is an unordered set that removes with `swap_remove`, so
//! removing *any* overlay moves the last entry into the freed slot. The
//! renderer collects the semantic tokens on screen from `query_viewport` and
//! looks each cell's colour up with a forward-only cursor (`span_at`), which
//! needs them in position order: once a later token sits in front of earlier
//! ones, every token before it on screen loses its colour and stays that way
//! until something moves the viewport off it.
//!
//! Double-clicking a word is enough to get there. The occurrence highlight of
//! the word under the cursor is in place before the server's tokens arrive, so
//! it sits *before* them in the store; selecting a word retires it (the
//! selection's own matches take over), and the removal drags the last tokens
//! to the front. In a file that fits on screen those tokens are on screen too,
//! so the whole file falls back to the default foreground.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::services::lsp::semantic_tokens::apply_semantic_tokens_to_state;
use fresh::state::SemanticTokenSpan;
use ratatui::style::Color;

/// `alpha beta gamma delta epsilon\nzeta beta eta\n`, with byte offsets:
/// alpha 0..5, beta 6..10, gamma 11..16, delta 17..22, epsilon 23..30,
/// zeta 31..35, beta 36..40, eta 41..44.
const LINE_1: &str = "alpha beta gamma delta epsilon";
const LINE_2: &str = "zeta beta eta";

fn token(range: std::ops::Range<usize>, token_type: &str) -> SemanticTokenSpan {
    SemanticTokenSpan {
        range,
        token_type: token_type.to_string(),
        modifiers: Vec::new(),
    }
}

/// Issue #3705. Fails without the position ordering in
/// `OverlayManager::query_viewport`: `alpha`, `gamma` and `delta` come back
/// bare after the double click.
#[test]
fn semantic_token_colours_survive_the_word_highlight_being_retired() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    // A plain-text buffer has no grammar colours, so every cell shows either
    // its semantic token's colour or the theme's default foreground — which
    // is what makes the assertions below mean anything.
    let _fixture = harness
        .load_buffer_from_text(&format!("{LINE_1}\n{LINE_2}\n"))
        .unwrap();
    harness.render().unwrap();

    let (col1, row1) = harness
        .find_text_on_screen(LINE_1)
        .expect("the first line should be on screen");
    let (col2, row2) = harness
        .find_text_on_screen(LINE_2)
        .expect("the second line should be on screen");

    // Every word that gets a token, with the colour that token asks for.
    // `beta` deliberately gets none: it is the word the double click lands
    // on, and the selection paints its own background over it.
    let theme = harness.editor().theme().clone();
    let tokens = [
        token(0..5, "keyword"),
        token(11..16, "type"),
        token(17..22, "string"),
        token(23..30, "number"),
        token(41..44, "function"),
    ];
    let words: [(&str, u16, u16, Color); 5] = [
        ("alpha", col1, row1, theme.syntax_keyword),
        ("gamma", col1 + 11, row1, theme.syntax_type),
        ("delta", col1 + 17, row1, theme.syntax_string),
        ("epsilon", col1 + 23, row1, theme.syntax_constant),
        ("eta", col2 + 10, row2, theme.syntax_function),
    ];

    let fg = |h: &EditorTestHarness, x: u16, y: u16| h.get_cell_style(x, y).and_then(|s| s.fg);
    let bg = |h: &EditorTestHarness, x: u16, y: u16| h.get_cell_style(x, y).and_then(|s| s.bg);

    // Nothing is vacuous below: with no tokens applied, none of the five
    // words is already wearing the colour its token will ask for.
    for (word, x, y, expected) in words {
        assert_ne!(
            fg(&harness, x, y),
            Some(expected),
            "`{word}` must not already have its token colour before the \
             tokens are applied, or the assertions below prove nothing"
        );
    }

    // Put the cursor on the "b" of `beta` on line 1 and let the occurrence
    // highlight settle: both `beta`s get a background of their own. This is
    // the overlay that ends up in front of the tokens.
    for _ in 0..6 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    let beta_1 = col1 + 6;
    let beta_2 = col2 + 5;
    let neutral_1 = col1 + 11; // 'g' of `gamma` — never highlighted
    let neutral_2 = col2; // 'z' of `zeta`
    harness
        .wait_until(|h| {
            bg(h, beta_1, row1) != bg(h, neutral_1, row1)
                && bg(h, beta_2, row2) != bg(h, neutral_2, row2)
        })
        .expect("the word under the cursor should be highlighted");

    // The server's tokens arrive now — after the occurrence highlight, as
    // they do in the reported flow. (`set_fold_range` in `folding.rs` seeds
    // an LSP result the same way; everything asserted below is on screen.)
    {
        let state = harness.editor_mut().active_state_mut();
        apply_semantic_tokens_to_state(state, &tokens, &theme);
    }
    harness.render().unwrap();
    for (word, x, y, expected) in words {
        assert_eq!(
            fg(&harness, x, y),
            Some(expected),
            "`{word}` should be wearing its token colour before the double click"
        );
    }

    // The trigger: double-click `beta`. Selecting it retires the
    // word-under-cursor highlight, and that removal is what used to shuffle
    // the tokens. The selection's background is its own, distinct from the
    // occurrence highlight's, so the wait below sees the handover.
    let word_highlight_bg = bg(&harness, beta_1, row1);
    harness.mouse_click(beta_1, row1).unwrap();
    harness.mouse_click(beta_1, row1).unwrap();
    harness
        .wait_until(|h| bg(h, beta_1, row1) != word_highlight_bg)
        .expect("the double click should select `beta`");

    for (word, x, y, expected) in words {
        assert_eq!(
            fg(&harness, x, y),
            Some(expected),
            "`{word}` should keep its token colour after the double click"
        );
    }
}
