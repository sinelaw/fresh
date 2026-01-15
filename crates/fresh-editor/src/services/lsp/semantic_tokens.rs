use crate::state::{EditorState, SemanticTokenSpan};
use crate::view::overlay::{Overlay, OverlayFace, OverlayNamespace};
use ratatui::style::Color;

const SEMANTIC_TOKENS_NAMESPACE: &str = "lsp-semantic-token";
const SEMANTIC_TOKENS_PRIORITY: i32 = 5;

/// Namespace for all LSP semantic token overlays.
pub fn lsp_semantic_tokens_namespace() -> OverlayNamespace {
    OverlayNamespace::from_string(SEMANTIC_TOKENS_NAMESPACE.to_string())
}

/// Returns true if an overlay belongs to the semantic token namespace.
pub fn is_semantic_token_overlay(overlay: &crate::view::overlay::Overlay) -> bool {
    overlay
        .namespace
        .as_ref()
        .map(|ns| ns.as_str() == SEMANTIC_TOKENS_NAMESPACE)
        .unwrap_or(false)
}

/// Map an LSP semantic token type + modifiers to a theme color.
pub fn semantic_token_color(
    token_type: &str,
    modifiers: &[String],
    theme: &crate::view::theme::Theme,
) -> Color {
    if modifiers.iter().any(|m| m == "deprecated") {
        return theme.diagnostic_warning_fg;
    }

    match token_type {
        "keyword" | "modifier" => theme.syntax_keyword,
        "function" | "method" | "macro" => theme.syntax_function,
        "parameter" | "variable" | "property" | "enumMember" | "event" | "label" => {
            theme.syntax_variable
        }
        "type" | "class" | "interface" | "struct" | "typeParameter" | "namespace" | "enum" => {
            theme.syntax_type
        }
        "number" => theme.syntax_constant,
        "string" | "regexp" => theme.syntax_string,
        "operator" => theme.syntax_operator,
        "comment" => theme.syntax_comment,
        "decorator" => theme.syntax_function,
        _ => theme.syntax_variable,
    }
}

/// Apply semantic tokens as overlays so their ranges track edits.
pub fn apply_semantic_tokens_to_state(
    state: &mut EditorState,
    tokens: &[SemanticTokenSpan],
    theme: &crate::view::theme::Theme,
) {
    let full_range = 0..state.buffer.len();
    apply_semantic_tokens_range_to_state(state, full_range, tokens, theme);
}

/// Apply semantic tokens for a specific buffer range.
pub fn apply_semantic_tokens_range_to_state(
    state: &mut EditorState,
    range: std::ops::Range<usize>,
    tokens: &[SemanticTokenSpan],
    theme: &crate::view::theme::Theme,
) {
    let ns = lsp_semantic_tokens_namespace();
    let mut new_overlays = Vec::with_capacity(tokens.len());

    for token in tokens {
        let color = semantic_token_color(&token.token_type, &token.modifiers, theme);
        let overlay = Overlay::with_namespace(
            &mut state.marker_list,
            token.range.clone(),
            OverlayFace::Foreground { color },
            ns.clone(),
        )
        .with_priority_value(SEMANTIC_TOKENS_PRIORITY);

        new_overlays.push(overlay);
    }

    state
        .overlays
        .replace_range_in_namespace(&ns, &range, new_overlays, &mut state.marker_list);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::LARGE_FILE_THRESHOLD_BYTES;
    use crate::model::event::{CursorId, Event};
    use crate::state::SemanticTokenSpan;
    use crate::view::theme::{Theme, THEME_DARK};

    #[test]
    fn semantic_token_overlays_shift_on_insert() {
        let mut state = EditorState::new(80, 24, LARGE_FILE_THRESHOLD_BYTES as usize);
        state.apply(&Event::Insert {
            position: 0,
            text: "fn main() {}".to_string(),
            cursor_id: CursorId::UNDO_SENTINEL,
        });

        let span = SemanticTokenSpan {
            range: 3..7, // "main"
            token_type: "function".to_string(),
            modifiers: Vec::new(),
        };

        let theme = Theme::from_name(THEME_DARK).expect("dark theme must exist");
        apply_semantic_tokens_to_state(&mut state, &[span], &theme);

        let ns = lsp_semantic_tokens_namespace();
        let overlay_handle = state
            .overlays
            .all()
            .iter()
            .find(|o| o.namespace.as_ref() == Some(&ns))
            .expect("semantic overlay missing")
            .handle
            .clone();

        let initial_range = state
            .overlays
            .all()
            .iter()
            .find(|o| o.handle == overlay_handle)
            .expect("semantic overlay missing")
            .range(&state.marker_list);
        assert_eq!(initial_range, 3..7);

        state.apply(&Event::Insert {
            position: 0,
            text: "abc".to_string(),
            cursor_id: CursorId::UNDO_SENTINEL,
        });

        let moved_range = state
            .overlays
            .all()
            .iter()
            .find(|o| o.handle == overlay_handle)
            .expect("semantic overlay missing")
            .range(&state.marker_list);
        assert_eq!(moved_range, 6..10);
    }
}
