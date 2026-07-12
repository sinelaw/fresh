//! Single-line text input control — state only.
//!
//! The control renders through the plugin widget framework (the
//! Settings mapping projects this state into a `WidgetSpec::Text`),
//! and its editing mechanics are the same [`TextEdit`] engine the
//! widget runtime drives — value, byte cursor, and selection live in
//! one implementation shared by every text surface in the editor.

use crate::primitives::text_edit::TextEdit;
use ratatui::layout::Rect;
use ratatui::style::Color;

use super::FocusState;

/// State for a text input control
#[derive(Debug, Clone)]
pub struct TextInputState {
    /// Single-line editing engine. Owns the value, the byte-offset
    /// cursor, and the selection — the exact same `TextEdit` the
    /// widget runtime uses for its `Text` widgets, so text-editing
    /// mechanics have one implementation.
    pub editor: TextEdit,
    /// Label displayed before the input
    pub label: String,
    /// Placeholder text when empty
    pub placeholder: String,
    /// Focus state
    pub focus: FocusState,
    /// If true, the user is actively editing (Enter was pressed). When
    /// the control is merely selected/highlighted via navigation this
    /// stays `false`, which suppresses the cursor block so the caret
    /// only appears once the user asks to type.
    pub editing: bool,
    /// If true, validate that value is valid JSON before allowing exit
    pub validate_json: bool,
    /// "Select-all" affordance: when the input gains focus the whole
    /// value is conceptually selected, so the next printable keystroke
    /// replaces it (matching the spinner UX from `NumberInputState`).
    /// Any cursor movement, deletion, or explicit `insert` cancels the
    /// flag and the input behaves normally from then on.
    pub pending_replace_on_type: bool,
}

impl TextInputState {
    /// Create a new text input state
    pub fn new(label: impl Into<String>) -> Self {
        Self {
            editor: TextEdit::single_line(),
            label: label.into(),
            placeholder: String::new(),
            focus: FocusState::Normal,
            editing: false,
            validate_json: false,
            pending_replace_on_type: false,
        }
    }

    /// The current text value.
    pub fn value(&self) -> String {
        self.editor.value()
    }

    /// The current text value as a borrowed slice. The editor is
    /// single-line, so the value is exactly its one line — no join,
    /// no allocation. For callers that return `&str`.
    pub fn value_str(&self) -> &str {
        self.editor.current_line()
    }

    /// The cursor position as a byte offset into the value.
    pub fn cursor_byte(&self) -> usize {
        self.editor.flat_cursor_byte()
    }

    /// Arm the "next-keystroke-replaces-value" affordance. Call when
    /// the input first gains focus from a normal/hovered state.
    pub fn arm_replace_on_type(&mut self) {
        self.pending_replace_on_type = !self.editor.value().is_empty();
    }

    /// Set JSON validation mode
    pub fn with_json_validation(mut self) -> Self {
        self.validate_json = true;
        self
    }

    /// Check if the current value is valid (valid JSON if validate_json is set)
    pub fn is_valid(&self) -> bool {
        if self.validate_json {
            serde_json::from_str::<serde_json::Value>(&self.value()).is_ok()
        } else {
            true
        }
    }

    /// Set the initial value
    pub fn with_value(mut self, value: impl Into<String>) -> Self {
        let value = value.into();
        self.editor = TextEdit::single_line_with_text(&value);
        self.editor.move_end();
        self
    }

    /// Set the placeholder text
    pub fn with_placeholder(mut self, placeholder: impl Into<String>) -> Self {
        self.placeholder = placeholder.into();
        self
    }

    /// Set the focus state
    pub fn with_focus(mut self, focus: FocusState) -> Self {
        self.focus = focus;
        self
    }

    /// Check if the control is enabled
    pub fn is_enabled(&self) -> bool {
        self.focus != FocusState::Disabled
    }

    /// Insert a character at the cursor position
    pub fn insert(&mut self, c: char) {
        if !self.is_enabled() {
            return;
        }
        self.consume_pending_replace();
        self.editor.insert_char(c);
    }

    /// Insert a string at the cursor position
    pub fn insert_str(&mut self, s: &str) {
        if !self.is_enabled() {
            return;
        }
        self.consume_pending_replace();
        self.editor.insert_str(s);
    }

    /// Delete the character before the cursor (backspace)
    pub fn backspace(&mut self) {
        if !self.is_enabled() {
            return;
        }
        if self.consume_pending_replace() {
            // The "selected" value is cleared; nothing left to backspace.
            return;
        }
        self.editor.backspace();
    }

    /// If a pending replace-on-type is armed, clear the value and the
    /// flag. Returns whether the pending state was consumed.
    fn consume_pending_replace(&mut self) -> bool {
        if self.pending_replace_on_type {
            self.editor.set_value("");
            self.pending_replace_on_type = false;
            true
        } else {
            false
        }
    }

    /// Delete the character at the cursor (delete key)
    pub fn delete(&mut self) {
        if !self.is_enabled() {
            return;
        }
        if self.consume_pending_replace() {
            return;
        }
        self.editor.delete();
    }

    /// Position the caret at a flat byte offset in the value — used for
    /// click-to-position (#2573). Like the arrow keys, an explicit caret
    /// placement cancels the "next keystroke replaces the value" arm: the
    /// user pointed at a spot, so the following keystroke must insert
    /// there, not wipe the field. The offset is grapheme-snapped and
    /// clamped by `TextEdit`.
    pub fn set_cursor_from_flat(&mut self, byte: usize) {
        self.pending_replace_on_type = false;
        self.editor.set_cursor_from_flat(byte);
    }

    /// Move cursor left
    pub fn move_left(&mut self) {
        self.pending_replace_on_type = false;
        self.editor.move_left();
    }

    /// Move cursor right
    pub fn move_right(&mut self) {
        self.pending_replace_on_type = false;
        self.editor.move_right();
    }

    /// Move cursor to start
    pub fn move_home(&mut self) {
        self.pending_replace_on_type = false;
        self.editor.move_home();
    }

    /// Move cursor to end
    pub fn move_end(&mut self) {
        self.pending_replace_on_type = false;
        self.editor.move_end();
    }

    /// Extend the selection one position left (Shift+Left)
    pub fn move_left_selecting(&mut self) {
        self.pending_replace_on_type = false;
        self.editor.move_left_selecting();
    }

    /// Extend the selection one position right (Shift+Right)
    pub fn move_right_selecting(&mut self) {
        self.pending_replace_on_type = false;
        self.editor.move_right_selecting();
    }

    /// Select the whole value (Ctrl+A). The selection is live editor
    /// state — the next insert replaces it — so the replace-on-type
    /// affordance is superseded and cleared.
    pub fn select_all(&mut self) {
        self.pending_replace_on_type = false;
        self.editor.select_all();
    }

    /// The currently selected text, if any (Ctrl+C).
    pub fn selected_text(&self) -> Option<String> {
        self.editor.selected_text()
    }

    /// Clear the input
    pub fn clear(&mut self) {
        if self.is_enabled() {
            self.editor.set_value("");
        }
    }

    /// Set the value directly (cursor moves to the end)
    pub fn set_value(&mut self, value: impl Into<String>) {
        if self.is_enabled() {
            self.force_value(value);
        }
    }

    /// Set the value regardless of the enabled state — for model
    /// updates (config loads, reset/inherit) rather than user input.
    pub fn force_value(&mut self, value: impl Into<String>) {
        self.editor = TextEdit::single_line_with_text(&value.into());
        self.editor.move_end();
    }
}

/// Colors for the text input control
#[derive(Debug, Clone, Copy)]
pub struct TextInputColors {
    /// Label color
    pub label: Color,
    /// Input text color
    pub text: Color,
    /// Border/bracket color
    pub border: Color,
    /// Placeholder text color
    pub placeholder: Color,
    /// Cursor color
    pub cursor: Color,
    /// Focused highlight color
    pub focused: Color,
    /// Disabled color
    pub disabled: Color,
    /// Background colour used when the field is actively being edited
    /// (state.focus == Focused && state.editing). Gives the user a
    /// clear "keystrokes go here" signal.
    pub editing_bg: Color,
}

impl Default for TextInputColors {
    fn default() -> Self {
        Self {
            label: Color::White,
            text: Color::White,
            border: Color::Gray,
            placeholder: Color::DarkGray,
            cursor: Color::Yellow,
            focused: Color::Cyan,
            disabled: Color::DarkGray,
            editing_bg: Color::DarkGray,
        }
    }
}

impl TextInputColors {
    /// Create colors from theme
    pub fn from_theme(theme: &crate::view::theme::Theme) -> Self {
        Self {
            label: theme.editor_fg,
            text: theme.editor_fg,
            border: theme.line_number_fg,
            placeholder: theme.line_number_fg,
            cursor: theme.cursor,
            // Use a fg-family colour for the focused/editing accent so
            // the label and bracket highlighting remain readable against
            // dark row backgrounds. `selection_bg` is a background colour
            // and renders as dark-on-dark on high-contrast themes.
            focused: theme.settings_selected_fg,
            disabled: theme.line_number_fg,
            // Reuse the popup-selection bg (`ui.popup_selection_bg`) —
            // the same key the plugin widget framework's Toggle /
            // Button use for focused chrome. Guaranteed to contrast
            // with popup_bg across all bundled themes.
            editing_bg: theme.popup_selection_bg,
        }
    }

    /// Create dimmed colors for read-only/inherited text inputs.
    /// Shows brackets but with muted styling to indicate the field exists
    /// but is not currently editable.
    pub fn from_theme_disabled(theme: &crate::view::theme::Theme) -> Self {
        Self {
            label: theme.editor_fg,
            text: theme.line_number_fg,
            border: theme.line_number_fg,
            placeholder: theme.line_number_fg,
            cursor: theme.cursor,
            focused: theme.settings_selected_fg,
            disabled: theme.line_number_fg,
            editing_bg: theme.popup_selection_bg,
        }
    }
}

/// Layout information returned after rendering for hit testing
#[derive(Debug, Clone, Copy, Default)]
pub struct TextInputLayout {
    /// The text input field area
    pub input_area: Rect,
    /// The full control area including label
    pub full_area: Rect,
    /// Cursor position in screen coordinates (if focused)
    pub cursor_pos: Option<(u16, u16)>,
}

impl TextInputLayout {
    /// Check if a point is within the input area
    pub fn is_input(&self, x: u16, y: u16) -> bool {
        x >= self.input_area.x
            && x < self.input_area.x + self.input_area.width
            && y >= self.input_area.y
            && y < self.input_area.y + self.input_area.height
    }

    /// Check if a point is within the full control area
    pub fn contains(&self, x: u16, y: u16) -> bool {
        x >= self.full_area.x
            && x < self.full_area.x + self.full_area.width
            && y >= self.full_area.y
            && y < self.full_area.y + self.full_area.height
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arm_replace_on_type_replaces_value_on_first_char() {
        let mut state = TextInputState::new("Width").with_value("30%");
        state.arm_replace_on_type();
        assert!(state.pending_replace_on_type);
        state.insert('2');
        assert_eq!(state.value(), "2");
        assert!(!state.pending_replace_on_type);
        state.insert('4');
        assert_eq!(state.value(), "24");
    }

    #[test]
    fn test_arm_replace_on_type_is_cancelled_by_cursor_movement() {
        let mut state = TextInputState::new("Width").with_value("30%");
        state.arm_replace_on_type();
        state.move_left();
        assert!(!state.pending_replace_on_type);
        state.insert('x');
        assert_eq!(state.value(), "30x%");
    }

    #[test]
    fn test_arm_replace_on_type_skips_when_empty() {
        let mut state = TextInputState::new("Width");
        state.arm_replace_on_type();
        assert!(!state.pending_replace_on_type);
    }

    #[test]
    fn test_arm_replace_on_type_backspace_clears_whole_value() {
        let mut state = TextInputState::new("Width").with_value("30%");
        state.arm_replace_on_type();
        state.backspace();
        assert_eq!(state.value(), "");
        assert!(!state.pending_replace_on_type);
    }

    #[test]
    fn test_text_input_insert() {
        let mut state = TextInputState::new("Test");
        state.insert('a');
        state.insert('b');
        state.insert('c');
        assert_eq!(state.value(), "abc");
        assert_eq!(state.cursor_byte(), 3);
    }

    #[test]
    fn test_text_input_backspace() {
        let mut state = TextInputState::new("Test").with_value("abc");
        state.backspace();
        assert_eq!(state.value(), "ab");
        assert_eq!(state.cursor_byte(), 2);
    }

    #[test]
    fn test_text_input_cursor_movement() {
        let mut state = TextInputState::new("Test").with_value("hello");
        assert_eq!(state.cursor_byte(), 5);

        state.move_left();
        assert_eq!(state.cursor_byte(), 4);

        state.move_home();
        assert_eq!(state.cursor_byte(), 0);

        state.move_right();
        assert_eq!(state.cursor_byte(), 1);

        state.move_end();
        assert_eq!(state.cursor_byte(), 5);
    }

    #[test]
    fn test_text_input_delete() {
        let mut state = TextInputState::new("Test").with_value("abc");
        state.move_home();
        state.delete();
        assert_eq!(state.value(), "bc");
        assert_eq!(state.cursor_byte(), 0);
    }

    #[test]
    fn test_text_input_disabled() {
        let mut state = TextInputState::new("Test").with_focus(FocusState::Disabled);
        state.insert('a');
        assert_eq!(state.value(), "");
    }

    #[test]
    fn test_text_input_clear() {
        let mut state = TextInputState::new("Test").with_value("hello");
        state.clear();
        assert_eq!(state.value(), "");
        assert_eq!(state.cursor_byte(), 0);
    }

    #[test]
    fn test_text_input_multibyte_insert_and_backspace() {
        // Regression test for issue #466: panic when backspacing multi-byte chars
        let mut state = TextInputState::new("Test");
        // © is 2 bytes in UTF-8
        state.insert('©');
        assert_eq!(state.value(), "©");
        assert_eq!(state.cursor_byte(), 2); // byte position, not char position

        // Backspace should delete the whole character, not cause a panic
        state.backspace();
        assert_eq!(state.value(), "");
        assert_eq!(state.cursor_byte(), 0);
    }

    #[test]
    fn test_text_input_multibyte_cursor_movement() {
        let mut state = TextInputState::new("Test").with_value("日本語");
        // Each Japanese character is 3 bytes
        assert_eq!(state.cursor_byte(), 9);

        state.move_left();
        assert_eq!(state.cursor_byte(), 6); // moved back by one character (3 bytes)

        state.move_left();
        assert_eq!(state.cursor_byte(), 3);

        state.move_right();
        assert_eq!(state.cursor_byte(), 6);

        state.move_home();
        assert_eq!(state.cursor_byte(), 0);

        state.move_right();
        assert_eq!(state.cursor_byte(), 3); // moved forward by one character (3 bytes)
    }

    #[test]
    fn test_text_input_multibyte_delete() {
        let mut state = TextInputState::new("Test").with_value("a日b");
        // 'a' is 1 byte, '日' is 3 bytes, 'b' is 1 byte = 5 bytes total
        assert_eq!(state.cursor_byte(), 5);

        state.move_home();
        state.move_right(); // cursor now at byte 1 (after 'a', before '日')
        assert_eq!(state.cursor_byte(), 1);

        state.delete(); // delete '日'
        assert_eq!(state.value(), "ab");
        assert_eq!(state.cursor_byte(), 1);
    }

    #[test]
    fn test_text_input_insert_between_multibyte() {
        let mut state = TextInputState::new("Test").with_value("日語");
        state.move_home();
        state.move_right(); // cursor after first character
        assert_eq!(state.cursor_byte(), 3);

        state.insert('本');
        assert_eq!(state.value(), "日本語");
        assert_eq!(state.cursor_byte(), 6);
    }
}
