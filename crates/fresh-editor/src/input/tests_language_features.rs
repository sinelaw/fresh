#[cfg(test)]
mod tests {
    use crate::input::actions::get_auto_close_char;
    use crate::input::multi_cursor::{add_cursor_at_next_match, AddCursorResult};
    use crate::model::buffer::Buffer;
    use crate::primitives::word_navigation::{find_word_start_left, find_word_start_right};
    use crate::state::EditorState;

    // --- Auto-Pairs Logic Tests ---

    #[test]
    fn test_auto_close_quotes_rust() {
        // In Rust, quotes should auto-close
        assert_eq!(get_auto_close_char('"', true, "rust"), Some('"'));
        assert_eq!(get_auto_close_char('\'', true, "rust"), Some('\''));
    }

    #[test]
    fn test_auto_close_quotes_text() {
        // In Text, quotes should NOT auto-close
        assert_eq!(get_auto_close_char('"', true, "text"), None);
        assert_eq!(get_auto_close_char('\'', true, "text"), None);

        // But brackets SHOULD still auto-close
        assert_eq!(get_auto_close_char('(', true, "text"), Some(')'));
        assert_eq!(get_auto_close_char('[', true, "text"), Some(']'));
        assert_eq!(get_auto_close_char('{', true, "text"), Some('}'));
    }

    // --- Word Movement Tests ---

    #[test]
    fn test_word_movement_punctuation() {
        let buffer = Buffer::from_str("foo.bar_baz", 0);

        // "foo|.bar_baz" -> Right -> "foo.|bar_baz"
        // Starting at 3 (after 'foo')
        // It should stop at start of '.', then start of 'bar'

        // Current impl of find_word_start_right:
        // skip current class, then skip whitespace.

        // Position 0 ('f'): Word
        // next boundary is 3 ('o' -> '.')
        assert_eq!(find_word_start_right(&buffer, 0), 3);

        // Position 3 ('.'): Punctuation
        // next boundary is 4 ('.' -> 'b')
        assert_eq!(find_word_start_right(&buffer, 3), 4);

        // Position 4 ('b'): Word
        // 'bar_baz' is all word chars? '_' is word char.
        // so it should go to end (11)
        assert_eq!(find_word_start_right(&buffer, 4), 11);
    }

    #[test]
    fn test_word_movement_whitespace_punctuation() {
        // "a . b"
        let buffer = Buffer::from_str("a . b", 0);

        // 0 ('a') -> Word. Ends at 1. Skip whitespace -> 2 ('.')
        assert_eq!(find_word_start_right(&buffer, 0), 2);

        // 2 ('.') -> Punctuation. Ends at 3. Skip whitespace -> 4 ('b')
        assert_eq!(find_word_start_right(&buffer, 2), 4);
    }

    #[test]
    fn test_word_movement_left() {
        // "foo.bar"
        let buffer = Buffer::from_str("foo.bar", 0);

        // 7 (end) -> Left -> 4 ('b')
        // 'bar' is word.
        assert_eq!(find_word_start_left(&buffer, 7), 4);

        // 4 ('b') -> Left -> 3 ('.')
        // '.' is punctuation
        assert_eq!(find_word_start_left(&buffer, 4), 3);

        // 3 ('.') -> Left -> 0 ('f')
        // 'foo' is word
        assert_eq!(find_word_start_left(&buffer, 3), 0);
    }

    // --- Multi-Cursor Tests ---

    use crate::model::event::{CursorId, Event};

    // Helper to apply the result of add_cursor_at_next_match to the state
    fn perform_add_cursor_at_next_match(state: &mut EditorState) -> AddCursorResult {
        let result = add_cursor_at_next_match(state);
        if let AddCursorResult::Success { cursor, .. } = &result {
            // Manually apply the change to the state since add_cursor_at_next_match is pure
            // We use a high ID to avoid conflicts in simple tests
            let next_id = CursorId(state.cursors.iter().count());
            state.apply(&Event::AddCursor {
                cursor_id: next_id,
                position: cursor.position,
                anchor: cursor.anchor,
            });
        }
        result
    }

    // Helper to create a basic editor state
    fn create_state(content: &str) -> EditorState {
        let mut state = EditorState::new(0, 0, 1024 * 1024); // sizes don't matter for these tests
                                                             // Manually replace buffer
        let buffer = Buffer::from_str(content, 0);
        // We need to swap the buffer. EditorState fields are public?
        state.buffer = buffer;
        state
    }

    #[test]
    fn test_ctrl_d_basic() {
        let mut state = create_state("foo foo foo");
        // Select first "foo"
        state.cursors.primary_mut().position = 3;
        state.cursors.primary_mut().set_anchor(0);

        // Add next match
        match perform_add_cursor_at_next_match(&mut state) {
            AddCursorResult::Success { total_cursors, .. } => {
                assert_eq!(total_cursors, 2);
                let cursors: Vec<_> = state.cursors.iter().map(|(_, c)| c.position).collect();
                // Should have cursor at 3 and 7 (end of second foo)
                assert!(cursors.contains(&3));
                assert!(cursors.contains(&7));
            }
            _ => panic!("Failed to add cursor"),
        }
    }

    #[test]
    fn test_ctrl_d_skip_overlap() {
        let mut state = create_state("foo foo foo");

        // Cursor 1 on first "foo"
        state.cursors.primary_mut().position = 3;
        state.cursors.primary_mut().set_anchor(0);

        // Manually add cursor on SECOND "foo" (4..7)
        // We use a hack or just ensure we simulate it properly.
        // Let's add it via add_cursor_at_next_match FIRST
        perform_add_cursor_at_next_match(&mut state); // Now we have 2 cursors

        // Now try to add THIRD match. It should skip the second one (already valid) and find the third.
        match perform_add_cursor_at_next_match(&mut state) {
            AddCursorResult::Success { total_cursors, .. } => {
                assert_eq!(total_cursors, 3);
                // Should have cursors at 3, 7, 11
                let cursors: Vec<_> = state.cursors.iter().map(|(_, c)| c.position).collect();
                assert!(cursors.contains(&11));
            }
            _ => panic!("Failed to add 3rd cursor"),
        }
    }

    #[test]
    fn test_ctrl_d_wrap_around() {
        let mut state = create_state("foo bar foo");

        // Cursor 1 on SECOND "foo" (8..11)
        state.cursors.primary_mut().position = 11;
        state.cursors.primary_mut().set_anchor(8);

        // Now add next match. It should wrap around to the FIRST "foo" (0..3)
        match perform_add_cursor_at_next_match(&mut state) {
            AddCursorResult::Success { total_cursors, .. } => {
                assert_eq!(total_cursors, 2);
                let cursors: Vec<_> = state.cursors.iter().map(|(_, c)| c.position).collect();
                assert!(cursors.contains(&11));
                assert!(cursors.contains(&3)); // Wrap around found start match
            }
            _ => panic!("Failed to wrap around"),
        }
    }

    #[test]
    fn test_ctrl_d_wrap_skip_existing() {
        let mut state = create_state("foo foo foo");

        // Cursor on 3rd foo
        state.cursors.primary_mut().position = 11;
        state.cursors.primary_mut().set_anchor(8);

        // Existing cursor on 1st foo

        use crate::model::event::CursorId;
        // Need to add cursor manually. Since state.cursors is Cursors struct (probably private fields?)
        // EditorState has 'cursors: Cursors'.
        // We can't clear easily, but we can verify behavior if we assume the first cursor is the primary one we set?
        // Actually, let's just use add_cursor_at_next_match to setup.

        // Reset state
        let mut state = create_state("foo foo foo");
        // Select 3rd foo properly
        state.cursors.primary_mut().position = 11;
        state.cursors.primary_mut().set_anchor(8);

        // Manually insert a cursor at 1st foo (0..3)
        // We might not have public API to insert duplicate cursor easily from test without using events
        // but 'add_cursor_at_next_match' uses 'state.cursors.iter()'.
        // Let's use internal specific API if available or just Event apply.
        // state.apply(Event::AddCursor ... ) might be cleaner?
        // But `state.apply` is available? Yes.

        state.apply(&Event::AddCursor {
            cursor_id: CursorId(1),
            position: 3,
            anchor: Some(0),
        });

        // Now we have cursor on 3rd (Primary) and 1st.
        // add_cursor_at_next_match should wrap around, see 1st is taken, and find 2nd "foo" (4..7).

        match perform_add_cursor_at_next_match(&mut state) {
            AddCursorResult::Success { total_cursors, .. } => {
                assert_eq!(total_cursors, 3);
                let cursors: Vec<_> = state.cursors.iter().map(|(_, c)| c.position).collect();
                assert!(cursors.contains(&7));
            }
            res => panic!(
                "Failed to find middle cursor with wrap: {:?}",
                match res {
                    AddCursorResult::Failed { message } => message,
                    _ => "".to_string(),
                }
            ),
        }
    }
}
