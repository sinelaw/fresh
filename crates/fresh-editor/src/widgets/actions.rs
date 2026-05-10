//! Pure helpers used by `WidgetCommand` dispatch.
//!
//! These are factored out of the plugin-dispatch module so they can
//! be tested without spinning up an `Editor`. The widget runtime's
//! state mutations are intentionally pure functions of (current
//! widget state, requested action) → next state — the dispatcher
//! reads from the registry, calls these, and fires events.

use fresh_core::api::{TreeNode, WidgetSpec};
use fresh_core::text_property::TextPropertyEntry;

/// Locate a widget node in a spec tree by its stable `key`. Returns
/// the matched node, or `None` if no widget has that key.
///
/// Walks `Row`/`Col` children. Skips `Raw`/`HintBar`/`Spacer` (those
/// kinds either have no key worth dispatching to, or no interactive
/// behaviour at all).
pub fn find_widget_by_key<'a>(spec: &'a WidgetSpec, target: &str) -> Option<&'a WidgetSpec> {
    if target.is_empty() {
        return None;
    }
    match spec {
        WidgetSpec::Row { children, .. } | WidgetSpec::Col { children, .. } => {
            for c in children {
                if let Some(found) = find_widget_by_key(c, target) {
                    return Some(found);
                }
            }
            None
        }
        WidgetSpec::Toggle { key: Some(k), .. }
        | WidgetSpec::Button { key: Some(k), .. }
        | WidgetSpec::Text { key: Some(k), .. }
        | WidgetSpec::List { key: Some(k), .. }
        | WidgetSpec::Tree { key: Some(k), .. }
            if k == target =>
        {
            Some(spec)
        }
        _ => None,
    }
}

/// Apply a non-printable editing key to a text widget's `(value,
/// cursor)` pair, choosing single-line or multi-line semantics from
/// `multiline`. This is the public entry point used by the host
/// dispatcher; `apply_text_input_key` and `apply_text_area_key`
/// remain available as internal helpers (and as the documented test
/// surface for each kind's behaviour).
///
/// `multiline=false` recognises `Backspace` / `Delete` / `Left` /
/// `Right` / `Home` / `End`. `multiline=true` adds `Up` / `Down` /
/// `Enter` (newline insertion) and reinterprets `Home` / `End` as
/// line-relative.
pub fn apply_text_key(
    value: &str,
    cursor: usize,
    key: &str,
    multiline: bool,
) -> (String, usize) {
    if multiline {
        apply_text_area_key(value, cursor, key)
    } else {
        apply_text_input_key(value, cursor, key)
    }
}

/// Insert `text` at `cursor` within `value`, returning `(new_value,
/// new_cursor)`. `cursor` is a UTF-8 byte offset; if it lands
/// mid-multibyte (which the existing key handlers shouldn't
/// produce, but external state could) it snaps down to the
/// preceding char boundary before insertion.
///
/// Used by both `TextInput` and `TextArea` for the printable-char
/// dispatch path (`mode_text_input:<char>`). The terminal layer
/// only delivers IME-committed text — preedit is invisible to the
/// editor — so `text` here is always a final UTF-8 string and may
/// be one codepoint (typical), one grapheme cluster (combining
/// marks), or a multi-codepoint commit (some IMEs push the full
/// word in one event).
pub fn apply_text_char(value: &str, cursor: usize, text: &str) -> (String, usize) {
    let mut cursor = cursor.min(value.len());
    while cursor > 0 && !value.is_char_boundary(cursor) {
        cursor -= 1;
    }
    let mut new_value = String::with_capacity(value.len() + text.len());
    new_value.push_str(&value[..cursor]);
    new_value.push_str(text);
    new_value.push_str(&value[cursor..]);
    (new_value, cursor + text.len())
}

/// Apply a non-printable editing key to a `(value, cursor)` pair,
/// returning `(new_value, new_cursor)`. `cursor` is a UTF-8 byte
/// offset clamped to `[0, value.len()]`.
///
/// Recognised keys: `"Backspace"`, `"Delete"`, `"Left"`, `"Right"`,
/// `"Home"`, `"End"`. Any other key string is a no-op.
///
/// All boundary handling respects UTF-8 char boundaries so the
/// renderer's cursor-byte logic doesn't land in the middle of a
/// multi-byte character. (`Left`/`Right` step by *grapheme* later
/// — for v1 we step by char, which is wrong for combining marks
/// but acceptable until a higher-fidelity grapheme iterator lands.)
pub fn apply_text_input_key(value: &str, cursor: usize, key: &str) -> (String, usize) {
    let cursor = cursor.min(value.len());
    match key {
        "Backspace" => {
            if cursor == 0 {
                return (value.to_string(), 0);
            }
            // Find the start of the previous char.
            let mut prev = cursor - 1;
            while prev > 0 && !value.is_char_boundary(prev) {
                prev -= 1;
            }
            let mut new_value = String::with_capacity(value.len() - (cursor - prev));
            new_value.push_str(&value[..prev]);
            new_value.push_str(&value[cursor..]);
            (new_value, prev)
        }
        "Delete" => {
            if cursor >= value.len() {
                return (value.to_string(), cursor);
            }
            // Find the start of the next char.
            let mut next = cursor + 1;
            while next < value.len() && !value.is_char_boundary(next) {
                next += 1;
            }
            let mut new_value = String::with_capacity(value.len() - (next - cursor));
            new_value.push_str(&value[..cursor]);
            new_value.push_str(&value[next..]);
            (new_value, cursor)
        }
        "Left" => {
            if cursor == 0 {
                return (value.to_string(), 0);
            }
            let mut prev = cursor - 1;
            while prev > 0 && !value.is_char_boundary(prev) {
                prev -= 1;
            }
            (value.to_string(), prev)
        }
        "Right" => {
            if cursor >= value.len() {
                return (value.to_string(), value.len());
            }
            let mut next = cursor + 1;
            while next < value.len() && !value.is_char_boundary(next) {
                next += 1;
            }
            (value.to_string(), next)
        }
        "Home" => (value.to_string(), 0),
        "End" => (value.to_string(), value.len()),
        _ => (value.to_string(), cursor),
    }
}

/// Apply a non-printable editing key to a multi-line `(value,
/// cursor)` pair, returning `(new_value, new_cursor)`. `cursor` is
/// a UTF-8 byte offset clamped to `[0, value.len()]`.
///
/// Recognised keys: `"Backspace"`, `"Delete"`, `"Left"`, `"Right"`,
/// `"Home"`, `"End"`, `"Up"`, `"Down"`, `"Enter"`. Any other key
/// string is a no-op.
///
/// Compared to `apply_text_input_key`:
/// * `"Home"`/`"End"` jump to the start/end of the *current line*
///   (the line containing `cursor`), not the whole buffer.
/// * `"Up"`/`"Down"` move between lines, preserving the byte column
///   within the line where possible (clamped to each target line's
///   length).
/// * `"Enter"` inserts a `'\n'` at the cursor — TextArea's defining
///   behaviour, separate from the smart-key dispatch path which also
///   funnels Enter here when the focused widget is a TextArea.
///
/// `Left`/`Right`/`Backspace`/`Delete` are unchanged from
/// `TextInput` semantics and respect UTF-8 char boundaries (an
/// embedded `\n` is just another char that gets crossed by these
/// keys).
pub fn apply_text_area_key(value: &str, cursor: usize, key: &str) -> (String, usize) {
    let cursor = cursor.min(value.len());
    match key {
        "Backspace" | "Delete" | "Left" | "Right" => apply_text_input_key(value, cursor, key),
        "Home" => (value.to_string(), line_start(value, cursor)),
        "End" => (value.to_string(), line_end(value, cursor)),
        "Up" => {
            let (line_start, col) = line_start_and_col(value, cursor);
            if line_start == 0 {
                // No previous line — clamp to start.
                return (value.to_string(), 0);
            }
            // Previous line spans [prev_start, line_start - 1]; the
            // newline at byte `line_start - 1` separates the two.
            let prev_end = line_start - 1;
            let prev_start = line_start_at(value, prev_end);
            let prev_len = prev_end - prev_start;
            let new_col = col.min(prev_len);
            let new_cursor = clamp_to_char_boundary(value, prev_start + new_col);
            (value.to_string(), new_cursor)
        }
        "Down" => {
            let (line_start, col) = line_start_and_col(value, cursor);
            let cur_line_end = line_end_at(value, line_start);
            if cur_line_end >= value.len() {
                // No next line — clamp to end.
                return (value.to_string(), value.len());
            }
            let next_start = cur_line_end + 1;
            let next_end = line_end_at(value, next_start);
            let next_len = next_end - next_start;
            let new_col = col.min(next_len);
            let new_cursor = clamp_to_char_boundary(value, next_start + new_col);
            (value.to_string(), new_cursor)
        }
        "Enter" => {
            let mut new_value = String::with_capacity(value.len() + 1);
            new_value.push_str(&value[..cursor]);
            new_value.push('\n');
            new_value.push_str(&value[cursor..]);
            (new_value, cursor + 1)
        }
        _ => (value.to_string(), cursor),
    }
}

/// Byte index of the start of the line containing `cursor`. The
/// "start of a line" is either byte 0 or one byte past the most
/// recent `\n`.
fn line_start(value: &str, cursor: usize) -> usize {
    line_start_at(value, cursor)
}

/// Byte index of the end of the line containing `cursor` — the
/// position of the next `\n`, or `value.len()` if there is none
/// after `cursor`.
fn line_end(value: &str, cursor: usize) -> usize {
    line_end_at(value, cursor)
}

fn line_start_at(value: &str, byte: usize) -> usize {
    let bytes = value.as_bytes();
    let mut i = byte;
    while i > 0 && bytes[i - 1] != b'\n' {
        i -= 1;
    }
    i
}

fn line_end_at(value: &str, byte: usize) -> usize {
    let bytes = value.as_bytes();
    let mut i = byte;
    while i < bytes.len() && bytes[i] != b'\n' {
        i += 1;
    }
    i
}

/// Returns `(line_start, col_in_bytes)` for `cursor`. `col_in_bytes`
/// is `cursor - line_start`.
fn line_start_and_col(value: &str, cursor: usize) -> (usize, usize) {
    let s = line_start_at(value, cursor);
    (s, cursor - s)
}

/// Snap `byte` down to the nearest UTF-8 char boundary at or before
/// it. Used when projecting an Up/Down byte-column into a line that
/// might split a multi-byte char at the target column.
fn clamp_to_char_boundary(value: &str, byte: usize) -> usize {
    let mut i = byte.min(value.len());
    while i > 0 && !value.is_char_boundary(i) {
        i -= 1;
    }
    i
}

/// In-place mutate a `Toggle`'s `checked` field by walking the
/// spec tree and matching on `widget_key`. Used by the
/// `WidgetMutate::SetChecked` IPC fast path.
///
/// Returns true when a matching Toggle was found and updated.
pub fn set_toggle_checked_in_spec(
    spec: &mut WidgetSpec,
    widget_key: &str,
    new_checked: bool,
) -> bool {
    if widget_key.is_empty() {
        return false;
    }
    match spec {
        WidgetSpec::Row { children, .. } | WidgetSpec::Col { children, .. } => {
            for c in children {
                if set_toggle_checked_in_spec(c, widget_key, new_checked) {
                    return true;
                }
            }
            false
        }
        WidgetSpec::Toggle { checked, key, .. } => {
            if key.as_deref() == Some(widget_key) {
                *checked = new_checked;
                true
            } else {
                false
            }
        }
        _ => false,
    }
}

/// In-place mutate a `List`'s `items` and `item_keys` fields.
/// Returns true when a matching List was found and updated.
pub fn set_list_items_in_spec(
    spec: &mut WidgetSpec,
    widget_key: &str,
    new_items: Vec<TextPropertyEntry>,
    new_item_keys: Vec<String>,
) -> bool {
    if widget_key.is_empty() {
        return false;
    }
    match spec {
        WidgetSpec::Row { children, .. } | WidgetSpec::Col { children, .. } => {
            // Workaround: take ownership of items to avoid double-borrow on recursive call
            for c in children.iter_mut() {
                if c.contains_key(widget_key) {
                    return set_list_items_in_spec(c, widget_key, new_items, new_item_keys);
                }
            }
            false
        }
        WidgetSpec::List {
            items,
            item_keys,
            key,
            ..
        } => {
            if key.as_deref() == Some(widget_key) {
                *items = new_items;
                *item_keys = new_item_keys;
                true
            } else {
                false
            }
        }
        _ => false,
    }
}

/// In-place mutate a `Tree`'s `nodes` and `item_keys` fields.
/// Returns true when a matching Tree was found and updated.
///
/// Note: this does *not* touch instance state (selected_index,
/// scroll, expanded_keys). The renderer will clamp the previous
/// selection to a now-visible node and orphan-discard expanded
/// keys that no longer match any item key on the next render.
pub fn set_tree_nodes_in_spec(
    spec: &mut WidgetSpec,
    widget_key: &str,
    new_nodes: Vec<TreeNode>,
    new_item_keys: Vec<String>,
) -> bool {
    if widget_key.is_empty() {
        return false;
    }
    match spec {
        WidgetSpec::Row { children, .. } | WidgetSpec::Col { children, .. } => {
            for c in children.iter_mut() {
                if c.contains_key(widget_key) {
                    return set_tree_nodes_in_spec(c, widget_key, new_nodes, new_item_keys);
                }
            }
            false
        }
        WidgetSpec::Tree {
            nodes,
            item_keys,
            key,
            ..
        } => {
            if key.as_deref() == Some(widget_key) {
                *nodes = new_nodes;
                *item_keys = new_item_keys;
                true
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Stamp `Some(checked)` onto every `TreeNode` whose item-key
/// appears in `keys`. Used by `WidgetMutation::SetCheckedKeys` —
/// the host writes the new checkbox state into the spec so the
/// next render reflects it without round-tripping through the
/// plugin. Nodes not in `keys` are unchanged. Nodes whose
/// existing `checked` is `None` are left as `None` (a node only
/// becomes checkable by the plugin emitting `Some(_)` in the
/// spec).
///
/// Returns true if the named tree was found.
pub fn set_tree_checked_keys_in_spec(
    spec: &mut WidgetSpec,
    widget_key: &str,
    checked: bool,
    keys: &[String],
) -> bool {
    if widget_key.is_empty() {
        return false;
    }
    match spec {
        WidgetSpec::Row { children, .. } | WidgetSpec::Col { children, .. } => {
            for c in children.iter_mut() {
                if c.contains_key(widget_key) {
                    return set_tree_checked_keys_in_spec(c, widget_key, checked, keys);
                }
            }
            false
        }
        WidgetSpec::Tree {
            nodes,
            item_keys,
            key,
            ..
        } => {
            if key.as_deref() != Some(widget_key) {
                return false;
            }
            let target: std::collections::HashSet<&str> = keys.iter().map(String::as_str).collect();
            for (i, node) in nodes.iter_mut().enumerate() {
                if node.checked.is_none() {
                    continue;
                }
                let item_key = item_keys.get(i).map(String::as_str).unwrap_or("");
                if !item_key.is_empty() && target.contains(item_key) {
                    node.checked = Some(checked);
                }
            }
            true
        }
        _ => false,
    }
}

/// Resolve the absolute `nodes` index of the parent of `child_idx`
/// in a `Tree`. The parent of a node at depth `d` is the most recent
/// earlier node at depth `d - 1`. Returns `None` for top-level nodes
/// (depth 0) and for out-of-range indices.
pub fn tree_parent_index(nodes: &[TreeNode], child_idx: usize) -> Option<usize> {
    let child = nodes.get(child_idx)?;
    if child.depth == 0 {
        return None;
    }
    let target_depth = child.depth - 1;
    nodes[..child_idx]
        .iter()
        .enumerate()
        .rev()
        .find(|(_, n)| n.depth == target_depth)
        .map(|(i, _)| i)
}

/// Recursive helper for `set_*_in_spec` — does this
/// subtree contain a widget (any kind) with `widget_key`?
trait ContainsKey {
    fn contains_key(&self, widget_key: &str) -> bool;
}

impl ContainsKey for WidgetSpec {
    fn contains_key(&self, widget_key: &str) -> bool {
        match self {
            WidgetSpec::Row { children, .. } | WidgetSpec::Col { children, .. } => {
                children.iter().any(|c| c.contains_key(widget_key))
            }
            WidgetSpec::Toggle { key, .. }
            | WidgetSpec::Button { key, .. }
            | WidgetSpec::Text { key, .. }
            | WidgetSpec::List { key, .. }
            | WidgetSpec::Tree { key, .. } => key.as_deref() == Some(widget_key),
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn toggle_with_key(k: &str) -> WidgetSpec {
        WidgetSpec::Toggle {
            checked: false,
            label: "T".into(),
            focused: false,
            key: Some(k.into()),
        }
    }

    #[test]
    fn find_widget_by_key_finds_top_level_match() {
        let spec = toggle_with_key("a");
        assert!(find_widget_by_key(&spec, "a").is_some());
        assert!(find_widget_by_key(&spec, "b").is_none());
    }

    #[test]
    fn find_widget_by_key_recurses_into_row() {
        let spec = WidgetSpec::Row {
            children: vec![toggle_with_key("a"), toggle_with_key("b")],
            key: None,
        };
        assert!(find_widget_by_key(&spec, "b").is_some());
    }

    #[test]
    fn find_widget_by_key_returns_none_for_empty_target() {
        let spec = toggle_with_key("a");
        assert!(find_widget_by_key(&spec, "").is_none());
    }

    #[test]
    fn backspace_at_start_is_noop() {
        assert_eq!(
            apply_text_input_key("hello", 0, "Backspace"),
            ("hello".into(), 0)
        );
    }

    #[test]
    fn backspace_in_middle_removes_previous_char() {
        assert_eq!(
            apply_text_input_key("hello", 3, "Backspace"),
            ("helo".into(), 2)
        );
    }

    #[test]
    fn backspace_at_end_removes_last_char() {
        assert_eq!(
            apply_text_input_key("hello", 5, "Backspace"),
            ("hell".into(), 4)
        );
    }

    #[test]
    fn delete_at_end_is_noop() {
        assert_eq!(
            apply_text_input_key("hello", 5, "Delete"),
            ("hello".into(), 5)
        );
    }

    #[test]
    fn delete_in_middle_removes_next_char() {
        assert_eq!(
            apply_text_input_key("hello", 2, "Delete"),
            ("helo".into(), 2)
        );
    }

    #[test]
    fn left_decrements_cursor() {
        assert_eq!(apply_text_input_key("abc", 2, "Left"), ("abc".into(), 1));
    }

    #[test]
    fn right_increments_cursor_until_end() {
        assert_eq!(apply_text_input_key("abc", 1, "Right"), ("abc".into(), 2));
        assert_eq!(apply_text_input_key("abc", 3, "Right"), ("abc".into(), 3));
    }

    #[test]
    fn home_jumps_to_zero() {
        assert_eq!(apply_text_input_key("abc", 2, "Home"), ("abc".into(), 0));
    }

    #[test]
    fn end_jumps_to_value_len() {
        assert_eq!(apply_text_input_key("abc", 1, "End"), ("abc".into(), 3));
    }

    #[test]
    fn unknown_key_is_noop() {
        assert_eq!(apply_text_input_key("abc", 1, "Wat"), ("abc".into(), 1));
    }

    // ---- apply_text_char (IME-commit / printable insertion) ----

    #[test]
    fn apply_text_char_inserts_ascii_at_cursor() {
        assert_eq!(apply_text_char("Hello", 5, "!"), ("Hello!".into(), 6));
        assert_eq!(apply_text_char("Hxllo", 1, "e"), ("Hexllo".into(), 2));
    }

    #[test]
    fn apply_text_char_inserts_multibyte_codepoint() {
        // "你" is 3 bytes (0xE4 0xBD 0xA0).
        let (v, c) = apply_text_char("", 0, "你");
        assert_eq!(v, "你");
        assert_eq!(c, 3);
    }

    #[test]
    fn apply_text_char_ime_commit_step_by_step() {
        // IME commits "你好" as two consecutive single-codepoint
        // events (the typical Pinyin → CJK flow).
        let (v, c) = apply_text_char("", 0, "你");
        let (v, c) = apply_text_char(&v, c, "好");
        assert_eq!(v, "你好");
        assert_eq!(c, 6);
    }

    #[test]
    fn apply_text_char_ime_commit_multi_codepoint_in_one_event() {
        // Some IMEs commit the whole word in one push — must work
        // identically to step-by-step.
        let (v, c) = apply_text_char("", 0, "你好");
        assert_eq!(v, "你好");
        assert_eq!(c, 6);
    }

    #[test]
    fn apply_text_char_inserts_into_middle_of_existing_text() {
        // "He|llo" — cursor at byte 2, insert "你". Expected:
        // "He你llo", cursor at byte 5 (2 + 3).
        let (v, c) = apply_text_char("Hello", 2, "你");
        assert_eq!(v, "He你llo");
        assert_eq!(c, 5);
    }

    #[test]
    fn apply_text_char_snaps_mid_multibyte_cursor_down_to_boundary() {
        // Cursor at byte 1 of "你" (mid-multibyte) — snap down to
        // byte 0, then insert.
        let (v, c) = apply_text_char("你", 1, "X");
        assert_eq!(v, "X你");
        assert_eq!(c, 1);
    }

    #[test]
    fn apply_text_char_clamps_cursor_past_end() {
        let (v, c) = apply_text_char("ab", 99, "c");
        assert_eq!(v, "abc");
        assert_eq!(c, 3);
    }

    #[test]
    fn backspace_handles_multibyte_chars() {
        // "héllo" — 'é' is 2 bytes (0xC3 0xA9).
        let s = "héllo";
        // Cursor after 'é' (byte 3). Backspace removes 'é'.
        let (new_value, new_cursor) = apply_text_input_key(s, 3, "Backspace");
        assert_eq!(new_value, "hllo");
        assert_eq!(new_cursor, 1);
    }

    #[test]
    fn left_handles_multibyte_chars() {
        let s = "héllo";
        // From byte 3 (after 'é'), Left goes to byte 1 (before 'é').
        let (_, cursor) = apply_text_input_key(s, 3, "Left");
        assert_eq!(cursor, 1);
    }

    #[test]
    fn right_handles_multibyte_chars() {
        let s = "héllo";
        // From byte 1 (before 'é'), Right goes to byte 3 (after 'é').
        let (_, cursor) = apply_text_input_key(s, 1, "Right");
        assert_eq!(cursor, 3);
    }

    fn node(text: &str, depth: u32, has_children: bool) -> TreeNode {
        TreeNode {
            text: TextPropertyEntry::text(text),
            depth,
            has_children,
            checked: None,
        }
    }

    #[test]
    fn tree_parent_index_top_level_returns_none() {
        let nodes = vec![node("root", 0, true)];
        assert!(tree_parent_index(&nodes, 0).is_none());
    }

    #[test]
    fn tree_parent_index_finds_immediate_parent() {
        let nodes = vec![
            node("root", 0, true),
            node("child", 1, false),
            node("child2", 1, false),
        ];
        assert_eq!(tree_parent_index(&nodes, 1), Some(0));
        assert_eq!(tree_parent_index(&nodes, 2), Some(0));
    }

    #[test]
    fn tree_parent_index_skips_intermediate_siblings() {
        // root, child, grandchild → grandchild's parent is child (idx 1).
        let nodes = vec![
            node("root", 0, true),
            node("child", 1, true),
            node("grand", 2, false),
        ];
        assert_eq!(tree_parent_index(&nodes, 2), Some(1));
    }

    #[test]
    fn tree_parent_index_finds_parent_across_unrelated_subtree() {
        // root_a, child_a, root_b, child_b — child_b's parent is root_b (idx 2),
        // not root_a.
        let nodes = vec![
            node("a", 0, true),
            node("a.0", 1, false),
            node("b", 0, true),
            node("b.0", 1, false),
        ];
        assert_eq!(tree_parent_index(&nodes, 3), Some(2));
    }

    #[test]
    fn set_tree_nodes_in_spec_replaces_nodes() {
        let mut spec = WidgetSpec::Tree {
            nodes: vec![node("old", 0, false)],
            item_keys: vec!["k0".into()],
            selected_index: -1,
            visible_rows: 5,
            expanded_keys: vec![],
            checkable: false,
            key: Some("t".into()),
        };
        let new_nodes = vec![node("new1", 0, false), node("new2", 0, false)];
        let new_keys = vec!["a".to_string(), "b".to_string()];
        let ok = set_tree_nodes_in_spec(&mut spec, "t", new_nodes.clone(), new_keys.clone());
        assert!(ok);
        match &spec {
            WidgetSpec::Tree {
                nodes, item_keys, ..
            } => {
                assert_eq!(nodes.len(), 2);
                assert_eq!(item_keys, &new_keys);
            }
            _ => unreachable!(),
        }
    }

    // ---- TextArea key tests --------------------------------------------

    #[test]
    fn text_area_enter_inserts_newline_at_cursor() {
        let (v, c) = apply_text_area_key("hello", 2, "Enter");
        assert_eq!(v, "he\nllo");
        assert_eq!(c, 3);
    }

    #[test]
    fn text_area_enter_at_end_appends_newline() {
        let (v, c) = apply_text_area_key("ab", 2, "Enter");
        assert_eq!(v, "ab\n");
        assert_eq!(c, 3);
    }

    #[test]
    fn text_area_left_right_share_text_input_semantics() {
        assert_eq!(apply_text_area_key("abc", 2, "Left"), ("abc".into(), 1));
        assert_eq!(apply_text_area_key("abc", 1, "Right"), ("abc".into(), 2));
    }

    #[test]
    fn text_area_backspace_can_delete_a_newline() {
        // Cursor right after the `\n` between line 0 and 1.
        let (v, c) = apply_text_area_key("ab\ncd", 3, "Backspace");
        assert_eq!(v, "abcd");
        assert_eq!(c, 2);
    }

    #[test]
    fn text_area_home_jumps_to_line_start_not_buffer_start() {
        // Cursor is on line 1 (after "\n"). Home → byte 3 (line 1's
        // start), not byte 0.
        let (v, c) = apply_text_area_key("ab\ncd", 4, "Home");
        assert_eq!(v, "ab\ncd");
        assert_eq!(c, 3);
    }

    #[test]
    fn text_area_end_jumps_to_line_end_not_buffer_end() {
        // Cursor at start of line 0. End → byte 2 (line 0's end),
        // not the value's full length.
        let (v, c) = apply_text_area_key("ab\ncd", 0, "End");
        assert_eq!(v, "ab\ncd");
        assert_eq!(c, 2);
    }

    #[test]
    fn text_area_up_moves_to_previous_line_preserving_column() {
        // "abcd\nef" — cursor at byte 7 (col 2 on line 1, after 'f').
        // Up → byte 2 (col 2 on line 0).
        let (v, c) = apply_text_area_key("abcd\nef", 7, "Up");
        assert_eq!(v, "abcd\nef");
        assert_eq!(c, 2);
    }

    #[test]
    fn text_area_up_clamps_column_to_short_target_line() {
        // "ab\nxyz" — cursor at byte 6 (col 3 on line 1).
        // Up → end of line 0 (byte 2), since col 3 exceeds line 0's
        // length 2.
        let (v, c) = apply_text_area_key("ab\nxyz", 6, "Up");
        assert_eq!(c, 2);
        assert_eq!(v, "ab\nxyz");
    }

    #[test]
    fn text_area_up_at_top_line_clamps_to_buffer_start() {
        let (_, c) = apply_text_area_key("abc\ndef", 2, "Up");
        assert_eq!(c, 0);
    }

    #[test]
    fn text_area_down_moves_to_next_line_preserving_column() {
        // "abcd\nefgh" — cursor at byte 2 (col 2 on line 0).
        // Down → byte 7 (col 2 on line 1, which starts at byte 5).
        let (_, c) = apply_text_area_key("abcd\nefgh", 2, "Down");
        assert_eq!(c, 7);
    }

    #[test]
    fn text_area_down_at_last_line_clamps_to_buffer_end() {
        let (v, c) = apply_text_area_key("abc\ndef", 5, "Down");
        assert_eq!(v, "abc\ndef");
        assert_eq!(c, 7);
    }

    #[test]
    fn text_area_unknown_key_is_noop() {
        assert_eq!(apply_text_area_key("abc", 1, "Wat"), ("abc".into(), 1));
    }

    #[test]
    fn text_area_up_clamps_to_char_boundary() {
        // Two-byte char `é` at start of line 1. Cursor at line 1,
        // byte 1 (start of the second `\xa9` byte of `é`). Up
        // shouldn't land mid-multibyte-char on line 0 either.
        // Use "aé\nbé" — line 0 = "aé" (3 bytes), line 1 = "bé"
        // (3 bytes). Cursor at byte 5 (mid-line 1 between 'b' and
        // 'é'). Up → col 1 on line 0, byte 1 (between 'a' and 'é').
        let (_, c) = apply_text_area_key("aé\nbé", 5, "Up");
        assert_eq!(c, 1);
    }

    #[test]
    fn set_tree_checked_keys_in_spec_flips_only_named_keys() {
        let mut a = node("a", 0, false);
        a.checked = Some(true);
        let mut b = node("b", 0, false);
        b.checked = Some(true);
        let mut c = node("c", 0, false);
        c.checked = Some(true);
        let mut spec = WidgetSpec::Tree {
            nodes: vec![a, b, c],
            item_keys: vec!["k_a".into(), "k_b".into(), "k_c".into()],
            selected_index: -1,
            visible_rows: 5,
            expanded_keys: vec![],
            checkable: true,
            key: Some("t".into()),
        };
        let ok = set_tree_checked_keys_in_spec(
            &mut spec,
            "t",
            false,
            &["k_a".to_string(), "k_c".to_string()],
        );
        assert!(ok);
        match &spec {
            WidgetSpec::Tree { nodes, .. } => {
                assert_eq!(nodes[0].checked, Some(false));
                assert_eq!(nodes[1].checked, Some(true), "untouched");
                assert_eq!(nodes[2].checked, Some(false));
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn set_tree_checked_keys_in_spec_skips_nodes_without_checkbox() {
        // Nodes with `checked: None` must stay None even when their
        // key is in the target list — the plugin's intent ("this
        // node has no checkbox") is preserved.
        let n_with = {
            let mut n = node("checked", 0, false);
            n.checked = Some(true);
            n
        };
        let n_without = node("no-checkbox", 0, false); // checked: None
        let mut spec = WidgetSpec::Tree {
            nodes: vec![n_with, n_without],
            item_keys: vec!["k0".into(), "k1".into()],
            selected_index: -1,
            visible_rows: 5,
            expanded_keys: vec![],
            checkable: true,
            key: Some("t".into()),
        };
        let _ok = set_tree_checked_keys_in_spec(
            &mut spec,
            "t",
            false,
            &["k0".to_string(), "k1".to_string()],
        );
        match &spec {
            WidgetSpec::Tree { nodes, .. } => {
                assert_eq!(nodes[0].checked, Some(false));
                assert_eq!(nodes[1].checked, None);
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn set_tree_nodes_in_spec_returns_false_for_unknown_key() {
        let mut spec = WidgetSpec::Tree {
            nodes: vec![node("a", 0, false)],
            item_keys: vec!["k".into()],
            selected_index: -1,
            visible_rows: 5,
            expanded_keys: vec![],
            checkable: false,
            key: Some("real".into()),
        };
        assert!(!set_tree_nodes_in_spec(&mut spec, "wrong", vec![], vec![]));
    }
}
