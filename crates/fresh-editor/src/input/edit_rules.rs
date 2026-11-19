//! Edit rules, applied inside Enter / Tab / Shift+Tab.
//!
//! The rules themselves — patterns, templates, the registry — live in
//! [`fresh_core::edit_rules`] and know nothing about buffers. This module is
//! the buffer side: for each cursor it reads the line around the cursor,
//! asks the buffer's rules for a rewrite — its language's `edit_rules`
//! setting first, then the sets plugins registered for the language — and
//! turns the answer into the key's own events, so a rule's edit is part of
//! the keystroke (one undo step, applied before the next key is read).

use crate::model::cursor::Cursors;
use crate::model::event::{CursorId, Event};
use crate::model::virtual_space::virtual_gap_text;
use crate::state::EditorState;
use fresh_core::api::EditRuleTrigger;
use fresh_core::edit_rules::{BufferEditRules, EditRuleRegistry, LineContext, RuleReach};

/// How far a line may run either side of the cursor for rules to look at
/// it. A longer line (minified text, a data dump) just gets the key's
/// normal behaviour rather than a scan of the whole line on every keystroke.
const LINE_CAP: usize = 64 * 1024;

/// What [`apply_edit_rules`] did.
pub(crate) struct RuleEdits {
    /// The cursors no rule handled, for the key's normal handler; `None`
    /// when every cursor was handled.
    pub rest: Option<Cursors>,
    /// The rules' edits as `(position, delete_len, insert_len)` in the
    /// buffer as it was before the key, for [`shift_default_moves`].
    pub edits: Vec<(usize, usize, usize)>,
}

/// The line holding `pos`: its start and its end before the line break.
/// `None` when either lies further than [`LINE_CAP`] away.
fn line_bounds(state: &mut EditorState, pos: usize) -> Option<(usize, usize)> {
    let start = state.buffer.prev_line_start_within(pos, LINE_CAP)?;
    let len = state.buffer.len();
    let end = match state.buffer.next_line_start_within(pos, LINE_CAP) {
        Some(next) => {
            let mut end = next - 1;
            if end > pos && state.buffer.slice_bytes(end - 1..end).first() == Some(&b'\r') {
                end -= 1;
            }
            end
        }
        None if len - pos <= LINE_CAP => len,
        None => return None,
    };
    Some((start, end))
}

/// The buffer's bytes in `start..end` as text, or `None` when they are not
/// UTF-8 (or can't be read).
fn utf8_range(state: &mut EditorState, start: usize, end: usize) -> Option<String> {
    let bytes = state.buffer.get_text_range_mut(start, end - start).ok()?;
    String::from_utf8(bytes).ok()
}

/// Byte length of the longest common prefix of `a` and `b`, on a character
/// boundary of both.
fn common_prefix_len(a: &str, b: &str) -> usize {
    a.char_indices()
        .zip(b.chars())
        .find(|((_, ca), cb)| ca != cb)
        .map_or(a.len().min(b.len()), |((i, _), _)| i)
}

/// Apply the edit rules for the buffer and `trigger` at each cursor they can
/// handle, pushing their events onto `events`: the buffer's configured rules
/// (`buffer_settings.edit_rules`), then, when the caller passes the
/// registry, the sets plugins registered for its language. `reach` says
/// which rewrites the key takes (all of them, for the user's own key).
///
/// A cursor is handled when it has no selection, is not in virtual space,
/// shares its line with no other cursor (or another cursor's selection), and
/// a rule applies there. `None` when no cursor was handled: the key then
/// does exactly what it does without rules.
pub(crate) fn apply_edit_rules(
    state: &mut EditorState,
    cursors: &Cursors,
    events: &mut Vec<Event>,
    trigger: EditRuleTrigger,
    registry: Option<&EditRuleRegistry>,
    reach: RuleReach,
) -> Option<RuleEdits> {
    // Held apart from `state`, which the loop below reads mutably.
    let configured = std::sync::Arc::clone(&state.buffer_settings.edit_rules);
    let language = state.language.clone();
    let rules = BufferEditRules {
        language: &language,
        configured: &configured,
        registry,
    };
    if !rules.has_rules(trigger) {
        return None;
    }
    let vs_mode = state.buffer_settings.virtual_space;
    let line_ending = state.buffer.line_ending().insertion_str();

    // What each cursor touches: its selection, or just its position.
    let mut spans: Vec<(usize, usize, CursorId)> = cursors
        .iter()
        .map(|(id, c)| {
            let r = c.selection_range().unwrap_or(c.position..c.position);
            (r.start, r.end, id)
        })
        .collect();
    spans.sort_by_key(|&(start, _, _)| start);

    // Candidates, handled from the end of the buffer back, so each edit
    // leaves the positions of the ones still to come alone.
    let mut handled: Vec<CursorId> = Vec::new();
    let mut edits: Vec<(usize, usize, usize)> = Vec::new();
    let mut max_end_before = 0usize;
    let mut candidates = Vec::new();
    for (i, &(start, end, id)) in spans.iter().enumerate() {
        let earlier_end = (i > 0).then_some(max_end_before);
        max_end_before = max_end_before.max(end);
        if start != end {
            continue;
        }
        let Some(cursor) = cursors.get(id) else {
            continue;
        };
        if !virtual_gap_text(vs_mode, &state.buffer, cursor, line_ending).is_empty() {
            continue;
        }
        let pos = cursor.position;
        let Some((line_start, line_end)) = line_bounds(state, pos) else {
            continue;
        };
        // Alone on its line: no earlier span reaches the line start, and the
        // next span starts past the line end.
        let crowded_before = earlier_end.is_some_and(|e| e >= line_start);
        let crowded_after = spans
            .get(i + 1)
            .is_some_and(|&(next_start, _, _)| next_start <= line_end);
        if crowded_before || crowded_after {
            continue;
        }
        candidates.push((id, pos, line_start, line_end, cursor.sticky_column));
    }

    let indent_unit = if state.buffer_settings.use_tabs {
        "\t".to_string()
    } else {
        " ".repeat(state.buffer_settings.tab_size)
    };
    for (id, pos, line_start, line_end, sticky) in candidates.into_iter().rev() {
        // The rules see text, but their edits are byte offsets into the
        // buffer: a line that isn't UTF-8 either side of the cursor (or a
        // cursor inside a character) gets the key's normal behaviour, rather
        // than offsets taken from a lossy copy with a different length.
        let (Some(before), Some(after)) = (
            utf8_range(state, line_start, pos),
            utf8_range(state, pos, line_end),
        ) else {
            continue;
        };
        let ctx = LineContext {
            before: &before,
            after: &after,
            indent_unit: &indent_unit,
            tab_width: state.buffer_settings.tab_size,
            line_ending,
        };
        let Some(edit) = rules.apply_within(trigger, &ctx, reach) else {
            continue;
        };
        handled.push(id);

        // The text after the cursor first (higher up the buffer), then the
        // text before it, whose insert puts the cursor where it belongs.
        let mut after_inserted = false;
        if edit.after != after {
            if !after.is_empty() {
                events.push(Event::Delete {
                    range: pos..line_end,
                    deleted_text: state.get_edit_text(pos, line_end),
                    cursor_id: id,
                });
            }
            if !edit.after.is_empty() {
                events.push(Event::Insert {
                    position: pos,
                    text: edit.after.clone().into(),
                    cursor_id: id,
                });
                after_inserted = true;
            }
            edits.push((pos, after.len(), edit.after.len()));
        }
        let keep = common_prefix_len(&before, &edit.before);
        let from = line_start + keep;
        let inserted = &edit.before[keep..];
        if from < pos {
            events.push(Event::Delete {
                range: from..pos,
                deleted_text: state.get_edit_text(from, pos),
                cursor_id: id,
            });
        }
        if !inserted.is_empty() {
            events.push(Event::Insert {
                position: from,
                text: inserted.to_string().into(),
                cursor_id: id,
            });
        }
        if from < pos || !inserted.is_empty() {
            edits.push((from, pos - from, inserted.len()));
        } else if after_inserted {
            // Only the text after the cursor changed: its insert would carry
            // the cursor past it, so put the cursor back.
            events.push(Event::MoveCursor {
                cursor_id: id,
                old_position: pos,
                new_position: pos,
                old_anchor: None,
                new_anchor: None,
                old_sticky_column: sticky,
                new_sticky_column: None,
            });
        }
    }

    if handled.is_empty() {
        return None;
    }
    let rest = (handled.len() < cursors.count()).then(|| {
        let mut rest = cursors.clone();
        for id in &handled {
            rest.remove(*id);
        }
        rest
    });
    Some(RuleEdits { rest, edits })
}

/// Shift the `MoveCursor` targets the key's normal handler computed for the
/// cursors no rule handled (`rest`, as they were before the key;
/// `default_events`), by the rules' `edits`.
///
/// A handler that moves cursors explicitly (Shift+Tab's dedent) computes
/// their new positions from its own edits only. The bulk apply
/// (`apply_events_as_bulk_edit`) takes such a target as final, unless the
/// cursor also has an insert at its own position from before the key (Enter's
/// bracket expansion, the auto-close shape): it then shifts the target for
/// every edit below that position itself, so such a move is left alone here
/// or it would be shifted twice. The test is the applier's own: an insert at
/// the cursor's position before the key, whatever the move's `old_position`.
/// The rules' edits are on other lines, so each one lies wholly before or
/// after the cursor, and the shift is the net length of those before it.
pub(crate) fn shift_default_moves(
    default_events: &mut [Event],
    rest: &Cursors,
    edits: &[(usize, usize, usize)],
) {
    if edits.is_empty() {
        return;
    }
    let shift_at = |pos: usize| -> isize {
        edits
            .iter()
            .filter(|(at, _, _)| *at < pos)
            .map(|(_, del, ins)| *ins as isize - *del as isize)
            .sum()
    };
    let inserts_at: Vec<(CursorId, usize)> = default_events
        .iter()
        .filter_map(|e| match e {
            Event::Insert {
                position,
                cursor_id,
                ..
            } => Some((*cursor_id, *position)),
            _ => None,
        })
        .collect();
    for event in default_events.iter_mut() {
        if let Event::MoveCursor {
            cursor_id,
            old_position,
            new_position,
            new_anchor,
            ..
        } = event
        {
            let original = rest.get(*cursor_id).map(|c| c.position);
            if original.is_some_and(|p| inserts_at.contains(&(*cursor_id, p))) {
                continue;
            }
            let shift = shift_at(original.unwrap_or(*old_position));
            *new_position = new_position.saturating_add_signed(shift);
            *new_anchor = new_anchor.map(|a| a.saturating_add_signed(shift));
        }
    }
}
