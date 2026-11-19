//! Edit rules, applied inside Enter / Tab / Shift+Tab.
//!
//! The rules themselves — patterns, templates, the registry — live in
//! [`fresh_core::edit_rules`] and know nothing about buffers. This module is
//! the buffer side: for each cursor it reads the line around the cursor,
//! asks the rules of the language at the cursor for a rewrite — that
//! language's `edit_rules` setting first, then the sets plugins registered
//! for it — and turns the answer into the key's own events, so a rule's edit
//! is part of the keystroke (one undo step, applied before the next key is
//! read).
//!
//! The language at a cursor is the buffer's, except on a content line of an
//! embedded-language region the buffer's highlighter knows (a fenced code
//! block in Markdown, a `<script>` in Vue): there it is the region's
//! language, and that language's rules and `auto_indent` apply instead
//! ([`cursor_languages`]).

use crate::config::{BufferConfig, Config};
use crate::input::actions::{
    add_move_cursor_event, indent_unit, logical_line_end_within, logical_line_start_within,
};
use crate::model::cursor::Cursors;
use crate::model::event::{CursorId, Event};
use crate::model::virtual_space::virtual_gap_text;
use crate::primitives::detected_language::language_id_for_syntax_name;
use crate::primitives::grammar::GrammarRegistry;
use crate::primitives::highlight_engine::LineLanguage;
use crate::state::EditorState;
use fresh_core::api::EditRuleTrigger;
use fresh_core::edit_rules::{
    BufferEditRules, CompiledEditRule, EditRuleRegistry, LineContext, RuleReach,
};
use std::sync::Arc;

/// What a key needs to resolve the language of an embedded region to its
/// settings: the config, and the grammar catalog that maps a syntax to a
/// language id.
#[derive(Clone, Copy)]
pub struct LanguageLookup<'a> {
    pub config: &'a Config,
    pub registry: &'a GrammarRegistry,
}

/// The settings Enter / Tab / Shift+Tab use on a cursor whose line is a
/// content line of an embedded-language region, in place of the buffer's.
#[derive(Debug, Clone)]
pub(crate) struct RegionLanguage {
    /// The region's language id; `None` when the region named no language
    /// that resolves to one. Such a region is language-less: no edit rules
    /// (neither configured nor plugin-registered) and the indentation of a
    /// buffer with no language.
    pub language: Option<String>,
    /// The syntax the highlighter resolved the region's language to, whose
    /// indentation rules Enter uses.
    pub syntax: Option<String>,
    /// The language's `edit_rules`, compiled (empty when language-less).
    pub edit_rules: Arc<[CompiledEditRule]>,
    /// The language's `auto_indent`, else `editor.auto_indent`.
    pub auto_indent: bool,
}

impl RegionLanguage {
    /// The edit rules that apply in the region: its language's configured
    /// rules, then the sets plugins registered for it — none at all for a
    /// language-less region.
    fn rules<'a>(&'a self, registry: Option<&'a EditRuleRegistry>) -> BufferEditRules<'a> {
        BufferEditRules {
            language: self.language.as_deref().unwrap_or(""),
            configured: &self.edit_rules,
            registry: self.language.as_ref().and(registry),
        }
    }

    /// The settings for a region whose language resolved to `syntax`
    /// (`None`: it named none that resolved).
    fn resolve(syntax: Option<&str>, lookup: &LanguageLookup<'_>) -> Self {
        let language = syntax.and_then(|name| {
            language_id_for_syntax_name(name, lookup.registry, &lookup.config.languages)
        });
        match &language {
            Some(id) => {
                let resolved = BufferConfig::resolve(lookup.config, Some(id));
                Self {
                    syntax: syntax.map(str::to_string),
                    edit_rules: resolved.edit_rules,
                    auto_indent: resolved.auto_indent,
                    language,
                }
            }
            None => Self {
                language: None,
                syntax: None,
                edit_rules: Arc::from([]),
                auto_indent: lookup.config.editor.auto_indent,
            },
        }
    }
}

/// The cursors of one key press whose line is a content line of an
/// embedded-language region, with that region's settings. Every other
/// cursor uses the buffer's language. Empty — and free — for a buffer whose
/// highlighter knows no embedded regions.
#[derive(Debug, Default)]
pub(crate) struct CursorLanguages {
    by_cursor: Vec<(CursorId, Arc<RegionLanguage>)>,
    /// The line each cursor was looked up on, by the position looked up, so
    /// [`apply_edit_rules`] locates it once.
    lines: Vec<(CursorId, usize, LineSpan)>,
}

impl CursorLanguages {
    /// The region language at `cursor`, or `None` for the buffer's own.
    pub fn get(&self, cursor: CursorId) -> Option<&RegionLanguage> {
        self.by_cursor
            .iter()
            .find(|(id, _)| *id == cursor)
            .map(|(_, lang)| &**lang)
    }

    fn iter(&self) -> impl Iterator<Item = &RegionLanguage> {
        self.by_cursor.iter().map(|(_, lang)| &**lang)
    }

    /// The line [`cursor_languages`] located for `cursor` at `pos`.
    fn line(&self, cursor: CursorId, pos: usize) -> Option<LineSpan> {
        self.lines
            .iter()
            .find(|(id, at, _)| *id == cursor && *at == pos)
            .map(|(_, _, line)| *line)
    }
}

/// Resolve the language at each cursor (at the start of its selection, if
/// any), from the embedded regions the buffer's highlighter recorded while
/// parsing: a cursor on a content line of a region gets the region's
/// language's settings ([`RegionLanguage`]); one on a delimiter line,
/// outside every region, or where the highlighter cannot say (its cache
/// does not cover the line, or is not anchored) gets the buffer's.
///
/// Asks nothing of a buffer whose highlighter hosts no regions, or when
/// the caller has no `lookup` (a key issued with no config at hand).
pub(crate) fn cursor_languages(
    state: &mut EditorState,
    cursors: &Cursors,
    lookup: Option<&LanguageLookup<'_>>,
) -> CursorLanguages {
    let mut out = CursorLanguages::default();
    let Some(lookup) = lookup else {
        return out;
    };
    if !state.highlighter.hosts_embedded_regions() {
        return out;
    }
    // One resolution per region language, however many cursors share it.
    let mut resolved: Vec<(Option<String>, Arc<RegionLanguage>)> = Vec::new();
    for (id, cursor) in cursors.iter() {
        let pos = cursor
            .selection_range()
            .map_or(cursor.position, |r| r.start);
        let Some(line) = line_span(state, pos) else {
            continue;
        };
        out.lines.push((id, pos, line));
        let Some(LineLanguage::Embedded(syntax)) =
            state
                .highlighter
                .line_language(&state.buffer, line.start, line.next)
        else {
            continue;
        };
        let lang = match resolved.iter().find(|(s, _)| *s == syntax) {
            Some((_, lang)) => Arc::clone(lang),
            None => {
                let lang = Arc::new(RegionLanguage::resolve(syntax.as_deref(), lookup));
                resolved.push((syntax, Arc::clone(&lang)));
                lang
            }
        };
        out.by_cursor.push((id, lang));
    }
    out
}

/// How far a line may run either side of the cursor for rules to look at
/// it. A longer line (minified text, a data dump) just gets the key's
/// normal behaviour rather than a scan of the whole line on every keystroke.
const LINE_CAP: usize = 64 * 1024;

/// What [`apply_edit_rules`] did.
pub(crate) struct RuleEdits {
    /// The cursors no rule handled, for the key's normal handler; `None`
    /// when every cursor was handled.
    pub rest: Option<Cursors>,
}

/// Where a cursor's line lies in the buffer.
#[derive(Debug, Clone, Copy)]
struct LineSpan {
    /// The line's first byte.
    start: usize,
    /// The end of its text, before the line break; before the cursor when
    /// it sits between the `\r` and `\n` of a CRLF break.
    end: usize,
    /// The start of the next line, past the line break; the buffer's end on
    /// the last line.
    next: usize,
}

/// The line holding `pos`, located with the logical-line searches the
/// `Home` / `End` motions use (which walk the piece tree's line-feed counts
/// rather than copy the line). `None` when either end lies further than
/// [`LINE_CAP`] away.
fn line_span(state: &mut EditorState, pos: usize) -> Option<LineSpan> {
    let start = logical_line_start_within(&mut state.buffer, pos, LINE_CAP)?;
    let (end, next) = logical_line_end_within(&mut state.buffer, pos, LINE_CAP)?;
    Some(LineSpan { start, end, next })
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
///
/// The events are positioned in the buffer as it was before the key, as the
/// bulk applier (`apply_events_as_bulk_edit`) reads them: the rules read
/// the buffer but leave it alone. The key's normal handler rebases its own
/// moves for the cursors left over onto them (`rebase_moves_onto_edits` in
/// [`crate::input::actions`]).
#[allow(clippy::too_many_arguments)]
pub(crate) fn apply_edit_rules(
    state: &mut EditorState,
    cursors: &Cursors,
    events: &mut Vec<Event>,
    trigger: EditRuleTrigger,
    registry: Option<&EditRuleRegistry>,
    reach: RuleReach,
    languages: &CursorLanguages,
) -> Option<RuleEdits> {
    // Held apart from `state`, which the loop below reads mutably.
    let configured = std::sync::Arc::clone(&state.buffer_settings.edit_rules);
    let language = state.language.clone();
    let buffer_rules = BufferEditRules {
        language: &language,
        configured: &configured,
        registry,
    };
    // The rules at a cursor: its region language's, when its line is a
    // region's content (none at all for a language-less region), else the
    // buffer's.
    let rules_for = |id: CursorId| {
        languages
            .get(id)
            .map_or(buffer_rules, |region| region.rules(registry))
    };
    if !buffer_rules.has_rules(trigger)
        && !languages
            .iter()
            .any(|region| region.rules(registry).has_rules(trigger))
    {
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
        let Some(LineSpan {
            start: line_start,
            end: line_end,
            ..
        }) = languages.line(id, pos).or_else(|| line_span(state, pos))
        else {
            continue;
        };
        // A cursor between the `\r` and `\n` of a CRLF line break is not on
        // the line's text (which ends before it): the key's normal behaviour.
        if line_end < pos {
            continue;
        }
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

    let indent_unit = indent_unit(
        state.buffer_settings.use_tabs,
        state.buffer_settings.tab_size,
    );
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
        let Some(edit) = rules_for(id).apply_within(trigger, &ctx, reach) else {
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
        if from == pos && inserted.is_empty() && after_inserted {
            // Only the text after the cursor changed: its insert would carry
            // the cursor past it, so put the cursor back.
            add_move_cursor_event(events, id, pos, pos, None, None, sticky);
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
    Some(RuleEdits { rest })
}
