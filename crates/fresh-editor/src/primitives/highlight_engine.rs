//! Unified highlighting engine over syntect (TextMate grammars) and
//! tree-sitter. Syntect is the default; tree-sitter `Language` is still
//! detected for non-highlighting features (indentation, semantic highlighting).
//!
//! # TextMate cache design
//!
//! Syntect's parser is a sequential state machine — it must process bytes
//! in order from a known parse state to track multi-line constructs and
//! embedded language transitions. To make scrolling cheap, the engine keeps
//! a span cache, a `(ParseState, ScopeStack)` snapshot at the cache tail,
//! and periodic checkpoint anchors to support resume-from-anywhere.
//!
//! Three render-time paths, gated by what the cache covers:
//!
//! - **Cache hit** — cache fully covers the parse range and there's no
//!   pending edit; filter cached spans for the viewport. Zero parse work.
//! - **Forward extension** — cache covers the start of the parse range but
//!   not its end; resume from `tail_state` and parse only the uncovered
//!   tail bytes. Steady-state scroll path.
//! - **Partial update** — there's a pending edit; resume from the nearest
//!   checkpoint before the dirty point and parse forward looking for
//!   convergence (state matches an existing checkpoint), bounded by a
//!   per-pass byte budget so pathological edits can't degenerate into
//!   whole-file reparses.
//! - **Cold start / fallback** — no cache, or none of the above applies;
//!   parse the appropriate range from a fresh state or nearest checkpoint.
//!
//! For files at or below `MAX_PARSE_BYTES` the parse range is the whole
//! file, so the cache is whole-file after the first parse and scrolling
//! becomes filter-only. Larger files use a viewport-centred window of
//! `±context_bytes` and rely on the forward-extension path to keep
//! scroll-cost bounded.
//!
//! Edits go through `notify_insert` / `notify_delete`, which shift cached
//! span byte offsets in place, set `dirty_from`, and invalidate `tail_state`
//! when the edit lies inside the cached range.

use crate::model::buffer::Buffer;
use crate::model::marker::{MarkerId, MarkerList};
use crate::primitives::grammar::GrammarRegistry;
use crate::primitives::highlighter::{
    highlight_color, HighlightCategory, HighlightSpan, Highlighter, Language,
};
use crate::view::theme::Theme;
use std::collections::HashMap;
use std::ops::Range;
use std::path::Path;
use std::sync::Arc;
use syntect::parsing::SyntaxSet;

/// Map TextMate scope to highlight category
fn scope_to_category(scope: &str) -> Option<HighlightCategory> {
    let scope_lower = scope.to_lowercase();

    // Comments - highest priority
    if scope_lower.starts_with("comment") {
        return Some(HighlightCategory::Comment);
    }

    // Strings
    if scope_lower.starts_with("string") {
        return Some(HighlightCategory::String);
    }

    // Markdown/markup scopes - handle before generic keyword/punctuation checks
    // See: https://macromates.com/manual/en/language_grammars (TextMate scope naming)
    // Headings: markup.heading and entity.name.section (used by syntect's markdown grammar)
    if scope_lower.starts_with("markup.heading") || scope_lower.starts_with("entity.name.section") {
        return Some(HighlightCategory::Keyword); // Headers styled like keywords (bold, prominent)
    }
    // Bold: markup.bold
    if scope_lower.starts_with("markup.bold") {
        return Some(HighlightCategory::Constant); // Bold styled like constants (bright)
    }
    // Italic: markup.italic
    if scope_lower.starts_with("markup.italic") {
        return Some(HighlightCategory::Variable); // Italic styled like variables
    }
    // Inline code and code blocks: markup.raw, markup.inline.raw
    if scope_lower.starts_with("markup.raw") || scope_lower.starts_with("markup.inline.raw") {
        return Some(HighlightCategory::String); // Code styled like strings
    }
    // Links: markup.underline.link
    if scope_lower.starts_with("markup.underline.link") {
        return Some(HighlightCategory::Function); // Links styled like functions (distinct color)
    }
    // Generic underline (often links)
    if scope_lower.starts_with("markup.underline") {
        return Some(HighlightCategory::Function);
    }
    // Block quotes: markup.quote
    if scope_lower.starts_with("markup.quote") {
        return Some(HighlightCategory::Comment); // Quotes styled like comments (subdued)
    }
    // Lists: markup.list
    if scope_lower.starts_with("markup.list") {
        return Some(HighlightCategory::Operator); // List markers styled like operators
    }
    // Strikethrough: markup.strikethrough
    if scope_lower.starts_with("markup.strikethrough") {
        return Some(HighlightCategory::Comment); // Strikethrough styled subdued
    }

    // Keywords
    if scope_lower.starts_with("keyword.control")
        || scope_lower.starts_with("keyword.other")
        || scope_lower.starts_with("keyword.declaration")
        || scope_lower.starts_with("keyword")
    {
        // keyword.operator should map to Operator, not Keyword
        if !scope_lower.starts_with("keyword.operator") {
            return Some(HighlightCategory::Keyword);
        }
    }

    // Punctuation that belongs to a parent construct (comment/string delimiters)
    // These must be checked before the generic punctuation rule below.
    // TextMate grammars assign e.g. `punctuation.definition.comment` to # // /* etc.
    if scope_lower.starts_with("punctuation.definition.comment") {
        return Some(HighlightCategory::Comment);
    }
    if scope_lower.starts_with("punctuation.definition.string") {
        return Some(HighlightCategory::String);
    }

    // Operators (keyword.operator only)
    if scope_lower.starts_with("keyword.operator") {
        return Some(HighlightCategory::Operator);
    }

    // Punctuation brackets ({, }, (, ), [, ], <, >)
    // Covers punctuation.section.*, punctuation.bracket.*,
    // and punctuation.definition.{array,block,brackets,group,inline-table,section,table,tag}
    if scope_lower.starts_with("punctuation.section")
        || scope_lower.starts_with("punctuation.bracket")
        || scope_lower.starts_with("punctuation.definition.array")
        || scope_lower.starts_with("punctuation.definition.block")
        || scope_lower.starts_with("punctuation.definition.brackets")
        || scope_lower.starts_with("punctuation.definition.group")
        || scope_lower.starts_with("punctuation.definition.inline-table")
        || scope_lower.starts_with("punctuation.definition.section")
        || scope_lower.starts_with("punctuation.definition.table")
        || scope_lower.starts_with("punctuation.definition.tag")
    {
        return Some(HighlightCategory::PunctuationBracket);
    }

    // Punctuation delimiters (;, ,, .)
    if scope_lower.starts_with("punctuation.separator")
        || scope_lower.starts_with("punctuation.terminator")
        || scope_lower.starts_with("punctuation.accessor")
    {
        return Some(HighlightCategory::PunctuationDelimiter);
    }

    // Functions
    if scope_lower.starts_with("entity.name.function")
        || scope_lower.starts_with("support.function")
        || scope_lower.starts_with("meta.function-call")
        || scope_lower.starts_with("variable.function")
    {
        return Some(HighlightCategory::Function);
    }

    // Types
    if scope_lower.starts_with("entity.name.type")
        || scope_lower.starts_with("entity.name.class")
        || scope_lower.starts_with("entity.name.struct")
        || scope_lower.starts_with("entity.name.enum")
        || scope_lower.starts_with("entity.name.interface")
        || scope_lower.starts_with("entity.name.trait")
        || scope_lower.starts_with("support.type")
        || scope_lower.starts_with("support.class")
        || scope_lower.starts_with("storage.type")
    {
        return Some(HighlightCategory::Type);
    }

    // Storage modifiers (pub, static, const as keywords)
    if scope_lower.starts_with("storage.modifier") {
        return Some(HighlightCategory::Keyword);
    }

    // Constants and numbers
    if scope_lower.starts_with("constant.numeric")
        || scope_lower.starts_with("constant.language.boolean")
    {
        return Some(HighlightCategory::Number);
    }
    if scope_lower.starts_with("constant") {
        return Some(HighlightCategory::Constant);
    }

    // Variables
    if scope_lower.starts_with("variable.parameter")
        || scope_lower.starts_with("variable.other")
        || scope_lower.starts_with("variable.language")
    {
        return Some(HighlightCategory::Variable);
    }

    // Properties / object keys
    if scope_lower.starts_with("entity.name.tag")
        || scope_lower.starts_with("support.other.property")
        || scope_lower.starts_with("meta.object-literal.key")
        || scope_lower.starts_with("variable.other.property")
        || scope_lower.starts_with("variable.other.object.property")
    {
        return Some(HighlightCategory::Property);
    }

    // Attributes (decorators, annotations)
    if scope_lower.starts_with("entity.other.attribute")
        || scope_lower.starts_with("meta.attribute")
        || scope_lower.starts_with("entity.name.decorator")
    {
        return Some(HighlightCategory::Attribute);
    }

    // Generic variable fallback
    if scope_lower.starts_with("variable") {
        return Some(HighlightCategory::Variable);
    }

    None
}

/// Unified highlighting engine supporting multiple backends
#[derive(Default)]
pub enum HighlightEngine {
    /// Tree-sitter based highlighting (built-in languages)
    TreeSitter(Box<Highlighter>),
    /// TextMate grammar based highlighting
    TextMate(Box<TextMateEngine>),
    /// No highlighting available
    #[default]
    None,
}

/// TextMate highlighting engine. See module docs for the cache design.
pub struct TextMateEngine {
    syntax_set: Arc<SyntaxSet>,
    syntax_index: usize,
    checkpoint_markers: MarkerList,
    checkpoint_states:
        HashMap<MarkerId, (syntect::parsing::ParseState, syntect::parsing::ScopeStack)>,
    dirty_from: Option<usize>,
    cache: Option<TextMateCache>,
    last_buffer_len: usize,
    ts_language: Option<Language>,
    stats: HighlightStats,
}

/// Counters for monitoring highlighting performance in tests.
#[derive(Debug, Default, Clone)]
pub struct HighlightStats {
    /// Number of bytes parsed by syntect (total across all highlight_viewport calls).
    pub bytes_parsed: usize,
    /// Number of highlight_viewport calls that hit the span cache.
    pub cache_hits: usize,
    /// Number of highlight_viewport calls that missed the cache and re-parsed.
    pub cache_misses: usize,
    /// Number of checkpoint states updated during convergence.
    pub checkpoints_updated: usize,
    /// Number of times convergence was detected (state matched existing checkpoint).
    pub convergences: usize,
}

#[derive(Debug, Clone)]
struct TextMateCache {
    range: Range<usize>,
    spans: Vec<CachedSpan>,
    // Parse state at `range.end`; powers forward extension. None when the
    // last mutation didn't end at `range.end`.
    tail_state: Option<(syntect::parsing::ParseState, syntect::parsing::ScopeStack)>,
}

#[derive(Debug, Clone)]
struct CachedSpan {
    range: Range<usize>,
    category: crate::primitives::highlighter::HighlightCategory,
}

/// Small/large file threshold (whole-file cache vs viewport window).
const MAX_PARSE_BYTES: usize = 1024 * 1024;

/// Distance between checkpoint anchors. Smaller = faster convergence on edit.
const CHECKPOINT_INTERVAL: usize = 256;

/// Per-pass cap on partial-update parsing past `dirty_pos`. Bounds work for
/// pathological edits whose effect doesn't converge.
const CONVERGENCE_BUDGET: usize = 64 * 1024;

impl TextMateEngine {
    /// Create a new TextMate engine for the given syntax
    pub fn new(syntax_set: Arc<SyntaxSet>, syntax_index: usize) -> Self {
        Self {
            syntax_set,
            syntax_index,
            checkpoint_markers: MarkerList::new(),
            checkpoint_states: HashMap::new(),
            dirty_from: None,
            cache: None,
            last_buffer_len: 0,
            ts_language: None,
            stats: HighlightStats::default(),
        }
    }

    /// Create a new TextMate engine with a tree-sitter language for non-highlighting features
    pub fn with_language(
        syntax_set: Arc<SyntaxSet>,
        syntax_index: usize,
        ts_language: Option<Language>,
    ) -> Self {
        Self {
            syntax_set,
            syntax_index,
            checkpoint_markers: MarkerList::new(),
            checkpoint_states: HashMap::new(),
            dirty_from: None,
            cache: None,
            last_buffer_len: 0,
            ts_language,
            stats: HighlightStats::default(),
        }
    }

    /// Get performance stats for testing and diagnostics.
    pub fn stats(&self) -> &HighlightStats {
        &self.stats
    }

    /// Reset performance counters.
    pub fn reset_stats(&mut self) {
        self.stats = HighlightStats::default();
    }

    /// Get the tree-sitter language (for indentation, semantic highlighting, etc.)
    pub fn language(&self) -> Option<&Language> {
        self.ts_language.as_ref()
    }

    /// Buffer-insert notification. Shifts span offsets in place and marks
    /// the cache dirty so the partial-update path runs on next render.
    pub fn notify_insert(&mut self, position: usize, length: usize) {
        self.checkpoint_markers.adjust_for_insert(position, length);
        self.dirty_from = Some(self.dirty_from.map_or(position, |d| d.min(position)));
        if let Some(cache) = &mut self.cache {
            for span in &mut cache.spans {
                if span.range.start >= position {
                    span.range.start += length;
                    span.range.end += length;
                } else if span.range.end > position {
                    span.range.end += length;
                }
            }
            if cache.range.end >= position {
                cache.range.end += length;
                if position < cache.range.end {
                    cache.tail_state = None;
                }
            }
        }
    }

    /// Buffer-delete notification. Mirror of `notify_insert`.
    pub fn notify_delete(&mut self, position: usize, length: usize) {
        self.checkpoint_markers.adjust_for_delete(position, length);
        self.dirty_from = Some(self.dirty_from.map_or(position, |d| d.min(position)));
        if let Some(cache) = &mut self.cache {
            let delete_end = position + length;
            cache.spans.retain_mut(|span| {
                if span.range.start >= delete_end {
                    span.range.start -= length;
                    span.range.end -= length;
                    true
                } else if span.range.end <= position {
                    true
                } else if span.range.start >= position && span.range.end <= delete_end {
                    false
                } else {
                    if span.range.start < position {
                        span.range.end = position.min(span.range.end);
                    } else {
                        span.range.start = position;
                        span.range.end = position + span.range.end.saturating_sub(delete_end);
                    }
                    span.range.start < span.range.end
                }
            });
            if cache.range.end > delete_end {
                cache.range.end -= length;
            } else if cache.range.end > position {
                cache.range.end = position;
            }
            if position < cache.range.end {
                cache.tail_state = None;
            }
        }
    }

    /// Highlight the visible viewport. Path selection is documented in the
    /// module-level docs ("TextMate cache design").
    pub fn highlight_viewport(
        &mut self,
        buffer: &Buffer,
        viewport_start: usize,
        viewport_end: usize,
        theme: &Theme,
        context_bytes: usize,
    ) -> Vec<HighlightSpan> {
        let buf_len = buffer.len();
        let (desired_parse_start, parse_end) = if buf_len <= MAX_PARSE_BYTES {
            (0, buf_len)
        } else {
            let s = viewport_start.saturating_sub(context_bytes);
            let e = (viewport_end + context_bytes).min(buf_len);
            (s, e)
        };

        let dirty = self.dirty_from.take();
        let cache_covers_viewport = self.cache.as_ref().is_some_and(|c| {
            c.range.start <= desired_parse_start && c.range.end >= desired_parse_start
        });
        let exact_cache_hit = cache_covers_viewport
            && dirty.is_none()
            && self.last_buffer_len == buffer.len()
            && self
                .cache
                .as_ref()
                .is_some_and(|c| c.range.end >= parse_end);

        // Cache hit.
        if exact_cache_hit {
            self.stats.cache_hits += 1;
            return self.filter_cached_spans(viewport_start, viewport_end, theme);
        }

        // Forward extension.
        if dirty.is_none()
            && cache_covers_viewport
            && self.last_buffer_len == buffer.len()
            && self
                .cache
                .as_ref()
                .is_some_and(|c| c.range.end < parse_end && c.tail_state.is_some())
        {
            return self.extend_cache_forward(buffer, parse_end, viewport_start, viewport_end, theme);
        }

        // Partial update.
        if cache_covers_viewport && dirty.is_some() {
            if let Some(dirty_pos) = dirty {
                if dirty_pos < parse_end {
                    if let Some(result) = self.try_partial_update(
                        buffer,
                        dirty_pos,
                        desired_parse_start,
                        parse_end,
                        viewport_start,
                        viewport_end,
                        theme,
                    ) {
                        return result;
                    }
                } else {
                    // Dirty region past viewport: cached spans are still valid.
                    self.dirty_from = Some(dirty_pos);
                    self.stats.cache_hits += 1;
                    return self.filter_cached_spans(viewport_start, viewport_end, theme);
                }
            }
        } else if let Some(d) = dirty {
            self.dirty_from = Some(d);
        }

        // Cold start / fallback.
        self.full_parse(
            buffer,
            desired_parse_start,
            parse_end,
            viewport_start,
            viewport_end,
            theme,
            context_bytes,
        )
    }

    /// Filter cached spans for the viewport and resolve colors.
    fn filter_cached_spans(
        &self,
        viewport_start: usize,
        viewport_end: usize,
        theme: &Theme,
    ) -> Vec<HighlightSpan> {
        let cache = self.cache.as_ref().unwrap();
        cache
            .spans
            .iter()
            .filter(|span| span.range.start < viewport_end && span.range.end > viewport_start)
            .map(|span| HighlightSpan {
                range: span.range.clone(),
                color: highlight_color(span.category, theme),
                category: Some(span.category),
            })
            .collect()
    }

    /// Partial update path. Returns `Some` whenever an anchor was available,
    /// even on budget hit or EOF (see post-loop classification). `None` only
    /// when no checkpoint anchor reaches the dirty point.
    #[allow(clippy::too_many_arguments)]
    fn try_partial_update(
        &mut self,
        buffer: &Buffer,
        dirty_pos: usize,
        desired_parse_start: usize,
        parse_end: usize,
        viewport_start: usize,
        viewport_end: usize,
        theme: &Theme,
    ) -> Option<Vec<HighlightSpan>> {
        let syntax = &self.syntax_set.syntaxes()[self.syntax_index];

        // Find checkpoint before the dirty point (bounded search)
        let (actual_start, mut state, mut current_scopes) = {
            let search_start = dirty_pos.saturating_sub(MAX_PARSE_BYTES);
            let markers = self.checkpoint_markers.query_range(search_start, dirty_pos);
            let nearest = markers.into_iter().max_by_key(|(_, start, _)| *start);
            if let Some((id, cp_pos, _)) = nearest {
                if let Some((s, sc)) = self.checkpoint_states.get(&id) {
                    (cp_pos, s.clone(), sc.clone())
                } else {
                    return None; // orphan, fall back
                }
            } else if parse_end <= MAX_PARSE_BYTES {
                (
                    0,
                    syntect::parsing::ParseState::new(syntax),
                    syntect::parsing::ScopeStack::new(),
                )
            } else {
                return None; // large file, no nearby checkpoint, fall back
            }
        };

        // Get markers from dirty point forward for convergence checking
        let mut markers_ahead: Vec<(MarkerId, usize)> = self
            .checkpoint_markers
            .query_range(dirty_pos, parse_end)
            .into_iter()
            .map(|(id, start, _)| (id, start))
            .collect();
        markers_ahead.sort_by_key(|(_, pos)| *pos);
        let mut marker_idx = 0;

        // Parse from actual_start to parse_end, looking for convergence
        let content_end = parse_end.min(buffer.len());
        if actual_start >= content_end {
            return None;
        }
        let content = buffer.slice_bytes(actual_start..content_end);
        let content_str = match std::str::from_utf8(&content) {
            Ok(s) => s,
            Err(_) => return None,
        };

        let mut new_spans = Vec::new();
        let content_bytes = content_str.as_bytes();
        let mut pos = 0;
        let mut current_offset = actual_start;
        let mut converged_at: Option<usize> = None;
        let mut budget_hit_at: Option<usize> = None;
        let mut bytes_since_checkpoint: usize = 0;

        while pos < content_bytes.len() {
            // Create checkpoints in new territory
            if bytes_since_checkpoint >= CHECKPOINT_INTERVAL {
                let nearby = self.checkpoint_markers.query_range(
                    current_offset.saturating_sub(CHECKPOINT_INTERVAL / 2),
                    current_offset + CHECKPOINT_INTERVAL / 2,
                );
                if nearby.is_empty() {
                    let marker_id = self.checkpoint_markers.create(current_offset, true);
                    self.checkpoint_states
                        .insert(marker_id, (state.clone(), current_scopes.clone()));
                }
                bytes_since_checkpoint = 0;
            }

            let line_start = pos;
            let mut line_end = pos;
            while line_end < content_bytes.len() {
                if content_bytes[line_end] == b'\n' {
                    line_end += 1;
                    break;
                } else if content_bytes[line_end] == b'\r' {
                    if line_end + 1 < content_bytes.len() && content_bytes[line_end + 1] == b'\n' {
                        line_end += 2;
                    } else {
                        line_end += 1;
                    }
                    break;
                }
                line_end += 1;
            }

            let line_bytes = &content_bytes[line_start..line_end];
            let actual_line_byte_len = line_bytes.len();

            let line_str = match std::str::from_utf8(line_bytes) {
                Ok(s) => s,
                Err(_) => {
                    pos = line_end;
                    current_offset += actual_line_byte_len;
                    bytes_since_checkpoint += actual_line_byte_len;
                    continue;
                }
            };

            let line_content = line_str.trim_end_matches(&['\r', '\n'][..]);
            let line_for_syntect = if line_end < content_bytes.len() || line_str.ends_with('\n') {
                format!("{}\n", line_content)
            } else {
                line_content.to_string()
            };

            let ops = match state.parse_line(&line_for_syntect, &self.syntax_set) {
                Ok(ops) => ops,
                Err(_) => {
                    pos = line_end;
                    current_offset += actual_line_byte_len;
                    bytes_since_checkpoint += actual_line_byte_len;
                    continue;
                }
            };

            // Collect spans for the dirty region
            let collect_spans =
                current_offset + actual_line_byte_len > desired_parse_start.max(actual_start);
            let mut syntect_offset = 0;
            let line_content_len = line_content.len();

            for (op_offset, op) in ops {
                let clamped_op_offset = op_offset.min(line_content_len);
                if collect_spans && clamped_op_offset > syntect_offset {
                    if let Some(category) = Self::scope_stack_to_category(&current_scopes) {
                        let byte_start = current_offset + syntect_offset;
                        let byte_end = current_offset + clamped_op_offset;
                        let clamped_start = byte_start.max(actual_start);
                        if clamped_start < byte_end {
                            new_spans.push(CachedSpan {
                                range: clamped_start..byte_end,
                                category,
                            });
                        }
                    }
                }
                syntect_offset = clamped_op_offset;
                #[allow(clippy::let_underscore_must_use)]
                let _ = current_scopes.apply(&op);
            }

            if collect_spans && syntect_offset < line_content_len {
                if let Some(category) = Self::scope_stack_to_category(&current_scopes) {
                    let byte_start = current_offset + syntect_offset;
                    let byte_end = current_offset + line_content_len;
                    let clamped_start = byte_start.max(actual_start);
                    if clamped_start < byte_end {
                        new_spans.push(CachedSpan {
                            range: clamped_start..byte_end,
                            category,
                        });
                    }
                }
            }

            pos = line_end;
            current_offset += actual_line_byte_len;
            bytes_since_checkpoint += actual_line_byte_len;

            // Check convergence at checkpoint markers
            while marker_idx < markers_ahead.len() && markers_ahead[marker_idx].1 <= current_offset
            {
                let (marker_id, _) = markers_ahead[marker_idx];
                marker_idx += 1;
                if let Some(stored) = self.checkpoint_states.get(&marker_id) {
                    if *stored == (state.clone(), current_scopes.clone()) {
                        self.stats.convergences += 1;
                        converged_at = Some(current_offset);
                        break;
                    }
                }
                self.stats.checkpoints_updated += 1;
                self.checkpoint_states
                    .insert(marker_id, (state.clone(), current_scopes.clone()));
            }

            if converged_at.is_some() {
                break;
            }

            // Bound work per pass: pathological edits (e.g. unclosed `/*`
            // re-scoping the rest of the file) can never converge. Stop here
            // and resume from `current_offset` on the next render.
            if current_offset.saturating_sub(dirty_pos) >= CONVERGENCE_BUDGET {
                budget_hit_at = Some(current_offset);
                break;
            }
        }

        self.stats.bytes_parsed += current_offset.saturating_sub(actual_start);

        // Splice classification: converged → clear dirty; budget hit → keep
        // dirty for next pass; EOF → clear dirty.
        let (splice_end, dirty_after) = if let Some(c) = converged_at {
            (c, None)
        } else if let Some(b) = budget_hit_at {
            (b, Some(b))
        } else {
            (current_offset, None)
        };

        self.stats.cache_misses += 1; // partial update counts as a miss

        Self::merge_adjacent_spans(&mut new_spans);

        if let Some(cache) = &mut self.cache {
            let splice_start = actual_start;
            cache
                .spans
                .retain(|span| span.range.end <= splice_start || span.range.start >= splice_end);
            cache.spans.extend(new_spans);
            cache.spans.sort_by_key(|s| s.range.start);
            Self::merge_adjacent_spans(&mut cache.spans);
            if splice_end > cache.range.end {
                cache.range.end = splice_end;
            }
            cache.tail_state = None;
        }

        self.last_buffer_len = buffer.len();
        self.dirty_from = dirty_after;

        Some(self.filter_cached_spans(viewport_start, viewport_end, theme))
    }

    /// Forward extension path (see module docs). Caller checks the cache
    /// exists, has a `tail_state`, has no dirty edits, and `cache.range.end
    /// < parse_end`.
    fn extend_cache_forward(
        &mut self,
        buffer: &Buffer,
        parse_end: usize,
        viewport_start: usize,
        viewport_end: usize,
        theme: &Theme,
    ) -> Vec<HighlightSpan> {
        self.stats.cache_misses += 1;
        let buf_len = buffer.len();
        let parse_end = parse_end.min(buf_len);

        let (extension_start, mut state, mut current_scopes) = {
            let cache = self
                .cache
                .as_ref()
                .expect("extend_cache_forward: cache must exist");
            let (s, sc) = cache
                .tail_state
                .as_ref()
                .expect("extend_cache_forward: tail_state must exist")
                .clone();
            (cache.range.end, s, sc)
        };

        if parse_end <= extension_start {
            return self.filter_cached_spans(viewport_start, viewport_end, theme);
        }

        let content = buffer.slice_bytes(extension_start..parse_end);
        let content_str = match std::str::from_utf8(&content) {
            Ok(s) => s,
            Err(_) => return self.filter_cached_spans(viewport_start, viewport_end, theme),
        };

        let mut new_spans = Vec::new();
        let content_bytes = content_str.as_bytes();
        let mut pos = 0;
        let mut current_offset = extension_start;
        let mut bytes_since_checkpoint: usize = 0;

        while pos < content_bytes.len() {
            if bytes_since_checkpoint >= CHECKPOINT_INTERVAL {
                let nearby = self.checkpoint_markers.query_range(
                    current_offset.saturating_sub(CHECKPOINT_INTERVAL / 2),
                    current_offset + CHECKPOINT_INTERVAL / 2,
                );
                if nearby.is_empty() {
                    let marker_id = self.checkpoint_markers.create(current_offset, true);
                    self.checkpoint_states
                        .insert(marker_id, (state.clone(), current_scopes.clone()));
                }
                bytes_since_checkpoint = 0;
            }

            let line_start = pos;
            let mut line_end = pos;
            while line_end < content_bytes.len() {
                if content_bytes[line_end] == b'\n' {
                    line_end += 1;
                    break;
                } else if content_bytes[line_end] == b'\r' {
                    if line_end + 1 < content_bytes.len() && content_bytes[line_end + 1] == b'\n' {
                        line_end += 2;
                    } else {
                        line_end += 1;
                    }
                    break;
                }
                line_end += 1;
            }

            let line_bytes = &content_bytes[line_start..line_end];
            let actual_line_byte_len = line_bytes.len();

            let line_str = match std::str::from_utf8(line_bytes) {
                Ok(s) => s,
                Err(_) => {
                    pos = line_end;
                    current_offset += actual_line_byte_len;
                    bytes_since_checkpoint += actual_line_byte_len;
                    continue;
                }
            };

            let line_content = line_str.trim_end_matches(&['\r', '\n'][..]);
            let line_for_syntect = if line_end < content_bytes.len() || line_str.ends_with('\n') {
                format!("{}\n", line_content)
            } else {
                line_content.to_string()
            };

            let ops = match state.parse_line(&line_for_syntect, &self.syntax_set) {
                Ok(ops) => ops,
                Err(_) => {
                    pos = line_end;
                    current_offset += actual_line_byte_len;
                    bytes_since_checkpoint += actual_line_byte_len;
                    continue;
                }
            };

            let mut syntect_offset = 0;
            let line_content_len = line_content.len();

            for (op_offset, op) in ops {
                let clamped_op_offset = op_offset.min(line_content_len);
                if clamped_op_offset > syntect_offset {
                    if let Some(category) = Self::scope_stack_to_category(&current_scopes) {
                        let byte_start = current_offset + syntect_offset;
                        let byte_end = current_offset + clamped_op_offset;
                        if byte_start < byte_end {
                            new_spans.push(CachedSpan {
                                range: byte_start..byte_end,
                                category,
                            });
                        }
                    }
                }
                syntect_offset = clamped_op_offset;
                #[allow(clippy::let_underscore_must_use)]
                let _ = current_scopes.apply(&op);
            }

            if syntect_offset < line_content_len {
                if let Some(category) = Self::scope_stack_to_category(&current_scopes) {
                    let byte_start = current_offset + syntect_offset;
                    let byte_end = current_offset + line_content_len;
                    if byte_start < byte_end {
                        new_spans.push(CachedSpan {
                            range: byte_start..byte_end,
                            category,
                        });
                    }
                }
            }

            pos = line_end;
            current_offset += actual_line_byte_len;
            bytes_since_checkpoint += actual_line_byte_len;
        }

        self.stats.bytes_parsed += parse_end - extension_start;

        Self::merge_adjacent_spans(&mut new_spans);

        let cache = self
            .cache
            .as_mut()
            .expect("extend_cache_forward: cache must still exist");
        cache.spans.extend(new_spans);
        Self::merge_adjacent_spans(&mut cache.spans);
        cache.range.end = parse_end;
        cache.tail_state = Some((state, current_scopes));
        self.last_buffer_len = buf_len;

        self.filter_cached_spans(viewport_start, viewport_end, theme)
    }

    /// Full re-parse from desired_parse_start to parse_end. Used on cold start
    /// or when partial update fails (no convergence).
    #[allow(clippy::too_many_arguments)]
    fn full_parse(
        &mut self,
        buffer: &Buffer,
        desired_parse_start: usize,
        parse_end: usize,
        viewport_start: usize,
        viewport_end: usize,
        theme: &Theme,
        _context_bytes: usize,
    ) -> Vec<HighlightSpan> {
        self.stats.cache_misses += 1;
        self.dirty_from = None; // consumed

        if parse_end <= desired_parse_start {
            return Vec::new();
        }

        let syntax = &self.syntax_set.syntaxes()[self.syntax_index];
        let (actual_start, mut state, mut current_scopes, create_checkpoints) =
            self.find_parse_resume_point(desired_parse_start, parse_end, syntax);

        let content = buffer.slice_bytes(actual_start..parse_end);
        let content_str = match std::str::from_utf8(&content) {
            Ok(s) => s,
            Err(_) => return Vec::new(),
        };

        let mut spans = Vec::new();
        let content_bytes = content_str.as_bytes();
        let mut pos = 0;
        let mut current_offset = actual_start;
        let mut bytes_since_checkpoint: usize = 0;

        while pos < content_bytes.len() {
            if create_checkpoints && bytes_since_checkpoint >= CHECKPOINT_INTERVAL {
                let nearby = self.checkpoint_markers.query_range(
                    current_offset.saturating_sub(CHECKPOINT_INTERVAL / 2),
                    current_offset + CHECKPOINT_INTERVAL / 2,
                );
                if nearby.is_empty() {
                    let marker_id = self.checkpoint_markers.create(current_offset, true);
                    self.checkpoint_states
                        .insert(marker_id, (state.clone(), current_scopes.clone()));
                }
                bytes_since_checkpoint = 0;
            }

            let line_start = pos;
            let mut line_end = pos;

            while line_end < content_bytes.len() {
                if content_bytes[line_end] == b'\n' {
                    line_end += 1;
                    break;
                } else if content_bytes[line_end] == b'\r' {
                    if line_end + 1 < content_bytes.len() && content_bytes[line_end + 1] == b'\n' {
                        line_end += 2;
                    } else {
                        line_end += 1;
                    }
                    break;
                }
                line_end += 1;
            }

            let line_bytes = &content_bytes[line_start..line_end];
            let actual_line_byte_len = line_bytes.len();

            let line_str = match std::str::from_utf8(line_bytes) {
                Ok(s) => s,
                Err(_) => {
                    pos = line_end;
                    current_offset += actual_line_byte_len;
                    bytes_since_checkpoint += actual_line_byte_len;
                    continue;
                }
            };

            let line_content = line_str.trim_end_matches(&['\r', '\n'][..]);
            let line_for_syntect = if line_end < content_bytes.len() || line_str.ends_with('\n') {
                format!("{}\n", line_content)
            } else {
                line_content.to_string()
            };

            let ops = match state.parse_line(&line_for_syntect, &self.syntax_set) {
                Ok(ops) => ops,
                Err(_) => {
                    pos = line_end;
                    current_offset += actual_line_byte_len;
                    bytes_since_checkpoint += actual_line_byte_len;
                    continue;
                }
            };

            let collect_spans = current_offset + actual_line_byte_len > desired_parse_start;
            let mut syntect_offset = 0;
            let line_content_len = line_content.len();

            for (op_offset, op) in ops {
                let clamped_op_offset = op_offset.min(line_content_len);
                if collect_spans && clamped_op_offset > syntect_offset {
                    if let Some(category) = Self::scope_stack_to_category(&current_scopes) {
                        let byte_start = current_offset + syntect_offset;
                        let byte_end = current_offset + clamped_op_offset;
                        let clamped_start = byte_start.max(desired_parse_start);
                        if clamped_start < byte_end {
                            spans.push(CachedSpan {
                                range: clamped_start..byte_end,
                                category,
                            });
                        }
                    }
                }
                syntect_offset = clamped_op_offset;
                #[allow(clippy::let_underscore_must_use)]
                let _ = current_scopes.apply(&op);
            }

            if collect_spans && syntect_offset < line_content_len {
                if let Some(category) = Self::scope_stack_to_category(&current_scopes) {
                    let byte_start = current_offset + syntect_offset;
                    let byte_end = current_offset + line_content_len;
                    let clamped_start = byte_start.max(desired_parse_start);
                    if clamped_start < byte_end {
                        spans.push(CachedSpan {
                            range: clamped_start..byte_end,
                            category,
                        });
                    }
                }
            }

            pos = line_end;
            current_offset += actual_line_byte_len;
            bytes_since_checkpoint += actual_line_byte_len;

            // Update checkpoint states as we pass them
            let markers_here: Vec<(MarkerId, usize)> = self
                .checkpoint_markers
                .query_range(
                    current_offset.saturating_sub(actual_line_byte_len),
                    current_offset,
                )
                .into_iter()
                .map(|(id, start, _)| (id, start))
                .collect();
            for (marker_id, _) in markers_here {
                self.checkpoint_states
                    .insert(marker_id, (state.clone(), current_scopes.clone()));
            }
        }

        self.stats.bytes_parsed += parse_end.saturating_sub(actual_start);

        Self::merge_adjacent_spans(&mut spans);

        self.cache = Some(TextMateCache {
            range: desired_parse_start..parse_end,
            spans: spans.clone(),
            tail_state: Some((state, current_scopes)),
        });
        self.last_buffer_len = buffer.len();

        spans
            .into_iter()
            .filter(|span| span.range.start < viewport_end && span.range.end > viewport_start)
            .map(|span| {
                let cat = span.category;
                HighlightSpan {
                    range: span.range,
                    color: highlight_color(cat, theme),
                    category: Some(cat),
                }
            })
            .collect()
    }

    /// Find the best point to resume parsing from for the viewport.
    fn find_parse_resume_point(
        &self,
        desired_start: usize,
        parse_end: usize,
        syntax: &syntect::parsing::SyntaxReference,
    ) -> (
        usize,
        syntect::parsing::ParseState,
        syntect::parsing::ScopeStack,
        bool,
    ) {
        use syntect::parsing::{ParseState, ScopeStack};

        // Look for a checkpoint near the desired start. For large files, only
        // consider checkpoints that are within MAX_PARSE_BYTES of desired_start
        // to avoid parsing hundreds of MB from a distant checkpoint.
        let search_start = desired_start.saturating_sub(MAX_PARSE_BYTES);
        let markers = self
            .checkpoint_markers
            .query_range(search_start, desired_start + 1);
        let nearest = markers.into_iter().max_by_key(|(_, start, _)| *start);

        if let Some((id, cp_pos, _)) = nearest {
            if let Some((s, sc)) = self.checkpoint_states.get(&id) {
                return (cp_pos, s.clone(), sc.clone(), true);
            }
        }

        if parse_end <= MAX_PARSE_BYTES {
            // File is small enough to parse from byte 0
            (0, ParseState::new(syntax), ScopeStack::new(), true)
        } else {
            // Large file, no nearby checkpoint — start fresh from desired_start.
            // Still create checkpoints so future visits to this region can resume.
            (
                desired_start,
                ParseState::new(syntax),
                ScopeStack::new(),
                true,
            )
        }
    }

    /// Map scope stack to highlight category
    fn scope_stack_to_category(scopes: &syntect::parsing::ScopeStack) -> Option<HighlightCategory> {
        for scope in scopes.as_slice().iter().rev() {
            let scope_str = scope.build_string();
            if let Some(cat) = scope_to_category(&scope_str) {
                return Some(cat);
            }
        }
        None
    }

    /// Merge adjacent spans with same category
    fn merge_adjacent_spans(spans: &mut Vec<CachedSpan>) {
        if spans.len() < 2 {
            return;
        }

        let mut write_idx = 0;
        for read_idx in 1..spans.len() {
            if spans[write_idx].category == spans[read_idx].category
                && spans[write_idx].range.end == spans[read_idx].range.start
            {
                spans[write_idx].range.end = spans[read_idx].range.end;
            } else {
                write_idx += 1;
                if write_idx != read_idx {
                    spans[write_idx] = spans[read_idx].clone();
                }
            }
        }
        spans.truncate(write_idx + 1);
    }

    /// Invalidate span cache for an edited range.
    /// Checkpoint positions are handled by notify_insert/notify_delete.
    /// The span cache is NOT cleared here — it will be patched (partial update)
    /// during the next highlight_viewport call using convergence. Only dirty_from
    /// (set by notify_insert/notify_delete) controls re-parsing scope.
    pub fn invalidate_range(&mut self, _edit_range: Range<usize>) {
        // Intentionally does NOT clear self.cache.
        // The cache will be partially updated in highlight_viewport when
        // dirty_from is set. This avoids full re-parses for small edits.
    }

    /// Invalidate all cache and checkpoints (file reload, language change, etc.)
    pub fn invalidate_all(&mut self) {
        self.cache = None;
        let ids: Vec<MarkerId> = self.checkpoint_states.keys().copied().collect();
        for id in ids {
            self.checkpoint_markers.delete(id);
        }
        self.checkpoint_states.clear();
        self.dirty_from = None;
    }

    /// Get the highlight category at a byte position from the cache.
    ///
    /// Returns the category if the position falls within a cached highlight span.
    /// The position must be within the last highlighted viewport range for a result.
    pub fn category_at_position(&self, position: usize) -> Option<HighlightCategory> {
        let cache = self.cache.as_ref()?;
        cache
            .spans
            .iter()
            .find(|span| span.range.start <= position && position < span.range.end)
            .map(|span| span.category)
    }

    /// Get syntax name
    pub fn syntax_name(&self) -> &str {
        &self.syntax_set.syntaxes()[self.syntax_index].name
    }
}

impl HighlightEngine {
    /// Build a highlighting engine for a catalog entry.
    ///
    /// Single chokepoint for the "prefer syntect, fall back to tree-sitter"
    /// logic. Callers that start from a path or a syntax name should resolve
    /// the entry through `GrammarRegistry::find_by_path` / `find_by_name` and
    /// then call this.
    pub fn from_entry(
        entry: &crate::primitives::grammar::GrammarEntry,
        registry: &GrammarRegistry,
    ) -> Self {
        let syntax_set = registry.syntax_set_arc();
        if let Some(index) = entry.engines.syntect {
            return Self::TextMate(Box::new(TextMateEngine::with_language(
                syntax_set,
                index,
                entry.engines.tree_sitter,
            )));
        }
        if let Some(lang) = entry.engines.tree_sitter {
            if let Ok(highlighter) = Highlighter::new(lang) {
                return Self::TreeSitter(Box::new(highlighter));
            }
        }
        Self::None
    }

    /// Create a highlighting engine for a file.
    ///
    /// Thin wrapper around `from_entry` that resolves the path via the catalog.
    /// User-config-declared filename/extension mappings are honoured as long as
    /// `GrammarRegistry::apply_language_config` has been called on the registry.
    /// `first_line` is used for shebang / first-line regex fallback — pass
    /// `None` when no content is available.
    pub fn for_file(path: &Path, first_line: Option<&str>, registry: &GrammarRegistry) -> Self {
        if let Some(entry) = registry.find_by_path(path, first_line) {
            return Self::from_entry(entry, registry);
        }
        Self::None
    }

    /// Create a highlighting engine for a syntax by name.
    ///
    /// Thin wrapper around `from_entry` that performs the lookup via
    /// `find_by_name`. The catalog entry already knows which tree-sitter
    /// `Language` (if any) serves it, so no separate hint is needed.
    pub fn for_syntax_name(name: &str, registry: &GrammarRegistry) -> Self {
        if let Some(entry) = registry.find_by_name(name) {
            return Self::from_entry(entry, registry);
        }
        Self::None
    }

    /// Highlight the visible viewport
    ///
    /// `context_bytes` controls how far before/after the viewport to parse for accurate
    /// highlighting of multi-line constructs (strings, comments, nested blocks).
    pub fn highlight_viewport(
        &mut self,
        buffer: &Buffer,
        viewport_start: usize,
        viewport_end: usize,
        theme: &Theme,
        context_bytes: usize,
    ) -> Vec<HighlightSpan> {
        match self {
            Self::TreeSitter(h) => {
                h.highlight_viewport(buffer, viewport_start, viewport_end, theme, context_bytes)
            }
            Self::TextMate(h) => {
                h.highlight_viewport(buffer, viewport_start, viewport_end, theme, context_bytes)
            }
            Self::None => Vec::new(),
        }
    }

    /// Notify the highlighting engine of a buffer insert (for checkpoint position tracking).
    pub fn notify_insert(&mut self, position: usize, length: usize) {
        if let Self::TextMate(h) = self {
            h.notify_insert(position, length);
        }
    }

    /// Notify the highlighting engine of a buffer delete (for checkpoint position tracking).
    pub fn notify_delete(&mut self, position: usize, length: usize) {
        if let Self::TextMate(h) = self {
            h.notify_delete(position, length);
        }
    }

    /// Invalidate cache for an edited range
    pub fn invalidate_range(&mut self, edit_range: Range<usize>) {
        match self {
            Self::TreeSitter(h) => h.invalidate_range(edit_range),
            Self::TextMate(h) => h.invalidate_range(edit_range),
            Self::None => {}
        }
    }

    /// Invalidate entire cache
    pub fn invalidate_all(&mut self) {
        match self {
            Self::TreeSitter(h) => h.invalidate_all(),
            Self::TextMate(h) => h.invalidate_all(),
            Self::None => {}
        }
    }

    /// Check if this engine has highlighting available
    pub fn has_highlighting(&self) -> bool {
        !matches!(self, Self::None)
    }

    /// Get a description of the active backend
    pub fn backend_name(&self) -> &str {
        match self {
            Self::TreeSitter(_) => "tree-sitter",
            Self::TextMate(_) => "textmate",
            Self::None => "none",
        }
    }

    /// Get performance stats (TextMate engine only).
    pub fn highlight_stats(&self) -> Option<&HighlightStats> {
        if let Self::TextMate(h) = self {
            Some(h.stats())
        } else {
            None
        }
    }

    /// Reset performance counters.
    pub fn reset_highlight_stats(&mut self) {
        if let Self::TextMate(h) = self {
            h.reset_stats();
        }
    }

    /// Get the language/syntax name if available
    pub fn syntax_name(&self) -> Option<&str> {
        match self {
            Self::TreeSitter(_) => None, // Tree-sitter doesn't expose name easily
            Self::TextMate(h) => Some(h.syntax_name()),
            Self::None => None,
        }
    }

    /// Get the highlight category at a byte position from the cache.
    ///
    /// Returns the category if the position falls within a cached highlight span.
    /// Useful for detecting whether the cursor is inside a string, comment, etc.
    pub fn category_at_position(&self, position: usize) -> Option<HighlightCategory> {
        match self {
            Self::TreeSitter(h) => h.category_at_position(position),
            Self::TextMate(h) => h.category_at_position(position),
            Self::None => None,
        }
    }

    /// Get the tree-sitter Language for non-highlighting features
    /// Returns the language even when using TextMate for highlighting
    pub fn language(&self) -> Option<&Language> {
        match self {
            Self::TreeSitter(h) => Some(h.language()),
            Self::TextMate(h) => h.language(),
            Self::None => None,
        }
    }
}

/// Highlight a code string using syntect (for markdown code blocks, hover popups, etc.)
/// Returns spans with byte ranges relative to the input string.
///
/// This uses TextMate grammars via syntect which provides broader language coverage
/// than tree-sitter (~150+ languages vs ~17).
pub fn highlight_string(
    code: &str,
    lang_hint: &str,
    registry: &GrammarRegistry,
    theme: &Theme,
) -> Vec<HighlightSpan> {
    use syntect::parsing::{ParseState, ScopeStack};

    // Find syntax by language token (handles aliases like "py" -> Python)
    let syntax = match registry.syntax_set().find_syntax_by_token(lang_hint) {
        Some(s) => s,
        None => return Vec::new(),
    };

    let syntax_set = registry.syntax_set();
    let mut state = ParseState::new(syntax);
    let mut spans = Vec::new();
    let mut current_scopes = ScopeStack::new();
    let mut current_offset = 0;

    // Parse line by line
    for line in code.split_inclusive('\n') {
        let line_start = current_offset;
        let line_len = line.len();

        // Remove trailing newline for syntect, then add it back
        let line_content = line.trim_end_matches(&['\r', '\n'][..]);
        let line_for_syntect = if line.ends_with('\n') {
            format!("{}\n", line_content)
        } else {
            line_content.to_string()
        };

        let ops = match state.parse_line(&line_for_syntect, syntax_set) {
            Ok(ops) => ops,
            Err(_) => {
                current_offset += line_len;
                continue;
            }
        };

        let mut syntect_offset = 0;
        let line_content_len = line_content.len();

        for (op_offset, op) in ops {
            let clamped_op_offset = op_offset.min(line_content_len);
            if clamped_op_offset > syntect_offset {
                if let Some(category) = scope_stack_to_category(&current_scopes) {
                    let byte_start = line_start + syntect_offset;
                    let byte_end = line_start + clamped_op_offset;
                    if byte_start < byte_end {
                        spans.push(HighlightSpan {
                            range: byte_start..byte_end,
                            color: highlight_color(category, theme),
                            category: Some(category),
                        });
                    }
                }
            }
            syntect_offset = clamped_op_offset;
            // Scope stack errors are non-fatal for highlighting
            #[allow(clippy::let_underscore_must_use)]
            let _ = current_scopes.apply(&op);
        }

        // Handle remaining text on line
        if syntect_offset < line_content_len {
            if let Some(category) = scope_stack_to_category(&current_scopes) {
                let byte_start = line_start + syntect_offset;
                let byte_end = line_start + line_content_len;
                if byte_start < byte_end {
                    spans.push(HighlightSpan {
                        range: byte_start..byte_end,
                        color: highlight_color(category, theme),
                        category: Some(category),
                    });
                }
            }
        }

        current_offset += line_len;
    }

    // Merge adjacent spans with same color
    merge_adjacent_highlight_spans(&mut spans);

    spans
}

/// Map scope stack to highlight category (for highlight_string)
fn scope_stack_to_category(scopes: &syntect::parsing::ScopeStack) -> Option<HighlightCategory> {
    for scope in scopes.as_slice().iter().rev() {
        let scope_str = scope.build_string();
        if let Some(cat) = scope_to_category(&scope_str) {
            return Some(cat);
        }
    }
    None
}

/// Merge adjacent spans with same color
fn merge_adjacent_highlight_spans(spans: &mut Vec<HighlightSpan>) {
    if spans.len() < 2 {
        return;
    }

    let mut write_idx = 0;
    for read_idx in 1..spans.len() {
        if spans[write_idx].color == spans[read_idx].color
            && spans[write_idx].range.end == spans[read_idx].range.start
        {
            spans[write_idx].range.end = spans[read_idx].range.end;
        } else {
            write_idx += 1;
            if write_idx != read_idx {
                spans[write_idx] = spans[read_idx].clone();
            }
        }
    }
    spans.truncate(write_idx + 1);
}

#[cfg(test)]
mod tests {
    use crate::model::filesystem::StdFileSystem;
    use std::sync::Arc;

    fn test_fs() -> Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> {
        Arc::new(StdFileSystem)
    }
    use super::*;
    use crate::view::theme;

    #[test]
    fn test_highlight_engine_default() {
        let engine = HighlightEngine::default();
        assert!(!engine.has_highlighting());
        assert_eq!(engine.backend_name(), "none");
    }

    #[test]
    fn test_textmate_backend_selection() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // Languages with TextMate grammars use TextMate for highlighting
        let engine = HighlightEngine::for_file(Path::new("test.rs"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        // Tree-sitter language should still be detected for other features
        assert!(engine.language().is_some());

        let engine = HighlightEngine::for_file(Path::new("test.py"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.language().is_some());

        let engine = HighlightEngine::for_file(Path::new("test.js"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.language().is_some());

        // TypeScript falls back to tree-sitter (syntect doesn't include TS by default)
        let engine = HighlightEngine::for_file(Path::new("test.ts"), None, &registry);
        assert_eq!(engine.backend_name(), "tree-sitter");
        assert!(engine.language().is_some());

        let engine = HighlightEngine::for_file(Path::new("test.tsx"), None, &registry);
        assert_eq!(engine.backend_name(), "tree-sitter");
        assert!(engine.language().is_some());
    }

    #[test]
    fn test_tree_sitter_direct() {
        // Verify tree-sitter highlighter can be created directly for Rust
        let highlighter = Highlighter::new(Language::Rust);
        assert!(highlighter.is_ok());
    }

    #[test]
    fn test_unknown_extension() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // Unknown extension
        let engine = HighlightEngine::for_file(Path::new("test.unknown_xyz_123"), None, &registry);
        // Might be none or might find something via syntect
        // Just verify it doesn't panic
        let _ = engine.backend_name();
    }

    #[test]
    fn test_highlight_viewport_empty_buffer_no_panic() {
        // Regression test: calling highlight_viewport with an empty buffer
        // and non-zero viewport range previously caused subtraction overflow panic.
        //
        // The bug occurred when:
        // - buffer is empty (len = 0)
        // - viewport_start > context_bytes (so parse_start > 0 after saturating_sub)
        // - parse_end = min(viewport_end + context_bytes, buffer.len()) = 0
        // - parse_end - parse_start would underflow (0 - positive = overflow)
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        let mut engine = HighlightEngine::for_file(Path::new("test.rs"), None, &registry);

        // Create empty buffer
        let buffer = Buffer::from_str("", 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        // Test the specific case that triggered the overflow:
        // viewport_start=100, context_bytes=10 => parse_start=90, parse_end=0
        // 0 - 90 = overflow!
        if let HighlightEngine::TextMate(ref mut tm) = engine {
            // Small context_bytes so parse_start remains > 0
            let spans = tm.highlight_viewport(&buffer, 100, 200, &theme, 10);
            assert!(spans.is_empty());
        }
    }

    /// Test that TextMateEngine produces correct byte offsets for CRLF content.
    /// This is a regression test for a bug where using str::lines() caused 1-byte
    /// offset drift per line because it strips line terminators.
    #[test]
    fn test_textmate_engine_crlf_byte_offsets() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        let mut engine = HighlightEngine::for_file(Path::new("test.java"), None, &registry);

        // Create CRLF content with keywords on each line
        // Each "public" keyword should be highlighted at byte positions:
        // Line 1: "public" at bytes 0-5
        // Line 2: "public" at bytes 8-13 (after "public\r\n" = 8 bytes)
        // Line 3: "public" at bytes 16-21 (after two "public\r\n" = 16 bytes)
        let content = b"public\r\npublic\r\npublic\r\n";
        let buffer = Buffer::from_bytes(content.to_vec(), test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        if let HighlightEngine::TextMate(ref mut tm) = engine {
            // Highlight the entire content
            let spans = tm.highlight_viewport(&buffer, 0, content.len(), &theme, 0);

            // Find spans that cover keyword positions
            // The keyword "public" should have spans at these byte ranges:
            // Line 1: 0..6
            // Line 2: 8..14 (NOT 7..13 which would be the buggy offset)
            // Line 3: 16..22 (NOT 14..20 which would be the buggy offset)

            eprintln!(
                "Spans: {:?}",
                spans.iter().map(|s| &s.range).collect::<Vec<_>>()
            );

            // Check that we have spans covering the correct positions
            let has_span_at = |start: usize, end: usize| -> bool {
                spans
                    .iter()
                    .any(|s| s.range.start <= start && s.range.end >= end)
            };

            // Line 1: "public" at bytes 0-6
            assert!(
                has_span_at(0, 6),
                "Should have span covering bytes 0-6 (line 1 'public'). Spans: {:?}",
                spans.iter().map(|s| &s.range).collect::<Vec<_>>()
            );

            // Line 2: "public" at bytes 8-14 (after "public\r\n")
            // If buggy, would be at 7-13
            assert!(
                has_span_at(8, 14),
                "Should have span covering bytes 8-14 (line 2 'public'). \
                 If this fails, CRLF offset drift is occurring. Spans: {:?}",
                spans.iter().map(|s| &s.range).collect::<Vec<_>>()
            );

            // Line 3: "public" at bytes 16-22 (after two "public\r\n")
            // If buggy, would be at 14-20
            assert!(
                has_span_at(16, 22),
                "Should have span covering bytes 16-22 (line 3 'public'). \
                 If this fails, CRLF offset drift is occurring. Spans: {:?}",
                spans.iter().map(|s| &s.range).collect::<Vec<_>>()
            );
        } else {
            panic!("Expected TextMate engine for .java file");
        }
    }

    #[test]
    fn test_git_rebase_todo_highlighting() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // git-rebase-todo files should use the Git Rebase Todo grammar
        let engine = HighlightEngine::for_file(Path::new("git-rebase-todo"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());
    }

    #[test]
    fn test_git_commit_message_highlighting() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // COMMIT_EDITMSG should use the Git Commit Message grammar
        let engine = HighlightEngine::for_file(Path::new("COMMIT_EDITMSG"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());

        // MERGE_MSG should also work
        let engine = HighlightEngine::for_file(Path::new("MERGE_MSG"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());
    }

    #[test]
    fn test_gitignore_highlighting() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // .gitignore should use the Gitignore grammar
        let engine = HighlightEngine::for_file(Path::new(".gitignore"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());

        // .dockerignore should also work
        let engine = HighlightEngine::for_file(Path::new(".dockerignore"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());
    }

    #[test]
    fn test_gitconfig_highlighting() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // .gitconfig should use the Git Config grammar
        let engine = HighlightEngine::for_file(Path::new(".gitconfig"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());

        // .gitmodules should also work
        let engine = HighlightEngine::for_file(Path::new(".gitmodules"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());
    }

    #[test]
    fn test_gitattributes_highlighting() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());

        // .gitattributes should use the Git Attributes grammar
        let engine = HighlightEngine::for_file(Path::new(".gitattributes"), None, &registry);
        assert_eq!(engine.backend_name(), "textmate");
        assert!(engine.has_highlighting());
    }

    #[test]
    fn test_comment_delimiter_uses_comment_color() {
        // Comment delimiters (#, //, /*) should use comment color, not operator
        assert_eq!(
            scope_to_category("punctuation.definition.comment"),
            Some(HighlightCategory::Comment)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.comment.python"),
            Some(HighlightCategory::Comment)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.comment.begin"),
            Some(HighlightCategory::Comment)
        );
    }

    #[test]
    fn test_string_delimiter_uses_string_color() {
        // String delimiters (", ', `) should use string color, not operator
        assert_eq!(
            scope_to_category("punctuation.definition.string.begin"),
            Some(HighlightCategory::String)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.string.end"),
            Some(HighlightCategory::String)
        );
    }

    #[test]
    fn test_punctuation_bracket() {
        // punctuation.section (TextMate standard for block delimiters)
        assert_eq!(
            scope_to_category("punctuation.section"),
            Some(HighlightCategory::PunctuationBracket)
        );
        assert_eq!(
            scope_to_category("punctuation.section.block.begin.c"),
            Some(HighlightCategory::PunctuationBracket)
        );
        assert_eq!(
            scope_to_category("punctuation.bracket"),
            Some(HighlightCategory::PunctuationBracket)
        );
        // punctuation.definition.* bracket-like scopes from sublime-syntax grammars
        assert_eq!(
            scope_to_category("punctuation.definition.array.begin.toml"),
            Some(HighlightCategory::PunctuationBracket)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.block.code.typst"),
            Some(HighlightCategory::PunctuationBracket)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.group.typst"),
            Some(HighlightCategory::PunctuationBracket)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.inline-table.begin.toml"),
            Some(HighlightCategory::PunctuationBracket)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.tag.end.svelte"),
            Some(HighlightCategory::PunctuationBracket)
        );
    }

    #[test]
    fn test_punctuation_delimiter() {
        assert_eq!(
            scope_to_category("punctuation.separator"),
            Some(HighlightCategory::PunctuationDelimiter)
        );
        assert_eq!(
            scope_to_category("punctuation.terminator.statement.c"),
            Some(HighlightCategory::PunctuationDelimiter)
        );
        assert_eq!(
            scope_to_category("punctuation.accessor"),
            Some(HighlightCategory::PunctuationDelimiter)
        );
    }

    /// First parse of a small file populates a whole-file cache; subsequent
    /// scrolls anywhere in the file are exact cache hits with no extra parse
    /// work.
    #[test]
    fn test_small_file_scroll_is_cache_hit() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("test.rs"), None, &registry);

        let mut content = String::new();
        for i in 0..200 {
            content.push_str(&format!("fn f_{i}() {{ let x = {i}; }}\n"));
        }
        let buffer = Buffer::from_str(&content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        let HighlightEngine::TextMate(ref mut tm) = engine else {
            panic!("expected TextMate engine for .rs");
        };

        // First call: cold start, full parse.
        let _ = tm.highlight_viewport(&buffer, 0, 200, &theme, 10_000);
        let stats_after_first = tm.stats().clone();
        assert_eq!(stats_after_first.cache_hits, 0, "first call cannot hit cache");
        assert_eq!(
            stats_after_first.cache_misses, 1,
            "first call must be a miss"
        );

        // Scroll anywhere — top, middle, end. All must be cache hits.
        let mid = buffer.len() / 2;
        let near_end = buffer.len().saturating_sub(200);
        let probes = [(0, 200), (mid, mid + 200), (near_end, buffer.len())];
        for (vs, ve) in probes {
            let _ = tm.highlight_viewport(&buffer, vs, ve, &theme, 10_000);
        }

        let stats_after_scroll = tm.stats().clone();
        assert_eq!(
            stats_after_scroll.cache_misses, 1,
            "scrolling must not add cache misses (got extra: {})",
            stats_after_scroll.cache_misses - 1
        );
        assert_eq!(
            stats_after_scroll.cache_hits, 3,
            "all three scroll probes must hit the cache"
        );
        assert_eq!(
            stats_after_scroll.bytes_parsed, stats_after_first.bytes_parsed,
            "scrolling must not parse any new bytes"
        );
    }

    /// After a small edit, the next render takes the partial-update path
    /// (convergence) and continues to serve cache hits afterwards. Crucially:
    /// the partial update parses far fewer bytes than the file is long.
    #[test]
    fn test_small_file_edit_uses_partial_update() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("test.rs"), None, &registry);

        let mut content = String::new();
        for i in 0..200 {
            content.push_str(&format!("fn f_{i}() {{ let x = {i}; }}\n"));
        }
        let buffer = Buffer::from_str(&content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        let HighlightEngine::TextMate(ref mut tm) = engine else {
            panic!("expected TextMate engine for .rs");
        };

        // Warm cache.
        let _ = tm.highlight_viewport(&buffer, 0, 100, &theme, 10_000);
        let bytes_before_edit = tm.stats().bytes_parsed;
        let buf_len = buffer.len();
        assert!(buf_len > 4000, "test needs a buffer larger than the partial-update region");

        // Simulate an edit deep in the file.
        let edit_pos = buf_len / 2;
        tm.notify_insert(edit_pos, 1);
        // The buffer itself doesn't change here (we test the engine in isolation),
        // but notify_insert sets dirty_from and shifts spans, which is what the
        // partial-update path consumes.

        let _ = tm.highlight_viewport(&buffer, 0, 100, &theme, 10_000);
        let bytes_after_edit = tm.stats().bytes_parsed;
        let parsed = bytes_after_edit - bytes_before_edit;

        assert!(
            parsed < buf_len,
            "edit must not trigger a whole-file reparse (parsed {parsed}, file {buf_len})"
        );
    }

    /// Convergence budget caps per-pass work even when the parse state never
    /// agrees with any existing checkpoint. Without the cap, a non-converging
    /// edit would parse the rest of the file on every keystroke.
    #[test]
    fn test_partial_update_budget_caps_work() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("test.rs"), None, &registry);

        // Build a buffer comfortably larger than CONVERGENCE_BUDGET.
        let mut content = String::new();
        while content.len() < (CONVERGENCE_BUDGET * 4) {
            content.push_str("fn name() { let mut v = 0; v += 1; }\n");
        }
        let buffer = Buffer::from_str(&content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        let HighlightEngine::TextMate(ref mut tm) = engine else {
            panic!("expected TextMate engine for .rs");
        };

        // Warm cache (whole-file parse).
        let _ = tm.highlight_viewport(&buffer, 0, 200, &theme, 10_000);
        // Simulate an edit and force every checkpoint to disagree by clearing
        // their stored states. The convergence loop will look at each marker,
        // find the slot empty, and never converge.
        tm.notify_insert(100, 0);
        tm.checkpoint_states.clear();

        let bytes_before = tm.stats().bytes_parsed;
        let _ = tm.highlight_viewport(&buffer, 0, 200, &theme, 10_000);
        let parsed = tm.stats().bytes_parsed - bytes_before;

        // Budget bounds the work to roughly CONVERGENCE_BUDGET past the dirty
        // point (plus the prefix back to the resume checkpoint). Allow a small
        // overshoot for the line that crossed the budget threshold.
        assert!(
            parsed <= CONVERGENCE_BUDGET + 4096,
            "partial update parsed {parsed}, expected <= {} \
             (budget {CONVERGENCE_BUDGET} + slack)",
            CONVERGENCE_BUDGET + 4096
        );

        // Budget hit must leave dirty_from set for follow-up passes.
        assert!(
            tm.dirty_from.is_some(),
            "budget exit must keep dirty_from set"
        );
    }

    /// Large files (above MAX_PARSE_BYTES) keep the existing windowed
    /// behaviour: parse range is bounded by ±context_bytes around the
    /// viewport, not the whole file.
    ///
    /// The viewport is placed past `MAX_PARSE_BYTES` so we exercise the
    /// "large file, no nearby checkpoint" branch in `find_parse_resume_point`
    /// — the symmetric branch that fires when `parse_end <= MAX_PARSE_BYTES`
    /// still parses from byte 0 even on big files (pre-existing behaviour,
    /// addressed in a later phase).
    #[test]
    fn test_large_file_uses_windowed_parse() {
        let registry =
            GrammarRegistry::load(&crate::primitives::grammar::LocalGrammarLoader::embedded_only());
        let mut engine = HighlightEngine::for_file(Path::new("test.rs"), None, &registry);

        // Build content well past MAX_PARSE_BYTES so we can put the viewport
        // beyond it.
        let line = "fn long_name_for_padding() { let v = 1; v + 1; }\n";
        let bytes_needed = MAX_PARSE_BYTES * 2;
        let lines_needed = bytes_needed / line.len() + 100;
        let mut content = String::with_capacity(lines_needed * line.len());
        for _ in 0..lines_needed {
            content.push_str(line);
        }
        assert!(content.len() > MAX_PARSE_BYTES * 2);
        let buffer = Buffer::from_str(&content, 0, test_fs());
        let theme = Theme::load_builtin(theme::THEME_LIGHT).unwrap();

        let HighlightEngine::TextMate(ref mut tm) = engine else {
            panic!("expected TextMate engine for .rs");
        };

        // Viewport past MAX_PARSE_BYTES: parse_end > MAX_PARSE_BYTES, so the
        // resume-from-byte-0 fallback in find_parse_resume_point doesn't fire.
        let context_bytes = 10_000usize;
        let viewport_start = MAX_PARSE_BYTES + 200_000;
        let viewport_end = viewport_start + 1000;
        let _ = tm.highlight_viewport(&buffer, viewport_start, viewport_end, &theme, context_bytes);
        let parsed = tm.stats().bytes_parsed;

        // Windowed parse covers viewport ± context_bytes plus a tiny prefix
        // for the resume anchor. Allow generous slack (4×) but reject
        // anything close to whole-file.
        let window = (viewport_end - viewport_start) + 2 * context_bytes;
        assert!(
            parsed <= window * 4,
            "large file windowed parse should be ~{window} bytes, got {parsed} \
             (file {})",
            buffer.len()
        );
    }
}
