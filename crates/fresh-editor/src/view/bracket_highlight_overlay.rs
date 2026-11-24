//! Bracket matching highlight using the overlay system
//!
//! This module manages bracket pair highlighting through overlays.
//! When the cursor is on a bracket, the matching bracket is highlighted.
//! Optional rainbow colors can be applied based on nesting depth.

use crate::model::buffer::{Buffer, ContentStamp};
use crate::model::marker::MarkerList;
use crate::primitives::highlight_engine::HighlightEngine;
use crate::view::overlay::{Overlay, OverlayFace, OverlayManager, OverlayNamespace};
use crate::view::theme::Theme;
use ratatui::style::{Color, Modifier, Style};
use std::ops::Range;

/// Default rainbow bracket colors (cycle through these based on nesting depth)
pub const DEFAULT_BRACKET_COLORS: [Color; 6] = [
    Color::Rgb(255, 215, 0),   // Gold
    Color::Rgb(218, 112, 214), // Orchid
    Color::Rgb(50, 205, 50),   // Lime Green
    Color::Rgb(30, 144, 255),  // Dodger Blue
    Color::Rgb(255, 127, 80),  // Coral
    Color::Rgb(147, 112, 219), // Medium Purple
];

/// Namespace for bracket highlight overlays
pub fn bracket_highlight_namespace() -> OverlayNamespace {
    OverlayNamespace::from_string("bracket-highlight".to_string())
}

/// Namespace for rainbow bracket colorization overlays
pub fn bracket_colorization_namespace() -> OverlayNamespace {
    OverlayNamespace::from_string("bracket-colorization".to_string())
}

/// Bracket types we match.
///
/// Angle brackets are **not** in the common set: they are delimiters only in
/// markup, and everywhere else `<` and `>` are comparison operators. See
/// [`BracketHighlightSettings::angle_brackets`] and
/// `Language::angle_brackets_are_delimiters`.
const BRACKET_PAIRS: &[(char, char)] = &[('(', ')'), ('[', ']'), ('{', '}')];

/// The set above plus `<`/`>`, for languages whose angle brackets really are
/// delimiters.
const BRACKET_PAIRS_WITH_ANGLES: &[(char, char)] =
    &[('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')];

/// The pairs that count as brackets for this buffer.
///
/// `pub(crate)` because the go-to-matching-bracket command needs the same
/// answer: two tables meant two surfaces disagreeing about whether `<` is a
/// bracket, which is how #3090 survived in the jump command after the
/// highlighter stopped counting it.
pub(crate) fn bracket_pairs(angle_brackets: bool) -> &'static [(char, char)] {
    if angle_brackets {
        BRACKET_PAIRS_WITH_ANGLES
    } else {
        BRACKET_PAIRS
    }
}

/// Maximum number of bytes to scan for bracket matching/nesting depth.
/// Prevents O(n) scans on huge files from hanging the editor.
pub(crate) const MAX_BRACKET_SEARCH_BYTES: usize = 1_000_000;

/// Chunk size for bulk reads during bracket scanning.
const BRACKET_SCAN_CHUNK: usize = 16 * 1024;

/// Bytes between the depth marks a colorization scan leaves behind it (see
/// [`DepthIndex`]): the most a frame rescans above its viewport once the
/// marks exist.
const DEPTH_MARK_INTERVAL: usize = 4 * 1024;

/// Check if a character is an opening bracket
fn is_opening_bracket(ch: char, angle_brackets: bool) -> bool {
    bracket_pairs(angle_brackets)
        .iter()
        .any(|(open, _)| *open == ch)
}

/// Check if a character is a closing bracket
fn is_closing_bracket(ch: char, angle_brackets: bool) -> bool {
    bracket_pairs(angle_brackets)
        .iter()
        .any(|(_, close)| *close == ch)
}

/// Get the opening bracket for a closing bracket
fn opening_for_closing(ch: char, angle_brackets: bool) -> Option<char> {
    bracket_pairs(angle_brackets)
        .iter()
        .find_map(|(open, close)| if *close == ch { Some(*open) } else { None })
}

/// Whether `pos` falls inside any of the given byte ranges.
///
/// `ranges` must be sorted by `start` and non-overlapping (the syntax
/// highlighter produces spans in this order), which lets us binary-search.
/// These are the comment/string ranges where brackets are prose/data rather
/// than structural punctuation, so they are excluded from bracket matching
/// and rainbow colorization (issue #2405).
fn pos_in_ranges(ranges: &[Range<usize>], pos: usize) -> bool {
    ranges
        .binary_search_by(|r| {
            if pos < r.start {
                std::cmp::Ordering::Greater
            } else if pos >= r.end {
                std::cmp::Ordering::Less
            } else {
                std::cmp::Ordering::Equal
            }
        })
        .is_ok()
}

/// The bracket matching the one at `position` — `opening`/`closing` its
/// pair, searched ahead of it when `forward` and behind it otherwise — no
/// further than `search_bound` (an absolute byte) nor
/// [`MAX_BRACKET_SEARCH_BYTES`]. Brackets inside `skip_ranges` (comments and
/// strings) are ignored, so the match is structural punctuation (#2405).
///
/// `pub(crate)` for go-to-matching-bracket, which scans the same way but
/// has no comment and string ranges to skip (they come from the
/// highlighting of the frame being drawn), so it counts their brackets too.
pub(crate) fn find_matching_bracket(
    buffer: &Buffer,
    position: usize,
    opening: char,
    closing: char,
    forward: bool,
    skip_ranges: &[Range<usize>],
    search_bound: usize,
) -> Option<usize> {
    let buffer_len = buffer.len();
    let open = opening as u8;
    let close = closing as u8;
    let mut depth: i32 = 1;

    if forward {
        let search_limit = (position + 1 + MAX_BRACKET_SEARCH_BYTES)
            .min(buffer_len)
            .min(search_bound.max(position + 1));
        let mut pos = position + 1;
        while pos < search_limit {
            let chunk_end = (pos + BRACKET_SCAN_CHUNK).min(search_limit);
            let chunk = buffer.slice_bytes(pos..chunk_end);
            for (i, &b) in chunk.iter().enumerate() {
                if pos_in_ranges(skip_ranges, pos + i) {
                    continue;
                }
                if b == open {
                    depth += 1;
                } else if b == close {
                    depth -= 1;
                    if depth == 0 {
                        return Some(pos + i);
                    }
                }
            }
            pos = chunk_end;
        }
    } else {
        let search_limit = position
            .saturating_sub(MAX_BRACKET_SEARCH_BYTES)
            .max(search_bound.min(position));
        let mut pos = position;
        while pos > search_limit {
            let chunk_start = pos.saturating_sub(BRACKET_SCAN_CHUNK).max(search_limit);
            let chunk = buffer.slice_bytes(chunk_start..pos);
            for (i, &b) in chunk.iter().enumerate().rev() {
                if pos_in_ranges(skip_ranges, chunk_start + i) {
                    continue;
                }
                if b == close {
                    depth += 1;
                } else if b == open {
                    depth -= 1;
                    if depth == 0 {
                        return Some(chunk_start + i);
                    }
                }
            }
            pos = chunk_start;
        }
    }

    None
}

/// The nearest opening bracket before `position` that its closing bracket
/// doesn't close before `position` — the bracket enclosing it — with its
/// pair: `(opening, closing, at)`. Searched no further back than
/// [`MAX_BRACKET_SEARCH_BYTES`], each of `pairs` nesting on its own.
pub(crate) fn find_enclosing_bracket(
    buffer: &Buffer,
    position: usize,
    pairs: &[(char, char)],
    skip_ranges: &[Range<usize>],
) -> Option<(char, char, usize)> {
    let search_limit = position.saturating_sub(MAX_BRACKET_SEARCH_BYTES);
    let mut depths = vec![0usize; pairs.len()];
    let mut pos = position.min(buffer.len());
    while pos > search_limit {
        let chunk_start = pos.saturating_sub(BRACKET_SCAN_CHUNK).max(search_limit);
        let chunk = buffer.slice_bytes(chunk_start..pos);
        for (i, &b) in chunk.iter().enumerate().rev() {
            if pos_in_ranges(skip_ranges, chunk_start + i) {
                continue;
            }
            for (depth, &(open, close)) in depths.iter_mut().zip(pairs) {
                if b == close as u8 {
                    *depth += 1;
                } else if b == open as u8 {
                    if *depth == 0 {
                        return Some((open, close, chunk_start + i));
                    }
                    *depth -= 1;
                }
            }
        }
        pos = chunk_start;
    }
    None
}

/// Get the matching bracket pair for a character
pub(crate) fn get_bracket_pair(ch: char, angle_brackets: bool) -> Option<(char, char, bool)> {
    for &(open, close) in bracket_pairs(angle_brackets) {
        if ch == open {
            return Some((open, close, true)); // forward search
        }
        if ch == close {
            return Some((open, close, false)); // backward search
        }
    }
    None
}

/// The `config.editor` bracket toggles, resolved for one frame.
///
/// Carried through the render pipeline instead of being latched onto the
/// per-buffer overlay at construction time, so a settings change takes effect
/// on the next frame for every buffer without a separate invalidation pass.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BracketHighlightSettings {
    /// `editor.highlight_matching_brackets`: highlight the pair the cursor
    /// sits on. Also the master switch — `rainbow_brackets` documents that it
    /// requires this to be enabled.
    pub matching: bool,
    /// `editor.rainbow_brackets`: color brackets by nesting depth.
    pub rainbow: bool,
    /// Whether `<` and `>` are brackets in this buffer's language. Not a
    /// config toggle — it is the language's answer, filled in by
    /// [`with_language`](Self::with_language) at the one place that knows
    /// which buffer is being drawn. Off for a buffer whose language is
    /// unknown, which is the safe end: a stray `<` then colours nothing
    /// rather than recolouring every bracket after it (issue #3090).
    pub angle_brackets: bool,
}

impl BracketHighlightSettings {
    /// Build from the editor config, applying the documented dependency of
    /// `rainbow_brackets` on `highlight_matching_brackets`.
    pub fn from_config(editor: &crate::config::EditorConfig) -> Self {
        Self {
            matching: editor.highlight_matching_brackets,
            rainbow: editor.highlight_matching_brackets && editor.rainbow_brackets,
            angle_brackets: false,
        }
    }

    /// Resolve [`angle_brackets`](Self::angle_brackets) for the buffer's
    /// language. `None` — no highlighter, or a grammar with no language
    /// behind it — leaves them out.
    pub fn with_language(mut self, language: Option<&fresh_languages::Language>) -> Self {
        self.angle_brackets = language.is_some_and(|l| l.angle_brackets_are_delimiters());
        self
    }
}

impl Default for BracketHighlightSettings {
    fn default() -> Self {
        Self {
            matching: true,
            rainbow: true,
            angle_brackets: false,
        }
    }
}

/// Manager for bracket highlight overlays
pub struct BracketHighlightOverlay {
    /// Colors to use for rainbow brackets (cycles through)
    pub rainbow_colors: [Color; 6],
    /// Default bracket match highlight color (when rainbow is disabled)
    pub match_color: Color,
    /// The attributes that mark the matched pair when rainbow is on (the
    /// theme's `editor.bracket_rainbow_match_modifier`).
    pub rainbow_match_modifier: Modifier,
    /// Last cursor position where we computed brackets
    last_cursor_pos: Option<usize>,
    /// Whether `<`/`>` counted as brackets when the pair overlays were last
    /// computed. Part of the short-circuit below: the pair the cursor sits on
    /// is a function of the language too, so a language switch with a
    /// stationary cursor has to re-derive rather than leave a stale `<`…`>`
    /// pair painted (or a real one unpainted).
    last_angle_brackets: Option<bool>,
    /// Whether depth-colorization overlays are currently present. Lets a
    /// disabled pass clear them exactly once instead of re-clearing (and
    /// reporting "updated") on every frame.
    colorization_active: bool,
    /// What the last colorization pass was computed from, and the byte range
    /// it wrote into. A frame whose inputs match this does no work at all:
    /// the overlays it would produce are already in place, and re-deriving
    /// them costs the whole overlay set (see `update_colorization`).
    colorization_cache: Option<ColorizationCache>,
    /// The bracket stack at line starts above the viewport, so a bracket's
    /// colour is its depth in the whole buffer rather than in a window
    /// around the viewport (issue #3478).
    depth_index: Option<DepthIndex>,
}

/// The bracket stack at known line starts, recorded as colorization scans
/// pass them, so a frame resumes from the nearest one above its viewport
/// instead of rescanning from the top of the buffer — or seeding an empty
/// stack a screenful up, which is what made colours shift by the number of
/// enclosing scopes that had scrolled out of that window (issue #3478).
///
/// Every mark is exact: the first scan starts at byte 0 with an empty stack,
/// and every later one starts at a mark. They are contiguous from the top —
/// new marks are only added past the last one — so the gap a frame rescans
/// is bounded by the mark spacing (4 KB, or one line where a line is
/// longer, since marks sit on line starts). A mark is the stack of every opener
/// before its offset that is still open there, with brackets in comments and
/// strings skipped by the same classification the viewport is drawn with.
///
/// What a mark depends on, and how each is tracked:
/// - the bytes before it: the buffer reports how much of its prefix is
///   unchanged since a stamp ([`Buffer::unchanged_prefix_since`]), so an
///   edit drops only the marks past it;
/// - how those bytes are classified: unchanged too, because marks sit on
///   line starts and the highlighter classifies line by line, forward only,
///   so an edit cannot reclassify a line above the one it lands in. Anything
///   else that could (new declared regions, a different engine) moves the
///   engine's [`classification_epoch`](HighlightEngine::classification_epoch),
///   which drops every mark;
/// - whether `<`/`>` count as brackets, which drops every mark when it flips.
struct DepthIndex {
    stamp: ContentStamp,
    classification_epoch: u64,
    angle_brackets: bool,
    /// Ascending by offset.
    marks: Vec<DepthMark>,
}

/// The stack of unclosed openers before `offset`, a line start.
#[derive(Debug, Clone, PartialEq, Eq)]
struct DepthMark {
    offset: usize,
    stack: Vec<u8>,
}

/// What one colorization pass reads: where its scan starts and ends, the
/// stack it starts with, and the comment/string ranges it skips.
#[derive(Debug, Clone, PartialEq, Eq)]
struct ScanPlan {
    start: usize,
    end: usize,
    seed: Vec<u8>,
    skip_ranges: Vec<Range<usize>>,
    /// Seeded from the depth index (so the stack is exact, and the scan may
    /// add marks) rather than from an empty stack a window above the
    /// viewport.
    exact: bool,
}

/// What a scan produced: each viewport bracket's level, and the marks it
/// passed beyond the index's last one.
struct ScanResult {
    wanted: std::collections::HashMap<usize, usize>,
    new_marks: Vec<DepthMark>,
}

/// Inputs a colorization pass depends on, plus the range it wrote.
struct ColorizationCache {
    plan: ScanPlan,
    buffer_version: u64,
    viewport: Range<usize>,
    colors: [Color; 6],
    /// Whether `<`/`>` counted as brackets — a language switch on the same
    /// buffer changes the overlays this pass would produce.
    angle_brackets: bool,
    /// The overlay set's removal counters when this pass ran — global, and
    /// for the colorization namespace. Anything that could have removed
    /// these overlays (a buffer switch, a plugin's `clearAllOverlays`, a
    /// clear of this namespace) moves one of them, and the cached overlays
    /// can no longer be assumed present.
    removal_epochs: (u64, u64),
}

/// The theme keys of the rainbow depths, in the order of `rainbow_colors`.
const RAINBOW_KEYS: [&str; 6] = [
    "editor.bracket_rainbow_1",
    "editor.bracket_rainbow_2",
    "editor.bracket_rainbow_3",
    "editor.bracket_rainbow_4",
    "editor.bracket_rainbow_5",
    "editor.bracket_rainbow_6",
];

impl BracketHighlightOverlay {
    /// Create a new bracket highlight overlay manager
    pub fn new() -> Self {
        Self {
            rainbow_colors: DEFAULT_BRACKET_COLORS,
            match_color: Color::Rgb(255, 215, 0), // Gold
            rainbow_match_modifier: Modifier::BOLD | Modifier::UNDERLINED,
            last_cursor_pos: None,
            last_angle_brackets: None,
            colorization_active: false,
            colorization_cache: None,
            depth_index: None,
        }
    }

    /// Update bracket highlights based on cursor position
    ///
    /// `skip_ranges` are byte ranges (comments and strings, sorted by start)
    /// whose brackets must be ignored — they are prose/data, not structural
    /// punctuation (issue #2405). They cover the frame's highlighted window;
    /// `highlighter` answers for text above it, which rainbow depth needs.
    ///
    /// Returns true if overlays were updated
    #[allow(clippy::too_many_arguments)]
    pub fn update(
        &mut self,
        buffer: &Buffer,
        overlays: &mut OverlayManager,
        marker_list: &mut MarkerList,
        highlighter: &HighlightEngine,
        theme: &Theme,
        settings: BracketHighlightSettings,
        cursor_position: usize,
        viewport_start: usize,
        viewport_end: usize,
        skip_ranges: &[Range<usize>],
    ) -> bool {
        let new_match_color = theme.bracket_match_fg;
        let new_rainbow_colors = [
            theme.bracket_rainbow_1,
            theme.bracket_rainbow_2,
            theme.bracket_rainbow_3,
            theme.bracket_rainbow_4,
            theme.bracket_rainbow_5,
            theme.bracket_rainbow_6,
        ];
        let new_rainbow_match_modifier = theme.bracket_rainbow_match_modifier;
        let colors_changed = self.match_color != new_match_color
            || self.rainbow_colors != new_rainbow_colors
            || self.rainbow_match_modifier != new_rainbow_match_modifier;
        if colors_changed {
            self.match_color = new_match_color;
            self.rainbow_colors = new_rainbow_colors;
            self.rainbow_match_modifier = new_rainbow_match_modifier;
        }

        let mut updated = false;

        // Update full rainbow bracket colorization
        if settings.rainbow {
            updated |= self.update_colorization(
                buffer,
                overlays,
                marker_list,
                highlighter,
                viewport_start,
                viewport_end,
                skip_ranges,
                settings.angle_brackets,
            );
        } else {
            updated |= self.clear_colorization(overlays, marker_list);
        }

        // Turning matching off has to retract the pair overlays a previously
        // enabled pass left behind — `last_cursor_pos` is Some exactly when
        // such overlays may exist, so this clears once and then no-ops.
        if !settings.matching {
            if self.last_cursor_pos.take().is_some() {
                overlays.clear_namespace(&bracket_highlight_namespace(), marker_list);
                self.last_angle_brackets = None;
                updated = true;
            }
            return updated;
        }

        if self.last_cursor_pos == Some(cursor_position)
            && self.last_angle_brackets == Some(settings.angle_brackets)
            && !colors_changed
        {
            return updated;
        }
        self.last_cursor_pos = Some(cursor_position);
        self.last_angle_brackets = Some(settings.angle_brackets);
        updated = true;

        // Clear existing bracket overlays
        let ns = bracket_highlight_namespace();
        overlays.clear_namespace(&ns, marker_list);

        // Check if cursor is on a bracket
        let buf_len = buffer.len();
        if cursor_position >= buf_len {
            return true;
        }

        let bytes = buffer.slice_bytes(cursor_position..cursor_position + 1);
        if bytes.is_empty() {
            return true;
        }

        let ch = bytes[0] as char;

        // Get bracket pair info
        let (opening, closing, forward) = match get_bracket_pair(ch, settings.angle_brackets) {
            Some(pair) => pair,
            None => return true, // Not on a bracket
        };

        // A bracket inside a comment or string is not structural punctuation,
        // so don't highlight it (issue #2405).
        if pos_in_ranges(skip_ranges, cursor_position) {
            return true;
        }

        // A match the frame cannot draw changes nothing on screen, so the
        // search covers the drawn span plus a screenful either side and stops
        // there. Unbounded by the viewport it was a megabyte of scanning per
        // keystroke on a file that is one long line — for a bracket that could
        // not have been highlighted wherever it turned out to be.
        let margin = viewport_end
            .saturating_sub(viewport_start)
            .max(BRACKET_SCAN_CHUNK);
        let search_bound = if forward {
            viewport_end.saturating_add(margin)
        } else {
            viewport_start.saturating_sub(margin)
        };

        // Find matching bracket
        let matching_pos = find_matching_bracket(
            buffer,
            cursor_position,
            opening,
            closing,
            forward,
            skip_ranges,
            search_bound,
        );

        // With rainbow on, the brackets already carry their depth color, so
        // re-painting the pair in that color changed nothing on screen
        // (issue #3084). Keep the color the colorization pass gave them — so
        // the two passes can't disagree about depth — and emphasize the pair
        // instead, with the theme's attributes. With rainbow off, the pair
        // gets the match color.
        let (face, theme_key) = if settings.rainbow {
            (
                OverlayFace::Style {
                    style: Style::default().add_modifier(self.rainbow_match_modifier),
                },
                None,
            )
        } else {
            (
                OverlayFace::Foreground {
                    color: self.match_color,
                },
                Some("editor.bracket_match_fg"),
            )
        };

        // Create overlay for the bracket at cursor
        let cursor_overlay = Overlay::with_namespace(
            marker_list,
            cursor_position..cursor_position + 1,
            face.clone(),
            ns.clone(),
        )
        .with_priority_value(10);
        let cursor_overlay = match theme_key {
            Some(key) => cursor_overlay.with_theme_key(key),
            None => cursor_overlay,
        };
        overlays.add(cursor_overlay);

        // Create overlay for the matching bracket if found
        if let Some(match_pos) = matching_pos {
            let match_overlay =
                Overlay::with_namespace(marker_list, match_pos..match_pos + 1, face, ns.clone())
                    .with_priority_value(10);
            let match_overlay = match theme_key {
                Some(key) => match_overlay.with_theme_key(key),
                None => match_overlay,
            };
            overlays.add(match_overlay);
        }

        updated
    }

    /// Force clear all highlights (e.g., when switching buffers)
    pub fn clear(&mut self, overlays: &mut OverlayManager, marker_list: &mut MarkerList) {
        let highlight_ns = bracket_highlight_namespace();
        overlays.clear_namespace(&highlight_ns, marker_list);
        let color_ns = bracket_colorization_namespace();
        overlays.clear_namespace(&color_ns, marker_list);
        self.last_cursor_pos = None;
        self.last_angle_brackets = None;
        self.colorization_active = false;
    }

    /// Force recalculation on next update
    pub fn invalidate(&mut self) {
        self.last_cursor_pos = None;
        self.last_angle_brackets = None;
    }

    /// Drop the depth-colorization overlays. Returns whether anything was
    /// actually retracted, so a permanently-disabled setting doesn't report a
    /// change on every frame.
    fn clear_colorization(
        &mut self,
        overlays: &mut OverlayManager,
        marker_list: &mut MarkerList,
    ) -> bool {
        if !self.colorization_active {
            return false;
        }
        let ns = bracket_colorization_namespace();
        overlays.clear_namespace(&ns, marker_list);
        self.colorization_active = false;
        // The overlays this cache describes are gone, so the next enabled
        // pass must rebuild rather than recognise its own inputs.
        self.colorization_cache = None;
        true
    }

    #[allow(clippy::too_many_arguments)]
    fn update_colorization(
        &mut self,
        buffer: &Buffer,
        overlays: &mut OverlayManager,
        marker_list: &mut MarkerList,
        highlighter: &HighlightEngine,
        viewport_start: usize,
        viewport_end: usize,
        skip_ranges: &[Range<usize>],
        angle_brackets: bool,
    ) -> bool {
        if viewport_start >= viewport_end || buffer.is_empty() {
            return self.clear_colorization(overlays, marker_list);
        }

        let scan_end = viewport_end.min(buffer.len());
        let windowed = || {
            // Without an exact seed, an empty stack one viewport-height up:
            // right for every scope that opens within that window, short by
            // one level per enclosing scope that opens above it.
            let viewport_size = viewport_end.saturating_sub(viewport_start);
            ScanPlan {
                start: viewport_start.saturating_sub(viewport_size),
                end: scan_end,
                seed: Vec::new(),
                skip_ranges: skip_ranges.to_vec(),
                exact: false,
            }
        };
        let plan = self
            .exact_plan(
                buffer,
                highlighter,
                viewport_start,
                scan_end,
                angle_brackets,
            )
            .unwrap_or_else(windowed);
        if plan.start >= plan.end {
            return self.clear_colorization(overlays, marker_list);
        }

        let ns = bracket_colorization_namespace();

        // Nothing this pass reads has moved since the last one, so the
        // overlays it would produce are the ones already on the buffer.
        // Without this a parked cursor re-derives them every frame, and the
        // replace below is what makes that expensive on a heavily-decorated
        // buffer (a review diff carries an overlay per line).
        if let Some(cache) = &self.colorization_cache {
            if cache.plan == plan
                && cache.buffer_version == buffer.version()
                && cache.viewport == (viewport_start..viewport_end)
                && cache.colors == self.rainbow_colors
                && cache.angle_brackets == angle_brackets
                && cache.removal_epochs == overlays.removal_epochs_for(&ns)
            {
                return false;
            }
        }

        let levels = self.rainbow_colors.len();
        let mut result = self.scan(buffer, &plan, viewport_start, angle_brackets, levels);
        let mut plan = plan;
        if result.is_none() && plan.exact {
            // Text between the mark and the viewport isn't loaded (a lazily
            // loaded buffer): colour from the window, as before the index.
            plan = windowed();
            result = self.scan(buffer, &plan, viewport_start, angle_brackets, levels);
        }
        let Some(ScanResult {
            mut wanted,
            new_marks,
        }) = result
        else {
            return self.clear_colorization(overlays, marker_list);
        };
        if let Some(index) = self.depth_index.as_mut() {
            index.marks.extend(new_marks);
        }

        self.colorization_active = !wanted.is_empty();
        // Reconcile against what this namespace already has rather than
        // replacing it: its markers moved with any edit, so typing a letter
        // leaves every overlay in place and costs no marker churn at all.
        let stale: Vec<_> = overlays
            .in_namespace(&ns)
            .filter(|o| {
                let range = o.range(marker_list);
                let current = match o.face {
                    OverlayFace::Foreground { color } if range.len() == 1 => {
                        Some((color, o.theme_key))
                    }
                    _ => None,
                };
                let want = wanted
                    .get(&range.start)
                    .map(|&level| (self.rainbow_colors[level], Some(RAINBOW_KEYS[level])));
                if current.is_some() && want == current {
                    wanted.remove(&range.start);
                    false
                } else {
                    true
                }
            })
            .map(|o| o.handle.clone())
            .collect();
        for handle in &stale {
            overlays.remove_by_handle(handle, marker_list);
        }
        overlays.extend(wanted.into_iter().map(|(pos, level)| {
            // The level's key rides along, so the theme's attributes for that
            // level apply too (and the inspector names it).
            Overlay::with_namespace(
                marker_list,
                pos..pos + 1,
                OverlayFace::Foreground {
                    color: self.rainbow_colors[level],
                },
                ns.clone(),
            )
            .with_priority_value(6)
            .with_theme_key(RAINBOW_KEYS[level])
        }));
        self.colorization_cache = Some(ColorizationCache {
            plan,
            buffer_version: buffer.version(),
            viewport: viewport_start..viewport_end,
            colors: self.rainbow_colors,
            angle_brackets,
            removal_epochs: overlays.removal_epochs_for(&ns),
        });
        true
    }

    /// A scan seeded from the depth index: from the last mark at or above
    /// the viewport, with the stack recorded there. `None` when the buffer is
    /// too large to index (beyond the scan cap, it may also be loaded lazily)
    /// or the highlighter can't classify the text between the mark and the
    /// viewport the way it classifies the viewport (see
    /// [`HighlightEngine::prose_ranges_in`]).
    fn exact_plan(
        &mut self,
        buffer: &Buffer,
        highlighter: &HighlightEngine,
        viewport_start: usize,
        scan_end: usize,
        angle_brackets: bool,
    ) -> Option<ScanPlan> {
        let buffer_len = buffer.len();
        if buffer_len > MAX_BRACKET_SEARCH_BYTES {
            self.depth_index = None;
            return None;
        }
        let epoch = highlighter.classification_epoch();
        let index = match self.depth_index.take() {
            Some(mut index)
                if index.classification_epoch == epoch
                    && index.angle_brackets == angle_brackets =>
            {
                let unchanged = buffer.unchanged_prefix_since(index.stamp);
                let keep = index.marks.partition_point(|mark| mark.offset <= unchanged);
                index.marks.truncate(keep);
                index.stamp = buffer.content_stamp();
                index
            }
            _ => DepthIndex {
                stamp: buffer.content_stamp(),
                classification_epoch: epoch,
                angle_brackets,
                marks: Vec::new(),
            },
        };
        let index = self.depth_index.insert(index);
        let from = index
            .marks
            .partition_point(|mark| mark.offset <= viewport_start);
        let (start, seed) = match from.checked_sub(1) {
            Some(i) => (index.marks[i].offset, index.marks[i].stack.clone()),
            None => (0, Vec::new()),
        };
        // The frame's own skip ranges are this same parse's answer for its
        // highlighted window, so these cover them.
        let skip_ranges = highlighter.prose_ranges_in(buffer_len, start..scan_end)?;
        Some(ScanPlan {
            start,
            end: scan_end,
            seed,
            skip_ranges,
            exact: true,
        })
    }

    /// Walk `plan`, colouring the brackets from `viewport_start` on by their
    /// depth. An exact plan also records a mark at a line start whenever it
    /// is far enough past the index's last one. `None` when part of the
    /// range isn't loaded.
    fn scan(
        &self,
        buffer: &Buffer,
        plan: &ScanPlan,
        viewport_start: usize,
        angle_brackets: bool,
        levels: usize,
    ) -> Option<ScanResult> {
        let mut stack = plan.seed.clone();
        let mut wanted = std::collections::HashMap::new();
        let mut new_marks = Vec::new();
        // Marks are only added past the index's last one, which keeps them
        // contiguous from the top of the buffer.
        let mut last_mark = plan.exact.then(|| {
            self.depth_index
                .as_ref()
                .and_then(|index| index.marks.last())
                .map_or(0, |mark| mark.offset)
                .max(plan.start)
        });
        let mut pos = plan.start;
        while pos < plan.end {
            let chunk_end = (pos + BRACKET_SCAN_CHUNK).min(plan.end);
            let chunk = buffer.slice_bytes(pos..chunk_end);
            if chunk.len() != chunk_end - pos {
                return None;
            }
            for (i, &byte) in chunk.iter().enumerate() {
                let at = pos + i;
                if byte == b'\n' {
                    if let Some(last) = last_mark.as_mut() {
                        // Spaced by the stack's size as well, so a deep
                        // stack can't cost more memory than the text it
                        // came from.
                        let line_start = at + 1;
                        if line_start.saturating_sub(*last) >= DEPTH_MARK_INTERVAL.max(stack.len())
                        {
                            new_marks.push(DepthMark {
                                offset: line_start,
                                stack: stack.clone(),
                            });
                            *last = line_start;
                        }
                    }
                    continue;
                }
                let c = byte as char;
                let opening = is_opening_bracket(c, angle_brackets);
                if !opening && !is_closing_bracket(c, angle_brackets) {
                    continue;
                }

                // Brackets inside comments/strings are prose, not structure:
                // don't colorize them and don't let them affect the nesting
                // depth of real brackets (issue #2405). Checked only for
                // brackets, because that is the only thing it can veto and
                // this runs per byte.
                if pos_in_ranges(&plan.skip_ranges, at) {
                    continue;
                }

                if opening {
                    let depth = stack.len();
                    stack.push(byte);
                    if at >= viewport_start {
                        wanted.insert(at, depth % levels);
                    }
                    continue;
                }

                let depth = stack.len().saturating_sub(1);
                if let Some(expected_open) = opening_for_closing(c, angle_brackets) {
                    if stack.last() == Some(&(expected_open as u8)) {
                        stack.pop();
                    }
                }
                if at >= viewport_start {
                    wanted.insert(at, depth % levels);
                }
            }
            pos = chunk_end;
        }
        Some(ScanResult { wanted, new_marks })
    }
}

impl Default for BracketHighlightOverlay {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::buffer::Buffer;

    /// Run one colorization pass over the whole buffer as the renderer
    /// would, returning whether it did any work.
    fn colorize_frame(
        overlay: &mut BracketHighlightOverlay,
        buffer: &Buffer,
        overlays: &mut OverlayManager,
        markers: &mut MarkerList,
    ) -> bool {
        overlay.update_colorization(
            buffer,
            overlays,
            markers,
            &HighlightEngine::None,
            0,
            buffer.len(),
            &[],
            false,
        )
    }

    /// The colour painted at `byte`, scanning the whole buffer.
    fn colour_at(buffer: &Buffer, byte: usize, skip_ranges: &[Range<usize>]) -> Option<Color> {
        let overlay = BracketHighlightOverlay::new();
        let plan = ScanPlan {
            start: 0,
            end: buffer.len(),
            seed: Vec::new(),
            skip_ranges: skip_ranges.to_vec(),
            exact: false,
        };
        let levels = overlay.rainbow_colors.len();
        let result = overlay.scan(buffer, &plan, 0, false, levels)?;
        result
            .wanted
            .get(&byte)
            .map(|&level| overlay.rainbow_colors[level])
    }

    /// One colorization pass over `viewport` as the renderer makes it,
    /// returning the colour painted at `byte`.
    fn colour_in_frame(
        overlay: &mut BracketHighlightOverlay,
        buffer: &Buffer,
        highlighter: &HighlightEngine,
        viewport: Range<usize>,
        angle_brackets: bool,
        byte: usize,
    ) -> Option<Color> {
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        // A fresh overlay set every frame, so the pass can't short-circuit
        // on overlays it left behind.
        overlay.colorization_cache = None;
        overlay.update_colorization(
            buffer,
            &mut overlays,
            &mut markers,
            highlighter,
            viewport.start,
            viewport.end,
            &[],
            angle_brackets,
        );
        let ns = bracket_colorization_namespace();
        let found = overlays.in_namespace(&ns).find_map(|o| match o.face {
            OverlayFace::Foreground { color } if o.range(&markers).start == byte => Some(color),
            _ => None,
        });
        found
    }

    /// A bracket's colour is its depth in the whole buffer, however far
    /// above the viewport its enclosing scopes open (issue #3478).
    #[test]
    fn depth_counts_scopes_opened_far_above_the_viewport() {
        let filler = "x\n".repeat(20_000);
        let text = format!("{{\n[\n{filler}()\n");
        let buffer = Buffer::from_str_test(&text);
        let pair = text.find('(').unwrap();
        let mut overlay = BracketHighlightOverlay::new();
        let colour = colour_in_frame(
            &mut overlay,
            &buffer,
            &HighlightEngine::None,
            pair - 100..buffer.len(),
            false,
            pair,
        );
        assert_eq!(colour, Some(overlay.rainbow_colors[2]));
    }

    /// Marks recorded while `<` was not a bracket say nothing about a
    /// language where it is.
    #[test]
    fn depth_marks_are_dropped_when_angle_brackets_start_counting() {
        let filler = "x\n".repeat(20_000);
        let text = format!("<\n{filler}()\n");
        let buffer = Buffer::from_str_test(&text);
        let pair = text.find('(').unwrap();
        let viewport = pair - 100..buffer.len();
        let mut overlay = BracketHighlightOverlay::new();
        let none = HighlightEngine::None;

        let without = colour_in_frame(&mut overlay, &buffer, &none, viewport.clone(), false, pair);
        assert_eq!(without, Some(overlay.rainbow_colors[0]));
        let with = colour_in_frame(&mut overlay, &buffer, &none, viewport, true, pair);
        assert_eq!(
            with,
            Some(overlay.rainbow_colors[1]),
            "the `<` encloses it now"
        );
    }

    /// Marks recorded under one classification of the text above the
    /// viewport say nothing about another: here a highlighter arrives that
    /// knows the `{` up top is inside a string.
    #[test]
    fn depth_marks_are_dropped_when_the_classification_changes() {
        let filler = "  \"x\": 1,\n".repeat(2_000);
        let text = format!("[\"{{\",\n{{\n{filler}  \"y\": [1]\n}}]\n");
        let buffer = Buffer::from_str_test(&text);
        let inner = text.rfind('[').unwrap();
        let viewport = inner - 100..buffer.len();
        let mut overlay = BracketHighlightOverlay::new();

        // Unclassified, the quoted `{` counts: `[`, `{`, `{` enclose it.
        let unclassified = colour_in_frame(
            &mut overlay,
            &buffer,
            &HighlightEngine::None,
            viewport.clone(),
            false,
            inner,
        );
        assert_eq!(unclassified, Some(overlay.rainbow_colors[3]));

        let registry = crate::primitives::grammar::GrammarRegistry::load(
            &crate::primitives::grammar::LocalGrammarLoader::embedded_only(),
        );
        let mut json = HighlightEngine::for_file(std::path::Path::new("a.json"), None, &registry);
        let theme = Theme::load_builtin("dark").expect("the built-in dark theme");
        json.highlight_viewport(&buffer, viewport.start, viewport.end, &theme, 1000);
        let classified = colour_in_frame(&mut overlay, &buffer, &json, viewport, false, inner);
        assert_eq!(
            classified,
            Some(overlay.rainbow_colors[2]),
            "the quoted `{{` is text"
        );
    }

    /// The skip check sits behind the test for a bracket, so this pins that a
    /// brace in a comment is still neither painted nor counted (issue #2405).
    #[test]
    fn a_bracket_in_a_skipped_span_is_neither_painted_nor_counted() {
        // Two skipped spans, so the binary search has more than one entry.
        let text = "# prose brace: {\n()\n# and a tail: }\n";
        let prose = text.find('{').expect("the prose brace");
        let pair = text.find('(').expect("the real pair");
        let tail = text.find('}').expect("the tail brace");
        let buffer = Buffer::from_str_test(text);
        let skips = [prose..prose + 1, tail..tail + 1];

        assert_eq!(colour_at(&buffer, prose, &skips), None, "not painted");

        // The same file without the stray braces: the pair is at depth 0 both
        // ways, so a skipped brace did not push it to 1.
        let clean = Buffer::from_str_test(&text.replace(['{', '}'], " "));
        assert_eq!(
            colour_at(&buffer, pair, &skips),
            colour_at(&clean, pair, &[]),
            "a skipped brace must not shift the depth of real brackets"
        );
    }

    /// One `update` as the renderer makes it, for a cursor that does not move.
    fn matching_frame(
        overlay: &mut BracketHighlightOverlay,
        buffer: &Buffer,
        overlays: &mut OverlayManager,
        markers: &mut MarkerList,
        cursor: usize,
        angle_brackets: bool,
    ) {
        overlay.update(
            buffer,
            overlays,
            markers,
            &HighlightEngine::None,
            &Theme::load_builtin("dark").expect("the built-in dark theme"),
            BracketHighlightSettings {
                matching: true,
                rainbow: false,
                angle_brackets,
            },
            cursor,
            0,
            buffer.len(),
            &[],
        );
    }

    /// The pair overlays are a function of the language as well as the
    /// cursor, so the short-circuit that skips a frame for a stationary
    /// cursor has to notice an angle-bracket change too. It did not: a
    /// language switch with the caret parked on a `<` left the old `<`…`>`
    /// pair painted until the caret moved.
    #[test]
    fn a_language_switch_redraws_the_pair_under_a_stationary_cursor() {
        let buffer = Buffer::from_str_test("<span>hi</span>\n");
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        let mut overlay = BracketHighlightOverlay::new();

        // Caret on the opening `<`, in a language where it is a delimiter.
        matching_frame(&mut overlay, &buffer, &mut overlays, &mut markers, 0, true);
        let with_angles = overlays.len();
        assert!(with_angles > 0, "the tag's delimiters are a pair in markup");

        // The same caret, in a language where it is not.
        matching_frame(&mut overlay, &buffer, &mut overlays, &mut markers, 0, false);
        assert_eq!(
            overlays.len(),
            0,
            "the pair must be retracted without waiting for the caret to move"
        );

        // And back again.
        matching_frame(&mut overlay, &buffer, &mut overlays, &mut markers, 0, true);
        assert_eq!(
            overlays.len(),
            with_angles,
            "switching back repaints it, again without moving the caret"
        );
    }

    /// A frame that changed nothing the pass reads must do nothing. Without
    /// this the pass re-derives its overlays every frame, and the replace
    /// that publishes them scans the buffer's whole overlay set — which is
    /// what made a large review diff cost ~230ms per frame instead of ~3ms.
    #[test]
    fn colorization_skips_a_frame_whose_inputs_have_not_moved() {
        let buffer = Buffer::from_str_test("fn a() { b(); }\n");
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        let mut overlay = BracketHighlightOverlay::new();

        assert!(
            colorize_frame(&mut overlay, &buffer, &mut overlays, &mut markers),
            "the first pass has overlays to publish"
        );
        let after_first = overlays.len();
        assert!(after_first > 0, "brackets in view should be colorized");

        assert!(
            !colorize_frame(&mut overlay, &buffer, &mut overlays, &mut markers),
            "an unchanged frame must not re-derive the colorization"
        );
        assert_eq!(
            overlays.len(),
            after_first,
            "and must leave the overlays it published alone"
        );
    }

    /// The cache is a claim about overlays still being on the buffer, so
    /// anything that removes them has to break it — otherwise the brackets
    /// stay uncolored until some unrelated input happens to move.
    #[test]
    fn colorization_rebuilds_after_the_overlays_are_cleared() {
        let buffer = Buffer::from_str_test("fn a() { b(); }\n");
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        let mut overlay = BracketHighlightOverlay::new();

        colorize_frame(&mut overlay, &buffer, &mut overlays, &mut markers);
        let published = overlays.len();
        assert!(published > 0);

        // Something else wipes the buffer's overlays — a buffer switch, or a
        // plugin calling `clearAllOverlays`.
        overlays.clear(&mut markers);
        assert_eq!(overlays.len(), 0);

        assert!(
            colorize_frame(&mut overlay, &buffer, &mut overlays, &mut markers),
            "a cleared buffer must be re-colorized, not assumed current"
        );
        assert_eq!(overlays.len(), published);
    }

    /// An edit moves the buffer version, and the brackets with it.
    #[test]
    fn colorization_rebuilds_after_an_edit() {
        let mut buffer = Buffer::from_str_test("fn a() { b(); }\n");
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        let mut overlay = BracketHighlightOverlay::new();

        colorize_frame(&mut overlay, &buffer, &mut overlays, &mut markers);
        buffer.insert(0, "{}\n");

        assert!(
            colorize_frame(&mut overlay, &buffer, &mut overlays, &mut markers),
            "an edited buffer must be re-colorized"
        );
    }

    /// An edit that adds or removes no bracket leaves the overlays in place:
    /// their markers moved with it, so there is nothing to retract or re-add.
    #[test]
    fn typing_a_letter_keeps_the_bracket_overlays() {
        let mut buffer = Buffer::from_str_test("fn a() { b(); }\n");
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        let mut overlay = BracketHighlightOverlay::new();
        let handles = |overlays: &OverlayManager| -> std::collections::HashSet<_> {
            overlays.all().iter().map(|o| o.handle.clone()).collect()
        };
        colorize_frame(&mut overlay, &buffer, &mut overlays, &mut markers);
        let before = handles(&overlays);
        assert_eq!(before.len(), 6);

        markers.adjust_for_insert(0, 1);
        buffer.insert(0, "x");
        assert!(colorize_frame(
            &mut overlay,
            &buffer,
            &mut overlays,
            &mut markers
        ));
        assert_eq!(handles(&overlays), before);

        // A bracket after the others adds its overlay and moves none of theirs.
        let end = buffer.len();
        markers.adjust_for_insert(end, 1);
        buffer.insert(end, "(");
        colorize_frame(&mut overlay, &buffer, &mut overlays, &mut markers);
        let after = handles(&overlays);
        assert_eq!(after.len(), 7);
        assert_eq!(after.intersection(&before).count(), 6);
    }

    #[test]
    fn test_bracket_pair_detection() {
        assert!(is_opening_bracket('(', false));
        assert!(is_opening_bracket('[', false));
        assert!(is_opening_bracket('{', false));
        assert!(!is_opening_bracket(')', false));
        assert!(!is_opening_bracket('a', false));

        assert!(is_closing_bracket(')', false));
        assert!(is_closing_bracket(']', false));
        assert!(is_closing_bracket('}', false));
        assert!(!is_closing_bracket('(', false));
        assert!(!is_closing_bracket('a', false));
    }

    /// Issue #3090: `<` and `>` are brackets only where the language says
    /// they are, and an unmatched one must not push a nesting level for the
    /// brackets that follow.
    #[test]
    fn angle_brackets_count_only_for_markup_languages() {
        assert!(!is_opening_bracket('<', false));
        assert!(!is_closing_bracket('>', false));
        assert_eq!(get_bracket_pair('<', false), None);

        assert!(is_opening_bracket('<', true));
        assert!(is_closing_bracket('>', true));
        assert_eq!(get_bracket_pair('<', true), Some(('<', '>', true)));
    }

    #[test]
    fn test_get_bracket_pair() {
        assert_eq!(get_bracket_pair('(', false), Some(('(', ')', true)));
        assert_eq!(get_bracket_pair(')', false), Some(('(', ')', false)));
        assert_eq!(get_bracket_pair('[', false), Some(('[', ']', true)));
        assert_eq!(get_bracket_pair(']', false), Some(('[', ']', false)));
        assert_eq!(get_bracket_pair('a', false), None);
    }

    #[test]
    fn test_find_matching_bracket_forward() {
        let buffer = Buffer::from_str_test("(hello)");

        let result = find_matching_bracket(&buffer, 0, '(', ')', true, &[], usize::MAX);
        assert_eq!(result, Some(6));
    }

    #[test]
    fn test_find_matching_bracket_backward() {
        let buffer = Buffer::from_str_test("(hello)");

        let result = find_matching_bracket(&buffer, 6, '(', ')', false, &[], 0);
        assert_eq!(result, Some(0));
    }

    #[test]
    fn test_find_matching_bracket_nested() {
        let buffer = Buffer::from_str_test("((inner))");

        // Outer opening bracket should match outer closing
        let result = find_matching_bracket(&buffer, 0, '(', ')', true, &[], usize::MAX);
        assert_eq!(result, Some(8));

        // Inner opening bracket should match inner closing
        let result = find_matching_bracket(&buffer, 1, '(', ')', true, &[], usize::MAX);
        assert_eq!(result, Some(7));
    }

    #[test]
    fn test_pos_in_ranges() {
        let ranges = [2..5, 8..10];
        assert!(!pos_in_ranges(&ranges, 1));
        assert!(pos_in_ranges(&ranges, 2)); // inclusive start
        assert!(pos_in_ranges(&ranges, 4));
        assert!(!pos_in_ranges(&ranges, 5)); // exclusive end
        assert!(!pos_in_ranges(&ranges, 6));
        assert!(pos_in_ranges(&ranges, 9));
        assert!(!pos_in_ranges(&ranges, 10));
        assert!(!pos_in_ranges(&[], 0));
    }

    #[test]
    // A slice of byte ranges is the argument, not a range of values.
    #[allow(clippy::single_range_in_vec_init)]
    fn test_find_matching_bracket_skips_comment_bracket() {
        // `( # ) )` — the first `)` sits inside a "comment" range and must be
        // ignored, so the opening `(` matches the second `)`.
        let buffer = Buffer::from_str_test("(a)b)");

        // Without skipping, `(` at 0 matches the `)` at 2.
        assert_eq!(
            find_matching_bracket(&buffer, 0, '(', ')', true, &[], usize::MAX),
            Some(2)
        );

        // Treat byte 2 (the first `)`) as inside a comment: it should be
        // skipped, so the match becomes the `)` at 4.
        assert_eq!(
            find_matching_bracket(&buffer, 0, '(', ')', true, &[2..3], usize::MAX),
            Some(4)
        );
    }

    #[test]
    // A slice of byte ranges is the argument, not a range of values.
    #[allow(clippy::single_range_in_vec_init)]
    fn test_find_enclosing_bracket() {
        let pairs = bracket_pairs(false);
        let buffer = Buffer::from_str_test("f(a, [b], {c}) x");
        // Inside the parens, past closed `[...]` and `{...}`.
        assert_eq!(
            find_enclosing_bracket(&buffer, 13, pairs, &[]),
            Some(('(', ')', 1))
        );
        // Inside the brackets.
        assert_eq!(
            find_enclosing_bracket(&buffer, 6, pairs, &[]),
            Some(('[', ']', 5))
        );
        // Outside everything.
        assert_eq!(find_enclosing_bracket(&buffer, 15, pairs, &[]), None);
        assert_eq!(find_enclosing_bracket(&buffer, 0, pairs, &[]), None);
        // A `(` in a comment encloses nothing.
        let buffer = Buffer::from_str_test("{ // (\n x");
        assert_eq!(
            find_enclosing_bracket(&buffer, 9, pairs, &[2..6]),
            Some(('{', '}', 0))
        );
    }

    /// Across the scan's chunk boundaries, both finders give what a naive
    /// byte-at-a-time scan gives.
    #[test]
    fn test_bracket_finders_across_chunks() {
        let mut text = "(".to_string();
        text.push_str(&"[x]".repeat(BRACKET_SCAN_CHUNK));
        text.push_str(" here ");
        text.push_str(&"{y}".repeat(BRACKET_SCAN_CHUNK));
        text.push(')');
        let buffer = Buffer::from_str_test(&text);
        let here = text.find("here").unwrap();
        let pairs = bracket_pairs(false);
        assert_eq!(
            find_enclosing_bracket(&buffer, here, pairs, &[]),
            Some(('(', ')', 0))
        );
        let last = text.len() - 1;
        assert_eq!(
            find_matching_bracket(&buffer, 0, '(', ')', true, &[], usize::MAX),
            Some(last)
        );
        assert_eq!(
            find_matching_bracket(&buffer, last, '(', ')', false, &[], 0),
            Some(0)
        );
    }

    fn naive_enclosing(text: &[u8], position: usize) -> Option<(char, char, usize)> {
        let pairs = bracket_pairs(false);
        let mut depths = vec![0usize; pairs.len()];
        for at in (0..position).rev() {
            for (depth, &(open, close)) in depths.iter_mut().zip(pairs) {
                if text[at] == close as u8 {
                    *depth += 1;
                } else if text[at] == open as u8 {
                    if *depth == 0 {
                        return Some((open, close, at));
                    }
                    *depth -= 1;
                }
            }
        }
        None
    }

    proptest::proptest! {
        #[test]
        fn prop_find_enclosing_bracket_is_the_naive_scan(
            text in "[()\\[\\]{}a]{0,64}",
            position in 0usize..70,
        ) {
            let buffer = Buffer::from_str_test(&text);
            let position = position.min(text.len());
            proptest::prop_assert_eq!(
                find_enclosing_bracket(&buffer, position, bracket_pairs(false), &[]),
                naive_enclosing(text.as_bytes(), position)
            );
        }
    }
}
