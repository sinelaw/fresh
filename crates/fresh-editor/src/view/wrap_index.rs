//! Byte ↔ visual row: the coordinate service.
//!
//! Replaces the deleted `VisualRowIndex` and the row-count
//! side of the wrap caches. Two things make it different from what it replaces,
//! and both are the point:
//!
//! 1. **It is not keyed on the buffer version.** Every cache in this area folds
//!    `buffer.version()` into its key, so an edit invalidates it and the next
//!    frame rebuilds from scratch — which is why a 500 KB single-line file
//!    re-wraps entirely on each keystroke despite two caches sitting right
//!    there. This structure is *repaired* by [`WrapIndex::damage_bytes`]: an
//!    edit at byte N leaves every row boundary before N's row untouched, and
//!    rewrapping forward from there resynchronises within a row or two.
//!
//! 2. **Totals are a Fenwick tree**, not a prefix-sum array — a flat array makes
//!    every edit O(lines) just to re-shift the sums.
//!
//! Byte↔line is deliberately *not* stored here: the buffer's own line index
//! already answers it and already shifts on edit, and duplicating it would mean
//! two structures to keep repaired.
//!
//! The index is **canonical** — built with no cursors, so cursor movement never
//! damages it. Cursor-aware layout exists only inside the renderer's window.

use crate::model::buffer::{Buffer, LineEnding};
use crate::view::line_wrap_cache::CacheViewMode;
use crate::view::ui::split_rendering::base_tokens::build_line_tokens_from;
use crate::view::wrap_machine::{RowCarry, RowInfo, WrapMachine, WrapOutput, WrapRule};
use fresh_core::api::{ViewTokenWire, ViewTokenWireKind};

/// Everything that changes where rows break, other than the text itself.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WrapIndexGeometry {
    pub rule: WrapRule,
    pub view_mode: CacheViewMode,
    /// Hash of the collapsed fold ranges, or 0 when nothing is folded.
    ///
    /// Folds live on the split, not the buffer — two panes on one file can fold
    /// differently — so they are part of the *view* half of the key, next to
    /// the rule and the view mode, rather than the buffer-global decorations.
    ///
    /// They are in the key at all because a collapsed line occupies no visual
    /// row, and a coordinate system that counts rows nobody can scroll to gives
    /// the scrollbar phantom rows and makes the wheel move a different distance
    /// than it reports. Keeping folds out instead means every consumer applies
    /// its own correction, which is what `folds.is_empty()` was standing in for
    /// — the render path did not correct for folds, it declined to run.
    pub fold_signature: u64,
}

/// Hash the collapsed ranges into a [`WrapIndexGeometry::fold_signature`].
pub fn fold_signature(folds: &[std::ops::Range<usize>]) -> u64 {
    use std::hash::{Hash, Hasher};
    if folds.is_empty() {
        return 0;
    }
    let mut h = std::collections::hash_map::DefaultHasher::new();
    for r in folds {
        r.start.hash(&mut h);
        r.end.hash(&mut h);
    }
    // 0 means "nothing folded"; a real fold set must never collide with it.
    h.finish() | 1
}

/// Plugin decorations that move row boundaries, snapshotted for one build.
///
/// Owned data rather than a borrow of the managers, because the build holds
/// `&mut Buffer` and a `&EditorState` alongside it does not borrow. Resolve it
/// with [`crate::state::EditorState::index_decorations`] before the build.
///
/// Cursor-blind by construction: every query is made with an empty cursor list.
/// That is the index's defining convention, not an approximation — scroll
/// position must not change because a cursor moved, which is also why
/// `pipeline_inputs_version` leaves cursor position out. Cursor-dependent
/// activation belongs to the renderer's window, where it is cheap and local.
#[derive(Debug, Clone, Default)]
pub struct IndexDecorations {
    /// Sorted `(byte, indent)`.
    pub soft_breaks: Vec<(usize, u16)>,
    /// Sorted by start; `None` replacement means "hide", `Some` means "replace".
    pub conceals: Vec<(std::ops::Range<usize>, Option<String>)>,
    /// Inline hints, styleless — the index measures and never draws.
    pub inline_hints: Vec<crate::view::ui::split_rendering::transforms::InlineHint>,
    /// Collapsed byte ranges, sorted by start. Per split, unlike the rest —
    /// see [`WrapIndexGeometry::fold_signature`], which keys the index on them.
    pub folds: Vec<std::ops::Range<usize>>,
    /// Sorted anchor bytes of plugin virtual *lines* (whole extra rows, e.g.
    /// markdown_compose's table borders). Part of the snapshot so the index
    /// derives per-line virtual row counts itself — callers used to pass a
    /// closure over a side list, which left the snapshot incomplete and the
    /// virtual half undiffable.
    pub virtual_lines: Vec<usize>,
}

impl IndexDecorations {
    pub fn is_empty(&self) -> bool {
        self.soft_breaks.is_empty()
            && self.conceals.is_empty()
            && self.inline_hints.is_empty()
            && self.folds.is_empty()
            && self.virtual_lines.is_empty()
    }

    /// Virtual lines anchored in `start..end`.
    pub fn virtual_rows_in(&self, start: usize, end: usize) -> u32 {
        let lo = self.virtual_lines.partition_point(|p| *p < start);
        let hi = self.virtual_lines.partition_point(|p| *p < end);
        (hi - lo) as u32
    }

    /// Move every stored position the way the buffer's markers moved for this
    /// edit, so the snapshot stays in current buffer coordinates.
    ///
    /// Positions inside the removed span clamp to its start; positions after
    /// it shift by the edit's delta; a position exactly at an insertion point
    /// stays put (left gravity). The live markers resolve gravity per marker,
    /// so an entry *at* the edit boundary can land one byte off — but such an
    /// entry is on the edited line, whose decorations the plugin re-emits (the
    /// edit re-fires `lines_changed` for it), and the re-emit shows up as a
    /// snapshot diff that repairs the line exactly. Every entry *away* from
    /// the boundary shifts identically to its marker, no gravity involved.
    ///
    /// The map is monotone, so all five lists stay sorted.
    pub fn shift_for_edit(&mut self, start: usize, removed: usize, inserted: usize) {
        if removed == 0 && inserted == 0 {
            return;
        }
        let end_old = start + removed;
        let delta = inserted as i64 - removed as i64;
        let shift = |p: usize| -> usize {
            if p <= start {
                p
            } else if p < end_old {
                start
            } else {
                (p as i64 + delta).max(0) as usize
            }
        };
        for (p, _) in &mut self.soft_breaks {
            *p = shift(*p);
        }
        for p in &mut self.virtual_lines {
            *p = shift(*p);
        }
        for h in &mut self.inline_hints {
            h.anchor = shift(h.anchor);
        }
        for (r, _) in &mut self.conceals {
            let s = shift(r.start);
            *r = s..shift(r.end).max(s);
        }
        for r in &mut self.folds {
            let s = shift(r.start);
            *r = s..shift(r.end).max(s);
        }
    }

    /// Byte ranges where `self` and `new` disagree, coalesced and sorted — the
    /// exact damage a decoration change did, computed with no help from the
    /// managers. Every entry present in one snapshot but not the other (moved
    /// entries count as removed + added) contributes its range; a line whose
    /// decorations appear in neither diff side wraps identically under both
    /// snapshots, which is what makes repairing only these ranges sound.
    fn changed_ranges(&self, new: &IndexDecorations) -> Vec<std::ops::Range<usize>> {
        let mut ranges: Vec<std::ops::Range<usize>> = Vec::new();

        diff_sorted(
            &self.soft_breaks,
            &new.soft_breaks,
            |a| a.0,
            |a, b| a == b,
            &mut |e: &(usize, u16)| ranges.push(e.0..e.0 + 1),
        );
        diff_sorted(
            &self.conceals,
            &new.conceals,
            |a| a.0.start,
            |a, b| a == b,
            &mut |e: &(std::ops::Range<usize>, Option<String>)| {
                ranges.push(e.0.start..e.0.end.max(e.0.start + 1))
            },
        );
        // Layout-relevant hint equality: anchor, text, and placement decide
        // where rows break; style is draw-only (and `None` on index paths).
        diff_sorted(
            &self.inline_hints,
            &new.inline_hints,
            |h| h.anchor,
            |a, b| a.anchor == b.anchor && a.text == b.text && a.position == b.position,
            &mut |h: &crate::view::ui::split_rendering::transforms::InlineHint| {
                ranges.push(h.anchor..h.anchor + 1)
            },
        );
        diff_sorted(
            &self.virtual_lines,
            &new.virtual_lines,
            |p| *p,
            |a, b| a == b,
            &mut |p: &usize| ranges.push(*p..*p + 1),
        );
        // Folds are part of the geometry key, so two snapshots under one
        // geometry should agree — diffed anyway so correctness never rests on
        // `fold_signature` being collision-free.
        diff_sorted(
            &self.folds,
            &new.folds,
            |r| r.start,
            |a, b| a == b,
            &mut |r: &std::ops::Range<usize>| ranges.push(r.start..r.end.max(r.start + 1)),
        );

        ranges.sort_by_key(|r| r.start);
        let mut merged: Vec<std::ops::Range<usize>> = Vec::with_capacity(ranges.len());
        for r in ranges {
            match merged.last_mut() {
                Some(last) if r.start <= last.end => last.end = last.end.max(r.end),
                _ => merged.push(r),
            }
        }
        merged
    }

    /// Is every byte of `line_start..line_end` inside a collapsed range?
    ///
    /// A fold's header line only *partly* overlaps its range — its own text is
    /// still drawn with the folded tail skipped — so it is not hidden. The lines
    /// after it are, and those are the ones that must stop occupying rows.
    fn line_is_hidden(&self, line_start: usize, line_end: usize) -> bool {
        if self.folds.is_empty() {
            return false;
        }
        if line_start >= line_end {
            return self.folds.iter().any(|r| r.contains(&line_start));
        }
        self.folds
            .iter()
            .any(|r| r.start <= line_start && line_end <= r.end)
    }

    /// The decorations touching `line_start..line_end`, as the transforms want
    /// them. Conceals are kept whenever they *overlap* the line rather than
    /// start inside it: one spanning a line break still hides part of this line.
    fn for_line(
        &self,
        line_start: usize,
        line_end: usize,
    ) -> (
        Vec<(usize, u16)>,
        Vec<(std::ops::Range<usize>, Option<&str>)>,
        Vec<crate::view::ui::split_rendering::transforms::InlineHint>,
    ) {
        let breaks = self
            .soft_breaks
            .iter()
            .filter(|(p, _)| *p >= line_start && *p < line_end)
            .copied()
            .collect();
        let conceals = self
            .conceals
            .iter()
            .filter(|(r, _)| r.start < line_end && r.end > line_start)
            .map(|(r, t)| (r.clone(), t.as_deref()))
            .collect();
        let hints = self
            .inline_hints
            .iter()
            .filter(|h| h.anchor >= line_start && h.anchor < line_end)
            .cloned()
            .collect();
        (breaks, conceals, hints)
    }
}

/// Two-pointer symmetric difference over position-sorted slices.
///
/// Items whose sort keys differ belong to one side only; items at the same
/// key are compared with `eq` (equal → common, skipped; different → both are
/// damage). Ties on the key are resolved conservatively — a run of same-key
/// entries that differs in any way reports the whole run — which only ever
/// *over*-reports damage, never under.
fn diff_sorted<T>(
    old: &[T],
    new: &[T],
    key: impl Fn(&T) -> usize,
    eq: impl Fn(&T, &T) -> bool,
    damaged: &mut impl FnMut(&T),
) {
    let (mut i, mut j) = (0, 0);
    while i < old.len() && j < new.len() {
        let (a, b) = (&old[i], &new[j]);
        match key(a).cmp(&key(b)) {
            std::cmp::Ordering::Less => {
                damaged(a);
                i += 1;
            }
            std::cmp::Ordering::Greater => {
                damaged(b);
                j += 1;
            }
            std::cmp::Ordering::Equal => {
                if !eq(a, b) {
                    damaged(a);
                    damaged(b);
                }
                i += 1;
                j += 1;
            }
        }
    }
    for a in &old[i..] {
        damaged(a);
    }
    for b in &new[j..] {
        damaged(b);
    }
}

/// Row structure of one logical line.
#[derive(Debug, Clone)]
pub struct LineWrap {
    /// Line-relative byte offset of each visual row's start. Always begins at 0.
    pub row_starts: Vec<u32>,
    /// State to resume each row with — what makes mid-line rendering and
    /// incremental repair possible.
    pub carries: Vec<RowCarry>,
    /// Whether each row can be resumed at by byte alone.
    ///
    /// False when the row opens with injected content the carry cannot
    /// reconstruct: a soft break's newline, an inline hint that wrapped onto
    /// this row, a conceal's replacement text. Whether such content sits on this
    /// row or the previous one is a decision the wrap made, and a byte offset
    /// does not record it. A hanging indent *is* reconstructible — it is in the
    /// carry — so indented continuations stay resumable.
    pub resumable: Vec<bool>,
    /// Plugin virtual lines anchored in this logical line.
    pub virtual_rows: u32,
    /// Every byte of this line is inside a collapsed fold, so it draws nothing
    /// and occupies no visual row.
    pub hidden: bool,
}

impl Default for LineWrap {
    fn default() -> Self {
        Self {
            row_starts: vec![0],
            carries: vec![RowCarry::default()],
            resumable: vec![true],
            virtual_rows: 0,
            hidden: false,
        }
    }
}

impl LineWrap {
    pub fn wrap_rows(&self) -> u32 {
        self.row_starts.len() as u32
    }

    pub fn total_rows(&self) -> u32 {
        if self.hidden {
            return 0;
        }
        self.wrap_rows() + self.virtual_rows
    }
}

/// Where a visual row lives.
#[derive(Debug, Clone, Copy)]
pub struct RowAddr {
    pub row: u32,
    pub byte: usize,
    pub line: usize,
    /// Drawn rows between the line's first row and this one — what
    /// `Viewport::top_view_line_offset` means, and every consumer stores it
    /// there.
    ///
    /// Counts the line's virtual rows, so `line_first_row(line) + row_in_line`
    /// reconstructs `row` exactly. Using the wrap-segment index here instead
    /// silently drops `virtual_rows`, and the reconstruction lands that many
    /// rows above where it started — a round trip that loses rows every time
    /// the viewport is re-derived from the pair.
    pub row_in_line: usize,
    pub carry: RowCarry,
    pub is_virtual: bool,
}

/// Point-update / prefix-query over per-line row counts.
#[derive(Debug, Default, Clone)]
pub struct Fenwick {
    n: usize,
    tree: Vec<i64>,
}

impl Fenwick {
    pub fn rebuild(&mut self, values: &[u32]) {
        self.n = values.len();
        self.tree = vec![0; self.n + 1];
        for (i, &v) in values.iter().enumerate() {
            self.add(i, v as i64);
        }
    }

    fn add(&mut self, i: usize, delta: i64) {
        let mut i = i + 1;
        while i <= self.n {
            self.tree[i] += delta;
            i += i & i.wrapping_neg();
        }
    }

    /// Replace entry `i`'s value.
    pub fn set(&mut self, i: usize, old: u32, new: u32) {
        if old != new && i < self.n {
            self.add(i, new as i64 - old as i64);
        }
    }

    /// Sum of entries `[0, i)`.
    pub fn prefix(&self, i: usize) -> u32 {
        let mut i = i.min(self.n);
        let mut total: i64 = 0;
        while i > 0 {
            total += self.tree[i];
            i -= i & i.wrapping_neg();
        }
        total.max(0) as u32
    }

    pub fn total(&self) -> u32 {
        self.prefix(self.n)
    }

    /// Largest index whose prefix sum is `<= row` — the line owning that row.
    pub fn find(&self, row: u32) -> usize {
        let mut idx = 0usize;
        let mut bit = 1usize;
        while bit * 2 <= self.n {
            bit *= 2;
        }
        let mut remaining = row as i64;
        while bit > 0 {
            let next = idx + bit;
            if next <= self.n && self.tree[next] <= remaining {
                idx = next;
                remaining -= self.tree[next];
            }
            bit /= 2;
        }
        idx
    }
}

/// One buffer's row boundaries under one geometry.
#[derive(Debug, Default)]
pub struct WrapIndex {
    geometry: Option<WrapIndexGeometry>,
    lines: Vec<LineWrap>,
    rows: Fenwick,
    built: bool,
    /// Versions of the pipeline inputs the current build reflects, kept
    /// per-component so staleness has a *shape*: a buffer mismatch is repaired
    /// by [`WrapIndex::damage_bytes`], a decoration mismatch by diffing the
    /// stored snapshot against the fresh one (see [`WrapIndex::ensure_built`]).
    inputs: crate::view::line_wrap_cache::PipelineInputs,
    /// The decorations this build reflects, in **current buffer coordinates**:
    /// repairs rebuild lines against it, and `damage_bytes` shifts its
    /// positions the way the live markers shifted, so it never goes stale
    /// across text edits. Content changes arrive by diff — `ensure_built`
    /// compares it against the freshly resolved snapshot and repairs exactly
    /// the lines where they disagree.
    decorations: IndexDecorations,
    stats: WrapIndexStats,
}

/// How much building this index has done, and through which channel.
///
/// A full rebuild is O(buffer) — it lays out every logical line — so
/// `lines_built` divided by the buffer's line count is how many times the
/// whole document has been re-laid-out; one is the design. Decoration changes
/// land in `decoration_repairs` / `lines_repaired` instead, and during a
/// scroll through a plugin-decorated file the repaired count tracks the lines
/// the plugin touched — the difference between a scroll that costs the
/// viewport and one that costs the file.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct WrapIndexStats {
    pub rebuilds: u64,
    pub lines_built: u64,
    pub decoration_repairs: u64,
    pub lines_repaired: u64,
}

impl WrapIndexStats {
    fn merge(self, other: WrapIndexStats) -> WrapIndexStats {
        WrapIndexStats {
            rebuilds: self.rebuilds + other.rebuilds,
            lines_built: self.lines_built + other.lines_built,
            decoration_repairs: self.decoration_repairs + other.decoration_repairs,
            lines_repaired: self.lines_repaired + other.lines_repaired,
        }
    }
}

impl WrapIndex {
    pub fn is_built_for(
        &self,
        geometry: &WrapIndexGeometry,
        inputs: crate::view::line_wrap_cache::PipelineInputs,
    ) -> bool {
        self.built && self.geometry.as_ref() == Some(geometry) && self.inputs == inputs
    }

    pub fn lines(&self) -> &[LineWrap] {
        &self.lines
    }

    /// Build work this index has done. See [`WrapIndexStats`].
    pub fn stats(&self) -> WrapIndexStats {
        self.stats
    }

    /// Make the index current for `geometry`, choosing the cheapest sufficient
    /// path:
    ///
    /// * everything matches — free;
    /// * only the decoration versions moved — diff the stored snapshot against
    ///   `decorations` and rebuild exactly the disagreeing lines. This is what
    ///   keeps a first scroll through a plugin-decorated file O(viewport) per
    ///   frame: each `lines_changed` batch damages only the lines it decorated,
    ///   while the rebuild-per-frame it replaced cost O(buffer) per frame;
    /// * the buffer version or geometry moved — full rebuild, the one
    ///   O(buffer) operation in the design. (Buffer edits normally arrive via
    ///   [`WrapIndex::damage_bytes`] and never reach this case; hitting it
    ///   means the index was cold or the buffer was replaced wholesale.)
    pub fn ensure_built(
        &mut self,
        buffer: &mut Buffer,
        geometry: WrapIndexGeometry,
        inputs: crate::view::line_wrap_cache::PipelineInputs,
        line_ending: LineEnding,
        decorations: &IndexDecorations,
    ) {
        if self.is_built_for(&geometry, inputs) {
            return;
        }
        if self.built
            && self.geometry.as_ref() == Some(&geometry)
            && self.inputs.buffer == inputs.buffer
            && self.repair_decorations(buffer, inputs, line_ending, decorations)
        {
            return;
        }
        self.rebuild_full(buffer, geometry, inputs, line_ending, decorations);
    }

    fn rebuild_full(
        &mut self,
        buffer: &mut Buffer,
        geometry: WrapIndexGeometry,
        inputs: crate::view::line_wrap_cache::PipelineInputs,
        line_ending: LineEnding,
        decorations: &IndexDecorations,
    ) {
        let line_count = buffer.line_count().unwrap_or(1).max(1);
        let mut lines = Vec::with_capacity(line_count);
        for line in 0..line_count {
            let vrows = line_virtual_rows(buffer, line, decorations);
            lines.push(build_line(
                buffer,
                line,
                geometry.rule,
                line_ending,
                vrows,
                decorations,
            ));
        }
        self.stats.rebuilds += 1;
        self.stats.lines_built += line_count as u64;
        let counts: Vec<u32> = lines.iter().map(|l| l.total_rows()).collect();
        self.rows.rebuild(&counts);
        self.lines = lines;
        self.geometry = Some(geometry);
        self.inputs = inputs;
        self.decorations = decorations.clone();
        self.built = true;
    }

    /// Adopt a decoration change by rebuilding only the lines where the stored
    /// snapshot and `decorations` disagree. Returns `false` when a rebuild is
    /// the better tool — the damage covers most of the buffer, or the line
    /// count disagrees (a text edit that bypassed `damage_bytes`).
    ///
    /// Sound because both snapshots are in current buffer coordinates
    /// (`damage_bytes` keeps the stored one shifted), so a line outside every
    /// diff range sees byte-identical decorations under either snapshot and
    /// its layout cannot differ. Decorations never change the *logical* line
    /// structure — that is buffer text — so lines are rebuilt in place and no
    /// splice is needed.
    fn repair_decorations(
        &mut self,
        buffer: &mut Buffer,
        inputs: crate::view::line_wrap_cache::PipelineInputs,
        line_ending: LineEnding,
        decorations: &IndexDecorations,
    ) -> bool {
        let line_count = buffer.line_count().unwrap_or(1).max(1);
        if line_count != self.lines.len() {
            return false;
        }
        let last_line = line_count - 1;
        // Merge the byte ranges into line spans before deciding anything:
        // ranges are many and small (one per changed decoration), lines are
        // what repair pays for.
        let mut spans: Vec<(usize, usize)> = Vec::new();
        for range in self.decorations.changed_ranges(decorations) {
            let first = buffer.get_line_number(range.start.min(buffer.len()));
            let last = buffer
                .get_line_number(range.end.min(buffer.len()))
                .min(last_line);
            match spans.last_mut() {
                Some((_, prev_last)) if first <= *prev_last + 1 => {
                    *prev_last = (*prev_last).max(last)
                }
                _ => spans.push((first, last)),
            }
        }
        let damaged: usize = spans.iter().map(|(f, l)| l - f + 1).sum();
        if damaged > line_count / 2 {
            return false;
        }

        // The new snapshot must be in place before any line is rebuilt —
        // `rebuild_one` reads `self.decorations`.
        self.decorations = decorations.clone();
        let geometry = self.geometry.expect("repair_decorations requires a build");
        for (first, last) in spans {
            for line in first..=last {
                self.rebuild_one(buffer, line, geometry.rule, line_ending);
            }
        }
        self.inputs = inputs;
        self.stats.decoration_repairs += 1;
        self.stats.lines_repaired += damaged as u64;
        true
    }

    // -- queries -------------------------------------------------------------

    pub fn total_rows(&self) -> u32 {
        self.rows.total().max(1)
    }

    pub fn rows_in_line(&self, line: usize) -> u32 {
        self.lines.get(line).map_or(1, |l| l.total_rows())
    }

    /// Absolute row index of `line`'s first row.
    /// One line's canonical wrap structure, for the render path's
    /// cursor-line expansion.
    pub fn line_wrap(&self, line: usize) -> Option<&LineWrap> {
        self.lines.get(line)
    }

    pub fn line_first_row(&self, line: usize) -> u32 {
        self.rows.prefix(line)
    }

    /// Absolute visual row containing `byte`. O(log lines + log rows).
    pub fn row_of_byte(&self, buffer: &Buffer, byte: usize) -> u32 {
        if self.lines.is_empty() {
            return 0;
        }
        let line = buffer.get_line_number(byte).min(self.lines.len() - 1);
        let lw = &self.lines[line];
        let line_start = buffer.line_start_offset(line).unwrap_or(0);
        let rel = byte.saturating_sub(line_start) as u32;
        let row_in_line = lw
            .row_starts
            .partition_point(|&s| s <= rel)
            .saturating_sub(1);
        self.rows.prefix(line) + lw.virtual_rows + row_in_line as u32
    }

    /// Address of absolute visual row `row`. O(log lines).
    pub fn byte_of_row(&self, buffer: &Buffer, row: u32) -> RowAddr {
        let total = self.total_rows();
        let row = row.min(total.saturating_sub(1));
        if self.lines.is_empty() {
            return RowAddr {
                row,
                byte: 0,
                line: 0,
                row_in_line: 0,
                carry: RowCarry::default(),
                is_virtual: false,
            };
        }
        let line = self.rows.find(row).min(self.lines.len() - 1);
        let base = self.rows.prefix(line);
        let lw = &self.lines[line];
        let line_start = buffer.line_start_offset(line).unwrap_or(0);
        let row_in_line = row.saturating_sub(base);
        if row_in_line < lw.virtual_rows {
            return RowAddr {
                row,
                byte: line_start,
                line,
                row_in_line: row_in_line as usize,
                carry: RowCarry::default(),
                is_virtual: true,
            };
        }
        let idx = ((row_in_line - lw.virtual_rows) as usize).min(lw.row_starts.len() - 1);
        RowAddr {
            row,
            byte: line_start + lw.row_starts[idx] as usize,
            line,
            // Drawn-row offset, not `idx`: the virtual rows above are drawn too.
            row_in_line: idx + lw.virtual_rows as usize,
            carry: lw.carries[idx],
            is_virtual: false,
        }
    }

    /// Nearest row at or before `row` that a render can start at.
    ///
    /// Returns `(start_row, skip)`: build from `start_row` and discard `skip`
    /// rows. A row opening with injected content the carry cannot reconstruct is
    /// not a valid entry point, so this walks back to one that is, falling back
    /// to the line's own first row (always valid — a logical line start needs no
    /// carry). `skip` is bounded by one logical line's rows, and is zero for the
    /// case this design exists for: a long line with no decorations, where every
    /// row is resumable.
    pub fn resumable_row_at_or_before(&self, buffer: &Buffer, row: u32) -> (u32, u32) {
        let addr = self.byte_of_row(buffer, row);
        if addr.is_virtual || self.lines.is_empty() {
            return (row, 0);
        }
        let lw = &self.lines[addr.line];
        // `row_in_line` is a drawn offset — virtual rows above the line count —
        // but `resumable` is indexed by wrap row. A non-virtual address is
        // always at or past the virtual block, so the subtraction is safe; the
        // walk-back distance is the same in both spaces.
        let mut idx = addr.row_in_line.saturating_sub(lw.virtual_rows as usize);
        while idx > 0 && !lw.resumable[idx] {
            idx -= 1;
        }
        let skip = (addr.row_in_line.saturating_sub(lw.virtual_rows as usize) - idx) as u32;
        (row - skip, skip)
    }

    // -- damage contract -----------------------------------------------------

    /// Throw the build away; the next query rebuilds from scratch.
    ///
    /// The big hammer, for changes with no expressible locality (buffer
    /// replaced wholesale). Decoration changes no longer come through here —
    /// `ensure_built` repairs them by diffing the stored snapshot against the
    /// fresh one.
    pub fn damage_all(&mut self) {
        self.built = false;
    }

    /// Repair after a buffer edit.
    ///
    /// The `*_before` arguments describe the edit in pre-edit line coordinates:
    /// the line its start fell in, that line's start byte, and the line its
    /// removed span ended in. The caller has the pre-edit buffer, so it supplies
    /// them for free; reconstructing them here would mean keeping a shadow copy
    /// of the old line index.
    pub fn damage_bytes(
        &mut self,
        buffer: &mut Buffer,
        edit: EditDamage,
        line_ending: LineEnding,
        new_inputs: crate::view::line_wrap_cache::PipelineInputs,
    ) {
        if !self.built {
            return;
        }
        let Some(geometry) = self.geometry else {
            return;
        };
        // The snapshot mirrors the marker-backed decorations, which the edit
        // just shifted — shift the mirror the same way *before* any line is
        // rebuilt against it. (Repairs used to read the unshifted snapshot and
        // relied on the next decoration bump's full rebuild to heal the drift;
        // with decoration bumps now repaired by diffing against this snapshot,
        // it has to be kept honest instead.)
        self.decorations
            .shift_for_edit(edit.start, edit.removed, edit.inserted);
        // Adopt the post-edit *buffer* version, or the repair is wasted: every
        // edit changes it, and the next `ensure_built` would find the index
        // stale and rebuild the whole buffer from scratch — after this method
        // had just done the incremental work. Measured at ~26% of a keystroke
        // on a single-line file: the repair (10.5%) and then the full rebuild
        // (15.6%) that discarded it.
        //
        // Only the buffer component: this call describes a text edit, and the
        // decoration components answer a different question. If a decoration
        // batch landed since the last frame, its versions must still read as
        // stale afterwards so `ensure_built` diffs and repairs it — adopting
        // them here would swallow that change until the plugin's next bump.
        self.inputs.buffer = new_inputs.buffer;
        let new_last = buffer.get_line_number(edit.start + edit.inserted);
        let spans_lines = edit.line_end_before != edit.line_before || new_last != edit.line_before;
        let line_count = buffer.line_count().unwrap_or(1).max(1);

        // Not only about the line *count* changing: replacing "\na" with "\n"
        // spans two lines while leaving the count identical, and repairing only
        // the first would keep a stale layout for the second.
        if spans_lines || line_count != self.lines.len() {
            self.repair_span(
                buffer,
                geometry.rule,
                edit.line_before,
                edit.line_end_before,
                new_last,
                line_ending,
            );
            return;
        }

        self.repair_line(buffer, geometry.rule, edit, line_ending);
    }

    /// Rebuild old lines `[first, old_last]` as new lines `[first, new_last]`.
    ///
    /// Lines before `first` are untouched. Lines after keep their `LineWrap`
    /// unchanged — `row_starts` are line-relative, so a line that merely shifted
    /// in the buffer needs no work at all.
    fn repair_span(
        &mut self,
        buffer: &mut Buffer,
        rule: WrapRule,
        first: usize,
        old_last: usize,
        new_last: usize,
        line_ending: LineEnding,
    ) {
        let first = first.min(self.lines.len());
        let mut rebuilt = Vec::new();
        for line in first..=new_last {
            let vrows = line_virtual_rows(buffer, line, &self.decorations);
            rebuilt.push(build_line(
                buffer,
                line,
                rule,
                line_ending,
                vrows,
                &self.decorations,
            ));
        }
        let end = (old_last + 1).min(self.lines.len());
        self.lines.splice(first..end, rebuilt);
        let counts: Vec<u32> = self.lines.iter().map(|l| l.total_rows()).collect();
        self.rows.rebuild(&counts);
    }

    /// Rewrap forward from the damaged row until the layout resynchronises.
    ///
    /// Correctness rests on [`RowCarry`] being the complete resume state: once a
    /// newly computed boundary lands on an old boundary shifted by the edit's
    /// delta *and* the carry matches, every later boundary must match too, so
    /// the tail can be spliced instead of recomputed.
    fn repair_line(
        &mut self,
        buffer: &mut Buffer,
        rule: WrapRule,
        edit: EditDamage,
        line_ending: LineEnding,
    ) {
        let line = edit.line_before;
        if line >= self.lines.len() {
            return;
        }
        let old_total = self.lines[line].total_rows();
        let rel_start = edit.start.saturating_sub(edit.line_start_before) as u32;
        let rel_end_old = edit.end_old().saturating_sub(edit.line_start_before) as u32;
        let delta = edit.delta();

        // Resume one row *before* the row containing the edit: the break that
        // ends a row is decided by the token that overflows it, which lives on
        // the next row. Resuming at the damaged row itself would miss text
        // reflowing backwards when the edit shrinks it, and a row disappearing
        // entirely when its content is deleted.
        let damaged = self.lines[line]
            .row_starts
            .partition_point(|&s| s <= rel_start)
            .saturating_sub(1);
        let mut resume_idx = damaged.saturating_sub(1);
        while resume_idx > 0 && !self.lines[line].resumable[resume_idx] {
            resume_idx -= 1;
        }
        if !self.lines[line].resumable[resume_idx] {
            self.rebuild_one(buffer, line, rule, line_ending);
            return;
        }

        let vrows = line_virtual_rows(buffer, line, &self.decorations);
        let line_start = buffer.line_start_offset(line).unwrap_or(0);
        let resume_rel = self.lines[line].row_starts[resume_idx];
        let resume_carry = self.lines[line].carries[resume_idx];
        let resume_byte = line_start + resume_rel as usize;
        // Decoration-aware, and read from the resume row rather than the line's
        // start: repair is meant to cost rows, not the line.
        let stream = line_token_stream(
            buffer,
            line,
            line_ending,
            &self.decorations,
            Some(resume_byte),
        );

        // The *new* stream may open this row with injected content the resume
        // cannot reconstruct (an edit can move a hint or a soft break onto a row
        // that previously started with plain source), or the resume byte may no
        // longer address anything while the line still has content.
        let tail = tokens_from(&stream, resume_byte);
        if !resume_is_safe(&stream, resume_byte) || (tail.is_empty() && !stream.is_empty()) {
            self.rebuild_one(buffer, line, rule, line_ending);
            return;
        }

        // Resync also requires the tail to be pure shifted source: past the last
        // decoration in the line. Beyond such a point the overlay reattaches to
        // different tokens, so "same byte, same carry" would not imply "same
        // continuation". With decorations applied by later passes rather than
        // here, that floor is just the edit's end.
        let resync_floor = rel_end_old;

        let mut machine = WrapMachine::resume(rule, resume_carry);
        let mut new_starts: Vec<u32> = Vec::new();
        let mut new_carries: Vec<RowCarry> = Vec::new();
        let mut new_resumable: Vec<bool> = Vec::new();
        let mut sealed = 0usize;
        let mut prev_rel = resume_rel;
        let mut resync_at: Option<usize> = None;

        for token in tail {
            machine.feed(token);
            if let Some(hit) = absorb_rows(
                &machine,
                line_start,
                resume_rel,
                &mut sealed,
                &mut prev_rel,
                &mut new_starts,
                &mut new_carries,
                &mut new_resumable,
                &self.lines[line],
                resume_idx,
                resync_floor,
                delta,
            ) {
                resync_at = Some(hit);
                break;
            }
        }
        if resync_at.is_none() {
            let finished = machine.finish();
            absorb_finished(
                &finished,
                line_start,
                resume_rel,
                &mut sealed,
                &mut prev_rel,
                &mut new_starts,
                &mut new_carries,
                &mut new_resumable,
            );
        }

        let old = &self.lines[line];
        let (mut starts, mut carries, mut resumable) = if let Some(k) = resync_at {
            // Drop the provisional row that matched — the spliced tail supplies it.
            let keep = new_starts.len().saturating_sub(1);
            let mut s: Vec<u32> = old.row_starts[..resume_idx].to_vec();
            let mut c: Vec<RowCarry> = old.carries[..resume_idx].to_vec();
            let mut r: Vec<bool> = old.resumable[..resume_idx].to_vec();
            s.extend_from_slice(&new_starts[..keep]);
            c.extend_from_slice(&new_carries[..keep]);
            r.extend_from_slice(&new_resumable[..keep]);
            s.extend(
                old.row_starts[k..]
                    .iter()
                    .map(|&v| (v as i64 + delta).max(0) as u32),
            );
            c.extend_from_slice(&old.carries[k..]);
            r.extend_from_slice(&old.resumable[k..]);
            (s, c, r)
        } else {
            let mut s: Vec<u32> = old.row_starts[..resume_idx].to_vec();
            let mut c: Vec<RowCarry> = old.carries[..resume_idx].to_vec();
            let mut r: Vec<bool> = old.resumable[..resume_idx].to_vec();
            s.extend_from_slice(&new_starts);
            c.extend_from_slice(&new_carries);
            r.extend_from_slice(&new_resumable);
            (s, c, r)
        };

        if starts.is_empty() {
            // The resumed region held every row and the edit emptied it. A
            // logical line always occupies at least one row.
            starts = vec![0];
            carries = vec![RowCarry::default()];
            resumable = vec![true];
        }

        let lw = &mut self.lines[line];
        lw.row_starts = starts;
        lw.carries = carries;
        lw.resumable = resumable;
        lw.virtual_rows = vrows;
        let new_total = lw.total_rows();
        self.rows.set(line, old_total, new_total);
    }

    fn rebuild_one(
        &mut self,
        buffer: &mut Buffer,
        line: usize,
        rule: WrapRule,
        line_ending: LineEnding,
    ) {
        let old_total = self.lines[line].total_rows();
        let vrows = line_virtual_rows(buffer, line, &self.decorations);
        self.lines[line] = build_line(buffer, line, rule, line_ending, vrows, &self.decorations);
        let new_total = self.lines[line].total_rows();
        self.rows.set(line, old_total, new_total);
    }
}

/// A buffer mutation, as the damage contract sees it.
#[derive(Debug, Clone, Copy)]
pub struct EditDamage {
    pub start: usize,
    pub removed: usize,
    pub inserted: usize,
    /// Logical line `start` fell in, before the edit.
    pub line_before: usize,
    /// Start byte of `line_before`, before the edit.
    pub line_start_before: usize,
    /// Logical line the removed span ended in, before the edit.
    pub line_end_before: usize,
}

impl EditDamage {
    pub fn delta(&self) -> i64 {
        self.inserted as i64 - self.removed as i64
    }

    pub fn end_old(&self) -> usize {
        self.start + self.removed
    }
}

/// Virtual rows anchored in `line`, resolved from the decoration snapshot.
fn line_virtual_rows(buffer: &mut Buffer, line: usize, decorations: &IndexDecorations) -> u32 {
    if decorations.virtual_lines.is_empty() {
        return 0;
    }
    let start = buffer.line_start_offset(line).unwrap_or(0);
    let end = buffer
        .line_start_offset(line + 1)
        .unwrap_or_else(|| buffer.len())
        .min(buffer.len());
    decorations.virtual_rows_in(start, end)
}

/// Canonical token stream for one logical line: the renderer's decoration chain.
///
/// Runs exactly what `build_view_data` runs — soft breaks, then conceals, then
/// inline hints — because rows the index reports have to be the rows that get
/// drawn. Wrapping the raw line instead was the gap `wrap_index_models_layout`
/// papered over: markdown_compose wraps each paragraph to its own narrower
/// width, so an index blind to that under-counts and scrolling clamps before
/// the end of the buffer.
fn line_token_stream(
    buffer: &mut Buffer,
    line: usize,
    line_ending: LineEnding,
    decorations: &IndexDecorations,
    from_byte: Option<usize>,
) -> Vec<ViewTokenWire> {
    use crate::view::ui::split_rendering::transforms::{
        apply_conceal_ranges, apply_soft_breaks, splice_inline_virtual_text,
    };

    let mut tokens =
        build_line_tokens_from(buffer, line, line_ending, &decorations.folds, from_byte);
    if decorations.is_empty() {
        return tokens;
    }
    let line_start = buffer.line_start_offset(line).unwrap_or(0);
    let line_end = buffer
        .line_start_offset(line + 1)
        .unwrap_or_else(|| buffer.len())
        .min(buffer.len());
    let (breaks, conceals, hints) = decorations.for_line(line_start, line_end);
    if !breaks.is_empty() {
        tokens = apply_soft_breaks(tokens, &breaks);
    }
    if !conceals.is_empty() {
        tokens = apply_conceal_ranges(tokens, &conceals);
    }
    if !hints.is_empty() {
        tokens = splice_inline_virtual_text(tokens, &hints);
    }
    tokens
}

/// Wrap one logical line from scratch.
///
/// Built from the *real* tokenizer rather than from raw line text: the
/// count-only mirrors this replaces wrapped a single synthetic `Text` token, so
/// the `Space`-overflow back-up (issue #1363) — which only fires on `Space`
/// tokens — never ran in them, and their counts could differ from what the
/// renderer drew. Sharing the tokenizer makes that class of drift impossible.
/// Drawn row starts of one line under `decorations`, line-relative.
///
/// The render path's cursor-line expansion: the index is canonical, but the
/// frame draws the cursor's line cursor-aware, and placement must target the
/// row the cursor is *drawn* on. Same pipeline as [`build_line`], for exactly
/// one line, with cursor-aware decorations resolved by the caller.
pub(crate) fn line_drawn_row_starts(
    buffer: &mut Buffer,
    line: usize,
    rule: WrapRule,
    line_ending: LineEnding,
    decorations: &IndexDecorations,
) -> Vec<u32> {
    let line_start = buffer.line_start_offset(line).unwrap_or(0);
    let tokens = line_token_stream(buffer, line, line_ending, decorations, None);
    let out = WrapMachine::run(tokens, rule);
    let (row_starts, _, _) = rows_to_starts(&out, line_start, 0);
    row_starts
}

fn build_line(
    buffer: &mut Buffer,
    line: usize,
    rule: WrapRule,
    line_ending: LineEnding,
    virtual_rows: u32,
    decorations: &IndexDecorations,
) -> LineWrap {
    let line_start = buffer.line_start_offset(line).unwrap_or(0);
    let tokens = line_token_stream(buffer, line, line_ending, decorations, None);
    let out = WrapMachine::run(tokens, rule);
    let (row_starts, carries, resumable) = rows_to_starts(&out, line_start, 0);
    let line_end = buffer
        .line_start_offset(line + 1)
        .unwrap_or_else(|| buffer.len())
        .min(buffer.len());
    LineWrap {
        row_starts,
        carries,
        resumable,
        virtual_rows,
        hidden: decorations.line_is_hidden(line_start, line_end),
    }
}

/// Row infos → line-relative starts, carries, and resumability.
///
/// A row can be all-injected and carry no source byte of its own; it still
/// occupies a row, and its start is taken to be the previous row's, which keeps
/// `byte_of_row` monotonic.
fn rows_to_starts(
    out: &WrapOutput,
    line_start: usize,
    first_rel: u32,
) -> (Vec<u32>, Vec<RowCarry>, Vec<bool>) {
    let mut starts = Vec::with_capacity(out.rows.len());
    let mut carries = Vec::with_capacity(out.rows.len());
    let mut resumable = Vec::with_capacity(out.rows.len());
    let mut prev = first_rel;
    for (i, row) in out.rows.iter().enumerate() {
        let rel = if i == 0 {
            first_rel
        } else {
            match row.source_byte {
                Some(b) => b.saturating_sub(line_start) as u32,
                None => prev,
            }
        };
        starts.push(rel);
        carries.push(row.carry);
        resumable.push(row_is_resumable(row, &out.tokens));
        prev = rel;
    }
    if starts.is_empty() {
        starts.push(first_rel);
        carries.push(RowCarry::default());
        resumable.push(true);
    }
    (starts, carries, resumable)
}

/// Can this row be rebuilt from its source byte and carry alone?
///
/// Yes when its first token — after any hanging indent, which the carry
/// reconstructs — is the source token the row starts at.
fn row_is_resumable(row: &RowInfo, tokens: &[ViewTokenWire]) -> bool {
    let Some(source_byte) = row.source_byte else {
        return false;
    };
    let mut idx = row.token_start;
    if let Some(t) = tokens.get(idx) {
        if t.source_offset.is_none() {
            if let ViewTokenWireKind::Text(s) = &t.kind {
                if s.len() == row.carry.line_indent && s.bytes().all(|b| b == b' ') {
                    idx += 1;
                }
            }
        }
    }
    tokens.get(idx).and_then(|t| t.source_offset) == Some(source_byte)
}

/// Is `byte` a valid resume point in this token stream?
///
/// Only when the first source token at or after `byte` is not preceded by
/// injected tokens. Decorations move with the text on an edit, so a hint or a
/// soft break can land on a row that previously started with plain source; the
/// row's stored flag describes the *old* stream, and this checks the new one.
fn resume_is_safe(tokens: &[ViewTokenWire], byte: usize) -> bool {
    let mut prev_injected = false;
    for token in tokens {
        let Some(offset) = token.source_offset else {
            prev_injected = true;
            continue;
        };
        if offset >= byte {
            return !prev_injected;
        }
        let end = offset
            + match &token.kind {
                ViewTokenWireKind::Text(s) => s.len(),
                _ => 1,
            };
        if end > byte {
            return !prev_injected;
        }
        prev_injected = false;
    }
    true
}

/// Sub-stream beginning at absolute `byte`, splitting a `Text` token if needed.
///
/// Strictly source-addressed: injected tokens before the first source token at
/// or after `byte` are not included. Rows that open with injected content are
/// marked non-resumable and never reach this function.
fn tokens_from(tokens: &[ViewTokenWire], byte: usize) -> Vec<ViewTokenWire> {
    let mut out = Vec::new();
    let mut started = false;
    for token in tokens {
        if started {
            out.push(token.clone());
            continue;
        }
        let Some(offset) = token.source_offset else {
            continue;
        };
        if offset >= byte {
            started = true;
            out.push(token.clone());
            continue;
        }
        if let ViewTokenWireKind::Text(s) = &token.kind {
            if offset + s.len() > byte {
                started = true;
                out.push(ViewTokenWire {
                    source_offset: Some(byte),
                    kind: ViewTokenWireKind::Text(s[byte - offset..].to_string()),
                    style: token.style.clone(),
                });
            }
        }
    }
    out
}

/// Seal rows the machine has completed; return the old row index resynced to.
#[allow(clippy::too_many_arguments)]
fn absorb_rows(
    machine: &WrapMachine,
    line_start: usize,
    resume_rel: u32,
    sealed: &mut usize,
    prev_rel: &mut u32,
    new_starts: &mut Vec<u32>,
    new_carries: &mut Vec<RowCarry>,
    new_resumable: &mut Vec<bool>,
    old: &LineWrap,
    resume_idx: usize,
    resync_floor: u32,
    delta: i64,
) -> Option<usize> {
    while machine.rows_so_far().len() > *sealed {
        let row = machine.rows_so_far()[*sealed];
        *sealed += 1;
        let rel = if *sealed == 1 {
            resume_rel
        } else {
            match row.source_byte {
                Some(b) => b.saturating_sub(line_start) as u32,
                None => *prev_rel,
            }
        };
        new_starts.push(rel);
        new_carries.push(row.carry);
        new_resumable.push(if *sealed == 1 {
            true
        } else {
            row_is_resumable(&row, machine.tokens_so_far())
        });
        *prev_rel = rel;
        if new_starts.len() > 1 {
            for k in (resume_idx + 1)..old.row_starts.len() {
                if old.row_starts[k] < resync_floor {
                    continue;
                }
                if (old.row_starts[k] as i64 + delta) == rel as i64 && old.carries[k] == row.carry {
                    return Some(k);
                }
            }
        }
    }
    None
}

#[allow(clippy::too_many_arguments)]
fn absorb_finished(
    out: &WrapOutput,
    line_start: usize,
    resume_rel: u32,
    sealed: &mut usize,
    prev_rel: &mut u32,
    new_starts: &mut Vec<u32>,
    new_carries: &mut Vec<RowCarry>,
    new_resumable: &mut Vec<bool>,
) {
    while out.rows.len() > *sealed {
        let row = out.rows[*sealed];
        *sealed += 1;
        let rel = if *sealed == 1 {
            resume_rel
        } else {
            match row.source_byte {
                Some(b) => b.saturating_sub(line_start) as u32,
                None => *prev_rel,
            }
        };
        new_starts.push(rel);
        new_carries.push(row.carry);
        new_resumable.push(if *sealed == 1 {
            true
        } else {
            row_is_resumable(&row, &out.tokens)
        });
        *prev_rel = rel;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::filesystem::StdFileSystem;
    use std::sync::Arc;

    fn test_fs() -> Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> {
        Arc::new(StdFileSystem)
    }

    fn geometry(width: usize) -> WrapIndexGeometry {
        WrapIndexGeometry {
            fold_signature: 0,
            rule: WrapRule::Word {
                content_width: width,
                gutter_width: 0,
                hanging_indent: false,
            },
            view_mode: CacheViewMode::Source,
        }
    }

    fn inputs(buffer_version: u64) -> crate::view::line_wrap_cache::PipelineInputs {
        crate::view::line_wrap_cache::PipelineInputs {
            buffer: buffer_version,
            ..Default::default()
        }
    }

    fn built(buffer: &mut Buffer, width: usize) -> WrapIndex {
        let mut index = WrapIndex::default();
        index.ensure_built(
            buffer,
            geometry(width),
            inputs(0),
            LineEnding::LF,
            &IndexDecorations::default(),
        );
        index
    }

    fn structure(index: &WrapIndex) -> Vec<Vec<u32>> {
        index.lines().iter().map(|l| l.row_starts.clone()).collect()
    }

    fn long_line(words: usize) -> String {
        (0..words)
            .map(|i| format!("w{i}"))
            .collect::<Vec<_>>()
            .join(" ")
    }

    /// Apply an edit to the buffer and repair the index, the way the render
    /// path will.
    fn edit(buffer: &mut Buffer, index: &mut WrapIndex, start: usize, removed: usize, text: &str) {
        let line_before = buffer.get_line_number(start);
        let line_start_before = buffer.line_start_offset(line_before).unwrap_or(0);
        let line_end_before = buffer.get_line_number(start + removed);
        if removed > 0 {
            buffer.delete(start..start + removed);
        }
        if !text.is_empty() {
            buffer.insert(start, text);
        }
        index.damage_bytes(
            buffer,
            EditDamage {
                start,
                removed,
                inserted: text.len(),
                line_before,
                line_start_before,
                line_end_before,
            },
            LineEnding::LF,
            inputs(0),
        );
    }

    /// A repair must leave the index *usable*, not merely correct.
    ///
    /// `pipeline_inputs_version` folds in `buffer.version()`, so every edit
    /// changes it. For one release `damage_bytes` updated the rows and left the
    /// version stale, so the next `ensure_built` found the index out of date and
    /// rebuilt the whole buffer — the repair ran and was thrown away, ~26% of a
    /// keystroke doing the same work twice.
    ///
    /// Nothing caught it: `repair_equals_rebuild` compares a repaired index
    /// against a rebuilt one, which is exactly what kept happening. Equality was
    /// never the property at risk. This asserts the one that was.
    #[test]
    fn a_repair_leaves_the_index_current() {
        let text = long_line(200);
        let mut buffer = Buffer::from_bytes(text.as_bytes().to_vec(), test_fs());
        let mut index = built(&mut buffer, 20);
        let geom = geometry(20);

        let start = buffer.len() / 2;
        let line_before = buffer.get_line_number(start);
        let line_start_before = buffer.line_start_offset(line_before).unwrap_or(0);
        buffer.insert(start, "x");
        let after = inputs(buffer.version());
        assert!(
            !index.is_built_for(&geom, after),
            "precondition: the edit must have staled the version"
        );

        index.damage_bytes(
            &mut buffer,
            EditDamage {
                start,
                removed: 0,
                inserted: 1,
                line_before,
                line_start_before,
                line_end_before: line_before,
            },
            LineEnding::LF,
            after,
        );

        assert!(
            index.is_built_for(&geom, after),
            "the repair left the index stale, so the next render rebuilds and \
             the repair was wasted"
        );
    }

    /// A build with `decorations` from scratch, for comparing repairs against.
    fn built_with(buffer: &mut Buffer, width: usize, decorations: &IndexDecorations) -> WrapIndex {
        let mut index = WrapIndex::default();
        index.ensure_built(
            buffer,
            geometry(width),
            inputs(0),
            LineEnding::LF,
            decorations,
        );
        index
    }

    /// Prose that wraps at the test width, so decorations move real row
    /// boundaries.
    fn paragraphs(lines: usize) -> String {
        (0..lines)
            .map(|i| long_line(12 + (i % 5)))
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// The core of the diff-repair contract: a decoration change re-lays-out
    /// only the lines it touched, and the result is indistinguishable from a
    /// from-scratch build with the new snapshot. This is what turned a first
    /// scroll through a plugin-decorated file from O(frames x buffer) into
    /// O(frames x batch).
    #[test]
    fn decoration_change_repairs_only_touched_lines_and_equals_rebuild() {
        let text = paragraphs(60);
        let mut buffer = Buffer::from_bytes(text.as_bytes().to_vec(), test_fs());
        let mut index = built(&mut buffer, 20);
        assert_eq!(index.stats().rebuilds, 1);

        // A plugin batch decorates a handful of lines mid-document: a soft
        // break, a conceal, and a virtual line, all in lines 30..33.
        let l30 = buffer.line_start_offset(30).unwrap();
        let l31 = buffer.line_start_offset(31).unwrap();
        let l32 = buffer.line_start_offset(32).unwrap();
        let decorations = IndexDecorations {
            soft_breaks: vec![(l30 + 10, 2)],
            conceals: vec![(l31 + 2..l31 + 8, Some("*".to_string()))],
            virtual_lines: vec![l32],
            ..Default::default()
        };
        let bumped = crate::view::line_wrap_cache::PipelineInputs {
            conceals: 1,
            soft_breaks: 1,
            virtual_text: 1,
            ..inputs(0)
        };
        index.ensure_built(
            &mut buffer,
            geometry(20),
            bumped,
            LineEnding::LF,
            &decorations,
        );

        let stats = index.stats();
        assert_eq!(stats.rebuilds, 1, "the change must not trigger a rebuild");
        assert_eq!(stats.decoration_repairs, 1);
        assert!(
            stats.lines_repaired <= 6,
            "three decorated lines should repair a handful of lines, not {}",
            stats.lines_repaired
        );
        assert!(
            index.is_built_for(&geometry(20), bumped),
            "the repair must leave the index current"
        );

        let fresh = built_with(&mut buffer, 20, &decorations);
        assert_eq!(structure(&index), structure(&fresh));
        assert_eq!(index.total_rows(), fresh.total_rows());
    }

    /// Removing decorations is damage too: the diff sees entries present only
    /// in the *old* snapshot and repairs their lines back to plain layout.
    #[test]
    fn decoration_removal_repairs_back_to_plain_layout() {
        let text = paragraphs(40);
        let mut buffer = Buffer::from_bytes(text.as_bytes().to_vec(), test_fs());
        let l20 = buffer.line_start_offset(20).unwrap();
        let decorated = IndexDecorations {
            soft_breaks: vec![(l20 + 8, 0)],
            ..Default::default()
        };
        let mut index = built_with(&mut buffer, 20, &decorated);
        let plain_rows = built(&mut buffer, 20).total_rows();
        assert_ne!(index.total_rows(), plain_rows, "the soft break added a row");

        let bumped = crate::view::line_wrap_cache::PipelineInputs {
            soft_breaks: 1,
            ..inputs(0)
        };
        index.ensure_built(
            &mut buffer,
            geometry(20),
            bumped,
            LineEnding::LF,
            &IndexDecorations::default(),
        );
        assert_eq!(index.stats().rebuilds, 1);
        assert_eq!(index.stats().decoration_repairs, 1);
        assert_eq!(index.total_rows(), plain_rows);
    }

    /// A version bump with no actual decoration change — plugins republish
    /// identical state all the time — adopts the new version for free instead
    /// of rebuilding anything.
    #[test]
    fn no_op_decoration_bump_repairs_zero_lines() {
        let text = paragraphs(30);
        let mut buffer = Buffer::from_bytes(text.as_bytes().to_vec(), test_fs());
        let mut index = built(&mut buffer, 20);

        let bumped = crate::view::line_wrap_cache::PipelineInputs {
            conceals: 7,
            ..inputs(0)
        };
        index.ensure_built(
            &mut buffer,
            geometry(20),
            bumped,
            LineEnding::LF,
            &IndexDecorations::default(),
        );
        let stats = index.stats();
        assert_eq!(
            (stats.rebuilds, stats.lines_repaired),
            (1, 0),
            "identical snapshots must cost nothing"
        );
        assert!(index.is_built_for(&geometry(20), bumped));
    }

    /// The snapshot shifts with text edits, so a decoration diff computed
    /// *after* an edit compares like coordinates with like: an edit above the
    /// decorated region followed by an unrelated decoration change must not
    /// see the shifted entries as damage and re-lay-out their lines.
    #[test]
    fn snapshot_shifts_with_edits_so_later_diffs_stay_local() {
        let text = paragraphs(50);
        let mut buffer = Buffer::from_bytes(text.as_bytes().to_vec(), test_fs());
        let l30 = buffer.line_start_offset(30).unwrap();
        let l40 = buffer.line_start_offset(40).unwrap();
        let mut decorations = IndexDecorations {
            soft_breaks: vec![(l30 + 5, 0)],
            conceals: vec![(l30 + 12..l30 + 15, None)],
            ..Default::default()
        };
        let mut index = built_with(&mut buffer, 20, &decorations);

        // Text edit on line 0, far above the decorations.
        let ins = "prefix ";
        buffer.insert(0, ins);
        let after_edit = inputs(buffer.version());
        index.damage_bytes(
            &mut buffer,
            EditDamage {
                start: 0,
                removed: 0,
                inserted: ins.len(),
                line_before: 0,
                line_start_before: 0,
                line_end_before: 0,
            },
            LineEnding::LF,
            after_edit,
        );

        // The plugin now decorates line 40; the line-30 entries have shifted
        // in the live managers, and the caller's fresh snapshot reflects that.
        decorations.shift_for_edit(0, 0, ins.len());
        decorations.soft_breaks.push((l40 + ins.len() + 4, 0));
        let bumped = crate::view::line_wrap_cache::PipelineInputs {
            soft_breaks: 1,
            ..after_edit
        };
        index.ensure_built(
            &mut buffer,
            geometry(20),
            bumped,
            LineEnding::LF,
            &decorations,
        );

        let stats = index.stats();
        assert_eq!(stats.rebuilds, 1, "no full rebuild anywhere in this dance");
        assert!(
            stats.lines_repaired <= 2,
            "only the newly decorated line should repair; repaired {}",
            stats.lines_repaired
        );
        let fresh = built_with(&mut buffer, 20, &decorations);
        assert_eq!(structure(&index), structure(&fresh));
    }

    /// Damage spanning most of the document falls back to one full rebuild —
    /// cheaper than line-at-a-time repair and the honest description of what
    /// happened.
    #[test]
    fn mass_decoration_change_falls_back_to_rebuild() {
        let text = paragraphs(40);
        let mut buffer = Buffer::from_bytes(text.as_bytes().to_vec(), test_fs());
        let mut index = built(&mut buffer, 20);

        let breaks: Vec<(usize, u16)> = (0..40)
            .map(|l| (buffer.line_start_offset(l).unwrap() + 3, 0))
            .collect();
        let decorations = IndexDecorations {
            soft_breaks: breaks,
            ..Default::default()
        };
        let bumped = crate::view::line_wrap_cache::PipelineInputs {
            soft_breaks: 1,
            ..inputs(0)
        };
        index.ensure_built(
            &mut buffer,
            geometry(20),
            bumped,
            LineEnding::LF,
            &decorations,
        );

        let stats = index.stats();
        assert_eq!(stats.rebuilds, 2, "every line damaged -> rebuild, once");
        assert_eq!(stats.decoration_repairs, 0);
        let fresh = built_with(&mut buffer, 20, &decorations);
        assert_eq!(structure(&index), structure(&fresh));
    }

    /// The merge gate for incremental repair: a repaired index is
    /// indistinguishable from one built from scratch.
    #[test]
    fn repair_equals_rebuild() {
        let cases: &[(&str, &[(usize, usize, &str)])] = &[
            (
                "alpha beta gamma delta epsilon",
                &[(0, 0, "x"), (10, 0, "yy"), (5, 3, "")],
            ),
            (
                "one\ntwo three four five six\nseven",
                &[(4, 0, "z"), (0, 4, ""), (8, 0, "\n")],
            ),
            (
                "aaaa bbbb cccc dddd eeee ffff",
                &[(28, 0, "!"), (14, 5, ""), (0, 0, "  ")],
            ),
        ];
        for (text, edits) in cases {
            for width in [8usize, 14, 22] {
                let mut buffer = Buffer::from_bytes(text.as_bytes().to_vec(), test_fs());
                let mut index = built(&mut buffer, width);
                for &(start, removed, insert) in *edits {
                    if start + removed > buffer.len() {
                        continue;
                    }
                    edit(&mut buffer, &mut index, start, removed, insert);
                    let fresh = built(&mut buffer, width);
                    assert_eq!(
                        structure(&index),
                        structure(&fresh),
                        "repair diverged for {text:?} at width {width} after edit \
                         ({start}, {removed}, {insert:?})"
                    );
                }
            }
        }
    }

    /// Appending to one enormous line must not rewrap the line: the point of
    /// repairing rather than invalidating.
    #[test]
    fn appending_to_a_huge_line_repairs_locally() {
        let text = long_line(400);
        let mut buffer = Buffer::from_bytes(text.as_bytes().to_vec(), test_fs());
        let mut index = built(&mut buffer, 20);
        let total_before = index.total_rows();
        assert!(
            total_before > 50,
            "expected a many-row line, got {total_before}"
        );

        let end = buffer.len();
        edit(&mut buffer, &mut index, end, 0, "X");

        let fresh = built(&mut buffer, 20);
        assert_eq!(structure(&index), structure(&fresh));
        assert_eq!(index.total_rows(), fresh.total_rows());
    }

    /// Editing at the *start* of a huge line resyncs: the boundaries after the
    /// edit are the old ones shifted, so the tail is spliced, not recomputed.
    #[test]
    fn editing_the_start_of_a_huge_line_resyncs() {
        let text = long_line(400);
        let mut buffer = Buffer::from_bytes(text.as_bytes().to_vec(), test_fs());
        let mut index = built(&mut buffer, 20);
        edit(&mut buffer, &mut index, 0, 0, "Z");
        let fresh = built(&mut buffer, 20);
        assert_eq!(structure(&index), structure(&fresh));
    }

    #[test]
    fn row_byte_roundtrip() {
        let text = "alpha beta gamma\n    indented continuation here\nshort";
        let mut buffer = Buffer::from_bytes(text.as_bytes().to_vec(), test_fs());
        let index = built(&mut buffer, 12);
        for row in 0..index.total_rows() {
            let addr = index.byte_of_row(&buffer, row);
            if addr.is_virtual {
                continue;
            }
            let back = index.row_of_byte(&buffer, addr.byte);
            assert_eq!(
                index.byte_of_row(&buffer, back).byte,
                addr.byte,
                "row {row} did not round-trip"
            );
        }
    }

    #[test]
    fn row_starts_begin_at_zero_and_ascend() {
        let text = "alpha beta gamma delta\nsecond line that also wraps a bit";
        let mut buffer = Buffer::from_bytes(text.as_bytes().to_vec(), test_fs());
        let index = built(&mut buffer, 10);
        for line in index.lines() {
            assert_eq!(line.row_starts.first(), Some(&0));
            let mut sorted = line.row_starts.clone();
            sorted.sort_unstable();
            assert_eq!(line.row_starts, sorted);
        }
    }

    #[test]
    fn total_rows_is_the_sum_over_lines() {
        let text = "alpha beta gamma delta epsilon\nzeta\n\neta theta iota";
        let mut buffer = Buffer::from_bytes(text.as_bytes().to_vec(), test_fs());
        let index = built(&mut buffer, 11);
        let sum: u32 = index.lines().iter().map(|l| l.total_rows()).sum();
        assert_eq!(index.total_rows(), sum.max(1));
    }

    #[test]
    fn newline_insertion_and_deletion_re_tree() {
        let mut buffer = Buffer::from_bytes(b"alpha beta\ngamma".to_vec(), test_fs());
        let mut index = built(&mut buffer, 30);
        assert_eq!(index.lines().len(), 2);

        edit(&mut buffer, &mut index, 5, 0, "\n");
        assert_eq!(index.lines().len(), 3);
        assert_eq!(structure(&index), structure(&built(&mut buffer, 30)));

        edit(&mut buffer, &mut index, 5, 1, "");
        assert_eq!(index.lines().len(), 2);
        assert_eq!(structure(&index), structure(&built(&mut buffer, 30)));
    }

    #[test]
    fn fenwick_prefix_find_and_update() {
        let mut tree = Fenwick::default();
        tree.rebuild(&[3, 1, 4, 1, 5]);
        assert_eq!(tree.total(), 14);
        assert_eq!(tree.prefix(0), 0);
        assert_eq!(tree.prefix(3), 8);
        for row in 0..14u32 {
            let line = tree.find(row);
            assert!(tree.prefix(line) <= row);
            assert!(row < tree.prefix(line) + [3, 1, 4, 1, 5][line]);
        }
        tree.set(2, 4, 9);
        assert_eq!(tree.total(), 19);
    }

    /// Two views on one buffer at different widths keep separate row
    /// structures, and one edit repairs both.
    ///
    /// A single shared index would be rebuilt from scratch on every frame that
    /// alternated between the two splits, which is the thrash `WrapIndexSet`
    /// exists to prevent.
    #[test]
    fn each_geometry_keeps_its_own_rows_and_both_repair() {
        let text = long_line(200);
        let mut buffer = Buffer::from_bytes(text.as_bytes().to_vec(), test_fs());
        let mut set = WrapIndexSet::default();

        for width in [16usize, 40] {
            set.entry(geometry(width)).ensure_built(
                &mut buffer,
                geometry(width),
                inputs(0),
                LineEnding::LF,
                &IndexDecorations::default(),
            );
        }
        let narrow = set.get(&geometry(16)).expect("built").total_rows();
        let wide = set.get(&geometry(40)).expect("built").total_rows();
        assert!(narrow > wide, "narrower pane must need more rows");

        let start = buffer.len();
        let line_before = buffer.get_line_number(start);
        let line_start_before = buffer.line_start_offset(line_before).unwrap_or(0);
        buffer.insert(start, "X");
        let after_edit = inputs(buffer.version());
        set.damage_bytes(
            &mut buffer,
            EditDamage {
                start,
                removed: 0,
                inserted: 1,
                line_before,
                line_start_before,
                line_end_before: line_before,
            },
            LineEnding::LF,
            after_edit,
        );

        for width in [16usize, 40] {
            let repaired = set.get(&geometry(width)).expect("present");
            let fresh = built(&mut buffer, width);
            assert_eq!(
                structure(repaired),
                structure(&fresh),
                "geometry {width} diverged after the shared edit"
            );
        }
    }

    /// The set is bounded: a buffer shown in more geometries than the cap
    /// evicts the least recently used, which only costs a rebuild.
    #[test]
    fn geometry_set_is_bounded() {
        let mut set = WrapIndexSet::default();
        for width in 10..20usize {
            set.entry(geometry(width));
        }
        assert!(set.get(&geometry(19)).is_some(), "newest kept");
        assert!(set.get(&geometry(10)).is_none(), "oldest evicted");
    }

    /// Row-space `ensure_visible` matches the Python model's rule exactly
    /// (`tests/wrap_model/wrap_model/viewport.py::Viewport.ensure_visible`):
    /// scroll the minimum that puts the cursor's row inside the margin, clamped
    /// to the document.
    #[test]
    fn ensure_visible_in_rows_matches_the_model() {
        use crate::view::viewport::Viewport;

        let text = long_line(300);
        let mut buffer = Buffer::from_bytes(text.as_bytes().to_vec(), test_fs());
        let index = built(&mut buffer, 20);
        let total = index.total_rows() as usize;

        let height = 10usize;
        let mut viewport = Viewport::new(20, height as u16);
        viewport.line_wrap_enabled = true;
        let visible = viewport.visible_line_count();

        for cursor_row in (0..total).step_by(7) {
            let cursor_byte = index.byte_of_row(&buffer, cursor_row as u32).byte;
            viewport.set_top_byte(0);
            viewport.set_top_view_line_offset(0);
            viewport.ensure_visible_in_rows(&index, &buffer, cursor_byte, None);

            let top_line = buffer.get_line_number(viewport.top_byte());
            let top_row = index.line_first_row(top_line) as usize + viewport.top_view_line_offset();
            let actual_cursor_row = index.row_of_byte(&buffer, cursor_byte) as usize;
            assert!(
                (top_row..top_row + visible).contains(&actual_cursor_row),
                "cursor row {actual_cursor_row} not visible in [{top_row}, {})",
                top_row + visible
            );
            assert!(
                top_row <= total.saturating_sub(visible),
                "scrolled past the end: top {top_row} of {total}"
            );
        }
    }

    /// Deciding the scroll costs no row building — the property that collapses
    /// the frame's build-scroll-rebuild cycle.
    #[test]
    fn ensure_visible_in_rows_needs_no_rows_built() {
        use crate::view::viewport::Viewport;

        let text = long_line(300);
        let mut buffer = Buffer::from_bytes(text.as_bytes().to_vec(), test_fs());
        let index = built(&mut buffer, 20);
        let last_row = index.total_rows() - 1;
        let cursor_byte = index.byte_of_row(&buffer, last_row).byte;

        let mut viewport = Viewport::new(20, 10);
        viewport.line_wrap_enabled = true;
        // No `ViewLine` is materialised anywhere in this call; it reads the
        // index only.
        assert!(viewport.ensure_visible_in_rows(&index, &buffer, cursor_byte, None));
        assert!(viewport.top_view_line_offset() > 0 || viewport.top_byte() > 0);
    }

    /// Wheel scrolling is arithmetic: it moves by whole rows, stays clamped,
    /// and is reversible — the model's `scroll_by_rows`, and no text is read.
    #[test]
    fn scroll_visual_rows_is_arithmetic() {
        use crate::view::viewport::Viewport;

        let text = long_line(300);
        let mut buffer = Buffer::from_bytes(text.as_bytes().to_vec(), test_fs());
        let index = built(&mut buffer, 20);
        let total = index.total_rows() as usize;

        let mut viewport = Viewport::new(20, 10);
        viewport.line_wrap_enabled = true;
        let height = viewport.visible_line_count();

        let top_row = |vp: &Viewport| {
            let line = buffer.get_line_number(vp.top_byte());
            index.line_first_row(line) as usize + vp.top_view_line_offset()
        };

        viewport.scroll_visual_rows(&index, &buffer, 7);
        assert_eq!(top_row(&viewport), 7);
        viewport.scroll_visual_rows(&index, &buffer, -7);
        assert_eq!(top_row(&viewport), 0, "scrolling is reversible");

        viewport.scroll_visual_rows(&index, &buffer, 10_000);
        assert_eq!(
            top_row(&viewport),
            total.saturating_sub(height),
            "clamped to the last full page"
        );
        viewport.scroll_visual_rows(&index, &buffer, -10_000);
        assert_eq!(top_row(&viewport), 0);
    }

    /// A line with no decorations is resumable at every row — the case the whole
    /// design exists for, where the renderer never has to walk back.
    #[test]
    fn plain_long_line_is_resumable_everywhere() {
        let text = long_line(120);
        let mut buffer = Buffer::from_bytes(text.as_bytes().to_vec(), test_fs());
        let index = built(&mut buffer, 20);
        for (row, resumable) in index.lines()[0].resumable.iter().enumerate() {
            assert!(*resumable, "row {row} was not resumable");
        }
        let last = index.total_rows() - 1;
        assert_eq!(index.resumable_row_at_or_before(&buffer, last), (last, 0));
    }
}

/// Per-buffer collection of wrap indices, one per geometry in use.
///
/// Row structure depends on the buffer's content *and* on geometry — pane
/// width, gutter, wrap flags, view mode — and those live on opposite sides of
/// the ownership split: content and decorations are per buffer, geometry is per
/// rendered view. Two splits showing the same buffer at different widths have
/// genuinely different row structures.
///
/// So the set lives with the buffer and is keyed by geometry. Each view looks up
/// its own entry; an edit damages every entry at once, because the edit is a
/// property of the buffer, not of any one view. A single shared index would
/// instead be rebuilt from scratch on every frame that alternated between two
/// splits — the thrash this shape exists to prevent.
///
/// Capped: a buffer visible in more geometries than this is vanishingly rare,
/// and the eviction only costs a rebuild.
#[derive(Debug, Default)]
pub struct WrapIndexSet {
    entries: Vec<(WrapIndexGeometry, WrapIndex)>,
}

/// Views on one buffer beyond this many distinct geometries evict the oldest.
const MAX_GEOMETRIES: usize = 4;

impl WrapIndexSet {
    /// Full-rebuild work summed over every geometry of this buffer. Evicted
    /// geometries take their counts with them, so this is a floor, not a total.
    pub fn stats(&self) -> WrapIndexStats {
        self.entries
            .iter()
            .fold(WrapIndexStats::default(), |acc, (_, i)| {
                acc.merge(i.stats())
            })
    }

    /// The index for `geometry`, creating it if this geometry is new.
    ///
    /// The returned index may need building; the caller decides whether to pay
    /// for that now (see [`WrapIndex::ensure_built`]).
    pub fn entry(&mut self, geometry: WrapIndexGeometry) -> &mut WrapIndex {
        if let Some(pos) = self.entries.iter().position(|(g, _)| *g == geometry) {
            // Move to the back so eviction drops the least recently used.
            let entry = self.entries.remove(pos);
            self.entries.push(entry);
        } else {
            if self.entries.len() >= MAX_GEOMETRIES {
                self.entries.remove(0);
            }
            self.entries.push((geometry, WrapIndex::default()));
        }
        &mut self.entries.last_mut().expect("just pushed").1
    }

    /// The index for `geometry` if it exists, without creating one.
    pub fn get(&self, geometry: &WrapIndexGeometry) -> Option<&WrapIndex> {
        self.entries
            .iter()
            .find(|(g, _)| g == geometry)
            .map(|(_, i)| i)
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// The most recently used entry.
    ///
    /// For readers that run immediately after the frame's own `entry()` call
    /// and want the geometry it just used, without rebuilding it.
    pub fn most_recent(&self) -> Option<&WrapIndex> {
        self.entries.last().map(|(_, index)| index)
    }

    /// Repair every geometry after a buffer edit.
    ///
    /// An edit changes the text all views share, so each index repairs against
    /// the same damage — cheaply, since repair is local.
    pub fn damage_bytes(
        &mut self,
        buffer: &mut Buffer,
        edit: EditDamage,
        line_ending: LineEnding,
        new_inputs: crate::view::line_wrap_cache::PipelineInputs,
    ) {
        for (_, index) in &mut self.entries {
            index.damage_bytes(buffer, edit, line_ending, new_inputs);
        }
    }

    /// Every geometry rebuilds lazily. See [`WrapIndex::damage_all`].
    pub fn damage_all(&mut self) {
        for (_, index) in &mut self.entries {
            index.damage_all();
        }
    }

    /// Drop everything — used when the buffer is replaced wholesale.
    pub fn clear(&mut self) {
        self.entries.clear();
    }
}
