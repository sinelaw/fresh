//! Layout containers — `Row`, `Col`, `LabeledSection`, `Overlay`.
//!
//! These four kinds compose other widgets rather than painting content
//! of their own, so they share the `RowPiece` two-pass machinery and
//! recurse through `render::render_collected` (which re-enters the
//! `kinds::behavior` dispatch for their children).

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;
use fresh_core::text_property::{InlineOverlay, OffsetUnit, TextPropertyEntry};

use super::WidgetImpl;
use crate::widgets::layout_box::LayoutBox;
use crate::widgets::registry::{HitArea, WidgetInstanceState};
use crate::widgets::render::{
    ensure_trailing_newline, pad_or_truncate_cols, render_collected, render_section_bottom_border,
    render_section_top_border, snap_down_to_char_boundary, strip_trailing_newline,
    wrap_in_side_border, CollectedOutput, EmbedRect, FocusCursor, OverlayRow, PanelPopup,
    RenderContext, LEFT_BORDER_PREFIX,
};

pub(crate) struct Row;

impl WidgetImpl for Row {
    fn box_meta(&self, _spec: &WidgetSpec) -> super::BoxMeta {
        super::BoxMeta::plain("row")
    }
    fn collect(
        &self,
        spec: &WidgetSpec,
        prev: &HashMap<String, WidgetInstanceState>,
        next_state: &mut HashMap<String, WidgetInstanceState>,
        ctx: RenderContext<'_>,
        panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::Row { children, wrap, .. } = spec else {
            return CollectedOutput::default();
        };
        collect_row(children, *wrap, prev, next_state, ctx, panel_width)
    }
}

pub(crate) struct Col;

impl WidgetImpl for Col {
    fn box_meta(&self, _spec: &WidgetSpec) -> super::BoxMeta {
        super::BoxMeta::plain("col")
    }
    fn collect(
        &self,
        spec: &WidgetSpec,
        prev: &HashMap<String, WidgetInstanceState>,
        next_state: &mut HashMap<String, WidgetInstanceState>,
        ctx: RenderContext<'_>,
        panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::Col { children, .. } = spec else {
            return CollectedOutput::default();
        };
        collect_col(children, prev, next_state, ctx, panel_width)
    }
}

pub(crate) struct LabeledSection;

impl WidgetImpl for LabeledSection {
    fn box_meta(&self, _spec: &WidgetSpec) -> super::BoxMeta {
        super::BoxMeta::plain("labeled_section")
    }
    fn collect(
        &self,
        spec: &WidgetSpec,
        prev: &HashMap<String, WidgetInstanceState>,
        next_state: &mut HashMap<String, WidgetInstanceState>,
        ctx: RenderContext<'_>,
        panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::LabeledSection { label, child, .. } = spec else {
            return CollectedOutput::default();
        };
        collect_labeled_section(label, child, prev, next_state, ctx, panel_width)
    }
}

pub(crate) struct Overlay;

impl WidgetImpl for Overlay {
    fn box_meta(&self, _spec: &WidgetSpec) -> super::BoxMeta {
        let mut m = super::BoxMeta::plain("overlay");
        // Promoted overlay content is an opaque surface: a click inside
        // it that nothing consumes must not fall through to the rows
        // beneath — the box tree's opacity gate enforces what the
        // long-deleted `overlay_hit_test` entry point used to.
        m.pointer_opaque = true;
        m
    }
    fn collect(
        &self,
        spec: &WidgetSpec,
        prev: &HashMap<String, WidgetInstanceState>,
        next_state: &mut HashMap<String, WidgetInstanceState>,
        ctx: RenderContext<'_>,
        panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::Overlay { child, .. } = spec else {
            return CollectedOutput::default();
        };
        collect_overlay(child, prev, next_state, ctx, panel_width)
    }
}

/// Predict whether a `WidgetSpec` will render as a multi-line
/// (Block) child of a Row, without doing the actual render. The
/// Row's layout uses this up-front to decide whether a child
/// should get its full `panel_width` (inline path) or a smaller
/// per-column budget (horizontal-zip path).
///
/// Slightly conservative — a `Col` with one inline child is
/// predicted inline (matches its actual one-line render); a `Row`
/// containing any block descendant is predicted block (so nested
/// rows participate in the zip correctly).
/// Extract the `width_pct` declaration of a Row child, if any
/// and in-range (1..=100). Currently only `LabeledSection`
/// carries this — other block kinds (Col, Tree, List,
/// multi-line Text, Raw) participate in the equal-split path.
/// Out-of-range (0, > 100, or unset) collapses to `None` so
/// callers don't have to re-check.
fn labeled_section_width_pct(spec: &WidgetSpec) -> Option<u32> {
    let WidgetSpec::LabeledSection { width_pct, .. } = spec else {
        return None;
    };
    width_pct.filter(|pct| (1..=100).contains(pct))
}

fn predicts_block(spec: &WidgetSpec) -> bool {
    match spec {
        WidgetSpec::Col { children, .. } => {
            if children.len() > 1 {
                return true;
            }
            children.first().map(predicts_block).unwrap_or(false)
        }
        WidgetSpec::LabeledSection { .. } => true,
        WidgetSpec::Tree { .. } => true,
        WidgetSpec::List { .. } => true,
        WidgetSpec::Text { rows, .. } => *rows > 1,
        WidgetSpec::WindowEmbed { rows, .. } => *rows > 1,
        WidgetSpec::Raw { entries, .. } => entries.len() > 1,
        WidgetSpec::Row { children, .. } => children.iter().any(predicts_block),
        _ => false,
    }
}

/// One position in a Row's two-pass layout. Used internally to
/// defer flex-spacer sizing until after we know all the inline
/// children's natural widths.
enum RowPiece {
    Inline {
        entry: TextPropertyEntry,
        hits: Vec<HitArea>,
        /// Some when this inline child was a focused TextInput.
        /// `byte_in_row` is the cursor's offset within the *child's*
        /// text — the Row collapse pass shifts it by the merged
        /// inline_shift before publishing.
        focus_cursor: Option<FocusCursor>,
        /// Embed rects propagated up from this inline child.
        /// Inlines collapse to row 0, so embeds inside them are
        /// pinned to that row. Rare but worth carrying through
        /// rather than dropping.
        embeds: Vec<EmbedRect>,
        /// Layout boxes from this inline child's subtree; the collapse
        /// pass shifts their columns by the *display width* of the
        /// line so far (boxes and embeds are column-addressed; hits
        /// stay byte-addressed within the row text).
        boxes: Vec<LayoutBox>,
    },
    Block {
        /// Allocated column width for the zip path. May differ
        /// from the entries' natural widths (each block was
        /// rendered with this as its `panel_width`, so the
        /// entries should already fit).
        column_width: u32,
        entries: Vec<TextPropertyEntry>,
        hits: Vec<HitArea>,
        focus_cursor: Option<FocusCursor>,
        /// Embed rects propagated up from this block child.
        /// Their `buffer_row` is already relative to the block's
        /// own row 0; the zip pass shifts row by `starting_row`
        /// and byte_in_row by the block's `byte_shift`.
        embeds: Vec<EmbedRect>,
        /// Layout boxes from this block child's subtree, shifted by the
        /// zip pass identically to `scroll_regions`.
        boxes: Vec<LayoutBox>,
    },
    Flex,
}

#[allow(clippy::too_many_arguments)]
fn collect_row(
    children: &[WidgetSpec],
    wrap: bool,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    ctx: RenderContext<'_>,
    panel_width: u32,
) -> CollectedOutput {
    let mut entries: Vec<TextPropertyEntry> = Vec::new();
    let mut hits: Vec<HitArea> = Vec::new();
    let mut focus_cursor: Option<FocusCursor> = None;
    let mut embeds: Vec<EmbedRect> = Vec::new();
    let mut overlays: Vec<OverlayRow> = Vec::new();
    let mut popups: Vec<PanelPopup> = Vec::new();
    let mut boxes: Vec<LayoutBox> = Vec::new();
    let mut wants_fill = false;
    let mut effective_rows: HashMap<String, u32> = HashMap::new();

    // Two-pass layout for Row:
    //  1. Walk children, render each. Track flex spacers
    //     by index in the accumulator; their text starts
    //     empty and grows in pass 2.
    //  2. Compute leftover width = panel_width - sum of
    //     non-flex widths; distribute evenly across flex
    //     slots; expand each flex spacer's text + shift
    //     subsequent overlays / hits accordingly.
    //
    // When ≥1 child is multi-line (a `Block`), the
    // assembly switches to a per-line zip instead of
    // the inline-collapse path — each block gets a
    // column budget and the layout walks block lines
    // left-to-right. See [the Phase 1b note in
    // docs/internal/orchestrator-open-dialog-and-lifecycle.md]
    // for the rationale.
    //
    // Width allocation for the zip path: blocks share
    // `panel_width`. Children with a `width_pct`
    // declaration get their explicit share first
    // (`panel_width * pct / 100`); the remainder splits
    // equally among blocks without an explicit width.
    // Inline children render at full `panel_width` (they
    // collapse to a single line so width is a soft cap).
    let per_child_width = allocate_row_child_widths(children, panel_width);
    let mut row_pieces: Vec<RowPiece> = Vec::new();
    for (idx, child) in children.iter().enumerate() {
        if let WidgetSpec::Spacer { flex: true, .. } = child {
            row_pieces.push(RowPiece::Flex);
            continue;
        }
        let child_panel_width = per_child_width[idx];
        let mut child_out = render_collected(child, prev, next_state, ctx, child_panel_width);
        // A Row is a horizontal packer — it has no leftover vertical
        // space to grant, so fill requests pass through to an
        // enclosing Col; effective windows merge straight up.
        wants_fill |= child_out.wants_fill;
        effective_rows.extend(std::mem::take(&mut child_out.effective_rows));
        // Rows can host overlays in principle (e.g. a
        // tooltip on a button); forward them up without
        // a row-offset adjustment — Row pieces all sit
        // on the same buffer-row as the merged row.
        overlays.extend(child_out.overlays);
        // A Dropdown in a Row collapses onto the row's single line, so
        // its pop-over anchors at the row's `buffer_row` (row 0 here;
        // the caller shifts it up). Forward unshifted, like overlays.
        popups.extend(child_out.popups);
        if child_out.entries.is_empty() {
            debug_assert!(child_out.hits.is_empty(), "empty children produce no hits");
            continue;
        }
        let child_boxes = std::mem::take(&mut child_out.boxes);
        if child_out.entries.len() == 1 {
            let mut entry = child_out.entries.into_iter().next().unwrap();
            // Inline children can't carry their own newlines
            // — that would split the merged Row across
            // buffer lines. The Row's final merged entry
            // gets exactly one newline appended below.
            strip_trailing_newline(&mut entry);
            row_pieces.push(RowPiece::Inline {
                entry,
                hits: child_out.hits,
                focus_cursor: child_out.focus_cursor,
                embeds: child_out.embeds,
                boxes: child_boxes,
            });
        } else {
            row_pieces.push(RowPiece::Block {
                column_width: child_panel_width,
                entries: child_out.entries,
                hits: child_out.hits,
                focus_cursor: child_out.focus_cursor,
                embeds: child_out.embeds,
                boxes: child_boxes,
            });
        }
    }
    // If any Block pieces survived classification, take
    // the horizontal-zip path; otherwise fall through to
    // the original inline-collapse assembly.
    let has_blocks = row_pieces
        .iter()
        .any(|p| matches!(p, RowPiece::Block { .. }));
    if has_blocks {
        zip_row_blocks(
            row_pieces,
            panel_width,
            &mut entries,
            &mut hits,
            &mut focus_cursor,
            &mut embeds,
            &mut boxes,
        );
    } else if wrap {
        // Wrapping path: greedily pack inline pieces onto lines no
        // wider than `panel_width`; a piece that doesn't fit starts a
        // new line (pieces are never split). Each piece's hits get
        // their byte offset shifted by the line-so-far and their
        // `buffer_row` set to the line index.
        assemble_wrapped_row(
            row_pieces,
            panel_width,
            &mut entries,
            &mut hits,
            &mut focus_cursor,
            &mut embeds,
            &mut boxes,
        );
    } else {
        assemble_inline_row(
            row_pieces,
            panel_width,
            &mut entries,
            &mut hits,
            &mut focus_cursor,
            &mut embeds,
            &mut boxes,
        );
    }

    CollectedOutput {
        entries,
        hits,
        focus_cursor,
        embeds,
        overlays,
        self_scroll: None,
        popups,
        wants_fill,
        effective_rows,
        boxes,
    }
}

/// Allocate a per-child column budget for a `Row`, aligned index-for-
/// index with `children`. Block children (those that render multi-line,
/// e.g. a `LabeledSection`) share `panel_width`: a child with an
/// explicit `width_pct` takes its declared share first, and the
/// remainder splits equally among the blocks without one. Non-block
/// children get the full `panel_width` (a soft cap — they collapse to a
/// single line, so width doesn't truncate them).
fn allocate_row_child_widths(children: &[WidgetSpec], panel_width: u32) -> Vec<u32> {
    let block_indices: Vec<usize> = children
        .iter()
        .enumerate()
        .filter(|(_, c)| predicts_block(c))
        .map(|(i, _)| i)
        .collect();
    let block_count = block_indices.len();
    let mut per_child_width: Vec<u32> = children.iter().map(|_| panel_width).collect();
    if block_count == 0 {
        return per_child_width;
    }
    let mut explicit_total: u32 = 0;
    let mut explicit_count: u32 = 0;
    for &idx in &block_indices {
        if let Some(pct) = labeled_section_width_pct(&children[idx]) {
            let w = (panel_width as u64 * pct as u64 / 100) as u32;
            per_child_width[idx] = w.max(1);
            explicit_total = explicit_total.saturating_add(w);
            explicit_count += 1;
        }
    }
    let remaining = panel_width.saturating_sub(explicit_total);
    let implicit_count = (block_count as u32).saturating_sub(explicit_count).max(1);
    let each_implicit = (remaining / implicit_count).max(1);
    for &idx in &block_indices {
        if labeled_section_width_pct(&children[idx]).is_none() {
            per_child_width[idx] = each_implicit;
        }
    }
    per_child_width
}

/// Assemble a `Row` of purely inline pieces (no multi-line `Block`s)
/// into a single merged entry. Flex spacers expand to fill the leftover
/// width (`panel_width` minus the natural inline width, measured in
/// display columns); child hits / focus / embeds / scroll regions are
/// shifted by the running byte offset so they stay aligned in the
/// merged row. The inline-only counterpart to [`zip_row_blocks`] and
/// [`assemble_wrapped_row`].
#[allow(clippy::too_many_arguments)]
fn assemble_inline_row(
    pieces: Vec<RowPiece>,
    panel_width: u32,
    entries: &mut Vec<TextPropertyEntry>,
    hits: &mut Vec<HitArea>,
    focus_cursor: &mut Option<FocusCursor>,
    embeds: &mut Vec<EmbedRect>,
    out_boxes: &mut Vec<LayoutBox>,
) {
    // Compute flex sizing. Width is measured in display columns
    // (`str_width`) to match `panel_width`; using the raw byte length
    // would over-count multi-byte glyphs (▣ · ▸ ↑ − …) and under-size
    // the flex spacer, leaving a right-aligned group floating short of
    // the edge.
    let inline_natural: usize = pieces
        .iter()
        .filter_map(|p| match p {
            RowPiece::Inline { entry, .. } => {
                Some(crate::primitives::display_width::str_width(&entry.text))
            }
            _ => None,
        })
        .sum();
    let flex_count = pieces
        .iter()
        .filter(|p| matches!(p, RowPiece::Flex))
        .count();
    let flex_total = (panel_width as usize).saturating_sub(inline_natural);
    // Distribute leftover evenly. With multiple flex slots,
    // the leftover bytes spread as evenly as possible (any
    // remainder lands in the first slot).
    let (flex_each, flex_extra) = match flex_total.checked_div(flex_count) {
        Some(each) => (each, flex_total % flex_count),
        None => (0, 0),
    };

    // Pass 2: assemble. Accumulate inline pieces (with
    // collapsed flex spacers) into one entry; flush block
    // pieces. Track byte-shift so child hits' offsets stay
    // correct.
    let mut acc: Option<TextPropertyEntry> = None;
    let mut flex_seen = 0usize;
    for piece in pieces {
        match piece {
            RowPiece::Inline {
                mut entry,
                hits: child_hits,
                focus_cursor: child_focus,
                embeds: child_embeds,
                boxes: child_boxes,
            } => {
                let inline_shift = match acc.as_ref() {
                    Some(e) => e.text.len(),
                    None => 0,
                };
                // Boxes and embeds are column-addressed, so they shift
                // by the *display width* of the line so far — not its
                // byte length, which over-counts every multi-byte glyph
                // (a localized toggle label, `▸`, `·`). Hits and the
                // focus cursor stay byte-addressed within the row text.
                let inline_cols = acc
                    .as_ref()
                    .map(|e| crate::primitives::display_width::str_width(&e.text))
                    .unwrap_or(0) as u32;
                // The arena merge remaps parent indices by the boxes so
                // far.
                let base = out_boxes.len();
                for mut b in child_boxes {
                    b.parent = b.parent.map(|pi| pi + base);
                    b.col += inline_cols;
                    out_boxes.push(b);
                }
                for mut h in child_hits {
                    h.byte_start += inline_shift;
                    h.byte_end += inline_shift;
                    hits.push(h);
                }
                if let Some(mut fc) = child_focus {
                    // buffer_row stays 0 — caller shifts.
                    fc.byte_in_row += inline_shift as u32;
                    *focus_cursor = Some(fc);
                }
                for mut emb in child_embeds {
                    emb.col_in_row += inline_cols;
                    embeds.push(emb);
                }
                match acc.as_mut() {
                    Some(merged) => merge_inline(merged, &mut entry),
                    None => acc = Some(entry),
                }
            }
            RowPiece::Flex => {
                // Materialize the flex spacer as N spaces.
                let n = flex_each + if flex_seen < flex_extra { 1 } else { 0 };
                flex_seen += 1;
                if n > 0 {
                    let mut text = String::with_capacity(n);
                    for _ in 0..n {
                        text.push(' ');
                    }
                    let entry = TextPropertyEntry {
                        text,
                        properties: Default::default(),
                        style: None,
                        inline_overlays: Vec::new(),
                        segments: Vec::new(),
                        pad_to_chars: None,
                        truncate_to_chars: None,
                    };
                    match acc.as_mut() {
                        Some(merged) => {
                            let mut e = entry;
                            merge_inline(merged, &mut e);
                        }
                        None => acc = Some(entry),
                    }
                }
            }
            RowPiece::Block { .. } => {
                // Unreachable in the inline-only path —
                // `has_blocks` was false here.
                debug_assert!(false, "block piece in inline-only Row path");
            }
        }
    }
    if let Some(mut merged) = acc {
        ensure_trailing_newline(&mut merged);
        entries.push(merged);
    }
}

#[allow(clippy::too_many_arguments)]
fn collect_col(
    children: &[WidgetSpec],
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    ctx: RenderContext<'_>,
    panel_width: u32,
) -> CollectedOutput {
    let entries: Vec<TextPropertyEntry> = Vec::new();
    let hits: Vec<HitArea> = Vec::new();
    let focus_cursor: Option<FocusCursor> = None;
    let embeds: Vec<EmbedRect> = Vec::new();
    let overlays: Vec<OverlayRow> = Vec::new();
    let popups: Vec<PanelPopup> = Vec::new();
    let boxes: Vec<LayoutBox> = Vec::new();
    let wants_fill = false;
    let effective_rows: HashMap<String, u32> = HashMap::new();

    // Pass 1 — render every child with NO height budget, so an
    // auto-sized List/Tree in a subtree reports `wants_fill` instead
    // of consuming a budget meant for the whole column.
    let child_ctx = RenderContext {
        avail_height: None,
        ..ctx
    };
    let mut child_outs: Vec<CollectedOutput> = children
        .iter()
        .map(|child| render_collected(child, prev, next_state, child_ctx, panel_width))
        .collect();

    // Fill pass — when this col HAS a height budget and exactly one
    // child subtree wants auto-sizing, that child's List/Tree gets
    // the leftover: the budget minus the rows every *other* child
    // occupies (Overlay children occupy none). This is what lets a
    // plugin write `list()` with no `visibleRows` and get "fill the
    // panel, minus my header and footer" without doing the
    // arithmetic. Two or more fill children is ambiguous — all keep
    // the legacy fallback (documented; matches flex-grow needing a
    // single growable child in this v1).
    if let Some(budget) = ctx.avail_height {
        let fill_idx: Vec<usize> = child_outs
            .iter()
            .enumerate()
            .filter(|(_, o)| o.wants_fill)
            .map(|(i, _)| i)
            .collect();
        if let [idx] = fill_idx[..] {
            let other_rows: u32 = child_outs
                .iter()
                .enumerate()
                .filter(|(i, _)| *i != idx)
                .zip(children.iter().enumerate().filter(|(i, _)| *i != idx))
                .filter(|(_, (_, c))| !promotes_as_overlay(c))
                .map(|((_, o), _)| o.entries.len() as u32)
                .sum();
            let child_budget = budget.saturating_sub(other_rows).max(1);
            let fill_ctx = RenderContext {
                avail_height: Some(child_budget),
                ..ctx
            };
            child_outs[idx] =
                render_collected(&children[idx], prev, next_state, fill_ctx, panel_width);
        }
    }

    // Vertical flex — the Row flex model turned 90°: with a height
    // budget, `Spacer { flex: true }` children absorb the leftover
    // rows (split evenly, remainder to the first), so "pin the hint
    // bar to the panel bottom" is a flex spacer instead of the
    // plugin counting chrome rows and emitting blank Raw padding.
    // Runs after the single-fill pass: a resolved auto-sized
    // List/Tree consumes the leftover first, and flex spacers then
    // see none — the two interact by documented precedence instead
    // of fighting.
    if let Some(budget) = ctx.avail_height {
        let flex_idx: Vec<usize> = children
            .iter()
            .enumerate()
            .filter(|(_, c)| matches!(c, WidgetSpec::Spacer { flex: true, .. }))
            .map(|(i, _)| i)
            .collect();
        if !flex_idx.is_empty() {
            let used: u32 = child_outs
                .iter()
                .zip(children.iter())
                .filter(|(_, c)| !promotes_as_overlay(c))
                .map(|(o, _)| o.entries.len() as u32)
                .sum();
            let leftover = budget.saturating_sub(used);
            let each = leftover / flex_idx.len() as u32;
            let extra = leftover % flex_idx.len() as u32;
            for (n, &i) in flex_idx.iter().enumerate() {
                let rows = each + if (n as u32) < extra { 1 } else { 0 };
                for _ in 0..rows {
                    let mut entry = TextPropertyEntry::text("");
                    ensure_trailing_newline(&mut entry);
                    child_outs[i].entries.push(entry);
                }
                // The spacer's own box was capped before the stretch;
                // keep its rectangle honest.
                let stretched = child_outs[i].entries.len() as u32;
                if let Some(b) = child_outs[i].boxes.last_mut() {
                    b.height = stretched;
                }
            }
        }
    }

    // Fold every child in through the one shift point — a Col cannot
    // shift some geometry channels and forget others. Overlay children
    // occupy no column height; their subtree is promoted a stacking
    // level and anchored at the current cursor.
    let mut acc = CollectedOutput {
        entries,
        hits,
        focus_cursor,
        embeds,
        overlays,
        self_scroll: None,
        popups,
        wants_fill,
        effective_rows,
        boxes,
    };
    for (child, child_out) in children.iter().zip(child_outs) {
        let row_offset = acc.entries.len() as u32;
        acc.absorb_child(child_out, row_offset, promotes_as_overlay(child));
    }
    // Resolved fill children re-rendered with a real budget no longer
    // set the flag; only an unresolved request bubbles.
    acc
}

#[allow(clippy::too_many_arguments)]
fn collect_labeled_section(
    label: &str,
    child: &WidgetSpec,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    ctx: RenderContext<'_>,
    panel_width: u32,
) -> CollectedOutput {
    let mut entries: Vec<TextPropertyEntry> = Vec::new();
    let mut hits: Vec<HitArea> = Vec::new();
    let mut focus_cursor: Option<FocusCursor> = None;
    let mut embeds: Vec<EmbedRect> = Vec::new();
    let mut overlays: Vec<OverlayRow> = Vec::new();
    let mut popups: Vec<PanelPopup> = Vec::new();

    // Inner area: 1 column of border + 1 column of
    // padding on each side ⇒ 4 columns of chrome.
    let inner_width = panel_width.saturating_sub(4).max(1);
    // The section's frame consumes two rows (top + bottom border), so
    // any height budget flowing through shrinks accordingly before it
    // reaches the child.
    let section_ctx = RenderContext {
        avail_height: ctx.avail_height.map(|h| h.saturating_sub(2)),
        ..ctx
    };
    let mut child_out = render_collected(child, prev, next_state, section_ctx, inner_width);
    let wants_fill = child_out.wants_fill;
    let effective_rows = std::mem::take(&mut child_out.effective_rows);
    // ONE translation for every geometry channel: +1 row (the top
    // border this section emits — the child authored anchors relative
    // to its own row 0, so the Text completion-popup overlay's anchor 1
    // lands on the section's bottom border row, anchor 2+ below it;
    // flow-anchored pop-overs shift the same way while absolute anchors
    // stay put), plus the `│ ` prefix in display columns for the
    // column-addressed channels and in bytes for the byte-addressed
    // ones. `shift_channels` moves them all together — this used to be
    // six hand-copied shifts synced by prose.
    child_out.shift_channels(
        1,
        LEFT_BORDER_PREFIX.chars().count() as u32,
        LEFT_BORDER_PREFIX.len(),
    );
    let mut boxes: Vec<LayoutBox> = std::mem::take(&mut child_out.boxes);
    for b in &mut boxes {
        // Scrollable boxes widen two more columns — through the right
        // padding onto the `│` border — so a wheel over the section
        // border still scrolls the widget inside it (a section-specific
        // tweak on top of the shared shift).
        if b.scrollable {
            b.width += 2;
        }
    }
    overlays.extend(std::mem::take(&mut child_out.overlays));
    popups.extend(std::mem::take(&mut child_out.popups));

    // Render the top border with the label embedded as a
    // legend: `╭─ <label> ─...─╮`. When the label is empty,
    // produce a plain `╭─...─╮` bar.
    let total_cols = panel_width.max(2) as usize;
    entries.push(render_section_top_border(label, total_cols));

    // Render each child row wrapped with the side borders
    // and one column of padding. Pad/truncate the child
    // text to exactly `inner_width` so the right border
    // lines up regardless of the child's natural width.
    for mut child_entry in child_out.entries {
        strip_trailing_newline(&mut child_entry);
        let wrapped = wrap_in_side_border(child_entry, inner_width as usize);
        let row_offset = entries.len() as u32;
        // Shift hits/focus emitted by the child by 1 row
        // (top border) and by the left-border prefix
        // ("│ " — 4 bytes for the box-drawing char + 1
        // for the space).
        let _ = row_offset;
        entries.push(wrapped);
    }

    // Hits, focus cursor and embeds were already translated by the
    // `shift_channels` call above (bytes for the byte-addressed
    // channels, display columns for the embeds — the `│ ` prefix is
    // 4 UTF-8 bytes but only 2 display columns wide).
    hits.extend(std::mem::take(&mut child_out.hits));
    if let Some(fc) = child_out.focus_cursor.take() {
        focus_cursor = Some(fc);
    }
    embeds.extend(std::mem::take(&mut child_out.embeds));

    entries.push(render_section_bottom_border(total_cols));

    CollectedOutput {
        entries,
        hits,
        focus_cursor,
        embeds,
        overlays,
        self_scroll: None,
        popups,
        wants_fill,
        effective_rows,
        boxes,
    }
}

/// Whether a Col child renders as a PROMOTED overlay — its rows float
/// over the panel at bumped z instead of consuming column height. The
/// `Overlay` wrapper, and the panel-clipped `Popup`
/// (`screen_space: false`), which documents itself as riding the same
/// promoted path (`popup.rs`) — matching only `Overlay` here left that
/// popup's rows flowing inline with its `pointer_opaque` box stuck at
/// z=0, where the panel opacity probe (which requires z > 0) never
/// saw it. ONE predicate for every site that decides promotion: the
/// absorb loop and both fill/flex row-budget filters.
fn promotes_as_overlay(child: &WidgetSpec) -> bool {
    matches!(child, WidgetSpec::Overlay { .. })
        || matches!(
            child,
            WidgetSpec::Popup {
                screen_space: false,
                ..
            }
        )
}

#[allow(clippy::too_many_arguments)]
pub(super) fn collect_overlay(
    child: &WidgetSpec,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    ctx: RenderContext<'_>,
    panel_width: u32,
) -> CollectedOutput {
    // Renders the child normally; the parent (`Col`)
    // is what decides to promote the resulting entries
    // into the overlay set instead of consuming
    // vertical space. Outside of a `Col`, an Overlay
    // behaves like a transparent wrapper — entries
    // flow through unchanged. This keeps the
    // Overlay-as-root case (no enclosing Col) sane:
    // it just renders inline.
    let child_out = render_collected(child, prev, next_state, ctx, panel_width);
    CollectedOutput {
        entries: child_out.entries,
        hits: child_out.hits,
        focus_cursor: child_out.focus_cursor,
        embeds: child_out.embeds,
        overlays: child_out.overlays,
        self_scroll: None,
        popups: child_out.popups,
        boxes: child_out.boxes,
        // An Overlay occupies no column rows, so it never receives a
        // fill budget from `collect_col`; a fill request inside one
        // stays on the legacy fallback (bubbling it would let a popup
        // consume the column's leftover height).
        wants_fill: false,
        effective_rows: child_out.effective_rows,
    }
}

/// Assemble a wrapping Row: pack inline pieces onto lines no wider than
/// `panel_width` (display columns), starting a new line when the next piece
/// would overflow. Pieces are never split, so wrap logical groups in a
/// nested non-wrapping Row to keep them intact. A whitespace-only piece (a
/// separator spacer) at the start of a fresh line is dropped so wrapped lines
/// don't begin with stray indentation. `Flex` spacers are ignored in the
/// wrap path (flex distribution is meaningless across reflowed lines).
fn assemble_wrapped_row(
    pieces: Vec<RowPiece>,
    panel_width: u32,
    entries: &mut Vec<TextPropertyEntry>,
    hits: &mut Vec<HitArea>,
    focus_cursor: &mut Option<FocusCursor>,
    embeds: &mut Vec<EmbedRect>,
    out_boxes: &mut Vec<LayoutBox>,
) {
    use crate::primitives::display_width::str_width;
    let max_w = panel_width as usize;
    let mut acc: Option<TextPropertyEntry> = None;
    let mut row: u32 = 0;
    // Hits for the current (not-yet-flushed) line, with byte offsets already
    // shifted but buffer_row not yet stamped (set when the line is started).
    let flush = |acc: &mut Option<TextPropertyEntry>, entries: &mut Vec<TextPropertyEntry>| {
        if let Some(mut merged) = acc.take() {
            ensure_trailing_newline(&mut merged);
            entries.push(merged);
        }
    };
    for piece in pieces {
        let RowPiece::Inline {
            mut entry,
            hits: child_hits,
            focus_cursor: piece_fc,
            embeds: child_embeds,
            boxes: child_boxes,
        } = piece
        else {
            // Flex / Block: ignored in the wrap path.
            continue;
        };
        let is_blank = entry.text.trim().is_empty();
        let piece_w = str_width(&entry.text);
        let acc_w = acc.as_ref().map(|e| str_width(&e.text)).unwrap_or(0);
        // Overflow → start a new line first.
        if acc.is_some() && acc_w + piece_w > max_w {
            flush(&mut acc, entries);
            row += 1;
        }
        // Drop a separator spacer that would lead a fresh line.
        if acc.is_none() && is_blank {
            continue;
        }
        let shift = acc.as_ref().map(|e| e.text.len()).unwrap_or(0);
        for mut h in child_hits {
            h.byte_start += shift;
            h.byte_end += shift;
            h.buffer_row = row;
            hits.push(h);
        }
        // Boxes are column-addressed: they land on the wrapped line at
        // the line-so-far *display width*, not its byte length — the
        // two diverge on every multi-byte glyph. (Recomputed here
        // rather than reusing `acc_w`, which is stale after a flush
        // started a fresh line.)
        let shift_cols = acc.as_ref().map(|e| str_width(&e.text)).unwrap_or(0) as u32;
        let base = out_boxes.len();
        for mut b in child_boxes {
            b.parent = b.parent.map(|pi| pi + base);
            b.row += row;
            b.col += shift_cols;
            out_boxes.push(b);
        }
        // Embeds ride the same column-addressed shift as boxes; the
        // wrap path used to drop them silently (`..` in the
        // destructure) while the inline and zip paths carried them.
        for mut emb in child_embeds {
            emb.buffer_row += row;
            emb.col_in_row += shift_cols;
            embeds.push(emb);
        }
        // A focused piece (e.g. the search TextInput) reports its caret;
        // shift it by the line-so-far and stamp the wrapped line index so
        // the host draws the cursor on the right row.
        if let Some(mut fc) = piece_fc {
            fc.byte_in_row += shift as u32;
            fc.buffer_row = row;
            *focus_cursor = Some(fc);
        }
        match acc.as_mut() {
            Some(merged) => merge_inline(merged, &mut entry),
            None => acc = Some(entry),
        }
    }
    flush(&mut acc, entries);
}

/// Merge `next` into `merged` for the inline-row collapse path.
/// `next`'s overlays are byte-shifted to account for the merged
/// text length so far.
fn merge_inline(merged: &mut TextPropertyEntry, next: &mut TextPropertyEntry) {
    let shift = merged.text.len();
    merged.text.push_str(&next.text);
    for overlay in next.inline_overlays.drain(..) {
        merged.inline_overlays.push(InlineOverlay {
            start: overlay.start + shift,
            end: overlay.end + shift,
            style: overlay.style,
            properties: overlay.properties,
            unit: overlay.unit,
        });
    }
    // `style` and `properties` from `next` are dropped — Row inline
    // collapse only preserves inline_overlays. Whole-entry style on
    // an inline-row child has no meaningful semantics here; if a
    // plugin needs whole-line styling it should produce a Col with
    // the styled child as its sole element.
}

/// Horizontal-zip pass for a Row that contains ≥1 multi-line
/// (Block) child. Each block has already been rendered with its
/// per-column budget (`block_width`); this helper walks the
/// row's pieces left-to-right per visual row and stitches them
/// into one merged line at a time.
///
/// Layout rules:
///   * Inline pieces sit at row 0 and become `chars().count()`
///     spaces on subsequent rows (so the right-hand block stays
///     aligned with its column).
///   * Block pieces contribute their `entries[row]` (or a blank
///     row of `block_width` spaces past their height).
///   * Flex pieces are intentionally a no-op in the block path —
///     `row(block, flexSpacer(), block)` is a rare shape and we
///     skip honouring flex here to keep the budget arithmetic
///     simple. Plugins that need a fixed gap should use
///     `spacer(n)` instead.
///
/// Hits and focus cursors get shifted by both the buffer-row
/// offset (which output line we're on) and the per-piece
/// byte-column offset (where in the merged text the piece
/// starts).
#[allow(clippy::too_many_arguments)]
fn zip_row_blocks(
    pieces: Vec<RowPiece>,
    panel_width: u32,
    out_entries: &mut Vec<TextPropertyEntry>,
    out_hits: &mut Vec<HitArea>,
    out_focus_cursor: &mut Option<FocusCursor>,
    out_embeds: &mut Vec<EmbedRect>,
    out_boxes: &mut Vec<LayoutBox>,
) {
    let starting_row = out_entries.len() as u32;
    let _ = panel_width;

    // Compute the merged height = max(block.entries.len()).
    let max_height = pieces
        .iter()
        .filter_map(|p| match p {
            RowPiece::Block { entries, .. } => Some(entries.len()),
            _ => None,
        })
        .max()
        .unwrap_or(0);
    if max_height == 0 {
        return;
    }

    for row_idx in 0..max_height {
        let mut text = String::new();
        let mut overlays: Vec<InlineOverlay> = Vec::new();
        for piece in &pieces {
            match piece {
                RowPiece::Inline {
                    entry,
                    hits,
                    focus_cursor,
                    embeds: inline_embeds,
                    boxes: piece_boxes,
                } => {
                    let inline_cols = entry.text.chars().count();
                    let byte_shift = text.len();
                    // Cumulative column width to the left of this
                    // piece, for embed/box positioning. Embeds and
                    // boxes are column-addressed (display width), not
                    // byte- or char-addressed.
                    let col_shift = crate::primitives::display_width::str_width(&text) as u32;
                    if row_idx == 0 {
                        text.push_str(&entry.text);
                        for emb in inline_embeds {
                            out_embeds.push(EmbedRect {
                                window_id: emb.window_id,
                                buffer_row: starting_row + emb.buffer_row,
                                col_in_row: emb.col_in_row + col_shift,
                                width_cols: emb.width_cols,
                                height_rows: emb.height_rows,
                            });
                        }
                        let base = out_boxes.len();
                        for b in piece_boxes {
                            let mut b = b.clone();
                            b.parent = b.parent.map(|pi| pi + base);
                            b.row += starting_row;
                            b.col += col_shift;
                            out_boxes.push(b);
                        }
                        for overlay in &entry.inline_overlays {
                            overlays.push(InlineOverlay {
                                start: overlay.start + byte_shift,
                                end: overlay.end + byte_shift,
                                style: overlay.style.clone(),
                                properties: overlay.properties.clone(),
                                unit: overlay.unit,
                            });
                        }
                        for h in hits {
                            let mut h = h.clone();
                            h.byte_start += byte_shift;
                            h.byte_end += byte_shift;
                            h.buffer_row = starting_row;
                            out_hits.push(h);
                        }
                        if let Some(fc) = focus_cursor {
                            *out_focus_cursor = Some(FocusCursor {
                                buffer_row: starting_row,
                                byte_in_row: fc.byte_in_row + byte_shift as u32,
                            });
                        }
                    } else {
                        for _ in 0..inline_cols {
                            text.push(' ');
                        }
                    }
                }
                RowPiece::Flex => {
                    // Skipped — see fn doc.
                }
                RowPiece::Block {
                    column_width,
                    entries,
                    hits,
                    focus_cursor,
                    embeds: block_embeds,
                    boxes: piece_boxes,
                } => {
                    let block_w = *column_width as usize;
                    let byte_shift = text.len();
                    // Cumulative column width to the left of this
                    // block, for embed/box positioning (display width).
                    let col_shift = crate::primitives::display_width::str_width(&text) as u32;
                    // Emit each embed exactly once, on the row
                    // where its top edge lands. The embed's
                    // buffer_row is relative to the block's row
                    // 0; absolute = starting_row + that.
                    if row_idx == 0 {
                        for emb in block_embeds {
                            out_embeds.push(EmbedRect {
                                window_id: emb.window_id,
                                buffer_row: starting_row + emb.buffer_row,
                                col_in_row: emb.col_in_row + col_shift,
                                width_cols: emb.width_cols,
                                height_rows: emb.height_rows,
                            });
                        }
                        let base = out_boxes.len();
                        for b in piece_boxes {
                            let mut b = b.clone();
                            b.parent = b.parent.map(|pi| pi + base);
                            b.row += starting_row;
                            b.col += col_shift;
                            out_boxes.push(b);
                        }
                    }
                    if let Some(line) = entries.get(row_idx) {
                        let mut line_text = line.text.clone();
                        // Strip the entry's trailing newline so it
                        // doesn't split our merged line.
                        if line_text.ends_with('\n') {
                            line_text.pop();
                        }
                        pad_or_truncate_cols(&mut line_text, block_w);
                        let padded_byte_len = line_text.len();
                        text.push_str(&line_text);
                        // Convert the entry's whole-line `style`
                        // into an inline overlay covering the
                        // block's column in the merged row. This is
                        // what carries through the list widget's
                        // selected-row bg (and any other
                        // whole-entry styling on individual block
                        // lines) — without it, the picker's
                        // selection highlight disappears in the
                        // zipped output.
                        if let Some(line_style) = &line.style {
                            // In the merged row this block owns only its
                            // own columns. A surviving `extend_to_line_end`
                            // (list selection band) would tail-fill the
                            // merged line past every sibling column to the
                            // split's right edge — the stray highlight
                            // block at the screen edge.
                            let mut style = line_style.clone();
                            style.extend_to_line_end = false;
                            overlays.push(InlineOverlay {
                                start: byte_shift,
                                end: byte_shift + padded_byte_len,
                                style,
                                properties: Default::default(),
                                unit: OffsetUnit::Byte,
                            });
                        }
                        for overlay in &line.inline_overlays {
                            // `pad_or_truncate_cols` may have cut the
                            // line (and appended a multi-byte `…`), so
                            // an overlay computed against the original
                            // line can now point past — or *inside* — a
                            // char of the truncated text. Clamp both
                            // ends to the truncated length and snap to a
                            // char boundary; otherwise the downstream
                            // span splitter slices mid-char and panics.
                            let start = snap_down_to_char_boundary(&line_text, overlay.start);
                            let end = snap_down_to_char_boundary(&line_text, overlay.end);
                            if start >= end {
                                continue;
                            }
                            // Same reasoning as the whole-line style above:
                            // an inline overlay's `extend_to_line_end` was
                            // authored against the block's own line and
                            // must not tail-fill the merged row.
                            let mut style = overlay.style.clone();
                            style.extend_to_line_end = false;
                            overlays.push(InlineOverlay {
                                start: start + byte_shift,
                                end: end + byte_shift,
                                style,
                                properties: overlay.properties.clone(),
                                unit: overlay.unit,
                            });
                        }
                        for h in hits {
                            if h.buffer_row != row_idx as u32 {
                                continue;
                            }
                            let mut h = h.clone();
                            h.byte_start += byte_shift;
                            h.byte_end += byte_shift;
                            h.buffer_row = starting_row + row_idx as u32;
                            out_hits.push(h);
                        }
                        if let Some(fc) = focus_cursor {
                            if fc.buffer_row == row_idx as u32 {
                                *out_focus_cursor = Some(FocusCursor {
                                    buffer_row: starting_row + row_idx as u32,
                                    byte_in_row: fc.byte_in_row + byte_shift as u32,
                                });
                            }
                        }
                    } else {
                        // Past this block's height — emit a blank
                        // column of `block_w` spaces.
                        for _ in 0..block_w {
                            text.push(' ');
                        }
                    }
                }
            }
        }
        text.push('\n');
        out_entries.push(TextPropertyEntry {
            text,
            properties: Default::default(),
            style: None,
            inline_overlays: overlays,
            segments: Vec::new(),
            pad_to_chars: None,
            truncate_to_chars: None,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::widgets::render::tests::{make_list, make_text_input};
    use crate::widgets::render::KEY_FOCUSED_BG;
    use crate::widgets::render_spec;
    use fresh_core::api::OverlayColorSpec;

    #[test]
    fn zip_row_blocks_keeps_overlays_on_char_boundaries() {
        // Regression for the orchestrator picker panic: a two-pane
        // `row(labeledSection, labeledSection)` whose left label is
        // long and contains a multi-byte `·`. The column is narrow
        // enough that `pad_or_truncate_cols` cuts the label and
        // appends a multi-byte `…`. Before the fix, the label's
        // byte-unit overlay end was clamped to the *pre*-truncation
        // length, leaving it pointing inside the `…` — and the app
        // span splitter then sliced `text[a..b]` mid-char and
        // panicked. Every emitted overlay offset must land on a char
        // boundary of its row text.
        let left = WidgetSpec::LabeledSection {
            label: "alpha/beta · this project (2)".into(),
            child: Box::new(make_text_input("x", -1, false, false, 4, Some("a"))),
            width_pct: Some(40),
            key: None,
        };
        let right = WidgetSpec::LabeledSection {
            label: "preview".into(),
            child: Box::new(make_text_input("y", -1, false, false, 4, Some("b"))),
            width_pct: None,
            key: None,
        };
        let spec = WidgetSpec::Row {
            wrap: false,
            children: vec![left, right],
            key: None,
        };
        let out = render_spec(&spec, &HashMap::new(), "", 40);
        for e in &out.entries {
            for o in &e.inline_overlays {
                assert!(
                    e.text.is_char_boundary(o.start.min(e.text.len())),
                    "overlay start {} not on a char boundary of {:?}",
                    o.start,
                    e.text,
                );
                assert!(
                    e.text.is_char_boundary(o.end.min(e.text.len())),
                    "overlay end {} not on a char boundary of {:?}",
                    o.end,
                    e.text,
                );
            }
        }
    }

    #[test]
    fn zip_row_blocks_scopes_selection_to_its_column() {
        // Two side-by-side sections, each holding a keyed list, the left
        // one with a selected row. The selection band's
        // `extend_to_line_end` must not survive into the merged rows: on
        // a merged row the block owns only its own columns, and a
        // surviving flag makes the painter tail-fill the row past the
        // panel's right border (the code tour's stray highlight block at
        // the screen edge).
        let left = WidgetSpec::LabeledSection {
            label: "Steps".into(),
            child: Box::new(make_list(0, 3, 10, Some("rail"))),
            width_pct: Some(30),
            key: None,
        };
        let right = WidgetSpec::LabeledSection {
            label: "Prose".into(),
            child: Box::new(make_list(-1, 3, 10, Some("prose"))),
            width_pct: Some(70),
            key: None,
        };
        let spec = WidgetSpec::Row {
            wrap: false,
            children: vec![left, right],
            key: None,
        };
        let out = render_spec(&spec, &HashMap::new(), "", 60);
        let mut saw_selection_band = false;
        for e in &out.entries {
            assert!(
                !e.style.as_ref().is_some_and(|s| s.extend_to_line_end),
                "merged row must not carry a row-level extend_to_line_end: {:?}",
                e.text
            );
            for o in &e.inline_overlays {
                assert!(
                    !o.style.extend_to_line_end,
                    "overlay [{}, {}) of {:?} must not extend to line end",
                    o.start, o.end, e.text
                );
                if matches!(&o.style.bg, Some(OverlayColorSpec::ThemeKey(k)) if k == KEY_FOCUSED_BG)
                {
                    saw_selection_band = true;
                }
            }
        }
        assert!(
            saw_selection_band,
            "the selected row must still paint its selection band"
        );
    }
}

#[cfg(test)]
mod fill_tests {
    use super::*;
    use crate::widgets::render::tests::make_list;
    use crate::widgets::render_spec_with_options;
    use crate::widgets::RenderOptions;
    use fresh_core::api::{HintEntry, WidgetSpec};

    fn auto_list(total: usize, key: &str) -> WidgetSpec {
        // make_list builds an explicit-visible list; blank the field to
        // exercise the auto path.
        let mut spec = make_list(0, 1, total, Some(key));
        if let WidgetSpec::List { visible_rows, .. } = &mut spec {
            *visible_rows = None;
        }
        spec
    }

    fn hint_bar() -> WidgetSpec {
        WidgetSpec::HintBar {
            entries: vec![HintEntry {
                keys: "Esc".into(),
                label: "close".into(),
            }],
            key: None,
        }
    }

    #[test]
    fn auto_list_fills_the_panel_height() {
        // A lone auto list in a 12-row panel windows to 12 rows.
        let out = render_spec_with_options(
            &auto_list(50, "l"),
            &HashMap::new(),
            40,
            RenderOptions {
                avail_height: Some(12),
                ..Default::default()
            },
        );
        assert_eq!(out.effective_rows.get("l"), Some(&12));
        assert_eq!(out.entries.len(), 12);
    }

    #[test]
    fn auto_list_without_budget_uses_legacy_fallback() {
        let out = render_spec_with_options(
            &auto_list(50, "l"),
            &HashMap::new(),
            40,
            RenderOptions::default(),
        );
        assert_eq!(
            out.effective_rows.get("l"),
            Some(&fresh_core::api::LEGACY_VISIBLE_ROWS_FALLBACK)
        );
    }

    #[test]
    fn col_fill_grants_the_leftover_after_siblings() {
        // header + auto list + footer in a 12-row panel: the list gets
        // 12 - 2 = 10 rows, so the footer still fits on screen. This is
        // the panel shape (chrome above AND below) that plugins used to
        // hand-compute with `getViewportHeight() - fixedRows`.
        let spec = WidgetSpec::Col {
            children: vec![hint_bar(), auto_list(50, "l"), hint_bar()],
            key: None,
        };
        let out = render_spec_with_options(
            &spec,
            &HashMap::new(),
            40,
            RenderOptions {
                avail_height: Some(12),
                ..Default::default()
            },
        );
        assert_eq!(out.effective_rows.get("l"), Some(&10));
        assert_eq!(out.entries.len(), 12);
    }

    #[test]
    fn explicit_visible_rows_still_wins_over_the_budget() {
        let spec = WidgetSpec::Col {
            children: vec![make_list(0, 4, 50, Some("l"))],
            key: None,
        };
        let out = render_spec_with_options(
            &spec,
            &HashMap::new(),
            40,
            RenderOptions {
                avail_height: Some(12),
                ..Default::default()
            },
        );
        assert_eq!(out.effective_rows.get("l"), Some(&4));
        assert_eq!(out.entries.len(), 4);
    }

    #[test]
    fn two_auto_children_is_ambiguous_and_keeps_the_fallback() {
        let spec = WidgetSpec::Col {
            children: vec![auto_list(50, "a"), auto_list(50, "b")],
            key: None,
        };
        let out = render_spec_with_options(
            &spec,
            &HashMap::new(),
            40,
            RenderOptions {
                avail_height: Some(12),
                ..Default::default()
            },
        );
        let legacy = fresh_core::api::LEGACY_VISIBLE_ROWS_FALLBACK;
        assert_eq!(out.effective_rows.get("a"), Some(&legacy));
        assert_eq!(out.effective_rows.get("b"), Some(&legacy));
    }
}
