//! Layout containers — `Row`, `Col`, `LabeledSection`, `Overlay`.
//!
//! These four kinds compose other widgets rather than painting content of
//! their own; what is left here is the width rule a `Row` shares with the
//! description ([`allocate_row_child_widths`]).

use fresh_core::api::WidgetSpec;

use super::WidgetImpl;

pub struct Row;

impl WidgetImpl for Row {
    fn box_meta(&self, _spec: &WidgetSpec) -> super::BoxMeta {
        super::BoxMeta::plain("row")
    }
}

pub struct Col;

impl WidgetImpl for Col {
    fn box_meta(&self, _spec: &WidgetSpec) -> super::BoxMeta {
        super::BoxMeta::plain("col")
    }
}

pub struct LabeledSection;

impl WidgetImpl for LabeledSection {
    fn box_meta(&self, _spec: &WidgetSpec) -> super::BoxMeta {
        super::BoxMeta::plain("labeled_section")
    }
}

pub struct Overlay;

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
/// The explicit width a section asks for, in columns, resolved against
/// the row's `panel_width`.
///
/// `width_cols` wins over `width_pct`: it is exact, where a percent
/// rounds — three equal siblings cannot be expressed as integer
/// percents, and rounding up overflows the panel so the host wraps the
/// last one onto its own line.
fn labeled_section_width(spec: &WidgetSpec, panel_width: u32) -> Option<u32> {
    let WidgetSpec::LabeledSection {
        width_pct,
        width_cols,
        ..
    } = spec
    else {
        return None;
    };
    if let Some(cols) = width_cols.filter(|c| *c > 0) {
        return Some(cols.min(panel_width.max(1)));
    }
    width_pct
        .filter(|pct| (1..=100).contains(pct))
        .map(|pct| (panel_width as u64 * pct as u64 / 100) as u32)
}

pub fn predicts_block(spec: &WidgetSpec) -> bool {
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

/// Allocate a per-child column budget for a `Row`, aligned index-for-
/// index with `children`. Block children (those that render multi-line,
/// e.g. a `LabeledSection`) share `panel_width`: a child with an
/// explicit `width_pct` takes its declared share first, and the
/// remainder splits equally among the blocks without one. Non-block
/// children get the full `panel_width` (a soft cap — they collapse to a
/// single line, so width doesn't truncate them).
pub fn allocate_row_child_widths(children: &[WidgetSpec], panel_width: u32) -> Vec<u32> {
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
        if let Some(w) = labeled_section_width(&children[idx], panel_width) {
            per_child_width[idx] = w.max(1);
            explicit_total = explicit_total.saturating_add(w);
            explicit_count += 1;
        }
    }
    let remaining = panel_width.saturating_sub(explicit_total);
    let implicit_count = (block_count as u32).saturating_sub(explicit_count).max(1);
    let each_implicit = (remaining / implicit_count).max(1);
    for &idx in &block_indices {
        if labeled_section_width(&children[idx], panel_width).is_none() {
            per_child_width[idx] = each_implicit;
        }
    }
    per_child_width
}
