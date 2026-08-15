//! Per-kind widget behaviour behind a single trait.
//!
//! This is phase 1 of `docs/internal/widget-framework-v2-review.md` §4.3:
//! `WidgetSpec` stays a closed, serializable wire type (it crosses the
//! plugin sandbox boundary and feeds the row and web renderers), while
//! the *behaviour* for each kind lives in one `WidgetImpl` per kind,
//! looked up through the single [`behavior`] dispatch below. The goal is
//! that exactly one `match` on the spec's kind survives in the codebase —
//! this one — and every other per-kind decision happens inside an impl
//! that only ever sees its own variant.
//!
//! The migration that built this module was incremental (kind by kind,
//! behaviour-preserving, guarded by the render unit tests); it is now
//! complete — [`behavior`] is total and `render::render_collected` is a
//! pure delegation to it.
//!
//! The trait currently has a single entry point, [`WidgetImpl::collect`],
//! mirroring today's one-pass renderer. The later phases of the plan grow
//! it (`measure`/`arrange` when the constraint layout lands, `on_event`
//! when input dispatch moves off the per-kind probes in
//! `app/widget_runtime.rs`) without moving the code again.

mod button;
mod containers;
mod divider;
mod dropdown;
mod dual_list;
mod hint_bar;
mod list;
mod number;
mod raw;
mod spacer;
mod text;
mod toggle;
mod tree;
mod window_embed;

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;

use super::registry::WidgetInstanceState;
use super::render::{CollectedOutput, RenderContext};

/// Behaviour for one widget kind. Implementations are unit structs;
/// each `collect` destructures its own `WidgetSpec` variant (a
/// mismatched variant is a dispatch bug and renders nothing rather
/// than panicking).
pub(crate) trait WidgetImpl: Sync {
    /// Render this node (and, for containers, its subtree) into rows,
    /// hit areas, and next-tick instance state. Semantics are identical
    /// to the corresponding arm of the legacy `render_collected` match.
    fn collect(
        &self,
        spec: &WidgetSpec,
        prev: &HashMap<String, WidgetInstanceState>,
        next_state: &mut HashMap<String, WidgetInstanceState>,
        ctx: RenderContext<'_>,
        panel_width: u32,
    ) -> CollectedOutput;
}

/// The one kind-dispatch — the single surviving `match` on a
/// `WidgetSpec`'s kind. Total: every kind has an impl.
pub(crate) fn behavior(spec: &WidgetSpec) -> &'static dyn WidgetImpl {
    match spec {
        WidgetSpec::HintBar { .. } => &hint_bar::HintBar,
        WidgetSpec::Spacer { .. } => &spacer::Spacer,
        WidgetSpec::Divider { .. } => &divider::Divider,
        WidgetSpec::Raw { .. } => &raw::Raw,
        WidgetSpec::Toggle { .. } => &toggle::Toggle,
        WidgetSpec::Button { .. } => &button::Button,
        WidgetSpec::WindowEmbed { .. } => &window_embed::WindowEmbed,
        WidgetSpec::Number { .. } => &number::Number,
        WidgetSpec::Dropdown { .. } => &dropdown::Dropdown,
        WidgetSpec::DualList { .. } => &dual_list::DualList,
        WidgetSpec::List { .. } => &list::List,
        WidgetSpec::Tree { .. } => &tree::Tree,
        WidgetSpec::Text { .. } => &text::Text,
        WidgetSpec::Row { .. } => &containers::Row,
        WidgetSpec::Col { .. } => &containers::Col,
        WidgetSpec::LabeledSection { .. } => &containers::LabeledSection,
        WidgetSpec::Overlay { .. } => &containers::Overlay,
    }
}
