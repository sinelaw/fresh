//! Plugin widget runtime.
//!
//! Plugins describe panels as a [`WidgetSpec`](fresh_core::api::WidgetSpec)
//! tree. The runtime in this module owns the panel registry, runs the
//! reconciler against the previous spec, renders the resulting tree
//! into [`TextPropertyEntry`]s, and (in later phases) routes events
//! back through the hook system.
//!
//! Every `WidgetSpec` variant is implemented; [`kinds::behavior`] is the
//! one total `match` on a spec kind and the dispatch every projection goes
//! through. (This paragraph used to say "v1 supports Row / Col / HintBar /
//! Raw" and list the rest as future work — all of them landed, and the same
//! stale list is at the head of [`render`].)
//!
//! See `docs/internal/plugin-widget-library-design.md` for the full
//! design.

mod actions;
pub(crate) mod kinds;
pub(crate) mod layout_box;
mod registry;
pub(crate) mod render;
mod text_click;

pub use actions::{
    append_tree_nodes_in_spec, find_widget_by_key, set_list_items_in_spec, set_raw_entries_in_spec,
    set_toggle_checked_in_spec, set_tree_checked_keys_in_spec, set_tree_nodes_in_spec,
    tree_parent_index,
};
pub use kinds::collect_visible_tree_indices;
pub use layout_box::LayoutBox;
pub use registry::{
    HitArea, PanelId, PanelKey, WidgetEvent, WidgetInstanceState, WidgetPanelState, WidgetRegistry,
};
pub use render::{
    apply_hover_band, clamp_number, dual_available_values, dual_label, dual_sanitize_included,
    fill_button_label, format_number_value, render_bare_button, render_button, render_dropdown,
    render_hint_bar, render_number, render_spec, render_spec_no_autofocus, render_spec_with_marker,
    render_spec_with_options, render_toggle, render_toggle_form, render_tree_row, wrap_index,
    EmbedRect, FocusCursor, MarkdownCtx, NumberEdit, OverlayRow, PanelPopup, RenderContext,
    RenderOptions, RenderOutput, RenderedTreeRow, DROPDOWN_VISIBLE_OPTIONS,
};
pub use text_click::{row_byte_to_value_byte, value_byte_from_hit};
