//! Plugin widget runtime.
//!
//! Plugins describe panels as a [`WidgetSpec`](fresh_core::api::WidgetSpec)
//! tree. The runtime in this module owns the panel registry, runs the
//! reconciler against the previous spec, renders the resulting tree
//! into [`TextPropertyEntry`]s, and (in later phases) routes events
//! back through the hook system.
//!
//! v1 supports the `Row` / `Col` / `HintBar` / `Raw` widget kinds.
//! Additional kinds (`Toggle`, `Button`, `TextInput`, `List`, `Tree`,
//! `Layer`, `Transient`, `Table`) plug into the `render` dispatch
//! without changing the IPC shape.
//!
//! See `docs/internal/plugin-widget-library-design.md` for the full
//! design.

mod actions;
mod registry;
mod render;

pub use actions::{
    apply_text_area_key, apply_text_char, apply_text_input_key, apply_text_key, find_widget_by_key,
    set_list_items_in_spec, set_toggle_checked_in_spec, set_tree_checked_keys_in_spec,
    set_tree_nodes_in_spec, tree_parent_index,
};
pub use registry::{HitArea, PanelId, WidgetInstanceState, WidgetPanelState, WidgetRegistry};
pub use render::{render_spec, EmbedRect, FocusCursor, RenderOutput};
