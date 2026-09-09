//! The decorations `markdown_compose` emits that belong to a Compose-mode
//! split alone.
//!
//! Decorations live on the *buffer*, but compose mode is a property of a
//! *split*: the plugin emits its frame whenever any split composes the buffer
//! (`is_composing_in_any_split`), so a Source-mode split showing the same
//! buffer — a sibling pane, or the same pane a moment after compose was turned
//! off — would otherwise draw compose's chrome around raw source.
//!
//! Every render pass therefore drops these, and only these, when the split it
//! is drawing is not composing. The names live here rather than at each pass so
//! the passes cannot disagree about what "compose-only" means: a namespace
//! suppressed in the token stream but not in the wrap index makes the index
//! describe rows the renderer never draws.
//!
//! Everything else a plugin emits (git blame headers, live-diff deletions,
//! flash's conceals, …) renders in both modes, which is why this is a list and
//! not a blanket gate.

use crate::view::virtual_text::VirtualText;

/// Conceal namespace: the cell-separator / emphasis-marker conceals that turn
/// raw `|` and `**` into the composed table, and the fence delimiters into the
/// code block's top and bottom border.
pub const MD_SYNTAX_NS: &str = "md-syntax";

/// Overlay namespace: the styling compose paints over what it composed.
pub const MD_EMPHASIS_NS: &str = "md-emphasis";

/// Virtual-line namespace: the `┌─┬─┐` / `├─┼─┤` / `└─┴─┘` table frame.
pub const MD_TABLE_BORDER_NS: &str = "md-tb";

/// Virtual-line namespace: the blank spacer rows between list items.
pub const MD_LIST_SPACING_NS: &str = "md-ls";

/// Inline virtual-text id prefix: a fenced code block's `│` side rails.
///
/// A prefix rather than a namespace because only virtual *lines* carry a
/// namespace — the inline add path keys on the id, which is also how the
/// plugin clears them (`removeVirtualTextsByPrefix`).
pub const MD_CODE_RAIL_ID_PREFIX: &str = "mdcr:";

/// The `md-syntax` conceal namespace, as the conceal store wants it.
pub fn md_syntax_namespace() -> fresh_core::overlay::OverlayNamespace {
    fresh_core::overlay::OverlayNamespace::from_string(MD_SYNTAX_NS.to_string())
}

/// The `md-emphasis` overlay namespace, as the overlay store wants it.
pub fn md_emphasis_namespace() -> fresh_core::overlay::OverlayNamespace {
    fresh_core::overlay::OverlayNamespace::from_string(MD_EMPHASIS_NS.to_string())
}

/// Whether this virtual text is one a Source-mode split must not draw.
///
/// Covers both halves of the code/table frame: the virtual *lines* that cap it
/// (matched by namespace) and the inline rails that close its sides (matched by
/// id prefix).
pub fn is_compose_only_virtual_text(vtext: &VirtualText) -> bool {
    // Compared as strings rather than by building a `VirtualTextNamespace`:
    // this runs for every virtual text in the viewport on every frame.
    if vtext
        .namespace
        .as_ref()
        .is_some_and(|ns| ns.as_str() == MD_TABLE_BORDER_NS || ns.as_str() == MD_LIST_SPACING_NS)
    {
        return true;
    }
    vtext
        .string_id
        .as_deref()
        .is_some_and(|id| id.starts_with(MD_CODE_RAIL_ID_PREFIX))
}
