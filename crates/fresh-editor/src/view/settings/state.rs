//! Settings state management
//!
//! Tracks the current state of the settings UI, pending changes,
//! and provides methods for reading/writing config values.

use super::entry_dialog::EntryDialogState;
use super::hit::SettingsHit;
use super::items::{control_to_value, SettingControl, SettingItem, SettingsPage};
use super::live;
use super::schema::{parse_schema, SettingCategory, SettingSchema};
use super::search::{search_settings, DeepMatch, SearchResult};
use crate::config::Config;
use crate::config_io::ConfigLayer;
use crate::view::ui::ScrollablePanel;
use std::collections::HashMap;

/// Set a value at a JSON pointer path, creating intermediate objects as
/// needed. Mirrors `config_io::set_json_pointer` (kept private there).
fn set_json_pointer_create(root: &mut serde_json::Value, pointer: &str, value: serde_json::Value) {
    if pointer.is_empty() || pointer == "/" {
        *root = value;
        return;
    }
    let parts: Vec<&str> = pointer.trim_start_matches('/').split('/').collect();
    let mut current = root;
    for (i, part) in parts.iter().enumerate() {
        if i == parts.len() - 1 {
            if let serde_json::Value::Object(map) = current {
                map.insert(part.to_string(), value);
            }
            return;
        }
        if let serde_json::Value::Object(map) = current {
            if !map.contains_key(*part) {
                map.insert(
                    part.to_string(),
                    serde_json::Value::Object(Default::default()),
                );
            }
            current = map.get_mut(*part).unwrap();
        } else {
            return;
        }
    }
}

/// Info needed to open a nested dialog (extracted before mutable borrow)
enum NestedDialogInfo {
    MapEntry {
        key: String,
        value: serde_json::Value,
        schema: SettingSchema,
        path: String,
        is_new: bool,
        no_delete: bool,
    },
    ArrayItem {
        index: Option<usize>,
        value: serde_json::Value,
        schema: SettingSchema,
        path: String,
        is_new: bool,
    },
}

/// A node of the dialog the focus fact can name — see
/// [`SettingsState::focus_on`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FocusTarget {
    Categories,
    /// A card of the current page, by item index.
    Card(usize),
    /// A footer button, by `shell::settings::Button::index`.
    Footer(usize),
}

/// Which panel currently has keyboard focus
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FocusPanel {
    /// Category list (left panel)
    #[default]
    Categories,
    /// Settings items (right panel)
    Settings,
    /// Footer buttons (Reset/Save/Cancel)
    Footer,
}

/// What the settings body's window is, read back from the tree after each
/// layout.
///
/// **Read, never computed.** `ScrollablePanel` kept these numbers by walking
/// `SettingItem::layout_box` over every item on the page — the same
/// arithmetic the painter used to draw the cards, in a second place, which is
/// exactly what goal 5 forbids. The cards are a `col` in a `viewport` now:
/// the column measures each of them once, and this is what it measured.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct BodyWindow {
    /// How far down the column the window starts.
    pub offset: u16,
    /// How tall the window is.
    pub height: u16,
    /// How tall the column is.
    pub content: u16,
    /// The card the window starts on.
    pub top_item: Option<usize>,
}

impl BodyWindow {
    /// The furthest the window can move before its bottom meets the column's.
    pub fn max_offset(&self) -> u16 {
        self.content.saturating_sub(self.height)
    }
}

/// The state of the settings UI
#[derive(Debug)]
pub struct SettingsState {
    /// Parsed schema categories
    categories: Vec<SettingCategory>,
    /// Pages built from categories
    pub pages: Vec<SettingsPage>,
    /// Currently selected category index
    pub selected_category: usize,
    /// Currently selected item index within the category
    pub selected_item: usize,
    /// Which panel has the keyboard.
    ///
    /// **One third of the dialog's focus fact**, with `selected_item` and
    /// `footer_button_index`; [`Self::focus_on`] is its one writer. The
    /// tree's ring is the fact's projection — the description marks the node
    /// the fact names, and a landing the tree makes on its own (Tab, a press)
    /// comes back through `focus_on` like every other decision.
    focus_panel: FocusPanel,
    /// Selected footer button index — `shell::settings::Button::index`.
    pub footer_button_index: usize,
    /// Pending changes (path -> new value)
    pub pending_changes: HashMap<String, serde_json::Value>,
    /// The original config value (for detecting changes)
    original_config: serde_json::Value,
    /// Whether the settings panel is visible
    pub visible: bool,
    /// The search filter's text field: the same editing engine every text
    /// surface runs on, so its caret and selection move as a field's do.
    pub search_input: crate::primitives::text_edit::TextEdit,
    /// Whether search is active
    pub search_active: bool,
    /// Current search results
    pub search_results: Vec<SearchResult>,
    /// Selected search result index
    pub selected_search_result: usize,
    /// Scroll offset for search results (first visible result index)
    pub search_scroll_offset: usize,
    /// Maximum number of visible search results (set during render)
    pub search_max_visible: usize,
    /// Whether the unsaved changes confirmation dialog is showing
    pub showing_confirm_dialog: bool,
    /// Selected option in confirmation dialog (0=Save, 1=Discard, 2=Cancel)
    pub confirm_dialog_selection: usize,
    /// Hovered option in confirmation dialog (for mouse hover feedback)
    pub confirm_dialog_hover: Option<usize>,
    /// Whether the reset confirmation dialog is showing
    pub showing_reset_dialog: bool,
    /// Selected option in reset dialog (0=Reset, 1=Cancel)
    pub reset_dialog_selection: usize,
    /// Hovered option in reset dialog (for mouse hover feedback)
    pub reset_dialog_hover: Option<usize>,
    /// Whether the "Discard changes?" prompt is showing over the
    /// currently-open entry dialog. Set when the user presses Esc
    /// on a dialog that has uncommitted edits; cleared by either
    /// button choice.
    pub showing_entry_discard_confirm: bool,
    /// Selection in the entry-discard prompt: 0 = Keep editing,
    /// 1 = Discard.
    pub entry_discard_confirm_selection: usize,
    /// Whether the "Delete <name>?" prompt is showing over the
    /// currently-open entry dialog. Set when the user activates
    /// the Delete button; deletion only fires once they confirm.
    pub showing_entry_delete_confirm: bool,
    /// Selection in the entry-delete prompt: 0 = Cancel (safe
    /// default), 1 = Delete.
    pub entry_delete_confirm_selection: usize,
    /// Key being deleted, displayed in the confirm prompt.
    pub entry_delete_target_name: String,
    /// True when the entry-delete prompt is targeting an array item.
    /// The confirm uses a generic "item" phrasing in this case since a
    /// numeric index in `entry_delete_target_name` would mean nothing
    /// to the user.
    pub entry_delete_target_is_array_item: bool,
    /// Whether the help overlay is showing
    pub showing_help: bool,
    /// What the body's window *is*, read back from the tree after each
    /// layout.
    ///
    /// **Read, never computed.** `ScrollablePanel` kept the same three numbers
    /// by re-deriving every item's height from `SettingItem::layout_box` —
    /// the second layout tree this migration exists to remove. The column in
    /// the `viewport` measures the cards once; these are what it measured.
    pub body: BodyWindow,
    /// The body window's handle.
    ///
    /// **The window is the tree's; this is how the keyboard reaches it.** The
    /// cards are a `col` in a `viewport`, so their heights are layout's answer
    /// and so is how far the window has moved. What the keyboard still owns is
    /// *which card* should be on screen — and that is one call
    /// (`Anchor::reveal_key`) rather than a second copy of every item's
    /// height, which is what `ScrollablePanel::ensure_focused_visible` needed.
    pub body_anchor: std::rc::Rc<fresh_ui::behavior::Anchor>,
    /// Current hover hit result (computed from hover_position and cached layout)
    pub hover_hit: Option<SettingsHit>,
    /// The open dropdown pop-over's hovered option, as a decimal index, or
    /// empty. The dialog renders its own controls with no panel behind them,
    /// so its pop-over's hover has nowhere else to live; see
    /// `UiFact::WidgetPopupHover`.
    pub hovered_popup_row: String,
    /// Stack of entry dialogs (for nested editing of Maps/ObjectArrays)
    /// The top of the stack (last element) is the currently active dialog.
    pub entry_dialog_stack: Vec<EntryDialogState>,
    /// Which configuration layer to save changes to.
    /// User layer is the default (global settings).
    /// Project layer saves to the current project's .fresh/config.json.
    pub target_layer: ConfigLayer,
    /// Snapshot of plugin-registered status-bar tokens (key → display title).
    /// Refreshed via `set_status_bar_tokens` when settings are opened.
    available_status_bar_tokens: HashMap<String, String>,
    /// Source layer for each setting path (where the value came from).
    /// Maps JSON pointer paths (e.g., "/editor/tab_size") to their source layer.
    /// Values not in this map come from system defaults.
    pub layer_sources: HashMap<String, ConfigLayer>,
    /// Paths to be removed from the current layer on save.
    /// When a user "resets" a setting, we remove it from the delta rather than
    /// setting it to the schema default.
    pub pending_deletions: std::collections::HashSet<String>,
    /// Visual style applied to every item in this state. Toggle with
    /// [`Self::set_item_style`] to swap between card / flat presentation.
    pub item_style: super::items::ItemBoxStyle,
    /// Categories whose sections are currently expanded in the left-panel
    /// tree view. Only categories with `sections.len() > 1` are eligible —
    /// a category with zero or one section stays flat.
    pub expanded_categories: std::collections::HashSet<usize>,
    /// Scroll state for the categories panel itself, separate from the body's
    /// window. Drives mouse-wheel + page-up/down on the left.
    pub categories_scroll: ScrollablePanel,
    /// Cursor position inside the currently-selected category's tree row.
    /// `None` = cursor is on the category row itself (the category row
    /// shows the `>` indicator).
    /// `Some(s)` = cursor is on the s-th section row of the current
    /// category (that section row shows the `>` indicator).
    ///
    /// Tracked explicitly so it is independent of the body's scroll
    /// position — pressing Right to expand a category keeps the cursor
    /// on the category, pressing Down then walks into the sections,
    /// and scrolling the body updates this so Up/Down resumes from the
    /// section the user is actually looking at.
    pub tree_cursor_section: Option<usize>,
    /// Whether the next body-window move is one the tree cursor asked for.
    ///
    /// **The tree follows the body only when the body moved on its own.**
    /// `sync_tree_cursor_to_body_scroll` exists so the highlight tracks the
    /// wheel and the scrollbar, but a cursor move scrolls the body too — and
    /// the window it reports lands a frame later, by which time the cursor has
    /// moved on. The sync then snapped the cursor back to the section under
    /// the *previous* key's scroll, so roughly every third Down did nothing.
    /// Set where the cursor drives the body, and taken by the frame that
    /// carries out that scroll.
    pub(crate) cursor_drove_body: bool,
    /// Snapshot of a plain `Text` control taken when its edit began, so Esc
    /// can revert an abandoned edit to exactly what it was. `None` whenever a
    /// Text edit is not in progress. See [`Self::start_editing`] /
    /// [`Self::revert_editing`].
    text_edit_snapshot: Option<TextEditSnapshot>,
    /// The page's controls as the widget kinds see them: the store the
    /// kinds read and write (a text field's editor, a number's draft, a
    /// dropdown's list), and `focus_key` — the path of the control that is
    /// *live*, holding the keyboard until it is left. Reset when the
    /// dialog opens; the description reads it every frame, so what the
    /// kinds decided is what is painted. See `view::settings::live`.
    pub controls: crate::widgets::WidgetPanelState,
    /// Live theme options `(display, value)` for the theme dropdown, sourced
    /// from the `ThemeRegistry` via [`Self::set_theme_options`] when Settings
    /// opens. Empty until set — the dropdown then simply lists nothing rather
    /// than a stale hand-maintained set. Resolves the `x-enum-from: "$themes"`
    /// schema hint so the Settings theme list matches "Select Theme" (#2738).
    theme_options: Vec<super::items::ThemeOption>,
}

/// What a `Text` setting said before its edit began. The kind applies what
/// is typed to the model as it goes, so a dialog title or a search can read
/// the field live; Enter and Tab record the result, and Escape puts this
/// back. Nothing else moves during the edit — the pending-change
/// bookkeeping is written once, when the edit is recorded.
#[derive(Debug)]
struct TextEditSnapshot {
    /// JSON pointer of the edited setting (`current_item().path`).
    path: String,
    value: String,
}

/// One row of the left-panel tree. Either a top-level category, or a section
/// row that appears under an expanded category.
///
/// Sections only appear when their owning category is in
/// `expanded_categories` AND has more than one section — single-section
/// categories show their items flat without a tree node.
#[derive(Debug, Clone, Copy)]
pub enum TreeRow {
    Category {
        idx: usize,
        expandable: bool,
        expanded: bool,
    },
    Section {
        cat_idx: usize,
        section_idx: usize,
    },
}

impl crate::view::ui::ScrollItem for TreeRow {
    fn height(&self, _width: u16) -> u16 {
        1
    }
}

impl SettingsState {
    /// Create a new settings state from schema and current config
    pub fn new(schema_json: &str, config: &Config) -> Result<Self, serde_json::Error> {
        Self::new_with_plugin_schemas(schema_json, config, &HashMap::new())
    }

    /// Same as [`Self::new`], plus inject per-plugin config schemas as
    /// subcategories of a "Plugin Settings" top-level category. Only
    /// enabled plugins with a schema are rendered.
    pub fn new_with_plugin_schemas(
        schema_json: &str,
        config: &Config,
        plugin_schemas: &HashMap<String, serde_json::Value>,
    ) -> Result<Self, serde_json::Error> {
        let mut categories = parse_schema(schema_json)?;

        // Collect enabled plugins that have a schema sidecar.
        let mut enabled_with_schema: Vec<String> = config
            .plugins
            .iter()
            .filter_map(|(name, cfg)| {
                if cfg.enabled && plugin_schemas.contains_key(name) {
                    Some(name.clone())
                } else {
                    None
                }
            })
            .collect();
        enabled_with_schema.sort();
        tracing::trace!(
            "SettingsState built: total plugin_schemas={}, enabled_with_schema={:?}",
            plugin_schemas.len(),
            enabled_with_schema
        );
        super::schema::append_plugin_settings_category(
            &mut categories,
            plugin_schemas,
            &enabled_with_schema,
        );

        let config_value = serde_json::to_value(config)?;
        let layer_sources = HashMap::new(); // Populated via set_layer_sources()
        let target_layer = ConfigLayer::User; // Default to user-global settings
        let available_status_bar_tokens: HashMap<String, String> = HashMap::new();
        let theme_options: Vec<super::items::ThemeOption> = Vec::new();
        let pages = super::items::build_pages(
            &categories,
            &config_value,
            &layer_sources,
            target_layer,
            &available_status_bar_tokens,
            &theme_options,
        );

        Ok(Self {
            categories,
            pages,
            selected_category: 0,
            selected_item: 0,
            focus_panel: FocusPanel::Categories,
            footer_button_index: 2, // Default to Save button (0=Layer, 1=Reset, 2=Save, 3=Cancel)
            pending_changes: HashMap::new(),
            original_config: config_value,
            visible: false,
            search_input: crate::primitives::text_edit::TextEdit::single_line(),
            search_active: false,
            search_results: Vec::new(),
            selected_search_result: 0,
            search_scroll_offset: 0,
            search_max_visible: 5, // Default, updated during render
            showing_confirm_dialog: false,
            confirm_dialog_selection: 0,
            confirm_dialog_hover: None,
            showing_reset_dialog: false,
            reset_dialog_selection: 0,
            reset_dialog_hover: None,
            showing_entry_discard_confirm: false,
            entry_discard_confirm_selection: 0,
            showing_entry_delete_confirm: false,
            entry_delete_confirm_selection: 0,
            entry_delete_target_name: String::new(),
            entry_delete_target_is_array_item: false,
            showing_help: false,
            body: BodyWindow::default(),
            body_anchor: fresh_ui::behavior::Anchor::new(),
            available_status_bar_tokens,
            hover_hit: None,
            hovered_popup_row: String::new(),
            entry_dialog_stack: Vec::new(),
            target_layer,
            layer_sources,
            pending_deletions: std::collections::HashSet::new(),
            item_style: super::items::ItemBoxStyle::default(),
            expanded_categories: std::collections::HashSet::new(),
            categories_scroll: ScrollablePanel::new(),
            tree_cursor_section: None,
            cursor_drove_body: false,
            text_edit_snapshot: None,
            controls: crate::widgets::WidgetPanelState::surface(fresh_core::api::WidgetSpec::Col {
                children: Vec::new(),
                key: None,
            }),
            theme_options,
        })
    }

    /// Get the currently focused panel
    #[inline]
    pub fn focus_panel(&self) -> FocusPanel {
        self.focus_panel
    }

    /// The node of the dialog the focus fact names.
    pub fn focus_target(&self) -> FocusTarget {
        match self.focus_panel {
            FocusPanel::Categories => FocusTarget::Categories,
            FocusPanel::Settings => FocusTarget::Card(self.selected_item),
            FocusPanel::Footer => FocusTarget::Footer(self.footer_button_index),
        }
    }

    /// **The one writer of the dialog's focus fact.** Every decider goes
    /// through here: the keys that move between the panels, a click on a
    /// card or a button, the tree's ring landing on a stop
    /// (`UiFact::SettingsFocus`), the category tree's jump to a section.
    ///
    /// Leaving the body drops the control's own focus state, entering it
    /// takes the control's (a Map's first entry, the `[Enter to edit]`
    /// hints); the selection is kept in range; and the window follows.
    pub fn focus_on(&mut self, target: FocusTarget) {
        // **Deciding what already holds is not a change.** A press on a row
        // inside a card names the card (`SettingsItem`) and the row
        // (`ControlMapRow`) alike, and the tree's landing echoes the card
        // again; only the first of those may re-seed the control's own
        // cursor, or the row the user clicked is lost to the card's first.
        if self.focus_target() == target {
            return;
        }
        let was = self.focus_panel;
        if was == FocusPanel::Settings {
            self.update_control_focus(false);
        }
        match target {
            FocusTarget::Categories => self.focus_panel = FocusPanel::Categories,
            FocusTarget::Card(i) => {
                self.focus_panel = FocusPanel::Settings;
                let n = self.current_page().map_or(0, |p| p.items.len());
                self.selected_item = match n {
                    0 => 0,
                    _ => i.min(n - 1),
                };
                self.enter_composite(true);
            }
            FocusTarget::Footer(i) => {
                self.focus_panel = FocusPanel::Footer;
                self.footer_button_index = i;
            }
        }
        self.ensure_visible();
    }

    /// Whether Nerd Font icons are enabled (`editor.nerd_font_icons`).
    ///
    /// Checks the unsaved pending value first so toggling the setting
    /// inside the Settings dialog previews immediately, then falls back
    /// to the value the dialog was opened with.
    pub fn nerd_font_icons_enabled(&self) -> bool {
        const PATH: &str = "/editor/nerd_font_icons";
        self.pending_changes
            .get(PATH)
            .or_else(|| self.original_config.pointer(PATH))
            .and_then(|v| v.as_bool())
            .unwrap_or(false)
    }

    /// Show the settings panel
    pub fn show(&mut self) {
        self.visible = true;
        self.focus_panel = FocusPanel::Categories;
        self.footer_button_index = 2; // Default to Save button (0=Layer, 1=Reset, 2=Save, 3=Cancel)
        self.selected_category = 0;
        self.selected_item = 0;
        self.body_anchor.scroll_to(fresh_ui::Point::ZERO);
        // Reset all dialog states so re-opening settings starts clean
        self.showing_confirm_dialog = false;
        self.confirm_dialog_selection = 0;
        self.confirm_dialog_hover = None;
        self.showing_reset_dialog = false;
        self.reset_dialog_selection = 0;
        self.reset_dialog_hover = None;
        self.showing_help = false;
        // The kinds' state is per-session; start clean.
        self.controls =
            crate::widgets::WidgetPanelState::surface(fresh_core::api::WidgetSpec::Col {
                children: Vec::new(),
                key: None,
            });
    }

    /// Rebuild pages with current state
    fn rebuild_pages(&mut self) {
        self.pages = super::items::build_pages(
            &self.categories,
            &self.original_config,
            &self.layer_sources,
            self.target_layer,
            &self.available_status_bar_tokens,
            &self.theme_options,
        );
    }

    /// Set the live theme options for the theme dropdown and rebuild pages.
    ///
    /// Called when Settings opens, from the editor which owns the
    /// `ThemeRegistry`. Pass `registry.settings_theme_options()` mapped into
    /// [`ThemeOption`](super::items::ThemeOption)s. This is the single source of
    /// truth shared with "Select Theme" (#2738).
    pub fn set_theme_options(&mut self, options: Vec<super::items::ThemeOption>) {
        self.theme_options = options;
        self.rebuild_pages();
    }

    fn paths_intersect(a: &str, b: &str) -> bool {
        if a.is_empty() || b.is_empty() {
            return a == b;
        }
        if a == b {
            return true;
        }
        let a_prefix = format!("{}/", a.trim_end_matches('/'));
        let b_prefix = format!("{}/", b.trim_end_matches('/'));
        a.starts_with(&b_prefix) || b.starts_with(&a_prefix)
    }

    /// True when this JSON pointer has an unsaved change in the current
    /// Settings session. This is intentionally separate from `item.modified`,
    /// which tracks whether a value is defined in the target config layer.
    pub fn path_has_pending_change(&self, path: &str) -> bool {
        self.pending_changes
            .keys()
            .any(|pending| Self::paths_intersect(path, pending))
            || self
                .pending_deletions
                .iter()
                .any(|pending| Self::paths_intersect(path, pending))
    }

    pub fn page_has_pending_changes(&self, page_idx: usize) -> bool {
        let Some(page) = self.pages.get(page_idx) else {
            return false;
        };
        (!page.path.is_empty() && self.path_has_pending_change(&page.path))
            || page
                .items
                .iter()
                .any(|item| self.path_has_pending_change(&item.path))
    }

    fn schema_default_for_path(&self, path: &str) -> Option<serde_json::Value> {
        self.pages
            .iter()
            .flat_map(|page| &page.items)
            .find(|item| item.path == path)
            .and_then(|item| item.default.clone())
    }

    /// Return the value this setting had when Settings was opened.
    ///
    /// Built-in settings are usually materialized in `original_config`.
    /// Plugin settings can exist only as schema defaults until the user saves
    /// an override, so the schema default is part of the original effective
    /// value for dirty-state comparisons.
    fn effective_original_value(&self, path: &str) -> Option<serde_json::Value> {
        self.original_config
            .pointer(path)
            .cloned()
            .or_else(|| self.schema_default_for_path(path))
    }

    fn value_matches_effective_original(&self, path: &str, value: &serde_json::Value) -> bool {
        self.effective_original_value(path).as_ref() == Some(value)
    }

    /// Hide the settings panel
    pub fn hide(&mut self) {
        self.visible = false;
        self.search_active = false;
        self.search_input.clear();
    }

    /// Get the current entry dialog (top of stack), if any
    pub fn entry_dialog(&self) -> Option<&EntryDialogState> {
        self.entry_dialog_stack.last()
    }

    /// Get the current entry dialog mutably (top of stack), if any
    pub fn entry_dialog_mut(&mut self) -> Option<&mut EntryDialogState> {
        self.entry_dialog_stack.last_mut()
    }

    /// Check if any entry dialog is open
    pub fn has_entry_dialog(&self) -> bool {
        !self.entry_dialog_stack.is_empty()
    }

    /// Get the currently selected page
    pub fn current_page(&self) -> Option<&SettingsPage> {
        self.pages.get(self.selected_category)
    }

    /// Get the currently selected page mutably
    pub fn current_page_mut(&mut self) -> Option<&mut SettingsPage> {
        self.pages.get_mut(self.selected_category)
    }

    /// Index of the item currently sitting at the top of the body window.
    /// The left-panel section indicator follows this so scrolling visibly
    /// moves the highlight in the tree, not just keyboard navigation.
    ///
    /// It used to be computed here, from the scroll offset and a walk of
    /// every item's `ScrollItem::height` — the same heights the painter
    /// planned each card with, kept in step by hand. The cards are laid out
    /// once now, and this is which one the window is showing.
    pub fn topmost_visible_item_index(&self) -> Option<usize> {
        self.body.top_item
    }

    /// Section currently displayed in the body — the section whose item
    /// range contains either the focused item or the topmost visible item
    /// (whichever is later). Returns `None` when the page has no sections
    /// or when the cursor is above the first section.
    pub fn current_section_index(&self) -> Option<usize> {
        let page = self.pages.get(self.selected_category)?;
        if page.sections.is_empty() {
            return None;
        }
        // Drive the section indicator off the topmost visible item — i.e.
        // strictly what the user is looking at right now, regardless of
        // where their last click landed. Earlier code did
        // `topmost.max(selected_item)`, which clamped the indicator so
        // wheel-UP after a click couldn't move the highlight back to an
        // earlier section. Falling back to selected_item only when the
        // body is genuinely empty (no items, no viewport) gives the
        // expected "tree follows scroll, both directions" behavior.
        let item_idx = self
            .topmost_visible_item_index()
            .unwrap_or(self.selected_item);
        // Walk sections in order and pick the last one whose first_item_index <= item_idx.
        let mut current: Option<usize> = None;
        for (s_idx, section) in page.sections.iter().enumerate() {
            if section.first_item_index <= item_idx {
                current = Some(s_idx);
            } else {
                break;
            }
        }
        current
    }

    /// Whether a category should render with a chevron + be expandable in
    /// the tree view. We require strictly more than one section, since one
    /// section adds no information beyond the category itself.
    pub fn is_category_expandable(&self, cat_idx: usize) -> bool {
        self.pages
            .get(cat_idx)
            .is_some_and(|p| p.sections.len() > 1)
    }

    /// Move the cursor in the categories tree by `delta` rows (positive =
    /// down, negative = up). The cursor walks every visible row — both
    /// category rows and the section rows under any expanded category — so
    /// users can step into discovered sections without leaving the keyboard.
    ///
    /// Maps the new row to state:
    /// * Category row → `selected_category = idx`, `selected_item = 0`.
    /// * Section row → category + first item of that section (same effect
    ///   as clicking the section).
    pub fn tree_step(&mut self, delta: i32) {
        let rows = self.visible_tree();
        if rows.is_empty() {
            return;
        }
        let cur = self.tree_cursor_index(&rows);
        let len = rows.len() as i32;
        let target = (cur as i32 + delta).clamp(0, len - 1) as usize;
        if target == cur {
            return;
        }
        let prev_category = self.selected_category;
        self.update_control_focus(false);
        match rows[target] {
            TreeRow::Category { idx, .. } => {
                // Cursor on a category row: body shows the page's first
                // item but the tree highlight stays on the category
                // header (no section is "current" yet).
                self.selected_category = idx;
                self.selected_item = 0;
                self.tree_cursor_section = None;
                if idx != prev_category {
                    self.body_anchor.scroll_to(fresh_ui::Point::ZERO);
                }
                self.update_control_focus(true);
            }
            TreeRow::Section {
                cat_idx,
                section_idx,
            } => {
                let first = self.pages[cat_idx].sections[section_idx].first_item_index;
                self.selected_category = cat_idx;
                self.selected_item = first;
                self.tree_cursor_section = Some(section_idx);
                if cat_idx != prev_category {
                    self.body_anchor.scroll_to(fresh_ui::Point::ZERO);
                }
                self.enter_composite(true);
                self.update_control_focus(true);
            }
        }
        // We deliberately do NOT auto-expand here: Up/Down sequential
        // navigation should walk through categories without unfolding
        // each one as you pass over it — that would balloon the tree
        // every time the user holds Down. Auto-expand fires on
        // deliberate visits (click, search-jump, Enter on a section).
        // When the cursor lands on a section, take the body to that
        // section's first item — same UX as clicking a section in the tree.
        // Merely *revealing* it would scroll just enough for it to be in
        // view, leaving `topmost_visible_item_index` on an earlier section
        // and making the cursor visually "stick" on the previous row.
        let key = super::super::shell::settings::card_key(self.selected_item);
        self.cursor_drove_body = true;
        match matches!(rows[target], TreeRow::Section { .. }) {
            true => self.body_anchor.top_key(key),
            false => self.body_anchor.reveal_key(key),
        }
        let new_rows = self.visible_tree();
        let new_cur = self.tree_cursor_index(&new_rows);
        // A tree row is one line tall whatever the width, so the tree's own
        // column is the honest number to measure it against.
        self.categories_scroll.ensure_focused_visible(
            &new_rows,
            new_cur,
            None,
            super::super::shell::settings::CATEGORY_COLS,
        );
    }

    /// Find the visible-tree index for the current selection. Prefers the
    /// section row matching the explicit `tree_cursor_section` cursor
    /// (when set), or the category row otherwise. The cursor is
    /// independent of body scroll position, so pressing Right to expand
    /// a category does NOT make the cursor jump to the first section.
    /// `Up`/`Down` walks the visible rows linearly; clicks and section
    /// jumps update `tree_cursor_section` directly.
    pub(super) fn tree_cursor_index(&self, rows: &[TreeRow]) -> usize {
        let cat = self.selected_category;
        if let Some(s_idx) = self.tree_cursor_section {
            for (i, row) in rows.iter().enumerate() {
                if let TreeRow::Section {
                    cat_idx,
                    section_idx,
                } = *row
                {
                    if cat_idx == cat && section_idx == s_idx {
                        return i;
                    }
                }
            }
        }
        for (i, row) in rows.iter().enumerate() {
            if let TreeRow::Category { idx, .. } = *row {
                if idx == cat {
                    return i;
                }
            }
        }
        0
    }

    /// Toggle whether a category is expanded in the tree view. No-op for
    /// categories that aren't expandable (zero or one section).
    /// Ensure the currently selected category is expanded in the tree view
    /// when it has more than one section. Called on any path that "visits"
    /// a category (Up/Down to it, click, search-jump) so the user
    /// immediately sees that the category contains sections — they don't
    /// have to remember to press Right.
    ///
    /// No-op for non-expandable categories (≤ 1 section). Idempotent.
    pub fn auto_expand_current_category(&mut self) {
        let idx = self.selected_category;
        if self.is_category_expandable(idx) {
            self.expanded_categories.insert(idx);
        }
    }

    pub fn toggle_category_expanded(&mut self, cat_idx: usize) {
        if !self.is_category_expandable(cat_idx) {
            return;
        }
        if !self.expanded_categories.insert(cat_idx) {
            self.expanded_categories.remove(&cat_idx);
        }
    }

    /// Jump the body panel to a specific section within a category. The
    /// category becomes the selected category, and the body's selected_item
    /// jumps to the section's first item.
    pub fn jump_to_section(&mut self, cat_idx: usize, section_idx: usize) {
        let Some(page) = self.pages.get(cat_idx) else {
            return;
        };
        let Some(section) = page.sections.get(section_idx) else {
            return;
        };
        let target_item = section.first_item_index;
        self.update_control_focus(false);
        self.selected_category = cat_idx;
        self.selected_item = target_item;
        self.tree_cursor_section = Some(section_idx);
        self.cursor_drove_body = true;
        self.focus_panel = FocusPanel::Settings;
        // Take the body to the top of the section. Revealing it would move
        // just enough to bring the target into view, which puts it at the
        // *bottom* of the window — and on a tight one clips its body below
        // the footer. Jumping to a section means "show this section".
        self.body_anchor
            .top_key(super::super::shell::settings::card_key(target_item));
        self.enter_composite(true);
        self.update_control_focus(true);
        self.auto_expand_current_category();
    }

    /// Flatten the categories list + currently expanded sections into the
    /// row order rendered in the left panel. Single source of truth for
    /// rendering, hit-testing, and Up/Down navigation in the tree.
    pub fn visible_tree(&self) -> Vec<TreeRow> {
        let mut rows = Vec::with_capacity(self.pages.len());
        for (idx, page) in self.pages.iter().enumerate() {
            let expandable = page.sections.len() > 1;
            let expanded = expandable && self.expanded_categories.contains(&idx);
            rows.push(TreeRow::Category {
                idx,
                expandable,
                expanded,
            });
            if expanded {
                for section_idx in 0..page.sections.len() {
                    rows.push(TreeRow::Section {
                        cat_idx: idx,
                        section_idx,
                    });
                }
            }
        }
        rows
    }

    /// Get the currently selected item
    pub fn current_item(&self) -> Option<&SettingItem> {
        self.current_page()
            .and_then(|page| page.items.get(self.selected_item))
    }

    /// Get the currently selected item mutably
    pub fn current_item_mut(&mut self) -> Option<&mut SettingItem> {
        self.pages
            .get_mut(self.selected_category)
            .and_then(|page| page.items.get_mut(self.selected_item))
    }

    /// Entering a card whose control's rows are a `List` — a map, an object
    /// array: the list takes the keyboard with its cursor on the first row
    /// when the card is entered from above, the last (the add row) from
    /// below. A text list's rows are fields, opened by Enter; a scalar has
    /// no rows.
    fn enter_composite(&mut self, from_above: bool) {
        let Some(item) = self.current_item() else {
            return;
        };
        if !item.control.has_list_rows() {
            return;
        }
        let n = item.control.list_row_count();
        if n == 0 {
            return;
        }
        let path = item.path.clone();
        live::seed_list(
            &mut self.controls,
            &path,
            if from_above { 0 } else { n - 1 },
        );
    }

    /// Leaving the selected card leaves its control: a live scalar is
    /// committed (a text field's or a number's draft) or closed (a
    /// dropdown's list), a list hands the keyboard back, before the
    /// selection moves on.
    pub(super) fn update_control_focus(&mut self, focused: bool) {
        if !focused {
            self.leave_live_control();
        }
    }

    /// Move selection up
    pub fn select_prev(&mut self) {
        match self.focus_panel() {
            FocusPanel::Categories => {
                self.tree_step(-1);
            }
            FocusPanel::Settings => {
                // A list's own rows are the kind's (`handle_live_control_key`);
                // this is the step off a card, entering the one above from
                // below.
                if self.selected_item > 0 {
                    self.update_control_focus(false);
                    self.selected_item -= 1;
                    self.enter_composite(false);
                }
                self.ensure_visible();
            }
            FocusPanel::Footer => {
                // Navigate between footer buttons (left)
                if self.footer_button_index > 0 {
                    self.footer_button_index -= 1;
                }
            }
        }
    }

    /// Move selection down
    pub fn select_next(&mut self) {
        match self.focus_panel() {
            FocusPanel::Categories => {
                self.tree_step(1);
            }
            FocusPanel::Settings => {
                let can_move = self
                    .current_page()
                    .is_some_and(|page| self.selected_item + 1 < page.items.len());
                if can_move {
                    self.update_control_focus(false);
                    self.selected_item += 1;
                    self.enter_composite(true);
                }
                self.ensure_visible();
            }
            FocusPanel::Footer => {
                // Navigate between footer buttons (right)
                if self.footer_button_index < 2 {
                    self.footer_button_index += 1;
                }
            }
        }
    }

    /// Move selection down by a page (viewport height worth of items)
    pub fn select_next_page(&mut self) {
        let page_size = self.body.height.max(1);
        for _ in 0..page_size {
            self.select_next();
        }
    }

    /// Move selection up by a page (viewport height worth of items)
    pub fn select_prev_page(&mut self) {
        let page_size = self.body.height.max(1);
        for _ in 0..page_size {
            self.select_prev();
        }
    }

    /// Toggle the visual style applied to every item.
    ///
    /// Style is cached per-item so the `ScrollItem::height(width)` trait impl
    /// can compute the correct height without taking a style parameter; this
    /// method propagates the change to every item across every page in one
    /// pass. Recomputes the scroll panel content height too, since heights
    /// just changed.
    pub fn set_item_style(&mut self, style: super::items::ItemBoxStyle) {
        if self.item_style == style {
            return;
        }
        self.item_style = style;
        for page in &mut self.pages {
            for item in &mut page.items {
                item.style = style;
            }
        }
    }

    /// Ensure the selected item is visible in the viewport.
    pub fn ensure_visible(&mut self) {
        if self.focus_panel() != FocusPanel::Settings {
            return;
        }

        // **One call, where a copy of every height used to be.**
        // `ensure_focused_visible` re-derived each item's rows to find where
        // the cursor's card started and how tall it was, and walked its
        // `focus_regions` again for the sub-row inside it. Both are bands the
        // layout already measured; the window is asked to hold the innermost
        // one that has a key.
        let key = self
            .focused_row_key()
            .unwrap_or_else(|| super::super::shell::settings::card_key(self.selected_item));
        self.body_anchor.reveal_key(key);
        // The tree cursor follows the body's window, and the window has not
        // moved yet — the request is applied between frames. It is synced
        // from what the window turns out to be, once per frame, rather than
        // guessed here.
    }

    /// Record a pending change for a setting
    pub fn set_pending_change(&mut self, path: &str, value: serde_json::Value) {
        let value = normalize_pending_change(path, value);
        if self.value_matches_effective_original(path, &value) {
            self.pending_changes.remove(path);
        } else {
            self.pending_changes.insert(path.to_string(), value);
        }
    }

    /// Check if there are unsaved changes
    pub fn has_changes(&self) -> bool {
        !self.pending_changes.is_empty() || !self.pending_deletions.is_empty()
    }

    /// Apply pending changes to a config
    pub fn apply_changes(&self, config: &Config) -> Result<Config, serde_json::Error> {
        let mut config_value = serde_json::to_value(config)?;

        // Process deletions first so a `pending_changes` write on the
        // same path (rare but possible) still wins.
        //
        // Writing `null` at a map-entry path would fail to round-trip
        // through the Config schema whenever the map's value type is
        // non-nullable (e.g. `HashMap<String, LspLanguageConfig>`) —
        // hence the dedicated removal path here, mirroring the
        // remove-then-write order used by `save_changes_to_layer` when
        // writing to disk.
        for path in &self.pending_deletions {
            crate::config_io::remove_json_pointer(&mut config_value, path);
        }

        for (path, value) in &self.pending_changes {
            let value = normalize_pending_change(path, value.clone());
            // `pointer_mut` only succeeds when the path already exists,
            // which is the common case (most settings are statically
            // declared in the Rust Config struct). For plugin settings
            // the user may be setting a brand-new leaf under
            // `/plugins/<name>/settings/...` — fall back to a
            // create-intermediate write so those land.
            if let Some(target) = config_value.pointer_mut(path) {
                *target = value;
            } else {
                set_json_pointer_create(&mut config_value, path, value);
            }
        }

        serde_json::from_value(config_value)
    }

    /// Discard all pending changes
    pub fn discard_changes(&mut self) {
        self.pending_changes.clear();
        self.pending_deletions.clear();
        // Rebuild pages from original config with layer info
        self.rebuild_pages();
    }

    /// Set the target layer for saving changes.
    pub fn set_target_layer(&mut self, layer: ConfigLayer) {
        if layer != ConfigLayer::System {
            // Cannot target System layer (read-only)
            self.target_layer = layer;
            // Clear pending changes when switching layers
            self.pending_changes.clear();
            self.pending_deletions.clear();
            // Rebuild pages with new target layer (affects "modified" indicators)
            self.rebuild_pages();
        }
    }

    /// Cycle through writable layers: User -> Project -> Session -> User
    pub fn cycle_target_layer(&mut self) {
        self.target_layer = match self.target_layer {
            ConfigLayer::System => ConfigLayer::User, // Should never be System, but handle it
            ConfigLayer::User => ConfigLayer::Project,
            ConfigLayer::Project => ConfigLayer::Session,
            ConfigLayer::Session => ConfigLayer::User,
        };
        // Clear pending changes when switching layers
        self.pending_changes.clear();
        self.pending_deletions.clear();
        // Rebuild pages with new target layer (affects "modified" indicators)
        self.rebuild_pages();
    }

    /// Get a display name for the current target layer.
    pub fn target_layer_name(&self) -> &'static str {
        match self.target_layer {
            ConfigLayer::System => "System (read-only)",
            ConfigLayer::User => "User",
            ConfigLayer::Project => "Project",
            ConfigLayer::Session => "Session",
        }
    }

    /// Set the layer sources map (called by Editor when opening settings).
    /// This also rebuilds pages to update modified indicators.
    pub fn set_layer_sources(&mut self, sources: HashMap<String, ConfigLayer>) {
        self.layer_sources = sources;
        // Rebuild pages with new layer sources (affects "modified" indicators)
        self.rebuild_pages();
    }

    /// Refresh the snapshot of plugin-registered status-bar tokens
    /// (called by Editor when opening settings).
    pub fn set_status_bar_tokens(&mut self, tokens: HashMap<String, String>) {
        self.available_status_bar_tokens = tokens;
        self.rebuild_pages();
    }

    /// Get the source layer for a setting path.
    /// Returns the layer where this value was defined, or System if it's the default.
    pub fn get_layer_source(&self, path: &str) -> ConfigLayer {
        self.layer_sources
            .get(path)
            .copied()
            .unwrap_or(ConfigLayer::System)
    }

    /// Get a short label for a layer source (for UI display).
    pub fn layer_source_label(layer: ConfigLayer) -> &'static str {
        match layer {
            ConfigLayer::System => "default",
            ConfigLayer::User => "user",
            ConfigLayer::Project => "project",
            ConfigLayer::Session => "session",
        }
    }

    /// Reset the current item by removing it from the target layer.
    ///
    /// NEW SEMANTICS: Instead of setting to schema default, we remove the value
    /// from the current layer's delta. The value then falls back to inherited
    /// (from lower-precedence layers) or to the schema default.
    ///
    /// Only items defined in the target layer can be reset.
    /// Reset the focused entry-dialog field to its schema default.
    ///
    /// Per-field counterpart of [`reset_current_to_default`], scoped to
    /// the active entry dialog rather than the main settings page. Skips
    /// read-only / non-modified / no-default fields. The dialog itself
    /// becomes dirty (user_edited = true) so the title flips to
    /// `• modified`, signalling that the parent still owes a save.
    /// If keyboard focus is on a field's per-field action button
    /// (`[Reset]`/`[Inherit]`/`[Clear]`), perform it and return true (so
    /// Enter/Space consume the key). These buttons are reached by Tab.
    pub fn entry_dialog_activate_focused_field_button(&mut self) -> bool {
        match self.entry_dialog_mut() {
            Some(dialog) => dialog.activate_focused_field_button(),
            None => false,
        }
    }

    pub fn reset_current_to_default(&mut self) {
        // Get the info we need first, then release the borrow
        let reset_info = self.current_item().and_then(|item| {
            // Only allow reset if the item is defined in the target layer
            // (i.e., if it's "modified" in the new semantics)
            if !item.modified || item.is_auto_managed {
                return None;
            }
            item.default
                .as_ref()
                .map(|default| (item.path.clone(), default.clone()))
        });

        if let Some((path, default)) = reset_info {
            let original_source = self.get_layer_source(&path);

            if original_source != self.target_layer {
                // The row is only modified because of an unsaved pending edit.
                // Reset should cancel that edit and return to the inherited
                // resolved value, not record a no-op deletion against a layer
                // that did not define the setting in the first place.
                self.pending_changes.remove(&path);
                self.pending_deletions.remove(&path);
                let original = self.effective_original_value(&path).unwrap_or(default);
                if let Some(item) = self.current_item_mut() {
                    update_control_from_value(&mut item.control, &original);
                    item.modified = false;
                    item.layer_source = original_source;
                    item.is_null = item.nullable && original.is_null();
                }
                return;
            }

            // Mark this path for deletion from the target layer
            self.pending_deletions.insert(path.clone());
            // Remove any pending change for this path
            self.pending_changes.remove(&path);

            // Update the control state to show the inherited value.
            // Since we don't have access to other layers' values here,
            // we use the schema default as the fallback display value.
            if let Some(item) = self.current_item_mut() {
                update_control_from_value(&mut item.control, &default);
                item.modified = false;
                // Update layer source to show where value now comes from
                item.layer_source = ConfigLayer::System; // Falls back to default
            }
        }
    }

    /// Set the current nullable setting to null (inherit value).
    ///
    /// This explicitly sets the value to null in the current layer,
    /// indicating that the setting should be inherited rather than overridden.
    /// Only applies to nullable settings that are not currently null.
    pub fn set_current_to_null(&mut self) {
        let target_layer = self.target_layer;
        let change_info = self.current_item().and_then(|item| {
            if !item.nullable || item.is_null || item.read_only {
                return None;
            }
            Some(item.path.clone())
        });

        if let Some(path) = change_info {
            // Set value to null (not a deletion — this is an explicit null value)
            self.pending_changes
                .insert(path.clone(), serde_json::Value::Null);
            self.pending_deletions.remove(&path);

            // Update the item's visual state
            if let Some(item) = self.current_item_mut() {
                item.is_null = true;
                item.modified = true;
                item.layer_source = target_layer;
            }
        }
    }

    /// Clear a nullable category by setting its path to null and updating all items.
    ///
    /// This sets the category's root path (e.g., `/fallback`) to null in the target layer,
    /// effectively removing the entire section. All items within the category are marked
    /// as null/inherited.
    pub fn clear_current_category(&mut self) {
        let target_layer = self.target_layer;
        let page = match self.current_page() {
            Some(p) if p.nullable => p,
            _ => return,
        };
        let page_path = page.path.clone();

        // Set the category root to null
        self.pending_changes
            .insert(page_path.clone(), serde_json::Value::Null);

        // Also remove any pending changes/deletions for child paths
        let prefix = format!("{}/", page_path);
        self.pending_changes
            .retain(|path, _| !path.starts_with(&prefix));
        self.pending_deletions
            .retain(|path| !path.starts_with(&prefix));

        // Update all items on the current page to reflect null/inherited state
        if let Some(page) = self.current_page_mut() {
            for item in &mut page.items {
                if item.nullable {
                    item.is_null = true;
                    item.modified = false;
                    item.layer_source = target_layer;
                }
            }
        }
    }

    /// Check if any items in the current nullable category have non-null values.
    pub fn current_category_has_values(&self) -> bool {
        match self.current_page() {
            Some(page) if page.nullable => {
                page.items.iter().any(|item| !item.is_null && item.nullable)
                    || page.items.iter().any(|item| item.modified)
            }
            _ => false,
        }
    }

    /// Handle a value change from user interaction
    pub fn on_value_changed(&mut self) {
        // Capture target_layer before any borrows
        let target_layer = self.target_layer;

        // Get value and path first, then release borrow
        let change_info = self.current_item().map(|item| {
            let value = control_to_value(&item.control);
            (item.path.clone(), value)
        });

        if let Some((path, value)) = change_info {
            let original_value = self.effective_original_value(&path);
            let matches_original = original_value.as_ref() == Some(&value);
            let original_source = self.get_layer_source(&path);

            // When user changes a value, it becomes "modified" (defined in target layer)
            // Remove from pending deletions if it was scheduled for removal
            self.pending_deletions.remove(&path);
            self.set_pending_change(&path, value);

            // Update the item's state
            if let Some(item) = self.current_item_mut() {
                if matches_original {
                    item.modified = !item.is_auto_managed && original_source == target_layer;
                    item.layer_source = original_source;
                    item.is_null = item.nullable
                        && original_value
                            .as_ref()
                            .map(|v| v.is_null())
                            .unwrap_or_else(|| {
                                item.default.as_ref().map(|d| d.is_null()).unwrap_or(true)
                            });
                } else {
                    item.modified = true; // New semantic: value is now defined in target layer
                    item.layer_source = target_layer; // Value now comes from target layer
                    item.is_null = false; // Explicit value clears the inherited state
                }
            }
        }
    }

    /// The current search filter text.
    pub fn search_query(&self) -> &str {
        self.search_input.current_line()
    }

    /// The search caret position, as a byte offset into
    /// [`SettingsState::search_query`] (always on a grapheme boundary).
    pub fn search_cursor(&self) -> usize {
        self.search_input.flat_cursor_byte()
    }

    /// Start search mode
    pub fn start_search(&mut self) {
        self.search_active = true;
        self.search_input.clear();
        self.search_results.clear();
        self.selected_search_result = 0;
        self.search_scroll_offset = 0;
    }

    /// Cancel search mode
    pub fn cancel_search(&mut self) {
        self.search_active = false;
        self.search_input.clear();
        self.search_results.clear();
        self.selected_search_result = 0;
        self.search_scroll_offset = 0;
    }

    /// Update search query and refresh results
    pub fn set_search_query(&mut self, query: String) {
        self.search_input.set_value(&query);
        self.search_input.move_end();
        self.refresh_search_results();
    }

    /// Recompute results after the query text changed and reset the
    /// results selection/scroll to the top.
    fn refresh_search_results(&mut self) {
        self.search_results = search_settings(&self.pages, &self.search_input.value());
        self.selected_search_result = 0;
        self.search_scroll_offset = 0;
    }

    /// Insert a character at the cursor position in the search query.
    pub fn search_insert_char(&mut self, c: char) {
        self.search_input.insert_char(c);
        self.refresh_search_results();
    }

    /// Add a character to the search query.
    ///
    /// Kept for backwards compatibility; delegates to `search_insert_char`.
    pub fn search_push_char(&mut self, c: char) {
        self.search_insert_char(c);
    }

    /// Delete the grapheme cluster before the cursor (Backspace).
    pub fn search_backspace(&mut self) {
        self.search_input.backspace();
        self.refresh_search_results();
    }

    /// Remove the grapheme cluster before the cursor.
    ///
    /// Kept for backwards compatibility; delegates to `search_backspace`.
    pub fn search_pop_char(&mut self) {
        self.search_backspace();
    }

    /// Delete the grapheme cluster at the cursor (Delete key).
    pub fn search_delete(&mut self) {
        self.search_input.delete();
        self.refresh_search_results();
    }

    /// Move the search cursor left by one grapheme cluster.
    ///
    /// Movement is grapheme-aware (via the shared control) so combining
    /// marks — Thai diacritics, emoji modifiers — move as a single unit,
    /// matching the Command Palette.
    pub fn search_cursor_left(&mut self) {
        self.search_input.move_left();
    }

    /// Move the search cursor right by one grapheme cluster.
    pub fn search_cursor_right(&mut self) {
        self.search_input.move_right();
    }

    /// Move the search cursor to the start of the query.
    pub fn search_cursor_home(&mut self) {
        self.search_input.move_home();
    }

    /// Move the search cursor to the end of the query.
    pub fn search_cursor_end(&mut self) {
        self.search_input.move_end();
    }

    /// Navigate to previous search result.
    ///
    /// **The window follows the selection on its own.** The results are a
    /// `List`, and a list moves its window to whichever row it is told is
    /// selected — so `ensure_search_selection_visible`, which nudged
    /// `search_scroll_offset` by `max_visible` here, went with the offset it
    /// was keeping. The offset that is left is read back off the window for
    /// the count row to report.
    pub fn search_prev(&mut self) {
        if !self.search_results.is_empty() && self.selected_search_result > 0 {
            self.selected_search_result -= 1;
        }
    }

    /// Navigate to next search result
    pub fn search_next(&mut self) {
        if !self.search_results.is_empty()
            && self.selected_search_result + 1 < self.search_results.len()
        {
            self.selected_search_result += 1;
        }
    }

    // **The results' window is the list's own.** A wheel notch, a press on
    // its bar and a drag along it are the `viewport`'s, and each of the three
    // had a method here that moved `search_scroll_offset` by a delta or a
    // ratio against a content height computed a second time from
    // `len - max_visible`.

    /// Jump to the currently selected search result
    pub fn jump_to_search_result(&mut self) {
        // Extract values first to avoid borrow issues
        let Some(result) = self
            .search_results
            .get(self.selected_search_result)
            .cloned()
        else {
            return;
        };
        let page_index = result.page_index;
        let item_index = result.item_index;

        // Unfocus old item first
        self.update_control_focus(false);
        self.selected_category = page_index;
        self.selected_item = item_index;
        self.focus_panel = FocusPanel::Settings;
        // Reset scroll offset but preserve viewport for ensure_visible
        self.body_anchor.scroll_to(fresh_ui::Point::ZERO);
        self.enter_composite(true);

        // Navigate into the deep match target if present
        if let Some(ref deep_match) = result.deep_match {
            self.jump_to_deep_match(deep_match);
        }

        self.auto_expand_current_category();
        // Whichever section the matched item lives in becomes the tree
        // cursor — so when the user closes search and Tabs to the
        // categories panel, Up/Down resumes from the right place.
        self.tree_cursor_section = self.current_section_index();
        self.ensure_visible();
        self.cancel_search();
    }

    /// Navigate into a composite control to focus a specific deep match:
    /// a map's list cursor goes to the entry, a text list's item opens.
    fn jump_to_deep_match(&mut self, deep_match: &DeepMatch) {
        match deep_match {
            DeepMatch::MapKey { entry_index, .. } | DeepMatch::MapValue { entry_index, .. } => {
                self.select_list_row(*entry_index)
            }
            DeepMatch::TextListItem { item_index, .. } => self.edit_list_row(Some(*item_index)),
        }
    }

    /// Get the currently selected search result
    pub fn current_search_result(&self) -> Option<&SearchResult> {
        self.search_results.get(self.selected_search_result)
    }

    /// Show the unsaved changes confirmation dialog
    pub fn show_confirm_dialog(&mut self) {
        self.showing_confirm_dialog = true;
        self.confirm_dialog_selection = 0; // Default to "Save and Exit"
    }

    /// Hide the confirmation dialog
    pub fn hide_confirm_dialog(&mut self) {
        self.showing_confirm_dialog = false;
        self.confirm_dialog_selection = 0;
    }

    /// Move to next option in confirmation dialog
    pub fn confirm_dialog_next(&mut self) {
        self.confirm_dialog_selection = (self.confirm_dialog_selection + 1) % 3;
    }

    /// Move to previous option in confirmation dialog
    pub fn confirm_dialog_prev(&mut self) {
        self.confirm_dialog_selection = if self.confirm_dialog_selection == 0 {
            2
        } else {
            self.confirm_dialog_selection - 1
        };
    }

    /// Toggle the help overlay
    pub fn toggle_help(&mut self) {
        self.showing_help = !self.showing_help;
    }

    /// Hide the help overlay
    pub fn hide_help(&mut self) {
        self.showing_help = false;
    }

    /// Check if the entry dialog is showing
    pub fn showing_entry_dialog(&self) -> bool {
        self.has_entry_dialog()
    }

    /// Open the entry dialog for the map entry at `entry_idx`.
    pub fn open_entry_dialog(&mut self, entry_idx: usize) {
        let Some(item) = self.current_item() else {
            return;
        };

        // Determine what type of entry we're editing based on the path
        let path = item.path.as_str();
        let SettingControl::Map {
            entries,
            value_schema,
            no_add,
            ..
        } = &item.control
        else {
            return;
        };

        let Some((key, value)) = entries.get(entry_idx) else {
            return;
        };

        // Get the value schema for this map
        let Some(schema) = value_schema.as_ref() else {
            return; // No schema available, can't create dialog
        };

        // If the map doesn't allow adding, it also doesn't allow deleting (auto-managed entries)
        let no_delete = *no_add;

        // Per-field [Reset] targets must be this entry's *built-in* values
        // (e.g. `languages.html.grammar = "HTML"`), not the generic schema
        // default for the field type (`""`). Look the entry up in the bundled
        // default config so Reset restores the right value.
        let entry_pointer = format!("{}/{}", path, key);
        let key = key.clone();
        let value = value.clone();

        // Create dialog from schema
        let mut dialog = EntryDialogState::from_schema(
            key,
            &value,
            schema,
            path,
            false,
            no_delete,
            &self.available_status_bar_tokens,
        );
        apply_builtin_defaults(&mut dialog, &entry_pointer);
        dialog.inheritable_fields = inheritable_fields_for(path);
        self.entry_dialog_stack.push(dialog);
    }

    /// Open entry dialog for adding a new entry (with empty key)
    pub fn open_add_entry_dialog(&mut self) {
        let Some(item) = self.current_item() else {
            return;
        };
        let SettingControl::Map { value_schema, .. } = &item.control else {
            return;
        };
        let Some(schema) = value_schema.as_ref() else {
            return;
        };
        let path = item.path.clone();

        // Create dialog with empty key - user will fill it in
        // no_delete is false for new entries (Delete button is not shown anyway for new entries)
        let dialog = EntryDialogState::from_schema(
            String::new(),
            &serde_json::json!({}),
            schema,
            &path,
            true,
            false,
            &self.available_status_bar_tokens,
        );
        self.entry_dialog_stack.push(dialog);
    }

    /// Open dialog for adding a new array item
    pub fn open_add_array_item_dialog(&mut self) {
        let Some(item) = self.current_item() else {
            return;
        };
        let SettingControl::ObjectArray { item_schema, .. } = &item.control else {
            return;
        };
        let Some(schema) = item_schema.as_ref() else {
            return;
        };
        let path = item.path.clone();

        // Create dialog with empty value - user will fill it in
        let dialog = EntryDialogState::for_array_item(
            None,
            &serde_json::json!({}),
            schema,
            &path,
            true,
            &self.available_status_bar_tokens,
        );
        self.entry_dialog_stack.push(dialog);
    }

    /// Open dialog for editing the array item at `index`.
    pub fn open_edit_array_item_dialog(&mut self, index: usize) {
        let Some(item) = self.current_item() else {
            return;
        };
        let SettingControl::ObjectArray {
            items, item_schema, ..
        } = &item.control
        else {
            return;
        };
        let Some(schema) = item_schema.as_ref() else {
            return;
        };
        let Some(value) = items.get(index) else {
            return;
        };
        let path = item.path.clone();

        let dialog = EntryDialogState::for_array_item(
            Some(index),
            value,
            schema,
            &path,
            false,
            &self.available_status_bar_tokens,
        );
        self.entry_dialog_stack.push(dialog);
    }

    /// Close the entry dialog without saving (pops from stack)
    pub fn close_entry_dialog(&mut self) {
        self.entry_dialog_stack.pop();
    }

    /// Open a nested entry dialog for a Map or ObjectArray field within the current dialog
    ///
    /// This enables recursive editing: if a dialog field is itself a Map or ObjectArray,
    /// pressing Enter will open a new dialog on top of the stack for that nested structure.
    pub fn open_nested_entry_dialog(&mut self) {
        // Get info from the current dialog's focused field
        let nested_info = self.entry_dialog().and_then(|dialog| {
            let item = dialog.current_item()?;
            // The nested dialog path must root at the current entry's full
            // path, not just at `map_path`. Otherwise the entry key segment
            // (e.g. `quicklsp` under `/universal_lsp`) is dropped and the
            // nested save records a pending change at `/universal_lsp/`,
            // which eventually writes an empty-string key into the config.
            let base = dialog.entry_path();
            let relative = item.path.trim_start_matches('/');
            let path = if relative.is_empty() {
                // `is_single_value` dialogs' one item is the entry's value
                // itself, at `SINGLE_VALUE_PATH` (`/`, nothing to join), so
                // the nested dialog lives at the entry path itself.
                base
            } else {
                format!("{}/{}", base, relative)
            };

            // The row the dialog's cursor is on: an entry, or the add row.
            let cursor = dialog.composite_cursor();
            match &item.control {
                SettingControl::Map {
                    entries,
                    value_schema,
                    no_add,
                    ..
                } => {
                    let schema = value_schema.as_ref()?;
                    let no_delete = *no_add; // If can't add, can't delete either
                    if let Some((key, value)) = cursor.and_then(|i| entries.get(i)) {
                        // Edit existing entry
                        Some(NestedDialogInfo::MapEntry {
                            key: key.clone(),
                            value: value.clone(),
                            schema: schema.as_ref().clone(),
                            path,
                            is_new: false,
                            no_delete,
                        })
                    } else {
                        // Add new entry
                        Some(NestedDialogInfo::MapEntry {
                            key: String::new(),
                            value: serde_json::json!({}),
                            schema: schema.as_ref().clone(),
                            path,
                            is_new: true,
                            no_delete: false, // New entries don't show Delete anyway
                        })
                    }
                }
                SettingControl::ObjectArray {
                    items, item_schema, ..
                } => {
                    let schema = item_schema.as_ref()?;
                    if let Some((index, value)) = cursor.and_then(|i| Some((i, items.get(i)?))) {
                        // Edit existing item
                        Some(NestedDialogInfo::ArrayItem {
                            index: Some(index),
                            value: value.clone(),
                            schema: schema.as_ref().clone(),
                            path,
                            is_new: false,
                        })
                    } else {
                        // Add new item
                        Some(NestedDialogInfo::ArrayItem {
                            index: None,
                            value: serde_json::json!({}),
                            schema: schema.as_ref().clone(),
                            path,
                            is_new: true,
                        })
                    }
                }
                _ => None,
            }
        });

        // Now create and push the dialog (outside the borrow)
        if let Some(info) = nested_info {
            let dialog = match info {
                NestedDialogInfo::MapEntry {
                    key,
                    value,
                    schema,
                    path,
                    is_new,
                    no_delete,
                } => EntryDialogState::from_schema(
                    key,
                    &value,
                    &schema,
                    &path,
                    is_new,
                    no_delete,
                    &self.available_status_bar_tokens,
                ),
                NestedDialogInfo::ArrayItem {
                    index,
                    value,
                    schema,
                    path,
                    is_new,
                } => EntryDialogState::for_array_item(
                    index,
                    &value,
                    &schema,
                    &path,
                    is_new,
                    &self.available_status_bar_tokens,
                ),
            };
            self.entry_dialog_stack.push(dialog);
        }
    }

    /// Save the entry dialog and apply changes
    ///
    /// Automatically detects whether this is a Map or ObjectArray dialog
    /// and handles saving appropriately.
    pub fn save_entry_dialog(&mut self) {
        // Determine if this is an array dialog by checking where we need to save
        // For nested dialogs (stack len > 1), check the parent dialog's item type
        // For top-level dialogs (stack len == 1), check current_item()
        let is_array = if self.entry_dialog_stack.len() > 1 {
            // Nested dialog - check parent dialog's focused item
            self.entry_dialog_stack
                .get(self.entry_dialog_stack.len() - 2)
                .and_then(|parent| parent.current_item())
                .map(|item| matches!(item.control, SettingControl::ObjectArray { .. }))
                .unwrap_or(false)
        } else {
            // Top-level dialog - check main settings page item
            self.current_item()
                .map(|item| matches!(item.control, SettingControl::ObjectArray { .. }))
                .unwrap_or(false)
        };

        if is_array {
            self.save_array_item_dialog_inner();
        } else {
            self.save_map_entry_dialog_inner();
        }
    }

    /// Save a Map entry dialog
    fn save_map_entry_dialog_inner(&mut self) {
        let Some(mut dialog) = self.entry_dialog_stack.pop() else {
            return;
        };
        // Treat any draft text in a TextList's `[+] Add new` slot as
        // committed (F21). Otherwise typing an item and hitting Ctrl+S
        // without a separate Enter silently dropped the text.
        dialog.commit_pending_list_drafts();

        // Get key from the dialog's key field (may have been edited)
        let key = dialog.get_key();
        if key.is_empty() {
            return; // Can't save with empty key
        }

        let value = dialog.to_value();
        let map_path = dialog.map_path.clone();
        let original_key = dialog.entry_key.clone();
        let is_new = dialog.is_new;
        let key_changed = !is_new && key != original_key;

        // Update the map control with the new value
        if let Some(item) = self.current_item_mut() {
            if let SettingControl::Map { entries, .. } = &mut item.control {
                // If key was changed, remove old entry first
                if key_changed {
                    if let Some(idx) = entries.iter().position(|(k, _)| k == &original_key) {
                        entries.remove(idx);
                    }
                }
                // Find or add the entry with the (possibly new) key
                super::items::map_set(entries, key.clone(), value.clone());
            }
        }

        // Record deletion of old key if key was changed
        if key_changed {
            let old_path = format!("{}/{}", map_path, original_key);
            self.pending_changes
                .insert(old_path, serde_json::Value::Null);
        }

        // Record the pending change
        let path = format!("{}/{}", map_path, key);
        self.set_pending_change(&path, value);
    }

    /// Save an ObjectArray item dialog
    fn save_array_item_dialog_inner(&mut self) {
        let Some(mut dialog) = self.entry_dialog_stack.pop() else {
            return;
        };
        // Commit any pending TextList draft (F21).
        dialog.commit_pending_list_drafts();

        let value = dialog.to_value();
        let array_path = dialog.map_path.clone();
        let is_new = dialog.is_new;
        let entry_key = dialog.entry_key.clone();

        // Determine if this is a nested dialog (parent still in stack)
        let is_nested = !self.entry_dialog_stack.is_empty();

        if is_nested {
            // Nested dialog - update the parent dialog's ObjectArray item.
            // Extract the item path within the parent dialog by stripping the
            // parent's full entry path (map_path + "/" + entry_key) from the
            // nested dialog's array path. For an is_single_value parent (e.g.
            // a quicklsp entry whose value schema is an array), the inner
            // ObjectArray item is the entry's value at `SINGLE_VALUE_PATH`
            // and the nested dialog lives exactly at the entry path, so the
            // stripped path is empty and names that item.
            let parent_entry_path = self
                .entry_dialog_stack
                .last()
                .map(|p| p.entry_path())
                .unwrap_or_default();
            let item_path = match array_path
                .strip_prefix(parent_entry_path.as_str())
                .unwrap_or(&array_path)
                .trim_end_matches('/')
            {
                "" => super::entry_dialog::SINGLE_VALUE_PATH.to_string(),
                rest => rest.to_string(),
            };

            // Find and update the ObjectArray in the parent dialog. Mark
            // the parent dirty so its title flips to `• modified` —
            // without this, a Ctrl+S in the inner dialog quietly mutated
            // the parent and the user had to guess whether they still
            // owed another save.
            if let Some(parent) = self.entry_dialog_stack.last_mut() {
                if let Some(item) = parent.items.iter_mut().find(|i| i.path == item_path) {
                    if let SettingControl::ObjectArray { items, .. } = &mut item.control {
                        if is_new {
                            items.push(value.clone());
                        } else if let Ok(index) = entry_key.parse::<usize>() {
                            if index < items.len() {
                                items[index] = value.clone();
                            }
                        }
                        parent.user_edited = true;
                    }
                }
            }

            // For nested arrays, the pending change will be recorded when parent dialog saves
            // We still record a pending change so the value persists
            if let Some(parent) = self.entry_dialog_stack.last() {
                if let Some(item) = parent.items.iter().find(|i| i.path == item_path) {
                    if let SettingControl::ObjectArray { items, .. } = &item.control {
                        let array_value = serde_json::Value::Array(items.clone());
                        self.set_pending_change(&array_path, array_value);
                    }
                }
            }
        } else {
            // Top-level dialog - update the main settings page item
            if let Some(item) = self.current_item_mut() {
                if let SettingControl::ObjectArray { items, .. } = &mut item.control {
                    if is_new {
                        items.push(value.clone());
                    } else if let Ok(index) = entry_key.parse::<usize>() {
                        if index < items.len() {
                            items[index] = value.clone();
                        }
                    }
                }
            }

            // Record the pending change for the entire array
            if let Some(item) = self.current_item() {
                if let SettingControl::ObjectArray { items, .. } = &item.control {
                    let array_value = serde_json::Value::Array(items.clone());
                    self.set_pending_change(&array_path, array_value);
                }
            }
        }
    }

    /// Delete the entry from the map and close the dialog
    /// Pop the "Delete <name>?" confirmation prompt. The actual
    /// delete only fires once the user confirms via the prompt.
    /// Cancel (selection 0) is the safe default, so a misplaced
    /// Tab+Enter on the Delete button no longer destroys data.
    pub fn request_entry_delete_confirm(&mut self) {
        let (name, is_array_item) = self
            .entry_dialog()
            .map(|d| (d.entry_key.clone(), d.is_array_item))
            .unwrap_or_default();
        // For array items the entry_key is a numeric index — meaningless
        // to the user. Drop it and let the confirm render fall back to
        // the generic "item" phrasing.
        self.entry_delete_target_name = if is_array_item { String::new() } else { name };
        self.entry_delete_target_is_array_item = is_array_item;
        self.entry_delete_confirm_selection = 0;
        self.showing_entry_delete_confirm = true;
    }

    pub fn delete_entry_dialog(&mut self) {
        // Check if this is a nested dialog BEFORE popping
        let is_nested = self.entry_dialog_stack.len() > 1;

        let Some(dialog) = self.entry_dialog_stack.pop() else {
            return;
        };

        let path = format!("{}/{}", dialog.map_path, dialog.entry_key);

        // Remove from the map control
        if is_nested {
            // Nested dialog - update the parent dialog's Map item
            // Extract the map field name from the path (last segment of map_path)
            let map_field = dialog.map_path.rsplit('/').next().unwrap_or("").to_string();
            let item_path = format!("/{}", map_field);

            // Find and update the Map in the parent dialog
            if let Some(parent) = self.entry_dialog_stack.last_mut() {
                if let Some(item) = parent.items.iter_mut().find(|i| i.path == item_path) {
                    if let SettingControl::Map { entries, .. } = &mut item.control {
                        entries.retain(|(k, _)| k != &dialog.entry_key);
                    }
                }
            }
        } else {
            // Top-level dialog - remove from the main settings page item
            if let Some(item) = self.current_item_mut() {
                if let SettingControl::Map { entries, .. } = &mut item.control {
                    entries.retain(|(k, _)| k != &dialog.entry_key);
                }
            }
        }

        // Record the deletion. Earlier this wrote `null` via
        // `set_pending_change`, but that round-trips through the Config
        // schema as `<map>/<key> = null` — invalid whenever the map's
        // value type is non-nullable. Routing through `pending_deletions`
        // both removes the key cleanly from the in-memory Config (via
        // `apply_changes`) and writes the removal to disk (via
        // `save_changes_to_layer`'s `remove_json_pointer` step).
        self.pending_changes.remove(&path);
        self.pending_deletions.insert(path);
    }

    /// After the body scroll position changes, snap the tree cursor to
    /// the section that now contains the topmost visible item — so the
    /// left-panel highlight follows wheel/scrollbar interaction in both
    /// directions, and a subsequent Up/Down on the tree resumes from
    /// the section the user is actually looking at.
    /// Whether the body move now being reported is the cursor's own.
    ///
    /// One-shot: each cursor move sets the flag and the frame that carries
    /// out its scroll takes it, so a later wheel or scrollbar drag still
    /// moves the highlight.
    pub(crate) fn take_cursor_drove_body(&mut self) -> bool {
        std::mem::take(&mut self.cursor_drove_body)
    }

    pub(crate) fn sync_tree_cursor_to_body_scroll(&mut self) {
        if let Some(section_idx) = self.current_section_index() {
            self.tree_cursor_section = Some(section_idx);
        }
        // No section under the topmost visible item (e.g. above the
        // first section) → leave the cursor where it is. Forcing it to
        // None would be a worse UX: the user typically wants the
        // highlight to *track* something, not blink off entirely.
    }

    /// Start text editing mode for TextList, Text, or Map controls
    /// Check if the current control is a number input
    pub fn is_number_control(&self) -> bool {
        self.current_item()
            .is_some_and(|item| matches!(item.control, SettingControl::Number { .. }))
    }

    /// Enter on the selected card, or a press that means the same: the
    /// control is edited from here on. A composite flips its own edit flag;
    /// a scalar's kind acts — a toggle flips, a number opens its draft, a
    /// dropdown its list, a text field its editor — and the control is
    /// live while the kind keeps it (`live`).
    pub fn start_editing(&mut self) {
        self.activate_control();
    }

    /// The selected card's control as its kind sees it, keyed by its path.
    fn current_spec(&self) -> Option<(String, fresh_core::api::WidgetSpec)> {
        let item = self.current_item()?;
        Some((item.path.clone(), self.spec_for(&item.path)?))
    }

    /// The node of the selected card's description that carries `key`: the
    /// control's own, or one of a text list's rows.
    fn spec_for(&self, key: &str) -> Option<fresh_core::api::WidgetSpec> {
        let item = self.current_item()?;
        Some(super::widget_map::live_widget(
            &item.path,
            &item.control,
            key,
        ))
    }

    /// The key of the live control: the selected card's, or one of its
    /// rows', when the store's focus names it.
    pub fn live_control(&self) -> Option<String> {
        let item = self.current_item()?;
        (self.focus_panel == FocusPanel::Settings
            && live::kind_edited(&item.control)
            && self.focus_key_of(item).is_some())
        .then(|| self.controls.focus_key.clone())
    }

    /// The store's focus key when it names `item`'s control or one of its
    /// rows — what the card paints as focused.
    pub fn focus_key_of(&self, item: &SettingItem) -> Option<&str> {
        let key = self.controls.focus_key.as_str();
        (key == item.path
            || key
                .strip_prefix(&item.path)
                .is_some_and(|r| r.starts_with("::")))
        .then_some(key)
    }

    /// The row the selected card's list cursor is on, while the list has
    /// the keyboard: a map's or an object array's entry, or its add row
    /// (`SettingControl::add_row`).
    pub fn composite_cursor(&self) -> Option<usize> {
        let item = self.current_item()?;
        self.composite_cursor_of(item)
    }

    /// [`composite_cursor`](Self::composite_cursor) for any card.
    pub fn composite_cursor_of(&self, item: &SettingItem) -> Option<usize> {
        if !item.control.has_list_rows() || self.controls.focus_key != item.path {
            return None;
        }
        let spec = super::widget_map::live_widget(&item.path, &item.control, &item.path);
        live::list_row(&self.controls, &spec, &item.path)
    }

    /// The tree key of the row the keyboard is on inside the selected card:
    /// a map's or an object array's cursor row, or a text list's live field.
    /// `None` when the card is the finest thing to reveal.
    pub fn focused_row_key(&self) -> Option<fresh_ui::Key> {
        let item = self.current_item()?;
        let row = match &item.control {
            SettingControl::TextList { items, .. } => {
                live::text_list::live_row(&self.controls, &item.path)?.unwrap_or(items.len())
            }
            _ => self.composite_cursor()?,
        };
        Some(item.control.row_tree_key(&item.path, row))
    }

    /// Whether the selected card's dropdown has its list up.
    pub fn is_dropdown_open(&self) -> bool {
        self.current_item().is_some_and(|item| {
            matches!(item.control, SettingControl::Dropdown { .. })
                && crate::widgets::kinds::dropdown::is_open(&item.path, &self.controls)
        })
    }

    /// Whether the selected card's number has a draft open.
    pub fn is_number_editing(&self) -> bool {
        self.current_item().is_some_and(|item| {
            matches!(item.control, SettingControl::Number { .. })
                && crate::widgets::kinds::number::editing(&item.path, &self.controls)
        })
    }

    /// Whether the selected card's text field is being edited.
    pub fn is_editing_text_control(&self) -> bool {
        self.live_control().is_some()
            && matches!(
                self.current_item().map(|i| &i.control),
                Some(SettingControl::Text { .. })
            )
    }

    /// The kinds' events onto the model. A number's, a dropdown's or a dual
    /// list's change is recorded at once; a text field's or a JSON editor's
    /// is applied to the model but recorded when its edit ends
    /// (`commit_text_edit`, `json_exit_editing`), so Escape has nothing to
    /// undo in the bookkeeping. A dual list's change also reaches its
    /// sibling, whose Available column must stop offering what this one
    /// took. Then: the control stays live while its kind holds it.
    fn absorb(&mut self, key: &str, events: &[(String, serde_json::Value)]) {
        let (changed, recorded_at_exit, is_dual_list, has_rows) = match self.current_item_mut() {
            Some(item) => (
                live::apply(&mut item.control, key, events),
                matches!(
                    item.control,
                    SettingControl::Text { .. } | SettingControl::Json { .. }
                ),
                matches!(item.control, SettingControl::DualList { .. }),
                item.control.has_list_rows(),
            ),
            None => (false, false, false, false),
        };
        if changed && is_dual_list {
            self.refresh_dual_list_sibling();
        }
        if changed && !recorded_at_exit {
            self.on_value_changed();
        }
        // A list's `activate` is the page's to answer: the row's entry
        // dialog opens, or the add row's.
        if has_rows {
            let activated = events
                .iter()
                .find(|(e, _)| e == "activate")
                .and_then(|(_, p)| p.get("index").and_then(|v| v.as_u64()));
            if let Some(index) = activated {
                self.composite_activate(index as usize);
            }
        }
        self.settle_live();
    }

    fn settle_live(&mut self) {
        let Some(path) = self.live_control() else {
            return;
        };
        let held = self
            .current_item()
            .is_some_and(|i| live::kind_holds(&i.control, &self.controls, &path));
        if !held {
            live::drop_state(&mut self.controls, &path);
        }
    }

    /// Enter (or its press) on the selected card's kind-edited control.
    pub fn activate_control(&mut self) {
        let Some((path, spec)) = self.current_spec() else {
            return;
        };
        let Some(item) = self.current_item() else {
            return;
        };
        match &item.control {
            SettingControl::Toggle { .. } => {
                let o = live::named(&mut self.controls, &spec, &path, "Enter");
                self.absorb(&path, &o.fx.events);
            }
            SettingControl::Number { .. } | SettingControl::Dropdown { .. } => {
                let opens_list = matches!(item.control, SettingControl::Dropdown { .. });
                self.controls.focus_key = path.clone();
                let o = live::named(&mut self.controls, &spec, &path, "Enter");
                self.absorb(&path, &o.fx.events);
                // An open list makes its card taller; the window is asked to
                // hold the taller card, which is the same request as any
                // other reveal.
                if opens_list && self.is_dropdown_open() {
                    self.body_anchor
                        .reveal_key(super::super::shell::settings::card_key(self.selected_item));
                }
            }
            SettingControl::Text { .. } => self.begin_text_edit(true),
            SettingControl::Json { .. } => self.begin_text_edit(false),
            // The dual list takes the keyboard: from here on its arrows
            // walk its columns rather than the page's cards.
            SettingControl::DualList { .. } => self.controls.focus_key = path,
            // A field of the text list already live keeps the keyboard;
            // otherwise its add row opens.
            SettingControl::TextList { .. } => {
                if live::text_list::live_row(&self.controls, &path).is_none() {
                    self.edit_list_row(None);
                }
            }
            // The list's Enter activates its cursor's row: the entry dialog
            // opens (`composite_activate`, from the kind's event).
            SettingControl::Map { .. } | SettingControl::ObjectArray { .. } => {
                if self.composite_cursor().is_none() {
                    self.enter_composite(true);
                }
                let o = live::named(&mut self.controls, &spec, &path, "Enter");
                self.absorb(&path, &o.fx.events);
            }
            SettingControl::Complex { .. } => {}
        }
    }

    /// Enter on the selected card from a gesture of the page's own — the
    /// web's activate: what the key does, the page's conventions included.
    pub fn activate_current(&mut self) {
        if self.live_control().is_none() {
            self.activate_control();
            return;
        }
        match self.current_item().map(|i| &i.control) {
            Some(SettingControl::TextList { .. }) => self.list_row_enter(),
            Some(SettingControl::Text { .. }) => {
                self.commit_text_edit();
            }
            _ => {
                self.live_dispatch(&crossterm::event::KeyEvent::new(
                    crossterm::event::KeyCode::Enter,
                    crossterm::event::KeyModifiers::NONE,
                ));
            }
        }
    }

    // =========== Lists: a map's or an object array's rows ===========

    /// A press on a row of the selected card's list: the list takes the
    /// keyboard with its cursor on the row.
    pub fn select_list_row(&mut self, row: usize) {
        let Some((path, spec)) = self.current_spec() else {
            return;
        };
        if !self
            .current_item()
            .is_some_and(|i| i.control.has_list_rows())
        {
            return;
        }
        self.controls.focus_key = path.clone();
        let o = live::pointer(
            &mut self.controls,
            &spec,
            &path,
            "select",
            &serde_json::json!({ "index": row }),
        );
        self.absorb(&path, &o.fx.events);
    }

    /// The list's cursor row was activated: an entry's dialog opens, or
    /// the add row's.
    fn composite_activate(&mut self, index: usize) {
        let Some(item) = self.current_item() else {
            return;
        };
        let add = item.control.add_row();
        match &item.control {
            SettingControl::Map { .. } if add == Some(index) => self.open_add_entry_dialog(),
            SettingControl::Map { .. } => self.open_entry_dialog(index),
            SettingControl::ObjectArray { .. } if add == Some(index) => {
                self.open_add_array_item_dialog()
            }
            SettingControl::ObjectArray { .. } => self.open_edit_array_item_dialog(index),
            _ => {}
        }
    }

    // =========== Text lists: rows as fields ===========

    /// The row of the selected text list whose field is live: `Some(i)`
    /// an item's, `None` the add row's.
    pub fn live_list_row(&self) -> Option<Option<usize>> {
        let item = self.current_item()?;
        live::text_list::live_row(&self.controls, &item.path)
    }

    /// Open a row of the selected text list for editing — an item's field,
    /// or the add row's for `None` — the caret at the end. A draft in the
    /// add row becomes an item first.
    pub fn edit_list_row(&mut self, row: Option<usize>) {
        if row.is_some() {
            self.commit_list_draft();
        }
        let Some(item) = self.current_item() else {
            return;
        };
        let SettingControl::TextList { items, .. } = &item.control else {
            return;
        };
        let (path, items) = (item.path.clone(), items.clone());
        live::text_list::edit_row(&mut self.controls, &path, &items, row);
        self.ensure_visible();
    }

    /// The add row's draft becomes an item. Returns whether one did.
    fn commit_list_draft(&mut self) -> bool {
        let Some(item) = self.current_item() else {
            return false;
        };
        let path = item.path.clone();
        let Some(text) = live::text_list::take_draft(&mut self.controls, &path) else {
            return false;
        };
        if let Some(SettingControl::TextList { items, .. }) =
            self.current_item_mut().map(|i| &mut i.control)
        {
            items.push(text);
        }
        self.on_value_changed();
        true
    }

    /// Up or Down in a live text list field: the adjacent row's field
    /// opens — the add row's after the last item. Returns whether the
    /// keyboard moved; at either end it stays.
    pub fn list_row_step(&mut self, delta: i32) -> bool {
        let Some(live) = self.live_list_row() else {
            return false;
        };
        // A draft in the add row becomes an item first, so the row above
        // the add row is the one just typed.
        if live.is_none() {
            self.commit_list_draft();
        }
        let Some(SettingControl::TextList { items, .. }) = self.current_item().map(|i| &i.control)
        else {
            return false;
        };
        let n = items.len();
        let cur = live.unwrap_or(n) as i32;
        let target = (cur + delta).clamp(0, n as i32) as usize;
        if target == cur as usize {
            return false;
        }
        self.edit_list_row((target < n).then_some(target));
        true
    }

    /// Enter in a live text list field: the add row's draft becomes an
    /// item and the add row stays open for the next; an item's field keeps
    /// the keyboard.
    pub fn list_row_enter(&mut self) {
        if self.live_list_row() == Some(None) && self.commit_list_draft() {
            self.edit_list_row(None);
        }
    }

    /// Leave the live text list field: the add row's draft becomes an item
    /// when `commit` (Tab), and is dropped otherwise (Escape).
    pub fn leave_list_row(&mut self, commit: bool) {
        if commit {
            self.commit_list_draft();
        }
        if let Some(item) = self.current_item() {
            let path = item.path.clone();
            live::text_list::leave(&mut self.controls, &path);
        }
    }

    /// Remove item `i` of the selected text list. A field live on it moves
    /// to the row that takes its place.
    pub fn remove_list_row(&mut self, i: usize) {
        let live = self.live_list_row();
        let Some(SettingControl::TextList { items, .. }) =
            self.current_item_mut().map(|it| &mut it.control)
        else {
            return;
        };
        if i >= items.len() {
            return;
        }
        items.remove(i);
        let n = items.len();
        self.on_value_changed();
        if let Some(row) = live {
            if let Some(item) = self.current_item() {
                let path = item.path.clone();
                live::text_list::leave(&mut self.controls, &path);
            }
            let row = match row {
                Some(r) if r > i => Some(r - 1),
                Some(r) if r == i => (r < n).then_some(r),
                other => other,
            };
            self.edit_list_row(row);
        }
    }

    /// Open the selected card's text field or JSON editor for editing: the
    /// whole value selected so the first keystroke replaces it
    /// (`select_all`), or the caret at the end. An unset JSON value opens
    /// empty, so what is typed is the value rather than an edit of the
    /// `null` literal.
    fn begin_text_edit(&mut self, select_all: bool) {
        let Some(item) = self.current_item() else {
            return;
        };
        let (value, seed, multiline) = match &item.control {
            SettingControl::Text { value, .. } => (value.clone(), value.clone(), false),
            SettingControl::Json { text, .. } => {
                let seed = match super::items::json_is_unset(text) {
                    true => String::new(),
                    false => text.clone(),
                };
                (text.clone(), seed, true)
            }
            _ => return,
        };
        let path = item.path.clone();
        self.text_edit_snapshot = Some(TextEditSnapshot {
            path: path.clone(),
            value,
        });
        live::seed_text(&mut self.controls, &path, &seed, select_all, multiline);
        self.controls.focus_key = path;
    }

    /// A keystroke while a control is live: the kind's, then the model's.
    /// `None` when nothing is live. What the kind declined is the caller's
    /// (`handle_live_control_key`): Enter, Tab and Escape on a text field,
    /// Tab and Escape on a JSON editor or a dual list, and the clipboard
    /// chords the host owns.
    ///
    /// The page's Enter on a live dual list moves the item under its
    /// cursor across — the kind's Space. The kind's own Enter is a form's
    /// (advance to the next field), which a page of cards has no use for.
    pub fn live_dispatch(&mut self, ev: &crossterm::event::KeyEvent) -> Option<live::Outcome> {
        let key = self.live_control()?;
        let spec = self.spec_for(&key)?;
        let outcome = match (&spec, ev.code) {
            (fresh_core::api::WidgetSpec::DualList { .. }, crossterm::event::KeyCode::Enter) => {
                live::named(&mut self.controls, &spec, &key, "Space")
            }
            _ => live::key(&mut self.controls, &spec, &key, ev),
        };
        self.absorb(&key, &outcome.fx.events);
        // The window is asked to hold the row the keyboard is on — a list's
        // cursor row after an arrow, as it holds the card after a step off it.
        self.ensure_visible();
        Some(outcome)
    }

    /// Type into the live control: a paste.
    fn live_text(&mut self, text: &str) -> bool {
        let Some(key) = self.live_control() else {
            return false;
        };
        let Some(spec) = self.spec_for(&key) else {
            return false;
        };
        let outcome = live::text(&mut self.controls, &spec, &key, text);
        self.absorb(&key, &outcome.fx.events);
        true
    }

    /// Escape on the live text field or JSON editor: what was typed is
    /// discarded and the value is what it was before the edit began.
    pub fn revert_text_edit(&mut self) {
        let Some(path) = self.live_control() else {
            return;
        };
        if let Some(snap) = self.text_edit_snapshot.take() {
            if let Some(item) = self.current_item_mut() {
                if item.path == snap.path {
                    match &mut item.control {
                        SettingControl::Text { value, .. } => *value = snap.value,
                        SettingControl::Json { text, .. } => *text = snap.value,
                        _ => {}
                    }
                }
            }
        }
        live::drop_state(&mut self.controls, &path);
    }

    /// Leaving the selected card while its control is live: a text field's
    /// and a number's draft are committed, a JSON editor's text is kept
    /// when it parses, a dropdown's list is closed on the selection it
    /// opened with, and a dual list hands the keyboard back.
    pub fn leave_live_control(&mut self) {
        let Some(path) = self.live_control() else {
            return;
        };
        let Some((_, spec)) = self.current_spec() else {
            return;
        };
        let name = match self.current_item().map(|i| &i.control) {
            Some(SettingControl::Text { .. }) => {
                self.commit_text_edit();
                return;
            }
            Some(SettingControl::Json { .. }) => {
                self.json_exit_editing();
                return;
            }
            Some(SettingControl::TextList { .. }) => {
                self.leave_list_row(true);
                return;
            }
            Some(
                SettingControl::DualList { .. }
                | SettingControl::Map { .. }
                | SettingControl::ObjectArray { .. },
            ) => {
                live::drop_state(&mut self.controls, &path);
                return;
            }
            Some(SettingControl::Dropdown { .. }) => "Escape",
            _ => "Enter",
        };
        let o = live::named(&mut self.controls, &spec, &path, name);
        self.absorb(&path, &o.fx.events);
        live::drop_state(&mut self.controls, &path);
    }

    /// Close the selected card's open list on the selection it opened with
    /// — a press outside it, or `Escape`.
    pub fn dropdown_cancel(&mut self) {
        if !self.is_dropdown_open() {
            return;
        }
        let Some((path, spec)) = self.current_spec() else {
            return;
        };
        let o = live::named(&mut self.controls, &spec, &path, "Escape");
        self.absorb(&path, &o.fx.events);
    }

    /// Step the selected card's dropdown by `delta`, list open or not — the
    /// `◂`/`▸` presses and the web's increment/decrement.
    pub fn cycle_dropdown(&mut self, delta: i32) {
        let Some((path, spec)) = self.current_spec() else {
            return;
        };
        if !matches!(
            self.current_item().map(|i| &i.control),
            Some(SettingControl::Dropdown { .. })
        ) {
            return;
        }
        let mut fx = crate::widgets::kinds::KeyFx::default();
        crate::widgets::kinds::dropdown::cycle_selection(
            &spec,
            &path,
            &mut self.controls,
            delta,
            &mut fx,
        );
        self.absorb(&path, &fx.events);
    }

    /// A press on an option row of the selected card's open list.
    pub fn select_dropdown_option(&mut self, index: usize) {
        let Some((path, spec)) = self.current_spec() else {
            return;
        };
        if !matches!(
            self.current_item().map(|i| &i.control),
            Some(SettingControl::Dropdown { .. })
        ) {
            return;
        }
        self.controls.focus_key = path.clone();
        let o = live::pointer(
            &mut self.controls,
            &spec,
            &path,
            "dropdown_select",
            &serde_json::json!({ "index": index }),
        );
        self.absorb(&path, &o.fx.events);
    }

    /// A press on the selected card's number cell: its draft opens, the
    /// value selected so the next digit replaces it.
    pub fn press_number_value(&mut self) {
        let Some((path, spec)) = self.current_spec() else {
            return;
        };
        if !matches!(
            self.current_item().map(|i| &i.control),
            Some(SettingControl::Number { .. })
        ) {
            return;
        }
        self.controls.focus_key = path.clone();
        let o = live::pointer(
            &mut self.controls,
            &spec,
            &path,
            "number_value",
            &serde_json::json!({}),
        );
        self.absorb(&path, &o.fx.events);
    }

    /// A press on the selected card's text field or JSON editor: the edit
    /// opens with the caret where the press landed, when the press said
    /// where.
    pub fn press_text(&mut self, byte: Option<usize>) {
        if self.live_control().is_none() {
            match self.current_item().map(|i| &i.control) {
                Some(SettingControl::Text { .. }) => self.begin_text_edit(true),
                Some(SettingControl::Json { .. }) => self.begin_text_edit(false),
                _ => return,
            }
        }
        if let Some(byte) = byte {
            self.position_text_cursor(byte);
        }
    }
    /// Record the live text field's value and leave it — Enter, Tab, and
    /// leaving the card land here. Returns whether a text edit was open.
    pub fn commit_text_edit(&mut self) -> bool {
        if !self.is_editing_text_control() {
            return false;
        }
        let Some(path) = self.live_control() else {
            return false;
        };
        self.on_value_changed();
        self.text_edit_snapshot = None;
        live::drop_state(&mut self.controls, &path);
        true
    }

    /// Stop editing. The *accept* path — Enter, Tab, and clicking away all
    /// land here and keep what was typed: a text field's value is recorded.
    pub fn stop_editing(&mut self) {
        self.leave_live_control();
    }
    /// Check if the current item is editable (TextList, DualList, Text, Map, or Json)
    pub fn is_editable_control(&self) -> bool {
        self.current_item().is_some_and(|item| {
            matches!(
                item.control,
                SettingControl::TextList { .. }
                    | SettingControl::DualList { .. }
                    | SettingControl::Text { .. }
                    | SettingControl::Map { .. }
                    | SettingControl::Json { .. }
            )
        })
    }

    /// Whether the selected card's JSON editor is being edited.
    pub fn is_editing_json(&self) -> bool {
        self.live_control().is_some()
            && matches!(
                self.current_item().map(|i| &i.control),
                Some(SettingControl::Json { .. })
            )
    }

    /// Move the live text field's caret to a byte of its value — a press
    /// (#2573). No-op unless a text edit is open.
    pub fn position_text_cursor(&mut self, byte: usize) {
        let Some(path) = self.live_control() else {
            return;
        };
        if let Some(editor) = live::text_editor(&mut self.controls, &path) {
            editor.clear_selection();
            editor.set_cursor_from_flat(byte);
        }
    }
    /// Paste into whatever is being edited: the live control's kind, or the
    /// entry dialog's field. Returns whether the text landed anywhere.
    pub fn paste_into_focused_text(&mut self, text: &str) -> bool {
        if let Some(dialog) = self.entry_dialog_mut() {
            return dialog.paste(text);
        }
        self.live_text(text)
    }

    /// Whether the selected card's dual list has the keyboard.
    pub fn is_editing_dual_list(&self) -> bool {
        self.live_control().is_some()
            && matches!(
                self.current_item().map(|i| &i.control),
                Some(SettingControl::DualList { .. })
            )
    }

    // =========== Dual list ===========

    /// One of the dual list's moves on the selected card, from a gesture
    /// of the page's own — the web's buttons beside the columns. The
    /// control becomes live, as a press on one of its cells makes it.
    pub fn dual_list_op(&mut self, op: crate::widgets::kinds::dual_list::DualOp) {
        let Some((path, spec)) = self.current_spec() else {
            return;
        };
        if !matches!(
            self.current_item().map(|i| &i.control),
            Some(SettingControl::DualList { .. })
        ) {
            return;
        }
        self.controls.focus_key = path.clone();
        let mut fx = crate::widgets::kinds::KeyFx::default();
        crate::widgets::kinds::dual_list::apply_op(&spec, &path, &mut self.controls, op, &mut fx);
        self.absorb(&path, &fx.events);
    }

    /// A press on a cell of the selected card's dual list: the control
    /// becomes live with that column active and its cursor on the row.
    pub fn press_dual_list(&mut self, included: bool, row: usize) {
        let Some((path, spec)) = self.current_spec() else {
            return;
        };
        if !matches!(
            self.current_item().map(|i| &i.control),
            Some(SettingControl::DualList { .. })
        ) {
            return;
        }
        self.controls.focus_key = path.clone();
        let o = live::pointer(
            &mut self.controls,
            &spec,
            &path,
            "dual_focus",
            &serde_json::json!({
                "column": match included {
                    true => "included",
                    false => "available",
                },
                "index": row,
            }),
        );
        self.absorb(&path, &o.fx.events);
    }

    /// After a dual list's included set changed, the sibling list's
    /// Available column must stop offering what this one took.
    ///
    /// Assumes the sibling setting lives on the same page as the current item.
    /// This holds for the current use case (`status_bar.left` and `.right` are both
    /// flattened into the Editor page under the "Status Bar" section). Cross-category
    /// siblings would silently no-op until the next `build_pages()`.
    pub fn refresh_dual_list_sibling(&mut self) {
        let (new_included, sibling_path) = {
            let Some(item) = self.current_item() else {
                return;
            };
            let SettingControl::DualList { included, .. } = &item.control else {
                return;
            };
            let Some(ref sib_path) = item.dual_list_sibling else {
                return;
            };
            (included.clone(), sib_path.clone())
        };

        // Find sibling item in same page and update its excluded
        if let Some(page) = self.pages.get_mut(self.selected_category) {
            for other in page.items.iter_mut() {
                if other.path == sibling_path {
                    if let SettingControl::DualList { excluded, .. } = &mut other.control {
                        *excluded = new_included;
                    }
                    break;
                }
            }
        }
    }

    // =========== JSON editor ===========

    /// Leaving the live JSON editor — Tab, Escape, or leaving the card: a
    /// text that parses is recorded; one that does not is put back to what
    /// it was when the edit began.
    pub fn json_exit_editing(&mut self) {
        let Some(path) = self.live_control() else {
            return;
        };
        let valid = matches!(
            self.current_item().map(|i| &i.control),
            Some(SettingControl::Json { text, .. }) if super::items::json_is_valid(text)
        );
        if valid {
            self.on_value_changed();
            self.text_edit_snapshot = None;
            live::drop_state(&mut self.controls, &path);
        } else {
            self.revert_text_edit();
        }
    }

    /// Get list of pending changes for display
    pub fn get_change_descriptions(&self) -> Vec<String> {
        let mut descriptions: Vec<String> = self
            .pending_changes
            .iter()
            .map(|(path, value)| {
                let value_str = match value {
                    serde_json::Value::Bool(b) => b.to_string(),
                    serde_json::Value::Number(n) => n.to_string(),
                    serde_json::Value::String(s) => format!("\"{}\"", s),
                    _ => value.to_string(),
                };
                format!("{}: {}", path, value_str)
            })
            .collect();
        // Also include pending deletions (resets)
        for path in &self.pending_deletions {
            descriptions.push(format!("{}: (reset to default)", path));
        }
        descriptions.sort();
        descriptions
    }
}

/// Update a control's state from a JSON value
/// Field names whose per-entry "set to null" genuinely *inherits* a parent
/// value (so the button reads `[Inherit]`) rather than just clearing the field
/// (`[Clear]`). For a `/languages` entry a field inherits when the global
/// `editor` config has a same-named setting (e.g. `line_wrap`, `tab_size`);
/// fields with no global fallback (e.g. `formatter`) are clear-only. Other maps
/// have no such parent scope, so nothing inherits.
fn inheritable_fields_for(map_path: &str) -> std::collections::HashSet<String> {
    if map_path == "/languages" {
        serde_json::to_value(crate::config::EditorConfig::default())
            .ok()
            .and_then(|v| {
                v.as_object().map(|o| {
                    o.keys()
                        .cloned()
                        .collect::<std::collections::HashSet<String>>()
                })
            })
            .unwrap_or_default()
    } else {
        std::collections::HashSet::new()
    }
}

/// Override each dialog field's `default` with the bundled config's value for
/// this map entry (e.g. `languages.html.grammar = "HTML"`), so `[Reset]`
/// restores the built-in per-entry value rather than the generic schema default
/// for the field type. Falls back to the schema defaults when the entry isn't
/// in the bundled config (e.g. a brand-new map key).
fn apply_builtin_defaults(dialog: &mut EntryDialogState, entry_pointer: &str) {
    let Ok(default_cfg) = serde_json::to_value(Config::default()) else {
        return;
    };
    let Some(entry) = default_cfg.pointer(entry_pointer) else {
        return;
    };
    for item in &mut dialog.items {
        if item.path == "__key__" {
            continue;
        }
        let field = item.path.trim_start_matches('/');
        if let Some(v) = entry.get(field) {
            item.default = Some(v.clone());
        }
    }
}

pub(crate) fn update_control_from_value(control: &mut SettingControl, value: &serde_json::Value) {
    match control {
        SettingControl::Toggle { checked, .. } => {
            if let Some(b) = value.as_bool() {
                *checked = b;
            }
        }
        SettingControl::Number { value: current, .. } => {
            if let Some(n) = value.as_f64() {
                *current = n;
            }
        }
        SettingControl::Dropdown {
            options,
            values,
            selected,
            ..
        } => {
            let stored = match values.is_empty() {
                true => options,
                false => values,
            };
            let wanted = match value.as_str() {
                Some(s) => s,
                // `null` is a nullable enum's "(none)", stored as the empty value.
                None if value.is_null() => "",
                None => return,
            };
            if let Some(idx) = stored.iter().position(|o| o == wanted) {
                *selected = idx;
            }
        }
        SettingControl::Text { value: current, .. } => {
            if let Some(s) = value.as_str() {
                *current = s.to_string();
            }
        }
        SettingControl::TextList { items, integer, .. } => {
            if let Some(arr) = value.as_array() {
                *items = arr
                    .iter()
                    .filter_map(|v| match integer {
                        true => v
                            .as_i64()
                            .map(|n| n.to_string())
                            .or_else(|| v.as_u64().map(|n| n.to_string()))
                            .or_else(|| v.as_f64().map(|n| n.to_string())),
                        false => v.as_str().map(String::from),
                    })
                    .collect();
            }
        }
        SettingControl::DualList { included, .. } => {
            if let Some(arr) = value.as_array() {
                *included = arr
                    .iter()
                    .filter_map(|v| v.as_str().map(String::from))
                    .collect();
            }
        }
        SettingControl::Map { entries, .. } => {
            if value.is_object() {
                *entries = super::items::map_entries(value);
            }
        }
        SettingControl::ObjectArray { items, .. } => {
            if let Some(arr) = value.as_array() {
                *items = arr.clone();
            }
        }
        SettingControl::Json { text, .. } => *text = super::items::json_text(Some(value)),
        SettingControl::Complex { .. } => {}
    }
}

fn normalize_pending_change(path: &str, value: serde_json::Value) -> serde_json::Value {
    if path == "/editor/indentation_guide_glyph" {
        if let serde_json::Value::String(value) = value {
            return serde_json::Value::String(crate::config::normalize_indentation_guide_glyph(
                &value,
            ));
        }
    }

    value
}

#[cfg(test)]
mod tests {
    use super::*;
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

    const TEST_SCHEMA: &str = r#"
{
  "type": "object",
  "properties": {
    "theme": {
      "type": "string",
      "default": "dark"
    },
    "line_numbers": {
      "type": "boolean",
      "default": true
    }
  },
  "$defs": {}
}
"#;

    fn test_config() -> Config {
        Config::default()
    }

    #[test]
    fn test_settings_state_creation() {
        let config = test_config();
        let state = SettingsState::new(TEST_SCHEMA, &config).unwrap();

        assert!(!state.visible);
        assert_eq!(state.selected_category, 0);
        assert!(!state.has_changes());
    }

    #[test]
    fn test_navigation() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA, &config).unwrap();

        // Start in category focus
        assert_eq!(state.focus_panel(), FocusPanel::Categories);

        // Into the body
        state.focus_on(FocusTarget::Card(0));
        assert_eq!(state.focus_panel(), FocusPanel::Settings);

        // Navigate items
        state.select_next();
        assert_eq!(state.selected_item, 1);

        state.select_prev();
        assert_eq!(state.selected_item, 0);
    }

    #[test]
    fn test_pending_changes() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA, &config).unwrap();

        assert!(!state.has_changes());

        state.set_pending_change("/theme", serde_json::Value::String("light".to_string()));
        assert!(state.has_changes());

        state.discard_changes();
        assert!(!state.has_changes());
    }

    #[test]
    fn test_indentation_guide_glyph_pending_change_is_normalized() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA, &config).unwrap();

        state.set_pending_change(
            "/editor/indentation_guide_glyph",
            serde_json::Value::String("  ┊  ".to_string()),
        );
        assert_eq!(
            state.pending_changes.get("/editor/indentation_guide_glyph"),
            Some(&serde_json::Value::String("┊".to_string()))
        );

        let config = state.apply_changes(&config).unwrap();
        assert_eq!(config.editor.indentation_guide_glyph, "┊");

        let mut state = SettingsState::new(TEST_SCHEMA, &config).unwrap();
        state.set_pending_change(
            "/editor/indentation_guide_glyph",
            serde_json::Value::String("   ".to_string()),
        );
        assert_eq!(
            state.pending_changes.get("/editor/indentation_guide_glyph"),
            Some(&serde_json::Value::String("▏".to_string()))
        );

        // Backstop for text-edit paths that may already have written a raw
        // pending value before normalization was added: apply_changes must
        // still produce the same normalized live config that reload would.
        state.pending_changes.insert(
            "/editor/indentation_guide_glyph".to_string(),
            serde_json::Value::String("  A  ".to_string()),
        );
        let config = state.apply_changes(&config).unwrap();
        assert_eq!(config.editor.indentation_guide_glyph, "A");
    }

    #[test]
    fn test_show_hide() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA, &config).unwrap();

        assert!(!state.visible);

        state.show();
        assert!(state.visible);
        assert_eq!(state.focus_panel(), FocusPanel::Categories);

        state.hide();
        assert!(!state.visible);
    }

    // Schema with dropdown (enum) and number controls for testing
    const TEST_SCHEMA_CONTROLS: &str = r#"
{
  "type": "object",
  "properties": {
    "theme": {
      "type": "string",
      "enum": ["dark", "light", "high-contrast"],
      "default": "dark"
    },
    "tab_size": {
      "type": "integer",
      "minimum": 1,
      "maximum": 8,
      "default": 4
    },
    "line_numbers": {
      "type": "boolean",
      "default": true
    }
  },
  "$defs": {}
}
"#;

    const TEST_SCHEMA_THEME_DEFAULT: &str = r#"
{
  "type": "object",
  "properties": {
    "theme": {
      "type": "string",
      "enum": ["dark", "light", "high-contrast"],
      "default": "high-contrast"
    }
  },
  "$defs": {}
}
"#;

    fn open_theme_dropdown_state() -> SettingsState {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA_THEME_DEFAULT, &config).unwrap();
        state.show();
        state.focus_on(FocusTarget::Card(0));
        state
    }

    fn press(state: &mut SettingsState, code: KeyCode) {
        state.live_dispatch(&KeyEvent::new(code, KeyModifiers::NONE));
    }

    fn type_text(state: &mut SettingsState, text: &str) {
        for c in text.chars() {
            press(state, KeyCode::Char(c));
        }
    }

    fn dropdown_selected(state: &SettingsState) -> usize {
        match state.current_item().map(|i| &i.control) {
            Some(SettingControl::Dropdown { selected, .. }) => *selected,
            other => panic!("not a dropdown: {other:?}"),
        }
    }

    fn number_value(state: &SettingsState) -> f64 {
        match state.current_item().map(|i| &i.control) {
            Some(SettingControl::Number { value, .. }) => *value,
            other => panic!("not a number: {other:?}"),
        }
    }

    fn text_value(state: &SettingsState) -> String {
        match state.current_item().map(|i| &i.control) {
            Some(SettingControl::Text { value, .. }) => value.clone(),
            other => panic!("not a text field: {other:?}"),
        }
    }

    fn select_theme_text(state: &mut SettingsState) {
        // `theme` is a plain string property -> a Text control. Items are
        // sorted within the page, so select it explicitly by path.
        let theme_idx = state.pages[0]
            .items
            .iter()
            .position(|i| i.path == "/theme")
            .expect("theme item present");
        state.show();
        state.focus_on(FocusTarget::Card(theme_idx));
        assert!(matches!(
            state.current_item().map(|i| &i.control),
            Some(SettingControl::Text { .. })
        ));
    }

    /// Enter and Tab keep what was typed: the value is recorded and
    /// survives a Save into the live config.
    #[test]
    fn a_text_edit_is_recorded_when_it_ends() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA, &config).unwrap();
        select_theme_text(&mut state);

        state.start_editing();
        assert!(state.is_editing_text_control());
        // The value opens selected, so the first keystroke replaces it.
        type_text(&mut state, "light");
        assert_eq!(text_value(&state), "light", "typed live into the model");
        assert!(
            !state.pending_changes.contains_key("/theme"),
            "nothing is recorded until the edit ends"
        );

        state.stop_editing();
        assert!(!state.is_editing_text_control());
        assert_eq!(
            state.pending_changes.get("/theme"),
            Some(&serde_json::Value::String("light".to_string())),
            "the typed value is recorded when the edit is accepted"
        );
        let config = state.apply_changes(&config).unwrap();
        assert_eq!(config.theme.0, "light");
    }

    /// Escape discards what was typed: the field says what it said before
    /// the edit began and nothing is recorded.
    #[test]
    fn a_text_edit_reverted_records_nothing() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA, &config).unwrap();
        select_theme_text(&mut state);
        let before = text_value(&state);

        state.start_editing();
        type_text(&mut state, "light");
        state.revert_text_edit();

        assert!(!state.is_editing_text_control());
        assert_eq!(text_value(&state), before);
        assert!(!state.pending_changes.contains_key("/theme"));
    }

    fn select_theme_dropdown(state: &mut SettingsState) {
        state.show();
        state.focus_on(FocusTarget::Card(0));
        // Items are sorted alphabetically: line_numbers, tab_size, theme
        state.select_next();
        state.select_next();
        assert!(matches!(
            state.current_item().map(|i| &i.control),
            Some(SettingControl::Dropdown { .. })
        ));
    }

    #[test]
    fn enter_opens_a_dropdowns_list_and_enter_closes_it() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA_CONTROLS, &config).unwrap();
        select_theme_dropdown(&mut state);
        assert!(!state.is_dropdown_open());

        state.activate_control();
        assert!(state.is_dropdown_open());
        assert_eq!(state.live_control().as_deref(), Some("/theme"));

        press(&mut state, KeyCode::Enter);
        assert!(!state.is_dropdown_open());
        assert!(
            state.live_control().is_none(),
            "the list closed, the control is left"
        );
    }

    #[test]
    fn escape_puts_a_dropdown_back_where_it_opened() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA_CONTROLS, &config).unwrap();
        select_theme_dropdown(&mut state);
        state.activate_control();
        let initial = dropdown_selected(&state);

        press(&mut state, KeyCode::Down);
        assert_ne!(
            dropdown_selected(&state),
            initial,
            "the selection moves live"
        );
        assert!(state.has_changes(), "and is recorded as it moves");

        state.dropdown_cancel();
        assert!(!state.is_dropdown_open());
        assert_eq!(dropdown_selected(&state), initial);
        assert!(!state.has_changes(), "back where it was is not a change");
    }

    #[test]
    fn enter_keeps_a_dropdowns_moved_selection() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA_CONTROLS, &config).unwrap();
        select_theme_dropdown(&mut state);
        state.activate_control();
        press(&mut state, KeyCode::Down);
        let moved = dropdown_selected(&state);

        press(&mut state, KeyCode::Enter);
        assert!(!state.is_dropdown_open());
        assert_eq!(dropdown_selected(&state), moved);
        assert!(state.has_changes());
    }

    #[test]
    fn dropdown_reverting_to_original_value_clears_pending_and_row_modified() {
        let mut state = open_theme_dropdown_state();

        state.select_dropdown_option(0); // dark
        assert!(state.has_changes());
        assert!(state.current_item().unwrap().modified);

        state.select_dropdown_option(2); // high-contrast, matching Config::default()
        assert!(!state.has_changes());
        let item = state.current_item().unwrap();
        assert!(!item.modified);
        assert_eq!(item.layer_source, ConfigLayer::System);
    }

    #[test]
    fn reset_after_unsaved_inherited_dropdown_change_cancels_pending_edit() {
        let mut state = open_theme_dropdown_state();

        state.select_dropdown_option(1); // light
        assert!(state.has_changes());
        assert!(state.current_item().unwrap().modified);

        state.reset_current_to_default();
        assert!(!state.has_changes());
        let item = state.current_item().unwrap();
        assert!(!item.modified);
        assert_eq!(item.layer_source, ConfigLayer::System);
        assert_eq!(
            item.control.dropdown_selected_value(),
            Some("high-contrast"),
            "theme should render as a dropdown showing the default"
        );
    }

    fn select_tab_size(state: &mut SettingsState) {
        state.show();
        state.focus_on(FocusTarget::Card(0));
        state.select_next(); // tab_size
        assert!(matches!(
            state.current_item().map(|i| &i.control),
            Some(SettingControl::Number { .. })
        ));
    }

    #[test]
    fn a_number_is_typed_into_its_draft_and_enter_records_it() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA_CONTROLS, &config).unwrap();
        select_tab_size(&mut state);
        assert!(!state.is_number_editing());

        state.activate_control();
        assert!(state.is_number_editing());
        // The draft opens selected: the digit replaces the value.
        press(&mut state, KeyCode::Char('8'));
        press(&mut state, KeyCode::Enter);

        assert!(!state.is_number_editing());
        assert!(state.live_control().is_none());
        assert_eq!(number_value(&state), 8.0);
        assert!(state.has_changes());
    }

    #[test]
    fn escape_abandons_a_numbers_draft() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA_CONTROLS, &config).unwrap();
        select_tab_size(&mut state);
        let initial = number_value(&state);

        state.activate_control();
        press(&mut state, KeyCode::Backspace);
        press(&mut state, KeyCode::Char('9'));
        press(&mut state, KeyCode::Char('9'));
        press(&mut state, KeyCode::Esc);

        assert!(!state.is_number_editing());
        assert_eq!(number_value(&state), initial);
        assert!(!state.has_changes());
    }

    #[test]
    fn backspace_edits_the_numbers_draft() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA_CONTROLS, &config).unwrap();
        select_tab_size(&mut state);
        state.activate_control();
        press(&mut state, KeyCode::Backspace);

        let path = state.current_item().unwrap().path.clone();
        let draft = crate::widgets::kinds::number::resolve(
            0.0,
            None,
            None,
            Some(&path),
            &state.controls.instance_states,
        )
        .draft
        .expect("the draft is open");
        // The selected "4" was deleted, leaving an empty draft.
        assert_eq!(draft.text, "");
        press(&mut state, KeyCode::Esc);
    }

    #[test]
    fn test_layer_selection() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA, &config).unwrap();

        // Default is User layer
        assert_eq!(state.target_layer, ConfigLayer::User);
        assert_eq!(state.target_layer_name(), "User");

        // Cycle through layers
        state.cycle_target_layer();
        assert_eq!(state.target_layer, ConfigLayer::Project);
        assert_eq!(state.target_layer_name(), "Project");

        state.cycle_target_layer();
        assert_eq!(state.target_layer, ConfigLayer::Session);
        assert_eq!(state.target_layer_name(), "Session");

        state.cycle_target_layer();
        assert_eq!(state.target_layer, ConfigLayer::User);

        // Set directly
        state.set_target_layer(ConfigLayer::Project);
        assert_eq!(state.target_layer, ConfigLayer::Project);

        // Setting to System should be ignored (read-only)
        state.set_target_layer(ConfigLayer::System);
        assert_eq!(state.target_layer, ConfigLayer::Project);
    }

    #[test]
    fn test_layer_switch_clears_pending_changes() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA, &config).unwrap();

        // Add a pending change
        state.set_pending_change("/theme", serde_json::Value::String("light".to_string()));
        assert!(state.has_changes());

        // Switching layers clears pending changes
        state.cycle_target_layer();
        assert!(!state.has_changes());
    }

    /// Regression test for the quicklsp settings-save bug.
    ///
    /// When editing an existing map entry whose value schema is itself an
    /// array (the `is_single_value` case — e.g. `universal_lsp.quicklsp`
    /// where the value schema is `LspLanguageConfig` = array of
    /// `LspServerConfig`), opening a nested ArrayItem dialog used to
    /// compute its `map_path` from `parent.map_path + item.path` only —
    /// dropping the entry key segment whenever `item.path` was `""`.
    /// The nested dialog's save would then record a pending change at
    /// `/universal_lsp/`, which downstream wrote an empty-string key
    /// under `universal_lsp` in the saved config file.
    ///
    /// This test exercises the real `open_nested_entry_dialog` + save
    /// path using a schema shaped like `LspLanguageConfig` and asserts:
    /// 1. The nested dialog's `map_path` is the full entry path.
    /// 2. The recorded pending-change path is the full entry path, not
    ///    `/universal_lsp/` and not any `/universal_lsp/*` path with a
    ///    trailing slash.
    #[test]
    fn nested_array_save_records_full_entry_path() {
        // EntryDialogState is already re-exported via `use super::*;`.
        // Pull in SettingType from the sibling schema module explicitly.
        use crate::view::settings::schema::SettingType;

        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA, &config).unwrap();

        // LspServerConfig-ish: a single "enabled" boolean field.
        let item_schema = SettingSchema {
            path: "/item".to_string(),
            name: "Server".to_string(),
            description: None,
            setting_type: SettingType::Object {
                properties: vec![SettingSchema {
                    path: "/enabled".to_string(),
                    name: "Enabled".to_string(),
                    description: None,
                    setting_type: SettingType::Boolean,
                    default: Some(serde_json::json!(false)),
                    read_only: false,
                    section: None,
                    order: None,
                    nullable: false,
                    enum_from: None,
                    dual_list_sibling: None,
                    dynamically_extendable_status_bar_elements: false,
                }],
            },
            default: None,
            read_only: false,
            section: None,
            order: None,
            nullable: false,
            enum_from: None,
            dual_list_sibling: None,
            dynamically_extendable_status_bar_elements: false,
        };

        // universal_lsp's value schema: ObjectArray of the item schema above.
        // Note: path is "" just like the real schema parser produces for
        // `parse_setting("value", "", ...)` — this is what drives the
        // `is_single_value` code path in EntryDialogState::from_schema.
        let value_schema = SettingSchema {
            path: String::new(),
            name: "value".to_string(),
            description: None,
            setting_type: SettingType::ObjectArray {
                item_schema: Box::new(item_schema.clone()),
                display_field: None,
            },
            default: None,
            read_only: false,
            section: None,
            order: None,
            nullable: false,
            enum_from: None,
            dual_list_sibling: None,
            dynamically_extendable_status_bar_elements: false,
        };

        // Parent dialog: user is editing the existing "quicklsp" entry
        // under /universal_lsp. This is the MapEntry dialog the real UI
        // opened via `open_entry_dialog`.
        let parent = EntryDialogState::from_schema(
            "quicklsp".to_string(),
            &serde_json::json!([{ "enabled": true }]),
            &value_schema,
            "/universal_lsp",
            false, // existing entry
            false,
            &HashMap::new(),
        );

        // Precondition: is_single_value triggers and entry_path is correct.
        assert!(
            parent.is_single_value,
            "array value_schema should trigger is_single_value path"
        );
        assert_eq!(parent.entry_path(), "/universal_lsp/quicklsp");

        state.entry_dialog_stack.push(parent);

        // Exercise the REAL open_nested_entry_dialog — this is the code
        // path that used to produce the wrong path. The outer dialog's
        // ObjectArray item is already focused with its first entry
        // selected (init_object_array_focus in from_schema).
        state.open_nested_entry_dialog();

        // A nested dialog should have been pushed.
        assert_eq!(
            state.entry_dialog_stack.len(),
            2,
            "open_nested_entry_dialog should have pushed a nested dialog"
        );

        // CRITICAL (part 1): the nested dialog must root at the full
        // entry path, not at the parent's map_path alone.
        let nested_map_path = state
            .entry_dialog_stack
            .last()
            .map(|d| d.map_path.clone())
            .unwrap();
        assert_eq!(
            nested_map_path, "/universal_lsp/quicklsp",
            "BUG: nested dialog's map_path dropped the 'quicklsp' key segment"
        );

        // Save the nested dialog via the normal dispatch.
        state.save_entry_dialog();

        // Nested dialog should be popped, parent still on the stack.
        assert_eq!(state.entry_dialog_stack.len(), 1);

        // CRITICAL (part 2): the pending change must be rooted at the
        // full entry path, not at `/universal_lsp/` with a trailing slash.
        assert!(
            !state.pending_changes.contains_key("/universal_lsp/"),
            "regression: pending change recorded under empty-key path /universal_lsp/. \
             All keys: {:?}",
            state.pending_changes.keys().collect::<Vec<_>>()
        );
        assert!(
            !state
                .pending_changes
                .keys()
                .any(|k| k.starts_with("/universal_lsp") && k.ends_with('/')),
            "no /universal_lsp/* path should end in a trailing slash; got {:?}",
            state.pending_changes.keys().collect::<Vec<_>>()
        );
        assert!(
            state
                .pending_changes
                .contains_key("/universal_lsp/quicklsp"),
            "expected pending change at /universal_lsp/quicklsp, got {:?}",
            state.pending_changes.keys().collect::<Vec<_>>()
        );
    }

    /// The status bar's two pickers share one option set: what the left one
    /// takes, the right one's Available column must stop offering. The
    /// carry is the kind's; the sibling's `excluded` follows the change.
    #[test]
    fn a_dual_list_change_reaches_its_sibling() {
        use crate::widgets::kinds::dual_list::DualOp;

        // Uses the real config schema (which has /editor/status_bar/left and /right
        // as DualList siblings).
        let schema = include_str!("../../../plugins/config-schema.json");
        let config = test_config();
        let mut state = SettingsState::new(schema, &config).unwrap();

        let editor_page_idx = state
            .pages
            .iter()
            .position(|p| p.path == "/editor")
            .expect("editor page");
        state.selected_category = editor_page_idx;
        state.focus_panel = FocusPanel::Settings;

        let (left_idx, right_idx) = {
            let page = &state.pages[editor_page_idx];
            let l = page
                .items
                .iter()
                .position(|i| i.path == "/editor/status_bar/left")
                .expect("left item");
            let r = page
                .items
                .iter()
                .position(|i| i.path == "/editor/status_bar/right")
                .expect("right item");
            (l, r)
        };
        let included_of = |state: &SettingsState, idx: usize| -> Vec<String> {
            match &state.pages[editor_page_idx].items[idx].control {
                SettingControl::DualList { included, .. } => included.clone(),
                _ => panic!("expected a dual list"),
            }
        };
        let excluded_of = |state: &SettingsState, idx: usize| -> Vec<String> {
            match &state.pages[editor_page_idx].items[idx].control {
                SettingControl::DualList { excluded, .. } => excluded.clone(),
                _ => panic!("expected a dual list"),
            }
        };

        // On build, left.excluded mirrors right's included.
        assert_eq!(
            excluded_of(&state, left_idx),
            included_of(&state, right_idx)
        );

        // Carry the first Available entry into left's Included column.
        state.selected_item = left_idx;
        let before = included_of(&state, left_idx);
        state.dual_list_op(DualOp::Carry(true));
        let after = included_of(&state, left_idx);
        assert_eq!(after.len(), before.len() + 1, "the carry adds one entry");
        let moved = after.last().unwrap().clone();
        assert!(state.is_editing_dual_list(), "the control is live");
        assert!(
            state
                .pending_changes
                .contains_key("/editor/status_bar/left"),
            "the change is recorded"
        );
        assert!(
            excluded_of(&state, right_idx).contains(&moved),
            "right.excluded follows left's new inclusion"
        );

        // Escape hands the keyboard back; the value stays.
        state.leave_live_control();
        assert!(!state.is_editing_dual_list());
        assert_eq!(included_of(&state, left_idx), after);
    }

    /// A JSON editor's text is the model's as it is typed; Tab keeps a
    /// text that parses and puts back one that does not.
    #[test]
    fn a_json_editor_keeps_what_parses_and_reverts_what_does_not() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA, &config).unwrap();
        state.focus_panel = FocusPanel::Settings;
        // Put a JSON control on the current card.
        let path = state.current_item().unwrap().path.clone();
        state.current_item_mut().unwrap().control = SettingControl::Json {
            label: "Formatter".into(),
            text: "null".into(),
        };

        state.activate_control();
        assert!(state.is_editing_json());
        state.paste_into_focused_text("{\"a\": 1}");
        let text_of = |state: &SettingsState| match &state.current_item().unwrap().control {
            SettingControl::Json { text, .. } => text.clone(),
            _ => panic!("expected a JSON control"),
        };
        assert_eq!(text_of(&state), "{\"a\": 1}");
        assert!(
            !state.pending_changes.contains_key(&path),
            "recorded when the edit ends, not as it is typed"
        );
        state.json_exit_editing();
        assert!(!state.is_editing_json());
        assert_eq!(
            state.pending_changes.get(&path),
            Some(&serde_json::json!({ "a": 1 }))
        );

        // Break it (the paste lands at the caret, at the end), then leave:
        // the text comes back.
        state.activate_control();
        state.paste_into_focused_text("{");
        assert_eq!(text_of(&state), "{\"a\": 1}{");
        state.json_exit_editing();
        assert_eq!(text_of(&state), "{\"a\": 1}");
    }

    /// The search filter moves by grapheme cluster (like the Command
    /// Palette), so a single Left crosses a multi-codepoint Thai cluster
    /// rather than landing between its combining marks.
    #[test]
    fn test_search_cursor_moves_by_grapheme_cluster() {
        let config = test_config();
        let mut state = SettingsState::new(TEST_SCHEMA, &config).unwrap();

        // "aที่b": 'a' (1 byte) + Thai cluster "ที่" (9 bytes) + 'b' (1 byte)
        state.start_search();
        for c in "aที่b".chars() {
            state.search_insert_char(c);
        }
        let end = state.search_query().len();
        assert_eq!(state.search_cursor(), end);

        // Left: past 'b' (1 byte)
        state.search_cursor_left();
        assert_eq!(state.search_cursor(), end - 1);

        // Left: skip the whole Thai cluster in one step (9 bytes)
        state.search_cursor_left();
        assert_eq!(state.search_cursor(), 1);

        // Left: before 'a'
        state.search_cursor_left();
        assert_eq!(state.search_cursor(), 0);

        // Backspace at start is a no-op; the query is untouched
        state.search_backspace();
        assert_eq!(state.search_query(), "aที่b");

        // Delete at the start removes the leading 'a' only
        state.search_delete();
        assert_eq!(state.search_query(), "ที่b");
        assert_eq!(state.search_cursor(), 0);

        // One Right skips the Thai cluster; Delete then removes the 'b'
        state.search_cursor_right();
        assert_eq!(state.search_cursor(), "ที่".len());
        state.search_delete();
        assert_eq!(state.search_query(), "ที่");
    }

    // **The results' window and its selection are the list's now.** The four
    // tests here drove `search_scroll_up`/`_down`/`_to_ratio` and asserted
    // that none of them moved the selection and that keyboard navigation
    // re-revealed it (#2860). Both halves are the framework's contract —
    // `fresh-ui`'s conformance suite is where a wheel that moves a window
    // without moving a selection is proved — and the settings half, that the
    // window follows the selected result and reports where it ended up, is
    // `view::shell::settings::the_results_window_follows_the_selected_result`.
}
