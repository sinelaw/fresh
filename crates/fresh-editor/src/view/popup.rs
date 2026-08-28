//! **A popup's model, not its picture.** Everything that drew one — the block,
//! the close button, the description rows, the three content bodies, the
//! scrollbar, the hover and selection highlighting — is a description in
//! `view::shell::popup` now, and the fold paints it. What is left here is what
//! a popup *is*: its content, where it wants to go, how big it asks to be, what
//! its keys do, and the text selection the ledger's finding B keeps host-side.

use ratatui::{layout::Rect, style::Style};

use super::markdown::{parse_markdown, wrap_styled_lines, wrap_text_lines, StyledLine};

pub mod input;
use crate::primitives::grammar::GrammarRegistry;

/// Position of a popup relative to a point in the buffer
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PopupPosition {
    /// At cursor position
    AtCursor,
    /// Below cursor position
    BelowCursor,
    /// Above cursor position
    AboveCursor,
    /// Fixed screen coordinates (x, y)
    Fixed { x: u16, y: u16 },
    /// Centered on screen
    Centered,
    /// Centered floating overlay sized as a percentage of the frame,
    /// regardless of the content's natural size. Used by Live Grep
    /// (issue #1796) so the input row and preview pane stay anchored
    /// while results stream in. Both fields are clamped to 1..=100 by
    /// the renderer.
    CenteredOverlay { width_pct: u8, height_pct: u8 },
    /// Bottom right corner (above status bar)
    BottomRight,
    /// Anchored above the status bar at a specific column (left-aligned at x).
    /// Used by the LSP-status popup so it appears directly above the LSP
    /// segment that opened it. `status_row` is the actual row of the status
    /// bar in the current frame — passing it in lets the popup hug the
    /// status bar regardless of whether the prompt line is visible (which
    /// shifts the status bar by a row when it auto-hides).
    AboveStatusBarAt { x: u16, status_row: u16 },
}

/// Kind of popup - determines input handling behavior
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PopupKind {
    /// LSP completion popup - supports type-to-filter, Tab/Enter accept
    Completion,
    /// Hover/documentation popup - read-only, scroll, dismiss on keypress
    Hover,
    /// Action popup with selectable actions - navigate and execute
    Action,
    /// Generic list popup
    List,
    /// Generic text popup
    Text,
}

/// How `handle_popup_confirm` / `handle_popup_cancel` should resolve the
/// popup. Each variant names the feature that owns this popup — adding a
/// new popup flavour is "add a variant + a confirm/cancel branch," with
/// zero precedence ordering to maintain between unrelated features.
///
/// Stored on the `Popup` itself so the confirm dispatcher inspects the
/// *currently focused* popup (global or buffer) and routes by value. No
/// out-of-band `Option` on the Editor can silently claim an Enter
/// belonging to a different popup.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum PopupResolver {
    /// Generic popup with no feature-specific confirm/cancel logic —
    /// confirm/cancel simply dismiss the popup.
    #[default]
    None,
    /// LSP completion popup. Confirm inserts the selected item's text.
    Completion,
    /// "Start LSP server?" confirmation. Confirm dispatches the selected
    /// row's `data` (e.g. "allow_once") through
    /// `handle_lsp_confirmation_response`.
    LspConfirm { language: String },
    /// LSP server-status / auto-prompt popup. Confirm dispatches the
    /// selected row's `data` through `handle_lsp_status_action`.
    LspStatus,
    /// LSP code-action chooser. Selected row's `data` is the index into
    /// `Editor::pending_code_actions` (heavy `lsp_types` payload stays
    /// there to keep the view crate free of LSP types).
    CodeAction,
    /// Plugin-requested action popup (`editor.showActionPopup`). Confirm
    /// fires `action_popup_result` with this popup's id and the selected
    /// row's `data` as the action id.
    PluginAction { popup_id: String },
    /// Remote-authority indicator popup (Local / Connected / Disconnected
    /// context menu anchored to the status bar's `{remote}` element).
    /// Confirm dispatches the selected row's `data` through
    /// `handle_remote_indicator_action`.
    RemoteIndicator,
    /// Workspace-trust prompt (shown on opening an untrusted project that has
    /// executable content). Confirm dispatches the selected row's `data`
    /// ("trusted" / "restricted" / "blocked") through
    /// `handle_workspace_trust_action`.
    WorkspaceTrust,
    /// Read-only indicator menu (anchored to the status bar's `{read_only}`
    /// segment). Confirm dispatches the selected row's `data`
    /// ("toggle_read_only" / "cancel") through `handle_read_only_menu_action`.
    ReadOnly,
    /// Update-available menu (anchored to the status bar's `{update}` segment).
    /// Confirm dispatches the selected row's `data` ("update" / "cancel_popup")
    /// through `handle_update_menu_action`; "update" opens a local terminal that
    /// runs the update.
    Update,
    /// "Couldn't save settings" error popup. Acknowledging it (confirm or
    /// cancel) opens the offending config file for `layer` in a buffer so the
    /// user can fix the syntax error that blocked the save.
    SettingsSaveError {
        layer: crate::config_io::ConfigLayer,
    },
}

/// Content of a popup window
#[derive(Debug, Clone, PartialEq)]
pub enum PopupContent {
    /// Simple text content
    Text(Vec<String>),
    /// Markdown content with styling
    Markdown(Vec<StyledLine>),
    /// List of selectable items
    List {
        items: Vec<PopupListItem>,
        selected: usize,
    },
}

/// Text selection within a popup (line, column positions)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PopupTextSelection {
    /// Start position (line index, column index)
    pub start: (usize, usize),
    /// End position (line index, column index)
    pub end: (usize, usize),
}

impl PopupTextSelection {
    /// Get normalized selection (start <= end)
    pub fn normalized(&self) -> ((usize, usize), (usize, usize)) {
        if self.start.0 < self.end.0 || (self.start.0 == self.end.0 && self.start.1 <= self.end.1) {
            (self.start, self.end)
        } else {
            (self.end, self.start)
        }
    }

    /// Check if a position is within the selection
    pub fn contains(&self, line: usize, col: usize) -> bool {
        let ((start_line, start_col), (end_line, end_col)) = self.normalized();
        if line < start_line || line > end_line {
            return false;
        }
        if line == start_line && line == end_line {
            col >= start_col && col < end_col
        } else if line == start_line {
            col >= start_col
        } else if line == end_line {
            col < end_col
        } else {
            true
        }
    }
}

/// A single item in a popup list
#[derive(Debug, Clone, PartialEq)]
pub struct PopupListItem {
    /// Main text to display
    pub text: String,
    /// Optional secondary text (description, type info, etc.)
    pub detail: Option<String>,
    /// Optional icon or prefix
    pub icon: Option<String>,
    /// User data associated with this item (for completion, etc.)
    pub data: Option<String>,
    /// If true, item is rendered grayed-out and not selectable.
    pub disabled: bool,
}

impl PopupListItem {
    pub fn new(text: String) -> Self {
        Self {
            text,
            detail: None,
            icon: None,
            data: None,
            disabled: false,
        }
    }

    pub fn with_detail(mut self, detail: String) -> Self {
        self.detail = Some(detail);
        self
    }

    pub fn with_icon(mut self, icon: String) -> Self {
        self.icon = Some(icon);
        self
    }

    pub fn with_data(mut self, data: String) -> Self {
        self.data = Some(data);
        self
    }

    pub fn disabled(mut self) -> Self {
        self.disabled = true;
        self
    }
}

/// A popup/floating window
/// This is a general-purpose UI primitive that can be used for:
/// - Completion menus
/// - Hover documentation
/// - Command palette
/// - File picker
/// - Diagnostic messages
/// - Quick fixes / code actions
#[derive(Debug, Clone, PartialEq)]
pub struct Popup {
    /// Kind of popup - determines input handling behavior
    pub kind: PopupKind,

    /// Title of the popup (optional)
    pub title: Option<String>,

    /// Description text shown below title, above content (optional)
    pub description: Option<String>,

    /// Whether this popup is transient (dismissed on focus loss, e.g. hover, signature help)
    pub transient: bool,

    /// Content to display
    pub content: PopupContent,

    /// Position strategy
    pub position: PopupPosition,

    /// Width of popup (in columns)
    pub width: u16,

    /// Maximum height (will be clamped to available space)
    pub max_height: u16,

    /// Whether to show borders
    pub bordered: bool,

    /// Border style
    pub border_style: Style,

    /// Background style
    pub background_style: Style,

    /// Scroll offset for content (for scrolling through long lists)
    pub scroll_offset: usize,

    /// Text selection for copy/paste (None if no selection)
    pub text_selection: Option<PopupTextSelection>,

    /// Key hint shown right-aligned on the selected item (e.g. "(Tab)")
    pub accept_key_hint: Option<String>,

    /// Feature-specific resolver for confirm/cancel dispatch. Default
    /// `None` means "no special handling — just dismiss."
    pub resolver: PopupResolver,

    /// Whether the popup currently has keyboard focus.
    ///
    /// LSP-spawned popups (completion, hover, signature help, the
    /// LSP-server status auto-prompt) are created with `focused = false`
    /// so a popup that pops up under the user's cursor does not silently
    /// swallow their next keystroke. The user explicitly transfers
    /// focus to the popup with the `popup_focus` action (default
    /// binding `Alt+T`); only then do popup-context bindings apply.
    pub focused: bool,

    /// Pre-rendered key hint for the `popup_focus` action shown in the
    /// title when `focused == false` (e.g. `"Alt+T"`). `None` falls back
    /// to a built-in default at render time. Set by the editor when
    /// constructing the popup so the hint reflects the user's actual
    /// keybinding for `popup_focus`.
    pub focus_key_hint: Option<String>,
}

impl Popup {
    /// Create a new popup with text content using theme colors
    pub fn text(content: Vec<String>, theme: &crate::view::theme::Theme) -> Self {
        Self {
            kind: PopupKind::Text,
            title: None,
            description: None,
            transient: false,
            content: PopupContent::Text(content),
            position: PopupPosition::AtCursor,
            width: 50,
            max_height: 15,
            bordered: true,
            border_style: Style::default().fg(theme.popup_border_fg),
            background_style: Style::default().bg(theme.popup_bg),
            scroll_offset: 0,
            text_selection: None,
            accept_key_hint: None,
            resolver: PopupResolver::None,
            focused: false,
            focus_key_hint: None,
        }
    }

    /// Create a new popup with markdown content using theme colors
    ///
    /// If `registry` is provided, code blocks will have syntax highlighting
    /// for ~150+ languages via syntect.
    pub fn markdown(
        markdown_text: &str,
        theme: &crate::view::theme::Theme,
        registry: Option<&GrammarRegistry>,
    ) -> Self {
        let styled_lines = parse_markdown(markdown_text, theme, registry);
        Self {
            kind: PopupKind::Text,
            title: None,
            description: None,
            transient: false,
            content: PopupContent::Markdown(styled_lines),
            position: PopupPosition::AtCursor,
            width: 60,      // Wider for markdown content
            max_height: 20, // Taller for documentation
            bordered: true,
            border_style: Style::default().fg(theme.popup_border_fg),
            background_style: Style::default().bg(theme.popup_bg),
            scroll_offset: 0,
            text_selection: None,
            accept_key_hint: None,
            resolver: PopupResolver::None,
            focused: false,
            focus_key_hint: None,
        }
    }

    /// Create a new popup with a list of items using theme colors
    pub fn list(items: Vec<PopupListItem>, theme: &crate::view::theme::Theme) -> Self {
        Self {
            kind: PopupKind::List,
            title: None,
            description: None,
            transient: false,
            content: PopupContent::List { items, selected: 0 },
            position: PopupPosition::AtCursor,
            width: 50,
            max_height: 15,
            bordered: true,
            border_style: Style::default().fg(theme.popup_border_fg),
            background_style: Style::default().bg(theme.popup_bg),
            scroll_offset: 0,
            text_selection: None,
            accept_key_hint: None,
            resolver: PopupResolver::None,
            focused: false,
            focus_key_hint: None,
        }
    }

    /// Set the title
    pub fn with_title(mut self, title: String) -> Self {
        self.title = Some(title);
        self
    }

    /// Set the popup kind (determines input handling behavior)
    pub fn with_kind(mut self, kind: PopupKind) -> Self {
        self.kind = kind;
        self
    }

    /// Mark this popup as transient (will be dismissed on focus loss)
    pub fn with_transient(mut self, transient: bool) -> Self {
        self.transient = transient;
        self
    }

    /// Set the position
    pub fn with_position(mut self, position: PopupPosition) -> Self {
        self.position = position;
        self
    }

    /// Set the width
    pub fn with_width(mut self, width: u16) -> Self {
        self.width = width;
        self
    }

    /// Set the max height
    pub fn with_max_height(mut self, max_height: u16) -> Self {
        self.max_height = max_height;
        self
    }

    /// Set border style
    pub fn with_border_style(mut self, style: Style) -> Self {
        self.border_style = style;
        self
    }

    /// Attach the confirm/cancel resolver so this popup dispatches to
    /// the right handler regardless of what other popups are on screen.
    pub fn with_resolver(mut self, resolver: PopupResolver) -> Self {
        self.resolver = resolver;
        self
    }

    /// Mark the popup as keyboard-focused (so popup-context bindings
    /// route through it). LSP popups stay unfocused on creation; the
    /// user toggles focus with the `popup_focus` action.
    pub fn with_focused(mut self, focused: bool) -> Self {
        self.focused = focused;
        self
    }

    /// Pre-render the focus-key hint shown in the popup title when the
    /// popup is unfocused.
    pub fn with_focus_key_hint(mut self, hint: String) -> Self {
        self.focus_key_hint = Some(hint);
        self
    }

    /// Compose the title text actually shown on the popup border.
    ///
    /// When the popup is unfocused, the focus-key hint (e.g. `"Alt+T"`)
    /// is appended so the user knows how to grab the popup with the
    /// keyboard. The hint falls back to a built-in label when no
    /// `focus_key_hint` is set, so the title never reads as an empty
    /// parenthetical.
    pub fn render_title(&self) -> Option<String> {
        let hint_label = if !self.focused {
            let hint = self
                .focus_key_hint
                .clone()
                .unwrap_or_else(|| "Alt+T".to_string());
            Some(format!("[{} to focus]", hint))
        } else {
            None
        };
        match (&self.title, hint_label) {
            (Some(title), Some(hint)) => Some(format!("{} {}", title, hint)),
            (Some(title), None) => Some(title.clone()),
            (None, Some(hint)) => Some(hint),
            (None, None) => None,
        }
    }

    /// Get the currently selected item (if this is a list popup)
    pub fn selected_item(&self) -> Option<&PopupListItem> {
        match &self.content {
            PopupContent::List { items, selected } => items.get(*selected),
            _ => None,
        }
    }

    /// Index of the currently selected row (if this is a list popup and the
    /// row exists). Rows are indexed independently of `scroll_offset`, so
    /// this is a stable handle on the row the user is looking at — which is
    /// how the completion accept path tells apart candidates that share a
    /// label.
    pub fn selected_index(&self) -> Option<usize> {
        match &self.content {
            PopupContent::List { items, selected } if *selected < items.len() => Some(*selected),
            _ => None,
        }
    }

    /// Get the actual visible content height (accounting for borders)
    fn visible_height(&self) -> usize {
        let border_offset = if self.bordered { 2 } else { 0 };
        (self.max_height as usize).saturating_sub(border_offset)
    }

    /// Move selection down (for list popups)
    pub fn select_next(&mut self) {
        let visible = self.visible_height();
        if let PopupContent::List { items, selected } = &mut self.content {
            if *selected < items.len().saturating_sub(1) {
                *selected += 1;
                // Adjust scroll if needed (use visible_height to account for borders)
                if *selected >= self.scroll_offset + visible {
                    self.scroll_offset = (*selected + 1).saturating_sub(visible);
                }
            }
        }
    }

    /// Move selection up (for list popups)
    pub fn select_prev(&mut self) {
        if let PopupContent::List { items: _, selected } = &mut self.content {
            if *selected > 0 {
                *selected -= 1;
                // Adjust scroll if needed
                if *selected < self.scroll_offset {
                    self.scroll_offset = *selected;
                }
            }
        }
    }

    /// Select a specific item by index. Returns true if the index was valid.
    pub fn select_index(&mut self, index: usize) -> bool {
        let visible = self.visible_height();
        if let PopupContent::List { items, selected } = &mut self.content {
            if index < items.len() {
                *selected = index;
                // Adjust scroll to keep selection visible
                if *selected >= self.scroll_offset + visible {
                    self.scroll_offset = (*selected + 1).saturating_sub(visible);
                } else if *selected < self.scroll_offset {
                    self.scroll_offset = *selected;
                }
                return true;
            }
        }
        false
    }

    /// Scroll down by one page
    pub fn page_down(&mut self) {
        let visible = self.visible_height();
        if let PopupContent::List { items, selected } = &mut self.content {
            *selected = (*selected + visible).min(items.len().saturating_sub(1));
            self.scroll_offset = (*selected + 1).saturating_sub(visible);
        } else {
            self.scroll_offset += visible;
        }
    }

    /// Scroll up by one page
    pub fn page_up(&mut self) {
        let visible = self.visible_height();
        if let PopupContent::List { items: _, selected } = &mut self.content {
            *selected = selected.saturating_sub(visible);
            self.scroll_offset = *selected;
        } else {
            self.scroll_offset = self.scroll_offset.saturating_sub(visible);
        }
    }

    /// Select the first item (for list popups)
    pub fn select_first(&mut self) {
        if let PopupContent::List { items: _, selected } = &mut self.content {
            *selected = 0;
            self.scroll_offset = 0;
        } else {
            self.scroll_offset = 0;
        }
    }

    /// Select the last item (for list popups)
    pub fn select_last(&mut self) {
        let visible = self.visible_height();
        if let PopupContent::List { items, selected } = &mut self.content {
            *selected = items.len().saturating_sub(1);
            // Ensure the last item is visible
            if *selected >= visible {
                self.scroll_offset = (*selected + 1).saturating_sub(visible);
            }
        } else {
            // For non-list content, scroll to the end
            let content_height = self.item_count();
            if content_height > visible {
                self.scroll_offset = content_height.saturating_sub(visible);
            }
        }
    }

    /// Scroll by a delta amount (positive = down, negative = up).
    ///
    /// Used for mouse-wheel scrolling and for the scrollbar handlers, so it
    /// moves the **view only**: a `List` popup's selection stays on whatever
    /// entry it was on, even when that entry scrolls off-screen. Dragging the
    /// selection along with the viewport made the wheel silently retarget
    /// what Enter would commit; the keyboard paths (`select_next`,
    /// `page_down`, …) are the ones that move the selection, and they scroll
    /// the view to follow it.
    pub fn scroll_by(&mut self, delta: i32) {
        let content_len = self.wrapped_item_count();
        let visible = self.visible_height();
        let max_scroll = content_len.saturating_sub(visible);

        if delta < 0 {
            // Scroll up
            self.scroll_offset = self.scroll_offset.saturating_sub((-delta) as usize);
        } else {
            // Scroll down
            self.scroll_offset = (self.scroll_offset + delta as usize).min(max_scroll);
        }
    }

    /// Get the total number of items/lines in the popup
    pub fn item_count(&self) -> usize {
        match &self.content {
            PopupContent::Text(lines) => lines.len(),
            PopupContent::Markdown(lines) => lines.len(),
            PopupContent::List { items, .. } => items.len(),
        }
    }

    /// Get the total number of wrapped lines in the popup
    ///
    /// This accounts for line wrapping based on the popup width,
    /// which is necessary for correct scroll calculations.
    fn wrapped_item_count(&self) -> usize {
        // Calculate wrap width same as render: width - borders (2) - scrollbar (2)
        let border_width = if self.bordered { 2 } else { 0 };
        let scrollbar_width = 2; // 1 for scrollbar + 1 for spacing
        let wrap_width = (self.width as usize)
            .saturating_sub(border_width)
            .saturating_sub(scrollbar_width);

        if wrap_width == 0 {
            return self.item_count();
        }

        match &self.content {
            PopupContent::Text(lines) => wrap_text_lines(lines, wrap_width).len(),
            PopupContent::Markdown(styled_lines) => {
                wrap_styled_lines(styled_lines, wrap_width).len()
            }
            // Lists and custom content don't wrap
            PopupContent::List { items, .. } => items.len(),
        }
    }

    /// Start text selection at position (used for mouse click)
    pub fn start_selection(&mut self, line: usize, col: usize) {
        self.text_selection = Some(PopupTextSelection {
            start: (line, col),
            end: (line, col),
        });
    }

    /// Extend text selection to position (used for mouse drag)
    pub fn extend_selection(&mut self, line: usize, col: usize) {
        if let Some(ref mut sel) = self.text_selection {
            sel.end = (line, col);
        }
    }

    /// Clear text selection
    pub fn clear_selection(&mut self) {
        self.text_selection = None;
    }

    /// Check if popup has active text selection
    pub fn has_selection(&self) -> bool {
        if let Some(sel) = &self.text_selection {
            sel.start != sel.end
        } else {
            false
        }
    }

    /// Compute the effective content wrap width, replicating the logic
    /// from `render_with_hover` so line indices match visual positions.
    fn content_wrap_width(&self) -> usize {
        let border_width: u16 = if self.bordered { 2 } else { 0 };
        let inner_width = self.width.saturating_sub(border_width);
        let scrollbar_reserved: u16 = 2;
        let conservative_width = inner_width.saturating_sub(scrollbar_reserved) as usize;

        if conservative_width == 0 {
            return 0;
        }

        let visible_height = self.max_height.saturating_sub(border_width) as usize;
        let line_count = match &self.content {
            PopupContent::Text(lines) => wrap_text_lines(lines, conservative_width).len(),
            PopupContent::Markdown(styled_lines) => {
                wrap_styled_lines(styled_lines, conservative_width).len()
            }
            _ => self.item_count(),
        };

        let needs_scrollbar = line_count > visible_height && inner_width > scrollbar_reserved;

        if needs_scrollbar {
            conservative_width
        } else {
            inner_width as usize
        }
    }

    /// Get plain text lines from popup content, wrapped to match rendering.
    ///
    /// Selection coordinates are in wrapped-line space (visual positions),
    /// so this must wrap lines identically to how `render_with_hover` does.
    fn get_text_lines(&self) -> Vec<String> {
        let wrap_width = self.content_wrap_width();

        match &self.content {
            PopupContent::Text(lines) => {
                if wrap_width > 0 {
                    wrap_text_lines(lines, wrap_width)
                } else {
                    lines.clone()
                }
            }
            PopupContent::Markdown(styled_lines) => {
                if wrap_width > 0 {
                    wrap_styled_lines(styled_lines, wrap_width)
                        .iter()
                        .map(|sl| sl.plain_text())
                        .collect()
                } else {
                    styled_lines.iter().map(|sl| sl.plain_text()).collect()
                }
            }
            PopupContent::List { items, .. } => items.iter().map(|i| i.text.clone()).collect(),
        }
    }

    /// Get selected text from popup content
    pub fn get_selected_text(&self) -> Option<String> {
        let sel = self.text_selection.as_ref()?;
        if sel.start == sel.end {
            return None;
        }

        let ((start_line, start_col), (end_line, end_col)) = sel.normalized();
        let lines = self.get_text_lines();

        if start_line >= lines.len() {
            return None;
        }

        if start_line == end_line {
            let line = &lines[start_line];
            let end_col = end_col.min(line.len());
            let start_col = start_col.min(end_col);
            Some(line[start_col..end_col].to_string())
        } else {
            let mut result = String::new();
            // First line from start_col to end
            let first_line = &lines[start_line];
            result.push_str(&first_line[start_col.min(first_line.len())..]);
            result.push('\n');
            // Middle lines (full)
            for line in lines.iter().take(end_line).skip(start_line + 1) {
                result.push_str(line);
                result.push('\n');
            }
            // Last line from start to end_col
            if end_line < lines.len() {
                let last_line = &lines[end_line];
                result.push_str(&last_line[..end_col.min(last_line.len())]);
            }
            Some(result)
        }
    }

    /// Check if the popup needs a scrollbar (content exceeds visible area)
    pub fn needs_scrollbar(&self) -> bool {
        self.item_count() > self.visible_height()
    }

    /// Get scroll state for scrollbar rendering
    pub fn scroll_state(&self) -> (usize, usize, usize) {
        let total = self.item_count();
        let visible = self.visible_height();
        (total, visible, self.scroll_offset)
    }

    /// Find the link URL at a given relative position within the popup content area.
    /// `relative_col` and `relative_row` are relative to the inner content area (after borders).
    /// Returns None if:
    /// - The popup doesn't contain markdown content
    /// - The position doesn't have a link
    pub fn link_at_position(&self, relative_col: usize, relative_row: usize) -> Option<String> {
        let PopupContent::Markdown(styled_lines) = &self.content else {
            return None;
        };

        // Calculate the content width for wrapping
        let border_width = if self.bordered { 2 } else { 0 };
        let scrollbar_reserved = 2;
        let content_width = self
            .width
            .saturating_sub(border_width)
            .saturating_sub(scrollbar_reserved) as usize;

        // Wrap the styled lines
        let wrapped_lines = wrap_styled_lines(styled_lines, content_width);

        // Account for scroll offset
        let line_index = self.scroll_offset + relative_row;

        // Get the line at this position
        let line = wrapped_lines.get(line_index)?;

        // Find the link at the column position
        line.link_at_column(relative_col).map(|s| s.to_string())
    }

    /// Get the height of the description area (including blank line separator)
    /// Returns 0 if there is no description.
    pub fn description_height(&self) -> u16 {
        if let Some(desc) = &self.description {
            let border_width = if self.bordered { 2 } else { 0 };
            let scrollbar_reserved = 2;
            let content_width = self
                .width
                .saturating_sub(border_width)
                .saturating_sub(scrollbar_reserved) as usize;
            let desc_vec = vec![desc.clone()];
            let wrapped = wrap_text_lines(&desc_vec, content_width.saturating_sub(2));
            wrapped.len() as u16 + 1 // +1 for blank line after description
        } else {
            0
        }
    }

    /// Calculate the actual content height based on the popup content
    fn content_height(&self) -> u16 {
        // Use the popup's configured width for wrapping calculation
        self.content_height_for_width(self.width)
    }

    /// Calculate content height for a specific width, accounting for word wrapping
    fn content_height_for_width(&self, popup_width: u16) -> u16 {
        // Calculate the effective content width (accounting for borders and scrollbar)
        let border_width = if self.bordered { 2 } else { 0 };
        let scrollbar_reserved = 2; // Reserve space for potential scrollbar
        let content_width = popup_width
            .saturating_sub(border_width)
            .saturating_sub(scrollbar_reserved) as usize;

        // Calculate description height if present
        let description_lines = if let Some(desc) = &self.description {
            let desc_vec = vec![desc.clone()];
            let wrapped = wrap_text_lines(&desc_vec, content_width.saturating_sub(2));
            wrapped.len() as u16 + 1 // +1 for blank line after description
        } else {
            0
        };

        let content_lines = match &self.content {
            PopupContent::Text(lines) => {
                // Count wrapped lines
                wrap_text_lines(lines, content_width).len() as u16
            }
            PopupContent::Markdown(styled_lines) => {
                // Count wrapped styled lines
                wrap_styled_lines(styled_lines, content_width).len() as u16
            }
            PopupContent::List { items, .. } => items.len() as u16,
        };

        // Add border lines if bordered
        let border_height = if self.bordered { 2 } else { 0 };

        description_lines + content_lines + border_height
    }

    /// What this popup asks to occupy, in cells, given the area it sits in.
    ///
    /// All that is left of `calculate_area`. Its six strategies each computed
    /// this identically and then disagreed about *placement*, which is the
    /// shell tree's answer now — the one exception being `CenteredOverlay`,
    /// whose whole reason to exist is that it takes a percentage of the frame
    /// instead of measuring its own content, so Live Grep's box does not
    /// resize per keystroke.
    ///
    /// Measurement has not moved: the description states the size, the tree
    /// states where it goes.
    pub fn asked_size(&self, terminal_area: Rect) -> (u16, u16) {
        if let PopupPosition::CenteredOverlay {
            width_pct,
            height_pct,
        } = self.position
        {
            let pct = |extent: u16, p: u8| {
                (((extent as u32 * p.clamp(1, 100) as u32) / 100) as u16)
                    .max(1)
                    .min(extent)
            };
            return (
                pct(terminal_area.width, width_pct),
                pct(terminal_area.height, height_pct),
            );
        }
        (
            self.width.min(terminal_area.width),
            self.content_height()
                .min(self.max_height)
                .min(terminal_area.height),
        )
    }
}

/// Manager for popups - can show multiple popups with z-ordering
#[derive(Debug, Clone)]
pub struct PopupManager {
    /// Stack of active popups (top of stack = topmost popup)
    popups: Vec<Popup>,
}

impl PopupManager {
    pub fn new() -> Self {
        Self { popups: Vec::new() }
    }

    /// Show a popup (adds to top of stack)
    pub fn show(&mut self, popup: Popup) {
        self.popups.push(popup);
    }

    /// Show a popup, replacing any existing popup of the same kind.
    /// If a popup with the same `PopupKind` already exists in the stack,
    /// it is replaced in-place. Otherwise the new popup is pushed on top.
    pub fn show_or_replace(&mut self, popup: Popup) {
        if let Some(pos) = self.popups.iter().position(|p| p.kind == popup.kind) {
            self.popups[pos] = popup;
        } else {
            self.popups.push(popup);
        }
    }

    /// Hide the topmost popup
    pub fn hide(&mut self) -> Option<Popup> {
        self.popups.pop()
    }

    /// Clear all popups
    pub fn clear(&mut self) {
        self.popups.clear();
    }

    /// Get the topmost popup
    pub fn top(&self) -> Option<&Popup> {
        self.popups.last()
    }

    /// Get mutable reference to topmost popup
    pub fn top_mut(&mut self) -> Option<&mut Popup> {
        self.popups.last_mut()
    }

    /// Get reference to popup by index
    pub fn get(&self, index: usize) -> Option<&Popup> {
        self.popups.get(index)
    }

    /// Get mutable reference to popup by index
    pub fn get_mut(&mut self, index: usize) -> Option<&mut Popup> {
        self.popups.get_mut(index)
    }

    /// Check if any popups are visible
    pub fn is_visible(&self) -> bool {
        !self.popups.is_empty()
    }

    /// Check if the topmost popup is a completion popup (supports type-to-filter)
    pub fn is_completion_popup(&self) -> bool {
        self.top()
            .map(|p| p.kind == PopupKind::Completion)
            .unwrap_or(false)
    }

    /// Check if the topmost popup is a hover popup
    pub fn is_hover_popup(&self) -> bool {
        self.top()
            .map(|p| p.kind == PopupKind::Hover)
            .unwrap_or(false)
    }

    /// Check if the topmost popup is an action popup
    pub fn is_action_popup(&self) -> bool {
        self.top()
            .map(|p| p.kind == PopupKind::Action)
            .unwrap_or(false)
    }

    /// Get all popups (for rendering)
    pub fn all(&self) -> &[Popup] {
        &self.popups
    }

    /// Dismiss transient popups if present at the top.
    /// These popups should be dismissed when the buffer loses focus.
    /// Returns true if a popup was dismissed.
    pub fn dismiss_transient(&mut self) -> bool {
        let is_transient = self.popups.last().is_some_and(|p| p.transient);

        if is_transient {
            self.popups.pop();
            true
        } else {
            false
        }
    }
}

impl Default for PopupManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::theme;

    #[test]
    fn test_popup_list_item() {
        let item = PopupListItem::new("test".to_string())
            .with_detail("detail".to_string())
            .with_icon("📄".to_string());

        assert_eq!(item.text, "test");
        assert_eq!(item.detail, Some("detail".to_string()));
        assert_eq!(item.icon, Some("📄".to_string()));
    }

    #[test]
    fn test_popup_selection() {
        let theme = crate::view::theme::Theme::load_builtin(theme::THEME_DARK).unwrap();
        let items = vec![
            PopupListItem::new("item1".to_string()),
            PopupListItem::new("item2".to_string()),
            PopupListItem::new("item3".to_string()),
        ];

        let mut popup = Popup::list(items, &theme);

        assert_eq!(popup.selected_item().unwrap().text, "item1");

        popup.select_next();
        assert_eq!(popup.selected_item().unwrap().text, "item2");

        popup.select_next();
        assert_eq!(popup.selected_item().unwrap().text, "item3");

        popup.select_next(); // Should stay at last item
        assert_eq!(popup.selected_item().unwrap().text, "item3");

        popup.select_prev();
        assert_eq!(popup.selected_item().unwrap().text, "item2");

        popup.select_prev();
        assert_eq!(popup.selected_item().unwrap().text, "item1");

        popup.select_prev(); // Should stay at first item
        assert_eq!(popup.selected_item().unwrap().text, "item1");
    }

    #[test]
    fn test_popup_manager() {
        let theme = crate::view::theme::Theme::load_builtin(theme::THEME_DARK).unwrap();
        let mut manager = PopupManager::new();

        assert!(!manager.is_visible());
        assert_eq!(manager.top(), None);

        let popup1 = Popup::text(vec!["test1".to_string()], &theme);
        manager.show(popup1);

        assert!(manager.is_visible());
        assert_eq!(manager.all().len(), 1);

        let popup2 = Popup::text(vec!["test2".to_string()], &theme);
        manager.show(popup2);

        assert_eq!(manager.all().len(), 2);

        manager.hide();
        assert_eq!(manager.all().len(), 1);

        manager.clear();
        assert!(!manager.is_visible());
        assert_eq!(manager.all().len(), 0);
    }

    #[test]
    fn test_popup_text_selection() {
        let theme = crate::view::theme::Theme::load_builtin(theme::THEME_DARK).unwrap();
        let mut popup = Popup::text(
            vec![
                "Line 0: Hello".to_string(),
                "Line 1: World".to_string(),
                "Line 2: Test".to_string(),
            ],
            &theme,
        );

        // Initially no selection
        assert!(!popup.has_selection());
        assert_eq!(popup.get_selected_text(), None);

        // Start selection at line 0, col 8 ("Hello" starts at col 8)
        popup.start_selection(0, 8);
        assert!(!popup.has_selection()); // Selection start == end

        // Extend selection to line 1, col 8 ("World" starts at col 8)
        popup.extend_selection(1, 8);
        assert!(popup.has_selection());

        // Get selected text: "Hello\nLine 1: "
        let selected = popup.get_selected_text().unwrap();
        assert_eq!(selected, "Hello\nLine 1: ");

        // Clear selection
        popup.clear_selection();
        assert!(!popup.has_selection());
        assert_eq!(popup.get_selected_text(), None);

        // Test single-line selection
        popup.start_selection(1, 8);
        popup.extend_selection(1, 13); // "World"
        let selected = popup.get_selected_text().unwrap();
        assert_eq!(selected, "World");
    }

    #[test]
    fn test_popup_text_selection_contains() {
        let sel = PopupTextSelection {
            start: (1, 5),
            end: (2, 10),
        };

        // Line 0 - before selection
        assert!(!sel.contains(0, 5));

        // Line 1 - start of selection
        assert!(!sel.contains(1, 4)); // Before start col
        assert!(sel.contains(1, 5)); // At start
        assert!(sel.contains(1, 10)); // After start on same line

        // Line 2 - end of selection
        assert!(sel.contains(2, 0)); // Beginning of last line
        assert!(sel.contains(2, 9)); // Before end col
        assert!(!sel.contains(2, 10)); // At end (exclusive)
        assert!(!sel.contains(2, 11)); // After end

        // Line 3 - after selection
        assert!(!sel.contains(3, 0));
    }

    #[test]
    fn test_popup_text_selection_normalized() {
        // Forward selection
        let sel = PopupTextSelection {
            start: (1, 5),
            end: (2, 10),
        };
        let ((s_line, s_col), (e_line, e_col)) = sel.normalized();
        assert_eq!((s_line, s_col), (1, 5));
        assert_eq!((e_line, e_col), (2, 10));

        // Backward selection (user dragged up)
        let sel_backward = PopupTextSelection {
            start: (2, 10),
            end: (1, 5),
        };
        let ((s_line, s_col), (e_line, e_col)) = sel_backward.normalized();
        assert_eq!((s_line, s_col), (1, 5));
        assert_eq!((e_line, e_col), (2, 10));
    }
}
