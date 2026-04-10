//! Status bar and prompt/minibuffer rendering

use std::path::Path;

use crate::app::WarningLevel;
use crate::config::{StatusBarConfig, StatusBarElement};
use chrono::Timelike;
use crate::primitives::display_width::{char_width, str_width};
use crate::state::EditorState;
use crate::view::prompt::Prompt;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;
use ratatui::Frame;
use rust_i18n::t;

/// Categorization of how a rendered element should be styled and tracked for click detection.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ElementKind {
    /// Normal text using base status bar colors
    Normal,
    /// Line ending indicator (clickable)
    LineEnding,
    /// Encoding indicator (clickable)
    Encoding,
    /// Language indicator (clickable)
    Language,
    /// LSP status indicator (colored by warning level, clickable)
    Lsp,
    /// Warning badge (colored, clickable)
    WarningBadge,
    /// Update available indicator (highlighted)
    Update,
    /// Command palette shortcut hint (distinct style)
    Palette,
    /// Status message area (clickable to show history)
    Messages,
    /// Remote disconnected prefix (error colors)
    RemoteDisconnected,
    /// Keybinding hints
    KeybindHints,
}

/// A single rendered status bar element with its text and styling info.
struct RenderedElement {
    text: String,
    kind: ElementKind,
}

/// Layout information returned from status bar rendering for mouse click detection
#[derive(Debug, Clone, Default)]
pub struct StatusBarLayout {
    /// LSP indicator area (row, start_col, end_col) - None if no LSP indicator shown
    pub lsp_indicator: Option<(u16, u16, u16)>,
    /// Warning badge area (row, start_col, end_col) - None if no warnings
    pub warning_badge: Option<(u16, u16, u16)>,
    /// Line ending indicator area (row, start_col, end_col)
    pub line_ending_indicator: Option<(u16, u16, u16)>,
    /// Encoding indicator area (row, start_col, end_col)
    pub encoding_indicator: Option<(u16, u16, u16)>,
    /// Language indicator area (row, start_col, end_col)
    pub language_indicator: Option<(u16, u16, u16)>,
    /// Status message area (row, start_col, end_col) - clickable to show full history
    pub message_area: Option<(u16, u16, u16)>,
}

/// Status bar hover state for styling clickable indicators
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum StatusBarHover {
    #[default]
    None,
    /// Mouse is over the LSP indicator
    LspIndicator,
    /// Mouse is over the warning badge
    WarningBadge,
    /// Mouse is over the line ending indicator
    LineEndingIndicator,
    /// Mouse is over the encoding indicator
    EncodingIndicator,
    /// Mouse is over the language indicator
    LanguageIndicator,
    /// Mouse is over the status message area
    MessageArea,
}

/// Which search option checkbox is being hovered
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SearchOptionsHover {
    #[default]
    None,
    CaseSensitive,
    WholeWord,
    Regex,
    ConfirmEach,
}

/// Layout information for search options bar hit testing
#[derive(Debug, Clone, Default)]
pub struct SearchOptionsLayout {
    /// Row where the search options are rendered
    pub row: u16,
    /// Case Sensitive checkbox area (start_col, end_col)
    pub case_sensitive: Option<(u16, u16)>,
    /// Whole Word checkbox area (start_col, end_col)
    pub whole_word: Option<(u16, u16)>,
    /// Regex checkbox area (start_col, end_col)
    pub regex: Option<(u16, u16)>,
    /// Confirm Each checkbox area (start_col, end_col) - only present in replace mode
    pub confirm_each: Option<(u16, u16)>,
}

impl SearchOptionsLayout {
    /// Check which search option checkbox (if any) is at the given position
    pub fn checkbox_at(&self, x: u16, y: u16) -> Option<SearchOptionsHover> {
        if y != self.row {
            return None;
        }

        if let Some((start, end)) = self.case_sensitive {
            if x >= start && x < end {
                return Some(SearchOptionsHover::CaseSensitive);
            }
        }
        if let Some((start, end)) = self.whole_word {
            if x >= start && x < end {
                return Some(SearchOptionsHover::WholeWord);
            }
        }
        if let Some((start, end)) = self.regex {
            if x >= start && x < end {
                return Some(SearchOptionsHover::Regex);
            }
        }
        if let Some((start, end)) = self.confirm_each {
            if x >= start && x < end {
                return Some(SearchOptionsHover::ConfirmEach);
            }
        }
        None
    }
}

/// Result of truncating a path for display
#[derive(Debug, Clone)]
pub struct TruncatedPath {
    /// The first component of the path (e.g., "/home" or "C:\")
    pub prefix: String,
    /// Whether truncation occurred (if true, display "[...]" between prefix and suffix)
    pub truncated: bool,
    /// The last components of the path (e.g., "project/src")
    pub suffix: String,
}

impl TruncatedPath {
    /// Get the full display string (without styling)
    pub fn to_string_plain(&self) -> String {
        if self.truncated {
            format!("{}/[...]{}", self.prefix, self.suffix)
        } else {
            format!("{}{}", self.prefix, self.suffix)
        }
    }

    /// Get the display length
    pub fn display_len(&self) -> usize {
        if self.truncated {
            self.prefix.len() + "/[...]".len() + self.suffix.len()
        } else {
            self.prefix.len() + self.suffix.len()
        }
    }
}

/// Truncate a path for display, showing the first component, [...], and last components
///
/// For example, `/private/var/folders/p6/nlmq.../T/.tmpNYt4Fc/project/file.txt`
/// becomes `/private/[...]/project/file.txt`
///
/// # Arguments
/// * `path` - The path to truncate
/// * `max_len` - Maximum length for the display string
///
/// # Returns
/// A TruncatedPath struct with prefix, truncation indicator, and suffix
pub fn truncate_path(path: &Path, max_len: usize) -> TruncatedPath {
    let path_str = path.to_string_lossy();

    // If path fits, return as-is
    if path_str.len() <= max_len {
        return TruncatedPath {
            prefix: String::new(),
            truncated: false,
            suffix: path_str.to_string(),
        };
    }

    let components: Vec<&str> = path_str.split('/').filter(|s| !s.is_empty()).collect();

    if components.is_empty() {
        return TruncatedPath {
            prefix: "/".to_string(),
            truncated: false,
            suffix: String::new(),
        };
    }

    // Always keep the root and first component as prefix
    let prefix = if path_str.starts_with('/') {
        format!("/{}", components.first().unwrap_or(&""))
    } else {
        components.first().unwrap_or(&"").to_string()
    };

    // The "[...]/" takes 6 characters
    let ellipsis_len = "/[...]".len();

    // Calculate how much space we have for the suffix
    let available_for_suffix = max_len.saturating_sub(prefix.len() + ellipsis_len);

    if available_for_suffix < 5 || components.len() <= 1 {
        // Not enough space or only one component, just truncate the end
        let truncated_path = if path_str.len() > max_len.saturating_sub(3) {
            format!("{}...", &path_str[..max_len.saturating_sub(3)])
        } else {
            path_str.to_string()
        };
        return TruncatedPath {
            prefix: String::new(),
            truncated: false,
            suffix: truncated_path,
        };
    }

    // Build suffix from the last components that fit
    let mut suffix_parts: Vec<&str> = Vec::new();
    let mut suffix_len = 0;

    for component in components.iter().skip(1).rev() {
        let component_len = component.len() + 1; // +1 for the '/'
        if suffix_len + component_len <= available_for_suffix {
            suffix_parts.push(component);
            suffix_len += component_len;
        } else {
            break;
        }
    }

    suffix_parts.reverse();

    // If we included all remaining components, no truncation needed
    if suffix_parts.len() == components.len() - 1 {
        return TruncatedPath {
            prefix: String::new(),
            truncated: false,
            suffix: path_str.to_string(),
        };
    }

    let suffix = if suffix_parts.is_empty() {
        // Can't fit any suffix components, truncate the last component
        let last = components.last().unwrap_or(&"");
        let truncate_to = available_for_suffix.saturating_sub(4); // "/.." and some chars
        if truncate_to > 0 && last.len() > truncate_to {
            format!("/{}...", &last[..truncate_to])
        } else {
            format!("/{}", last)
        }
    } else {
        format!("/{}", suffix_parts.join("/"))
    };

    TruncatedPath {
        prefix,
        truncated: true,
        suffix,
    }
}

/// Truncate a string to fit within `max_width` display columns, appending "..." if truncated.
fn truncate_to_width(s: &str, max_width: usize) -> String {
    let width = str_width(s);
    if width <= max_width {
        return s.to_string();
    }
    let truncate_at = max_width.saturating_sub(3);
    if truncate_at == 0 {
        return if max_width >= 3 {
            "...".to_string()
        } else {
            s.chars().take(max_width).collect()
        };
    }
    let mut w = 0;
    let truncated: String = s
        .chars()
        .take_while(|ch| {
            let cw = char_width(*ch);
            if w + cw <= truncate_at {
                w += cw;
                true
            } else {
                false
            }
        })
        .collect();
    format!("{}...", truncated)
}

/// Renders the status bar and prompt/minibuffer
pub struct StatusBarRenderer;

impl StatusBarRenderer {
    /// Render only the status bar (without prompt)
    ///
    /// # Returns
    /// Layout information with positions of clickable indicators
    #[allow(clippy::too_many_arguments)]
    pub fn render_status_bar(
        frame: &mut Frame,
        area: Rect,
        state: &mut EditorState,
        cursors: &crate::model::cursor::Cursors,
        status_message: &Option<String>,
        plugin_status_message: &Option<String>,
        lsp_status: &str,
        theme: &crate::view::theme::Theme,
        display_name: &str,
        keybindings: &crate::input::keybindings::KeybindingResolver,
        chord_state: &[(crossterm::event::KeyCode, crossterm::event::KeyModifiers)],
        update_available: Option<&str>,
        warning_level: WarningLevel,
        general_warning_count: usize,
        hover: StatusBarHover,
        remote_connection: Option<&str>,
        session_name: Option<&str>,
        read_only: bool,
        status_bar_config: &StatusBarConfig,
        clock_blink_on: bool,
    ) -> StatusBarLayout {
        Self::render_status(
            frame,
            area,
            state,
            cursors,
            status_message,
            plugin_status_message,
            lsp_status,
            theme,
            display_name,
            keybindings,
            chord_state,
            update_available,
            warning_level,
            general_warning_count,
            hover,
            remote_connection,
            session_name,
            read_only,
            status_bar_config,
            clock_blink_on,
        )
    }

    /// Render the prompt/minibuffer
    pub fn render_prompt(
        frame: &mut Frame,
        area: Rect,
        prompt: &Prompt,
        theme: &crate::view::theme::Theme,
    ) {
        let base_style = Style::default().fg(theme.prompt_fg).bg(theme.prompt_bg);

        // Create spans for the prompt
        let mut spans = vec![Span::styled(prompt.message.clone(), base_style)];

        // If there's a selection, split the input into parts
        if let Some((sel_start, sel_end)) = prompt.selection_range() {
            let input = &prompt.input;

            // Text before selection
            if sel_start > 0 {
                spans.push(Span::styled(input[..sel_start].to_string(), base_style));
            }

            // Selected text (blue background for visibility, cursor remains visible)
            if sel_start < sel_end {
                // Use theme colors for selection to ensure consistency across themes
                let selection_style = Style::default()
                    .fg(theme.prompt_selection_fg)
                    .bg(theme.prompt_selection_bg);
                spans.push(Span::styled(
                    input[sel_start..sel_end].to_string(),
                    selection_style,
                ));
            }

            // Text after selection
            if sel_end < input.len() {
                spans.push(Span::styled(input[sel_end..].to_string(), base_style));
            }
        } else {
            // No selection, render entire input normally
            spans.push(Span::styled(prompt.input.clone(), base_style));
        }

        let line = Line::from(spans);
        let prompt_line = Paragraph::new(line).style(base_style);

        frame.render_widget(prompt_line, area);

        // Set cursor position in the prompt
        // Use display width (not byte length) for proper handling of:
        // - Double-width CJK characters
        // - Zero-width combining characters (Thai diacritics, etc.)
        let message_width = str_width(&prompt.message);
        let input_width_before_cursor = str_width(&prompt.input[..prompt.cursor_pos]);
        let cursor_x = (message_width + input_width_before_cursor) as u16;
        if cursor_x < area.width {
            frame.set_cursor_position((area.x + cursor_x, area.y));
        }
    }

    /// Render the file open prompt with colorized path
    /// Shows: "Open: /path/to/current/dir/filename" where the directory part is dimmed
    /// Long paths are truncated: "/private/[...]/project/" with [...] styled differently
    pub fn render_file_open_prompt(
        frame: &mut Frame,
        area: Rect,
        prompt: &Prompt,
        file_open_state: &crate::app::file_open::FileOpenState,
        theme: &crate::view::theme::Theme,
    ) {
        let base_style = Style::default().fg(theme.prompt_fg).bg(theme.prompt_bg);
        let dir_style = Style::default()
            .fg(theme.help_separator_fg)
            .bg(theme.prompt_bg);
        // Style for the [...] ellipsis - use a more visible color
        let ellipsis_style = Style::default()
            .fg(theme.menu_highlight_fg)
            .bg(theme.prompt_bg);

        let mut spans = Vec::new();

        // "Open: " prefix
        let open_prompt = t!("file.open_prompt").to_string();
        spans.push(Span::styled(open_prompt.clone(), base_style));

        // Calculate if we need to truncate
        // Only truncate if full path + input exceeds 90% of available width
        let prefix_len = str_width(&open_prompt);
        let dir_path = file_open_state.current_dir.to_string_lossy();
        let dir_path_len = dir_path.len() + 1; // +1 for trailing slash
        let input_len = prompt.input.len();
        let total_len = prefix_len + dir_path_len + input_len;
        let threshold = (area.width as usize * 90) / 100;

        // Truncate the path only if total length exceeds 90% of width
        let truncated = if total_len > threshold {
            // Calculate how much space we have for the path after truncation
            let available_for_path = threshold
                .saturating_sub(prefix_len)
                .saturating_sub(input_len);
            truncate_path(&file_open_state.current_dir, available_for_path)
        } else {
            // No truncation needed - return full path
            TruncatedPath {
                prefix: String::new(),
                truncated: false,
                suffix: dir_path.to_string(),
            }
        };

        // Build the directory display with separate spans for styling
        if truncated.truncated {
            // Prefix (dimmed)
            spans.push(Span::styled(truncated.prefix.clone(), dir_style));
            // Ellipsis "/[...]" (highlighted)
            spans.push(Span::styled("/[...]", ellipsis_style));
            // Suffix with trailing slash (dimmed)
            let suffix_with_slash = if truncated.suffix.ends_with('/') {
                truncated.suffix.clone()
            } else {
                format!("{}/", truncated.suffix)
            };
            spans.push(Span::styled(suffix_with_slash, dir_style));
        } else {
            // No truncation - just show the path with trailing slash
            let path_display = if truncated.suffix.ends_with('/') {
                truncated.suffix.clone()
            } else {
                format!("{}/", truncated.suffix)
            };
            spans.push(Span::styled(path_display, dir_style));
        }

        // User input (the filename part) - normal color
        spans.push(Span::styled(prompt.input.clone(), base_style));

        let line = Line::from(spans);
        let prompt_line = Paragraph::new(line).style(base_style);

        frame.render_widget(prompt_line, area);

        // Set cursor position in the prompt
        // Use display width for proper handling of Unicode characters
        // We need to calculate the visual width of: "Open: " + dir_display + input[..cursor_pos]
        let prefix_width = str_width(&open_prompt);
        let dir_display_width = if truncated.truncated {
            let suffix_with_slash = if truncated.suffix.ends_with('/') {
                &truncated.suffix
            } else {
                // We already added "/" in the suffix_with_slash above, so approximate
                &truncated.suffix
            };
            str_width(&truncated.prefix) + str_width("/[...]") + str_width(suffix_with_slash) + 1
        } else {
            str_width(&truncated.suffix) + 1 // +1 for trailing slash
        };
        let input_width_before_cursor = str_width(&prompt.input[..prompt.cursor_pos]);
        let cursor_x = (prefix_width + dir_display_width + input_width_before_cursor) as u16;
        if cursor_x < area.width {
            frame.set_cursor_position((area.x + cursor_x, area.y));
        }
    }

    /// Render a single element to its text representation.
    /// Returns None if the element has nothing to display.
    #[allow(clippy::too_many_arguments)]
    fn render_element(
        element: &StatusBarElement,
        state: &mut EditorState,
        cursors: &crate::model::cursor::Cursors,
        status_message: &Option<String>,
        plugin_status_message: &Option<String>,
        lsp_status: &str,
        display_name: &str,
        keybindings: &crate::input::keybindings::KeybindingResolver,
        chord_state: &[(crossterm::event::KeyCode, crossterm::event::KeyModifiers)],
        update_available: Option<&str>,
        general_warning_count: usize,
        remote_connection: Option<&str>,
        session_name: Option<&str>,
        read_only: bool,
        clock_blink_on: bool,
    ) -> Option<RenderedElement> {
        match element {
            StatusBarElement::Filename => {
                let modified = if state.buffer.is_modified() { " [+]" } else { "" };
                let read_only_indicator = if read_only { " [RO]" } else { "" };
                let remote_disconnected = remote_connection
                    .map(|conn| conn.contains("(Disconnected)"))
                    .unwrap_or(false);
                let remote_prefix = remote_connection
                    .map(|conn| format!("[SSH:{}] ", conn))
                    .unwrap_or_default();
                let session_prefix = session_name
                    .map(|name| format!("[{}] ", name))
                    .unwrap_or_default();
                let text = format!(
                    "{session_prefix}{remote_prefix}{display_name}{modified}{read_only_indicator}"
                );
                let kind = if remote_disconnected {
                    ElementKind::RemoteDisconnected
                } else {
                    ElementKind::Normal
                };
                Some(RenderedElement { text, kind })
            }
            StatusBarElement::Cursor => {
                if !state.show_cursors {
                    return None;
                }
                let cursor = *cursors.primary();
                let byte_offset_mode = state.buffer.line_count().is_none();
                let text = if byte_offset_mode {
                    format!("Byte {}", cursor.position)
                } else {
                    let cursor_iter = state.buffer.line_iterator(cursor.position, 80);
                    let line_start = cursor_iter.current_position();
                    let col = cursor.position.saturating_sub(line_start);
                    let line = state.primary_cursor_line_number.value();
                    format!("Ln {}, Col {}", line + 1, col + 1)
                };
                Some(RenderedElement { text, kind: ElementKind::Normal })
            }
            StatusBarElement::CursorCompact => {
                if !state.show_cursors {
                    return None;
                }
                let cursor = *cursors.primary();
                let byte_offset_mode = state.buffer.line_count().is_none();
                let text = if byte_offset_mode {
                    format!("{}", cursor.position)
                } else {
                    let cursor_iter = state.buffer.line_iterator(cursor.position, 80);
                    let line_start = cursor_iter.current_position();
                    let col = cursor.position.saturating_sub(line_start);
                    let line = state.primary_cursor_line_number.value();
                    format!("{}:{}", line + 1, col + 1)
                };
                Some(RenderedElement { text, kind: ElementKind::Normal })
            }
            StatusBarElement::Diagnostics => {
                let diagnostics = state.overlays.all();
                let mut error_count = 0usize;
                let mut warning_count = 0usize;
                let mut info_count = 0usize;
                let diagnostic_ns =
                    crate::services::lsp::diagnostics::lsp_diagnostic_namespace();
                for overlay in diagnostics {
                    if overlay.namespace.as_ref() == Some(&diagnostic_ns) {
                        match overlay.priority {
                            100 => error_count += 1,
                            50 => warning_count += 1,
                            _ => info_count += 1,
                        }
                    }
                }
                if error_count + warning_count + info_count == 0 {
                    return None;
                }
                let mut parts = Vec::new();
                if error_count > 0 { parts.push(format!("E:{}", error_count)); }
                if warning_count > 0 { parts.push(format!("W:{}", warning_count)); }
                if info_count > 0 { parts.push(format!("I:{}", info_count)); }
                Some(RenderedElement { text: parts.join(" "), kind: ElementKind::Normal })
            }
            StatusBarElement::CursorCount => {
                if cursors.count() <= 1 { return None; }
                Some(RenderedElement {
                    text: t!("status.cursors", count = cursors.count()).to_string(),
                    kind: ElementKind::Normal,
                })
            }
            StatusBarElement::Messages => {
                let mut parts: Vec<&str> = Vec::new();
                if let Some(msg) = status_message {
                    if !msg.is_empty() { parts.push(msg); }
                }
                if let Some(msg) = plugin_status_message {
                    if !msg.is_empty() { parts.push(msg); }
                }
                if parts.is_empty() { return None; }
                Some(RenderedElement {
                    text: parts.join(" | "),
                    kind: ElementKind::Messages,
                })
            }
            StatusBarElement::Chord => {
                if chord_state.is_empty() { return None; }
                let chord_str = chord_state
                    .iter()
                    .map(|(code, modifiers)| {
                        crate::input::keybindings::format_keybinding(code, modifiers)
                    })
                    .collect::<Vec<_>>()
                    .join(" ");
                Some(RenderedElement {
                    text: format!("[{}]", chord_str),
                    kind: ElementKind::Normal,
                })
            }
            StatusBarElement::LineEnding => Some(RenderedElement {
                text: format!(" {} ", state.buffer.line_ending().display_name()),
                kind: ElementKind::LineEnding,
            }),
            StatusBarElement::Encoding => Some(RenderedElement {
                text: format!(" {} ", state.buffer.encoding().display_name()),
                kind: ElementKind::Encoding,
            }),
            StatusBarElement::Language => {
                let text = if state.language == "text"
                    && state.display_name != "Text"
                    && state.display_name != "Plain Text"
                    && state.display_name != "text"
                {
                    format!(" {} [syntax only] ", &state.display_name)
                } else {
                    format!(" {} ", &state.display_name)
                };
                Some(RenderedElement { text, kind: ElementKind::Language })
            }
            StatusBarElement::Lsp => {
                if lsp_status.is_empty() { return None; }
                Some(RenderedElement {
                    text: format!(" {} ", lsp_status),
                    kind: ElementKind::Lsp,
                })
            }
            StatusBarElement::Warnings => {
                if general_warning_count == 0 { return None; }
                Some(RenderedElement {
                    text: format!(" [\u{26a0} {}] ", general_warning_count),
                    kind: ElementKind::WarningBadge,
                })
            }
            StatusBarElement::Update => {
                let version = update_available?;
                Some(RenderedElement {
                    text: format!(" {} ", t!("status.update_available", version = version)),
                    kind: ElementKind::Update,
                })
            }
            StatusBarElement::Palette => {
                let shortcut = keybindings
                    .get_keybinding_for_action(
                        &crate::input::keybindings::Action::QuickOpen,
                        crate::input::keybindings::KeyContext::Global,
                    )
                    .unwrap_or_else(|| "?".to_string());
                Some(RenderedElement {
                    text: format!(" {} ", t!("status.palette", shortcut = shortcut)),
                    kind: ElementKind::Palette,
                })
            }
            StatusBarElement::KeybindHints => {
                let hints = Self::build_keybind_hints(keybindings);
                if hints.is_empty() { return None; }
                Some(RenderedElement { text: hints, kind: ElementKind::KeybindHints })
            }
            StatusBarElement::Clock => {
                let now = chrono::Local::now();
                let sep = if clock_blink_on { ':' } else { ' ' };
                let text = format!("{:02}{}{:02}", now.hour(), sep, now.minute());
                Some(RenderedElement { text, kind: ElementKind::Normal })
            }
        }
    }

    /// Build nano-style keybinding hints string
    fn build_keybind_hints(
        keybindings: &crate::input::keybindings::KeybindingResolver,
    ) -> String {
        use crate::input::keybindings::{Action, KeyContext};
        let hint_actions = [
            (Action::Save, "Save"),
            (Action::Quit, "Exit"),
            (Action::Search, "Search"),
            (Action::Undo, "Undo"),
            (Action::Redo, "Redo"),
            (Action::Copy, "Copy"),
            (Action::Paste, "Paste"),
            (Action::Cut, "Cut"),
        ];
        let mut parts = Vec::new();
        for (action, label) in &hint_actions {
            if let Some(key) = keybindings
                .get_keybinding_for_action(action, KeyContext::Normal)
                .or_else(|| keybindings.get_keybinding_for_action(action, KeyContext::Global))
            {
                parts.push(format!("{} {}", key, label));
            }
        }
        parts.join("  ")
    }

    /// Get the style for a rendered element based on its kind, theme, and hover state.
    fn element_style(
        kind: ElementKind,
        theme: &crate::view::theme::Theme,
        hover: StatusBarHover,
        warning_level: WarningLevel,
    ) -> Style {
        match kind {
            ElementKind::Normal | ElementKind::KeybindHints | ElementKind::Messages => {
                Style::default().fg(theme.status_bar_fg).bg(theme.status_bar_bg)
            }
            ElementKind::RemoteDisconnected => Style::default()
                .fg(theme.status_error_indicator_fg)
                .bg(theme.status_error_indicator_bg),
            ElementKind::LineEnding => {
                let is_hovering = hover == StatusBarHover::LineEndingIndicator;
                let (fg, bg) = if is_hovering {
                    (theme.menu_hover_fg, theme.menu_hover_bg)
                } else {
                    (theme.status_bar_fg, theme.status_bar_bg)
                };
                let mut style = Style::default().fg(fg).bg(bg);
                if is_hovering { style = style.add_modifier(Modifier::UNDERLINED); }
                style
            }
            ElementKind::Encoding => {
                let is_hovering = hover == StatusBarHover::EncodingIndicator;
                let (fg, bg) = if is_hovering {
                    (theme.menu_hover_fg, theme.menu_hover_bg)
                } else {
                    (theme.status_bar_fg, theme.status_bar_bg)
                };
                let mut style = Style::default().fg(fg).bg(bg);
                if is_hovering { style = style.add_modifier(Modifier::UNDERLINED); }
                style
            }
            ElementKind::Language => {
                let is_hovering = hover == StatusBarHover::LanguageIndicator;
                let (fg, bg) = if is_hovering {
                    (theme.menu_hover_fg, theme.menu_hover_bg)
                } else {
                    (theme.status_bar_fg, theme.status_bar_bg)
                };
                let mut style = Style::default().fg(fg).bg(bg);
                if is_hovering { style = style.add_modifier(Modifier::UNDERLINED); }
                style
            }
            ElementKind::Lsp => {
                let is_hovering = hover == StatusBarHover::LspIndicator;
                let (fg, bg) = match (warning_level, is_hovering) {
                    (WarningLevel::Error, true) => (
                        theme.status_error_indicator_hover_fg,
                        theme.status_error_indicator_hover_bg,
                    ),
                    (WarningLevel::Error, false) => (
                        theme.status_error_indicator_fg,
                        theme.status_error_indicator_bg,
                    ),
                    (WarningLevel::Warning, true) => (
                        theme.status_warning_indicator_hover_fg,
                        theme.status_warning_indicator_hover_bg,
                    ),
                    (WarningLevel::Warning, false) => (
                        theme.status_warning_indicator_fg,
                        theme.status_warning_indicator_bg,
                    ),
                    (WarningLevel::None, _) => (theme.status_bar_fg, theme.status_bar_bg),
                };
                let mut style = Style::default().fg(fg).bg(bg);
                if is_hovering && warning_level != WarningLevel::None {
                    style = style.add_modifier(Modifier::UNDERLINED);
                }
                style
            }
            ElementKind::WarningBadge => {
                let is_hovering = hover == StatusBarHover::WarningBadge;
                let (fg, bg) = if is_hovering {
                    (theme.status_warning_indicator_hover_fg, theme.status_warning_indicator_hover_bg)
                } else {
                    (theme.status_warning_indicator_fg, theme.status_warning_indicator_bg)
                };
                let mut style = Style::default().fg(fg).bg(bg);
                if is_hovering { style = style.add_modifier(Modifier::UNDERLINED); }
                style
            }
            ElementKind::Update => Style::default()
                .fg(theme.menu_highlight_fg)
                .bg(theme.menu_dropdown_bg),
            ElementKind::Palette => Style::default()
                .fg(theme.help_indicator_fg)
                .bg(theme.help_indicator_bg),
        }
    }

    /// Map an ElementKind to the layout field it should populate.
    fn update_layout_for_element(
        layout: &mut StatusBarLayout,
        kind: ElementKind,
        row: u16,
        start_col: u16,
        end_col: u16,
    ) {
        match kind {
            ElementKind::LineEnding => layout.line_ending_indicator = Some((row, start_col, end_col)),
            ElementKind::Encoding => layout.encoding_indicator = Some((row, start_col, end_col)),
            ElementKind::Language => layout.language_indicator = Some((row, start_col, end_col)),
            ElementKind::Lsp => layout.lsp_indicator = Some((row, start_col, end_col)),
            ElementKind::WarningBadge => layout.warning_badge = Some((row, start_col, end_col)),
            ElementKind::Messages => layout.message_area = Some((row, start_col, end_col)),
            _ => {}
        }
    }

    /// Render the normal status bar (config-driven)
    #[allow(clippy::too_many_arguments)]
    fn render_status(
        frame: &mut Frame,
        area: Rect,
        state: &mut EditorState,
        cursors: &crate::model::cursor::Cursors,
        status_message: &Option<String>,
        plugin_status_message: &Option<String>,
        lsp_status: &str,
        theme: &crate::view::theme::Theme,
        display_name: &str,
        keybindings: &crate::input::keybindings::KeybindingResolver,
        chord_state: &[(crossterm::event::KeyCode, crossterm::event::KeyModifiers)],
        update_available: Option<&str>,
        warning_level: WarningLevel,
        general_warning_count: usize,
        hover: StatusBarHover,
        remote_connection: Option<&str>,
        session_name: Option<&str>,
        read_only: bool,
        config: &StatusBarConfig,
        clock_blink_on: bool,
    ) -> StatusBarLayout {
        let mut layout = StatusBarLayout::default();
        let base_style = Style::default()
            .fg(theme.status_bar_fg)
            .bg(theme.status_bar_bg);

        let lines = config.lines.max(1);
        let rows_available = area.height as usize;

        // Render each row of the status bar
        for row_idx in 0..lines.min(rows_available) {
            let row_area = Rect {
                x: area.x,
                y: area.y + row_idx as u16,
                width: area.width,
                height: 1,
            };

            if row_idx == 0 {
                Self::render_status_row(
                    frame, row_area, &mut layout, &config.left, &config.right,
                    state, cursors, status_message, plugin_status_message,
                    lsp_status, theme, display_name, keybindings, chord_state,
                    update_available, warning_level, general_warning_count,
                    hover, remote_connection, session_name, read_only,
                    clock_blink_on,
                );
            } else if config.show_keybind_hints && row_idx == 1 {
                let hints = Self::build_keybind_hints(keybindings);
                let available = row_area.width as usize;
                let displayed = truncate_to_width(&hints, available);
                let displayed_width = str_width(&displayed);
                let mut spans = vec![Span::styled(displayed, base_style)];
                if displayed_width < available {
                    spans.push(Span::styled(
                        " ".repeat(available - displayed_width),
                        base_style,
                    ));
                }
                frame.render_widget(Paragraph::new(Line::from(spans)), row_area);
            } else {
                let spans = vec![Span::styled(
                    " ".repeat(row_area.width as usize),
                    base_style,
                )];
                frame.render_widget(Paragraph::new(Line::from(spans)), row_area);
            }
        }

        layout
    }

    /// Render a single status bar row with left and right element containers.
    #[allow(clippy::too_many_arguments)]
    fn render_status_row(
        frame: &mut Frame,
        area: Rect,
        layout: &mut StatusBarLayout,
        left_elements: &[StatusBarElement],
        right_elements: &[StatusBarElement],
        state: &mut EditorState,
        cursors: &crate::model::cursor::Cursors,
        status_message: &Option<String>,
        plugin_status_message: &Option<String>,
        lsp_status: &str,
        theme: &crate::view::theme::Theme,
        display_name: &str,
        keybindings: &crate::input::keybindings::KeybindingResolver,
        chord_state: &[(crossterm::event::KeyCode, crossterm::event::KeyModifiers)],
        update_available: Option<&str>,
        warning_level: WarningLevel,
        general_warning_count: usize,
        hover: StatusBarHover,
        remote_connection: Option<&str>,
        session_name: Option<&str>,
        read_only: bool,
        clock_blink_on: bool,
    ) {
        let base_style = Style::default()
            .fg(theme.status_bar_fg)
            .bg(theme.status_bar_bg);
        let available_width = area.width as usize;

        if available_width == 0 {
            return;
        }

        // Render all left elements
        let left_rendered: Vec<RenderedElement> = left_elements
            .iter()
            .filter_map(|elem| Self::render_element(
                elem, state, cursors, status_message, plugin_status_message,
                lsp_status, display_name, keybindings, chord_state,
                update_available, general_warning_count, remote_connection,
                session_name, read_only, clock_blink_on,
            ))
            .collect();

        // Render all right elements
        let right_rendered: Vec<RenderedElement> = right_elements
            .iter()
            .filter_map(|elem| Self::render_element(
                elem, state, cursors, status_message, plugin_status_message,
                lsp_status, display_name, keybindings, chord_state,
                update_available, general_warning_count, remote_connection,
                session_name, read_only, clock_blink_on,
            ))
            .collect();

        // Build left text with " | " separators
        let left_text = Self::join_left_elements(&left_rendered);

        // Calculate right side total width
        let right_width: usize = right_rendered.iter().map(|e| str_width(&e.text)).sum();

        // If terminal is too narrow, just show truncated left text
        if available_width < 15 {
            let displayed = truncate_to_width(&left_text, available_width);
            let displayed_width = str_width(&displayed);
            let mut spans = vec![Span::styled(displayed, base_style)];
            if displayed_width < available_width {
                spans.push(Span::styled(
                    " ".repeat(available_width - displayed_width),
                    base_style,
                ));
            }
            frame.render_widget(Paragraph::new(Line::from(spans)), area);
            return;
        }

        // Reserve space for right side
        let left_max_width = if available_width > right_width + 1 {
            available_width - right_width - 1
        } else {
            1
        };

        // Truncate left text if needed
        let displayed_left = truncate_to_width(&left_text, left_max_width);
        let displayed_left_width = str_width(&displayed_left);

        // Build spans for left side, with special handling for disconnected remote
        let mut spans = Vec::new();
        let has_disconnected_remote = left_rendered
            .first()
            .map(|e| e.kind == ElementKind::RemoteDisconnected)
            .unwrap_or(false);

        if has_disconnected_remote && displayed_left.starts_with("[SSH:") {
            if let Some(prefix_end) = displayed_left.find("] ") {
                let prefix = &displayed_left[..prefix_end + 2];
                let rest = &displayed_left[prefix_end + 2..];
                spans.push(Span::styled(
                    prefix.to_string(),
                    Style::default()
                        .fg(theme.status_error_indicator_fg)
                        .bg(theme.status_error_indicator_bg),
                ));
                spans.push(Span::styled(rest.to_string(), base_style));
            } else {
                spans.push(Span::styled(
                    displayed_left.clone(),
                    Style::default()
                        .fg(theme.status_error_indicator_fg)
                        .bg(theme.status_error_indicator_bg),
                ));
            }
        } else {
            spans.push(Span::styled(displayed_left.clone(), base_style));
        }

        // Track message area for click detection
        Self::track_left_message_area(layout, &left_rendered, &displayed_left, area);

        // Add padding between left and right
        let mut col_offset = displayed_left_width;
        if col_offset + right_width < available_width {
            let padding = available_width - col_offset - right_width;
            spans.push(Span::styled(" ".repeat(padding), base_style));
            col_offset = available_width - right_width;
        } else if col_offset < available_width {
            spans.push(Span::styled(" ", base_style));
            col_offset += 1;
        }

        // Add right side elements with proper styling and layout tracking
        let mut current_col = area.x + col_offset as u16;
        for rendered in &right_rendered {
            let elem_width = str_width(&rendered.text) as u16;
            let style = Self::element_style(rendered.kind, theme, hover, warning_level);
            Self::update_layout_for_element(
                layout, rendered.kind, area.y, current_col, current_col + elem_width,
            );
            spans.push(Span::styled(rendered.text.clone(), style));
            current_col += elem_width;
        }

        frame.render_widget(Paragraph::new(Line::from(spans)), area);
    }

    /// Join left-side elements with " | " separators.
    fn join_left_elements(elements: &[RenderedElement]) -> String {
        let mut parts: Vec<&str> = Vec::new();
        for rendered in elements {
            if !rendered.text.is_empty() {
                parts.push(&rendered.text);
            }
        }
        parts.join(" | ")
    }

    /// Track the message area position within the displayed left text for click detection.
    fn track_left_message_area(
        layout: &mut StatusBarLayout,
        left_rendered: &[RenderedElement],
        displayed_left: &str,
        area: Rect,
    ) {
        let mut offset: usize = 0;
        let mut has_prev = false;
        for rendered in left_rendered {
            if rendered.text.is_empty() {
                continue;
            }
            if has_prev {
                offset += 3; // " | "
            }
            if rendered.kind == ElementKind::Messages {
                let displayed_width = str_width(displayed_left);
                let msg_start = offset.min(displayed_width);
                let msg_end = (offset + str_width(&rendered.text)).min(displayed_width);
                if msg_end > msg_start {
                    layout.message_area = Some((
                        area.y,
                        area.x + msg_start as u16,
                        area.x + msg_end as u16,
                    ));
                }
                return;
            }
            offset += str_width(&rendered.text);
            has_prev = true;
        }
    }

    /// Render the search options bar (shown when search prompt is active)
    ///
    /// Displays checkboxes for search options with their keyboard shortcuts:
    /// - Case Sensitive (Alt+C)
    /// - Whole Word (Alt+W)
    /// - Regex (Alt+R)
    /// - Confirm Each (Alt+I) - only shown in replace mode
    ///
    /// # Returns
    /// Layout information for hit testing mouse clicks on checkboxes
    #[allow(clippy::too_many_arguments)]
    pub fn render_search_options(
        frame: &mut Frame,
        area: Rect,
        case_sensitive: bool,
        whole_word: bool,
        use_regex: bool,
        confirm_each: Option<bool>, // None = don't show, Some(value) = show with this state
        theme: &crate::view::theme::Theme,
        keybindings: &crate::input::keybindings::KeybindingResolver,
        hover: SearchOptionsHover,
    ) -> SearchOptionsLayout {
        use crate::primitives::display_width::str_width;

        let mut layout = SearchOptionsLayout {
            row: area.y,
            ..Default::default()
        };

        // Use menu dropdown background (dark gray) for the options bar
        let base_style = Style::default()
            .fg(theme.menu_dropdown_fg)
            .bg(theme.menu_dropdown_bg);

        // Style for hovered options - use menu hover colors
        let hover_style = Style::default()
            .fg(theme.menu_hover_fg)
            .bg(theme.menu_hover_bg);

        // Helper to look up keybinding for an action (Prompt context first, then Global)
        let get_shortcut = |action: &crate::input::keybindings::Action| -> Option<String> {
            keybindings
                .get_keybinding_for_action(action, crate::input::keybindings::KeyContext::Prompt)
                .or_else(|| {
                    keybindings.get_keybinding_for_action(
                        action,
                        crate::input::keybindings::KeyContext::Global,
                    )
                })
        };

        // Get keybindings for search options
        let case_shortcut =
            get_shortcut(&crate::input::keybindings::Action::ToggleSearchCaseSensitive);
        let word_shortcut = get_shortcut(&crate::input::keybindings::Action::ToggleSearchWholeWord);
        let regex_shortcut = get_shortcut(&crate::input::keybindings::Action::ToggleSearchRegex);

        // Build the options display with checkboxes
        let case_checkbox = if case_sensitive { "[x]" } else { "[ ]" };
        let word_checkbox = if whole_word { "[x]" } else { "[ ]" };
        let regex_checkbox = if use_regex { "[x]" } else { "[ ]" };

        // Style for active (checked) options - highlighted with menu highlight colors
        let active_style = Style::default()
            .fg(theme.menu_highlight_fg)
            .bg(theme.menu_dropdown_bg);

        // Style for keyboard shortcuts - use theme color for consistency
        let shortcut_style = Style::default()
            .fg(theme.help_separator_fg)
            .bg(theme.menu_dropdown_bg);

        // Hovered shortcut style
        let hover_shortcut_style = Style::default()
            .fg(theme.menu_hover_fg)
            .bg(theme.menu_hover_bg);

        let mut spans = Vec::new();
        let mut current_col = area.x;

        // Left padding
        spans.push(Span::styled(" ", base_style));
        current_col += 1;

        // Helper to get style based on hover and checked state
        let get_checkbox_style = |is_hovered: bool, is_checked: bool| -> Style {
            if is_hovered {
                hover_style
            } else if is_checked {
                active_style
            } else {
                base_style
            }
        };

        // Case Sensitive option
        let case_hovered = hover == SearchOptionsHover::CaseSensitive;
        let case_start = current_col;
        let case_label = format!("{} {}", case_checkbox, t!("search.case_sensitive"));
        let case_shortcut_text = case_shortcut
            .as_ref()
            .map(|s| format!(" ({})", s))
            .unwrap_or_default();
        let case_full_width = str_width(&case_label) + str_width(&case_shortcut_text);

        spans.push(Span::styled(
            case_label,
            get_checkbox_style(case_hovered, case_sensitive),
        ));
        if !case_shortcut_text.is_empty() {
            spans.push(Span::styled(
                case_shortcut_text,
                if case_hovered {
                    hover_shortcut_style
                } else {
                    shortcut_style
                },
            ));
        }
        current_col += case_full_width as u16;
        layout.case_sensitive = Some((case_start, current_col));

        // Separator
        spans.push(Span::styled("   ", base_style));
        current_col += 3;

        // Whole Word option
        let word_hovered = hover == SearchOptionsHover::WholeWord;
        let word_start = current_col;
        let word_label = format!("{} {}", word_checkbox, t!("search.whole_word"));
        let word_shortcut_text = word_shortcut
            .as_ref()
            .map(|s| format!(" ({})", s))
            .unwrap_or_default();
        let word_full_width = str_width(&word_label) + str_width(&word_shortcut_text);

        spans.push(Span::styled(
            word_label,
            get_checkbox_style(word_hovered, whole_word),
        ));
        if !word_shortcut_text.is_empty() {
            spans.push(Span::styled(
                word_shortcut_text,
                if word_hovered {
                    hover_shortcut_style
                } else {
                    shortcut_style
                },
            ));
        }
        current_col += word_full_width as u16;
        layout.whole_word = Some((word_start, current_col));

        // Separator
        spans.push(Span::styled("   ", base_style));
        current_col += 3;

        // Regex option
        let regex_hovered = hover == SearchOptionsHover::Regex;
        let regex_start = current_col;
        let regex_label = format!("{} {}", regex_checkbox, t!("search.regex"));
        let regex_shortcut_text = regex_shortcut
            .as_ref()
            .map(|s| format!(" ({})", s))
            .unwrap_or_default();
        let regex_full_width = str_width(&regex_label) + str_width(&regex_shortcut_text);

        spans.push(Span::styled(
            regex_label,
            get_checkbox_style(regex_hovered, use_regex),
        ));
        if !regex_shortcut_text.is_empty() {
            spans.push(Span::styled(
                regex_shortcut_text,
                if regex_hovered {
                    hover_shortcut_style
                } else {
                    shortcut_style
                },
            ));
        }
        current_col += regex_full_width as u16;
        layout.regex = Some((regex_start, current_col));

        // Show capture group hint when regex is enabled in replace mode
        if use_regex && confirm_each.is_some() {
            let hint = " \u{2502} $1,$2,…";
            spans.push(Span::styled(hint, shortcut_style));
            current_col += str_width(hint) as u16;
        }

        // Confirm Each option (only shown in replace mode)
        if let Some(confirm_value) = confirm_each {
            let confirm_shortcut =
                get_shortcut(&crate::input::keybindings::Action::ToggleSearchConfirmEach);
            let confirm_checkbox = if confirm_value { "[x]" } else { "[ ]" };

            // Separator
            spans.push(Span::styled("   ", base_style));
            current_col += 3;

            let confirm_hovered = hover == SearchOptionsHover::ConfirmEach;
            let confirm_start = current_col;
            let confirm_label = format!("{} {}", confirm_checkbox, t!("search.confirm_each"));
            let confirm_shortcut_text = confirm_shortcut
                .as_ref()
                .map(|s| format!(" ({})", s))
                .unwrap_or_default();
            let confirm_full_width = str_width(&confirm_label) + str_width(&confirm_shortcut_text);

            spans.push(Span::styled(
                confirm_label,
                get_checkbox_style(confirm_hovered, confirm_value),
            ));
            if !confirm_shortcut_text.is_empty() {
                spans.push(Span::styled(
                    confirm_shortcut_text,
                    if confirm_hovered {
                        hover_shortcut_style
                    } else {
                        shortcut_style
                    },
                ));
            }
            current_col += confirm_full_width as u16;
            layout.confirm_each = Some((confirm_start, current_col));
        }

        // Fill remaining space
        let current_width = (current_col - area.x) as usize;
        let available_width = area.width as usize;
        if current_width < available_width {
            spans.push(Span::styled(
                " ".repeat(available_width.saturating_sub(current_width)),
                base_style,
            ));
        }

        let options_line = Paragraph::new(Line::from(spans));
        frame.render_widget(options_line, area);

        layout
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_truncate_path_short_path() {
        let path = PathBuf::from("/home/user/project");
        let result = truncate_path(&path, 50);

        assert!(!result.truncated);
        assert_eq!(result.suffix, "/home/user/project");
        assert!(result.prefix.is_empty());
    }

    #[test]
    fn test_truncate_path_long_path() {
        let path = PathBuf::from(
            "/private/var/folders/p6/nlmq3k8146990kpkxl73mq340000gn/T/.tmpNYt4Fc/project_root",
        );
        let result = truncate_path(&path, 40);

        assert!(result.truncated, "Path should be truncated");
        assert_eq!(result.prefix, "/private");
        assert!(
            result.suffix.contains("project_root"),
            "Suffix should contain project_root"
        );
    }

    #[test]
    fn test_truncate_path_preserves_last_components() {
        let path = PathBuf::from("/a/b/c/d/e/f/g/h/i/j/project/src");
        let result = truncate_path(&path, 30);

        assert!(result.truncated);
        // Should preserve the last components that fit
        assert!(
            result.suffix.contains("src"),
            "Should preserve last component 'src', got: {}",
            result.suffix
        );
    }

    #[test]
    fn test_truncate_path_display_len() {
        let path = PathBuf::from("/private/var/folders/deep/nested/path/here");
        let result = truncate_path(&path, 30);

        // The display length should not exceed max_len (approximately)
        let display = result.to_string_plain();
        assert!(
            display.len() <= 35, // Allow some slack for trailing slash
            "Display should be truncated to around 30 chars, got {} chars: {}",
            display.len(),
            display
        );
    }

    #[test]
    fn test_truncate_path_root_only() {
        let path = PathBuf::from("/");
        let result = truncate_path(&path, 50);

        assert!(!result.truncated);
        assert_eq!(result.suffix, "/");
    }

    #[test]
    fn test_truncated_path_to_string_plain() {
        let truncated = TruncatedPath {
            prefix: "/home".to_string(),
            truncated: true,
            suffix: "/project/src".to_string(),
        };

        assert_eq!(truncated.to_string_plain(), "/home/[...]/project/src");
    }

    #[test]
    fn test_truncated_path_to_string_plain_no_truncation() {
        let truncated = TruncatedPath {
            prefix: String::new(),
            truncated: false,
            suffix: "/home/user/project".to_string(),
        };

        assert_eq!(truncated.to_string_plain(), "/home/user/project");
    }
}
