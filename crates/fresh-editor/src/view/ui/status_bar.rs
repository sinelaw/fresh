//! Status bar and prompt/minibuffer rendering

use std::path::Path;

use crate::app::WarningLevel;
use crate::config::{StatusBarConfig, StatusBarElement};
use crate::primitives::display_width::{char_width, str_width};
use crate::state::EditorState;
use crate::view::prompt::Prompt;
use chrono::Timelike;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;
use ratatui::Frame;
use rust_i18n::t;

/// Text that both marks a buffer as "edited over a disconnected SSH session"
/// and styles the prefix in the status bar. Kept as constants so `render_element`
/// and `element_spans` stay in sync.
const SSH_PREFIX: &str = "[SSH:";
const SSH_PREFIX_TERMINATOR: &str = "] ";

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
    /// Clock element — colon rendered with hardware blink
    Clock,
}

/// A single rendered status bar element with its text and styling info.
struct RenderedElement {
    text: String,
    kind: ElementKind,
}

/// Editor state, theming, and runtime inputs needed to render a status bar frame.
pub struct StatusBarContext<'a> {
    pub state: &'a mut EditorState,
    pub cursors: &'a crate::model::cursor::Cursors,
    pub status_message: &'a Option<String>,
    pub plugin_status_message: &'a Option<String>,
    pub lsp_status: &'a str,
    pub theme: &'a crate::view::theme::Theme,
    pub display_name: &'a str,
    pub keybindings: &'a crate::input::keybindings::KeybindingResolver,
    pub chord_state: &'a [(crossterm::event::KeyCode, crossterm::event::KeyModifiers)],
    pub update_available: Option<&'a str>,
    pub warning_level: WarningLevel,
    pub general_warning_count: usize,
    pub hover: StatusBarHover,
    pub remote_connection: Option<&'a str>,
    pub session_name: Option<&'a str>,
    pub read_only: bool,
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
    /// Render only the status bar (without prompt).
    ///
    /// Returns layout information with positions of clickable indicators.
    pub fn render_status_bar(
        frame: &mut Frame,
        area: Rect,
        ctx: &mut StatusBarContext<'_>,
        config: &StatusBarConfig,
    ) -> StatusBarLayout {
        Self::render_status(frame, area, ctx, config)
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
    fn render_element(
        element: &StatusBarElement,
        ctx: &mut StatusBarContext<'_>,
    ) -> Option<RenderedElement> {
        match element {
            StatusBarElement::Filename => {
                let modified = if ctx.state.buffer.is_modified() {
                    " [+]"
                } else {
                    ""
                };
                let read_only_indicator = if ctx.read_only { " [RO]" } else { "" };
                let remote_disconnected = ctx
                    .remote_connection
                    .map(|conn| conn.contains("(Disconnected)"))
                    .unwrap_or(false);
                let remote_prefix = ctx
                    .remote_connection
                    .map(|conn| format!("{SSH_PREFIX}{conn}{SSH_PREFIX_TERMINATOR}"))
                    .unwrap_or_default();
                let session_prefix = ctx
                    .session_name
                    .map(|name| format!("[{}] ", name))
                    .unwrap_or_default();
                let display_name = ctx.display_name;
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
                if !ctx.state.show_cursors {
                    return None;
                }
                let cursor = *ctx.cursors.primary();
                let byte_offset_mode = ctx.state.buffer.line_count().is_none();
                let text = if byte_offset_mode {
                    format!("Byte {}", cursor.position)
                } else {
                    let cursor_iter = ctx.state.buffer.line_iterator(cursor.position, 80);
                    let line_start = cursor_iter.current_position();
                    let col = cursor.position.saturating_sub(line_start);
                    let line = ctx.state.primary_cursor_line_number.value();
                    format!("Ln {}, Col {}", line + 1, col + 1)
                };
                Some(RenderedElement {
                    text,
                    kind: ElementKind::Normal,
                })
            }
            StatusBarElement::CursorCompact => {
                if !ctx.state.show_cursors {
                    return None;
                }
                let cursor = *ctx.cursors.primary();
                let byte_offset_mode = ctx.state.buffer.line_count().is_none();
                let text = if byte_offset_mode {
                    format!("{}", cursor.position)
                } else {
                    let cursor_iter = ctx.state.buffer.line_iterator(cursor.position, 80);
                    let line_start = cursor_iter.current_position();
                    let col = cursor.position.saturating_sub(line_start);
                    let line = ctx.state.primary_cursor_line_number.value();
                    format!("{}:{}", line + 1, col + 1)
                };
                Some(RenderedElement {
                    text,
                    kind: ElementKind::Normal,
                })
            }
            StatusBarElement::Diagnostics => {
                let diagnostics = ctx.state.overlays.all();
                let mut error_count = 0usize;
                let mut warning_count = 0usize;
                let mut info_count = 0usize;
                let diagnostic_ns = crate::services::lsp::diagnostics::lsp_diagnostic_namespace();
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
                if error_count > 0 {
                    parts.push(format!("E:{}", error_count));
                }
                if warning_count > 0 {
                    parts.push(format!("W:{}", warning_count));
                }
                if info_count > 0 {
                    parts.push(format!("I:{}", info_count));
                }
                Some(RenderedElement {
                    text: parts.join(" "),
                    kind: ElementKind::Normal,
                })
            }
            StatusBarElement::CursorCount => {
                if ctx.cursors.count() <= 1 {
                    return None;
                }
                Some(RenderedElement {
                    text: t!("status.cursors", count = ctx.cursors.count()).to_string(),
                    kind: ElementKind::Normal,
                })
            }
            StatusBarElement::Messages => {
                let mut parts: Vec<&str> = Vec::new();
                if let Some(msg) = ctx.status_message {
                    if !msg.is_empty() {
                        parts.push(msg);
                    }
                }
                if let Some(msg) = ctx.plugin_status_message {
                    if !msg.is_empty() {
                        parts.push(msg);
                    }
                }
                if parts.is_empty() {
                    return None;
                }
                Some(RenderedElement {
                    text: parts.join(" | "),
                    kind: ElementKind::Messages,
                })
            }
            StatusBarElement::Chord => {
                if ctx.chord_state.is_empty() {
                    return None;
                }
                let chord_str = ctx
                    .chord_state
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
                text: format!(" {} ", ctx.state.buffer.line_ending().display_name()),
                kind: ElementKind::LineEnding,
            }),
            StatusBarElement::Encoding => Some(RenderedElement {
                text: format!(" {} ", ctx.state.buffer.encoding().display_name()),
                kind: ElementKind::Encoding,
            }),
            StatusBarElement::Language => {
                let text = if ctx.state.language == "text"
                    && ctx.state.display_name != "Text"
                    && ctx.state.display_name != "Plain Text"
                    && ctx.state.display_name != "text"
                {
                    format!(" {} [syntax only] ", &ctx.state.display_name)
                } else {
                    format!(" {} ", &ctx.state.display_name)
                };
                Some(RenderedElement {
                    text,
                    kind: ElementKind::Language,
                })
            }
            StatusBarElement::Lsp => {
                if ctx.lsp_status.is_empty() {
                    return None;
                }
                Some(RenderedElement {
                    text: format!(" {} ", ctx.lsp_status),
                    kind: ElementKind::Lsp,
                })
            }
            StatusBarElement::Warnings => {
                if ctx.general_warning_count == 0 {
                    return None;
                }
                Some(RenderedElement {
                    text: format!(" [\u{26a0} {}] ", ctx.general_warning_count),
                    kind: ElementKind::WarningBadge,
                })
            }
            StatusBarElement::Update => {
                let version = ctx.update_available?;
                Some(RenderedElement {
                    text: format!(" {} ", t!("status.update_available", version = version)),
                    kind: ElementKind::Update,
                })
            }
            StatusBarElement::Palette => {
                let shortcut = ctx
                    .keybindings
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
            StatusBarElement::Clock => {
                let now = chrono::Local::now();
                let text = format!("{:02}:{:02}", now.hour(), now.minute());
                Some(RenderedElement {
                    text,
                    kind: ElementKind::Clock,
                })
            }
        }
    }

    /// Get the style for a rendered element based on its kind, theme, and hover state.
    fn element_style(
        kind: ElementKind,
        theme: &crate::view::theme::Theme,
        hover: StatusBarHover,
        warning_level: WarningLevel,
    ) -> Style {
        match kind {
            ElementKind::Normal | ElementKind::Messages | ElementKind::Clock => Style::default()
                .fg(theme.status_bar_fg)
                .bg(theme.status_bar_bg),
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
                if is_hovering {
                    style = style.add_modifier(Modifier::UNDERLINED);
                }
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
                if is_hovering {
                    style = style.add_modifier(Modifier::UNDERLINED);
                }
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
                if is_hovering {
                    style = style.add_modifier(Modifier::UNDERLINED);
                }
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
                    (
                        theme.status_warning_indicator_hover_fg,
                        theme.status_warning_indicator_hover_bg,
                    )
                } else {
                    (
                        theme.status_warning_indicator_fg,
                        theme.status_warning_indicator_bg,
                    )
                };
                let mut style = Style::default().fg(fg).bg(bg);
                if is_hovering {
                    style = style.add_modifier(Modifier::UNDERLINED);
                }
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
            ElementKind::LineEnding => {
                layout.line_ending_indicator = Some((row, start_col, end_col))
            }
            ElementKind::Encoding => layout.encoding_indicator = Some((row, start_col, end_col)),
            ElementKind::Language => layout.language_indicator = Some((row, start_col, end_col)),
            ElementKind::Lsp => layout.lsp_indicator = Some((row, start_col, end_col)),
            ElementKind::WarningBadge => layout.warning_badge = Some((row, start_col, end_col)),
            ElementKind::Messages => layout.message_area = Some((row, start_col, end_col)),
            _ => {}
        }
    }

    /// Build the styled spans for a single rendered element, honoring the
    /// special-case two-color rendering for a disconnected remote filename.
    ///
    /// Returns the spans and the total display width of the emitted text.
    fn element_spans(
        rendered: &RenderedElement,
        theme: &crate::view::theme::Theme,
        hover: StatusBarHover,
        warning_level: WarningLevel,
    ) -> (Vec<Span<'static>>, usize) {
        let base_style = Style::default()
            .fg(theme.status_bar_fg)
            .bg(theme.status_bar_bg);
        let width = str_width(&rendered.text);

        if rendered.kind == ElementKind::RemoteDisconnected && rendered.text.starts_with(SSH_PREFIX)
        {
            let error_style = Style::default()
                .fg(theme.status_error_indicator_fg)
                .bg(theme.status_error_indicator_bg);
            if let Some(term_off) = rendered.text.find(SSH_PREFIX_TERMINATOR) {
                let split_at = term_off + SSH_PREFIX_TERMINATOR.len();
                let prefix = rendered.text[..split_at].to_string();
                let rest = rendered.text[split_at..].to_string();
                return (
                    vec![
                        Span::styled(prefix, error_style),
                        Span::styled(rest, base_style),
                    ],
                    width,
                );
            }
            return (
                vec![Span::styled(rendered.text.clone(), error_style)],
                width,
            );
        }

        let style = Self::element_style(rendered.kind, theme, hover, warning_level);
        let spans = if rendered.kind == ElementKind::Clock {
            // "HH:MM" — blink the colon via terminal hardware (SGR 5)
            vec![
                Span::styled(rendered.text[..2].to_string(), style),
                Span::styled(":".to_string(), style.add_modifier(Modifier::SLOW_BLINK)),
                Span::styled(rendered.text[3..].to_string(), style),
            ]
        } else {
            vec![Span::styled(rendered.text.clone(), style)]
        };
        (spans, width)
    }

    /// Render a configured side (left/right) into styled per-element groups.
    fn render_side(
        config_side: &[StatusBarElement],
        ctx: &mut StatusBarContext<'_>,
    ) -> Vec<(Vec<Span<'static>>, usize, ElementKind)> {
        let rendered: Vec<RenderedElement> = config_side
            .iter()
            .filter_map(|elem| Self::render_element(elem, ctx))
            .filter(|e| !e.text.is_empty())
            .collect();

        let theme = ctx.theme;
        let hover = ctx.hover;
        let warning_level = ctx.warning_level;
        rendered
            .into_iter()
            .map(|r| {
                let kind = r.kind;
                let (spans, width) = Self::element_spans(&r, theme, hover, warning_level);
                (spans, width, kind)
            })
            .collect()
    }

    /// Render the normal status bar (config-driven).
    fn render_status(
        frame: &mut Frame,
        area: Rect,
        ctx: &mut StatusBarContext<'_>,
        config: &StatusBarConfig,
    ) -> StatusBarLayout {
        let mut layout = StatusBarLayout::default();
        let base_style = Style::default()
            .fg(ctx.theme.status_bar_fg)
            .bg(ctx.theme.status_bar_bg);
        let available_width = area.width as usize;

        if available_width == 0 || area.height == 0 {
            return layout;
        }

        let left_items = Self::render_side(&config.left, ctx);
        let right_items = Self::render_side(&config.right, ctx);

        const SEPARATOR: &str = " | ";
        let separator_width = str_width(SEPARATOR);

        let right_width: usize = right_items.iter().map(|(_, w, _)| *w).sum();

        let narrow = available_width < 15;
        let left_max_width = if narrow {
            available_width
        } else if available_width > right_width + 1 {
            available_width - right_width - 1
        } else {
            1
        };

        // Emit left side, consuming `left_items` so each element's spans move
        // directly into the output without a clone. Widths are cached so the
        // truncation check doesn't re-measure text.
        let mut spans: Vec<Span<'static>> = Vec::new();
        let mut used_left: usize = 0;

        for (idx, (item_spans, width, kind)) in left_items.into_iter().enumerate() {
            let sep_width = if idx == 0 { 0 } else { separator_width };
            if used_left + sep_width >= left_max_width {
                break;
            }
            if sep_width > 0 {
                spans.push(Span::styled(SEPARATOR, base_style));
                used_left += sep_width;
            }

            let remaining = left_max_width - used_left;
            let start_col = used_left;

            if width <= remaining {
                spans.extend(item_spans);
                used_left += width;

                Self::update_layout_for_element(
                    &mut layout,
                    kind,
                    area.y,
                    area.x + start_col as u16,
                    area.x + (start_col + width) as u16,
                );
            } else {
                // Overflow: truncate the concatenated text of this element.
                // Per-span styling is lost for the overflowed slice — we fall
                // back to whatever `element_style` would have returned.
                let group_text: String = item_spans.iter().map(|s| s.content.as_ref()).collect();
                let truncated = truncate_to_width(&group_text, remaining);
                let truncated_width = str_width(&truncated);
                let overflow_style =
                    Self::element_style(kind, ctx.theme, ctx.hover, ctx.warning_level);
                spans.push(Span::styled(truncated, overflow_style));
                used_left += truncated_width;

                Self::update_layout_for_element(
                    &mut layout,
                    kind,
                    area.y,
                    area.x + start_col as u16,
                    area.x + (start_col + truncated_width) as u16,
                );
                break;
            }
        }

        if narrow {
            if used_left < available_width {
                spans.push(Span::styled(
                    " ".repeat(available_width - used_left),
                    base_style,
                ));
            }
            frame.render_widget(Paragraph::new(Line::from(spans)), area);
            return layout;
        }

        let mut col_offset = used_left;
        if col_offset + right_width < available_width {
            let padding = available_width - col_offset - right_width;
            spans.push(Span::styled(" ".repeat(padding), base_style));
            col_offset = available_width - right_width;
        } else if col_offset < available_width {
            spans.push(Span::styled(" ", base_style));
            col_offset += 1;
        }

        let mut current_col = area.x + col_offset as u16;
        for (item_spans, width, kind) in right_items {
            Self::update_layout_for_element(
                &mut layout,
                kind,
                area.y,
                current_col,
                current_col + width as u16,
            );
            spans.extend(item_spans);
            current_col += width as u16;
        }

        frame.render_widget(Paragraph::new(Line::from(spans)), area);
        layout
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
