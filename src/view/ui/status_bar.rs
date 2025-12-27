//! Status bar and prompt/minibuffer rendering

use std::path::Path;

use crate::app::WarningLevel;
use crate::primitives::display_width::{char_width, str_width};
use crate::state::EditorState;
use crate::view::prompt::Prompt;
use ratatui::layout::Rect;
use ratatui::style::Style;
use ratatui::text::{Line, Span};
use ratatui::widgets::Paragraph;
use ratatui::Frame;

/// Layout information returned from status bar rendering for mouse click detection
#[derive(Debug, Clone, Default)]
pub struct StatusBarLayout {
    /// LSP indicator area (row, start_col, end_col) - None if no LSP indicator shown
    pub lsp_indicator: Option<(u16, u16, u16)>,
    /// Warning badge area (row, start_col, end_col) - None if no warnings
    pub warning_badge: Option<(u16, u16, u16)>,
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

/// Renders the status bar and prompt/minibuffer
pub struct StatusBarRenderer;

impl StatusBarRenderer {
    /// Render only the status bar (without prompt)
    ///
    /// # Arguments
    /// * `frame` - The ratatui frame to render to
    /// * `area` - The rectangular area to render in
    /// * `state` - The active buffer's editor state
    /// * `status_message` - Optional status message to display
    /// * `lsp_status` - LSP status indicator
    /// * `theme` - The active theme for colors
    /// * `display_name` - The display name for the file (project-relative path)
    /// * `chord_state` - Current chord sequence state (for multi-key bindings)
    /// * `update_available` - Optional new version string if an update is available
    /// * `warning_level` - LSP warning level (for coloring LSP indicator)
    /// * `general_warning_count` - Number of general warnings (for badge display)
    ///
    /// # Returns
    /// Layout information with positions of clickable indicators
    pub fn render_status_bar(
        frame: &mut Frame,
        area: Rect,
        state: &mut EditorState,
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
    ) -> StatusBarLayout {
        Self::render_status(
            frame,
            area,
            state,
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
        // Cursor should be at: message.len() + cursor_pos
        let cursor_x = (prompt.message.len() + prompt.cursor_pos) as u16;
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
        spans.push(Span::styled("Open: ", base_style));

        // Calculate if we need to truncate
        // Only truncate if full path + input exceeds 90% of available width
        let prefix_len = 6; // "Open: "
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
        let dir_display_len = if truncated.truncated {
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
            let len = truncated.prefix.len() + "/[...]".len() + suffix_with_slash.len();
            spans.push(Span::styled(suffix_with_slash, dir_style));
            len
        } else {
            // No truncation - just show the path with trailing slash
            let path_display = if truncated.suffix.ends_with('/') {
                truncated.suffix.clone()
            } else {
                format!("{}/", truncated.suffix)
            };
            let len = path_display.len();
            spans.push(Span::styled(path_display, dir_style));
            len
        };

        // User input (the filename part) - normal color
        spans.push(Span::styled(prompt.input.clone(), base_style));

        let line = Line::from(spans);
        let prompt_line = Paragraph::new(line).style(base_style);

        frame.render_widget(prompt_line, area);

        // Set cursor position in the prompt
        // Cursor should be at: "Open: ".len() + dir_display.len() + cursor_pos
        let cursor_offset = prefix_len + dir_display_len + prompt.cursor_pos;
        let cursor_x = cursor_offset as u16;
        if cursor_x < area.width {
            frame.set_cursor_position((area.x + cursor_x, area.y));
        }
    }

    /// Render the normal status bar
    fn render_status(
        frame: &mut Frame,
        area: Rect,
        state: &mut EditorState,
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
    ) -> StatusBarLayout {
        // Initialize layout tracking
        let mut layout = StatusBarLayout::default();
        // Use the pre-computed display name from buffer metadata
        let filename = display_name;

        let modified = if state.buffer.is_modified() {
            " [+]"
        } else {
            ""
        };

        // Format chord state if present
        let chord_display = if !chord_state.is_empty() {
            let chord_str = chord_state
                .iter()
                .map(|(code, modifiers)| {
                    crate::input::keybindings::format_keybinding(code, modifiers)
                })
                .collect::<Vec<_>>()
                .join(" ");
            format!(" [{}]", chord_str)
        } else {
            String::new()
        };

        // View mode indicator
        let _mode_label = match state.view_mode {
            crate::state::ViewMode::Compose => " | Compose",
            _ => "",
        };

        let cursor = *state.primary_cursor();

        // Get line number and column efficiently using cached values
        let (line, col) = {
            // Find the start of the line containing the cursor
            let cursor_iter = state.buffer.line_iterator(cursor.position, 80);
            let line_start = cursor_iter.current_position();
            let col = cursor.position.saturating_sub(line_start);

            // Use cached line number from state
            let line_num = state.primary_cursor_line_number.value();
            (line_num, col)
        };

        // Count diagnostics by severity
        let diagnostics = state.overlays.all();
        let mut error_count = 0;
        let mut warning_count = 0;
        let mut info_count = 0;

        // Use the lsp-diagnostic namespace to identify diagnostic overlays
        let diagnostic_ns = crate::services::lsp::diagnostics::lsp_diagnostic_namespace();
        for overlay in diagnostics {
            if overlay.namespace.as_ref() == Some(&diagnostic_ns) {
                // Check priority to determine severity
                // Based on lsp_diagnostics.rs: Error=100, Warning=50, Info=30, Hint=10
                match overlay.priority {
                    100 => error_count += 1,
                    50 => warning_count += 1,
                    _ => info_count += 1,
                }
            }
        }

        // Build diagnostics summary if there are any
        let diagnostics_summary = if error_count + warning_count + info_count > 0 {
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
            format!(" | {}", parts.join(" "))
        } else {
            String::new()
        };

        // Build cursor count indicator (only show if multiple cursors)
        let cursor_count_indicator = if state.cursors.count() > 1 {
            format!(" | {} cursors", state.cursors.count())
        } else {
            String::new()
        };

        // Build status message parts
        let mut message_parts: Vec<&str> = Vec::new();
        if let Some(msg) = status_message {
            if !msg.is_empty() {
                message_parts.push(msg);
            }
        }
        if let Some(msg) = plugin_status_message {
            if !msg.is_empty() {
                message_parts.push(msg);
            }
        }

        let message_suffix = if message_parts.is_empty() {
            String::new()
        } else {
            format!(" | {}", message_parts.join(" | "))
        };

        // Build left status (file info, position, diagnostics, messages)
        let base_status = format!(
            "{filename}{modified} | Ln {line}, Col {col}{diagnostics_summary}{cursor_count_indicator}"
        );

        let left_status = format!("{base_status}{chord_display}{message_suffix}");

        // Build right-side indicators (these stay fixed on the right)
        // Order: [LSP indicator] [warning badge] [update] [Palette]

        // LSP indicator (right-aligned, with colored background if warning/error)
        let lsp_indicator = if !lsp_status.is_empty() {
            format!(" {} ", lsp_status)
        } else {
            String::new()
        };
        let lsp_indicator_width = str_width(&lsp_indicator);

        // General warning badge (right-aligned)
        let warning_badge = if general_warning_count > 0 {
            format!(" [⚠ {}] ", general_warning_count)
        } else {
            String::new()
        };
        let warning_badge_width = str_width(&warning_badge);

        // Build update indicator for right side (if update available)
        let update_indicator = update_available.map(|version| format!(" Update: v{} ", version));
        let update_width = update_indicator.as_ref().map(|s| s.len()).unwrap_or(0);

        // Build Command Palette indicator for right side
        // Always show Command Palette indicator on the right side
        let cmd_palette_shortcut = keybindings
            .get_keybinding_for_action(
                &crate::input::keybindings::Action::CommandPalette,
                crate::input::keybindings::KeyContext::Global,
            )
            .unwrap_or_else(|| "?".to_string());
        let cmd_palette_indicator = format!("Palette: {}", cmd_palette_shortcut);
        let padded_cmd_palette = format!(" {} ", cmd_palette_indicator);

        // Calculate available width and right side width
        // Right side: [LSP indicator] [warning badge] [update] [Palette]
        let available_width = area.width as usize;
        let cmd_palette_width = str_width(&padded_cmd_palette);
        let right_side_width =
            lsp_indicator_width + warning_badge_width + update_width + cmd_palette_width;

        // Only show command palette indicator if there's enough space (at least 15 chars for minimal display)
        let spans = if available_width >= 15 {
            // Reserve space for right side indicators
            let left_max_width = if available_width > right_side_width + 1 {
                available_width - right_side_width - 1 // -1 for at least one space separator
            } else {
                1 // Minimal space
            };

            let mut spans = vec![];

            // Truncate left status if it's too long (use visual width, not char count)
            let left_visual_width = str_width(&left_status);
            let displayed_left = if left_visual_width > left_max_width {
                let truncate_at = left_max_width.saturating_sub(3); // -3 for "..."
                if truncate_at > 0 {
                    // Take characters up to visual width limit
                    let mut width = 0;
                    let truncated: String = left_status
                        .chars()
                        .take_while(|ch| {
                            let w = char_width(*ch);
                            if width + w <= truncate_at {
                                width += w;
                                true
                            } else {
                                false
                            }
                        })
                        .collect();
                    format!("{}...", truncated)
                } else {
                    String::from("...")
                }
            } else {
                left_status.clone()
            };

            let displayed_left_len = str_width(&displayed_left);

            spans.push(Span::styled(
                displayed_left.clone(),
                Style::default()
                    .fg(theme.status_bar_fg)
                    .bg(theme.status_bar_bg),
            ));

            // Add spacing to push right side indicators to the right
            if displayed_left_len + right_side_width < available_width {
                let padding_len = available_width - displayed_left_len - right_side_width;
                spans.push(Span::styled(
                    " ".repeat(padding_len),
                    Style::default()
                        .fg(theme.status_bar_fg)
                        .bg(theme.status_bar_bg),
                ));
            } else if displayed_left_len < available_width {
                // Add minimal space
                spans.push(Span::styled(
                    " ",
                    Style::default()
                        .fg(theme.status_bar_fg)
                        .bg(theme.status_bar_bg),
                ));
            }

            // Track current column for layout positions
            let mut current_col = area.x + displayed_left_len as u16;
            if displayed_left_len + right_side_width < available_width {
                current_col = area.x + (available_width - right_side_width) as u16;
            }

            // Add LSP indicator with colored background if warning/error
            if !lsp_indicator.is_empty() {
                let (lsp_fg, lsp_bg) = match warning_level {
                    WarningLevel::Error => (
                        theme.status_error_indicator_fg,
                        theme.status_error_indicator_bg,
                    ),
                    WarningLevel::Warning => (
                        theme.status_warning_indicator_fg,
                        theme.status_warning_indicator_bg,
                    ),
                    WarningLevel::None => (theme.status_bar_fg, theme.status_bar_bg),
                };
                // Record LSP indicator position for click detection
                layout.lsp_indicator = Some((area.y, current_col, current_col + lsp_indicator_width as u16));
                current_col += lsp_indicator_width as u16;
                spans.push(Span::styled(
                    lsp_indicator.clone(),
                    Style::default().fg(lsp_fg).bg(lsp_bg),
                ));
            }

            // Add general warning badge if there are warnings
            if !warning_badge.is_empty() {
                // Record warning badge position for click detection
                layout.warning_badge = Some((area.y, current_col, current_col + warning_badge_width as u16));
                current_col += warning_badge_width as u16;
                spans.push(Span::styled(
                    warning_badge.clone(),
                    Style::default()
                        .fg(theme.status_warning_indicator_fg)
                        .bg(theme.status_warning_indicator_bg),
                ));
            }
            // Keep current_col in scope to avoid unused warning
            let _ = current_col;

            // Add update indicator if available (with highlighted styling)
            if let Some(ref update_text) = update_indicator {
                spans.push(Span::styled(
                    update_text.clone(),
                    Style::default()
                        .fg(theme.menu_highlight_fg)
                        .bg(theme.menu_dropdown_bg),
                ));
            }

            // Add command palette indicator with distinct styling and padding
            spans.push(Span::styled(
                padded_cmd_palette.clone(),
                Style::default()
                    .fg(theme.help_indicator_fg)
                    .bg(theme.help_indicator_bg),
            ));

            spans
        } else {
            // Terminal too narrow or no command palette indicator - fill entire width with left status
            let mut spans = vec![];
            let left_visual_width = str_width(&left_status);
            let displayed_left = if left_visual_width > available_width {
                let truncate_at = available_width.saturating_sub(3);
                if truncate_at > 0 {
                    // Take characters up to visual width limit
                    let mut width = 0;
                    let truncated: String = left_status
                        .chars()
                        .take_while(|ch| {
                            let w = char_width(*ch);
                            if width + w <= truncate_at {
                                width += w;
                                true
                            } else {
                                false
                            }
                        })
                        .collect();
                    format!("{}...", truncated)
                } else {
                    // Take characters up to available width
                    let mut width = 0;
                    left_status
                        .chars()
                        .take_while(|ch| {
                            let w = char_width(*ch);
                            if width + w <= available_width {
                                width += w;
                                true
                            } else {
                                false
                            }
                        })
                        .collect()
                }
            } else {
                left_status.clone()
            };

            spans.push(Span::styled(
                displayed_left.clone(),
                Style::default()
                    .fg(theme.status_bar_fg)
                    .bg(theme.status_bar_bg),
            ));

            // Fill remaining width
            if displayed_left.len() < available_width {
                spans.push(Span::styled(
                    " ".repeat(available_width - displayed_left.len()),
                    Style::default()
                        .fg(theme.status_bar_fg)
                        .bg(theme.status_bar_bg),
                ));
            }

            spans
        };

        let status_line = Paragraph::new(Line::from(spans));

        frame.render_widget(status_line, area);

        layout
    }

    /// Render the search options bar (shown when search prompt is active)
    ///
    /// Displays checkboxes for search options with their keyboard shortcuts:
    /// - Case Sensitive (Alt+C)
    /// - Whole Word (Alt+W)
    /// - Regex (Alt+R)
    /// - Confirm Each (Alt+I) - only shown in replace mode
    pub fn render_search_options(
        frame: &mut Frame,
        area: Rect,
        case_sensitive: bool,
        whole_word: bool,
        use_regex: bool,
        confirm_each: Option<bool>, // None = don't show, Some(value) = show with this state
        theme: &crate::view::theme::Theme,
        keybindings: &crate::input::keybindings::KeybindingResolver,
    ) {
        // Use menu dropdown background (dark gray) for the options bar
        let base_style = Style::default()
            .fg(theme.menu_dropdown_fg)
            .bg(theme.menu_dropdown_bg);

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

        // Style for keyboard shortcuts - use a lighter gray that's visible on dark background
        let shortcut_style = Style::default()
            .fg(ratatui::style::Color::Rgb(140, 140, 140))
            .bg(theme.menu_dropdown_bg);

        let mut spans = Vec::new();

        // Left padding
        spans.push(Span::styled(" ", base_style));

        // Case Sensitive option
        spans.push(Span::styled(
            case_checkbox,
            if case_sensitive {
                active_style
            } else {
                base_style
            },
        ));
        spans.push(Span::styled(" Case Sensitive", base_style));
        if let Some(shortcut) = &case_shortcut {
            spans.push(Span::styled(format!(" ({})", shortcut), shortcut_style));
        }

        // Separator
        spans.push(Span::styled("   ", base_style));

        // Whole Word option
        spans.push(Span::styled(
            word_checkbox,
            if whole_word { active_style } else { base_style },
        ));
        spans.push(Span::styled(" Whole Word", base_style));
        if let Some(shortcut) = &word_shortcut {
            spans.push(Span::styled(format!(" ({})", shortcut), shortcut_style));
        }

        // Separator
        spans.push(Span::styled("   ", base_style));

        // Regex option
        spans.push(Span::styled(
            regex_checkbox,
            if use_regex { active_style } else { base_style },
        ));
        spans.push(Span::styled(" Regex", base_style));
        if let Some(shortcut) = &regex_shortcut {
            spans.push(Span::styled(format!(" ({})", shortcut), shortcut_style));
        }

        // Confirm Each option (only shown in replace mode)
        if let Some(confirm_value) = confirm_each {
            let confirm_shortcut =
                get_shortcut(&crate::input::keybindings::Action::ToggleSearchConfirmEach);
            let confirm_checkbox = if confirm_value { "[x]" } else { "[ ]" };

            // Separator
            spans.push(Span::styled("   ", base_style));

            spans.push(Span::styled(
                confirm_checkbox,
                if confirm_value {
                    active_style
                } else {
                    base_style
                },
            ));
            spans.push(Span::styled(" Confirm each", base_style));
            if let Some(shortcut) = &confirm_shortcut {
                spans.push(Span::styled(format!(" ({})", shortcut), shortcut_style));
            }
        }

        // Fill remaining space
        let current_width: usize = spans.iter().map(|s| s.content.len()).sum();
        let available_width = area.width as usize;
        if current_width < available_width {
            spans.push(Span::styled(
                " ".repeat(available_width.saturating_sub(current_width)),
                base_style,
            ));
        }

        let options_line = Paragraph::new(Line::from(spans));
        frame.render_widget(options_line, area);
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
