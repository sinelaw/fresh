//! Status bar and prompt/minibuffer rendering

use std::collections::HashMap;
use std::path::Path;

use crate::app::types::CellThemeRecorder;
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
    /// Remote authority indicator — styling driven by connection state
    RemoteIndicator(RemoteIndicatorState),
    /// Workspace-trust indicator — always present, styling driven by the
    /// active session's trust level. Clickable (opens the trust prompt).
    WorkspaceTrust(crate::services::workspace_trust::TrustLevel),
    /// Custom plugin token
    Custom,
}

/// Visual/semantic state of the remote authority indicator.
///
/// Covers the full dev-container UX lifecycle the spec asks for —
/// Local, Connecting to a remote authority, Connected, FailedAttach,
/// Disconnected — while remaining general enough for any remote
/// authority Fresh currently supports (SSH today; containers;
/// anything a plugin installs via `editor.setAuthority(...)`).
///
/// Variants deliberately hold no data — phase labels and error text
/// are passed alongside via `StatusBarContext::remote_state_override`
/// (added in Phase B-2) so the enum stays `Copy` and core code never
/// learns devcontainer-specific vocabulary (see
/// `AUTHORITY_DESIGN.md` principle 3).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RemoteIndicatorState {
    /// Editing local files; rendered with the default status-bar palette.
    #[default]
    Local,
    /// An attach (or reconnect) is in flight — rendered with a spinner
    /// glyph and the help-indicator palette. Plugins drive this via
    /// `setRemoteIndicatorState` before kicking off `devcontainer up`
    /// or similar long-running setup.
    Connecting,
    /// Connected to an SSH / container / other remote authority.
    Connected,
    /// The last attach attempt failed. Rendered with the error palette
    /// so the state is visible at a glance; the popup surfaces the
    /// error detail and a Retry action.
    FailedAttach,
    /// Connection lost — rendered with the error palette as a persistent
    /// warning that writes/saves are no longer reaching the authority.
    Disconnected,
}

/// Plugin-supplied override for the Remote Indicator. Carries both
/// the state enum and a user-visible label/error text so core doesn't
/// need to know how to phrase "Connecting..." or a specific failure
/// string — the plugin owns the copy.
///
/// Deserialized from the tagged JSON shape accepted by the
/// `SetRemoteIndicatorState` plugin op (see `fresh-core::api`). Kept
/// in the view crate so the enum lives next to the rendering that
/// consumes it.
#[derive(Debug, Clone, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum RemoteIndicatorOverride {
    /// Force the indicator to "Local" even when the authority would
    /// otherwise read as Connected. Rarely needed in practice.
    Local,
    /// Attach is in flight. `label` is the short text shown next to
    /// the spinner glyph (e.g. "Building", "Pulling image").
    Connecting {
        #[serde(default)]
        label: Option<String>,
    },
    /// Force Connected. `label` overrides the authority's display
    /// string if present; otherwise the derived label is shown.
    Connected {
        #[serde(default)]
        label: Option<String>,
    },
    /// Last attach attempt failed. `error` is the short message the
    /// indicator renders; longer context belongs in the popup.
    FailedAttach {
        #[serde(default)]
        error: Option<String>,
    },
    /// Explicitly disconnected (e.g. plugin detected a container
    /// stop that the authority doesn't know about yet).
    Disconnected {
        #[serde(default)]
        label: Option<String>,
    },
}

impl RemoteIndicatorOverride {
    /// Project into the Copy enum consumed by `element_style`.
    pub fn state(&self) -> RemoteIndicatorState {
        match self {
            Self::Local => RemoteIndicatorState::Local,
            Self::Connecting { .. } => RemoteIndicatorState::Connecting,
            Self::Connected { .. } => RemoteIndicatorState::Connected,
            Self::FailedAttach { .. } => RemoteIndicatorState::FailedAttach,
            Self::Disconnected { .. } => RemoteIndicatorState::Disconnected,
        }
    }

    /// Short label rendered inside the indicator element. Defaults
    /// are chosen so an override with no `label`/`error` field still
    /// displays something sensible.
    pub fn label(&self) -> String {
        match self {
            Self::Local => "Local".to_string(),
            Self::Connecting { label } => match label {
                Some(s) if !s.is_empty() => format!("⠿ {}", s),
                _ => "⠿ Connecting".to_string(),
            },
            Self::Connected { label } => label
                .as_deref()
                .filter(|s| !s.is_empty())
                .unwrap_or("Connected")
                .to_string(),
            Self::FailedAttach { error } => match error {
                Some(s) if !s.is_empty() => format!("Attach failed: {}", s),
                _ => "Attach failed".to_string(),
            },
            Self::Disconnected { label } => match label {
                Some(s) if !s.is_empty() => format!("{} (Disconnected)", s),
                _ => "Disconnected".to_string(),
            },
        }
    }
}

/// A single rendered status bar element with its text and styling info.
struct RenderedElement {
    text: String,
    kind: ElementKind,
    /// For `ElementKind::Custom` elements, the plugin-registered token
    /// key (`"<plugin>:<token>"`) — preserved here so the layout pass
    /// can record this element's screen area under the same key for
    /// click dispatch. `None` for every built-in element kind.
    token_key: Option<String>,
}

/// Three-state LSP status used by the status bar `Lsp` element.
///
/// Collapses the previous "running / auto_start-dormant / opt-in-dormant /
/// nothing" fan-out into the three user-meaningful buckets the indicator
/// actually needs to communicate:
///
/// - `On`            — at least one server for this language is running
/// - `Off`           — configured servers exist for this language, none are running
/// - `OffDismissed`  — like `Off`, but the user clicked "Disable" from the
///                     popup; rendered with a muted style so it stops
///                     shouting for attention while remaining clickable
///                     (so the user can still open the popup to re-enable
///                     or see install help).
/// - `Error`         — at least one server for this language is in the Error state
/// - `None`          — no LSP configured or running for this language
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LspIndicatorState {
    #[default]
    None,
    On,
    Off,
    OffDismissed,
    Error,
}

/// Editor state, theming, and runtime inputs needed to render a status bar frame.
pub struct StatusBarContext<'a> {
    pub state: &'a mut EditorState,
    pub cursors: &'a crate::model::cursor::Cursors,
    pub status_message: &'a Option<String>,
    pub plugin_status_message: &'a Option<String>,
    pub lsp_status: &'a str,
    /// Three-state LSP indicator: On / Off / Error / None.  Drives the
    /// indicator's background color independently of `warning_level` (the
    /// latter still scopes whether a warning badge is shown on the right
    /// side of the status bar).
    pub lsp_indicator_state: LspIndicatorState,
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
    /// Plugin-supplied override for the `{remote}` indicator. When
    /// `Some`, its state+label are rendered instead of the one
    /// derived from `remote_connection`. Set via the
    /// `SetRemoteIndicatorState` plugin op; cleared by
    /// `ClearRemoteIndicatorState` or by a `None` pass at the call
    /// site.
    pub remote_state_override: Option<&'a RemoteIndicatorOverride>,
    /// True when the active buffer is the synthesized placeholder kept
    /// alive by the close path with `auto_create_empty_buffer_on_last_buffer_close`
    /// disabled. Buffer-specific elements (filename, cursor, line ending,
    /// encoding, language, diagnostics) suppress themselves so the bar
    /// reflects "no real buffer is open" rather than `[No Name] | Ln 1, Col 1 …`.
    pub is_synthetic_placeholder: bool,
    /// True when the user's status-bar layout contains the
    /// `RemoteIndicator` element. Set by the renderer after
    /// inspecting `StatusBarConfig.left` / `.right`. Read by the
    /// `Filename` element's branch to decide whether to emit the
    /// legacy `[Container:<id>] ` / SSH prefix on the filename
    /// — when the dedicated indicator is on the bar that prefix
    /// is redundant; when it's not, the filename keeps the prefix
    /// so users still see the connection at a glance.
    pub remote_indicator_on_bar: bool,
    /// Values of custom status bar elements registered by plugins.
    /// Key: "plugin_name:token_name", Value: current value to render.
    /// Populated by `render.rs` before rendering.
    pub dynamic_status_bar_elements: HashMap<String, String>,
    /// Active session's workspace-trust level. Drives the always-present
    /// `{trust}` indicator (read from the active authority each frame, so it
    /// never goes stale or vanishes — unlike a per-buffer plugin token).
    pub workspace_trust_level: crate::services::workspace_trust::TrustLevel,
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
    /// Remote authority indicator area (row, start_col, end_col) - clickable
    /// to open the remote-authority context menu.
    pub remote_indicator: Option<(u16, u16, u16)>,
    /// Workspace-trust indicator area (row, start_col, end_col) - clickable
    /// to open the workspace-trust prompt.
    pub trust_indicator: Option<(u16, u16, u16)>,
    /// Plugin-registered status-bar token areas, keyed by the
    /// `"<plugin_name>:<token_name>"` registry key (same key the
    /// editor uses in `status_bar_token_registry`). Populated by the
    /// renderer when it draws each plugin token. Mouse click dispatch
    /// (`handle_click_status_bar`) walks this map after the built-in
    /// indicators; on a hit, it fires the `status_bar_token_clicked`
    /// hook so the plugin can react. This is what makes the env
    /// pill, trust chip, and any future plugin chip first-class
    /// affordances back to their decisions — see
    /// `docs/internal/trust-env-devcontainer-ux-plan.md`
    /// §"Path from here to the North Star".
    pub plugin_token_areas: std::collections::HashMap<String, (u16, u16, u16)>,
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
    /// Mouse is over the remote authority indicator
    RemoteIndicator,
    /// Mouse is over the workspace-trust indicator
    WorkspaceTrust,
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
    /// The first component(s) of the path (e.g. "/home" or "C:\Users")
    pub prefix: String,
    /// Whether truncation occurred (if true, display "[...]" between prefix and suffix)
    pub truncated: bool,
    /// The last components of the path (e.g. "project/src")
    pub suffix: String,
    /// The path's own separator, reused when re-joining the prefix /
    /// ellipsis / suffix so a Windows `\`-path doesn't display with `/`.
    pub sep: char,
}

impl TruncatedPath {
    /// Get the full display string (without styling)
    pub fn to_string_plain(&self) -> String {
        if self.truncated {
            format!("{}{}[...]{}", self.prefix, self.sep, self.suffix)
        } else {
            format!("{}{}", self.prefix, self.suffix)
        }
    }

    /// Get the display length. The ellipsis marker is one separator column
    /// plus "[...]" regardless of which separator is in use.
    pub fn display_len(&self) -> usize {
        if self.truncated {
            self.prefix.len() + self.sep.len_utf8() + "[...]".len() + self.suffix.len()
        } else {
            self.prefix.len() + self.suffix.len()
        }
    }
}

/// The separator a path string is written with: `\` for a Windows-style
/// path (so it round-trips natively), `/` otherwise. Splitting always
/// accepts both — this only decides how pieces are re-joined for display.
fn path_display_sep(path_str: &str) -> char {
    if path_str.contains('\\') {
        '\\'
    } else {
        '/'
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
    // Re-join pieces with the path's own separator so a Windows `\`-path
    // doesn't render as `C:/[...]/x`. Splitting accepts both separators —
    // crucially so a `\`-path isn't treated as one giant component (which
    // previously forced the crude end-truncation branch on Windows).
    let sep = path_display_sep(&path_str);

    // If path fits, return as-is
    if path_str.len() <= max_len {
        return TruncatedPath {
            prefix: String::new(),
            truncated: false,
            suffix: path_str.to_string(),
            sep,
        };
    }

    let components: Vec<&str> = path_str
        .split(['/', '\\'])
        .filter(|s| !s.is_empty())
        .collect();

    if components.is_empty() {
        return TruncatedPath {
            prefix: sep.to_string(),
            truncated: false,
            suffix: String::new(),
            sep,
        };
    }

    // Keep "root + first directory" as the prefix, like the Unix display
    // (`/private/[...]`). A Windows drive letter ("C:") plays the part of
    // the root, so keep `C:\<firstdir>` to stay symmetric instead of just
    // the bare drive.
    let leading_sep = path_str.starts_with('/') || path_str.starts_with('\\');
    let is_drive = |c: &str| {
        let b = c.as_bytes();
        b.len() == 2 && b[1] == b':' && b[0].is_ascii_alphabetic()
    };
    let prefix_count = if !leading_sep && is_drive(components[0]) {
        2
    } else {
        1
    }
    .min(components.len());
    let sep_str = sep.to_string();
    let prefix = {
        let joined = components[..prefix_count].join(&sep_str);
        if leading_sep {
            format!("{}{}", sep, joined)
        } else {
            joined
        }
    };

    // The "<sep>[...]" marker takes 6 bytes (separator + "[...]").
    let ellipsis_len = sep.len_utf8() + "[...]".len();

    // Calculate how much space we have for the suffix
    let available_for_suffix = max_len.saturating_sub(prefix.len() + ellipsis_len);

    if available_for_suffix < 5 || components.len() <= prefix_count {
        // Not enough space or nothing past the prefix, just truncate the
        // end. Walk back to a char boundary so paths with non-ASCII
        // components (e.g. `/home/ユーザー/project`) don't byte-slice
        // through a multi-byte UTF-8 sequence and panic (same class as
        // #1718).
        let truncated_path = if path_str.len() > max_len.saturating_sub(3) {
            let cut = path_str.floor_char_boundary(max_len.saturating_sub(3));
            format!("{}...", &path_str[..cut])
        } else {
            path_str.to_string()
        };
        return TruncatedPath {
            prefix: String::new(),
            truncated: false,
            suffix: truncated_path,
            sep,
        };
    }

    // Build suffix from the last components that fit
    let mut suffix_parts: Vec<&str> = Vec::new();
    let mut suffix_len = 0;

    for component in components.iter().skip(prefix_count).rev() {
        let component_len = component.len() + 1; // +1 for the separator
        if suffix_len + component_len <= available_for_suffix {
            suffix_parts.push(component);
            suffix_len += component_len;
        } else {
            break;
        }
    }

    suffix_parts.reverse();

    // If we included all remaining components, no truncation needed
    if suffix_parts.len() == components.len() - prefix_count {
        return TruncatedPath {
            prefix: String::new(),
            truncated: false,
            suffix: path_str.to_string(),
            sep,
        };
    }

    let suffix = if suffix_parts.is_empty() {
        // Can't fit any suffix components, truncate the last component.
        // floor_char_boundary keeps the slice on a valid UTF-8 boundary
        // when `last` contains non-ASCII characters.
        let last = components.last().unwrap_or(&"");
        let truncate_to = available_for_suffix.saturating_sub(4); // "/.." and some chars
        if truncate_to > 0 && last.len() > truncate_to {
            let cut = last.floor_char_boundary(truncate_to);
            format!("{}{}...", sep, &last[..cut])
        } else {
            format!("{}{}", sep, last)
        }
    } else {
        format!("{}{}", sep, suffix_parts.join(&sep_str))
    };

    TruncatedPath {
        prefix,
        truncated: true,
        suffix,
        sep,
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

/// Minimum column-width to reserve for the column number portion of the
/// cursor indicator. Chosen so the bar stays stable across lines with up
/// to 3-digit column numbers without showing leading padding for the
/// common single-digit case (the text is suffix-padded, not number-padded).
const CURSOR_COL_RESERVE: usize = 3;

/// Compute the cursor column as the number of grapheme clusters between the
/// start of the cursor's line and the cursor. The line start is derived from
/// the live cursor byte position (not a cached line number), so it stays
/// correct in diff/split views where the two can disagree. Counting graphemes
/// — rather than bytes or code points — keeps the reported column consistent
/// with the editor's grapheme-based cursor movement.
fn cursor_column(buffer: &mut crate::model::buffer::TextBuffer, cursor_position: usize) -> usize {
    let mut iter = buffer.line_iterator(cursor_position, 80);
    let line_start = iter.current_position();
    let byte_col = cursor_position.saturating_sub(line_start);
    if byte_col == 0 {
        return 0;
    }
    // Prefer counting grapheme clusters over the line's text so multi-byte
    // characters advance the column by one (issue #2090). Composite/diff
    // buffers don't expose readable line content here; in that case fall back
    // to the byte distance, which equals the grapheme count for the ASCII
    // content those views render and matches the prior behavior.
    match iter.next_line() {
        Some((_, text)) if text.len() >= byte_col => {
            let mut end = byte_col;
            while end > 0 && !text.is_char_boundary(end) {
                end -= 1;
            }
            crate::primitives::grapheme::grapheme_count(&text[..end])
        }
        _ => byte_col,
    }
}

/// Format the cursor's `Ln X, Col Y` indicator so its rendered width is
/// stable as the cursor moves. The numbers themselves are emitted with
/// their natural width — preserving the format existing tests and screen-
/// readers rely on — and trailing spaces are appended to reach a minimum
/// width derived from the buffer's total line count and a fixed reserve
/// for the column number. Fixes the status bar shifting reported in
/// issue #1967.
fn format_cursor_position(line: usize, col: usize, line_count: usize) -> String {
    let text = format!("Ln {line}, Col {col}");
    let line_digits = line_count.max(1).to_string().len();
    // "Ln , Col " literals are 9 ASCII chars.
    let min_width = 9 + line_digits + CURSOR_COL_RESERVE;
    if text.len() < min_width {
        format!("{text:<min_width$}")
    } else {
        text
    }
}

/// Compact variant of `format_cursor_position`, used by
/// `StatusBarElement::CursorCompact`. Renders as `line:col` with the same
/// stable-width trailing-space strategy.
fn format_cursor_position_compact(line: usize, col: usize, line_count: usize) -> String {
    let text = format!("{line}:{col}");
    let line_digits = line_count.max(1).to_string().len();
    // ":" literal is 1 ASCII char.
    let min_width = 1 + line_digits + CURSOR_COL_RESERVE;
    if text.len() < min_width {
        format!("{text:<min_width$}")
    } else {
        text
    }
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
        rec: Option<&mut CellThemeRecorder>,
    ) -> StatusBarLayout {
        Self::render_status(frame, area, ctx, config, rec)
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
                sep: path_display_sep(&dir_path),
            }
        };

        // Build the directory display with separate spans for styling
        if truncated.truncated {
            // Prefix (dimmed)
            spans.push(Span::styled(truncated.prefix.clone(), dir_style));
            // Ellipsis "<sep>[...]" (highlighted)
            spans.push(Span::styled(
                format!("{}[...]", truncated.sep),
                ellipsis_style,
            ));
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
        // Buffer-specific elements have nothing meaningful to show when
        // the active buffer is just a synthesized placeholder kept alive
        // for editor invariants. Suppress them so the status bar tells
        // the truth: there's no real file open.
        if ctx.is_synthetic_placeholder
            && matches!(
                element,
                StatusBarElement::Filename
                    | StatusBarElement::Cursor
                    | StatusBarElement::CursorCompact
                    | StatusBarElement::CursorCount
                    | StatusBarElement::Diagnostics
                    | StatusBarElement::LineEnding
                    | StatusBarElement::Encoding
                    | StatusBarElement::Language
            )
        {
            return None;
        }
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
                // The `[Container:<id>] ` / `<SSH_PREFIX>conn<...>`
                // prefix is redundant when the dedicated `{remote}`
                // indicator is on the bar — same identity, two
                // places. Skip it then. When `{remote}` is NOT on
                // the bar, keep the prefix so users still see the
                // connection at a glance from the filename.
                let remote_prefix = if ctx.remote_indicator_on_bar {
                    String::new()
                } else {
                    ctx.remote_connection
                        .map(|conn| {
                            if conn.starts_with("Container:") {
                                format!("[{}] ", conn)
                            } else {
                                format!("{SSH_PREFIX}{conn}{SSH_PREFIX_TERMINATOR}")
                            }
                        })
                        .unwrap_or_default()
                };
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
                Some(RenderedElement {
                    text,
                    kind,
                    token_key: None,
                })
            }
            StatusBarElement::Cursor => {
                if !ctx.state.show_cursors {
                    return None;
                }
                let cursor = *ctx.cursors.primary();
                let line_count = ctx.state.buffer.line_count();
                let text = if let Some(lc) = line_count {
                    let line = ctx.state.primary_cursor_line_number.value();
                    let col = cursor_column(&mut ctx.state.buffer, cursor.position);
                    format_cursor_position(line + 1, col + 1, lc)
                } else {
                    format!("Byte {}", cursor.position)
                };
                Some(RenderedElement {
                    text,
                    kind: ElementKind::Normal,
                    token_key: None,
                })
            }
            StatusBarElement::CursorCompact => {
                if !ctx.state.show_cursors {
                    return None;
                }
                let cursor = *ctx.cursors.primary();
                let line_count = ctx.state.buffer.line_count();
                let text = if let Some(lc) = line_count {
                    let line = ctx.state.primary_cursor_line_number.value();
                    let col = cursor_column(&mut ctx.state.buffer, cursor.position);
                    format_cursor_position_compact(line + 1, col + 1, lc)
                } else {
                    format!("{}", cursor.position)
                };
                Some(RenderedElement {
                    text,
                    kind: ElementKind::Normal,
                    token_key: None,
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
                    token_key: None,
                })
            }
            StatusBarElement::CursorCount => {
                if ctx.cursors.count() <= 1 {
                    return None;
                }
                Some(RenderedElement {
                    text: t!("status.cursors", count = ctx.cursors.count()).to_string(),
                    kind: ElementKind::Normal,
                    token_key: None,
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
                    token_key: None,
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
                    token_key: None,
                })
            }
            StatusBarElement::LineEnding => Some(RenderedElement {
                text: ctx.state.buffer.line_ending().display_name().to_string(),
                kind: ElementKind::LineEnding,
                token_key: None,
            }),
            StatusBarElement::Encoding => Some(RenderedElement {
                text: ctx.state.buffer.encoding().display_name().to_string(),
                kind: ElementKind::Encoding,
                token_key: None,
            }),
            StatusBarElement::Language => {
                let text = if ctx.state.language == "text"
                    && ctx.state.display_name != "Text"
                    && ctx.state.display_name != "Plain Text"
                    && ctx.state.display_name != "text"
                {
                    format!("{} [syntax only]", &ctx.state.display_name)
                } else {
                    ctx.state.display_name.to_string()
                };
                Some(RenderedElement {
                    text,
                    kind: ElementKind::Language,
                    token_key: None,
                })
            }
            StatusBarElement::Lsp => {
                if ctx.lsp_status.is_empty() {
                    return None;
                }
                Some(RenderedElement {
                    text: ctx.lsp_status.to_string(),
                    kind: ElementKind::Lsp,
                    token_key: None,
                })
            }
            StatusBarElement::Warnings => {
                if ctx.general_warning_count == 0 {
                    return None;
                }
                Some(RenderedElement {
                    text: format!("[\u{26a0} {}]", ctx.general_warning_count),
                    kind: ElementKind::WarningBadge,
                    token_key: None,
                })
            }
            StatusBarElement::Update => {
                let version = ctx.update_available?;
                Some(RenderedElement {
                    text: t!("status.update_available", version = version).to_string(),
                    kind: ElementKind::Update,
                    token_key: None,
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
                    text: t!("status.palette", shortcut = shortcut).to_string(),
                    kind: ElementKind::Palette,
                    token_key: None,
                })
            }
            StatusBarElement::Clock => {
                let now = chrono::Local::now();
                let text = format!("{:02}:{:02}", now.hour(), now.minute());
                Some(RenderedElement {
                    text,
                    kind: ElementKind::Clock,
                    token_key: None,
                })
            }
            StatusBarElement::RemoteIndicator => {
                // Persistent remote-authority entry point. When local we
                // still emit a short label so the indicator is visible —
                // the spec calls for a persistent control, not one that
                // vanishes when there is nothing to report.
                //
                // Precedence: plugin-supplied override (via
                // `SetRemoteIndicatorState`) wins over the authority-
                // derived state. The override carries its own label;
                // derived states synthesize one from `remote_connection`.
                let (text, state) = if let Some(over) = ctx.remote_state_override {
                    (over.label(), over.state())
                } else {
                    match ctx.remote_connection {
                        None => ("Local".to_string(), RemoteIndicatorState::Local),
                        Some(conn) if conn.contains("(Disconnected)") => {
                            (conn.to_string(), RemoteIndicatorState::Disconnected)
                        }
                        Some(conn) => (conn.to_string(), RemoteIndicatorState::Connected),
                    }
                };
                Some(RenderedElement {
                    text,
                    kind: ElementKind::RemoteIndicator(state),
                    token_key: None,
                })
            }
            StatusBarElement::WorkspaceTrust => {
                // Always-present trust control, read from the active session's
                // trust level each frame. Persistent like `{remote}` — it never
                // vanishes, so the user always knows whether repo-controlled
                // execution is gated. Capitalized for a status-bar label.
                use crate::services::workspace_trust::TrustLevel;
                let level = ctx.workspace_trust_level;
                let text = match level {
                    TrustLevel::Trusted => t!("statusbar.trust.trusted"),
                    TrustLevel::Restricted => t!("statusbar.trust.restricted"),
                    TrustLevel::Blocked => t!("statusbar.trust.blocked"),
                }
                .to_string();
                Some(RenderedElement {
                    text,
                    kind: ElementKind::WorkspaceTrust(level),
                    token_key: None,
                })
            }
            StatusBarElement::CustomToken(key) => {
                if let Some(value) = ctx.dynamic_status_bar_elements.get(key) {
                    Some(RenderedElement {
                        text: value.clone(),
                        kind: ElementKind::Custom,
                        token_key: Some(key.clone()),
                    })
                } else {
                    None // Skip rendering if no value set
                }
            }
        }
    }

    /// Get the style for a rendered element based on its kind, theme, and hover state.
    fn element_style(
        kind: ElementKind,
        theme: &crate::view::theme::Theme,
        hover: StatusBarHover,
        _warning_level: WarningLevel,
        lsp_state: LspIndicatorState,
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
                // Color by LSP state:
                //   Error  → diagnostic_error_*       (red-ish; problem)
                //   Off    → status_lsp_actionable_*  (prominent; click to act)
                //   On     → status_lsp_on_*          (neutral; healthy)
                //   Dismissed/None → status-bar palette (muted; nothing to do)
                //
                // Off is the indicator's main signal that the user has
                // useful options behind a click — drawn prominently so
                // it stands out in the status bar without auto-popping
                // a dialog.
                let (fg, bg) = match lsp_state {
                    LspIndicatorState::Error => {
                        (theme.diagnostic_error_fg, theme.diagnostic_error_bg)
                    }
                    LspIndicatorState::Off => (
                        theme.status_lsp_actionable_fg,
                        theme.status_lsp_actionable_bg,
                    ),
                    LspIndicatorState::On => (theme.status_lsp_on_fg, theme.status_lsp_on_bg),
                    LspIndicatorState::OffDismissed => (theme.status_bar_fg, theme.status_bar_bg),
                    LspIndicatorState::None => (theme.status_bar_fg, theme.status_bar_bg),
                };
                let mut style = Style::default().fg(fg).bg(bg);
                // Always underline on hover — the indicator is clickable
                // in all non-empty states.  Previously we only underlined
                // when warning_level != None, so "LSP (on)" gave no hover
                // cue that it was clickable.
                if is_hovering && lsp_state != LspIndicatorState::None {
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
            // The palette shortcut hint is purely informational — driven
            // by the dedicated `status_palette_*` theme keys (default
            // to the neutral status-bar palette so it blends into the
            // bar instead of breaking the color band at the right edge).
            ElementKind::Palette => Style::default()
                .fg(theme.status_palette_fg)
                .bg(theme.status_palette_bg),
            ElementKind::Custom => Style::default()
                .fg(theme.status_bar_fg)
                .bg(theme.status_bar_bg),
            ElementKind::RemoteIndicator(state) => {
                let is_hovering = hover == StatusBarHover::RemoteIndicator;
                let (fg, bg) = match state {
                    // Connecting and Connected share the "help
                    // indicator" palette so the transition from one to
                    // the other is a glyph swap rather than a color
                    // flash — the user's eye tracks the indicator
                    // changing, not disappearing.
                    RemoteIndicatorState::Connecting | RemoteIndicatorState::Connected => {
                        (theme.help_indicator_fg, theme.help_indicator_bg)
                    }
                    // FailedAttach + Disconnected share the error
                    // palette. Both are "the remote isn't reaching you
                    // right now" states, differing only in cause.
                    RemoteIndicatorState::FailedAttach | RemoteIndicatorState::Disconnected => (
                        theme.status_error_indicator_fg,
                        theme.status_error_indicator_bg,
                    ),
                    // Local: neutral status-bar palette.
                    RemoteIndicatorState::Local => (theme.status_bar_fg, theme.status_bar_bg),
                };
                let mut style = Style::default().fg(fg).bg(bg);
                if is_hovering {
                    style = style.add_modifier(Modifier::UNDERLINED);
                }
                style
            }
            ElementKind::WorkspaceTrust(level) => {
                use crate::services::workspace_trust::TrustLevel;
                let is_hovering = hover == StatusBarHover::WorkspaceTrust;
                let (fg, bg) = match level {
                    // Gated states reuse the warning indicator palette so
                    // "execution is restricted/blocked" reads at a glance.
                    TrustLevel::Restricted | TrustLevel::Blocked => (
                        theme.status_warning_indicator_fg,
                        theme.status_warning_indicator_bg,
                    ),
                    // Trusted: neutral status-bar palette (everything-works).
                    TrustLevel::Trusted => (theme.status_bar_fg, theme.status_bar_bg),
                };
                let mut style = Style::default().fg(fg).bg(bg);
                if is_hovering {
                    style = style.add_modifier(Modifier::UNDERLINED);
                }
                style
            }
        }
    }

    /// The (fg, bg) theme-key strings an element paints with — its non-hover
    /// provenance for the theme inspector, mirroring `element_style`. Hover is
    /// transient so the recorded key is always the element's semantic key.
    fn element_keys(
        kind: ElementKind,
        lsp_state: LspIndicatorState,
    ) -> (&'static str, &'static str) {
        match kind {
            ElementKind::Normal
            | ElementKind::Messages
            | ElementKind::Clock
            | ElementKind::Custom
            | ElementKind::LineEnding
            | ElementKind::Encoding
            | ElementKind::Language => ("ui.status_bar_fg", "ui.status_bar_bg"),
            ElementKind::RemoteDisconnected => (
                "ui.status_error_indicator_fg",
                "ui.status_error_indicator_bg",
            ),
            ElementKind::Lsp => match lsp_state {
                LspIndicatorState::Error => ("diagnostic.error_fg", "diagnostic.error_bg"),
                LspIndicatorState::Off => {
                    ("ui.status_lsp_actionable_fg", "ui.status_lsp_actionable_bg")
                }
                LspIndicatorState::On => ("ui.status_lsp_on_fg", "ui.status_lsp_on_bg"),
                LspIndicatorState::OffDismissed | LspIndicatorState::None => {
                    ("ui.status_bar_fg", "ui.status_bar_bg")
                }
            },
            ElementKind::WarningBadge => (
                "ui.status_warning_indicator_fg",
                "ui.status_warning_indicator_bg",
            ),
            ElementKind::Update => ("ui.menu_highlight_fg", "ui.menu_dropdown_bg"),
            ElementKind::Palette => ("ui.status_palette_fg", "ui.status_palette_bg"),
            ElementKind::RemoteIndicator(state) => match state {
                RemoteIndicatorState::Connecting | RemoteIndicatorState::Connected => {
                    ("ui.help_indicator_fg", "ui.help_indicator_bg")
                }
                RemoteIndicatorState::FailedAttach | RemoteIndicatorState::Disconnected => (
                    "ui.status_error_indicator_fg",
                    "ui.status_error_indicator_bg",
                ),
                RemoteIndicatorState::Local => ("ui.status_bar_fg", "ui.status_bar_bg"),
            },
            ElementKind::WorkspaceTrust(level) => {
                use crate::services::workspace_trust::TrustLevel;
                match level {
                    TrustLevel::Restricted | TrustLevel::Blocked => (
                        "ui.status_warning_indicator_fg",
                        "ui.status_warning_indicator_bg",
                    ),
                    TrustLevel::Trusted => ("ui.status_bar_fg", "ui.status_bar_bg"),
                }
            }
        }
    }

    /// Map a rendered element to the layout field(s) it should populate.
    /// Built-in indicators get their dedicated `Option<(row, start_col,
    /// end_col)>` slot. Plugin tokens (`ElementKind::Custom` carrying a
    /// `token_key`) get an entry in `plugin_token_areas`, keyed by the
    /// plugin's registry key — that's what `handle_click_status_bar`
    /// uses to dispatch clicks back to the right plugin.
    fn update_layout_for_element(
        layout: &mut StatusBarLayout,
        kind: ElementKind,
        token_key: Option<&str>,
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
            ElementKind::RemoteIndicator(_) => {
                layout.remote_indicator = Some((row, start_col, end_col))
            }
            ElementKind::WorkspaceTrust(_) => {
                layout.trust_indicator = Some((row, start_col, end_col))
            }
            ElementKind::Custom => {
                if let Some(key) = token_key {
                    layout
                        .plugin_token_areas
                        .insert(key.to_string(), (row, start_col, end_col));
                }
            }
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
        lsp_state: LspIndicatorState,
    ) -> (Vec<Span<'static>>, usize) {
        let base_style = Style::default()
            .fg(theme.status_bar_fg)
            .bg(theme.status_bar_bg);
        // Each entry carries a one-space margin on each side painted in its own
        // style, so entries with a distinct background (LSP / warnings / update
        // / palette / remote) render as a padded pill. The separator is then a
        // bare glyph drawn between these padded entries.
        let width = str_width(&rendered.text) + 2;

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
                        Span::styled(" ", error_style),
                        Span::styled(prefix, error_style),
                        Span::styled(rest, base_style),
                        Span::styled(" ", base_style),
                    ],
                    width,
                );
            }
            return (
                vec![
                    Span::styled(" ", error_style),
                    Span::styled(rendered.text.clone(), error_style),
                    Span::styled(" ", error_style),
                ],
                width,
            );
        }

        let style = Self::element_style(rendered.kind, theme, hover, warning_level, lsp_state);
        let mut spans = vec![Span::styled(" ", style)];
        if rendered.kind == ElementKind::Clock {
            // "HH:MM" — blink the colon via terminal hardware (SGR 5)
            spans.push(Span::styled(rendered.text[..2].to_string(), style));
            spans.push(Span::styled(
                ":".to_string(),
                style.add_modifier(Modifier::SLOW_BLINK),
            ));
            spans.push(Span::styled(rendered.text[3..].to_string(), style));
        } else {
            spans.push(Span::styled(rendered.text.clone(), style));
        }
        spans.push(Span::styled(" ", style));
        (spans, width)
    }

    /// Render a configured side (left/right) into styled per-element groups.
    /// Each tuple carries the rendered spans, total width, the kind tag
    /// (for layout/click-area routing of built-ins), and the plugin
    /// token key (`Some` only for `ElementKind::Custom`) so the
    /// placement loops can record the screen area under the same key
    /// the plugin registered.
    fn render_side(
        config_side: &[StatusBarElement],
        ctx: &mut StatusBarContext<'_>,
    ) -> Vec<(Vec<Span<'static>>, usize, ElementKind, Option<String>)> {
        let rendered: Vec<RenderedElement> = config_side
            .iter()
            .filter_map(|elem| Self::render_element(elem, ctx))
            .filter(|e| !e.text.is_empty())
            .collect();

        let theme = ctx.theme;
        let hover = ctx.hover;
        let warning_level = ctx.warning_level;
        let lsp_state = ctx.lsp_indicator_state;
        rendered
            .into_iter()
            .map(|r| {
                let kind = r.kind;
                let token_key = r.token_key.clone();
                let (spans, width) =
                    Self::element_spans(&r, theme, hover, warning_level, lsp_state);
                (spans, width, kind, token_key)
            })
            .collect()
    }

    /// Render the normal status bar (config-driven).
    fn render_status(
        frame: &mut Frame,
        area: Rect,
        ctx: &mut StatusBarContext<'_>,
        config: &StatusBarConfig,
        mut rec: Option<&mut CellThemeRecorder>,
    ) -> StatusBarLayout {
        let mut layout = StatusBarLayout::default();
        let base_style = Style::default()
            .fg(ctx.theme.status_bar_fg)
            .bg(ctx.theme.status_bar_bg);
        let available_width = area.width as usize;

        if available_width == 0 || area.height == 0 {
            return layout;
        }

        // Lay down the bar's base keys across the whole row so padding / gaps
        // resolve to the status bar; each element below overwrites its own
        // cells with their specific keys as it's emitted.
        let lsp_state = ctx.lsp_indicator_state;
        if let Some(r) = rec.as_deref_mut() {
            r.run(
                area.x,
                area.y,
                area.width,
                Some("ui.status_bar_fg"),
                Some("ui.status_bar_bg"),
                "Status Bar",
            );
        }

        // Tell the per-element renderer whether the dedicated
        // RemoteIndicator is on the bar so the Filename branch
        // can drop its now-redundant `[Container:<id>] ` /
        // SSH prefix.
        ctx.remote_indicator_on_bar = config
            .left
            .iter()
            .chain(config.right.iter())
            .any(|e| matches!(e, StatusBarElement::RemoteIndicator));

        let left_items = Self::render_side(&config.left, ctx);
        let mut right_items = Self::render_side(&config.right, ctx);

        // Separator drawn between elements, used verbatim from config.
        // An empty value disables separators and consumes no width.
        let separator: &str = &config.separator;
        let separator_width = str_width(separator);
        // The separator glyph is colored by the theme's dedicated separator
        // keys so it can be dimmed against the bar; both fall back to the bar.
        let separator_style = Style::default()
            .fg(ctx.theme.status_separator_fg)
            .bg(ctx.theme.status_separator_bg);

        // Reserve a sane minimum for the left side so the buffer name and
        // cursor position aren't truncated to a single character on narrow
        // terminals (regression originally reported as
        // `t  LF  ASCII  Markdown ...`).  Drop low-priority right elements
        // (configured right-most first) until the remaining right side fits
        // alongside that minimum left budget.  We never drop the *first*
        // right element so the user keeps at least one piece of right-side
        // status if any was configured.
        let total_right_width: usize = right_items.iter().map(|(_, w, _, _)| *w).sum::<usize>()
            + separator_width * right_items.len().saturating_sub(1);
        let left_min_target = available_width
            .saturating_mul(2)
            .saturating_div(5) // ~40% of width reserved for left when feasible
            .min(40); // but never demand more than 40 cols even on wide terminals
        let right_budget = available_width.saturating_sub(left_min_target + 1);
        if total_right_width > right_budget && right_items.len() > 1 {
            let mut current = total_right_width;
            while current > right_budget && right_items.len() > 1 {
                if let Some(dropped) = right_items.pop() {
                    current = current.saturating_sub(dropped.1);
                    // Also remove the separator that preceded the dropped
                    // element (always present since we never drop the first)
                    current = current.saturating_sub(separator_width);
                } else {
                    break;
                }
            }
        }

        let right_width: usize = right_items.iter().map(|(_, w, _, _)| *w).sum::<usize>()
            + separator_width * right_items.len().saturating_sub(1);

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

        for (idx, (item_spans, width, kind, token_key)) in left_items.into_iter().enumerate() {
            let sep_width = if idx == 0 { 0 } else { separator_width };
            if used_left + sep_width >= left_max_width {
                break;
            }
            if sep_width > 0 {
                if let Some(r) = rec.as_deref_mut() {
                    r.run(
                        area.x + used_left as u16,
                        area.y,
                        sep_width as u16,
                        Some("ui.status_separator_fg"),
                        Some("ui.status_separator_bg"),
                        "Status Bar",
                    );
                }
                spans.push(Span::styled(separator.to_string(), separator_style));
                used_left += sep_width;
            }

            let remaining = left_max_width - used_left;
            let start_col = used_left;

            if width <= remaining {
                spans.extend(item_spans);
                used_left += width;

                if let Some(r) = rec.as_deref_mut() {
                    let (fg, bg) = Self::element_keys(kind, lsp_state);
                    r.run(
                        area.x + start_col as u16,
                        area.y,
                        width as u16,
                        Some(fg),
                        Some(bg),
                        "Status Bar",
                    );
                }

                Self::update_layout_for_element(
                    &mut layout,
                    kind,
                    token_key.as_deref(),
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
                let overflow_style = Self::element_style(
                    kind,
                    ctx.theme,
                    ctx.hover,
                    ctx.warning_level,
                    ctx.lsp_indicator_state,
                );
                spans.push(Span::styled(truncated, overflow_style));

                if let Some(r) = rec.as_deref_mut() {
                    let (fg, bg) = Self::element_keys(kind, lsp_state);
                    r.run(
                        area.x + start_col as u16,
                        area.y,
                        truncated_width as u16,
                        Some(fg),
                        Some(bg),
                        "Status Bar",
                    );
                }
                used_left += truncated_width;

                Self::update_layout_for_element(
                    &mut layout,
                    kind,
                    token_key.as_deref(),
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
        for (idx, (item_spans, width, kind, token_key)) in right_items.into_iter().enumerate() {
            if idx > 0 && separator_width > 0 {
                if let Some(r) = rec.as_deref_mut() {
                    r.run(
                        current_col,
                        area.y,
                        separator_width as u16,
                        Some("ui.status_separator_fg"),
                        Some("ui.status_separator_bg"),
                        "Status Bar",
                    );
                }
                spans.push(Span::styled(separator.to_string(), separator_style));
                current_col += separator_width as u16;
            }
            if let Some(r) = rec.as_deref_mut() {
                let (fg, bg) = Self::element_keys(kind, lsp_state);
                r.run(
                    current_col,
                    area.y,
                    width as u16,
                    Some(fg),
                    Some(bg),
                    "Status Bar",
                );
            }
            Self::update_layout_for_element(
                &mut layout,
                kind,
                token_key.as_deref(),
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

        // Helper to look up keybinding for an action. The search-option toggles
        // live in the SearchPrompt context; fall back to Prompt then Global so a
        // user override in either still surfaces in the hint.
        let get_shortcut = |action: &crate::input::keybindings::Action| -> Option<String> {
            keybindings
                .get_keybinding_for_action(
                    action,
                    crate::input::keybindings::KeyContext::SearchPrompt,
                )
                .or_else(|| {
                    keybindings.get_keybinding_for_action(
                        action,
                        crate::input::keybindings::KeyContext::Prompt,
                    )
                })
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

        // Style for active (checked) options. Keeps the same `menu_dropdown_bg`
        // as the rest of the toolbar (so a checked option doesn't sprout a
        // jarring color block) and signals the checked state purely through an
        // accent foreground. `help_key_fg` is the right key for this: it's the
        // theme's "accent text on a panel/popup body" color — every theme tunes
        // it to contrast with the dropdown/popup background, and it's the same
        // key the plugin search panel's toggle uses for its checked glyph, so
        // the two search UIs read consistently. The previous code used
        // `menu_highlight_fg`, which is designed to pair with `menu_highlight_bg`
        // and on Dracula equals `menu_dropdown_bg` ([40,42,54]) — so the checked
        // checkbox rendered invisible fg-on-same-bg.
        let active_style = Style::default()
            .fg(theme.help_key_fg)
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
    fn test_truncate_path_multibyte_single_component_does_not_panic() {
        // Routes into the "truncate the end" branch (line 414): the prefix
        // alone exceeds max_len, so available_for_suffix becomes 0. Before
        // the fix, byte-slicing `path_str` at `max_len - 3 = 2` lands
        // inside the 3-byte UTF-8 sequence for `ユ` and panicked the
        // editor — same class as #1718.
        let path = PathBuf::from("/ユーザーのプロジェクト名前/file");
        let result = truncate_path(&path, 5);
        let display = result.to_string_plain();
        assert!(display.is_char_boundary(display.len()));
        assert!(display.ends_with("..."));
    }

    #[test]
    fn test_truncate_path_multibyte_last_component_does_not_panic() {
        // Routes into the "truncate the last component" branch (line 453):
        // available_for_suffix is large enough to enter the suffix-build
        // loop, but the only remaining component doesn't fit, so we fall
        // back to truncating it. Before the fix, byte-slicing the
        // non-ASCII component at `truncate_to = 1` lands inside the 3-byte
        // UTF-8 sequence for `ユ` and panicked.
        let path = PathBuf::from("/a/ユーザーのプロジェクト名前");
        let result = truncate_path(&path, 13);
        let display = result.to_string_plain();
        assert!(display.is_char_boundary(display.len()));
    }

    #[test]
    fn test_truncated_path_to_string_plain() {
        let truncated = TruncatedPath {
            prefix: "/home".to_string(),
            truncated: true,
            suffix: "/project/src".to_string(),
            sep: '/',
        };

        assert_eq!(truncated.to_string_plain(), "/home/[...]/project/src");
    }

    #[test]
    fn test_truncated_path_to_string_plain_no_truncation() {
        let truncated = TruncatedPath {
            prefix: String::new(),
            truncated: false,
            suffix: "/home/user/project".to_string(),
            sep: '/',
        };

        assert_eq!(truncated.to_string_plain(), "/home/user/project");
    }

    /// A Windows-style "\"-path must middle-truncate (keeping drive +
    /// first dir and the tail) and render with backslashes — not fall into
    /// the crude end-truncation that `split('/')` forced because a
    /// backslash path has no '/' to split on. (We can exercise this on any
    /// OS because `truncate_path` works on the path *string*.)
    #[test]
    fn test_truncate_path_windows_backslashes() {
        let path = Path::new(r"C:\Users\me\projects\fresh\crates\editor\src\main.rs");
        let t = truncate_path(path, 34);
        assert!(t.truncated, "long backslash path should middle-truncate");
        assert_eq!(t.sep, '\\', "should re-join with backslashes");
        let shown = t.to_string_plain();
        assert!(
            shown.starts_with(r"C:\Users"),
            "keeps drive + first dir: {shown}"
        );
        assert!(
            shown.contains(r"\[...]\"),
            "uses a backslash ellipsis: {shown}"
        );
        assert!(shown.ends_with("main.rs"), "keeps the tail: {shown}");
        assert!(!shown.contains('/'), "no forward slashes leak in: {shown}");
        assert!(shown.len() <= 34, "respects max_len: {shown}");
    }

    /// A short backslash path that fits is returned unchanged.
    #[test]
    fn test_truncate_path_windows_short_unchanged() {
        let path = Path::new(r"C:\a\b");
        let t = truncate_path(path, 80);
        assert!(!t.truncated);
        assert_eq!(t.to_string_plain(), r"C:\a\b");
    }

    #[test]
    fn test_remote_indicator_element_kind_equality() {
        // Each lifecycle state produces a distinct ElementKind so the styler
        // can pick the right palette for Local / Connecting / Connected /
        // FailedAttach / Disconnected.
        assert_eq!(
            ElementKind::RemoteIndicator(RemoteIndicatorState::Local),
            ElementKind::RemoteIndicator(RemoteIndicatorState::Local)
        );
        let distinct = [
            RemoteIndicatorState::Local,
            RemoteIndicatorState::Connecting,
            RemoteIndicatorState::Connected,
            RemoteIndicatorState::FailedAttach,
            RemoteIndicatorState::Disconnected,
        ];
        for (i, a) in distinct.iter().enumerate() {
            for (j, b) in distinct.iter().enumerate() {
                if i == j {
                    continue;
                }
                assert_ne!(
                    ElementKind::RemoteIndicator(*a),
                    ElementKind::RemoteIndicator(*b),
                    "expected {:?} != {:?}",
                    a,
                    b
                );
            }
        }
    }

    #[test]
    fn test_remote_indicator_state_default_is_local() {
        // `Default` → `Local` is relied on by callers that construct the
        // indicator before a connection is known.
        assert_eq!(RemoteIndicatorState::default(), RemoteIndicatorState::Local);
    }

    #[test]
    fn test_remote_indicator_override_deserializes_kind_tags() {
        // Pins the wire shape the `SetRemoteIndicatorState` plugin op
        // accepts. A breaking change here would silently reject plugin
        // payloads after upgrade.
        let cases: &[(&str, RemoteIndicatorOverride)] = &[
            (r#"{"kind":"local"}"#, RemoteIndicatorOverride::Local),
            (
                r#"{"kind":"connecting","label":"Building"}"#,
                RemoteIndicatorOverride::Connecting {
                    label: Some("Building".into()),
                },
            ),
            (
                r#"{"kind":"connecting"}"#,
                RemoteIndicatorOverride::Connecting { label: None },
            ),
            (
                r#"{"kind":"connected","label":"Container:abc"}"#,
                RemoteIndicatorOverride::Connected {
                    label: Some("Container:abc".into()),
                },
            ),
            (
                r#"{"kind":"failed_attach","error":"exit 1"}"#,
                RemoteIndicatorOverride::FailedAttach {
                    error: Some("exit 1".into()),
                },
            ),
            (
                r#"{"kind":"disconnected","label":"Container:abc"}"#,
                RemoteIndicatorOverride::Disconnected {
                    label: Some("Container:abc".into()),
                },
            ),
        ];
        for (json, expected) in cases {
            let parsed: RemoteIndicatorOverride = serde_json::from_str(json)
                .unwrap_or_else(|e| panic!("failed to parse {}: {}", json, e));
            assert_eq!(&parsed, expected, "wire shape mismatch for {}", json);
        }
    }

    #[test]
    fn test_remote_indicator_override_labels() {
        // Labels surface in the `{remote}` element text directly, so
        // defaults matter — a missing `label` must still produce
        // something readable.
        let connecting = RemoteIndicatorOverride::Connecting { label: None };
        assert!(
            connecting.label().contains("Connecting"),
            "connecting default label should mention Connecting, got {:?}",
            connecting.label()
        );

        let connecting_labeled = RemoteIndicatorOverride::Connecting {
            label: Some("Building".into()),
        };
        assert!(
            connecting_labeled.label().contains("Building"),
            "labeled connecting should include the label, got {:?}",
            connecting_labeled.label()
        );

        let failed_bare = RemoteIndicatorOverride::FailedAttach { error: None };
        assert_eq!(failed_bare.label(), "Attach failed");

        let failed_detail = RemoteIndicatorOverride::FailedAttach {
            error: Some("exit 1".into()),
        };
        assert!(
            failed_detail.label().contains("exit 1"),
            "failed with error should include the error, got {:?}",
            failed_detail.label()
        );
    }

    #[test]
    fn test_palette_and_lsp_on_use_dedicated_theme_keys() {
        // Repro for issue #1711: the Palette hint and the "LSP on"
        // indicator used distinct palettes (help-indicator and
        // diagnostic-info), causing the status bar's color band to
        // break at the far right.
        //
        // Now they're driven by dedicated theme keys whose defaults
        // resolve to the status-bar palette, so the bar reads as a
        // single continuous color out of the box, while still letting
        // themes override these elements independently. Off / Error
        // LSP states keep their vivid diagnostic palette so real
        // problems still pop.
        let theme = crate::view::theme::Theme::from_json(
            r#"{"name":"t","editor":{},"ui":{},"search":{},"diagnostic":{},"syntax":{}}"#,
        )
        .expect("minimal theme should parse");

        // Defaults: dedicated keys resolve to the status-bar palette.
        assert_eq!(theme.status_palette_fg, theme.status_bar_fg);
        assert_eq!(theme.status_palette_bg, theme.status_bar_bg);
        assert_eq!(theme.status_lsp_on_fg, theme.status_bar_fg);
        assert_eq!(theme.status_lsp_on_bg, theme.status_bar_bg);

        let palette_style = StatusBarRenderer::element_style(
            ElementKind::Palette,
            &theme,
            StatusBarHover::None,
            WarningLevel::None,
            LspIndicatorState::None,
        );
        assert_eq!(palette_style.fg, Some(theme.status_palette_fg));
        assert_eq!(palette_style.bg, Some(theme.status_palette_bg));

        let lsp_on_style = StatusBarRenderer::element_style(
            ElementKind::Lsp,
            &theme,
            StatusBarHover::None,
            WarningLevel::None,
            LspIndicatorState::On,
        );
        assert_eq!(lsp_on_style.fg, Some(theme.status_lsp_on_fg));
        assert_eq!(lsp_on_style.bg, Some(theme.status_lsp_on_bg));

        // Sanity: Off / Error must still differ from the status-bar
        // palette so they remain user-visible signals.
        let lsp_off_style = StatusBarRenderer::element_style(
            ElementKind::Lsp,
            &theme,
            StatusBarHover::None,
            WarningLevel::None,
            LspIndicatorState::Off,
        );
        assert_eq!(lsp_off_style.fg, Some(theme.status_lsp_actionable_fg));
        assert_eq!(lsp_off_style.bg, Some(theme.status_lsp_actionable_bg));

        let lsp_error_style = StatusBarRenderer::element_style(
            ElementKind::Lsp,
            &theme,
            StatusBarHover::None,
            WarningLevel::None,
            LspIndicatorState::Error,
        );
        assert_eq!(lsp_error_style.fg, Some(theme.diagnostic_error_fg));
        assert_eq!(lsp_error_style.bg, Some(theme.diagnostic_error_bg));
    }

    #[test]
    fn test_status_palette_and_lsp_on_keys_override_independently() {
        // A theme that only sets the new keys should produce styles
        // that follow the override, not the underlying status_bar_*
        // colors. This is the entire point of introducing dedicated
        // keys: themes can repaint these specific indicators without
        // touching the rest of the status bar.
        let theme_json = r#"{
            "name":"t",
            "editor":{},
            "ui":{
                "status_bar_fg":"White",
                "status_bar_bg":"DarkGray",
                "status_palette_fg":"Black",
                "status_palette_bg":"Yellow",
                "status_lsp_on_fg":"Black",
                "status_lsp_on_bg":"Cyan"
            },
            "search":{},
            "diagnostic":{},
            "syntax":{}
        }"#;
        let theme = crate::view::theme::Theme::from_json(theme_json).expect("theme should parse");
        assert_ne!(theme.status_palette_fg, theme.status_bar_fg);
        assert_ne!(theme.status_palette_bg, theme.status_bar_bg);
        assert_ne!(theme.status_lsp_on_fg, theme.status_bar_fg);
        assert_ne!(theme.status_lsp_on_bg, theme.status_bar_bg);
    }

    #[test]
    fn test_status_separator_keys_default_and_override() {
        // The separator glyph is painted by dedicated theme keys so it can
        // be dimmed against the bar. By default both resolve to the
        // status-bar palette, keeping the bar a single continuous color.
        let theme = crate::view::theme::Theme::from_json(
            r#"{"name":"t","editor":{},"ui":{},"search":{},"diagnostic":{},"syntax":{}}"#,
        )
        .expect("minimal theme should parse");
        assert_eq!(theme.status_separator_fg, theme.status_bar_fg);
        assert_eq!(theme.status_separator_bg, theme.status_bar_bg);

        // A theme that sets only the separator keys repaints the glyph
        // without touching the rest of the bar.
        let theme = crate::view::theme::Theme::from_json(
            r#"{
                "name":"t",
                "editor":{},
                "ui":{
                    "status_bar_fg":"White",
                    "status_bar_bg":"DarkGray",
                    "status_separator_fg":"Gray",
                    "status_separator_bg":"Black"
                },
                "search":{},
                "diagnostic":{},
                "syntax":{}
            }"#,
        )
        .expect("theme should parse");
        assert_ne!(theme.status_separator_fg, theme.status_bar_fg);
        assert_ne!(theme.status_separator_bg, theme.status_bar_bg);
    }

    #[test]
    fn test_remote_indicator_override_state_projection() {
        assert_eq!(
            RemoteIndicatorOverride::Local.state(),
            RemoteIndicatorState::Local
        );
        assert_eq!(
            RemoteIndicatorOverride::Connecting { label: None }.state(),
            RemoteIndicatorState::Connecting
        );
        assert_eq!(
            RemoteIndicatorOverride::Connected { label: None }.state(),
            RemoteIndicatorState::Connected
        );
        assert_eq!(
            RemoteIndicatorOverride::FailedAttach { error: None }.state(),
            RemoteIndicatorState::FailedAttach
        );
        assert_eq!(
            RemoteIndicatorOverride::Disconnected { label: None }.state(),
            RemoteIndicatorState::Disconnected
        );
    }

    // Regression coverage for issue #1967 — the cursor indicator must keep
    // a stable rendered width as the cursor moves so the bar doesn't
    // shift. The helpers reserve the digit count of the buffer's total
    // line count for the line number and `CURSOR_COL_RESERVE` for the
    // column number, suffix-padding the text without altering the
    // numbers themselves so existing screen assertions still see
    // literals like "Ln 1, Col 1".

    #[test]
    fn test_cursor_position_widths_stable_across_cursor_movement() {
        let line_count = 50;
        // Movement across a 50-line file (two-digit line_count) should
        // produce a constant rendered width regardless of cursor position.
        let widths: Vec<usize> = [(1, 1), (5, 12), (12, 5), (50, 100), (1, 1)]
            .into_iter()
            .map(|(ln, col)| format_cursor_position(ln, col, line_count).len())
            .collect();
        assert!(
            widths.windows(2).all(|w| w[0] == w[1]),
            "rendered widths drift across cursor movements: {widths:?}"
        );
    }

    #[test]
    fn test_cursor_position_preserves_natural_number_text() {
        // The natural "Ln 1, Col 1" substring must remain intact so
        // existing screen-content assertions (and screen readers) keep
        // working. Padding is suffix-only.
        let text = format_cursor_position(1, 1, 50);
        assert!(
            text.starts_with("Ln 1, Col 1"),
            "expected text to start with natural numbers, got {text:?}"
        );
        assert!(
            text.ends_with(' '),
            "expected trailing padding, got {text:?}"
        );
    }

    #[test]
    fn test_cursor_position_no_padding_for_single_line_buffer() {
        // For a single-line buffer the reserved line-digit width is 1,
        // so a small column number still produces the canonical
        // "Ln 1, Col 1" with reserve-only trailing padding.
        let text = format_cursor_position(1, 1, 1);
        // Min width = "Ln , Col ".len()(=9) + 1 (line_digits) + 3 (col reserve) = 13
        assert_eq!(text.len(), 13);
        assert!(text.starts_with("Ln 1, Col 1"));
    }

    #[test]
    fn test_cursor_position_does_not_shrink_below_actual() {
        // When the actual numbers exceed the reserve, the rendered text
        // is returned unmodified (rare wide-line case).
        let text = format_cursor_position(99, 99999, 50);
        assert_eq!(text, "Ln 99, Col 99999");
    }

    #[test]
    fn test_cursor_position_compact_widths_stable() {
        let line_count = 50;
        let widths: Vec<usize> = [(1, 1), (5, 12), (12, 5), (50, 100), (1, 1)]
            .into_iter()
            .map(|(ln, col)| format_cursor_position_compact(ln, col, line_count).len())
            .collect();
        assert!(
            widths.windows(2).all(|w| w[0] == w[1]),
            "compact widths drift across cursor movements: {widths:?}"
        );
    }

    #[test]
    fn test_cursor_position_compact_preserves_natural_text() {
        let text = format_cursor_position_compact(1, 1, 50);
        assert!(
            text.starts_with("1:1"),
            "expected text to start with natural numbers, got {text:?}"
        );
    }

    #[test]
    fn test_cursor_position_scales_with_line_count() {
        // Larger buffers reserve more line-digit width so that line
        // numbers at the high end of the buffer don't widen the bar.
        let short = format_cursor_position(1, 1, 9);
        let long = format_cursor_position(1, 1, 10_000);
        assert!(
            long.len() > short.len(),
            "wider buffers should reserve more width: {short:?} vs {long:?}"
        );
        // And the wide-buffer rendering should match what a top-of-file
        // line number near the buffer's high end would render to.
        let top = format_cursor_position(1, 1, 10_000);
        let high = format_cursor_position(9_999, 999, 10_000);
        assert_eq!(top.len(), high.len());
    }

    #[test]
    fn test_cursor_column_counts_chars_not_bytes() {
        let mut buf =
            crate::model::buffer::TextBuffer::from_str_test("hello\ncafé résumé\nworld\n");
        let line_start = buf.line_start_offset(1).unwrap();

        // 'r' starts at byte 6 ("café " = 5 chars / 6 bytes), char column 5.
        let col = cursor_column(&mut buf, line_start + 6);
        assert_eq!(
            col, 5,
            "cursor at 'r' should be column 5, not byte offset 6"
        );

        // 'é' starts at byte 3 (after "caf"), column 3.
        let col = cursor_column(&mut buf, line_start + 3);
        assert_eq!(col, 3, "cursor at 'é' should be column 3");

        // 'u' in "résumé" sits at byte 10, column 8.
        let col = cursor_column(&mut buf, line_start + 10);
        assert_eq!(col, 8, "cursor at 'u' should be column 8");
    }

    #[test]
    fn test_cursor_column_counts_grapheme_clusters() {
        // Line 1 is "e + combining acute" followed by 'x'. The accented 'e' is
        // two code points but one grapheme; counting graphemes (not chars or
        // bytes) keeps the column aligned with grapheme-based cursor movement.
        let mut buf = crate::model::buffer::TextBuffer::from_str_test("ab\ne\u{0301}x\n");
        let line_start = buf.line_start_offset(1).unwrap();

        // 'x' sits after the 1-byte 'e' and 2-byte combining accent (byte 3),
        // which is char column 2 but grapheme column 1.
        let col = cursor_column(&mut buf, line_start + 3);
        assert_eq!(
            col, 1,
            "accented 'e' is one grapheme; 'x' should be column 1, not 2"
        );
    }

    #[test]
    fn test_cursor_column_zwj_emoji_is_one_grapheme() {
        // Family emoji is several code points joined by ZWJ but a single
        // grapheme cluster (18 bytes).
        let mut buf = crate::model::buffer::TextBuffer::from_str_test("👨\u{200D}👩\u{200D}👧z\n");
        let line_start = buf.line_start_offset(0).unwrap();

        let col = cursor_column(&mut buf, line_start + 18);
        assert_eq!(col, 1, "ZWJ family emoji should count as one column");
    }

    #[test]
    fn test_cursor_column_at_line_start_is_zero() {
        let mut buf = crate::model::buffer::TextBuffer::from_str_test("hello\nworld\n");
        let line_start = buf.line_start_offset(1).unwrap();
        assert_eq!(cursor_column(&mut buf, line_start), 0);
    }
}
