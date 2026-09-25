//! The status bar's content: what each configured element says, the theme
//! keys it paints with, and what it answers to. Where it lands is
//! `view::shell::status_bar`'s.

use std::collections::HashMap;

use crate::app::shell_host::shell_theme::{Attrs, Ink};
use crate::config::{StatusBarElement, VirtualSpaceMode};
use crate::state::EditorState;
use crate::view::shell::status_bar::Item;
use chrono::Timelike;
use fresh_i18n::t;

/// Text that both marks a buffer as "edited over a disconnected SSH session"
/// and styles the prefix in the status bar. Kept as constants so `render_element`
/// and `element_runs` stay in sync.
const SSH_PREFIX: &str = "[SSH:";
const SSH_PREFIX_TERMINATOR: &str = "] ";

/// Stable identity of a *clickable* status-bar segment.
///
/// The element carrying it answers its own press and hover, the tree keys it
/// by this id (`view::shell::status_bar::clickable_key`) so a popup it opens
/// can hang off it, and `dispatch_status_bar_click` maps it to an editor
/// `Action`. Wiring a new clickable built-in element is: pass its id where
/// `StatusBarRenderer::render_element` builds it, and add one arm to that
/// dispatch.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StatusBarClickable {
    LineEnding,
    Encoding,
    Language,
    Lsp,
    Warnings,
    Messages,
    RemoteIndicator,
    WorkspaceTrust,
    ReadOnly,
    /// The "Update: vX.Y.Z" indicator — click to offer an in-editor update.
    Update,
    /// The restart indicator on a terminal buffer whose process quit — click
    /// to respawn it (resuming the agent conversation when there is one).
    RestartTerminal,
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
    /// Project into the Copy enum the indicator is styled by.
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

/// Three-state LSP status used by the status bar `Lsp` element.
///
/// Collapses the previous "running / auto_start-dormant / opt-in-dormant /
/// nothing" fan-out into the three user-meaningful buckets the indicator
/// actually needs to communicate:
///
/// - `On`            — at least one server for this language is running
/// - `Off`           — configured servers exist for this language, none are running
/// - `OffDismissed`  — like `Off`, but the user clicked "Disable" from the
///   popup; rendered with a muted style so it stops shouting for
///   attention while remaining clickable (so the user can still open the
///   popup to re-enable or see install help).
/// - `Error`         — at least one server for this language is in the Error state
/// - `None`          — no LSP configured or running for this language
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LspIndicatorState {
    #[default]
    None,
    On,
    Off,
    OffDismissed,
    /// Server is up but not answering requests (issue #2197).
    Warning,
    Error,
}

/// Editor state, theming, and runtime inputs needed to render a status bar frame.
pub struct StatusBarContext<'a> {
    pub state: &'a mut EditorState,
    pub cursors: &'a crate::model::cursor::Cursors,
    /// 0-indexed line of the primary cursor in `cursors`, as
    /// `Editor::primary_cursor_line` derives it for this frame.
    pub primary_cursor_line: usize,
    pub status_message: &'a Option<String>,
    pub plugin_status_message: &'a Option<String>,
    pub lsp_status: &'a str,
    /// LSP indicator state; drives the indicator's colours.
    pub lsp_indicator_state: LspIndicatorState,
    pub display_name: &'a str,
    pub keybindings: &'a crate::input::keybindings::KeybindingResolver,
    pub chord_state: &'a [(crossterm::event::KeyCode, crossterm::event::KeyModifiers)],
    pub update_available: Option<&'a str>,
    /// Lifecycle of an in-progress in-editor self-update; overrides the
    /// "Update: vX" text with progress/outcome so the indicator itself relays
    /// the result (no transient status message).
    pub update_phase: crate::services::release_checker::SelfUpdatePhase,
    pub general_warning_count: usize,
    /// The clickable status-bar segment the mouse is currently over, if any.
    /// Drives hover styling generically — each element underlines/recolors when
    /// its own clickable id equals this.
    pub hovered: Option<StatusBarClickable>,
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
    /// Error from the active window's most recent failed *reconnect* of a
    /// dormant remote workspace. When `Some` (and no plugin override is set),
    /// the `{remote}` indicator renders `FailedAttach` with this text instead
    /// of the connection-derived state — the core counterpart to the plugin
    /// override, but scoped to the active window so a failed SSH/kube reconnect
    /// can't bleed onto another window's indicator.
    pub remote_reconnect_error: Option<&'a str>,
    /// True when the active window is a dormant remote session's shell whose
    /// backend connect is currently in flight — the dive committed the switch
    /// while the SSH/kube handshake is still pending. Renders the `{remote}`
    /// indicator as `Connecting` instead of the placeholder authority's
    /// misleading "Local".
    pub remote_connecting: bool,
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
    /// Set when the active buffer is a terminal whose process has quit and can
    /// be restarted in place. Drives the `{terminal_restart}` indicator, which
    /// renders only in that state. `None` for every other buffer — including a
    /// live terminal, so the indicator never offers to restart a running agent.
    pub terminal_restart: Option<TerminalRestartState>,
}

/// What the `{terminal_restart}` indicator needs to describe the dead process
/// behind the active buffer. Derived per frame from the window's
/// `exited_terminals` record.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TerminalRestartState {
    /// Short program name of the process that died (`claude`, `codex`, …), or
    /// `None` when the terminal was just a shell.
    pub program: Option<String>,
    /// Wait-status exit code, when the platform reported one. Shown only when
    /// non-zero — a clean exit is the common "the agent finished" case and
    /// doesn't need a number shouting on the bar.
    pub exit_code: Option<i32>,
    /// Whether restarting rejoins an agent conversation rather than starting a
    /// fresh process. Drives "Resume" vs "Restart" wording.
    pub resumes_agent: bool,
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
    // The line's real start, not the reader's guess at one. The reader's
    // backward scan is bounded, and past that bound it reports how far it
    // looked — which is a column of 65,537 for a cursor anywhere beyond 64 KB
    // into its line, the same wrong number for every position past it.
    let line_start = buffer
        .prev_line_start_within(cursor_position, buffer.len())
        .unwrap_or(0);
    let mut iter = buffer.line_iterator(line_start, 80);
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

/// Adjust the primary cursor's `(line_index, column_index)` for virtual
/// space so the status-bar readout tracks the caret where it visibly sits,
/// instead of freezing at the line's real content end (#2577).
///
/// Cursor byte positions always clamp to real text (virtual space is a
/// view-only concept), so the raw `line` / `col_base` derived from the byte
/// position stall the moment the caret floats past EOL or onto a virtual line
/// below the buffer end. This adds the derived virtual offset:
/// - On a virtual line below the buffer end, the whole line is empty, so the
///   column is purely the sticky (goal) column and the line advances by the
///   number of virtual lines.
/// - Otherwise (horizontal virtual space past a line's content end), the
///   column advances by the virtual column count; every virtual column is a
///   would-be space, so it reads the same in graphemes as on screen.
fn virtual_space_adjusted_position(
    mode: VirtualSpaceMode,
    buffer: &crate::model::buffer::Buffer,
    cursor: &crate::model::cursor::Cursor,
    line: usize,
    col_base: usize,
) -> (usize, usize) {
    let vlines = crate::model::virtual_space::cursor_virtual_lines(mode, buffer, cursor);
    if vlines > 0 {
        (line + vlines, cursor.sticky_column.unwrap_or(col_base))
    } else {
        let vcols = crate::model::virtual_space::cursor_virtual_columns(mode, buffer, cursor);
        (line, col_base + vcols)
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

/// Renders the status bar.
pub struct StatusBarRenderer;

impl StatusBarRenderer {
    /// One configured element as it sits on the bar, or `None` when it has
    /// nothing to show.
    fn render_element(element: &StatusBarElement, ctx: &mut StatusBarContext<'_>) -> Option<Item> {
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
        let hovered = ctx.hovered;
        // An element that answers a press, painted in `keys` and answering
        // the pointer as `hover` says.
        let clickable = |text: String, name, id, keys, hover| {
            item(text, name, keys, Some((id, hover)), hovered)
        };
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
                if remote_disconnected {
                    disconnected_filename(text)
                } else {
                    item(text, "text", BAR, None, hovered)
                }
            }
            StatusBarElement::ReadOnly => {
                // Persistent `[RO]` indicator. Renders only while the active
                // buffer is read-only, as a steady status segment — the
                // documented affordance that tells the user editing is
                // disabled *before* they try to type. Independent of the
                // `{filename}` element (which also carries `[RO]` but is
                // omitted from the default layout).
                if !ctx.read_only {
                    return None;
                }
                clickable(
                    "[RO]".to_string(),
                    "text",
                    StatusBarClickable::ReadOnly,
                    BAR,
                    Hover::Underline,
                )
            }
            StatusBarElement::Cursor => {
                if !ctx.state.show_cursors {
                    return None;
                }
                let cursor = *ctx.cursors.primary();
                let line_count = ctx.state.buffer.line_count();
                let text = if let Some(lc) = line_count {
                    let line = ctx.primary_cursor_line;
                    let col = cursor_column(&mut ctx.state.buffer, cursor.position);
                    let mode = ctx.state.buffer_settings.virtual_space;
                    let (line, col) = virtual_space_adjusted_position(
                        mode,
                        &ctx.state.buffer,
                        &cursor,
                        line,
                        col,
                    );
                    format_cursor_position(line + 1, col + 1, lc)
                } else {
                    format!("Byte {}", cursor.position)
                };
                item(text, "text", BAR, None, hovered)
            }
            StatusBarElement::CursorCompact => {
                if !ctx.state.show_cursors {
                    return None;
                }
                let cursor = *ctx.cursors.primary();
                let line_count = ctx.state.buffer.line_count();
                let text = if let Some(lc) = line_count {
                    let line = ctx.primary_cursor_line;
                    let col = cursor_column(&mut ctx.state.buffer, cursor.position);
                    let mode = ctx.state.buffer_settings.virtual_space;
                    let (line, col) = virtual_space_adjusted_position(
                        mode,
                        &ctx.state.buffer,
                        &cursor,
                        line,
                        col,
                    );
                    format_cursor_position_compact(line + 1, col + 1, lc)
                } else {
                    format!("{}", cursor.position)
                };
                item(text, "text", BAR, None, hovered)
            }
            StatusBarElement::Diagnostics => {
                let mut error_count = 0usize;
                let mut warning_count = 0usize;
                let mut info_count = 0usize;
                let diagnostic_ns = crate::services::lsp::diagnostics::lsp_diagnostic_namespace();
                // Ask for the diagnostics namespace rather than filtering
                // every overlay on the buffer: this runs on every frame, and
                // a decorated buffer (a review diff carries an overlay per
                // line) has tens of thousands of overlays that are not
                // diagnostics.
                for overlay in ctx.state.overlays.in_namespace(&diagnostic_ns) {
                    match overlay.priority {
                        100 => error_count += 1,
                        50 => warning_count += 1,
                        _ => info_count += 1,
                    }
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
                item(parts.join(" "), "text", BAR, None, hovered)
            }
            StatusBarElement::CursorCount => {
                if ctx.cursors.count() <= 1 {
                    return None;
                }
                let text = t!("status.cursors", count = ctx.cursors.count()).to_string();
                item(text, "text", BAR, None, hovered)
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
                // Clickable (it opens the message log) without a hover cue.
                clickable(
                    parts.join(" | "),
                    "message",
                    StatusBarClickable::Messages,
                    BAR,
                    Hover::None,
                )
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
                item(format!("[{}]", chord_str), "text", BAR, None, hovered)
            }
            StatusBarElement::LineEnding => clickable(
                ctx.state.buffer.line_ending().display_name().to_string(),
                "lineEnding",
                StatusBarClickable::LineEnding,
                BAR,
                Hover::Swap(MENU_HOVER),
            ),
            StatusBarElement::Encoding => clickable(
                ctx.state.buffer.encoding().display_name().to_string(),
                "encoding",
                StatusBarClickable::Encoding,
                BAR,
                Hover::Swap(MENU_HOVER),
            ),
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
                clickable(
                    text,
                    "language",
                    StatusBarClickable::Language,
                    BAR,
                    Hover::Swap(MENU_HOVER),
                )
            }
            StatusBarElement::Lsp => {
                let (keys, hover) = lsp_look(ctx.lsp_indicator_state);
                clickable(
                    ctx.lsp_status.to_string(),
                    "lsp",
                    StatusBarClickable::Lsp,
                    keys,
                    hover,
                )
            }
            StatusBarElement::Warnings => {
                if ctx.general_warning_count == 0 {
                    return None;
                }
                clickable(
                    format!("[\u{26a0} {}]", ctx.general_warning_count),
                    "warning",
                    StatusBarClickable::Warnings,
                    WARNING,
                    Hover::Swap((
                        "ui.status_warning_indicator_hover_fg",
                        "ui.status_warning_indicator_hover_bg",
                    )),
                )
            }
            StatusBarElement::Update => {
                use crate::services::release_checker::SelfUpdatePhase;
                // A running/finished update owns the indicator text even though
                // `update_available` is still set (the running process still
                // sees itself as out of date until a restart).
                let text = match ctx.update_phase {
                    SelfUpdatePhase::Running => t!("status.update_running").to_string(),
                    SelfUpdatePhase::Succeeded => t!("status.update_done").to_string(),
                    SelfUpdatePhase::ActionRequired => {
                        t!("status.update_action_required").to_string()
                    }
                    SelfUpdatePhase::Failed => t!("status.update_failed").to_string(),
                    SelfUpdatePhase::Idle => {
                        let version = ctx.update_available?;
                        t!("status.update_available", version = version).to_string()
                    }
                };
                clickable(
                    text,
                    "text",
                    StatusBarClickable::Update,
                    ("ui.menu_highlight_fg", "ui.menu_dropdown_bg"),
                    Hover::Underline,
                )
            }
            StatusBarElement::TerminalRestart => {
                // Absent unless the active buffer is a terminal whose process
                // quit — this is a call to action, not a persistent control.
                let restart = ctx.terminal_restart.as_ref()?;
                // "Resume claude" when the restart rejoins the conversation,
                // "Restart claude" when it re-runs the launch command, and a
                // bare "Restart terminal" for a plain shell.
                let text = match (&restart.program, restart.resumes_agent) {
                    (Some(program), true) => {
                        t!("status.terminal_resume", program = program).to_string()
                    }
                    (Some(program), false) => {
                        t!("status.terminal_restart", program = program).to_string()
                    }
                    (None, _) => t!("status.terminal_restart_shell").to_string(),
                };
                // A non-zero code is the signal that something went wrong, so
                // it rides along; exit 0 (the agent simply finished) doesn't.
                let text = match restart.exit_code {
                    Some(code) if code != 0 => {
                        t!("status.terminal_restart_code", label = text, code = code).to_string()
                    }
                    _ => text,
                };
                // The error palette: a dead agent is a state the user has to
                // act on, and the indicator is the action.
                clickable(
                    text,
                    "terminalRestart",
                    StatusBarClickable::RestartTerminal,
                    ERROR,
                    Hover::Swap((
                        "ui.status_error_indicator_hover_fg",
                        "ui.status_error_indicator_hover_bg",
                    )),
                )
            }
            StatusBarElement::Palette => {
                let shortcut = ctx
                    .keybindings
                    .get_keybinding_for_action(
                        &crate::input::keybindings::Action::QuickOpen,
                        crate::input::keybindings::KeyContext::Global,
                    )
                    .unwrap_or_else(|| "?".to_string());
                let text = t!("status.palette", shortcut = shortcut).to_string();
                item(text, "text", PALETTE, None, hovered)
            }
            StatusBarElement::Clock => {
                let now = chrono::Local::now();
                let text = format!("{:02}:{:02}", now.hour(), now.minute());
                item(text, "text", BAR, None, hovered)
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
                } else if ctx.remote_connecting {
                    // The active window is a dormant remote session's shell
                    // whose backend connect is in flight (dive-committed
                    // switch / retry). Without this the shell — which runs
                    // on a local placeholder authority — would claim
                    // "Local" while the status message says Connecting.
                    ("Connecting…".to_string(), RemoteIndicatorState::Connecting)
                } else if ctx.remote_reconnect_error.is_some() {
                    // A reconnect of the active window's remote workspace failed.
                    // Keep the indicator short — just "Disconnected" — rather
                    // than inlining the full SSH error, which used to swamp the
                    // whole status bar. The detail isn't lost: it's emitted as a
                    // `tracing::warn!` (which lights the warning indicator) and
                    // surfaced in the remote-indicator popup. Takes precedence
                    // over the connection-derived state (which for a dormant
                    // workspace would read "Local", since the live authority is
                    // still the local placeholder until a reconnect lands).
                    (
                        "Disconnected".to_string(),
                        RemoteIndicatorState::Disconnected,
                    )
                } else {
                    match ctx.remote_connection {
                        None => ("Local".to_string(), RemoteIndicatorState::Local),
                        Some(conn) if conn.contains("(Disconnected)") => {
                            (conn.to_string(), RemoteIndicatorState::Disconnected)
                        }
                        Some(conn) => (conn.to_string(), RemoteIndicatorState::Connected),
                    }
                };
                // Connecting and Connected share a palette so the transition
                // is a glyph swap rather than a colour flash; the two ways of
                // not reaching the remote share the error palette.
                let keys = match state {
                    RemoteIndicatorState::Connecting | RemoteIndicatorState::Connected => {
                        ("ui.help_indicator_fg", "ui.help_indicator_bg")
                    }
                    RemoteIndicatorState::FailedAttach | RemoteIndicatorState::Disconnected => {
                        ERROR
                    }
                    RemoteIndicatorState::Local => BAR,
                };
                clickable(
                    text,
                    "remote",
                    StatusBarClickable::RemoteIndicator,
                    keys,
                    Hover::Underline,
                )
            }
            StatusBarElement::WorkspaceTrust => {
                // Always-present trust control, read from the active session's
                // trust level each frame. Persistent like `{remote}` — it never
                // vanishes, so the user always knows whether repo-controlled
                // execution is gated. Capitalized for a status-bar label.
                use crate::services::workspace_trust::TrustLevel;
                let (text, keys) = match ctx.workspace_trust_level {
                    TrustLevel::Trusted => (t!("statusbar.trust.trusted"), BAR),
                    TrustLevel::Restricted => (t!("statusbar.trust.restricted"), WARNING),
                    TrustLevel::Blocked => (t!("statusbar.trust.blocked"), WARNING),
                };
                clickable(
                    text.to_string(),
                    "trust",
                    StatusBarClickable::WorkspaceTrust,
                    keys,
                    Hover::Underline,
                )
            }
            StatusBarElement::CustomToken(key) => {
                let value = ctx.dynamic_status_bar_elements.get(key)?;
                let mut it = item(value.clone(), "plugin", BAR, None, hovered)?;
                it.token_key = Some(key.clone());
                Some(it)
            }
        }
    }

    /// Render a configured side (left/right), in order, skipping elements
    /// with nothing to show.
    pub(crate) fn render_side(
        config_side: &[StatusBarElement],
        ctx: &mut StatusBarContext<'_>,
    ) -> Vec<Item> {
        config_side
            .iter()
            .filter_map(|elem| Self::render_element(elem, ctx))
            .collect()
    }
}

/// A `(fg, bg)` pair of theme keys.
type Keys = (&'static str, &'static str);

const BAR: Keys = ("ui.status_bar_fg", "ui.status_bar_bg");
const WARNING: Keys = (
    "ui.status_warning_indicator_fg",
    "ui.status_warning_indicator_bg",
);
const ERROR: Keys = (
    "ui.status_error_indicator_fg",
    "ui.status_error_indicator_bg",
);
const MENU_HOVER: Keys = ("ui.menu_hover_fg", "ui.menu_hover_bg");
/// The palette hint, on keys of its own so a theme can repaint it without
/// breaking the bar's colour band (#1711).
const PALETTE: Keys = ("ui.status_palette_fg", "ui.status_palette_bg");

/// How an element answers the pointer over it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Hover {
    /// Not at all.
    None,
    /// Underlined, in its own colours.
    Underline,
    /// Underlined, and repainted in these keys.
    Swap(Keys),
}

/// The LSP indicator's keys by state. Error is a problem, Off is prominent
/// because a click has useful options behind it, On is healthy, and a
/// dismissed or absent indicator blends into the bar. It underlines on hover
/// only while it has a state to act on.
fn lsp_look(state: LspIndicatorState) -> (Keys, Hover) {
    match state {
        LspIndicatorState::Error => (
            ("diagnostic.error_fg", "diagnostic.error_bg"),
            Hover::Underline,
        ),
        LspIndicatorState::Warning => (WARNING, Hover::Underline),
        LspIndicatorState::Off => (
            ("ui.status_lsp_actionable_fg", "ui.status_lsp_actionable_bg"),
            Hover::Underline,
        ),
        LspIndicatorState::On => (
            ("ui.status_lsp_on_fg", "ui.status_lsp_on_bg"),
            Hover::Underline,
        ),
        LspIndicatorState::OffDismissed => (BAR, Hover::Underline),
        LspIndicatorState::None => (BAR, Hover::None),
    }
}

/// An element as one run, padded by a cell on either side in its own colours
/// so an element with a distinct background reads as a pill. `None` for empty
/// text: an element with nothing to say is not on the bar.
fn item(
    text: String,
    name: &'static str,
    keys: Keys,
    click: Option<(StatusBarClickable, Hover)>,
    hovered: Option<StatusBarClickable>,
) -> Option<Item> {
    if text.is_empty() {
        return None;
    }
    let (keys, underline) = match click {
        Some((id, hover)) if Some(id) == hovered => match hover {
            Hover::None => (keys, false),
            Hover::Underline => (keys, true),
            Hover::Swap(swapped) => (swapped, true),
        },
        _ => (keys, false),
    };
    Some(Item {
        runs: vec![(format!(" {text} "), ink(keys, underline))],
        name,
        clickable: click.map(|(id, _)| id),
        token_key: None,
    })
}

/// The filename edited over a disconnected remote, in the error palette. Over
/// SSH, only its `[SSH:…] ` prefix is: the name after it keeps the bar's.
fn disconnected_filename(text: String) -> Option<Item> {
    let split = text
        .starts_with(SSH_PREFIX)
        .then(|| text.find(SSH_PREFIX_TERMINATOR))
        .flatten();
    let Some(term_off) = split else {
        return item(text, "text", ERROR, None, None);
    };
    let (prefix, rest) = text.split_at(term_off + SSH_PREFIX_TERMINATOR.len());
    Some(Item {
        runs: vec![
            (format!(" {prefix}"), ink(ERROR, false)),
            (format!("{rest} "), ink(BAR, false)),
        ],
        name: "text",
        clickable: None,
        token_key: None,
    })
}

/// A run's theme name: two keys, underlined or not.
fn ink((fg, bg): Keys, underline: bool) -> String {
    let attrs = if underline {
        Attrs::UNDERLINE
    } else {
        Attrs::NONE
    };
    Ink::keys(fg, bg).plus(attrs).to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

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

        assert_eq!(PALETTE, ("ui.status_palette_fg", "ui.status_palette_bg"));
        let keys = |lsp| lsp_look(lsp).0;
        assert_eq!(
            keys(LspIndicatorState::On),
            ("ui.status_lsp_on_fg", "ui.status_lsp_on_bg")
        );
        // Off / Error keep their own keys so they remain visible signals.
        assert_eq!(
            keys(LspIndicatorState::Off),
            ("ui.status_lsp_actionable_fg", "ui.status_lsp_actionable_bg")
        );
        assert_eq!(
            keys(LspIndicatorState::Error),
            ("diagnostic.error_fg", "diagnostic.error_bg")
        );
    }

    fn runs_of(it: Option<Item>) -> Vec<(String, Ink)> {
        it.expect("an element")
            .runs
            .into_iter()
            .map(|(t, theme)| (t, Ink::parse(&theme).expect("a readable theme name")))
            .collect()
    }

    fn encoding(hovered: Option<StatusBarClickable>) -> Vec<(String, Ink)> {
        let click = Some((StatusBarClickable::Encoding, Hover::Swap(MENU_HOVER)));
        runs_of(item("UTF-8".into(), "encoding", BAR, click, hovered))
    }

    /// A hovered clickable element is underlined, and the underline reaches
    /// the painted style. The modifier used to be written as `underlined`,
    /// which the grammar does not know and silently dropped.
    #[test]
    fn a_hovered_clickable_run_is_underlined() {
        let theme = crate::view::theme::Theme::from_json(
            r#"{"name":"t","editor":{},"ui":{},"search":{},"diagnostic":{},"syntax":{}}"#,
        )
        .expect("minimal theme should parse");
        let hovered = encoding(Some(StatusBarClickable::Encoding));
        let [(text, ink)] = hovered.as_slice() else {
            panic!("one run, got {hovered:?}");
        };
        assert_eq!(text, " UTF-8 ");
        assert!(ink.attrs.contains(Attrs::UNDERLINE), "{ink:?}");
        assert_eq!(
            ink.names(),
            (Some("ui.menu_hover_fg"), Some("ui.menu_hover_bg"))
        );
        let style = ink.style(&theme).expect("the keys resolve");
        assert!(style
            .add_modifier
            .contains(ratatui::style::Modifier::UNDERLINED));

        // Not hovered, or hovering a different element: plain bar colours.
        for other in [None, Some(StatusBarClickable::Lsp)] {
            let runs = encoding(other);
            assert_eq!(runs[0].1.attrs, Attrs::NONE);
            assert_eq!(
                runs[0].1.names(),
                (Some("ui.status_bar_fg"), Some("ui.status_bar_bg"))
            );
        }
    }

    /// The LSP indicator only underlines while it has a state to act on.
    #[test]
    fn an_empty_lsp_indicator_is_not_underlined_on_hover() {
        let hovered = Some(StatusBarClickable::Lsp);
        let lsp = |state| {
            let (keys, hover) = lsp_look(state);
            let click = Some((StatusBarClickable::Lsp, hover));
            runs_of(item("LSP".into(), "lsp", keys, click, hovered))
        };
        assert_eq!(lsp(LspIndicatorState::None)[0].1.attrs, Attrs::NONE);
        assert!(lsp(LspIndicatorState::On)[0]
            .1
            .attrs
            .contains(Attrs::UNDERLINE));
    }

    /// A filename over a disconnected SSH session paints its prefix in the
    /// error palette and the rest in the bar's.
    #[test]
    fn a_disconnected_ssh_filename_is_two_colours() {
        let runs = runs_of(disconnected_filename(
            "[SSH:host (Disconnected)] main.rs".into(),
        ));
        let got: Vec<_> = runs
            .iter()
            .map(|(t, ink)| (t.as_str(), ink.names()))
            .collect();
        assert_eq!(
            got,
            vec![
                (
                    " [SSH:host (Disconnected)] ",
                    (ERROR.0.into(), ERROR.1.into())
                ),
                ("main.rs ", (BAR.0.into(), BAR.1.into())),
            ]
        );
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
