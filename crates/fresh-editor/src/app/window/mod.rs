//! Editor `Window` — a project-rooted unit of editor state.
//!
//! A `Window` bundles the state that is logically scoped to one
//! project root: the file tree, ignore matcher, LSP client set,
//! file watchers, split layout, and buffer membership. Switching the
//! active window re-targets the entire editor UI (file explorer,
//! quick-open, LSP roots) without recreating buffers, terminals, or
//! plugin state — those live on the `Editor` and survive switches.
//!
//! See `docs/internal/orchestrator-sessions-design.md` for the full
//! design rationale.
//!
//! ## Naming
//!
//! Internally we call these "windows" (modelled on VS Code windows)
//! to disambiguate from Fresh's pre-existing workspace-recovery and
//! config-layer "session" concepts. Orchestrator presents windows as
//! "agent sessions" in its UX, since the parallel-agents domain
//! language is what users see — but the editor types are `Window`,
//! `WindowId`, etc.
//!
//! ## Migration status
//!
//! Steps 0a–0f, 0j, 0k phases 1–3, and 0l shipped. Per-subsystem
//! state that used to warm-swap on `setActiveWindow` —
//! `panel_ids`, `file_mod_times`, `file_explorer`, `lsp`, the
//! `splits` pair, `buffers`, `buffer_metadata`, the terminal
//! subsystem (`terminal_manager` + `terminal_buffers` +
//! `terminal_backing_files` + `terminal_log_files`),
//! `event_logs`, `position_history` (with its `in_navigation` /
//! `suppress_position_history_once` companion flags),
//! `bookmarks`, `grouped_subtrees`, `composite_buffers`,
//! `composite_view_states`, all 23 LSP-request-tracking maps
//! (pending-/in-flight/applied, debounce timers,
//! `next_lsp_request_id`, `completion_items`, `dabbrev_state`,
//! code-action attribution), the per-window async `bridge`, and
//! the chrome surfaces (`status_message`, `plugin_status_message`,
//! `prompt`) — all live directly on `Window`. `set_active_window`
//! is a pointer write (plus first-dive seed allocation for
//! windows that have never been activated).

pub mod buffers;
pub mod process_group;

pub use buffers::WindowBuffers;
pub use process_group::{LocalSignaller, ProcessGroupEntry, ProcessGroups, Signaller};

use crate::app::types::{ChromeLayout, WindowLayoutCache};
use crate::app::window_resources::WindowResources;
use crate::model::event::{Event, LeafId};
use crate::services::lsp::diagnostics::AnchoredDiagnostic;
use crate::services::lsp::manager::LspManager;
use crate::types::LspFeature;
use crate::view::file_tree::FileTreeView;
use crate::view::split::{SplitManager, SplitViewState};
use fresh_core::{BufferId, WindowId};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

/// A project-rooted unit of editor state.
///
/// After Step 0b every per-subsystem field listed below is owned
/// outright by the window — there are no warm-swap stashes.
/// `setActiveWindow` is a pointer write; reads of the active
/// window's state route through Editor accessors
/// (`active_layout()`, `split_manager()`, `file_explorer()`, `lsp()`,
/// `panel_ids()`, `file_mod_times()`, …). Cross-window access goes
/// through `Editor.windows.get(&id)` directly.
/// A clickable path-link highlighted under a Ctrl+hover in the live terminal
/// grid. Coordinates are relative to the terminal content area.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TerminalLinkHover {
    /// The terminal buffer the link is in.
    pub buffer_id: BufferId,
    /// Grid row (0-based, within the content area) containing the link.
    pub row: u16,
    /// Column range (0-based char columns) the link spans, for underlining.
    pub cols: std::ops::Range<usize>,
}

/// Per-terminal-buffer editor state, keyed by `BufferId` in
/// [`Window::terminal_buffers`]. PTY I/O lives in the `TerminalManager`; the
/// byte-stream backing files stay keyed by `TerminalId`.
#[derive(Debug, Clone)]
pub struct TerminalBuffer {
    /// The PTY session feeding this buffer, scoped to this window.
    pub terminal_id: crate::services::terminal::TerminalId,

    /// The splits currently viewing THIS terminal in read-only scrollback.
    ///
    /// This is the single source of truth for live-vs-scrollback, and it is
    /// **per split**: a split showing this terminal is *live* iff its `LeafId`
    /// is absent here, and *scrollback* iff present. Keeping it per-split is
    /// what lets the same terminal stream the live PTY grid in one split while
    /// another split reads frozen scrollback, independently, even when neither
    /// is focused (fresh#2595).
    ///
    /// There is exactly one representation of "live" (absence), so no split can
    /// be in a contradictory state, and — because this lives on `TerminalBuffer`
    /// — only terminals can carry a mode at all. Membership is written solely
    /// through [`Window::set_split_terminal_scrollback`] and pruned on split
    /// close via [`Window::forget_split_terminal_modes`].
    scrollback_splits: std::collections::HashSet<LeafId>,

    /// The subset of `scrollback_splits` that entered scrollback *implicitly*
    /// — dropped there by a selection gesture on the live grid
    /// (`begin_terminal_grid_selection`) rather than by an explicit request
    /// (Ctrl+Space, Shift+PageUp, wheel-up, process exit).
    ///
    /// Implicit scrollback is an implementation detail of drag-to-select, so
    /// it also *ends* automatically: completing the gesture (copying the
    /// selection) or abandoning it (a bare click) resumes the live grid.
    /// Engaging with the scrollback as a view — scrolling it — clears the
    /// marker, converting the visit to an explicit one that only ends by the
    /// explicit rules. Invariant: `drag_scrollback_splits ⊆
    /// scrollback_splits`, maintained by `set_scrollback_in` (any transition
    /// through the sole mutator clears the marker; the drag path re-marks
    /// afterwards).
    drag_scrollback_splits: std::collections::HashSet<LeafId>,
}

impl TerminalBuffer {
    /// A freshly opened/attached terminal buffer. Every split showing it starts
    /// live (the scrollback set is empty).
    pub fn new_live(terminal_id: crate::services::terminal::TerminalId) -> Self {
        Self {
            terminal_id,
            scrollback_splits: std::collections::HashSet::new(),
            drag_scrollback_splits: std::collections::HashSet::new(),
        }
    }

    /// Whether `split` is viewing this terminal in read-only scrollback.
    pub fn is_scrollback_in(&self, split: LeafId) -> bool {
        self.scrollback_splits.contains(&split)
    }

    /// Whether `split`'s scrollback visit is implicit (drag-initiated).
    pub fn is_drag_scrollback_in(&self, split: LeafId) -> bool {
        self.drag_scrollback_splits.contains(&split)
    }

    /// Record `split`'s live↔scrollback edge for this terminal: `true` adds it
    /// to the scrollback set, `false` removes it (back to live). Sole mutator,
    /// reached via [`Window::set_split_terminal_scrollback`]. Every transition
    /// clears the implicit (drag-initiated) marker: leaving scrollback ends
    /// the visit, and an explicit (re-)entry downgrades an implicit one —
    /// only `begin_terminal_grid_selection` re-marks, after calling this.
    fn set_scrollback_in(&mut self, split: LeafId, scrollback: bool) {
        if scrollback {
            self.scrollback_splits.insert(split);
        } else {
            self.scrollback_splits.remove(&split);
        }
        self.drag_scrollback_splits.remove(&split);
    }

    /// Mark or unmark `split`'s scrollback visit as implicit (drag-initiated).
    /// Marking a split that isn't in scrollback is a no-op, preserving the
    /// subset invariant.
    fn set_drag_scrollback_in(&mut self, split: LeafId, drag: bool) {
        if drag && self.scrollback_splits.contains(&split) {
            self.drag_scrollback_splits.insert(split);
        } else if !drag {
            self.drag_scrollback_splits.remove(&split);
        }
    }

    /// Drop `split` from this terminal's scrollback set (split closed / no
    /// longer shows this terminal), reverting it to the live default.
    fn forget_split(&mut self, split: LeafId) {
        self.scrollback_splits.remove(&split);
        self.drag_scrollback_splits.remove(&split);
    }
}

pub struct Window {
    /// Stable identifier. The base window is always `WindowId(1)`.
    pub id: WindowId,

    /// This workspace's backend — *where* it acts, *whether* it may
    /// (`workspace_trust`), and *with what env*. **Owned outright by this
    /// window**, never shared with another: it lives here (not in the
    /// `Clone` `WindowResources`) so the type system prevents one workspace's
    /// authority/trust/env from leaking into another (issue #2280). The
    /// editor's active backend is just `active_window().authority` — there is
    /// no separate clonable editor-wide copy.
    pub(crate) authority: crate::services::authority::Authority,

    /// User-visible label. Defaults to the basename of `root` (or
    /// "main" when the root is the original process cwd). Not
    /// required to be unique.
    pub label: String,

    /// Canonical absolute path of the project root. Read-only after
    /// construction; closing a window and creating a new one is the
    /// way to "rename" the root.
    pub root: PathBuf,

    /// File-explorer view (expansion, scroll, selection). `None`
    /// means "never opened" — the caller rebuilds at `root` on first
    /// toggle. Each window has its own view; switching windows shows
    /// the new window's tree (or none, if it hasn't been opened yet).
    pub file_explorer: Option<FileTreeView>,

    /// Polling-based mtime cache for auto-revert. Auto-revert only
    /// fires for the active window's files; inactive windows' mtimes
    /// stay frozen at dive-out time and resync on dive-back —
    /// matching the user's mental model that a dormant window "is
    /// paused".
    pub file_mod_times: HashMap<PathBuf, std::time::SystemTime>,

    /// LSP manager (running language servers, configs, per-language
    /// root URIs). Each window owns its own LSP set, rooted at its
    /// project root; inactive windows' servers remain running in the
    /// background — that's the warm-LSP property the design's
    /// trade-off discussion calls out as a memory cost worth paying
    /// so dive-back is instant.
    ///
    /// `None` means "this window has never spawned any LSP"; the
    /// next LSP feature trigger will lazily create one.
    /// This window's language-server manager. Every window owns one,
    /// built in [`Window::new`] rooted at the window's project root —
    /// there is no "window without a manager" state (that was the
    /// "No LSP manager available" bug). Servers are still spawned
    /// lazily on demand; an idle window's manager holds only config.
    pub lsp: LspManager,

    /// Utility-dock panel-id → buffer-id occupancy. Each window
    /// gets its own dock — when one window has the search panel
    /// claimed and the user dives elsewhere, the new window starts
    /// with an empty dock and rebuilds on demand.
    pub panel_ids: HashMap<String, BufferId>,

    /// Buffers attached to this window. Each window owns its
    /// `EditorState`s outright; closing the window drops them.
    /// Opening the same file in two windows produces two independent
    /// buffers.
    pub buffers: WindowBuffers,

    /// Per-buffer metadata (display name, file path / LSP URI,
    /// virtual-buffer mode, read-only flag, LSP-opened set, preview
    /// flag, etc.) for the buffers in `Window.buffers`. Lives next
    /// to the buffer storage it describes; closing a window drops
    /// every metadata entry along with the buffers themselves.
    pub buffer_metadata: HashMap<BufferId, crate::app::types::BufferMetadata>,

    /// Per-buffer undo/redo event log. Lives next to `buffers`
    /// because undo history is buffer-scoped — closing a window
    /// drops the buffer and its log together.
    pub event_logs: HashMap<BufferId, crate::model::event::EventLog>,

    /// Status message (shown in this window's status bar). Per-window
    /// because each window has its own context — a save in window A
    /// shouldn't flash a status message into window B's UI. Only the
    /// active window's chrome renders, so background-window status
    /// messages are naturally invisible.
    pub status_message: Option<String>,

    /// Plugin-provided status message (displayed alongside the core
    /// status, also per-window).
    pub plugin_status_message: Option<String>,

    /// Active prompt (minibuffer) for this window. Each window can
    /// have its own prompt mid-flight; switching windows preserves
    /// each window's prompt state independently.
    pub prompt: Option<crate::view::prompt::Prompt>,

    /// Per-window async bridge — the (Sender, Receiver) pair the
    /// LSP manager (and per-window terminal/file-explorer tasks
    /// once they migrate) uses to deliver async responses back to
    /// the main loop. Each window owns its own channel so cleanup
    /// on `closeWindow` is automatic (the receiver drops, senders
    /// error and stop). Editor-global async messages (plugin
    /// runtime callbacks, file-open dialog) flow through
    /// `Editor.async_bridge` instead.
    pub bridge: crate::services::async_bridge::AsyncBridge,

    // ---- LSP request-tracking state (moved from Editor in Step 0k) ----
    /// Per-window LSP request-id allocator. Each window's LspManager
    /// talks to its own server connections, and each connection only
    /// requires per-connection request-id uniqueness — no global
    /// namespace needed. Starts at 0 per window.
    pub next_lsp_request_id: u64,

    /// Pending LSP completion request ids (multi-server).
    pub pending_completion_requests: std::collections::HashSet<u64>,

    /// Original LSP completion items (for type-to-filter).
    pub completion_items: Option<Vec<lsp_types::CompletionItem>>,

    /// Scheduled completion-trigger time (debounced quick-suggestions).
    pub scheduled_completion_trigger: Option<std::time::Instant>,

    /// Dabbrev cycling state (Alt+/ session).
    pub dabbrev_state: Option<crate::app::DabbrevCycleState>,

    /// Pending LSP go-to-definition request id.
    pub pending_goto_definition_request: Option<u64>,

    /// Pending LSP find-references request id and the symbol name.
    pub pending_references_request: Option<u64>,
    pub pending_references_symbol: String,

    /// Pending LSP go-to-implementation request id and the symbol name.
    pub pending_implementation_request: Option<u64>,
    pub pending_implementation_symbol: String,

    /// Pending LSP signature-help request id.
    pub pending_signature_help_request: Option<u64>,

    /// Pending LSP code-actions request ids and per-request server-name
    /// attribution + the selected-from list.
    pub pending_code_actions_requests: std::collections::HashSet<u64>,
    pub pending_code_actions_server_names: std::collections::HashMap<u64, String>,
    pub pending_code_actions: Option<Vec<(String, lsp_types::CodeActionOrCommand)>>,

    /// Pending inlay-hints requests keyed by request id.
    pub(crate) pending_inlay_hints_requests:
        std::collections::HashMap<u64, crate::app::InlayHintsRequest>,

    /// Pending folding-range requests + per-buffer in-flight tracking + debounce.
    pub(crate) pending_folding_range_requests:
        std::collections::HashMap<u64, crate::app::FoldingRangeRequest>,
    pub folding_ranges_in_flight: std::collections::HashMap<BufferId, (u64, u64)>,
    pub folding_ranges_debounce: std::collections::HashMap<BufferId, std::time::Instant>,

    /// Pending semantic-tokens-full requests + per-buffer in-flight tracking +
    /// the next-allowed-refresh debounce.
    pub(crate) pending_semantic_token_requests:
        std::collections::HashMap<u64, crate::app::SemanticTokenFullRequest>,
    pub(crate) semantic_tokens_in_flight:
        std::collections::HashMap<BufferId, (u64, u64, crate::app::SemanticTokensFullRequestKind)>,
    pub semantic_tokens_full_debounce: std::collections::HashMap<BufferId, std::time::Instant>,

    /// Pending semantic-tokens-range requests + per-buffer in-flight,
    /// last-request, and last-applied tracking.
    pub(crate) pending_semantic_token_range_requests:
        std::collections::HashMap<u64, crate::app::SemanticTokenRangeRequest>,
    pub semantic_tokens_range_in_flight:
        std::collections::HashMap<BufferId, (u64, usize, usize, u64)>,
    pub semantic_tokens_range_last_request:
        std::collections::HashMap<BufferId, (usize, usize, u64, std::time::Instant)>,
    pub semantic_tokens_range_applied: std::collections::HashMap<BufferId, (usize, usize, u64)>,

    /// Back/forward navigation stack (cursor jumps, file switches)
    /// scoped to this window. Each window has its own history so
    /// switching windows doesn't pollute the other window's
    /// back-stack — diving back into a window resumes navigation
    /// where you left it.
    pub position_history: crate::input::position_history::PositionHistory,

    /// `true` while a back/forward jump is in progress. Suppresses
    /// `track_cursor_movement` from recording the jump itself as a
    /// new entry. Per-window so windows don't fight over the flag
    /// during cross-window orchestration.
    pub in_navigation: bool,

    /// One-shot suppression of position-history recording for the
    /// next buffer-switch (used by file-open paths that don't want
    /// to leave a trail entry for the about-to-be-loaded file).
    pub suppress_position_history_once: bool,

    /// Bookmarks (single-char register → buffer + byte position) for
    /// this window. Bookmarks point at this window's buffers and
    /// follow the window across `setActiveWindow` switches — every
    /// window has its own register set.
    pub(crate) bookmarks: crate::app::bookmarks::BookmarkState,

    /// Composite buffers in this window (separate from regular
    /// buffers). These display multiple source buffers in a single
    /// tab — Live Grep results, References, Diagnostics list,
    /// etc. Owned per-window so the panel state follows the window
    /// that opened it.
    pub composite_buffers: HashMap<BufferId, crate::model::composite_buffer::CompositeBuffer>,

    /// Per-split view state for composite buffers in this window.
    /// Keyed by (split_id, buffer_id) — each split that hosts a
    /// composite buffer gets its own scroll-row tracking.
    pub composite_view_states:
        HashMap<(LeafId, BufferId), crate::view::composite_view::CompositeViewState>,

    /// Grouped `SplitNode` subtrees for this window, keyed by their
    /// `LeafId` (which is what `TabTarget::Group(leaf_id)`
    /// references). Each entry is a `SplitNode::Grouped` node
    /// holding the layout for one buffer group (Live Grep, References,
    /// Diagnostics, etc.). These subtrees are NOT part of the main
    /// split tree — they live here and are dispatched to at render
    /// time when the current split's active target is a `Group`.
    /// Per-window because a buffer-group panel belongs to the window
    /// that opened it.
    pub grouped_subtrees: HashMap<LeafId, crate::view::split::SplitNode>,

    /// Terminal subsystem (PTY processes + render-state grids) for
    /// this window. Owned per-window so closing a window joins its
    /// PTY threads — no orphan agents survive a `closeWindow`.
    pub terminal_manager: crate::services::terminal::TerminalManager,

    /// Per-terminal-buffer editor state, keyed by buffer id and scoped to
    /// this window: the PTY id feeding the buffer plus its remembered
    /// live/scrollback interaction mode. See [`TerminalBuffer`].
    pub terminal_buffers: HashMap<BufferId, TerminalBuffer>,

    /// Backing files for terminal buffers (the rendered visible-screen
    /// + scrollback content the buffer actually displays).
    pub terminal_backing_files: HashMap<crate::services::terminal::TerminalId, std::path::PathBuf>,

    /// Raw log files for terminal buffers (the unfiltered byte stream
    /// from the PTY, used for replay / save-history).
    pub terminal_log_files: HashMap<crate::services::terminal::TerminalId, std::path::PathBuf>,

    /// Terminal buffers whose tab title was set explicitly (plugin- or
    /// command-derived). These are excluded from foreground-process
    /// auto-naming so a program running inside doesn't clobber the chosen
    /// title; an OSC title emitted by the program still takes precedence.
    pub terminal_explicit_titles: std::collections::HashSet<BufferId>,

    /// Last time foreground-process names were polled for terminal tab
    /// auto-naming. Throttles the `tcgetpgrp` + `/proc` reads to roughly
    /// once a second rather than every frame. `None` until the first poll.
    pub(crate) terminal_fg_poll_at: Option<std::time::Instant>,

    /// Cached foreground-process name per terminal buffer, refreshed on the
    /// [`FG_POLL_INTERVAL`] poll. Present means a name was read; absent
    /// means none was available (so callers fall back to the OSC title or
    /// default). Applied to the tab every frame so the title stays put
    /// between polls without re-running the syscall.
    pub(crate) terminal_fg_cache: HashMap<BufferId, String>,

    /// Plugin-managed per-window state. Outer key is plugin name,
    /// inner is the plugin-defined key. Read via
    /// `editor.getWindowState(key)` and written via
    /// `editor.setWindowState(key, value)`. Persisted to the
    /// orchestrator's global `windows.json` under the platform
    /// data dir so it survives editor restarts.
    pub plugin_state: HashMap<String, HashMap<String, serde_json::Value>>,

    /// Declarative spec for *how to rebuild this workspace's backend* — the
    /// persisted source of truth behind the live `resources.authority`. Set
    /// when an authority is installed for the workspace (`setAuthority` →
    /// `Plugin`, born-attached remote → `RemoteAgent`, new local → `Local`)
    /// and round-tripped through the workspace's workspace file so a restart /
    /// relaunch can reconnect the backend rather than degrade it to local.
    /// `Local` (the default) for an ordinary host-local workspace. A workspace
    /// whose spec is remote but whose live `authority` is local is *dormant*
    /// — disconnected, awaiting reconnect. See
    /// `docs/internal/PER_SESSION_BACKENDS_DESIGN.md`.
    pub authority_spec: crate::services::authority::SessionAuthoritySpec,

    /// Error from the most recent failed *reconnect* of this dormant remote
    /// workspace (the dive-triggered `reconnect_dormant_session_if_needed`
    /// path). `Some` drives the status-bar remote indicator into `FailedAttach`
    /// for this window — a persistent, per-window signal that survives until the
    /// next successful reconnect (cleared on success / on a fresh reconnect
    /// attempt) or the user dismisses it. Per-window rather than the editor-wide
    /// `remote_indicator_override` (which the devcontainer plugin owns) so a
    /// failed SSH/kube reconnect on one workspace can't bleed its error onto
    /// another window's indicator.
    pub remote_reconnect_error: Option<String>,

    /// Window-scoped layout hit-test cache: split-leaf rects, tab
    /// rects, the file-explorer rect, separators, scrollbars, and
    /// per-leaf `view_line_mappings` that mouse positioning and
    /// visual-line motion read. Repopulated by the renderer on every
    /// frame; stale until the next render after a window switch (the
    /// post-switch render fills it in before any input handling).
    /// Editor-chrome rects (status bar, menu, popups, prompt overlay)
    /// live on `Window::chrome_layout` (also per-window).
    pub(crate) layout_cache: WindowLayoutCache,

    /// Per-window editor-chrome layout cache: status bar, menu,
    /// popups, prompt overlay, full-frame cell-theme map. Each
    /// window has its own status bar / prompt / popup state, so the
    /// cache is per-window. Repopulated by the renderer for the
    /// active window every frame.
    pub(crate) chrome_layout: ChromeLayout,

    /// Last-known terminal screen dimensions, mirrored from
    /// `Editor::terminal_width` / `Editor::terminal_height` whenever
    /// `Editor::resize` loops over windows. Per-window because
    /// `Window::resize_visible_terminals` and other per-window resize
    /// logic need the screen size without reaching back to `Editor`.
    pub(crate) terminal_width: u16,
    pub(crate) terminal_height: u16,

    /// Effective width (cols) of the editor-global left dock, pushed
    /// down by `Editor::relayout` (the single layout funnel). Mirrored
    /// here — like `terminal_width` — so per-window terminal sizing
    /// (`resize_visible_terminals`) can subtract the dock without
    /// reaching back to `Editor`. `0` when no dock is shown. This is a
    /// derived cache, never a source of truth: `Editor::dock` owns the
    /// real placement and `relayout` recomputes this from it.
    pub(crate) dock_cols: u16,

    /// Editor-global resources shared by `Arc` clone (config, theme
    /// registry, keybindings, command registry, filesystem authority,
    /// the buffer-id allocator, …). See [`WindowResources`] for the
    /// full inventory and rationale.
    pub(crate) resources: WindowResources,

    /// Buffer currently opened in "preview" (ephemeral) mode, together
    /// with the split (pane) it lives in. At most one preview exists
    /// per window. Pre Step-0 this lived on `Editor`; moved here so
    /// preview tracking follows the window's other view-state.
    ///
    /// This is the **single source of truth** for preview state: a buffer is
    /// "the preview" iff this tuple is `Some` and points at it
    /// ([`Window::is_buffer_preview`]). At most one preview exists editor-wide,
    /// so the "two previews" / "orphan preview flag" states are unrepresentable.
    ///
    /// Invariants:
    /// - The preview is anchored to the split it was opened in.
    /// - Cleared when the buffer is closed or promoted. Promotion fires the
    ///   deferred `after_file_open` hook (see `promote_buffer_from_preview`).
    pub preview: Option<(LeafId, BufferId)>,

    /// Path-link currently highlighted under a Ctrl+hover over the live
    /// terminal grid. `Some` means the renderer underlines the given grid row
    /// columns to signal it's clickable. Cleared when Ctrl is released or the
    /// pointer leaves a resolvable path. See [`TerminalLinkHover`].
    pub terminal_link_hover: Option<TerminalLinkHover>,

    /// Track which byte ranges have been seen per buffer (for the
    /// `lines_changed` plugin-hook optimisation). Keyed by `BufferId`,
    /// follows the buffers onto Window.
    pub seen_byte_ranges: HashMap<BufferId, std::collections::HashSet<(usize, usize)>>,

    /// Previous viewport states for `viewport_changed` hook detection.
    /// Stores `(top_byte, width, height)` from the end of the last
    /// render frame. Keyed by `LeafId`, per-window because the splits
    /// it tracks are per-window.
    pub previous_viewports: HashMap<LeafId, (usize, u16, u16)>,

    /// Whether scroll syncing applies to splits showing the same
    /// buffer. Per-window UX toggle.
    pub same_buffer_scroll_sync: bool,

    /// Per-window interactive search-and-replace session state.
    /// Drives the F+y/n/!/q UX during `replace_in_buffer` /
    /// `replace_all`. Per-window because the search target buffer
    /// and the visible matches are window-scoped.
    pub(crate) interactive_replace_state: Option<crate::app::types::InteractiveReplaceState>,

    /// Cross-split scroll-sync manager for side-by-side diff views.
    /// Per-window because the splits it pairs are per-window.
    pub scroll_sync_manager: crate::view::scroll_sync::ScrollSyncManager,

    /// Whether the file-explorer panel is visible in this window.
    pub file_explorer_visible: bool,

    /// Whether a file-explorer rebuild is in flight (debounce flag).
    pub file_explorer_sync_in_progress: bool,

    /// Width of the file-explorer panel.
    pub file_explorer_width: crate::config::ExplorerWidth,

    /// Side (left/right) the file-explorer panel docks on.
    pub file_explorer_side: crate::config::FileExplorerSide,

    /// Pending toggles for show-hidden/show-gitignored that apply on
    /// the next file-explorer rebuild.
    pub pending_file_explorer_show_hidden: Option<bool>,
    pub pending_file_explorer_show_gitignored: Option<bool>,

    /// Decorations supplied by plugins for the file explorer (badges,
    /// status icons, etc.) keyed by absolute path.
    pub file_explorer_decorations:
        HashMap<String, Vec<crate::view::file_tree::FileExplorerDecoration>>,

    /// Compiled decoration lookup cache invalidated when
    /// `file_explorer_decorations` changes.
    pub file_explorer_decoration_cache: crate::view::file_tree::FileExplorerDecorationCache,

    /// Slot overrides supplied by plugins for the file explorer keyed by
    /// namespace. These are additive overrides: unspecified fields continue to
    /// fall back to compatibility providers.
    pub file_explorer_slot_overrides:
        HashMap<String, Vec<fresh_core::file_explorer::FileExplorerSlotEntry>>,

    /// Compiled slot-override lookup cache invalidated when
    /// `file_explorer_slot_overrides` changes.
    pub file_explorer_slot_override_cache: crate::view::file_tree::FileExplorerSlotOverrideCache,

    /// Hover-popup correlation state (which buffer / cursor a hover
    /// request was issued from). Per-window because hover requests
    /// route through the active window's LSP.
    pub(crate) hover: crate::app::hover::HoverState,

    /// Active find-in-buffer search session (if any).
    pub(crate) search_state: Option<crate::app::types::SearchState>,

    /// Overlay namespace used for search-result highlights. Per-window
    /// because the overlays it scopes are per-buffer (per-window).
    pub search_namespace: crate::view::overlay::OverlayNamespace,

    /// Range that should be reused when the next search is confirmed
    /// (e.g. after the user picks a hit in the search overlay).
    pub pending_search_range: Option<std::ops::Range<usize>>,

    /// Last live-grep panel state (cached so re-opening the panel
    /// preserves the user's query / scroll / selection).
    pub live_grep_last_state: Option<crate::services::live_grep_state::LiveGrepLastState>,

    /// Overlay-preview state used by the floating-prompt preview pane
    /// when it's showing a buffer view.
    pub overlay_preview_state: Option<crate::app::types::OverlayPreviewState>,

    /// Tracks rapid file-change events for debouncing the auto-revert
    /// reload trigger.
    pub file_rapid_change_counts: HashMap<PathBuf, (std::time::Instant, u32)>,

    /// Cursor-position snapshot captured when the user opens the
    /// goto-line prompt, restored on Esc.
    pub(crate) goto_line_preview: Option<crate::app::GotoLinePreviewSnapshot>,

    /// Pending plugin-issued prompt callback id (used by
    /// `editor.startPrompt` to deliver the prompt result back).
    pub pending_async_prompt_callback: Option<fresh_core::api::JsCallbackId>,

    /// Buffer ids the user picked "save before quit" for via the
    /// modified-buffers prompt; consumed in order on quit.
    pub pending_quit_unnamed_save: Vec<BufferId>,

    /// Per-window search UX toggles. Each window has its own search
    /// session, so these flags follow the search state.
    pub search_case_sensitive: bool,
    pub search_whole_word: bool,
    pub search_use_regex: bool,
    pub search_confirm_each: bool,

    /// Scheduled (debounced) per-buffer LSP feature requests for the
    /// active window's LSP. Per-window because the LSP they target is
    /// per-window (Step 0k).
    pub scheduled_diagnostic_pull: Option<(BufferId, std::time::Instant)>,
    pub scheduled_inlay_hints_request: Option<(BufferId, std::time::Instant)>,

    /// LSP languages the user dismissed the "do you want to enable
    /// LSP for this language?" popup for. Per-window because LSP is
    /// per-window — different windows can prompt independently.
    pub user_dismissed_lsp_languages: std::collections::HashSet<String>,

    /// Active editor mode (e.g. "search", "replace", "macro-record").
    /// Per-window because the modes drive UI affordances that belong
    /// to one window's UX flow.
    pub editor_mode: Option<String>,

    /// Per-window prompt histories (one ring per `PromptType`). Each
    /// window has its own minibuffer, so each maintains its own
    /// history.
    pub prompt_histories: HashMap<String, crate::input::input_history::InputHistory>,

    /// Buffer id pending close-confirmation prompt resolution.
    /// Per-window because the prompt that produced this is per-window.
    pub pending_close_buffer: Option<BufferId>,

    /// Pluggable completion service that orchestrates this window's
    /// completion providers (dabbrev, buffer words, LSP, plugin
    /// providers). Per-window because the providers it orchestrates
    /// (notably the LSP set) are per-window.
    pub completion_service: crate::services::completion::CompletionService,

    /// Overlay namespace for LSP diagnostic overlays in this window
    /// (filter / bulk-remove key). The diagnostics it scopes are buffer
    /// overlays, and buffers are per-window, so the namespace follows.
    pub lsp_diagnostic_namespace: crate::view::overlay::OverlayNamespace,

    /// Last `result_id` seen per URI *and per server* for incremental
    /// pull diagnostics. Keyed URI → server name → result_id. Per-server
    /// because `Diagnostics` is a merged feature: several servers (e.g.
    /// `ruff` + `ty`) can each pull-serve the same URI with their own
    /// independent result-id streams, so a single shared id would send
    /// one server's cursor to another. Per-window because each window has
    /// its own LSP manager.
    pub diagnostic_result_ids: HashMap<String, HashMap<String, String>>,

    /// `$/progress` token → progress info for this window's LSP servers.
    /// Drives the spinner in the status bar's LSP pill. Per-window
    /// because the LspManager that emits these tokens is per-window.
    pub(crate) lsp_progress: HashMap<String, crate::app::LspProgressInfo>,

    /// Status of each `(language, server_name)` pair attached to this
    /// window's LspManager (running, errored, restarting, …).
    pub lsp_server_statuses:
        HashMap<(String, String), crate::services::async_bridge::LspServerStatus>,

    /// Plugin-contributed menu items merged into the LSP-Servers popup
    /// (the one opened by clicking the LSP indicator). Keyed by
    /// `(language, plugin_id)` so each plugin owns its own slice and
    /// can refresh it independently. The items render as an extra
    /// section in `build_and_show_lsp_status_popup` between the
    /// built-in actions and the trailing "View Log / Dismiss" rows.
    /// Selecting one fires `action_popup_result` with `popup_id =
    /// "lsp_status"` and `action_id = "{plugin_id}|{item_id}"` so the
    /// contributing plugin can react.
    ///
    /// See #1941 follow-up "Option B": instead of plugins pushing
    /// their own separate popup (which created the stacked-popup UX
    /// problem), they contribute items into the single LSP-Servers
    /// popup.
    pub lsp_menu_contributions: HashMap<(String, String), Vec<crate::app::LspMenuItem>>,

    /// Recent `window/showMessage` payloads from this window's LSP
    /// servers. Bounded ring (newest entries kept, drops the oldest
    /// when the soft cap is exceeded).
    pub(crate) lsp_window_messages: Vec<crate::app::LspMessageEntry>,

    /// Recent `window/logMessage` payloads from this window's LSP
    /// servers, on the same bounded-ring pattern as `lsp_window_messages`.
    pub(crate) lsp_log_messages: Vec<crate::app::LspMessageEntry>,

    /// Push-model diagnostics keyed by URI, then by server name. Each
    /// `publishDiagnostics` from a server replaces that server's slice
    /// for the URI; the merged view is materialised in
    /// `stored_diagnostics`. Each entry carries a byte anchor so its
    /// position tracks edits between re-publishes via `CoordMap` (#2602).
    pub stored_push_diagnostics: HashMap<String, HashMap<String, Vec<AnchoredDiagnostic>>>,

    /// Pull-model diagnostics (rust-analyzer-style native pull) keyed by
    /// URI, then by server name — mirroring `stored_push_diagnostics`.
    /// `Diagnostics` is a merged feature, so multiple servers (e.g.
    /// `ruff` + `ty` for Python) can each pull-serve the same URI; keying
    /// by server keeps their results from clobbering one another (#2615).
    /// Independent of `stored_push_diagnostics`; both are merged into
    /// `stored_diagnostics`. Each entry carries a byte anchor so its
    /// position tracks edits between re-pulls via `CoordMap` (#2602).
    pub stored_pull_diagnostics: HashMap<String, HashMap<String, Vec<AnchoredDiagnostic>>>,

    /// Merged view of push + pull diagnostics with every position mapped
    /// forward to the buffer's current version, exposed to plugins and read
    /// by hover / the diagnostics panel. Derived — never a source of truth;
    /// `recompute_merged_diagnostics` rebuilds it from the anchored push/pull
    /// stores. `Arc` wrapper so plugin snapshots can hold a refcount-bumped
    /// reference; mutation goes through `Arc::make_mut` (CoW).
    pub stored_diagnostics: Arc<HashMap<String, Vec<lsp_types::Diagnostic>>>,

    /// Per-URI folding ranges from `textDocument/foldingRange`. Same
    /// `Arc` + CoW pattern as `stored_diagnostics` so plugin snapshots
    /// don't pin the underlying map across mutations.
    pub stored_folding_ranges: Arc<HashMap<String, Vec<lsp_types::FoldingRange>>>,

    /// Per-directory mtime cache (paired with `file_mod_times`) for
    /// detecting file-tree changes in this window. Per-window because
    /// the file tree is per-window.
    pub dir_mod_times: HashMap<PathBuf, std::time::SystemTime>,

    /// Last time auto-revert polled this window's open buffers.
    pub last_auto_revert_poll: std::time::Instant,

    /// Last time the file-tree change-detection poll fired for this window.
    pub last_file_tree_poll: std::time::Instant,

    /// Whether this window has resolved and seeded the `.git/index`
    /// path in `dir_mod_times`.
    pub git_index_resolved: bool,

    /// The explicit set of `.git/index` paths this window watches for
    /// changes (one per repo; several in a monorepo). Stored directly
    /// rather than re-derived from `dir_mod_times` by name so that
    /// submodule/worktree gitdirs (`.git/modules/<x>/index`) are not
    /// dropped by a `.git/index` suffix match.
    pub watched_git_indexes: Vec<PathBuf>,

    /// Receiver for background file change poll results for this window.
    /// `Some` while a metadata poll is in flight.
    #[allow(clippy::type_complexity)]
    pub pending_file_poll_rx:
        Option<std::sync::mpsc::Receiver<Vec<(PathBuf, Option<std::time::SystemTime>)>>>,

    /// Receiver for background directory change poll results for this window.
    /// The second element carries mtimes for all watched `.git/index` files
    /// (one per repo in a monorepo, or a single entry for a normal repo).
    #[allow(clippy::type_complexity)]
    pub pending_dir_poll_rx: Option<
        std::sync::mpsc::Receiver<(
            Vec<(
                crate::view::file_tree::NodeId,
                PathBuf,
                Option<std::time::SystemTime>,
            )>,
            Vec<(PathBuf, std::time::SystemTime)>,
        )>,
    >,

    /// Receiver for the one-shot background git-index resolution (discovering
    /// every repo's `.git/index` to watch, including monorepo sub-repos).
    /// `Some` while that resolution thread is in flight. Each entry pairs an
    /// index path with its mtime at resolution time.
    #[allow(clippy::type_complexity)]
    pub pending_git_index_rx:
        Option<std::sync::mpsc::Receiver<Vec<(PathBuf, Option<std::time::SystemTime>)>>>,

    /// Terminals in this window that should not persist to the
    /// workspace file. Plugin-created terminals default to ephemeral;
    /// user-opened terminals are absent and persist as before.
    pub ephemeral_terminals: std::collections::HashSet<crate::services::terminal::TerminalId>,

    /// Argv each terminal was spawned with, when it ran a command other
    /// than the plain shell (e.g. an Orchestrator agent). Captured at spawn
    /// and persisted into the workspace so a restored workspace re-runs the
    /// same command rather than coming back as a bare shell. Terminals
    /// spawned as a plain shell have no entry.
    pub terminal_commands:
        std::collections::HashMap<crate::services::terminal::TerminalId, Vec<String>>,

    /// Argv to run on *restore* instead of re-running the launch command,
    /// for terminals that carry an agent-resume spec (Orchestrator sets this
    /// to e.g. `claude --resume <id>` / `claude --continue`). Persisted into
    /// the workspace's `agent_resume`; absent for plain terminals, which
    /// just re-run their launch command.
    pub terminal_resume_commands:
        std::collections::HashMap<crate::services::terminal::TerminalId, Vec<String>>,

    /// Plugin-development workspace per buffer (temp dir + LSP
    /// configuration for plugin buffers). Buffer-keyed and buffers
    /// are per-window, so the workspace map follows.
    pub plugin_dev_workspaces:
        HashMap<BufferId, crate::services::plugins::plugin_dev_workspace::PluginDevWorkspace>,

    /// Per-buffer plugin status-bar token values. Outer key: BufferId;
    /// inner key: "plugin_name:token_name"; inner value: current text
    /// to render. The registry of which tokens exist lives globally on
    /// `Editor.status_bar_token_registry`; this map holds only the
    /// values plugins have pushed for individual buffers.
    pub status_bar_values: HashMap<BufferId, HashMap<String, String>>,

    /// Mouse drag/selection/scrollbar state for this window. Drag
    /// targets reference per-window LeafIds and BufferIds.
    pub(crate) mouse_state: crate::app::types::MouseState,

    /// Currently focused widget context (Normal / FileExplorer /
    /// Terminal / Prompt …). Per-window because each window has its
    /// own focus state — switching windows preserves each window's
    /// focused widget.
    pub key_context: crate::input::keybindings::KeyContext,

    /// Pending chord sequence for multi-key bindings (e.g. C-x C-s).
    /// Each window tracks its own in-progress chord.
    pub chord_state: Vec<(crossterm::event::KeyCode, crossterm::event::KeyModifiers)>,

    /// Multi-click detection state (per-window because clicks land
    /// inside a window).
    pub previous_click_time: Option<std::time::Instant>,
    pub previous_click_position: Option<(u16, u16)>,
    pub click_count: u8,

    /// Whether mouse capture is enabled in this window.
    pub mouse_enabled: bool,

    /// GPM software-cursor position for this window (when GPM is
    /// active and we draw our own cursor).
    pub mouse_cursor_position: Option<(u16, u16)>,
    pub gpm_active: bool,

    /// Per-window chrome toggles. Each window can independently show
    /// or hide its menu bar / tab bar / status bar / prompt line.
    pub menu_bar_visible: bool,
    pub menu_bar_auto_shown: bool,
    pub tab_bar_visible: bool,
    pub status_bar_visible: bool,
    pub prompt_line_visible: bool,

    /// Timing state for auto-recovery saves and persistent auto-saves
    /// in this window.
    pub last_auto_recovery_save: std::time::Instant,
    pub last_persistent_auto_save: std::time::Instant,

    /// Warning domain registry for this window's status indicator.
    pub warning_domains: crate::app::warning_domains::WarningDomainRegistry,

    /// Tab context menu state (right-click on a tab in this window).
    pub tab_context_menu: Option<crate::app::types::TabContextMenu>,

    /// "+" new-tab popup menu state (left-click on the tab bar's trailing
    /// `+` button). Offers "New Terminal" / "New File".
    pub new_tab_menu: Option<crate::app::types::NewTabMenu>,

    /// File-explorer context menu state (right-click in the explorer).
    pub file_explorer_context_menu: Option<crate::app::types::FileExplorerContextMenu>,

    /// Theme inspector popup (Ctrl+Right-Click) anchored in this window.
    pub theme_info_popup: Option<crate::app::types::ThemeInfoPopup>,

    /// Event debug dialog state (when the event-debug modal is open in
    /// this window). The dialog records keystrokes for the window's
    /// input pipeline so it's logically per-window.
    pub event_debug: Option<crate::app::event_debug::EventDebug>,

    /// File-open dialog state (when PromptType::OpenFile is active in
    /// this window's prompt).
    pub file_open_state: Option<crate::app::file_open::FileOpenState>,

    /// Cached layout for the file browser (mouse hit-testing).
    pub file_browser_layout: Option<crate::view::ui::FileBrowserLayout>,

    /// Buffer groups (multiple buffers shown as one tab) in this window.
    pub buffer_groups: HashMap<crate::app::types::BufferGroupId, crate::app::types::BufferGroup>,
    /// Reverse index: buffer ID → group ID.
    pub buffer_to_group: HashMap<BufferId, crate::app::types::BufferGroupId>,
    /// Next buffer group id within this window.
    pub next_buffer_group_id: usize,

    /// Plugin keystroke-callback queue (in-flight `getNextKey()` callbacks).
    pub pending_next_key_callbacks: std::collections::VecDeque<fresh_core::api::JsCallbackId>,

    /// Whether a plugin currently has key-capture active in this window.
    pub key_capture_active: bool,

    /// Keys queued while `key_capture_active` was set but no callback
    /// was pending — drained on the next `AwaitNextKey`.
    pub pending_key_capture_buffer: std::collections::VecDeque<fresh_core::api::KeyEventPayload>,

    /// Macro state (record/playback/registers) — one window's macro
    /// session at a time.
    pub(crate) macros: crate::app::macros::MacroState,

    /// Plugin-defined custom contexts active in this window (drives
    /// command palette visibility, e.g. "config-editor").
    pub active_custom_contexts: std::collections::HashSet<String>,

    /// Whether keyboard capture is active for the terminal in this
    /// window (terminal mode swallows non-toggle keys).
    pub keyboard_capture: bool,

    /// In-flight review session hunks for this window.
    pub review_hunks: Vec<fresh_core::api::ReviewHunk>,

    /// Pending file-open queue (PendingFileOpen) for this window.
    pub pending_file_opens: Vec<crate::app::PendingFileOpen>,

    /// Whether this window has a hot-exit recovery prompt pending.
    pub pending_hot_exit_recovery: bool,

    /// Plugin "wait until file opens" tracking (buffer_id → (wait_id, …)).
    pub wait_tracking: HashMap<BufferId, (u64, bool)>,

    /// Wait ids that have completed and need to be reported back to plugins.
    pub completed_waits: Vec<u64>,

    /// Background line-scan state for this window (line counts for
    /// large files).
    pub(crate) line_scan: crate::app::line_scan::LineScan,

    /// Background search-scan state for this window.
    pub(crate) search_scan: crate::app::search_scan::SearchScan,

    /// Anchor for the search-result overlay in this window.
    pub search_overlay_top_byte: Option<usize>,

    /// Per-window UI animation runner.
    pub animations: crate::view::animation::AnimationRunner,

    /// Plugin error log (populated when plugin status messages match
    /// error patterns; tests assert against this).
    pub plugin_errors: Vec<String>,

    /// Cut/copy clipboard for file-explorer ops in this window. Each
    /// window has its own paste buffer; cross-window file ops would
    /// require a separately-shared clipboard.
    pub file_explorer_clipboard: Option<crate::app::file_explorer::FileExplorerClipboard>,

    /// Process-group tracking for everything this window owns
    /// (today: pty children from `terminal_manager.spawn`).
    /// Exposed through `signal_all` so window-level lifecycle
    /// operations can terminate every spawned process in one
    /// call regardless of how many terminals the window owns —
    /// see [`process_group`] module docs for the authority-
    /// pluggable `Signaller` design.
    pub process_groups: ProcessGroups,
}

/// Apply language-server configuration to a freshly-created
/// [`LspManager`]: per-language configs, the universal (global)
/// servers, and the Deno auto-detection override. Shared by every
/// window's construction so the server set is identical regardless of
/// how the window came to exist (boot, orchestrator new-session,
/// disk-restored shell).
pub(crate) fn configure_lsp_servers(
    lsp: &mut LspManager,
    root: &std::path::Path,
    config: &crate::config::Config,
) {
    use crate::types::{LspServerConfig, ProcessLimits};

    // Global master switch — gates auto-start of every server below.
    lsp.set_globally_enabled(config.lsp_enabled);

    // Per-language servers from config.
    for (language, lsp_configs) in &config.lsp {
        lsp.set_language_configs(language.clone(), lsp_configs.as_slice().to_vec());
    }

    // Universal (global) servers — spawned once, shared across languages.
    let universal_servers: Vec<LspServerConfig> = config
        .universal_lsp
        .values()
        .flat_map(|lc| lc.as_slice().to_vec())
        .filter(|c| c.enabled)
        .collect();
    lsp.set_universal_configs(universal_servers);

    // Auto-detect Deno projects: if deno.json or deno.jsonc exists in the
    // window root, override JS/TS LSP to use `deno lsp` (#1191). Checked
    // against the window's own root so each workspace gets the detection for
    // its actual project rather than the process cwd.
    if root.join("deno.json").exists() || root.join("deno.jsonc").exists() {
        tracing::info!("Detected Deno project (deno.json found), using deno lsp for JS/TS");
        let deno_config = LspServerConfig {
            command: "deno".to_string(),
            args: Some(vec!["lsp".to_string()]),
            enabled: true,
            auto_start: false,
            process_limits: ProcessLimits::default(),
            initialization_options: Some(serde_json::json!({"enable": true})),
            ..Default::default()
        };
        lsp.set_language_config("javascript".to_string(), deno_config.clone());
        lsp.set_language_config("typescript".to_string(), deno_config);
    }
}

/// Build the [`LspManager`] every window owns: rooted at the window's
/// own `root`, wired to its own `bridge` (which
/// `process_async_messages` drains every frame) and the shared tokio
/// runtime, and configured with the full server set. Called from
/// [`Window::new`] so the manager is present *by construction* — there
/// is no window without one, and no "No LSP manager available" state to
/// represent.
pub(crate) fn build_window_lsp(
    id: WindowId,
    root: &std::path::Path,
    authority: &crate::services::authority::Authority,
    resources: &crate::app::window_resources::WindowResources,
    bridge: &crate::services::async_bridge::AsyncBridge,
) -> LspManager {
    let root_uri = crate::app::types::file_path_to_lsp_uri(root);
    let mut lsp = LspManager::new(id, root_uri);

    // No runtime means async features are disabled (matches the
    // historical base-window path when the tokio runtime fails to build).
    if let Some(runtime) = resources.tokio_runtime.as_ref() {
        lsp.set_runtime(runtime.handle().clone(), bridge.clone());
    }

    // Wire the LSP backend from the window's authority at construction:
    // `force_spawn` routes server processes through the long-running
    // spawner, URIs are host↔container-translated via path translation, and
    // trust gates spawning. Doing this here (rather than via a later
    // `set_boot_authority`) means the manager is never left pointing at a
    // backend that doesn't match the authority the window was built with.
    lsp.set_long_running_spawner(authority.long_running_spawner.clone());
    lsp.set_path_translation(authority.path_translation.clone());
    lsp.set_workspace_trust(authority.workspace_trust.clone());

    configure_lsp_servers(&mut lsp, root, &resources.config);
    lsp
}

impl Window {
    /// Apply LSP folding ranges to the named buffer's `folding_ranges`
    /// store. Pure window mutation — no editor-global state touched.
    /// Used by the LSP folding-ranges response dispatcher after the
    /// editor-global URI-keyed map has been updated.
    pub fn apply_folding_ranges_response(
        &mut self,
        buffer_id: BufferId,
        lsp_ranges: Vec<lsp_types::FoldingRange>,
    ) {
        let Some(state) = self.buffers.get_mut(&buffer_id) else {
            return;
        };
        state
            .folding_ranges
            .set_from_lsp(&state.buffer, &mut state.marker_list, lsp_ranges);
    }

    /// Allocate a fresh per-window LSP request id and return it. The
    /// counter is per-window because each window's `LspManager` talks
    /// to its own server connections — no global namespace needed.
    pub fn alloc_lsp_request_id(&mut self) -> u64 {
        let id = self.next_lsp_request_id;
        self.next_lsp_request_id += 1;
        id
    }

    /// True if this window has any in-flight LSP completion or
    /// goto-definition request whose response would still be relevant.
    pub fn has_pending_lsp_requests(&self) -> bool {
        !self.pending_completion_requests.is_empty()
            || self.pending_goto_definition_request.is_some()
    }

    /// Cancel any in-flight LSP requests on this window. Called when
    /// the user does something that would make the response stale
    /// (cursor movement, text edit, scroll). Drains the pending
    /// completion id set, clears the goto-definition slot, and sends
    /// `$/cancelRequest` to the appropriate server for each.
    pub(crate) fn cancel_pending_lsp_requests(&mut self) {
        self.scheduled_completion_trigger = None;
        if !self.pending_completion_requests.is_empty() {
            let ids: Vec<u64> = self.pending_completion_requests.drain().collect();
            for request_id in ids {
                tracing::debug!("Canceling pending LSP completion request {}", request_id);
                self.send_lsp_cancel_request(request_id);
            }
        }
        if let Some(request_id) = self.pending_goto_definition_request.take() {
            tracing::debug!(
                "Canceling pending LSP goto-definition request {}",
                request_id
            );
            self.send_lsp_cancel_request(request_id);
        }
    }

    /// Send `$/cancelRequest` to the LSP server backing the active
    /// buffer's language, if a server is already running. Called only
    /// from cancel paths — does not spawn a server just to cancel.
    pub(crate) fn send_lsp_cancel_request(&mut self, request_id: u64) {
        let buffer_id = self.active_buffer();
        let Some(language) = self.buffers.get(&buffer_id).map(|s| s.language.clone()) else {
            return;
        };
        {
            let lsp = &mut self.lsp;
            if let Some(handle) = lsp.get_handle_mut(&language) {
                if let Err(e) = handle.cancel_request(request_id) {
                    tracing::warn!("Failed to send LSP cancel request: {}", e);
                } else {
                    tracing::debug!("Sent $/cancelRequest for request_id={}", request_id);
                }
            }
        }
    }

    /// Toggle this window's tab-bar visibility and post a status message.
    pub fn toggle_tab_bar(&mut self) {
        self.tab_bar_visible = !self.tab_bar_visible;
        let key = if self.tab_bar_visible {
            "toggle.tab_bar_shown"
        } else {
            "toggle.tab_bar_hidden"
        };
        self.set_status_message(rust_i18n::t!(key).to_string());
    }

    /// Toggle this window's status-bar visibility and post a status message.
    pub fn toggle_status_bar(&mut self) {
        self.status_bar_visible = !self.status_bar_visible;
        let key = if self.status_bar_visible {
            "toggle.status_bar_shown"
        } else {
            "toggle.status_bar_hidden"
        };
        self.set_status_message(rust_i18n::t!(key).to_string());
    }

    /// Toggle this window's prompt-line visibility and post a status message.
    pub fn toggle_prompt_line(&mut self) {
        self.prompt_line_visible = !self.prompt_line_visible;
        let key = if self.prompt_line_visible {
            "toggle.prompt_line_shown"
        } else {
            "toggle.prompt_line_hidden"
        };
        self.set_status_message(rust_i18n::t!(key).to_string());
    }

    /// Toggle this window's same-buffer scroll-sync flag and post a
    /// status message announcing the new state.
    pub fn toggle_scroll_sync(&mut self) {
        self.same_buffer_scroll_sync = !self.same_buffer_scroll_sync;
        let key = if self.same_buffer_scroll_sync {
            "toggle.scroll_sync_enabled"
        } else {
            "toggle.scroll_sync_disabled"
        };
        self.set_status_message(rust_i18n::t!(key).to_string());
    }

    /// Toggle the active buffer's `debug_highlight_mode` (shows byte
    /// positions and highlight-span info on screen). No-op if there is
    /// no active buffer.
    pub fn toggle_debug_highlights(&mut self) {
        let buffer_id = self.active_buffer();
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.debug_highlight_mode = !state.debug_highlight_mode;
            let key = if state.debug_highlight_mode {
                "toggle.debug_mode_on"
            } else {
                "toggle.debug_mode_off"
            };
            self.set_status_message(rust_i18n::t!(key).to_string());
        }
    }

    /// Build a compiled `regex::Regex` from this window's current
    /// search-flags (`use_regex`, `whole_word`, `case_sensitive`)
    /// applied to `query`. Returns the compiled regex or a
    /// human-readable error string.
    pub(crate) fn build_search_regex(&self, query: &str) -> Result<regex::Regex, String> {
        crate::app::regex_replace::build_search_regex(
            query,
            self.search_use_regex,
            self.search_whole_word,
            self.search_case_sensitive,
        )
    }

    /// True iff editing should be disabled for the active buffer
    /// (e.g. read-only virtual buffers like the help manual).
    pub fn is_editing_disabled(&self) -> bool {
        self.active_state().editing_disabled
    }

    /// Recompute the active buffer's `modified` flag from the event log's
    /// position relative to its last-saved point. Called after undo/redo
    /// to correctly report "buffer is dirty / clean" in the status bar.
    pub(super) fn update_modified_from_event_log(&mut self) {
        let buffer_id = self.active_buffer();
        let is_at_saved = self
            .event_logs
            .get(&buffer_id)
            .map(|log| log.is_at_saved_position())
            .unwrap_or(false);
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.buffer.set_modified(!is_at_saved);
        }
    }

    /// True iff `language` is currently user-dismissed in this window's
    /// LSP status pill.
    pub fn is_lsp_language_user_dismissed(&self, language: &str) -> bool {
        self.user_dismissed_lsp_languages.contains(language)
    }

    /// Dismiss the LSP pill for `language` in this window until the user
    /// re-enables it (or the editor restarts).
    pub fn dismiss_lsp_language(&mut self, language: &str) {
        self.user_dismissed_lsp_languages
            .insert(language.to_string());
    }

    /// Undo a previous dismissal — the pill for `language` returns to its
    /// normal style.
    pub fn undismiss_lsp_language(&mut self, language: &str) {
        self.user_dismissed_lsp_languages.remove(language);
    }

    /// True iff at least one LSP server attached to the active buffer's
    /// language advertises `codeAction/resolve`.
    pub(crate) fn server_supports_code_action_resolve(&self) -> bool {
        let Some(language) = self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
        else {
            return false;
        };
        {
            let lsp = &self.lsp;
            for sh in lsp.get_handles(&language) {
                if sh.capabilities.code_action_resolve {
                    return true;
                }
            }
        }
        false
    }

    /// True iff at least one LSP server attached to the active buffer's
    /// language advertises `completionItem/resolve`.
    pub(crate) fn server_supports_completion_resolve(&self) -> bool {
        let Some(language) = self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
        else {
            return false;
        };
        {
            let lsp = &self.lsp;
            for sh in lsp.get_handles(&language) {
                if sh.capabilities.completion_resolve {
                    return true;
                }
            }
        }
        false
    }

    /// True iff at least one LSP server attached to the active buffer's
    /// language advertises `textDocument/rename` (and therefore the
    /// `prepareRename` request, which the editor surfaces only through
    /// the rename feature flag).
    pub(crate) fn server_supports_prepare_rename(&self) -> bool {
        let Some(language) = self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
        else {
            return false;
        };
        {
            let lsp = &self.lsp;
            for sh in lsp.get_handles(&language) {
                if sh.capabilities.rename {
                    return true;
                }
            }
        }
        false
    }

    /// Send `textDocument/prepareRename` for the symbol at the active
    /// cursor. No-op if the buffer has no LSP metadata, no language, or
    /// no rename-capable handle. The response is dispatched to
    /// `handle_prepare_rename_response`.
    pub(crate) fn send_prepare_rename(&mut self) {
        let cursor_pos = self.active_cursors().primary().position;
        let (line, character) = self
            .active_state()
            .buffer
            .position_to_lsp_position(cursor_pos);

        let buffer_id = self.active_buffer();
        let metadata = match self.buffer_metadata.get(&buffer_id) {
            Some(m) if m.lsp_enabled => m,
            _ => return,
        };
        let uri = match metadata.file_uri() {
            Some(u) => u.clone(),
            None => return,
        };
        let Some(language) = self.buffers.get(&buffer_id).map(|s| s.language.clone()) else {
            return;
        };

        let request_id = self.alloc_lsp_request_id();

        {
            let lsp = &mut self.lsp;
            if let Some(sh) = lsp.handle_for_feature_mut(&language, LspFeature::Rename) {
                if let Err(e) = sh.handle.prepare_rename(
                    request_id,
                    uri.as_uri().clone(),
                    line as u32,
                    character as u32,
                ) {
                    tracing::warn!("Failed to send prepareRename: {}", e);
                }
            }
        }
    }

    /// Send `completionItem/resolve` for `item` to the first LSP server
    /// (in language order) that advertises `completion_resolve` for the
    /// active buffer's language. No-op if no server is running or no
    /// server supports the resolve.
    pub(crate) fn send_completion_resolve(&mut self, item: lsp_types::CompletionItem) {
        let Some(language) = self
            .buffers
            .get(&self.active_buffer())
            .map(|s| s.language.clone())
        else {
            return;
        };
        let request_id = self.alloc_lsp_request_id();
        {
            let lsp = &mut self.lsp;
            for sh in lsp.get_handles_mut(&language) {
                if sh.capabilities.completion_resolve {
                    if let Err(e) = sh.handle.completion_resolve(request_id, item.clone()) {
                        tracing::warn!(
                            "Failed to send completionItem/resolve to '{}': {}",
                            sh.name,
                            e
                        );
                    }
                    return;
                }
            }
        }
    }

    /// Apply an event to a buffer + the cursors of a split inside this
    /// window. Window-level method (not Editor-level) so the borrow
    /// checker can split-borrow `self.buffers` and `self.splits`
    /// cleanly without inline `self.windows.get_mut(...)` boilerplate
    /// at the call site. No-op if the buffer or split is missing.
    pub fn apply_event_to_buffer(
        &mut self,
        buffer_id: BufferId,
        split_id: LeafId,
        event: &crate::model::event::Event,
    ) {
        self.buffers
            .with_buffer_and_split(buffer_id, split_id, |state, vs| {
                state.apply(&mut vs.cursors, event);
            });
    }

    /// Same as [`apply_event_to_buffer`] but operates on a buffer-group
    /// panel's keyed cursor (the `keyed_states[buffer_id].cursors`
    /// inside the host split's view state, not the host's own cursors).
    /// Used by event-apply paths that target a focused inner panel of
    /// a Grouped split rather than the outer split's leaf buffer.
    pub fn apply_event_to_keyed_buffer(
        &mut self,
        buffer_id: BufferId,
        split_id: LeafId,
        event: &crate::model::event::Event,
    ) {
        self.buffers
            .with_buffer_and_split(buffer_id, split_id, |state, vs| {
                if let Some(keyed) = vs.keyed_states.get_mut(&buffer_id) {
                    state.apply(&mut keyed.cursors, event);
                }
            });
    }

    /// Scroll the named split's viewport so the buffer's primary cursor
    /// is visible. Calls into `SplitViewState::ensure_cursor_visible`
    /// with the buffer's text + marker list. No-op if buffer/split is
    /// missing.
    pub fn ensure_cursor_visible_for_split(&mut self, buffer_id: BufferId, split_id: LeafId) {
        self.buffers
            .with_buffer_and_split(buffer_id, split_id, |state, vs| {
                vs.ensure_cursor_visible(&mut state.buffer, &state.marker_list);
            });
    }

    /// Scroll a split's viewport to the given line, given a buffer to
    /// resolve the line→byte offset. No-op if buffer/split is missing.
    /// `lock_against_ensure_visible`: when true, sets the
    /// skip-ensure-visible flag so the next render's cursor-visibility
    /// pass doesn't undo this scroll. Plugin-driven jumps want true;
    /// scroll-sync-from-active-to-other-splits wants false.
    pub fn scroll_split_viewport_to(
        &mut self,
        buffer_id: BufferId,
        split_id: LeafId,
        target_line: usize,
        lock_against_ensure_visible: bool,
    ) {
        self.buffers
            .with_buffer_and_split(buffer_id, split_id, |state, vs| {
                vs.viewport.scroll_to(&mut state.buffer, target_line);
                if lock_against_ensure_visible {
                    vs.viewport.set_skip_ensure_visible();
                }
            });
    }

    /// Add a collapsed fold range on `buffer_id`'s marker list and on
    /// every view state hosting the buffer. Returns `true` when the
    /// buffer was found (so the caller knows to flag a render). No-op
    /// when the buffer is missing.
    pub fn add_fold(
        &mut self,
        buffer_id: BufferId,
        start: usize,
        end: usize,
        placeholder: Option<String>,
    ) -> bool {
        self.buffers
            .with_buffer_and_view_states(buffer_id, |state, vs_map| {
                for vs in vs_map.values_mut() {
                    if vs.keyed_states.contains_key(&buffer_id) {
                        let buf_state = vs.ensure_buffer_state(buffer_id);
                        buf_state.folds.add(
                            &mut state.marker_list,
                            start,
                            end,
                            placeholder.clone(),
                        );
                    }
                }
            })
            .is_some()
    }

    /// Clear every fold range on `buffer_id` across the window's view
    /// states. Returns `true` when the buffer was found.
    pub fn clear_folds(&mut self, buffer_id: BufferId) -> bool {
        self.buffers
            .with_buffer_and_view_states(buffer_id, |state, vs_map| {
                for vs in vs_map.values_mut() {
                    if vs.keyed_states.contains_key(&buffer_id) {
                        let buf_state = vs.ensure_buffer_state(buffer_id);
                        buf_state.folds.clear(&mut state.marker_list);
                    }
                }
            })
            .is_some()
    }

    /// Move every supplied split's primary cursor to `position` in
    /// `buffer_id` and re-anchor the viewport to keep it visible.
    /// Caller is responsible for computing `splits` (typically by
    /// walking the split tree plus any grouped subtrees on the
    /// editor — those live outside the window). No-op for missing
    /// buffer/splits.
    pub fn set_buffer_cursor_in_splits(
        &mut self,
        buffer_id: BufferId,
        position: usize,
        splits: &[LeafId],
    ) {
        self.buffers
            .with_buffer_and_view_states(buffer_id, |state, vs_map| {
                let mut moved_any = false;
                for leaf_id in splits {
                    let Some(view_state) = vs_map.get_mut(leaf_id) else {
                        continue;
                    };
                    view_state.cursors.primary_mut().move_to(position, false);
                    view_state.ensure_cursor_visible(&mut state.buffer, &state.marker_list);
                    moved_any = true;
                }
                // Refresh the cached primary cursor line number so the status
                // bar (and any other consumer of `primary_cursor_line_number`)
                // reflects the new position. Other cursor-move paths update
                // this cache themselves; without doing the same here, a
                // plugin-driven setBufferCursor would leave the cache pinned
                // to its initial Absolute(0) — the user-visible "off-by-one"
                // when opening blame from line 2 still shows "Ln 1". Guard
                // on `moved_any` so we don't desync the cache when no split
                // was actually carrying the buffer's cursor.
                if moved_any {
                    let line = state
                        .buffer
                        .offset_to_position(position)
                        .map(|p| p.line)
                        .unwrap_or(0);
                    state.primary_cursor_line_number =
                        crate::model::buffer::LineNumber::Absolute(line);
                }
            });
    }

    /// Scroll `leaf_id`'s viewport so the byte position `top_byte` is
    /// the new top line, using `buffer_id` to resolve byte→line. Sets
    /// `skip_ensure_visible` so the next render's cursor-visibility
    /// pass doesn't undo the plugin-driven scroll. No-op for missing
    /// buffer/split.
    pub fn set_split_scroll_to_byte(
        &mut self,
        buffer_id: BufferId,
        leaf_id: LeafId,
        top_byte: usize,
    ) {
        self.buffers
            .with_buffer_and_split(buffer_id, leaf_id, |state, view_state| {
                let total_bytes = state.buffer.len();
                let clamped_byte = top_byte.min(total_bytes);
                let target_line = state
                    .buffer
                    .offset_to_position(clamped_byte)
                    .map(|p| p.line)
                    .unwrap_or(0);
                view_state
                    .viewport
                    .scroll_to(&mut state.buffer, target_line);
                view_state.viewport.top_byte = clamped_byte;
                view_state.viewport.top_view_line_offset = 0;
                view_state.viewport.set_skip_ensure_visible();
            });
    }

    /// Scroll every supplied split so `line` is roughly a third
    /// from the top of the viewport, using `buffer_id` for line
    /// resolution. Used for plugin-driven "scroll buffer to line"
    /// where the caller has already collected target leaves
    /// (including those from grouped subtrees).
    pub fn scroll_buffer_to_line_in_splits(
        &mut self,
        buffer_id: BufferId,
        target_leaves: &[LeafId],
        line: usize,
    ) {
        self.buffers
            .with_buffer_and_view_states(buffer_id, |state, vs_map| {
                for leaf_id in target_leaves {
                    let Some(view_state) = vs_map.get_mut(leaf_id) else {
                        continue;
                    };
                    let viewport_height = view_state.viewport.height as usize;
                    let lines_above = viewport_height / 3;
                    let target = line.saturating_sub(lines_above);
                    view_state.viewport.scroll_to(&mut state.buffer, target);
                    view_state.viewport.set_skip_ensure_visible();
                }
            });
    }

    /// Apply a previously-saved cursor + scroll position to a
    /// specific buffer's keyed view state inside a specific split.
    /// Restoration must NOT go through `view_state.viewport` /
    /// `view_state.cursors` — those Deref to the split's *active*
    /// buffer's view, which for `open_file_no_focus` is still the
    /// previously-active buffer; writing through the Deref would
    /// scroll the unrelated active buffer. After restoring the
    /// fields, reconciles cursor visibility against viewport
    /// (#1689 follow-up). No-op if buffer/split is missing.
    pub fn restore_buffer_state_in_split(
        &mut self,
        buffer_id: BufferId,
        split_id: LeafId,
        file_state: &crate::workspace::SerializedFileState,
    ) {
        self.buffers
            .with_buffer_and_split(buffer_id, split_id, |buffer_state, vs| {
                let Some(buf_state) = vs.keyed_states.get_mut(&buffer_id) else {
                    return;
                };
                let max_pos = buffer_state.buffer.len();
                let cursor_pos = file_state.cursor.position.min(max_pos);
                buf_state.cursors.primary_mut().position = cursor_pos;
                buf_state.cursors.primary_mut().anchor =
                    file_state.cursor.anchor.map(|a| a.min(max_pos));
                buf_state.viewport.top_byte = file_state.scroll.top_byte;
                buf_state.viewport.left_column = file_state.scroll.left_column;
                crate::app::navigation::reconcile_restored_buffer_view(
                    buf_state,
                    &mut buffer_state.buffer,
                );
            });
    }

    /// Configure `leaf_id`'s viewport for a terminal-buffer
    /// scrollback view: disable line wrap, clear any pending
    /// skip-ensure-visible flag, then scroll so the buffer's primary
    /// cursor (positioned at end-of-buffer when entering scrollback)
    /// is visible. No-op if the buffer or split is missing.
    pub fn enter_terminal_scrollback_view(&mut self, buffer_id: BufferId, leaf_id: LeafId) {
        self.buffers
            .with_buffer_and_split(buffer_id, leaf_id, |state, view_state| {
                view_state.viewport.line_wrap_enabled = false;
                view_state.viewport.clear_skip_ensure_visible();
                view_state.ensure_cursor_visible(&mut state.buffer, &state.marker_list);
            });
    }

    /// Install a freshly-loaded `EditorState` for a terminal buffer:
    /// replace the slot's state, push every per-split cursor showing
    /// the buffer to end-of-buffer (scrollback start), clear the
    /// modified flag (terminals are never user-modified), disable
    /// editing (scrollback mode), and turn off line-number margins.
    /// Used by workspace restore when re-loading the on-disk
    /// rendering of a previously-running terminal.
    pub fn install_terminal_buffer_state(
        &mut self,
        buffer_id: BufferId,
        new_state: crate::state::EditorState,
    ) {
        self.buffers
            .with_buffer_and_view_states(buffer_id, |state, vs_map| {
                *state = new_state;
                let total = state.buffer.total_bytes();
                for vs in vs_map.values_mut() {
                    if vs.has_buffer(buffer_id) {
                        vs.cursors.primary_mut().position = total;
                        // Disable gutter + current-line highlight for the
                        // terminal buffer's per-buffer view state so that
                        // exiting terminal mode on a restored terminal
                        // doesn't flash a line-number column. The render
                        // path overwrites the buffer's margin config from
                        // this flag every frame, so the buffer-level
                        // `configure_for_line_numbers(false)` below isn't
                        // enough on its own.
                        let buf_state = vs.ensure_buffer_state(buffer_id);
                        buf_state.show_line_numbers = false;
                        buf_state.highlight_current_line = false;
                        buf_state.viewport.line_wrap_enabled = false;
                    }
                }
                state.buffer.set_modified(false);
                state.editing_disabled = true;
                state.margins.configure_for_line_numbers(false);
            });
    }

    /// Scroll `leaf_id`'s viewport by `delta` lines (negative = up,
    /// positive = down). Honours `view_transform_tokens` when present
    /// (uses view-aware scrolling) and falls back to buffer-based
    /// `scroll_up` / `scroll_down`. After scrolling, skips
    /// ensure_visible and snaps the viewport top to a fold boundary
    /// if the new top byte landed inside a collapsed fold.
    /// `tab_size` is needed for view-line tokenization.
    pub fn scroll_split_by_lines(
        &mut self,
        buffer_id: BufferId,
        leaf_id: LeafId,
        delta: i32,
        view_transform_tokens: Option<Vec<fresh_core::api::ViewTokenWire>>,
        tab_size: usize,
    ) {
        self.buffers
            .with_buffer_and_split(buffer_id, leaf_id, |state, view_state| {
                let soft_breaks = state.collect_soft_break_positions();
                let virtual_lines = state.collect_virtual_line_positions();
                let buffer = &mut state.buffer;
                let top_byte_before = view_state.viewport.top_byte;
                if let Some(tokens) = view_transform_tokens {
                    use crate::view::ui::view_pipeline::ViewLineIterator;
                    let view_lines: Vec<_> =
                        ViewLineIterator::new(&tokens, false, false, tab_size, false).collect();
                    view_state
                        .viewport
                        .scroll_view_lines(&view_lines, delta as isize);
                } else if delta < 0 {
                    let lines_to_scroll = delta.unsigned_abs() as usize;
                    view_state.viewport.scroll_up(
                        buffer,
                        &soft_breaks,
                        &virtual_lines,
                        lines_to_scroll,
                    );
                } else {
                    let lines_to_scroll = delta as usize;
                    view_state.viewport.scroll_down(
                        buffer,
                        &soft_breaks,
                        &virtual_lines,
                        lines_to_scroll,
                    );
                }
                view_state.viewport.set_skip_ensure_visible();

                if let Some(folds) = view_state.keyed_states.get(&buffer_id).map(|bs| &bs.folds) {
                    if !folds.is_empty() {
                        let top_line = buffer.get_line_number(view_state.viewport.top_byte);
                        if let Some(range) = folds
                            .resolved_ranges(buffer, &state.marker_list)
                            .iter()
                            .find(|r| top_line >= r.start_line && top_line <= r.end_line)
                        {
                            let target_line = if delta >= 0 {
                                range.end_line.saturating_add(1)
                            } else {
                                range.header_line
                            };
                            let target_byte = buffer
                                .line_start_offset(target_line)
                                .unwrap_or_else(|| buffer.len());
                            view_state.viewport.top_byte = target_byte;
                            view_state.viewport.top_view_line_offset = 0;
                        }
                    }
                }
                tracing::trace!(
                    "scroll_split_by_lines: delta={}, top_byte {} -> {}",
                    delta,
                    top_byte_before,
                    view_state.viewport.top_byte
                );
            });
    }

    /// Scroll the Live Grep overlay's preview pane by `delta` lines
    /// (issue #2119). The preview lives in `overlay_preview_state` (not in
    /// the split tree), so it needs its own scroll path rather than going
    /// through `scroll_split_by_lines`. Returns true if a preview was present
    /// and scrolled.
    pub fn scroll_overlay_preview_by_lines(&mut self, delta: i32) -> bool {
        let buffer_id = match self.overlay_preview_state.as_ref() {
            Some(ps) if !ps.blanked => ps.buffer_id,
            _ => return false,
        };
        // Gather buffer-derived inputs first so the mutable borrows below stay
        // disjoint (buffer store vs. overlay preview state).
        let (soft_breaks, virtual_lines) = match self.buffers.get(&buffer_id) {
            Some(s) => (
                s.collect_soft_break_positions(),
                s.collect_virtual_line_positions(),
            ),
            None => return false,
        };
        let Some(state) = self.buffers.get_mut(&buffer_id) else {
            return false;
        };
        let buffer = &mut state.buffer;
        let Some(ps) = self.overlay_preview_state.as_mut() else {
            return false;
        };
        let viewport = &mut ps.view_state.active_state_mut().viewport;
        if delta < 0 {
            viewport.scroll_up(
                buffer,
                &soft_breaks,
                &virtual_lines,
                delta.unsigned_abs() as usize,
            );
        } else {
            viewport.scroll_down(buffer, &soft_breaks, &virtual_lines, delta as usize);
        }
        viewport.set_skip_ensure_visible();
        true
    }

    /// Clear LSP-related overlays (diagnostics, virtual texts,
    /// folding ranges, and folds) for `buffer_id`, used when LSP is
    /// being disabled for the buffer. Pure window-state mutation.
    pub fn clear_lsp_overlays_for_buffer(
        &mut self,
        buffer_id: BufferId,
        diagnostic_namespace: &crate::model::event::OverlayNamespace,
    ) {
        self.buffers
            .with_buffer_and_view_states(buffer_id, |state, vs_map| {
                state
                    .overlays
                    .clear_namespace(diagnostic_namespace, &mut state.marker_list);
                state.virtual_texts.clear(&mut state.marker_list);
                state.folding_ranges.clear(&mut state.marker_list);
                for view_state in vs_map.values_mut() {
                    if let Some(buf_state) = view_state.keyed_states.get_mut(&buffer_id) {
                        buf_state.folds.clear(&mut state.marker_list);
                    }
                }
            });
    }

    /// Mutable handle to this window's split tree (or `None` when
    /// the layout hasn't been seeded yet). Useful at sites where
    /// the caller already has a `&mut Window` from a direct
    /// `self.windows.get_mut(&id)` and wants the split layout
    /// without going back through Editor's accessor.
    pub fn split_manager_mut(&mut self) -> Option<&mut SplitManager> {
        self.buffers.split_manager_mut()
    }

    /// Mutable handle to this window's per-leaf view state map.
    pub fn split_view_states_mut(&mut self) -> Option<&mut HashMap<LeafId, SplitViewState>> {
        self.buffers.split_view_states_mut()
    }

    /// Both halves of the split layout at once. Returns `None` if
    /// the layout hasn't been seeded yet.
    pub fn splits_mut(
        &mut self,
    ) -> Option<(&mut SplitManager, &mut HashMap<LeafId, SplitViewState>)> {
        self.buffers.splits_mut().map(|(m, vs)| (m, vs))
    }

    /// Construct a window.
    ///
    /// `root` is taken as-is (the caller is responsible for
    /// canonicalisation). `label` defaults to the basename of
    /// `root` when empty. `resources` is the editor-global service
    /// bundle every window holds an `Arc`-cloned reference to — see
    /// [`WindowResources`] for the rationale.
    pub fn new(
        id: WindowId,
        label: impl Into<String>,
        root: PathBuf,
        authority: crate::services::authority::Authority,
        resources: WindowResources,
    ) -> Self {
        let mut label = label.into();
        if label.is_empty() {
            label = root
                .file_name()
                .and_then(|n| n.to_str())
                .map(str::to_owned)
                .unwrap_or_else(|| "main".to_owned());
        }
        // Seed every poll/throttle timestamp with the *editor's* time
        // source rather than real wall-clock — otherwise tests using
        // `TestTimeSource::advance` see a misaligned baseline and
        // `elapsed_since` returns less than the configured interval
        // (broke auto-save / auto-recovery tests after these fields
        // moved off `Editor`).
        let now = resources.time_source.now();
        // Build this window's bridge and LSP manager up front so the
        // manager is wired to the window's own channel and present by
        // construction (see `build_window_lsp`). `&root`/`&resources`
        // are borrowed here, then moved into the struct below.
        let bridge = crate::services::async_bridge::AsyncBridge::new();
        let lsp = build_window_lsp(id, &root, &authority, &resources, &bridge);
        Self {
            id,
            label,
            root,
            authority,
            file_explorer: None,
            file_mod_times: HashMap::new(),
            plugin_state: HashMap::new(),
            authority_spec: crate::services::authority::SessionAuthoritySpec::Local,
            remote_reconnect_error: None,
            lsp,
            panel_ids: HashMap::new(),
            buffers: WindowBuffers::new(),
            buffer_metadata: HashMap::new(),
            terminal_manager: crate::services::terminal::TerminalManager::new(id),
            terminal_buffers: HashMap::new(),
            terminal_backing_files: HashMap::new(),
            terminal_log_files: HashMap::new(),
            terminal_explicit_titles: std::collections::HashSet::new(),
            terminal_fg_poll_at: None,
            terminal_fg_cache: HashMap::new(),
            event_logs: HashMap::new(),
            status_message: None,
            plugin_status_message: None,
            prompt: None,
            bridge,
            next_lsp_request_id: 0,
            pending_completion_requests: std::collections::HashSet::new(),
            completion_items: None,
            scheduled_completion_trigger: None,
            dabbrev_state: None,
            pending_goto_definition_request: None,
            pending_references_request: None,
            pending_references_symbol: String::new(),
            pending_implementation_request: None,
            pending_implementation_symbol: String::new(),
            pending_signature_help_request: None,
            pending_code_actions_requests: std::collections::HashSet::new(),
            pending_code_actions_server_names: std::collections::HashMap::new(),
            pending_code_actions: None,
            pending_inlay_hints_requests: std::collections::HashMap::new(),
            pending_folding_range_requests: std::collections::HashMap::new(),
            folding_ranges_in_flight: std::collections::HashMap::new(),
            folding_ranges_debounce: std::collections::HashMap::new(),
            pending_semantic_token_requests: std::collections::HashMap::new(),
            semantic_tokens_in_flight: std::collections::HashMap::new(),
            semantic_tokens_full_debounce: std::collections::HashMap::new(),
            pending_semantic_token_range_requests: std::collections::HashMap::new(),
            semantic_tokens_range_in_flight: std::collections::HashMap::new(),
            semantic_tokens_range_last_request: std::collections::HashMap::new(),
            semantic_tokens_range_applied: std::collections::HashMap::new(),
            position_history: crate::input::position_history::PositionHistory::new(),
            in_navigation: false,
            suppress_position_history_once: false,
            bookmarks: crate::app::bookmarks::BookmarkState::default(),
            grouped_subtrees: HashMap::new(),
            composite_buffers: HashMap::new(),
            composite_view_states: HashMap::new(),
            layout_cache: WindowLayoutCache::default(),
            chrome_layout: ChromeLayout::default(),
            terminal_width: 80,
            terminal_height: 24,
            dock_cols: 0,
            preview: None,
            terminal_link_hover: None,
            seen_byte_ranges: HashMap::new(),
            previous_viewports: HashMap::new(),
            same_buffer_scroll_sync: false,
            interactive_replace_state: None,
            scroll_sync_manager: crate::view::scroll_sync::ScrollSyncManager::new(),
            file_explorer_visible: false,
            file_explorer_sync_in_progress: false,
            file_explorer_width: resources.config.file_explorer.width,
            file_explorer_side: resources.config.file_explorer.side,
            pending_file_explorer_show_hidden: None,
            pending_file_explorer_show_gitignored: None,
            file_explorer_decorations: HashMap::new(),
            file_explorer_decoration_cache:
                crate::view::file_tree::FileExplorerDecorationCache::default(),
            file_explorer_slot_overrides: HashMap::new(),
            file_explorer_slot_override_cache:
                crate::view::file_tree::FileExplorerSlotOverrideCache::default(),
            hover: crate::app::hover::HoverState::default(),
            search_state: None,
            search_namespace: crate::view::overlay::OverlayNamespace::from_string(
                "search".to_string(),
            ),
            pending_search_range: None,
            live_grep_last_state: None,
            overlay_preview_state: None,
            file_rapid_change_counts: HashMap::new(),
            goto_line_preview: None,
            pending_async_prompt_callback: None,
            pending_quit_unnamed_save: Vec::new(),
            search_case_sensitive: true,
            search_whole_word: false,
            search_use_regex: false,
            search_confirm_each: false,
            scheduled_diagnostic_pull: None,
            scheduled_inlay_hints_request: None,
            user_dismissed_lsp_languages: std::collections::HashSet::new(),
            editor_mode: None,
            prompt_histories: HashMap::new(),
            pending_close_buffer: None,
            completion_service: crate::services::completion::CompletionService::new(),
            lsp_diagnostic_namespace: crate::view::overlay::OverlayNamespace::from_string(
                "lsp-diagnostic".to_string(),
            ),
            diagnostic_result_ids: HashMap::new(),
            lsp_progress: HashMap::new(),
            lsp_server_statuses: HashMap::new(),
            lsp_menu_contributions: HashMap::new(),
            lsp_window_messages: Vec::new(),
            lsp_log_messages: Vec::new(),
            stored_push_diagnostics: HashMap::new(),
            stored_pull_diagnostics: HashMap::new(),
            stored_diagnostics: Arc::new(HashMap::new()),
            stored_folding_ranges: Arc::new(HashMap::new()),
            dir_mod_times: HashMap::new(),
            last_auto_revert_poll: now,
            last_file_tree_poll: now,
            git_index_resolved: false,
            watched_git_indexes: Vec::new(),
            pending_git_index_rx: None,
            pending_file_poll_rx: None,
            pending_dir_poll_rx: None,
            ephemeral_terminals: std::collections::HashSet::new(),
            terminal_commands: std::collections::HashMap::new(),
            terminal_resume_commands: std::collections::HashMap::new(),
            plugin_dev_workspaces: HashMap::new(),
            status_bar_values: HashMap::new(),
            mouse_state: crate::app::types::MouseState::default(),
            key_context: crate::input::keybindings::KeyContext::Normal,
            chord_state: Vec::new(),
            previous_click_time: None,
            previous_click_position: None,
            click_count: 0,
            mouse_enabled: false,
            mouse_cursor_position: None,
            gpm_active: false,
            menu_bar_visible: resources.config.editor.show_menu_bar,
            menu_bar_auto_shown: false,
            tab_bar_visible: resources.config.editor.show_tab_bar,
            status_bar_visible: resources.config.editor.show_status_bar,
            prompt_line_visible: resources.config.editor.show_prompt_line,
            last_auto_recovery_save: now,
            last_persistent_auto_save: now,
            warning_domains: crate::app::warning_domains::WarningDomainRegistry::default(),
            tab_context_menu: None,
            new_tab_menu: None,
            file_explorer_context_menu: None,
            theme_info_popup: None,
            event_debug: None,
            file_open_state: None,
            file_browser_layout: None,
            buffer_groups: HashMap::new(),
            buffer_to_group: HashMap::new(),
            next_buffer_group_id: 0,
            pending_next_key_callbacks: std::collections::VecDeque::new(),
            key_capture_active: false,
            pending_key_capture_buffer: std::collections::VecDeque::new(),
            macros: crate::app::macros::MacroState::default(),
            active_custom_contexts: std::collections::HashSet::new(),
            keyboard_capture: false,
            review_hunks: Vec::new(),
            pending_file_opens: Vec::new(),
            pending_hot_exit_recovery: false,
            wait_tracking: HashMap::new(),
            completed_waits: Vec::new(),
            line_scan: crate::app::line_scan::LineScan::default(),
            search_scan: crate::app::search_scan::SearchScan::default(),
            search_overlay_top_byte: None,
            animations: crate::view::animation::AnimationRunner::default(),
            plugin_errors: Vec::new(),
            file_explorer_clipboard: None,
            process_groups: ProcessGroups::default(),
            resources,
        }
    }

    // ---- Resource accessors (canonical reading API) ----
    //
    // These are thin wrappers around `self.resources.X` for the most
    // commonly-read resources. Use them at sites where the borrow
    // checker is happy with a method call; fall back to direct
    // `self.resources.X` field access at sites that need to split-borrow
    // alongside other Window sub-fields.

    /// Read-only handle to editor configuration.
    ///
    /// Per-window snapshot (an `Arc<Config>` clone in `WindowResources`); after
    /// `Editor::config_mut` it stays stale until `Editor::sync_windows_config` runs.
    pub fn config(&self) -> &crate::config::Config {
        &self.resources.config
    }

    /// This window's backend (local / devcontainer / remote) — owned by the
    /// window, never shared with another.
    pub fn authority(&self) -> &crate::services::authority::Authority {
        &self.authority
    }

    /// Allocate the next globally-unique `BufferId`.
    pub fn alloc_buffer_id(&self) -> BufferId {
        self.resources.buffer_id_alloc.next()
    }

    /// Set this window's status-bar message. Mirrors
    /// `Editor::set_status_message` — moved here so handlers on
    /// `impl Window` can post status without an `Editor` reference.
    /// Clears any plugin-supplied status (matches Editor behaviour).
    pub fn set_status_message(&mut self, message: String) {
        tracing::info!(target: "status", "{}", message);
        self.plugin_status_message = None;
        self.status_message = Some(message);
    }

    /// Clear this window's status-bar message.
    pub fn clear_status_message(&mut self) {
        self.status_message = None;
    }

    /// Resolve the effective (split, buffer) pair for the currently-
    /// focused target inside this window. Returned invariant: the split
    /// id is in `splits.1` (view_states), its `active_buffer` equals
    /// the returned buffer id, `self.buffers` contains the buffer id,
    /// and the split's `keyed_states` contains an entry for the buffer.
    ///
    /// Falls back to the outer split when a buffer-group panel is
    /// focused but any of those invariants doesn't hold for the inner
    /// leaf. Mirrors `Editor::effective_active_pair`.
    pub fn effective_active_pair(&self) -> (LeafId, BufferId) {
        let (mgr, vs_map) = self
            .buffers
            .splits()
            .expect("active window must have a populated split layout");
        let active_split = mgr.active_split();
        if let Some(vs) = vs_map.get(&active_split) {
            if vs.active_group_tab.is_some() {
                if let Some(inner_leaf) = vs.focused_group_leaf {
                    if let Some(inner_vs) = vs_map.get(&inner_leaf) {
                        let inner_buf = inner_vs.active_buffer;
                        if self.buffers.get(&inner_buf).is_some()
                            && inner_vs.keyed_states.contains_key(&inner_buf)
                        {
                            return (inner_leaf, inner_buf);
                        }
                    }
                }
            }
        }
        let outer_buf = mgr
            .active_buffer_id()
            .expect("Editor always has at least one buffer");
        // Validate against `self.buffers` — the group-tab branch above
        // already does this for its return; the outer fallback used to
        // skip the check and any caller that then did
        // `self.buffers.get(&active_buf).unwrap()` would panic. Issue
        // #1939: `set_pane_buffer` writes the leaf's `buffer_id` +
        // `vs.active_buffer` without touching `vs.open_buffers`, so
        // `clean_orphaned_buffers` (which filters by `buffer_tab_ids`)
        // can remove a buffer the split manager still points at.
        // When that happens, fall back to any live buffer and warn
        // loudly — the split manager pointer is stale until something
        // repairs it, and we want the underlying state corruption
        // visible in logs even though render itself no longer crashes.
        if self.buffers.get(&outer_buf).is_some() {
            (active_split, outer_buf)
        } else if let Some(any) = self.buffers.find_id(|_, _| true) {
            tracing::warn!(
                stale_buffer_id = ?outer_buf,
                fallback_buffer_id = ?any,
                active_split = ?active_split,
                "effective_active_pair: split manager's active leaf points at \
                 a BufferId missing from window.buffers (issue #1939). Falling \
                 back to any live buffer; the split tree is in an inconsistent \
                 state and should be repaired"
            );
            (active_split, any)
        } else {
            // `self.buffers` empty: a bigger invariant violation than
            // this helper can recover from. Preserve old behaviour so
            // the panic surfaces at the next `.unwrap()` site.
            tracing::error!(
                stale_buffer_id = ?outer_buf,
                active_split = ?active_split,
                "effective_active_pair: window.buffers is empty AND the split \
                 manager has a stale active buffer — no recovery possible, \
                 next render will panic"
            );
            (active_split, outer_buf)
        }
    }

    /// The id of the buffer currently focused in this window.
    #[inline]
    pub fn active_buffer(&self) -> BufferId {
        let (_, buf) = self.effective_active_pair();
        buf
    }

    /// Width available for tabs in this window. When the file explorer is
    /// visible the tabs row only spans the editor area; otherwise it spans
    /// the full terminal width.
    pub fn effective_tabs_width(&self) -> u16 {
        // Start from the chrome left after the editor-global dock, then
        // subtract the file explorer — same carve-out order as the
        // renderer and `editor_content_area`, so tab-scroll math matches
        // the width the tabs actually paint into when the dock is shown.
        let chrome = self.terminal_width.saturating_sub(self.dock_cols);
        if self.file_explorer_visible && self.file_explorer.is_some() {
            let explorer = self.file_explorer_width.to_cols(chrome);
            chrome.saturating_sub(explorer)
        } else {
            chrome
        }
    }

    /// The split id whose `SplitViewState` owns the currently-focused
    /// cursors/viewport for this window.
    #[inline]
    pub fn effective_active_split(&self) -> LeafId {
        let (split, _) = self.effective_active_pair();
        split
    }

    /// Read-only handle to this window's active buffer state. Panics
    /// if the active buffer is missing — the invariants on
    /// `effective_active_pair` guarantee it's present.
    pub fn active_state(&self) -> &crate::state::EditorState {
        let buf = self.active_buffer();
        self.buffers
            .get(&buf)
            .expect("active buffer must be present in window")
    }

    /// Mutable handle to this window's active buffer state.
    pub fn active_state_mut(&mut self) -> &mut crate::state::EditorState {
        let buf = self.active_buffer();
        self.buffers
            .get_mut(&buf)
            .expect("active buffer must be present in window")
    }

    /// Mutable handle to a specific buffer's editor state, if it is loaded in this window.
    pub fn buffer_state_mut(&mut self, id: BufferId) -> Option<&mut crate::state::EditorState> {
        self.buffers.get_mut(&id)
    }

    /// Read-only handle to a specific buffer's editor state, if it is loaded in this window.
    pub fn buffer_state(&self, id: BufferId) -> Option<&crate::state::EditorState> {
        self.buffers.get(&id)
    }

    /// Read-only cursor set for the active buffer in the active split.
    /// Group panels return their own cursors, not the outer split's
    /// stale ones.
    pub fn active_cursors(&self) -> &crate::model::cursor::Cursors {
        let split_id = self.effective_active_split();
        &self
            .buffers
            .splits()
            .expect("active window must have a populated split layout")
            .1
            .get(&split_id)
            .expect("active split must be in view-state map")
            .cursors
    }

    /// Mutable cursor set for the active buffer in the active split.
    pub fn active_cursors_mut(&mut self) -> &mut crate::model::cursor::Cursors {
        let split_id = self.effective_active_split();
        &mut self
            .buffers
            .splits_mut()
            .expect("active window must have a populated split layout")
            .1
            .get_mut(&split_id)
            .expect("active split must be in view-state map")
            .cursors
    }

    /// Read-only event log for the active buffer.
    pub fn active_event_log(&self) -> &crate::model::event::EventLog {
        let buf = self.active_buffer();
        self.event_logs
            .get(&buf)
            .expect("active buffer must have an event log")
    }

    /// Mutable event log for the active buffer.
    pub fn active_event_log_mut(&mut self) -> &mut crate::model::event::EventLog {
        let buf = self.active_buffer();
        self.event_logs
            .get_mut(&buf)
            .expect("active buffer must have an event log")
    }

    // ---- Preview-tab methods ----

    /// Promote a specific buffer from preview to permanent, if it is
    /// currently the preview. No-op otherwise.
    ///
    /// Escalating a preview is a commitment to the file, so this is where
    /// the `after_file_open` hook — deferred at preview-open time — finally
    /// fires. Because `self.preview` is the single source of truth, this
    /// can fire at most once per buffer: a directly-opened (already
    /// committed) buffer is never `self.preview`, and a promoted buffer is
    /// cleared from `self.preview` here so a later promote is a no-op.
    pub fn promote_buffer_from_preview(&mut self, buffer_id: BufferId) {
        if self.preview.map(|(_, id)| id) == Some(buffer_id) {
            self.preview = None;
            self.fire_deferred_after_file_open(buffer_id);
        }
    }

    /// Promote the active buffer from preview to permanent. Called on
    /// any buffer mutation so touching a preview buffer commits it.
    pub fn promote_active_buffer_from_preview(&mut self) {
        let id = self.active_buffer();
        self.promote_buffer_from_preview(id);
    }

    /// Promote the current preview, regardless of which buffer it
    /// points at. Used before layout changes (split, close-split,
    /// move-tab) where the preview invariant ("anchored to a specific
    /// split") would otherwise be broken by the operation itself.
    pub fn promote_current_preview(&mut self) {
        if let Some((_, id)) = self.preview.take() {
            self.fire_deferred_after_file_open(id);
        }
    }

    /// Promote the current preview if it belongs to a split other
    /// than `new_split`. Called from split-focus-change paths so
    /// that moving focus away from the preview's pane commits it.
    pub fn promote_preview_if_not_in_split(&mut self, new_split: LeafId) {
        if let Some((preview_split, _)) = self.preview {
            if preview_split != new_split {
                self.promote_current_preview();
            }
        }
    }

    /// Whether the given buffer is currently the preview (ephemeral) tab.
    /// Derived from `self.preview`, the single source of truth — at most one
    /// buffer is the preview editor-wide.
    pub fn is_buffer_preview(&self, buffer_id: BufferId) -> bool {
        self.preview.map(|(_, id)| id) == Some(buffer_id)
    }

    /// The (split, buffer) tuple of the current preview tab, if any.
    /// Intended for tests that verify preview anchoring semantics.
    pub fn current_preview(&self) -> Option<(LeafId, BufferId)> {
        self.preview
    }

    // ---- Terminal-buffer query helpers ----

    /// The [`TerminalBuffer`] record for a buffer, or `None` if it isn't a
    /// terminal buffer in this window. The single lookup the helpers below
    /// build on.
    pub fn terminal_buffer(&self, buffer_id: BufferId) -> Option<&TerminalBuffer> {
        self.terminal_buffers.get(&buffer_id)
    }

    pub fn terminal_buffer_mut(&mut self, buffer_id: BufferId) -> Option<&mut TerminalBuffer> {
        self.terminal_buffers.get_mut(&buffer_id)
    }

    /// Check if a buffer is a terminal buffer (in this window).
    pub fn is_terminal_buffer(&self, buffer_id: BufferId) -> bool {
        self.terminal_buffer(buffer_id).is_some()
    }

    /// Terminal buffers never line-wrap (see `resolve_line_wrap_for_buffer`):
    /// their content is column-formatted, and wrapping a large scrollback turns
    /// the scrollbar's visual-row index into an O(all-lines) scan every frame,
    /// freezing the UI (fresh#2608). Heal any per-buffer viewport a global
    /// line-wrap toggle (or restored state) left enabled — cheap enough to run
    /// each frame, and a no-op when the window has no terminals.
    pub(crate) fn enforce_terminal_no_wrap(&mut self) {
        let terminals = &self.terminal_buffers;
        if terminals.is_empty() {
            return;
        }
        let Some(vs_map) = self.buffers.split_view_states_mut() else {
            return;
        };
        for vs in vs_map.values_mut() {
            for (buffer_id, buffer_state) in vs.keyed_states.iter_mut() {
                if terminals.contains_key(buffer_id) {
                    buffer_state.viewport.line_wrap_enabled = false;
                }
            }
        }
    }

    /// Whether auto-revert (reload on external file change) applies to
    /// `buffer_id`. Auto-revert is a per-buffer property: it honours the
    /// buffer's metadata flag, and is always forced off for terminal-backed
    /// buffers. A terminal's backing file is append-streamed by the PTY
    /// reader every write; reverting it fights that stream and rebuilds the
    /// whole scrollback's visual-row index on each change (fresh#2608).
    pub fn buffer_auto_revert_enabled(&self, buffer_id: BufferId) -> bool {
        if self.is_terminal_buffer(buffer_id) {
            return false;
        }
        self.buffer_metadata
            .get(&buffer_id)
            .is_none_or(|meta| meta.auto_revert_enabled)
    }

    /// Get the terminal ID for a buffer (if it's a terminal buffer in
    /// this window).
    pub fn get_terminal_id(
        &self,
        buffer_id: BufferId,
    ) -> Option<crate::services::terminal::TerminalId> {
        self.terminal_buffer(buffer_id).map(|tb| tb.terminal_id)
    }

    /// Whether `split` is viewing terminal `buffer_id` in read-only scrollback.
    /// `false` for a live terminal split, and for any non-terminal buffer.
    /// The single read of the per-split live↔scrollback source of truth.
    pub fn split_terminal_scrollback(&self, split: LeafId, buffer_id: BufferId) -> bool {
        self.terminal_buffer(buffer_id)
            .is_some_and(|tb| tb.is_scrollback_in(split))
    }

    /// The only writer of a terminal split's live↔scrollback edge — the real
    /// transitions (Ctrl+Space, scroll-up, re-entry, process exit) all route
    /// here. `scrollback = true` drops `split` into read-only scrollback for
    /// this terminal; `false` returns it to the live PTY grid. No-op if
    /// `buffer_id` isn't a terminal buffer in this window.
    pub fn set_split_terminal_scrollback(
        &mut self,
        split: LeafId,
        buffer_id: BufferId,
        scrollback: bool,
    ) {
        if let Some(tb) = self.terminal_buffer_mut(buffer_id) {
            tb.set_scrollback_in(split, scrollback);
        }
    }

    /// Whether `split`'s scrollback visit for terminal `buffer_id` is
    /// implicit — entered by a selection gesture on the live grid rather than
    /// an explicit request. `false` for live splits, explicit scrollback, and
    /// non-terminal buffers.
    pub fn split_terminal_drag_scrollback(&self, split: LeafId, buffer_id: BufferId) -> bool {
        self.terminal_buffer(buffer_id)
            .is_some_and(|tb| tb.is_drag_scrollback_in(split))
    }

    /// Mark (or unmark) `split`'s scrollback visit for terminal `buffer_id`
    /// as implicit (drag-initiated). No-op for non-terminal buffers and, when
    /// marking, for splits not currently in scrollback.
    pub fn set_split_terminal_drag_scrollback(
        &mut self,
        split: LeafId,
        buffer_id: BufferId,
        drag: bool,
    ) {
        if let Some(tb) = self.terminal_buffer_mut(buffer_id) {
            tb.set_drag_scrollback_in(split, drag);
        }
    }

    /// Prune `split` from every terminal's scrollback set. Called when a split
    /// closes so no terminal keeps a stale (split, scrollback) edge — the
    /// per-split source of truth stays exactly the live layout.
    pub fn forget_split_terminal_modes(&mut self, split: LeafId) {
        for tb in self.terminal_buffers.values_mut() {
            tb.forget_split(split);
        }
    }

    /// Whether the focused split is a **live** terminal — i.e. keystrokes are
    /// forwarded to a PTY (the old "terminal mode").
    ///
    /// This is the focused-split projection of the per-split scrollback source
    /// of truth: [`Window::sync_terminal_mode_flags`] sets the `Terminal` key
    /// context iff the editor pane owns focus, the active buffer is a terminal,
    /// and the active split is not in that terminal's scrollback set — and drops
    /// it to `Normal` otherwise. So `key_context == Terminal` is exactly "the
    /// focused split is a live terminal." Rendering reads the per-split set
    /// directly (see `render_terminal_splits`); this accessor is for input,
    /// cursor, and status decisions that concern the focused split only.
    pub fn focused_terminal_live(&self) -> bool {
        self.key_context == crate::input::keybindings::KeyContext::Terminal
    }

    /// Clear the visual search overlays for the active buffer,
    /// preserving search state so F3/Shift+F3 still work.
    pub fn clear_search_overlays(&mut self) {
        let ns = self.search_namespace.clone();
        let state = self.active_state_mut();
        state.overlays.clear_namespace(&ns, &mut state.marker_list);
    }

    /// Clear all search highlights from the active buffer and reset
    /// search state.
    pub fn clear_search_highlights(&mut self) {
        self.clear_search_overlays();
        self.search_state = None;
    }

    /// List the languages with currently-running LSP server handles in
    /// this window. Wraps `LspManager::running_servers`.
    pub fn running_lsp_servers(&self) -> Vec<String> {
        self.lsp.running_servers()
    }

    /// Number of in-flight completion requests for this window.
    pub fn pending_completion_requests_count(&self) -> usize {
        self.pending_completion_requests.len()
    }

    /// Number of stored completion items currently visible in this
    /// window's completion popup.
    pub fn completion_items_count(&self) -> usize {
        self.completion_items.as_ref().map_or(0, |v| v.len())
    }

    /// Number of initialized (handshake-complete) LSP servers for
    /// `language` in this window.
    pub fn initialized_lsp_server_count(&self, language: &str) -> usize {
        self.lsp
            .get_handles(language)
            .iter()
            .filter(|sh| sh.capabilities.initialized)
            .count()
    }

    /// Number of LSP servers for `language` whose effective capabilities
    /// currently advertise `textDocument/completion`. This reflects
    /// dynamically-registered providers (`client/registerCapability`), not
    /// only the static `initialize` result, so tests can wait for a server
    /// like jdtls to register completion before triggering it.
    pub fn completion_capable_lsp_server_count(&self, language: &str) -> usize {
        self.lsp
            .get_handles(language)
            .iter()
            .filter(|sh| sh.capabilities.completion)
            .count()
    }

    /// Shutdown the LSP server for `language` in this window (marks it
    /// disabled until manual restart). Returns true if a server was
    /// shutdown, false if no server was running for that language.
    pub fn shutdown_lsp_server(&mut self, language: &str) -> bool {
        self.lsp.shutdown_server(language)
    }

    /// Enable event-log streaming to `path` for every buffer's event
    /// log in this window.
    pub fn enable_event_streaming<P: AsRef<std::path::Path>>(
        &mut self,
        path: P,
    ) -> anyhow::Result<()> {
        for event_log in self.event_logs.values_mut() {
            event_log.enable_streaming(&path)?;
        }
        Ok(())
    }

    /// Log a keystroke against the active buffer's event log. No-op if
    /// the active buffer has no log entry.
    pub fn log_keystroke(&mut self, key_code: &str, modifiers: &str) {
        let buffer_id = self.active_buffer();
        if let Some(event_log) = self.event_logs.get_mut(&buffer_id) {
            event_log.log_keystroke(key_code, modifiers);
        }
    }

    /// Check if LSP has any active progress tasks (e.g., indexing) in
    /// this window.
    pub fn has_active_lsp_progress(&self) -> bool {
        !self.lsp_progress.is_empty()
    }

    /// Snapshot of the current LSP progress entries for this window:
    /// `(token, title, message)` tuples.
    pub fn get_lsp_progress(&self) -> Vec<(String, String, Option<String>)> {
        self.lsp_progress
            .iter()
            .map(|(token, info)| (token.clone(), info.title.clone(), info.message.clone()))
            .collect()
    }

    /// Check if any LSP server for `language` is running in this
    /// window. Includes servers registered under another language whose
    /// scope accepts `language` (universal servers).
    pub fn is_lsp_server_ready(&self, language: &str) -> bool {
        use crate::services::async_bridge::LspServerStatus;
        self.lsp_server_statuses
            .iter()
            .any(|((lang, server_name), status)| {
                if !matches!(status, LspServerStatus::Running) {
                    return false;
                }
                if lang == language {
                    return true;
                }
                self.lsp
                    .server_scope(server_name)
                    .map(|scope| scope.accepts(language))
                    .unwrap_or(false)
            })
    }

    /// If the active leaf carries `SplitRole::UtilityDock`, move the
    /// active leaf back to the user's last regular editor leaf (or any
    /// non-dock leaf as a fallback). Called from the file-open path so
    /// that opening a file while a utility panel holds focus doesn't
    /// turn the dock into a tab strip for ordinary files.
    pub fn redirect_active_split_away_from_dock_if_needed(&mut self) {
        use crate::view::split::SplitRole;
        let Some((mgr, _)) = self.buffers.splits() else {
            return;
        };
        let active = mgr.active_split();
        if mgr.leaf_role(active) != Some(SplitRole::UtilityDock) {
            return;
        }
        let is_editor_leaf = |leaf| mgr.leaf_role(leaf) != Some(SplitRole::UtilityDock);
        let target = mgr.last_focused_where(is_editor_leaf).or_else(|| {
            mgr.root()
                .leaf_split_ids()
                .into_iter()
                .find(|leaf| is_editor_leaf(*leaf))
        });
        let Some(target) = target else {
            return;
        };
        if target == active {
            return;
        }
        self.split_manager_mut()
            .expect("active window must have a populated split layout")
            .set_active_split(target);
    }

    /// Restore per-file state (cursors, scroll, etc.) for a buffer in a
    /// specific split, lazily loaded from disk via
    /// `PersistedFileWorkspace::load`. No-op if there's no saved state
    /// for this path.
    pub fn restore_global_file_state(
        &mut self,
        buffer_id: BufferId,
        path: &std::path::Path,
        split_id: LeafId,
    ) {
        use crate::workspace::PersistedFileWorkspace;

        let file_state = match PersistedFileWorkspace::load(path) {
            Some(state) => state,
            None => return,
        };

        self.restore_buffer_state_in_split(buffer_id, split_id, &file_state);
    }

    /// Save file state when a buffer is closed (for per-file session
    /// persistence). Walks this window's splits to find one that has
    /// the buffer; no-op if no split contains it or the buffer isn't
    /// a real on-disk file.
    pub fn save_file_state_on_close(&self, buffer_id: BufferId) {
        use crate::workspace::{
            PersistedFileWorkspace, SerializedCursor, SerializedFileState, SerializedScroll,
        };

        let abs_path = match self.buffer_metadata.get(&buffer_id) {
            Some(metadata) => match metadata.file_path() {
                Some(path) => path.to_path_buf(),
                None => return,
            },
            None => return,
        };

        let view_state = self
            .buffers
            .splits()
            .expect("active window must have a populated split layout")
            .1
            .values()
            .find(|vs| vs.has_buffer(buffer_id));

        let view_state = match view_state {
            Some(vs) => vs,
            None => return,
        };

        let buf_state = match view_state.keyed_states.get(&buffer_id) {
            Some(bs) => bs,
            None => return,
        };

        let primary_cursor = buf_state.cursors.primary();
        let file_state = SerializedFileState {
            cursor: SerializedCursor {
                position: primary_cursor.position,
                anchor: primary_cursor.anchor,
                sticky_column: primary_cursor.sticky_column.unwrap_or(0),
            },
            additional_cursors: buf_state
                .cursors
                .iter()
                .skip(1)
                .map(|(_, cursor)| SerializedCursor {
                    position: cursor.position,
                    anchor: cursor.anchor,
                    sticky_column: cursor.sticky_column.unwrap_or(0),
                })
                .collect(),
            scroll: SerializedScroll {
                top_byte: buf_state.viewport.top_byte,
                top_view_line_offset: buf_state.viewport.top_view_line_offset,
                left_column: buf_state.viewport.left_column,
            },
            view_mode: Default::default(),
            compose_width: None,
            // Per-buffer line-number / line-wrap / virtual-space overrides are
            // workspace-scoped, not part of the cross-project global per-file state.
            line_numbers: None,
            line_wrap: None,
            virtual_space: None,
            plugin_state: std::collections::HashMap::new(),
            folds: Vec::new(),
        };

        PersistedFileWorkspace::save(&abs_path, file_state);
        tracing::debug!("Saved file state on close for {:?}", abs_path);
    }

    /// Remove a pending semantic-token request from this window's tracking maps.
    pub(crate) fn take_pending_semantic_token_request(
        &mut self,
        request_id: u64,
    ) -> Option<crate::app::SemanticTokenFullRequest> {
        if let Some(request) = self.pending_semantic_token_requests.remove(&request_id) {
            self.semantic_tokens_in_flight.remove(&request.buffer_id);
            Some(request)
        } else {
            None
        }
    }

    /// Remove a pending semantic-token range request from this window's tracking maps.
    pub(crate) fn take_pending_semantic_token_range_request(
        &mut self,
        request_id: u64,
    ) -> Option<crate::app::SemanticTokenRangeRequest> {
        if let Some(request) = self
            .pending_semantic_token_range_requests
            .remove(&request_id)
        {
            self.semantic_tokens_range_in_flight
                .remove(&request.buffer_id);
            Some(request)
        } else {
            None
        }
    }

    /// Move the cursor to a visible position within the current viewport.
    /// Called after scrollbar operations to ensure the cursor is in view.
    pub fn move_cursor_to_visible_area(&mut self, split_id: LeafId, buffer_id: BufferId) {
        let (top_byte, viewport_height) =
            if let Some(view_state) = self.buffers.splits().and_then(|(_, vs)| vs.get(&split_id)) {
                (
                    view_state.viewport.top_byte,
                    view_state.viewport.height as usize,
                )
            } else {
                return;
            };

        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            let buffer_len = state.buffer.len();

            let mut iter = state.buffer.line_iterator(top_byte, 80);
            let mut bottom_byte = buffer_len;

            for _ in 0..viewport_height {
                if let Some((pos, line)) = iter.next_line() {
                    bottom_byte = pos + line.len();
                } else {
                    bottom_byte = buffer_len;
                    break;
                }
            }

            if let Some(view_state) = self
                .split_view_states_mut()
                .and_then(|vs| vs.get_mut(&split_id))
            {
                let cursor_pos = view_state.cursors.primary().position;
                if cursor_pos < top_byte || cursor_pos > bottom_byte {
                    let cursor = view_state.cursors.primary_mut();
                    cursor.position = top_byte;
                    // Keep the existing sticky_column value so vertical
                    // navigation preserves column.
                }
            }
        }
    }

    /// Calculate the maximum allowed scroll position so the last line
    /// is always at the bottom unless the buffer is smaller than the
    /// viewport. Pure function on `Buffer`; lives on `Window` so the
    /// scrollbar helpers (also on `Window`) can reach it.
    pub fn calculate_max_scroll_position(
        buffer: &mut crate::model::buffer::Buffer,
        viewport_height: usize,
    ) -> usize {
        if viewport_height == 0 {
            return 0;
        }

        let buffer_len = buffer.len();
        if buffer_len == 0 {
            return 0;
        }

        let mut line_count = 0;
        let mut iter = buffer.line_iterator(0, 80);
        while iter.next_line().is_some() {
            line_count += 1;
        }

        if line_count <= viewport_height {
            return 0;
        }

        let scrollable_lines = line_count.saturating_sub(viewport_height);

        let mut iter = buffer.line_iterator(0, 80);
        let mut current_line = 0;
        let mut max_byte_pos = 0;

        while current_line < scrollable_lines {
            if let Some((pos, _content)) = iter.next_line() {
                max_byte_pos = pos;
                current_line += 1;
            } else {
                break;
            }
        }

        max_byte_pos
    }

    /// Find the split whose content or scrollbar area contains the
    /// screen cell `(col, row)`. Returns the split id and its buffer
    /// id, or `None` when the position falls outside every split's
    /// content rect and outside every scrollbar gutter.
    pub fn split_at_position(&self, col: u16, row: u16) -> Option<(LeafId, BufferId)> {
        for &(split_id, buffer_id, content_rect, scrollbar_rect, _, _) in
            &self.layout_cache.split_areas
        {
            let in_content = col >= content_rect.x
                && col < content_rect.x + content_rect.width
                && row >= content_rect.y
                && row < content_rect.y + content_rect.height;
            let in_scrollbar = scrollbar_rect.width > 0
                && scrollbar_rect.height > 0
                && col >= scrollbar_rect.x
                && col < scrollbar_rect.x + scrollbar_rect.width
                && row >= scrollbar_rect.y
                && row < scrollbar_rect.y + scrollbar_rect.height;
            if in_content || in_scrollbar {
                return Some((split_id, buffer_id));
            }
        }
        None
    }

    /// If a per-edit diagnostic-pull debounce has fired, send a fresh
    /// `textDocument/diagnostic` request to the language server for the
    /// scheduled buffer. Returns false because the new diagnostics arrive
    /// asynchronously — the response handler will trigger any redraw.
    pub fn check_diagnostic_pull_timer(&mut self) -> bool {
        let Some((buffer_id, trigger_time)) = self.scheduled_diagnostic_pull else {
            return false;
        };

        if std::time::Instant::now() < trigger_time {
            return false;
        }

        self.scheduled_diagnostic_pull = None;

        let Some(metadata) = self.buffer_metadata.get(&buffer_id) else {
            return false;
        };
        let Some(uri) = metadata.file_uri().cloned() else {
            return false;
        };
        let Some(language) = self.buffers.get(&buffer_id).map(|s| s.language.clone()) else {
            return false;
        };

        self.pull_diagnostics_for_uri(&language, &uri);

        false
    }

    /// Send a `textDocument/diagnostic` pull request for `uri` to **every**
    /// diagnostic-capable server for `language`, each with its own last
    /// `result_id`.
    ///
    /// `Diagnostics` is a merged feature, so when several servers pull-serve
    /// the same language (e.g. `ruff` + `ty` for Python) each one must be
    /// re-pulled — pulling only the first-listed server leaves the others'
    /// diagnostics (type errors, in the reported case) stale until restart
    /// (sinelaw/fresh#2615). Servers that never advertised `diagnosticProvider`
    /// drop the request on the async side, so fanning out to every eligible
    /// handle is safe.
    pub(crate) fn pull_diagnostics_for_uri(
        &mut self,
        language: &str,
        uri: &crate::app::types::LspUri,
    ) {
        // Snapshot the eligible server names first so we can read
        // `diagnostic_result_ids` and bump request ids without holding the
        // `&mut self.lsp` borrow that `handles_for_feature_mut` needs.
        let server_names: Vec<String> = self
            .lsp
            .handles_for_feature(language, crate::types::LspFeature::Diagnostics)
            .into_iter()
            .map(|sh| sh.name.clone())
            .collect();
        if server_names.is_empty() {
            return;
        }

        let mut plan: Vec<(String, u64, Option<String>)> = Vec::with_capacity(server_names.len());
        for name in server_names {
            let previous_result_id = self
                .diagnostic_result_ids
                .get(uri.as_str())
                .and_then(|m| m.get(&name))
                .cloned();
            let request_id = self.next_lsp_request_id;
            self.next_lsp_request_id += 1;
            plan.push((name, request_id, previous_result_id));
        }

        let lsp = &mut self.lsp;
        for sh in lsp.handles_for_feature_mut(language, crate::types::LspFeature::Diagnostics) {
            let Some((_, request_id, previous_result_id)) =
                plan.iter().find(|(n, _, _)| n.as_str() == sh.name.as_str())
            else {
                continue;
            };
            if let Err(e) = sh.handle.document_diagnostic(
                *request_id,
                uri.as_uri().clone(),
                previous_result_id.clone(),
            ) {
                tracing::debug!(
                    "Failed to pull diagnostics for {} from '{}': {}",
                    uri.as_str(),
                    sh.name,
                    e
                );
            } else {
                tracing::debug!(
                    "Pulling diagnostics for {} from '{}' (request_id={})",
                    uri.as_str(),
                    sh.name,
                    request_id
                );
            }
        }
    }

    /// Open a local file in this window (always uses local filesystem,
    /// not remote). Used for opening files like the warning log when
    /// the editor is connected to a remote server. Returns the buffer
    /// id and switches the active buffer to it (via
    /// [`Window::set_active_buffer`], so no plugin hook fires — the
    /// Editor caller is responsible for re-firing
    /// `buffer_activated` if the hook is required).
    pub fn open_local_file(&mut self, path: &std::path::Path) -> anyhow::Result<BufferId> {
        // Resolve relative paths against this window's root.
        let resolved_path = if path.is_relative() {
            self.root.join(path)
        } else {
            path.to_path_buf()
        };

        // Save user-visible path for language detection before canonicalizing.
        let display_path = resolved_path.clone();

        // Canonicalize the path.
        let canonical_path = resolved_path
            .canonicalize()
            .unwrap_or_else(|_| resolved_path.clone());
        let path = canonical_path.as_path();

        // Check if already open.
        let already_open = self
            .buffers
            .iter()
            .find(|(_, state)| state.buffer.file_path() == Some(path))
            .map(|(id, _)| *id);

        if let Some(id) = already_open {
            self.set_active_buffer(id);
            return Ok(id);
        }

        // Create new buffer.
        let buffer_id = self.alloc_buffer_id();

        // Load from canonical path (for I/O and dedup), detect language from
        // display path (for glob pattern matching against user-visible names).
        let buffer = crate::model::buffer::Buffer::load_from_file(
            &canonical_path,
            self.config().editor.large_file_threshold_bytes as usize,
            std::sync::Arc::clone(&self.resources.local_filesystem),
        )?;
        let first_line = buffer.first_line_lossy();
        let detected =
            crate::primitives::detected_language::DetectedLanguage::from_path_with_fallback(
                &display_path,
                first_line.as_deref(),
                &self.resources.grammar_registry,
                &self.config().languages,
                self.config().default_language.as_deref(),
            );
        let state = crate::state::EditorState::from_buffer_with_language(buffer, detected);

        self.buffers.insert(buffer_id, state);
        self.event_logs
            .insert(buffer_id, crate::model::event::EventLog::new());

        // Create metadata.
        let metadata = crate::app::types::BufferMetadata::with_file(
            path.to_path_buf(),
            &display_path,
            &self.root,
            self.authority().path_translation.as_ref(),
            self.config().editor.auto_read_only,
        );
        self.buffer_metadata.insert(buffer_id, metadata);

        // Add to preferred split's tabs (avoids labeled splits like sidebars).
        let target_split = self.preferred_split_for_file();
        let line_wrap = self.resolve_line_wrap_for_buffer(buffer_id);
        let wrap_column = self.resolve_wrap_column_for_buffer(buffer_id);
        // Snapshot config values before taking the mutable view-states borrow
        // so the closure body doesn't have to re-borrow `self`.
        let cfg = self.config().editor.clone();
        if let Some(view_state) = self
            .split_view_states_mut()
            .expect("active window must have a populated split layout")
            .get_mut(&target_split)
        {
            view_state.add_buffer(buffer_id);
            let buf_state = view_state.ensure_buffer_state(buffer_id);
            buf_state.apply_config_defaults(crate::view::split::ViewConfigDefaults {
                line_numbers: cfg.line_numbers,
                highlight_current_line: cfg.highlight_current_line,
                line_wrap,
                wrap_indent: cfg.wrap_indent,
                wrap_column,
                rulers: cfg.rulers,
                scroll_offset: cfg.scroll_offset,
            });
        }

        self.set_active_buffer(buffer_id);

        let display_name = path.display().to_string();
        self.set_status_message(rust_i18n::t!("buffer.opened", name = display_name).to_string());

        Ok(buffer_id)
    }

    /// Mark a buffer in this window as read-only (or writable), keeping
    /// the per-buffer metadata `read_only` flag and the editor state's
    /// `editing_disabled` flag in sync.
    pub fn mark_buffer_read_only(&mut self, buffer_id: BufferId, read_only: bool) {
        if let Some(metadata) = self.buffer_metadata.get_mut(&buffer_id) {
            metadata.read_only = read_only;
        }
        if let Some(state) = self.buffers.get_mut(&buffer_id) {
            state.editing_disabled = read_only;
        }
    }

    /// Clear all warning indicators for this window (general + LSP) and
    /// post a "Warnings cleared" status message.
    pub fn clear_warnings(&mut self) {
        self.warning_domains.general.clear();
        self.warning_domains.lsp.clear();
        self.set_status_message("Warnings cleared".to_string());
    }

    /// Recompute the LSP warning-domain level for this window from its
    /// `lsp_server_statuses` map. Called whenever a server transitions
    /// state.
    pub fn update_lsp_warning_domain(&mut self) {
        // Clone to release the immutable borrow before mutating warning_domains.
        let statuses = self.lsp_server_statuses.clone();
        self.warning_domains.lsp.update_from_statuses(&statuses);
    }

    /// Check if semantic highlight debounce timer has expired for any
    /// buffer in this window. Returns true if a redraw is needed because
    /// the debounce period has elapsed and semantic highlights need to
    /// be recomputed.
    pub fn check_semantic_highlight_timer(&self) -> bool {
        self.buffers.any_needs_semantic_redraw()
    }

    /// If an active search has placed the cursor inside a match, return that
    /// match's byte range.  Used by Ctrl-D ("Add cursor at next match") so a
    /// substring search drives the selection — instead of expanding to the
    /// whole word — when the user presses Ctrl-D right after searching
    /// (issue #1697).
    pub fn search_match_at_primary_cursor(&self) -> Option<std::ops::Range<usize>> {
        let search_state = self.search_state.as_ref()?;
        let pos = self.active_cursors().primary().position;
        let idx = match search_state.matches.binary_search(&pos) {
            Ok(i) => i,
            Err(0) => return None,
            Err(i) => i - 1,
        };
        let start = search_state.matches[idx];
        let len = *search_state.match_lengths.get(idx)?;
        if pos < start + len {
            Some(start..start + len)
        } else {
            None
        }
    }

    /// Update search highlights in the visible viewport for the active
    /// buffer. Caller passes theme colors as parameters because `theme`
    /// is editor-global (not yet on `Window.resources`).
    pub fn update_search_highlights(
        &mut self,
        query: &str,
        search_fg: ratatui::style::Color,
        search_bg: ratatui::style::Color,
    ) {
        if query.is_empty() {
            self.clear_search_highlights();
            return;
        }

        let ns = self.search_namespace.clone();

        // Share the one search-regex builder so highlight anchoring (`^`/`$`
        // line-matching) stays consistent with the actual search results.
        let regex = match self.build_search_regex(query) {
            Ok(r) => r,
            Err(_) => {
                self.clear_search_highlights();
                return;
            }
        };

        let active_split = self.effective_active_split();
        let (top_byte, visible_height) = self
            .buffers
            .splits()
            .expect("active window must have a populated split layout")
            .1
            .get(&active_split)
            .map(|vs| (vs.viewport.top_byte, vs.viewport.height.saturating_sub(2)))
            .unwrap_or((0, 20));

        let state = self.active_state_mut();
        state.overlays.clear_namespace(&ns, &mut state.marker_list);

        let visible_start = top_byte;
        let mut visible_end = top_byte;
        {
            let mut line_iter = state.buffer.line_iterator(top_byte, 80);
            for _ in 0..visible_height {
                if let Some((line_start, line_content)) = line_iter.next_line() {
                    visible_end = line_start + line_content.len();
                } else {
                    break;
                }
            }
        }
        visible_end = visible_end.min(state.buffer.len());
        let visible_text = state.get_text_range(visible_start, visible_end);

        for mat in regex.find_iter(&visible_text) {
            let absolute_pos = visible_start + mat.start();
            let match_len = mat.end() - mat.start();
            let search_style = ratatui::style::Style::default().fg(search_fg).bg(search_bg);
            let overlay = crate::view::overlay::Overlay::with_namespace_fixed_end(
                &mut state.marker_list,
                absolute_pos..(absolute_pos + match_len),
                crate::view::overlay::OverlayFace::Style {
                    style: search_style,
                },
                ns.clone(),
            )
            .with_priority_value(10);
            state.overlays.add(overlay);
        }
    }

    /// Re-evaluate committed search highlights around an edited region.
    ///
    /// Search-match overlays are anchored by markers that merely *track*
    /// byte positions through edits; they never re-check whether the text
    /// they cover still matches the query. So editing inside a highlighted
    /// match (or typing against its boundary, which can break a `\b`
    /// whole-word rule) would leave a stale highlight on text that no
    /// longer matches. This recomputes matches on just the line(s) touched
    /// by the edit and swaps the search overlays in that span, so highlights
    /// drop and appear exactly where the text starts/stops matching.
    ///
    /// `edit_start` / `edit_new_len` are in post-edit byte coordinates (for
    /// a deletion, `edit_new_len` is 0). Bounded to the affected lines to
    /// keep it viewport-localized rather than a full-buffer rescan.
    pub fn reevaluate_search_overlays_around(
        &mut self,
        edit_start: usize,
        edit_new_len: usize,
        search_fg: ratatui::style::Color,
        search_bg: ratatui::style::Color,
    ) {
        let query = match self.search_state.as_ref() {
            Some(ss) if !ss.query.is_empty() => ss.query.clone(),
            _ => return,
        };

        let ns = self.search_namespace.clone();

        // Share the one search-regex builder so re-highlighting after an edit
        // keeps `^`/`$` line-anchoring consistent with the search results.
        let regex = match self.build_search_regex(&query) {
            Ok(r) => r,
            Err(_) => return,
        };

        let state = self.active_state_mut();
        let buf_len = state.buffer.len();
        let edit_end = edit_start.saturating_add(edit_new_len).min(buf_len);

        // Expand the edited byte span to the full line(s) it touches so that
        // word-boundary context on either side of the edit is included.
        let start_line = state.buffer.get_line_number(edit_start.min(buf_len));
        let end_line = state.buffer.get_line_number(edit_end);
        let win_start = state.buffer.line_start_offset(start_line).unwrap_or(0);
        let win_end = state
            .buffer
            .line_start_offset(end_line + 1)
            .unwrap_or(buf_len)
            .min(buf_len);

        let text = state.get_text_range(win_start, win_end);

        let mut new_overlays = Vec::new();
        for mat in regex.find_iter(&text) {
            let absolute_pos = win_start + mat.start();
            let match_len = mat.end() - mat.start();
            let search_style = ratatui::style::Style::default().fg(search_fg).bg(search_bg);
            new_overlays.push(
                crate::view::overlay::Overlay::with_namespace_fixed_end(
                    &mut state.marker_list,
                    absolute_pos..(absolute_pos + match_len),
                    crate::view::overlay::OverlayFace::Style {
                        style: search_style,
                    },
                    ns.clone(),
                )
                .with_priority_value(10),
            );
        }

        state.overlays.replace_range_in_namespace(
            &ns,
            &(win_start..win_end),
            new_overlays,
            &mut state.marker_list,
        );
    }

    // ---- File-explorer leaf delegators ----

    /// Whether this window's file-explorer panel is visible.
    pub fn file_explorer_is_visible(&self) -> bool {
        self.file_explorer_visible && self.file_explorer.is_some()
    }

    /// Extend the file-explorer selection upward.
    pub fn file_explorer_extend_selection_up(&mut self) {
        if let Some(explorer) = self.file_explorer.as_mut() {
            explorer.extend_selection_up();
        }
    }

    /// Extend the file-explorer selection downward.
    pub fn file_explorer_extend_selection_down(&mut self) {
        if let Some(explorer) = self.file_explorer.as_mut() {
            explorer.extend_selection_down();
        }
    }

    /// Toggle the selection state of the focused file-explorer entry.
    pub fn file_explorer_toggle_select(&mut self) {
        if let Some(explorer) = self.file_explorer.as_mut() {
            explorer.toggle_select();
        }
    }

    /// Select every visible entry in the file explorer.
    pub fn file_explorer_select_all(&mut self) {
        if let Some(explorer) = self.file_explorer.as_mut() {
            explorer.select_all();
        }
    }

    /// Push a character onto the file-explorer search filter.
    pub fn file_explorer_search_push_char(&mut self, c: char) {
        if let Some(explorer) = self.file_explorer.as_mut() {
            explorer.search_push_char(c);
            explorer.update_scroll_for_selection();
        }
    }

    /// Pop the last character from the file-explorer search filter.
    pub fn file_explorer_search_pop_char(&mut self) {
        if let Some(explorer) = self.file_explorer.as_mut() {
            explorer.search_pop_char();
            explorer.update_scroll_for_selection();
        }
    }

    // ---- LSP scheduling helpers ----

    /// Schedule a folding-range refresh for a buffer (debounced). The
    /// debounce window timestamp is stored on the window's per-buffer
    /// folding-ranges debounce map.
    pub fn schedule_folding_ranges_refresh(&mut self, buffer_id: BufferId) {
        const FOLDING_RANGES_DEBOUNCE_MS: u64 = 300;
        let next_time = std::time::Instant::now()
            + std::time::Duration::from_millis(FOLDING_RANGES_DEBOUNCE_MS);
        self.folding_ranges_debounce.insert(buffer_id, next_time);
    }

    /// Schedule a full semantic-tokens refresh for a buffer (debounced).
    /// No-op when `enable_semantic_tokens_full` is off in the active
    /// config.
    pub fn schedule_semantic_tokens_full_refresh(&mut self, buffer_id: BufferId) {
        const SEMANTIC_TOKENS_FULL_DEBOUNCE_MS: u64 = 500;
        if !self.resources.config.editor.enable_semantic_tokens_full {
            return;
        }
        let next_time = std::time::Instant::now()
            + std::time::Duration::from_millis(SEMANTIC_TOKENS_FULL_DEBOUNCE_MS);
        self.semantic_tokens_full_debounce
            .insert(buffer_id, next_time);
    }

    /// Forward incremental LSP `didChange` notifications for `buffer_id`
    /// to every server registered for the buffer's language. Sends
    /// `didOpen` first when a server hasn't yet seen this buffer, and
    /// reschedules diagnostic / inlay-hint pulls.
    ///
    /// Pure per-window operation: every piece of state it touches
    /// (`buffer_metadata`, `buffers`, the LSP manager, debounce maps)
    /// lives on `Window`. Editor-side wrappers exist only as forwarding
    /// shims for legacy call sites.
    pub(crate) fn send_lsp_changes_for_buffer(
        &mut self,
        buffer_id: BufferId,
        changes: Vec<lsp_types::TextDocumentContentChangeEvent>,
    ) {
        const INLAY_HINTS_DEBOUNCE_MS: u64 = 500;

        if changes.is_empty() {
            return;
        }

        let metadata = match self.buffer_metadata.get(&buffer_id) {
            Some(m) => m,
            None => {
                tracing::debug!(
                    "send_lsp_changes_for_buffer: no metadata for buffer {:?}",
                    buffer_id
                );
                return;
            }
        };

        if !metadata.lsp_enabled {
            tracing::debug!("send_lsp_changes_for_buffer: LSP disabled for this buffer");
            return;
        }

        let uri = match metadata.file_uri() {
            Some(u) => u.clone(),
            None => {
                tracing::debug!(
                    "send_lsp_changes_for_buffer: no URI for buffer (not a file or URI creation failed)"
                );
                return;
            }
        };
        let file_path = metadata.file_path().cloned();

        let language = match self.buffers.get(&buffer_id).map(|s| s.language.clone()) {
            Some(l) => l,
            None => {
                tracing::debug!(
                    "send_lsp_changes_for_buffer: no buffer state for {:?}",
                    buffer_id
                );
                return;
            }
        };

        tracing::trace!(
            "send_lsp_changes_for_buffer: sending {} changes to {} in single didChange notification",
            changes.len(),
            uri.as_str()
        );

        use crate::services::lsp::manager::LspSpawnResult;
        let lsp = &mut self.lsp;

        if lsp.try_spawn(&language, file_path.as_deref()) != LspSpawnResult::Spawned {
            tracing::debug!(
                "send_lsp_changes_for_buffer: LSP not running for {} (auto_start disabled)",
                language
            );
            return;
        }

        let handles_needing_open: Vec<_> = {
            let Some(metadata) = self.buffer_metadata.get(&buffer_id) else {
                return;
            };
            lsp.get_handles(&language)
                .into_iter()
                .filter(|sh| !metadata.lsp_opened_with.contains(&sh.handle.id()))
                .map(|sh| (sh.name.clone(), sh.handle.id()))
                .collect()
        };

        if !handles_needing_open.is_empty() {
            let text = match self
                .buffers
                .get(&buffer_id)
                .and_then(|s| s.buffer.to_string())
            {
                Some(t) => t,
                None => {
                    tracing::debug!(
                        "send_lsp_changes_for_buffer: buffer text not available for didOpen"
                    );
                    return;
                }
            };

            let lsp = &mut self.lsp;
            for sh in lsp.get_handles_mut(&language) {
                if handles_needing_open
                    .iter()
                    .any(|(_, id)| *id == sh.handle.id())
                {
                    if let Err(e) =
                        sh.handle
                            .did_open(uri.as_uri().clone(), text.clone(), language.clone())
                    {
                        tracing::warn!(
                            "Failed to send didOpen to '{}' before didChange: {}",
                            sh.name,
                            e
                        );
                    } else {
                        tracing::debug!(
                            "Sent didOpen for {} to LSP handle '{}' before didChange",
                            uri.as_str(),
                            sh.name
                        );
                    }
                }
            }

            if let Some(metadata) = self.buffer_metadata.get_mut(&buffer_id) {
                for (_, handle_id) in &handles_needing_open {
                    metadata.lsp_opened_with.insert(*handle_id);
                }
            }

            // didOpen already contains the full current buffer content, so we must
            // NOT also send didChange (which carries pre-edit incremental changes).
            // Sending both would corrupt the server's view of the document.
            return;
        }

        let lsp = &mut self.lsp;
        let mut any_sent = false;
        for sh in lsp.get_handles_mut(&language) {
            if let Err(e) = sh.handle.did_change(uri.as_uri().clone(), changes.clone()) {
                tracing::warn!("Failed to send didChange to '{}': {}", sh.name, e);
            } else {
                any_sent = true;
            }
        }

        // Re-derive the merged diagnostics at the buffer's current version so
        // hover, the diagnostics panel, and plugin snapshots track the text
        // between server re-publishes. A server (e.g. rust-analyzer's cargo
        // checkOnSave) only re-publishes on save; the diagnostic overlays ride
        // their markers in the meantime, and this keeps the raw readers aligned
        // with them by mapping the anchors forward through `CoordMap` — no
        // dependency on the LSP change payload, so it covers every edit kind
        // including bulk edits and undo/redo (#2602).
        self.recompute_merged_diagnostics(uri.as_str());

        if any_sent {
            tracing::trace!("Successfully sent batched didChange to LSP");

            if let Some(state) = self.buffers.get(&buffer_id) {
                if let Some(path) = state.buffer.file_path() {
                    crate::services::lsp::diagnostics::invalidate_cache_for_file(
                        &path.to_string_lossy(),
                    );
                }
            }

            self.scheduled_diagnostic_pull = Some((
                buffer_id,
                std::time::Instant::now() + std::time::Duration::from_millis(1000),
            ));

            if self.resources.config.editor.enable_inlay_hints {
                self.scheduled_inlay_hints_request = Some((
                    buffer_id,
                    std::time::Instant::now()
                        + std::time::Duration::from_millis(INLAY_HINTS_DEBOUNCE_MS),
                ));
            }
        }
    }

    /// The open buffer showing `uri`, if any. Diagnostics are keyed by URI but
    /// only an open buffer has a `CoordMap`/marker tree to map positions with.
    fn buffer_id_for_uri(&self, uri: &str) -> Option<BufferId> {
        self.buffer_metadata
            .iter()
            .find_map(|(id, meta)| meta.file_uri().filter(|u| u.as_str() == uri).map(|_| *id))
    }

    /// Rebuild `stored_diagnostics[uri]` from the anchored push/pull stores,
    /// mapping every diagnostic forward to the buffer's current version via
    /// `CoordMap` (the same shift the overlays' markers apply). This is the
    /// single materialisation point for the merged view: it runs on every
    /// publish/pull and on every edit, so the raw readers (hover, panel, plugin
    /// snapshots) never observe a stale position. Overlays are left to ride
    /// their markers and are only rebuilt on a real publish (#2602).
    ///
    /// Returns the merged diagnostics (current positions) for the caller to
    /// apply as overlays; empty when the URI has none.
    pub(crate) fn recompute_merged_diagnostics(&mut self, uri: &str) -> Vec<lsp_types::Diagnostic> {
        let state = self
            .buffer_id_for_uri(uri)
            .and_then(|id| self.buffers.get(&id));

        let mut merged = Vec::new();
        if let Some(server_map) = self.stored_push_diagnostics.get(uri) {
            for entries in server_map.values() {
                merged.extend(entries.iter().map(|e| e.at_current_version(state)));
            }
        }
        if let Some(server_map) = self.stored_pull_diagnostics.get(uri) {
            for entries in server_map.values() {
                merged.extend(entries.iter().map(|e| e.at_current_version(state)));
            }
        }

        let store = Arc::make_mut(&mut self.stored_diagnostics);
        if merged.is_empty() {
            store.remove(uri);
        } else {
            store.insert(uri.to_string(), merged.clone());
        }
        merged
    }

    /// Invalidate cached layouts and view transforms for every split
    /// that displays `buffer_id`. Pure window-state mutation: walks
    /// the window's split tree and view-state map.
    pub fn invalidate_layouts_for_buffer(&mut self, buffer_id: BufferId) {
        let Some((mgr, vs_map)) = self.buffers.splits_mut() else {
            return;
        };
        let splits_for_buffer = mgr.splits_for_buffer(buffer_id);
        for split_id in splits_for_buffer {
            if let Some(view_state) = vs_map.get_mut(&split_id) {
                view_state.invalidate_layout();
                view_state.view_transform = None;
                view_state.view_transform_stale = true;
            }
        }
    }

    /// Adjust cursors in other splits that share the same buffer after
    /// an edit. The split that originated the event already had its
    /// cursors moved by `BufferState::apply`; this method walks every
    /// other split displaying the same buffer and shifts (or, for a
    /// `BulkEdit`, resets) their cursors so they don't dangle past
    /// freshly-deleted text.
    pub fn adjust_other_split_cursors_for_event(&mut self, event: &Event) {
        let current_buffer_id = self.active_buffer();
        let buffer_len = self
            .buffers
            .get(&current_buffer_id)
            .map(|s| s.buffer.len())
            .unwrap_or(0);
        let Some((mgr, vs_map)) = self.buffers.splits_mut() else {
            return;
        };
        let current_split_id = mgr.active_split();
        let splits_for_buffer = mgr.splits_for_buffer(current_buffer_id);

        if let Event::BulkEdit { new_cursors, .. } = event {
            for split_id in splits_for_buffer {
                if split_id == current_split_id {
                    continue;
                }
                if let Some(view_state) = vs_map.get_mut(&split_id) {
                    if let Some((_, pos, _)) = new_cursors.first() {
                        let new_pos = (*pos).min(buffer_len);
                        view_state.cursors.primary_mut().position = new_pos;
                        view_state.cursors.primary_mut().anchor = None;
                    }
                }
            }
            return;
        }

        let adjustments: Vec<(usize, usize, usize)> = match event {
            Event::Insert { position, text, .. } => {
                vec![(*position, 0, text.len())]
            }
            Event::Delete { range, .. } => {
                vec![(range.start, range.len(), 0)]
            }
            Event::Batch { events, .. } => events
                .iter()
                .filter_map(|e| match e {
                    Event::Insert { position, text, .. } => Some((*position, 0, text.len())),
                    Event::Delete { range, .. } => Some((range.start, range.len(), 0)),
                    _ => None,
                })
                .collect(),
            _ => Vec::new(),
        };

        if adjustments.is_empty() {
            return;
        }

        for split_id in splits_for_buffer {
            if split_id == current_split_id {
                continue;
            }
            if let Some(view_state) = vs_map.get_mut(&split_id) {
                for (edit_pos, old_len, new_len) in &adjustments {
                    view_state
                        .cursors
                        .adjust_for_edit(*edit_pos, *old_len, *new_len);
                }
            }
        }
    }

    /// Handle scroll events using the active split's viewport.
    ///
    /// View events (like `Scroll`) target SplitViewState rather than
    /// EditorState so scroll limits are correct when view transforms
    /// inject extra rows.
    pub(crate) fn handle_scroll_event(&mut self, line_offset: isize) {
        use crate::view::ui::view_pipeline::ViewLineIterator;

        let Some((mgr, _)) = self.buffers.splits() else {
            return;
        };
        let active_split = mgr.active_split();

        if let Some(group) = self
            .scroll_sync_manager
            .find_group_for_split(active_split.into())
        {
            let left = group.left_split;
            let right = group.right_split;
            if let Some(vs_map) = self.split_view_states_mut() {
                if let Some(vs) = vs_map.get_mut(&LeafId(left)) {
                    vs.viewport.set_skip_ensure_visible();
                }
                if let Some(vs) = vs_map.get_mut(&LeafId(right)) {
                    vs.viewport.set_skip_ensure_visible();
                }
            }
        }

        let (mgr, vs_map) = self.buffers.splits().expect("splits checked above");
        let sync_group = vs_map.get(&active_split).and_then(|vs| vs.sync_group);
        let splits_to_scroll = if let Some(group_id) = sync_group {
            mgr.get_splits_in_group(group_id, vs_map)
        } else {
            vec![active_split]
        };

        let tab_size = self.resources.config.editor.tab_size;
        for split_id in splits_to_scroll {
            let (mgr, vs_map) = self.buffers.splits().expect("splits checked above");
            let Some(buffer_id) = mgr.buffer_for_split(split_id) else {
                continue;
            };

            let view_transform_tokens = vs_map
                .get(&split_id)
                .and_then(|vs| vs.view_transform.as_ref())
                .map(|vt| vt.tokens.clone());

            self.buffers
                .with_buffer_and_split(buffer_id, split_id, |state, view_state| {
                    let soft_breaks = state.collect_soft_break_positions();
                    let virtual_lines = state.collect_virtual_line_positions();
                    let buffer = &mut state.buffer;
                    if let Some(tokens) = view_transform_tokens {
                        let view_lines: Vec<_> =
                            ViewLineIterator::new(&tokens, false, false, tab_size, false).collect();
                        view_state
                            .viewport
                            .scroll_view_lines(&view_lines, line_offset);
                    } else if line_offset > 0 {
                        view_state.viewport.scroll_down(
                            buffer,
                            &soft_breaks,
                            &virtual_lines,
                            line_offset as usize,
                        );
                    } else {
                        view_state.viewport.scroll_up(
                            buffer,
                            &soft_breaks,
                            &virtual_lines,
                            line_offset.unsigned_abs(),
                        );
                    }
                    view_state.viewport.set_skip_ensure_visible();
                });
        }
    }

    /// Handle a `SetViewport` event using the active split's viewport.
    pub(crate) fn handle_set_viewport_event(&mut self, top_line: usize) {
        let Some((mgr, _)) = self.buffers.splits() else {
            return;
        };
        let active_split = mgr.active_split();

        if self
            .scroll_sync_manager
            .is_split_synced(active_split.into())
        {
            if let Some(group) = self
                .scroll_sync_manager
                .find_group_for_split_mut(active_split.into())
            {
                let scroll_line = if group.is_left_split(active_split.into()) {
                    top_line
                } else {
                    group.right_to_left_line(top_line)
                };
                group.set_scroll_line(scroll_line);
            }

            let (left, right) = match self
                .scroll_sync_manager
                .find_group_for_split(active_split.into())
            {
                Some(group) => (group.left_split, group.right_split),
                None => return,
            };
            if let Some(vs_map) = self.split_view_states_mut() {
                if let Some(vs) = vs_map.get_mut(&LeafId(left)) {
                    vs.viewport.set_skip_ensure_visible();
                }
                if let Some(vs) = vs_map.get_mut(&LeafId(right)) {
                    vs.viewport.set_skip_ensure_visible();
                }
            }
            return;
        }

        let (mgr, vs_map) = self.buffers.splits().expect("splits checked above");
        let sync_group = vs_map.get(&active_split).and_then(|vs| vs.sync_group);
        let splits_to_scroll = if let Some(group_id) = sync_group {
            mgr.get_splits_in_group(group_id, vs_map)
        } else {
            vec![active_split]
        };

        for split_id in splits_to_scroll {
            let (mgr, _) = self.buffers.splits().expect("splits checked above");
            let Some(buffer_id) = mgr.buffer_for_split(split_id) else {
                continue;
            };

            self.buffers
                .with_buffer_and_split(buffer_id, split_id, |state, view_state| {
                    view_state.viewport.scroll_to(&mut state.buffer, top_line);
                    view_state.viewport.set_skip_ensure_visible();
                });
        }
    }

    /// Handle a `Recenter` event using the active split's viewport.
    pub(crate) fn handle_recenter_event(&mut self) {
        let Some((mgr, vs_map)) = self.buffers.splits() else {
            return;
        };
        let active_split = mgr.active_split();

        let sync_group = vs_map.get(&active_split).and_then(|vs| vs.sync_group);
        let splits_to_recenter = if let Some(group_id) = sync_group {
            mgr.get_splits_in_group(group_id, vs_map)
        } else {
            vec![active_split]
        };

        for split_id in splits_to_recenter {
            let (mgr, _) = self.buffers.splits().expect("splits checked above");
            let Some(buffer_id) = mgr.buffer_for_split(split_id) else {
                continue;
            };

            self.buffers
                .with_buffer_and_split(buffer_id, split_id, |state, view_state| {
                    let buffer = &mut state.buffer;
                    let cursor_pos = view_state.cursors.primary().position;
                    // `center_on_position` counts real visual rows, so a
                    // recenter in a wrapped document doesn't under-scroll
                    // and leave the cursor below the viewport (each logical
                    // line above the cursor can span many rows).
                    view_state.viewport.center_on_position(buffer, cursor_pos);
                    view_state.viewport.set_skip_ensure_visible();
                });
        }
    }

    /// Atomically update both sides of the pane-buffer invariant for a
    /// given leaf split: the split tree's stored buffer AND the matching
    /// `SplitViewState.active_buffer` / `keyed_states` map.
    ///
    /// This is the one place that's allowed to change "which buffer is
    /// shown in pane `leaf`". The two stores can never drift if every
    /// caller goes through here (issue #1620).
    ///
    /// If the leaf has no `SplitViewState` yet (e.g. mid-session-restore,
    /// when the SVS is registered later), the tree is still updated and
    /// the SVS sync is skipped — the caller is responsible for ensuring
    /// the SVS exists by the time any input is routed.
    pub fn set_pane_buffer(&mut self, leaf: LeafId, buffer_id: BufferId) {
        let (mgr, vs_map) = self
            .buffers
            .splits_mut()
            .expect("active window must have a populated split layout");
        mgr.set_split_buffer(leaf, buffer_id);
        if let Some(view_state) = vs_map.get_mut(&leaf) {
            view_state.switch_buffer(buffer_id);
            view_state.add_buffer(buffer_id);
        }
    }
}

// Label-defaulting unit tests (`empty_label_defaults_to_root_basename`,
// `explicit_label_is_kept`, `empty_label_with_rootless_path_falls_back_to_main`)
// were removed when `Window::new` started taking a `WindowResources`
// argument — stubbing every editor-global service for a 3-line label
// assertion isn't worth the maintenance, and the same behaviour is
// already exercised by every `EditorTestHarness::create` path that
// names a window.
