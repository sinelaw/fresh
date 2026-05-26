//! Editor-global resources shared into every `Window` by `Arc` clone.
//!
//! ## Why this exists
//!
//! `Window` was originally constructed with only its own per-project state
//! (buffers, splits, file_explorer, lsp, …) and reached back to `Editor`
//! for everything else (`config`, `theme`, `plugin_manager`, registries,
//! filesystem authority). The reach-back forced almost every handler to
//! live on `impl Editor` rather than `impl Window`, because the body
//! needed `&self` *as Editor* to read those resources.
//!
//! `WindowResources` flips that: every editor-global service a handler
//! could plausibly need is shared into `Window` as an `Arc<…>` clone (or
//! `Clone`-by-value for handles that already carry their own `Arc`s,
//! like `Authority`). A `Window` method now has direct access to
//! `self.config.editor.line_wrap`, `self.authority.path_translation`,
//! etc., without any `Editor` reference. Methods that previously had to
//! sit on `impl Editor` to read these can move to `impl Window`.
//!
//! ## What stays on `Editor` (not in `WindowResources`)
//!
//! - `next_buffer_id` allocator (separate concept — see
//!   [`BufferIdAllocator`])
//! - `theme: Theme` — direct value (not `Arc`); pending Tier-2 migration
//! - `clipboard: Clipboard` — owned value; pending Tier-2 migration
//! - `mode_registry: ModeRegistry` — owned value; pending wrap
//! - `quick_open_registry: QuickOpenRegistry` — owned value; pending wrap
//! - `event_broadcaster: EventBroadcaster` — owned value; needs check
//! - `plugin_manager: PluginManager` — needs `Arc<Mutex<…>>` wrapping
//!   when the first hook-firing handler migrates to `impl Window`
//! - All `*_registry` types currently owned by value
//!
//! These are deliberately deferred: they can be added to `WindowResources`
//! incrementally as method migrations surface the need. Foundation PR
//! lands what's cheap to share today; later PRs widen the surface as
//! needed by each `impl Window` move.

use crate::config::Config;
use crate::config_io::DirectoryContext;
use crate::input::command_registry::CommandRegistry;
use crate::input::keybindings::KeybindingResolver;
use crate::model::filesystem::FileSystem;
use crate::primitives::grammar::GrammarRegistry;
use crate::services::authority::Authority;
use crate::services::fs::FsManager;
use crate::services::time_source::SharedTimeSource;
use crate::view::theme::ThemeRegistry;
use fresh_core::BufferId;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, RwLock};

/// Globally-unique `BufferId` allocator shared across every `Window`.
///
/// `BufferId`s must be unique editor-wide so plugin APIs that thread
/// ids around (`editor.openFile(...) -> BufferId`, `setActiveBuffer(id)`,
/// terminal `terminalId` correlation, recovery files keyed by id) don't
/// have to disambiguate by `WindowId`. The allocator is a single
/// `Arc<AtomicUsize>` cloned into every `Window` — concurrent
/// `next()` calls return distinct ids without locking.
#[derive(Debug, Clone)]
pub struct BufferIdAllocator(Arc<AtomicUsize>);

impl BufferIdAllocator {
    /// Construct an allocator starting at `start`. The first `next()`
    /// call returns `BufferId(start)`.
    pub fn new(start: usize) -> Self {
        Self(Arc::new(AtomicUsize::new(start)))
    }

    /// Allocate the next `BufferId`. Thread-safe and lock-free.
    pub fn next(&self) -> BufferId {
        BufferId(self.0.fetch_add(1, Ordering::Relaxed))
    }

    /// Peek at the value the next `next()` call would return without
    /// advancing. Test-only — production code should always use `next()`.
    #[doc(hidden)]
    pub fn peek(&self) -> usize {
        self.0.load(Ordering::Relaxed)
    }

    /// Restore the counter to a specific value (used by workspace
    /// rehydration so persisted ids don't collide with freshly-allocated
    /// ones after restart).
    pub fn set(&self, value: usize) {
        self.0.store(value, Ordering::Relaxed);
    }
}

/// Editor-global resources that every `Window` holds an `Arc`-cloned
/// reference to.
///
/// One instance is constructed in `editor_init` and cloned into the base
/// `Window`. `Editor::create_window` and the first-dive seed path in
/// `set_active_window` clone it into every subsequent `Window`. All
/// fields are cheap to clone (`Arc` increment or `Clone`-by-value where
/// the inner type already carries `Arc`s like `Authority`).
///
/// A `Window` handler that needs any of these reads it directly:
/// `self.resources.config.editor.line_wrap`,
/// `self.resources.authority.path_translation`, etc. The
/// [`Window::config()`] / `Window::authority()` accessors are the
/// canonical reading API; the field itself stays `pub(crate)` so call
/// sites can split-borrow disjoint sub-fields when the borrow checker
/// needs it.
#[derive(Clone)]
pub struct WindowResources {
    /// Read-only editor configuration. Hot-reloaded by swapping the
    /// `Arc`'s pointee when the user edits their config file.
    pub config: Arc<Config>,

    /// Tree-sitter grammar registry. `Arc` because grammar loading
    /// can be expensive and is shared across windows.
    pub grammar_registry: Arc<GrammarRegistry>,

    /// Theme registry (the *catalogue* of available themes, not the
    /// active theme — that's `Editor::theme` for now, pending the
    /// Tier-2 wrap).
    pub theme_registry: Arc<ThemeRegistry>,

    /// Cache of plugin-supplied theme JSONs, populated by plugin
    /// commands and read by the theme loader.
    pub theme_cache: Arc<RwLock<HashMap<String, serde_json::Value>>>,

    /// Keybinding resolver (mode → key → command map). `RwLock` because
    /// plugin commands can mutate the resolver at runtime.
    pub keybindings: Arc<RwLock<KeybindingResolver>>,

    /// Command registry (named commands a plugin or user can invoke).
    /// `RwLock` for the same reason as `keybindings`.
    pub command_registry: Arc<RwLock<CommandRegistry>>,

    /// Filesystem operation manager (background renames, deletes, etc.)
    pub fs_manager: Arc<FsManager>,

    /// Direct host-filesystem handle. Held alongside the active
    /// authority's filesystem because some operations (recovery file
    /// IO, history persistence) intentionally bypass the authority's
    /// translation.
    pub local_filesystem: Arc<dyn FileSystem + Send + Sync>,

    /// Globally-unique `BufferId` allocator (see [`BufferIdAllocator`]).
    pub buffer_id_alloc: BufferIdAllocator,

    /// Active filesystem authority (local / devcontainer / remote).
    /// `Authority` is `Clone` because it internally holds `Arc`s for
    /// the filesystem and path-translation handles; cloning here gives
    /// each window an independent handle that points at the same
    /// underlying authority.
    pub authority: Authority,

    /// Editor-wide time source (real clock in production, controllable
    /// in tests). Already `Arc`-internal.
    pub time_source: SharedTimeSource,

    /// Directory context (config dir, themes dir, plugins dir, etc.).
    /// Cloned by value because it's a small struct of `PathBuf`s.
    pub dir_context: DirectoryContext,

    /// Tokio runtime for async I/O tasks (LSP, file watchers, git, etc.).
    /// Single runtime shared across all windows via `Arc`. `None` means
    /// the editor was constructed without async support (rare).
    pub tokio_runtime: Option<Arc<tokio::runtime::Runtime>>,

    /// Async-message bridge (Sender + Arc'd Receiver). Windows clone this
    /// to publish messages back to the editor's main loop. The Receiver
    /// is `Arc<Mutex<>>` internally, so all clones drain the same queue.
    pub async_bridge: Option<crate::services::async_bridge::AsyncBridge>,

    /// Plugin manager (single QuickJS instance), wrapped in
    /// `Arc<RwLock<>>` so windows can fire hooks and read state
    /// without going through `Editor`. Reads take a read lock; the
    /// few `&mut self` methods (process_commands, check_thread_health,
    /// test_inject_command) take a write lock.
    pub plugin_manager: Arc<RwLock<crate::services::plugins::manager::PluginManager>>,

    /// Active resolved theme, mirrored from `Editor.theme`. Each
    /// `set_theme` / theme-reload pushes the new value into this
    /// `Arc<RwLock<>>` so Window methods can read colors without
    /// going through Editor.
    pub theme: Arc<RwLock<crate::view::theme::Theme>>,

    /// Editor-wide event broadcaster (cloneable, Arc inside).
    pub event_broadcaster: crate::model::control_event::EventBroadcaster,

    /// Hot-exit / crash recovery service. `Arc<Mutex>` because it carries
    /// mutable session state (`session_started`, per-buffer save times)
    /// and is shared by every window — per-window restore and auto-save
    /// reach it directly (no active-window flip).
    pub recovery_service: Arc<std::sync::Mutex<crate::services::recovery::RecoveryService>>,
}
