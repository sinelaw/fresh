//! Editor construction and initialization.
//!
//! `Editor::new` and friends — the entry points that take a configuration,
//! terminal dimensions, color capability, and filesystem implementation
//! and return a ready-to-use Editor with every field initialized.
//!
//! Also includes `start_background_grammar_build`, which kicks off the
//! initial grammar registry build asynchronously so startup doesn't block.

// Re-use everything mod.rs imports — the constructors touch every field
// on Editor and most of the types in the module.
use super::*;

/// Phase-timing helper used when `FRESH_TEST_TIMING=1` is set so test
/// authors can see where `Editor::with_options` spends its wall clock.
/// No-op when the env var is unset; printed to stderr otherwise.
struct InitTimer {
    label: &'static str,
    start: std::time::Instant,
    last: std::time::Instant,
    enabled: bool,
}

impl InitTimer {
    fn start(label: &'static str) -> Self {
        let enabled = std::env::var("FRESH_TEST_TIMING").is_ok_and(|v| !v.is_empty() && v != "0");
        let now = std::time::Instant::now();
        if enabled {
            eprintln!("[timing] {label}  start");
        }
        Self {
            label,
            start: now,
            last: now,
            enabled,
        }
    }
    fn phase(&mut self, name: &str) {
        if !self.enabled {
            return;
        }
        let now = std::time::Instant::now();
        let delta = now.duration_since(self.last);
        let cumul = now.duration_since(self.start);
        eprintln!(
            "[timing]     {name:<30} +{delta:>8.1}ms  (cumul {cumul:.1}ms)",
            name = name,
            delta = delta.as_secs_f64() * 1000.0,
            cumul = cumul.as_secs_f64() * 1000.0,
        );
        self.last = now;
    }
    fn finish(self) {
        if !self.enabled {
            return;
        }
        eprintln!(
            "[timing] {label}  total {total:.1}ms",
            label = self.label,
            total = self.start.elapsed().as_secs_f64() * 1000.0,
        );
    }
}

/// Set a value at a dot-separated path inside a JSON object, creating
/// intermediate maps as needed.
fn set_dot_path(root: &mut serde_json::Value, path: &str, value: serde_json::Value) {
    let segments: Vec<&str> = path.split('.').filter(|s| !s.is_empty()).collect();
    if segments.is_empty() {
        return;
    }
    let mut cur = root;
    for seg in &segments[..segments.len() - 1] {
        if !cur.is_object() {
            *cur = serde_json::Value::Object(serde_json::Map::new());
        }
        cur = cur
            .as_object_mut()
            .unwrap()
            .entry((*seg).to_string())
            .or_insert(serde_json::Value::Null);
    }
    let last = segments[segments.len() - 1];
    if !cur.is_object() {
        *cur = serde_json::Value::Object(serde_json::Map::new());
    }
    cur.as_object_mut().unwrap().insert(last.to_string(), value);
}

/// Pre-built non-trivial inputs handed to [`Editor::from_parts`].
///
/// Everything in here either depends on external resources (filesystem,
/// config, plugins, themes, terminal dimensions, …) or is one of the
/// few editor-global fields a caller wants to control directly — most
/// notably the initial set of `windows`. Trivial fields (counters at
/// zero, empty collections, `None` options, registries built from
/// scratch with no dependencies) are filled in by the constructor.
///
/// The factory methods (`Editor::new`, `Editor::with_working_dir`,
/// `Editor::with_working_dir_opts`, `Editor::for_test`,
/// `Editor::with_options`) build a value of this type and pass it to
/// `Editor::from_parts`. No production code constructs `Editor`
/// without going through `from_parts`, so adding a field here forces
/// every factory to provide it.
pub(super) struct EditorParts {
    // Config / paths
    pub(super) config: Arc<Config>,
    pub(super) config_snapshot_anchor: Arc<Config>,
    pub(super) config_cached_json: Arc<serde_json::Value>,
    pub(super) user_config_raw: Arc<serde_json::Value>,
    pub(super) dir_context: DirectoryContext,
    pub(super) working_dir: PathBuf,

    // Themes
    pub(super) theme: Arc<RwLock<crate::view::theme::Theme>>,
    pub(super) theme_registry: Arc<crate::view::theme::ThemeRegistry>,
    pub(super) theme_cache: Arc<RwLock<HashMap<String, serde_json::Value>>>,

    // Grammar
    pub(super) grammar_registry: Arc<crate::primitives::grammar::GrammarRegistry>,
    pub(super) pending_grammars: Vec<PendingGrammar>,
    pub(super) needs_full_grammar_build: bool,

    // Keybindings + buffer-id allocation
    pub(super) keybindings: Arc<RwLock<KeybindingResolver>>,
    pub(super) buffer_id_alloc: crate::app::window_resources::BufferIdAllocator,
    pub(super) next_buffer_id: usize,

    // Terminal
    pub(super) terminal_width: u16,
    pub(super) terminal_height: u16,
    pub(super) color_capability: crate::view::color_support::ColorCapability,

    // Async / IO
    pub(super) tokio_runtime: Option<Arc<tokio::runtime::Runtime>>,
    pub(super) async_bridge: AsyncBridge,
    pub(super) fs_manager: Arc<FsManager>,
    pub(super) authority: crate::services::authority::Authority,
    pub(super) local_filesystem: Arc<dyn FileSystem + Send + Sync>,

    // Chrome flags resolved from config

    // Windows — the whole point of the split: the factory builds these
    // (from disk persistence or a single seed window), the constructor
    // just installs them.
    pub(super) windows: HashMap<fresh_core::WindowId, crate::app::window::Window>,
    pub(super) active_window: fresh_core::WindowId,
    pub(super) next_window_id: u64,

    // Registries / managers
    pub(super) command_registry: Arc<RwLock<CommandRegistry>>,
    pub(super) quick_open_registry: QuickOpenRegistry,
    pub(super) plugin_manager: Arc<RwLock<PluginManager>>,
    pub(super) recovery_service: RecoveryService,
    pub(super) key_translator: crate::input::key_translator::KeyTranslator,
    pub(super) update_checker: Option<crate::services::release_checker::PeriodicUpdateChecker>,

    // Time
    pub(super) time_source: SharedTimeSource,

    // Persisted plugin global state (one map per plugin). Pulled from
    // `<data_dir>/orchestrator/state/<plugin>.json` by the
    // factory so plugins reading `getGlobalState(...)` on first tick
    // see the previous run's values without a separate
    // post-construction load step.
    pub(super) plugin_global_state: HashMap<String, HashMap<String, serde_json::Value>>,

    /// Editor-wide event broadcaster, shared with every WindowResources.
    pub(super) event_broadcaster: crate::model::control_event::EventBroadcaster,
}

impl Editor {
    /// Lightweight constructor. Takes the non-trivial editor-global
    /// resources via [`EditorParts`] and fills in every other field
    /// with its empty/default value. No I/O, no plugin loading, no
    /// disk reads happen here — that's all the factory's job
    /// ([`Editor::with_options`] and friends), so this method can
    /// also serve as a building block for narrowly-scoped tests that
    /// want to assemble an `Editor` from hand-built parts.
    ///
    /// Fields that need a `time_source` for their initial value
    /// (auto-revert timestamps, etc.) read it out of `parts` rather
    /// than capturing a new clock — so two editors built from the
    /// same parts agree on "now".
    pub(super) fn from_parts(parts: EditorParts) -> Self {
        Editor {
            // From parts (non-trivial):
            next_buffer_id: parts.next_buffer_id,
            buffer_id_alloc: parts.buffer_id_alloc,
            config: parts.config,
            config_snapshot_anchor: parts.config_snapshot_anchor,
            config_cached_json: parts.config_cached_json,
            user_config_raw: parts.user_config_raw,
            dir_context: parts.dir_context.clone(),
            grammar_registry: parts.grammar_registry,
            pending_grammars: parts.pending_grammars,
            needs_full_grammar_build: parts.needs_full_grammar_build,
            theme: parts.theme,
            theme_registry: parts.theme_registry,
            theme_cache: parts.theme_cache,
            keybindings: parts.keybindings,
            terminal_width: parts.terminal_width,
            terminal_height: parts.terminal_height,
            tokio_runtime: parts.tokio_runtime,
            async_bridge: Some(parts.async_bridge),
            fs_manager: parts.fs_manager,
            authority: parts.authority,
            local_filesystem: parts.local_filesystem,
            menu_state: crate::view::ui::MenuState::new(parts.dir_context.themes_dir()),
            working_dir: parts.working_dir,
            windows: parts.windows,
            active_window: parts.active_window,
            next_window_id: parts.next_window_id,
            command_registry: parts.command_registry,
            quick_open_registry: parts.quick_open_registry,
            plugin_manager: parts.plugin_manager,
            recovery_service: parts.recovery_service,
            time_source: parts.time_source,
            color_capability: parts.color_capability,
            update_checker: parts.update_checker,
            key_translator: parts.key_translator,

            // Trivial defaults (no external dependencies):
            grammar_reload_pending: false,
            grammar_build_in_progress: false,
            pending_grammar_callbacks: Vec::new(),
            expanded_menus_cache: crate::view::ui::ExpandedMenusCache::default(),
            ansi_background: None,
            ansi_background_path: None,
            background_fade: crate::primitives::ansi_background::DEFAULT_BACKGROUND_FADE,
            clipboard: crate::services::clipboard::Clipboard::new(),
            should_quit: false,
            should_detach: false,
            session_mode: false,
            software_cursor_only: false,
            session_name: None,
            pending_escape_sequences: Vec::new(),
            restart_with_dir: None,
            last_window_title: None,
            mode_registry: ModeRegistry::new(),
            pending_authority: None,
            remote_indicator_override: None,
            menus: crate::config::MenuConfig::translated(),
            background_process_handles: HashMap::new(),
            host_process_handles: HashMap::new(),
            status_bar_token_registry: Mutex::new(HashMap::new()),
            event_broadcaster: parts.event_broadcaster,
            #[cfg(feature = "plugins")]
            pending_plugin_actions: Vec::new(),
            #[cfg(feature = "plugins")]
            plugin_render_requested: false,
            full_redraw_requested: false,
            suspend_requested: false,
            plugin_global_state: parts.plugin_global_state,
            warning_log: None,
            status_log_path: None,
            file_watcher_manager: crate::services::file_watcher::FileWatcherManager::new(),
            last_path_change_for_test: None,
            last_watch_response_for_test: None,
            preview_window_id: None,
            settings_state: None,
            calibration_wizard: None,
            // event_debug moved to Window
            keybinding_editor: None,
            stdin_stream: stdin_stream::StdinStream::default(),
            global_popups: crate::view::popup::PopupManager::new(),
            previous_cursor_screen_pos: None,
            cursor_jump_animation: None,
            pending_vb_animations: Vec::new(),
            widget_registry: crate::widgets::WidgetRegistry::new(),
            floating_widget_panel: None,
        }
    }

    /// Create a new editor with the given configuration and terminal dimensions
    /// Uses system directories for state (recovery, sessions, etc.)
    pub fn new(
        config: Config,
        width: u16,
        height: u16,
        dir_context: DirectoryContext,
        color_capability: crate::view::color_support::ColorCapability,
        filesystem: Arc<dyn FileSystem + Send + Sync>,
    ) -> AnyhowResult<Self> {
        Self::with_working_dir(
            config,
            width,
            height,
            None,
            dir_context,
            true,
            color_capability,
            filesystem,
        )
    }

    /// Create a new editor with an explicit working directory
    /// This is useful for testing with isolated temporary directories
    #[allow(clippy::too_many_arguments)]
    pub fn with_working_dir(
        config: Config,
        width: u16,
        height: u16,
        working_dir: Option<PathBuf>,
        dir_context: DirectoryContext,
        plugins_enabled: bool,
        color_capability: crate::view::color_support::ColorCapability,
        filesystem: Arc<dyn FileSystem + Send + Sync>,
    ) -> AnyhowResult<Self> {
        Self::with_working_dir_opts(
            config,
            width,
            height,
            working_dir,
            dir_context,
            plugins_enabled,
            color_capability,
            filesystem,
            false,
        )
    }

    /// Like [`Self::with_working_dir`] but with `defer_plugin_load`
    /// exposed. When `true`, plugin loading is dispatched to the plugin
    /// thread and the constructor returns immediately; results arrive
    /// later via `AsyncMessage::PluginsDirLoaded` /
    /// `PluginDeclarationsReady` and are applied in `process_async_messages`.
    /// Used by the TUI startup path so the first frame draws without
    /// waiting on TS parse/transpile/register.
    #[allow(clippy::too_many_arguments)]
    pub fn with_working_dir_opts(
        config: Config,
        width: u16,
        height: u16,
        working_dir: Option<PathBuf>,
        dir_context: DirectoryContext,
        plugins_enabled: bool,
        color_capability: crate::view::color_support::ColorCapability,
        filesystem: Arc<dyn FileSystem + Send + Sync>,
        defer_plugin_load: bool,
    ) -> AnyhowResult<Self> {
        tracing::info!("Building default grammar registry...");
        let start = std::time::Instant::now();
        let mut grammar_registry = crate::primitives::grammar::GrammarRegistry::defaults_only();
        // Merge user config so find_by_path respects user globs/filenames
        // from the very first lookup. `defaults_only` just built the Arc, so
        // we're the sole owner; get_mut is guaranteed to succeed. Assert
        // rather than silently drop config — a failure here would leave the
        // user wondering why their `*.conf → bash` rule doesn't highlight.
        std::sync::Arc::get_mut(&mut grammar_registry)
            .expect("defaults_only returned a shared Arc")
            .apply_language_config(&config.languages);
        tracing::info!("Default grammar registry built in {:?}", start.elapsed());
        // Don't start background grammar build here — it's deferred to the
        // first flush_pending_grammars() call so that plugin-registered grammars
        // from the first event-loop tick are included in a single build.
        Self::with_options(
            config,
            width,
            height,
            working_dir,
            filesystem,
            plugins_enabled,
            true, // enable_embedded_plugins (production: always allow embedded fallback)
            dir_context,
            None,
            color_capability,
            grammar_registry,
            defer_plugin_load,
        )
    }

    /// Create a new editor for testing with custom backends
    ///
    /// By default uses empty grammar registry for fast initialization.
    /// Pass `Some(registry)` for tests that need syntax highlighting or shebang detection.
    ///
    /// `enable_plugins` controls whether the plugin runtime is active at all.
    /// `enable_embedded_plugins` separately gates the cargo-binstall embedded
    /// plugins fallback — tests that pre-populate `<config_dir>/plugins/` and
    /// want exact control over which plugins load can pass `false` here while
    /// keeping `enable_plugins = true`.
    #[allow(clippy::too_many_arguments)]
    pub fn for_test(
        config: Config,
        width: u16,
        height: u16,
        working_dir: Option<PathBuf>,
        dir_context: DirectoryContext,
        color_capability: crate::view::color_support::ColorCapability,
        filesystem: Arc<dyn FileSystem + Send + Sync>,
        time_source: Option<SharedTimeSource>,
        grammar_registry: Option<Arc<crate::primitives::grammar::GrammarRegistry>>,
        enable_plugins: bool,
        enable_embedded_plugins: bool,
    ) -> AnyhowResult<Self> {
        let mut grammar_registry =
            grammar_registry.unwrap_or_else(crate::primitives::grammar::GrammarRegistry::empty);
        // Merge user `[languages]` config into the catalog — production code
        // does this at startup and again after the background grammar build,
        // tests need the same so config-declared grammars/extensions resolve
        // through `find_by_path`. Both call sites that feed into `for_test`
        // (`HarnessOptions::with_full_grammar_registry` and the default
        // `GrammarRegistry::empty()`) hand us the sole Arc owner.
        std::sync::Arc::get_mut(&mut grammar_registry)
            .expect("grammar registry Arc must be uniquely owned at for_test entry")
            .apply_language_config(&config.languages);
        let mut editor = Self::with_options(
            config,
            width,
            height,
            working_dir,
            filesystem,
            enable_plugins,
            enable_embedded_plugins,
            dir_context,
            time_source,
            color_capability,
            grammar_registry,
            false,
        )?;
        // Tests typically have no async_bridge, so the deferred grammar build
        // would just drain pending_grammars and early-return. Skip it entirely.
        editor.needs_full_grammar_build = false;
        Ok(editor)
    }

    /// Create a new editor with custom options
    /// This is primarily used for testing with slow or mock backends
    /// to verify editor behavior under various I/O conditions
    #[allow(clippy::too_many_arguments)]
    fn with_options(
        mut config: Config,
        width: u16,
        height: u16,
        working_dir: Option<PathBuf>,
        filesystem: Arc<dyn FileSystem + Send + Sync>,
        enable_plugins: bool,
        #[cfg_attr(not(feature = "embed-plugins"), allow(unused_variables))]
        enable_embedded_plugins: bool,
        dir_context: DirectoryContext,
        time_source: Option<SharedTimeSource>,
        color_capability: crate::view::color_support::ColorCapability,
        grammar_registry: Arc<crate::primitives::grammar::GrammarRegistry>,
        defer_plugin_load: bool,
    ) -> AnyhowResult<Self> {
        let mut t = InitTimer::start("Editor::with_options");
        // Use provided time_source or default to RealTimeSource
        let time_source = time_source.unwrap_or_else(RealTimeSource::shared);
        tracing::info!("Editor::new called with width={}, height={}", width, height);

        // Use provided working_dir or capture from environment
        let working_dir = working_dir
            .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

        // Canonicalize working_dir to resolve symlinks and normalize path components
        // This ensures consistent path comparisons throughout the editor
        let working_dir = working_dir.canonicalize().unwrap_or(working_dir);

        t.phase("preamble");
        // Load all themes into registry
        tracing::info!("Loading themes...");
        let theme_loader = crate::view::theme::ThemeLoader::new(dir_context.themes_dir());
        t.phase("ThemeLoader::new");
        // Scan installed packages (language packs + bundles) before plugin loading.
        // This replaces the JS loadInstalledPackages() — configs, grammars, plugin dirs,
        // and theme dirs are all collected here and applied synchronously.
        let scan_result =
            crate::services::packages::scan_installed_packages(&dir_context.config_dir);
        t.phase("scan_installed_packages");

        // Apply package language configs (user config takes priority via or_insert)
        for (lang_id, lang_config) in &scan_result.language_configs {
            config
                .languages
                .entry(lang_id.clone())
                .or_insert_with(|| lang_config.clone());
        }

        // Apply package LSP configs (user config takes priority via or_insert)
        for (lang_id, lsp_config) in &scan_result.lsp_configs {
            config
                .lsp
                .entry(lang_id.clone())
                .or_insert_with(|| LspLanguageConfig::Multi(vec![lsp_config.clone()]));
        }

        let theme_registry = Arc::new(theme_loader.load_all(&scan_result.bundle_theme_dirs));
        t.phase("theme_loader.load_all");
        tracing::info!("Themes loaded");

        // Get active theme from registry, falling back to default if not found
        let theme_inner = theme_registry.get_cloned(&config.theme).unwrap_or_else(|| {
            tracing::warn!(
                "Theme '{}' not found, falling back to default theme",
                config.theme.0
            );
            theme_registry
                .get_cloned(&crate::config::ThemeName(
                    crate::view::theme::THEME_HIGH_CONTRAST.to_string(),
                ))
                .expect("Default theme must exist")
        });

        // Set terminal cursor color to match theme
        theme_inner.set_terminal_cursor_color();
        let theme = Arc::new(RwLock::new(theme_inner));

        t.phase("theme_setup");
        let keybindings = Arc::new(RwLock::new(KeybindingResolver::new(&config)));
        t.phase("keybindings");

        // Create an empty initial buffer
        let mut buffers = crate::app::window::WindowBuffers::new();
        let mut event_logs = HashMap::new();

        // Buffer IDs start at 1 (not 0) because the plugin API returns 0 to
        // mean "no active buffer" from getActiveBufferId().  JavaScript treats
        // 0 as falsy (`if (!bufferId)` would wrongly reject buffer 0), so
        // using 1-based IDs avoids this entire class of bugs in plugins.
        let buffer_id = BufferId(1);
        let mut state = EditorState::new(
            width,
            height,
            config.editor.large_file_threshold_bytes as usize,
            Arc::clone(&filesystem),
        );
        // Configure initial buffer settings from config
        state
            .margins
            .configure_for_line_numbers(config.editor.line_numbers);
        state.buffer_settings.tab_size = config.editor.tab_size;
        state.buffer_settings.auto_close = config.editor.auto_close;
        // Note: line_wrap_enabled is now stored in SplitViewState.viewport
        tracing::info!("EditorState created for buffer {:?}", buffer_id);
        buffers.insert(buffer_id, state);
        event_logs.insert(buffer_id, EventLog::new());

        // Create metadata for the initial empty buffer. After Step 0l
        // this lives on the base `Window`; we accumulate it locally and
        // hand it off when the window is constructed below.
        let mut buffer_metadata: HashMap<BufferId, BufferMetadata> = HashMap::new();
        buffer_metadata.insert(buffer_id, BufferMetadata::new());

        // Read orchestrator persistence (`windows.json` and
        // `state/*.json` under `<data_dir>/orchestrator/`)
        // before the LSP and base-window construction below.
        // Pulling persistence in here lets the factory build the
        // right windows up front: previously this ran from
        // `main.rs` after construction, so the freshly built
        // single-base window had to be torn down and replaced with
        // an inert shell — leaving the active window with
        // `splits = None` until something re-seeded it. Now the
        // factory picks the persisted active id/root, attaches the
        // seed buffer + LSP to it directly, and the constructor
        // sees a well-formed windows map.
        let persisted_env = crate::app::orchestrator_persistence::read_persisted_windows_env(
            filesystem.as_ref(),
            &dir_context.data_dir,
            &working_dir,
        );
        let plugin_global_state = crate::app::orchestrator_persistence::read_persisted_plugin_state(
            filesystem.as_ref(),
            &dir_context.data_dir,
            &working_dir,
        );

        // Determine the active window's id and root. Persisted
        // `env.active` is *cross-project* — it just names the last
        // session the user touched anywhere. Honoring it blindly
        // when the user re-launches inside a different project
        // strands the editor with an LSP and Open-Terminal default
        // pointed at the wrong tree (see issue #2026). Prefer a
        // persisted window that actually belongs to `working_dir`,
        // and fall back to the boot-time defaults (id 1, process
        // cwd) when no persisted window matches.
        let (active_window_id, active_window_root) =
            crate::app::orchestrator_persistence::pick_active_window_for_cwd(
                persisted_env.as_ref(),
                &working_dir,
            )
            .map(|w| (fresh_core::WindowId(w.id), w.root.clone()))
            .unwrap_or((fresh_core::WindowId(1), working_dir.clone()));

        // Initialize LSP manager with active window's root.
        let root_uri = types::file_path_to_lsp_uri(&active_window_root);

        t.phase("buffer_state");
        // Create Tokio runtime for async I/O (LSP, file watching, git, etc.)
        let tokio_runtime = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(2) // Small pool for I/O tasks
            .thread_name("editor-async")
            .enable_all()
            .build()
            .ok()
            .map(Arc::new);
        t.phase("tokio_runtime");

        // Create editor-global async bridge for editor-scoped async
        // sources (plugin runtime callbacks, file-open dialog, etc.).
        // Per-window subsystems (LSP, terminal output, file-explorer
        // async expansion) flow through their owning window's
        // bridge instead — see `Window.bridge`.
        let async_bridge = AsyncBridge::new();
        let event_broadcaster = crate::model::control_event::EventBroadcaster::default();

        // Create the base window's per-window bridge up front so the
        // LSP manager (configured below) can receive its responses
        // through the window's channel rather than the editor-global
        // one. The same `AsyncBridge` is moved into `base.bridge`
        // when the base Window is constructed at the end of init.
        let base_window_bridge = AsyncBridge::new();

        if tokio_runtime.is_none() {
            tracing::warn!("Failed to create Tokio runtime - async features disabled");
        }

        // Create LSP manager with async support, scoped to the
        // active window (matches `active_window_id` + the LSP
        // root_uri above). LSP responses route through the active
        // window's per-window bridge.
        let mut lsp = LspManager::new(active_window_id, root_uri);

        // Configure runtime and bridge if available — the LSP manager
        // is wired to the base window's bridge, so its async responses
        // land in `base.bridge` (not the editor-global `async_bridge`).
        if let Some(ref runtime) = tokio_runtime {
            lsp.set_runtime(runtime.handle().clone(), base_window_bridge.clone());
        }

        // Configure LSP servers from config
        for (language, lsp_configs) in &config.lsp {
            lsp.set_language_configs(language.clone(), lsp_configs.as_slice().to_vec());
        }

        // Configure universal (global) LSP servers — spawned once, shared across languages
        let universal_servers: Vec<LspServerConfig> = config
            .universal_lsp
            .values()
            .flat_map(|lc| lc.as_slice().to_vec())
            .filter(|c| c.enabled)
            .collect();
        lsp.set_universal_configs(universal_servers);

        // Auto-detect Deno projects: if deno.json or deno.jsonc exists in the
        // workspace root, override JS/TS LSP to use `deno lsp` (#1191).
        // Checked against `active_window_root` so persisted sessions get the
        // detection their actual project — process cwd would be wrong for a
        // restored session rooted elsewhere.
        if active_window_root.join("deno.json").exists()
            || active_window_root.join("deno.jsonc").exists()
        {
            tracing::info!("Detected Deno project (deno.json found), using deno lsp for JS/TS");
            let deno_config = LspServerConfig {
                command: "deno".to_string(),
                args: vec!["lsp".to_string()],
                enabled: true,
                auto_start: false,
                process_limits: ProcessLimits::default(),
                initialization_options: Some(serde_json::json!({"enable": true})),
                ..Default::default()
            };
            lsp.set_language_config("javascript".to_string(), deno_config.clone());
            lsp.set_language_config("typescript".to_string(), deno_config);
        }

        t.phase("lsp_setup");
        // Initialize split manager with the initial buffer
        let split_manager = SplitManager::new(buffer_id);

        // Initialize per-split view state for the initial split
        let mut split_view_states = HashMap::new();
        let initial_split_id = split_manager.active_split();
        let mut initial_view_state = SplitViewState::with_buffer(width, height, buffer_id);
        initial_view_state.apply_config_defaults(
            config.editor.line_numbers,
            config.editor.highlight_current_line,
            config.editor.line_wrap,
            config.editor.wrap_indent,
            config.editor.wrap_column,
            config.editor.rulers.clone(),
        );
        split_view_states.insert(initial_split_id, initial_view_state);

        // Initialize filesystem manager for file explorer
        let fs_manager = Arc::new(FsManager::new(Arc::clone(&filesystem)));

        // Initialize command registry (always available, used by both plugins and core)
        let command_registry = Arc::new(RwLock::new(CommandRegistry::new()));

        // Construct the boot-time authority. Per principle 6, the editor
        // always boots with a local authority and renders immediately;
        // SSH startup and plugins replace it via `install_authority`
        // after their async work is done. The supplied `filesystem`
        // overrides the local default to support tests that mock IO.
        let authority = crate::services::authority::Authority {
            filesystem: Arc::clone(&filesystem),
            ..crate::services::authority::Authority::local()
        };
        let process_spawner = Arc::clone(&authority.process_spawner);

        // Initialize Quick Open registry with all providers
        let mut quick_open_registry = QuickOpenRegistry::new();
        quick_open_registry.register(Box::new(FileProvider::new(
            Arc::clone(&filesystem),
            Arc::clone(&process_spawner),
            tokio_runtime.as_ref().map(|rt| rt.handle().clone()),
            Some(async_bridge.sender()),
        )));
        quick_open_registry.register(Box::new(CommandProvider::new(
            Arc::clone(&command_registry),
            Arc::clone(&keybindings),
        )));
        quick_open_registry.register(Box::new(BufferProvider::new()));
        quick_open_registry.register(Box::new(GotoLineProvider::new()));

        // Build shared theme cache for plugin access
        let theme_cache = Arc::new(RwLock::new(theme_registry.to_json_map()));

        t.phase("split_quickopen_authority");
        // Initialize plugin manager (handles both enabled and disabled cases internally)
        let plugin_manager = Arc::new(RwLock::new(PluginManager::new(
            enable_plugins,
            Arc::clone(&command_registry),
            dir_context.clone(),
            Arc::clone(&theme_cache),
        )));
        t.phase("PluginManager::new");

        // Update the plugin state snapshot with working_dir BEFORE loading plugins
        // This ensures plugins can call getCwd() correctly during initialization
        #[cfg(feature = "plugins")]
        if let Some(snapshot_handle) = plugin_manager.read().unwrap().state_snapshot_handle() {
            let mut snapshot = snapshot_handle.write().unwrap();
            snapshot.working_dir = working_dir.clone();
            // Pre-populate keybinding labels for the static built-in
            // keymap so `editor.getKeybindingLabel(action, context)`
            // works for actions that aren't behind a plugin-defined
            // buffer mode. Without this, a plugin asking
            // `getKeybindingLabel("cycle_live_grep_provider",
            // "prompt")` gets null even though Alt+P is bound, and
            // ends up hardcoding the key in its UI.
            populate_builtin_keybinding_labels(&mut snapshot, &keybindings);
        }

        // Load TypeScript plugins from multiple directories:
        // 1. Next to the executable (for cargo-dist installations)
        // 2. From embedded plugins (for cargo-binstall and `cargo run`,
        //    when embed-plugins feature is enabled)
        // 3. User plugins directory (~/.config/fresh/plugins)
        // 4. Package manager installed plugins (~/.config/fresh/plugins/packages/*)
        if plugin_manager.read().unwrap().is_active() {
            let mut plugin_dirs: Vec<std::path::PathBuf> = vec![];

            // Check next to executable first (for cargo-dist installations)
            if let Ok(exe_path) = std::env::current_exe() {
                if let Some(exe_dir) = exe_path.parent() {
                    let exe_plugin_dir = exe_dir.join("plugins");
                    if exe_plugin_dir.exists() {
                        plugin_dirs.push(exe_plugin_dir);
                    }
                }
            }

            // No working-directory `plugins/` check: a user project with a
            // folder named `plugins/` (e.g. a Vite/Rollup project, a Hugo
            // site) is not a Fresh plugin source. Bundled plugins for the
            // dev workflow come in via the embedded fallback below; user
            // plugins live under `<config_dir>/plugins/`. See issue #1722.

            // If no disk plugins found, try embedded plugins (cargo-binstall builds).
            // `enable_embedded_plugins` lets tests opt out so they get exactly
            // the plugin set they pre-populated under `<config_dir>/plugins/`,
            // without the bundled set leaking in.
            #[cfg(feature = "embed-plugins")]
            if enable_embedded_plugins && plugin_dirs.is_empty() {
                if let Some(embedded_dir) =
                    crate::services::plugins::embedded::get_embedded_plugins_dir()
                {
                    tracing::info!("Using embedded plugins from: {:?}", embedded_dir);
                    plugin_dirs.push(embedded_dir.clone());
                }
            }

            // Always check user config plugins directory (~/.config/fresh/plugins)
            let user_plugins_dir = dir_context.config_dir.join("plugins");
            if user_plugins_dir.exists() && !plugin_dirs.contains(&user_plugins_dir) {
                tracing::info!("Found user plugins directory: {:?}", user_plugins_dir);
                plugin_dirs.push(user_plugins_dir.clone());
            }

            // Check for package manager installed plugins (~/.config/fresh/plugins/packages/*)
            let packages_dir = dir_context.config_dir.join("plugins").join("packages");
            if packages_dir.exists() {
                if let Ok(entries) = std::fs::read_dir(&packages_dir) {
                    for entry in entries.flatten() {
                        let path = entry.path();
                        // Skip hidden directories (like .index for registry cache)
                        if path.is_dir() {
                            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                                if !name.starts_with('.') {
                                    tracing::info!("Found package manager plugin: {:?}", path);
                                    plugin_dirs.push(path);
                                }
                            }
                        }
                    }
                }
            }

            // Add bundle plugin directories from package scan
            for dir in &scan_result.bundle_plugin_dirs {
                tracing::info!("Found bundle plugin directory: {:?}", dir);
                plugin_dirs.push(dir.clone());
            }

            if plugin_dirs.is_empty() {
                tracing::debug!(
                    "No plugins directory found next to executable or in working dir: {:?}",
                    working_dir
                );
            }

            if defer_plugin_load {
                // Async startup path: hand each dir + a trailing
                // ListPlugins request to the plugin thread now, return
                // before they finish, and let a forwarder thread
                // translate the responses into AsyncMessages that the
                // main loop applies via `process_async_messages`. The
                // plugin thread is FIFO, so submitting in this exact
                // order guarantees declarations cover only the startup
                // batch — init.ts and lifecycle hooks queue *after*
                // ListPlugins from main.rs after construction returns,
                // matching the original blocking behaviour.
                #[cfg(feature = "plugins")]
                {
                    let bridge = &async_bridge;
                    let mut dir_receivers: Vec<(
                        std::path::PathBuf,
                        fresh_plugin_runtime::thread::oneshot::Receiver<
                            fresh_plugin_runtime::thread::PluginsDirLoadResult,
                        >,
                    )> = Vec::with_capacity(plugin_dirs.len());
                    for plugin_dir in &plugin_dirs {
                        tracing::info!(
                            "Submitting async TypeScript plugin load for: {:?}",
                            plugin_dir
                        );
                        if let Some(rx) = plugin_manager
                            .read()
                            .unwrap()
                            .load_plugins_from_dir_with_config_request(plugin_dir, &config.plugins)
                        {
                            dir_receivers.push((plugin_dir.clone(), rx));
                        }
                    }
                    let declarations_rx = if !dir_receivers.is_empty() {
                        plugin_manager.read().unwrap().list_plugins_request()
                    } else {
                        None
                    };
                    if !dir_receivers.is_empty() {
                        let sender = bridge.sender();
                        std::thread::Builder::new()
                            .name("plugin-load-forwarder".to_string())
                            .spawn(move || {
                                for (dir, rx) in dir_receivers {
                                    let load_start = std::time::Instant::now();
                                    match rx.recv() {
                                        Ok((errors, discovered_plugins)) => {
                                            tracing::info!(
                                                "Loaded TypeScript plugins from {:?} in {:?}",
                                                dir,
                                                load_start.elapsed()
                                            );
                                            drop(sender.send(
                                                crate::services::async_bridge::AsyncMessage::PluginsDirLoaded {
                                                    dir,
                                                    errors,
                                                    discovered_plugins,
                                                },
                                            ));
                                        }
                                        Err(e) => {
                                            tracing::warn!(
                                                "plugin-load-forwarder: dir {:?} recv failed: {}",
                                                dir,
                                                e
                                            );
                                        }
                                    }
                                }
                                if let Some(rx) = declarations_rx {
                                    match rx.recv() {
                                        Ok(plugin_infos) => {
                                            let declarations: Vec<(String, String)> = plugin_infos
                                                .into_iter()
                                                .filter_map(|info| {
                                                    info.declarations.map(|d| (info.name, d))
                                                })
                                                .collect();
                                            drop(sender.send(
                                                crate::services::async_bridge::AsyncMessage::PluginDeclarationsReady {
                                                    declarations,
                                                },
                                            ));
                                        }
                                        Err(e) => {
                                            tracing::warn!(
                                                "plugin-load-forwarder: list_plugins recv failed: {}",
                                                e
                                            );
                                        }
                                    }
                                }
                            })
                            .ok();
                    }
                }
            } else {
                // Synchronous (legacy / test) path. Used by `for_test`,
                // server, GUI: every other code path that wants the
                // editor fully constructed before the constructor
                // returns.
                for plugin_dir in plugin_dirs {
                    tracing::info!("Loading TypeScript plugins from: {:?}", plugin_dir);
                    let load_start = std::time::Instant::now();
                    let (errors, discovered_plugins) = plugin_manager
                        .read()
                        .unwrap()
                        .load_plugins_from_dir_with_config(&plugin_dir, &config.plugins);
                    tracing::info!(
                        "Loaded TypeScript plugins from {:?} in {:?}",
                        plugin_dir,
                        load_start.elapsed()
                    );

                    // Merge discovered plugins into config
                    // discovered_plugins already contains the merged config (saved enabled state + discovered path)
                    for (name, plugin_config) in discovered_plugins {
                        config.plugins.insert(name, plugin_config);
                    }

                    if !errors.is_empty() {
                        for err in &errors {
                            tracing::error!("TypeScript plugin load error: {}", err);
                        }
                        // In debug/test builds, panic to surface plugin loading errors
                        #[cfg(debug_assertions)]
                        panic!(
                            "TypeScript plugin loading failed with {} error(s): {}",
                            errors.len(),
                            errors.join("; ")
                        );
                    }
                }

                // Collect `.d.ts` emits from every loaded plugin into a
                // single aggregate under `<config_dir>/types/plugins.d.ts`.
                // This is what makes `getPluginApi("foo")` typed in the
                // user's init.ts without a hand-written cast — each plugin
                // that uses `declare global { interface FreshPluginRegistry }`
                // contributes its augmentation, and init.ts's tsconfig
                // picks the aggregate up via `files`.
                let declarations = plugin_manager.read().unwrap().plugin_declarations();
                crate::init_script::write_plugin_declarations(
                    &dir_context.config_dir,
                    &declarations,
                );
            }
        }

        t.phase("plugin_loading");
        // Extract config values before moving config into the struct
        let recovery_enabled = config.editor.recovery_enabled;
        let check_for_updates = config.check_for_updates;

        // Start periodic update checker if enabled (also sends daily telemetry)
        let update_checker = if check_for_updates {
            tracing::debug!("Update checking enabled, starting periodic checker");
            Some(
                crate::services::release_checker::start_periodic_update_check(
                    crate::services::release_checker::DEFAULT_RELEASES_URL,
                    time_source.clone(),
                    dir_context.data_dir.clone(),
                ),
            )
        } else {
            tracing::debug!("Update checking disabled by config");
            None
        };

        // Cache raw user config at startup (to avoid re-reading file every frame)
        let user_config_raw = Config::read_user_config_raw(&working_dir);

        // Wrap config in Arc and pre-seed the snapshot mirror + JSON cache.
        // Doing this at construction means the strong count of the live
        // `config` Arc starts at 2 and stays there: every `Arc::make_mut`
        // call on `config` is forced to CoW, so no mutation path (direct or
        // via `config_mut()`) can leave `config_cached_json` referring to
        // stale memory.
        let config_arc = Arc::new(config);
        let config_cached_json =
            Arc::new(serde_json::to_value(&*config_arc).unwrap_or(serde_json::Value::Null));
        let config_snapshot_anchor = Arc::clone(&config_arc);

        // The buffer-id allocator starts at the same value as
        // `next_buffer_id`. Both are kept in sync by every allocation
        // path (`Editor::alloc_buffer_id` advances both); the allocator
        // is what gets cloned into every `Window` so handlers on
        // `impl Window` can mint ids without an `Editor` reference.
        let buffer_id_alloc = crate::app::window_resources::BufferIdAllocator::new(2);

        // The local-host filesystem handle. Hoisted here (rather than
        // constructed inline in the `Editor` literal below) so the
        // base window's `WindowResources` and the editor share the same
        // `Arc` from the start.
        let local_filesystem: Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> =
            Arc::new(crate::model::filesystem::StdFileSystem);

        // Build the resource bundle every `Window` gets a clone of. The
        // base window receives one clone here; subsequent windows
        // (created via `Editor::create_window_at` or first-dive seeding
        // in `set_active_window`) reach back to `Editor::window_resources()`
        // for an equivalent bundle.
        let base_resources = crate::app::window_resources::WindowResources {
            config: Arc::clone(&config_arc),
            grammar_registry: Arc::clone(&grammar_registry),
            theme_registry: Arc::clone(&theme_registry),
            theme_cache: Arc::clone(&theme_cache),
            keybindings: Arc::clone(&keybindings),
            command_registry: Arc::clone(&command_registry),
            fs_manager: Arc::clone(&fs_manager),
            local_filesystem: Arc::clone(&local_filesystem),
            buffer_id_alloc: buffer_id_alloc.clone(),
            authority: authority.clone(),
            time_source: Arc::clone(&time_source),
            dir_context: dir_context.clone(),
            tokio_runtime: tokio_runtime.clone(),
            async_bridge: Some(async_bridge.clone()),
            plugin_manager: Arc::clone(&plugin_manager),
            theme: Arc::clone(&theme),
            event_broadcaster: event_broadcaster.clone(),
        };

        // Build the active window — the one that holds the seed
        // buffer, the SplitManager, the LSP, and the
        // already-configured per-window bridge. Its label/root
        // come from persistence when present so a restored session
        // looks correct from frame zero; otherwise it falls back to
        // the boot defaults.
        let (active_label, active_root, active_plugin_state) = persisted_env
            .as_ref()
            .and_then(|env| env.windows.iter().find(|w| w.id == active_window_id.0))
            .map(|w| (w.label.clone(), w.root.clone(), w.plugin_state.clone()))
            .unwrap_or_else(|| (String::new(), working_dir.clone(), HashMap::new()));

        let mut active_win = crate::app::window::Window::new(
            active_window_id,
            active_label,
            active_root,
            base_resources,
        );
        // Seed the window's terminal dimensions from the editor's
        // initial size — `Window::new` defaults to 80x24, which is
        // wrong for any harness that constructs the editor at a
        // different size (issue surfaces in
        // test_hidden_terminal_resyncs_pty_size_when_revealed).
        active_win.terminal_width = width;
        active_win.terminal_height = height;
        // Hand the eagerly-spawned LSP manager + the initial split
        // layout off to the active window — that's where they live
        // now (Step 0b).
        active_win.lsp = Some(lsp);
        active_win.buffers = buffers;
        active_win
            .buffers
            .set_splits((split_manager, split_view_states));
        active_win.buffer_metadata = buffer_metadata;
        active_win.event_logs = event_logs;
        active_win.plugin_state = active_plugin_state;
        // Replace the default bridge created by `Window::new` with
        // the bridge we already configured the LSP manager against.
        // Both halves now point at the same channel; LSP responses
        // arriving on the manager's sender land in
        // `active_win.bridge`'s receiver.
        active_win.bridge = base_window_bridge;
        // Load prompt histories from disk for the active window.
        // Each window has its own prompt-history rings.
        for history_name in ["search", "replace", "goto_line"] {
            let path = dir_context.prompt_history_path(history_name);
            let history = crate::input::input_history::InputHistory::load_from_file(&path)
                .unwrap_or_else(|e| {
                    tracing::warn!("Failed to load {} history: {}", history_name, e);
                    crate::input::input_history::InputHistory::new()
                });
            active_win
                .prompt_histories
                .insert(history_name.to_string(), history);
        }

        // Build the inert shells for every other persisted window.
        // Their `splits` stays `None`; first dive into them re-warms
        // exactly like a freshly created window.
        let mut windows = HashMap::new();
        if let Some(ref env) = persisted_env {
            for ps in &env.windows {
                let id = fresh_core::WindowId(ps.id);
                if id == active_window_id {
                    continue;
                }
                let resources = crate::app::window_resources::WindowResources {
                    config: Arc::clone(&config_arc),
                    grammar_registry: Arc::clone(&grammar_registry),
                    theme_registry: Arc::clone(&theme_registry),
                    theme_cache: Arc::clone(&theme_cache),
                    keybindings: Arc::clone(&keybindings),
                    command_registry: Arc::clone(&command_registry),
                    fs_manager: Arc::clone(&fs_manager),
                    local_filesystem: Arc::clone(&local_filesystem),
                    buffer_id_alloc: buffer_id_alloc.clone(),
                    authority: authority.clone(),
                    time_source: Arc::clone(&time_source),
                    dir_context: dir_context.clone(),
                    tokio_runtime: tokio_runtime.clone(),
                    async_bridge: Some(async_bridge.clone()),
                    plugin_manager: Arc::clone(&plugin_manager),
                    theme: Arc::clone(&theme),
                    event_broadcaster: event_broadcaster.clone(),
                };
                let mut shell = crate::app::window::Window::new(
                    id,
                    ps.label.clone(),
                    ps.root.clone(),
                    resources,
                );
                shell.terminal_width = width;
                shell.terminal_height = height;
                shell.plugin_state = ps.plugin_state.clone();
                windows.insert(id, shell);
            }
        }
        windows.insert(active_window_id, active_win);

        // Allocate next window ids past every persisted entry and
        // past our active id, so `createWindow` after restart never
        // collides with an id the user might still see in plugin
        // state. Falls back to 2 (the post-base-window default)
        // when there's no persistence.
        let max_existing = windows.keys().map(|k| k.0).max().unwrap_or(0);
        let next_window_id = persisted_env
            .as_ref()
            .map(|env| env.next_id.max(max_existing + 1))
            .unwrap_or(2);

        let recovery_service = {
            let recovery_config = RecoveryConfig {
                enabled: recovery_enabled,
                ..RecoveryConfig::default()
            };
            // Default to a CWD-scoped recovery directory so each working
            // directory keeps its own hot-exit recovery files. If this
            // editor is later promoted to session mode, `set_session_name`
            // re-creates the service with `RecoveryScope::Session`.
            // Issue #1550: without per-CWD scoping, opening Fresh in a
            // second folder would clobber the first folder's unsaved
            // unnamed buffers on shutdown.
            let scope = crate::services::recovery::RecoveryScope::Standalone {
                working_dir: working_dir.clone(),
            };
            RecoveryService::with_scope(recovery_config, &dir_context.recovery_dir(), &scope)
        };

        let key_translator = crate::input::key_translator::KeyTranslator::load_from_config_dir(
            &dir_context.config_dir,
        )
        .unwrap_or_default();

        let pending_grammars = scan_result
            .additional_grammars
            .iter()
            .map(|g| PendingGrammar {
                language: g.language.clone(),
                grammar_path: g.path.to_string_lossy().to_string(),
                extensions: g.extensions.clone(),
            })
            .collect();

        let parts = EditorParts {
            config: config_arc,
            config_snapshot_anchor,
            config_cached_json,
            user_config_raw: Arc::new(user_config_raw),
            dir_context: dir_context.clone(),
            working_dir: working_dir.clone(),
            theme,
            theme_registry,
            theme_cache,
            grammar_registry,
            pending_grammars,
            needs_full_grammar_build: true,
            keybindings,
            buffer_id_alloc: buffer_id_alloc.clone(),
            next_buffer_id: 2,
            terminal_width: width,
            terminal_height: height,
            color_capability,
            tokio_runtime,
            async_bridge,
            fs_manager,
            authority,
            local_filesystem: Arc::clone(&local_filesystem),
            windows,
            active_window: active_window_id,
            next_window_id,
            command_registry,
            quick_open_registry,
            plugin_manager,
            recovery_service,
            key_translator,
            update_checker,
            time_source: time_source.clone(),
            plugin_global_state,
            event_broadcaster: event_broadcaster.clone(),
        };

        let mut editor = Editor::from_parts(parts);

        t.phase("editor_struct_assembly");
        // Apply clipboard configuration
        editor.clipboard.apply_config(&editor.config.clipboard);

        // Seed splits/buffers for every persisted inactive window so they
        // render in preview surfaces (Orchestrator's WindowEmbed) before the
        // user first dives in. Without this, restored windows have
        // `splits == None` and paint blank in the preview pane. We also
        // catch the (rarer) inverse where splits is set but the buffer
        // map is empty — that combo is what hit the historic
        // "active buffer must be present" panic in render.
        let needs_seed: Vec<fresh_core::WindowId> = editor
            .windows
            .iter()
            .filter(|(_, s)| s.buffers.splits().is_none() || s.buffers.len() == 0)
            .map(|(id, _)| *id)
            .collect();
        for id in needs_seed {
            if let Some((buf, state, metadata, event_log, mgr, vs)) =
                editor.build_fresh_layout_if_needed(id)
            {
                if let Some(s) = editor.windows.get_mut(&id) {
                    s.buffers.set_splits((mgr, vs));
                    s.buffers.insert(buf, state);
                    s.buffer_metadata.insert(buf, metadata);
                    s.event_logs.insert(buf, event_log);
                }
            }
        }

        #[cfg(feature = "plugins")]
        {
            editor.update_plugin_state_snapshot();
            if editor.plugin_manager.read().unwrap().is_active() {
                editor.plugin_manager.read().unwrap().run_hook(
                    "editor_initialized",
                    crate::services::plugins::hooks::HookArgs::EditorInitialized {},
                );
            }
        }
        t.phase("post_struct_hooks");
        t.finish();
        Ok(editor)
    }

    /// Get a reference to the event broadcaster
    pub fn event_broadcaster(&self) -> &crate::model::control_event::EventBroadcaster {
        &self.event_broadcaster
    }

    /// Spawn a background thread to build the full grammar registry
    /// (embedded grammars, user grammars, language packs, and any plugin-registered grammars).
    /// Called on the first event-loop tick (via `flush_pending_grammars`) so that
    /// plugin grammars registered during init are included in a single build.
    pub(super) fn start_background_grammar_build(
        &mut self,
        additional: Vec<crate::primitives::grammar::GrammarSpec>,
        callback_ids: Vec<fresh_core::api::JsCallbackId>,
    ) {
        let Some(bridge) = &self.async_bridge else {
            return;
        };
        self.grammar_build_in_progress = true;
        let sender = bridge.sender();
        let config_dir = self.dir_context.config_dir.clone();
        tracing::info!(
            "Spawning background grammar build thread ({} plugin grammars)...",
            additional.len()
        );
        std::thread::Builder::new()
            .name("grammar-build".to_string())
            .spawn(move || {
                tracing::info!("[grammar-build] Thread started");
                let start = std::time::Instant::now();
                let registry = if additional.is_empty() {
                    crate::primitives::grammar::GrammarRegistry::for_editor(config_dir)
                } else {
                    crate::primitives::grammar::GrammarRegistry::for_editor_with_additional(
                        config_dir,
                        &additional,
                    )
                };
                tracing::info!("[grammar-build] Complete in {:?}", start.elapsed());
                drop(sender.send(
                    crate::services::async_bridge::AsyncMessage::GrammarRegistryBuilt {
                        registry,
                        callback_ids,
                    },
                ));
            })
            .ok();
    }

    // =========================================================================
    // init.ts / runtime-overlay surface (design docs §3–§6)
    // =========================================================================

    /// Auto-load `~/.config/fresh/init.ts` if present, through the existing
    /// plugin pipeline under the stable name `crate::init_script::INIT_PLUGIN_NAME`.
    pub fn load_init_script(&mut self, enabled: bool) {
        use crate::init_script::{
            check, decide_load, describe, record_success, refresh_types_scaffolding, CheckSeverity,
            InitOutcome, LoadDecision,
        };

        let config_dir = self.dir_context.config_dir.clone();

        if enabled {
            // Refresh the types mirror from the embedded copy before anything
            // reads init.ts. Guarantees the declarations the user sees match
            // the running build — stale types would hide API drift.
            refresh_types_scaffolding(&config_dir);

            // Re-check init.ts right after the refresh so drift between the
            // user's script and the current API surface (at least syntax-level
            // fallout like unterminated blocks from a botched rename) shows up
            // in the log immediately rather than only at eval time.
            let report = check(&config_dir);
            if !report.ok {
                for d in &report.diagnostics {
                    let level = match d.severity {
                        CheckSeverity::Error => "error",
                        CheckSeverity::Warning => "warning",
                    };
                    tracing::warn!(
                        "init.ts pre-load {level} at {}:{}: {}",
                        d.line,
                        d.column,
                        d.message
                    );
                }
            }
        }

        let outcome = match decide_load(&config_dir, enabled) {
            LoadDecision::Skip(outcome) => outcome,
            LoadDecision::Load { source } => {
                if !self.plugin_manager.read().unwrap().is_active() {
                    InitOutcome::Failed {
                        message: "plugin runtime inactive (--no-plugins); init.ts cannot run"
                            .into(),
                    }
                } else {
                    match self.plugin_manager.read().unwrap().load_plugin_from_source(
                        &source,
                        crate::init_script::INIT_PLUGIN_NAME,
                        true,
                    ) {
                        Ok(()) => {
                            record_success(&config_dir);
                            InitOutcome::Loaded
                        }
                        Err(e) => InitOutcome::Failed {
                            message: format!("{e}"),
                        },
                    }
                }
            }
        };

        let summary = describe(&outcome);
        match outcome {
            InitOutcome::NotFound | InitOutcome::Disabled => tracing::debug!("{}", summary),
            InitOutcome::Loaded => tracing::info!("{}", summary),
            InitOutcome::CrashFused { .. } | InitOutcome::Failed { .. } => {
                tracing::warn!("{}", summary);
                self.set_status_message(summary);
            }
        }
    }

    /// Non-blocking variant of [`Self::load_init_script`] for the TUI
    /// startup path. Does the synchronous pre-work (types scaffolding
    /// refresh, syntax check, fuse check), then either submits the
    /// `LoadPluginFromSource` request to the plugin thread and spawns a
    /// forwarder that translates the result into
    /// `AsyncMessage::PluginInitScriptLoaded`, or — for the `Skip(...)`
    /// outcomes — emits the message directly so the same async-dispatch
    /// handler logs and applies status. The request goes through the
    /// same FIFO channel as the startup plugin loads, so by the time the
    /// plugin thread evaluates init.ts every batch plugin has already
    /// finished — preserving the original load ordering.
    pub fn load_init_script_async(&mut self, enabled: bool) {
        use crate::init_script::{
            check, decide_load, refresh_types_scaffolding, CheckSeverity, InitOutcome, LoadDecision,
        };
        use crate::services::async_bridge::PluginInitScriptOutcome;

        let config_dir = self.dir_context.config_dir.clone();

        if enabled {
            refresh_types_scaffolding(&config_dir);
            let report = check(&config_dir);
            if !report.ok {
                for d in &report.diagnostics {
                    let level = match d.severity {
                        CheckSeverity::Error => "error",
                        CheckSeverity::Warning => "warning",
                    };
                    tracing::warn!(
                        "init.ts pre-load {level} at {}:{}: {}",
                        d.line,
                        d.column,
                        d.message
                    );
                }
            }
        }

        let outcome_now: Option<PluginInitScriptOutcome> = match decide_load(&config_dir, enabled) {
            LoadDecision::Skip(outcome) => Some(match outcome {
                InitOutcome::NotFound => PluginInitScriptOutcome::NotFound,
                InitOutcome::Disabled => PluginInitScriptOutcome::Disabled,
                InitOutcome::CrashFused { failures } => {
                    PluginInitScriptOutcome::CrashFused { failures }
                }
                // decide_load only returns these via Load; keep total to
                // satisfy the matcher.
                InitOutcome::Loaded => PluginInitScriptOutcome::Loaded,
                InitOutcome::Failed { message } => PluginInitScriptOutcome::Failed { message },
            }),
            LoadDecision::Load { source } => {
                if !self.plugin_manager.read().unwrap().is_active() {
                    Some(PluginInitScriptOutcome::Failed {
                        message: "plugin runtime inactive (--no-plugins); init.ts cannot run"
                            .into(),
                    })
                } else {
                    self.spawn_init_script_forwarder(source);
                    None
                }
            }
        };

        if let Some(outcome) = outcome_now {
            // Skip / fused / inactive paths: emit through the bridge so
            // the same handler runs them as the success path. Falls back
            // to direct application if the bridge is missing (test).
            if let Some(bridge) = &self.async_bridge {
                drop(bridge.sender().send(
                    crate::services::async_bridge::AsyncMessage::PluginInitScriptLoaded(outcome),
                ));
            } else {
                self.handle_plugin_init_script_loaded(outcome);
            }
        }
    }

    #[cfg(feature = "plugins")]
    fn spawn_init_script_forwarder(&self, source: String) {
        let Some(bridge) = &self.async_bridge else {
            return;
        };
        let Some(rx) = self
            .plugin_manager
            .read()
            .unwrap()
            .load_plugin_from_source_request(&source, crate::init_script::INIT_PLUGIN_NAME, true)
        else {
            return;
        };
        let sender = bridge.sender();
        std::thread::Builder::new()
            .name("plugin-init-forwarder".to_string())
            .spawn(move || {
                let outcome = match rx.recv() {
                    Ok(Ok(())) => crate::services::async_bridge::PluginInitScriptOutcome::Loaded,
                    Ok(Err(e)) => crate::services::async_bridge::PluginInitScriptOutcome::Failed {
                        message: format!("{e}"),
                    },
                    Err(e) => crate::services::async_bridge::PluginInitScriptOutcome::Failed {
                        message: format!("plugin thread closed: {e}"),
                    },
                };
                drop(sender.send(
                    crate::services::async_bridge::AsyncMessage::PluginInitScriptLoaded(outcome),
                ));
            })
            .ok();
    }

    #[cfg(not(feature = "plugins"))]
    fn spawn_init_script_forwarder(&self, _source: String) {}

    /// Handle `setSetting(path, value)`. Fire-and-forget: patches Config
    /// directly via JSON round-trip. No overlay, no per-plugin tracking,
    /// no revert on unload — same model as Neovim/VS Code/Emacs/Sublime.
    pub fn handle_set_setting(&mut self, path: String, value: serde_json::Value) {
        let mut json = serde_json::to_value(&*self.config).unwrap_or_default();
        set_dot_path(&mut json, &path, value);
        match serde_json::from_value::<crate::config::Config>(json) {
            Ok(new_config) => {
                let old_theme = self.config.theme.clone();
                self.config = Arc::new(new_config);
                if old_theme != self.config.theme {
                    if let Some(theme) = self.theme_registry.get_cloned(&self.config.theme) {
                        *self.theme.write().unwrap() = theme;
                    }
                }
                *self.keybindings.write().unwrap() =
                    crate::input::keybindings::KeybindingResolver::new(&self.config);
                self.clipboard.apply_config(&self.config.clipboard);
                {
                    let cfg = self.config.editor.clone();
                    let win = self.active_window_mut();
                    win.menu_bar_visible = cfg.show_menu_bar;
                    win.tab_bar_visible = cfg.show_tab_bar;
                    win.status_bar_visible = cfg.show_status_bar;
                    win.prompt_line_visible = cfg.show_prompt_line;
                }
                #[cfg(feature = "plugins")]
                self.update_plugin_state_snapshot();
            }
            Err(e) => {
                self.set_status_message(format!("setSetting({path}): {e}"));
            }
        }
    }

    /// Apply the result of one async startup-batch directory load.
    /// Mirrors the per-iteration body of the legacy synchronous loop in
    /// `with_options`: merge discovered plugins into config, log errors,
    /// and panic in debug builds (the legacy behaviour).
    pub(crate) fn handle_plugins_dir_loaded(
        &mut self,
        dir: std::path::PathBuf,
        errors: Vec<String>,
        discovered_plugins: std::collections::HashMap<String, fresh_core::config::PluginConfig>,
    ) {
        if !discovered_plugins.is_empty() {
            let cfg = std::sync::Arc::make_mut(&mut self.config);
            for (name, plugin_config) in discovered_plugins {
                cfg.plugins.insert(name, plugin_config);
            }
        }
        if !errors.is_empty() {
            for err in &errors {
                tracing::error!("TypeScript plugin load error: {}", err);
            }
            #[cfg(debug_assertions)]
            panic!(
                "TypeScript plugin loading failed for {:?} with {} error(s): {}",
                dir,
                errors.len(),
                errors.join("; ")
            );
            #[cfg(not(debug_assertions))]
            {
                let _ = dir;
            }
        }
    }

    /// Apply the declarations harvested at the end of the async startup
    /// batch. Mirrors the synchronous `plugin_declarations` +
    /// `write_plugin_declarations` pair in `with_options`.
    pub(crate) fn handle_plugin_declarations_ready(&self, declarations: Vec<(String, String)>) {
        crate::init_script::write_plugin_declarations(&self.dir_context.config_dir, &declarations);
    }

    /// Apply the result of the async `init.ts` load. Mirrors the trailing
    /// `match outcome { ... }` block of the legacy synchronous
    /// `load_init_script`.
    pub(crate) fn handle_plugin_init_script_loaded(
        &mut self,
        outcome: crate::services::async_bridge::PluginInitScriptOutcome,
    ) {
        use crate::init_script::{describe, record_success, InitOutcome};
        use crate::services::async_bridge::PluginInitScriptOutcome as O;
        let outcome = match outcome {
            O::NotFound => InitOutcome::NotFound,
            O::Disabled => InitOutcome::Disabled,
            O::CrashFused { failures } => InitOutcome::CrashFused { failures },
            O::Loaded => {
                record_success(&self.dir_context.config_dir);
                InitOutcome::Loaded
            }
            O::Failed { message } => InitOutcome::Failed { message },
        };
        let summary = describe(&outcome);
        match outcome {
            InitOutcome::NotFound | InitOutcome::Disabled => tracing::debug!("{}", summary),
            InitOutcome::Loaded => tracing::info!("{}", summary),
            InitOutcome::CrashFused { .. } | InitOutcome::Failed { .. } => {
                tracing::warn!("{}", summary);
                self.set_status_message(summary);
            }
        }
    }

    /// Fire the `plugins_loaded` hook (design M2, §3.3 phase 2).
    pub fn fire_plugins_loaded_hook(&self) {
        #[cfg(feature = "plugins")]
        if self.plugin_manager.read().unwrap().is_active() {
            self.plugin_manager.read().unwrap().run_hook(
                "plugins_loaded",
                crate::services::plugins::hooks::HookArgs::PluginsLoaded {},
            );
        }
    }

    /// Fire the `ready` hook (design M2, §3.3 phase 3).
    pub fn fire_ready_hook(&self) {
        #[cfg(feature = "plugins")]
        if self.plugin_manager.read().unwrap().is_active() {
            self.plugin_manager
                .read()
                .unwrap()
                .run_hook("ready", crate::services::plugins::hooks::HookArgs::Ready {});
        }
    }

    /// Test-only accessor for the current effective config.
    #[doc(hidden)]
    pub fn config_for_tests(&self) -> &crate::config::Config {
        &self.config
    }

    /// Test-only shim that dispatches an action through the normal path.
    #[doc(hidden)]
    pub fn dispatch_action_for_tests(&mut self, action: crate::input::keybindings::Action) {
        if let Err(e) = self.handle_action(action) {
            tracing::warn!("dispatch_action_for_tests: {e}");
        }
    }

    /// Test-only accessor for the Live Grep Resume cache (issue #1796).
    #[doc(hidden)]
    pub fn live_grep_last_state_for_tests(
        &self,
    ) -> Option<&crate::services::live_grep_state::LiveGrepLastState> {
        self.active_window().live_grep_last_state.as_ref()
    }

    /// Test-only setter for the Live Grep Resume cache.
    #[doc(hidden)]
    pub fn set_live_grep_last_state_for_tests(
        &mut self,
        state: Option<crate::services::live_grep_state::LiveGrepLastState>,
    ) {
        self.active_window_mut().live_grep_last_state = state;
    }

    /// Test-only accessor for the split tree, so layout-shape
    /// regression tests can assert on the structure directly.
    #[doc(hidden)]
    pub fn split_manager_for_tests(&self) -> &crate::view::split::SplitManager {
        self.windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(mgr, _)| mgr)
            .expect("active window must have a populated split layout")
    }

    /// Test-only accessor for a leaf's `SplitViewState`, so tab-list
    /// regression tests can verify which buffers are open in a given
    /// pane (the dock should only contain the buffer the user
    /// actually asked for, not phantom placeholders).
    #[doc(hidden)]
    pub fn split_view_state_for_tests(
        &self,
        leaf: crate::model::event::LeafId,
    ) -> Option<&crate::view::split::SplitViewState> {
        self.windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&leaf)
    }

    /// Refresh the plugin-readable keybinding-label snapshot from
    /// the current keymap. Call this whenever a plugin is about to
    /// surface key hints in its UI (overlay headers, tooltips,
    /// menus) so the user's most-recent rebinds are reflected.
    ///
    /// Cheap — walks every typed `Action` × ~9 contexts; runs in
    /// well under a millisecond on this hardware. Cheaper than
    /// adding refresh hooks to every keymap-mutation site.
    #[cfg(feature = "plugins")]
    pub(crate) fn refresh_keybinding_labels_snapshot(&self) {
        if let Some(snapshot_handle) = self.plugin_manager.read().unwrap().state_snapshot_handle() {
            if let Ok(mut snapshot) = snapshot_handle.write() {
                populate_builtin_keybinding_labels(&mut snapshot, &self.keybindings);
            }
        }
    }
}

/// Walk every typed `Action` and the contexts most relevant to UI
/// labels (`Normal`, `Prompt`, `Popup`, `FileExplorer`,
/// `CompositeBuffer`, `Settings`, `Terminal`), and populate the
/// snapshot's `keybinding_labels` map with `<action>\0<context>` →
/// formatted label (e.g. `"cycle_live_grep_provider\0prompt"` →
/// `"Alt+P"`). The plugin-side `editor.getKeybindingLabel(action,
/// mode)` API reads from this map, so plugins displaying hints in
/// their UIs (overlay headers, status messages) can look up the
/// user's *actual* binding rather than hardcoding a key string.
///
/// This runs once at startup. If the user later edits their keymap
/// without restarting fresh, the labels go stale. That's acceptable
/// for v1 — keymap edits today already require a restart for full
/// effect; a subsequent commit can wire snapshot refresh into the
/// keymap-reload path.
#[cfg(feature = "plugins")]
fn populate_builtin_keybinding_labels(
    snapshot: &mut crate::services::plugins::api::EditorStateSnapshot,
    keybindings: &std::sync::Arc<std::sync::RwLock<crate::input::keybindings::KeybindingResolver>>,
) {
    use crate::input::keybindings::{Action, KeyContext};
    let Ok(resolver) = keybindings.read() else {
        return;
    };
    let contexts = [
        KeyContext::Normal,
        KeyContext::Prompt,
        KeyContext::Popup,
        KeyContext::Completion,
        KeyContext::FileExplorer,
        KeyContext::Menu,
        KeyContext::Terminal,
        KeyContext::Settings,
        KeyContext::CompositeBuffer,
    ];
    // Clear stale built-in entries first so a re-populate after
    // the user un-binds an action drops the label rather than
    // leaving the old key visible. Entries whose `\0<context>`
    // suffix isn't in our list are left alone — those belong to
    // plugin-defined buffer modes and have their own
    // re-population path in `handle_register_mode`.
    let known_suffixes: Vec<String> = contexts
        .iter()
        .map(|c| format!("\0{}", c.to_when_clause()))
        .collect();
    snapshot
        .keybinding_labels
        .retain(|k, _| !known_suffixes.iter().any(|s| k.ends_with(s)));
    for action_name in Action::all_action_names() {
        for ctx in &contexts {
            if let Some(label) = resolver.find_keybinding_for_action(&action_name, ctx.clone()) {
                let key = format!("{}\0{}", action_name, ctx.to_when_clause());
                snapshot.keybinding_labels.insert(key, label);
            }
        }
    }
}
