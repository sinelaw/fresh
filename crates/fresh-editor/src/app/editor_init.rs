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

/// Discover startup plugin directories and load every plugin found in them.
///
/// Extracted from `Editor::with_options` to keep the constructor readable:
/// this owns the whole "where do plugins live and how do we load them"
/// concern. Directory discovery walks (in priority order) the embedded set,
/// the user `plugins/` dir, package-manager installs, and bundle dirs from
/// the package scan. Loading then takes one of two paths:
///
/// * `defer_plugin_load` (async startup): submit each dir to the plugin
///   thread and let a forwarder thread translate the results into
///   `AsyncMessage`s the main loop applies later.
/// * otherwise (sync / test / server): load each dir inline, merge the
///   discovered plugin configs back into `config`, and write the aggregate
///   `.d.ts` declarations.
///
/// No-op when the plugin manager is inactive.
#[allow(clippy::too_many_arguments)]
fn load_startup_plugins(
    plugin_manager: &Arc<RwLock<PluginManager>>,
    dir_context: &DirectoryContext,
    bundle_plugin_dirs: &[std::path::PathBuf],
    config: &mut Config,
    #[cfg_attr(not(feature = "plugins"), allow(unused_variables))] async_bridge: &AsyncBridge,
    working_dir: &std::path::Path,
    #[cfg_attr(not(feature = "embed-plugins"), allow(unused_variables))]
    enable_embedded_plugins: bool,
    defer_plugin_load: bool,
) {
    if !plugin_manager.read().unwrap().is_active() {
        return;
    }
    let mut plugin_dirs: Vec<std::path::PathBuf> = vec![];

    // Embedded plugins. `enable_embedded_plugins` lets tests opt out so
    // they get exactly the plugin set they pre-populated under
    // `<config_dir>/plugins/`, without the bundled set leaking in.
    #[cfg(feature = "embed-plugins")]
    if enable_embedded_plugins && plugin_dirs.is_empty() {
        if let Some(embedded_dir) = crate::services::plugins::embedded::get_embedded_plugins_dir() {
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
    for dir in bundle_plugin_dirs {
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
            let bridge = async_bridge;
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
        crate::init_script::write_plugin_declarations(&dir_context.config_dir, &declarations);
    }
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
    pub(super) local_filesystem: Arc<dyn FileSystem + Send + Sync>,

    // Chrome flags resolved from config

    // Windows — the whole point of the split: the factory builds these
    // (from disk persistence or a single seed window), the constructor
    // just installs them.
    pub(super) windows: HashMap<fresh_core::WindowId, crate::app::window::Window>,
    /// Persisted remote (SSH / kube) sessions discovered at boot, kept as
    /// authority-less descriptors rather than placeholder-authority shell
    /// windows. Promoted to real windows on first dive (after connect).
    pub(super) dormant_remote:
        HashMap<fresh_core::WindowId, crate::app::orchestrator_persistence::PersistedWindow>,
    pub(super) active_window: fresh_core::WindowId,
    pub(super) next_window_id: u64,

    // Registries / managers
    pub(super) command_registry: Arc<RwLock<CommandRegistry>>,
    pub(super) quick_open_registry: QuickOpenRegistry,
    pub(super) plugin_manager: Arc<RwLock<PluginManager>>,
    pub(super) recovery_service: Arc<std::sync::Mutex<RecoveryService>>,
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

    /// Per-plugin config schemas discovered from `<plugin>.schema.json` sidecars.
    pub(super) plugin_schemas: HashMap<String, serde_json::Value>,

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
            last_layout_signature: None,
            tokio_runtime: parts.tokio_runtime,
            async_bridge: Some(parts.async_bridge),
            paste_pending: std::collections::HashMap::new(),
            paste_slow_path_just_armed: false,
            paste_render_suppress_until: None,
            system_clipboard_reader: None,
            local_filesystem: parts.local_filesystem,
            menu_state: crate::view::ui::MenuState::new(parts.dir_context.themes_dir()),
            windows: parts.windows,
            dormant_remote: parts.dormant_remote,
            session_keepalives: HashMap::new(),
            remote_attach_inflight: std::collections::HashSet::new(),
            remote_attach_cancelled: std::collections::HashSet::new(),
            remote_attach_cancels: std::collections::HashMap::new(),
            active_window: parts.active_window,
            next_window_id: parts.next_window_id,
            window_cycle_order: None,
            command_registry: parts.command_registry,
            quick_open_registry: parts.quick_open_registry,
            plugin_manager: parts.plugin_manager,
            recovery_service: parts.recovery_service,
            time_source: parts.time_source,
            color_capability: parts.color_capability,
            update_checker: parts.update_checker,
            key_translator: parts.key_translator,

            // Trivial defaults (no external dependencies):
            remote_reconnect_forwarders: std::collections::HashSet::new(),
            materialize_pending: std::collections::HashSet::new(),
            grammar_reload_pending: false,
            grammar_build_in_progress: false,
            pending_grammar_callbacks: Vec::new(),
            expanded_menus_cache: crate::view::ui::ExpandedMenusCache::default(),
            ansi_background: None,
            ansi_background_path: None,
            background_fade: crate::primitives::ansi_background::DEFAULT_BACKGROUND_FADE,
            clipboard: crate::services::clipboard::Clipboard::new(),
            should_quit: false,
            workspace_trust_prompt_cancellable: false,
            workspace_trust_markers: Vec::new(),
            workspace_trust_scroll: 0,
            should_detach: false,
            session_mode: false,
            software_cursor_only: false,
            session_name: None,
            pending_escape_sequences: Vec::new(),
            restart_with_dir: None,
            last_window_title: None,
            mode_registry: ModeRegistry::new(),
            pending_authority: None,
            pending_keepalive: None,
            remote_indicator_override: None,
            menus: crate::config::MenuConfig::translated(),
            background_process_handles: HashMap::new(),
            host_process_handles: HashMap::new(),
            status_bar_token_registry: Mutex::new(HashMap::new()),
            plugin_schemas: std::sync::Arc::new(std::sync::RwLock::new(parts.plugin_schemas)),
            event_broadcaster: parts.event_broadcaster,
            #[cfg(feature = "plugins")]
            pending_plugin_actions: Vec::new(),
            #[cfg(feature = "plugins")]
            plugin_render_requested: false,
            full_redraw_requested: false,
            suppress_chrome_cells: false,
            suspend_requested: false,
            plugin_global_state: parts.plugin_global_state,
            warning_log: None,
            status_log_path: None,
            #[cfg(feature = "plugins")]
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
            dock: None,
            dock_width: None,
            dock_resizing: false,
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
        // Convenience constructor (tests, and any caller that only has a
        // filesystem to inject): the editor's real authority *is* a local one
        // backed by that filesystem. Build it here so the editor is still
        // constructed with the authority it runs under — production callers
        // that own a non-local authority pass it straight to
        // `with_working_dir_opts` instead.
        let authority = Self::local_authority_with_filesystem(filesystem);
        Self::with_working_dir_opts(
            config,
            width,
            height,
            working_dir,
            dir_context,
            plugins_enabled,
            color_capability,
            authority,
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
        authority: crate::services::authority::Authority,
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
        crate::config::reload_indent_overrides(&config.languages);
        tracing::info!("Default grammar registry built in {:?}", start.elapsed());
        // Don't start background grammar build here — it's deferred to the
        // first flush_pending_grammars() call so that plugin-registered grammars
        // from the first event-loop tick are included in a single build.
        Self::with_options(
            config,
            width,
            height,
            working_dir,
            authority,
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
        crate::config::reload_indent_overrides(&config.languages);
        let authority = Self::local_authority_with_filesystem(filesystem);
        let mut editor = Self::with_options(
            config,
            width,
            height,
            working_dir,
            authority,
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

    /// Build a local authority whose filesystem is the supplied one.
    ///
    /// The bridge for callers that only have a `FileSystem` to inject (the
    /// `new` / `with_working_dir` / `for_test` convenience constructors): a
    /// local-backed authority *is* the real authority such an editor runs
    /// under, so this is construction with the true authority, not a
    /// placeholder destined to be replaced. Carries a permissive trust and an
    /// inactive env provider — the defaults `Authority::local` uses for the
    /// host backend.
    fn local_authority_with_filesystem(
        filesystem: Arc<dyn FileSystem + Send + Sync>,
    ) -> crate::services::authority::Authority {
        crate::services::authority::Authority {
            filesystem,
            ..crate::services::authority::Authority::local(
                Arc::new(crate::services::workspace_trust::WorkspaceTrust::permissive()),
                Arc::new(crate::services::env_provider::EnvProvider::inactive()),
            )
        }
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
        authority: crate::services::authority::Authority,
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
        // The editor is constructed with the *real* authority it will run
        // under — never a local placeholder that gets replaced later (that
        // left a window where, e.g., quick-open's `git ls-files` ran through
        // the local spawner while the filesystem was already remote). The
        // filesystem is derived from it; the spawner/long-running/terminal
        // ride along on `self.authority`.
        let filesystem = std::sync::Arc::clone(&authority.filesystem);
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
        // Orchestrator persistence lives under the local `data_dir`, so it
        // must be read through the local filesystem — never the authority's
        // (which is the *remote* SSH backend on an SSH launch). Routing these
        // local-disk reads over SSH is both wrong (it queries the remote
        // host's copy of a local-machine question) and catastrophically slow
        // (one network round-trip per workspace file). The window content
        // below still flows through the authority filesystem; only this
        // editor-wide registry read is pinned local.
        let orchestrator_filesystem: Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> =
            Arc::new(crate::model::filesystem::StdFileSystem);
        tracing::debug!(
            data_dir = %dir_context.data_dir.display(),
            "editor_init: reading persisted windows env"
        );
        let persisted_env = crate::app::orchestrator_persistence::read_persisted_windows_env(
            orchestrator_filesystem.as_ref(),
            &dir_context.data_dir,
            &working_dir,
        );
        tracing::debug!("editor_init: reading persisted plugin state");
        let plugin_global_state = crate::app::orchestrator_persistence::read_persisted_plugin_state(
            orchestrator_filesystem.as_ref(),
            &dir_context.data_dir,
            &working_dir,
        );
        tracing::debug!("editor_init: persistence reads complete");

        // Reopen the session the user last used *in this project*, if
        // any — never a session from another project. Cross-project
        // restore is what dragged yesterday's directories/files into a
        // different project's window; `pick_active_window_for_cwd` only
        // ever returns a window rooted at `working_dir`, so launching
        // elsewhere can't pull this project's sessions in (and vice
        // versa). When the cwd has no sessions, fall back to a clean
        // base window (id 1) at the launch cwd. This also keeps the LSP
        // / Open-Terminal default pointed at the launch cwd (issue
        // #2026).
        let picked_active = crate::app::orchestrator_persistence::pick_active_window_for_cwd(
            persisted_env.as_ref(),
            &working_dir,
        );
        let (active_window_id, _active_window_root) = picked_active
            .map(|w| (fresh_core::WindowId(w.id), w.root.clone()))
            .unwrap_or((fresh_core::WindowId(1), working_dir.clone()));

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

        if tokio_runtime.is_none() {
            tracing::warn!("Failed to create Tokio runtime - async features disabled");
        }

        // The base window's LSP manager is built by `Window::new`
        // (rooted at the window's root, wired to its own bridge), just
        // like every other window — there is no special boot-time LSP
        // construction here anymore. See `build_window_lsp`.

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
            config.editor.scroll_offset,
        );
        split_view_states.insert(initial_split_id, initial_view_state);

        // Initialize filesystem manager for file explorer
        let fs_manager = Arc::new(FsManager::new(Arc::clone(&filesystem)));

        // Initialize command registry (always available, used by both plugins and core)
        let command_registry = Arc::new(RwLock::new(CommandRegistry::new()));

        // The authority is the *real* one this editor runs under, handed in
        // by the caller — not a local placeholder swapped out later. Every
        // backend-derived seam below (quick-open's file provider, the LSP
        // spawner, each window's `resources.authority`) is wired from it at
        // construction, so there is no window in which, e.g., quick-open's
        // `git ls-files` runs through a local spawner while the filesystem is
        // already remote. Runtime authority transitions still go through the
        // destructive `install_authority` restart (principle 7), which
        // rebuilds the editor with the next authority via this same path.
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
            // Seed the snapshot's `config` view with the resolved
            // initial config so plugins reading
            // `editor.getPluginConfig()` (and the lower-level
            // `defineConfigX` snapshot-lookups) see user-set values
            // on their very first call. Without this seed the
            // synchronous test path runs plugin scripts BEFORE the
            // first `update_plugin_state_snapshot` tick, so a
            // preset `plugins.<name>.settings.<field>` is invisible
            // to the plugin until much later — defeating any
            // "react to user config at startup" pattern (e.g.
            // vi_mode's `autoStart`).
            if let Ok(json) = serde_json::to_value(&config) {
                snapshot.config = std::sync::Arc::new(json);
            }
        }

        // Plugin schemas populated lazily by plugins calling
        // `editor.definePluginConfig(...)` at load time. See
        // `handle_register_plugin_config_schema`.
        let plugin_schemas: HashMap<String, serde_json::Value> = HashMap::new();

        // Discover plugin directories and load every plugin (see the helper for
        // the discovery order and the async-vs-sync load paths).
        load_startup_plugins(
            &plugin_manager,
            &dir_context,
            &scan_result.bundle_plugin_dirs,
            &mut config,
            &async_bridge,
            &working_dir,
            enable_embedded_plugins,
            defer_plugin_load,
        );

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

        // The local-host filesystem handle, shared with the base window's
        // `WindowResources` and the editor. Same `Arc` the orchestrator
        // persistence read above used — local state has one local backend.
        let local_filesystem = Arc::clone(&orchestrator_filesystem);

        // Hot-exit recovery service, shared (Arc<Mutex>) into every
        // `Window` via `WindowResources` so per-window restore/auto-save
        // can reach it without an active-window flip.
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
            std::sync::Arc::new(std::sync::Mutex::new(RecoveryService::with_scope(
                recovery_config,
                &dir_context.recovery_dir(),
                &scope,
            )))
        };

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
            time_source: Arc::clone(&time_source),
            dir_context: dir_context.clone(),
            tokio_runtime: tokio_runtime.clone(),
            async_bridge: Some(async_bridge.clone()),
            plugin_manager: Arc::clone(&plugin_manager),
            theme: Arc::clone(&theme),
            event_broadcaster: event_broadcaster.clone(),
            recovery_service: Arc::clone(&recovery_service),
        };

        // Build the active window — the one that holds the seed
        // buffer, the SplitManager, the LSP, and the
        // already-configured per-window bridge. Its label / root /
        // plugin state come from the persisted session we chose to
        // reopen (the last-used one for this cwd). When there was none
        // we boot a clean base: empty label, cwd root, no inherited
        // state. We deliberately key off the *picked* window, not a
        // lookup by `active_window_id` — a clean base reuses id 1, and
        // a stale persisted id-1 window (a different project's old
        // base) must not lend its label/root/state to it.
        let (active_label, active_root, active_plugin_state, active_authority_spec) = picked_active
            .map(|w| {
                (
                    w.label.clone(),
                    w.root.clone(),
                    w.plugin_state.clone(),
                    w.authority_spec.clone(),
                )
            })
            .unwrap_or_else(|| {
                (
                    String::new(),
                    working_dir.clone(),
                    HashMap::new(),
                    crate::services::authority::SessionAuthoritySpec::Local,
                )
            });
        // A plain `fresh ssh://…` (or `user@host:path`) launch installs a remote
        // authority but has no persisted window to carry a spec, so the default
        // above is `Local` — which left persistence and manual reconnect inert
        // for CLI remote sessions. Derive the real spec from the live authority
        // when the resolved one is `Local` but the backend isn't. Never
        // downgrades an already-remote spec (e.g. a restored dormant session
        // booted on a local placeholder), since that path is `RemoteAgent` here.
        let active_authority_spec = match active_authority_spec {
            crate::services::authority::SessionAuthoritySpec::Local => authority.session_spec(),
            spec => spec,
        };

        // The active window owns the editor's boot authority outright — moved
        // in, not cloned (there is no editor-wide copy).
        let mut active_win = crate::app::window::Window::new(
            active_window_id,
            active_label,
            active_root,
            authority,
            base_resources,
        );
        // Seed the window's terminal dimensions from the editor's
        // initial size — `Window::new` defaults to 80x24, which is
        // wrong for any harness that constructs the editor at a
        // different size (issue surfaces in
        // test_hidden_terminal_resyncs_pty_size_when_revealed).
        active_win.terminal_width = width;
        active_win.terminal_height = height;
        // Install the initial split layout. The LSP manager and per-
        // window bridge were already built by `Window::new` (rooted at
        // this window's root, wired together), so there's nothing to
        // hand off here — every window owns its manager by construction.
        active_win.buffers = buffers;
        active_win
            .buffers
            .set_splits((split_manager, split_view_states));
        active_win.buffer_metadata = buffer_metadata;
        active_win.event_logs = event_logs;
        active_win.plugin_state = active_plugin_state;
        active_win.authority_spec = active_authority_spec;
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
        // Background (restored, non-active) windows are distinct projects
        // and do NOT inherit the active session's backend: when this
        // construction is an `install_authority` restart (the editor is
        // rebuilt with a container/SSH/k8s `authority` re-rooted at the
        // active project), fanning that authority onto every restored shell
        // is exactly the bug where switching to another project via the
        // Orchestrator dock kept acting through the devcontainer. Each shell
        // gets its own local authority (sharing trust + env) and a matching
        // local `fs_manager` so its file explorer reads the host, not the
        // active session's remote/container backend. The active window keeps
        // `authority` (wired into `base_resources` above). Each shell's
        // authority — with its **own** per-session trust scoped to its root —
        // is built in the loop below so trusting the active project never
        // raises another session's trust level.
        let background_fs_manager = Arc::new(FsManager::new(Arc::new(
            crate::model::filesystem::StdFileSystem,
        )));
        let mut windows = HashMap::new();
        let mut dormant_remote: HashMap<
            fresh_core::WindowId,
            crate::app::orchestrator_persistence::PersistedWindow,
        > = HashMap::new();
        if let Some(ref env) = persisted_env {
            // The active window came from a real pick when `picked_active`
            // is `Some` — its persisted entry must NOT also become a shell.
            // When the pick found nothing we synthesized a clean base at
            // `WindowId(1)` (the base is always id 1); a global
            // `windows.json` may already hold a *different* project's id-1
            // base, which would collide. Re-id that collider onto a fresh
            // id so it survives as an inactive shell instead of being
            // shadowed/dropped (issue #2056 cross-project case).
            let active_came_from_pick = picked_active.is_some();
            let active_root_key =
                crate::app::orchestrator_persistence::canonical_key(&active_win.root);
            let mut next_fresh_id = env
                .next_id
                .max(env.windows.iter().map(|w| w.id).max().unwrap_or(0) + 1)
                .max(active_window_id.0 + 1);
            for ps in &env.windows {
                if active_came_from_pick && ps.id == active_window_id.0 {
                    continue;
                }
                // One session per directory: never seed a shell that
                // resolves to the active window's own directory (the
                // clean-base case where the cwd has a stale persisted
                // window the pick didn't claim).
                if crate::app::orchestrator_persistence::canonical_key(&ps.root) == active_root_key
                {
                    continue;
                }
                let id = if ps.id == active_window_id.0 {
                    let fresh = fresh_core::WindowId(next_fresh_id);
                    next_fresh_id += 1;
                    fresh
                } else {
                    fresh_core::WindowId(ps.id)
                };
                // Remote (SSH / kube) sessions are NOT built as windows here.
                // A `Window` must own its session's real authority, and a remote
                // backend doesn't exist until it connects — building one now
                // would require a dummy local-placeholder authority (the old
                // shell), the "local before, remote later" pattern that ran
                // restored terminals on the local host. Keep them as
                // authority-less dormant descriptors instead: listed in the dock
                // (via the `WindowInfo` snapshot) and promoted to a real window,
                // born with the connected authority, on first dive (see
                // `bring_dormant_remote_online`).
                if matches!(
                    ps.authority_spec,
                    crate::services::authority::SessionAuthoritySpec::RemoteAgent(_)
                ) {
                    let mut descriptor = ps.clone();
                    descriptor.id = id.0;
                    dormant_remote.insert(id, descriptor);
                    continue;
                }
                // This shell's own local authority, gated by its own
                // per-session trust + env (scoped to its root + project store)
                // — never a clone of the active session's handles.
                let shell_authority = crate::services::authority::Authority::local_scoped(
                    crate::services::authority::SessionScope::for_root(
                        &ps.root,
                        &dir_context.project_state_dir(&ps.root),
                    ),
                );
                let resources = crate::app::window_resources::WindowResources {
                    config: Arc::clone(&config_arc),
                    grammar_registry: Arc::clone(&grammar_registry),
                    theme_registry: Arc::clone(&theme_registry),
                    theme_cache: Arc::clone(&theme_cache),
                    keybindings: Arc::clone(&keybindings),
                    command_registry: Arc::clone(&command_registry),
                    fs_manager: Arc::clone(&background_fs_manager),
                    local_filesystem: Arc::clone(&local_filesystem),
                    buffer_id_alloc: buffer_id_alloc.clone(),
                    time_source: Arc::clone(&time_source),
                    dir_context: dir_context.clone(),
                    tokio_runtime: tokio_runtime.clone(),
                    async_bridge: Some(async_bridge.clone()),
                    plugin_manager: Arc::clone(&plugin_manager),
                    theme: Arc::clone(&theme),
                    event_broadcaster: event_broadcaster.clone(),
                    recovery_service: Arc::clone(&recovery_service),
                };
                let mut shell = crate::app::window::Window::new(
                    id,
                    ps.label.clone(),
                    ps.root.clone(),
                    shell_authority,
                    resources,
                );
                shell.terminal_width = width;
                shell.terminal_height = height;
                shell.plugin_state = ps.plugin_state.clone();
                // Carry the session's backend spec so an unmaterialized
                // background remote session keeps its identity (and a later
                // save doesn't clobber it back to local). Its live authority
                // stays the local placeholder until reconnect — i.e. dormant.
                shell.authority_spec = ps.authority_spec.clone();
                windows.insert(id, shell);
            }
        }
        windows.insert(active_window_id, active_win);

        // Allocate next window ids past every persisted entry and
        // past our active id, so `createWindow` after restart never
        // collides with an id the user might still see in plugin
        // state. Falls back to 2 (the post-base-window default)
        // when there's no persistence.
        let max_existing = windows
            .keys()
            .chain(dormant_remote.keys())
            .map(|k| k.0)
            .max()
            .unwrap_or(0);
        let next_window_id = persisted_env
            .as_ref()
            .map(|env| env.next_id.max(max_existing + 1))
            .unwrap_or(2);

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
            local_filesystem: Arc::clone(&local_filesystem),
            windows,
            dormant_remote,
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
            plugin_schemas,
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

        // Lazy materialization: every non-active window keeps only its
        // empty seed layout for now and is restored from disk on first
        // dive/preview (see `materialize_window`). Only the foreground
        // (CLI-dir) window is restored eagerly, by the caller's
        // `try_restore_workspace`.
        editor.materialize_pending = editor
            .windows
            .keys()
            .copied()
            .filter(|id| *id != editor.active_window)
            .collect();

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
                        self.start_theme_transition_animation();
                    }
                }
                // Keep plugin-contributed bindings alive across the reload (#2307).
                self.keybindings
                    .write()
                    .unwrap()
                    .reload_from_config(&self.config);
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

    /// Append a single config field to a plugin's accumulated schema and
    /// pre-populate its default value. Each `defineConfigX(...)` call
    /// from the plugin's TS code fires one of these.
    ///
    /// On first call for a plugin we synthesise a fresh
    /// `{"type": "object", "properties": {}}` schema and grow it as more
    /// fields arrive. Re-registering the same `field_name` overwrites
    /// the previous definition (which is what we want on plugin
    /// reload — plugins re-run their `defineConfigX` calls).
    pub fn handle_add_plugin_config_field(
        &mut self,
        plugin_name: String,
        field_name: String,
        field_schema: serde_json::Value,
    ) {
        tracing::trace!(
            "Registering plugin config field: {}.{}",
            plugin_name,
            field_name
        );
        // Merge the new field into the existing accumulated schema (or a
        // fresh one) and run the same strict validation as a bulk-register.
        let updated_schema = {
            let schemas = self.plugin_schemas.read().ok();
            let existing = schemas.as_ref().and_then(|m| m.get(&plugin_name)).cloned();
            let mut schema = existing.unwrap_or_else(|| {
                serde_json::json!({
                    "type": "object",
                    "properties": {},
                })
            });
            if let Some(props) = schema
                .as_object_mut()
                .and_then(|o| o.get_mut("properties"))
                .and_then(|p| p.as_object_mut())
            {
                props.insert(field_name.clone(), field_schema.clone());
            }
            schema
        };

        if let Err(msg) = crate::plugin_schemas::validate_plugin_schema(&updated_schema) {
            // Field passed JS-side validation but somehow broke the full
            // schema — log and skip so we don't poison the registry.
            self.set_status_message(format!(
                "defineConfig({}.{}): {}",
                plugin_name, field_name, msg
            ));
            return;
        }

        // Pre-populate the default for THIS field only.
        if let Some(default) = field_schema.get("default").cloned() {
            let cfg = std::sync::Arc::make_mut(&mut self.config);
            let entry = cfg.plugins.entry(plugin_name.clone()).or_default();
            let settings_obj = match &mut entry.settings {
                serde_json::Value::Object(_) => &mut entry.settings,
                slot => {
                    *slot = serde_json::Value::Object(Default::default());
                    slot
                }
            };
            if let serde_json::Value::Object(map) = settings_obj {
                map.entry(field_name.clone()).or_insert(default);
            }
        }

        if let Ok(mut schemas) = self.plugin_schemas.write() {
            schemas.insert(plugin_name, updated_schema);
        }

        #[cfg(feature = "plugins")]
        self.update_plugin_state_snapshot();
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

    /// Test-only accessor for the keybinding resolver (issue #2307), so
    /// tests can register plugin-mode bindings and assert how they resolve
    /// after config-triggered resolver rebuilds.
    #[doc(hidden)]
    pub fn keybindings_for_tests(
        &self,
    ) -> std::sync::Arc<std::sync::RwLock<crate::input::keybindings::KeybindingResolver>> {
        self.keybindings.clone()
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
    // Built-in actions plus any plugin actions that are actually bound
    // (e.g. the Universal Search scope toggles `live_grep_toggle_*`), so
    // `getKeybindingLabel` can resolve a plugin control's accelerator.
    let plugin_action_names = resolver.bound_plugin_action_names();
    let action_names = Action::all_action_names()
        .into_iter()
        .chain(plugin_action_names);
    for action_name in action_names {
        for ctx in &contexts {
            if let Some(label) = resolver.find_keybinding_for_action(&action_name, ctx.clone()) {
                let key = format!("{}\0{}", action_name, ctx.to_when_clause());
                snapshot.keybinding_labels.insert(key, label);
            }
        }
    }
}
