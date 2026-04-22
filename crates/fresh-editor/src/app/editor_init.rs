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

impl Editor {
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
            dir_context,
            None,
            color_capability,
            grammar_registry,
        )
    }

    /// Create a new editor for testing with custom backends
    ///
    /// By default uses empty grammar registry for fast initialization.
    /// Pass `Some(registry)` for tests that need syntax highlighting or shebang detection.
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
            true,
            dir_context,
            time_source,
            color_capability,
            grammar_registry,
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
        dir_context: DirectoryContext,
        time_source: Option<SharedTimeSource>,
        color_capability: crate::view::color_support::ColorCapability,
        grammar_registry: Arc<crate::primitives::grammar::GrammarRegistry>,
    ) -> AnyhowResult<Self> {
        // Use provided time_source or default to RealTimeSource
        let time_source = time_source.unwrap_or_else(RealTimeSource::shared);
        tracing::info!("Editor::new called with width={}, height={}", width, height);

        // Use provided working_dir or capture from environment
        let working_dir = working_dir
            .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

        // Canonicalize working_dir to resolve symlinks and normalize path components
        // This ensures consistent path comparisons throughout the editor
        let working_dir = working_dir.canonicalize().unwrap_or(working_dir);

        // Load all themes into registry
        tracing::info!("Loading themes...");
        let theme_loader = crate::view::theme::ThemeLoader::new(dir_context.themes_dir());
        // Scan installed packages (language packs + bundles) before plugin loading.
        // This replaces the JS loadInstalledPackages() — configs, grammars, plugin dirs,
        // and theme dirs are all collected here and applied synchronously.
        let scan_result =
            crate::services::packages::scan_installed_packages(&dir_context.config_dir);

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

        let theme_registry = theme_loader.load_all(&scan_result.bundle_theme_dirs);
        tracing::info!("Themes loaded");

        // Get active theme from registry, falling back to default if not found
        let theme = theme_registry.get_cloned(&config.theme).unwrap_or_else(|| {
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
        theme.set_terminal_cursor_color();

        let keybindings = Arc::new(RwLock::new(KeybindingResolver::new(&config)));

        // Create an empty initial buffer
        let mut buffers = HashMap::new();
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

        // Create metadata for the initial empty buffer
        let mut buffer_metadata = HashMap::new();
        buffer_metadata.insert(buffer_id, BufferMetadata::new());

        // Initialize LSP manager with current working directory as root
        let root_uri = types::file_path_to_lsp_uri(&working_dir);

        // Create Tokio runtime for async I/O (LSP, file watching, git, etc.)
        let tokio_runtime = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(2) // Small pool for I/O tasks
            .thread_name("editor-async")
            .enable_all()
            .build()
            .ok();

        // Create async bridge for communication
        let async_bridge = AsyncBridge::new();

        if tokio_runtime.is_none() {
            tracing::warn!("Failed to create Tokio runtime - async features disabled");
        }

        // Create LSP manager with async support
        let mut lsp = LspManager::new(root_uri);

        // Configure runtime and bridge if available
        if let Some(ref runtime) = tokio_runtime {
            lsp.set_runtime(runtime.handle().clone(), async_bridge.clone());
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
        // workspace root, override JS/TS LSP to use `deno lsp` (#1191)
        if working_dir.join("deno.json").exists() || working_dir.join("deno.jsonc").exists() {
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

        // Initialize plugin manager (handles both enabled and disabled cases internally)
        let plugin_manager = PluginManager::new(
            enable_plugins,
            Arc::clone(&command_registry),
            dir_context.clone(),
            Arc::clone(&theme_cache),
        );

        // Update the plugin state snapshot with working_dir BEFORE loading plugins
        // This ensures plugins can call getCwd() correctly during initialization
        #[cfg(feature = "plugins")]
        if let Some(snapshot_handle) = plugin_manager.state_snapshot_handle() {
            let mut snapshot = snapshot_handle.write().unwrap();
            snapshot.working_dir = working_dir.clone();
        }

        // Load TypeScript plugins from multiple directories:
        // 1. Next to the executable (for cargo-dist installations)
        // 2. In the working directory (for development/local usage)
        // 3. From embedded plugins (for cargo-binstall, when embed-plugins feature is enabled)
        // 4. User plugins directory (~/.config/fresh/plugins)
        // 5. Package manager installed plugins (~/.config/fresh/plugins/packages/*)
        if plugin_manager.is_active() {
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

            // Then check working directory (for development)
            let working_plugin_dir = working_dir.join("plugins");
            if working_plugin_dir.exists() && !plugin_dirs.contains(&working_plugin_dir) {
                plugin_dirs.push(working_plugin_dir);
            }

            // If no disk plugins found, try embedded plugins (cargo-binstall builds)
            #[cfg(feature = "embed-plugins")]
            if plugin_dirs.is_empty() {
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

            // Load from all found plugin directories, respecting config
            for plugin_dir in plugin_dirs {
                tracing::info!("Loading TypeScript plugins from: {:?}", plugin_dir);
                let (errors, discovered_plugins) =
                    plugin_manager.load_plugins_from_dir_with_config(&plugin_dir, &config.plugins);

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
            let declarations = plugin_manager.plugin_declarations();
            crate::init_script::write_plugin_declarations(&dir_context.config_dir, &declarations);
        }

        // Extract config values before moving config into the struct
        let file_explorer_width = config.file_explorer.width;
        let recovery_enabled = config.editor.recovery_enabled;
        let check_for_updates = config.check_for_updates;
        let show_menu_bar = config.editor.show_menu_bar;
        let show_tab_bar = config.editor.show_tab_bar;
        let show_status_bar = config.editor.show_status_bar;
        let show_prompt_line = config.editor.show_prompt_line;

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

        let mut editor = Editor {
            buffers,
            event_logs,
            next_buffer_id: 2,
            config: config_arc,
            config_snapshot_anchor,
            config_cached_json,
            user_config_raw: Arc::new(user_config_raw),
            dir_context: dir_context.clone(),
            grammar_registry,
            pending_grammars: scan_result
                .additional_grammars
                .iter()
                .map(|g| PendingGrammar {
                    language: g.language.clone(),
                    grammar_path: g.path.to_string_lossy().to_string(),
                    extensions: g.extensions.clone(),
                })
                .collect(),
            grammar_reload_pending: false,
            grammar_build_in_progress: false,
            needs_full_grammar_build: true,
            streaming_grep_cancellation: None,
            pending_grammar_callbacks: Vec::new(),
            theme,
            theme_registry,
            theme_cache,
            ansi_background: None,
            ansi_background_path: None,
            background_fade: crate::primitives::ansi_background::DEFAULT_BACKGROUND_FADE,
            keybindings,
            clipboard: crate::services::clipboard::Clipboard::new(),
            should_quit: false,
            should_detach: false,
            session_mode: false,
            software_cursor_only: false,
            session_name: None,
            pending_escape_sequences: Vec::new(),
            restart_with_dir: None,
            status_message: None,
            plugin_status_message: None,
            last_window_title: None,
            plugin_errors: Vec::new(),
            prompt: None,
            terminal_width: width,
            terminal_height: height,
            lsp: Some(lsp),
            buffer_metadata,
            mode_registry: ModeRegistry::new(),
            tokio_runtime,
            async_bridge: Some(async_bridge),
            split_manager,
            split_view_states,
            previous_viewports: HashMap::new(),
            scroll_sync_manager: ScrollSyncManager::new(),
            file_explorer: None,
            preview: None,
            suppress_position_history_once: false,
            fs_manager,
            authority,
            pending_authority: None,
            remote_indicator_override: None,
            local_filesystem: Arc::new(crate::model::filesystem::StdFileSystem),
            file_explorer_visible: false,
            file_explorer_sync_in_progress: false,
            file_explorer_width,
            pending_file_explorer_show_hidden: None,
            pending_file_explorer_show_gitignored: None,
            menu_bar_visible: show_menu_bar,
            file_explorer_decorations: HashMap::new(),
            file_explorer_decoration_cache:
                crate::view::file_tree::FileExplorerDecorationCache::default(),
            file_explorer_clipboard: None,
            menu_bar_auto_shown: false,
            tab_bar_visible: show_tab_bar,
            status_bar_visible: show_status_bar,
            prompt_line_visible: show_prompt_line,
            mouse_enabled: true,
            same_buffer_scroll_sync: false,
            mouse_cursor_position: None,
            gpm_active: false,
            key_context: KeyContext::Normal,
            menu_state: crate::view::ui::MenuState::new(dir_context.themes_dir()),
            menus: crate::config::MenuConfig::translated(),
            working_dir,
            position_history: PositionHistory::new(),
            in_navigation: false,
            next_lsp_request_id: 0,
            pending_completion_requests: HashSet::new(),
            completion_items: None,
            scheduled_completion_trigger: None,
            completion_service: crate::services::completion::CompletionService::new(),
            dabbrev_state: None,
            pending_goto_definition_request: None,
            hover: hover::HoverState::default(),
            pending_references_request: None,
            pending_references_symbol: String::new(),
            pending_signature_help_request: None,
            pending_code_actions_requests: HashSet::new(),
            pending_code_actions_server_names: HashMap::new(),
            pending_code_actions: None,
            pending_inlay_hints_requests: HashMap::new(),
            pending_folding_range_requests: HashMap::new(),
            folding_ranges_in_flight: HashMap::new(),
            folding_ranges_debounce: HashMap::new(),
            pending_semantic_token_requests: HashMap::new(),
            semantic_tokens_in_flight: HashMap::new(),
            pending_semantic_token_range_requests: HashMap::new(),
            semantic_tokens_range_in_flight: HashMap::new(),
            semantic_tokens_range_last_request: HashMap::new(),
            semantic_tokens_range_applied: HashMap::new(),
            semantic_tokens_full_debounce: HashMap::new(),
            search_state: None,
            search_namespace: crate::view::overlay::OverlayNamespace::from_string(
                "search".to_string(),
            ),
            lsp_diagnostic_namespace: crate::view::overlay::OverlayNamespace::from_string(
                "lsp-diagnostic".to_string(),
            ),
            pending_search_range: None,
            interactive_replace_state: None,
            mouse_state: MouseState::default(),
            tab_context_menu: None,
            theme_info_popup: None,
            cached_layout: CachedLayout::default(),
            command_registry,
            quick_open_registry,
            plugin_manager,
            plugin_dev_workspaces: HashMap::new(),
            seen_byte_ranges: HashMap::new(),
            panel_ids: HashMap::new(),
            buffer_groups: HashMap::new(),
            buffer_to_group: HashMap::new(),
            next_buffer_group_id: 0,
            grouped_subtrees: HashMap::new(),
            background_process_handles: HashMap::new(),
            host_process_handles: HashMap::new(),
            prompt_histories: {
                // Load prompt histories from disk if available
                let mut histories = HashMap::new();
                for history_name in ["search", "replace", "goto_line"] {
                    let path = dir_context.prompt_history_path(history_name);
                    let history = crate::input::input_history::InputHistory::load_from_file(&path)
                        .unwrap_or_else(|e| {
                            tracing::warn!("Failed to load {} history: {}", history_name, e);
                            crate::input::input_history::InputHistory::new()
                        });
                    histories.insert(history_name.to_string(), history);
                }
                histories
            },
            pending_async_prompt_callback: None,
            goto_line_preview: None,
            lsp_progress: std::collections::HashMap::new(),
            lsp_server_statuses: std::collections::HashMap::new(),
            lsp_window_messages: Vec::new(),
            lsp_log_messages: Vec::new(),
            diagnostic_result_ids: HashMap::new(),
            scheduled_diagnostic_pull: None,
            scheduled_inlay_hints_request: None,
            stored_push_diagnostics: HashMap::new(),
            stored_pull_diagnostics: HashMap::new(),
            stored_diagnostics: Arc::new(HashMap::new()),
            stored_folding_ranges: Arc::new(HashMap::new()),
            event_broadcaster: crate::model::control_event::EventBroadcaster::default(),
            bookmarks: bookmarks::BookmarkState::default(),
            search_case_sensitive: true,
            search_whole_word: false,
            search_use_regex: false,
            search_confirm_each: false,
            macros: macros::MacroState::default(),
            #[cfg(feature = "plugins")]
            pending_plugin_actions: Vec::new(),
            #[cfg(feature = "plugins")]
            plugin_render_requested: false,
            chord_state: Vec::new(),
            user_dismissed_lsp_languages: std::collections::HashSet::new(),
            auto_start_prompted_languages: std::collections::HashSet::new(),
            pending_auto_start_prompts: std::collections::HashSet::new(),
            lsp_auto_prompt_enabled: super::lsp_auto_prompt::default_enabled(),
            pending_close_buffer: None,
            auto_revert_enabled: true,
            last_auto_revert_poll: time_source.now(),
            last_file_tree_poll: time_source.now(),
            git_index_resolved: false,
            file_mod_times: HashMap::new(),
            dir_mod_times: HashMap::new(),
            pending_file_poll_rx: None,
            pending_dir_poll_rx: None,
            file_rapid_change_counts: HashMap::new(),
            file_open_state: None,
            file_browser_layout: None,
            recovery_service: {
                let recovery_config = RecoveryConfig {
                    enabled: recovery_enabled,
                    ..RecoveryConfig::default()
                };
                RecoveryService::with_config_and_dir(recovery_config, dir_context.recovery_dir())
            },
            full_redraw_requested: false,
            suspend_requested: false,
            time_source: time_source.clone(),
            last_auto_recovery_save: time_source.now(),
            last_persistent_auto_save: time_source.now(),
            active_custom_contexts: HashSet::new(),
            plugin_global_state: HashMap::new(),
            editor_mode: None,
            warning_log: None,
            status_log_path: None,
            warning_domains: WarningDomainRegistry::new(),
            update_checker,
            terminal_manager: crate::services::terminal::TerminalManager::new(),
            terminal_buffers: HashMap::new(),
            terminal_backing_files: HashMap::new(),
            terminal_log_files: HashMap::new(),
            ephemeral_terminals: std::collections::HashSet::new(),
            terminal_mode: false,
            keyboard_capture: false,
            terminal_mode_resume: std::collections::HashSet::new(),
            previous_click_time: None,
            previous_click_position: None,
            click_count: 0,
            settings_state: None,
            calibration_wizard: None,
            event_debug: None,
            keybinding_editor: None,
            key_translator: crate::input::key_translator::KeyTranslator::load_from_config_dir(
                &dir_context.config_dir,
            )
            .unwrap_or_default(),
            color_capability,
            pending_file_opens: Vec::new(),
            pending_hot_exit_recovery: false,
            wait_tracking: HashMap::new(),
            completed_waits: Vec::new(),
            stdin_stream: stdin_stream::StdinStream::default(),
            line_scan: line_scan::LineScan::default(),
            search_scan: search_scan::SearchScan::default(),
            search_overlay_top_byte: None,
            review_hunks: Vec::new(),
            global_popups: crate::view::popup::PopupManager::new(),
            composite_buffers: HashMap::new(),
            composite_view_states: HashMap::new(),
        };

        // Apply clipboard configuration
        editor.clipboard.apply_config(&editor.config.clipboard);

        #[cfg(feature = "plugins")]
        {
            editor.update_plugin_state_snapshot();
            if editor.plugin_manager.is_active() {
                editor.plugin_manager.run_hook(
                    "editor_initialized",
                    crate::services::plugins::hooks::HookArgs::EditorInitialized,
                );
            }
        }

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
                if !self.plugin_manager.is_active() {
                    InitOutcome::Failed {
                        message: "plugin runtime inactive (--no-plugins); init.ts cannot run"
                            .into(),
                    }
                } else {
                    match self.plugin_manager.load_plugin_from_source(
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
                        self.theme = theme;
                    }
                }
                *self.keybindings.write().unwrap() =
                    crate::input::keybindings::KeybindingResolver::new(&self.config);
                self.clipboard.apply_config(&self.config.clipboard);
                self.menu_bar_visible = self.config.editor.show_menu_bar;
                self.tab_bar_visible = self.config.editor.show_tab_bar;
                self.status_bar_visible = self.config.editor.show_status_bar;
                self.prompt_line_visible = self.config.editor.show_prompt_line;
                #[cfg(feature = "plugins")]
                self.update_plugin_state_snapshot();
            }
            Err(e) => {
                self.set_status_message(format!("setSetting({path}): {e}"));
            }
        }
    }

    /// Fire the `plugins_loaded` hook (design M2, §3.3 phase 2).
    pub fn fire_plugins_loaded_hook(&self) {
        #[cfg(feature = "plugins")]
        if self.plugin_manager.is_active() {
            self.plugin_manager.run_hook(
                "plugins_loaded",
                crate::services::plugins::hooks::HookArgs::PluginsLoaded,
            );
        }
    }

    /// Fire the `ready` hook (design M2, §3.3 phase 3).
    pub fn fire_ready_hook(&self) {
        #[cfg(feature = "plugins")]
        if self.plugin_manager.is_active() {
            self.plugin_manager
                .run_hook("ready", crate::services::plugins::hooks::HookArgs::Ready);
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
}
