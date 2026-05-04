// EditorTestHarness - Virtual terminal environment for E2E testing

use anyhow::Result as AnyhowResult;

// Common initialization (non-plugin related)
#[ctor::ctor]
fn init_test_environment() {
    // Force Linux-style keybindings (Ctrl/Alt/Shift instead of ⌘/⌥/⇧)
    // to ensure consistent visual test output across platforms
    fresh::input::keybindings::set_force_linux_keybindings(true);

    // Install signal handlers for debugging hangs (dumps JS + Rust stack traces on Ctrl+C)
    fresh::services::signal_handler::install_signal_handlers();

    // Enable panicking on JS errors so plugin bugs surface immediately in tests
    #[cfg(feature = "plugins")]
    fresh_plugin_runtime::backend::set_panic_on_js_errors(true);
}

use crossterm::event::{KeyCode, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};

/// Terminal layout constants
/// The editor uses a fixed layout with reserved rows for UI elements
pub mod layout {
    /// Menu bar is always at row 0
    pub const MENU_BAR_ROW: usize = 0;

    /// Tab bar is at row 1 (within the main content area)
    pub const TAB_BAR_ROW: usize = 1;

    /// Content starts at row 2 (after menu bar and tab bar)
    pub const CONTENT_START_ROW: usize = 2;

    /// Number of rows reserved at the bottom (status bar + prompt line)
    pub const BOTTOM_RESERVED_ROWS: usize = 2;

    /// Total reserved rows (menu bar at top, status bar + prompt at bottom)
    pub const TOTAL_RESERVED_ROWS: usize = 4;

    /// Get the status bar row for a given terminal height
    #[inline]
    pub const fn status_bar_row(terminal_height: usize) -> usize {
        terminal_height - 2
    }

    /// Get the prompt line row for a given terminal height
    #[inline]
    pub const fn prompt_line_row(terminal_height: usize) -> usize {
        terminal_height - 1
    }

    /// Get the content end row (exclusive) for a given terminal height
    #[inline]
    pub const fn content_end_row(terminal_height: usize) -> usize {
        terminal_height - BOTTOM_RESERVED_ROWS
    }

    /// Get the number of content rows for a given terminal height
    #[inline]
    pub const fn content_row_count(terminal_height: usize) -> usize {
        terminal_height.saturating_sub(TOTAL_RESERVED_ROWS)
    }
}
use fresh::config_io::DirectoryContext;
use fresh::model::filesystem::{FileSystem, StdFileSystem};
use fresh::primitives::highlight_engine::{HighlightEngine, HighlightStats};
use fresh::services::fs::{BackendMetrics, SlowFileSystem, SlowFsConfig};
use fresh::services::time_source::{SharedTimeSource, TestTimeSource};
use fresh::{app::Editor, config::Config};
use ratatui::{backend::TestBackend, Terminal};
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tempfile::TempDir;

/// Copy a plugin and its i18n file (if exists) from the main plugins directory to a test plugins directory.
///
/// # Arguments
/// * `plugins_dir` - The destination plugins directory in the test project
/// * `plugin_name` - The plugin name without extension (e.g., "vi_mode", "todo_highlighter")
///
/// # Example
/// ```ignore
/// let plugins_dir = project_root.join("plugins");
/// fs::create_dir_all(&plugins_dir).unwrap();
/// copy_plugin(&plugins_dir, "vi_mode");
/// copy_plugin(&plugins_dir, "todo_highlighter");
/// ```
pub fn copy_plugin(plugins_dir: &Path, plugin_name: &str) {
    // Use CARGO_MANIFEST_DIR to find plugins regardless of current working directory
    let source_dir = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("plugins");

    // Copy the .ts file
    let ts_src = source_dir.join(format!("{}.ts", plugin_name));
    let ts_dest = plugins_dir.join(format!("{}.ts", plugin_name));
    fs::copy(&ts_src, &ts_dest)
        .unwrap_or_else(|e| panic!("Failed to copy {}.ts: {}", plugin_name, e));

    // Copy the .i18n.json file if it exists
    let i18n_src = source_dir.join(format!("{}.i18n.json", plugin_name));
    if i18n_src.exists() {
        let i18n_dest = plugins_dir.join(format!("{}.i18n.json", plugin_name));
        fs::copy(&i18n_src, &i18n_dest)
            .unwrap_or_else(|e| panic!("Failed to copy {}.i18n.json: {}", plugin_name, e));
    }
}

/// Copy the plugins/lib directory (contains TypeScript declarations like fresh.d.ts)
pub fn copy_plugin_lib(plugins_dir: &Path) {
    // Use CARGO_MANIFEST_DIR to find plugins regardless of current working directory
    let lib_src = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("plugins/lib");
    let lib_dest = plugins_dir.join("lib");
    if lib_src.exists() {
        fs::create_dir_all(&lib_dest).unwrap();
        for entry in fs::read_dir(&lib_src).unwrap() {
            let entry = entry.unwrap();
            let dest_path = lib_dest.join(entry.file_name());
            fs::copy(entry.path(), dest_path).unwrap();
        }
    }
}

/// Recursively copy `<src>` into `<dst>`, creating `<dst>` if missing.
/// Existing files at the destination are overwritten (for re-runs).
fn mirror_plugins_dir(src: &Path, dst: &Path) -> std::io::Result<()> {
    if !src.is_dir() {
        return Ok(());
    }
    fs::create_dir_all(dst)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let src_path = entry.path();
        let dst_path = dst.join(entry.file_name());
        if src_path.is_dir() {
            mirror_plugins_dir(&src_path, &dst_path)?;
        } else {
            fs::copy(&src_path, &dst_path)?;
        }
    }
    Ok(())
}

/// Configuration options for creating an EditorTestHarness.
///
/// Use the builder pattern to configure the harness:
/// ```ignore
/// let harness = EditorTestHarness::create(
///     HarnessOptions::new(80, 24)
///         .with_config(my_config)
///         .with_project_root()
/// )?;
/// ```
#[derive(Default)]
pub struct HarnessOptions {
    /// Editor configuration (defaults to Config::default() with test-friendly settings)
    pub config: Option<Config>,
    /// Explicit working directory. If None, uses a temp directory.
    pub working_dir: Option<PathBuf>,
    /// Create a "project_root" subdirectory for deterministic paths in snapshots.
    /// When true, `project_dir()` returns this subdirectory path.
    pub create_project_root: bool,
    /// Disable plugin loading (embedded + user) for test isolation.
    /// Defaults to true. Set to false (via `without_empty_plugins_dir()`)
    /// for tests that exercise plugin behavior — they then either pre-
    /// populate `<working_dir>/plugins/` (which the harness mirrors into
    /// `<config_dir>/plugins/`) or rely on the embedded plugins fallback.
    ///
    /// (Field name kept for source-compat with existing callers; the
    /// historical "create empty plugins dir" mechanism is gone — see
    /// issue #1722.)
    pub create_empty_plugins_dir: bool,
    /// Shared DirectoryContext. If None, creates a new one for test isolation.
    pub dir_context: Option<DirectoryContext>,
    /// Slow filesystem configuration for performance testing.
    pub slow_fs_config: Option<SlowFsConfig>,
    /// Custom filesystem backend. If provided, overrides slow_fs_config.
    pub filesystem: Option<Arc<dyn FileSystem + Send + Sync>>,
    /// Preserve the keybinding map from the config (don't force "default").
    /// Set this when testing a specific keymap like emacs.
    pub preserve_keybinding_map: bool,
    /// Use full grammar registry with syntax highlighting support.
    /// Defaults to false (uses empty registry for fast test startup).
    /// Set to true only for tests that need syntax highlighting or shebang detection.
    pub use_full_grammar_registry: bool,
    /// Force the embedded-plugins fallback to fire regardless of whether
    /// the test created `<working_dir>/plugins/`. The harness's normal
    /// "directory presence == user controls plugins" rule is for test
    /// isolation; tests reproducing user-visible plugin behavior (e.g.
    /// the #1722 regression test) need to override it to match
    /// production semantics. Defaults to false.
    pub force_embedded_plugins: bool,
    /// Per-test fake-devcontainer state. Set by [`HarnessOptions::with_fake_devcontainer`];
    /// moved into the harness on `create()` so the lock + tempdir live as long as the test.
    /// Unix-only: the fake CLI is a bash script that doesn't run on Windows.
    #[cfg(unix)]
    pub fake_devcontainer: Option<FakeDevcontainerHandle>,
}

impl HarnessOptions {
    /// Create new options with default settings.
    /// - `create_empty_plugins_dir`: true (disables plugin loading for isolation)
    /// - `create_project_root`: false
    pub fn new() -> Self {
        Self {
            config: None,
            working_dir: None,
            create_project_root: false,
            create_empty_plugins_dir: true,
            dir_context: None,
            slow_fs_config: None,
            filesystem: None,
            preserve_keybinding_map: false,
            use_full_grammar_registry: false,
            force_embedded_plugins: false,
            #[cfg(unix)]
            fake_devcontainer: None,
        }
    }

    /// Force the embedded-plugins fallback to fire regardless of test
    /// fixtures under `<working_dir>/plugins/`. Use only when reproducing
    /// production plugin-loading behavior in a test (e.g. issue #1722
    /// regression).
    pub fn with_forced_embedded_plugins(mut self) -> Self {
        self.force_embedded_plugins = true;
        self
    }

    /// Set a custom editor configuration.
    pub fn with_config(mut self, config: Config) -> Self {
        self.config = Some(config);
        self
    }

    /// Set an explicit working directory.
    /// The editor will use this directory for file operations and plugin loading.
    pub fn with_working_dir(mut self, dir: PathBuf) -> Self {
        self.working_dir = Some(dir);
        self
    }

    /// Create a "project_root" subdirectory for deterministic test paths.
    /// Use `harness.project_dir()` to get the path.
    pub fn with_project_root(mut self) -> Self {
        self.create_project_root = true;
        // Tests using project_root often install their own plugin fixtures
        // and need plugin loading enabled; opt them out of the no-plugins
        // default.
        self.create_empty_plugins_dir = false;
        self
    }

    /// Disable plugin loading for the test (default for isolation). Method
    /// name kept for source-compat with existing tests.
    pub fn with_empty_plugins_dir(mut self) -> Self {
        self.create_empty_plugins_dir = true;
        self
    }

    /// Enable plugin loading for the test (embedded plugins fire, and any
    /// fixtures in `<working_dir>/plugins/` are mirrored into the editor's
    /// scanned `<config_dir>/plugins/`). Method name kept for source-compat
    /// with existing tests.
    pub fn without_empty_plugins_dir(mut self) -> Self {
        self.create_empty_plugins_dir = false;
        self
    }

    /// Share a DirectoryContext with other harness instances.
    /// Useful for session restore tests.
    pub fn with_shared_dir_context(mut self, dir_context: DirectoryContext) -> Self {
        self.dir_context = Some(dir_context);
        self
    }

    /// Configure a slow filesystem backend for performance testing.
    pub fn with_slow_fs(mut self, config: SlowFsConfig) -> Self {
        self.slow_fs_config = Some(config);
        self
    }

    /// Set a custom filesystem backend.
    pub fn with_filesystem(mut self, fs: Arc<dyn FileSystem + Send + Sync>) -> Self {
        self.filesystem = Some(fs);
        self
    }

    /// Preserve the keybinding map from the config (don't force "default").
    /// Use this when testing a specific keymap like emacs or vscode.
    pub fn with_preserved_keybinding_map(mut self) -> Self {
        self.preserve_keybinding_map = true;
        self
    }

    /// Use full grammar registry with syntax highlighting support.
    /// By default, tests use an empty registry for fast startup.
    /// Only enable this for tests that need syntax highlighting or shebang detection.
    pub fn with_full_grammar_registry(mut self) -> Self {
        self.use_full_grammar_registry = true;
        self
    }

    /// Wire the test process so that `devcontainer` and `docker` resolve
    /// to the in-tree fake CLIs at `scripts/fake-devcontainer/bin`. The
    /// returned `HarnessOptions` carries a per-test state directory and
    /// a process-global mutex guard so concurrent harness instances
    /// don't clobber each other's `FAKE_DEVCONTAINER_STATE`.
    ///
    /// The fake state path is reachable via
    /// [`EditorTestHarness::fake_devcontainer_state`].
    ///
    /// Sets `FAKE_DC_UP_DELAY_MS=0` so build-progress lines are emitted
    /// without sleeps; tests that want to exercise streaming should
    /// override the env var explicitly after this call.
    ///
    /// Unix-only: the fake CLI is a bash script (`#!/usr/bin/env bash`)
    /// that doesn't run on native Windows, and the PATH manipulation
    /// below uses `:` as the separator. The test files that opt in
    /// (`devcontainer_attach_e2e`, `devcontainer_spec_repros`,
    /// `devcontainer_spec_conformance`) are gated to `cfg(unix)` in
    /// `tests/e2e/plugins/mod.rs`, so this stays available everywhere
    /// they compile.
    #[cfg(unix)]
    pub fn with_fake_devcontainer(mut self) -> Self {
        let fake_bin = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../../scripts/fake-devcontainer/bin")
            .canonicalize()
            .expect("scripts/fake-devcontainer/bin must exist relative to CARGO_MANIFEST_DIR");

        let temp = TempDir::new().expect("tempdir for fake-devcontainer state");
        let state_path = temp.path().to_path_buf();

        // Serialize against other harness instances using the fake CLI:
        // FAKE_DEVCONTAINER_STATE is process-global env, so a parallel
        // test setting its own state would steal ours mid-run.
        let guard = fake_devcontainer_lock()
            .lock()
            .unwrap_or_else(|p| p.into_inner());

        // PATH prepend is idempotent — every harness call points at the
        // same in-tree fake bin dir, so racing threads only ever
        // converge on the same prefix.
        let path = std::env::var("PATH").unwrap_or_default();
        let already_on_path = path.split(':').any(|p| std::path::Path::new(p) == fake_bin);
        if !already_on_path {
            std::env::set_var("PATH", format!("{}:{}", fake_bin.display(), path));
        }
        std::env::set_var("FAKE_DEVCONTAINER_STATE", &state_path);
        std::env::set_var("FAKE_DC_UP_DELAY_MS", "0");

        self.fake_devcontainer = Some(FakeDevcontainerHandle {
            _guard: guard,
            _temp: temp,
            state_path,
        });
        self
    }
}

/// Lock that serializes harness instances using the fake devcontainer CLI.
/// Held in `FakeDevcontainerHandle::_guard` for the lifetime of the test.
#[cfg(unix)]
fn fake_devcontainer_lock() -> &'static std::sync::Mutex<()> {
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    LOCK.get_or_init(|| std::sync::Mutex::new(()))
}

/// State directory + lock guard for a single harness's fake-devcontainer
/// session. Kept inside `EditorTestHarness` so dropping the harness
/// drops the guard (releasing the lock for the next test) and the
/// tempdir (cleaning up state files).
#[cfg(unix)]
pub struct FakeDevcontainerHandle {
    /// Held for the harness's lifetime so concurrent tests can't race
    /// on the global `FAKE_DEVCONTAINER_STATE` env var.
    _guard: std::sync::MutexGuard<'static, ()>,
    /// Tempdir kept alive for the harness's lifetime.
    _temp: TempDir,
    /// Path to the fake's state directory (`<temp>/`).
    pub state_path: PathBuf,
}

/// A wrapper that captures CrosstermBackend output for vt100 parsing
struct CaptureBuffer {
    data: Vec<u8>,
}

impl CaptureBuffer {
    fn new() -> Self {
        Self { data: Vec::new() }
    }

    fn take(&mut self) -> Vec<u8> {
        std::mem::take(&mut self.data)
    }
}

impl Write for CaptureBuffer {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.data.extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

/// Strip OSC 8 hyperlink escape sequences from a string.
///
/// Removes both `ESC ] 8 ; params ; url BEL` sequences and the
/// variant without ESC prefix (as seen in ratatui cell symbols after
/// post-render OSC 8 application).
///
/// # Examples
/// ```
/// assert_eq!(strip_osc8("\x1b]8;;https://example.com\x07Click\x1b]8;;\x07"), "Click");
/// assert_eq!(strip_osc8("plain text"), "plain text");
/// ```
pub fn strip_osc8(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        // ESC ] 8 ; — standard OSC 8 start
        if i + 3 < bytes.len()
            && bytes[i] == 0x1b
            && bytes[i + 1] == b']'
            && bytes[i + 2] == b'8'
            && bytes[i + 3] == b';'
        {
            i += 4;
            // Skip until BEL (\x07)
            while i < bytes.len() && bytes[i] != 0x07 {
                i += 1;
            }
            if i < bytes.len() {
                i += 1; // skip BEL
            }
        } else {
            result.push(bytes[i] as char);
            i += 1;
        }
    }
    result
}

/// Find `text` in the row formed by joining `cell_symbols` and return the
/// **cell column** (not byte offset) where the match starts.
///
/// `str::find` returns a byte offset into the joined row, but a single
/// cell can hold a multi-byte symbol (e.g. `│` is 3 UTF-8 bytes). Casting
/// the byte offset directly to a column would yield values past the
/// buffer width whenever the row contains box-drawing or other non-ASCII
/// glyphs, which then causes `Buffer::index_of` to panic with
/// "index outside of buffer".
pub fn find_text_in_row<'a, I: IntoIterator<Item = &'a str>>(
    cell_symbols: I,
    text: &str,
) -> Option<u16> {
    let mut row_text = String::new();
    let mut byte_to_col: Vec<u16> = Vec::new();
    for (col, sym) in cell_symbols.into_iter().enumerate() {
        let col = u16::try_from(col).ok()?;
        for _ in 0..sym.len() {
            byte_to_col.push(col);
        }
        row_text.push_str(sym);
    }
    let byte_offset = row_text.find(text)?;
    Some(byte_to_col[byte_offset])
}

/// Virtual editor environment for testing
/// Captures all rendering output without displaying to actual terminal
pub struct EditorTestHarness {
    /// The editor instance
    editor: Editor,

    /// Virtual terminal backend
    terminal: Terminal<TestBackend>,

    /// Optional temp directory (kept alive for the duration of the test)
    _temp_dir: Option<TempDir>,

    /// Optional fake-devcontainer state (kept alive for the duration
    /// of the test). The `Drop` of this field releases the global lock
    /// so the next test can claim it.
    /// Unix-only: see [`HarnessOptions::with_fake_devcontainer`].
    #[cfg(unix)]
    fake_devcontainer: Option<FakeDevcontainerHandle>,

    /// Optional metrics for slow filesystem backend
    fs_metrics: Option<Arc<BackendMetrics>>,

    /// Tokio runtime for async operations (needed for TypeScript plugins)
    _tokio_runtime: Option<tokio::runtime::Runtime>,

    /// Test time source for controllable time in tests
    /// All harness constructors use TestTimeSource for fast, deterministic testing
    time_source: Arc<TestTimeSource>,

    /// Shadow string that mirrors editor operations for validation
    /// This helps catch discrepancies between piece tree and simple string operations
    shadow_string: String,

    /// Shadow cursor position
    shadow_cursor: usize,

    /// Whether to enable shadow buffer validation (off by default)
    /// Enable this only in tests that focus on simple text editing operations
    enable_shadow_validation: bool,

    /// Shadow undo stack: (content_before, undo_cursor, redo_cursor)
    /// undo_cursor = cursor position after undoing (accounts for adjust_for_edit)
    /// redo_cursor = cursor position after redoing (post-action cursor)
    shadow_undo_stack: Vec<(String, usize, usize)>,

    /// Shadow redo stack: (content_after, redo_cursor, undo_cursor)
    shadow_redo_stack: Vec<(String, usize, usize)>,

    /// VT100 parser for testing real ANSI terminal output
    /// This simulates how a real terminal would interpret the escape sequences
    vt100_parser: vt100::Parser,

    /// Terminal dimensions for vt100
    term_width: u16,
    term_height: u16,
}

impl EditorTestHarness {
    // =========================================================================
    // Unified constructor
    // =========================================================================

    /// Create a test harness with full configuration control.
    ///
    /// This is the unified constructor that all other constructors delegate to.
    /// Use `HarnessOptions` builder to configure the harness behavior.
    ///
    /// # Example
    /// ```ignore
    /// let harness = EditorTestHarness::create(80, 24, HarnessOptions::new()
    ///     .with_project_root()
    ///     .with_config(my_config)
    /// )?;
    /// ```
    pub fn create(width: u16, height: u16, options: HarnessOptions) -> anyhow::Result<Self> {
        let mut t = crate::common::timing::Timer::start("harness::create");
        // Create temp directory if we don't have a shared dir_context
        let temp_dir = if options.dir_context.is_none() || options.create_project_root {
            Some(TempDir::new()?)
        } else {
            None
        };
        t.phase("temp_dir");

        // Determine the base path for our temp directory
        let temp_base = temp_dir.as_ref().map(|d| d.path().to_path_buf());

        // Determine working directory
        let working_dir = if let Some(dir) = options.working_dir {
            dir
        } else if options.create_project_root {
            let project_root = temp_base
                .as_ref()
                .expect("temp_dir must exist when create_project_root is true")
                .join("project_root");
            std::fs::create_dir(&project_root)?;
            project_root
        } else {
            temp_base
                .clone()
                .expect("temp_dir must exist when no working_dir provided")
        };

        t.phase("working_dir_setup");
        // Plugin loading. Historically the harness created an empty
        // `<working_dir>/plugins/` so the editor would scan it and
        // skip the embedded fallback (suppression by directory
        // presence). The editor no longer scans `<working_dir>/plugins/`
        // (issue #1722), but the harness keeps the directory-presence
        // contract for tests:
        //
        //   * `<working_dir>/plugins/` exists       → user controls the
        //     plugin set; embedded fallback is off, and any contents
        //     are mirrored into `<config_dir>/plugins/` (which the
        //     editor does scan).
        //   * `<working_dir>/plugins/` doesn't exist → embedded loads
        //     normally (used by tests that exercise bundled plugins).
        //
        // The default `create_empty_plugins_dir = true` continues to
        // mean "auto-create the dir to opt out of embedded". The plugin
        // runtime is always active so tests can introspect it via
        // `plugin_manager().state_snapshot_handle()` etc.
        let working_plugins_path = working_dir.join("plugins");
        if options.create_empty_plugins_dir && !working_plugins_path.exists() {
            std::fs::create_dir(&working_plugins_path)?;
        }
        let user_controls_plugins = working_plugins_path.is_dir();
        let working_plugins_populated = working_plugins_path
            .read_dir()
            .map(|mut it| it.next().is_some())
            .unwrap_or(false);
        let enable_plugins_for_editor = true;

        t.phase("plugin_dir_setup");

        // Get or create DirectoryContext
        let dir_context = options.dir_context.unwrap_or_else(|| {
            DirectoryContext::for_testing(
                temp_base
                    .as_ref()
                    .expect("temp_dir must exist when no dir_context provided"),
            )
        });
        t.phase("dir_context");

        // Create TestTimeSource for controllable time in tests
        let test_time_source = Arc::new(TestTimeSource::new());
        let time_source: SharedTimeSource = test_time_source.clone();

        // Prepare config with test-friendly defaults
        // If no config provided, use defaults; if config provided, respect its settings
        let config_was_provided = options.config.is_some();
        let mut config = options.config.unwrap_or_default();
        // Only override auto_indent/auto_close/animations if no config was
        // explicitly provided. Animations span multiple render ticks, so
        // tests that drive the UI via single `render()` calls and then
        // inspect the screen would see mid-slide frames instead of the
        // settled result. Tests that want to exercise the animation
        // layer pass their own Config with `animations: true`.
        if !config_was_provided {
            config.editor.auto_indent = false; // Disable for simpler testing
            config.editor.auto_close = false; // Disable for simpler testing
            config.editor.animations = false;
        }
        // Always force the prompt line to be visible in tests so layout-
        // sensitive assertions (scrolling, scrollbar invariants, status bar
        // positioning, content_area_rows) stay stable. The user-facing
        // default is auto-hide (show_prompt_line=false); tests that need
        // to exercise the auto-hide path do so via the runtime toggle
        // (`editor.toggle_prompt_line()`) or the Settings UI rather than
        // by passing a config, so the override here never gets in their
        // way.
        config.editor.show_prompt_line = true;
        // Force "default" keybinding map for consistent test behavior across platforms
        // (Config::default() uses platform-specific keymaps which breaks test assumptions)
        // Skip this if the test explicitly wants to preserve its keymap (e.g., testing emacs bindings)
        if !options.preserve_keybinding_map {
            config.active_keybinding_map = fresh::config::KeybindingMapName("default".to_string());
        }
        config.check_for_updates = false; // Disable update checking in tests

        // Initialize i18n with the config's locale before creating the editor
        // This ensures menu defaults are created with the correct translations.
        //
        // TODO(flakiness): this mutates a *process-global* locale
        // (`rust_i18n::set_locale`). Cargo runs all test functions in one
        // process in parallel, so harness-created tests can race each other:
        // a German-locale harness can flip the global to `de` while an
        // unrelated test is asserting on English UI strings, producing
        // intermittent failures like
        // `e2e::locale::test_default_locale_shows_english_search_options`
        // when the full suite is run in parallel. (The tests in
        // `tests/e2e/locale.rs` use a module-local mutex, which serializes
        // them against each other but not against the rest of the suite.)
        //
        // Pre-exists this branch; see master for identical parallel-run
        // failures. A robust fix means either moving rust_i18n locale into
        // a thread-local, or holding a *global* (process-wide) test lock
        // around every harness-driven render. Not worth the scope creep
        // from this perf fix; flagging here so the next reader knows.
        fresh::i18n::init_with_config(config.locale.as_option());
        config.editor.double_click_time_ms = 10; // Fast double-click for faster tests
        t.phase("config_and_i18n");

        // Create filesystem backend (custom, slow, or default)
        let (filesystem, fs_metrics): (Arc<dyn FileSystem + Send + Sync>, _) =
            if let Some(fs) = options.filesystem {
                (fs, None)
            } else if let Some(slow_config) = options.slow_fs_config {
                let std_fs: Arc<dyn FileSystem + Send + Sync> = Arc::new(StdFileSystem);
                let slow_fs = SlowFileSystem::new(std_fs, slow_config);
                let metrics = slow_fs.metrics().clone();
                (Arc::new(slow_fs), Some(metrics))
            } else {
                (Arc::new(StdFileSystem), None)
            };

        t.phase("filesystem");

        // Create terminal
        let backend = TestBackend::new(width, height);
        let terminal = Terminal::new(backend)?;
        t.phase("terminal");

        // Create grammar registry if requested (slow, only for tests that need syntax highlighting)
        let grammar_registry = if options.use_full_grammar_registry {
            Some(fresh::primitives::grammar::GrammarRegistry::for_editor(
                dir_context.config_dir.clone(),
            ))
        } else {
            None // Use empty registry for fast test startup
        };

        // Mirror any plugins the test pre-populated under
        // `<working_dir>/plugins/` into `<config_dir>/plugins/` (which the
        // editor scans). Tests historically set up plugin fixtures via
        // `copy_plugin()` against `working_dir/plugins/`; the editor no
        // longer scans that path (issue #1722), so we forward the fixtures
        // here to keep the test API working without touching every caller.
        //
        // When the test pre-populated its own plugins, treat that as an
        // explicit "use exactly these" request and disable the embedded
        // plugin fallback so the bundled set doesn't leak in. This matches
        // the pre-#1722 behavior, where any `working_dir/plugins/` (even
        // empty) suppressed embedded loading.
        t.phase("grammar_registry");

        if working_plugins_populated {
            let target = dir_context.config_dir.join("plugins");
            mirror_plugins_dir(&working_plugins_path, &target)?;
        }
        t.phase("mirror_plugins");
        // Embedded loads only when the test isn't taking control. The
        // `force_embedded_plugins` escape hatch overrides this for tests
        // that need production plugin-loading semantics (see #1722).
        let enable_embedded_plugins = options.force_embedded_plugins || !user_controls_plugins;

        // Create editor
        let mut editor = Editor::for_test(
            config,
            width,
            height,
            Some(working_dir),
            dir_context.clone(),
            fresh::view::color_support::ColorCapability::TrueColor,
            filesystem,
            Some(time_source),
            grammar_registry,
            enable_plugins_for_editor,
            enable_embedded_plugins,
        )?;

        t.phase("Editor::for_test");

        // Process any pending plugin commands
        editor.process_async_messages();
        t.phase("process_async_messages");

        let h = EditorTestHarness {
            editor,
            terminal,
            _temp_dir: temp_dir,
            #[cfg(unix)]
            fake_devcontainer: options.fake_devcontainer,
            fs_metrics,
            _tokio_runtime: None,
            time_source: test_time_source,
            shadow_string: String::new(),
            shadow_cursor: 0,
            enable_shadow_validation: false,
            shadow_undo_stack: Vec::new(),
            shadow_redo_stack: Vec::new(),
            vt100_parser: vt100::Parser::new(height, width, 0),
            term_width: width,
            term_height: height,
        };
        t.finish();
        Ok(h)
    }

    // =========================================================================
    // Convenience constructors (delegate to create())
    // =========================================================================

    /// Create new test harness with virtual terminal.
    /// Uses a temporary directory and prevents embedded plugin loading.
    pub fn new(width: u16, height: u16) -> anyhow::Result<Self> {
        Self::create(width, height, HarnessOptions::new())
    }

    /// Create with custom config.
    pub fn with_config(width: u16, height: u16, config: Config) -> anyhow::Result<Self> {
        Self::create(width, height, HarnessOptions::new().with_config(config))
    }

    /// Create harness with an isolated temporary project directory.
    /// Creates a "project_root" subdirectory for deterministic paths in snapshots.
    /// Does NOT create a plugins directory inside project_root (use `.with_empty_plugins_dir()` if needed).
    pub fn with_temp_project(width: u16, height: u16) -> anyhow::Result<Self> {
        Self::create(width, height, HarnessOptions::new().with_project_root())
    }

    /// Create a test harness with a temporary project directory and custom config.
    pub fn with_temp_project_and_config(
        width: u16,
        height: u16,
        config: Config,
    ) -> anyhow::Result<Self> {
        Self::create(
            width,
            height,
            HarnessOptions::new()
                .with_project_root()
                .with_config(config),
        )
    }

    /// Same as [`with_temp_project`] but with TypeScript plugin loading
    /// disabled. Reserved for tests whose code paths provably do not
    /// observe plugin behavior — every plugin-loading test boundary is
    /// ~440 ms of TS transpile + QuickJS evaluation, so the suite-wide
    /// savings are large.
    ///
    /// **Don't reach for this casually.** Disabling plugins removes any
    /// hooks they install (`editor_initialized`, buffer-changed
    /// reactions, command registrations). Currently used only by
    /// scenarios whose contract guarantees no plugin interaction:
    /// `BufferScenario` (text + caret only, dispatched through core
    /// `Action`s) and `TraceScenario` (forward + undo trace on the same
    /// core dispatch). Other scenario runners (Input, Modal, Layout,
    /// etc.) keep plugins enabled so their tests preserve coverage of
    /// any plugin-side effect that could reach the asserted state.
    pub fn with_temp_project_no_plugins(width: u16, height: u16) -> anyhow::Result<Self> {
        Self::create(
            width,
            height,
            HarnessOptions::new()
                .with_project_root()
                .with_empty_plugins_dir(),
        )
    }

    /// Same as [`with_temp_project_and_config`] but with plugins
    /// disabled. See [`with_temp_project_no_plugins`] for when this is
    /// safe to use.
    pub fn with_temp_project_and_config_no_plugins(
        width: u16,
        height: u16,
        config: Config,
    ) -> anyhow::Result<Self> {
        Self::create(
            width,
            height,
            HarnessOptions::new()
                .with_project_root()
                .with_empty_plugins_dir()
                .with_config(config),
        )
    }

    /// Create with explicit working directory, using default config.
    pub fn with_working_dir(width: u16, height: u16, working_dir: PathBuf) -> anyhow::Result<Self> {
        let config = Config::default();
        Self::with_config_and_working_dir(width, height, config, working_dir)
    }

    /// Create with custom config and explicit working directory.
    pub fn with_config_and_working_dir(
        width: u16,
        height: u16,
        config: Config,
        working_dir: PathBuf,
    ) -> anyhow::Result<Self> {
        Self::create(
            width,
            height,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(working_dir)
                .without_empty_plugins_dir(), // Don't create plugins in user-provided dir
        )
    }

    /// Create new test harness with line wrapping disabled.
    pub fn new_no_wrap(width: u16, height: u16) -> anyhow::Result<Self> {
        let mut config = Config::default();
        config.editor.line_wrap = false;
        Self::with_config(width, height, config)
    }

    /// Create with custom config, working directory, and shared DirectoryContext.
    /// Useful for session restore tests that need to share state directories.
    pub fn with_shared_dir_context(
        width: u16,
        height: u16,
        config: Config,
        working_dir: PathBuf,
        dir_context: DirectoryContext,
    ) -> anyhow::Result<Self> {
        Self::create(
            width,
            height,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(working_dir)
                .with_shared_dir_context(dir_context)
                .without_empty_plugins_dir(),
        )
    }

    /// Create a test harness with a slow filesystem backend for performance testing.
    pub fn with_slow_fs(
        width: u16,
        height: u16,
        slow_config: SlowFsConfig,
    ) -> anyhow::Result<Self> {
        Self::create(
            width,
            height,
            HarnessOptions::new().with_slow_fs(slow_config),
        )
    }

    /// Advance the test time source by the given duration (instant, no real wait).
    ///
    /// Use this for time-based editor logic like:
    /// - Auto-save intervals
    /// - Debounce timers that check elapsed time
    /// - Rate limiting based on time
    ///
    /// Do NOT use this for waiting on async I/O operations (file changes, LSP responses).
    /// For those, use `wait_until` or real `std::thread::sleep`.
    pub fn advance_time(&self, duration: std::time::Duration) {
        self.time_source.advance(duration);
    }

    /// Sleep using the test time source (instant logical time advancement).
    ///
    /// This is equivalent to `advance_time` - it advances logical time without
    /// actually waiting. Use this to replace `thread::sleep` in tests that are
    /// waiting for time-based editor logic.
    ///
    /// # When to use this vs `std::thread::sleep`:
    /// - Use `sleep()` for time-based editor logic (debounce, rate limiting, auto-save)
    /// - Use `std::thread::sleep()` for waiting on real async I/O (file changes, LSP, plugins)
    pub fn sleep(&self, duration: std::time::Duration) {
        self.advance_time(duration);
    }

    /// Get the test time source.
    pub fn time_source(&self) -> &Arc<TestTimeSource> {
        &self.time_source
    }

    /// Get filesystem metrics (if using slow filesystem backend)
    pub fn fs_metrics(&self) -> Option<&Arc<BackendMetrics>> {
        self.fs_metrics.as_ref()
    }

    /// Get total filesystem calls count
    pub fn get_fs_total_calls(&self) -> Option<usize> {
        self.fs_metrics.as_ref().map(|m| m.total_calls())
    }

    /// Get the path to the temp project directory (if created with with_temp_project)
    /// Returns the "project_root" subdirectory path for deterministic naming
    pub fn project_dir(&self) -> Option<PathBuf> {
        self._temp_dir
            .as_ref()
            .map(|d| d.path().join("project_root"))
    }

    /// Path to the per-test fake-devcontainer state directory, set up
    /// by [`HarnessOptions::with_fake_devcontainer`]. Returns `None`
    /// for harnesses that didn't opt into the fake CLI. Tests use this
    /// to read `last_id`, container `logs` files, etc.
    /// Unix-only: see [`HarnessOptions::with_fake_devcontainer`].
    #[cfg(unix)]
    pub fn fake_devcontainer_state(&self) -> Option<&Path> {
        self.fake_devcontainer
            .as_ref()
            .map(|h| h.state_path.as_path())
    }

    /// Get the recovery directory path for this test harness.
    ///
    /// Returns the *scoped* directory where the editor actually writes
    /// recovery files. In standalone mode (no `set_session_name`) the path
    /// is `<base>/default/<encoded_working_dir>/`, so each working directory
    /// keeps its own recovery files (issue #1550).
    pub fn recovery_dir(&self) -> Option<PathBuf> {
        let base = self
            ._temp_dir
            .as_ref()
            .map(|d| d.path().join("data").join("recovery"))?;
        let hash = fresh::workspace::encode_path_for_filename(self.editor.working_dir());
        Some(base.join("default").join(hash))
    }

    /// Take ownership of the temp directory, preventing it from being cleaned up
    /// when the harness is dropped. This is useful for tests that need to access
    /// the recovery directory after dropping the harness.
    /// Returns the TempDir which should be kept alive until the test ends.
    pub fn take_temp_dir(&mut self) -> Option<TempDir> {
        self._temp_dir.take()
    }

    /// Borrow the harness's temp directory path. Used by
    /// `PersistenceScenario` to write fixture files the editor will
    /// then open. None if the harness wasn't created with a
    /// project root.
    pub fn temp_dir_path(&self) -> Option<&Path> {
        self._temp_dir.as_ref().map(|t| t.path())
    }

    /// Enable software-cursor-only mode (no hardware cursor).
    /// Use this in tests that need REVERSED cell styling on cursor positions,
    /// e.g. when verifying that cursor styling doesn't bleed through overlays.
    pub fn set_software_cursor_only(&mut self, enabled: bool) {
        self.editor.set_software_cursor_only(enabled);
    }

    /// Enable shadow buffer validation
    /// Call this at the start of tests that focus on simple text editing operations
    /// where you want to validate that the piece tree matches simple string operations
    pub fn enable_shadow_validation(&mut self) {
        self.enable_shadow_validation = true;
    }

    /// Open a file in the editor
    pub fn open_file(&mut self, path: &Path) -> anyhow::Result<()> {
        let mut t = crate::common::timing::Timer::start("harness::open_file");
        self.editor.open_file(path)?;
        t.phase("editor.open_file");
        self.render()?;
        t.phase("render");

        // Initialize shadow string with the file content (if available)
        // For large files with lazy loading, shadow validation is not supported
        self.shadow_string = self.get_buffer_content().unwrap_or_default();
        self.shadow_cursor = self.cursor_position();
        t.phase("shadow_init");
        t.finish();
        Ok(())
    }

    /// Load text content into the editor by creating a temporary file and opening it
    /// This is much faster than type_text() for large amounts of text in tests
    /// Returns a TestFixture that must be kept alive for the duration of the test
    pub fn load_buffer_from_text(
        &mut self,
        content: &str,
    ) -> anyhow::Result<crate::common::fixtures::TestFixture> {
        self.load_buffer_from_text_named("test_buffer.txt", content)
    }

    /// Like [`load_buffer_from_text`], but the on-disk fixture uses
    /// the given filename. Choose the extension to drive
    /// language-detection — `"x.rs"` → Rust, `"x.py"` → Python,
    /// `"x.yaml"` → YAML, etc. Needed for theorems whose behavior
    /// depends on the buffer's language (toggle-comment prefix,
    /// auto-close of quote chars, etc.).
    pub fn load_buffer_from_text_named(
        &mut self,
        filename: &str,
        content: &str,
    ) -> anyhow::Result<crate::common::fixtures::TestFixture> {
        let fixture = crate::common::fixtures::TestFixture::new(filename, content)?;
        self.open_file(&fixture.path)?;
        Ok(fixture)
    }

    /// Create a new empty buffer
    pub fn new_buffer(&mut self) -> anyhow::Result<()> {
        self.editor.new_buffer();
        self.render()?;
        Ok(())
    }

    /// Simulate a key press
    pub fn send_key(&mut self, code: KeyCode, modifiers: KeyModifiers) -> AnyhowResult<()> {
        // Update shadow string to mirror the operation (only if validation is enabled)
        if self.enable_shadow_validation {
            self.update_shadow_for_key(code, modifiers);
        }

        // Delegate to the editor's handle_key method (just like main.rs does)
        self.editor.handle_key(code, modifiers)?;
        // Process any async messages that may have been generated by the key press
        // This ensures that actions like opening files complete before the next operation
        let _ = self.editor.process_async_messages();

        // After undo/redo, sync shadow cursor from the real editor.
        // The redo algorithm may replay MoveCursor events from independent user movements,
        // making cursor position hard to predict in the shadow model. Content is validated
        // independently, so syncing cursor here still catches content bugs.
        if self.enable_shadow_validation {
            let is_undo =
                modifiers.contains(KeyModifiers::CONTROL) && matches!(code, KeyCode::Char('z'));
            let is_redo =
                modifiers.contains(KeyModifiers::CONTROL) && matches!(code, KeyCode::Char('y'));
            if is_undo || is_redo {
                self.shadow_cursor = self.editor.active_cursors().primary().position;
            }
        }

        // Render to make state changes visible
        self.render()?;

        Ok(())
    }

    /// Send the same key press multiple times without rendering after each one
    /// This is optimized for tests that need to send many keys in a row (e.g., scrolling)
    /// Only renders once at the end, which is much faster than calling send_key() in a loop
    pub fn send_key_repeat(
        &mut self,
        code: KeyCode,
        modifiers: KeyModifiers,
        count: usize,
    ) -> anyhow::Result<()> {
        for _ in 0..count {
            // Call handle_key directly without rendering (unlike send_key which renders every time)
            self.editor.handle_key(code, modifiers)?;
        }
        // Process any async messages that accumulated
        let _ = self.editor.process_async_messages();
        // Render once at the end instead of after every key press
        self.render()?;
        Ok(())
    }

    /// Simulate typing a string of text
    /// Optimized to avoid rendering after each character - only renders once at the end
    pub fn type_text(&mut self, text: &str) -> anyhow::Result<()> {
        for ch in text.chars() {
            // Update shadow string (only if validation is enabled)
            if self.enable_shadow_validation {
                self.shadow_string.insert(self.shadow_cursor, ch);
                self.shadow_cursor += ch.len_utf8();
            }

            // Call handle_key directly without rendering (unlike send_key which renders every time)
            self.editor
                .handle_key(KeyCode::Char(ch), KeyModifiers::NONE)?;
        }
        // Process any async messages that accumulated during typing
        let _ = self.editor.process_async_messages();
        // Render once at the end instead of after every character
        self.render()?;
        Ok(())
    }

    /// Simulate a mouse event
    pub fn send_mouse(&mut self, mouse_event: MouseEvent) -> anyhow::Result<()> {
        // Delegate to the editor's handle_mouse method (just like main.rs does)
        self.editor.handle_mouse(mouse_event)?;
        Ok(())
    }

    /// Simulate a mouse click at specific coordinates
    pub fn mouse_click(&mut self, col: u16, row: u16) -> anyhow::Result<()> {
        let mouse_event = MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: col,
            row,
            modifiers: KeyModifiers::empty(),
        };
        self.send_mouse(mouse_event)?;

        // Also send the release event
        let mouse_up = MouseEvent {
            kind: MouseEventKind::Up(MouseButton::Left),
            column: col,
            row,
            modifiers: KeyModifiers::empty(),
        };
        self.send_mouse(mouse_up)?;
        // Process any async messages that may have been generated by the mouse event
        let _ = self.editor.process_async_messages();
        self.render()?;
        Ok(())
    }

    /// Simulate a shift+click at specific coordinates (for extending selection)
    pub fn mouse_shift_click(&mut self, col: u16, row: u16) -> anyhow::Result<()> {
        let mouse_event = MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: col,
            row,
            modifiers: KeyModifiers::SHIFT,
        };
        self.send_mouse(mouse_event)?;

        // Also send the release event
        let mouse_up = MouseEvent {
            kind: MouseEventKind::Up(MouseButton::Left),
            column: col,
            row,
            modifiers: KeyModifiers::SHIFT,
        };
        self.send_mouse(mouse_up)?;
        self.render()?;
        Ok(())
    }

    /// Simulate a right-click at specific coordinates
    pub fn mouse_right_click(&mut self, col: u16, row: u16) -> anyhow::Result<()> {
        let mouse_down = MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Right),
            column: col,
            row,
            modifiers: KeyModifiers::empty(),
        };
        self.send_mouse(mouse_down)?;
        let mouse_up = MouseEvent {
            kind: MouseEventKind::Up(MouseButton::Right),
            column: col,
            row,
            modifiers: KeyModifiers::empty(),
        };
        self.send_mouse(mouse_up)?;
        self.render()?;
        Ok(())
    }

    /// Simulate a mouse move (hover) at specific coordinates
    pub fn mouse_move(&mut self, col: u16, row: u16) -> anyhow::Result<()> {
        let mouse_event = MouseEvent {
            kind: MouseEventKind::Moved,
            column: col,
            row,
            modifiers: KeyModifiers::empty(),
        };
        self.send_mouse(mouse_event)?;
        self.render()?;
        Ok(())
    }

    /// Simulate a mouse scroll up at specific coordinates
    pub fn mouse_scroll_up(&mut self, col: u16, row: u16) -> anyhow::Result<()> {
        let mouse_event = MouseEvent {
            kind: MouseEventKind::ScrollUp,
            column: col,
            row,
            modifiers: KeyModifiers::empty(),
        };
        self.send_mouse(mouse_event)?;
        self.render()?;
        Ok(())
    }

    /// Simulate a mouse scroll down at specific coordinates
    pub fn mouse_scroll_down(&mut self, col: u16, row: u16) -> anyhow::Result<()> {
        let mouse_event = MouseEvent {
            kind: MouseEventKind::ScrollDown,
            column: col,
            row,
            modifiers: KeyModifiers::empty(),
        };
        self.send_mouse(mouse_event)?;
        self.render()?;
        Ok(())
    }

    /// Simulate a mouse drag from one position to another
    pub fn mouse_drag(
        &mut self,
        start_col: u16,
        start_row: u16,
        end_col: u16,
        end_row: u16,
    ) -> anyhow::Result<()> {
        // Send initial press
        let mouse_down = MouseEvent {
            kind: MouseEventKind::Down(MouseButton::Left),
            column: start_col,
            row: start_row,
            modifiers: KeyModifiers::empty(),
        };
        self.send_mouse(mouse_down)?;

        // Interpolate intermediate positions for smooth dragging
        let steps = ((end_row as i32 - start_row as i32).abs())
            .max((end_col as i32 - start_col as i32).abs())
            .max(1);
        for i in 1..=steps {
            let t = i as f32 / steps as f32;
            let col = start_col as f32 + (end_col as f32 - start_col as f32) * t;
            let row = start_row as f32 + (end_row as f32 - start_row as f32) * t;

            let mouse_drag_event = MouseEvent {
                kind: MouseEventKind::Drag(MouseButton::Left),
                column: col as u16,
                row: row as u16,
                modifiers: KeyModifiers::empty(),
            };
            self.send_mouse(mouse_drag_event)?;
        }

        // Send final release
        let mouse_up = MouseEvent {
            kind: MouseEventKind::Up(MouseButton::Left),
            column: end_col,
            row: end_row,
            modifiers: KeyModifiers::empty(),
        };
        self.send_mouse(mouse_up)?;
        self.render()?;
        Ok(())
    }

    /// Apply an event directly to the active buffer
    pub fn apply_event(&mut self, event: fresh::model::event::Event) -> anyhow::Result<()> {
        self.editor.apply_event_to_active_buffer(&event);
        Ok(())
    }

    /// Force a render cycle and capture output
    pub fn render(&mut self) -> anyhow::Result<()> {
        self.terminal.draw(|frame| {
            self.editor.render(frame);
        })?;
        Ok(())
    }

    /// Render through the real CrosstermBackend and parse with vt100
    /// This tests the actual ANSI escape sequences, not just the buffer contents
    /// Returns the screen content as parsed by a real terminal emulator
    pub fn render_real(&mut self) -> anyhow::Result<()> {
        // Generate ANSI escape sequences manually

        // First render to TestBackend to get the buffer
        self.render()?;

        // Now manually generate ANSI sequences from the buffer
        // This simulates what CrosstermBackend would do without needing a real terminal
        let buffer = self.terminal.backend().buffer();
        let mut ansi_output = Vec::new();

        // Clear screen and move to home position
        ansi_output.extend_from_slice(b"\x1b[2J\x1b[H");

        // Render each cell
        for y in 0..buffer.area.height {
            // Move to start of line
            ansi_output.extend_from_slice(format!("\x1b[{};1H", y + 1).as_bytes());

            for x in 0..buffer.area.width {
                let idx = buffer.index_of(x, y);
                if let Some(cell) = buffer.content.get(idx) {
                    let symbol = cell.symbol();
                    ansi_output.extend_from_slice(symbol.as_bytes());
                }
            }
        }

        // Feed to vt100 parser
        self.vt100_parser.process(&ansi_output);

        Ok(())
    }

    /// Alternative: Render using ratatui's diff-based rendering to capture incremental updates
    /// This more closely matches what happens in the real application
    pub fn render_real_incremental(&mut self) -> anyhow::Result<()> {
        use ratatui::backend::CrosstermBackend;

        // Create a buffer to capture ANSI output
        let mut capture = CaptureBuffer::new();

        // Use a scope to ensure backend is dropped before we take the buffer
        {
            // Create CrosstermBackend with our capture buffer
            // Note: This may fail if crossterm tries to query terminal state
            let mut backend = CrosstermBackend::new(&mut capture);

            // Manually render cells without using Terminal::draw which does extra setup
            let buffer = self.terminal.backend().buffer();

            // Write clear screen
            use std::io::Write;
            write!(backend, "\x1b[2J\x1b[H")?;

            // Write each cell manually
            for y in 0..buffer.area.height {
                write!(backend, "\x1b[{};1H", y + 1)?;
                for x in 0..buffer.area.width {
                    let idx = buffer.index_of(x, y);
                    if let Some(cell) = buffer.content.get(idx) {
                        write!(backend, "{}", cell.symbol())?;
                    }
                }
            }
            backend.flush()?;
        }

        // Get the captured output and feed to vt100
        let ansi_output = capture.take();
        self.vt100_parser.process(&ansi_output);

        // Also render to TestBackend
        self.render()?;

        Ok(())
    }

    /// Get the screen content as parsed by vt100 (simulating real terminal)
    /// This is what a real terminal would show after processing ANSI sequences
    pub fn vt100_screen_to_string(&self) -> String {
        let screen = self.vt100_parser.screen();
        let mut result = String::new();

        for row in 0..self.term_height {
            for col in 0..self.term_width {
                let cell = screen.cell(row, col);
                if let Some(cell) = cell {
                    result.push_str(&cell.contents());
                } else {
                    result.push(' ');
                }
            }
            if row < self.term_height - 1 {
                result.push('\n');
            }
        }

        result
    }

    /// Compare TestBackend output with vt100-parsed output
    /// Returns a list of differences if any, or empty vec if they match
    pub fn compare_test_vs_real(&self) -> Vec<String> {
        let test_screen = self.screen_to_string();
        let vt100_screen = self.vt100_screen_to_string();

        let test_lines: Vec<&str> = test_screen.lines().collect();
        let vt100_lines: Vec<&str> = vt100_screen.lines().collect();

        let mut differences = Vec::new();

        for (row, (test_line, vt100_line)) in test_lines.iter().zip(vt100_lines.iter()).enumerate()
        {
            if test_line != vt100_line {
                differences.push(format!(
                    "Row {}: TestBackend vs VT100 mismatch:\n  Test:  {:?}\n  VT100: {:?}",
                    row, test_line, vt100_line
                ));

                // Character-by-character comparison for debugging
                let test_chars: Vec<char> = test_line.chars().collect();
                let vt100_chars: Vec<char> = vt100_line.chars().collect();
                for (col, (tc, vc)) in test_chars.iter().zip(vt100_chars.iter()).enumerate() {
                    if tc != vc {
                        differences.push(format!("    Col {}: '{}' vs '{}'", col, tc, vc));
                    }
                }
            }
        }

        differences
    }

    /// Assert that TestBackend and vt100 show the same content
    /// This catches bugs in ANSI escape sequence generation
    pub fn assert_test_matches_real(&self) {
        let differences = self.compare_test_vs_real();
        if !differences.is_empty() {
            panic!(
                "TestBackend and VT100 output differ!\n{}\n\nTestBackend:\n{}\n\nVT100:\n{}",
                differences.join("\n"),
                self.screen_to_string(),
                self.vt100_screen_to_string()
            );
        }
    }

    /// Get a specific cell from the vt100-parsed screen
    pub fn vt100_get_cell(&self, col: u16, row: u16) -> Option<String> {
        let screen = self.vt100_parser.screen();
        screen
            .cell(row, col)
            .map(|cell| cell.contents().to_string())
    }

    /// Cursor position the vt100 parser sees, as `(col, row)`. Used
    /// by the scenario framework's `RoundTripGrid` observable
    /// (Phase 8 / TerminalIoScenario).
    pub fn vt100_cursor_position(&self) -> Option<(u16, u16)> {
        let (row, col) = self.vt100_parser.screen().cursor_position();
        if row >= self.term_height || col >= self.term_width {
            None
        } else {
            Some((col, row))
        }
    }

    /// Get the current terminal buffer (what would be displayed)
    pub fn buffer(&self) -> &ratatui::buffer::Buffer {
        self.terminal.backend().buffer()
    }

    /// Get the editor's config
    pub fn config(&self) -> &fresh::config::Config {
        self.editor.config()
    }

    /// Get text at specific cell position
    pub fn get_cell(&self, x: u16, y: u16) -> Option<String> {
        let buffer = self.buffer();
        let pos = buffer.index_of(x, y);
        buffer
            .content
            .get(pos)
            .map(|cell| cell.symbol().to_string())
    }

    /// Get the style (color, modifiers) of a specific cell
    pub fn get_cell_style(&self, x: u16, y: u16) -> Option<ratatui::style::Style> {
        let buffer = self.buffer();
        let pos = buffer.index_of(x, y);
        buffer.content.get(pos).map(|cell| cell.style())
    }

    /// Check if a cell at the given position is a scrollbar thumb.
    ///
    /// Since the scrollbar is rendered using background colors (not characters),
    /// this checks if the cell has a background color matching scrollbar thumb colors.
    pub fn is_scrollbar_thumb_at(&self, x: u16, y: u16) -> bool {
        self.get_cell_style(x, y)
            .map(crate::common::scrollbar::is_scrollbar_thumb_style)
            .unwrap_or(false)
    }

    /// Check if a cell at the given position is a scrollbar track.
    ///
    /// Since the scrollbar is rendered using background colors (not characters),
    /// this checks if the cell has a background color matching scrollbar track colors.
    pub fn is_scrollbar_track_at(&self, x: u16, y: u16) -> bool {
        self.get_cell_style(x, y)
            .map(crate::common::scrollbar::is_scrollbar_track_style)
            .unwrap_or(false)
    }

    /// Check if any scrollbar (thumb or track) is visible at the given column.
    ///
    /// Scans the content area rows at the specified column for scrollbar cells.
    pub fn has_scrollbar_at_column(&self, col: u16) -> bool {
        let (first_row, last_row) = self.content_area_rows();
        for row in first_row..=last_row {
            if self.is_scrollbar_thumb_at(col, row as u16)
                || self.is_scrollbar_track_at(col, row as u16)
            {
                return true;
            }
        }
        false
    }

    /// Get the text content of a specific screen row
    pub fn get_row_text(&self, y: u16) -> String {
        let buffer = self.buffer();
        let width = buffer.area.width;
        let mut row_text = String::new();

        for x in 0..width {
            let pos = buffer.index_of(x, y);
            if let Some(cell) = buffer.content.get(pos) {
                row_text.push_str(cell.symbol());
            }
        }

        row_text
    }

    /// Get entire screen as string (for debugging)
    pub fn screen_to_string(&self) -> String {
        let buffer = self.buffer();
        let (width, height) = (buffer.area.width, buffer.area.height);
        let mut result = String::new();

        for y in 0..height {
            for x in 0..width {
                let pos = buffer.index_of(x, y);
                if let Some(cell) = buffer.content.get(pos) {
                    result.push_str(cell.symbol());
                }
            }
            if y < height - 1 {
                result.push('\n');
            }
        }

        result
    }

    /// Read a screen row as clean text, stripping OSC 8 hyperlink sequences.
    ///
    /// This reads cells directly from the ratatui buffer and strips any
    /// OSC 8 escape sequences, producing clean text suitable for assertions.
    /// Unlike `screen_to_string()`, this handles the 2-char chunking used
    /// for OSC 8 rendering without garbling the output.
    pub fn screen_row_text(&self, row: u16) -> String {
        let buffer = self.buffer();
        let width = buffer.area.width;
        let mut s = String::new();
        let mut col = 0u16;
        while col < width {
            let pos = buffer.index_of(col, row);
            if let Some(cell) = buffer.content.get(pos) {
                let sym = cell.symbol();
                let stripped = strip_osc8(sym);
                if stripped.len() > 1 {
                    // This is a multi-char OSC 8 chunk — it contains chars
                    // from this cell and the next cell(s). Push the stripped
                    // content and skip the extra cells.
                    let char_count = stripped.chars().count();
                    s.push_str(&stripped);
                    col += char_count as u16;
                } else {
                    s.push_str(&stripped);
                    col += 1;
                }
            } else {
                col += 1;
            }
        }
        s.trim_end().to_string()
    }

    /// Verify text appears on screen
    pub fn assert_screen_contains(&self, text: &str) {
        let screen = self.screen_to_string();
        assert!(
            screen.contains(text),
            "Expected screen to contain '{text}'\nScreen content:\n{screen}"
        );
    }

    /// Verify text does not appear on screen
    pub fn assert_screen_not_contains(&self, text: &str) {
        let screen = self.screen_to_string();
        assert!(
            !screen.contains(text),
            "Expected screen to not contain '{text}'\nScreen content:\n{screen}"
        );
    }

    /// Find the position of text on screen, returning (col, row) of the first match.
    /// Returns None if text is not found.
    pub fn find_text_on_screen(&self, text: &str) -> Option<(u16, u16)> {
        let buffer = self.buffer();
        let (width, height) = (buffer.area.width, buffer.area.height);

        for y in 0..height {
            let symbols = (0..width).map(|x| {
                buffer
                    .content
                    .get(buffer.index_of(x, y))
                    .map(|cell| cell.symbol())
                    .unwrap_or("")
            });
            if let Some(col) = find_text_in_row(symbols, text) {
                return Some((col, y));
            }
        }
        None
    }

    /// Assert that no plugin errors have occurred
    /// This checks the accumulated error messages from plugin execution
    /// Call this at key points in tests to catch plugin errors early
    pub fn assert_no_plugin_errors(&self) {
        let errors = self.editor.get_plugin_errors();
        if !errors.is_empty() {
            let screen = self.screen_to_string();
            panic!(
                "Plugin error(s) occurred:\n{}\n\nScreen content:\n{}",
                errors.join("\n"),
                screen
            );
        }
    }

    /// Get any accumulated plugin errors
    pub fn get_plugin_errors(&self) -> &[String] {
        self.editor.get_plugin_errors()
    }

    /// Clear accumulated plugin errors (useful if testing error handling)
    pub fn clear_plugin_errors(&mut self) {
        self.editor.clear_plugin_errors();
    }

    /// Get the buffer content (not screen, actual buffer text)
    /// Returns None for large files with unloaded regions (lazy loading)
    pub fn get_buffer_content(&self) -> Option<String> {
        self.editor.active_state().buffer.to_string()
    }

    /// Verify buffer content matches expected
    /// Panics if buffer has unloaded regions (large file mode)
    pub fn assert_buffer_content(&self, expected: &str) {
        let actual = self
            .get_buffer_content()
            .expect("Cannot assert buffer content: buffer has unloaded regions (large file mode)");

        // Also verify shadow string matches to catch discrepancies (only if validation is enabled)
        if self.enable_shadow_validation {
            assert_eq!(
                self.shadow_string, expected,
                "Shadow string mismatch (bug in test harness shadow tracking)\nExpected: {expected:?}\nShadow: {:?}",
                self.shadow_string
            );
        }

        assert_eq!(
            actual, expected,
            "Buffer content mismatch\nExpected: {expected:?}\nActual: {actual:?}",
        );
    }

    /// Access the editor directly (for advanced testing)
    pub fn editor(&self) -> &Editor {
        &self.editor
    }

    /// Access the editor mutably (for advanced testing)
    pub fn editor_mut(&mut self) -> &mut Editor {
        &mut self.editor
    }

    /// Access the editor through the semantic test API.
    ///
    /// This is the entry point used by theorem-style tests under
    /// `tests/semantic/`. Those tests deliberately do not use
    /// `editor_mut()` or any other internal accessor — see
    /// `docs/internal/e2e-test-migration-design.md` §2.1.
    pub fn api_mut(&mut self) -> &mut dyn fresh::test_api::EditorTestApi {
        &mut self.editor
    }

    /// Check if editor wants to quit
    pub fn should_quit(&self) -> bool {
        self.editor.should_quit()
    }

    // =========================================================================
    // Session lifecycle helpers — mirrors the real startup/shutdown flows
    // in main.rs so tests exercise the actual production code paths.
    // =========================================================================

    /// Perform a clean shutdown, mirroring `run_event_loop_common` exit path.
    ///
    /// Calls auto-save (if enabled), `end_recovery_session`, and `save_workspace`
    /// in the same order as the production shutdown code.
    pub fn shutdown(&mut self, workspace_enabled: bool) -> anyhow::Result<()> {
        if self.editor.config().editor.auto_save_enabled {
            self.editor.save_all_on_exit()?;
        }
        self.editor.end_recovery_session()?;
        if workspace_enabled {
            self.editor.save_workspace()?;
        }
        Ok(())
    }

    /// Perform startup, mirroring `handle_first_run_setup` in main.rs.
    ///
    /// Restores the workspace (if enabled), queues CLI file opens, processes
    /// them, schedules hot-exit recovery, and runs crash recovery — all in the
    /// same order as production.  Returns whether workspace restore succeeded.
    pub fn startup(
        &mut self,
        workspace_enabled: bool,
        cli_files: &[std::path::PathBuf],
    ) -> anyhow::Result<bool> {
        self.startup_with_force_restore(workspace_enabled, false, cli_files)
    }

    /// Variant of [`Self::startup`] that can force workspace restore even
    /// when `editor.restore_previous_session` is disabled — mirrors the
    /// `--restore` CLI flag in `main.rs`.
    pub fn startup_with_force_restore(
        &mut self,
        workspace_enabled: bool,
        force_restore: bool,
        cli_files: &[std::path::PathBuf],
    ) -> anyhow::Result<bool> {
        let restore_full_session = workspace_enabled
            && (force_restore || self.editor.config().editor.restore_previous_session);
        let mut restored = false;
        if restore_full_session {
            restored = self.editor.try_restore_workspace().unwrap_or(false);
        } else {
            // Session restore opted out, but hot-exit content (unsaved
            // modified files + unnamed buffers with content) is still
            // restored — matches production behaviour in `main.rs`.
            let _ = self.editor.try_restore_hot_exit_buffers();
        }

        let has_cli_files = !cli_files.is_empty();
        for path in cli_files {
            self.editor
                .queue_file_open(path.clone(), None, None, None, None, None, None);
        }

        if has_cli_files {
            self.editor.schedule_hot_exit_recovery();
        }

        // Process pending file opens (in production this happens on first
        // iteration of the event loop).
        self.editor.process_pending_file_opens();

        if self.editor.has_recovery_files().unwrap_or(false) {
            let _ = self.editor.recover_all_buffers();
        }

        self.editor.start_recovery_session()?;

        self.render()?;
        Ok(restored)
    }

    /// Update shadow string to mirror key operations
    /// This helps catch discrepancies between piece tree and simple string operations
    fn update_shadow_for_key(&mut self, code: KeyCode, modifiers: KeyModifiers) {
        // Handle Ctrl combinations: undo, redo, and special navigation
        if modifiers.contains(KeyModifiers::CONTROL) {
            match code {
                KeyCode::Char('z') => {
                    // Undo: restore content, set undo_cursor, push current to redo stack
                    if let Some((content, undo_cursor, redo_cursor)) = self.shadow_undo_stack.pop()
                    {
                        self.shadow_redo_stack.push((
                            self.shadow_string.clone(),
                            redo_cursor,
                            undo_cursor,
                        ));
                        self.shadow_string = content;
                        self.shadow_cursor = undo_cursor;
                    }
                    return;
                }
                KeyCode::Char('y') => {
                    // Redo: restore content, set redo_cursor, push current to undo stack
                    if let Some((content, redo_cursor, undo_cursor)) = self.shadow_redo_stack.pop()
                    {
                        self.shadow_undo_stack.push((
                            self.shadow_string.clone(),
                            undo_cursor,
                            redo_cursor,
                        ));
                        self.shadow_string = content;
                        self.shadow_cursor = redo_cursor;
                    }
                    return;
                }
                KeyCode::Home => {
                    self.shadow_cursor = 0;
                    return;
                }
                KeyCode::End => {
                    self.shadow_cursor = self.shadow_string.len();
                    return;
                }
                _ => {
                    // Ignore other Ctrl combinations
                    return;
                }
            }
        }

        if modifiers.contains(KeyModifiers::ALT) {
            match code {
                KeyCode::Up => {
                    let len = self.shadow_string.len();
                    if len == 0 {
                        return;
                    }
                    let line_start = self.shadow_string[..self.shadow_cursor]
                        .rfind('\n')
                        .map(|pos| pos + 1)
                        .unwrap_or(0);
                    if line_start == 0 || line_start >= len {
                        return;
                    }
                    let line_end = self.shadow_string[self.shadow_cursor..]
                        .find('\n')
                        .map(|pos| self.shadow_cursor + pos)
                        .unwrap_or(len);
                    let prev_line_end = line_start.saturating_sub(1);
                    let prev_line_start = self.shadow_string[..prev_line_end]
                        .rfind('\n')
                        .map(|pos| pos + 1)
                        .unwrap_or(0);
                    let line_has_newline = line_end < len;
                    let line_end_with_sep = if line_has_newline {
                        line_end + 1
                    } else {
                        line_end
                    };

                    let before_prev = &self.shadow_string[..prev_line_start];
                    let prev_line = &self.shadow_string[prev_line_start..prev_line_end];
                    let cur_line = &self.shadow_string[line_start..line_end];
                    let after_cur = &self.shadow_string[line_end_with_sep..];

                    let mut new_string = String::with_capacity(self.shadow_string.len());
                    new_string.push_str(before_prev);
                    new_string.push_str(cur_line);
                    new_string.push('\n');
                    new_string.push_str(prev_line);
                    if line_has_newline {
                        new_string.push('\n');
                    }
                    new_string.push_str(after_cur);

                    let column = self.shadow_cursor.saturating_sub(line_start);
                    let redo_cursor = prev_line_start + column.min(cur_line.len());
                    // Real editor skips when new_text == old_text (no event recorded)
                    if new_string != self.shadow_string {
                        let undo_cursor = self.shadow_cursor;
                        self.shadow_undo_stack.push((
                            self.shadow_string.clone(),
                            undo_cursor,
                            redo_cursor,
                        ));
                        self.shadow_redo_stack.clear();
                    }
                    self.shadow_cursor = redo_cursor;
                    self.shadow_string = new_string;
                    return;
                }
                KeyCode::Down => {
                    let len = self.shadow_string.len();
                    if len == 0 {
                        return;
                    }
                    let line_start = self.shadow_string[..self.shadow_cursor]
                        .rfind('\n')
                        .map(|pos| pos + 1)
                        .unwrap_or(0);
                    let line_end = self.shadow_string[self.shadow_cursor..]
                        .find('\n')
                        .map(|pos| self.shadow_cursor + pos)
                        .unwrap_or(len);
                    if line_end == len {
                        return;
                    }
                    let next_line_start = line_end + 1;
                    // Trailing empty line after final '\n' — real editor treats as no-op
                    if next_line_start >= len {
                        return;
                    }
                    let next_line_end = self.shadow_string[next_line_start..]
                        .find('\n')
                        .map(|pos| next_line_start + pos)
                        .unwrap_or(len);
                    let next_has_newline = next_line_end < len;
                    let next_end_with_sep = if next_has_newline {
                        next_line_end + 1
                    } else {
                        next_line_end
                    };

                    let before_line = &self.shadow_string[..line_start];
                    let cur_line = &self.shadow_string[line_start..line_end];
                    let next_line = &self.shadow_string[next_line_start..next_line_end];
                    let after_next = &self.shadow_string[next_end_with_sep..];

                    let mut new_string = String::with_capacity(self.shadow_string.len());
                    new_string.push_str(before_line);
                    new_string.push_str(next_line);
                    new_string.push('\n');
                    new_string.push_str(cur_line);
                    if next_has_newline {
                        new_string.push('\n');
                    }
                    new_string.push_str(after_next);

                    let column = self.shadow_cursor.saturating_sub(line_start);
                    let new_line_start = line_start + next_line.len() + 1;
                    let redo_cursor = new_line_start + column.min(cur_line.len());
                    // Real editor skips when new_text == old_text (no event recorded)
                    if new_string != self.shadow_string {
                        let undo_cursor = self.shadow_cursor;
                        self.shadow_undo_stack.push((
                            self.shadow_string.clone(),
                            undo_cursor,
                            redo_cursor,
                        ));
                        self.shadow_redo_stack.clear();
                    }
                    self.shadow_cursor = redo_cursor;
                    self.shadow_string = new_string;
                    return;
                }
                _ => {
                    return;
                }
            }
        }

        match code {
            KeyCode::Char(ch) => {
                // Insert: undo_cursor = pre-action, redo_cursor = post-action
                let undo_cursor = self.shadow_cursor;
                let redo_cursor = self.shadow_cursor + ch.len_utf8();
                self.shadow_undo_stack
                    .push((self.shadow_string.clone(), undo_cursor, redo_cursor));
                self.shadow_redo_stack.clear();
                self.shadow_string.insert(self.shadow_cursor, ch);
                self.shadow_cursor = redo_cursor;
            }
            KeyCode::Backspace => {
                if self.shadow_cursor > 0 {
                    // Smart backspace dedent: if cursor is preceded only by whitespace
                    // on the current line, remove up to tab_size spaces at once.
                    let line_start = self.shadow_string[..self.shadow_cursor]
                        .rfind('\n')
                        .map(|pos| pos + 1)
                        .unwrap_or(0);
                    let prefix = &self.shadow_string[line_start..self.shadow_cursor];
                    let all_whitespace =
                        !prefix.is_empty() && prefix.bytes().all(|b| b == b' ' || b == b'\t');

                    let chars_to_remove = if all_whitespace {
                        let last_byte = prefix.as_bytes()[prefix.len() - 1];
                        if last_byte == b'\t' {
                            1
                        } else {
                            let trailing_spaces =
                                prefix.bytes().rev().take_while(|&b| b == b' ').count();
                            let tab_size = 4; // default tab_size
                            trailing_spaces.min(tab_size)
                        }
                    } else {
                        1
                    };

                    let undo_cursor = self.shadow_cursor;
                    let redo_cursor = self.shadow_cursor - chars_to_remove;
                    self.shadow_undo_stack.push((
                        self.shadow_string.clone(),
                        undo_cursor,
                        redo_cursor,
                    ));
                    self.shadow_redo_stack.clear();
                    // Remove chars_to_remove characters before cursor
                    self.shadow_string.drain(redo_cursor..self.shadow_cursor);
                    self.shadow_cursor = redo_cursor;
                }
            }
            KeyCode::Delete => {
                if self.shadow_cursor < self.shadow_string.len() {
                    // Forward delete: undo inserts text back, which shifts cursor right
                    // undo_cursor = P + 1 (adjust_for_edit shifts cursor on undo-insert)
                    // redo_cursor = P (cursor stays on forward delete)
                    let undo_cursor = self.shadow_cursor + 1;
                    let redo_cursor = self.shadow_cursor;
                    self.shadow_undo_stack.push((
                        self.shadow_string.clone(),
                        undo_cursor,
                        redo_cursor,
                    ));
                    self.shadow_redo_stack.clear();
                    self.shadow_string.remove(self.shadow_cursor);
                }
            }
            KeyCode::Enter => {
                // Enter (insert newline): undo_cursor = pre-action, redo_cursor = post-action
                let undo_cursor = self.shadow_cursor;
                let redo_cursor = self.shadow_cursor + 1;
                self.shadow_undo_stack
                    .push((self.shadow_string.clone(), undo_cursor, redo_cursor));
                self.shadow_redo_stack.clear();
                self.shadow_string.insert(self.shadow_cursor, '\n');
                self.shadow_cursor = redo_cursor;
            }
            KeyCode::Left => {
                if self.shadow_cursor > 0 {
                    self.shadow_cursor -= 1;
                }
            }
            KeyCode::Right => {
                if self.shadow_cursor < self.shadow_string.len() {
                    self.shadow_cursor += 1;
                }
            }
            KeyCode::Home => {
                // Smart home: toggle between first non-whitespace and line start
                let line_start = self.shadow_string[..self.shadow_cursor]
                    .rfind('\n')
                    .map(|pos| pos + 1)
                    .unwrap_or(0);
                let line_end = self.shadow_string[line_start..]
                    .find('\n')
                    .map(|pos| line_start + pos)
                    .unwrap_or(self.shadow_string.len());
                let first_non_ws = self.shadow_string[line_start..line_end]
                    .find(|c: char| c != ' ' && c != '\t')
                    .map(|offset| line_start + offset)
                    .unwrap_or(line_start);
                if self.shadow_cursor == first_non_ws {
                    self.shadow_cursor = line_start;
                } else {
                    self.shadow_cursor = first_non_ws;
                }
            }
            KeyCode::End => {
                // Find end of current line
                let line_end = self.shadow_string[self.shadow_cursor..]
                    .find('\n')
                    .map(|pos| self.shadow_cursor + pos)
                    .unwrap_or(self.shadow_string.len());
                self.shadow_cursor = line_end;
            }
            KeyCode::Up => {
                // Move to previous line, same column position
                let current_line_start = self.shadow_string[..self.shadow_cursor]
                    .rfind('\n')
                    .map(|pos| pos + 1)
                    .unwrap_or(0);
                let column = self.shadow_cursor - current_line_start;

                if current_line_start > 0 {
                    // Find start of previous line
                    let prev_line_end = current_line_start - 1; // The '\n' before current line
                    let prev_line_start = self.shadow_string[..prev_line_end]
                        .rfind('\n')
                        .map(|pos| pos + 1)
                        .unwrap_or(0);
                    let prev_line_len = prev_line_end - prev_line_start;

                    // Move to same column or end of previous line
                    self.shadow_cursor = prev_line_start + column.min(prev_line_len);
                }
            }
            KeyCode::Down => {
                // Move to next line, same column position
                let current_line_start = self.shadow_string[..self.shadow_cursor]
                    .rfind('\n')
                    .map(|pos| pos + 1)
                    .unwrap_or(0);
                let column = self.shadow_cursor - current_line_start;

                // Find next line start
                if let Some(next_line_start_offset) =
                    self.shadow_string[self.shadow_cursor..].find('\n')
                {
                    let next_line_start = self.shadow_cursor + next_line_start_offset + 1;
                    if next_line_start < self.shadow_string.len() {
                        // Find next line end
                        let next_line_end = self.shadow_string[next_line_start..]
                            .find('\n')
                            .map(|pos| next_line_start + pos)
                            .unwrap_or(self.shadow_string.len());
                        let next_line_len = next_line_end - next_line_start;

                        // Move to same column or end of next line
                        self.shadow_cursor = next_line_start + column.min(next_line_len);
                    }
                }
            }
            _ => {
                // Other keys don't modify shadow (e.g., PageUp, PageDown)
            }
        }
    }

    /// Get the primary cursor position
    pub fn cursor_position(&self) -> usize {
        self.editor.active_cursors().primary().position
    }

    /// Get the buffer length in bytes
    pub fn buffer_len(&self) -> usize {
        self.editor.active_state().buffer.len()
    }

    /// Check if the current buffer has a highlighter set up
    pub fn has_highlighter(&self) -> bool {
        !matches!(
            self.editor.active_state().highlighter,
            HighlightEngine::None
        )
    }

    /// Get highlight performance stats (TextMate engine only).
    pub fn highlight_stats(&self) -> Option<&HighlightStats> {
        self.editor.active_state().highlighter.highlight_stats()
    }

    /// Reset highlight performance counters.
    pub fn reset_highlight_stats(&mut self) {
        self.editor
            .active_state_mut()
            .highlighter
            .reset_highlight_stats();
    }

    /// Get the shadow string (for property testing)
    pub fn get_shadow_string(&self) -> &str {
        &self.shadow_string
    }

    /// Get the shadow cursor position (for property testing)
    pub fn get_shadow_cursor(&self) -> usize {
        self.shadow_cursor
    }

    /// Get the number of cursors
    pub fn cursor_count(&self) -> usize {
        self.editor.active_cursors().count()
    }

    /// Count the number of search highlight overlays in the current buffer
    pub fn count_search_highlights(&self) -> usize {
        self.editor
            .active_state()
            .overlays
            .all()
            .iter()
            .filter(|o| {
                o.namespace
                    .as_ref()
                    .map(|ns| ns.as_str().starts_with("search"))
                    .unwrap_or(false)
            })
            .count()
    }

    /// Get the screen cursor position (x, y) from the terminal
    pub fn screen_cursor_position(&mut self) -> (u16, u16) {
        let pos = self.terminal.get_cursor_position().unwrap_or_default();
        (pos.x, pos.y)
    }

    /// Render and report whether the hardware cursor was shown, and where.
    ///
    /// Returns `Some((x, y))` if ratatui's `Terminal::draw` ended the frame
    /// by calling `show_cursor` + `set_cursor_position` — i.e. the editor
    /// populated `Frame::cursor_position`. Returns `None` if `hide_cursor`
    /// was called — i.e. `Frame::cursor_position` was `None`.
    ///
    /// Detection uses a sentinel: before `draw`, we move the backend's
    /// cursor to `(0, 0)`. `Terminal::draw` only updates the backend
    /// position when the frame asked for the cursor to be shown, so if the
    /// position stays at the sentinel afterward, the frame hid it. The
    /// editor never places its hardware cursor at `(0, 0)` (row 0 is the
    /// menu bar), so the sentinel is unambiguous.
    pub fn render_observing_cursor(&mut self) -> anyhow::Result<Option<(u16, u16)>> {
        self.terminal
            .set_cursor_position(ratatui::layout::Position::new(0, 0))?;
        self.render()?;
        let pos = self.terminal.get_cursor_position().unwrap_or_default();
        if pos.x == 0 && pos.y == 0 {
            Ok(None)
        } else {
            Ok(Some((pos.x, pos.y)))
        }
    }

    /// Find all visible cursors on screen
    /// Returns a vec of (x, y, character_at_cursor, is_primary)
    /// Primary cursor is detected at hardware cursor position
    /// Secondary cursors are detected by REVERSED style modifier or inactive cursor background
    pub fn find_all_cursors(&mut self) -> Vec<(u16, u16, String, bool)> {
        use ratatui::style::Modifier;
        let mut cursors = Vec::new();

        // Get hardware cursor position (primary cursor)
        let (hw_x, hw_y) = self.screen_cursor_position();

        // Get the buffer to read cell content
        let theme_inactive_cursor = self.editor.theme().inactive_cursor;
        let buffer = self.terminal.backend().buffer();
        let content_start = layout::CONTENT_START_ROW as u16;
        let content_end = buffer
            .area
            .height
            .saturating_sub(layout::BOTTOM_RESERVED_ROWS as u16);

        // Scrollbar is in the rightmost column - exclude from cursor detection
        let scrollbar_col = buffer.area.width.saturating_sub(1);

        // Add primary cursor at hardware position
        if hw_y >= content_start && hw_y < content_end {
            if let Some(cell) = buffer.content.get(buffer.index_of(hw_x, hw_y)) {
                cursors.push((hw_x, hw_y, cell.symbol().to_string(), true));
            }
        }

        // Inactive cursor bg is taken from the active theme; any cell matching
        // that bg in the content area is treated as a secondary cursor.
        let inactive_cursor_bg = theme_inactive_cursor;

        for y in content_start..content_end {
            for x in 0..buffer.area.width {
                // Skip if this is the hardware cursor position
                if x == hw_x && y == hw_y {
                    continue;
                }

                // Skip scrollbar column - scrollbar uses background colors that overlap
                // with inactive cursor colors (DarkGray, Rgb(180,180,180))
                if x == scrollbar_col {
                    continue;
                }

                let pos = buffer.index_of(x, y);
                if let Some(cell) = buffer.content.get(pos) {
                    let is_reversed = cell.modifier.contains(Modifier::REVERSED);
                    let has_inactive_cursor_bg = cell.bg == inactive_cursor_bg;
                    if is_reversed || has_inactive_cursor_bg {
                        cursors.push((x, y, cell.symbol().to_string(), false));
                    }
                }
            }
        }

        cursors
    }

    /// Get the top line number currently visible in the viewport
    pub fn top_line_number(&mut self) -> usize {
        let top_byte = self.editor.active_viewport().top_byte;
        self.editor
            .active_state_mut()
            .buffer
            .get_line_number(top_byte)
    }

    /// Get the top byte position of the viewport
    pub fn top_byte(&self) -> usize {
        self.editor.active_viewport().top_byte
    }

    /// Get the top view line offset (number of view lines to skip)
    pub fn top_view_line_offset(&self) -> usize {
        self.editor.active_viewport().top_view_line_offset
    }

    /// Get the viewport height (number of content lines that can be displayed)
    pub fn viewport_height(&self) -> usize {
        self.editor.active_viewport().height as usize
    }

    /// Get the content area row range on screen (start_row, end_row inclusive)
    /// This accounts for menu bar, tab bar, status bar, and prompt line
    pub fn content_area_rows(&self) -> (usize, usize) {
        let terminal_height = self.terminal.size().unwrap().height as usize;
        let content_first_row = layout::CONTENT_START_ROW;
        let content_last_row = layout::content_end_row(terminal_height).saturating_sub(1);
        (content_first_row, content_last_row)
    }

    /// Get the terminal height
    pub fn terminal_height(&self) -> usize {
        self.terminal.size().unwrap().height as usize
    }

    /// Get a specific row from the screen as a string
    pub fn get_screen_row(&self, row: usize) -> String {
        let screen = self.screen_to_string();
        screen
            .lines()
            .nth(row)
            .map(|s| s.to_string())
            .unwrap_or_default()
    }

    /// Get the menu bar row content
    pub fn get_menu_bar(&self) -> String {
        self.get_screen_row(layout::MENU_BAR_ROW)
    }

    /// Get the tab bar row content
    pub fn get_tab_bar(&self) -> String {
        self.get_screen_row(layout::TAB_BAR_ROW)
    }

    /// Get the status bar row content
    pub fn get_status_bar(&self) -> String {
        self.get_screen_row(layout::status_bar_row(self.terminal_height()))
    }

    /// Get the prompt line content
    pub fn get_prompt_line(&self) -> String {
        self.get_screen_row(layout::prompt_line_row(self.terminal_height()))
    }

    /// Get the primary cursor's selection range, if any
    pub fn get_selection_range(&self) -> Option<std::ops::Range<usize>> {
        self.editor.active_cursors().primary().selection_range()
    }

    /// Check if there's an active selection
    pub fn has_selection(&self) -> bool {
        !self.editor.active_cursors().primary().collapsed()
    }

    /// Get the selected text (if any)
    pub fn get_selected_text(&mut self) -> String {
        if let Some(range) = self.get_selection_range() {
            self.editor
                .active_state_mut()
                .get_text_range(range.start, range.end)
        } else {
            String::new()
        }
    }

    /// Assert that no selection exists
    pub fn assert_no_selection(&self) {
        assert!(!self.has_selection(), "Expected no selection but found one");
    }

    /// Resize the terminal to new dimensions
    /// This simulates terminal resize events and updates both the virtual terminal
    /// backend and the editor's viewport
    pub fn resize(&mut self, width: u16, height: u16) -> anyhow::Result<()> {
        // Resize the virtual terminal backend
        self.terminal.backend_mut().resize(width, height);
        // Resize the editor's viewports
        self.editor.resize(width, height);
        // Re-render to reflect the new size
        self.render()?;
        Ok(())
    }

    /// Process pending async messages (including file polling) and render
    /// Useful for testing async features like git grep, file explorer, auto-revert, etc.
    pub fn process_async_and_render(&mut self) -> anyhow::Result<()> {
        let _ = self.editor.process_async_messages();
        // Check debounced completion trigger timer (quick suggestions)
        self.editor.check_completion_trigger_timer();
        self.render()?;
        Ok(())
    }

    /// Run a full editor tick (the same work the real event loop performs
    /// between frames) followed by a render.  This includes async message
    /// processing, search-scan progress, search-overlay refresh, and all
    /// other periodic checks that `editor_tick()` handles.
    pub fn tick_and_render(&mut self) -> anyhow::Result<()> {
        let _ = fresh::app::editor_tick(&mut self.editor, || Ok(()));
        self.render()?;
        Ok(())
    }
    /// Wait indefinitely for async operations until condition is met.
    /// Runs a full editor tick each iteration — the same work the real event
    /// loop performs between frames — so that hover timers, debounced requests,
    /// diagnostic pulls, and all other periodic checks fire naturally.
    ///
    /// Note: Uses a short real wall-clock sleep between iterations to allow
    /// async I/O operations (running on tokio runtime) time to complete.
    pub fn wait_until<F>(&mut self, mut condition: F) -> anyhow::Result<()>
    where
        F: FnMut(&Self) -> bool,
    {
        const WAIT_SLEEP: std::time::Duration = std::time::Duration::from_millis(50);
        // Dump the screen periodically so CI logs show what the test sees while stuck
        const SCREEN_DUMP_INTERVAL: std::time::Duration = std::time::Duration::from_secs(10);

        tracing::info!("waiting...");
        let start = std::time::Instant::now();
        let mut last_dump = start;
        loop {
            self.tick_and_render()?;
            if condition(self) {
                return Ok(());
            }
            let now = std::time::Instant::now();
            if now.duration_since(last_dump) >= SCREEN_DUMP_INTERVAL {
                let elapsed = now.duration_since(start);
                tracing::warn!(
                    "wait_until still pending after {:.1}s — screen:\n{}",
                    elapsed.as_secs_f64(),
                    self.screen_to_string()
                );
                last_dump = now;
            }
            // Sleep for real wall-clock time to allow async I/O operations to complete
            // These run on the tokio runtime and need actual time, not logical time
            std::thread::sleep(WAIT_SLEEP);
            // Also advance test time so time-based features (polling, debounce) continue working
            self.advance_time(WAIT_SLEEP);
        }
    }
    /// Like `wait_until`, but after the condition is met, continues waiting
    /// until the screen stops changing (two consecutive renders produce the
    /// same output).  Use this when the semantic condition can be met before
    /// all async plugin work (conceals, soft-breaks, etc.) has settled.
    pub fn wait_until_stable<F>(&mut self, mut condition: F) -> anyhow::Result<()>
    where
        F: FnMut(&Self) -> bool,
    {
        // Phase 1: wait for the semantic condition
        self.wait_until(&mut condition)?;
        // Phase 2: wait for screen stability
        let mut prev = String::new();
        self.wait_until(|h| {
            let s = h.screen_to_string();
            let stable = s == prev;
            prev = s;
            stable
        })
    }

    // ===== File Explorer Wait Helpers =====

    /// Wait for file explorer to be initialized (has a view)
    pub fn wait_for_file_explorer(&mut self) -> anyhow::Result<()> {
        self.wait_until(|h| h.editor().file_explorer().is_some())
    }

    /// Wait for file explorer to show a specific item by name (in the tree, not tabs)
    /// The file explorer tree uses │ characters, so we check for lines containing both
    /// Also ensures the file_explorer object exists (not taken for async operation)
    pub fn wait_for_file_explorer_item(&mut self, name: &str) -> anyhow::Result<()> {
        let name = name.to_string();
        self.wait_until(move |h| {
            // Ensure file_explorer exists (not None during async operation)
            if h.editor().file_explorer().is_none() {
                return false;
            }
            let screen = h.screen_to_string();
            // Look for the item in a file explorer tree line (contains │ tree connector)
            // or in a line with tree markers like > or ▼
            screen.lines().any(|line| {
                line.contains(&name)
                    && (line.contains("│") || line.contains(">") || line.contains("▼"))
            })
        })
    }

    /// Wait for a prompt to become active
    pub fn wait_for_prompt(&mut self) -> anyhow::Result<()> {
        self.wait_until(|h| h.editor().is_prompting())
    }

    /// Wait for prompt to close (no longer prompting)
    pub fn wait_for_prompt_closed(&mut self) -> anyhow::Result<()> {
        self.wait_until(|h| !h.editor().is_prompting())
    }

    /// Open the settings dialog via command palette
    /// This is the preferred way to open settings since Ctrl+, doesn't work reliably in terminals
    pub fn open_settings(&mut self) -> anyhow::Result<()> {
        // Open command palette
        self.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
        self.wait_for_prompt()?;

        // Type "Settings" and press Enter
        self.type_text("Settings")?;
        self.send_key(KeyCode::Enter, KeyModifiers::NONE)?;

        // Wait for settings to appear
        self.wait_for_screen_contains("Settings")?;
        self.render()?;

        Ok(())
    }

    /// Wait for screen to contain specific text
    pub fn wait_for_screen_contains(&mut self, text: &str) -> anyhow::Result<()> {
        let text = text.to_string();
        tracing::info!("wait_for_screen_contains: {:?}", text);
        self.wait_until(move |h| h.screen_to_string().contains(&text))
    }

    /// Wait for buffer content to match expected value
    /// Useful for async plugin operations that modify the buffer
    pub fn wait_for_buffer_content(&mut self, expected: &str) -> anyhow::Result<()> {
        let expected = expected.to_string();
        self.wait_until(move |h| h.get_buffer_content() == Some(expected.clone()))
    }

    /// Capture a visual step for regression testing
    /// This takes both a text snapshot (for testing) and generates an SVG (for visualization)
    pub fn capture_visual_step(
        &mut self,
        flow: &mut crate::common::visual_testing::VisualFlow,
        step_name: &str,
        description: &str,
    ) -> anyhow::Result<()> {
        self.render()?;
        let cursor_pos = self.screen_cursor_position();
        flow.step(self.buffer(), cursor_pos, step_name, description)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_harness_creation() {
        let harness = EditorTestHarness::new(80, 24).unwrap();
        assert!(!harness.should_quit());
    }

    #[test]
    fn test_harness_render() {
        let mut harness = EditorTestHarness::new(80, 24).unwrap();
        harness.render().unwrap();

        let screen = harness.screen_to_string();
        assert!(!screen.is_empty());
    }

    #[test]
    fn test_strip_osc8() {
        // Plain text unchanged
        assert_eq!(strip_osc8("hello world"), "hello world");

        // Standard OSC 8 link
        assert_eq!(
            strip_osc8("\x1b]8;;https://example.com\x07Click\x1b]8;;\x07"),
            "Click",
        );

        // Multiple OSC 8 links
        assert_eq!(
            strip_osc8(
                "before \x1b]8;;url1\x07A\x1b]8;;\x07 mid \x1b]8;;url2\x07B\x1b]8;;\x07 after"
            ),
            "before A mid B after",
        );

        // 2-char chunk (as produced by apply_osc8_to_cells)
        assert_eq!(strip_osc8("\x1b]8;;#install\x07Qu\x1b]8;;\x07"), "Qu",);

        // Empty OSC 8 (link terminator)
        assert_eq!(strip_osc8("\x1b]8;;\x07"), "");

        // No BEL terminator (malformed) — consumed without output
        assert_eq!(strip_osc8("\x1b]8;;url"), "");

        // Empty string
        assert_eq!(strip_osc8(""), "");
    }

    #[test]
    fn test_buffer_content() {
        let harness = EditorTestHarness::new(80, 24).unwrap();
        let content = harness.get_buffer_content().unwrap();
        assert_eq!(content, ""); // New buffer is empty
    }

    #[test]
    fn find_text_in_row_returns_cell_column_not_byte_offset() {
        // Mixed-width row: 30 box-drawing cells (`│`, 3 bytes each), 70
        // ASCII spaces, then "ALPHA". Joined byte length is 30*3 + 70 + 5
        // = 165 bytes, but ALPHA's cell column is 100 in a 120-cell row.
        // Before the byte→cell translation, callers got byte offset 160
        // back, which then panicked in `Buffer::index_of` because it's
        // past the 120-cell buffer width.
        let mut cells: Vec<&str> = Vec::with_capacity(120);
        cells.extend(std::iter::repeat_n("│", 30));
        cells.extend(std::iter::repeat_n(" ", 70));
        cells.extend(["A", "L", "P", "H", "A"]);
        // Pad out to a 120-wide row so the test mirrors the dashboard's
        // actual buffer width.
        cells.extend(std::iter::repeat_n(" ", 120 - cells.len()));

        let col = find_text_in_row(cells.iter().copied(), "ALPHA");
        assert_eq!(col, Some(100));
    }

    #[test]
    fn find_text_in_row_handles_pure_ascii() {
        // For all-ASCII rows the cell column equals the byte offset, so
        // this just guards against regressions in the simple path.
        let row = ["a", "b", "c", "d", "e", "f"];
        assert_eq!(find_text_in_row(row.iter().copied(), "cd"), Some(2));
        assert_eq!(find_text_in_row(row.iter().copied(), "zz"), None);
    }
}
