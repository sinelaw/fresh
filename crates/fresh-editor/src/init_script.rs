//! User init.ts support.
//!
//! At startup Fresh reads `~/.config/fresh/init.ts` (if present) and feeds it
//! through the existing plugin pipeline as a plugin named `init.ts`. This is
//! the same code path as "Load Plugin from Buffer", so reload, unload, and
//! per-plugin registration tagging are free.
//!
//! Recovery: a lightweight crash fuse at
//! `~/.config/fresh/logs/init.crashes` counts consecutive init.ts failures
//! within a rolling window. After N failures the next launch auto-skips
//! init.ts until the user fixes or removes it. A successful evaluation
//! clears the counter.

use std::path::{Path, PathBuf};

/// How many consecutive failed attempts trigger auto-skip.
const CRASH_FUSE_THRESHOLD: u32 = 3;
/// Rolling window (seconds) beyond which stale failures are ignored.
const CRASH_FUSE_WINDOW_SECS: u64 = 300;
/// Plugin name Fresh uses when loading init.ts — stable so hot-reload works.
pub const INIT_PLUGIN_NAME: &str = "init.ts";

/// Starter content written by `init: Edit init.ts` when the file doesn't
/// exist yet. Every example is commented out — an empty init() body is
/// valid and users un-comment what they want.
///
/// The comments establish what init.ts is *not* for (static preferences,
/// keybindings, themes, reusable features) so users don't reach for this
/// file when another surface is the right tool.
pub const STARTER_TEMPLATE: &str = r#"/// <reference path="./types/fresh.d.ts" />
/// <reference path="./types/plugins.d.ts" />
const editor = getEditor();

// Fresh init.ts — decisions that depend on the environment at startup.
//
// init.ts is NOT for:
//   - Static preferences (tab size, line numbers, ...)  -> Settings UI
//   - Key bindings                                      -> Keybindings editor
//   - Themes you always want                            -> Theme selector
//   - Reusable features                                 -> A plugin package
//
// init.ts IS for things that:
//   - Register code handlers, commands, etc.
//   - Depend on where/how Fresh is starting (host, SSH, $TERM, project, ...)
//   - Would differ across machines or launches
//   - Can't live in a shared config.json without lying to teammates
//
// API reference: ~/.config/fresh/types/fresh.d.ts (same as plugins)
// Commands:  Ctrl+P -> "init: Reload", "init: Check"
// CLI:       fresh --cmd init check | fresh --safe | fresh --no-init

// Example: Add a command to select (mark) from current cursor to target line.
//
// registerHandler("select_to_line_handler", async function start_review_range() {
//   editor.executeActions([
//     { action: "set_mark", count: 1 },
//     { action: "goto_line", count: 1 },
//   ]);
// });
//
// editor.registerCommand(
//   "select_to_line",
//   "Select from current position to target line",
//   "select_to_line_handler",
// );
//

// Example: fade the editor in from black to the target theme. Uses
// `overrideThemeColors` (in-memory, no disk I/O) for each frame, then
// calls `applyTheme` at the end to drop the overrides and land cleanly
// on the saved theme. `editor.delay(ms)` returns a Promise, so an async
// for-loop is all the timing machinery we need — no setInterval.
// (async () => {
//     const target = "one-dark";
//     const data = editor.getThemeData(target) as
//         | { editor?: Record<string, [number, number, number]> }
//         | null;
//     const bg = data?.editor?.bg ?? [30, 30, 30];
//     const fg = data?.editor?.fg ?? [220, 220, 220];
//     const frames = 18;
//     const stepMs = 16;
//     const lerp = (a: number, b: number, t: number) =>
//         Math.round(a + (b - a) * t);
//     for (let i = 1; i <= frames; i++) {
//         const t = i / frames;
//         editor.overrideThemeColors({
//             "editor.bg": [lerp(0, bg[0], t), lerp(0, bg[1], t), lerp(0, bg[2], t)],
//             "editor.fg": [lerp(0, fg[0], t), lerp(0, fg[1], t), lerp(0, fg[2], t)],
//         });
//         await editor.delay(stepMs);
//     }
//     editor.applyTheme(target); // drop overrides, settle on the real theme
// })();

// Example: calmer UI over SSH. setSetting writes to the runtime layer —
// nothing is persisted to disk, and removing this file is a complete undo.
// if (editor.getEnv("SSH_TTY")) {
//     editor.setSetting("editor.diagnostics_inline_text", false);
//     editor.setSetting("terminal.mouse", false);
// }

// Example: host-specific rust-analyzer path.
// if (editor.getEnv("HOSTNAME") === "my-mac") {
//     editor.registerLspServer("rust", {
//         command: "/opt/homebrew/bin/rust-analyzer",
//         args: [],
//         autoStart: true,
//         initializationOptions: null,
//         processLimits: null,
//     });
// }

// Example: env-driven profile (fresh invoked as FRESH_PROFILE=writing fresh).
// if (editor.getEnv("FRESH_PROFILE") === "writing") {
//     editor.setSetting("editor.line_wrap", true);
//     editor.setSetting("editor.wrap_column", 80);
// }

// Example: configure a plugin once it loads. `plugins_loaded` fires after
// every registry plugin and init.ts top-level code has run.
// editor.on("plugins_loaded", () => {
//     const api = editor.getPluginApi("my-plugin");
//     if (api) api.configure({ option: "value" });
// });

// Example: enable the opt-in Dashboard widgets (weather, GitHub).
// Both hit the network on every refresh, so the plugin ships with
// only `git` and `disk` registered by default. The handlers live
// on the exported plugin API as `builtinHandlers` — pass them to
// `registerSection` with whatever name you like.
//
// editor.on("plugins_loaded", () => {
//     const dash = editor.getPluginApi("dashboard");
//     if (!dash) return;
//     dash.registerSection("weather", dash.builtinHandlers.weather);
//     dash.registerSection("github", dash.builtinHandlers.github);
// });

// Example: disable the Dashboard's auto-open behaviour on this
// machine (it will still be available via the "Show Dashboard"
// command). The same toggle can also be set persistently in
// config.json at `plugins.dashboard.auto-open`.
//
// editor.on("plugins_loaded", () => {
//     const dash = editor.getPluginApi("dashboard");
//     if (dash) dash.setAutoOpen(false);
// });

// Example: add a custom section to the Dashboard plugin.
//
// `editor.getPluginApi("dashboard")` is typed automatically via
// `types/plugins.d.ts` — no `as` cast needed. Hover over `dash` or
// `ctx` in your editor to see the full API.
//
// editor.on("plugins_loaded", () => {
//     const dash = editor.getPluginApi("dashboard");
//     if (!dash) return;
//     dash.registerSection("todo", async (ctx) => {
//         // Pretend we read a TODO count from somewhere async.
//         const count = 3;
//         if (count === 0) {
//             ctx.kv("status", "inbox zero", "ok");
//             return;
//         }
//         ctx.kv("open", String(count), count > 5 ? "warn" : "value");
//         ctx.text("    " + "see all".padEnd(10), { color: "muted" });
//         ctx.text("open inbox", {
//             color: "accent",
//             bold: true,
//             onClick: () => editor.executeAction("open_inbox"),
//         });
//         ctx.newline();
//     });
// });
"#;

/// `tsconfig.json` for the user's init.ts. Matches the plugin-dev
/// workspace (no DOM, no ambient types) so LSP behaviour is consistent
/// with plugins.
const INIT_TSCONFIG: &str = r#"{
  "compilerOptions": {
    "target": "ES2020",
    "module": "ES2020",
    "moduleResolution": "node",
    "strict": true,
    "noEmit": true,
    "skipLibCheck": true,
    "lib": ["ES2020"],
    "types": []
  },
  "files": ["init.ts", "types/fresh.d.ts", "types/plugins.d.ts"]
}
"#;

/// Resolve the path to `fresh.d.ts` inside the embedded-plugins cache.
/// Only embedded content is used — never an on-disk copy that isn't
/// guaranteed to match this binary — so the types always track the
/// running build.
#[cfg(feature = "embed-plugins")]
fn embedded_fresh_dts_path() -> Option<PathBuf> {
    let embedded = crate::services::plugins::embedded::get_embedded_plugins_dir()?;
    let p = embedded.join("lib").join("fresh.d.ts");
    p.exists().then_some(p)
}

#[cfg(not(feature = "embed-plugins"))]
fn embedded_fresh_dts_path() -> Option<PathBuf> {
    None
}

/// Refresh `~/.config/fresh/types/fresh.d.ts` from the embedded copy and
/// write `tsconfig.json` if it isn't already present.
///
/// `fresh.d.ts` is **always overwritten** — it's an auto-generated API
/// mirror that must track the running binary. Keeping a stale copy in
/// `~/.config/fresh/types/` would silently hide drift between the API
/// the user's `init.ts` was written against and the one the running
/// binary actually exposes. `tsconfig.json` is treated as user-editable
/// and only written on first run.
///
/// Errors are logged but not returned: type scaffolding is best-effort
/// and must not block opening or loading init.ts.
pub fn refresh_types_scaffolding(config_dir: &Path) {
    let Some(source) = embedded_fresh_dts_path() else {
        tracing::warn!(
            "init.ts: embedded fresh.d.ts unavailable; \
             LSP completions in init.ts will be unavailable"
        );
        return;
    };

    let types_dir = config_dir.join("types");
    if let Err(e) = std::fs::create_dir_all(&types_dir) {
        tracing::warn!("init.ts: failed to create {}: {e}", types_dir.display());
        return;
    }
    let dest_dts = types_dir.join("fresh.d.ts");
    if let Err(e) = std::fs::copy(&source, &dest_dts) {
        tracing::warn!(
            "init.ts: failed to copy fresh.d.ts from {} to {}: {e}",
            source.display(),
            dest_dts.display()
        );
    }

    let tsconfig = config_dir.join("tsconfig.json");
    if !tsconfig.exists() {
        if let Err(e) = std::fs::write(&tsconfig, INIT_TSCONFIG) {
            tracing::warn!("init.ts: failed to write {}: {e}", tsconfig.display());
        }
    }
}

/// Write `<config_dir>/types/plugins.d.ts` from the `.d.ts` emit of
/// each loaded plugin. The editor calls this after scanning every
/// plugin directory, so by the time `init.ts` is evaluated the
/// ambient `FreshPluginRegistry` is fully populated and
/// `editor.getPluginApi("dashboard")` resolves to the typed overload.
///
/// Errors are logged but not returned: an empty or stale
/// `plugins.d.ts` must not block startup.
pub fn write_plugin_declarations(config_dir: &Path, declarations: &[(String, String)]) {
    let types_dir = config_dir.join("types");
    if let Err(e) = std::fs::create_dir_all(&types_dir) {
        tracing::warn!("init.ts: failed to create {}: {e}", types_dir.display());
        return;
    }
    let dest = types_dir.join("plugins.d.ts");

    // Stable order so a re-scan doesn't produce a needlessly
    // different file (and a noisy diff for users who version-control
    // their config dir).
    let mut sorted: Vec<&(String, String)> = declarations.iter().collect();
    sorted.sort_by(|a, b| a.0.cmp(&b.0));

    let mut body = String::new();
    body.push_str(
        "// AUTO-GENERATED by fresh — do not edit.\n\
         //\n\
         // Aggregate of every loaded plugin's isolated-declarations\n\
         // emit (oxc). This is what makes `editor.getPluginApi(\"foo\")`\n\
         // return a typed result in init.ts / downstream plugins —\n\
         // each plugin that declares `FreshPluginRegistry` here\n\
         // contributes its augmentation.\n\n",
    );
    for (name, dts) in sorted {
        let trimmed = dts.trim();
        // Script-style plugins with no exports get `export {};` appended
        // in the parser to force module mode. After isolated-declarations
        // strips internals, that's all that remains — a per-plugin
        // section with just `export {};` is pure noise in the aggregate.
        if trimmed.is_empty() || trimmed == "export {};" {
            continue;
        }
        body.push_str(&format!("// ── {name} ─────────────────────\n"));
        body.push_str(dts.trim_end());
        body.push_str("\n\n");
    }

    if let Err(e) = std::fs::write(&dest, &body) {
        tracing::warn!("init.ts: failed to write {}: {e}", dest.display());
    }
}

/// Ensure `~/.config/fresh/init.ts` exists. If absent, writes the starter
/// template. Also refreshes `types/fresh.d.ts` + `tsconfig.json` so the
/// template's `/// <reference path=...` directive resolves and
/// `getEditor()` type-checks in any TS-aware editor.
/// Returns the (possibly newly-created) `init.ts` path.
pub fn ensure_starter(config_dir: &Path) -> std::io::Result<PathBuf> {
    let path = init_ts_path(config_dir);
    if !path.exists() {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&path, STARTER_TEMPLATE)?;
    }
    refresh_types_scaffolding(config_dir);
    Ok(path)
}

/// Outcome of [`autoload`].
#[derive(Debug)]
pub enum InitOutcome {
    /// init.ts did not exist; nothing to do.
    NotFound,
    /// Skipped because `--no-init` / `--safe` was passed.
    Disabled,
    /// Skipped because the crash fuse engaged.
    CrashFused { failures: u32 },
    /// Loaded and evaluated successfully.
    Loaded,
    /// Evaluation produced an error; the status message has been set.
    Failed { message: String },
}

/// Resolve `~/.config/fresh/init.ts`.
pub fn init_ts_path(config_dir: &Path) -> PathBuf {
    config_dir.join("init.ts")
}

/// Resolve the crash-fuse counter file path.
fn crashes_path(config_dir: &Path) -> PathBuf {
    config_dir.join("logs").join("init.crashes")
}

#[derive(Debug, Default)]
struct CrashState {
    count: u32,
    last_increment_epoch: u64,
}

impl CrashState {
    fn load(config_dir: &Path) -> Self {
        let path = crashes_path(config_dir);
        let Ok(text) = std::fs::read_to_string(&path) else {
            return Self::default();
        };
        let mut count = 0u32;
        let mut last = 0u64;
        for (i, line) in text.lines().enumerate() {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }
            match i {
                0 => count = trimmed.parse().unwrap_or(0),
                1 => last = trimmed.parse().unwrap_or(0),
                _ => break,
            }
        }
        Self {
            count,
            last_increment_epoch: last,
        }
    }

    fn save(&self, config_dir: &Path) -> std::io::Result<()> {
        let path = crashes_path(config_dir);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(
            &path,
            format!("{}\n{}\n", self.count, self.last_increment_epoch),
        )
    }

    fn clear(config_dir: &Path) {
        let path = crashes_path(config_dir);
        if let Err(e) = std::fs::remove_file(&path) {
            if e.kind() != std::io::ErrorKind::NotFound {
                tracing::debug!(
                    "init.ts crash-fuse: failed to clear {}: {e}",
                    path.display()
                );
            }
        }
    }
}

fn now_epoch_secs() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

/// Called before loading init.ts. Returns `Some(failures)` if the fuse has
/// tripped and init.ts should be skipped; `None` if loading may proceed.
///
/// Also increments the counter — if init.ts evaluation succeeds, the caller
/// must invoke [`record_success`] to reset it.
fn check_and_increment_fuse(config_dir: &Path) -> Option<u32> {
    let now = now_epoch_secs();
    let mut state = CrashState::load(config_dir);

    // Stale entries outside the rolling window: treat as a clean slate.
    if state.last_increment_epoch == 0
        || now.saturating_sub(state.last_increment_epoch) > CRASH_FUSE_WINDOW_SECS
    {
        state.count = 0;
    }

    if state.count >= CRASH_FUSE_THRESHOLD {
        return Some(state.count);
    }

    state.count += 1;
    state.last_increment_epoch = now;
    if let Err(e) = state.save(config_dir) {
        tracing::debug!("init.ts crash-fuse: failed to persist counter: {e}");
    }

    None
}

/// Called after init.ts finishes cleanly. Resets the crash-fuse counter so
/// the next launch starts from zero.
pub fn record_success(config_dir: &Path) {
    CrashState::clear(config_dir);
}

/// Read init.ts from disk. Returns `Ok(None)` when the file simply doesn't
/// exist.
pub fn read_init_script(config_dir: &Path) -> std::io::Result<Option<String>> {
    let path = init_ts_path(config_dir);
    match std::fs::read_to_string(&path) {
        Ok(s) => Ok(Some(s)),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(e) => Err(e),
    }
}

/// Decide, without touching disk for the source, whether init.ts loading
/// should run at all.
pub fn should_skip(enabled: bool) -> bool {
    !enabled
}

/// Human-readable summary for the status bar / logs.
pub fn describe(outcome: &InitOutcome) -> String {
    match outcome {
        InitOutcome::NotFound => String::from("init.ts: not present"),
        InitOutcome::Disabled => String::from("init.ts: skipped (--no-init / --safe)"),
        InitOutcome::CrashFused { failures } => format!(
            "init.ts: skipped after {failures} consecutive failures — fix ~/.config/fresh/init.ts or remove it"
        ),
        InitOutcome::Loaded => String::from("init.ts: loaded"),
        InitOutcome::Failed { message } => format!("init.ts: {message}"),
    }
}

/// Pre-flight for the caller: check fuse, return either the source to load
/// or an outcome explaining why we're not loading.
pub enum LoadDecision {
    Skip(InitOutcome),
    Load { source: String },
}

pub fn decide_load(config_dir: &Path, enabled: bool) -> LoadDecision {
    if should_skip(enabled) {
        return LoadDecision::Skip(InitOutcome::Disabled);
    }
    match read_init_script(config_dir) {
        Ok(None) => LoadDecision::Skip(InitOutcome::NotFound),
        Err(e) => LoadDecision::Skip(InitOutcome::Failed {
            message: format!("read failed: {e}"),
        }),
        Ok(Some(source)) => {
            if let Some(failures) = check_and_increment_fuse(config_dir) {
                LoadDecision::Skip(InitOutcome::CrashFused { failures })
            } else {
                LoadDecision::Load { source }
            }
        }
    }
}

/// Result of `fresh --cmd init check`.
#[derive(Debug)]
pub struct CheckReport {
    pub ok: bool,
    pub diagnostics: Vec<CheckDiagnostic>,
    pub path: PathBuf,
}

#[derive(Debug)]
pub struct CheckDiagnostic {
    pub severity: CheckSeverity,
    pub message: String,
    /// Best-effort: 1-based line number. `0` if the parser didn't surface one.
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CheckSeverity {
    Error,
    Warning,
}

/// Parse `~/.config/fresh/init.ts` via oxc and report syntax errors.
///
/// This is the "parse mode" from the design (§5.1): always-on, low-latency,
/// catches the mistakes that would otherwise blow up at startup. The
/// deeper type-check (`tsc --noEmit`) and the scope-discipline lints
/// (`init/unconditional-preference`, `init/unconditional-plugin-load`)
/// are deliberately not implemented here — they're strict-mode concerns
/// that can grow on top of this foundation.
pub fn check(config_dir: &Path) -> CheckReport {
    use oxc_allocator::Allocator;
    use oxc_parser::Parser;
    use oxc_span::SourceType;

    let path = init_ts_path(config_dir);

    let source = match std::fs::read_to_string(&path) {
        Ok(s) => s,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            return CheckReport {
                ok: true,
                diagnostics: Vec::new(),
                path,
            };
        }
        Err(e) => {
            return CheckReport {
                ok: false,
                diagnostics: vec![CheckDiagnostic {
                    severity: CheckSeverity::Error,
                    message: format!("read failed: {e}"),
                    line: 0,
                    column: 0,
                }],
                path,
            };
        }
    };

    let allocator = Allocator::default();
    let source_type = SourceType::from_path(&path).unwrap_or_default();
    let parser_ret = Parser::new(&allocator, &source, source_type).parse();

    let mut diagnostics = Vec::new();
    for err in &parser_ret.errors {
        // oxc errors carry labels/spans but the formatting is embedded in
        // the miette-style Display impl. Pull the primary message + try to
        // recover line/column from the start of the first label.
        let (line, column) = err
            .labels
            .as_ref()
            .and_then(|v| v.first())
            .map(|l| line_col(&source, l.offset()))
            .unwrap_or((0, 0));
        diagnostics.push(CheckDiagnostic {
            severity: CheckSeverity::Error,
            message: err.message.to_string(),
            line,
            column,
        });
    }

    CheckReport {
        ok: parser_ret.errors.is_empty(),
        diagnostics,
        path,
    }
}

/// Convert a byte offset into a (line, column) pair, 1-based, for display.
fn line_col(source: &str, offset: usize) -> (u32, u32) {
    let clipped = source.get(..offset).unwrap_or(source);
    let line = 1 + clipped.bytes().filter(|&b| b == b'\n').count();
    let col = 1 + clipped
        .rsplit('\n')
        .next()
        .map(|s| s.chars().count())
        .unwrap_or(0);
    (line as u32, col as u32)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn init_ts_path_is_under_config_dir() {
        let p = init_ts_path(Path::new("/tmp/fresh"));
        assert_eq!(p, PathBuf::from("/tmp/fresh/init.ts"));
    }

    #[test]
    fn crash_fuse_trips_after_threshold_consecutive_failures() {
        let tmp = TempDir::new().unwrap();
        let dir = tmp.path();

        // Three attempts that never record success — each returns None
        // (proceed) and bumps the counter.
        for _ in 0..CRASH_FUSE_THRESHOLD {
            assert!(check_and_increment_fuse(dir).is_none());
        }

        // Fourth attempt should be short-circuited.
        let tripped = check_and_increment_fuse(dir);
        assert!(tripped.is_some());
        assert_eq!(tripped.unwrap(), CRASH_FUSE_THRESHOLD);
    }

    #[test]
    fn record_success_resets_the_fuse() {
        let tmp = TempDir::new().unwrap();
        let dir = tmp.path();

        for _ in 0..CRASH_FUSE_THRESHOLD {
            check_and_increment_fuse(dir);
        }
        record_success(dir);

        // After success, we should have room for another full cycle.
        assert!(check_and_increment_fuse(dir).is_none());
    }

    #[test]
    fn stale_failures_outside_window_are_ignored() {
        let tmp = TempDir::new().unwrap();
        let dir = tmp.path();

        // Manually plant an old, tripped counter.
        let state = CrashState {
            count: CRASH_FUSE_THRESHOLD + 5,
            last_increment_epoch: now_epoch_secs().saturating_sub(CRASH_FUSE_WINDOW_SECS + 1),
        };
        state.save(dir).unwrap();

        // Next attempt should treat it as fresh: proceed, counter back to 1.
        assert!(check_and_increment_fuse(dir).is_none());
    }

    #[test]
    fn decide_load_reports_not_found_when_missing() {
        let tmp = TempDir::new().unwrap();
        match decide_load(tmp.path(), true) {
            LoadDecision::Skip(InitOutcome::NotFound) => {}
            other => panic!("expected NotFound, got {other:?}"),
        }
    }

    #[test]
    fn decide_load_reports_disabled_when_flag_says_so() {
        let tmp = TempDir::new().unwrap();
        std::fs::write(init_ts_path(tmp.path()), "// hi").unwrap();
        match decide_load(tmp.path(), false) {
            LoadDecision::Skip(InitOutcome::Disabled) => {}
            other => panic!("expected Disabled, got {other:?}"),
        }
    }

    #[test]
    fn decide_load_returns_source_when_file_present_and_enabled() {
        let tmp = TempDir::new().unwrap();
        std::fs::write(init_ts_path(tmp.path()), "const x = 1;").unwrap();
        match decide_load(tmp.path(), true) {
            LoadDecision::Load { source } => assert_eq!(source, "const x = 1;"),
            other => panic!("expected Load, got {other:?}"),
        }
    }

    // Minor: LoadDecision/InitOutcome must be Debug to use in assertions.
    impl std::fmt::Debug for LoadDecision {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                LoadDecision::Skip(o) => write!(f, "Skip({o:?})"),
                LoadDecision::Load { source } => write!(f, "Load({} chars)", source.len()),
            }
        }
    }

    #[test]
    fn check_no_file_is_ok() {
        let tmp = TempDir::new().unwrap();
        let report = check(tmp.path());
        assert!(report.ok);
        assert!(report.diagnostics.is_empty());
    }

    #[test]
    fn check_clean_source_is_ok() {
        let tmp = TempDir::new().unwrap();
        std::fs::write(
            init_ts_path(tmp.path()),
            "const editor = getEditor();\neditor.setStatus('hi');\n",
        )
        .unwrap();
        let report = check(tmp.path());
        assert!(report.ok, "diagnostics: {:?}", report.diagnostics);
    }

    #[test]
    fn check_syntax_error_reports_a_diagnostic() {
        let tmp = TempDir::new().unwrap();
        // Missing closing paren — unambiguous parse error.
        std::fs::write(init_ts_path(tmp.path()), "function broken(\n").unwrap();
        let report = check(tmp.path());
        assert!(!report.ok);
        assert!(!report.diagnostics.is_empty());
        assert_eq!(report.diagnostics[0].severity, CheckSeverity::Error);
    }

    #[test]
    fn starter_template_references_both_dts_files() {
        assert!(
            STARTER_TEMPLATE.contains(r#"/// <reference path="./types/fresh.d.ts" />"#),
            "starter template must reference fresh.d.ts"
        );
        assert!(
            STARTER_TEMPLATE.contains(r#"/// <reference path="./types/plugins.d.ts" />"#),
            "starter template must reference plugins.d.ts so plugin APIs are typed"
        );
    }

    #[test]
    fn write_plugin_declarations_skips_empty_export_plugins() {
        let tmp = TempDir::new().unwrap();
        let decls = vec![
            ("noop".to_string(), "export {};\n".to_string()),
            ("blank".to_string(), "".to_string()),
            (
                "dashboard".to_string(),
                "export type DashboardApi = { foo(): void; };\n\
                 declare global { interface FreshPluginRegistry { dashboard: DashboardApi; } }\n\
                 export {};\n"
                    .to_string(),
            ),
        ];
        write_plugin_declarations(tmp.path(), &decls);
        let body = std::fs::read_to_string(tmp.path().join("types/plugins.d.ts")).unwrap();
        assert!(
            body.contains("// ── dashboard ─"),
            "dashboard section missing: {body}"
        );
        assert!(
            body.contains("DashboardApi"),
            "dashboard API missing: {body}"
        );
        assert!(
            !body.contains("// ── noop ─"),
            "empty-export plugin should not get a section header: {body}"
        );
        assert!(
            !body.contains("// ── blank ─"),
            "blank-emit plugin should not get a section header: {body}"
        );
    }
}
