// Theme screenshot gallery generator
//
// Walks through many editor views and UI states to capture screenshots
// showing every theme color key in action. Run for a specific theme via
// the FRESH_THEME env var (defaults to "dark").
//
// Usage:
//   FRESH_THEME=dracula cargo nextest run --package fresh-editor --test e2e_tests theme_screenshot_gallery -- --ignored --nocapture
//   # Then:
//   scripts/generate-theme-screenshots.sh dracula

use crate::common::blog_showcase::BlogShowcase;
use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, ThemeName};
use fresh::config_io::DirectoryContext;
use fresh::model::event::{Event, OverlayFace};
use fresh::view::overlay::OverlayNamespace;
use fresh::view::theme::ThemeFile;
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::{Color, Style};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn theme_name() -> String {
    std::env::var("FRESH_THEME").unwrap_or_else(|_| "dark".to_string())
}

fn snap(h: &mut EditorTestHarness, s: &mut BlogShowcase, key: Option<&str>, ms: u32) {
    h.render().unwrap();
    let c = h.screen_cursor_position();
    s.capture_frame(h.buffer(), c, key, None, ms).unwrap();
}

fn snap_mouse(
    h: &mut EditorTestHarness,
    s: &mut BlogShowcase,
    key: Option<&str>,
    mouse: (u16, u16),
    ms: u32,
) {
    h.render().unwrap();
    let c = h.screen_cursor_position();
    s.capture_frame(h.buffer(), c, key, Some(mouse), ms)
        .unwrap();
}

/// Create a rich demo project with multiple file types for maximum theme coverage.
fn create_theme_demo_project(project_dir: &std::path::Path) {
    fs::create_dir_all(project_dir.join("src")).unwrap();

    // Rust file with diverse syntax: keywords, strings, comments, functions, types,
    // constants, operators, brackets, delimiters
    fs::write(
        project_dir.join("src/main.rs"),
        r#"use std::collections::HashMap;
use std::io::{self, Read};

/// Configuration for the application.
/// Supports loading from JSON files.
const MAX_RETRIES: u32 = 3;
const VERSION: &str = "1.0.0";

#[derive(Debug, Clone)]
pub struct Config {
    pub name: String,
    pub port: u16,
    pub debug: bool,
    pub tags: Vec<String>,
}

impl Config {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            port: 8080,
            debug: false,
            tags: vec!["default".to_string()],
        }
    }

    /// Load configuration from a file path.
    pub fn load(path: &str) -> io::Result<Self> {
        let mut file = std::fs::File::open(path)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        // TODO: parse JSON properly
        Ok(Self::new("loaded"))
    }
}

fn main() {
    let config = Config::new("my-app");
    let items = vec!["alpha", "beta", "gamma", "delta"];
    let mut results: HashMap<&str, usize> = HashMap::new();

    for (index, item) in items.iter().enumerate() {
        if config.debug {
            println!("[DEBUG] Processing: {}", item);
        }
        results.insert(item, index * 2);
    }

    let total: usize = results.values().sum();
    println!("Processed {} items, total = {}", results.len(), total);

    match total {
        0 => println!("No results"),
        1..=10 => println!("Few results"),
        _ => println!("Many results: {}", total),
    }
}

// Helper function with error handling
fn read_input() -> Result<String, io::Error> {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer)?;
    Ok(buffer.trim().to_string())
}
"#,
    )
    .unwrap();

    fs::write(
        project_dir.join("src/utils.rs"),
        r#"/// Format a duration in human-readable form.
pub fn format_duration(secs: u64) -> String {
    let hours = secs / 3600;
    let mins = (secs % 3600) / 60;
    let secs = secs % 60;
    format!("{}h {}m {}s", hours, mins, secs)
}

pub fn truncate(s: &str, max_len: usize) -> &str {
    if s.len() > max_len { &s[..max_len] } else { s }
}
"#,
    )
    .unwrap();

    fs::write(
        project_dir.join("README.md"),
        "# My Project\n\nA demo project for theme screenshots.\n\n## Features\n\n- Fast startup\n- Rich syntax highlighting\n- Multiple themes\n",
    )
    .unwrap();

    fs::write(
        project_dir.join("Cargo.toml"),
        "[package]\nname = \"demo\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n[dependencies]\nserde = { version = \"1\", features = [\"derive\"] }\n",
    )
    .unwrap();
}

// ---------------------------------------------------------------------------
// Screenshot scenes — each captures a different UI state
// ---------------------------------------------------------------------------

/// Scene 1: Syntax highlighting — just code, no overlays.
/// Covers: editor.bg/fg, syntax.*, line_number_fg/bg, current_line_bg, cursor
fn scene_syntax(h: &mut EditorTestHarness, s: &mut BlogShowcase) {
    // Move cursor to an interesting line (inside main fn)
    for _ in 0..7 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    h.render().unwrap();
    snap(h, s, None, 300);
}

/// Scene 2: Selection — select a block of text.
/// Covers: editor.selection_bg
fn scene_selection(h: &mut EditorTestHarness, s: &mut BlogShowcase) {
    // Select word then extend
    h.send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    snap(h, s, Some("Ctrl+W"), 200);

    // Select a few lines
    for _ in 0..3 {
        h.send_key(KeyCode::Down, KeyModifiers::SHIFT).unwrap();
    }
    snap(h, s, Some("Shift+↓"), 300);

    // Deselect
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
}

/// Scene 3: Multi-cursor editing.
/// Covers: cursor, inactive_cursor
fn scene_multi_cursor(h: &mut EditorTestHarness, s: &mut BlogShowcase) {
    // Go to top, find "item"
    h.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    for _ in 0..9 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    // Select word
    h.send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    // Add next occurrences
    for _ in 0..3 {
        h.send_key(KeyCode::Char('d'), KeyModifiers::CONTROL)
            .unwrap();
    }
    snap(h, s, Some("Ctrl+D"), 300);

    // Escape multi-cursor
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
}

/// Scene 4: Search with highlights.
/// Covers: search.match_bg, search.match_fg, prompt_fg/bg, prompt_selection_fg/bg
fn scene_search(h: &mut EditorTestHarness, s: &mut BlogShowcase) {
    h.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    h.render().unwrap();

    // Open search (Ctrl+F)
    h.send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();

    // Type search query
    for ch in "config".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
    }
    h.render().unwrap();
    snap(h, s, Some("Search"), 300);

    // Close search
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
}

/// Scene 5: Command palette open.
/// Covers: popup_*, suggestion_bg, suggestion_selected_bg
fn scene_command_palette(h: &mut EditorTestHarness, s: &mut BlogShowcase) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(h, s, Some("Ctrl+P"), 200);

    // Type a partial filter
    for ch in "split".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
    }
    h.render().unwrap();
    snap(h, s, Some("Filter"), 300);

    // Close
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
}

/// Scene 6: File explorer open.
/// Covers: ui file explorer colors, split_separator_fg
fn scene_file_explorer(h: &mut EditorTestHarness, s: &mut BlogShowcase) {
    // Toggle file explorer
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();

    // Wait a beat for file tree to load
    let _ = h.wait_until(|h| h.screen_to_string().contains("src"));
    h.render().unwrap();

    // Navigate down in explorer
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();

    snap(h, s, Some("File Explorer"), 300);

    // Focus back on editor and close explorer
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
}

/// Scene 7: Split view with two files.
/// Covers: tab_active/inactive, split_separator_fg/hover_fg, inactive_cursor
fn scene_split_view(h: &mut EditorTestHarness, s: &mut BlogShowcase) {
    let pd = h.project_dir().unwrap();

    // Open split
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    for ch in "Split Right".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
    }
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();

    // Open a different file in the right split
    h.open_file(&pd.join("src/utils.rs")).unwrap();
    h.render().unwrap();

    snap(h, s, Some("Split View"), 300);

    // Close the split — go back to single pane
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    for ch in "Close Split".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
    }
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
}

/// Scene 8: Diagnostics / overlays.
/// Covers: diagnostic.error/warning/info/hint fg/bg, status_warning/error_indicator
fn scene_diagnostics(h: &mut EditorTestHarness, s: &mut BlogShowcase) {
    // Add simulated diagnostic overlays
    h.apply_event(Event::AddOverlay {
        namespace: Some(OverlayNamespace::from_string("lsp-diagnostic".to_string())),
        range: 40..50,
        face: OverlayFace::Background {
            color: (80, 20, 20),
        },
        priority: 100,
        message: Some("error: unused variable `x`".to_string()),
        extend_to_line_end: false,
        url: None,
    })
    .unwrap();

    h.apply_event(Event::AddOverlay {
        namespace: Some(OverlayNamespace::from_string("lsp-diagnostic".to_string())),
        range: 120..135,
        face: OverlayFace::Background {
            color: (60, 50, 10),
        },
        priority: 90,
        message: Some("warning: unused import".to_string()),
        extend_to_line_end: false,
        url: None,
    })
    .unwrap();

    h.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    h.render().unwrap();
    snap(h, s, Some("Diagnostics"), 300);

    // Clear overlays
    h.apply_event(Event::ClearOverlays).unwrap();
    h.render().unwrap();
}

/// Scene 9: Menu bar open (dropdown).
/// Covers: menu_bg/fg, menu_active/hover/highlight, menu_dropdown, menu_border,
///         menu_separator, menu_disabled
fn scene_menu_bar(h: &mut EditorTestHarness, s: &mut BlogShowcase) {
    // Open menu bar with F10
    h.send_key(KeyCode::F(10), KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(h, s, Some("F10"), 200);

    // Open first menu dropdown
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();

    // Navigate down to show hover states
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(h, s, Some("Menu"), 300);

    // Close menu
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
}

/// Scene 10: Help overlay (F1).
/// Covers: help_bg/fg, help_key_fg, help_separator_fg, help_indicator_fg/bg
fn scene_help(h: &mut EditorTestHarness, s: &mut BlogShowcase) {
    h.send_key(KeyCode::F(1), KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(h, s, Some("F1 Help"), 300);

    // Close help
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
}

/// Scene 11: Settings view.
/// Covers: settings_selected_bg/fg, popup borders, scrollbar
fn scene_settings(h: &mut EditorTestHarness, s: &mut BlogShowcase) {
    h.open_settings().unwrap();
    h.render().unwrap();

    // Navigate down in settings
    for _ in 0..3 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    h.render().unwrap();
    snap(h, s, Some("Settings"), 300);

    // Close settings
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
}

/// Scene 12: Diff highlights (simulate git diff coloring).
/// Covers: diff_add_bg, diff_remove_bg, diff_modify_bg
fn scene_diff_highlights(h: &mut EditorTestHarness, s: &mut BlogShowcase) {
    // Simulate diff overlays
    h.apply_event(Event::AddOverlay {
        namespace: Some(OverlayNamespace::from_string("git-diff".to_string())),
        range: 200..230,
        face: OverlayFace::Background {
            color: (20, 60, 20),
        },
        priority: 50,
        message: Some("added line".to_string()),
        extend_to_line_end: true,
        url: None,
    })
    .unwrap();

    h.apply_event(Event::AddOverlay {
        namespace: Some(OverlayNamespace::from_string("git-diff".to_string())),
        range: 250..280,
        face: OverlayFace::Background {
            color: (60, 20, 20),
        },
        priority: 50,
        message: Some("removed line".to_string()),
        extend_to_line_end: true,
        url: None,
    })
    .unwrap();

    h.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    for _ in 0..6 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    h.render().unwrap();
    snap(h, s, Some("Diff"), 300);

    h.apply_event(Event::ClearOverlays).unwrap();
    h.render().unwrap();
}

/// Scene 13: Scrollbar visible (scroll down to make it appear).
/// Covers: scrollbar_track_fg/hover_fg, scrollbar_thumb_fg/hover_fg
fn scene_scrollbar(h: &mut EditorTestHarness, s: &mut BlogShowcase) {
    // Scroll to middle of file
    for _ in 0..15 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    h.render().unwrap();

    // Mouse hover on scrollbar area (rightmost column)
    let width = h.buffer().area.width;
    snap_mouse(h, s, Some("Scrollbar"), (width - 1, 10), 300);

    // Go back to top
    h.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    h.render().unwrap();
}

/// Scene 14: Whitespace indicators.
/// Covers: whitespace_indicator_fg
fn scene_whitespace(h: &mut EditorTestHarness, s: &mut BlogShowcase) {
    // Toggle whitespace via command palette
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    for ch in "Toggle Whitespace".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
    }
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();

    snap(h, s, Some("Whitespace"), 300);

    // Toggle off
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    for ch in "Toggle Whitespace".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
    }
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
}

/// Run the full scene suite against a harness/showcase pair.
///
/// Every scene captures one or more frames showing a distinct UI surface.
/// Shared by the single-theme gallery and the before/after diff gallery so
/// both render an identical sequence (frame indices line up one-to-one).
fn run_all_scenes(h: &mut EditorTestHarness, s: &mut BlogShowcase) {
    scene_syntax(h, s);
    scene_selection(h, s);
    scene_multi_cursor(h, s);
    scene_search(h, s);
    scene_command_palette(h, s);
    scene_file_explorer(h, s);
    scene_split_view(h, s);
    scene_diagnostics(h, s);
    scene_menu_bar(h, s);
    scene_help(h, s);
    scene_settings(h, s);
    scene_diff_highlights(h, s);
    scene_scrollbar(h, s);
    scene_whitespace(h, s);
}

// ---------------------------------------------------------------------------
// Main gallery test
// ---------------------------------------------------------------------------

/// Generate a complete theme screenshot gallery.
///
/// Set FRESH_THEME env var to choose the theme (defaults to "dark").
/// Each scene produces one or more frames showing different UI states.
#[test]
#[ignore]
fn theme_screenshot_gallery() {
    let theme = theme_name();

    let mut config = Config::default();
    config.theme = fresh::config::ThemeName(theme.clone());

    let mut h = EditorTestHarness::with_temp_project_and_config(120, 35, config).unwrap();
    let pd = h.project_dir().unwrap();
    create_theme_demo_project(&pd);
    h.open_file(&pd.join("src/main.rs")).unwrap();

    let gallery_name = format!("theme-gallery/{}", theme);
    let mut s = BlogShowcase::new(
        &gallery_name,
        &format!("Theme Gallery: {}", theme),
        &format!(
            "Screenshots of the {} theme across all editor views.",
            theme
        ),
    );

    run_all_scenes(&mut h, &mut s);

    s.finalize().unwrap();

    println!("Theme gallery generated for '{}'", theme);
    println!("Frames: docs/blog/{}/frames/", gallery_name);
}

// ===========================================================================
// Before/after theme diff gallery
//
// Renders the scene suite twice for every theme a PR changes — once with the
// theme colors from the base ref ("before") and once with the working-tree
// colors ("after") — then writes an HTML gallery pairing them side by side.
// See docs/theme-screenshot-diff.md for objectives & criteria.
//
// Usage:
//   FRESH_THEME_BASE_REF=origin/master \
//     cargo nextest run --package fresh-editor --test e2e_tests \
//     -E 'test(theme_diff_gallery)' --run-ignored ignored-only --no-capture
// ===========================================================================

/// Workspace root (two levels up from `crates/fresh-editor`).
fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .expect("workspace root")
        .to_path_buf()
}

/// Directory holding the built-in theme JSON files.
fn themes_src_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("themes")
}

/// Git-tracked path of a theme file, used in `git show <ref>:<path>`.
fn theme_relpath(file: &str) -> String {
    format!("crates/fresh-editor/themes/{}", file)
}

/// Same normalization the theme registry applies to names, so our activation
/// guard compares apples to apples.
fn normalize_name(name: &str) -> String {
    name.to_lowercase().replace(['_', ' '], "-")
}

/// Run a git command in `root`, returning trimmed stdout on success.
fn git_stdout(root: &Path, args: &[&str]) -> Option<String> {
    let out = Command::new("git")
        .current_dir(root)
        .args(args)
        .output()
        .ok()?;
    if out.status.success() {
        Some(String::from_utf8_lossy(&out.stdout).trim().to_string())
    } else {
        None
    }
}

/// Read file content at a git ref (`git show <ref>:<path>`). Returns `None`
/// when the path didn't exist at that ref (a newly added theme) or git fails.
fn git_show(root: &Path, reference: &str, relpath: &str) -> Option<String> {
    let spec = format!("{}:{}", reference, relpath);
    let out = Command::new("git")
        .current_dir(root)
        .args(["show", &spec])
        .output()
        .ok()?;
    if out.status.success() {
        Some(String::from_utf8_lossy(&out.stdout).to_string())
    } else {
        None
    }
}

/// Resolve the base ref to diff against. First match wins:
/// `FRESH_THEME_BASE_REF` → merge-base with origin/master → origin/master →
/// master. Returns `None` if nothing resolves (e.g. a shallow clone).
fn resolve_base_ref(root: &Path) -> Option<String> {
    if let Ok(r) = std::env::var("FRESH_THEME_BASE_REF") {
        let r = r.trim();
        if !r.is_empty() {
            return Some(r.to_string());
        }
    }
    if let Some(mb) = git_stdout(root, &["merge-base", "HEAD", "origin/master"]) {
        if !mb.is_empty() {
            return Some(mb);
        }
    }
    for candidate in ["origin/master", "master"] {
        if git_stdout(root, &["rev-parse", "--verify", candidate]).is_some() {
            return Some(candidate.to_string());
        }
    }
    None
}

/// Rewrite a theme JSON's `name` field so the version loads under a unique
/// key (no clash with the identically-named built-in) and can be selected via
/// `config.theme`.
fn rewrite_theme_name(json: &str, token: &str) -> String {
    match serde_json::from_str::<serde_json::Value>(json) {
        Ok(mut v) => {
            if let Some(obj) = v.as_object_mut() {
                obj.insert(
                    "name".to_string(),
                    serde_json::Value::String(token.to_string()),
                );
            }
            serde_json::to_string_pretty(&v).unwrap_or_else(|_| json.to_string())
        }
        Err(_) => json.to_string(),
    }
}

// ---------------------------------------------------------------------------
// Theme-key swatch board
//
// The 14 realistic scenes show many theme keys in context, but not all of
// them (terminal, git file-status, semantic/LSP indicators, rulers, hover/drag
// states, …). To *guarantee* every defined key is represented in the
// before/after comparison, we also render a deterministic swatch board built
// generically from the live theme — so any key (including ones added later)
// shows up automatically.
// ---------------------------------------------------------------------------

/// Parse a serialized `ColorDef` value (`[r, g, b]` or a named color string)
/// back into a ratatui `Color`. The SVG renderer resolves named colors, so we
/// keep them named rather than flattening to RGB here.
fn color_from_value(v: &serde_json::Value) -> Option<Color> {
    if let Some(arr) = v.as_array() {
        if arr.len() == 3 {
            let comp = |i: usize| arr[i].as_u64().map(|n| n as u8);
            if let (Some(r), Some(g), Some(b)) = (comp(0), comp(1), comp(2)) {
                return Some(Color::Rgb(r, g, b));
            }
        }
        return None;
    }
    let s = v.as_str()?;
    Some(match s {
        "White" => Color::White,
        "Black" => Color::Black,
        "Red" => Color::Red,
        "Green" => Color::Green,
        "Blue" => Color::Blue,
        "Yellow" => Color::Yellow,
        "Magenta" => Color::Magenta,
        "Cyan" => Color::Cyan,
        "Gray" | "Grey" => Color::Gray,
        "DarkGray" | "DarkGrey" => Color::DarkGray,
        "LightRed" => Color::LightRed,
        "LightGreen" => Color::LightGreen,
        "LightBlue" => Color::LightBlue,
        "LightYellow" => Color::LightYellow,
        "LightMagenta" => Color::LightMagenta,
        "LightCyan" => Color::LightCyan,
        "Default" | "Reset" => Color::Reset,
        _ => return None,
    })
}

/// Flatten a serialized `ThemeFile` into `(section.key, color)` pairs, in a
/// stable section/alphabetical order so before/after boards line up.
fn theme_color_entries(theme_json: &serde_json::Value) -> Vec<(String, Color)> {
    let mut out = Vec::new();
    let Some(obj) = theme_json.as_object() else {
        return out;
    };
    for sec in ["editor", "ui", "search", "diagnostic", "syntax"] {
        let Some(inner) = obj.get(sec).and_then(|v| v.as_object()) else {
            continue;
        };
        let mut keys: Vec<&String> = inner.keys().collect();
        keys.sort();
        for k in keys {
            if let Some(c) = color_from_value(&inner[k]) {
                out.push((format!("{sec}.{k}"), c));
            }
        }
    }
    out
}

/// Write `text` into `buf` starting at `(x, y)`, clipped to `width`.
fn put_str(buf: &mut Buffer, x: u16, y: u16, text: &str, fg: Color, bg: Color, width: u16) {
    for (i, ch) in text.chars().enumerate() {
        let cx = x + i as u16;
        if cx >= width {
            break;
        }
        buf[(cx, y)]
            .set_char(ch)
            .set_style(Style::default().fg(fg).bg(bg));
    }
}

/// Build a swatch board: one labelled color cell per theme key, laid out in
/// columns. Rendered into a standalone buffer so it can be captured as a frame
/// without involving the editor UI.
fn build_swatch_board(
    entries: &[(String, Color)],
    base_bg: Color,
    base_fg: Color,
    width: u16,
    height: u16,
) -> Buffer {
    let mut buf = Buffer::empty(Rect::new(0, 0, width, height));
    for y in 0..height {
        for x in 0..width {
            buf[(x, y)]
                .set_char(' ')
                .set_style(Style::default().bg(base_bg).fg(base_fg));
        }
    }
    put_str(
        &mut buf,
        1,
        0,
        "Theme color keys — every defined key, resolved",
        base_fg,
        base_bg,
        width,
    );

    let ncols: u16 = 4;
    let colw = width / ncols;
    let top: u16 = 1;
    let rows = height.saturating_sub(top);
    let n = entries.len() as u16;
    let per_col = n.div_ceil(ncols).max(1);

    for (i, (label, color)) in entries.iter().enumerate() {
        let i = i as u16;
        let col = i / per_col;
        let row = i % per_col;
        if col >= ncols || row >= rows {
            continue;
        }
        let x0 = col * colw;
        let y = top + row;
        // 4-cell swatch
        for sx in 0..4u16 {
            if x0 + sx < width {
                buf[(x0 + sx, y)]
                    .set_char(' ')
                    .set_style(Style::default().bg(*color));
            }
        }
        let max_label = colw.saturating_sub(5) as usize;
        let label: String = label.chars().take(max_label).collect();
        put_str(&mut buf, x0 + 5, y, &label, base_fg, base_bg, width);
    }
    buf
}

/// Render the scene suite with `theme_json` active, writing frames under
/// `docs/blog/<gallery_name>/`. Returns `true` if the theme actually loaded
/// and frames were produced; `false` (without writing frames) if the version
/// could not be selected — which happens when a baseline JSON is incompatible
/// with the current theme schema. The caller decides whether that's fatal.
fn try_render_version(theme_token: &str, theme_json: &str, gallery_name: &str) -> bool {
    let cfg_temp = match tempfile::TempDir::new() {
        Ok(t) => t,
        Err(e) => {
            eprintln!("theme-diff: could not create temp dir: {e}");
            return false;
        }
    };
    let ctx = DirectoryContext::for_testing(cfg_temp.path());
    let themes_dir = ctx.themes_dir();
    if let Err(e) = fs::create_dir_all(&themes_dir) {
        eprintln!("theme-diff: could not create themes dir: {e}");
        return false;
    }
    let rewritten = rewrite_theme_name(theme_json, theme_token);
    if let Err(e) = fs::write(themes_dir.join(format!("{theme_token}.json")), &rewritten) {
        eprintln!("theme-diff: could not write theme file: {e}");
        return false;
    }

    let config = Config {
        theme: ThemeName(theme_token.to_string()),
        ..Default::default()
    };

    let opts = HarnessOptions::new()
        .with_project_root()
        .with_config(config)
        .with_shared_dir_context(ctx)
        .with_full_grammar_registry();
    let mut h = match EditorTestHarness::create(120, 35, opts) {
        Ok(h) => h,
        Err(e) => {
            eprintln!("theme-diff: harness creation failed for '{theme_token}': {e}");
            return false;
        }
    };

    // Guard: if the version didn't load, the frames would silently show the
    // default theme. Bail without finalizing so no misleading output is left.
    let active = h.editor().theme().name.clone();
    if normalize_name(&active) != normalize_name(theme_token) {
        eprintln!(
            "theme-diff: version '{theme_token}' did not activate (active theme is '{active}'); \
             skipping this side"
        );
        return false;
    }

    let pd = h.project_dir().unwrap();
    create_theme_demo_project(&pd);
    h.open_file(&pd.join("src/main.rs")).unwrap();

    let mut s = BlogShowcase::new(
        gallery_name,
        gallery_name,
        "Before/after theme diff frames.",
    );

    // Frame 0: deterministic swatch board covering every defined key, so the
    // comparison represents keys no realistic scene happens to show.
    {
        let (bg, fg) = {
            let t = h.editor().theme();
            (t.editor_bg, t.editor_fg)
        };
        let theme_file: ThemeFile = (*h.editor().theme()).clone().into();
        if let Ok(val) = serde_json::to_value(&theme_file) {
            let entries = theme_color_entries(&val);
            let board = build_swatch_board(&entries, bg, fg, 120, 35);
            s.capture_frame(&board, (0, 0), Some("Theme keys"), None, 400)
                .expect("capture swatch board");
        }
    }

    run_all_scenes(&mut h, &mut s);
    s.finalize().expect("finalize diff gallery");
    true
}

/// A leaf color value, flattened to "section.key" with a printable value.
fn flatten_colors(value: &serde_json::Value, out: &mut std::collections::BTreeMap<String, String>) {
    if let Some(obj) = value.as_object() {
        for (section, body) in obj {
            if section == "name" {
                continue;
            }
            if let Some(inner) = body.as_object() {
                for (key, val) in inner {
                    out.insert(format!("{section}.{key}"), val.to_string());
                }
            }
        }
    }
}

/// Changed color keys between two theme JSONs, as `(key, before, after)`.
/// Added/removed keys use "—" for the missing side.
fn changed_color_keys(before: &str, after: &str) -> Vec<(String, String, String)> {
    let mut b = std::collections::BTreeMap::new();
    let mut a = std::collections::BTreeMap::new();
    if let Ok(v) = serde_json::from_str::<serde_json::Value>(before) {
        flatten_colors(&v, &mut b);
    }
    if let Ok(v) = serde_json::from_str::<serde_json::Value>(after) {
        flatten_colors(&v, &mut a);
    }
    let mut keys: std::collections::BTreeSet<String> = b.keys().cloned().collect();
    keys.extend(a.keys().cloned());
    let mut changed = Vec::new();
    for k in keys {
        let bv = b.get(&k);
        let av = a.get(&k);
        if bv != av {
            changed.push((
                k,
                bv.cloned().unwrap_or_else(|| "—".to_string()),
                av.cloned().unwrap_or_else(|| "—".to_string()),
            ));
        }
    }
    changed
}

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
}

/// Read the frame filenames captured for one side of a theme diff.
fn read_frame_files(theme_dir: &Path, side: &str) -> Vec<String> {
    let json_path = theme_dir.join(side).join("showcase.json");
    let Ok(content) = fs::read_to_string(&json_path) else {
        return Vec::new();
    };
    let Ok(meta) = serde_json::from_str::<crate::common::blog_showcase::ShowcaseMetadata>(&content)
    else {
        return Vec::new();
    };
    meta.frames.into_iter().map(|f| f.filename).collect()
}

/// Render a CSS color swatch from a `[r, g, b]` JSON value string, or empty.
fn swatch(value: &str) -> String {
    let trimmed = value.trim_start_matches('[').trim_end_matches(']');
    let parts: Vec<_> = trimmed.split(',').map(|p| p.trim().parse::<u8>()).collect();
    if parts.len() == 3 && parts.iter().all(|p| p.is_ok()) {
        let r = *parts[0].as_ref().unwrap();
        let g = *parts[1].as_ref().unwrap();
        let b = *parts[2].as_ref().unwrap();
        format!(
            "<span style=\"display:inline-block;width:12px;height:12px;border:1px solid #888;\
             vertical-align:middle;background:rgb({r},{g},{b})\"></span> "
        )
    } else {
        String::new()
    }
}

/// Write the per-theme `index.html`: a changed-keys table followed by every
/// captured frame as a before | after pair.
fn write_theme_index(
    theme_dir: &Path,
    theme_name: &str,
    changed: &[(String, String, String)],
    has_before: bool,
) {
    let before_frames = read_frame_files(theme_dir, "before");
    let after_frames = read_frame_files(theme_dir, "after");
    let n = before_frames.len().max(after_frames.len());

    let mut html = String::new();
    html.push_str(&format!(
        "<!doctype html><meta charset=\"utf-8\"><title>theme diff: {name}</title>\n\
         <style>body{{font-family:sans-serif;background:#1e1e2e;color:#cdd6f4;margin:24px}}\
         h1,h2{{font-weight:600}} a{{color:#89b4fa}}\
         table{{border-collapse:collapse;margin:12px 0}} td,th{{border:1px solid #45475a;padding:4px 8px;font-size:13px}}\
         .pair{{display:flex;gap:16px;align-items:flex-start;margin:18px 0;flex-wrap:wrap}}\
         .pair figure{{margin:0}} .pair img{{max-width:560px;width:100%;border:1px solid #45475a}}\
         figcaption{{font-size:12px;color:#a6adc8;margin-bottom:4px}} code{{color:#f9e2af}}</style>\n\
         <p><a href=\"../index.html\">← all changed themes</a></p>\n\
         <h1>Theme diff: {name}</h1>\n",
        name = html_escape(theme_name)
    ));

    if !has_before {
        html.push_str(
            "<p><strong>New theme — no baseline.</strong> Showing the proposed theme only.</p>\n",
        );
    }

    if !changed.is_empty() {
        html.push_str(&format!(
            "<h2>Changed color keys ({})</h2>\n<table><tr><th>key</th><th>before</th><th>after</th></tr>\n",
            changed.len()
        ));
        for (k, b, a) in changed {
            html.push_str(&format!(
                "<tr><td><code>{}</code></td><td>{}<code>{}</code></td><td>{}<code>{}</code></td></tr>\n",
                html_escape(k),
                swatch(b),
                html_escape(b),
                swatch(a),
                html_escape(a),
            ));
        }
        html.push_str("</table>\n");
    }

    html.push_str("<h2>Screenshots</h2>\n");
    for i in 0..n {
        html.push_str("<div class=\"pair\">\n");
        if has_before {
            match before_frames.get(i) {
                Some(f) => html.push_str(&format!(
                    "<figure><figcaption>before</figcaption><img loading=\"lazy\" src=\"before/frames/{}\"></figure>\n",
                    html_escape(f)
                )),
                None => html.push_str(
                    "<figure><figcaption>before</figcaption><em>(no frame)</em></figure>\n",
                ),
            }
        }
        match after_frames.get(i) {
            Some(f) => html.push_str(&format!(
                "<figure><figcaption>after</figcaption><img loading=\"lazy\" src=\"after/frames/{}\"></figure>\n",
                html_escape(f)
            )),
            None => html
                .push_str("<figure><figcaption>after</figcaption><em>(no frame)</em></figure>\n"),
        }
        html.push_str("</div>\n");
    }

    let _ = fs::write(theme_dir.join("index.html"), html);
}

/// Generate before/after theme diff galleries for every theme a PR changed.
///
/// `#[ignore]` so it never runs in the normal suite; the dedicated
/// theme-screenshots workflow runs it with `--run-ignored ignored-only`.
#[test]
#[ignore]
fn theme_diff_gallery() {
    let root = workspace_root();
    let out_root = root.join("docs/blog/theme-diff");
    let _ = fs::remove_dir_all(&out_root); // start clean

    let base_ref = resolve_base_ref(&root);
    match &base_ref {
        Some(r) => println!("theme-diff: base ref = {r}"),
        None => println!(
            "theme-diff: no base ref resolved (set FRESH_THEME_BASE_REF); rendering 'after' only"
        ),
    }

    let render_all = std::env::var("FRESH_THEME_DIFF_ALL").is_ok();
    let only: Option<Vec<String>> = std::env::var("FRESH_THEME_DIFF_THEMES")
        .ok()
        .filter(|s| !s.trim().is_empty())
        .map(|s| s.split(',').map(|t| t.trim().to_string()).collect());

    // Collect candidate theme files (sorted for deterministic output).
    let mut files: Vec<String> = fs::read_dir(themes_src_dir())
        .expect("themes dir")
        .flatten()
        .filter_map(|e| {
            let p = e.path();
            if p.extension().is_some_and(|x| x == "json") {
                p.file_name().map(|n| n.to_string_lossy().to_string())
            } else {
                None
            }
        })
        .collect();
    files.sort();

    let mut summary: Vec<(String, usize, bool)> = Vec::new(); // (name, changed_keys, has_before)

    for file in &files {
        let name = file.trim_end_matches(".json").to_string();
        if let Some(only) = &only {
            if !only.iter().any(|t| t == &name) {
                continue;
            }
        }

        let after = match fs::read_to_string(themes_src_dir().join(file)) {
            Ok(c) => c,
            Err(_) => continue,
        };
        let before = base_ref
            .as_deref()
            .and_then(|r| git_show(&root, r, &theme_relpath(file)));

        let changed = match &before {
            Some(b) => b.as_str() != after.as_str(),
            None => true, // newly added theme (no baseline) counts as changed
        };
        if !changed && !render_all && only.is_none() {
            continue;
        }

        println!(
            "theme-diff: rendering '{name}' ({})",
            if before.is_some() {
                "before + after"
            } else {
                "after only (new theme)"
            }
        );

        let theme_dir = out_root.join(&name);

        let mut has_before = false;
        if let Some(before_json) = &before {
            has_before = try_render_version(
                &format!("zdiff-before-{name}"),
                before_json,
                &format!("theme-diff/{name}/before"),
            );
        }

        let after_ok = try_render_version(
            &format!("zdiff-after-{name}"),
            &after,
            &format!("theme-diff/{name}/after"),
        );
        assert!(
            after_ok,
            "current theme '{name}' failed to load — its JSON is invalid under the active schema"
        );

        let changed_keys = match &before {
            Some(b) => changed_color_keys(b, &after),
            None => Vec::new(),
        };
        write_theme_index(&theme_dir, &name, &changed_keys, has_before);
        summary.push((name, changed_keys.len(), has_before));
    }

    // Top-level index.
    let mut index = String::from(
        "<!doctype html><meta charset=\"utf-8\"><title>Theme diffs</title>\n\
         <style>body{font-family:sans-serif;background:#1e1e2e;color:#cdd6f4;margin:24px}\
         a{color:#89b4fa} li{margin:6px 0}</style>\n<h1>Theme diffs (before → after)</h1>\n",
    );
    if summary.is_empty() {
        index.push_str("<p>No theme changes detected against the base ref.</p>\n");
        println!("theme-diff: no changed themes");
    } else {
        index.push_str("<ul>\n");
        for (name, keys, has_before) in &summary {
            let note = if *has_before {
                format!("{keys} color key(s) changed")
            } else {
                "new theme".to_string()
            };
            index.push_str(&format!(
                "<li><a href=\"{n}/index.html\">{n}</a> — {note}</li>\n",
                n = html_escape(name)
            ));
        }
        index.push_str("</ul>\n");
    }
    fs::create_dir_all(&out_root).ok();
    let _ = fs::write(out_root.join("index.html"), index);

    println!(
        "theme-diff: done — {} theme(s); open {}",
        summary.len(),
        out_root.join("index.html").display()
    );
}
