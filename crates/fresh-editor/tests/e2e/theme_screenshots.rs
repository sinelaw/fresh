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
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::model::event::{Event, OverlayFace};
use fresh::view::overlay::OverlayNamespace;
use std::fs;

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

    // Run all scenes in sequence
    scene_syntax(&mut h, &mut s);
    scene_selection(&mut h, &mut s);
    scene_multi_cursor(&mut h, &mut s);
    scene_search(&mut h, &mut s);
    scene_command_palette(&mut h, &mut s);
    scene_file_explorer(&mut h, &mut s);
    scene_split_view(&mut h, &mut s);
    scene_diagnostics(&mut h, &mut s);
    scene_menu_bar(&mut h, &mut s);
    scene_help(&mut h, &mut s);
    scene_settings(&mut h, &mut s);
    scene_diff_highlights(&mut h, &mut s);
    scene_scrollbar(&mut h, &mut s);
    scene_whitespace(&mut h, &mut s);

    s.finalize().unwrap();

    println!("Theme gallery generated for '{}'", theme);
    println!("Frames: docs/blog/{}/frames/", gallery_name);
}
