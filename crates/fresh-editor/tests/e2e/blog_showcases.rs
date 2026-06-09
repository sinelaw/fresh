// Blog showcase tests - individual feature demos for blog posts
//
// Each test generates a separate animated GIF for one feature.
// Two blog posts: "editing" (text editing features) and "productivity" (broader features).
//
// Usage:
//   cargo test --package fresh-editor --test e2e_tests blog_showcase_ -- --ignored --nocapture
//   # Then for each generated showcase:
//   scripts/frames-to-gif.sh docs/blog/editing/multi-cursor
//   scripts/frames-to-gif.sh docs/blog/editing/search-replace
//   # ... etc

use crate::common::blog_showcase::BlogShowcase;
use crate::common::fixtures::TestFixture;
use crate::common::git_test_helper::{git_command, GitTestRepo};
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use lsp_types::FoldingRange;
use std::fs;

// =========================================================================
// Helpers
// =========================================================================

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

fn hold(h: &mut EditorTestHarness, s: &mut BlogShowcase, count: usize, ms: u32) {
    h.render().unwrap();
    let c = h.screen_cursor_position();
    s.hold_frames(h.buffer(), c, None, None, count, ms).unwrap();
}

fn hold_key(h: &mut EditorTestHarness, s: &mut BlogShowcase, key: &str, count: usize, ms: u32) {
    h.render().unwrap();
    let c = h.screen_cursor_position();
    s.hold_frames(h.buffer(), c, Some(key), None, count, ms)
        .unwrap();
}

/// Create a standard Rust project for demos
fn create_demo_project(project_dir: &std::path::Path) {
    fs::create_dir_all(project_dir.join("src")).unwrap();
    fs::write(
        project_dir.join("src/main.rs"),
        r#"use std::collections::HashMap;

fn main() {
    let config = load_config("settings.json");
    let items = vec!["alpha", "beta", "gamma", "delta"];

    for item in &items {
        process_item(item, &config);
    }

    let results: HashMap<&str, i32> = items
        .iter()
        .enumerate()
        .map(|(i, item)| (*item, i as i32))
        .collect();

    println!("Processed {} items", results.len());
}

fn load_config(path: &str) -> HashMap<String, String> {
    println!("Loading config from {}", path);
    HashMap::new()
}

fn process_item(item: &str, _config: &HashMap<String, String>) {
    let value = item.to_uppercase();
    let length = value.len();
    println!("[{}] {} (len: {})", item, value, length);
}
"#,
    )
    .unwrap();

    fs::write(
        project_dir.join("src/utils.rs"),
        r#"pub fn format_duration(secs: u64) -> String {
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
        "# My Project\n\nA demo project.\n",
    )
    .unwrap();
    fs::write(
        project_dir.join("Cargo.toml"),
        "[package]\nname = \"demo\"\nversion = \"0.1.0\"\nedition = \"2021\"\n",
    )
    .unwrap();
}

// =========================================================================
// Blog Post 1: Editing Features
// =========================================================================

/// Multi-cursor editing: Ctrl+W to select word, Ctrl+D to add next occurrence
#[test]
#[ignore]
fn blog_showcase_editing_multi_cursor() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let pd = h.project_dir().unwrap();
    create_demo_project(&pd);
    h.open_file(&pd.join("src/main.rs")).unwrap();

    let mut s = BlogShowcase::new(
        "editing/multi-cursor",
        "Multi-Cursor Editing",
        "Select multiple occurrences and edit them all at once.",
    );

    hold(&mut h, &mut s, 5, 100);

    // Navigate to "item" on line 7
    for _ in 0..6 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    h.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..2 {
        h.send_key(KeyCode::Right, KeyModifiers::CONTROL).unwrap();
    }
    h.render().unwrap();
    snap(&mut h, &mut s, None, 200);

    // Select word
    h.send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    snap(&mut h, &mut s, Some("Ctrl+W"), 150);
    hold_key(&mut h, &mut s, "Ctrl+W", 2, 100);

    // Add 3 more occurrences
    for _ in 0..3 {
        h.send_key(KeyCode::Char('d'), KeyModifiers::CONTROL)
            .unwrap();
        snap(&mut h, &mut s, Some("Ctrl+D"), 120);
        hold(&mut h, &mut s, 1, 80);
    }
    hold(&mut h, &mut s, 4, 100);

    // Type replacement
    for ch in "entry".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 50);
    }
    hold(&mut h, &mut s, 6, 100);

    s.finalize().unwrap();
}

/// Search & Replace: open via command palette and replace text
#[test]
#[ignore]
fn blog_showcase_editing_search_replace() {
    let mut h = EditorTestHarness::new(80, 24).unwrap();

    h.type_text("fn main() {\n    let item = get_item();\n    process_item(&item);\n    println!(\"item: {}\", item);\n}").unwrap();
    h.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();

    let mut s = BlogShowcase::new(
        "editing/search-replace",
        "Search & Replace",
        "Find and replace with incremental highlighting.",
    );

    hold(&mut h, &mut s, 4, 100);

    // Open command palette
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 120);

    // Type "Replace" to find the command
    for ch in "Replace".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 50);
    }
    hold(&mut h, &mut s, 2, 100);

    // Execute the Replace command
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 3, 100);

    // Type search term
    for ch in "item".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 70);
    }
    hold(&mut h, &mut s, 3, 100);

    // Enter to confirm search and move to replace field
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    snap(&mut h, &mut s, Some("Enter"), 120);

    // Type replacement
    for ch in "element".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 70);
    }
    hold(&mut h, &mut s, 3, 100);

    // Enter to confirm replacement
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 5, 100);

    s.finalize().unwrap();
}

/// Line editing: move lines up/down with Alt+Arrow
#[test]
#[ignore]
fn blog_showcase_editing_line_move() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let pd = h.project_dir().unwrap();
    create_demo_project(&pd);
    h.open_file(&pd.join("src/main.rs")).unwrap();

    let mut s = BlogShowcase::new(
        "editing/line-move",
        "Move Lines",
        "Move lines up and down with Alt+Arrow keys.",
    );

    // Go to line 5 (the items vec)
    h.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    for _ in 0..4 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    h.render().unwrap();
    hold(&mut h, &mut s, 4, 100);

    // Move line down twice
    h.send_key(KeyCode::Down, KeyModifiers::ALT).unwrap();
    snap(&mut h, &mut s, Some("Alt+↓"), 180);
    h.send_key(KeyCode::Down, KeyModifiers::ALT).unwrap();
    snap(&mut h, &mut s, Some("Alt+↓"), 180);
    hold(&mut h, &mut s, 3, 100);

    // Move back up twice
    h.send_key(KeyCode::Up, KeyModifiers::ALT).unwrap();
    snap(&mut h, &mut s, Some("Alt+↑"), 180);
    h.send_key(KeyCode::Up, KeyModifiers::ALT).unwrap();
    snap(&mut h, &mut s, Some("Alt+↑"), 180);
    hold(&mut h, &mut s, 5, 100);

    s.finalize().unwrap();
}

/// Block selection: Alt+Shift+Arrow for rectangular selection
#[test]
#[ignore]
fn blog_showcase_editing_block_selection() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let pd = h.project_dir().unwrap();

    // Create a file with aligned columns
    fs::create_dir_all(pd.join("src")).unwrap();
    fs::write(
        pd.join("data.txt"),
        "name       age  city\nalice      30   london\nbob        25   paris\ncharlie    35   tokyo\ndiana      28   berlin\neve        22   rome\nfrank      40   madrid\n",
    )
    .unwrap();
    h.open_file(&pd.join("data.txt")).unwrap();

    let mut s = BlogShowcase::new(
        "editing/block-selection",
        "Block Selection",
        "Rectangular column editing with Alt+Shift+Arrow.",
    );

    hold(&mut h, &mut s, 4, 100);

    // Position at start of "age" column (row 0, col 11)
    h.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..11 {
        h.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    }
    h.render().unwrap();
    snap(&mut h, &mut s, None, 200);

    // Block select down 6 rows and right 3 chars
    for _ in 0..6 {
        h.send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
            .unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some("Alt+Shift+↓"), 100);
    }
    for _ in 0..2 {
        h.send_key(KeyCode::Right, KeyModifiers::ALT | KeyModifiers::SHIFT)
            .unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some("Alt+Shift+→"), 100);
    }
    hold(&mut h, &mut s, 5, 100);

    // Escape
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    hold(&mut h, &mut s, 3, 100);

    s.finalize().unwrap();
}

/// Triple-click to select entire line
#[test]
#[ignore]
fn blog_showcase_editing_triple_click() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let pd = h.project_dir().unwrap();
    create_demo_project(&pd);
    h.open_file(&pd.join("src/main.rs")).unwrap();

    let mut s = BlogShowcase::new(
        "editing/triple-click",
        "Triple-Click Selection",
        "Triple-click to select an entire line.",
    );

    hold(&mut h, &mut s, 4, 100);

    // Triple click on line 5 (row ~6 in terminal including menu+tab bar)
    let click_row = 6u16;
    let click_col = 15u16;

    // First click
    h.mouse_click(click_col, click_row).unwrap();
    snap_mouse(&mut h, &mut s, Some("Click"), (click_col, click_row), 120);

    // Second click (double-click selects word)
    h.mouse_click(click_col, click_row).unwrap();
    snap_mouse(
        &mut h,
        &mut s,
        Some("Double-click"),
        (click_col, click_row),
        120,
    );

    // Third click (triple-click selects line)
    h.mouse_click(click_col, click_row).unwrap();
    snap_mouse(
        &mut h,
        &mut s,
        Some("Triple-click"),
        (click_col, click_row),
        200,
    );
    hold(&mut h, &mut s, 5, 100);

    // Click elsewhere to deselect
    h.mouse_click(5, 10).unwrap();
    hold(&mut h, &mut s, 3, 100);

    s.finalize().unwrap();
}

// =========================================================================
// Blog Post 2: Productivity Features
// =========================================================================

/// Command Palette: Ctrl+P for unified file/command/buffer navigation
#[test]
#[ignore]
fn blog_showcase_productivity_command_palette() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let pd = h.project_dir().unwrap();
    create_demo_project(&pd);
    h.open_file(&pd.join("src/main.rs")).unwrap();

    let mut s = BlogShowcase::new(
        "productivity/command-palette",
        "Command Palette",
        "Unified access to files, commands, buffers, and line navigation.",
    );

    hold(&mut h, &mut s, 4, 100);

    // Open palette
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 200);
    hold(&mut h, &mut s, 2, 100);

    // Search for a command (Ctrl+P already starts in command mode with ">")
    for ch in "theme".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 60);
    }
    hold(&mut h, &mut s, 4, 100);

    // Clear and try file mode
    // Escape and reopen
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    hold(&mut h, &mut s, 2, 100);

    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 150);

    // Type filename search
    for ch in "util".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 60);
    }
    hold(&mut h, &mut s, 4, 100);

    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    hold(&mut h, &mut s, 3, 100);

    s.finalize().unwrap();
}

/// Flash Jump: type nothing, press a single-letter label, teleport.
///
/// The "ultimate jump" demo — empty-pattern mode labels every visible
/// word start as soon as flash activates, so the user goes from
/// "where do I want to be?" to "I am there" in a single keypress.
/// Filtering by typing is also supported (any non-label letter
/// switches the labels to a substring search) but is intentionally
/// not shown here — that's a secondary mode and would muddy the demo.
#[test]
#[ignore]
fn blog_showcase_productivity_flash_jump() {
    use fresh::input::keybindings::Action::PluginAction;

    // Per-test isolated project root so the flash plugin is loaded
    // alongside the editor (same pattern as `flash` e2e tests).
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "flash");
    copy_plugin_lib(&plugins_dir);

    // Wider terminal so the flash status banner (`Flash[]`) survives
    // status-bar truncation — same width the e2e tests use.
    let mut h = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Real Rust code for the showcase.  The file is a stripped-down
    // text-buffer impl — one method per visible line, snake_case
    // throughout, with blank lines between methods.  The labeler's
    // skip rule removes the first letter of each unique word from
    // the pool; snake_case + repeated `pub fn` / `self` keeps the
    // skip set small (~14 letters), leaving roughly a dozen pool
    // letters to assign across ~30 visible word starts.  Distance-
    // sorting from the top-of-file cursor pushes the last few labels
    // down to the bottom methods, so picking a late-pool letter
    // teleports the cursor across most of the screen.
    let sample = r#"impl Buffer {
    pub fn new() -> Self {
        Self { rope: Rope::new(), dirty: false }
    }

    pub fn from_file(path: &Path) -> io::Result<Self> {
        let text = fs::read_to_string(path)?;
        Ok(Self { rope: Rope::from(text), dirty: false })
    }

    pub fn insert(&mut self, offset: usize, text: &str) {
        self.rope.insert(offset, text);
        self.dirty = true;
    }

    pub fn delete(&mut self, range: Range<usize>) {
        self.rope.remove(range);
        self.dirty = true;
    }

    pub fn save(&mut self, path: &Path) -> io::Result<()> {
        fs::write(path, self.rope.to_string())?;
        self.dirty = false;
        Ok(())
    }
}
"#;
    let sample_path = project_root.join("sample.rs");
    fs::write(&sample_path, sample).unwrap();
    h.open_file(&sample_path).unwrap();

    // Cursor stays at byte 0 (line 1, col 0).  Distance-sort then
    // assigns the first labels to nearby words at the top of the
    // file and the last available labels to the farthest visible
    // words near the bottom — so picking a late-pool label lets the
    // jump cross many lines and reads as a clear teleport.

    // Wait for the flash plugin's command to be registered before we
    // try to invoke it via the palette.
    h.wait_until(|h| {
        let commands = h.editor().command_registry().read().unwrap().get_all();
        commands
            .iter()
            .any(|c| c.action == PluginAction("flash_jump".to_string()))
    })
    .unwrap();

    let mut s = BlogShowcase::new(
        "productivity/flash-jump",
        "Flash Jump",
        "Type one letter, press a label, teleport across the file.",
    );

    // Pacing: previous version was too fast and dominated by typing.
    // This pass bumps each frame ~2× and pares the typed pattern
    // down to a single character — enough to spread labels across
    // every visible line of real code, but the demo still reads as
    // "press a key, jump", not "type a query then jump".

    // ---- Opening: settle on the file ----
    hold(&mut h, &mut s, 12, 120);

    // ---- Activate flash via the command palette ----
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 600);
    hold(&mut h, &mut s, 3, 200);

    for ch in "Flash: Jump".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
    }
    h.wait_for_screen_contains("Flash: Jump").unwrap();
    snap(&mut h, &mut s, None, 800);

    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| {
        h.editor().editor_mode() == Some("flash".to_string())
            && h.screen_to_string().contains("Flash[]")
    })
    .unwrap();
    snap(&mut h, &mut s, Some("Enter"), 500);
    hold(&mut h, &mut s, 4, 150);

    // ---- Hero moment: type a single letter, labels everywhere ----
    // `s` matches almost every line of real code (`Self`, `self`,
    // `string`, `&str`, …) — far more spots than the empty-pattern
    // word-start mode could cover, since the labeler's skip rule no
    // longer eats most of the pool.
    h.send_key(KeyCode::Char('s'), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Flash[s]"))
        .unwrap();
    snap(&mut h, &mut s, Some("s"), 600);

    // Long, deliberate hold on the labelled viewport — the single
    // most important frame in the GIF.  The viewer needs to be able
    // to read each label and pick one mentally.
    hold(&mut h, &mut s, 20, 160);

    // ---- The jump ----
    // With cursor at byte 0, label `m` is reliably assigned to the
    // `s` inside `io::Result` on line 21 — a near 20-line teleport
    // across the whole visible file.
    let initial_cursor = h.cursor_position();
    h.send_key(KeyCode::Char('m'), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| {
        h.editor().editor_mode() != Some("flash".to_string())
            && h.cursor_position() != initial_cursor
    })
    .unwrap();
    // Two extra renders so the status bar's cursor snapshot catches
    // up before capture.  (Pre-existing Fresh quirk: the status's
    // `Ln` value reads from a cached `primary_cursor_line_number`
    // that lags one tick behind `setBufferCursor`.)
    h.render().unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("m"), 700);

    // Closing: settle on the new cursor position so the viewer can
    // see where the jump landed.
    hold(&mut h, &mut s, 16, 150);

    s.finalize().unwrap();
}

/// Split View: horizontal and vertical splits with independent panes
#[test]
#[ignore]
fn blog_showcase_productivity_split_view() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let pd = h.project_dir().unwrap();
    create_demo_project(&pd);
    h.open_file(&pd.join("src/main.rs")).unwrap();

    let mut s = BlogShowcase::new(
        "productivity/split-view",
        "Split View",
        "Side-by-side editing with independent panes.",
    );

    hold(&mut h, &mut s, 4, 100);

    // Create horizontal split
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    h.type_text("split horiz").unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 120);
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 2, 100);

    // Open different file in new split
    h.open_file(&pd.join("src/utils.rs")).unwrap();
    h.render().unwrap();
    hold(&mut h, &mut s, 4, 100);

    // Switch between splits
    h.send_key(KeyCode::Char('k'), KeyModifiers::CONTROL)
        .unwrap();
    snap(&mut h, &mut s, Some("Ctrl+K"), 200);
    hold(&mut h, &mut s, 3, 100);

    h.send_key(KeyCode::Char('k'), KeyModifiers::CONTROL)
        .unwrap();
    snap(&mut h, &mut s, Some("Ctrl+K"), 200);
    hold(&mut h, &mut s, 3, 100);

    // Close split
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    h.type_text("close split").unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    hold(&mut h, &mut s, 4, 100);

    s.finalize().unwrap();
}

/// File Explorer: sidebar tree navigation
#[test]
#[ignore]
fn blog_showcase_productivity_file_explorer() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let pd = h.project_dir().unwrap();
    create_demo_project(&pd);
    h.open_file(&pd.join("src/main.rs")).unwrap();

    let mut s = BlogShowcase::new(
        "productivity/file-explorer",
        "File Explorer",
        "Sidebar tree view with fuzzy search and git indicators.",
    );

    hold(&mut h, &mut s, 4, 100);

    // Open explorer
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    snap(&mut h, &mut s, Some("Ctrl+E"), 200);
    hold(&mut h, &mut s, 2, 100);

    // Navigate down
    for _ in 0..3 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some("↓"), 100);
    }

    // Expand directory
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    snap(&mut h, &mut s, Some("Enter"), 150);
    hold(&mut h, &mut s, 2, 100);

    // Navigate into expanded dir
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    snap(&mut h, &mut s, Some("↓"), 120);
    hold(&mut h, &mut s, 3, 100);

    // Open file
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 4, 100);

    // Toggle back to editor
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    hold(&mut h, &mut s, 3, 100);

    s.finalize().unwrap();
}

/// Settings UI: graphical configuration editor
#[test]
#[ignore]
fn blog_showcase_productivity_settings() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let pd = h.project_dir().unwrap();
    create_demo_project(&pd);
    h.open_file(&pd.join("src/main.rs")).unwrap();

    let mut s = BlogShowcase::new(
        "productivity/settings",
        "Settings UI",
        "Graphical editor for all configuration options.",
    );

    hold(&mut h, &mut s, 3, 100);

    // Open settings via command palette
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    h.type_text("settings").unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 120);
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 3, 100);

    // Navigate settings categories
    for _ in 0..3 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some("↓"), 100);
    }
    hold(&mut h, &mut s, 3, 100);

    // Navigate more
    for _ in 0..2 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some("↓"), 100);
    }
    hold(&mut h, &mut s, 4, 100);

    // Filter settings with /
    h.send_key(KeyCode::Char('/'), KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("/"), 150);

    for ch in "terminal bg".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 70);
    }
    hold(&mut h, &mut s, 5, 100);

    // Confirm filter
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 4, 100);

    // Close settings
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    hold(&mut h, &mut s, 3, 100);

    s.finalize().unwrap();
}

/// Keybinding Editor: full-featured modal for customizing key bindings
#[test]
#[ignore]
fn blog_showcase_productivity_keybinding_editor() {
    let mut h = EditorTestHarness::new(120, 35).unwrap();
    h.render().unwrap();

    let mut s = BlogShowcase::new(
        "productivity/keybinding-editor",
        "Keybinding Editor",
        "Search, add, edit, and delete key bindings with conflict detection.",
    );

    hold(&mut h, &mut s, 3, 100);

    // Open keybinding editor directly
    h.editor_mut().open_keybinding_editor();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Keybinding Editor"), 250);
    hold(&mut h, &mut s, 3, 100);

    // Navigate down through bindings
    for _ in 0..6 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some("↓"), 80);
    }
    hold(&mut h, &mut s, 3, 100);

    // Activate search with /
    h.send_key(KeyCode::Char('/'), KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("/"), 150);

    // Type search query
    for ch in "save".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 70);
    }
    hold(&mut h, &mut s, 4, 100);

    // Press Enter to confirm search
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    hold(&mut h, &mut s, 3, 100);

    // Close
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    hold(&mut h, &mut s, 3, 100);

    s.finalize().unwrap();
}

/// Integrated Terminal: open a terminal split inside the editor
#[test]
#[ignore]
fn blog_showcase_productivity_terminal() {
    use portable_pty::{native_pty_system, PtySize};

    // Check PTY availability
    if native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_err()
    {
        eprintln!("Skipping terminal showcase: PTY not available");
        return;
    }

    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let pd = h.project_dir().unwrap();
    create_demo_project(&pd);
    h.open_file(&pd.join("src/main.rs")).unwrap();

    let mut s = BlogShowcase::new(
        "productivity/terminal",
        "Integrated Terminal",
        "Split terminal with scrollback and session persistence.",
    );

    hold(&mut h, &mut s, 4, 100);

    // Open terminal via command palette
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 120);

    for ch in "open terminal".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 50);
    }
    hold(&mut h, &mut s, 2, 100);

    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);

    // Wait for terminal to initialize
    std::thread::sleep(std::time::Duration::from_millis(500));
    h.render().unwrap();
    hold(&mut h, &mut s, 3, 100);

    // Type 'ls' in the terminal
    h.send_key(KeyCode::Char('l'), KeyModifiers::NONE).unwrap();
    std::thread::sleep(std::time::Duration::from_millis(50));
    h.render().unwrap();
    snap(&mut h, &mut s, Some("l"), 100);
    h.send_key(KeyCode::Char('s'), KeyModifiers::NONE).unwrap();
    std::thread::sleep(std::time::Duration::from_millis(50));
    h.render().unwrap();
    snap(&mut h, &mut s, Some("s"), 100);
    hold(&mut h, &mut s, 2, 100);

    // Press Enter to execute
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    std::thread::sleep(std::time::Duration::from_millis(200));
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 5, 100);

    // Switch back to editor
    h.send_key(KeyCode::Char('k'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+K"), 200);
    hold(&mut h, &mut s, 4, 100);

    s.finalize().unwrap();
}

// =========================================================================
// Blog Post 1 (additional): More Editing Features
// =========================================================================

/// Sort Lines: select lines and sort alphabetically
#[test]
#[ignore]
fn blog_showcase_editing_sort_lines() {
    let mut h = EditorTestHarness::new(80, 24).unwrap();

    let mut s = BlogShowcase::new(
        "editing/sort-lines",
        "Sort Lines",
        "Select lines and sort them alphabetically via command palette.",
    );

    // Type unsorted lines
    h.type_text("cherry\norange\napple\nbanana\ndate\nelderberry")
        .unwrap();
    h.render().unwrap();
    hold(&mut h, &mut s, 4, 100);

    // Select all with Ctrl+A
    h.send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+A"), 200);
    hold(&mut h, &mut s, 2, 100);

    // Open command palette
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 120);

    // Type "sort lines"
    for ch in "sort lines".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 50);
    }
    hold(&mut h, &mut s, 2, 100);

    // Execute
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 5, 100);

    s.finalize().unwrap();
}

/// Case conversion: Alt+U for uppercase, Alt+L for lowercase
#[test]
#[ignore]
fn blog_showcase_editing_case_conversion() {
    let mut h = EditorTestHarness::new(80, 24).unwrap();

    let mut s = BlogShowcase::new(
        "editing/case-conversion",
        "Case Conversion",
        "Convert selected text to uppercase (Alt+U) or lowercase (Alt+L).",
    );

    // Type some text
    h.type_text("hello world from fresh editor").unwrap();
    h.render().unwrap();
    hold(&mut h, &mut s, 4, 100);

    // Go to start, select "hello world"
    h.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..11 {
        h.send_key(KeyCode::Right, KeyModifiers::SHIFT).unwrap();
    }
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Select"), 200);
    hold(&mut h, &mut s, 2, 100);

    // Convert to uppercase
    h.send_key(KeyCode::Char('u'), KeyModifiers::ALT).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Alt+U"), 250);
    hold(&mut h, &mut s, 4, 100);

    // Select "FROM FRESH" (already uppercase from previous)
    // First click to deselect, then re-select
    h.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    // Move past "HELLO WORLD "
    for _ in 0..12 {
        h.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    }
    // Select "from fresh"
    for _ in 0..10 {
        h.send_key(KeyCode::Right, KeyModifiers::SHIFT).unwrap();
    }
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Select"), 200);
    hold(&mut h, &mut s, 2, 100);

    // Convert to uppercase too
    h.send_key(KeyCode::Char('u'), KeyModifiers::ALT).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Alt+U"), 250);
    hold(&mut h, &mut s, 3, 100);

    // Now select all and lowercase
    h.send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+A"), 150);

    h.send_key(KeyCode::Char('l'), KeyModifiers::ALT).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Alt+L"), 250);
    hold(&mut h, &mut s, 5, 100);

    s.finalize().unwrap();
}

/// Duplicate line: Ctrl+Shift+D to duplicate current line
#[test]
#[ignore]
fn blog_showcase_editing_duplicate_line() {
    let mut h = EditorTestHarness::new(80, 24).unwrap();

    let mut s = BlogShowcase::new(
        "editing/duplicate-line",
        "Duplicate Line",
        "Duplicate the current line with a single command.",
    );

    // Type some code
    h.type_text("fn greet(name: &str) {\n    println!(\"Hello, {}!\", name);\n}")
        .unwrap();
    h.render().unwrap();
    hold(&mut h, &mut s, 4, 100);

    // Go to line 2 (the println line)
    h.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, None, 200);
    hold(&mut h, &mut s, 2, 100);

    // Duplicate via command palette
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    h.type_text("duplicate line").unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 100);
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 3, 100);

    // Duplicate again
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    h.type_text("duplicate line").unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Duplicate"), 200);
    hold(&mut h, &mut s, 5, 100);

    s.finalize().unwrap();
}

/// Tab indent/dedent: Tab indents selected lines, Shift+Tab dedents
#[test]
#[ignore]
fn blog_showcase_editing_tab_indent() {
    let mut h = EditorTestHarness::new(80, 24).unwrap();

    let mut s = BlogShowcase::new(
        "editing/tab-indent",
        "Tab Indent Selection",
        "Tab indents selected lines, Shift+Tab dedents.",
    );

    // Type code lines
    h.type_text("fn example() {\nlet a = 1;\nlet b = 2;\nlet c = 3;\n}")
        .unwrap();
    h.render().unwrap();
    hold(&mut h, &mut s, 4, 100);

    // Select lines 2-4
    h.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    for _ in 0..3 {
        h.send_key(KeyCode::Down, KeyModifiers::SHIFT).unwrap();
    }
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Select"), 200);
    hold(&mut h, &mut s, 2, 100);

    // Indent with Tab
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Tab"), 200);
    hold(&mut h, &mut s, 2, 100);

    // Indent again
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Tab"), 200);
    hold(&mut h, &mut s, 2, 100);

    // Dedent with Shift+Tab
    h.send_key(KeyCode::BackTab, KeyModifiers::SHIFT).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Shift+Tab"), 200);
    hold(&mut h, &mut s, 4, 100);

    s.finalize().unwrap();
}

// =========================================================================
// Blog Post 3: Themes
// =========================================================================

/// Select Theme: browse and apply color themes
#[test]
#[ignore]
fn blog_showcase_themes_select_theme() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let pd = h.project_dir().unwrap();
    create_demo_project(&pd);
    h.open_file(&pd.join("src/main.rs")).unwrap();

    let mut s = BlogShowcase::new(
        "themes/select-theme",
        "Select Theme",
        "Browse and apply color themes from the command palette.",
    );

    hold(&mut h, &mut s, 4, 100);

    // Open command palette
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 120);

    // Type "Select Theme"
    for ch in "Select Theme".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 50);
    }
    hold(&mut h, &mut s, 2, 100);

    // Execute
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 3, 100);

    // Browse themes with arrow keys
    for _ in 0..3 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some("↓"), 150);
        hold(&mut h, &mut s, 2, 100);
    }

    // Select a theme
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 5, 100);

    s.finalize().unwrap();
}

// =========================================================================
// Blog Post 4: What's New in Fresh (0.2.3–0.2.9)
// =========================================================================

/// Code Folding: fold/unfold code blocks via gutter click
#[test]
#[ignore]
fn blog_showcase_fresh_0_2_9_code_folding() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let pd = h.project_dir().unwrap();
    create_demo_project(&pd);
    h.open_file(&pd.join("src/main.rs")).unwrap();

    // Set up folding ranges for the functions in our demo file
    // load_config starts at line 17 (0-indexed), process_item at line 22
    {
        let state = h.editor_mut().active_state_mut();
        let ranges = vec![
            FoldingRange {
                start_line: 2, // fn main()
                end_line: 15,
                start_character: None,
                end_character: None,
                kind: None,
                collapsed_text: None,
            },
            FoldingRange {
                start_line: 17, // fn load_config
                end_line: 20,
                start_character: None,
                end_character: None,
                kind: None,
                collapsed_text: None,
            },
            FoldingRange {
                start_line: 22, // fn process_item
                end_line: 26,
                start_character: None,
                end_character: None,
                kind: None,
                collapsed_text: None,
            },
        ];
        state
            .folding_ranges
            .set_from_lsp(&state.buffer, &mut state.marker_list, ranges);
    }

    let mut s = BlogShowcase::new(
        "fresh-0.2.9/code-folding",
        "Code Folding",
        "Fold and unfold code blocks via gutter click or command palette.",
    );

    // Show the file with fold indicators in the gutter
    hold(&mut h, &mut s, 5, 100);

    // Click gutter on line 2 (fn main) to fold — content row 2 + menu/tab offset
    let fold_row = 4u16; // CONTENT_START_ROW (2) + line 2 = row 4
    h.mouse_click(0, fold_row).unwrap();
    snap(&mut h, &mut s, Some("Click gutter"), 200);
    hold(&mut h, &mut s, 4, 100);

    // Fold load_config too — after main is folded, it shifts up
    // After folding main (lines 2-15), load_config header moves up
    h.mouse_click(0, fold_row + 1).unwrap();
    snap(&mut h, &mut s, Some("Click gutter"), 200);
    hold(&mut h, &mut s, 4, 100);

    // Navigate down past the folded regions
    for _ in 0..5 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        snap(&mut h, &mut s, Some("↓"), 80);
    }
    hold(&mut h, &mut s, 3, 100);

    // Unfold main by clicking gutter again
    h.mouse_click(0, fold_row).unwrap();
    snap(&mut h, &mut s, Some("Unfold"), 200);
    hold(&mut h, &mut s, 5, 100);

    s.finalize().unwrap();
}

/// Markdown Compose Mode: side-by-side source and composed view with scroll sync
#[test]
#[ignore]
fn blog_showcase_fresh_0_2_9_compose_mode() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    // Use a messy, unaligned table so the compose rendering difference is obvious.
    // Document is long enough that scrolling visibly moves the viewport.
    let md_content = r#"# Project Notes

## Overview

This is a **terminal text editor** written in Rust. It supports *syntax highlighting*, **code folding**, and [LSP integration](https://langserver.org/).

## Features

- Multi-cursor editing with **Ctrl+D**
- Markdown **compose mode** for distraction-free writing
- Side-by-side *diff view* with word-level highlights
- Integrated terminal with *session persistence*
- [Plugin system](https://getfresh.dev/plugins/) with package manager

## Status

|Feature|Status|Notes|
|---|---|---|
|Code folding|done|via LSP foldingRange|
|Auto-save|done|configurable interval|
|Vertical rulers|done|per-buffer|
|Line scanning|done|SSH server-side|
|Compose mode|experimental|tables, conceals, soft breaks|

## Getting Started

Install with `cargo install fresh-editor` or download a binary from the [releases page](https://github.com/sinelaw/fresh/releases).

Run `fresh` to start editing. Press **Ctrl+P** to open the command palette.

## Architecture

Fresh is built on a **piece table** data structure for efficient text manipulation. The rendering pipeline uses [ratatui](https://ratatui.rs/) for terminal UI.

Plugins run in a **QuickJS** sandbox with an async message-passing API. Each plugin can register hooks for editor events like `buffer_activated`, `cursor_moved`, and `buffer_saved`.

## Configuration

Settings are stored in `~/.config/fresh/config.json`. You can edit them via the **Settings UI** or directly in the JSON file.

Keybindings live in the same config file under the `keybindings` key. Use the **Keybinding Editor** for a visual interface.

## Remote Editing

Fresh supports editing files over **SSH** with `fresh user@host:path`. The connection uses your existing SSH keys or prompts for a password.

Large file operations like *line scanning* run server-side — only the index is transferred, not the file content.

## License

Released under the **Apache 2.0** license. See [LICENSE](https://github.com/sinelaw/fresh/blob/master/LICENSE) for details.
"#;

    let md_path = project_root.join("notes.md");
    fs::write(&md_path, md_content).unwrap();

    let mut h =
        EditorTestHarness::with_config_and_working_dir(120, 30, Default::default(), project_root)
            .unwrap();
    h.open_file(&md_path).unwrap();
    h.render().unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.2.9/compose-mode",
        "Markdown Compose Mode",
        "Distraction-free writing with concealed markup and table rendering.",
    );

    // Show the raw markdown
    hold(&mut h, &mut s, 5, 100);

    // Create a vertical split
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    h.type_text("split vert").unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Split Vertical"), 200);
    hold(&mut h, &mut s, 3, 100);

    // Enable compose mode in the right panel
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    h.type_text("Toggle Compose").unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // Wait for compose mode to take effect
    for _ in 0..10 {
        h.render().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(50));
    }
    snap(&mut h, &mut s, Some("Toggle Compose"), 300);
    hold(&mut h, &mut s, 5, 100);

    // Enable scroll sync
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    h.type_text("Toggle Scroll Sync").unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Scroll Sync"), 200);
    hold(&mut h, &mut s, 3, 100);

    // Switch to source panel (left) so scrolling there drives the compose panel
    h.send_key(KeyCode::Char('k'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+K"), 150);
    hold(&mut h, &mut s, 2, 100);

    // Scroll down well past the viewport — both panels move together
    for i in 0..45 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        if i % 4 == 3 {
            snap(&mut h, &mut s, Some("↓"), 80);
        }
    }
    hold(&mut h, &mut s, 5, 100);

    s.finalize().unwrap();
    drop(temp_dir);
}

/// Large File Line Scanning: Go to Line triggers scan, then jumps precisely
#[test]
#[ignore]
fn blog_showcase_fresh_0_2_9_large_file_scanning() {
    let big_txt_path = TestFixture::big_txt_for_test("blog_showcase").unwrap();

    let mut h = EditorTestHarness::new(100, 30).unwrap();
    h.open_file(&big_txt_path).unwrap();
    h.render().unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.2.9/large-file-scanning",
        "Precise Go to Line in Large Files",
        "Go to Line triggers an efficient scan, then jumps to the exact line.",
    );

    // Show the file with byte offsets in the gutter
    hold(&mut h, &mut s, 5, 200);

    // Navigate down a bit to show byte offsets
    for _ in 0..8 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    h.render().unwrap();
    snap(&mut h, &mut s, None, 200);
    hold(&mut h, &mut s, 3, 200);

    // Press Ctrl+G to trigger Go to Line — shows scan confirm prompt
    h.send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+G"), 500);
    hold(&mut h, &mut s, 3, 200);

    // Type "y" to accept the scan
    h.send_key(KeyCode::Char('y'), KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("y"), 300);
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 400);

    // Drive the scan to completion
    while h.editor_mut().process_line_scan() {}
    h.render().unwrap();

    // The "Go to line:" prompt is now open — show it
    hold(&mut h, &mut s, 5, 200);

    // Type a line number to jump to
    for ch in "500000".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 120);
    }
    hold(&mut h, &mut s, 2, 200);

    // Press Enter to jump
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);

    // Hold on the result — gutter shows exact line numbers at the target
    hold(&mut h, &mut s, 8, 200);

    s.finalize().unwrap();
}

/// Vertical Rulers: add column rulers via command palette
#[test]
#[ignore]
fn blog_showcase_fresh_0_2_9_vertical_rulers() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let pd = h.project_dir().unwrap();
    create_demo_project(&pd);
    h.open_file(&pd.join("src/main.rs")).unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.2.9/vertical-rulers",
        "Vertical Rulers",
        "Add column rulers to enforce line length limits.",
    );

    hold(&mut h, &mut s, 4, 100);

    // Add ruler at column 40 (visible in 100-col terminal)
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    h.type_text("Add Ruler").unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 120);
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 150);
    hold(&mut h, &mut s, 2, 100);

    // Type column number
    h.type_text("40").unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("40"), 150);
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 4, 100);

    // Add a second ruler at column 80
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    h.type_text("Add Ruler").unwrap();
    h.render().unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Add Ruler"), 150);
    hold(&mut h, &mut s, 2, 100);

    h.type_text("80").unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("80"), 150);
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 5, 100);

    s.finalize().unwrap();
}

/// Smart Editing: smart home, smart backspace dedent, auto-indent
#[test]
#[ignore]
fn blog_showcase_fresh_0_2_9_smart_editing() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    fs::create_dir(&project_root).unwrap();

    let rs_path = project_root.join("test.rs");
    fs::write(&rs_path, "").unwrap();

    let mut h =
        EditorTestHarness::with_config_and_working_dir(80, 24, Default::default(), project_root)
            .unwrap();
    h.open_file(&rs_path).unwrap();
    h.render().unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.2.9/smart-editing",
        "Smart Editing",
        "Smart Home, smart backspace dedent, and improved auto-indent.",
    );

    // Start typing a function declaration
    hold(&mut h, &mut s, 3, 200);

    for ch in "fn example() {".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 60);
    }
    hold(&mut h, &mut s, 2, 200);

    // Press Enter — auto-indent adds leading whitespace
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 300);
    hold_key(&mut h, &mut s, "auto-indent", 3, 200);

    // Type a line of code
    for ch in "let x = 1;".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 60);
    }
    hold(&mut h, &mut s, 2, 200);

    // Press Enter again — indent is preserved
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 300);
    hold_key(&mut h, &mut s, "auto-indent", 3, 200);

    // Type another line
    for ch in "let y = 2;".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 60);
    }
    hold(&mut h, &mut s, 2, 200);

    // Press Enter
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 2, 200);

    // Backspace — smart dedent removes one indent level
    h.send_key(KeyCode::Backspace, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Backspace"), 400);
    hold_key(&mut h, &mut s, "smart dedent", 3, 200);

    // Tab — indent back
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Tab"), 400);
    hold_key(&mut h, &mut s, "indent", 3, 200);

    hold(&mut h, &mut s, 5, 200);

    s.finalize().unwrap();
    drop(temp_dir);
}

/// Auto-Save: enable persistent auto-save via settings
#[test]
#[ignore]
fn blog_showcase_fresh_0_2_9_auto_save() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let pd = h.project_dir().unwrap();
    create_demo_project(&pd);
    h.open_file(&pd.join("src/main.rs")).unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.2.9/auto-save",
        "Auto-Save",
        "Persistent auto-save writes modified buffers to disk at a configurable interval.",
    );

    hold(&mut h, &mut s, 4, 100);

    // Open settings via command palette
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 120);

    for ch in "settings".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 50);
    }
    hold(&mut h, &mut s, 2, 100);

    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 3, 100);

    // Filter settings to find auto_save
    h.send_key(KeyCode::Char('/'), KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("/"), 150);

    for ch in "auto_save".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 60);
    }
    hold(&mut h, &mut s, 3, 100);

    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 3, 100);

    // Navigate to the setting and toggle it
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("↓"), 100);
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 5, 100);

    // Close settings
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    hold(&mut h, &mut s, 3, 100);

    s.finalize().unwrap();
}

// =========================================================================
// Blog Post 5: What's New in Fresh (0.2.11–0.2.18)
// =========================================================================

/// Project-Wide Search & Replace: search across files and replace with Alt+Enter
#[test]
#[ignore]
fn blog_showcase_fresh_0_2_18_project_search_replace() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    fs::create_dir_all(project_root.join("src")).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "search_replace");

    fs::write(
        project_root.join("src/main.rs"),
        r#"use std::collections::HashMap;

fn main() {
    let config = load_config("settings.json");
    let items = vec!["alpha", "beta", "gamma"];

    for item in &items {
        process_item(item, &config);
    }

    println!("Done processing {} items", items.len());
}

fn load_config(path: &str) -> HashMap<String, String> {
    println!("Loading config from {}", path);
    HashMap::new()
}

fn process_item(item: &str, _config: &HashMap<String, String>) {
    let value = item.to_uppercase();
    println!("[{}] {}", item, value);
}
"#,
    )
    .unwrap();

    fs::write(
        project_root.join("src/utils.rs"),
        r#"pub fn process_batch(items: &[&str]) -> Vec<String> {
    items.iter().map(|item| process_item(item)).collect()
}

pub fn process_item(item: &str) -> String {
    item.to_uppercase()
}
"#,
    )
    .unwrap();

    fs::write(
        project_root.join("src/tests.rs"),
        r#"#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_process_item() {
        let config = HashMap::new();
        process_item("test", &config);
    }
}
"#,
    )
    .unwrap();

    let mut h = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new()
            .with_working_dir(project_root)
            .with_full_grammar_registry()
            .without_empty_plugins_dir(),
    )
    .unwrap();
    h.open_file(&temp_dir.path().join("project/src/main.rs"))
        .unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.2.18/project-search-replace",
        "Project-Wide Search & Replace",
        "Search and replace across your entire project. Alt+Enter to replace all matches.",
    );

    hold(&mut h, &mut s, 4, 100);

    // Open project search via command palette
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 150);

    for ch in "Search and Replace".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
    }
    snap(&mut h, &mut s, None, 100);
    hold(&mut h, &mut s, 2, 100);

    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 3, 100);

    // Type search term
    for ch in "process_item".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 50);
    }

    // Press Enter to confirm search and move to replace field
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 150);

    // Type replacement
    for ch in "handle_item".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 50);
    }
    hold(&mut h, &mut s, 2, 100);

    // Press Enter to confirm replacement and trigger the search
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 150);

    // Wait for streaming search results to arrive
    h.wait_until_stable(|h| {
        let screen = h.screen_to_string();
        screen.contains("[v]") || screen.contains("matches")
    })
    .unwrap();
    hold(&mut h, &mut s, 6, 100);

    // Alt+Enter to replace all
    h.send_key(KeyCode::Enter, KeyModifiers::ALT).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Alt+Enter"), 400);
    hold_key(&mut h, &mut s, "Replace All", 5, 150);

    s.finalize().unwrap();
}

/// Inline Diagnostics: diagnostic text displayed at end of lines
#[test]
#[ignore]
fn blog_showcase_fresh_0_2_18_inline_diagnostics() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    fs::create_dir_all(project_root.join("src")).unwrap();

    let rs_path = project_root.join("src/main.rs");
    fs::write(
        &rs_path,
        r#"fn main() {
    let x: i32 = "hello";
    let y = unknown_function();
    let z: bool = 42;
    println!("{} {} {}", x, y, z);
}
"#,
    )
    .unwrap();

    let mut config = fresh::config::Config::default();
    config.editor.diagnostics_inline_text = true;

    let mut h = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(project_root)
            .with_full_grammar_registry()
            .without_empty_plugins_dir(),
    )
    .unwrap();
    h.open_file(&rs_path).unwrap();

    // Inject fake diagnostics via the proper API
    {
        let diagnostics = vec![
            lsp_types::Diagnostic {
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 1,
                        character: 18,
                    },
                    end: lsp_types::Position {
                        line: 1,
                        character: 25,
                    },
                },
                severity: Some(lsp_types::DiagnosticSeverity::ERROR),
                message: "mismatched types: expected `i32`, found `&str`".to_string(),
                code: None,
                code_description: None,
                source: None,
                related_information: None,
                tags: None,
                data: None,
            },
            lsp_types::Diagnostic {
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 2,
                        character: 12,
                    },
                    end: lsp_types::Position {
                        line: 2,
                        character: 28,
                    },
                },
                severity: Some(lsp_types::DiagnosticSeverity::ERROR),
                message: "cannot find function `unknown_function`".to_string(),
                code: None,
                code_description: None,
                source: None,
                related_information: None,
                tags: None,
                data: None,
            },
            lsp_types::Diagnostic {
                range: lsp_types::Range {
                    start: lsp_types::Position {
                        line: 3,
                        character: 19,
                    },
                    end: lsp_types::Position {
                        line: 3,
                        character: 21,
                    },
                },
                severity: Some(lsp_types::DiagnosticSeverity::WARNING),
                message: "mismatched types: expected `bool`, found integer".to_string(),
                code: None,
                code_description: None,
                source: None,
                related_information: None,
                tags: None,
                data: None,
            },
        ];
        let state = h.editor_mut().active_state_mut();
        let theme = fresh::view::theme::Theme::load_builtin("dark").unwrap();
        fresh::services::lsp::diagnostics::apply_diagnostics_to_state(state, &diagnostics, &theme);
    }

    let mut s = BlogShowcase::new(
        "fresh-0.2.18/inline-diagnostics",
        "Inline Diagnostics",
        "Diagnostic messages displayed right-aligned at the end of each line.",
    );

    // Show the file with inline diagnostics
    hold(&mut h, &mut s, 8, 100);

    // Navigate through diagnostic lines
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    snap(&mut h, &mut s, Some("↓"), 200);
    hold(&mut h, &mut s, 3, 100);

    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    snap(&mut h, &mut s, Some("↓"), 200);
    hold(&mut h, &mut s, 3, 100);

    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    snap(&mut h, &mut s, Some("↓"), 200);
    hold(&mut h, &mut s, 5, 100);

    s.finalize().unwrap();
}

/// Surround Selection: typing a delimiter wraps the selection
#[test]
#[ignore]
fn blog_showcase_fresh_0_2_18_surround_selection() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    fs::create_dir(&project_root).unwrap();

    let rs_path = project_root.join("main.rs");
    fs::write(
        &rs_path,
        r#"fn render_page(title: &str, items: &[Item]) -> String {
    let header = format!("Welcome to {}", title);
    let count = items.len().to_string();
    let body = items.iter()
        .map(|item| item.name.clone())
        .collect::<Vec<_>>()
        .join(", ");
    format!("{}\n{} items: {}", header, count, body)
}
"#,
    )
    .unwrap();

    let mut h = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(fresh::config::Config::default())
            .with_working_dir(project_root)
            .with_full_grammar_registry()
            .without_empty_plugins_dir(),
    )
    .unwrap();
    h.open_file(&rs_path).unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.2.18/surround-selection",
        "Surround Selection",
        "Select text and type a delimiter to wrap it.",
    );

    hold(&mut h, &mut s, 4, 100);

    // Go to line 3: "let count = items.len().to_string();"
    // We'll select "items.len()" and wrap it with parentheses — a common
    // pattern when you want to add a method call on a sub-expression.
    for _ in 0..2 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    h.render().unwrap();

    // Move to start of "items.len()" — SmartHome goes to first non-ws (col 4),
    // then 12 right to reach col 16 where "items" starts
    h.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..12 {
        h.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    }
    h.render().unwrap();
    snap(&mut h, &mut s, None, 150);

    // Select "items.len()" with Shift+Right x11
    for _ in 0..11 {
        h.send_key(KeyCode::Right, KeyModifiers::SHIFT).unwrap();
    }
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Select"), 200);
    hold(&mut h, &mut s, 3, 100);

    // Type ( to surround — result: (items.len())
    h.send_key(KeyCode::Char('('), KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("("), 400);
    hold_key(&mut h, &mut s, "(items.len())", 4, 150);

    // Now demonstrate with braces: select "body" on the format! line and wrap with {
    // Go to line 8: "    format!(..., header, count, body)"
    for _ in 0..5 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    h.render().unwrap();

    // Move to start of "body" — last arg in format! macro
    h.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    // back past ")" at EOL
    for _ in 0..5 {
        h.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    }
    h.render().unwrap();
    snap(&mut h, &mut s, None, 150);

    // Select "body" (4 chars)
    for _ in 0..4 {
        h.send_key(KeyCode::Right, KeyModifiers::SHIFT).unwrap();
    }
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Select"), 200);
    hold(&mut h, &mut s, 2, 100);

    // Type { to surround with braces
    h.send_key(KeyCode::Char('{'), KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("{"), 400);
    hold_key(&mut h, &mut s, "{body}", 4, 150);

    hold(&mut h, &mut s, 3, 100);

    s.finalize().unwrap();
}

/// Whitespace Indicators: granular control over space and tab visibility
#[test]
#[ignore]
fn blog_showcase_fresh_0_2_18_whitespace_indicators() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    fs::create_dir(&project_root).unwrap();

    let py_path = project_root.join("example.py");
    fs::write(
        &py_path,
        "def process(items):\n    for item in items:\n        if item.is_valid():  \n            result = item.transform()\n            yield result\n\ndef main():\n    data = [1, 2, 3]   \n    for r in process(data):\n        print(r)\n",
    )
    .unwrap();

    let mut config = fresh::config::Config::default();
    config.editor.whitespace_spaces_leading = true;
    config.editor.whitespace_spaces_inner = true;
    config.editor.whitespace_spaces_trailing = true;
    config.editor.whitespace_tabs_leading = true;
    config.editor.whitespace_tabs_inner = true;
    config.editor.whitespace_tabs_trailing = true;

    let mut h = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(project_root)
            .with_full_grammar_registry()
            .without_empty_plugins_dir(),
    )
    .unwrap();
    h.open_file(&py_path).unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.2.18/whitespace-indicators",
        "Whitespace Indicators",
        "Granular control over space and tab visibility — leading, inner, trailing, or all.",
    );

    // Show the file with whitespace indicators visible
    hold(&mut h, &mut s, 8, 100);

    // Navigate through to show different whitespace patterns
    for _ in 0..4 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        snap(&mut h, &mut s, Some("↓"), 150);
    }
    hold(&mut h, &mut s, 4, 100);

    // Open settings to show configuration
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 120);

    for ch in "settings".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
    }
    snap(&mut h, &mut s, None, 80);

    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 2, 100);

    // Filter for whitespace
    h.send_key(KeyCode::Char('/'), KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    for ch in "whitespace".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
    }
    snap(&mut h, &mut s, Some("whitespace"), 150);
    hold(&mut h, &mut s, 5, 100);

    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    hold(&mut h, &mut s, 3, 100);

    s.finalize().unwrap();
}

/// Theme Editor Redesign: virtual scrolling, mouse support, inspect at cursor
#[test]
#[ignore]
fn blog_showcase_fresh_0_2_18_theme_editor() {
    let mut h = EditorTestHarness::with_temp_project(100, 30).unwrap();
    let pd = h.project_dir().unwrap();
    create_demo_project(&pd);
    h.open_file(&pd.join("src/main.rs")).unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.2.18/theme-editor",
        "Theme Editor Redesign",
        "Redesigned theme editor with virtual scrolling, mouse support, and Inspect Theme at Cursor.",
    );

    hold(&mut h, &mut s, 4, 100);

    // Open theme editor via command palette
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 150);

    for ch in "theme editor".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
    }
    snap(&mut h, &mut s, None, 100);
    hold(&mut h, &mut s, 2, 100);

    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 200);
    hold(&mut h, &mut s, 4, 100);

    // Scroll down through theme entries
    for _ in 0..6 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some("↓"), 80);
    }
    hold(&mut h, &mut s, 3, 100);

    // Scroll more to show virtual scrolling
    for _ in 0..8 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some("↓"), 60);
    }
    hold(&mut h, &mut s, 4, 100);

    // Go back up
    for _ in 0..5 {
        h.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some("↑"), 60);
    }
    hold(&mut h, &mut s, 4, 100);

    // Close
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    hold(&mut h, &mut s, 3, 100);

    s.finalize().unwrap();
}

/// Syntax Grammars: 30 new languages with highlighting
#[test]
#[ignore]
fn blog_showcase_fresh_0_2_18_syntax_grammars() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    fs::create_dir_all(project_root.join("src")).unwrap();

    // Create files in various languages to show off syntax highlighting
    fs::write(
        project_root.join("Dockerfile"),
        r#"FROM rust:1.75 AS builder
WORKDIR /app
COPY Cargo.toml Cargo.lock ./
RUN cargo build --release
COPY src/ ./src/
RUN cargo build --release

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y libssl3
COPY --from=builder /app/target/release/fresh /usr/local/bin/
ENTRYPOINT ["fresh"]
"#,
    )
    .unwrap();

    fs::write(
        project_root.join("config.nix"),
        r#"{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    rustc
    cargo
    pkg-config
    openssl
  ];

  shellHook = ''
    echo "Fresh development environment"
    export RUST_LOG=debug
  '';
}
"#,
    )
    .unwrap();

    fs::write(
        project_root.join("app.svelte"),
        r#"<script lang="ts">
  import { onMount } from 'svelte';

  let count = $state(0);
  let doubled = $derived(count * 2);

  function increment() {
    count += 1;
  }
</script>

<main>
  <h1>Count: {count}</h1>
  <p>Doubled: {doubled}</p>
  <button onclick={increment}>+1</button>
</main>

<style>
  main { padding: 2rem; }
  button { font-size: 1.5rem; }
</style>
"#,
    )
    .unwrap();

    let mut h = EditorTestHarness::create(
        100,
        30,
        HarnessOptions::new()
            .with_working_dir(project_root.clone())
            .with_full_grammar_registry()
            .without_empty_plugins_dir(),
    )
    .unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.2.18/syntax-grammars",
        "30 New Syntax Grammars",
        "Dockerfile, Nix, Svelte, Vue, Kotlin, Swift, and 24 more languages with syntax highlighting.",
    );

    // Open Dockerfile
    h.open_file(&project_root.join("Dockerfile")).unwrap();
    hold(&mut h, &mut s, 6, 100);

    // Show Nix file
    h.open_file(&project_root.join("config.nix")).unwrap();
    hold(&mut h, &mut s, 6, 100);

    // Show Svelte file
    h.open_file(&project_root.join("app.svelte")).unwrap();
    hold(&mut h, &mut s, 6, 100);

    // Scroll through Svelte to show mixed HTML/JS/CSS highlighting
    for _ in 0..10 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some("↓"), 60);
    }
    hold(&mut h, &mut s, 5, 100);

    s.finalize().unwrap();
}

/// Hot Exit: unnamed buffer content persists across editor restarts
#[test]
#[ignore]
fn blog_showcase_fresh_0_2_18_hot_exit() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    fs::create_dir(&project_root).unwrap();

    let dir_context = fresh::config_io::DirectoryContext::for_testing(temp_dir.path());

    let mut h = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_shared_dir_context(dir_context.clone())
            .with_working_dir(project_root.clone())
            .without_empty_plugins_dir(),
    )
    .unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.2.18/hot-exit",
        "Hot Exit",
        "Unnamed scratch buffers persist across editor restarts — never lose your quick notes.",
    );

    // Type some quick notes into the unnamed buffer
    let notes = "TODO before release:\n- run full test suite\n- update changelog\n- tag v0.2.18\n- build release artifacts";
    for ch in notes.chars() {
        if ch == '\n' {
            h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
        } else {
            h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        }
    }
    h.render().unwrap();
    hold(&mut h, &mut s, 8, 100);

    // Quit the editor (Ctrl+Q)
    h.shutdown(true).unwrap();
    snap(&mut h, &mut s, Some("Ctrl+Q"), 400);
    hold_key(&mut h, &mut s, "quit", 4, 150);

    // ---- Restart the editor in a new harness ----
    let mut h2 = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_shared_dir_context(dir_context)
            .with_working_dir(project_root)
            .without_empty_plugins_dir(),
    )
    .unwrap();

    h2.startup(true, &[]).unwrap();
    h2.render().unwrap();

    // Show the restored buffer — notes are back!
    hold(&mut h2, &mut s, 10, 100);

    s.finalize().unwrap();
}

// =========================================================================
// Blog Post 6: Modernized Git Log Panel (buffer group + live preview)
// =========================================================================

/// Build a hermetic repo with several commits by distinct authors so the
/// aligned-column log and right-panel detail have something meaningful
/// to display in the showcase.
fn build_git_log_demo_repo() -> GitTestRepo {
    let repo = GitTestRepo::new();

    // Commit 1 — initial scaffold (Alice).
    repo.create_file(
        "src/main.rs",
        "fn main() {\n    println!(\"Hello, world!\");\n}\n",
    );
    repo.create_file("README.md", "# Fresh demo\n\nA tiny sample project.\n");
    repo.git_add_all();
    repo.git_commit("feat: initial scaffold");

    // Commit 2 — add add() (Alice).
    repo.create_file(
        "src/lib.rs",
        "pub fn add(a: i32, b: i32) -> i32 {\n    a + b\n}\n",
    );
    repo.git_add_all();
    repo.git_commit("feat: add add() helper");

    // Commit 3 — add sub() from a different author (John Doe).
    std::process::Command::new("git")
        .args(["config", "user.name", "John Doe"])
        .current_dir(&repo.path)
        .output()
        .unwrap();
    std::process::Command::new("git")
        .args(["config", "user.email", "john@example.com"])
        .current_dir(&repo.path)
        .output()
        .unwrap();
    repo.create_file(
        "src/lib.rs",
        "pub fn add(a: i32, b: i32) -> i32 {\n    a + b\n}\n\n\
         pub fn sub(a: i32, b: i32) -> i32 {\n    a - b\n}\n",
    );
    repo.git_add_all();
    repo.git_commit("feat: add sub() helper");

    // Commit 4 — docs (Alice again).
    std::process::Command::new("git")
        .args(["config", "user.name", "Alice Liddell"])
        .current_dir(&repo.path)
        .output()
        .unwrap();
    std::process::Command::new("git")
        .args(["config", "user.email", "alice@example.com"])
        .current_dir(&repo.path)
        .output()
        .unwrap();
    repo.create_file(
        "README.md",
        "# Fresh demo\n\nA tiny sample project.\n\n\
         Provides basic arithmetic helpers.\n",
    );
    repo.git_add_all();
    repo.git_commit("docs: describe the helpers");

    // Commit 5 — cli args TODO (Alice). Tag v0.1 on this one so the log
    // panel shows a tag ref as well as the HEAD branch ref.
    repo.create_file(
        "src/main.rs",
        "fn main() {\n    println!(\"Hello, world!\");\n}\n\
         // TODO: support CLI args\n",
    );
    repo.git_add_all();
    repo.git_commit("chore(main): note CLI args TODO");
    std::process::Command::new("git")
        .args(["tag", "v0.1.0"])
        .current_dir(&repo.path)
        .output()
        .unwrap();

    // Commit 6 — CLI parser (John again).
    std::process::Command::new("git")
        .args(["config", "user.name", "John Doe"])
        .current_dir(&repo.path)
        .output()
        .unwrap();
    std::process::Command::new("git")
        .args(["config", "user.email", "john@example.com"])
        .current_dir(&repo.path)
        .output()
        .unwrap();
    repo.create_file(
        "src/args.rs",
        "pub fn parse_args(args: &[String]) -> Vec<String> {\n\
         \u{0020}   args.iter().skip(1).cloned().collect()\n}\n",
    );
    repo.git_add_all();
    repo.git_commit("feat(cli): add args parser");

    repo
}

/// Git Log: the modern buffer-group layout with a live-preview right panel.
/// Walk through the commit list with j/k and show the right-hand detail
/// update in step, then focus the detail panel with Tab and close with q.
#[test]
#[ignore]
fn blog_showcase_productivity_git_log() {
    let repo = build_git_log_demo_repo();
    repo.setup_git_log_plugin();

    // 140x34 is comfortable — the log panel's aligned columns + the detail
    // panel's diff read well side-by-side at that width.
    let mut h = EditorTestHarness::with_config_and_working_dir(
        140,
        34,
        Default::default(),
        repo.path.clone(),
    )
    .unwrap();
    h.render().unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.2.26/git-log",
        "Git Log",
        "Magit-style git log with live-preview: a buffer-group tab pairs the aligned commit list with the selected commit's detail, updated as you navigate.",
    );

    // Opening hold on the blank editor — a moment to read the key badge.
    hold(&mut h, &mut s, 5, 120);

    // Open the command palette and type "Git Log".
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 150);

    for ch in "Git Log".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 60);
    }
    hold(&mut h, &mut s, 2, 120);

    // Confirm — this runs `show_git_log` which creates the buffer group.
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    // The group buffer + `git show` dispatch is async; wait for the log
    // panel's commit list AND the detail-panel's `git show` output to
    // actually land (not just the "Loading commit..." placeholder).
    // `diff --git` is the giveaway that git-show's stdout has arrived.
    h.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("feat: initial scaffold") && screen.contains("diff --git")
    })
    .unwrap();
    snap(&mut h, &mut s, Some("Enter"), 300);
    hold(&mut h, &mut s, 5, 120);

    // Walk down the commit list — each `j` fires `cursor_moved`, which
    // re-renders the right panel with the newly-selected commit's diff.
    // Wait for the async `git show` to complete before snapshotting, so
    // the right panel matches the highlighted row.
    for _ in 0..3 {
        h.send_key(KeyCode::Char('j'), KeyModifiers::NONE).unwrap();
        h.wait_until(|h| !h.screen_to_string().contains("Loading commit"))
            .unwrap();
        snap(&mut h, &mut s, Some("j"), 180);
        hold(&mut h, &mut s, 2, 120);
    }

    // Climb back up to highlight live-update going both directions.
    for _ in 0..2 {
        h.send_key(KeyCode::Char('k'), KeyModifiers::NONE).unwrap();
        h.wait_until(|h| !h.screen_to_string().contains("Loading commit"))
            .unwrap();
        snap(&mut h, &mut s, Some("k"), 180);
    }
    hold(&mut h, &mut s, 3, 120);

    // Tab jumps focus into the detail panel so the user can scroll the
    // diff without losing the log's cursor.
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Tab"), 200);
    hold(&mut h, &mut s, 4, 150);

    // q in the detail panel hops back to the log panel (it doesn't close
    // the group until q is pressed from the log panel).
    h.send_key(KeyCode::Char('q'), KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("q"), 180);
    hold(&mut h, &mut s, 3, 120);

    // Final close — q in the log panel tears the group down.
    h.send_key(KeyCode::Char('q'), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| !h.screen_to_string().contains("feat: initial scaffold"))
        .unwrap();
    snap(&mut h, &mut s, Some("q"), 220);
    hold(&mut h, &mut s, 4, 150);

    s.finalize().unwrap();
}

// =========================================================================
// Blog Post 7: Fresh 0.2.26
// =========================================================================

/// Dashboard: opt-in TUI dashboard replaces [No Name] with weather,
/// git status, GitHub PRs, and disk usage. Driven from init.ts.
#[test]
#[ignore]
fn blog_showcase_fresh_0_2_26_dashboard() {
    // A small real git repo gives the GIT section something to draw.
    let repo = GitTestRepo::new();
    repo.create_file("README.md", "# demo\n\nA small project.\n");
    repo.create_file("src/main.rs", "fn main() { println!(\"hi\"); }\n");
    repo.git_add_all();
    repo.git_commit("initial commit");

    // Make the repo's plugins/ dir contain the dashboard plugin so the
    // runtime picks it up. PluginConfig defaults to enabled=true, so
    // the dashboard auto-loads.
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "dashboard");

    // The dashboard opens itself on the `ready` hook (or if buffers
    // exist at load time). The harness skips main()'s boot sequence,
    // so we need a shared dir context for the plugin's cache location
    // and we fire `ready` manually after startup.
    let temp = tempfile::TempDir::new().unwrap();
    let dir_context = fresh::config_io::DirectoryContext::for_testing(temp.path());
    fs::create_dir_all(&dir_context.config_dir).unwrap();

    let mut h = EditorTestHarness::with_shared_dir_context(
        120,
        32,
        Default::default(),
        repo.path.clone(),
        dir_context,
    )
    .unwrap();
    h.editor_mut().fire_ready_hook();
    h.render().unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.2.26/dashboard",
        "Dashboard",
        "Opt-in TUI dashboard replaces the default [No Name] buffer with weather, git status, GitHub PRs, and disk usage.",
    );

    // Wait for the dashboard buffer to swap in and hold a few frames of
    // its initial "loading…" state so readers can see the skeleton.
    h.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("WEATHER") || screen.contains("GIT") || screen.contains("GITHUB")
    })
    .unwrap();
    hold(&mut h, &mut s, 4, 200);

    // Then wait for every section to leave its "loading…" state. Network
    // failures surface as "error" or similar; either way, "loading…"
    // should not still be present.
    h.wait_until(|h| !h.screen_to_string().contains("loading…"))
        .unwrap();
    hold(&mut h, &mut s, 10, 250);

    s.finalize().unwrap();
}

/// init.ts — open the startup script via the command palette and show
/// the auto-generated starter template.
#[test]
#[ignore]
fn blog_showcase_fresh_0_2_26_init_ts() {
    let temp = tempfile::TempDir::new().unwrap();
    let dir_context = fresh::config_io::DirectoryContext::for_testing(temp.path());
    let working_dir = temp.path().join("work");
    fs::create_dir_all(&working_dir).unwrap();

    let mut h = EditorTestHarness::with_shared_dir_context(
        100,
        30,
        Default::default(),
        working_dir,
        dir_context,
    )
    .unwrap();
    h.render().unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.2.26/init-ts",
        "Startup Script (init.ts)",
        "Fresh auto-loads ~/.config/fresh/init.ts. Run init: Edit from the palette to open it with a starter template; init: Reload re-runs without restarting.",
    );

    hold(&mut h, &mut s, 4, 150);

    // Open the command palette and type the query — Ctrl+P fuzzy-searches
    // across both files and commands, so "init: Edit" surfaces the
    // "init: Edit init.ts" command specifically (init: Check and
    // init: Reload are filtered out by typing "Edit").
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 200);

    for ch in "init: Edit".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 70);
    }
    hold(&mut h, &mut s, 3, 150);

    // Accept the match — "init: Edit init.ts" — which creates the
    // starter template in the config dir and opens it in a buffer.
    // Wait for the template content to render (it contains the
    // `getEditor()` reference line).
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("getEditor()"))
        .unwrap();
    snap(&mut h, &mut s, Some("Enter"), 300);
    hold(&mut h, &mut s, 5, 180);

    // Scroll down through the template to show the examples.
    for _ in 0..8 {
        h.send_key(KeyCode::PageDown, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some("PgDn"), 220);
    }
    hold(&mut h, &mut s, 4, 150);

    s.finalize().unwrap();
}

/// Preview tabs: single-click opens an ephemeral tab the next single-click
/// replaces; double-click promotes to permanent.
#[test]
#[ignore]
fn blog_showcase_fresh_0_2_26_preview_tabs() {
    let mut h = EditorTestHarness::with_temp_project(120, 30).unwrap();
    let pd = h.project_dir().unwrap();
    create_demo_project(&pd);
    h.open_file(&pd.join("src/main.rs")).unwrap();
    h.render().unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.2.26/preview-tabs",
        "Preview Tabs",
        "Single-click opens a preview tab that the next single-click replaces; double-click promotes it to a permanent tab.",
    );

    // Open the file explorer so we can click on files.
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+E"), 250);
    hold(&mut h, &mut s, 3, 150);

    // Navigate to the src/ directory and expand it so main.rs and
    // utils.rs are both visible in the tree.
    h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("↓"), 120);
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 180);
    hold(&mut h, &mut s, 2, 150);

    // Tree rows (row 0 is menu bar, row 1 is tab bar):
    //   2: project_root
    //   3: v src
    //   4: main.rs
    //   5: utils.rs
    //   6: Cargo.toml
    //   7: README.md
    // Click at col 4 within each file name.
    let click_col = 4u16;
    let row_utils = 5u16;
    let row_readme = 7u16;

    // Single-click utils.rs — opens as preview tab (replaces main.rs's
    // preview). Tab bar should still show one tab.
    h.mouse_click(click_col, row_utils).unwrap();
    h.render().unwrap();
    snap_mouse(
        &mut h,
        &mut s,
        Some("Click utils.rs"),
        (click_col, row_utils),
        300,
    );
    hold(&mut h, &mut s, 4, 150);

    // Single-click README.md — replaces the utils.rs preview tab.
    h.mouse_click(click_col, row_readme).unwrap();
    h.render().unwrap();
    snap_mouse(
        &mut h,
        &mut s,
        Some("Click README.md"),
        (click_col, row_readme),
        300,
    );
    hold(&mut h, &mut s, 5, 150);

    // Double-click README.md — promotes it to a permanent tab.
    h.mouse_click(click_col, row_readme).unwrap();
    h.mouse_click(click_col, row_readme).unwrap();
    h.render().unwrap();
    snap_mouse(
        &mut h,
        &mut s,
        Some("Double-click"),
        (click_col, row_readme),
        300,
    );
    hold(&mut h, &mut s, 3, 150);

    // Now single-click utils.rs — README.md stays (permanent), utils.rs
    // opens in its own preview tab alongside.
    h.mouse_click(click_col, row_utils).unwrap();
    h.render().unwrap();
    snap_mouse(
        &mut h,
        &mut s,
        Some("Click utils.rs"),
        (click_col, row_utils),
        300,
    );
    hold(&mut h, &mut s, 6, 150);

    s.finalize().unwrap();
}

/// Review Diff rewrite: unified buffer listing files and hunks, `n`/`p`
/// hunk navigation, persistent comments.
#[test]
#[ignore]
fn blog_showcase_fresh_0_2_26_review_diff() {
    let repo = GitTestRepo::new();

    // Seed a repo with a couple of files committed, then create varied
    // working-tree changes so the review buffer has staged, unstaged,
    // and untracked content to show.
    repo.create_file(
        "src/main.rs",
        "fn main() {\n    println!(\"Hello, world!\");\n}\n\n\
         fn greet(name: &str) {\n    println!(\"Hello, {}!\", name);\n}\n",
    );
    repo.create_file(
        "src/lib.rs",
        "pub fn add(a: i32, b: i32) -> i32 {\n    a + b\n}\n\n\
         pub fn sub(a: i32, b: i32) -> i32 {\n    a - b\n}\n",
    );
    repo.create_file("README.md", "# demo\n\nA small project.\n");
    repo.git_add_all();
    repo.git_commit("initial commit");

    // Unstaged change: modify an existing hunk.
    repo.modify_file(
        "src/main.rs",
        "fn main() {\n    println!(\"Hello, Fresh!\");\n}\n\n\
         fn greet(name: &str) {\n    println!(\"Hi there, {}!\", name);\n}\n",
    );
    // Staged change: new helper in lib.rs.
    repo.modify_file(
        "src/lib.rs",
        "pub fn add(a: i32, b: i32) -> i32 {\n    a + b\n}\n\n\
         pub fn sub(a: i32, b: i32) -> i32 {\n    a - b\n}\n\n\
         pub fn mul(a: i32, b: i32) -> i32 {\n    a * b\n}\n",
    );
    repo.stage_file("src/lib.rs");
    // Untracked file.
    repo.create_file("notes.txt", "scratch notes\n- TODO: wire up CLI\n");

    // Install the audit_mode plugin (which provides "Review Diff") into
    // the test repo, so the command shows up in the palette.
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "audit_mode");

    let mut h = EditorTestHarness::with_config_and_working_dir(
        140,
        34,
        Default::default(),
        repo.path.clone(),
    )
    .unwrap();
    h.render().unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.2.26/review-diff",
        "Review Diff Rewrite",
        "Files and hunks in one scrollable unified buffer. n/p jump between hunks; stage, unstage, or discard on the cursor row.",
    );

    hold(&mut h, &mut s, 4, 150);

    // Open Review Diff via the command palette.
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 180);

    for ch in "Review Diff".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 60);
    }
    hold(&mut h, &mut s, 2, 150);

    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    // Wait for the review buffer to populate.
    h.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("src/main.rs") || screen.contains("Unstaged") || screen.contains("@@")
    })
    .unwrap();
    snap(&mut h, &mut s, Some("Enter"), 300);
    hold(&mut h, &mut s, 6, 150);

    // Hop through hunks with `n`.
    for _ in 0..3 {
        h.send_key(KeyCode::Char('n'), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some("n"), 220);
        hold(&mut h, &mut s, 2, 150);
    }

    // Back up with `p`.
    for _ in 0..2 {
        h.send_key(KeyCode::Char('p'), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some("p"), 220);
    }
    hold(&mut h, &mut s, 5, 150);

    s.finalize().unwrap();
}

// =========================================================================
// Blog Post: Orchestrator — New SSH Session (0.3.10)
// =========================================================================
//
// Showcases the SSH backend of the Orchestrator's "New Session" dialog
// against a *real* connection: a throwaway, non-root `sshd` on
// `127.0.0.1`, reached through a fake hostname (`demo-box`) that resolves
// via `/etc/hosts`. The dialog points the SSH backend at `demo-box:<port>`
// and Fresh attaches the full remote-agent stack (filesystem + terminal +
// LSP) over the connection — the same path a user gets typing a host into
// the form.
//
// The connection is genuine: ssh bootstraps the Python agent on the remote
// (here, loopback) exactly as in production, so the "Connecting…" → live
// session transition in the GIF is real, not staged.
//
// Linux-only and self-skipping: it needs `ssh`/`sshd`/`ssh-keygen` and a
// `demo-box → 127.0.0.1` resolution. When OpenSSH is missing, or the fake
// host can't be made to resolve (no write access to `/etc/hosts` and no
// pre-existing entry), the test prints why and returns — a no-op rather
// than a failure, matching the other environment-gated showcases.
#[cfg(all(target_os = "linux", feature = "plugins"))]
mod ssh_session_showcase_support {
    use std::net::{TcpListener, TcpStream};
    use std::os::unix::fs::PermissionsExt;
    use std::path::{Path, PathBuf};
    use std::process::{Child, Command, Stdio};
    use std::time::{Duration, Instant};

    /// The fake hostname the GIF connects to. Resolves to loopback via
    /// `/etc/hosts` so the demo reads like a real remote box.
    pub const DEMO_HOST: &str = "demo-box";

    /// Resolve a program by searching `PATH`, then a few well-known absolute
    /// fallbacks (`sshd` usually lives in `/usr/sbin`, often absent from a
    /// test process's `PATH`).
    pub fn resolve(name: &str, fallbacks: &[&str]) -> Option<PathBuf> {
        if let Some(path) = std::env::var_os("PATH") {
            for dir in std::env::split_paths(&path) {
                let cand = dir.join(name);
                if cand.is_file() {
                    return Some(cand);
                }
            }
        }
        fallbacks
            .iter()
            .map(PathBuf::from)
            .find(|cand| cand.is_file())
    }

    fn keygen(keygen_bin: &Path, path: &Path) {
        let status = Command::new(keygen_bin)
            .args(["-t", "ed25519", "-q", "-N", ""])
            .arg("-f")
            .arg(path)
            .status()
            .expect("run ssh-keygen");
        assert!(status.success(), "ssh-keygen failed for {path:?}");
    }

    fn set_mode(path: &Path, mode: u32) {
        std::fs::set_permissions(path, std::fs::Permissions::from_mode(mode)).unwrap();
    }

    /// A free localhost TCP port. Bound briefly to discover the number, then
    /// released for `sshd` to claim.
    fn free_port() -> u16 {
        TcpListener::bind("127.0.0.1:0")
            .expect("bind ephemeral port")
            .local_addr()
            .unwrap()
            .port()
    }

    fn wait_for_listen(port: u16, timeout: Duration) -> bool {
        let start = Instant::now();
        while start.elapsed() < timeout {
            if TcpStream::connect(("127.0.0.1", port)).is_ok() {
                return true;
            }
            std::thread::sleep(Duration::from_millis(50));
        }
        false
    }

    /// Kill the child `sshd` when the test ends (pass or panic).
    pub struct KillOnDrop(pub Child);
    impl Drop for KillOnDrop {
        fn drop(&mut self) {
            let _ = self.0.kill();
            let _ = self.0.wait();
        }
    }

    /// Ensure `demo-box` resolves to `127.0.0.1`. Returns true when the name
    /// resolves (either it already did, or we appended it to `/etc/hosts`).
    /// Never mutates an existing mapping; only appends when the name is
    /// entirely absent and `/etc/hosts` is writable.
    pub fn ensure_demo_host_resolves() -> bool {
        if TcpStream::connect((DEMO_HOST, 9)).is_ok() {
            return true; // resolves (connection refused still means it resolved)
        }
        // `connect` failing could be resolution OR a refused port; the only
        // failure that matters here is name resolution. Probe that directly by
        // checking whether the entry is already in /etc/hosts, and add it if
        // not. Use a getent-style read of the file rather than libc so the
        // check matches what we (might) write.
        let hosts = std::fs::read_to_string("/etc/hosts").unwrap_or_default();
        let already = hosts
            .lines()
            .filter(|l| !l.trim_start().starts_with('#'))
            .any(|l| l.split_whitespace().skip(1).any(|h| h == DEMO_HOST));
        if already {
            return true;
        }
        // Not present — append if we can.
        use std::io::Write;
        match std::fs::OpenOptions::new().append(true).open("/etc/hosts") {
            Ok(mut f) => {
                let _ = writeln!(f, "127.0.0.1 {DEMO_HOST}");
                true
            }
            Err(_) => false,
        }
    }

    /// A running throwaway sshd plus the bits a client needs to reach it.
    pub struct DemoSshd {
        pub port: u16,
        pub identity: PathBuf,
        pub known_hosts: PathBuf,
        pub work: PathBuf,
        _guard: KillOnDrop,
        _root: PathBuf,
    }

    /// Spin up a non-root `sshd` on `127.0.0.1` rooted under `root`, with a
    /// fresh ed25519 keypair authorized for the current user. Returns `None`
    /// if the daemon never came up.
    pub fn spawn_demo_sshd(sshd: &Path, ssh_keygen: &Path, root: &Path) -> Option<DemoSshd> {
        let _ = std::fs::remove_dir_all(root);
        std::fs::create_dir_all(root).unwrap();
        let work = root.join("work");
        std::fs::create_dir_all(&work).unwrap();
        // A small workspace so the remote file finder / explorer land somewhere
        // alive — including a syntax-highlightable source file to open over SSH.
        std::fs::write(
            work.join("README.md"),
            "# demo-box\n\nRemote workspace served over SSH.\n",
        )
        .unwrap();
        std::fs::write(work.join("deploy.sh"), "#!/bin/sh\necho deploying…\n").unwrap();
        std::fs::write(
            work.join("app.py"),
            "\"\"\"demo-box deployment service.\"\"\"\n\
             import os\n\
             \n\
             \n\
             def deploy(target: str) -> bool:\n\
             \x20   print(f\"deploying to {target}…\")\n\
             \x20   return os.path.exists(target)\n\
             \n\
             \n\
             if __name__ == \"__main__\":\n\
             \x20   deploy(\"/srv/www\")\n",
        )
        .unwrap();

        let hostkey = root.join("hostkey");
        let id = root.join("id");
        let authorized = root.join("authorized_keys");
        let config = root.join("sshd_config");
        let known_hosts = root.join("known_hosts");
        std::fs::write(&known_hosts, "").unwrap();

        keygen(ssh_keygen, &hostkey);
        keygen(ssh_keygen, &id);
        std::fs::copy(root.join("id.pub"), &authorized).unwrap();
        set_mode(&authorized, 0o600);

        let port = free_port();
        std::fs::write(
            &config,
            format!(
                "Port {port}\n\
                 ListenAddress 127.0.0.1\n\
                 HostKey {hostkey}\n\
                 PidFile {pid}\n\
                 AuthorizedKeysFile {authorized}\n\
                 StrictModes no\n\
                 UsePAM no\n\
                 PasswordAuthentication no\n\
                 PubkeyAuthentication yes\n",
                hostkey = hostkey.display(),
                pid = root.join("sshd.pid").display(),
                authorized = authorized.display(),
            ),
        )
        .unwrap();

        let log = root.join("sshd.log");
        let logf = std::fs::File::create(&log).unwrap();
        let child = Command::new(sshd)
            .arg("-D") // foreground
            .arg("-e") // log to stderr
            .arg("-f")
            .arg(&config)
            .stdout(Stdio::from(logf.try_clone().unwrap()))
            .stderr(Stdio::from(logf))
            .spawn()
            .ok()?;
        let guard = KillOnDrop(child);

        if !wait_for_listen(port, Duration::from_secs(10)) {
            eprintln!(
                "demo sshd never listened on {port}.\nsshd log:\n{}",
                std::fs::read_to_string(&log).unwrap_or_default()
            );
            return None;
        }

        Some(DemoSshd {
            port,
            identity: id,
            known_hosts,
            work,
            _guard: guard,
            _root: root.to_path_buf(),
        })
    }
}

/// Orchestrator: New SSH Session — open the New Session dialog, pick the
/// SSH backend, point it at `demo-box:<port>` (a fake hostname resolving to
/// a local user-space sshd via `/etc/hosts`), and watch Fresh attach the
/// remote session for real.
#[cfg(all(target_os = "linux", feature = "plugins"))]
#[test]
#[ignore]
fn blog_showcase_fresh_0_4_0_ssh_session() {
    use ssh_session_showcase_support as sup;

    // --- Environment gates: skip (not fail) when prerequisites are absent. --
    let (Some(sshd), Some(ssh_keygen)) = (
        sup::resolve(
            "sshd",
            &["/usr/sbin/sshd", "/sbin/sshd", "/usr/local/sbin/sshd"],
        ),
        sup::resolve("ssh-keygen", &[]),
    ) else {
        eprintln!("Skipping SSH-session showcase: sshd/ssh-keygen not installed");
        return;
    };
    if sup::resolve("ssh", &[]).is_none() {
        eprintln!("Skipping SSH-session showcase: ssh client not installed");
        return;
    }
    if !sup::ensure_demo_host_resolves() {
        eprintln!(
            "Skipping SSH-session showcase: '{}' does not resolve and /etc/hosts \
             is not writable. Add `127.0.0.1 {}` to /etc/hosts to enable.",
            sup::DEMO_HOST,
            sup::DEMO_HOST,
        );
        return;
    }

    // --- Bring up the throwaway remote (loopback) sshd. ---------------------
    let demo_root = std::path::PathBuf::from("/tmp/fresh-ssh-showcase");
    let Some(server) = sup::spawn_demo_sshd(&sshd, &ssh_keygen, &demo_root) else {
        eprintln!("Skipping SSH-session showcase: demo sshd failed to start");
        return;
    };

    // --- Local workspace (the launch session) + orchestrator plugin. --------
    // A small Rust project so the local session has a real file open — the one
    // we switch back to from the dock at the end.
    fresh::i18n::set_locale("en");
    let workspace = demo_root.join("local-app");
    std::fs::create_dir_all(workspace.join("src")).unwrap();
    std::fs::write(
        workspace.join("src/main.rs"),
        "// local-app — runs on this machine.\n\
         use std::collections::HashMap;\n\
         \n\
         fn main() {\n\
         \x20   let mut env: HashMap<&str, &str> = HashMap::new();\n\
         \x20   env.insert(\"region\", \"local\");\n\
         \x20   for (k, v) in &env {\n\
         \x20       println!(\"{k} = {v}\");\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();
    std::fs::write(
        workspace.join("Cargo.toml"),
        "[package]\nname = \"local-app\"\nversion = \"0.1.0\"\nedition = \"2021\"\n",
    )
    .unwrap();
    let plugins_dir = workspace.join("plugins");
    std::fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");

    let mut h = EditorTestHarness::with_working_dir(120, 34, workspace.clone()).unwrap();
    h.tick_and_render().unwrap();
    // Wait for the orchestrator to register its command.
    h.wait_until(|h| {
        let reg = h.editor().command_registry().read().unwrap();
        reg.get_all()
            .iter()
            .any(|c| c.get_localized_name() == "Orchestrator: New Session")
    })
    .unwrap();
    // Open the local file so the launch session has content to show.
    h.open_file(&workspace.join("src/main.rs")).unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.4.0/ssh-session",
        "New SSH Session",
        "Start a remote SSH session from the Orchestrator's New Session dialog: \
         pick the SSH backend and point it at a host. Fresh attaches its \
         filesystem, terminal, and LSP over the connection, so you can open \
         remote files in buffers — then hop back to a local session through \
         the dock.",
    );

    // Open on the local session: a local Rust file, "Local" in the status bar.
    hold(&mut h, &mut s, 5, 160);

    // --- Open the New Session dialog via the command palette. ---------------
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 160);

    for ch in "New Session".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 55);
    }
    h.wait_until(|h| h.screen_to_string().contains("Orchestrator: New Session"))
        .unwrap();
    hold(&mut h, &mut s, 2, 120);

    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("ORCHESTRATOR :: New Session"))
        .unwrap();
    snap(&mut h, &mut s, Some("Enter"), 220);
    hold(&mut h, &mut s, 4, 150);

    // --- Switch to the SSH backend by clicking the "Run in: … SSH" tab. -----
    let (ssh_col, ssh_row) = h
        .find_text_on_screen("SSH")
        .expect("the 'Run in:' tab row should offer an SSH backend");
    snap_mouse(&mut h, &mut s, None, (ssh_col, ssh_row), 200);
    h.mouse_click(ssh_col, ssh_row).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Remote Path"))
        .unwrap();
    snap_mouse(&mut h, &mut s, Some("Click"), (ssh_col, ssh_row), 180);
    hold(&mut h, &mut s, 2, 110);

    // Enter on the already-active SSH tab dives into the first field (Host).
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Enter"), 130);

    // --- Host: the fake hostname + the throwaway sshd's port. ---------------
    let host_value = format!("{}:{}", sup::DEMO_HOST, server.port);
    for ch in host_value.chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 38);
    }
    hold(&mut h, &mut s, 2, 110);

    // --- The remaining fields fill in quickly (they're the "plumbing"; the
    //     host is the star). Each lands its whole value in one go. -----------
    // Remote Path: where the session is rooted on the remote.
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Tab"), 90);
    h.type_text(&server.work.to_string_lossy()).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, None, 120);

    // Identity file: the keypair authorized on the demo sshd.
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Tab"), 90);
    h.type_text(&server.identity.to_string_lossy()).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, None, 120);

    // SSH options: a throwaway known_hosts so the demo leaves no trace in the
    // user's ~/.ssh (and to show the free-form options field).
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Tab"), 90);
    h.type_text(&format!(
        "-o UserKnownHostsFile={}",
        server.known_hosts.to_string_lossy()
    ))
    .unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, None, 120);

    // Session name.
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Tab"), 90);
    h.type_text("deploy-box").unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, None, 150);
    hold(&mut h, &mut s, 2, 120);

    // --- Submit: click "Create Session". ------------------------------------
    let (create_col, create_row) = h
        .find_text_on_screen("Create Session")
        .expect("the form should offer a 'Create Session' button");
    snap_mouse(&mut h, &mut s, None, (create_col, create_row), 160);
    h.mouse_click(create_col, create_row).unwrap();
    h.render().unwrap();
    // The disabled "Connecting…" view flashes up while ssh handshakes + the
    // Python agent boots on the remote — kept brief so the connect feels snappy.
    snap_mouse(&mut h, &mut s, Some("Click"), (create_col, create_row), 150);
    hold(&mut h, &mut s, 2, 110);

    // --- Wait for the attach to resolve. On success the form panel is
    //     unmounted (its header disappears) and the born-attached remote
    //     window takes over; on failure the form stays up with an inline
    //     Error. Keying on the header (not the "Connecting…" glyph) keeps the
    //     wait robust. -------------------------------------------------------
    h.wait_until(|h| {
        let screen = h.screen_to_string();
        !screen.contains("ORCHESTRATOR :: New Session") || screen.contains("Error:")
    })
    .unwrap();
    let screen = h.screen_to_string();
    assert!(
        !screen.contains("Error:"),
        "SSH attach failed — the New Session dialog surfaced an error.\nScreen:\n{screen}",
    );

    // The born-attached remote window is now live, with an integrated terminal
    // running the remote login shell. Wait for the remote prompt to paint so
    // the GIF opens on a settled session.
    h.wait_until(|h| {
        h.screen_to_string()
            .contains(&server.work.to_string_lossy().into_owned())
    })
    .unwrap();
    hold(&mut h, &mut s, 3, 160);

    // --- Open a remote file in a buffer. Quick Open (Ctrl+P → Backspace to
    //     drop the `>` command prefix) fuzzy-finds files through the SSH
    //     window's authority, so typing `app` surfaces the remote `app.py`. --
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 200);
    h.send_key(KeyCode::Backspace, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
    snap(&mut h, &mut s, Some("Bksp"), 150);
    for ch in "app".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 90);
    }
    // The remote file finder lists `app.py` (served over SSH).
    h.wait_until(|h| h.screen_to_string().contains("app.py"))
        .unwrap();
    hold(&mut h, &mut s, 2, 150);
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    // Wait for the remote file's contents to render in the buffer.
    h.wait_until(|h| h.screen_to_string().contains("deployment service"))
        .unwrap();
    snap(&mut h, &mut s, Some("Enter"), 280);
    hold(&mut h, &mut s, 7, 200);

    // --- Show the persistent Orchestrator dock: the left column lists every
    //     session — the local launch session and the remote `deploy-box`. ----
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Toggle Dock").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Toggle Dock"))
        .unwrap();
    snap(&mut h, &mut s, Some("Toggle Dock"), 220);
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    // The dock mounts and takes keyboard focus (polling avoids the focus race).
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("deploy-box") && s.contains("local-app") && h.editor().is_dock_focused()
    })
    .unwrap();
    snap(&mut h, &mut s, Some("Enter"), 260);
    hold(&mut h, &mut s, 6, 200);

    // --- Switch to the local session: ↑ moves the dock selection off the
    //     active remote session onto the local one, live-swapping the editor
    //     back to the local Rust file. --------------------------------------
    h.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| h.screen_to_string().contains("runs on this machine"))
        .unwrap();
    snap(&mut h, &mut s, Some("↑"), 280);
    hold(&mut h, &mut s, 10, 200);

    s.finalize().unwrap();

    // Keep the server alive until all frames are captured.
    drop(server);
}

// =========================================================================
// Blog Post: Universal Search (0.4.0)
// =========================================================================

/// Universal Search — Live Grep grown into a multi-scope search overlay:
/// project files (git-grep), open buffers, and terminal scrollback, with
/// Word/Regex modes, a clickable scope toolbar, and a live syntax-highlighted
/// preview pane. Drives the overlay against a real git repo so the git-grep
/// provider is selected and only tracked source files surface (the plugin
/// dir is left untracked, so it never clutters the results).
#[cfg(feature = "plugins")]
#[test]
#[ignore]
fn blog_showcase_fresh_0_4_0_universal_search() {
    let git_ok = std::process::Command::new("git")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !git_ok {
        eprintln!("Skipping universal-search showcase: git not installed");
        return;
    }

    fresh::i18n::set_locale("en");
    let temp = tempfile::TempDir::new().unwrap();
    let root = temp.path().canonicalize().unwrap().join("proj");
    std::fs::create_dir_all(root.join("src")).unwrap();

    // A small, readable project. `config` (lowercase) recurs across all three
    // source files, so a single query lands matches in each and the preview
    // pane hops between files as we navigate.
    std::fs::write(
        root.join("src/server.rs"),
        "// HTTP server entry point.\n\
         pub fn start_server(port: u16) {\n\
         \x20   let config = load_config();\n\
         \x20   println!(\"server listening on {port}\");\n\
         \x20   handle_requests(&config);\n\
         }\n\
         \n\
         fn load_config() -> Config {\n\
         \x20   Config::default()\n\
         }\n",
    )
    .unwrap();
    std::fs::write(
        root.join("src/config.rs"),
        "pub struct Config {\n\
         \x20   pub timeout: u32,\n\
         }\n\
         \n\
         impl Config {\n\
         \x20   pub fn default() -> Self {\n\
         \x20       Config { timeout: 30 }\n\
         \x20   }\n\
         \n\
         \x20   pub fn validate_config(&self) -> bool {\n\
         \x20       self.timeout > 0\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();
    std::fs::write(
        root.join("src/handlers.rs"),
        "use crate::config::Config;\n\
         \n\
         pub fn handle_requests(config: &Config) {\n\
         \x20   for _ in 0..config.timeout {\n\
         \x20       route_request();\n\
         \x20   }\n\
         }\n\
         \n\
         fn route_request() {}\n",
    )
    .unwrap();
    std::fs::write(
        root.join("README.md"),
        "# proj\n\nRun the server with `cargo run`. config lives in src/config.rs.\n",
    )
    .unwrap();

    let run_git = |args: &[&str]| {
        let out = git_command(&root).args(args).output().unwrap();
        assert!(
            out.status.success(),
            "git {:?} failed: {}",
            args,
            String::from_utf8_lossy(&out.stderr)
        );
    };
    run_git(&["init", "--quiet", "-b", "main"]);
    run_git(&[
        "add",
        "src/server.rs",
        "src/config.rs",
        "src/handlers.rs",
        "README.md",
    ]);
    run_git(&["commit", "--quiet", "-m", "seed"]);

    // Plugins are copied *after* the commit and never staged, so git-grep
    // (which only reports tracked files) never surfaces plugin sources.
    let plugins_dir = root.join("plugins");
    std::fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "live_grep");

    // 140 cols so the overlay paints its results + live preview split.
    let mut h =
        EditorTestHarness::with_config_and_working_dir(140, 34, Default::default(), root.clone())
            .unwrap();
    h.open_file(&root.join("src/server.rs")).unwrap();
    h.render().unwrap();
    h.wait_until(|h| {
        let reg = h.editor().command_registry().read().unwrap();
        reg.get_all()
            .iter()
            .any(|c| c.get_localized_name().starts_with("Live Grep"))
    })
    .unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.4.0/universal-search",
        "Universal Search",
        "Live Grep grew into a multi-scope search overlay — project files, open \
         buffers, and terminal scrollback — with Word/Regex modes, a clickable \
         scope toolbar, and a live syntax-highlighted preview.",
    );

    hold(&mut h, &mut s, 5, 160);

    // --- Open the Live Grep overlay from the palette. -----------------------
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    snap(&mut h, &mut s, Some("Ctrl+P"), 160);
    h.type_text("Live Grep").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Live Grep (Find in Files)"))
        .unwrap();
    snap(&mut h, &mut s, None, 160);
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    // The overlay's scope toolbar paints "Search in:".
    h.wait_until(|h| h.screen_to_string().contains("Search in:"))
        .unwrap();
    snap(&mut h, &mut s, Some("Enter"), 220);
    hold(&mut h, &mut s, 3, 150);

    // --- Type a query that recurs across every source file. -----------------
    for ch in "config".chars() {
        h.send_key(KeyCode::Char(ch), KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some(&ch.to_string()), 70);
    }
    // Results stream in; the preview pane shows the first match's file.
    h.wait_until(|h| h.screen_to_string().contains("src/config.rs"))
        .unwrap();
    hold(&mut h, &mut s, 5, 180);

    // --- Word vs. substring: Alt+O flips whole-word matching. Wait for the
    //     re-search to settle so we paint the new (smaller) result set rather
    //     than the transient "Searching…". `validate_config` drops out — its
    //     `config` isn't a whole word. ----------------------------------------
    h.send_key(KeyCode::Char('o'), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| {
        let scr = h.screen_to_string();
        scr.contains("src/server.rs") && !scr.contains("Searching")
    })
    .unwrap();
    snap(&mut h, &mut s, Some("Alt+O"), 280);
    hold(&mut h, &mut s, 5, 200);

    // --- Navigate results: the preview pane hops between files as the
    //     selection moves down onto a source match. --------------------------
    for _ in 0..3 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        h.render().unwrap();
        snap(&mut h, &mut s, Some("↓"), 260);
        hold(&mut h, &mut s, 2, 150);
    }

    // --- Enter jumps to the highlighted match: the overlay closes and the
    //     file opens at that line (a "resume search" hint confirms the jump). -
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| {
        let scr = h.screen_to_string().to_lowercase();
        !scr.contains("search in:") && scr.contains("resume")
    })
    .unwrap();
    snap(&mut h, &mut s, Some("Enter"), 320);
    hold(&mut h, &mut s, 8, 200);

    s.finalize().unwrap();
}

// =========================================================================
// Blog Post: The Orchestrator Dock (0.4.0)
// =========================================================================

/// Orchestrator Dock — the persistent, non-modal left-column session
/// switcher. Sets up three project sessions (each with its own file open),
/// toggles the dock, and live-switches between them with the arrow keys so
/// the editor on the right swaps to each session's buffer.
#[cfg(feature = "plugins")]
#[test]
#[ignore]
fn blog_showcase_fresh_0_4_0_orchestrator_dock() {
    let git_ok = std::process::Command::new("git")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    if !git_ok {
        eprintln!("Skipping orchestrator-dock showcase: git not installed");
        return;
    }

    fresh::i18n::set_locale("en");
    let temp = tempfile::TempDir::new().unwrap();
    let base = temp.path().canonicalize().unwrap();

    // Three projects in three languages, each a git repo (so the dock shows a
    // branch) with a distinctive file. The differing syntax highlighting makes
    // each live-switch unmistakable.
    let mk_repo = |name: &str, rel: &str, content: &str| -> std::path::PathBuf {
        let root = base.join(name);
        std::fs::create_dir_all(root.join(rel).parent().unwrap()).unwrap();
        std::fs::write(root.join(rel), content).unwrap();
        let run = |args: &[&str]| {
            let out = git_command(&root).args(args).output().unwrap();
            assert!(
                out.status.success(),
                "git {:?}: {}",
                args,
                String::from_utf8_lossy(&out.stderr)
            );
        };
        run(&["init", "--quiet", "-b", "main"]);
        run(&["add", "-A"]);
        run(&["commit", "--quiet", "-m", "init"]);
        root
    };

    let api_root = mk_repo(
        "api",
        "src/main.rs",
        "// api gateway — routes requests to services.\n\
         fn main() {\n\
         \x20   let router = Router::new();\n\
         \x20   router.listen(8080);\n\
         }\n",
    );
    let web_root = mk_repo(
        "web",
        "src/app.ts",
        "// web dashboard front-end.\n\
         export function App(): View {\n\
         \x20   const data = useStore();\n\
         \x20   return render(data);\n\
         }\n",
    );
    let infra_root = mk_repo(
        "infra",
        "deploy.yaml",
        "# infra: production deployment.\n\
         kind: Deployment\n\
         metadata:\n\
         \x20 name: api\n\
         spec:\n\
         \x20 replicas: 3\n",
    );

    // The orchestrator plugin lives in the launch project's plugins dir.
    let plugins_dir = api_root.join("plugins");
    std::fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "orchestrator");

    // Animations off so the dock's live-switch slide doesn't freeze
    // mid-transition under the test clock (it would capture a half-slid,
    // doubled frame). The switch is instant and clean instead.
    let mut cfg = fresh::config::Config::default();
    cfg.editor.animations = false;
    cfg.editor.cursor_jump_animation = false;
    let mut h =
        EditorTestHarness::with_config_and_working_dir(130, 34, cfg, api_root.clone()).unwrap();
    h.tick_and_render().unwrap();
    h.wait_until(|h| {
        let reg = h.editor().command_registry().read().unwrap();
        reg.get_all()
            .iter()
            .any(|c| c.get_localized_name() == "Orchestrator: Toggle Dock")
    })
    .unwrap();

    // Launch session: open its file. This is the window we start (and end) on.
    let api_win = h.editor().active_window_id();
    h.open_file(&api_root.join("src/main.rs")).unwrap();

    // Two more sessions, each rooted in its project with its own file open.
    let web_win = h
        .editor_mut()
        .create_window_at(web_root.clone(), "web".to_string());
    h.editor_mut().set_active_window(web_win);
    h.open_file(&web_root.join("src/app.ts")).unwrap();

    let infra_win = h
        .editor_mut()
        .create_window_at(infra_root.clone(), "infra".to_string());
    h.editor_mut().set_active_window(infra_win);
    h.open_file(&infra_root.join("deploy.yaml")).unwrap();

    // Back to the launch (api) session to start the tour.
    h.editor_mut().set_active_window(api_win);
    h.tick_and_render().unwrap();

    let mut s = BlogShowcase::new(
        "fresh-0.4.0/orchestrator-dock",
        "The Orchestrator Dock",
        "A persistent, non-modal left-column session switcher: every session in \
         one process, each row showing project, branch, and status. The arrow \
         keys live-switch the active session.",
    );

    // Open on the api session (Rust).
    hold(&mut h, &mut s, 5, 160);

    // --- Toggle the dock open. ----------------------------------------------
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Toggle Dock").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Toggle Dock"))
        .unwrap();
    snap(&mut h, &mut s, Some("Toggle Dock"), 220);
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    // Dock mounts, lists all three sessions, and takes keyboard focus.
    h.wait_until(|h| {
        let scr = h.screen_to_string();
        scr.contains("api")
            && scr.contains("web")
            && scr.contains("infra")
            && h.editor().is_dock_focused()
    })
    .unwrap();
    snap(&mut h, &mut s, Some("Enter"), 280);
    hold(&mut h, &mut s, 6, 200);

    // --- Live-switch with the arrows. Each press flips the active session;
    //     the editor on the right swaps to that session's buffer. Detect the
    //     switch by the active window's root, which is order-independent. ----
    let switch = |h: &mut EditorTestHarness, s: &mut BlogShowcase, key: KeyCode, label: &str| {
        let prev = h.editor().active_window().root.clone();
        h.send_key(key, KeyModifiers::NONE).unwrap();
        h.wait_until(|h| h.editor().active_window().root != prev)
            .unwrap();
        snap(h, s, Some(label), 320);
        hold(h, s, 6, 200);
    };

    switch(&mut h, &mut s, KeyCode::Down, "↓");
    switch(&mut h, &mut s, KeyCode::Down, "↓");
    switch(&mut h, &mut s, KeyCode::Up, "↑");

    hold(&mut h, &mut s, 4, 200);

    s.finalize().unwrap();
}
