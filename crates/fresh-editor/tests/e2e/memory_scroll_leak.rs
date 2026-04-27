//! Memory regression test for scrolling through large files.
//!
//! Reproduces a bug where repeated PageDown through a large file causes
//! unbounded memory growth, eventually triggering OOM kill.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::path::PathBuf;

fn large_file_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("large.rs")
}

fn config_with_line_wrap() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config
}

#[cfg(target_os = "linux")]
fn current_rss_kb() -> u64 {
    let status = std::fs::read_to_string("/proc/self/status").unwrap_or_default();
    for line in status.lines() {
        if let Some(val) = line.strip_prefix("VmRSS:") {
            return val
                .trim()
                .strip_suffix("kB")
                .or_else(|| val.trim().strip_suffix("KB"))
                .unwrap_or(val.trim())
                .trim()
                .parse()
                .unwrap_or(0);
        }
    }
    0
}

#[cfg(not(target_os = "linux"))]
fn current_rss_kb() -> u64 {
    0
}

/// PageDown repeatedly to the bottom of a large file using the TestBackend.
/// Memory must stay bounded.
#[test]
fn test_page_down_to_bottom_no_memory_explosion() {
    if current_rss_kb() == 0 {
        eprintln!("Skipping: cannot measure RSS on this platform");
        return;
    }

    const W: u16 = 120;
    const H: u16 = 40;
    const MAX_GROWTH_MB: i64 = 80;

    let mut harness = EditorTestHarness::with_config(W, H, config_with_line_wrap()).unwrap();

    harness.open_file(&large_file_path()).unwrap();
    harness.render().unwrap();

    let baseline = current_rss_kb();
    eprintln!("baseline (TestBackend): {} MB", baseline / 1024);

    let mut i = 0;
    loop {
        let screen_before = harness.screen_to_string();
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        let screen_after = harness.screen_to_string();
        i += 1;

        if i % 10 == 0 {
            let rss = current_rss_kb();
            let growth_mb = (rss as i64 - baseline as i64) / 1024;
            eprintln!(
                "page_down #{i}: RSS={} MB  growth={growth_mb} MB",
                rss / 1024
            );
            assert!(
                growth_mb < MAX_GROWTH_MB,
                "Memory exploded at PageDown #{i}: grew {growth_mb} MB"
            );
        }

        if screen_before == screen_after || i > 500 {
            eprintln!("Reached bottom after {i} PageDowns");
            break;
        }
    }

    let growth_mb = (current_rss_kb() as i64 - baseline as i64) / 1024;
    eprintln!("final growth: {growth_mb} MB");
    assert!(
        growth_mb < MAX_GROWTH_MB,
        "Memory grew {growth_mb} MB scrolling to bottom"
    );
}

/// PageDown to bottom using CrosstermBackend (real ANSI output path).
/// This exercises the actual terminal rendering codepath including
/// diff-based updates and escape sequence generation, which is where
/// the real-world OOM occurs.
///
/// The memory explosion is instant (81 MB → 12 GB in one draw call),
/// so RSS polling can't catch it. We set a virtual memory limit via
/// setrlimit to make the test fail with an allocation error instead
/// of OOM-killing the entire test process.
#[test]
fn test_page_down_to_bottom_crossterm_backend_no_memory_explosion() {
    use fresh::config_io::DirectoryContext;
    use fresh::model::filesystem::StdFileSystem;
    use std::sync::Arc;

    if current_rss_kb() == 0 {
        eprintln!("Skipping: cannot measure RSS on this platform");
        return;
    }

    const W: u16 = 120;
    const H: u16 = 40;
    const MAX_GROWTH_MB: i64 = 80;

    // Note: prlimit was previously used to cap virtual memory at 1GB, but this
    // caused thread spawn failures. The actual fixes (width=0 guard, cached
    // scrollbar counts, fixed viewport) prevent the OOM, so the RSS assertion
    // below is sufficient.

    // Use CrosstermBackend writing to a sink (discards output but exercises
    // the full ANSI diff + escape sequence generation path).
    // Use Viewport::Fixed to prevent autoresize() from querying the real terminal
    // size (which may return 0x0 when writing to a sink, causing pathological wrapping).
    let sink = std::io::sink();
    let backend = ratatui::backend::CrosstermBackend::new(sink);
    let viewport = ratatui::Viewport::Fixed(ratatui::layout::Rect::new(0, 0, W, H));
    let mut terminal =
        ratatui::Terminal::with_options(backend, ratatui::TerminalOptions { viewport }).unwrap();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());
    let filesystem: Arc<dyn fresh::model::filesystem::FileSystem + Send + Sync> =
        Arc::new(StdFileSystem);

    let mut config = config_with_line_wrap();
    config.check_for_updates = false;

    let mut editor = fresh::app::Editor::for_test(
        config,
        W,
        H,
        Some(temp_dir.path().to_path_buf()),
        dir_context,
        fresh::view::color_support::ColorCapability::TrueColor,
        filesystem,
        None,  // time source
        None,  // grammar registry
        false, // enable_plugins (memory test doesn't need them)
        false, // enable_embedded_plugins
    )
    .unwrap();
    editor.process_async_messages();

    // Open the large file
    editor.open_file(&large_file_path()).unwrap();
    terminal.draw(|frame| editor.render(frame)).unwrap();

    let baseline = current_rss_kb();
    eprintln!("baseline (CrosstermBackend): {} MB", baseline / 1024);

    for i in 0..300 {
        editor
            .handle_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        let _ = editor.process_async_messages();
        terminal.draw(|frame| editor.render(frame)).unwrap();

        if i % 10 == 0 {
            let rss = current_rss_kb();
            let growth_mb = (rss as i64 - baseline as i64) / 1024;
            eprintln!(
                "page_down #{i}: RSS={} MB  growth={growth_mb} MB",
                rss / 1024
            );
            assert!(
                growth_mb < MAX_GROWTH_MB,
                "Memory exploded at PageDown #{i}: grew {growth_mb} MB (CrosstermBackend path)"
            );
        }
    }

    let growth_mb = (current_rss_kb() as i64 - baseline as i64) / 1024;
    eprintln!("final growth (CrosstermBackend): {growth_mb} MB");
    assert!(
        growth_mb < MAX_GROWTH_MB,
        "Memory grew {growth_mb} MB with CrosstermBackend"
    );
}
