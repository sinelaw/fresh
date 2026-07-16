//! Switching to a window whose file-explorer sidebar is open must not paint
//! the surrounding page in two stages. The explorer's tree is built
//! asynchronously (uniformly — local and remote filesystems take the same
//! path), so the guarantee is: the sidebar claims its layout and paints its
//! FINAL chrome (title, borders, close button) with a "Loading…" body on the
//! very first frame after the switch, and the tree landing later swaps only
//! that body — no layout shift, no chrome pop.
//!
//! Regression: the mid-build placeholder was an anonymous, untitled box, so
//! the first frame after a lazy window materialization (`set_active_window`
//! → `materialize_window` → `restore_window`) showed the restored buffers
//! next to what read as "no explorer", and the panel then visibly popped in
//! a tick later — a two-stage flash on every Orchestrator-dock live-switch /
//! "Next Window" into a not-yet-materialized session.
//!
//! Per CONTRIBUTING §2 the test drives only keyboard input and asserts on
//! rendered output; per §3 all waiting is semantic. The switch key is bound
//! directly to `next_window` (a user-configurable binding) and sent through
//! `Editor::handle_key` + a single `render()` — exactly what the production
//! event loop does for one input event. `EditorTestHarness::send_key` is
//! deliberately NOT used for that one key: it also drains async work before
//! rendering, which would let the async tree-build land "between frames" and
//! hide the very frame the real event loop shows.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use std::fs;
use tempfile::TempDir;

fn launch_harness(
    project_dir: &std::path::Path,
    dir_context: &DirectoryContext,
) -> EditorTestHarness {
    // F7 → next_window: gives the test a single-keystroke, synchronous
    // window switch (the palette routes through the async plugin thread,
    // which cannot pin down "the first frame after the switch").
    let mut config = Config::default();
    config.keybindings.push(fresh::config::Keybinding {
        key: "F7".to_string(),
        modifiers: vec![],
        keys: vec![],
        action: "next_window".to_string(),
        args: std::collections::HashMap::new(),
        when: None,
    });
    EditorTestHarness::create(
        120,
        32,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(project_dir.to_path_buf())
            .with_shared_dir_context(dir_context.clone())
            .without_empty_plugins_dir(),
    )
    .unwrap()
}

/// The first rendered frame after switching into a lazily-materialized
/// window must already show the sidebar panel with its final chrome (either
/// still "Loading…" or already carrying the tree), and the async tree load
/// must not move or restyle that chrome afterwards.
#[test]
fn switched_to_window_paints_explorer_chrome_on_first_frame() {
    let temp_dir = TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Two projects. `beta` carries a marker directory that only ever
    // appears in the explorer tree (nothing opens it as a buffer), plus a
    // file nested in `src/` whose reveal requires expanding the tree.
    let alpha = temp_dir.path().join("alpha");
    fs::create_dir(&alpha).unwrap();
    fs::write(alpha.join("alpha.txt"), "ALPHA CONTENT\n").unwrap();

    let beta = temp_dir.path().join("beta");
    fs::create_dir(&beta).unwrap();
    fs::create_dir(beta.join("zz-tree-only-dir")).unwrap();
    fs::create_dir(beta.join("src")).unwrap();
    fs::write(
        beta.join("src").join("beta_marker.txt"),
        "BETA MARKER CONTENT\n",
    )
    .unwrap();

    // Session 1: launch in `beta`. The bare-directory default opens the
    // explorer; open the nested file so the persisted workspace has a real
    // active buffer; shut down cleanly to save the workspace (explorer
    // visible + src/beta_marker.txt open).
    {
        let mut h = launch_harness(&beta, &dir_context);
        h.editor_mut()
            .restore_active_window_on_launch(false)
            .unwrap();
        h.wait_until(|h| h.screen_to_string().contains("zz-tree-only-dir"))
            .unwrap();
        h.open_file(&beta.join("src").join("beta_marker.txt"))
            .unwrap();
        h.wait_until(|h| h.screen_to_string().contains("BETA MARKER CONTENT"))
            .unwrap();
        h.shutdown(true).unwrap();
    }

    // Mirror beta's saved workspace into the shared DirectoryContext's
    // workspaces dir. `Workspace::save` writes to the process-global data
    // dir while session discovery scans `dir_context.data_dir/workspaces`;
    // in production both are `<data_dir>/fresh`, but the test harness keeps
    // them apart for isolation, so bridge the two by hand. (The global copy
    // must stay: `materialize_window` loads the workspace content from it.)
    let saved = fresh::workspace::get_workspace_path(&beta).unwrap();
    let discovery_dir = dir_context.data_dir.join("workspaces");
    fs::create_dir_all(&discovery_dir).unwrap();
    fs::copy(&saved, discovery_dir.join(saved.file_name().unwrap())).unwrap();

    // Session 2: launch in `alpha`. Startup discovers beta's persisted
    // session and holds it as a lazily-materialized shell — the same state
    // an Orchestrator-dock card or "Next Window" target is in.
    let mut h = launch_harness(&alpha, &dir_context);
    h.editor_mut()
        .restore_active_window_on_launch(false)
        .unwrap();
    h.wait_until(|h| h.screen_to_string().contains("alpha.txt"))
        .unwrap();

    // Switch with the bound key and render exactly one frame — the frame
    // the real event loop paints right after the input event, before any
    // async bridge message can be processed. This is the frame that used
    // to show an anonymous empty box where the explorer belongs.
    h.editor_mut()
        .handle_key(KeyCode::F(7), KeyModifiers::NONE)
        .unwrap();
    h.render().unwrap();

    let first_frame = h.screen_to_string();

    // The switch itself landed in this frame: the restored window's active
    // buffer is on screen.
    assert!(
        first_frame.contains("BETA MARKER CONTENT"),
        "first frame after the switch must show the restored window's \
         buffer; screen:\n{first_frame}"
    );
    // The sidebar is already the explorer panel — final title chrome, with
    // its body either still loading or already carrying the tree. An
    // anonymous or blank column is exactly the two-stage flash.
    assert!(
        first_frame.contains("File Explorer"),
        "first frame after the switch must show the explorer panel's final \
         chrome (title), not an anonymous placeholder; screen:\n{first_frame}"
    );
    assert!(
        first_frame.contains("Loading") || first_frame.contains("zz-tree-only-dir"),
        "the explorer body must be either the loading indicator or the tree \
         on the first frame — never blank; screen:\n{first_frame}"
    );
    // Record the chrome/tab row (explorer top border + title + tab bar).
    // It must be byte-identical after the tree lands: the load may swap
    // only the panel body.
    let chrome_row = |s: &str| {
        s.lines()
            .find(|l| l.starts_with('┌'))
            .map(str::to_string)
            .unwrap_or_default()
    };
    let first_chrome = chrome_row(&first_frame);
    assert!(
        first_chrome.contains("File Explorer"),
        "the panel title must sit on the top-border chrome row; \
         screen:\n{first_frame}"
    );

    // The async build lands: the tree content appears and the restored
    // active file is revealed in it — `beta_marker.txt` shows both as the
    // tab and as the revealed (src/ expanded) tree entry.
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("zz-tree-only-dir")
            && s.lines().filter(|l| l.contains("beta_marker.txt")).count() >= 2
    })
    .unwrap();

    // ...and the load changed nothing about the chrome row: same title, same
    // borders, same tab bar, same columns — no second-stage paint of the
    // page around the tree.
    assert_eq!(
        chrome_row(&h.screen_to_string()),
        first_chrome,
        "the tree landing must not move or restyle the explorer chrome / \
         tab row"
    );
}
