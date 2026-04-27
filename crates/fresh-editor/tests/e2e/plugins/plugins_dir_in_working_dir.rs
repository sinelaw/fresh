//! Regression test for https://github.com/sinelaw/fresh/issues/1722
//!
//! When a user runs Fresh from a project directory that happens to contain
//! a folder called `plugins/` (unrelated to Fresh's plugin system), the
//! embedded bundled plugins (`pkg.ts`, `find_references.ts`, etc.) silently
//! fail to load. This makes commands like "Package: Packages" disappear
//! from the command palette and breaks features like "Find References".
//!
//! The reproducer from the issue is:
//! ```shell
//! mkdir -p lorem/plugins
//! cd lorem
//! fresh
//! ```
//! Expected: the Package Manager command shows up in the command palette.
//! Actual: it's missing.

#![cfg(all(feature = "plugins", feature = "embed-plugins"))]

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use tempfile::TempDir;

/// Reproduces issue #1722: an empty `plugins/` directory next to the user's
/// project must not stop the bundled embedded plugins from loading.
///
/// Drives the command palette via keyboard and asserts only on rendered
/// output: typing a short fragment of the Package Manager command name
/// must surface the *description* registered by `pkg.ts`
/// ("Browse and manage installed and available packages"). That string is
/// only ever rendered as part of the suggestion row and is unique to the
/// `pkg.ts` plugin's i18n, so its presence on screen proves the embedded
/// plugin loaded.
#[test]
fn test_empty_plugins_dir_in_working_dir_does_not_hide_embedded_plugins() {
    // Simulate `mkdir -p lorem/plugins; cd lorem; fresh` from the issue.
    let temp_dir = TempDir::new().unwrap();
    let working_dir = temp_dir.path().to_path_buf();
    let user_plugins_dir = working_dir.join("plugins");
    fs::create_dir_all(&user_plugins_dir).unwrap();

    // `with_config_and_working_dir` calls `.without_empty_plugins_dir()`
    // internally, so the harness will not overwrite our setup — the empty
    // `plugins/` folder we just created is what Fresh sees on startup.
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        140,
        40,
        Default::default(),
        working_dir.clone(),
    )
    .unwrap();

    // Open the command palette.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    // Type a short fragment that should match the Package Manager command
    // ("Package: Packages") via the palette's fuzzy match. We deliberately
    // type *less* than the description we'll assert on, so the assertion
    // can only succeed by reading text rendered from the suggestion list.
    harness.type_text("Packages").unwrap();

    // The description string is unique to the `pkg.ts` plugin's i18n. If
    // embedded plugins didn't load (the bug), this text never appears.
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .contains("Browse and manage installed and available packages")
        })
        .unwrap();
}
