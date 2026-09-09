//! Traps a plugin author (or an agent writing init.ts on their behalf) used to
//! fall into, pinned so they stay fixed.
//!
//! Each test here corresponds to a specific way the API used to mislead:
//! a value you can read but not pass back, and a panel you create but cannot
//! find again. Neither produced a useful error — one threw about a numeric
//! conversion, the other silently returned an empty name — so both cost far
//! more time than the underlying problem was worth.

use crate::common::harness::EditorTestHarness;
use fresh::config_io::DirectoryContext;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;

fn harness_with_scratch_config_dir() -> (EditorTestHarness, tempfile::TempDir, PathBuf) {
    let temp = tempfile::TempDir::new().expect("tempdir");
    let dir_context = DirectoryContext::for_testing(temp.path());
    let config_dir = dir_context.config_dir.clone();
    fs::create_dir_all(&config_dir).unwrap();

    let working_dir = temp.path().join("work");
    fs::create_dir_all(&working_dir).unwrap();

    // Wide enough that the status line these tests read back is not elided:
    // the status bar drops elements to fit, and an assertion on a message
    // that never had room to render proves nothing.
    let harness = EditorTestHarness::with_shared_dir_context(
        120,
        24,
        Default::default(),
        working_dir,
        dir_context,
    )
    .expect("harness");
    (harness, temp, config_dir)
}

fn write_init_ts(config_dir: &Path, body: &str) {
    fs::write(config_dir.join("init.ts"), body).unwrap();
}

/// The rendered status line — CONTRIBUTING.md Testing §2 (observe, don't
/// inspect): what the plugin announced is read off the screen, not out of
/// `Editor::get_status_message()`.
fn status(harness: &EditorTestHarness) -> String {
    harness.get_status_bar()
}

/// The number in a `<prefix>=<n>` marker on the status line, if one is shown.
fn marker_count(harness: &EditorTestHarness, prefix: &str) -> Option<u32> {
    let line = status(harness);
    let rest = line.split(prefix).nth(1)?;
    let digits: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
    digits.parse().ok()
}

/// Pump the editor until `done` holds, *without* advancing logical time.
///
/// The harness's own `wait_until` moves its clock forward on every iteration,
/// which would let plugin timers come due while we wait — no good where the
/// property under test is "this happened before any timer tick". Renders each
/// round so screen-reading conditions see a current frame.
fn pump_until(harness: &mut EditorTestHarness, done: impl Fn(&EditorTestHarness) -> bool) -> bool {
    for _ in 0..300 {
        harness.editor_mut().process_async_messages();
        harness.render().expect("render");
        if done(harness) {
            return true;
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    harness.editor_mut().process_async_messages();
    harness.render().expect("render");
    done(harness)
}

/// A negative window id is refused, not thrown on.
///
/// `listWorkspaces()` reports a negative `windowId` for a workspace that
/// exists on disk but has never been activated. Feeding that straight back to
/// `setActiveWindow` — the obvious thing to do with an id you were just
/// handed — used to abort the whole script with
/// `Error converting from js 'f64' into type 'u64': Underflow`: an exception
/// about a numeric conversion, naming nothing the author had written, from a
/// line that looked correct. Returning `false` lets the caller branch.
#[test]
fn set_active_window_refuses_a_negative_id_instead_of_throwing() {
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();
        try {
            // The sentinel listWorkspaces() reports for a workspace with no
            // window yet.
            const ok = editor.setActiveWindow(-3);
            editor.setStatus(ok ? "RETURNED_TRUE" : "RETURNED_FALSE");
        } catch (e) {
            editor.setStatus("THREW");
        }
        "#,
    );

    harness.editor_mut().load_init_script(true);
    assert!(
        pump_until(&mut harness, |h| {
            let s = status(h);
            s.contains("RETURNED_") || s.contains("THREW")
        }),
        "init.ts never reported; status = {:?}",
        status(&harness)
    );
    assert!(
        status(&harness).contains("RETURNED_FALSE"),
        "a non-positive window id must be refused, not thrown on; status = {:?}",
        status(&harness)
    );
}

/// The live-panel recipe from `fresh --cmd help plugin` actually runs.
///
/// A worked example that does not work is worse than no example: an agent
/// copying it inherits the bug and then debugs the wrong thing. So the shape
/// shipped in the help text is exercised here — `registerHandler`, an
/// awaited first paint from inside the open command, `setInterval` keeping it
/// painting afterwards, the re-entrancy and token guards, and `buffer_closed`
/// stopping it.
///
/// The data source is swapped for a counter (the help text reads workspaces,
/// which need an orchestrator this harness has no reason to spin up); every
/// piece of the mechanism around it is the same.
#[test]
fn the_documented_live_panel_recipe_runs() {
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();
        let panelId = null;
        let opening = false;
        let token = 0;
        let inFlight = false;
        let paints = 0;

        registerHandler("panel_render", async function () {
            if (panelId === null || inFlight) return;
            inFlight = true;
            const mine = token;
            let data = null;
            try {
                const timedOut = Symbol("timeout");
                const got = await Promise.race([
                    Promise.resolve(["alpha", "beta"]),
                    editor.delay(8000).then(() => timedOut),
                ]);
                if (got !== timedOut) data = got;
            } catch (e) {
                // keep the last good paint
            } finally {
                inFlight = false;
            }
            if (mine !== token || panelId === null || data === null) return;
            paints += 1;
            editor.setVirtualBufferContent(
                panelId,
                data.map(function (name) {
                    return { text: name.padEnd(12) + "ok\n" };
                })
            );
            editor.setStatus("PAINTS=" + paints);
        });

        registerHandler("open_panel", async function () {
            if (panelId !== null) return;
            if (opening) return;
            opening = true;
            try {
                const res = await editor.createVirtualBuffer({
                    name: "Recipe Panel",
                    readOnly: true,
                    editingDisabled: true,
                    showCursors: false,
                });
                panelId = res.bufferId;
                token++;
            } finally {
                opening = false;
            }
            await globalThis.panel_render();
            editor.setInterval(50, "panel_render");
        });
        editor.registerCommand("Recipe Panel", "the documented panel",
                               "open_panel", null);

        registerHandler("panel_closed", function (ev) {
            if (ev.bufferId === panelId) { panelId = null; token++; }
        });
        editor.on("buffer_closed", "panel_closed");
        "#,
    );

    harness.editor_mut().load_init_script(true);

    // Ctrl+P → "Recipe Panel" → Enter, the way the author would run it. The
    // palette waits for init.ts to have registered the row before confirming,
    // so it doubles as the registration gate — and, unlike a synthetic
    // `RunEditorCommand` with an invented callback id, it answers a promise
    // somebody is actually holding.
    harness
        .run_palette_command("Recipe Panel")
        .expect("opening the panel from the palette");

    // The awaited render inside the open command means the panel has content
    // before any timer has ticked — the property that separates a working
    // panel from one stuck on "loading…". Hence a pump that does *not* move
    // the clock: the first paint must not need a tick to arrive.
    assert!(
        pump_until(&mut harness, |h| marker_count(h, "PAINTS=") == Some(1)),
        "the open command should paint once immediately; status = {:?}",
        status(&harness)
    );

    // And then the timer keeps it painting. Waited for rather than counted
    // after a fixed number of advances: how many paints have landed by an
    // arbitrary deadline is a property of the machine, not of the timer.
    harness
        .wait_until(|h| marker_count(h, "PAINTS=").unwrap_or(0) >= 3)
        .expect("the panel should keep refreshing after the first paint");
}

/// Reloading init.ts closes the panels its previous copy created, instead of
/// leaving one behind on every iteration.
///
/// This is the plugin dev loop itself — edit, reload, run — so a leak here
/// compounds: three iterations, three identical panels, and a layout the
/// author has to clean up by hand before they can see what they changed. It
/// went unnoticed because the cleanup path *looked* right: the runtime tracks
/// `virtual_buffer_ids` and closes them on unload, but nothing ever populated
/// that list (every `createVirtualBuffer*` answers by resolving its callback
/// directly, bypassing the response handler that did the tracking), and the
/// close it did send was refused anyway because a panel with content counts
/// as modified.
#[test]
fn reloading_init_closes_the_panels_its_previous_copy_created() {
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    let body = r#"
        const editor = getEditor();
        (async () => {
            await editor.createVirtualBuffer({
                name: "Leaky Panel",
                entries: [{ text: "content makes it modified\n" }],
            });
            await editor.flush();
            editor.setStatus("PANEL_OPEN");
        })();
    "#;

    // Counted from the plugin state snapshot — the same view `listBuffers()`
    // serves a plugin, so the test sees what an author would.
    let count = |h: &EditorTestHarness| {
        let Some(handle) = h.editor().plugin_manager().state_snapshot_handle() else {
            return 0;
        };
        let Ok(snapshot) = handle.read() else {
            return 0;
        };
        snapshot
            .buffers
            .values()
            .filter(|b| b.name == "Leaky Panel")
            .count()
    };

    write_init_ts(&config_dir, body);
    harness.editor_mut().load_init_script(true);
    assert!(
        pump_until(&mut harness, |h| count(h) == 1),
        "the first load should open exactly one panel, saw {}",
        count(&harness)
    );

    // Reload the same source twice, as an author iterating would.
    for round in 1..=2 {
        write_init_ts(&config_dir, body);
        harness.editor_mut().load_init_script(true);
        assert!(
            pump_until(&mut harness, |h| count(h) == 1),
            "reload {round} should leave exactly one panel, saw {}",
            count(&harness)
        );
    }
}

/// A virtual buffer is findable in `listBuffers()` by the name it was created
/// with.
///
/// Before `BufferInfo.name` carried it, `path` was empty and `name` absent for
/// every virtual buffer, so a plugin looking for its own panel had nothing to
/// match on but `is_virtual && path === ""` — which matches every *other*
/// plugin's panel too, and every one of its own.
#[test]
fn a_virtual_buffer_is_findable_by_name_in_list_buffers() {
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();
        (async () => {
            await editor.createVirtualBuffer({
                name: "Agent Panel",
                entries: [{ text: "hello\n" }],
            });
            await editor.flush();
            const mine = editor.listBuffers()
                .filter(b => b.is_virtual && b.name.indexOf("Agent Panel") >= 0);
            editor.setStatus("FOUND=" + mine.length);
        })();
        "#,
    );

    harness.editor_mut().load_init_script(true);
    assert!(
        pump_until(&mut harness, |h| status(h).contains("FOUND=")),
        "the panel probe never reported; status = {:?}",
        status(&harness)
    );
    assert_eq!(
        marker_count(&harness, "FOUND="),
        Some(1),
        "a virtual buffer should be findable by the name it was created with; \
         status = {:?}",
        status(&harness)
    );
}
