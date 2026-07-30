//! Frame-time regression test under a deliberately hostile plugin.
//!
//! The executable form of "the editor is structurally incapable of UI
//! hangs, no matter how a plugin is written": a sidecar plugin abuses the
//! dashboard section API with a tight `grepProject` loop, a subprocess
//! storm, giant row emission, a throwing section, and a never-resolving
//! section — and the test asserts the editor loop keeps ticking and
//! painting under all of it.
//!
//! The threshold is deliberately loose (seconds, not the interactive
//! 30ms target) so the test is CI-stable while still failing hard on the
//! defect class it guards: plugin-originated work running inline on the
//! editor thread, which showed up as multi-second `render` and
//! `process_plugin_commands` spans before the off-loop split.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use fresh::config::{Config, PluginConfig};
use std::fs;
use std::time::{Duration, Instant};

fn config_with_dashboard() -> Config {
    let mut config = Config::default();
    config.plugins.insert(
        "dashboard".to_string(),
        PluginConfig {
            enabled: true,
            path: None,
            settings: serde_json::json!({ "autoOpen": true }),
        },
    );
    config
}

#[test]
fn hostile_plugin_cannot_stall_the_editor_loop() {
    let temp = tempfile::TempDir::new().expect("tempdir");
    let working_dir = temp.path().join("work");
    fs::create_dir_all(&working_dir).unwrap();
    let plugins_dir = working_dir.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "dashboard");
    copy_plugin_lib(&plugins_dir);

    // Enough content that an inline (editor-thread) grep would cost real,
    // measurable time, while an off-loop grep costs the loop nothing.
    let src_dir = working_dir.join("src");
    fs::create_dir_all(&src_dir).unwrap();
    let line = "// TODO alpha beta gamma delta epsilon zeta eta theta iota\n";
    let blob = line.repeat(4_000); // ~240KB per file
    for i in 0..40 {
        fs::write(src_dir.join(format!("gen_{i}.rs")), &blob).unwrap();
    }

    // The hostile sidecar. Every section redlines at the minimum TTL so the
    // scheduler re-runs them as fast as it allows.
    let hostile = r#"/// <reference path="./lib/fresh.d.ts" />
/// @depends-on dashboard
const editor = getEditor();
type AnyCtx = {
    kv: (label: string, value: string, color?: string) => void;
    text: (s: string, o?: { color?: string; onClick?: () => void }) => void;
    newline: () => void;
};
const dash = editor.getPluginApi("dashboard") as {
    registerSection: (
        name: string,
        refresh: (ctx: AnyCtx) => Promise<void>,
        options?: { ttlMs?: number; timeoutMs?: number },
    ) => () => void;
} | null;
let wide = "";
while (wide.length < 300) wide += "HOSTILE0123456789";
if (dash) {
    dash.registerSection("hgrep", async (ctx) => {
        // Back-to-back project greps; inline execution would stall the loop
        // for the full walk+search each time.
        for (let i = 0; i < 3; i++) {
            const m = await editor.grepProject("TODO", true, true, 5000, false);
            ctx.kv("pass" + i, String(m.length));
        }
    }, { ttlMs: 1000, timeoutMs: 30000 });
    dash.registerSection("hspawn", async (ctx) => {
        const jobs = [];
        for (let i = 0; i < 8; i++) {
            jobs.push(editor.spawnProcess("echo", ["storm", String(i)]));
        }
        await Promise.all(jobs);
        ctx.kv("spawned", "8");
    }, { ttlMs: 1000 });
    dash.registerSection("hwide", async (ctx) => {
        for (let i = 0; i < 5; i++) {
            ctx.text(wide, { color: "accent", onClick: () => { } });
            ctx.newline();
        }
    }, { ttlMs: 1000 });
    dash.registerSection("hthrow", async () => {
        // Reject on a later microtask, not synchronously: the dashboard's
        // catch handles both, but QuickJS's rejection tracker flags a
        // sync-throw async fn as unhandled before the handler attaches,
        // and the test harness promotes that to plugin-thread death
        // (set_panic_on_js_errors). Production only logs it.
        await editor.delay(1);
        throw new Error("always throws " + wide);
    }, { ttlMs: 1000 });
    dash.registerSection("hhang", async () => {
        await new Promise(() => { });
    }, { ttlMs: 1000, timeoutMs: 1000 });
}
"#;
    fs::write(plugins_dir.join("hostile.ts"), hostile).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        200,
        50,
        config_with_dashboard(),
        working_dir,
    )
    .expect("harness");
    harness.editor_mut().fire_ready_hook();

    // Dashboard open with the hostile sections registered and painting.
    harness
        .wait_until(|h| h.screen_to_string().contains("HGREP"))
        .unwrap();

    // Give the sections one full cycle so greps, spawns and timeouts are all
    // genuinely in flight while we measure.
    harness
        .wait_until(|h| h.screen_to_string().contains("pass0"))
        .unwrap();

    // Measure: every editor tick + render must complete promptly while the
    // hostile plugin does its worst. The pre-fix defect showed up here as
    // individual iterations costing the full inline grep (seconds).
    let mut worst = Duration::ZERO;
    let mut over_500ms = 0usize;
    let iterations = 120;
    for _ in 0..iterations {
        let t0 = Instant::now();
        harness.tick_and_render().unwrap();
        let dt = t0.elapsed();
        if dt > worst {
            worst = dt;
        }
        if dt > Duration::from_millis(500) {
            over_500ms += 1;
        }
        std::thread::sleep(Duration::from_millis(15));
    }

    assert!(
        worst < Duration::from_millis(1500),
        "editor loop stalled under hostile plugin: worst tick+render {worst:?} \
         — plugin work is running inline on the editor thread again"
    );
    assert!(
        over_500ms <= 2,
        "editor loop degraded under hostile plugin: {over_500ms}/{iterations} \
         iterations exceeded 500ms (worst {worst:?})"
    );

    // The frame must still be a well-formed box despite the 300-col rows.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains('╭') && screen.contains('╰'),
        "dashboard frame missing after hostile run:\n{screen}"
    );
    for row in screen.lines() {
        let bars = row.chars().filter(|c| *c == '│').count();
        assert!(
            bars <= 2,
            "torn frame row (>{bars} borders) after hostile run: {row:?}"
        );
    }
}
