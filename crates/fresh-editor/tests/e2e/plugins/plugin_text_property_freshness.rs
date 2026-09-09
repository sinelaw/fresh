//! What a plugin reads back from a buffer's text properties must be what
//! it last wrote there.
//!
//! This is the contract the plugin state snapshot sits in the middle of.
//! The snapshot used to copy every buffer's properties on every tick,
//! which was correct but made a tick's cost grow with the buffers'
//! content. The first attempt to avoid the copy compared a version and
//! skipped when it looked unchanged — and the version turned out to be the
//! property *count*, so re-rendering a panel with the same number of rows
//! and different values served the old values indefinitely.
//!
//! The snapshot now shares the property set rather than copying it, so
//! there is no freshness question to get wrong. This test does not know
//! that: it writes, reads back, rewrites with the same shape, and reads
//! again — so it holds against whatever the host does underneath.

use crate::common::harness::EditorTestHarness;
use std::fs;

const PLUGIN_NAME: &str = "prop_probe";

/// A plugin that owns a virtual buffer, rewrites its rows on command, and
/// reports what `getTextPropertiesAtCursor` gives back for row 0.
const PLUGIN_SOURCE: &str = r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

let bufferId: number | null = null;

/** One row per file name, each carrying it as a text property. */
function rows(files: string[]): Record<string, unknown>[] {
    return files.map(f => ({
        text: `row for ${f}\n`,
        properties: { file: f },
    }));
}

async function probeOpen(): Promise<void> {
    const res = await editor.createVirtualBuffer({
        name: "*prop probe*",
        mode: "prop-probe",
        readOnly: true,
    });
    bufferId = res.bufferId;
    editor.setVirtualBufferContent(bufferId, rows(["a.rs", "second.rs"]));
    editor.setStatus("probe: opened");
}
registerHandler("probe_open", probeOpen);

/** Rewrite with the SAME number of property-bearing rows, new values. */
function probeRewrite(): void {
    if (bufferId === null) return;
    editor.setVirtualBufferContent(bufferId, rows(["b.rs", "second.rs"]));
    editor.setStatus("probe: rewritten");
}
registerHandler("probe_rewrite", probeRewrite);

/** Report the `file` property the host has for the cursor's row. */
function probeRead(): void {
    if (bufferId === null) return;
    const props = editor.getTextPropertiesAtCursor(bufferId);
    const first = props.length > 0 ? props[0] : {};
    editor.setStatus(`probe: file=${String((first as Record<string, unknown>)["file"])}`);
}
registerHandler("probe_read", probeRead);

editor.registerCommand("probe: Open", "Open the probe buffer", "probe_open", null);
editor.registerCommand("probe: Rewrite", "Rewrite the probe rows", "probe_rewrite", null);
editor.registerCommand("probe: Read", "Report the cursor row's property", "probe_read", null);
"#;

fn harness_with_probe_plugin() -> (EditorTestHarness, tempfile::TempDir) {
    let temp = tempfile::TempDir::new().expect("tempdir");
    let working_dir = temp.path().join("work");
    fs::create_dir_all(&working_dir).unwrap();
    let plugins_dir = working_dir.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    fs::write(plugins_dir.join(format!("{PLUGIN_NAME}.ts")), PLUGIN_SOURCE).unwrap();
    crate::common::harness::copy_plugin_lib(&plugins_dir);

    let harness = EditorTestHarness::with_working_dir(120, 40, working_dir).expect("harness");
    (harness, temp)
}

/// Rewriting a buffer's rows with the same number of properties and
/// different values must change what the plugin reads back.
#[test]
fn a_rewrite_with_the_same_shape_is_visible_to_the_plugin() {
    let (mut harness, _temp) = harness_with_probe_plugin();

    harness.run_palette_command("probe: Open").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("probe: opened"))
        .unwrap();

    harness.run_palette_command("probe: Read").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("probe: file="))
        .unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("probe: file=a.rs"),
        "the plugin should read back what it wrote. Screen:\n{screen}"
    );

    // Same row count, same property count — only the values differ. This
    // is the shape a count-based freshness check cannot see.
    harness.run_palette_command("probe: Rewrite").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("probe: rewritten"))
        .unwrap();

    harness.run_palette_command("probe: Read").unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("probe: file=a.rs") || s.contains("probe: file=b.rs")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("probe: file=b.rs"),
        "after rewriting the row, the plugin must read the new property — \
         reading `a.rs` back means the host served a stale set. Screen:\n{screen}"
    );
}
