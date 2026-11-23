//! A plugin can put a picture over a buffer's pane (`setBufferImage`), show
//! only part of it (`setBufferImageView`), and take it away again
//! (`clearBufferImage`, or by closing the buffer); the bundled image viewer
//! builds on that. The test terminal answers no graphics query, so the
//! picture comes out as coloured half blocks — the fallback every frontend
//! has.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::style::Color;
use std::fs;
use std::path::Path;

const PLUGIN_SOURCE: &str = r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();
let bufferId: number | null = null;

async function imgOpen(): Promise<void> {
    const res = await editor.createVirtualBuffer({ name: "*picture*", readOnly: true });
    bufferId = res.bufferId;
    editor.setVirtualBufferContent(bufferId, [{ text: "under the picture\n" }]);
    const missing = editor.setBufferImage(bufferId, editor.getCwd() + "/missing.png");
    editor.setBufferImage(bufferId, editor.getCwd() + "/red-blue.png", true);
    // Short: the status bar has room for about 25 characters.
    editor.setStatus("img: " + editor.getImageSupport()?.protocol + (missing ? " read" : " skip"));
}
registerHandler("img_open", imgOpen);

function imgLeft(): void {
    if (bufferId !== null) editor.setBufferImageView(bufferId, 0, 0, 0.25, 0.5);
}
registerHandler("img_left", imgLeft);

function imgClear(): void {
    if (bufferId === null) return;
    editor.clearBufferImage(bufferId);
    editor.setStatus("img: cleared");
}
registerHandler("img_clear", imgClear);

editor.registerCommand("img: Open", "Open a picture buffer", "img_open", null);
editor.registerCommand("img: Left", "Show the top left corner", "img_left", null);
editor.registerCommand("img: Clear", "Clear the picture", "img_clear", null);
"#;

const RED: Color = Color::Rgb(255, 0, 0);
const BLUE: Color = Color::Rgb(0, 0, 255);

/// Cells painted in `color`. A half-block cell whose two halves match is
/// drawn as a space on that background.
fn cells(h: &EditorTestHarness, color: Color) -> usize {
    h.buffer()
        .content()
        .iter()
        .filter(|c| c.bg == color || (c.symbol() == "▀" && c.fg == color))
        .count()
}

/// A 40x20 picture: its left half red, its right half blue.
fn write_red_blue(path: &Path) {
    image::RgbImage::from_fn(40, 20, |x, _| {
        if x < 20 {
            image::Rgb([255, 0, 0])
        } else {
            image::Rgb([0, 0, 255])
        }
    })
    .save(path)
    .unwrap();
}

#[test]
fn a_plugin_shows_part_of_a_picture_and_clears_it() {
    let temp = tempfile::TempDir::new().unwrap();
    let work = temp.path().join("work");
    let plugins = work.join("plugins");
    fs::create_dir_all(&plugins).unwrap();
    fs::write(plugins.join("img_probe.ts"), PLUGIN_SOURCE).unwrap();
    copy_plugin_lib(&plugins);
    write_red_blue(&work.join("red-blue.png"));

    let mut h = EditorTestHarness::with_working_dir(100, 30, work).unwrap();
    assert_eq!(cells(&h, RED), 0);

    h.run_palette_command("img: Open").unwrap();
    // Enlarged to the pane's width: far more cells than the 40x20 pixels
    // would take at their own size.
    h.wait_until(|h| cells(h, RED) > 200 && cells(h, BLUE) > 200)
        .unwrap();
    // A file that cannot be read shows nothing and says so.
    h.wait_for_screen_contains("img: halfblocks skip")
        .unwrap();

    // The top left corner, enlarged and widened to the pane's shape (to
    // less than half the picture's width): all red.
    h.run_palette_command("img: Left").unwrap();
    h.wait_until(|h| cells(h, BLUE) == 0 && cells(h, RED) > 400)
        .unwrap();

    h.run_palette_command("img: Clear").unwrap();
    h.wait_until(|h| cells(h, RED) == 0).unwrap();
    assert!(h.screen_to_string().contains("under the picture"));
}

#[test]
fn the_image_viewer_opens_a_picture_and_zooms_into_it() {
    let temp = tempfile::TempDir::new().unwrap();
    let work = temp.path().join("work");
    let plugins = work.join("plugins");
    fs::create_dir_all(&plugins).unwrap();
    copy_plugin(&plugins, "image_viewer");
    copy_plugin_lib(&plugins);
    let picture = work.join("red-blue.png");
    write_red_blue(&picture);

    let mut h = EditorTestHarness::with_working_dir(100, 30, work).unwrap();
    h.open_file(&picture).unwrap();
    // The bytes give way to the picture, both halves of it.
    h.wait_until(|h| cells(h, RED) > 200 && cells(h, BLUE) > 200)
        .unwrap();

    // Zoom in, then go left: only the red half shows.
    for _ in 0..3 {
        h.send_key(KeyCode::Char('+'), KeyModifiers::NONE).unwrap();
    }
    for _ in 0..4 {
        h.send_key(KeyCode::Char('h'), KeyModifiers::NONE).unwrap();
    }
    h.wait_until(|h| cells(h, BLUE) == 0 && cells(h, RED) > 400)
        .unwrap();
    h.wait_for_screen_contains("Image: zoom 338%").unwrap();

    // Fit: the whole picture again.
    h.send_key(KeyCode::Char('0'), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| cells(h, BLUE) > 200).unwrap();

    // Show as Text: the same tab shows the file's text again, and the
    // picture goes.
    h.send_key(KeyCode::Char('t'), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| cells(h, RED) == 0 && cells(h, BLUE) == 0)
        .unwrap();
    // The tab is the file's own: one tab for it, never a second one.
    assert_eq!(h.screen_to_string().matches("red-blue.png").count(), 1);

    // View as Image: the picture is back, in that tab.
    h.run_palette_command("Image: View as Image").unwrap();
    h.wait_until(|h| cells(h, RED) > 200 && cells(h, BLUE) > 200)
        .unwrap();

    // Close: the picture goes with its tab.
    h.send_key(KeyCode::Char('q'), KeyModifiers::NONE).unwrap();
    h.wait_until(|h| cells(h, RED) == 0 && cells(h, BLUE) == 0)
        .unwrap();
    assert!(!h.screen_to_string().contains("red-blue.png"));
}
