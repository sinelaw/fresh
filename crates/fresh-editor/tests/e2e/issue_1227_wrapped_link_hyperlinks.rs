//! Issue #1227: a long link in a Markdown file that wraps onto several rows
//! could not be opened with ctrl-click — the terminal recognises URLs one row
//! at a time, so it only ever saw a fragment.
//!
//! In page view the markdown plugin marks a link's label and its URL with
//! the link target, and the editor now draws those cells as OSC 8 terminal
//! hyperlinks, so every row the link wraps over carries the whole URL. These
//! tests replay the frames the editor writes through a terminal model and
//! check the hyperlinks a terminal ends up with — while the link is drawn,
//! while the cursor reveals and hides its markup, and while scrolling.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::terminal_model::WidthPolicy;
use crossterm::event::{KeyCode, KeyModifiers};

const WIDTH: u16 = 100;
const HEIGHT: u16 = 30;

fn long_url() -> String {
    let segments: Vec<String> = (0..30).map(|i| format!("segment{i:03}")).collect();
    format!("https://example.com/{}/end", segments.join("/"))
}

/// Open `content` as a Markdown file, with frames going to a terminal that
/// takes hyperlinks, and switch to page view.
fn page_view(content: &str) -> (EditorTestHarness, tempfile::TempDir) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);
    let md_path = project_root.join("links.md");
    std::fs::write(&md_path, content).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        WIDTH,
        HEIGHT,
        Default::default(),
        project_root,
    )
    .unwrap();
    harness.editor_mut().set_terminal_hyperlinks(true);
    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();
    harness.enable_terminal_model(WidthPolicy::ClusterWidth);

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Toggle Compose").unwrap();
    harness.wait_for_screen_contains("Toggle Compose").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness
        .wait_until_stable(|h| model_text(h).contains(" — https://example.com/"))
        .unwrap();
    (harness, temp_dir)
}

fn model_text(harness: &EditorTestHarness) -> String {
    let model = harness.terminal_model();
    (0..model.rows())
        .map(|row| model.row_text(row))
        .collect::<Vec<_>>()
        .join("\n")
}

/// The text of every column the terminal links to `url`, row by row, with
/// the blanks a wrap may consume dropped.
fn linked_text_per_row(harness: &EditorTestHarness, url: &str) -> Vec<String> {
    let model = harness.terminal_model();
    (0..model.rows())
        .map(|row| {
            model
                .columns(row)
                .iter()
                .zip(model.links(row))
                .filter(|(_, link)| link.as_deref() == Some(url))
                .map(|(glyph, _)| glyph.as_str())
                .collect::<String>()
        })
        .filter(|text| !text.is_empty())
        .collect()
}

#[test]
fn wrapped_link_is_a_hyperlink_on_every_row() {
    let url = long_url();
    let (harness, _temp) = page_view(&format!(
        "# Links\n\nSee [the long link]({url}) for details.\n\nAnother line.\n"
    ));
    harness.assert_terminal_model_matches_frame();

    let rows = linked_text_per_row(&harness, &url);
    assert!(
        rows.len() >= 5,
        "the URL should wrap over several rows, each a hyperlink: {rows:?}\n{}",
        model_text(&harness)
    );
    // Label and URL, and nothing else, link to the whole URL.
    assert_eq!(rows.concat(), format!("thelonglink{url}"), "{rows:?}");
    // Every row that shows part of the URL carries the link.
    let model = harness.terminal_model();
    for row in 0..model.rows() {
        let text = model.row_text(row);
        if text.contains("segment") {
            let linked: String = model
                .columns(row)
                .iter()
                .zip(model.links(row))
                .filter(|(_, l)| l.is_some())
                .map(|(g, _)| g.as_str())
                .collect();
            // The part of the row that is URL: after the ` — ` separator on
            // the first row, the whole row after it.
            let shown = text.trim();
            let url_part = shown.rsplit_once("— ").map_or(shown, |(_, u)| u);
            assert!(
                linked.ends_with(url_part),
                "row {row}: URL text {url_part:?} is not all linked, only {linked:?}"
            );
        }
    }
}

#[test]
fn revealing_and_hiding_link_markup_keeps_screen_and_links_in_step() {
    let url = long_url();
    let (mut harness, _temp) = page_view(&format!(
        "# Links\n\nSee [the long link]({url}) for details.\n\nAnother line.\n"
    ));
    // Into the link: its markup is revealed and the rows re-laid out.
    for _ in 0..3 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.assert_terminal_model_matches_frame();
    }
    harness
        .wait_until_stable(|h| model_text(h).contains("link](https://"))
        .unwrap();
    harness.assert_terminal_model_matches_frame();
    let rows = linked_text_per_row(&harness, &url);
    assert_eq!(rows.concat(), format!("thelonglink{url}"), "{rows:?}");

    // And out again, below it: concealed once more.
    for _ in 0..8 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.assert_terminal_model_matches_frame();
    }
    harness
        .wait_until_stable(|h| model_text(h).contains(" — https://"))
        .unwrap();
    harness.assert_terminal_model_matches_frame();
    let rows = linked_text_per_row(&harness, &url);
    assert_eq!(rows.concat(), format!("thelonglink{url}"), "{rows:?}");
}

#[test]
fn scrolling_links_with_wide_labels_leaves_no_stale_cells() {
    let labels = [
        "plain label",
        "\u{2764}\u{FE0F} hearts",
        "\u{6F22}\u{5B57} label",
        "x",
    ];
    let mut content = String::from("# Links\n\n");
    for i in 0..40 {
        let segments: Vec<String> = (0..5 + (i * 7) % 40).map(|j| format!("s{j:02}")).collect();
        content.push_str(&format!(
            "Item {i}: [{}](https://example.com/p{i}/{}/end) trailing text after the link.\n\n",
            labels[i % labels.len()],
            segments.join("/")
        ));
    }
    let (mut harness, _temp) = page_view(&content);
    harness.assert_terminal_model_matches_frame();
    for _ in 0..4 {
        harness.mouse_scroll_down(30, 12).unwrap();
        harness.assert_terminal_model_matches_frame();
    }
    harness.mouse_scroll_up(30, 12).unwrap();
    harness.assert_terminal_model_matches_frame();
    for _ in 0..6 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.assert_terminal_model_matches_frame();
    }
}

/// A link's title is not part of what it opens, and a target inside the
/// document (`#heading`, a relative path) is not something a terminal can
/// open, so neither reaches the terminal as a hyperlink.
#[test]
fn link_opens_its_destination_only_and_only_an_absolute_one() {
    let url = "https://example.com/guide";
    let (harness, _temp) = page_view(&format!(
        "# Links\n\nRead [the guide]({url} \"The Guide\") or [intro](#intro) or [notes](notes.md).\n"
    ));
    harness.assert_terminal_model_matches_frame();
    let model = harness.terminal_model();
    let targets: std::collections::BTreeSet<String> = (0..model.rows())
        .flat_map(|row| model.links(row))
        .flatten()
        .collect();
    assert_eq!(
        targets.into_iter().collect::<Vec<_>>(),
        vec![url.to_string()],
        "\n{}",
        model_text(&harness)
    );
    let rows = linked_text_per_row(&harness, url);
    assert_eq!(rows.concat(), format!("theguide{url}"), "{rows:?}");
}
