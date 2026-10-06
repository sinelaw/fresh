//! E2E: Settings shows the config schema's labels in the configured locale —
//! category names, section headers, setting names and descriptions — and
//! keeps the page layout the same as in English.

use crate::common::global_state::pin_config_globals;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use unicode_width::UnicodeWidthStr;

/// A string from the Japanese catalog, with `%{name}`-style placeholders
/// filled from `args`.
fn ja(key: &str, args: &[(&str, &str)]) -> String {
    let mut s = fresh_i18n::translate_in("ja", key)
        .unwrap_or_else(|| panic!("ja has no {key:?}"))
        .to_string();
    for (name, value) in args {
        s = s.replace(&format!("%{{{name}}}"), value);
    }
    s
}

/// Settings opened in Japanese.
///
/// Through the palette by the command's Japanese name: the harness's own
/// `open_settings` types the English one, which a Japanese palette does not
/// list.
fn japanese_settings() -> EditorTestHarness {
    let config = Config {
        locale: Some("ja").into(),
        ..Default::default()
    };
    let mut harness = EditorTestHarness::with_config(120, 40, config).unwrap();
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("設定を開く").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    let title = ja(
        "settings.title",
        &[("layer", &ja("settings.layer.user", &[]))],
    );
    harness.wait_until(|h| read(h).contains(&title)).unwrap();
    harness
}

/// The screen as it reads, cell by cell. A wide glyph's cell is followed by
/// a continuation cell, which is skipped: `一 般` reads `一般`.
fn read(harness: &EditorTestHarness) -> String {
    let buffer = harness.buffer();
    let mut out = String::new();
    for y in 0..buffer.area.height {
        let mut x = 0;
        while x < buffer.area.width {
            let symbol = buffer.content[buffer.index_of(x, y)].symbol();
            out.push_str(symbol);
            x += symbol.width().max(1) as u16;
        }
        out.push('\n');
    }
    out
}

fn select_category(harness: &mut EditorTestHarness, category: &str) {
    let selected = |screen: &str| {
        screen
            .lines()
            .flat_map(|l| l.split('│'))
            .any(|cell| cell.trim_start().starts_with('>') && cell.contains(category))
    };
    for _ in 0..30 {
        if selected(&read(harness)) {
            return;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    panic!("{category:?} never became selected:\n{}", read(harness));
}

/// Where the expanded `category` row sits: its screen row, and its column
/// among the dialog's `│`-separated columns.
fn expanded_category(screen: &str, category: &str) -> (usize, usize) {
    screen
        .lines()
        .enumerate()
        .find_map(|(row, l)| {
            l.split('│')
                .position(|cell| cell.contains('▼') && cell.contains(category))
                .map(|column| (row, column))
        })
        .unwrap_or_else(|| panic!("{category:?} is not expanded in the tree:\n{screen}"))
}

/// The trimmed cells of one `│`-separated column, from screen row `top` down.
fn column_labels(screen: &str, column: usize, top: usize) -> Vec<String> {
    screen
        .lines()
        .skip(top)
        .filter_map(|l| l.split('│').nth(column))
        .map(|cell| cell.trim().to_string())
        .collect()
}

#[test]
fn settings_labels_follow_the_locale() {
    let _pin = pin_config_globals();
    let harness = japanese_settings();
    let screen = read(&harness);

    for label in [
        "一般",
        "エディタ",
        // The first page is General: a setting's name and its description.
        "テーマ",
        "カラーテーマの名前",
    ] {
        assert!(
            screen.contains(label),
            "{label:?} is not on screen:\n{screen}"
        );
    }
    assert!(!screen.contains("Color theme name"), "{screen}");
}

/// Pages are laid out by the schema, not by the translated labels: sections
/// keep the order they have in English. Sorted by their Japanese names they
/// would come out in code-point order — `LSP` ahead of `括弧の対応`.
#[test]
fn sections_keep_their_english_order() {
    let _pin = pin_config_globals();
    let mut harness = japanese_settings();

    select_category(&mut harness, "エディタ");
    // Expanding the category lists its sections under it in the tree.
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| read(h).contains("空白文字"))
        .unwrap();

    let screen = read(&harness);
    // The same words also appear in the menu bar and on the page beside the
    // tree, so only the tree's column is read, from the category down.
    let (top, tree) = expanded_category(&screen, "エディタ");
    let labels = column_labels(&screen, tree, top);
    // "Bracket Matching" < "Completion" < "Keyboard" < "LSP" < "Whitespace".
    let order = ["括弧の対応", "補完", "キーボード", "LSP", "空白文字"];
    let rows: Vec<usize> = order
        .iter()
        .map(|s| {
            labels
                .iter()
                .position(|l| l == s)
                .unwrap_or_else(|| panic!("section {s:?} is not in the tree:\n{screen}"))
        })
        .collect();
    assert!(
        rows.windows(2).all(|w| w[0] < w[1]),
        "sections out of order {rows:?}:\n{screen}"
    );

    // The page draws its section headings separately from the tree.
    let page = column_labels(&screen, tree + 1, 0);
    assert!(
        page.iter().any(|l| l == "括弧の対応"),
        "the page has no translated section heading:\n{screen}"
    );
    assert!(
        !page.iter().any(|l| l == "Bracket Matching"),
        "the page still heads a section in English:\n{screen}"
    );
}

/// An entry dialog heads its sections too, on a path of its own: the fields
/// of an LSP server's Edit Item dialog end in an `Advanced` section.
#[test]
fn entry_dialog_sections_follow_the_locale() {
    let _pin = pin_config_globals();
    let mut harness = japanese_settings();

    // Search reaches the LSP map by its path, whatever the locale.
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.type_text("lsp").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    // Down to the python row, whose edit hint shows once it is focused.
    let focused = |h: &EditorTestHarness| {
        read(h)
            .lines()
            .any(|l| l.contains("python") && l.contains(&ja("settings.enter_to_edit", &[])))
    };
    for _ in 0..80 {
        if focused(&harness) {
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    assert!(
        focused(&harness),
        "python never focused:\n{}",
        read(&harness)
    );

    // Edit Value for python, then Edit Item for its first server.
    let edit = |what: &str| ja("settings.entry_edit_title", &[("name", &ja(what, &[]))]);
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    let edit_value = edit("settings.entry_value");
    harness
        .wait_until(|h| read(h).contains(&edit_value))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_until(|h| read(h).contains("[pylsp")).unwrap();

    let screen = read(&harness);
    assert!(screen.contains("── 詳細 ──"), "{screen}");
    assert!(!screen.contains("── Advanced ──"), "{screen}");
    // A list item's name is its own, not its key path's `*`: a catalog
    // entry for `settings.field.lsp.*.*` once titled this "Edit *".
    assert!(screen.contains(&edit("settings.entry_item")), "{screen}");
    // The dialog's own chrome follows the locale too.
    for english in ["Edit Item", "[ Save ]", "[+] Add new", "↑↓:Navigate"] {
        assert!(
            !screen.contains(english),
            "{english:?} is still English:\n{screen}"
        );
    }
    let save = format!("[ {} ]", ja("settings.btn_save", &[]));
    assert!(screen.contains(&save), "{screen}");
    assert!(screen.contains(&ja("settings.add_new", &[])), "{screen}");
}

/// The screen's own chrome — the search hint, the footer's layer button —
/// is translated, not only what the schema says.
#[test]
fn settings_chrome_follows_the_locale() {
    let _pin = pin_config_globals();
    let harness = japanese_settings();
    let screen = read(&harness);

    assert!(!screen.contains("to search settings"), "{screen}");
    let hint = ja("settings.search_hint", &[]);
    let after_key = hint.split("%{key}").last().unwrap().trim();
    assert!(
        screen.contains(after_key),
        "{after_key:?} missing:\n{screen}"
    );
    let layer = format!("[ {} ]", ja("settings.layer.user", &[]));
    assert!(screen.contains(&layer), "{layer:?} missing:\n{screen}");
}

/// A map's column header is the entry field's translated name, and a
/// changed entry dialog says so in the locale.
#[test]
fn map_header_and_dirty_dialog_follow_the_locale() {
    let _pin = pin_config_globals();
    let mut harness = japanese_settings();

    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.type_text("/languages").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    let grammar = ja("settings.field.languages.*.grammar", &[]);
    let header = |h: &EditorTestHarness| {
        read(h)
            .lines()
            .any(|l| l.contains(&ja("settings.map_name_column", &[])) && l.contains(&grammar))
    };
    harness.wait_until(header).unwrap();
    assert!(!read(&harness).contains("Grammar"), "{}", read(&harness));

    // Open the first language, then flip a checkbox.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    let title = ja(
        "settings.entry_edit_title",
        &[("name", &ja("settings.entry_value", &[]))],
    );
    harness.wait_until(|h| read(h).contains(&title)).unwrap();
    let modified = format!("• {}", ja("settings.modified", &[]));
    assert!(!read(&harness).contains(&modified), "{}", read(&harness));
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_until(|h| read(h).contains(&modified)).unwrap();
    assert!(!read(&harness).contains("• modified"), "{}", read(&harness));
}
