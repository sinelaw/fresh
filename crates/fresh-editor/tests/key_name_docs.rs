//! The documented key names are generated from the code that parses them.
//!
//! `docs/configuration/keyboard.md` used to say nothing at all about which
//! spellings `"key"` accepts, and `docs/features/keybinding-editor.md` offered
//! four examples. A name nobody can find is a name nobody uses — issue #1128
//! was filed by someone who guessed `asterisk` and `kp_multiply` and got
//! silence — so the list is rendered from the tables themselves and this test
//! fails when the two drift.
//!
//! Regenerate with:
//!
//! ```sh
//! UPDATE_DOCS=1 cargo test -p fresh-editor --test all_tests -- key_name_docs::
//! ```

use std::fs;
use std::path::PathBuf;

use crossterm::event::KeyCode;
use fresh::input::keybindings::{KeyName, NAMED_KEYS, PUNCTUATION_KEYS};
use fresh_input_parser::keypad::KEYPAD_KEYS;

const BEGIN: &str = "<!-- BEGIN GENERATED KEY NAMES -->";
const END: &str = "<!-- END GENERATED KEY NAMES -->";

fn doc_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../docs/configuration/keyboard.md")
}

/// Wrap `s` as a markdown code span that survives a table cell.
///
/// Two hazards: a literal backtick needs a wider fence and padding spaces, and
/// a `|` ends the cell even inside a code span, so it is escaped.
fn code_span(s: &str) -> String {
    let span = if s.contains('`') {
        format!("`` {s} ``")
    } else {
        format!("`{s}`")
    };
    span.replace('|', "\\|")
}

/// A reader-facing description of what a `KeyCode` is.
fn describe(code: KeyCode) -> String {
    match code {
        KeyCode::Char(' ') => "the space bar".to_string(),
        KeyCode::Char(c) => code_span(&c.to_string()),
        other => code_span(&format!("{other:?}")),
    }
}

fn names_cell(k: &KeyName) -> String {
    k.names
        .iter()
        .map(|n| code_span(n))
        .collect::<Vec<_>>()
        .join(", ")
}

fn render() -> String {
    let mut out = String::new();
    out.push_str(BEGIN);
    out.push_str("\n\n");
    out.push_str(
        "*This section is generated from the key tables in the source. \
         Edit those, not this text — see `crates/fresh-editor/tests/key_name_docs.rs`.*\n\n",
    );

    out.push_str("### Named keys\n\n");
    out.push_str("| Name | Key |\n|---|---|\n");
    for k in NAMED_KEYS {
        out.push_str(&format!("| {} | {} |\n", names_cell(k), describe(k.code)));
    }

    out.push_str("\nAny single character is also a key name — `\"a\"`, `\"7\"`, `\"é\"` — \
                  as is a function key, `\"f1\"` through `\"f24\"`. Names are \
                  case-insensitive.\n\n");

    out.push_str("### Punctuation\n\n");
    out.push_str(
        "The single character is always accepted and is what the keybinding editor \
         writes back. These X11 keysym spellings are accepted too, for the keys that \
         are awkward to write literally in JSON.\n\n",
    );
    out.push_str("| Name | Character |\n|---|---|\n");
    for k in PUNCTUATION_KEYS {
        out.push_str(&format!("| {} | {} |\n", names_cell(k), describe(k.code)));
    }

    out.push_str("\n### Numeric keypad\n\n");
    out.push_str(
        "**These are aliases, not separate keys.** A terminal reports the keypad using \
         the same code as the main keyboard, so binding `kp_multiply` also binds `*`, \
         and binding `kp_enter` also binds `Enter`. There is no way to tell the two \
         apart at this layer. `kp_begin` — the `5` key with Num Lock off — is the one \
         exception: nothing on the main keyboard sends it.\n\n",
    );
    out.push_str("| Name | Binds the same key as |\n|---|---|\n");
    for k in KEYPAD_KEYS {
        out.push_str(&format!(
            "| {} | {} |\n",
            code_span(k.keysym),
            describe(k.code)
        ));
    }

    out.push('\n');
    out.push_str(END);
    out
}

#[test]
fn generated_key_name_docs_match_the_code() {
    let path = doc_path();
    let doc = fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("cannot read {}: {e}", path.display()));

    let expected = render();

    let (Some(start), Some(end)) = (doc.find(BEGIN), doc.find(END)) else {
        panic!(
            "{} has no generated key-name block. Add\n\n{BEGIN}\n{END}\n\nand rerun with \
             UPDATE_DOCS=1.",
            path.display()
        );
    };
    let end = end + END.len();
    let current = &doc[start..end];

    if current == expected {
        return;
    }

    if std::env::var_os("UPDATE_DOCS").is_some() {
        let updated = format!("{}{}{}", &doc[..start], expected, &doc[end..]);
        fs::write(&path, updated).unwrap();
        return;
    }

    panic!(
        "the key names in {} are out of date with the tables in \
         `input/keybindings.rs` and `fresh-input-parser`'s `keypad`.\n\n\
         Regenerate:\n  UPDATE_DOCS=1 cargo test -p fresh-editor --test all_tests -- \
         key_name_docs::\n\n--- documented ---\n{current}\n\n--- expected ---\n{expected}",
        path.display()
    );
}
