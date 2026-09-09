//! Diagnostics (plan phase L2c): the dump answers what is mounted, how often it
//! built, and what marked it.

use std::rc::Rc;

use fresh_ui::{
    col, provide, text, Ambient, BuildCx, Component, ComponentExt, DirtyCause, Node, Ui,
};

#[derive(Default, Debug)]
struct Count {
    n: u32,
}

struct Row;

impl Component<()> for Row {
    type State = Count;

    fn build(&self, s: &Count, _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        text(format!("n={}", s.n))
    }

    fn describe_state(&self, s: &Count) -> Option<String> {
        Some(format!("n={}", s.n))
    }
}

struct Screen;

impl Component<()> for Screen {
    type State = ();
    fn build(&self, _s: &(), _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        col().child(Row.node().key(("row", 1usize)))
    }
}

#[test]
fn the_dump_names_the_type_key_state_build_count_and_cause() {
    let mut ui: Ui<()> = Ui::new();
    ui.reconcile(Screen.node());
    let row = ui.at(&[0, 0]).unwrap();

    ui.set_state::<Count>(row, |s| s.n = 3);
    ui.flush();

    let dump = ui.dump();
    assert!(dump.contains("Row #row:1"), "type and key\n{dump}");
    assert!(dump.contains("state=Count(n=3)"), "live state\n{dump}");
    assert!(dump.contains("builds=2"), "rebuild counter\n{dump}");
    assert!(
        // Not "tests/diagnose.rs": `file!()` spells the separator the host
        // platform's way, and the claim here is that the site is named.
        dump.contains("cause=set_state") && dump.contains("diagnose.rs:"),
        "the set_state site that marked it\n{dump}"
    );
    assert!(
        dump.contains("cause=parent"),
        "and why the rest rebuilt\n{dump}"
    );

    // The same facts are readable programmatically.
    assert_eq!(ui.builds(row), 2);
    assert!(matches!(ui.last_dirty(row), Some(DirtyCause::SetState(_))));
}

#[test]
fn the_dump_attributes_an_ambient_change_to_the_ambient() {
    static THEME: Ambient<&'static str> = Ambient::new("theme");

    struct Themed;
    impl Component<()> for Themed {
        type State = ();
        fn build(&self, _s: &(), cx: &mut BuildCx<'_, ()>) -> Node<()> {
            text(*cx.read(&THEME).unwrap_or_else(|| Rc::new("none")))
        }
    }

    let mut ui: Ui<()> = Ui::new();
    let value = Rc::new("dark");
    ui.reconcile(provide(&THEME, value.clone(), Themed.node()));
    let themed = ui.at(&[0]).unwrap();

    // Change the value without re-supplying the root: the only thing that can
    // mark the reader is its ambient dependency.
    ui.reconcile(provide(&THEME, Rc::new("light"), Themed.node()));

    assert_eq!(ui.last_dirty(themed), Some(DirtyCause::Ambient("theme")));
    assert!(ui.dump().contains("cause=ambient @theme"), "{}", ui.dump());
    assert_eq!(
        ui.text_of(ui.at(&[0, 0]).unwrap()).as_deref(),
        Some("light")
    );
}

#[test]
fn shape_is_the_dump_without_the_volatile_parts() {
    let mut ui: Ui<()> = Ui::new();
    ui.reconcile(Screen.node());
    let before = ui.shape();

    let row = ui.at(&[0, 0]).unwrap();
    ui.set_state::<Count>(row, |s| s.n += 1);
    ui.flush();

    assert_eq!(
        ui.shape(),
        before,
        "structure is unchanged by a state change"
    );
    assert_ne!(ui.dump(), before);
    assert_eq!(before, "Screen\n  Box\n    Row #row:1\n      TextRun\n");
}

#[test]
fn dump_json_carries_the_same_facts_in_a_shape_a_program_can_read() {
    let mut ui: Ui<()> = Ui::new();
    ui.reconcile(Screen.node());
    let row = ui.at(&[0, 0]).unwrap();
    ui.set_state::<Count>(row, |s| s.n = 3);
    ui.flush();

    let j = ui.dump_json();
    for needle in [
        r#""type": "Row""#,
        r#""key": "row:1""#, // the `#` sigil belongs to the text dump
        r#""state": "Count""#,
        r#""state_detail": "n=3""#,
        r#""builds": 2"#,
        r#""text": "n=3""#,
        r#""rect": {"x": 0, "y": 0,"#,
    ] {
        assert!(j.contains(needle), "missing {needle} in:\n{j}");
    }
    assert!(
        j.contains(r#""cause": "set_state"#),
        "the marking site\n{j}"
    );

    // No serde here — `fresh-ui` depends on `unicode-width` and nothing else —
    // so check well-formedness the way this crate can: brackets balance outside
    // strings, and every leaf closes its `children`.
    let (mut curly, mut square, mut in_str, mut esc) = (0i32, 0i32, false, false);
    for c in j.chars() {
        match (in_str, esc, c) {
            (true, true, _) => esc = false,
            (true, false, '\\') => esc = true,
            (true, false, '"') => in_str = false,
            (true, false, _) => {}
            (false, _, '"') => in_str = true,
            (false, _, '{') => curly += 1,
            (false, _, '}') => curly -= 1,
            (false, _, '[') => square += 1,
            (false, _, ']') => square -= 1,
            _ => {}
        }
        assert!(curly >= 0 && square >= 0, "closed too early\n{j}");
    }
    assert_eq!((curly, square, in_str), (0, 0, false), "unbalanced\n{j}");
}

#[test]
fn dump_json_escapes_text_that_would_otherwise_break_the_document() {
    struct Awkward;
    impl Component<()> for Awkward {
        type State = ();
        fn build(&self, _s: &(), _cx: &mut BuildCx<'_, ()>) -> Node<()> {
            text("a \"quoted\" \\ path\tand\na newline")
        }
    }
    let mut ui: Ui<()> = Ui::new();
    ui.reconcile(Awkward.node());
    assert!(
        ui.dump_json()
            .contains(r#""text": "a \"quoted\" \\ path\tand\na newline""#),
        "{}",
        ui.dump_json()
    );
}
