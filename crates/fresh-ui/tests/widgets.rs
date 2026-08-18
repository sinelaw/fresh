//! The widget set (plan phase L7).
//!
//! Every assertion here goes through the public event path — dispatch an input,
//! read the messages and the display list. Nothing reaches into the framework.

use fresh_ui::test::fake::Recorder;
use fresh_ui::{
    col, Button, ComponentExt, Draw, Dropdown, Input, KeyCode, KeyPress, List, Mods, MouseButton,
    Node, Number, Point, RadioGroup, Size, Sizing, TextField, Toggle, Tree, TreeNode, Ui,
};

const FRAME: Size = Size { w: 30, h: 10 };

#[derive(Debug, PartialEq, Eq, Clone)]
enum Msg {
    Pressed,
    Toggled(bool),
    Changed(String),
    Submitted,
    Selected(usize),
    Activated(usize),
    Chose(String),
    Number(i64),
    Opened(String),
}

fn click(ui: &mut Ui<Msg>, x: i32, y: i32) -> Vec<Msg> {
    let pos = Point::new(x, y);
    let mut out = ui.dispatch(Input::Press {
        pos,
        button: MouseButton::Left,
        mods: Mods::NONE,
    });
    out.extend(ui.dispatch(Input::Release {
        pos,
        button: MouseButton::Left,
        mods: Mods::NONE,
    }));
    out
}

fn key(ui: &mut Ui<Msg>, code: KeyCode) -> Vec<Msg> {
    ui.dispatch(Input::Key(KeyPress::new(code)))
}

fn texts(ui: &Ui<Msg>) -> Vec<String> {
    ui.spec()
        .items
        .iter()
        .filter_map(|i| match &i.draw {
            Draw::Lines(l) => Some(l.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("")),
            _ => None,
        })
        .collect()
}

fn themes_of(ui: &Ui<Msg>, needle: &str) -> Vec<String> {
    ui.spec()
        .items
        .iter()
        .filter(|i| matches!(&i.draw, Draw::Lines(l) if l.iter().any(|s| s.contains(needle))))
        .map(|i| i.theme.as_str().to_string())
        .collect()
}

// -- Button ------------------------------------------------------------------

#[test]
fn a_button_responds_to_the_pointer_and_to_the_keyboard() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(Button::new("Add").on_press(|_| Msg::Pressed).node(), FRAME);

    assert_eq!(click(&mut ui, 1, 0), vec![Msg::Pressed]);

    key(&mut ui, KeyCode::Tab);
    assert_eq!(key(&mut ui, KeyCode::Enter), vec![Msg::Pressed]);
}

#[test]
fn a_button_shows_that_it_has_focus() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(Button::new("Add").on_press(|_| Msg::Pressed).node(), FRAME);
    assert_eq!(themes_of(&ui, "Add"), vec!["button"]);

    key(&mut ui, KeyCode::Tab);
    ui.tick();
    assert_eq!(themes_of(&ui, "Add"), vec!["button.focused"]);
}

#[test]
fn a_disabled_button_neither_fires_nor_takes_focus() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        Button::new("Add")
            .on_press(|_| Msg::Pressed)
            .enabled(false)
            .node(),
        FRAME,
    );
    assert!(click(&mut ui, 1, 0).is_empty());
    key(&mut ui, KeyCode::Tab);
    assert_eq!(ui.focused(), None);
}

// -- Toggle ------------------------------------------------------------------

#[test]
fn a_toggle_reports_the_value_it_would_move_to() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        Toggle::new("wrap", false).on_change(Msg::Toggled).node(),
        FRAME,
    );
    assert!(texts(&ui).iter().any(|t| t.contains("[ ]")));
    assert_eq!(click(&mut ui, 1, 0), vec![Msg::Toggled(true)]);

    // Controlled: the owner decides, and hands the new value back down.
    ui.frame(
        Toggle::new("wrap", true).on_change(Msg::Toggled).node(),
        FRAME,
    );
    assert!(texts(&ui).iter().any(|t| t.contains("[x]")));
    assert_eq!(click(&mut ui, 1, 0), vec![Msg::Toggled(false)]);
}

// -- TextField ---------------------------------------------------------------

#[test]
fn a_text_field_edits_a_value_its_owner_holds() {
    let mut ui: Ui<Msg> = Ui::new();
    let field = |v: &str| -> Node<Msg> {
        TextField::new(v)
            .on_change(Msg::Changed)
            .on_submit(|_| Msg::Submitted)
            .node()
    };

    ui.frame(field(""), FRAME);
    key(&mut ui, KeyCode::Tab);

    assert_eq!(
        key(&mut ui, KeyCode::Char('h')),
        vec![Msg::Changed("h".into())]
    );
    // The owner applies the change and hands the new value back down.
    ui.frame(field("h"), FRAME);
    assert_eq!(
        key(&mut ui, KeyCode::Char('i')),
        vec![Msg::Changed("hi".into())]
    );
    ui.frame(field("hi"), FRAME);

    assert_eq!(
        key(&mut ui, KeyCode::Backspace),
        vec![Msg::Changed("h".into())]
    );
    ui.frame(field("h"), FRAME);
    assert_eq!(key(&mut ui, KeyCode::Enter), vec![Msg::Submitted]);
}

#[test]
fn a_text_field_shows_its_placeholder_while_empty() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(TextField::new("").placeholder("search…").node(), FRAME);
    assert!(texts(&ui).iter().any(|t| t.contains("search")));
}

#[test]
fn the_caret_moves_without_changing_the_value() {
    let mut ui: Ui<Msg> = Ui::new();
    let field = |v: &str| -> Node<Msg> { TextField::new(v).on_change(Msg::Changed).node() };
    ui.frame(field("abc"), FRAME);
    key(&mut ui, KeyCode::Tab);
    // A frame is rendered between inputs, as an event loop does: descriptions
    // capture the state they were built from, so the caret the next handler
    // sees is the one the last build published.
    ui.frame(field("abc"), FRAME);

    assert!(key(&mut ui, KeyCode::Left).is_empty());
    ui.frame(field("abc"), FRAME);
    // The caret is now before 'c'; typing lands there.
    assert_eq!(
        key(&mut ui, KeyCode::Char('X')),
        vec![Msg::Changed("abXc".into())]
    );
}

// -- Number ------------------------------------------------------------------

#[test]
fn a_number_clamps_to_its_range() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        Number::new(9).range(0, 10).on_change(Msg::Number).node(),
        FRAME,
    );
    key(&mut ui, KeyCode::Tab);
    assert_eq!(key(&mut ui, KeyCode::Up), vec![Msg::Number(10)]);

    ui.frame(
        Number::new(10).range(0, 10).on_change(Msg::Number).node(),
        FRAME,
    );
    assert_eq!(key(&mut ui, KeyCode::Up), vec![Msg::Number(10)], "clamped");
    assert_eq!(key(&mut ui, KeyCode::Down), vec![Msg::Number(9)]);
}

// -- List --------------------------------------------------------------------

fn eager_list(selected: usize) -> Node<Msg> {
    let items: Vec<usize> = (0..6).collect();
    List::keyed(
        &items,
        |i| fresh_ui::Key::from(*i),
        |i| fresh_ui::text(format!("item {i}")),
    )
    .selected(selected)
    .on_select(Msg::Selected)
    .on_activate(Msg::Activated)
    .node()
}

#[test]
fn a_list_moves_its_selection_and_activates_it() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(eager_list(0), FRAME);
    key(&mut ui, KeyCode::Tab);

    assert_eq!(key(&mut ui, KeyCode::Down), vec![Msg::Selected(1)]);
    ui.frame(eager_list(1), FRAME);
    assert_eq!(key(&mut ui, KeyCode::Enter), vec![Msg::Activated(1)]);
    assert_eq!(key(&mut ui, KeyCode::End), vec![Msg::Selected(5)]);
    ui.frame(eager_list(5), FRAME);
    assert_eq!(key(&mut ui, KeyCode::Home), vec![Msg::Selected(0)]);
}

#[test]
fn the_selected_row_is_marked_in_the_display_list() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(eager_list(2), FRAME);
    assert_eq!(themes_of(&ui, "item 2"), vec!["list.row.selected"]);
    assert_eq!(themes_of(&ui, "item 3"), vec!["list.row"]);
}

#[test]
fn a_list_scrolls_to_keep_the_selection_visible() {
    let mut ui: Ui<Msg> = Ui::new();
    let small = Size::new(20, 3);
    ui.frame(eager_list(0), small);
    assert!(texts(&ui).iter().any(|t| t == "item 0"));

    ui.frame(eager_list(5), small);
    let shown = texts(&ui);
    assert!(shown.iter().any(|t| t == "item 5"), "{shown:?}");
    assert!(!shown.iter().any(|t| t == "item 0"), "{shown:?}");
}

#[test]
fn a_million_row_list_does_a_screenful_of_work_per_frame() {
    const N: usize = 1_000_000;
    let recorder = Recorder::new();
    let mut ui: Ui<Msg> = Ui::with_renderer(Box::new(recorder.clone()));

    let list = || -> Node<Msg> {
        List::windowed(N, fresh_ui::Key::from, |i| {
            fresh_ui::text(format!("row {i}"))
        })
        .on_activate(Msg::Activated)
        .node()
    };

    ui.frame(list(), FRAME);
    let (created, _, _) = recorder.counts();
    assert!(
        created < 100,
        "mounting a million rows created {created} elements"
    );
    assert!(ui.live_count() < 100, "{} elements exist", ui.live_count());

    // Scrolling does not change the order of the work.
    recorder.clear();
    ui.dispatch(Input::Wheel {
        pos: Point::new(1, 1),
        delta: 40,
        mods: Mods::NONE,
    });
    ui.tick();
    let (c, u, d) = recorder.counts();
    assert!(c + u + d < 200, "a scroll touched {c}/{u}/{d} elements");
    assert!(
        texts(&ui).iter().any(|t| t.starts_with("row 4")),
        "{:?}",
        texts(&ui)
    );
}

// -- Tree --------------------------------------------------------------------

#[test]
fn a_tree_expands_and_collapses_on_click() {
    let mut ui: Ui<Msg> = Ui::new();
    let tree = || -> Node<Msg> {
        Tree::new(vec![TreeNode::branch(
            "src",
            fresh_ui::text("src"),
            vec![
                TreeNode::leaf("main", fresh_ui::text("main.rs")),
                TreeNode::leaf("lib", fresh_ui::text("lib.rs")),
            ],
        )])
        .node()
    };
    ui.frame(tree(), FRAME);
    assert!(!texts(&ui).iter().any(|t| t.contains("main.rs")));

    click(&mut ui, 2, 0);
    ui.tick();
    let shown = texts(&ui).join("|");
    assert!(
        shown.contains("main.rs") && shown.contains("lib.rs"),
        "{shown}"
    );

    click(&mut ui, 2, 0);
    ui.tick();
    assert!(!texts(&ui).iter().any(|t| t.contains("main.rs")));
}

// -- Dropdown ----------------------------------------------------------------

#[test]
fn a_dropdown_opens_on_press_and_dismisses_on_a_click_outside() {
    let mut ui: Ui<Msg> = Ui::new();
    let menu = || -> Node<Msg> {
        col().child(
            Dropdown::new("File")
                .item("open", "Open")
                .item("save", "Save")
                .on_choose(|k| Msg::Chose(format!("{k}")))
                .node(),
        )
    };

    ui.frame(menu(), FRAME);
    assert!(!texts(&ui).iter().any(|t| t.contains("Open")));

    click(&mut ui, 1, 0);
    ui.tick();
    assert!(
        texts(&ui).iter().any(|t| t.contains("Open")),
        "{:?}",
        texts(&ui)
    );

    // Outside the layer: the dropdown is told to close, and closes itself.
    click(&mut ui, 25, 8);
    ui.tick();
    assert!(!texts(&ui).iter().any(|t| t.contains("Open")));
}

#[test]
fn a_dropdown_reports_the_item_that_was_chosen() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        col().child(
            Dropdown::new("File")
                .item("open", "Open")
                .item("save", "Save")
                .on_choose(|k| Msg::Chose(format!("{k}")))
                .node(),
        ),
        FRAME,
    );
    click(&mut ui, 1, 0);
    ui.tick();

    // The open menu autofocuses nothing, so drive it the way a user would:
    // move into the list, then confirm.
    let list = ui
        .spec()
        .items
        .iter()
        .find(|i| matches!(&i.draw, Draw::Lines(l) if l.iter().any(|s| s.contains("Save"))))
        .map(|i| i.id)
        .expect("the Save row");
    let _ = list;
    key(&mut ui, KeyCode::Tab);
    key(&mut ui, KeyCode::Tab);
    let chosen = key(&mut ui, KeyCode::Enter);
    assert_eq!(chosen, vec![Msg::Chose("#open".into())]);
}

// -- RadioGroup --------------------------------------------------------------

#[test]
fn a_radio_group_marks_the_selected_option() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        RadioGroup::new()
            .option("light", "Light")
            .option("dark", "Dark")
            .selected("dark")
            .on_change(|k| Msg::Opened(format!("{k}")))
            .node(),
        FRAME,
    );
    let shown = texts(&ui).join("|");
    assert!(
        shown.contains("( ) Light") || shown.contains("( )"),
        "{shown}"
    );
    assert!(shown.contains("(o)"), "{shown}");
}

// -- composition -------------------------------------------------------------

#[test]
fn widgets_compose_into_a_form_that_tabs_in_order() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        col().children([
            TextField::new("name").on_change(Msg::Changed).node(),
            Toggle::new("enabled", true).on_change(Msg::Toggled).node(),
            Button::new("Save")
                .on_press(|_| Msg::Pressed)
                .node()
                .h(Sizing::Cells(1)),
        ]),
        FRAME,
    );

    key(&mut ui, KeyCode::Tab);
    let first = ui.focused();
    key(&mut ui, KeyCode::Tab);
    let second = ui.focused();
    key(&mut ui, KeyCode::Tab);
    let third = ui.focused();
    assert!(first != second && second != third);

    assert_eq!(
        key(&mut ui, KeyCode::Enter),
        vec![Msg::Pressed],
        "the button is last"
    );
}
