//! The widget set (plan phase L7).
//!
//! Every assertion here goes through the public event path — dispatch an input,
//! read the messages and the display list. Nothing reaches into the framework.

mod support;
use fresh_ui::Axis;
use fresh_ui::{
    col, Button, ComponentExt, Draw, Dropdown, Input, KeyCode, KeyPress, List, Mods, MouseButton,
    Node, Number, Point, RadioGroup, Size, Sizing, TextField, Toggle, Tree, TreeNode, Ui,
};
use support::fake::Recorder;

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
    let mut out = ui
        .dispatch(Input::press(pos, MouseButton::Left, Mods::NONE))
        .msgs;
    out.extend(
        ui.dispatch(Input::release(pos, MouseButton::Left, Mods::NONE))
            .msgs,
    );
    out
}

fn key(ui: &mut Ui<Msg>, code: KeyCode) -> Vec<Msg> {
    ui.dispatch(Input::Key(KeyPress::new(code))).msgs
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

/// **The third state a checkbox has wherever a value can be unset.** A
/// definite `[ ]` there reads as "the user turned this off", which is a
/// different fact and usually the wrong one.
#[test]
fn an_indeterminate_toggle_shows_neither_on_nor_off() {
    let mut ui: Ui<Msg> = Ui::new();
    for value in [false, true] {
        ui.frame(
            Toggle::new("wrap", value)
                .indeterminate(true)
                .on_change(Msg::Toggled)
                .node(),
            FRAME,
        );
        assert!(
            texts(&ui).iter().any(|t| t.contains("[-]")),
            "the mark does not depend on the value it is hiding (value={value})"
        );
    }
}

/// It is display only, and the toggle still reports `!value`. What leaving
/// the unset state *means* is the owner's question — "inherit" resolves to on
/// for some fields and off for others — so the widget does not guess.
#[test]
fn an_indeterminate_toggle_still_reports_the_flip() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        Toggle::new("wrap", false)
            .indeterminate(true)
            .on_change(Msg::Toggled)
            .node(),
        FRAME,
    );
    assert_eq!(click(&mut ui, 1, 0), vec![Msg::Toggled(true)]);
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
    // The list has no focus here, so its selected row reads as the blurred
    // variant — a list shows a vivid selection only while it has focus, so the
    // eye can tell which of several lists the keyboard is driving.
    assert_eq!(themes_of(&ui, "item 2"), vec!["list.row.selected.blur"]);
    assert_eq!(themes_of(&ui, "item 3"), vec!["list.row"]);
}

/// **A controlled selection can be empty, and an empty one marks nothing.**
///
/// `selected(i)` says two things at once — "the owner holds the selection" and
/// "it is on row i" — so an owner whose selection is empty could only omit it,
/// which hands the selection back to the element and its own starts at row
/// zero. A one-row list that is only selected when the keyboard is on it (a
/// settings field's `[+] Add new` sentinel) then looked selected always.
#[test]
fn a_controlled_empty_selection_marks_no_row() {
    let items: Vec<usize> = (0..6).collect();
    let list = |sel: Option<usize>| {
        List::keyed(
            &items,
            |i| fresh_ui::Key::from(*i),
            |i| fresh_ui::text(format!("item {i}")),
        )
        .selection(sel)
        .on_select(Msg::Selected)
        .on_activate(Msg::Activated)
        .node()
    };
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(list(None), FRAME);
    for i in 0..6 {
        assert_eq!(
            themes_of(&ui, &format!("item {i}")),
            vec!["list.row"],
            "row {i} must not be marked while the selection is empty"
        );
    }
    // Confirm has nothing to confirm; the arrows start a walk from either end.
    key(&mut ui, KeyCode::Tab);
    assert_eq!(key(&mut ui, KeyCode::Enter), Vec::<Msg>::new());
    assert_eq!(key(&mut ui, KeyCode::Down), vec![Msg::Selected(0)]);
    ui.frame(list(None), FRAME);
    assert_eq!(key(&mut ui, KeyCode::Up), vec![Msg::Selected(5)]);
    // And a selection that is `Some` still marks its row.
    ui.frame(list(Some(2)), FRAME);
    assert_eq!(themes_of(&ui, "item 2"), vec!["list.row.selected"]);
}

/// **A host names its own row appearance.** The stamped vocabulary
/// (`list.row.selected` and the rest) overwrites whatever the row builder set,
/// so a host migrating a surface that already has theme names had no way to
/// keep them — and it cannot compute the name itself either, because `hovered`
/// and `focused` live in `ListState`. `row_theme` hands out the state instead
/// of the name.
#[test]
fn a_host_can_name_each_row_state_itself() {
    let items: Vec<usize> = (0..6).collect();
    let list = List::keyed(
        &items,
        |i| fresh_ui::Key::from(*i),
        |i| fresh_ui::text(format!("item {i}")),
    )
    .selected(2)
    .row_theme(|i, st| match st {
        fresh_ui::widgets::RowState::Normal => format!("mine.plain.{i}"),
        fresh_ui::widgets::RowState::Selected => "mine.on".into(),
        fresh_ui::widgets::RowState::SelectedBlur => "mine.on.blur".into(),
        fresh_ui::widgets::RowState::Hover => "mine.hover".into(),
    })
    .node();
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(list, FRAME);
    assert_eq!(themes_of(&ui, "item 2"), vec!["mine.on.blur"]);
    assert_eq!(themes_of(&ui, "item 3"), vec!["mine.plain.3"]);
}

/// **A row under the pointer says so on the next frame.**
///
/// `row_theme` is handed [`RowState::Hover`], but nothing drove a pointer over
/// a row and re-framed to see it: the widget's own `Enter`/`Leave` handlers
/// write `ListState::hovered`, and a write that never survives to the next
/// build is a highlight nobody sees. The editor's completion popup was the
/// symptom — every row read `Normal` however the pointer moved.
#[test]
fn a_row_under_the_pointer_reads_as_hovered() {
    let items: Vec<usize> = (0..6).collect();
    let list = || {
        List::keyed(
            &items,
            |i| fresh_ui::Key::from(*i),
            |i| fresh_ui::text(format!("item {i}")),
        )
        .selected(0)
        .row_theme(|i, st| match st {
            fresh_ui::widgets::RowState::Hover => "mine.hover".into(),
            _ => format!("mine.plain.{i}"),
        })
        .node()
    };
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(list(), FRAME);
    let at = ui.rect_of(ui.find_by_key(&fresh_ui::Key::from(3u64)).expect("row 3"));

    ui.dispatch(Input::Move {
        pos: Point::new(at.x, at.y),
        mods: Mods::NONE,
    });
    ui.frame(list(), FRAME);
    assert_eq!(themes_of(&ui, "item 3"), vec!["mine.hover"]);
    assert_eq!(themes_of(&ui, "item 4"), vec!["mine.plain.4"]);

    // And leaving takes it back: a hover is where the pointer is now, not
    // where it has ever been.
    ui.dispatch(Input::Move {
        pos: Point::new(at.x, at.y + 1),
        mods: Mods::NONE,
    });
    ui.frame(list(), FRAME);
    assert_eq!(themes_of(&ui, "item 3"), vec!["mine.plain.3"]);
    assert_eq!(themes_of(&ui, "item 4"), vec!["mine.hover"]);
}

/// **Which click commits is the host's rule, not the widget's.**
///
/// `on_activate` fired on the first click and won over `on_select`, so a list
/// that wants select-then-open — a file browser, the editor's suggestion list
/// with its double-click confirm — could not have both: setting the two
/// handlers confirmed on every click. The click run is already carried to the
/// handler on `Event::clicks`; this is only a matter of the widget consulting
/// it.
#[test]
fn a_double_click_list_selects_first_and_activates_second() {
    use fresh_ui::widgets::Activate;
    let items: Vec<usize> = (0..6).collect();
    let list = || {
        List::keyed(
            &items,
            |i| fresh_ui::Key::from(*i),
            |i| fresh_ui::text(format!("item {i}")),
        )
        .selected(0)
        .on_select(Msg::Selected)
        .on_activate(Msg::Activated)
        .activate_on(Activate::DoubleClick)
        .node()
    };
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(list(), FRAME);
    let at = ui.rect_of(ui.find_by_key(&fresh_ui::Key::from(2u64)).expect("row 2"));
    let p = Point::new(at.x, at.y);

    let mut click = |n: u8| {
        let mut out = ui
            .dispatch(Input::press_n(p, MouseButton::Left, Mods::NONE, n))
            .msgs;
        out.extend(
            ui.dispatch(Input::release(p, MouseButton::Left, Mods::NONE))
                .msgs,
        );
        out
    };
    assert_eq!(click(1), vec![Msg::Selected(2)], "the first click selects");
    assert_eq!(click(2), vec![Msg::Activated(2)], "the second activates");
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
        axis: Axis::Vertical,
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

// -- Controlled dropdown and global mnemonics --------------------------------

#[test]
fn a_controlled_dropdown_opens_from_its_owner() {
    // The open state is held by the owner, not the element: a menu a global
    // command must be able to open needs its flag somewhere a command can reach.
    let mut ui: Ui<Msg> = Ui::new();
    let build = |open: bool| -> Node<Msg> {
        col().child(
            Dropdown::new("File")
                .item("open", "Open")
                .item("save", "Save")
                .open(open)
                .on_toggle(|now| Msg::Chose(format!("toggle:{now}")))
                .on_choose(|k| Msg::Chose(format!("{k}")))
                .node(),
        )
    };

    // Closed: no menu, and nothing was clicked.
    ui.frame(build(false), FRAME);
    assert!(!texts(&ui).iter().any(|t| t.contains("Open")));

    // The owner sets the flag and the menu appears — no pointer involved.
    ui.frame(build(true), FRAME);
    assert!(texts(&ui).iter().any(|t| t.contains("Open")));

    // Clicking the trigger does not flip a private flag; it reports the toggle
    // the owner should record.
    assert_eq!(
        click(&mut ui, 1, 0),
        vec![Msg::Chose("toggle:false".into())]
    );
}

#[test]
fn a_global_alt_shortcut_reaches_a_root_action_from_anywhere() {
    use fresh_ui::desc::focusable;
    use fresh_ui::focus::{Intent, Shortcut};

    let mut ui: Ui<Msg> = Ui::new();
    ui.set_shortcuts(vec![Shortcut::new(
        KeyPress::with(KeyCode::Char('f'), Mods::ALT),
        Intent::Custom("menu.file"),
    )]);

    // A root focusable that traversal skips, catching the app-global intent no
    // more specific part of the tree claimed — the idiom for a menu mnemonic.
    let view = || -> Node<Msg> {
        focusable(col().child(TextField::new("body").on_change(Msg::Changed).node()))
            .skip_traversal()
            .action(Intent::Custom("menu.file"), |_| Msg::Chose("file".into()))
    };
    ui.frame(view(), FRAME);

    // Focus is on the field (typing works there), yet the chord still fires the
    // root action rather than being swallowed as input.
    ui.dispatch(Input::Key(KeyPress::new(KeyCode::Tab)));
    let chord = ui.dispatch(Input::Key(KeyPress::with(KeyCode::Char('f'), Mods::ALT)));
    assert_eq!(chord, vec![Msg::Chose("file".into())]);
}

#[test]
fn a_scrollbar_sits_in_a_gutter_the_content_does_not_cover() {
    // Many rows in a short window: the list overflows, so a scrollbar appears.
    // The regression this guards: a node paints under its children, so a
    // full-width row background used to erase the scrollbar. The viewport now
    // insets its content by one column, leaving the bar a gutter of its own.
    let list = |n: usize| -> Node<Msg> {
        List::windowed(n, fresh_ui::Key::from, |i| {
            fresh_ui::col()
                .theme("list.row")
                .child(fresh_ui::text(format!("row {i}")))
        })
        .scrollbar()
        .node()
    };
    let frame = Size::new(20, 5);
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(list(500), frame);

    let bar = ui
        .spec()
        .items
        .iter()
        .find(|i| matches!(i.draw, Draw::Scrollbar { .. }))
        .expect("an overflowing list shows a scrollbar");
    let col = bar.rect.x;
    assert_eq!(col, frame.w as i32 - 1, "the bar sits in the last column");

    // No fill or text row overlaps that column: the gutter is the bar's alone.
    let overlap = ui.spec().items.iter().any(|i| {
        matches!(i.draw, Draw::Fill | Draw::Lines(_)) && i.rect.x <= col && i.rect.right() > col
    });
    assert!(!overlap, "content must not cover the scrollbar gutter");
}

/// **A backend clips a run to the item's own rectangle.** Layout gives a
/// constrained node the width it was *allowed*, not the width its content
/// wants, so a `Draw::Lines` run can be longer than the rect that carries it.
/// A backend that writes the string and honours only the inherited clip paints
/// straight through whatever encloses the node — a menu row through its own
/// border, a status segment through the segment beside it.
///
/// The reference backend in `support::screen` is the contract; the interactive
/// example's fold does the same.
#[test]
fn a_run_longer_than_its_item_is_clipped_to_it() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col()
            .w(Sizing::Cells(6))
            .child(fresh_ui::text("0123456789").w(Sizing::Cells(4))),
        Size { w: 8, h: 2 },
    );
    let screen = support::screen::render(spec);
    assert_eq!(
        screen.line(0),
        "0123    ",
        "four columns were given, four were painted"
    );
}

/// **A stable gutter is there whether the bar is or not.**
///
/// Without it the column appears with the bar and goes with it, so a list that
/// grows past its window reflows its content by a cell — and a window whose
/// gutter is part of the frame around it gets the bar drawn *beside* that
/// frame instead of on it, because the column the bar wants is one the frame
/// already owns.
#[test]
fn a_stable_gutter_reserves_its_column_with_no_bar_to_put_in_it() {
    let list = |n: usize| -> Node<Msg> {
        List::windowed(n, fresh_ui::Key::from, |i| {
            fresh_ui::col()
                .theme("list.row")
                .child(fresh_ui::text(format!("row {i}")))
        })
        .scrollbar_gutter()
        .node()
    };
    let frame = Size::new(20, 5);

    // Short enough to fit: no bar is drawn.
    let mut short: Ui<Msg> = Ui::new();
    short.frame(list(3), frame);
    assert!(
        !short
            .spec()
            .items
            .iter()
            .any(|i| matches!(i.draw, Draw::Scrollbar { .. })),
        "a list that fits draws no bar"
    );

    // Long enough to overflow: a bar appears in the last column.
    let mut long: Ui<Msg> = Ui::new();
    long.frame(list(500), frame);
    let bar = long
        .spec()
        .items
        .iter()
        .find(|i| matches!(i.draw, Draw::Scrollbar { .. }))
        .expect("an overflowing list shows a bar");
    assert_eq!(bar.rect.x, frame.w as i32 - 1);

    // And the rows are the same width either way: the gutter did not move.
    let row_width = |ui: &Ui<Msg>| {
        ui.spec()
            .items
            .iter()
            .filter(|i| matches!(i.draw, Draw::Fill))
            .map(|i| i.rect.w)
            .max()
            .expect("rows paint their ground")
    };
    assert_eq!(
        row_width(&short),
        row_width(&long),
        "content must not reflow when the bar appears"
    );
    assert_eq!(row_width(&short), frame.w - 1, "the gutter is not content");
}

/// **An overlay bar: there, and not drawn.**
///
/// A window whose bar comes and goes is answering a question the window
/// cannot ask — is anyone looking — so the caller answers it, once per frame.
/// What the window owes in return is that nothing else moves: an overlay bar
/// carves no gutter and floats over the last column, so the rows are the same
/// width whether it is showing or not.
#[test]
fn a_revealed_bar_comes_and_goes_without_moving_the_rows() {
    let list = |shown: bool| -> Node<Msg> {
        List::windowed(500, fresh_ui::Key::from, |i| {
            fresh_ui::col()
                .theme("list.row")
                .child(fresh_ui::text(format!("row {i}")))
        })
        .scrollbar_revealed(shown)
        .node()
    };
    let frame = Size::new(20, 5);
    let ui_of = |shown: bool| {
        let mut ui: Ui<Msg> = Ui::new();
        ui.frame(list(shown), frame);
        ui
    };
    let (hidden, shown) = (ui_of(false), ui_of(true));

    assert!(
        !hidden
            .spec()
            .items
            .iter()
            .any(|i| matches!(i.draw, Draw::Scrollbar { .. })),
        "a bar nobody is revealing is not drawn, overflow or not"
    );
    let bar = shown
        .spec()
        .items
        .iter()
        .find(|i| matches!(i.draw, Draw::Scrollbar { .. }))
        .expect("revealed, the same list shows its bar");
    assert_eq!(bar.rect.x, frame.w as i32 - 1);

    let row_width = |ui: &Ui<Msg>| {
        ui.spec()
            .items
            .iter()
            .filter(|i| matches!(i.draw, Draw::Fill))
            .map(|i| i.rect.w)
            .max()
            .expect("rows paint their ground")
    };
    assert_eq!(
        row_width(&hidden),
        row_width(&shown),
        "revealing the bar must not reflow the rows under the pointer"
    );
    assert_eq!(
        row_width(&shown),
        frame.w,
        "an overlay bar takes no column from the content"
    );
}

/// **An overlay bar is painted after the rows it reports on.**
///
/// A node's own paint is under its children, which is right for a ground and
/// wrong for a bar that has no gutter to sit in: emitted with the rest of the
/// window's output it would be covered by the very rows underneath it.
#[test]
fn an_overlay_bar_lands_on_top_of_the_rows() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        List::windowed(500, fresh_ui::Key::from, |i| {
            fresh_ui::col()
                .theme("list.row")
                .child(fresh_ui::text(format!("row {i}")))
        })
        .scrollbar_revealed(true)
        .node(),
        Size::new(20, 5),
    );
    let items = &ui.spec().items;
    let bar = items
        .iter()
        .position(|i| matches!(i.draw, Draw::Scrollbar { .. }))
        .expect("an overflowing list shows its bar");
    let last_row = items
        .iter()
        .rposition(|i| matches!(i.draw, Draw::Fill))
        .expect("rows paint their ground");
    assert!(
        bar > last_row,
        "the bar must come after every row it floats over ({bar} vs {last_row})"
    );
}

/// **A bar nobody can see is a bar nobody can catch.**
///
/// The track's press is answered before propagation. An overlay bar that was
/// not being revealed but still claimed its column would swallow presses
/// aimed at the row drawn in it — and the row is what is visibly there.
#[test]
fn a_withheld_bar_leaves_its_column_to_whatever_is_behind_it() {
    use std::cell::RefCell;
    use std::rc::Rc;
    let log: Rc<RefCell<Vec<usize>>> = Rc::new(RefCell::new(Vec::new()));
    let frame = Size::new(20, 5);
    let ui_of = |shown: bool, log: Rc<RefCell<Vec<usize>>>| {
        let mut ui: Ui<Msg> = Ui::new();
        ui.frame(
            List::windowed(500, fresh_ui::Key::from, move |i| {
                let log = log.clone();
                fresh_ui::gesture(
                    fresh_ui::col()
                        .theme("list.row")
                        .child(fresh_ui::text(format!("row {i}"))),
                )
                .on_click(move |_| {
                    log.borrow_mut().push(i);
                    Msg::Selected(i)
                })
            })
            .scrollbar_revealed(shown)
            .node(),
            frame,
        );
        ui
    };
    let gutter = frame.w as i32 - 1;
    let press = |ui: &mut Ui<Msg>| {
        ui.dispatch(Input::press(
            Point::new(gutter, 2),
            MouseButton::Left,
            Mods::NONE,
        ));
        ui.dispatch(Input::release(
            Point::new(gutter, 2),
            MouseButton::Left,
            Mods::NONE,
        ));
    };

    // Revealed: the column is the track, and the press scrolls rather than
    // reaching the row.
    let mut shown = ui_of(true, log.clone());
    press(&mut shown);
    assert!(
        log.borrow().is_empty(),
        "a visible track takes its own column: {:?}",
        log.borrow()
    );

    // Withheld: the same cell belongs to the row drawn in it.
    log.borrow_mut().clear();
    let mut hidden = ui_of(false, log.clone());
    press(&mut hidden);
    assert_eq!(
        log.borrow().len(),
        1,
        "with no bar drawn the column is the row's"
    );
}

/// **The bar's appearance is named apart from the window's.**
///
/// `theme` tags a node *and its descendants*, and a region that names its
/// appearance is a region that paints — so a window that named its bar that
/// way would also fill itself in the bar's colours behind every row.
#[test]
fn a_bar_carries_its_own_theme_and_the_rows_keep_theirs() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        List::windowed(500, fresh_ui::Key::from, |i| {
            fresh_ui::col()
                .theme("list.row")
                .child(fresh_ui::text(format!("row {i}")))
        })
        .scrollbar()
        .scrollbar_theme("bar.thumb/bar.track")
        .node(),
        Size::new(20, 5),
    );
    let bar = ui
        .spec()
        .items
        .iter()
        .find(|i| matches!(i.draw, Draw::Scrollbar { .. }))
        .expect("an overflowing list shows a bar");
    assert_eq!(bar.theme.as_str(), "bar.thumb/bar.track");
    assert!(
        ui.spec()
            .items
            .iter()
            .filter(|i| !matches!(i.draw, Draw::Scrollbar { .. }))
            .all(|i| !i.theme.as_str().starts_with("bar.")),
        "the bar's name reaches nothing but the bar"
    );
}

/// **A card list is a list whose items are blocks.**
///
/// Each item takes a fixed band of rows, and everything else about the list —
/// the window, the index the offset counts, the selection, the click — is
/// unchanged, because an item is still an item. What would break it is rows
/// that each decide their own height: then the window could not say which
/// items it holds without measuring all of them.
#[test]
fn a_card_lists_items_take_a_band_of_rows_each() {
    let card = |i: usize| -> Node<Msg> {
        fresh_ui::col()
            .child(fresh_ui::text(format!("title {i}")))
            .child(fresh_ui::text(format!("body {i}")))
            .child(fresh_ui::text("────"))
    };
    let mut ui: Ui<Msg> = Ui::new();
    // Nine rows of window, three rows per card: three cards fit.
    ui.frame(
        List::windowed(20, fresh_ui::Key::from, card)
            .row_rows(3)
            .scrollbar()
            .node(),
        Size::new(20, 9),
    );
    let band = |i: usize| {
        let id = ui
            .find_by_key(&fresh_ui::Key::from(i))
            .unwrap_or_else(|| panic!("card {i}"));
        let r = ui.rect_of(id);
        (r.y, r.h)
    };
    assert_eq!(band(0), (0, 3), "the first card's band");
    assert_eq!(band(1), (3, 3), "and they stack by their own height");
    assert_eq!(band(2), (6, 3));
    // The window is three cards tall; a fourth is built for overscan and lands
    // below it, which is the whole of "the window knows what it holds".
    assert_eq!(band(3).0, 9, "past the window's last row");
}

/// And the bar reads in items, not in cells. Nine cells of window over cards
/// three rows tall is a window of *three* items — a thumb sized from the nine
/// would claim the list is three times as visible as it is.
#[test]
fn a_card_lists_bar_measures_the_window_in_items() {
    let card = |i: usize| -> Node<Msg> {
        fresh_ui::col()
            .child(fresh_ui::text(format!("title {i}")))
            .child(fresh_ui::text(format!("body {i}")))
            .child(fresh_ui::text("────"))
    };
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        List::windowed(20, fresh_ui::Key::from, card)
            .row_rows(3)
            .scrollbar()
            .node(),
        Size::new(20, 9),
    );
    let bar = ui
        .spec()
        .items
        .iter()
        .find_map(|i| match i.draw {
            Draw::Scrollbar {
                offset,
                content,
                window,
            } => Some((offset, content, window)),
            _ => None,
        })
        .expect("an overflowing card list shows a bar");
    assert_eq!(bar, (0, 20, 3), "twenty items, three of them visible");
}

/// **Declining the focus ring is about the keyboard, not about being inert.**
///
/// A list driven from outside — its selection set by the caller each frame —
/// should not be a stop on the way round, or Tab lands on a widget that has
/// nothing to do with the key. Its rows still answer the mouse.
#[test]
fn a_list_that_declines_focus_still_answers_a_click() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        List::windowed(10, fresh_ui::Key::from, |i| {
            fresh_ui::col().child(fresh_ui::text(format!("row {i}")))
        })
        .focusable(false)
        .on_select(Msg::Selected)
        .node(),
        FRAME,
    );
    assert_eq!(click(&mut ui, 2, 3), vec![Msg::Selected(3)]);

    // Tab does not stop here: with nothing focusable in the frame, the key is
    // left for whoever else is listening.
    let tab = ui.dispatch(Input::Key(KeyPress::new(KeyCode::Tab)));
    assert!(tab.claimed == false && tab.msgs.is_empty());
}
