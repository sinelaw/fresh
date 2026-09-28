//! The widget set (plan phase L7).
//!
//! Every assertion here goes through the public event path — dispatch an input,
//! read the messages and the display list. Nothing reaches into the framework.

mod support;
use fresh_ui::Axis;
use fresh_ui::{
    col, Button, ComponentExt, Draw, Dropdown, Input, KeyCode, KeyPress, List, Mods, MouseButton,
    Node, Number, Point, RadioGroup, RowHeight, Size, Sizing, TextField, Toggle, Tree, TreeNode,
    Ui,
};
use std::cell::RefCell;
use std::rc::Rc;
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
    Scrolled(usize),
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

/// A page is the window layout gave the list, in items: ten rows in a
/// ten-row frame. The owner's pager answers the same number the list's own
/// PageDown moves by, and a page past either end stops at it.
#[test]
fn a_page_is_the_height_layout_gave_the_list() {
    let pager = fresh_ui::behavior::Pager::new();
    let list = |sel: usize| -> Node<Msg> {
        List::windowed(100, fresh_ui::Key::from, |i| {
            fresh_ui::text(format!("row {i}"))
        })
        .selected(sel)
        .on_select(Msg::Selected)
        .pager(pager.clone())
        .node()
    };
    assert_eq!(pager.target(0, 1, 100), None, "not laid out: no page yet");

    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(list(0), FRAME);
    assert_eq!(pager.target(0, 1, 100), Some(FRAME.h as usize));
    assert_eq!(pager.target(95, 1, 100), Some(99));
    assert_eq!(pager.target(3, -1, 100), Some(0));

    key(&mut ui, KeyCode::Tab);
    assert_eq!(key(&mut ui, KeyCode::PageDown), vec![Msg::Selected(10)]);
    ui.frame(list(10), FRAME);
    assert_eq!(key(&mut ui, KeyCode::PageUp), vec![Msg::Selected(0)]);

    // A shorter frame is a shorter page, on the next layout.
    ui.frame(list(10), Size { w: 30, h: 4 });
    assert_eq!(pager.target(10, 1, 100), Some(14));

    // A list that has left the tree has no window, and so no page.
    ui.frame(fresh_ui::text("gone"), FRAME);
    assert_eq!(pager.target(10, 1, 100), None);
}

/// **A list that owns its window can start it somewhere.** An owner that
/// mounts the list again — a panel handed out while a background task works
/// on it — puts the window back where `on_scroll` last said it was; after
/// that the window is the list's, and a wheel moves it.
#[test]
fn a_list_starts_where_it_is_told_and_then_keeps_its_own_window() {
    let list = || -> Node<Msg> {
        List::windowed(100, fresh_ui::Key::from, |i| {
            fresh_ui::text(format!("row {i}"))
        })
        .selection(None)
        .start_at(40)
        .on_scroll(Msg::Scrolled)
        .node()
    };
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(list(), FRAME);
    assert_eq!(texts(&ui).first().map(String::as_str), Some("row 40"));

    let got = ui
        .dispatch(Input::Wheel {
            pos: Point::new(1, 1),
            delta: 3,
            axis: Axis::Vertical,
            mods: Mods::NONE,
        })
        .msgs;
    ui.frame(list(), FRAME);
    assert_eq!(got, vec![Msg::Scrolled(43)]);
    assert_eq!(
        texts(&ui).first().map(String::as_str),
        Some("row 43"),
        "the start is read once; the wheel's move stands"
    );

    // Past the end, the window's ceiling is the answer.
    let mut ui: Ui<Msg> = Ui::new();
    let late = List::windowed(100, fresh_ui::Key::from, |i| {
        fresh_ui::text(format!("row {i}"))
    })
    .selection(None)
    .start_at(500)
    .node();
    ui.frame(late, FRAME);
    assert_eq!(texts(&ui).last().map(String::as_str), Some("row 99"));
}

/// **The window follows the selected row, not its index.** Rows inserted
/// above the selection move it down, but it is the same row: the wheel had
/// taken the window away from it, and it stays away. Selecting another row
/// brings the window to it.
#[test]
fn rows_inserted_above_the_selection_do_not_pull_the_window_back() {
    let list = |first: usize, sel: usize| -> Node<Msg> {
        let key = move |i: usize| match i < first {
            true => fresh_ui::Key::Str(format!("new {i}").into()),
            false => fresh_ui::Key::from(i - first),
        };
        List::windowed(100 + first, key, move |i| {
            fresh_ui::text(match i < first {
                true => format!("new {i}"),
                false => format!("row {}", i - first),
            })
        })
        .selected(sel)
        .focusable(false)
        .node()
    };
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(list(0, 2), FRAME);
    ui.dispatch(Input::Wheel {
        pos: Point::new(1, 1),
        delta: 50,
        axis: Axis::Vertical,
        mods: Mods::NONE,
    });
    ui.frame(list(0, 2), FRAME);
    assert_eq!(texts(&ui).first().map(String::as_str), Some("row 50"));

    // Three rows arrive above the selection; its index is now 5.
    ui.frame(list(3, 5), FRAME);
    assert_eq!(
        texts(&ui).first().map(String::as_str),
        Some("row 47"),
        "the window keeps its offset, and is not pulled back to the selection: {:?}",
        texts(&ui)
    );

    ui.frame(list(3, 6), FRAME);
    assert!(
        texts(&ui).iter().any(|t| t == "row 3"),
        "a new selection is revealed: {:?}",
        texts(&ui)
    );
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

/// **A row inserted above keeps every other row's element and state.**
///
/// Rows are matched by key, so an insertion moves the rows below it rather
/// than rewriting them in place — and the list's own element state goes with
/// them: the hover names the row the pointer is over, not the position it was
/// at, so the row that slides down under an insertion keeps its highlight and
/// the newcomer does not inherit it.
#[test]
fn an_insertion_moves_the_rows_below_it_with_their_state() {
    let list = |items: &[&'static str]| {
        let items: Vec<&'static str> = items.to_vec();
        List::keyed(
            &items,
            |s| fresh_ui::Key::Str((*s).into()),
            |s| fresh_ui::text(format!("item {s}")),
        )
        .selected(0)
        .row_theme(|_, st| match st {
            fresh_ui::widgets::RowState::Hover => "mine.hover".into(),
            _ => "mine.plain".into(),
        })
        .node()
    };
    let before = ["a", "b", "c", "d"];
    let after = ["a", "new", "b", "c", "d"];
    let key = |s: &str| fresh_ui::Key::Str(s.into());
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(list(&before), FRAME);
    let c = ui.find_by_key(&key("c")).expect("row c");
    let at = ui.rect_of(c);
    ui.dispatch(Input::Move {
        pos: Point::new(at.x, at.y),
        mods: Mods::NONE,
    });
    ui.frame(list(&before), FRAME);
    assert_eq!(themes_of(&ui, "item c"), vec!["mine.hover"]);
    let ids: Vec<_> = before
        .iter()
        .map(|s| ui.find_by_key(&key(s)).expect("a row"))
        .collect();

    ui.frame(list(&after), FRAME);
    for (s, id) in before.iter().zip(&ids) {
        assert_eq!(
            ui.find_by_key(&key(s)),
            Some(*id),
            "row {s} is the same element after the insertion"
        );
    }
    assert_eq!(
        themes_of(&ui, "item c"),
        vec!["mine.hover"],
        "the hover moved down with its row"
    );
    assert_eq!(themes_of(&ui, "item b"), vec!["mine.plain"]);
    assert_eq!(themes_of(&ui, "item new"), vec!["mine.plain"]);
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

/// **Rows of their own heights are windowed in cells.** Cards of three rows
/// between one-row headers: the list builds the rows that overlap the window,
/// the column scrolls a row at a time over all of them, and a selection far
/// down is shown whole.
#[test]
fn rows_of_their_own_heights_are_windowed_in_cells() {
    use std::cell::Cell;
    let built = Rc::new(Cell::new(0usize));
    let list = |sel: usize, built: Rc<Cell<usize>>| -> Node<Msg> {
        List::windowed(1000, fresh_ui::Key::from, move |i| {
            built.set(built.get() + 1);
            match i % 2 {
                0 => fresh_ui::text(format!("head {i}")),
                _ => col().children((0..3).map(move |r| fresh_ui::text(format!("card {i}.{r}")))),
            }
        })
        .row_heights(|i| if i % 2 == 0 { 1 } else { 3 })
        .focusable(false)
        .selected(sel)
        .node()
    };
    let mut ui: Ui<Msg> = Ui::new();
    let screen = support::screen::render(ui.frame(list(0, built.clone()), FRAME)).text();
    assert!(screen.starts_with("head 0\ncard 1.0"), "{screen}");
    assert!(
        built.get() < 30,
        "built {} rows for a ten-row window",
        built.get()
    );

    // Row 801 starts at cell 1600 (400 pairs of 4 cells, then its head).
    built.set(0);
    ui.frame(list(801, built.clone()), FRAME);
    let screen = support::screen::render(ui.frame(list(801, built.clone()), FRAME)).text();
    for r in 0..3 {
        assert!(
            screen.contains(&format!("card 801.{r}")),
            "all of card 801: {screen}"
        );
    }
    assert!(built.get() < 60, "built {} rows to get there", built.get());

    // A wheel moves a row, not a card: a card can be half in view.
    ui.dispatch(Input::Wheel {
        pos: Point::new(1, 1),
        delta: 1,
        axis: Axis::Vertical,
        mods: Mods::NONE,
    });
    let before = screen.lines().next().unwrap_or_default().to_string();
    let after = support::screen::render(ui.frame(list(801, built.clone()), FRAME)).text();
    assert_ne!(after.lines().next().unwrap_or_default(), before, "{after}");
}

/// An empty list selects nothing, whatever its owner passed: with rows of
/// their own heights, a selection of row 0 in an empty list asked for a band
/// the heights do not have.
#[test]
fn an_empty_list_of_stated_heights_selects_nothing() {
    let mut ui: Ui<Msg> = Ui::new();
    let list = List::windowed(0, fresh_ui::Key::from, |i| {
        fresh_ui::text(format!("row {i}"))
    })
    .row_heights(|_| 3)
    .selected(0)
    .on_select(Msg::Selected)
    .node();
    ui.frame(list, FRAME);
    key(&mut ui, KeyCode::Tab);
    assert_eq!(
        key(&mut ui, KeyCode::Enter),
        Vec::<Msg>::new(),
        "nothing to confirm"
    );
}

/// **The cut is the rows on screen.** With rows of their own heights the
/// window is in cells, and the per-window work is handed the rows that
/// overlap it — three rows of three cells in a ten-cell frame are four rows
/// (the fourth half in view), not ten.
#[test]
fn the_cut_of_rows_of_stated_heights_is_the_rows_on_screen() {
    use std::cell::RefCell;
    let cuts: Rc<RefCell<Vec<std::ops::Range<usize>>>> = Rc::default();
    let seen = cuts.clone();
    let list = List::windowed_cut(
        100,
        fresh_ui::Key::from,
        move |r| seen.borrow_mut().push(r),
        |i, _, _: &()| fresh_ui::text(format!("row {i}")),
    )
    .row_heights(|_| 3)
    .focusable(false)
    .node();
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(list, FRAME);
    let last = cuts.borrow().last().cloned().expect("the cut ran");
    assert_eq!(last, 0..4, "{:?}", cuts.borrow());
}

// -- Tree --------------------------------------------------------------------

/// **A windowed tree builds its window, over the owner's projection.** Ten
/// folders of a hundred files each, all open, flattened by the owner; the
/// tree asks for the rows on screen and nothing else, and pins the folder the
/// run's first file is in above it.
#[test]
fn a_windowed_tree_builds_its_window_and_pins_the_folder_it_is_in() {
    use fresh_ui::widgets::TreeRow;
    use std::cell::Cell;
    // Visible index i: folder when i % 101 == 0, else a file in it.
    let built = Rc::new(Cell::new(0usize));
    let tree = |built: Rc<Cell<usize>>| -> Node<Msg> {
        fresh_ui::Tree::windowed(
            1010,
            fresh_ui::Key::from,
            |i| TreeRow {
                depth: usize::from(i % 101 != 0),
                has_children: i % 101 == 0,
                open: true,
            },
            move |i, row, _| {
                built.set(built.get() + 1);
                let name = match row.has_children {
                    true => format!("dir {}", i / 101),
                    false => format!("  file {}.{}", i / 101, i % 101),
                };
                fresh_ui::text(name)
            },
        )
        .sticky(3, |i| (i % 101 != 0).then_some(i / 101 * 101))
        .list()
        .focusable(false)
        .node()
    };
    let mut ui: Ui<Msg> = Ui::new();
    let screen = support::screen::render(ui.frame(tree(built.clone()), FRAME)).text();
    assert!(screen.starts_with("dir 0"), "{screen}");
    assert!(
        built.get() < 40,
        "built {} rows for a ten-row window",
        built.get()
    );

    // Into folder 3: its header stays above the run.
    ui.dispatch(Input::Wheel {
        pos: Point::new(1, 1),
        delta: 350,
        axis: Axis::Vertical,
        mods: Mods::NONE,
    });
    let screen = support::screen::render(ui.frame(tree(built.clone()), FRAME)).text();
    let first = screen
        .lines()
        .next()
        .unwrap_or_default()
        .trim_end()
        .to_string();
    assert_eq!(first, "dir 3", "{screen}");
    assert!(screen.contains("file 3."), "{screen}");
}

/// **A reveal counts the run of the offset it lands on.** From the top of a
/// deep tree nothing is pinned, so the run is the whole window; the offset
/// that shows the last row pins its ancestors, and its run is that much
/// shorter. Revealing by the run it had would leave the row under the fold.
#[test]
fn a_reveal_lands_in_the_run_under_the_pins_it_brings() {
    use fresh_ui::widgets::TreeRow;
    // a/b/c, then 30 files in c: every file's ancestors are rows 0, 1, 2.
    let tree = |sel: usize| -> Node<Msg> {
        fresh_ui::Tree::windowed(
            33,
            fresh_ui::Key::from,
            |i| TreeRow {
                depth: i.min(3),
                has_children: i < 3,
                open: true,
            },
            |i, _, _| fresh_ui::text(format!("row {i}")),
        )
        .sticky(7, |i| i.checked_sub(1).map(|p| p.min(2)))
        .list()
        .selected(sel)
        .focusable(false)
        .node()
    };
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(tree(0), FRAME);
    let screen = support::screen::render(ui.frame(tree(32), FRAME)).text();
    let rows: Vec<&str> = screen.lines().map(str::trim_end).collect();
    assert_eq!(&rows[..3], ["row 0", "row 1", "row 2"], "{screen}");
    assert_eq!(
        rows[9], "row 32",
        "the selection is the run's last row: {screen}"
    );
}

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

/// **A revealed bar can have a column of its own.**
///
/// The two are separate answers: *when* the bar is drawn (on attention) and
/// *where* it goes (over the rows, or in a column they never use). Asked for
/// together they are the combination a window whose content reaches its last
/// column needs — a row ending in a button, or in the `…` that says it was
/// cut — because a floating bar covers whatever is under it, and a gutter
/// that came and went would move the row being reached for.
#[test]
fn a_revealed_bar_with_a_gutter_neither_covers_the_rows_nor_moves_them() {
    let list = |n: usize, shown: bool| -> Node<Msg> {
        List::windowed(n, fresh_ui::Key::from, |i| {
            fresh_ui::col()
                .theme("list.row")
                .child(fresh_ui::text(format!("row {i}")))
        })
        .scrollbar_gutter()
        .scrollbar_revealed(shown)
        .node()
    };
    let frame = Size::new(20, 5);
    let ui_of = |n: usize, shown: bool| {
        let mut ui: Ui<Msg> = Ui::new();
        ui.frame(list(n, shown), frame);
        ui
    };
    let row_width = |ui: &Ui<Msg>| {
        ui.spec()
            .items
            .iter()
            .filter(|i| matches!(i.draw, Draw::Fill))
            .map(|i| i.rect.w)
            .max()
            .expect("rows paint their ground")
    };

    let (fits, hidden, shown) = (ui_of(3, false), ui_of(500, false), ui_of(500, true));
    for (what, ui) in [("fits", &fits), ("hidden", &hidden), ("shown", &shown)] {
        assert_eq!(
            row_width(ui),
            frame.w - 1,
            "the gutter is the bar's column in every state, not the rows' ({what})"
        );
    }
    assert!(
        !hidden
            .spec()
            .items
            .iter()
            .any(|i| matches!(i.draw, Draw::Scrollbar { .. })),
        "a bar that is not being revealed is still not drawn"
    );
    let bar = shown
        .spec()
        .items
        .iter()
        .find(|i| matches!(i.draw, Draw::Scrollbar { .. }))
        .expect("revealed, and overflowing");
    assert_eq!(
        bar.rect.x,
        frame.w as i32 - 1,
        "and when it is drawn it lands in the column that was kept for it"
    );
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

/// **A stated band measures nothing, and that is what it is for.**
///
/// The million-row case is the one `RowHeight::Cells` defends, and the way to
/// see it defended is that the row builder is never asked about a row the
/// window does not hold. This is the guard on the measured band below: adding
/// a variant that measures must not make the variant that does not.
#[test]
fn a_stated_row_height_never_asks_about_an_off_screen_row() {
    let seen = Rc::new(RefCell::new(Vec::new()));
    let asked = seen.clone();
    let card = move |i: usize| -> Node<Msg> {
        asked.borrow_mut().push(i);
        fresh_ui::col()
            .child(fresh_ui::text(format!("title {i}")))
            .child(fresh_ui::text(format!("body {i}")))
            .child(fresh_ui::text("────"))
    };
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        List::windowed(1_000_000, fresh_ui::Key::from, card)
            .row_rows(3)
            .node(),
        Size::new(20, 9),
    );
    let asked = seen.borrow().clone();
    assert!(
        asked.iter().all(|i| *i < 8),
        "a stated band asked about {} rows, up to row {:?}",
        asked.len(),
        asked.iter().max()
    );
}

/// **A measured band is the tallest item's, and the window is still a grid.**
///
/// The number is one nobody could have stated: it is a fact about the items at
/// this width, and it exists only once they have been laid out. What it must
/// not cost is the thing that makes a window a window — every item still starts
/// at `index * band`, so the shorter ones pad rather than closing the gap.
#[test]
fn a_measured_card_lists_band_is_the_tallest_items() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        List::windowed(20, fresh_ui::Key::from, uneven_card)
            .row_height(RowHeight::UniformMeasured)
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
    // Card 7 is three rows; every other card is two. The band is three.
    assert_eq!(band(0), (0, 3), "a two-row card padded to the tallest");
    assert_eq!(band(1), (3, 3), "and the next starts one band down");
    assert_eq!(band(2), (6, 3));
}

/// **The invariant is index addressability at scroll time, not "never
/// measure".**
///
/// The band is measured against the width and kept, so moving the window is
/// arithmetic over an index the way it always was. The row builder is the only
/// witness that can tell the two apart: a measurement has to ask about every
/// item, and a scroll must ask about none but the ones it is about to show.
#[test]
fn a_measured_card_list_does_not_measure_to_scroll() {
    let seen = Rc::new(RefCell::new(Vec::new()));
    let asked = seen.clone();
    let card = move |i: usize| -> Node<Msg> {
        asked.borrow_mut().push(i);
        uneven_card(i)
    };
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        List::windowed(20, fresh_ui::Key::from, card)
            .row_height(RowHeight::UniformMeasured)
            .node(),
        Size::new(20, 9),
    );
    assert!(
        seen.borrow().contains(&19),
        "the first layout measures every item, or it cannot know the tallest"
    );

    seen.borrow_mut().clear();
    ui.dispatch(Input::Wheel {
        pos: Point::new(1, 1),
        delta: 3,
        axis: Axis::Vertical,
        mods: Mods::NONE,
    });
    ui.tick();
    let asked = seen.borrow().clone();
    assert!(!asked.is_empty(), "a scroll builds the rows it moved to");
    assert!(
        asked.iter().all(|i| *i < 9),
        "a scroll asked about {asked:?} — anything past the window is a re-measure"
    );
    // And it moved: the window starts three items down.
    let id = ui
        .find_by_key(&fresh_ui::Key::from(3usize))
        .expect("card 3");
    assert_eq!(
        ui.rect_of(id).y,
        0,
        "the fourth card is at the window's top"
    );
}

/// **The width is the other half of the measurement, so it invalidates it.**
///
/// A card's height is a function of the width it wraps at, which is the whole
/// reason the number cannot be stated. A band cached against a width the list
/// no longer has would clip every card in it.
#[test]
fn a_measured_band_is_measured_again_when_the_width_changes() {
    let card = |i: usize| -> Node<Msg> {
        fresh_ui::col()
            .child(fresh_ui::text(format!("card {i}")))
            .child(fresh_ui::text("one two three four five").wrap())
    };
    let list = || {
        List::windowed(6, fresh_ui::Key::from, card)
            .row_height(RowHeight::UniformMeasured)
            .node()
    };
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(list(), Size::new(24, 12));
    let band = |ui: &Ui<Msg>| {
        let id = ui
            .find_by_key(&fresh_ui::Key::from(0usize))
            .expect("card 0");
        ui.rect_of(id).h
    };
    let wide = band(&ui);
    assert_eq!(wide, 2, "at twenty-four columns the body is one row");
    ui.frame(list(), Size::new(10, 12));
    let narrow = band(&ui);
    assert!(
        narrow > wide,
        "at ten columns the body wraps, so the band grows: {wide} -> {narrow}"
    );
    // And the grid follows it, rather than staying on the old band.
    let id = ui
        .find_by_key(&fresh_ui::Key::from(1usize))
        .expect("card 1");
    assert_eq!(ui.rect_of(id).y, narrow as i32, "the second card's top");
}

/// **A window's height is in items, and that is what an outside caller reads
/// back.**
///
/// The number is right inside the tree already — the viewport publishes it,
/// the reveal path uses it, the scrollbar is drawn from it — and until now
/// nothing outside could get at it: `scroll()` handed back an offset with no
/// way to tell items from cells. A caller that paged this list by its
/// *rectangle* would move eleven cards where four are on screen, which is the
/// bug this exists to make unavailable. Asserted before anything has scrolled,
/// because that is when a window is easiest to mistake for a box.
#[test]
fn a_keyed_card_lists_window_is_reported_in_items() {
    let card = |i: usize| -> Node<Msg> {
        fresh_ui::col()
            .child(fresh_ui::text(format!("title {i}")))
            .child(fresh_ui::text(format!("body {i}")))
            .child(fresh_ui::text("more"))
    };
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        List::windowed(20, fresh_ui::Key::from, card)
            .row_height(RowHeight::UniformMeasured)
            .node()
            .key("cards"),
        Size::new(24, 12),
    );

    let key = fresh_ui::Key::from("cards");
    let win = ui.item_window(&key).expect("the keyed list has a window");
    assert_eq!(
        win.h, 4,
        "four three-row cards fit a twelve-row box; twelve is the cells answer"
    );
    assert_eq!(win.y, 0, "and nothing has scrolled yet");
    assert_eq!(
        ui.band(ui.find_by_key(&key).expect("the list")),
        None,
        "the key is on the component, whose own render node is not the viewport \
         — which is why the read has to descend"
    );

    // The twelve is right there to be taken by mistake: the list is twelve
    // rows tall and every one of them is drawn.
    assert_eq!(ui.rect_of(ui.find_by_key(&key).expect("the list")).h, 12);
    // And it is a window, not the whole list: the wheel can still move it.
    ui.dispatch(Input::Wheel {
        pos: Point::new(1, 1),
        delta: 2,
        axis: Axis::Vertical,
        mods: Mods::NONE,
    });
    ui.tick();
    let moved = ui.item_window(&key).expect("still there");
    assert_eq!(
        (moved.y, moved.h),
        (2, 4),
        "two items down, four still showing"
    );
}

/// **Two lists, one key, two windows — and the scope is what tells them
/// apart.**
///
/// `find_by_key_in`'s own doc says a key is unique only where its owner says
/// it is, and a frame that puts two panels side by side is exactly the case:
/// each names its list `"items"`, and a frame-wide lookup answers with
/// whichever comes first in tree order for both. `item_window_in` scopes the
/// search, and reports the window's height in cells beside its height in
/// items, because for a caller whose offset counts rows the band is the only
/// thing that converts one to the other.
#[test]
fn a_scoped_item_window_answers_for_its_own_subtree() {
    let card = |i: usize| -> Node<Msg> {
        fresh_ui::col()
            .child(fresh_ui::text(format!("title {i}")))
            .child(fresh_ui::text(format!("body {i}")))
            .child(fresh_ui::text("more"))
    };
    let list = move |n: usize, h: u16| -> Node<Msg> {
        List::windowed(n, fresh_ui::Key::from, card)
            .row_height(RowHeight::UniformMeasured)
            .node()
            .key("items")
            .h(Sizing::Cells(h))
    };
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        col()
            .child(col().key("left").child(list(20, 12)))
            .child(col().key("right").child(list(20, 6))),
        Size::new(24, 18),
    );

    let key = fresh_ui::Key::from("items");
    let left = ui.find_by_key(&fresh_ui::Key::from("left")).expect("left");
    let right = ui
        .find_by_key(&fresh_ui::Key::from("right"))
        .expect("right");
    assert_eq!(
        ui.item_window_in(left, &key).map(|(w, cells)| (w.h, cells)),
        Some((4, 12)),
        "four three-row cards in twelve rows"
    );
    assert_eq!(
        ui.item_window_in(right, &key)
            .map(|(w, cells)| (w.h, cells)),
        Some((2, 6)),
        "two in six — the same key, a different window"
    );
    // The unscoped read cannot tell them apart, which is the whole point.
    assert_eq!(ui.item_window(&key).map(|w| w.h), Some(4));
    // A subtree that does not contain the key answers nothing rather than
    // reaching outside itself.
    assert_eq!(
        ui.item_window_in(
            ui.find_by_key(&fresh_ui::Key::from("left")).expect("left"),
            &fresh_ui::Key::from("nobody")
        ),
        None
    );
}

/// A key that names nothing, and one that names something that does not scroll
/// in items, are both "no item window" rather than a number in the wrong unit.
#[test]
fn an_item_window_is_none_where_there_are_no_items() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(
        col().child(
            fresh_ui::viewport(
                col().children((0..40).map(|i| fresh_ui::text(format!("line {i}")))),
            )
            .h(Sizing::Cells(12))
            .key("cells"),
        ),
        Size::new(24, 12),
    );
    assert_eq!(
        ui.item_window(&fresh_ui::Key::from("cells")),
        None,
        "a cell-scrolling window has no items to count"
    );
    assert_eq!(
        ui.window(
            ui.find_by_key(&fresh_ui::Key::from("cells"))
                .expect("the scroll")
        )
        .map(|w| w.h),
        Some(12),
        "its window is its own height, in cells"
    );
    assert_eq!(ui.item_window(&fresh_ui::Key::from("nobody")), None);
}

/// Two rows of card, one of them uneven: item seven is a row taller than the
/// rest, so it is the one that sets a measured band.
fn uneven_card(i: usize) -> Node<Msg> {
    let mut c = fresh_ui::col()
        .child(fresh_ui::text(format!("title {i}")))
        .child(fresh_ui::text(format!("body {i}")));
    if i == 7 {
        c = c.child(fresh_ui::text("and one more line"));
    }
    c
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
                ..
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
    assert!(!tab.claimed && tab.msgs.is_empty());
}

// -- pinned rows and controlled windows --------------------------------------

/// Forty rows, the given ones pinned, the window held by the owner at `at`.
fn pinned_list(pinned: &[usize], at: usize, selected: Option<usize>) -> Node<Msg> {
    let mut l = List::windowed(40, fresh_ui::Key::from, |i| {
        fresh_ui::text(format!("row {i}"))
    })
    .pinned(pinned)
    .scroll(at)
    .on_scroll(Msg::Scrolled)
    .on_select(Msg::Selected)
    .scrollbar();
    if let Some(s) = selected {
        l = l.selected(s);
    }
    l.node()
}

/// **A pinned row is an ordinary row for the pointer.** It is built by the
/// same builder, keyed and themed the same way, and it is where layout put
/// it — so a press on window row 1 names the pinned index that sits there,
/// not `offset + 1`. This is the hit-testing pinning has to get right, and it
/// gets it right by having no arithmetic to get wrong.
#[test]
fn a_list_draws_its_pinned_rows_above_the_run_and_they_answer_the_pointer() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(pinned_list(&[0, 3], 20, None), FRAME);
    let shown = texts(&ui);
    assert_eq!(
        &shown[..4],
        ["row 0", "row 3", "row 20", "row 21"],
        "{shown:?}"
    );
    assert_eq!(shown.len(), 10 + 2, "ten rows plus the overscan");

    assert_eq!(
        click(&mut ui, 2, 1),
        vec![Msg::Selected(3)],
        "the pinned row"
    );
    assert_eq!(
        click(&mut ui, 2, 2),
        vec![Msg::Selected(20)],
        "the first of the run"
    );
    assert_eq!(
        click(&mut ui, 2, 9),
        vec![Msg::Selected(27)],
        "the last visible"
    );
}

/// The controlled window: a wheel is reported and not kept, exactly as a
/// controlled selection is.
#[test]
fn a_controlled_list_reports_the_wheel_and_keeps_its_owners_offset() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(pinned_list(&[], 4, None), FRAME);
    let got = ui.dispatch(Input::Wheel {
        pos: Point::new(2, 2),
        delta: 3,
        axis: Axis::Vertical,
        mods: Mods::NONE,
    });
    assert_eq!(got.msgs, vec![Msg::Scrolled(7)]);
    ui.frame(pinned_list(&[], 4, None), FRAME);
    assert_eq!(
        texts(&ui)[0],
        "row 4",
        "the owner kept 4, so the list shows 4"
    );
    ui.frame(pinned_list(&[], 7, None), FRAME);
    assert_eq!(texts(&ui)[0], "row 7", "the owner took 7");
}

/// A selection the owner moves out of the window asks the window to follow,
/// and for a controlled window that ask is a report — the owner is told
/// where the window would have to be, and the window stays where the owner
/// has it until the owner says otherwise.
#[test]
fn a_controlled_list_reports_the_window_a_selection_move_needs() {
    let mut ui: Ui<Msg> = Ui::new();
    ui.frame(pinned_list(&[], 0, Some(0)), FRAME);
    assert!(ui.take_messages().is_empty());
    ui.frame(pinned_list(&[], 0, Some(20)), FRAME);
    assert_eq!(
        ui.take_messages(),
        vec![Msg::Scrolled(11)],
        "the shortest move that shows row 20 in a ten-row window"
    );
    assert_eq!(texts(&ui)[0], "row 0", "held at 0 until the owner moves it");
    ui.frame(pinned_list(&[], 11, Some(20)), FRAME);
    assert_eq!(texts(&ui)[0], "row 11");
    assert!(
        ui.take_messages().is_empty(),
        "revealed; nothing further to ask"
    );
}

/// With rows pinned, "inside the window" means inside the run: a reveal
/// counts the rows the pins left, not the box.
#[test]
fn a_reveal_in_a_pinned_list_counts_only_the_run() {
    let mut ui: Ui<Msg> = Ui::new();
    // Two pins in a ten-row box: an eight-row run from 10, showing 10..18.
    ui.frame(pinned_list(&[0, 1], 10, Some(10)), FRAME);
    ui.take_messages();
    // Row 18 is the first past the run: one row of scroll, not none.
    ui.frame(pinned_list(&[0, 1], 10, Some(18)), FRAME);
    assert_eq!(ui.take_messages(), vec![Msg::Scrolled(11)]);
}

/// **Pins asked at layout.** Rows 0, 10, 20… head their groups of ten; the
/// row heading the first row of the run is pinned above it. The window asks
/// which at whatever offset it lands on — the owner is never told the offset
/// and asked again — and its ceiling is the offset whose run, under that
/// offset's own pin, reaches the last row.
#[test]
fn pins_that_depend_on_the_offset_are_asked_at_layout() {
    let list = || -> Node<Msg> {
        List::windowed(100, fresh_ui::Key::from, |i| {
            fresh_ui::text(format!("row {i:02}"))
        })
        .focusable(false)
        .pinned_at(|first| match first % 10 {
            0 => Rc::from(Vec::new()),
            _ => Rc::from(vec![first / 10 * 10]),
        })
        .node()
    };
    let mut ui: Ui<Msg> = Ui::new();
    let screen = support::screen::render(ui.frame(list(), FRAME)).text();
    assert!(screen.starts_with("row 00"), "{screen}");

    // All the way down: the last row is on screen, under its group's head.
    ui.dispatch(Input::Wheel {
        pos: Point::new(1, 1),
        delta: 200,
        axis: Axis::Vertical,
        mods: Mods::NONE,
    });
    let screen = support::screen::render(ui.frame(list(), FRAME)).text();
    let rows: Vec<&str> = screen
        .lines()
        .map(str::trim_end)
        .filter(|l| !l.is_empty())
        .collect();
    assert_eq!(rows.first(), Some(&"row 90"), "{screen}");
    assert_eq!(rows.last(), Some(&"row 99"), "{screen}");
    assert_eq!(rows.len(), FRAME.h as usize, "{screen}");
}

/// **A follow is of the selected row, not its index.** Rows arriving above
/// the selection while the list follows it are followed past; after a wheel
/// has taken the window elsewhere they do not pull it back. The owner's
/// token does.
#[test]
fn a_follow_tracks_its_row_and_rows_arriving_above_do_not_rearm_it() {
    let list = |first: usize, sel: usize, token: u64| -> Node<Msg> {
        let key = move |i: usize| match i < first {
            true => fresh_ui::Key::Str(format!("new {i}").into()),
            false => fresh_ui::Key::from(i - first),
        };
        List::windowed(100 + first, key, move |i| {
            fresh_ui::text(match i < first {
                true => format!("new {i}"),
                false => format!("row {}", i - first),
            })
        })
        .focusable(false)
        .selection(Some(sel))
        .follow_selection(token)
        .node()
    };
    let mut ui: Ui<Msg> = Ui::new();
    let screen =
        |ui: &mut Ui<Msg>, n: Node<Msg>| support::screen::render(ui.frame(n, FRAME)).text();

    // Following row 60; twenty rows arrive above it. It is still in view.
    assert!(screen(&mut ui, list(0, 60, 1)).contains("row 60"));
    assert!(
        screen(&mut ui, list(20, 80, 1)).contains("row 60"),
        "the follow moved with its row"
    );

    // The wheel takes the window away; more rows arrive; it stays away.
    ui.dispatch(Input::Wheel {
        pos: Point::new(1, 1),
        delta: -40,
        axis: Axis::Vertical,
        mods: Mods::NONE,
    });
    assert!(!screen(&mut ui, list(20, 80, 1)).contains("row 60"));
    assert!(
        !screen(&mut ui, list(25, 85, 1)).contains("row 60"),
        "rows arriving above are not a new request"
    );
    assert!(
        screen(&mut ui, list(25, 85, 2)).contains("row 60"),
        "the owner's token is"
    );
}

/// **A following list keeps its selection in view on every layout** — not
/// only on the build that moved it. The window here shrinks a frame after the
/// selection arrived (a box resized by its owner a beat later), which a
/// one-shot reveal answered against the old height and then forgot. A wheel is
/// the reader choosing where to look, and wins until the owner's token moves.
#[test]
fn a_following_list_keeps_its_selection_in_view_as_its_window_changes() {
    let list = |sel: usize, token: u64, h: u16| -> Node<Msg> {
        List::windowed(40, fresh_ui::Key::from, |i| {
            col().child(fresh_ui::text(format!("row {i:02}")))
        })
        .focusable(false)
        .selection(Some(sel))
        .follow_selection(token)
        .node()
        .h(Sizing::Cells(h))
    };
    let frame = Size::new(20, 12);
    let mut ui: Ui<Msg> = Ui::new();
    let shows = |ui: &mut Ui<Msg>, sel: usize, token: u64, h: u16| {
        let spec = ui.frame(list(sel, token, h), frame);
        support::screen::render(spec).contains("row 20")
    };

    assert!(shows(&mut ui, 20, 1, 10), "the selection is in view");
    // The window shrinks under an unmoved selection.
    assert!(
        shows(&mut ui, 20, 1, 4),
        "and stays in view at the new height"
    );
    assert!(shows(&mut ui, 20, 1, 4));

    // The reader wheels away: the window is theirs until the caret moves.
    ui.dispatch(Input::Wheel {
        pos: Point::new(2, 1),
        delta: -10,
        axis: Axis::Vertical,
        mods: Mods::NONE,
    });
    assert!(!shows(&mut ui, 20, 1, 4), "a wheel is not fought");
    assert!(
        shows(&mut ui, 20, 2, 4),
        "a move of the caret follows again"
    );
}
