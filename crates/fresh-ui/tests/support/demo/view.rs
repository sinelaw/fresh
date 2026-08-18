//! The demo application's view: a pure function from state to a description.
//!
//! Between them, the pieces here use every capability the library has —
//! all seven primitives, ambients, behaviors, layers of three different
//! modalities, both list forms, pointer capture, focus scopes and intents.

use std::rc::Rc;

use fresh_ui::ambient::{provide, Ambient};
use fresh_ui::desc::{
    col, gesture, host, layer, row, shared_rc, stack, text, Align, Anchor, Dismiss, Fit, Modality,
    Node, Place, Scrim, Sizing,
};
use fresh_ui::event::{Event, GestureKind, KeyCode, KeyPress, Mods};
use fresh_ui::focus::Intent;
use fresh_ui::key::Key;
use fresh_ui::render::geom::Point;
use fresh_ui::schedule::BuildCx;
use fresh_ui::widgets::{divider, Button, Dropdown, List, RadioGroup, TextField};
use fresh_ui::{Component, ComponentExt};

use super::model::{App, Filter, Msg, Task, Theme, COMMANDS};

/// The theme reaches the status bar and the rows without appearing in a single
/// intermediate signature.
pub static THEME: Ambient<Theme> = Ambient::new("theme");

pub fn view(app: &App) -> Node<Msg> {
    // A root focusable that traversal skips: it exists to catch the intents no
    // more specific part of the interface claimed. It deliberately does *not*
    // claim Cancel — a layer that declares `dismiss(ESCAPE)` needs Escape to
    // reach it, and an action here would swallow it first.
    fresh_ui::desc::focusable(app_body(app))
        .skip_traversal()
        .action(Intent::Custom("palette"), |_| Msg::OpenPalette)
}

fn app_body(app: &App) -> Node<Msg> {
    provide(
        &THEME,
        app.theme.clone(),
        col()
            .theme("app")
            .children([
                menu_bar(app),
                row().flex(1).children([
                    sidebar(app).w(Sizing::Cells(app.sidebar)),
                    grip(),
                    body(app).flex(1),
                    preview(app),
                ]),
                StatusBar {
                    count: app.tasks.len(),
                    status: app.status.clone(),
                    slot: app.sync_slot.clone(),
                }
                .node(),
            ])
            .child_if(app.context.is_some(), || context_menu(app))
            .child_if(app.confirm.is_some(), || confirm_modal(app))
            .child_if(app.palette.is_some(), || palette(app)),
    )
}

// -- the menu bar ------------------------------------------------------------

thread_local! {
    /// Hoisted: the menu bar does not depend on any application state, so the
    /// same instance is handed back every frame and the reconciler skips the
    /// whole subtree. Reference identity is the only skip rule there is.
    static MENU_BAR: Rc<Node<Msg>> = Rc::new(build_menu_bar());
}

fn menu_bar(_app: &App) -> Node<Msg> {
    MENU_BAR.with(|m| shared_rc(m.clone())).h(Sizing::Cells(1))
}

fn build_menu_bar() -> Node<Msg> {
    row().theme("menubar").h(Sizing::Cells(1)).children([
        Dropdown::new("File")
            .item("new", "New task")
            .item("quit", "Quit")
            .on_choose(Msg::MenuChoice)
            .node()
            .key("menu.file"),
        Dropdown::new("Go")
            .item("palette", "Command palette")
            .on_choose(Msg::MenuChoice)
            .node()
            .key("menu.go"),
    ])
}

// -- the sidebar and the divider --------------------------------------------

fn sidebar(app: &App) -> Node<Msg> {
    let mut group = RadioGroup::new().selected(app.filter.key());
    for f in Filter::ALL {
        group = group.option(f.key(), f.label());
    }
    col().theme("sidebar").children([
        text("Filter").h(Sizing::Cells(1)).theme("sidebar.title"),
        group
            .on_change(|k| {
                Msg::SetFilter(match format!("{k}").as_str() {
                    "#Open" => Filter::Open,
                    "#Done" => Filter::Done,
                    _ => Filter::All,
                })
            })
            .node(),
    ])
}

/// A one-cell column that resizes the sidebar. Pressing it captures the
/// pointer, so the drag continues wherever the pointer goes — that is the whole
/// drag mechanism.
fn grip() -> Node<Msg> {
    gesture(col().theme("grip"))
        .w(Sizing::Cells(1))
        .on(
            GestureKind::Press,
            Rc::new(|e: &Event| {
                e.capture_pointer();
                Some(Msg::BeginResize)
            }),
        )
        .on(
            GestureKind::Move,
            Rc::new(|e: &Event| Some(Msg::Resize(e.pos.x))),
        )
        .on(GestureKind::Release, Rc::new(|_| Some(Msg::EndResize)))
}

// -- the task list -----------------------------------------------------------

fn body(app: &App) -> Node<Msg> {
    col().children([
        new_task_row(app).h(Sizing::Cells(1)),
        divider(0).h(Sizing::Cells(0)),
        task_list(app).flex(1),
    ])
}

fn new_task_row(app: &App) -> Node<Msg> {
    row().children([
        TextField::new(&app.draft)
            .placeholder("new task…")
            .on_change(Msg::Draft)
            .on_submit(|_| Msg::Add)
            .node()
            .flex(1),
        Button::new("Add").on_press(|_| Msg::Add).node(),
    ])
}

fn task_list(app: &App) -> Node<Msg> {
    let visible = app.visible();
    let tasks = app.tasks.clone();
    let n = visible.len();
    let by_row = visible.clone();
    let accent = app.theme.accent;

    List::windowed(
        n,
        {
            let v = visible.clone();
            let t = tasks.clone();
            move |i| Key::from(v.get(i).map(|x| t[x].id).unwrap_or(0))
        },
        move |i| {
            let idx = by_row.get(i).expect("index inside the window");
            task_row(&tasks[idx], i, accent)
        },
    )
    .selected(app.selected.min(n.saturating_sub(1)))
    .on_select(Msg::Select)
    .on_activate(Msg::Toggle)
    .node()
}

fn task_row(task: &Task, i: usize, accent: &'static str) -> Node<Msg> {
    let mark = if task.done { "[x]" } else { "[ ]" };
    let id = task.id;
    // No Click handler here: the enclosing List activates a row on click and
    // on Enter alike, and this row's list is wired with on_activate(Toggle), so
    // a click toggles it once. The row keeps only what the List does not do —
    // the secondary-click context menu.
    let _ = id;
    gesture(row().children([text(format!("{accent}{mark} ")), text(task.title.clone())])).on(
        GestureKind::SecondaryClick,
        Rc::new(move |e: &Event| Some(Msg::OpenContext(i, e.pos))),
    )
}

fn preview(app: &App) -> Node<Msg> {
    if !app.preview {
        return Node::nil();
    }
    // Foreign content the host draws itself: a buffer, a terminal grid, an
    // image. The library gives it a rectangle and nothing else. A stack puts a
    // caption over it without either one affecting the other's layout.
    stack()
        .w(Sizing::Cells(12))
        .key("preview")
        .children([host(1u64), text("preview").h(Sizing::Cells(1))])
}

// -- layers ------------------------------------------------------------------

/// Anchored to the point the click landed on, dismissed by a click outside or
/// by Escape. Non-modal: the rest of the interface keeps working.
fn context_menu(app: &App) -> Node<Msg> {
    let at = app.context.map(|(_, p)| p).unwrap_or(Point::ZERO);
    layer()
        .key("context")
        .anchor(Anchor::Point(at.x as u16, at.y as u16))
        .place(Place::Below)
        .fit(Fit::FLIP.or(Fit::CLAMP))
        // Inert: while the menu is up, the rest of the interface takes no
        // input, and traversal cannot leave it.
        .modality(Modality::Inert)
        .dismiss(Dismiss::OUTSIDE_POINTER.or(Dismiss::ESCAPE))
        .on_dismiss(|_| Msg::CloseContext)
        .child(
            // A fixed width, so rows stretch across it and the selected one is
            // a full-width region rather than exactly its own text.
            col().border().theme("menu").w(Sizing::Cells(11)).child(
                List::keyed(
                    &[("toggle", "Toggle"), ("delete", "Delete…")],
                    |(k, _)| Key::from(*k),
                    |(_, label)| text(*label),
                )
                .autofocus()
                .on_activate(|i| {
                    Msg::ContextChoice(Key::from(if i == 0 { "toggle" } else { "delete" }))
                })
                .node(),
            ),
        )
}

/// Exclusive: everything outside is inert, traversal cannot leave, and a scrim
/// covers what is behind.
fn confirm_modal(app: &App) -> Node<Msg> {
    let id = app.confirm.unwrap_or(0);
    layer()
        .key("modal")
        .anchor(Anchor::Screen(Align::Center))
        .modality(Modality::Exclusive)
        .scrim(Some(Scrim::Dim))
        .dismiss(Dismiss::ESCAPE)
        .on_dismiss(|_| Msg::CancelModal)
        .child(
            col().border().pad(1, 0).theme("modal").children([
                text(format!("Delete task #{id}?")).h(Sizing::Cells(1)),
                row().gap(2).children([
                    Button::new("Delete")
                        .on_press(|_| Msg::ConfirmDelete)
                        .autofocus()
                        .node(),
                    Button::new("Cancel").on_press(|_| Msg::CancelModal).node(),
                ]),
            ]),
        )
}

/// A prompt over the whole frame: a field that takes focus, and a list of
/// matches under it. Re-keyed on nothing here, but the pattern is the same one
/// that resets a prompt's editing state by re-keying it on its kind.
fn palette(app: &App) -> Node<Msg> {
    let query = app.palette.clone().unwrap_or_default();
    let matches = app.palette_matches();
    let labels: Vec<&'static str> = matches.iter().map(|i| COMMANDS[*i]).collect();

    layer()
        .key("palette")
        .anchor(Anchor::Screen(Align::Center))
        .modality(Modality::Inert)
        .dismiss(Dismiss::ESCAPE)
        .on_dismiss(|_| Msg::ClosePalette)
        .child(
            col()
                .border()
                .theme("palette")
                .w(Sizing::Cells(24))
                .children([
                    TextField::new(&query)
                        .placeholder("command…")
                        .autofocus()
                        .on_change(Msg::PaletteQuery)
                        .on_submit(|_| Msg::RunCommand(0))
                        .node()
                        .h(Sizing::Cells(1)),
                    List::keyed(&labels, |l| Key::from(*l), |l| text(*l))
                        .selected(app.palette_selection.min(labels.len().saturating_sub(1)))
                        .on_select(Msg::PaletteSelect)
                        .on_activate(Msg::RunCommand)
                        .node()
                        .h(Sizing::Cells(labels.len().max(1) as u16)),
                ]),
        )
}

// -- a component that reads an ambient ---------------------------------------

pub struct StatusBar {
    count: usize,
    status: String,
    /// Where the harness (standing in for a network client) picks up the handle
    /// it delivers results through.
    slot: SyncSlot,
}

pub type SyncSlot = Rc<std::cell::RefCell<Option<fresh_ui::behavior::TaskHandle<usize>>>>;

#[derive(Default)]
pub struct StatusState {
    synced: usize,
    /// Held so the behavior lives exactly as long as the element does.
    _tasks: Option<Rc<fresh_ui::behavior::Tasks<usize>>>,
}

impl Component<Msg> for StatusBar {
    type State = StatusState;

    fn init(&self, cx: &mut fresh_ui::schedule::InitCx<'_, Msg>) -> StatusState {
        let tasks = cx.register(fresh_ui::behavior::Tasks::<usize>::new());
        let up = cx.updater::<StatusState>();
        tasks.on_result(move |n| up.set(move |s: &mut StatusState| s.synced = n));
        *self.slot.borrow_mut() = Some(tasks.handle("sync"));
        StatusState {
            synced: 0,
            _tasks: Some(tasks),
        }
    }

    fn build(&self, s: &StatusState, cx: &mut BuildCx<'_, Msg>) -> Node<Msg> {
        // One explicit hop. When the theme changes this element is marked, and
        // nothing else is.
        let theme = cx.read(&THEME).map(|t| t.name).unwrap_or("none");
        row()
            .theme("status")
            .h(Sizing::Cells(1))
            .children([text(format!(
                "{} tasks · {} · synced {} · {}",
                self.count, theme, s.synced, self.status
            ))])
    }

    fn describe_state(&self, s: &StatusState) -> Option<String> {
        Some(format!("synced={}", s.synced))
    }
}

/// Global shortcuts the demo installs on top of the default map.
pub fn shortcuts() -> Vec<fresh_ui::focus::Shortcut> {
    let mut s = fresh_ui::focus::default_shortcuts();
    s.push(fresh_ui::focus::Shortcut::new(
        KeyPress::with(KeyCode::Char('p'), Mods::CTRL),
        Intent::Custom("palette"),
    ));
    s
}
