//! The demo application's state and its update function.
//!
//! Ordinary code. It knows nothing about the widget library: no descriptions,
//! no elements, no geometry. The view is a pure function of this, and every
//! change to it goes through `update`.

use std::rc::Rc;

use fresh_ui::key::Key;
use fresh_ui::render::geom::Point;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Task {
    pub id: u64,
    pub title: String,
    pub done: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Filter {
    All,
    Open,
    Done,
}

impl Filter {
    pub const ALL: [Filter; 3] = [Filter::All, Filter::Open, Filter::Done];

    pub fn label(&self) -> &'static str {
        match self {
            Filter::All => "All",
            Filter::Open => "Open",
            Filter::Done => "Done",
        }
    }

    pub fn key(&self) -> Key {
        Key::from(self.label())
    }
}

/// The values a theme ambient carries. The library never looks inside it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Theme {
    pub name: &'static str,
    pub accent: &'static str,
}

pub const LIGHT: Theme = Theme {
    name: "light",
    accent: "»",
};
pub const DARK: Theme = Theme {
    name: "dark",
    accent: "▸",
};

/// The top-level menus, as a type rather than a string key. Which one is open
/// is app state — a mnemonic must be able to open it — and making it an enum
/// means a typo cannot silently fail to match.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Menu {
    File,
    Go,
}

/// Which task indices the current filter admits.
///
/// `All` is a range rather than a vector, so a million-row list costs nothing
/// to filter — the index is the interface, and the application resolves it.
#[derive(Clone)]
pub enum Visible {
    All(usize),
    Some(Rc<Vec<usize>>),
}

impl Visible {
    pub fn len(&self) -> usize {
        match self {
            Visible::All(n) => *n,
            Visible::Some(v) => v.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn get(&self, i: usize) -> Option<usize> {
        match self {
            Visible::All(n) => (i < *n).then_some(i),
            Visible::Some(v) => v.get(i).copied(),
        }
    }
}

pub struct App {
    pub tasks: Rc<Vec<Task>>,
    pub filter: Filter,
    pub selected: usize,
    /// Which top-level menu is open, if any. Lifted here — not left on the
    /// dropdown — because a global mnemonic (Alt+F) must be able to open it.
    pub menu: Option<Menu>,
    /// Set by File → Quit; the host loop reads it and exits.
    pub quit: bool,
    pub draft: String,
    pub status: String,
    pub sidebar: u16,
    pub resizing: bool,
    pub theme: Rc<Theme>,
    pub preview: bool,
    /// The row a context menu is open for, and where it was raised.
    pub context: Option<(usize, Point)>,
    /// The task a delete confirmation is open for.
    pub confirm: Option<u64>,
    /// The command palette's query, when it is open.
    pub palette: Option<String>,
    pub palette_selection: usize,
    /// How many results the sync task has reported.
    pub synced: usize,
    pub next_id: u64,
    /// Where the status bar publishes the handle its sync task delivers
    /// through. The application owns the client; the framework owns the timing.
    pub sync_slot: super::view::SyncSlot,
}

pub const COMMANDS: [&str; 4] = ["Toggle theme", "Toggle preview", "Clear done", "Sync"];

impl Default for App {
    fn default() -> Self {
        App::with_tasks(vec![
            Task {
                id: 1,
                title: "write the spec".into(),
                done: true,
            },
            Task {
                id: 2,
                title: "build the reconciler".into(),
                done: true,
            },
            Task {
                id: 3,
                title: "lay it out".into(),
                done: false,
            },
            Task {
                id: 4,
                title: "route the pointer".into(),
                done: false,
            },
            Task {
                id: 5,
                title: "wire up focus".into(),
                done: false,
            },
        ])
    }
}

impl App {
    pub fn with_tasks(tasks: Vec<Task>) -> Self {
        let next_id = tasks.iter().map(|t| t.id).max().unwrap_or(0) + 1;
        App {
            tasks: Rc::new(tasks),
            filter: Filter::All,
            selected: 0,
            menu: None,
            quit: false,
            draft: String::new(),
            status: "ready".into(),
            sidebar: 12,
            resizing: false,
            theme: Rc::new(LIGHT),
            preview: false,
            context: None,
            confirm: None,
            palette: None,
            palette_selection: 0,
            synced: 0,
            next_id,
            sync_slot: Default::default(),
        }
    }

    /// A stress configuration: the list the design document is built around.
    pub fn huge(n: usize) -> Self {
        App::with_tasks(
            (0..n)
                .map(|i| Task {
                    id: i as u64 + 1,
                    title: format!("task {i}"),
                    done: i % 3 == 0,
                })
                .collect(),
        )
    }

    pub fn visible(&self) -> Visible {
        match self.filter {
            Filter::All => Visible::All(self.tasks.len()),
            Filter::Open => Visible::Some(Rc::new(
                self.tasks
                    .iter()
                    .enumerate()
                    .filter(|(_, t)| !t.done)
                    .map(|(i, _)| i)
                    .collect(),
            )),
            Filter::Done => Visible::Some(Rc::new(
                self.tasks
                    .iter()
                    .enumerate()
                    .filter(|(_, t)| t.done)
                    .map(|(i, _)| i)
                    .collect(),
            )),
        }
    }

    pub fn palette_matches(&self) -> Vec<usize> {
        let q = self.palette.clone().unwrap_or_default().to_lowercase();
        COMMANDS
            .iter()
            .enumerate()
            .filter(|(_, c)| q.is_empty() || c.to_lowercase().contains(&q))
            .map(|(i, _)| i)
            .collect()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Msg {
    Draft(String),
    Add,
    Toggle(usize),
    Select(usize),
    SetFilter(Filter),
    OpenContext(usize, Point),
    CloseContext,
    ContextChoice(Key),
    AskDelete(u64),
    ConfirmDelete,
    CancelModal,
    OpenPalette,
    PaletteQuery(String),
    PaletteSelect(usize),
    RunCommand(usize),
    ClosePalette,
    MenuChoice(Key),
    /// Open the named menu, or close whatever is open.
    Menu(Option<Menu>),
    BeginResize,
    Resize(i32),
    EndResize,
    /// Reported by the sync task, from another thread.
    Synced(usize),
}

/// The whole of the application's behaviour, in one place.
pub fn update(app: &mut App, msg: Msg) {
    match msg {
        Msg::Draft(s) => app.draft = s,
        Msg::Add => {
            let title = std::mem::take(&mut app.draft);
            if !title.trim().is_empty() {
                let id = app.next_id;
                app.next_id += 1;
                Rc::make_mut(&mut app.tasks).push(Task {
                    id,
                    title,
                    done: false,
                });
                app.status = format!("added #{id}");
            }
        }
        Msg::Toggle(i) => {
            if let Some(idx) = app.visible().get(i) {
                let tasks = Rc::make_mut(&mut app.tasks);
                tasks[idx].done = !tasks[idx].done;
                app.status = format!("toggled #{}", tasks[idx].id);
            }
        }
        Msg::Select(i) => app.selected = i,
        Msg::SetFilter(f) => {
            app.filter = f;
            app.selected = 0;
        }
        Msg::OpenContext(i, at) => app.context = Some((i, at)),
        Msg::CloseContext => app.context = None,
        Msg::ContextChoice(k) => {
            let row = app.context.take().map(|(i, _)| i);
            if let (Some(i), Some(idx)) = (row, row.and_then(|i| app.visible().get(i))) {
                let _ = i;
                match format!("{k}").as_str() {
                    "#toggle" => update(app, Msg::Toggle(row.unwrap())),
                    "#delete" => app.confirm = Some(app.tasks[idx].id),
                    _ => {}
                }
            }
        }
        Msg::AskDelete(id) => app.confirm = Some(id),
        Msg::ConfirmDelete => {
            if let Some(id) = app.confirm.take() {
                Rc::make_mut(&mut app.tasks).retain(|t| t.id != id);
                app.selected = app.selected.min(app.visible().len().saturating_sub(1));
                app.status = format!("deleted #{id}");
            }
        }
        Msg::CancelModal => app.confirm = None,
        Msg::OpenPalette => {
            app.palette = Some(String::new());
            app.palette_selection = 0;
        }
        Msg::PaletteQuery(q) => {
            app.palette = Some(q);
            app.palette_selection = 0;
        }
        Msg::PaletteSelect(i) => app.palette_selection = i,
        Msg::RunCommand(i) => {
            let cmd = app.palette_matches().get(i).copied();
            app.palette = None;
            match cmd {
                Some(0) => {
                    app.theme = Rc::new(if app.theme.name == "light" {
                        DARK
                    } else {
                        LIGHT
                    });
                    app.status = format!("theme {}", app.theme.name);
                }
                Some(1) => {
                    app.preview = !app.preview;
                    app.status = format!("preview {}", app.preview);
                }
                Some(2) => {
                    Rc::make_mut(&mut app.tasks).retain(|t| !t.done);
                    app.selected = 0;
                    app.status = "cleared done".into();
                }
                Some(3) => app.status = "syncing…".into(),
                _ => {}
            }
        }
        Msg::ClosePalette => app.palette = None,
        Msg::Menu(which) => app.menu = which,
        Msg::MenuChoice(k) => {
            // Choosing an item closes the menu it came from.
            app.menu = None;
            match format!("{k}").as_str() {
                "#new" => app.draft = "new task".into(),
                "#palette" => update(app, Msg::OpenPalette),
                "#quit" => app.quit = true,
                other => app.status = format!("menu {other}"),
            }
        }
        Msg::BeginResize => app.resizing = true,
        Msg::Resize(x) => {
            if app.resizing {
                app.sidebar = x.clamp(6, 30) as u16;
            }
        }
        Msg::EndResize => app.resizing = false,
        Msg::Synced(n) => {
            app.synced = n;
            app.status = format!("synced {n}");
        }
    }
    // Selection is an index into the visible set, and several messages change
    // what is visible — a toggle can drop a task out of the current filter, a
    // click can select and toggle the same row at once. Clamp once, here, so
    // the rest of the update logic never has to.
    app.selected = app.selected.min(app.visible().len().saturating_sub(1));
}
