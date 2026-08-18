//! Golden functional tests: drive the demo application through a scripted
//! session and compare each frame, character for character, against a recorded
//! screen.
//!
//! These are the tests that catch what unit tests cannot — a change that is
//! locally correct everywhere and wrong when assembled. They are also the
//! readable record of what the library produces: a diff shows the interface
//! moving, not a list of rectangles changing.
//!
//! Re-record after an intended change with:
//!
//! ```text
//! UPDATE_GOLDEN=1 cargo test -p fresh-ui --test golden
//! ```

use std::path::PathBuf;

mod support;
use fresh_ui::{Input, KeyCode, KeyPress, Mods, MouseButton, Point, Size};
use support::demo::{App, Demo, Msg};

const FRAME: Size = Size { w: 56, h: 14 };

fn golden_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/golden")
        .join(format!("{name}.txt"))
}

/// Compare a rendered session against its recording.
fn check(name: &str, session: &str) {
    let path = golden_path(name);
    if std::env::var("UPDATE_GOLDEN").is_ok() {
        std::fs::write(&path, session).expect("write golden");
        return;
    }
    let expected = std::fs::read_to_string(&path).unwrap_or_else(|_| {
        panic!(
            "no golden file at {}. Record it with UPDATE_GOLDEN=1.",
            path.display()
        )
    });
    if expected != session {
        panic!(
            "{name} differs from its recording.\n\
             --- recorded ---\n{expected}\n\
             --- produced ---\n{session}\n\
             Re-record with UPDATE_GOLDEN=1 if the change is intended."
        );
    }
}

/// Accumulates labelled frames into one comparable document.
struct Session {
    out: String,
    demo: Demo,
}

impl Session {
    fn new() -> Self {
        Session::with_app(App::default())
    }

    fn with_app(app: App) -> Self {
        Session {
            out: String::new(),
            demo: Demo::with_app(app, FRAME),
        }
    }

    fn shot(&mut self, label: &str) -> &mut Self {
        self.out
            .push_str(&format!("=== {label}\n{}\n\n", self.demo.screen()));
        self
    }

    fn feed(&mut self, input: Input) -> Vec<Msg> {
        self.demo.input(input)
    }

    fn key(&mut self, code: KeyCode) -> &mut Self {
        self.feed(Input::Key(KeyPress::new(code)));
        self
    }

    fn chord(&mut self, code: KeyCode, mods: Mods) -> &mut Self {
        self.feed(Input::Key(KeyPress::with(code, mods)));
        self
    }

    fn click(&mut self, x: i32, y: i32) -> &mut Self {
        let pos = Point::new(x, y);
        self.feed(Input::Press {
            pos,
            button: MouseButton::Left,
            mods: Mods::NONE,
        });
        self.feed(Input::Release {
            pos,
            button: MouseButton::Left,
            mods: Mods::NONE,
        });
        self
    }

    fn right_click(&mut self, x: i32, y: i32) -> &mut Self {
        let pos = Point::new(x, y);
        self.feed(Input::Press {
            pos,
            button: MouseButton::Right,
            mods: Mods::NONE,
        });
        self.feed(Input::Release {
            pos,
            button: MouseButton::Right,
            mods: Mods::NONE,
        });
        self
    }

    fn wheel(&mut self, x: i32, y: i32, delta: i32) -> &mut Self {
        self.feed(Input::Wheel {
            pos: Point::new(x, y),
            delta,
            mods: Mods::NONE,
        });
        self
    }

    fn tab(&mut self, n: usize) -> &mut Self {
        for _ in 0..n {
            self.key(KeyCode::Tab);
        }
        self
    }

    fn finish(self, name: &str) {
        check(name, &self.out);
    }
}

#[test]
fn opening_frame() {
    let mut s = Session::new();
    s.shot("as mounted");
    s.finish("opening");
}

#[test]
fn typing_a_task_and_submitting_it() {
    let mut s = Session::new();
    s.tab(4).shot("focus in the new-task field");
    for c in "ship it".chars() {
        s.key(KeyCode::Char(c));
    }
    s.shot("typed");
    s.key(KeyCode::Enter).shot("submitted");
    s.key(KeyCode::Backspace)
        .shot("the field is empty, so backspace does nothing");
    s.finish("typing");
}

#[test]
fn filtering_through_the_sidebar() {
    let mut s = Session::new();
    s.click(2, 3).shot("clicked Open in the sidebar");
    s.click(2, 4).shot("clicked Done");
    s.click(2, 2).shot("back to All");
    s.finish("filtering");
}

#[test]
fn a_context_menu_leads_to_a_modal() {
    let mut s = Session::new();
    s.right_click(24, 4).shot("context menu on a row");
    s.key(KeyCode::Down).shot("moved to Delete");
    s.key(KeyCode::Enter)
        .shot("the confirmation modal, with a scrim");
    s.key(KeyCode::Tab)
        .shot("traversal is confined to the modal");
    s.key(KeyCode::Esc)
        .shot("escape dismisses it, and focus goes back");
    s.finish("context_modal");
}

#[test]
fn deleting_a_task_through_the_modal() {
    let mut s = Session::new();
    s.right_click(24, 4).key(KeyCode::Down).key(KeyCode::Enter);
    s.shot("about to delete");
    s.key(KeyCode::Enter).shot("deleted");
    s.finish("deleting");
}

#[test]
fn the_command_palette() {
    let mut s = Session::new();
    s.chord(KeyCode::Char('p'), Mods::CTRL).shot("opened");
    s.key(KeyCode::Char('p'))
        .shot("filtered to matches containing p");
    s.key(KeyCode::Backspace).key(KeyCode::Char('t'));
    s.shot("filtered to matches containing t");
    s.key(KeyCode::Enter)
        .shot("ran Toggle theme: the accent and the status change");
    s.finish("palette");
}

#[test]
fn a_menu_opens_over_the_content_and_dismisses_outside() {
    let mut s = Session::new();
    s.click(2, 0).shot("the File menu");
    s.click(40, 8).shot("a click outside dismisses it");
    s.finish("menu");
}

#[test]
fn dragging_the_divider_resizes_the_sidebar() {
    let mut s = Session::new();
    s.feed(Input::Press {
        pos: Point::new(12, 6),
        button: MouseButton::Left,
        mods: Mods::NONE,
    });
    s.shot("pressed the divider");
    s.feed(Input::Move {
        pos: Point::new(20, 9),
        mods: Mods::NONE,
    });
    s.shot("dragged right — the pointer left the divider's own column");
    s.feed(Input::Move {
        pos: Point::new(8, 2),
        mods: Mods::NONE,
    });
    s.shot("and back left, over an unrelated widget");
    s.feed(Input::Release {
        pos: Point::new(8, 2),
        button: MouseButton::Left,
        mods: Mods::NONE,
    });
    s.feed(Input::Move {
        pos: Point::new(30, 5),
        mods: Mods::NONE,
    });
    s.shot("released: moving no longer resizes");
    s.finish("divider");
}

#[test]
fn a_million_rows_scroll_by_wheel() {
    let mut s = Session::with_app(App::huge(1_000_000));
    s.shot("the top of a million rows");
    s.wheel(30, 6, 3).shot("three notches down");
    s.wheel(30, 6, 499_000).shot("half a million rows down");
    s.wheel(30, 6, -1_000_000).shot("and back to the top");
    s.finish("million");
}

#[test]
fn the_display_list_stays_bounded_over_a_million_rows() {
    let mut demo = Demo::with_app(App::huge(1_000_000), FRAME);
    demo.input(Input::Wheel {
        pos: Point::new(30, 6),
        delta: 700_000,
        mods: Mods::NONE,
    });
    assert!(
        demo.ui.live_count() < 200,
        "{} elements mounted",
        demo.ui.live_count()
    );
    assert!(
        demo.ui.spec().items.len() < 200,
        "{} display-list items",
        demo.ui.spec().items.len()
    );
}

#[test]
fn resizing_the_frame_reflows_everything() {
    let mut s = Session::new();
    s.shot("at 56x14");
    s.demo.resize(Size::new(30, 8));
    s.shot("at 30x8");
    s.demo.resize(Size::new(90, 6));
    s.shot("at 90x6");
    s.demo.resize(FRAME);
    s.shot("back to 56x14");
    s.finish("resize");
}
