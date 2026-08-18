//! Drives the demo application through a scripted session and prints each
//! frame. No terminal, no Fresh code, no dependencies — the display list is
//! folded into characters by the crate's own reference backend.
//!
//!     cargo run -p fresh-ui --example demo

#[path = "../tests/support/mod.rs"]
mod support;
use fresh_ui::{Input, KeyCode, KeyPress, Mods, MouseButton, Point, Size};
use support::demo::Demo;

fn key(c: KeyCode) -> Input {
    Input::Key(KeyPress::new(c))
}

fn press(x: i32, y: i32) -> Input {
    Input::Press {
        pos: Point::new(x, y),
        button: MouseButton::Left,
        mods: Mods::NONE,
    }
}

fn release(x: i32, y: i32) -> Input {
    Input::Release {
        pos: Point::new(x, y),
        button: MouseButton::Left,
        mods: Mods::NONE,
    }
}

fn right(x: i32, y: i32) -> Vec<Input> {
    vec![
        Input::Press {
            pos: Point::new(x, y),
            button: MouseButton::Right,
            mods: Mods::NONE,
        },
        Input::Release {
            pos: Point::new(x, y),
            button: MouseButton::Right,
            mods: Mods::NONE,
        },
    ]
}

fn main() {
    let mut demo = Demo::new(Size::new(64, 16));
    let mut step = 0;
    let show = |demo: &Demo, what: &str, step: &mut usize| {
        *step += 1;
        println!(
            "── {step}. {what} {}",
            "─".repeat(50usize.saturating_sub(what.len()))
        );
        println!("{}\n", demo.screen());
    };

    show(&demo, "opening frame", &mut step);

    // Tab past the two menus, the filter list and into the new-task field.
    for _ in 0..4 {
        demo.input(key(KeyCode::Tab));
    }
    demo.input(key(KeyCode::Char('s')));
    demo.input(key(KeyCode::Char('h')));
    demo.input(key(KeyCode::Char('i')));
    demo.input(key(KeyCode::Char('p')));
    show(&demo, "typing into the new-task field", &mut step);

    demo.input(key(KeyCode::Enter));
    show(&demo, "submitting it", &mut step);

    for i in right(24, 5) {
        demo.input(i);
    }
    show(&demo, "right-click raises a context menu", &mut step);

    demo.input(key(KeyCode::Down));
    demo.input(key(KeyCode::Enter));
    show(&demo, "choosing Delete opens a modal", &mut step);

    demo.input(key(KeyCode::Esc));
    show(&demo, "escape dismisses it", &mut step);

    demo.input(Input::Key(KeyPress::with(KeyCode::Char('p'), Mods::CTRL)));
    show(&demo, "the command palette", &mut step);

    demo.input(key(KeyCode::Char('t')));
    show(&demo, "filtering it", &mut step);

    demo.input(key(KeyCode::Enter));
    show(&demo, "running Toggle theme", &mut step);

    demo.input(press(12, 8));
    demo.input(Input::Move {
        pos: Point::new(20, 8),
        mods: Mods::NONE,
    });
    demo.input(release(20, 8));
    show(&demo, "dragging the sidebar divider", &mut step);

    demo.deliver_sync(42);
    show(&demo, "a result arrives from another thread", &mut step);

    let mut huge = Demo::with_app(support::demo::App::huge(1_000_000), Size::new(64, 16));
    huge.input(Input::Wheel {
        pos: Point::new(30, 6),
        delta: 400_000,
        mods: Mods::NONE,
    });
    println!("── one million rows ───────────────────────────────────────────");
    println!("{}\n", huge.screen());
    println!(
        "elements mounted: {}   display-list items: {}",
        huge.ui.live_count(),
        huge.ui.spec().items.len()
    );
}
