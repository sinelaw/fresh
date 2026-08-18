//! Async ownership (plan phase L5a).

use std::sync::mpsc::channel;
use std::thread;

use fresh_ui::behavior::{TaskHandle, Tasks};
use fresh_ui::{col, text, BuildCx, Component, ComponentExt, InitCx, Node, Size, Ui};

const FRAME: Size = Size { w: 20, h: 5 };

#[derive(Default)]
struct Loaded {
    lines: Vec<String>,
    /// Held so the behavior lives as long as the state that owns it.
    _tasks: Option<std::rc::Rc<Tasks<String>>>,
}

/// Publishes its handle so the test can play the part of the worker thread.
struct Loader(
    std::rc::Rc<std::cell::RefCell<Option<TaskHandle<String>>>>,
    &'static str,
);

impl Component<()> for Loader {
    type State = Loaded;

    fn init(&self, cx: &mut InitCx<'_, ()>) -> Loaded {
        let tasks = cx.register(Tasks::<String>::new());
        let up = cx.updater::<Loaded>();
        tasks.on_result(move |line| {
            up.set(move |s: &mut Loaded| s.lines.push(line));
        });
        *self.0.borrow_mut() = Some(tasks.launch_replacing(self.1));
        Loaded {
            lines: Vec::new(),
            tasks: Some(tasks),
        }
    }

    fn build(&self, s: &Loaded, _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        col().children(s.lines.iter().map(text))
    }
}

fn lines(ui: &Ui<()>) -> Vec<String> {
    ui.spec()
        .items
        .iter()
        .filter_map(|i| match &i.draw {
            fresh_ui::Draw::Lines(l) => Some(l[0].to_string()),
            _ => None,
        })
        .collect()
}

#[test]
fn a_result_is_delivered_between_frames_and_never_during_one() {
    let slot = std::rc::Rc::new(std::cell::RefCell::new(None));
    let mut ui: Ui<()> = Ui::new();
    ui.frame(col().child(Loader(slot.clone(), "load").node()), FRAME);
    assert!(lines(&ui).is_empty());

    let handle = slot.borrow_mut().take().unwrap();
    // From another thread, as real work would be.
    let (done_tx, done_rx) = channel();
    thread::spawn(move || {
        assert!(handle.deliver("first".into()));
        done_tx.send(()).unwrap();
    })
    .join()
    .unwrap();
    done_rx.recv().unwrap();

    // Nothing has happened yet: delivery is the scheduler's move, not the
    // worker's.
    assert!(lines(&ui).is_empty());

    ui.tick();
    assert_eq!(lines(&ui), vec!["first".to_string()]);
}

#[test]
fn a_result_that_arrives_after_teardown_never_reaches_a_handler() {
    let slot = std::rc::Rc::new(std::cell::RefCell::new(None));
    let mut ui: Ui<()> = Ui::new();
    ui.frame(col().child(Loader(slot.clone(), "load").node()), FRAME);
    let handle = slot
        .borrow()
        .as_ref()
        .map(|h: &TaskHandle<String>| h.is_live());
    assert_eq!(handle, Some(true));

    // Unmount the component while the work is still outstanding.
    ui.frame(col().child(text("gone")), FRAME);

    let handle = slot.borrow_mut().take().unwrap();
    assert!(!handle.is_live(), "the launch knows its owner is gone");
    assert!(!handle.deliver("late".into()), "and refuses the result");

    ui.tick();
    assert_eq!(lines(&ui), vec!["gone".to_string()]);
}

#[test]
fn two_launches_under_one_tag_leave_only_the_later_one_live() {
    let tasks = Tasks::<u32>::new();
    let first = tasks.launch_replacing("search");
    let second = tasks.launch_replacing("search");

    assert!(!first.is_live(), "superseded");
    assert!(second.is_live());
    assert!(!first.deliver(1), "the older result is dropped, not raced");
    assert!(second.deliver(2));

    let seen = std::rc::Rc::new(std::cell::RefCell::new(Vec::new()));
    let s = seen.clone();
    tasks.on_result(move |v| s.borrow_mut().push(v));
    assert_eq!(tasks.drain(), 1);
    assert_eq!(*seen.borrow(), vec![2]);
}

#[test]
fn a_different_tag_is_not_superseded() {
    let tasks = Tasks::<u32>::new();
    let a = tasks.launch_replacing("a");
    let b = tasks.launch_replacing("b");
    assert!(a.is_live() && b.is_live());
    assert!(a.deliver(1) && b.deliver(2));

    let seen = std::rc::Rc::new(std::cell::RefCell::new(Vec::new()));
    let s = seen.clone();
    tasks.on_result(move |v| s.borrow_mut().push(v));
    tasks.drain();
    assert_eq!(*seen.borrow(), vec![1, 2]);
}
