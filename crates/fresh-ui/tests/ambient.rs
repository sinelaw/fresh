//! Ambient values (plan phase L2b).

use std::rc::Rc;

use fresh_ui::{col, provide, text, Ambient, BuildCx, Component, ComponentExt, InitCx, Node, Ui};

#[derive(Debug, PartialEq, Eq)]
struct Theme(&'static str);

#[derive(Debug, PartialEq, Eq)]
struct Locale(&'static str);

static THEME: Ambient<Theme> = Ambient::new("theme");
static LOCALE: Ambient<Locale> = Ambient::new("locale");

/// Reads the theme in `build`, so it tracks changes.
struct Themed;

impl Component<()> for Themed {
    type State = ();
    fn build(&self, _s: &(), cx: &mut BuildCx<'_, ()>) -> Node<()> {
        let theme = cx.read(&THEME).map(|t| t.0).unwrap_or("none");
        text(theme)
    }
}

/// Reads nothing. Must not rebuild when the theme changes.
struct Indifferent;

impl Component<()> for Indifferent {
    type State = ();
    fn build(&self, _s: &(), _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        text("static")
    }
}

/// Reads the theme in `init` and caches it. This is the snapshot rule's
/// failure case: the cached value does not track later changes.
struct Snapshotter;

impl Component<()> for Snapshotter {
    type State = Option<Rc<Theme>>;
    fn init(&self, cx: &mut InitCx<'_, ()>) -> Self::State {
        cx.read(&THEME)
    }
    fn build(&self, s: &Self::State, _cx: &mut BuildCx<'_, ()>) -> Node<()> {
        text(s.as_ref().map(|t| t.0).unwrap_or("none"))
    }
}

fn tree(theme: &'static str) -> Node<()> {
    provide(
        &THEME,
        Rc::new(Theme(theme)),
        col().children([Themed.node(), Indifferent.node()]),
    )
}

#[test]
fn a_dependent_rebuilds_when_the_value_changes_and_not_otherwise() {
    let mut ui: Ui<()> = Ui::new();
    ui.reconcile(tree("dark"));
    let themed = ui.at(&[0, 0]).unwrap();
    let other = ui.at(&[0, 1]).unwrap();

    let builds_themed = ui.builds(themed);
    let builds_other = ui.builds(other);

    // Same value instance-for-instance is still a different Rc, so this counts
    // as a change; the point of the test is which elements react.
    ui.reconcile(tree("light"));

    assert_eq!(
        ui.builds(themed),
        builds_themed + 1,
        "the reader rebuilds once"
    );
    assert_eq!(
        ui.builds(other),
        builds_other + 1,
        "and its sibling rebuilds too — but only because the parent rebuilt it"
    );

    // Now change the ambient without re-supplying the whole tree, which is the
    // case the dependent list exists for.
    let provider = ui.root().unwrap();
    assert_eq!(ui.dependents(provider), vec![themed]);
}

#[test]
fn an_unchanged_value_marks_nobody() {
    let value = Rc::new(Theme("dark"));
    let build = |v: &Rc<Theme>| {
        provide(
            &THEME,
            v.clone(),
            col().children([Themed.node(), Indifferent.node()]),
        )
    };

    let mut ui: Ui<()> = Ui::new();
    ui.reconcile(build(&value));
    let themed = ui.at(&[0, 0]).unwrap();
    let provider = ui.root().unwrap();

    // The same instance handed back: the provider swaps nothing and marks
    // nobody. The rebuild that does happen comes from re-supplying the root.
    let before = ui.builds(themed);
    ui.reconcile(build(&value));
    assert_eq!(ui.builds(themed), before + 1);
    assert!(ui.dependents(provider).contains(&themed));

    // A rebuild driven only by the dirty set now finds nothing marked.
    ui.trace(true);
    ui.flush();
    assert!(ui.take_build_log().is_empty());
}

#[test]
fn the_nearest_provider_wins_and_unrelated_ambients_do_not_collide() {
    let mut ui: Ui<()> = Ui::new();
    ui.reconcile(provide(
        &THEME,
        Rc::new(Theme("outer")),
        provide(
            &LOCALE,
            Rc::new(Locale("en")),
            provide(&THEME, Rc::new(Theme("inner")), Themed.node()),
        ),
    ));

    let themed = ui.at(&[0, 0, 0]).unwrap();
    let inner_provider = ui.at(&[0, 0]).unwrap();
    let outer_provider = ui.root().unwrap();

    assert_eq!(ui.dependents(inner_provider), vec![themed]);
    assert!(
        ui.dependents(outer_provider).is_empty(),
        "shadowed, so never read"
    );
    let rendered = ui.at(&[0, 0, 0, 0]).unwrap();
    assert_eq!(
        ui.text_of(rendered).as_deref(),
        Some("inner"),
        "the nearest provider supplied the value"
    );
}

#[test]
fn replacing_one_ambient_with_another_remounts_rather_than_aliasing() {
    let mut ui: Ui<()> = Ui::new();
    ui.reconcile(provide(&THEME, Rc::new(Theme("dark")), Themed.node()));
    let before = ui.at(&[0]).unwrap();

    ui.reconcile(provide(&LOCALE, Rc::new(Locale("en")), Themed.node()));
    let after = ui.at(&[0]).unwrap();

    assert_ne!(
        before, after,
        "the ambient's identity is part of the element type"
    );
}

#[test]
fn a_dependent_that_is_disposed_stops_being_a_dependent() {
    let mut ui: Ui<()> = Ui::new();
    ui.reconcile(provide(
        &THEME,
        Rc::new(Theme("dark")),
        col().child(Themed.node()),
    ));
    let provider = ui.root().unwrap();
    let themed = ui.at(&[0, 0]).unwrap();
    assert_eq!(ui.dependents(provider), vec![themed]);

    ui.reconcile(provide(
        &THEME,
        Rc::new(Theme("dark")),
        col().child(text("plain")),
    ));
    assert!(ui.dependents(provider).is_empty());
    assert!(!ui.is_live(themed));
}

#[cfg(debug_assertions)]
#[test]
#[should_panic(expected = "only read it in init()")]
fn a_constructor_read_that_goes_stale_is_reported() {
    let mut ui: Ui<()> = Ui::new();
    ui.reconcile(provide(&THEME, Rc::new(Theme("dark")), Snapshotter.node()));
    // The provider's value changes; the snapshotter cached it and cannot see
    // the change. Without the assertion this produces stale output silently.
    ui.reconcile(provide(&THEME, Rc::new(Theme("light")), Snapshotter.node()));
}
