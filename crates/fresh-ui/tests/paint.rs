//! The display list (plan phase L4).

use fresh_ui::{
    col, layer, layout_reader, row, text, viewport, Align, Anchor, Draw, Modality, Node, Place,
    Rect, Scrim, Size, Sizing, Ui,
};

const FRAME: Size = Size { w: 40, h: 6 };

/// A compact rendering of the display list, for snapshot comparison.
fn snapshot(spec: &fresh_ui::LayoutSpec) -> String {
    let mut out = String::new();
    for i in &spec.items {
        let r = i.rect;
        let what = match &i.draw {
            Draw::Fill => "fill".to_string(),
            Draw::Border => "border".to_string(),
            Draw::Selectable => "selectable".to_string(),
            Draw::Scrim(s) => format!("scrim {s:?}"),
            Draw::Lines(l) => format!(
                "text {:?}",
                l.iter().map(|s| s.to_string()).collect::<Vec<_>>()
            ),
            Draw::Scrollbar {
                offset,
                content,
                window,
            } => {
                format!("scrollbar {offset}/{content}/{window}")
            }
            Draw::Host(h) => format!("host {}", h.0),
        };
        let theme = match i.theme.as_str() {
            "" => String::new(),
            t => format!(" [{t}]"),
        };
        out.push_str(&format!(
            "{},{} {}x{}{} {}\n",
            r.x, r.y, r.w, r.h, theme, what
        ));
    }
    out
}

#[test]
fn the_display_list_is_flat_ordered_and_absolute() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col().theme("app").children([
            text("title").h(Sizing::Cells(1)).theme("title"),
            row().flex(1).children([
                col().w(Sizing::Cells(10)).border().child(text("side")),
                text("body").flex(1),
            ]),
        ]),
        FRAME,
    );

    assert_eq!(
        snapshot(spec),
        "\
0,0 40x6 [app] fill
0,0 40x1 [title] fill
0,0 40x1 [title] text [\"title\"]
0,1 10x5 [app] border
1,2 8x1 [app] text [\"side\"]
10,1 30x5 [app] text [\"body\"]
"
    );
}

#[test]
fn a_keyed_subtree_can_be_found_in_the_index() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col().children([
            text("a").key("first"),
            col().key("group").children([text("b"), text("c")]),
        ]),
        FRAME,
    );

    let group = spec.items_for(&"group".into());
    assert_eq!(
        group.len(),
        2,
        "the group's own box draws nothing; its texts do"
    );
    assert!(matches!(&group[0].draw, Draw::Lines(l) if &*l[0] == "b"));
    assert_eq!(spec.items_for(&"first".into()).len(), 1);
    assert_eq!(spec.items_for(&"absent".into()).len(), 0);
}

#[test]
fn a_viewport_emits_only_what_is_inside_its_window() {
    let mut ui: Ui<()> = Ui::new();
    let rows: Vec<Node<()>> = (0..100).map(|i| text(format!("row {i}"))).collect();
    let spec = ui.frame(viewport(col().children(rows)).scrollbar(), FRAME);

    let visible: Vec<_> = spec.visible().collect();
    // Six rows fit; the rest are clipped out, and one scrollbar is emitted.
    assert_eq!(
        visible
            .iter()
            .filter(|i| matches!(i.draw, Draw::Lines(_)))
            .count(),
        6
    );
    assert!(visible.iter().any(|i| matches!(
        i.draw,
        Draw::Scrollbar {
            offset: 0,
            content: 100,
            window: 6
        }
    )));
}

#[test]
fn a_windowed_list_over_a_hundred_thousand_rows_emits_a_screenful() {
    const N: usize = 100_000;
    let mut ui: Ui<()> = Ui::new();

    // The shape `List::virtual` takes: a reader that turns the height it is
    // given into an index range, so build itself is O(visible).
    let spec = ui.frame(
        layout_reader(|info| {
            col().children((0..info.constraints.max_h as usize).map(|i| text(format!("row {i}"))))
        }),
        FRAME,
    );

    assert_eq!(spec.items.len(), 6, "a screenful, out of {N}");
    assert!(
        ui.live_count() < 20,
        "and only a screenful of elements exist"
    );
}

#[test]
fn an_opaque_full_frame_layer_suppresses_everything_beneath_it() {
    let mut ui: Ui<()> = Ui::new();
    let lit = col().children([
        text("background one"),
        text("background two"),
        layer()
            .anchor(Anchor::Screen(Align::Center))
            .modality(Modality::Exclusive)
            .scrim(Some(Scrim::Opaque))
            .child(text("on top")),
    ]);
    let spec = ui.frame(lit, FRAME);

    assert_eq!(
        snapshot(spec),
        "\
0,0 40x6 scrim Opaque
17,2 6x1 text [\"on top\"]
"
    );
}

#[test]
fn a_dimming_layer_keeps_what_is_beneath_it() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col().children([
            text("behind"),
            layer()
                .anchor(Anchor::Screen(Align::Center))
                .scrim(Some(Scrim::Dim))
                .child(text("front")),
        ]),
        FRAME,
    );
    let kinds: Vec<&Draw> = spec.items.iter().map(|i| &i.draw).collect();
    assert!(
        matches!(kinds[0], Draw::Lines(_)),
        "the content is still there"
    );
    assert!(matches!(kinds[1], Draw::Scrim(Scrim::Dim)));
    assert!(matches!(kinds[2], Draw::Lines(_)));
}

#[test]
fn a_layer_anchored_below_its_parent_flips_when_it_would_overflow() {
    let mut ui: Ui<()> = Ui::new();
    // Trigger near the bottom: a dropdown placed below would run off the frame,
    // so FLIP puts it above instead.
    let spec = ui.frame(
        col().children([
            text("filler").flex(1),
            col().h(Sizing::Cells(1)).children([
                text("trigger"),
                layer()
                    .anchor(Anchor::Parent)
                    .place(Place::Below)
                    .fit(fresh_ui::Fit::FLIP.or(fresh_ui::Fit::CLAMP))
                    .child(col().children([text("one"), text("two"), text("three")])),
            ]),
        ]),
        FRAME,
    );

    let popup: Vec<Rect> = spec
        .items
        .iter()
        .filter(|i| matches!(&i.draw, Draw::Lines(l) if ["one","two","three"].contains(&&*l[0])))
        .map(|i| i.rect)
        .collect();
    assert_eq!(popup.len(), 3);
    assert_eq!(popup[0].y, 2, "flipped above the trigger at row 5");
}

#[test]
fn theme_provenance_is_inherited_and_overridden() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col().theme("outer").children([
            text("inherits"),
            col().theme("inner").child(text("overrides")),
        ]),
        FRAME,
    );
    let themes: Vec<String> = spec
        .items
        .iter()
        .filter(|i| matches!(i.draw, Draw::Lines(_)))
        .map(|i| i.theme.as_str().to_string())
        .collect();
    assert_eq!(themes, vec!["outer".to_string(), "inner".to_string()]);
}
