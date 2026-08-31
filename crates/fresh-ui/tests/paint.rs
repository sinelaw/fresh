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
            Draw::Border(bs) => format!("border:{bs:?}"),
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

// -- styled runs -------------------------------------------------------------

/// Rows of painted text, keyed by screen row.
///
/// Handles both shapes a text run can take: an unstyled run is one item whose
/// `Lines` holds successive rows, and a styled one is an item per fragment,
/// several of which may share a row.
fn painted_rows(spec: &fresh_ui::LayoutSpec) -> Vec<String> {
    let mut rows: std::collections::BTreeMap<i32, String> = Default::default();
    for it in &spec.items {
        if let Draw::Lines(lines) = &it.draw {
            for (i, line) in lines.iter().enumerate() {
                rows.entry(it.rect.y + i as i32).or_default().push_str(line);
            }
        }
    }
    rows.into_values().collect()
}

/// A run whose pieces are styled independently emits one item per piece, each
/// carrying its own theme. The display list keeps its one-theme-per-item
/// contract, so no backend has to learn about spans.
#[test]
fn a_styled_run_emits_one_item_per_piece() {
    use fresh_ui::{text_runs, Run};
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        text_runs([
            Run::plain("Op"),
            Run::themed("e", "mnemonic"),
            Run::plain("n"),
        ]),
        Size::new(20, 1),
    );

    let items: Vec<(String, String, i32, u16)> = spec
        .items
        .iter()
        .filter_map(|it| match &it.draw {
            Draw::Lines(l) => Some((
                l.join(""),
                it.theme.as_str().to_string(),
                it.rect.x,
                it.rect.w,
            )),
            _ => None,
        })
        .collect();

    assert_eq!(
        items,
        vec![
            ("Op".to_string(), String::new(), 0, 2),
            ("e".to_string(), "mnemonic".to_string(), 2, 1),
            ("n".to_string(), String::new(), 3, 1),
        ],
        "pieces should tile left to right, each with its own theme"
    );
}

/// An unstyled run is still a single item — the common case pays nothing.
#[test]
fn an_unstyled_run_is_still_one_item() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(text("plain text"), Size::new(20, 1));
    let lines: Vec<&Draw> = spec
        .items
        .iter()
        .map(|i| &i.draw)
        .filter(|d| matches!(d, Draw::Lines(_)))
        .collect();
    assert_eq!(lines.len(), 1);
}

/// **The reason spans exist rather than sibling nodes.** The pieces are one
/// logical string, so wrapping runs across the boundaries between them: a break
/// may fall inside a piece, and a row may be composed of several.
#[test]
fn wrapping_runs_across_piece_boundaries() {
    use fresh_ui::{text_runs, Run};
    let mut ui: Ui<()> = Ui::new();
    // "hello " + "brave " + "world" = 17 cells, wrapped at 12.
    let spec = ui.frame(
        text_runs([
            Run::plain("hello "),
            Run::themed("brave ", "em"),
            Run::plain("world"),
        ])
        .wrap()
        .w(Sizing::Cells(12)),
        Size::new(12, 4),
    );

    let text = painted_rows(spec);
    assert_eq!(
        text,
        vec!["hello brave".to_string(), "world".to_string()],
        "the pieces wrap as one string"
    );

    // The styled piece kept its theme even though the wrap fell inside it.
    let em: Vec<String> = spec
        .items
        .iter()
        .filter(|it| it.theme.as_str() == "em")
        .filter_map(|it| match &it.draw {
            Draw::Lines(l) => Some(l.join("")),
            _ => None,
        })
        .collect();
    assert_eq!(em, vec!["brave".to_string()]);
}

/// Styling never changes where text breaks: a styled run and the same text
/// unstyled wrap to identical rows.
#[test]
fn styling_does_not_change_where_text_breaks() {
    use fresh_ui::{text_runs, Run};
    let rows_of = |node: Node<()>| -> Vec<String> {
        let mut ui: Ui<()> = Ui::new();
        painted_rows(ui.frame(node, Size::new(12, 6)))
    };

    let sentence = "the quick brown fox jumps";
    let plain = rows_of(text(sentence).wrap().w(Sizing::Cells(12)));
    let styled = rows_of(
        text_runs([
            Run::plain("the quick "),
            Run::themed("brown", "em"),
            Run::plain(" fox jumps"),
        ])
        .wrap()
        .w(Sizing::Cells(12)),
    );
    assert_eq!(plain, styled);
}

/// **A wrapped list entry stays inside its own entry.**
///
/// `Wrap::Hanging` starts every continuation row at the line's own leading
/// whitespace. Only the thing that wraps knows where it broke, so only it can
/// say what the next row starts with — a caller wanting this had to wrap the
/// text itself, which means deciding the width, which is layout's answer.
#[test]
fn a_hanging_wrap_indents_every_row_after_the_first() {
    let rows = |node: Node<()>| -> Vec<String> {
        let mut ui: Ui<()> = Ui::new();
        painted_rows(ui.frame(node, Size::new(20, 6)))
    };
    let entry = "    sep  a string put between the values";

    assert_eq!(
        rows(text(entry).wrap_hanging().w(Sizing::Cells(20))),
        vec![
            "    sep  a string".to_string(),
            "    put between the".to_string(),
            "    values".to_string(),
        ],
        "the four leading spaces carry to every continuation row"
    );
    // Plain wrapping is unchanged: the continuation rows start at the edge.
    assert_eq!(
        rows(text(entry).wrap().w(Sizing::Cells(20))),
        vec![
            "    sep  a string".to_string(),
            "put between the".to_string(),
            "values".to_string(),
        ],
    );
}

/// **Wrapping is not allowed to normalise the text it wraps.**
///
/// `split(' ')` yields an empty piece per space, and skipping empties quietly
/// turned `"    sep  a string"` into `"sep a string"` — so a wrapped popup lost
/// the indent on its *first* row and the double space that separated a
/// parameter name from its description. The break still eats the space it
/// broke at; everything else survives.
#[test]
fn wrapping_keeps_the_spaces_the_text_came_with() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        text("    sep  a string here").wrap().w(Sizing::Cells(20)),
        Size::new(20, 4),
    );
    assert_eq!(
        painted_rows(spec),
        vec!["    sep  a string".to_string(), "here".to_string()],
        "the leading four and the inner two are the text's, not the wrapper's"
    );
}

/// The indent is dropped when it would leave the text almost nothing — a
/// deeply indented line in a narrow box reads better flush left than one word
/// per row.
#[test]
fn a_hanging_indent_yields_when_it_would_starve_the_text() {
    let mut ui: Ui<()> = Ui::new();
    // 8 spaces of indent in 12 columns leaves 4 for the text, under the
    // `HANGING_MIN_TEXT` floor of 10.
    let spec = ui.frame(
        text("        alpha beta gamma")
            .wrap_hanging()
            .w(Sizing::Cells(12)),
        Size::new(12, 6),
    );
    let rows = painted_rows(spec);
    assert!(
        rows.iter().skip(1).all(|r| !r.starts_with(' ')),
        "no room for the indent, so it is not applied: {rows:?}"
    );
}

// -- the in-flow / out-of-flow split -----------------------------------------

/// **What `layers_from` is for.** A backend that draws content of its own
/// between the tree's in-flow half and its layers — a host mid-migration
/// painting surfaces the tree does not own yet — has to know where one ends
/// and the other begins. Everything before the mark is the tree; everything
/// from it on is a layer.
#[test]
fn layers_from_marks_where_the_out_of_flow_half_begins() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col().children([
            text("behind one"),
            text("behind two"),
            layer()
                .anchor(Anchor::Screen(Align::Center))
                .child(text("front")),
        ]),
        FRAME,
    );
    assert_eq!(spec.layers_from, 2);
    assert_eq!(spec.in_flow().len(), 2);
    assert_eq!(spec.layers().len(), 1);
    assert!(matches!(&spec.layers()[0].draw, Draw::Lines(l) if &*l[0] == "front"));
}

/// With no layer at all the whole list is in flow, and the out-of-flow half is
/// empty rather than absent — a backend's second pass simply finds nothing.
#[test]
fn a_frame_with_no_layers_is_all_in_flow() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(col().children([text("a"), text("b")]), FRAME);
    assert_eq!(spec.layers_from, spec.items.len());
    assert!(spec.layers().is_empty());
    assert_eq!(spec.in_flow().len(), spec.items.len());
}

/// **The case nothing outside the library can get right.** A scrim belongs to
/// no keyed subtree — it carries no key at all — and it is pushed *before* the
/// layer's own items. A backend deriving the split from the key index would
/// put it on the wrong side and paint the dimming under the content it is
/// meant to dim.
#[test]
fn a_scrim_belongs_to_the_out_of_flow_half() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col().children([
            text("behind"),
            layer()
                .key("modal")
                .anchor(Anchor::Screen(Align::Center))
                .scrim(Some(Scrim::Dim))
                .child(text("front")),
        ]),
        FRAME,
    );
    assert!(
        spec.layers()
            .iter()
            .any(|i| matches!(i.draw, Draw::Scrim(Scrim::Dim))),
        "the scrim is out of flow: {:?}",
        spec.items.iter().map(|i| &i.draw).collect::<Vec<_>>()
    );
    assert!(
        spec.in_flow()
            .iter()
            .all(|i| !matches!(i.draw, Draw::Scrim(_))),
        "and nothing in flow is one"
    );
    assert!(
        spec.layers()[0].key.is_none(),
        "and it is unkeyed, which is why the index cannot classify it"
    );
}

/// **The other case.** A layer need not be keyed — `widgets::Dropdown`'s is
/// not — so it produces no index entry, and a backend deriving the split from
/// the index would treat its whole pop-over as in-flow content.
#[test]
fn an_unkeyed_layer_is_still_out_of_flow() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col().children([
            text("behind"),
            layer()
                .anchor(Anchor::Screen(Align::Center))
                .child(text("front")),
        ]),
        FRAME,
    );
    assert!(spec.index.is_empty(), "nothing here is keyed");
    assert_eq!(spec.layers().len(), 1, "and the split still knows");
}

/// An opaque scrim erases the in-flow half, so the whole list is out of flow
/// and the mark has to move with it — otherwise it points past the end.
#[test]
fn an_opaque_scrim_leaves_nothing_in_flow() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col().children([
            text("behind one"),
            text("behind two"),
            layer()
                .anchor(Anchor::Screen(Align::Center))
                .modality(Modality::Exclusive)
                .scrim(Some(Scrim::Opaque))
                .child(text("on top")),
        ]),
        FRAME,
    );
    assert_eq!(spec.layers_from, 0);
    assert!(spec.in_flow().is_empty());
    assert_eq!(spec.layers().len(), spec.items.len());
}

/// The two halves partition the list: nothing is dropped and nothing is
/// counted twice, which is what makes a two-pass backend equivalent to a
/// one-pass one.
#[test]
fn the_two_halves_partition_the_list() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col().children([
            text("behind"),
            layer()
                .key("a")
                .anchor(Anchor::Screen(Align::Start))
                .scrim(Some(Scrim::Dim))
                .child(text("one")),
            layer()
                .key("b")
                .anchor(Anchor::Screen(Align::End))
                .child(text("two")),
        ]),
        FRAME,
    );
    assert_eq!(spec.in_flow().len() + spec.layers().len(), spec.items.len());
    assert!(spec.layers_from <= spec.items.len());
}

/// **An item declares how much room it has.** Layout gives a constrained node
/// the width it was allowed, not the width its content wants — so a backend
/// that ignores the rect and simply writes the string paints through whatever
/// encloses it. The clipping fix is in the backends (`tests/support/screen.rs`
/// and `examples/interactive.rs`); this pins the fact they have to honour.
#[test]
fn a_constrained_run_reports_the_rect_it_was_given_not_the_one_it_wanted() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col()
            .w(Sizing::Cells(6))
            .child(text("0123456789").w(Sizing::Cells(4))),
        Size { w: 12, h: 3 },
    );
    let item = spec
        .items
        .iter()
        .find(|i| matches!(i.draw, Draw::Lines(_)))
        .expect("a text item");
    assert_eq!(item.rect.w, 4, "four columns is what it was given");
    let Draw::Lines(lines) = &item.draw else {
        unreachable!()
    };
    assert_eq!(
        &*lines[0], "0123456789",
        "and the run is longer than that, which is the backend's problem to \
         clip rather than the library's to hide"
    );
}

// -- anchoring and placement -------------------------------------------------
//
// `Anchor::Node` and four of the six `Place` variants had no caller and no test
// anywhere in the repository — not in the library, its tests, its demo, or the
// editor consuming it. These cover the placement each one promises, so that the
// first real consumer inherits a guarantee rather than an assumption.

/// The rectangles a keyed node's own subtree produced.
fn rects_of(spec: &fresh_ui::LayoutSpec, needles: &[&str]) -> Vec<Rect> {
    spec.items
        .iter()
        .filter(|i| matches!(&i.draw, Draw::Lines(l) if needles.contains(&&*l[0])))
        .map(|i| i.rect)
        .collect()
}

/// A trigger of known geometry with a layer hung off it, so each `Place` can be
/// checked against one fixed anchor.
fn placed(place: Place, anchor: Anchor) -> fresh_ui::LayoutSpec {
    let mut ui: Ui<()> = Ui::new();
    ui.frame(
        col().children([
            text("pad").h(Sizing::Cells(2)),
            row().children([
                text("pad2").w(Sizing::Cells(4)),
                col()
                    .key("trigger")
                    .w(Sizing::Cells(6))
                    .h(Sizing::Cells(2))
                    .children([
                        text("T").h(Sizing::Cells(1)),
                        layer()
                            .anchor(anchor)
                            .place(place)
                            .child(text("pop").w(Sizing::Cells(3)).h(Sizing::Cells(1))),
                    ]),
            ]),
        ]),
        Size { w: 30, h: 12 },
    )
    .clone()
}

/// **`Anchor::Node` resolves a key to that node's rectangle**, which is what a
/// chain of dependent layers needs: each level names the node it opens from
/// rather than a point someone computed.
#[test]
fn a_layer_anchored_to_a_keyed_node_places_against_that_node() {
    let spec = placed(Place::Below, Anchor::Node("trigger".into()));
    let pop = rects_of(&spec, &["pop"]);
    assert_eq!(pop.len(), 1);
    // The trigger sits at x=4, y=2, 6x2 — so "below" is its bottom edge.
    assert_eq!((pop[0].x, pop[0].y), (4, 4));
}

/// An `Anchor::Node` naming a key that is not in the tree falls back to the
/// declaring parent rather than to the origin — a stale key misplaces a layer,
/// it does not teleport it to a corner.
#[test]
fn an_anchor_naming_a_missing_node_falls_back_to_the_parent() {
    let spec = placed(Place::Below, Anchor::Node("nope".into()));
    let pop = rects_of(&spec, &["pop"]);
    assert_eq!(pop.len(), 1);
    assert_eq!(
        (pop[0].x, pop[0].y),
        (4, 4),
        "the parent is the trigger here"
    );
}

/// `Place::Above` puts the layer's *bottom* on the anchor's top edge.
#[test]
fn place_above_sits_on_the_anchors_top_edge() {
    let spec = placed(Place::Above, Anchor::Parent);
    let pop = rects_of(&spec, &["pop"]);
    assert_eq!((pop[0].x, pop[0].y), (4, 1), "one row tall, so top - 1");
}

/// `Place::RightOf` puts the layer's left edge on the anchor's right edge, at
/// the anchor's top — the placement a submenu chain is built from.
#[test]
fn place_right_of_starts_at_the_anchors_right_edge() {
    let spec = placed(Place::RightOf, Anchor::Parent);
    let pop = rects_of(&spec, &["pop"]);
    assert_eq!((pop[0].x, pop[0].y), (10, 2), "trigger x=4 w=6");
}

/// `Place::LeftOf` is its mirror: the layer's *right* edge on the anchor's left.
#[test]
fn place_left_of_ends_at_the_anchors_left_edge() {
    let spec = placed(Place::LeftOf, Anchor::Parent);
    let pop = rects_of(&spec, &["pop"]);
    assert_eq!((pop[0].x, pop[0].y), (1, 2), "three wide, so x - 3");
}

/// `Place::Fill` takes the anchor's rectangle outright — the layer is not
/// placed *near* the anchor, it is sized *to* it.
#[test]
fn place_fill_takes_the_anchors_whole_rectangle() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui
        .frame(
            col().children([
                text("pad").h(Sizing::Cells(2)),
                col()
                    .key("target")
                    .w(Sizing::Cells(8))
                    .h(Sizing::Cells(3))
                    .children([
                        text("T").h(Sizing::Cells(1)),
                        layer()
                            .anchor(Anchor::Node("target".into()))
                            .place(Place::Fill)
                            .child(col().theme("cover").child(text("x"))),
                    ]),
            ]),
            Size { w: 30, h: 12 },
        )
        .clone();
    let cover = spec
        .items
        .iter()
        .find(|i| i.theme.as_str() == "cover")
        .expect("the filling layer paints its ground");
    assert_eq!(
        (cover.rect.x, cover.rect.y, cover.rect.w, cover.rect.h),
        (0, 2, 8, 3),
        "the anchor's rectangle, not a placement beside it"
    );
}

/// **Nested layers resolve against their parent's final rectangle.** A layer
/// declared inside another is appended to the worklist while that one is being
/// arranged, so the inner one sees where the outer actually landed — which is
/// what makes a dependent chain (a submenu off a menu) expressible at all.
#[test]
fn a_layer_inside_a_layer_places_against_where_its_parent_landed() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui
        .frame(
            col().children([
                text("pad").h(Sizing::Cells(3)),
                col().key("root").w(Sizing::Cells(5)).children([
                    text("R").h(Sizing::Cells(1)),
                    layer()
                        .key("outer")
                        .anchor(Anchor::Node("root".into()))
                        .place(Place::Below)
                        .child(
                            col().key("outerbox").w(Sizing::Cells(7)).children([
                                text("out").h(Sizing::Cells(1)),
                                layer()
                                    .anchor(Anchor::Node("outerbox".into()))
                                    .place(Place::RightOf)
                                    .child(text("in").w(Sizing::Cells(2))),
                            ]),
                        ),
                ]),
            ]),
            Size { w: 30, h: 12 },
        )
        .clone();
    let out = rects_of(&spec, &["out"]);
    let inn = rects_of(&spec, &["in"]);
    assert_eq!(out.len(), 1, "the outer layer painted");
    assert_eq!(inn.len(), 1, "and so did the one inside it");
    assert_eq!(
        inn[0].x,
        out[0].x + 7,
        "the inner layer starts at the outer box's right edge, so it saw the \
         rectangle the outer one was finally given"
    );
}

// ── clipping ───────────────────────────────────────────────────────────────

/// A row whose fixed children cannot fit, where one of them carries an
/// unsatisfiable `min_w` floor. Ordinary over-wide children are clamped to the
/// space left, so they never escape; a floor is a promise layout keeps even
/// when keeping it puts a sibling outside the parent. That is the one way
/// content reaches a box's own frame, and it is not exotic — a name, a gap
/// that will not close, and a status slot is the shape of a file-tree row.
fn overflowing_row<M: 'static>() -> Node<M> {
    row().h(Sizing::Cells(1)).children([
        text("a-name!").w(Sizing::Cells(7)),
        row().flex(1).min_w(1),
        text("M").w(Sizing::Cells(1)),
    ])
}

/// The escape, with nothing to stop it: the slot lands on the frame's column.
#[test]
fn a_min_w_floor_can_place_a_child_outside_its_parent() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col()
            .w(Sizing::Cells(10))
            .border()
            .clip(false)
            .child(overflowing_row()),
        Size { w: 20, h: 4 },
    );
    let slot = spec
        .items
        .iter()
        .find(|i| matches!(&i.draw, Draw::Lines(l) if l.first().map(|s| &**s) == Some("M")))
        .expect("the slot paints");
    assert_eq!(
        slot.visible_rect(),
        Rect {
            x: 9,
            y: 1,
            w: 1,
            h: 1
        },
        "x=9 is the border's own column: unbounded, the slot overwrites the frame"
    );
}

/// The same description, bounded — which is the default, because `border()`
/// turns the bound on. The slot keeps the rectangle layout gave it and paints
/// nothing, so the frame survives.
#[test]
fn a_bordered_box_bounds_what_its_children_paint() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col().w(Sizing::Cells(10)).border().child(overflowing_row()),
        Size { w: 20, h: 4 },
    );
    assert!(
        !spec
            .items
            .iter()
            .any(|i| matches!(&i.draw, Draw::Lines(l) if l.first().map(|s| &**s) == Some("M"))),
        "fully bounded away, so it is not in the display list at all"
    );
    // And nothing else reached the ring either.
    for i in &spec.items {
        if matches!(&i.draw, Draw::Lines(_)) {
            let v = i.visible_rect();
            assert!(v.x >= 1 && v.right() <= 9, "{v:?} escaped the content rect");
        }
    }
}

/// The bound is the *content* rect. A box that clips without a border still
/// subtracts its padding, because padding is the box's own space too.
#[test]
fn the_bound_is_the_content_rect_not_the_outer_edge() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col()
            .w(Sizing::Cells(10))
            .h(Sizing::Cells(4))
            .clip(true)
            .pad(2, 1)
            .child(overflowing_row()),
        Size { w: 20, h: 6 },
    );
    for i in &spec.items {
        let v = i.visible_rect();
        if matches!(&i.draw, Draw::Lines(_)) && !v.is_empty() {
            assert!(
                v.x >= 2 && v.right() <= 8 && v.y >= 1,
                "{v:?} escaped the padding"
            );
        }
    }
}

/// Off by default, so a plain grouping box keeps whatever overflow behaviour
/// its caller was relying on.
#[test]
fn a_plain_box_does_not_bound_its_children() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col().w(Sizing::Cells(8)).child(overflowing_row()),
        Size { w: 20, h: 4 },
    );
    let slot = spec
        .items
        .iter()
        .find(|i| matches!(&i.draw, Draw::Lines(l) if l.first().map(|s| &**s) == Some("M")))
        .expect("the slot paints");
    assert_eq!(slot.visible_rect().x, 8, "one past the box's own 8 cells");
}

/// A box too small to hold its own frame bounds its children to nothing,
/// rather than to a rectangle that wrapped around into a large one.
#[test]
fn a_box_smaller_than_its_own_frame_bounds_to_nothing() {
    let mut ui: Ui<()> = Ui::new();
    let spec = ui.frame(
        col()
            .w(Sizing::Cells(2))
            .h(Sizing::Cells(2))
            .border()
            .child(text("xxxx").w(Sizing::Cells(4))),
        Size { w: 20, h: 4 },
    );
    for i in &spec.items {
        if matches!(&i.draw, Draw::Lines(_)) {
            assert!(
                i.visible_rect().is_empty(),
                "a 2x2 frame has no inside for text to paint in"
            );
        }
    }
}
