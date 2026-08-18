//! Descriptions are plain data (plan phase L0).

use fresh_ui::{
    col, gesture, host, layer, node_key, node_type, resolve, row, text, viewport, Anchor, Desc,
    Dismiss, ElemType, Fit, Key, Modality, Node, Place, Sizing,
};

#[test]
fn a_tree_is_inspectable_without_mounting_it() {
    let n: Node<()> = col()
        .children([
            text("title").h(Sizing::Cells(1)),
            row().flex(1).children([
                text("sidebar").w(Sizing::Cells(20)),
                viewport(text("body")).flex(1),
            ]),
        ])
        .pad(1, 0)
        .gap(2);

    assert_eq!(node_type(&n).0, ElemType::Box);
    assert_eq!(n.children.len(), 2);
    assert_eq!(n.children[0].h, Sizing::Cells(1));
    assert_eq!(n.children[1].children[0].w, Sizing::Cells(20));
    assert_eq!(node_type(&n.children[1].children[1]).0, ElemType::Viewport);
    match &n.desc {
        Desc::Box(p) => {
            assert_eq!(p.pad.x, 1);
            assert_eq!(p.gap, 2);
        }
        _ => panic!("expected a Box"),
    }
}

#[test]
fn keys_come_from_the_caller_and_shared_is_transparent() {
    let inner: Node<()> = text("row").key(("row", 3usize));
    assert_eq!(node_key(&inner), Some(Key::from(("row", 3usize))));

    let wrapped = inner.clone().shared();
    assert_eq!(
        node_type(&wrapped).0,
        ElemType::TextRun,
        "Shared is not a type"
    );
    assert_eq!(node_key(&wrapped), Some(Key::from(("row", 3usize))));
    assert_eq!(resolve(&wrapped).children.len(), 0);
}

#[test]
fn if_collapses_a_node_without_removing_it_from_the_list() {
    let shown: Node<()> = text("x").if_(true);
    let hidden: Node<()> = text("x").if_(false);
    assert_eq!(node_type(&shown).0, ElemType::TextRun);
    assert_eq!(node_type(&hidden).0, ElemType::Box);
    assert_eq!(hidden.w, Sizing::Cells(0));
    assert_eq!(hidden.h, Sizing::Cells(0));
}

#[test]
fn layer_and_leaf_props_are_constructible() {
    let l: Node<()> = layer()
        .anchor(Anchor::Point(4, 9))
        .place(Place::Below)
        .fit(Fit::FLIP.or(Fit::CLAMP))
        .modality(Modality::Inert)
        .dismiss(Dismiss::OUTSIDE_POINTER.or(Dismiss::ESCAPE))
        .child(gesture(text("item")).on_click(|_| ()));

    match &l.desc {
        Desc::Layer(p) => {
            assert_eq!(p.anchor, Anchor::Point(4, 9));
            assert!(p.fit.flip && p.fit.clamp && !p.fit.shift);
            assert!(p.dismiss.outside_pointer && p.dismiss.escape);
        }
        _ => panic!("expected a Layer"),
    }
    assert_eq!(node_type(&l.children[0]).0, ElemType::Gesture);

    let h: Node<()> = host(7u64);
    assert_eq!(node_type(&h).0, ElemType::Host);
}
