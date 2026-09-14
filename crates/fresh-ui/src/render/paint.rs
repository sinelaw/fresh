//! The paint walk, over the render tree.
//!
//! Cost is O(visible items). The walk skips work four ways: off-screen
//! (`rect ∩ clip` empty), occluded (an opaque full-frame layer above), scrolled
//! out (a viewport's clip removes what is outside its window), and absent
//! (never built at all).
//!
//! What each node draws is the render object's business; the framework supplies
//! identity, provenance and order.

use crate::desc::{resolve, Scrim};
use crate::render::geom::{Rect, Size};
use crate::render::object::{Geom, RenderId};
use crate::render::spec::{CursorSpec, Draw, DrawList, Item, LayoutSpec, ThemeKey};
use crate::schedule::Ui;
use crate::ElementId;

/// Whether a paint walk honours the clips layout recorded.
#[derive(Clone, Copy)]
enum Clipping {
    /// The frame's walk: an ancestor's clip cuts what is under it.
    Inherited,
    /// A subtree read for its content: every node is clipped only by
    /// itself, so nothing a viewport scrolled away is lost.
    None,
}

impl<M: 'static> Ui<M> {
    pub(crate) fn flush_paint(&mut self, frame: Size) {
        let mut spec = std::mem::take(&mut self.spec);
        spec.clear();
        spec.frame = frame;
        if let Some(root) = self.render_root {
            self.paint_render(root, &mut spec, Clipping::Inherited);
            // Everything from here on came out of a layer. Recorded before the
            // loop rather than derived after it, because a scrim carries no key
            // and an unkeyed layer leaves no index entry — nothing outside can
            // tell the two halves apart. See `LayoutSpec::layers_from`.
            spec.layers_from = spec.items.len();
            // Layers paint above the content they were declared in, in the
            // order the arrange walk found them.
            for i in 0..self.pending_layers.len() {
                let (lr, _) = self.pending_layers[i];
                self.paint_layer(lr, frame, &mut spec);
            }
        }
        self.spec = spec;
    }

    fn paint_layer(&mut self, lr: RenderId, frame: Size, spec: &mut LayoutSpec) {
        let Some(element) = self.element_of(lr) else {
            return;
        };
        let Some(scrim) = self.layer_geom(lr).map(|g| g.scrim) else {
            return;
        };
        // **A cursor under a layer is not on screen.** The terminal draws its
        // one cursor on top of every cell, so a caret an in-flow surface
        // placed — a text pane's, a field's in the tree below — would blink
        // through whatever a layer paints over it. The cursor placed so far
        // is set aside while the layer paints; afterwards the layer's own
        // cursor stands if it placed one, and otherwise the one set aside
        // comes back unless an item the layer painted — a scrim, a box, a
        // run — lies over its cell. A layer that paints nothing there (a
        // keyboard layer with no surface, a popup beside the caret) leaves
        // it alone.
        let under: Option<CursorSpec> = spec.cursor.take();
        let painted_from = spec.items.len();
        if let Some(kind) = scrim {
            if kind == Scrim::Opaque {
                // Everything under an opaque full-frame scrim is invisible;
                // emitting it would make the backend draw and then overdraw.
                spec.items.clear();
                spec.index.clear();
                // The in-flow half is gone with it, so the whole list is now
                // out of flow.
                spec.layers_from = 0;
            }
            spec.items.push(Item {
                key: None,
                id: element,
                rect: Rect::from_size(frame),
                clip: Rect::from_size(frame),
                theme: ThemeKey::default(),
                classes: crate::render::spec::Classes::default(),
                draw: Draw::Scrim(kind),
            });
        }
        self.paint_render(lr, spec, Clipping::Inherited);
        if spec.cursor.is_none() {
            spec.cursor = under.filter(|c| {
                !spec.items[painted_from..]
                    .iter()
                    .any(|i| i.rect.intersect(i.clip).contains(c.pos))
            });
        }
    }

    /// The display list of one subtree, unclipped.
    ///
    /// Every item the subtree would paint if nothing above it cut it — the
    /// rows a viewport has scrolled out of its window included — in absolute
    /// coordinates, each with its own rectangle as its clip, and in-flow only:
    /// a layer declared inside the subtree is not part of it. It is not a
    /// frame: nothing is drawn and the frame's own list is untouched. For a
    /// host that keeps a text mirror of a subtree somewhere the screen is not
    /// and needs the rows the screen does not show.
    pub fn paint_subtree(&mut self, root: ElementId) -> LayoutSpec {
        let mut spec = LayoutSpec {
            frame: self.frame_size,
            ..LayoutSpec::default()
        };
        if let Some(r) = self.render_for(root) {
            self.paint_render(r, &mut spec, Clipping::None);
        }
        spec.layers_from = spec.items.len();
        spec
    }

    fn paint_render(&mut self, r: RenderId, spec: &mut LayoutSpec, clipping: Clipping) {
        let (element, rect, clip, theme, classes, key, kids, out_of_flow) = {
            let Some(n) = self.render.get(r) else { return };
            (
                n.element,
                n.data.rect,
                match clipping {
                    Clipping::Inherited => n.data.clip,
                    Clipping::None => n.data.rect,
                },
                n.theme.clone(),
                n.classes.clone(),
                n.key.clone(),
                n.children.clone(),
                n.out_of_flow,
            )
        };
        // Off-screen: nothing below can be visible either, because a child's
        // rect is contained in its parent's clip.
        if rect.intersect(clip).is_empty() && !rect.size().is_empty() {
            return;
        }

        let mut list = DrawList::new(element);
        list.key = key.clone();
        list.theme = ThemeKey(theme.clone());
        list.classes = crate::render::spec::Classes(classes.clone());

        let start = spec.items.len();

        // A region that names its own appearance is a region that paints: the
        // backend decides what the name looks like. Emitted before the node's
        // own content, so anything drawn inside it wins.
        //
        // Naming a class counts, for the same reason naming a theme does — both
        // say a backend has something to say about this rectangle, and without
        // an item to hang it on there is nowhere to say it. It is also what
        // separates a control's box from its contents once the list is flat:
        // the `Fill` is where the button *is*, the runs inside it are what the
        // button *says*, and both wear the class.
        let (names_itself, ground) = self
            .arena
            .get(element)
            .map(|e| {
                let d = resolve(&e.desc);
                // A rule is a ground the backend tiles, so naming one makes
                // the box paint exactly as naming a theme or a class does —
                // otherwise a rule on an otherwise-unthemed box would have
                // nowhere to be said.
                let ground = match &d.desc {
                    crate::desc::Desc::Box(b) if b.rule.is_some() => {
                        Some(Draw::Rule(b.rule.clone().expect("checked")))
                    }
                    crate::desc::Desc::Box(b) if b.wash => Some(Draw::Wash),
                    _ => None,
                };
                (
                    e.desc.theme.is_some()
                        || d.theme.is_some()
                        || e.desc.classes.is_some()
                        || d.classes.is_some()
                        || matches!(ground, Some(Draw::Rule(_))),
                    ground,
                )
            })
            .unwrap_or((false, None));
        if names_itself && !rect.is_empty() {
            list.push(ground.unwrap_or(Draw::Fill), Geom { rect, clip });
        }

        if let Some(obj) = self.render.get(r).and_then(|n| n.obj.as_ref()) {
            obj.paint(Geom { rect, clip }, &mut list);
        }
        if let Some(c) = list.cursor {
            spec.cursor = Some(c);
        }
        spec.items.append(&mut list.items);

        for k in kids {
            if self.render.get(k).map(|n| n.out_of_flow).unwrap_or(false) {
                continue;
            }
            self.paint_render(k, spec, clipping);
        }
        let _ = out_of_flow;

        // Anything the node draws *on top of* its own contents — an overlay
        // scrollbar. Emitted here rather than above so the rows it reports on
        // do not cover it.
        if let Some(obj) = self.render.get(r).and_then(|n| n.obj.as_ref()) {
            let mut over = DrawList::new(element);
            over.key = key.clone();
            over.theme = ThemeKey(theme.clone());
            over.classes = crate::render::spec::Classes(classes.clone());
            obj.paint_over(Geom { rect, clip }, &mut over);
            spec.items.append(&mut over.items);
        }

        if let Some(k) = key {
            let end = spec.items.len();
            if end > start {
                spec.index.push((k, start..end));
            }
        }
    }
}
