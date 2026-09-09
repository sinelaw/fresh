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

impl<M: 'static> Ui<M> {
    pub(crate) fn flush_paint(&mut self, frame: Size) {
        let mut spec = std::mem::take(&mut self.spec);
        spec.clear();
        spec.frame = frame;
        if let Some(root) = self.render_root {
            self.paint_render(root, &mut spec);
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
        self.paint_render(lr, spec);
        if spec.cursor.is_none() {
            spec.cursor = under.filter(|c| {
                !spec.items[painted_from..]
                    .iter()
                    .any(|i| i.rect.intersect(i.clip).contains(c.pos))
            });
        }
    }

    fn paint_render(&mut self, r: RenderId, spec: &mut LayoutSpec) {
        let (element, rect, clip, theme, classes, key, kids, out_of_flow) = {
            let Some(n) = self.render.get(r) else { return };
            (
                n.element,
                n.data.rect,
                n.data.clip,
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
        let (names_itself, wash) = self
            .arena
            .get(element)
            .map(|e| {
                let d = resolve(&e.desc);
                (
                    e.desc.theme.is_some()
                        || d.theme.is_some()
                        || e.desc.classes.is_some()
                        || d.classes.is_some(),
                    matches!(&d.desc, crate::desc::Desc::Box(b) if b.wash),
                )
            })
            .unwrap_or((false, false));
        if names_itself && !rect.is_empty() {
            let ground = match wash {
                true => Draw::Wash,
                false => Draw::Fill,
            };
            list.push(ground, Geom { rect, clip });
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
            self.paint_render(k, spec);
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
