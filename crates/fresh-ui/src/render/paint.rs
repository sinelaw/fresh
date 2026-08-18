//! The paint walk.
//!
//! Cost is O(visible items). The walk skips work four ways: off-screen
//! (`rect ∩ clip` empty), occluded (an opaque full-frame layer above), scrolled
//! out (a viewport emits only the rows inside its window), and absent (never
//! built at all).

use std::rc::Rc;

use crate::desc::{resolve, Desc, ElemType, Scrim};
use crate::element::ElementId;
use crate::schedule::Ui;

use super::geom::{Rect, Size};
use super::spec::{Draw, Item, ThemeKey};

impl<M: 'static> Ui<M> {
    pub(crate) fn flush_paint(&mut self, frame: Size) {
        let mut spec = std::mem::take(&mut self.spec);
        spec.clear();
        spec.frame = frame;
        if let Some(root) = self.root {
            self.paint_node(root, &ThemeKey::default(), &mut spec);
            // Layers paint above the content they were declared in, in the
            // order the arrange walk found them.
            for i in 0..self.pending_layers.len() {
                let (lid, _) = self.pending_layers[i];
                self.paint_layer(lid, frame, &mut spec);
            }
        }
        self.spec = spec;
    }

    fn paint_layer(&mut self, lid: ElementId, frame: Size, spec: &mut super::spec::LayoutSpec) {
        let props = match &resolve(&self.arena[lid].desc).desc {
            Desc::Layer(p) => p.clone(),
            _ => return,
        };
        if let Some(kind) = props.scrim {
            if kind == Scrim::Opaque {
                // Everything under an opaque full-frame scrim is invisible;
                // emitting it would make the backend draw and then overdraw.
                spec.items.clear();
                spec.index.clear();
            }
            spec.items.push(Item {
                key: None,
                id: lid,
                rect: Rect::from_size(frame),
                clip: Rect::from_size(frame),
                theme: ThemeKey::default(),
                draw: Draw::Scrim(kind),
            });
        }
        let theme = ThemeKey::default();
        for k in self.arena[lid].children.clone() {
            self.paint_node(k, &theme, spec);
        }
    }

    fn paint_node(&mut self, id: ElementId, theme: &ThemeKey, spec: &mut super::spec::LayoutSpec) {
        let Some(el) = self.arena.get(id) else { return };
        let ty = el.ty;
        let rect = el.layout.rect;
        let clip = el.layout.clip;
        let key = el.key.clone();
        let kids = el.children.clone();
        // A `Shared` wrapper is transparent to provenance, as it is to type.
        let node_theme = el
            .desc
            .theme
            .clone()
            .or_else(|| resolve(&el.desc).theme.clone());

        // Off-screen: nothing below can be visible either, because a child's
        // rect is contained in its parent's clip.
        if ty != ElemType::Layer && rect.intersect(clip).is_empty() && ty != ElemType::Viewport {
            // A zero-size pass-through node still has visible children (it does
            // not clip them), so only cull nodes that actually bound content.
            if !matches!(
                ty,
                ElemType::Gesture
                    | ElemType::Focusable
                    | ElemType::Provide(_)
                    | ElemType::LayoutReader
                    | ElemType::Component(_)
            ) {
                return;
            }
        }

        let theme = match &node_theme {
            Some(t) => ThemeKey(Some(t.clone())),
            None => theme.clone(),
        };
        let start = spec.items.len();

        match ty {
            ElemType::Box => {
                let props = match &resolve(&self.arena[id].desc).desc {
                    Desc::Box(p) => p.clone(),
                    _ => unreachable!(),
                };
                if node_theme.is_some() {
                    spec.items.push(Item {
                        key: key.clone(),
                        id,
                        rect,
                        clip,
                        theme: theme.clone(),
                        draw: Draw::Fill,
                    });
                }
                if props.border {
                    spec.items.push(Item {
                        key: key.clone(),
                        id,
                        rect,
                        clip,
                        theme: theme.clone(),
                        draw: Draw::Border,
                    });
                }
            }
            ElemType::TextRun => {
                let (text, wrap) = match &resolve(&self.arena[id].desc).desc {
                    Desc::TextRun(p) => (p.text.clone(), p.wrap),
                    _ => unreachable!(),
                };
                let lines: Vec<Rc<str>> = if wrap {
                    super::layout::wrap_text(&text, rect.w)
                        .iter()
                        .map(|l| Rc::from(l.as_str()))
                        .collect()
                } else {
                    text.split('\n').map(Rc::from).collect()
                };
                spec.items.push(Item {
                    key: key.clone(),
                    id,
                    rect,
                    clip,
                    theme: theme.clone(),
                    draw: Draw::Lines(lines),
                });
            }
            ElemType::Host => {
                let h = match &resolve(&self.arena[id].desc).desc {
                    Desc::Host(h) => *h,
                    _ => unreachable!(),
                };
                spec.items.push(Item {
                    key: key.clone(),
                    id,
                    rect,
                    clip,
                    theme: theme.clone(),
                    draw: Draw::Host(h),
                });
            }
            ElemType::Viewport => {
                let props = match &resolve(&self.arena[id].desc).desc {
                    Desc::Viewport(p) => p.clone(),
                    _ => unreachable!(),
                };
                if props.scrollbar {
                    let el = &self.arena[id];
                    let content = el.layout.content.h;
                    let window = el.layout.size.h;
                    if content > window {
                        spec.items.push(Item {
                            key: key.clone(),
                            id,
                            rect: Rect::new(rect.right() - 1, rect.y, 1, rect.h),
                            clip,
                            theme: theme.clone(),
                            draw: Draw::Scrollbar {
                                offset: el.layout.scroll.y.max(0) as u16,
                                content,
                                window,
                            },
                        });
                    }
                }
            }
            // Layers are painted after the tree they were declared in, so the
            // in-flow walk steps over them.
            ElemType::Layer => return,
            _ => {}
        }

        for k in kids {
            self.paint_node(k, &theme, spec);
        }

        if let Some(k) = key {
            let end = spec.items.len();
            if end > start {
                spec.index.push((k, start..end));
            }
        }
    }
}
