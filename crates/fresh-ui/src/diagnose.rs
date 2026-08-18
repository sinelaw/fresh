//! Tree inspection.
//!
//! The retained tree exists partly so that it can be looked at. One dump
//! answers the three questions a failing test asks: what is mounted, how many
//! times has it built, and what marked it.

use std::fmt::Write;

use crate::desc::component_of;
use crate::element::ElementId;
use crate::schedule::{DirtyCause, Ui};

impl<M: 'static> Ui<M> {
    /// How many times this element has built since it mounted.
    pub fn builds(&self, id: ElementId) -> u32 {
        self.arena.get(id).map(|e| e.builds).unwrap_or(0)
    }

    /// What last marked this element for rebuild.
    pub fn last_dirty(&self, id: ElementId) -> Option<DirtyCause> {
        self.arena.get(id).and_then(|e| e.last_dirty)
    }

    /// The elements registered as dependents of a `Provide` element.
    pub fn dependents(&self, id: ElementId) -> Vec<ElementId> {
        self.arena
            .get(id)
            .map(|e| e.dependents.clone())
            .unwrap_or_default()
    }

    /// Names of the behaviors registered on an element, in registration order.
    pub fn behaviors(&self, id: ElementId) -> Vec<&'static str> {
        self.arena
            .get(id)
            .map(|e| e.behaviors.iter().map(|b| b.behavior_name()).collect())
            .unwrap_or_default()
    }

    /// The text a mounted `TextRun` element holds. Until the paint phase lands
    /// this is how a test reads what a component actually produced.
    pub fn text_of(&self, id: ElementId) -> Option<std::rc::Rc<str>> {
        match &crate::desc::resolve(&self.arena.get(id)?.desc).desc {
            crate::desc::Desc::TextRun(p) => Some(p.text.clone()),
            _ => None,
        }
    }

    /// One line per element: indentation is depth, then type, key, id, build
    /// count, state and the last dirty cause.
    pub fn dump(&self) -> String {
        let mut out = String::new();
        if let Some(r) = self.root {
            self.dump_into(r, 0, &mut out);
        }
        out
    }

    /// The dump with build counts and dirty causes stripped, for tests that
    /// assert on structure alone.
    pub fn shape(&self) -> String {
        let mut out = String::new();
        if let Some(r) = self.root {
            self.shape_into(r, 0, &mut out);
        }
        out
    }

    fn label(&self, id: ElementId) -> String {
        let name = self.name_of(id).unwrap_or("<gone>");
        match self.key_of(id) {
            Some(k) => format!("{name} {k}"),
            None => name.to_string(),
        }
    }

    fn shape_into(&self, id: ElementId, indent: usize, out: &mut String) {
        let _ = writeln!(out, "{:indent$}{}", "", self.label(id), indent = indent * 2);
        for c in self.children(id) {
            self.shape_into(c, indent + 1, out);
        }
    }

    fn dump_into(&self, id: ElementId, indent: usize, out: &mut String) {
        let mut line = format!(
            "{:indent$}{} {id:?}",
            "",
            self.label(id),
            indent = indent * 2
        );

        if let Some(el) = self.arena.get(id) {
            if el.state.is_some() {
                let described = component_of(&el.desc)
                    .and_then(|c| el.state.as_ref().and_then(|s| c.describe_state_any(&**s)))
                    .map(|d| format!("({d})"))
                    .unwrap_or_default();
                let _ = write!(line, "  state={}{described}", el.state_name);
            }
            if el.builds > 0 {
                let _ = write!(line, "  builds={}", el.builds);
            }
            if !el.behaviors.is_empty() {
                let names: Vec<_> = el.behaviors.iter().map(|b| b.behavior_name()).collect();
                let _ = write!(line, "  behaviors=[{}]", names.join(", "));
            }
            if !el.dependents.is_empty() {
                let _ = write!(line, "  dependents={:?}", el.dependents);
            }
            if el.needs_build {
                let _ = write!(line, "  DIRTY");
            }
            if let Some(c) = el.last_dirty {
                let _ = write!(line, "  cause={c}");
            }
        }

        let _ = writeln!(out, "{line}");
        for c in self.children(id) {
            self.dump_into(c, indent + 1, out);
        }
    }
}
