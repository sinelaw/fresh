//! Tree inspection.
//!
//! Partial: rebuild counters and the last-dirty cause are added by the
//! diagnostics phase. What exists here is the structural dump, which is what
//! the reconciler's own tests read when they fail.

use std::fmt::Write;

use crate::element::ElementId;
use crate::schedule::Ui;

impl<M: 'static> Ui<M> {
    /// One line per element: indentation is depth, then type, key and id.
    pub fn dump(&self) -> String {
        let mut out = String::new();
        if let Some(r) = self.root {
            self.dump_into(r, 0, &mut out);
        }
        out
    }

    fn dump_into(&self, id: ElementId, indent: usize, out: &mut String) {
        let name = self.name_of(id).unwrap_or("<gone>");
        let key = match self.key_of(id) {
            Some(k) => format!(" {k}"),
            None => String::new(),
        };
        let _ = writeln!(out, "{:indent$}{name}{key} {id:?}", "", indent = indent * 2);
        for c in self.children(id) {
            self.dump_into(c, indent + 1, out);
        }
    }
}
