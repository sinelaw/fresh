//! `Action::DumpUiTree`: the retained shell tree, as JSON, in a buffer.
//!
//! One object per element — type, key, id, rect, a run's text, build count,
//! dirty cause — straight from [`fresh_ui::Ui::dump_json`]. JSON so that the
//! dump can be read by a program as well as by a person: the laid-out rects
//! are what a layout diff or a screenshot annotator wants, and re-parsing
//! indented text to get them is a needless second format. The tree persists
//! between frames, so a command handler reads the one the last frame built.

use super::Editor;
use fresh_core::text_property::TextPropertyEntry;

impl Editor {
    pub(super) fn dump_ui_tree(&mut self) {
        let Some(text) = self.shell_ui.as_ref().map(|ui| ui.dump_json()) else {
            return;
        };
        let id = self.active_window_mut().create_virtual_buffer(
            "*ui-tree*".to_string(),
            "json".to_string(),
            true,
        );
        match self.set_virtual_buffer_content(id, vec![TextPropertyEntry::text(text)]) {
            Ok(()) => self.set_active_buffer(id),
            Err(e) => tracing::error!("dump_ui_tree: {e}"),
        }
    }
}
