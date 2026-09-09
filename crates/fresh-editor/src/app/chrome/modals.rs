//! The full-screen modal band: Settings, the keybinding editor, the
//! calibration wizard, and the workspace-trust prompt.
//!
//! **Their mouse and their keyboard have both moved.** A modal is a
//! `Modality::Exclusive` layer in the shell's tree: it owns both channels by
//! *containment*. Its pointer is answered by its own nodes; a key nothing
//! inside it answers comes back as `UiFact::ModalKey`, which names the
//! surface rather than a rank. Which keyboard vocabulary applies while one
//! is up is read off the focus chain (`frame::key_context_of`), and whether
//! a terminal takes raw input beneath one is `Ui::modal_up`.
//!
//! What a key *means* is still the interior's — a bespoke dispatcher over a
//! vocabulary the tree's `KeyPress` does not carry. What is left here is the
//! settings dispatcher, with one caller.

use super::Editor;

impl Editor {
    /// Settings' own keyboard, reached by containment: every key is this
    /// modal's while its layer is up, which is what `Modality::Exclusive`
    /// says. The interior is eleven modules of `InputHandler` and stays
    /// exactly where it is.
    pub(crate) fn dispatch_settings_key(&mut self, event: &crossterm::event::KeyEvent) {
        use crate::input::handler::{InputContext, InputHandler};
        let mut ctx = InputContext::new();
        if let Some(settings) = self.settings_state.as_mut() {
            settings.dispatch_input(event, &mut ctx);
        }
        self.process_deferred_actions(ctx);
    }
}
