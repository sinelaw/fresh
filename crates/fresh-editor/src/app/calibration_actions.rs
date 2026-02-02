//! Calibration wizard action handling
//!
//! This module provides the action handlers for the input calibration wizard.

use super::calibration_wizard::{CalibrationWizard, WizardAction};
use super::Editor;
use crate::input::handler::InputResult;
use crossterm::event::KeyEvent;
use rust_i18n::t;

impl Editor {
    /// Open the calibration wizard
    pub fn open_calibration_wizard(&mut self) {
        self.calibration_wizard = Some(CalibrationWizard::new());
        self.set_status_message(t!("calibration.started").to_string());
    }

    /// Save calibration and close wizard
    pub fn save_calibration(&mut self, wizard: CalibrationWizard) {
        let translator = wizard.build_translator();
        let count = translator.len();

        // Save to config file
        if let Err(e) = translator.save_to_config_dir(&self.dir_context.config_dir) {
            tracing::error!("Failed to save key calibration: {}", e);
            self.set_status_message(
                t!("calibration.save_error", error = e.to_string()).to_string(),
            );
            return;
        }

        // Update the active translator
        self.key_translator = translator;

        self.set_status_message(t!("calibration.saved", count = count).to_string());
    }

    /// Handle input when calibration wizard is active
    pub fn handle_calibration_input(&mut self, event: &KeyEvent) -> InputResult {
        // Take the wizard temporarily to avoid borrowing issues
        let mut wizard = match self.calibration_wizard.take() {
            Some(w) => w,
            None => return InputResult::Ignored,
        };

        // Handle the key based on current state
        let action = if wizard.has_pending_confirmation() {
            wizard.handle_confirmation_key(*event)
        } else if wizard.is_verify_phase() {
            wizard.handle_verify_key(*event)
        } else {
            wizard.handle_capture_key(*event)
        };

        // Update status message from wizard
        if let Some(msg) = wizard.status_message.take() {
            self.set_status_message(msg);
        }

        // Process the action, deciding what to do with the wizard
        match action {
            WizardAction::Continue
            | WizardAction::GoBack
            | WizardAction::SkipGroup
            | WizardAction::KeyCaptured
            | WizardAction::KeyVerified
            | WizardAction::ReservedKey
            | WizardAction::ShowConfirmation => {
                // Put wizard back and continue
                self.calibration_wizard = Some(wizard);
            }
            WizardAction::Abort => {
                // Drop wizard (don't put back), show message
                self.set_status_message(t!("calibration.aborted").to_string());
            }
            WizardAction::Save => {
                // Pass wizard to save - compiler enforces we have it
                self.save_calibration(wizard);
            }
            WizardAction::Restart => {
                // Restart already called by handle_confirmation_key
                self.calibration_wizard = Some(wizard);
            }
        }

        InputResult::Consumed
    }

    /// Check if calibration wizard is active
    pub fn is_calibration_active(&self) -> bool {
        self.calibration_wizard.is_some()
    }
}
