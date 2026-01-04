//! Input Calibration Wizard
//!
//! A fail-safe wizard for calibrating keyboard input in hostile terminal environments.
//! Uses only lowercase ASCII letters for navigation (s, g, a, y, r) because they work
//! on virtually every terminal since 1970.
//!
//! The wizard operates in two phases:
//! 1. Capture Phase: User presses each target key, wizard records what the terminal sends
//! 2. Verify Phase: User can test their mappings work correctly before saving

use crate::input::key_translator::{KeyEventKey, KeyTranslator};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use std::collections::{HashMap, HashSet};

/// What the user's key SHOULD produce (the expected/normalized key)
#[derive(Debug, Clone)]
pub struct ExpectedKey {
    pub code: KeyCode,
    pub modifiers: KeyModifiers,
}

impl ExpectedKey {
    pub const fn new(code: KeyCode, modifiers: KeyModifiers) -> Self {
        Self { code, modifiers }
    }

    /// Convert to a KeyEvent for comparison
    pub fn to_key_event(&self) -> KeyEvent {
        KeyEvent::new(self.code.clone(), self.modifiers)
    }
}

/// A single key that can be calibrated
#[derive(Debug, Clone)]
pub struct CalibrationTarget {
    /// Display name for the key (e.g., "BACKSPACE", "CTRL+LEFT")
    pub name: &'static str,
    /// What Fresh expects to receive (the normalized key)
    pub expected: ExpectedKey,
}

/// A group of related keys to calibrate
#[derive(Debug, Clone)]
pub struct CalibrationGroup {
    /// Group name (e.g., "Basic Editing", "Line Navigation")
    pub name: &'static str,
    /// Keys in this group
    pub targets: Vec<CalibrationTarget>,
}

/// Build the calibration groups as defined in the design document
pub fn calibration_groups() -> Vec<CalibrationGroup> {
    vec![
        // Group 1: Basic Editing (4 keys)
        CalibrationGroup {
            name: "Basic Editing",
            targets: vec![
                CalibrationTarget {
                    name: "BACKSPACE",
                    expected: ExpectedKey::new(KeyCode::Backspace, KeyModifiers::NONE),
                },
                CalibrationTarget {
                    name: "DELETE",
                    expected: ExpectedKey::new(KeyCode::Delete, KeyModifiers::NONE),
                },
                CalibrationTarget {
                    name: "TAB",
                    expected: ExpectedKey::new(KeyCode::Tab, KeyModifiers::NONE),
                },
                CalibrationTarget {
                    name: "SHIFT+TAB",
                    expected: ExpectedKey::new(KeyCode::BackTab, KeyModifiers::SHIFT),
                },
            ],
        },
        // Group 2: Line Navigation (4 keys)
        CalibrationGroup {
            name: "Line Navigation",
            targets: vec![
                CalibrationTarget {
                    name: "HOME",
                    expected: ExpectedKey::new(KeyCode::Home, KeyModifiers::NONE),
                },
                CalibrationTarget {
                    name: "END",
                    expected: ExpectedKey::new(KeyCode::End, KeyModifiers::NONE),
                },
                CalibrationTarget {
                    name: "SHIFT+HOME",
                    expected: ExpectedKey::new(KeyCode::Home, KeyModifiers::SHIFT),
                },
                CalibrationTarget {
                    name: "SHIFT+END",
                    expected: ExpectedKey::new(KeyCode::End, KeyModifiers::SHIFT),
                },
            ],
        },
        // Group 3: Word Navigation (8 keys)
        CalibrationGroup {
            name: "Word Navigation",
            targets: vec![
                CalibrationTarget {
                    name: "ALT+LEFT",
                    expected: ExpectedKey::new(KeyCode::Left, KeyModifiers::ALT),
                },
                CalibrationTarget {
                    name: "ALT+RIGHT",
                    expected: ExpectedKey::new(KeyCode::Right, KeyModifiers::ALT),
                },
                CalibrationTarget {
                    name: "ALT+SHIFT+LEFT",
                    expected: ExpectedKey::new(
                        KeyCode::Left,
                        KeyModifiers::ALT.union(KeyModifiers::SHIFT),
                    ),
                },
                CalibrationTarget {
                    name: "ALT+SHIFT+RIGHT",
                    expected: ExpectedKey::new(
                        KeyCode::Right,
                        KeyModifiers::ALT.union(KeyModifiers::SHIFT),
                    ),
                },
                CalibrationTarget {
                    name: "CTRL+LEFT",
                    expected: ExpectedKey::new(KeyCode::Left, KeyModifiers::CONTROL),
                },
                CalibrationTarget {
                    name: "CTRL+RIGHT",
                    expected: ExpectedKey::new(KeyCode::Right, KeyModifiers::CONTROL),
                },
                CalibrationTarget {
                    name: "CTRL+SHIFT+LEFT",
                    expected: ExpectedKey::new(
                        KeyCode::Left,
                        KeyModifiers::CONTROL.union(KeyModifiers::SHIFT),
                    ),
                },
                CalibrationTarget {
                    name: "CTRL+SHIFT+RIGHT",
                    expected: ExpectedKey::new(
                        KeyCode::Right,
                        KeyModifiers::CONTROL.union(KeyModifiers::SHIFT),
                    ),
                },
            ],
        },
        // Group 4: Document Navigation (4 keys)
        CalibrationGroup {
            name: "Document Navigation",
            targets: vec![
                CalibrationTarget {
                    name: "PAGE UP",
                    expected: ExpectedKey::new(KeyCode::PageUp, KeyModifiers::NONE),
                },
                CalibrationTarget {
                    name: "PAGE DOWN",
                    expected: ExpectedKey::new(KeyCode::PageDown, KeyModifiers::NONE),
                },
                CalibrationTarget {
                    name: "CTRL+HOME",
                    expected: ExpectedKey::new(KeyCode::Home, KeyModifiers::CONTROL),
                },
                CalibrationTarget {
                    name: "CTRL+END",
                    expected: ExpectedKey::new(KeyCode::End, KeyModifiers::CONTROL),
                },
            ],
        },
        // Group 5: Emacs-Style Navigation (4 keys)
        CalibrationGroup {
            name: "Emacs-Style",
            targets: vec![
                CalibrationTarget {
                    name: "CTRL+A",
                    expected: ExpectedKey::new(KeyCode::Char('a'), KeyModifiers::CONTROL),
                },
                CalibrationTarget {
                    name: "CTRL+E",
                    expected: ExpectedKey::new(KeyCode::Char('e'), KeyModifiers::CONTROL),
                },
                CalibrationTarget {
                    name: "CTRL+K",
                    expected: ExpectedKey::new(KeyCode::Char('k'), KeyModifiers::CONTROL),
                },
                CalibrationTarget {
                    name: "CTRL+Y",
                    expected: ExpectedKey::new(KeyCode::Char('y'), KeyModifiers::CONTROL),
                },
            ],
        },
    ]
}

/// Current step in the calibration wizard
#[derive(Debug, Clone)]
pub enum CalibrationStep {
    /// Capturing key for a specific target
    Capture {
        /// Index into calibration_groups()
        group_idx: usize,
        /// Index into group's targets list
        key_idx: usize,
    },
    /// Verification phase - testing mapped keys
    Verify,
}

/// Status of a single key calibration
#[derive(Debug, Clone, PartialEq)]
pub enum KeyStatus {
    /// Not yet calibrated (waiting)
    Pending,
    /// Key was captured (different from expected)
    Captured,
    /// Key was skipped (using default)
    Skipped,
    /// Key was verified in verification phase
    Verified,
}

/// Result of handling a key input
#[derive(Debug, Clone)]
pub enum WizardAction {
    /// Continue to next key
    Continue,
    /// Skip to next group
    SkipGroup,
    /// Abort wizard (discard changes)
    Abort,
    /// Save and exit
    Save,
    /// Restart wizard
    Restart,
    /// Key was reserved, show message
    ReservedKey,
    /// Key captured, auto-advance
    KeyCaptured,
    /// Key verified in verification phase
    KeyVerified,
}

/// The calibration wizard state machine
#[derive(Debug)]
pub struct CalibrationWizard {
    /// Current step in the wizard
    pub step: CalibrationStep,
    /// Calibration groups (loaded once)
    groups: Vec<CalibrationGroup>,
    /// Pending translations: raw terminal event -> expected normalized event
    pending_translations: HashMap<KeyEventKey, KeyEventKey>,
    /// Status of each key (flattened index)
    key_statuses: Vec<KeyStatus>,
    /// Groups that were skipped entirely
    skipped_groups: HashSet<usize>,
    /// Which keys have been verified in verification phase
    verified: HashSet<usize>,
    /// Status message to display
    pub status_message: Option<String>,
}

impl CalibrationWizard {
    /// Create a new calibration wizard
    pub fn new() -> Self {
        let groups = calibration_groups();
        let total_keys: usize = groups.iter().map(|g| g.targets.len()).sum();

        Self {
            step: CalibrationStep::Capture {
                group_idx: 0,
                key_idx: 0,
            },
            groups,
            pending_translations: HashMap::new(),
            key_statuses: vec![KeyStatus::Pending; total_keys],
            skipped_groups: HashSet::new(),
            verified: HashSet::new(),
            status_message: None,
        }
    }

    /// Get calibration groups
    pub fn groups(&self) -> &[CalibrationGroup] {
        &self.groups
    }

    /// Get key status by flattened index
    pub fn key_status(&self, flat_idx: usize) -> &KeyStatus {
        self.key_statuses
            .get(flat_idx)
            .unwrap_or(&KeyStatus::Pending)
    }

    /// Check if a group was skipped
    pub fn is_group_skipped(&self, group_idx: usize) -> bool {
        self.skipped_groups.contains(&group_idx)
    }

    /// Get the current target being calibrated (capture phase only)
    pub fn current_target(&self) -> Option<(&CalibrationGroup, &CalibrationTarget, usize)> {
        match &self.step {
            CalibrationStep::Capture { group_idx, key_idx } => {
                let group = self.groups.get(*group_idx)?;
                let target = group.targets.get(*key_idx)?;
                let flat_idx = self.flat_index(*group_idx, *key_idx);
                Some((group, target, flat_idx))
            }
            CalibrationStep::Verify => None,
        }
    }

    /// Get the current step number (1-indexed) and total
    pub fn current_step_info(&self) -> (usize, usize) {
        let total: usize = self.groups.iter().map(|g| g.targets.len()).sum();
        match &self.step {
            CalibrationStep::Capture { group_idx, key_idx } => {
                let step = self.flat_index(*group_idx, *key_idx) + 1;
                (step, total)
            }
            CalibrationStep::Verify => (total, total),
        }
    }

    /// Convert (group_idx, key_idx) to flattened index
    fn flat_index(&self, group_idx: usize, key_idx: usize) -> usize {
        let mut idx = 0;
        for (i, group) in self.groups.iter().enumerate() {
            if i == group_idx {
                return idx + key_idx;
            }
            idx += group.targets.len();
        }
        idx
    }

    /// Convert flattened index to (group_idx, key_idx)
    #[allow(dead_code)]
    fn unflat_index(&self, flat_idx: usize) -> Option<(usize, usize)> {
        let mut idx = 0;
        for (group_idx, group) in self.groups.iter().enumerate() {
            if flat_idx < idx + group.targets.len() {
                return Some((group_idx, flat_idx - idx));
            }
            idx += group.targets.len();
        }
        None
    }

    /// Handle a key event during capture phase
    pub fn handle_capture_key(&mut self, key: KeyEvent) -> WizardAction {
        let CalibrationStep::Capture { group_idx, key_idx } = &self.step else {
            return WizardAction::Continue;
        };

        let group_idx = *group_idx;
        let key_idx = *key_idx;

        // Check for reserved control keys (lowercase letters without modifiers)
        if key.modifiers == KeyModifiers::NONE {
            match key.code {
                KeyCode::Char('s') => {
                    // Skip this key
                    let flat_idx = self.flat_index(group_idx, key_idx);
                    self.key_statuses[flat_idx] = KeyStatus::Skipped;
                    self.status_message = Some("Skipped (using default)".to_string());
                    self.advance_to_next();
                    return WizardAction::Continue;
                }
                KeyCode::Char('g') => {
                    // Skip entire group
                    self.skip_current_group();
                    return WizardAction::SkipGroup;
                }
                KeyCode::Char('a') => {
                    // Abort wizard
                    return WizardAction::Abort;
                }
                KeyCode::Char('y') | KeyCode::Char('n') | KeyCode::Char('r') => {
                    // Reserved for verification phase
                    self.status_message =
                        Some("Reserved key. Press the target key or [s] to skip.".to_string());
                    return WizardAction::ReservedKey;
                }
                _ => {}
            }
        }

        // Capture the key
        let flat_idx = self.flat_index(group_idx, key_idx);
        let target = &self.groups[group_idx].targets[key_idx];
        let expected = target.expected.to_key_event();

        // Check if the key is already what we expect (no translation needed)
        if key.code == expected.code && key.modifiers == expected.modifiers {
            self.key_statuses[flat_idx] = KeyStatus::Skipped;
            self.status_message = Some("Key works correctly (no mapping needed)".to_string());
        } else {
            // Record the translation: raw -> expected
            let raw_key = KeyEventKey::from_key_event(&key);
            let expected_key = KeyEventKey::from_key_event(&expected);
            self.pending_translations.insert(raw_key, expected_key);
            self.key_statuses[flat_idx] = KeyStatus::Captured;
            self.status_message = Some(format!("Captured: {:?} -> {}", key.code, target.name));
        }

        self.advance_to_next();
        WizardAction::KeyCaptured
    }

    /// Handle a key event during verification phase
    pub fn handle_verify_key(&mut self, key: KeyEvent) -> WizardAction {
        // Check for control keys
        if key.modifiers == KeyModifiers::NONE {
            match key.code {
                KeyCode::Char('y') => {
                    return WizardAction::Save;
                }
                KeyCode::Char('r') => {
                    return WizardAction::Restart;
                }
                KeyCode::Char('a') => {
                    return WizardAction::Abort;
                }
                _ => {}
            }
        }

        // Try to find a matching expected key
        // Apply translation first
        let translated = self.translate_key(key);

        // Find which target this matches
        for (group_idx, group) in self.groups.iter().enumerate() {
            if self.skipped_groups.contains(&group_idx) {
                continue;
            }
            for (key_idx, target) in group.targets.iter().enumerate() {
                let expected = target.expected.to_key_event();
                if translated.code == expected.code && translated.modifiers == expected.modifiers {
                    let flat_idx = self.flat_index(group_idx, key_idx);
                    self.verified.insert(flat_idx);
                    self.key_statuses[flat_idx] = KeyStatus::Verified;
                    self.status_message = Some(format!("{} verified!", target.name));
                    return WizardAction::KeyVerified;
                }
            }
        }

        self.status_message = Some("Key not recognized".to_string());
        WizardAction::Continue
    }

    /// Translate a key using pending translations
    fn translate_key(&self, key: KeyEvent) -> KeyEvent {
        let raw_key = KeyEventKey::from_key_event(&key);
        if let Some(expected_key) = self.pending_translations.get(&raw_key) {
            expected_key.to_key_event()
        } else {
            key
        }
    }

    /// Skip the current group
    fn skip_current_group(&mut self) {
        if let CalibrationStep::Capture { group_idx, key_idx } = &self.step {
            let group_idx = *group_idx;
            let key_idx = *key_idx;

            // Mark all remaining keys in this group as skipped
            let group = &self.groups[group_idx];
            for i in key_idx..group.targets.len() {
                let flat_idx = self.flat_index(group_idx, i);
                self.key_statuses[flat_idx] = KeyStatus::Skipped;
            }

            self.skipped_groups.insert(group_idx);
            self.status_message = Some(format!("Skipped group: {}", group.name));

            // Advance to next group
            if group_idx + 1 < self.groups.len() {
                self.step = CalibrationStep::Capture {
                    group_idx: group_idx + 1,
                    key_idx: 0,
                };
            } else {
                self.step = CalibrationStep::Verify;
            }
        }
    }

    /// Advance to the next key/group/phase
    fn advance_to_next(&mut self) {
        if let CalibrationStep::Capture { group_idx, key_idx } = &self.step {
            let group_idx = *group_idx;
            let key_idx = *key_idx;

            let group = &self.groups[group_idx];
            if key_idx + 1 < group.targets.len() {
                // Next key in same group
                self.step = CalibrationStep::Capture {
                    group_idx,
                    key_idx: key_idx + 1,
                };
            } else if group_idx + 1 < self.groups.len() {
                // First key in next group
                self.step = CalibrationStep::Capture {
                    group_idx: group_idx + 1,
                    key_idx: 0,
                };
            } else {
                // All keys captured, move to verification
                self.step = CalibrationStep::Verify;
                self.status_message =
                    Some("Capture complete! Test your keys or [y] to save.".to_string());
            }
        }
    }

    /// Reset the wizard to start over
    pub fn restart(&mut self) {
        let total_keys: usize = self.groups.iter().map(|g| g.targets.len()).sum();
        self.step = CalibrationStep::Capture {
            group_idx: 0,
            key_idx: 0,
        };
        self.pending_translations.clear();
        self.key_statuses = vec![KeyStatus::Pending; total_keys];
        self.skipped_groups.clear();
        self.verified.clear();
        self.status_message = Some("Wizard restarted".to_string());
    }

    /// Check if we're in verify phase
    pub fn is_verify_phase(&self) -> bool {
        matches!(self.step, CalibrationStep::Verify)
    }

    /// Get the number of translations captured
    pub fn translation_count(&self) -> usize {
        self.pending_translations.len()
    }

    /// Build a KeyTranslator from the pending translations
    pub fn build_translator(&self) -> KeyTranslator {
        let mut translator = KeyTranslator::new();
        for (raw, expected) in &self.pending_translations {
            translator.add_translation(raw.to_key_event(), expected.to_key_event());
        }
        translator
    }

    /// Get verification progress (verified, total)
    pub fn verification_progress(&self) -> (usize, usize) {
        let total: usize = self
            .key_statuses
            .iter()
            .filter(|s| matches!(s, KeyStatus::Captured))
            .count();
        let verified = self.verified.len();
        (verified, total)
    }

    /// Get all key statuses with their group/key info
    pub fn all_key_info(&self) -> Vec<(usize, usize, &CalibrationTarget, &KeyStatus)> {
        let mut result = Vec::new();
        let mut flat_idx = 0;
        for (group_idx, group) in self.groups.iter().enumerate() {
            for (key_idx, target) in group.targets.iter().enumerate() {
                let status = &self.key_statuses[flat_idx];
                result.push((group_idx, key_idx, target, status));
                flat_idx += 1;
            }
        }
        result
    }
}

impl Default for CalibrationWizard {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wizard_creation() {
        let wizard = CalibrationWizard::new();
        assert!(matches!(
            wizard.step,
            CalibrationStep::Capture {
                group_idx: 0,
                key_idx: 0
            }
        ));
        assert_eq!(wizard.translation_count(), 0);
    }

    #[test]
    fn test_step_info() {
        let wizard = CalibrationWizard::new();
        let (step, total) = wizard.current_step_info();
        assert_eq!(step, 1);
        assert_eq!(total, 24); // 4 + 4 + 8 + 4 + 4 = 24 keys
    }

    #[test]
    fn test_skip_key() {
        let mut wizard = CalibrationWizard::new();

        // Skip first key with 's'
        let key = KeyEvent::new(KeyCode::Char('s'), KeyModifiers::NONE);
        let action = wizard.handle_capture_key(key);

        assert!(matches!(action, WizardAction::Continue));
        assert_eq!(*wizard.key_status(0), KeyStatus::Skipped);

        // Should have advanced to next key
        assert!(matches!(
            wizard.step,
            CalibrationStep::Capture {
                group_idx: 0,
                key_idx: 1
            }
        ));
    }

    #[test]
    fn test_skip_group() {
        let mut wizard = CalibrationWizard::new();

        // Skip group with 'g'
        let key = KeyEvent::new(KeyCode::Char('g'), KeyModifiers::NONE);
        let action = wizard.handle_capture_key(key);

        assert!(matches!(action, WizardAction::SkipGroup));
        assert!(wizard.is_group_skipped(0));

        // Should have advanced to next group
        assert!(matches!(
            wizard.step,
            CalibrationStep::Capture {
                group_idx: 1,
                key_idx: 0
            }
        ));
    }

    #[test]
    fn test_abort() {
        let mut wizard = CalibrationWizard::new();

        let key = KeyEvent::new(KeyCode::Char('a'), KeyModifiers::NONE);
        let action = wizard.handle_capture_key(key);

        assert!(matches!(action, WizardAction::Abort));
    }

    #[test]
    fn test_reserved_key() {
        let mut wizard = CalibrationWizard::new();

        // 'y' is reserved
        let key = KeyEvent::new(KeyCode::Char('y'), KeyModifiers::NONE);
        let action = wizard.handle_capture_key(key);

        assert!(matches!(action, WizardAction::ReservedKey));
    }

    #[test]
    fn test_capture_key() {
        let mut wizard = CalibrationWizard::new();

        // Simulate a terminal sending 0x7F for backspace
        let key = KeyEvent::new(KeyCode::Char('\x7f'), KeyModifiers::NONE);
        let action = wizard.handle_capture_key(key);

        assert!(matches!(action, WizardAction::KeyCaptured));
        assert_eq!(*wizard.key_status(0), KeyStatus::Captured);
        assert_eq!(wizard.translation_count(), 1);
    }

    #[test]
    fn test_capture_correct_key() {
        let mut wizard = CalibrationWizard::new();

        // Send the correct key (Backspace)
        let key = KeyEvent::new(KeyCode::Backspace, KeyModifiers::NONE);
        let action = wizard.handle_capture_key(key);

        assert!(matches!(action, WizardAction::KeyCaptured));
        // No translation needed, marked as skipped
        assert_eq!(*wizard.key_status(0), KeyStatus::Skipped);
        assert_eq!(wizard.translation_count(), 0);
    }

    #[test]
    fn test_restart() {
        let mut wizard = CalibrationWizard::new();

        // Capture a key
        let key = KeyEvent::new(KeyCode::Char('\x7f'), KeyModifiers::NONE);
        wizard.handle_capture_key(key);

        assert_eq!(wizard.translation_count(), 1);

        // Restart
        wizard.restart();

        assert_eq!(wizard.translation_count(), 0);
        assert!(matches!(
            wizard.step,
            CalibrationStep::Capture {
                group_idx: 0,
                key_idx: 0
            }
        ));
    }

    #[test]
    fn test_verify_phase() {
        let mut wizard = CalibrationWizard::new();

        // Skip all keys to get to verify phase
        for _ in 0..24 {
            let key = KeyEvent::new(KeyCode::Char('s'), KeyModifiers::NONE);
            wizard.handle_capture_key(key);
        }

        assert!(wizard.is_verify_phase());
    }

    #[test]
    fn test_verify_save() {
        let mut wizard = CalibrationWizard::new();
        wizard.step = CalibrationStep::Verify;

        let key = KeyEvent::new(KeyCode::Char('y'), KeyModifiers::NONE);
        let action = wizard.handle_verify_key(key);

        assert!(matches!(action, WizardAction::Save));
    }

    #[test]
    fn test_build_translator() {
        let mut wizard = CalibrationWizard::new();

        // Capture a key mapping
        let raw = KeyEvent::new(KeyCode::Char('\x7f'), KeyModifiers::NONE);
        wizard.handle_capture_key(raw.clone());

        let translator = wizard.build_translator();
        assert_eq!(translator.len(), 1);

        // Test the translation
        let translated = translator.translate(raw);
        assert_eq!(translated.code, KeyCode::Backspace);
    }
}
