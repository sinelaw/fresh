//! Key translation layer for terminal input calibration
//!
//! This module provides a translation layer that sits between the terminal and the keymap.
//! It allows broken terminal key sequences to be normalized to expected key codes.
//!
//! The translation happens BEFORE keybinding resolution, so:
//! 1. Terminal sends raw KeyEvent
//! 2. KeyTranslator.translate(raw) → normalized KeyEvent
//! 3. KeybindingResolver.resolve(normalized) → Action
//!
//! This keeps calibration separate from keymap customization.

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

/// Key translation table that normalizes terminal input
///
/// Maps raw terminal events to expected/normalized events.
/// For example, if a terminal sends Char('\x7f') for backspace,
/// this translator can map it to KeyCode::Backspace.
#[derive(Debug, Clone, Default)]
pub struct KeyTranslator {
    /// Translation table: raw event → normalized event
    translations: HashMap<KeyEventKey, KeyEventKey>,
}

/// A serializable key event (simplified version of crossterm::KeyEvent)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct KeyEventKey {
    /// The key code
    pub code: SerializableKeyCode,
    /// Key modifiers (Shift, Ctrl, Alt, etc.)
    pub modifiers: u8, // KeyModifiers as bits
}

impl KeyEventKey {
    /// Create a KeyEventKey from a crossterm KeyEvent
    pub fn from_key_event(event: &KeyEvent) -> Self {
        Self {
            code: SerializableKeyCode::from_key_code(&event.code),
            modifiers: event.modifiers.bits(),
        }
    }

    /// Convert to a crossterm KeyEvent
    pub fn to_key_event(&self) -> KeyEvent {
        KeyEvent::new(
            self.code.to_key_code(),
            KeyModifiers::from_bits_truncate(self.modifiers),
        )
    }
}

/// A serializable key code
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(tag = "type", content = "value")]
pub enum SerializableKeyCode {
    /// Backspace key
    Backspace,
    /// Enter key
    Enter,
    /// Left arrow
    Left,
    /// Right arrow
    Right,
    /// Up arrow
    Up,
    /// Down arrow
    Down,
    /// Home key
    Home,
    /// End key
    End,
    /// Page up
    PageUp,
    /// Page down
    PageDown,
    /// Tab key
    Tab,
    /// Shift+Tab
    BackTab,
    /// Delete key
    Delete,
    /// Insert key
    Insert,
    /// Function key (1-12)
    F(u8),
    /// A character key
    Char(char),
    /// Null character (Ctrl+Space, etc.)
    Null,
    /// Escape key
    Esc,
    /// Caps Lock
    CapsLock,
    /// Scroll Lock
    ScrollLock,
    /// Num Lock
    NumLock,
    /// Print Screen
    PrintScreen,
    /// Pause key
    Pause,
    /// Menu key
    Menu,
    /// Modifier key only
    Modifier(String),
    /// Unknown key
    Unknown,
}

impl SerializableKeyCode {
    /// Convert from crossterm KeyCode
    pub fn from_key_code(code: &KeyCode) -> Self {
        match code {
            KeyCode::Backspace => SerializableKeyCode::Backspace,
            KeyCode::Enter => SerializableKeyCode::Enter,
            KeyCode::Left => SerializableKeyCode::Left,
            KeyCode::Right => SerializableKeyCode::Right,
            KeyCode::Up => SerializableKeyCode::Up,
            KeyCode::Down => SerializableKeyCode::Down,
            KeyCode::Home => SerializableKeyCode::Home,
            KeyCode::End => SerializableKeyCode::End,
            KeyCode::PageUp => SerializableKeyCode::PageUp,
            KeyCode::PageDown => SerializableKeyCode::PageDown,
            KeyCode::Tab => SerializableKeyCode::Tab,
            KeyCode::BackTab => SerializableKeyCode::BackTab,
            KeyCode::Delete => SerializableKeyCode::Delete,
            KeyCode::Insert => SerializableKeyCode::Insert,
            KeyCode::F(n) => SerializableKeyCode::F(*n),
            KeyCode::Char(c) => SerializableKeyCode::Char(*c),
            KeyCode::Null => SerializableKeyCode::Null,
            KeyCode::Esc => SerializableKeyCode::Esc,
            KeyCode::CapsLock => SerializableKeyCode::CapsLock,
            KeyCode::ScrollLock => SerializableKeyCode::ScrollLock,
            KeyCode::NumLock => SerializableKeyCode::NumLock,
            KeyCode::PrintScreen => SerializableKeyCode::PrintScreen,
            KeyCode::Pause => SerializableKeyCode::Pause,
            KeyCode::Menu => SerializableKeyCode::Menu,
            KeyCode::Modifier(m) => SerializableKeyCode::Modifier(format!("{:?}", m)),
            _ => SerializableKeyCode::Unknown,
        }
    }

    /// Convert to crossterm KeyCode
    pub fn to_key_code(&self) -> KeyCode {
        match self {
            SerializableKeyCode::Backspace => KeyCode::Backspace,
            SerializableKeyCode::Enter => KeyCode::Enter,
            SerializableKeyCode::Left => KeyCode::Left,
            SerializableKeyCode::Right => KeyCode::Right,
            SerializableKeyCode::Up => KeyCode::Up,
            SerializableKeyCode::Down => KeyCode::Down,
            SerializableKeyCode::Home => KeyCode::Home,
            SerializableKeyCode::End => KeyCode::End,
            SerializableKeyCode::PageUp => KeyCode::PageUp,
            SerializableKeyCode::PageDown => KeyCode::PageDown,
            SerializableKeyCode::Tab => KeyCode::Tab,
            SerializableKeyCode::BackTab => KeyCode::BackTab,
            SerializableKeyCode::Delete => KeyCode::Delete,
            SerializableKeyCode::Insert => KeyCode::Insert,
            SerializableKeyCode::F(n) => KeyCode::F(*n),
            SerializableKeyCode::Char(c) => KeyCode::Char(*c),
            SerializableKeyCode::Null => KeyCode::Null,
            SerializableKeyCode::Esc => KeyCode::Esc,
            SerializableKeyCode::CapsLock => KeyCode::CapsLock,
            SerializableKeyCode::ScrollLock => KeyCode::ScrollLock,
            SerializableKeyCode::NumLock => KeyCode::NumLock,
            SerializableKeyCode::PrintScreen => KeyCode::PrintScreen,
            SerializableKeyCode::Pause => KeyCode::Pause,
            SerializableKeyCode::Menu => KeyCode::Menu,
            SerializableKeyCode::Modifier(_) | SerializableKeyCode::Unknown => KeyCode::Null,
        }
    }
}

/// JSON format for the calibration file
#[derive(Debug, Serialize, Deserialize)]
struct CalibrationFile {
    /// Comment explaining the file format
    #[serde(rename = "_comment")]
    comment: String,
    /// Format description
    #[serde(rename = "_format")]
    format: String,
    /// Translation mappings
    translations: Vec<TranslationEntry>,
}

/// A single translation entry in the calibration file
#[derive(Debug, Serialize, Deserialize)]
struct TranslationEntry {
    /// The raw key that the terminal sends
    raw: KeyEventKey,
    /// The expected/normalized key
    expected: KeyEventKey,
}

impl KeyTranslator {
    /// Create a new empty translator (no translations)
    pub fn new() -> Self {
        Self {
            translations: HashMap::new(),
        }
    }

    /// Translate a raw terminal event to a normalized event
    ///
    /// If a translation exists for the raw event, returns the normalized event.
    /// Otherwise, returns the raw event unchanged.
    pub fn translate(&self, raw: KeyEvent) -> KeyEvent {
        let key = KeyEventKey::from_key_event(&raw);
        if let Some(normalized) = self.translations.get(&key) {
            normalized.to_key_event()
        } else {
            raw
        }
    }

    /// Check if a translation exists for the given key
    pub fn has_translation(&self, raw: &KeyEvent) -> bool {
        let key = KeyEventKey::from_key_event(raw);
        self.translations.contains_key(&key)
    }

    /// Add a translation mapping
    pub fn add_translation(&mut self, raw: KeyEvent, expected: KeyEvent) {
        let raw_key = KeyEventKey::from_key_event(&raw);
        let expected_key = KeyEventKey::from_key_event(&expected);
        self.translations.insert(raw_key, expected_key);
    }

    /// Remove a translation mapping
    pub fn remove_translation(&mut self, raw: &KeyEvent) {
        let key = KeyEventKey::from_key_event(raw);
        self.translations.remove(&key);
    }

    /// Get the number of translations
    pub fn len(&self) -> usize {
        self.translations.len()
    }

    /// Check if there are no translations
    pub fn is_empty(&self) -> bool {
        self.translations.is_empty()
    }

    /// Clear all translations
    pub fn clear(&mut self) {
        self.translations.clear();
    }

    /// Load translations from a JSON file
    ///
    /// Returns an empty translator if the file doesn't exist.
    /// Returns an error if the file exists but is invalid.
    pub fn load_from_file(path: &Path) -> Result<Self, std::io::Error> {
        if !path.exists() {
            return Ok(Self::new());
        }

        let content = std::fs::read_to_string(path)?;
        let file: CalibrationFile = serde_json::from_str(&content).map_err(|e| {
            std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("Invalid calibration file: {}", e),
            )
        })?;

        let mut translator = Self::new();
        for entry in file.translations {
            translator.translations.insert(entry.raw, entry.expected);
        }

        tracing::info!(
            "Loaded {} key translations from {}",
            translator.len(),
            path.display()
        );

        Ok(translator)
    }

    /// Save translations to a JSON file
    pub fn save_to_file(&self, path: &Path) -> Result<(), std::io::Error> {
        // Ensure parent directory exists
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let entries: Vec<TranslationEntry> = self
            .translations
            .iter()
            .map(|(raw, expected)| TranslationEntry {
                raw: raw.clone(),
                expected: expected.clone(),
            })
            .collect();

        let file = CalibrationFile {
            comment: "Generated by 'Calibrate Input Keys' wizard".to_string(),
            format: "raw_key → expected_key".to_string(),
            translations: entries,
        };

        let content = serde_json::to_string_pretty(&file)?;
        std::fs::write(path, content)?;

        tracing::info!(
            "Saved {} key translations to {}",
            self.len(),
            path.display()
        );

        Ok(())
    }

    /// Get the default calibration file path
    pub fn default_path() -> Option<std::path::PathBuf> {
        dirs::config_dir().map(|p| p.join("fresh").join("key_calibration.json"))
    }

    /// Load from the default config location
    pub fn load_default() -> Result<Self, std::io::Error> {
        if let Some(path) = Self::default_path() {
            Self::load_from_file(&path)
        } else {
            Ok(Self::new())
        }
    }

    /// Save to the default config location
    pub fn save_default(&self) -> Result<(), std::io::Error> {
        if let Some(path) = Self::default_path() {
            self.save_to_file(&path)
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "Could not determine config directory",
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_translator_empty() {
        let translator = KeyTranslator::new();
        assert!(translator.is_empty());

        // Raw events pass through unchanged
        let raw = KeyEvent::new(KeyCode::Backspace, KeyModifiers::NONE);
        let result = translator.translate(raw);
        assert_eq!(result.code, KeyCode::Backspace);
    }

    #[test]
    fn test_translator_with_mapping() {
        let mut translator = KeyTranslator::new();

        // Map Char('\x7f') to Backspace
        let raw = KeyEvent::new(KeyCode::Char('\x7f'), KeyModifiers::NONE);
        let expected = KeyEvent::new(KeyCode::Backspace, KeyModifiers::NONE);
        translator.add_translation(raw, expected);

        assert_eq!(translator.len(), 1);
        assert!(translator.has_translation(&raw));

        // Translation works
        let result = translator.translate(raw);
        assert_eq!(result.code, KeyCode::Backspace);
    }

    #[test]
    fn test_translator_preserves_unmapped() {
        let mut translator = KeyTranslator::new();

        // Add one mapping
        let mapped_raw = KeyEvent::new(KeyCode::Char('\x7f'), KeyModifiers::NONE);
        let mapped_expected = KeyEvent::new(KeyCode::Backspace, KeyModifiers::NONE);
        translator.add_translation(mapped_raw, mapped_expected);

        // Unmapped keys pass through
        let unmapped = KeyEvent::new(KeyCode::Char('a'), KeyModifiers::NONE);
        let result = translator.translate(unmapped);
        assert_eq!(result.code, KeyCode::Char('a'));
    }

    #[test]
    fn test_translator_with_modifiers() {
        let mut translator = KeyTranslator::new();

        // Map Alt+b to Alt+Left
        let raw = KeyEvent::new(KeyCode::Char('b'), KeyModifiers::ALT);
        let expected = KeyEvent::new(KeyCode::Left, KeyModifiers::ALT);
        translator.add_translation(raw, expected);

        let result = translator.translate(raw);
        assert_eq!(result.code, KeyCode::Left);
        assert!(result.modifiers.contains(KeyModifiers::ALT));
    }

    #[test]
    fn test_key_event_key_serialization() {
        let key = KeyEventKey {
            code: SerializableKeyCode::Backspace,
            modifiers: KeyModifiers::NONE.bits(),
        };

        let json = serde_json::to_string(&key).unwrap();
        let deserialized: KeyEventKey = serde_json::from_str(&json).unwrap();

        assert_eq!(key, deserialized);
    }

    #[test]
    fn test_roundtrip_key_event() {
        let original = KeyEvent::new(KeyCode::Home, KeyModifiers::SHIFT);
        let key = KeyEventKey::from_key_event(&original);
        let restored = key.to_key_event();

        assert_eq!(original.code, restored.code);
        assert_eq!(original.modifiers, restored.modifiers);
    }
}
