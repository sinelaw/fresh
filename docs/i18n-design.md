# Internationalization (i18n) Design for Fresh Editor

## Overview

This document outlines the design for adding internationalization support to Fresh, enabling the UI to display translated text for different locales.

## Research Summary

### Available Rust i18n Libraries

| Library | Pros | Cons |
|---------|------|------|
| **rust-i18n** | Simple `t!()` macro, compile-time embedding, YAML/JSON/TOML support, fallback locales | Less sophisticated pluralization |
| **Project Fluent** | Natural-sounding translations, advanced grammar (plurals, gender), Mozilla-backed | More complex setup |
| **gettext-rs** | Industry standard, familiar to translators | FFI dependency, larger binary |
| **i18n_codegen** | Compile-time key validation | Less maintained |

### Recommendation: rust-i18n

For Fresh, **rust-i18n** is the recommended choice because:

1. **Simplicity**: The `t!()` macro integrates naturally into Rust code
2. **Compile-time safety**: Translations are embedded into the binary at compile time
3. **Zero runtime overhead**: No file I/O or parsing at startup
4. **Familiar formats**: Uses JSON for translations (consistent with Fresh's config format)
5. **Fallback support**: Gracefully falls back to English for missing translations
6. **TUI-appropriate**: Designed for applications where translations should be embedded

## Current Hard-coded String Categories

### 1. Menu Labels (JSON config + runtime)
**Location**: `src/config.rs`, menu config files
**Count**: ~50+ labels
**Examples**:
- Menu names: "File", "Edit", "View", "Selection", "Go", "Help", "Terminal"
- Menu items: "New", "Save", "Quit", "Undo", "Redo", "Toggle Explorer"

### 2. Status Bar Strings
**Location**: `src/view/ui/status_bar.rs`
**Count**: ~20 strings
**Examples**:
- `"Ln {}, Col {}"` - Line/column indicators
- `"Palette: "` - Command palette indicator
- `"Update: v{}"` - Update available notice
- `"Open: "` - File open prompt prefix
- `"[x] Case Sensitive"`, `"[x] Whole Word"`, `"[x] Regex"` - Search options
- `"[x] Confirm each"` - Replace confirmation option

### 3. Line Ending Indicators
**Location**: `src/model/buffer.rs`
**Count**: 3 strings
**Examples**:
- `"LF"`, `"CRLF"`, `"CR"`

### 4. Buffer/Tab Names
**Location**: `src/app/types.rs`
**Count**: ~5 strings
**Examples**:
- `"[No Name]"` - Unnamed buffer
- `"[Unknown]"` - Unknown path
- `"Virtual buffer"`, `"Unnamed buffer"` - LSP disabled reasons

### 5. Tab Context Menu
**Location**: `src/app/types.rs`
**Count**: 5 strings
**Examples**:
- `"Close"`, `"Close Others"`, `"Close to the Right"`, `"Close to the Left"`, `"Close All"`

### 6. File Browser Dialog
**Location**: `src/view/ui/file_browser.rs`
**Count**: ~15 strings
**Examples**:
- Column headers: `"Name"`, `"Size"`, `"Modified"`
- Navigation labels: `"Documents"`, `"Downloads"`, `"Navigation: "`
- States: `" Loading..."`, `" Error: {error}"`

### 7. Status Messages
**Location**: `src/app/render.rs`, `src/app/prompt_actions.rs`
**Count**: ~60+ strings
**Examples**:
- Search: `"No text to search"`, `"No more matches."`, `"Search cancelled."`
- Replace: `"Replace '{}' with: "`, `"Query replace '{}' with: "`
- File operations: `"Saved as: {}"`, `"Error saving file: {}"`
- Macros: `"Macro '{}' saved ({} actions)"`, `"No macros recorded"`
- Bookmarks: `"Bookmark '{}' set"`, `"Jumped to bookmark '{}'"`

### 8. LSP-related Messages
**Location**: `src/app/render.rs`, `src/app/lsp_requests.rs`
**Count**: ~15 strings
**Examples**:
- `"Start LSP Server: {}?"` - Confirmation dialog title
- `"Allow this time"`, `"Always allow"`, `"Don't start"` - Dialog buttons
- `"LSP server for {} started"`, `"LSP server for {} startup cancelled"`
- Install hints: `"Install with: pip install python-lsp-server"`

### 9. Prompt Messages
**Location**: `src/app/prompt_actions.rs`, `src/app/clipboard.rs`
**Count**: ~15 strings
**Examples**:
- `"Shell command: "`, `"Shell command (replace): "`
- `"Copy with theme: "`, `"Save as: "`
- `"Not a directory: {}"`, `"Invalid line number: {}"`

### 10. Error Messages
**Location**: Various files
**Count**: ~30 strings
**Examples**:
- `"Failed to spawn shell: {}"`, `"Command failed: {}"`
- `"Invalid UTF-8 in output: {}"`, `"Buffer not fully loaded"`

## Proposed Directory Structure

```
fresh/
├── Cargo.toml                    # Add rust-i18n dependency
├── locales/
│   ├── en.json                   # English (default/fallback)
│   ├── de.json                   # German
│   ├── fr.json                   # French
│   ├── es.json                   # Spanish
│   ├── zh-CN.json                # Simplified Chinese
│   ├── ja.json                   # Japanese
│   └── ...
├── src/
│   ├── i18n.rs                   # i18n initialization and helpers
│   ├── lib.rs                    # Add i18n! macro initialization
│   └── ...
```

## Translation File Format (JSON)

Using rust-i18n's version 1 format (one file per locale):

```json
{
  "_version": 1,

  "menu.file": "File",
  "menu.edit": "Edit",
  "menu.view": "View",
  "menu.selection": "Selection",
  "menu.go": "Go",
  "menu.help": "Help",
  "menu.terminal": "Terminal",

  "menu.file.new": "New",
  "menu.file.open": "Open",
  "menu.file.save": "Save",
  "menu.file.save_as": "Save As",
  "menu.file.quit": "Quit",

  "menu.edit.undo": "Undo",
  "menu.edit.redo": "Redo",
  "menu.edit.cut": "Cut",
  "menu.edit.copy": "Copy",
  "menu.edit.paste": "Paste",

  "status.line_col": "Ln %{line}, Col %{col}",
  "status.palette": "Palette: %{shortcut}",
  "status.update_available": "Update: v%{version}",

  "line_ending.lf": "LF",
  "line_ending.crlf": "CRLF",
  "line_ending.cr": "CR",

  "buffer.no_name": "[No Name]",
  "buffer.unknown": "[Unknown]",

  "tab.close": "Close",
  "tab.close_others": "Close Others",
  "tab.close_to_right": "Close to the Right",
  "tab.close_to_left": "Close to the Left",
  "tab.close_all": "Close All",

  "file_browser.name": "Name",
  "file_browser.size": "Size",
  "file_browser.modified": "Modified",
  "file_browser.navigation": "Navigation: ",
  "file_browser.loading": "Loading...",
  "file_browser.error": "Error: %{error}",
  "file_browser.documents": "Documents",
  "file_browser.downloads": "Downloads",

  "search.no_text": "No text to search",
  "search.no_matches": "No more matches.",
  "search.cancelled": "Search cancelled.",
  "search.match_of": "Match %{current} of %{total}",
  "search.case_sensitive": "Case Sensitive",
  "search.whole_word": "Whole Word",
  "search.regex": "Regex",
  "search.confirm_each": "Confirm each",

  "replace.prompt": "Replace '%{search}' with: ",
  "replace.query_prompt": "Query replace '%{search}' with: ",
  "replace.empty_query": "Replace: empty search query.",
  "replace.no_occurrences": "No occurrences of '%{search}' found.",
  "replace.completed": "Replaced %{count} occurrence(s)",

  "file.open_prompt": "Open: ",
  "file.save_as_prompt": "Save as: ",
  "file.saved_as": "Saved as: %{path}",
  "file.error_saving": "Error saving file: %{error}",
  "file.error_opening": "Error opening file: %{error}",
  "file.not_directory": "Not a directory: %{path}",

  "lsp.start_server": "Start LSP Server: %{language}?",
  "lsp.allow_once": "Allow this time",
  "lsp.always_allow": "Always allow",
  "lsp.dont_start": "Don't start",
  "lsp.server_started": "LSP server for %{language} started",
  "lsp.startup_cancelled": "LSP server for %{language} startup cancelled",
  "lsp.disabled.unnamed": "Unnamed buffer",
  "lsp.disabled.virtual": "Virtual buffer",

  "macro.saved": "Macro '%{key}' saved (%{count} actions)",
  "macro.played": "Played macro '%{key}' (%{count} actions)",
  "macro.empty": "Macro '%{key}' is empty",
  "macro.not_found": "No macro recorded for '%{key}'",
  "macro.none_recorded": "No macros recorded",
  "macro.not_recording": "Not recording a macro",

  "bookmark.set": "Bookmark '%{key}' set",
  "bookmark.jumped": "Jumped to bookmark '%{key}'",
  "bookmark.not_set": "Bookmark '%{key}' not set",
  "bookmark.cleared": "Bookmark '%{key}' cleared",
  "bookmark.buffer_gone": "Bookmark '%{key}': buffer no longer exists",

  "shell.prompt": "Shell command: ",
  "shell.prompt_replace": "Shell command (replace): ",
  "shell.spawn_failed": "Failed to spawn shell: %{error}",
  "shell.command_failed": "Command failed: %{error}",

  "error.invalid_regex": "Invalid regex: %{error}",
  "error.invalid_line": "Invalid line number: %{input}",
  "error.buffer_not_loaded": "Buffer not fully loaded",

  "diagnostics.none": "No diagnostics in current buffer",
  "diagnostics.bracket_none": "No bracket at cursor",
  "diagnostics.bracket_no_match": "No matching bracket found",

  "view.compose": "Compose",

  "clipboard.no_text": "No text to copy",
  "clipboard.copied_plain": "Copied as plain text",
  "clipboard.copy_theme_prompt": "Copy with theme: ",

  "lines.commented": "Comment",
  "lines.uncommented": "Uncomment",
  "lines.action": "%{action}ed %{count} line(s)"
}
```

## Implementation Plan

### Phase 1: Infrastructure Setup

1. **Add rust-i18n dependency to Cargo.toml**:
   ```toml
   [dependencies]
   rust-i18n = "3"
   ```

2. **Create i18n module** (`src/i18n.rs`):
   ```rust
   //! Internationalization support for Fresh

   use rust_i18n::t;

   // Re-export the t! macro for convenience
   pub use rust_i18n::t;

   /// Initialize i18n with the user's locale preference
   pub fn init() {
       // Try to detect system locale, fallback to English
       let locale = detect_locale().unwrap_or_else(|| "en".to_string());
       rust_i18n::set_locale(&locale);
   }

   /// Detect the user's preferred locale from environment
   fn detect_locale() -> Option<String> {
       // Check LANG, LC_ALL, LC_MESSAGES environment variables
       std::env::var("LANG")
           .or_else(|_| std::env::var("LC_ALL"))
           .or_else(|_| std::env::var("LC_MESSAGES"))
           .ok()
           .map(|s| {
               // Parse locale string (e.g., "en_US.UTF-8" -> "en")
               s.split('_').next().unwrap_or("en").to_string()
           })
   }

   /// Get the current locale
   pub fn current_locale() -> String {
       rust_i18n::locale().to_string()
   }

   /// Set the locale explicitly (for user preference override)
   pub fn set_locale(locale: &str) {
       rust_i18n::set_locale(locale);
   }

   /// Get list of available locales
   pub fn available_locales() -> Vec<&'static str> {
       rust_i18n::available_locales!()
   }
   ```

3. **Initialize in lib.rs**:
   ```rust
   #[macro_use]
   extern crate rust_i18n;

   i18n!("locales", fallback = "en");
   ```

### Phase 2: String Extraction and Translation Keys

Create a helper script or use manual extraction to identify all translatable strings:

1. **Status bar strings** -> `status.*` keys
2. **Menu labels** -> `menu.*` keys
3. **Buffer/tab names** -> `buffer.*`, `tab.*` keys
4. **File browser** -> `file_browser.*` keys
5. **Search/Replace** -> `search.*`, `replace.*` keys
6. **LSP messages** -> `lsp.*` keys
7. **Error messages** -> `error.*` keys
8. **Macro/Bookmark** -> `macro.*`, `bookmark.*` keys

### Phase 3: Code Migration

Replace hard-coded strings with `t!()` macro calls:

**Before:**
```rust
self.set_status_message("No text to search".to_string());
```

**After:**
```rust
use rust_i18n::t;
self.set_status_message(t!("search.no_text").to_string());
```

**With interpolation - Before:**
```rust
self.set_status_message(format!("Saved as: {}", path.display()));
```

**After:**
```rust
self.set_status_message(t!("file.saved_as", path = path.display().to_string()).to_string());
```

### Phase 4: Menu System Integration

The menu system uses JSON configuration. Two approaches:

**Option A: Translate at render time (recommended)**
- Keep menu config in English as keys
- Look up translations when rendering menu labels
- Pros: No config changes, labels auto-update with locale change

```rust
// In menu rendering
let translated_label = t!(&format!("menu.{}", menu.label.to_lowercase()));
```

**Option B: Use translation keys in config**
- Change menu config to use translation keys instead of raw strings
- Pros: Explicit, testable
- Cons: Requires config changes

### Phase 5: Configuration Option

Add locale preference to config:

```rust
// In config.rs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    // ... existing fields ...

    /// UI language/locale (e.g., "en", "de", "fr", "ja")
    /// If not set, uses system locale
    #[serde(default)]
    pub locale: Option<String>,
}
```

### Phase 6: Testing

1. **Unit tests**: Verify translations load correctly
2. **Integration tests**: Test UI rendering with different locales
3. **Missing translation detection**: CI check for untranslated keys

## Migration Strategy

### Priority Order

1. **High visibility strings** (users see these frequently):
   - Status bar indicators
   - Menu labels
   - Common status messages (save, search, errors)

2. **Dialog strings**:
   - File browser headers and labels
   - LSP confirmation dialogs
   - Tab context menu

3. **Infrequent messages**:
   - Macro/bookmark messages
   - Advanced error messages
   - Shell command prompts

### File-by-File Migration

| File | String Count | Priority |
|------|-------------|----------|
| `view/ui/status_bar.rs` | ~20 | High |
| `view/ui/menu.rs` | ~5 (runtime) | High |
| `app/types.rs` | ~10 | High |
| `app/render.rs` | ~60 | High |
| `view/ui/file_browser.rs` | ~15 | Medium |
| `app/prompt_actions.rs` | ~25 | Medium |
| `app/lsp_requests.rs` | ~10 | Medium |
| `model/buffer.rs` | ~3 | Low |

## Estimated String Counts by Category

| Category | Count | Example |
|----------|-------|---------|
| Menu labels | ~50 | "File", "Save", "Undo" |
| Status messages | ~60 | "Saved as: {}", "Match 1 of 5" |
| Dialog labels | ~20 | "Name", "Size", "Allow this time" |
| Error messages | ~30 | "Invalid regex: {}" |
| Buffer/Tab names | ~10 | "[No Name]", "Close Others" |
| **Total** | **~170** | |

## Contributors Guide

### Adding a New Locale

1. Copy `locales/en.json` to `locales/<locale>.json`
2. Translate all strings, preserving `%{variable}` placeholders
3. Test with `LANG=<locale> cargo run`
4. Submit PR with the new locale file

### Adding New Translatable Strings

1. Add English string to `locales/en.json` with appropriate key
2. Use `t!("key.path")` in Rust code
3. Update other locale files (can be done by translators later)

## Runtime Considerations

- **Binary size**: ~1-2KB per locale (JSON compiled to binary)
- **Startup time**: Negligible (no file I/O, embedded strings)
- **Memory**: Minimal (strings loaded on demand)
- **Performance**: Zero overhead for translation lookups (compile-time)

## Future Enhancements

1. **Locale switcher**: Add command palette action to change locale at runtime
2. **RTL support**: Consider right-to-left languages (Arabic, Hebrew)
3. **Pluralization**: Use Fluent for languages with complex plural rules
4. **Date/time formatting**: Locale-aware timestamp display

## References

- [rust-i18n documentation](https://github.com/longbridge/rust-i18n)
- [Project Fluent](https://projectfluent.org/) (alternative for complex translations)
- [LogRocket Rust i18n guide](https://blog.logrocket.com/rust-internationalization-localization-and-translation/)

## Sources

- [Rust i18n crates comparison](https://lib.rs/internationalization)
- [rust-i18n GitHub repository](https://github.com/longbridge/rust-i18n)
- [Project Fluent GitHub](https://github.com/projectfluent/fluent-rs)
