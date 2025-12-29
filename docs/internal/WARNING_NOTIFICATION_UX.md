# Warning Notification UX Design (Proposed)

This document describes the proposed UX for warning notifications, addressing issues #493 and #451
regarding the auto-opening of warning log files.

## Problem Statement

The current implementation auto-opens a warning log file tab when errors or warnings occur
(e.g., LSP server not found). This creates several UX issues:

1. **Unwanted interruption**: A new tab appears without user consent, cluttering the workspace
2. **Not actionable**: The warning log shows raw error text without suggesting solutions
3. **No user control**: No way to suppress or configure this behavior
4. **Repetitive noise**: Same warnings repeat endlessly in the log (e.g., "Failed to send didChange")

### Current Behavior

```
User opens .py file (pylsp not installed)
         |
         v
Tab auto-opens: fresh-warnings-XXXX.log
Status bar shows: "Warnings detected - see log"
         |
         v
User must manually close unwanted tab
```

Warning log content (repetitive, not actionable):
```
2025-12-27 19:18:30.960 ERROR: Failed to spawn LSP: 'pylsp' not found
2025-12-27 19:18:30.960 WARN: Failed to send didChange to LSP
2025-12-27 19:18:30.961 WARN: Failed to send didChange to LSP  <- repeats
2025-12-27 19:18:30.962 WARN: Failed to send didChange to LSP  <- repeats
...
```

## Design Principles

Based on NNGroup usability heuristics:

1. **User Control and Freedom**: User decides when to see details, not the system
2. **Progressive Disclosure**: Badge -> Click -> Details -> Full log
3. **Recognition over Recall**: Visual indicators (color, icons) are quickly scannable
4. **Aesthetic and Minimalist Design**: Show only what's needed, when needed
5. **Help Users Recover from Errors**: Provide actionable solutions, not just error text

**Core principle**: The editor respects that the user is trying to edit text.
Warnings are available, not pushed.

## Proposed Solution

### Two-Tier Notification System

**Tier 1: Visual Indicator (Non-Intrusive)**
- Colored background segment in status bar
- No popup, no tab, no modal
- User can completely ignore if they don't care

**Tier 2: Actionable Popup (User-Initiated)**
- Appears only when user clicks the indicator
- Shows grouped/deduplicated warnings
- Provides actionable solutions (e.g., install commands for LSP)
- Transient - dismisses on click outside or Escape

### Status Bar Visual States

```
Normal (no issues):
+-----------------------------------------------------------------------------+
| test.py | Ln 1, Col 0 | LSP [python]                         Palette: Ctrl+P|
+-----------------------------------------------------------------------------+

LSP not configured (silent - user doesn't care about LSP):
+-----------------------------------------------------------------------------+
| test.py | Ln 1, Col 0                                        Palette: Ctrl+P|
+-----------------------------------------------------------------------------+

LSP warning (noticeable but non-intrusive):
+-----------------------------------------------------------------------------+
| test.py | Ln 1, Col 0 |##LSP [python]##|                     Palette: Ctrl+P|
+-----------------------------------------------------------------------------+
                         ^^^^^^^^^^^^^^^^
                         Amber/yellow background
                         Visually distinct, not demanding

LSP error (more prominent):
+-----------------------------------------------------------------------------+
| test.py | Ln 1, Col 0 |@@LSP [python]@@|                     Palette: Ctrl+P|
+-----------------------------------------------------------------------------+
                         ^^^^^^^^^^^^^^^^
                         Red background
```

### Initial Appearance Animation

When a warning first occurs, use a brief visual pulse to catch the user's peripheral vision:

```
T=0 (error occurs):     [### BRIGHT/INVERTED ###]   <- Eye-catching
T=1s:                   [## Fading... ##]           <- Settling
T=2s:                   [# Steady state #]          <- Persistent subtle bg
```

This ensures the user notices without demanding immediate attention.

### User Clicks Indicator -> Contextual Popup

Only when user explicitly clicks the status bar segment or presses keybinding:

```
+-----------------------------------------------------------------------------+
| test.py x                                                                   |
|-----------------------------------------------------------------------------|
|    1 | print("hello")                                                       |
|~                                                                            |
|~                          +-- Python LSP ------------------+                |
|~                          |                                |                |
|~                          | [!] Server 'pylsp' not found   |                |
|~                          |                                |                |
|~                          | Install:                       |                |
|~                          | pip install python-lsp-server  |                |
|~                          |                       [Copy]   |                |
|~                          |                                |                |
|~                          | [Disable Python LSP] [Dismiss] |                |
|~                          +--------------------------------+                |
|~                                                                            |
|-----------------------------------------------------------------------------|
| test.py | Ln 1, Col 0 |##LSP [python]##|                     Palette: Ctrl+P|
+-----------------------------------------------------------------------------+
```

**Popup characteristics:**
- Transient: clicking outside or pressing Escape dismisses it
- Actionable: shows install command with Copy button
- Escape hatch: "Disable LSP" button for users who don't want LSP
- No "Don't show again" needed - the design is already non-intrusive

### Generic Warnings (Non-LSP)

For other warning types, similar pattern with warning count badge:

```
Status bar:
+-----------------------------------------------------------------------------+
| test.py | Ln 1, Col 0 |##[!] 2##|                            Palette: Ctrl+P|
+-----------------------------------------------------------------------------+

Click -> popup:
+-- Warnings (2) ---------------+
| * Failed to load plugin X     |
| * Theme file not found        |
|                               |
| [View Full Log]    [Dismiss]  |
+-------------------------------+
```

`[View Full Log]` opens the warning file in a new tab - **user-initiated**, not automatic.

## Generic Warning Domain Architecture

The warning system is designed to be extensible, allowing different subsystems (LSP, plugins,
config, etc.) to register their own warning domains with custom popup content.

### WarningDomain Trait

```rust
/// A domain that can report warnings/errors with custom popups
pub trait WarningDomain: Send + Sync {
    /// Domain identifier (e.g., "lsp", "plugin", "config")
    fn id(&self) -> &str;

    /// Display label for status bar (e.g., "LSP [python]", "Plugins")
    fn label(&self) -> String;

    /// Current warning level
    fn level(&self) -> WarningLevel;

    /// Content for popup when user clicks the indicator
    fn popup_content(&self) -> WarningPopupContent;
}

pub struct WarningPopupContent {
    pub title: String,
    pub message: String,           // Supports markdown
    pub actions: Vec<WarningAction>,
}

pub struct WarningAction {
    pub label: String,
    pub action_id: String,         // e.g., "copy_install_cmd", "disable_lsp", "view_log"
}
```

### Status Bar with Multiple Domains

```
Multiple warning sources:
+-----------------------------------------------------------------------------+
| file.py | Ln 1, Col 5 | LSP [python: ⚠] | [⚠ 2] |           Palette: Ctrl+P |
+-----------------------------------------------------------------------------+
                         ^^^^^^^^^^^^^^^^^   ^^^^^^
                         LSP domain          General warnings
                         (amber bg)          (amber bg)
```

### Domain Registration

Built-in domains are registered at startup:
- `LspWarningDomain` - LSP server errors/warnings
- `GeneralWarningDomain` - Catch-all for other logged warnings

Plugins can register additional domains via the plugin API.

### Benefits

1. **Extensible** - Plugins can add their own warning indicators
2. **Consistent UX** - All warnings follow same pattern (colored badge → click → popup)
3. **Domain-specific actions** - LSP shows install commands, plugins show disable option
4. **Decoupled** - Each domain manages its own state and popup content

## Plugin Architecture for LSP Install Helpers

LSP installation helpers are fully plugin-based, allowing different plugins for different languages.

### Required Hooks (Core → Plugin)

```rust
// In src/services/plugins/hooks.rs

/// LSP server failed to start
LspServerError {
    /// The language that failed (e.g., "python", "rust")
    language: String,
    /// The server command that failed (e.g., "pylsp", "rust-analyzer")
    server_command: String,
    /// Error type: "not_found", "spawn_failed", "timeout", "crash"
    error_type: String,
    /// Human-readable error message
    message: String,
}

/// User clicked the LSP status indicator in the status bar
LspStatusClicked {
    /// The language of the current buffer
    language: String,
    /// Whether there's an active error
    has_error: bool,
}

/// User selected an action from an action popup
ActionPopupResult {
    /// The popup ID (set when showing popup)
    popup_id: String,
    /// The action ID selected, or "dismissed" if closed without selection
    action_id: String,
}
```

### Required API (Plugin → Core)

```typescript
// New API additions to fresh.d.ts

interface ActionPopupAction {
  id: string;      // Unique action identifier
  label: string;   // Display text (can include install command)
}

interface ActionPopupOptions {
  id: string;                    // Popup identifier for ActionPopupResult
  title: string;                 // Popup title
  message: string;               // Body text (supports basic formatting)
  actions: ActionPopupAction[];  // Action buttons
}

// Show an action popup (user must click an action or dismiss)
fresh.ui.showActionPopup(options: ActionPopupOptions): void;

// Disable LSP for a specific language (persists to config)
fresh.lsp.disableForLanguage(language: string): void;

// Existing API (already implemented):
fresh.setClipboard(text: string): void;
fresh.setStatus(message: string): void;
```

### Example: Python LSP Helper Plugin

```typescript
// plugins/python-lsp.ts
// Users can create similar plugins for any language

const INSTALL_COMMANDS = {
  pip: "pip install python-lsp-server",
  pipx: "pipx install python-lsp-server",
  pip_all: "pip install 'python-lsp-server[all]'",
};

// Track error state
let pythonLspError: { serverCommand: string; message: string } | null = null;

// Listen for LSP errors
fresh.hooks.on("lspServerError", (event) => {
  if (event.language === "python") {
    pythonLspError = {
      serverCommand: event.serverCommand,
      message: event.message,
    };
  }
});

// Handle status bar click
fresh.hooks.on("lspStatusClicked", (event) => {
  if (event.language !== "python" || !pythonLspError) return;

  fresh.ui.showActionPopup({
    id: "python-lsp-help",
    title: "Python LSP Error",
    message: `Server '${pythonLspError.serverCommand}' not found.\n\nInstall with one of these commands:`,
    actions: [
      { id: "copy_pip", label: `Copy: ${INSTALL_COMMANDS.pip}` },
      { id: "copy_pipx", label: `Copy: ${INSTALL_COMMANDS.pipx}` },
      { id: "disable", label: "Disable Python LSP" },
      { id: "dismiss", label: "Dismiss" },
    ],
  });
});

// Handle action selection
fresh.hooks.on("actionPopupResult", (event) => {
  if (event.popup_id !== "python-lsp-help") return;

  switch (event.action_id) {
    case "copy_pip":
      fresh.setClipboard(INSTALL_COMMANDS.pip);
      fresh.setStatus("Copied: " + INSTALL_COMMANDS.pip);
      break;
    case "copy_pipx":
      fresh.setClipboard(INSTALL_COMMANDS.pipx);
      fresh.setStatus("Copied: " + INSTALL_COMMANDS.pipx);
      break;
    case "disable":
      fresh.lsp.disableForLanguage("python");
      fresh.setStatus("Python LSP disabled");
      pythonLspError = null;
      break;
  }
});
```

### Plugin Distribution

Each language can have its own plugin file:
- `plugins/python-lsp.ts` - Python LSP helper (bundled)
- `plugins/rust-lsp.ts` - Rust LSP helper (bundled)
- `plugins/typescript-lsp.ts` - TypeScript LSP helper (bundled)
- `~/.config/fresh/plugins/go-lsp.ts` - User-created Go helper

This allows:
1. **Language-specific behavior** - Each plugin knows its ecosystem (pip vs npm vs cargo)
2. **User extensibility** - Users add plugins for languages we don't bundle
3. **Community sharing** - Plugins can be shared independently
4. **No core changes** - Adding new language support doesn't require editor changes

## Theme Colors

Add warning indicator colors to the theme system:

```rust
// In src/view/theme.rs

pub struct Theme {
    // ... existing fields ...

    /// Background for warning indicators (amber/yellow)
    pub warning_indicator_bg: Color,
    /// Foreground for warning indicators
    pub warning_indicator_fg: Color,
    /// Background for error indicators (red)
    pub error_indicator_bg: Color,
    /// Foreground for error indicators
    pub error_indicator_fg: Color,
}
```

Default values (work on both dark and light themes):
- Warning: `#B58900` background (amber), `#000000` foreground
- Error: `#DC322F` background (red), `#FFFFFF` foreground

## Settings

Minimal settings - the design is already non-intrusive:

```json
{
  "warnings": {
    "show_status_indicator": true   // default: true, can disable entirely
  }
}
```

No complex per-warning suppression needed because nothing is intrusive.

## Interaction Flow

```
User opens .py file
         |
         v
    LSP init fails
         |
         v
Update status bar: colored [python] segment
Store install helper info in state
         |
         v
      DONE  <-- No popup, no tab, no modal
                User continues editing uninterrupted


User notices colored indicator later
         |
         v
Clicks indicator (or presses Ctrl+Shift+L)
         |
         v
+---------------------------+
| Transient popup appears   |
| with:                     |
| * Error explanation       |
| * Install command + Copy  |
| * [Disable LSP] button    |
+---------------------------+
         |
         v
User takes action or dismisses
         |
         v
Popup closes, user continues editing
```

## Files to Modify

1. **`src/config.rs`** - Add `warnings.show_status_indicator` setting
2. **`src/services/warning_log.rs`** - Add warning deduplication, remove auto-open trigger
3. **`src/app/mod.rs`** - Replace `check_warning_log()` auto-open with state update
4. **`src/view/ui/status_bar.rs`** - Add colored warning segment rendering with animation
5. **`src/view/theme.rs`** - Add `warning_indicator_*` and `error_indicator_*` colors
6. **`src/services/plugins/hooks.rs`** - Add `LspInitError` hook
7. **`src/view/popup.rs`** - Add warning popup variant with action buttons
8. **`plugins/lsp-install-helper.ts`** - New bundled plugin for install helpers

## Comparison: Before and After

| Aspect | Before | After |
|--------|--------|-------|
| Auto-open tab | Yes (intrusive) | No |
| Modal popup | N/A | No |
| User interruption | Every warning | Never |
| Visual indicator | Text only | Colored background segment |
| Discoverability | Low (easy to miss status text) | High (color draws attention) |
| Actionability | None (raw log text) | Install commands with Copy |
| User control | None | Click to see, Disable LSP option |
| Extensibility | None | Plugin-based install helpers |

## Implementation Status

### Completed ✓

1. **Status bar colored indicators** - LSP indicator and warning badge with colored backgrounds
2. **Warning domain architecture** - `WarningDomain` trait, `GeneralWarningDomain`, `LspWarningDomain`
3. **Theme colors** - `status_warning_indicator_bg/fg`, `status_error_indicator_bg/fg` + hover variants
4. **Hover styling** - Lighter colors and underline on hover to indicate clickability
5. **Warning log deduplication** - Tracks seen warnings to avoid repetitive log entries
6. **Commands** - `ShowWarnings`, `ShowLspStatus`, `ClearWarnings` via command palette
7. **Mouse click handlers** - Clicking LSP indicator or warning badge triggers appropriate action
8. **Settings** - `warnings.show_status_indicator` config option
9. **No auto-open** - Removed intrusive auto-opening of warning log tab
10. **E2E tests** - Tests for command existence and basic execution

### Plugin API ✓

**New Hooks (Core → Plugin):**
1. `LspServerError` - Emitted when LSP fails to start (language, server_command, error_type, message) ✓
2. `LspStatusClicked` - Emitted when user clicks LSP indicator (language, has_error) ✓
3. `ActionPopupResult` - Emitted when user selects action or dismisses popup (popup_id, action_id) ✓

**New API functions (Plugin → Core):**
1. `editor.showActionPopup(options)` - Show popup with selectable action list ✓
2. `editor.disableLspForLanguage(language)` - Disable LSP and persist to config ✓

**Bundled plugins:**
1. `plugins/python-lsp.ts` - Python LSP helper (pip, pipx install commands) ✓
2. `plugins/rust-lsp.ts` - Rust LSP helper (rustup, brew, cargo install commands) ✓
3. `plugins/typescript-lsp.ts` - TypeScript/JavaScript LSP helper (npm, yarn, pnpm install commands) ✓

### Dropped

- Initial appearance animation (unnecessary complexity)

## References

- GitHub Issue #493: How to suppress auto opening of warning file
- GitHub Issue #451: Log file appeared (LSP error tab auto-opens)
- NNGroup: 10 Usability Heuristics for User Interface Design
