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

## Plugin Architecture for LSP Install Helpers

LSP installation helpers are implemented as plugins, making them user-extensible.

### New Hook: LspInitError

```rust
// In src/services/plugins/hooks.rs

/// LSP initialization error occurred
LspInitError {
    /// The language that failed (e.g., "python", "rust")
    language: String,
    /// The server command that failed (e.g., "pylsp", "rust-analyzer")
    server_command: String,
    /// The error type: "server_not_found", "spawn_failed", "init_timeout"
    error_type: String,
    /// Human-readable error message
    message: String,
}
```

### Plugin API

```typescript
// plugins/lsp-install-helper.ts

// Extensible registry - users can add their own in ~/.config/fresh/plugins/
const lspInstallHelpers: Record<string, LspInstallHelper> = {
  python: {
    serverName: "pylsp",
    installCommand: "pip install python-lsp-server",
    alternatives: [
      "pip install 'python-lsp-server[all]'",
      "pipx install python-lsp-server",
    ],
  },
  rust: {
    serverName: "rust-analyzer",
    installCommand: "rustup component add rust-analyzer",
    alternatives: [
      "brew install rust-analyzer",
    ],
  },
  typescript: {
    serverName: "typescript-language-server",
    installCommand: "npm install -g typescript-language-server typescript",
    alternatives: [],
  },
};

// Users can extend via their own plugins:
fresh.lsp.registerInstallHelper("go", {
  serverName: "gopls",
  installCommand: "go install golang.org/x/tools/gopls@latest",
  alternatives: ["brew install gopls"],
});
```

### Plugin Hook Handler

```typescript
fresh.hooks.on("lspInitError", async (event) => {
  const helper = lspInstallHelpers[event.language];
  if (helper && event.errorType === "server_not_found") {
    // Store helper info for when user clicks the status indicator
    fresh.state.set(`lsp.${event.language}.installHelper`, helper);
  }
});
```

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

## References

- GitHub Issue #493: How to suppress auto opening of warning file
- GitHub Issue #451: Log file appeared (LSP error tab auto-opens)
- NNGroup: 10 Usability Heuristics for User Interface Design
