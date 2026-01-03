# Input Calibration Wizard Design

## Overview

The Input Calibration Wizard is a fail-safe feature designed for hostile terminal environments where standard keyboard input may be unreliable. This includes scenarios where:

- `Enter` might send a newline instead of a submit signal
- `Esc` might be trapped by the browser or window manager
- `Ctrl` key combinations might be intercepted by the terminal emulator or OS

The wizard uses **standard lowercase ASCII characters** (`a`, `s`, `y`, `n`, `r`) as control commands because they work on virtually every terminal since 1970.

## Problem Statement

Terminal emulators and environments vary widely in how they handle special keys:
- Web-based terminals (browser-based SSH clients, cloud IDEs)
- Nested terminal sessions (tmux inside SSH inside screen)
- Non-standard terminal emulators
- Accessibility tools that intercept keys
- Virtual machine consoles

Users in these environments often find that keys like Backspace, Delete, Home, End, or Ctrl+Arrow don't work as expected. The calibration wizard provides a way to remap these keys to whatever the terminal actually sends.

### Relationship to Kitty Keyboard Protocol

Fresh already enables the **Kitty keyboard protocol** (progressive enhancement) via crossterm's `PushKeyboardEnhancementFlags`:

```rust
// From main.rs
let keyboard_flags = KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES
    | KeyboardEnhancementFlags::REPORT_ALTERNATE_KEYS;
```

**When Kitty protocol is supported** (Kitty, WezTerm, foot, etc.):
- Keys are unambiguous (Backspace vs Ctrl+H, Enter vs Ctrl+M)
- Modifier keys are reported precisely
- No calibration needed for most users

**When Kitty protocol is NOT supported** (legacy terminals, web terminals, some SSH clients):
- Falls back to traditional escape sequences
- Ambiguity between keys (0x7F vs 0x08 for backspace)
- **This is where the calibration wizard helps**

The calibration wizard is the **fallback** for terminals that don't support the Kitty protocol. The translation layer sits after crossterm's parsing, so it works with both Kitty and legacy terminals.

## Design Goals

1. **Absolutely Fail-Safe**: Use only lowercase ASCII letters for navigation/control
2. **No Dependencies on Problem Keys**: Never require Enter, Esc, or Ctrl to navigate the wizard
3. **Simple UI**: Plain text rendering that doesn't require specific terminal capabilities
4. **Verification Before Save**: Let users test their mappings before committing
5. **Easy Abort**: Users can always exit without saving changes

## Architecture: Translation Layer

The calibration system is a **translation layer** that sits between the terminal and the keymap:

```
┌─────────────┐     ┌──────────────────┐     ┌───────────────────┐     ┌────────┐
│  Terminal   │ ──► │  KeyTranslator   │ ──► │ KeybindingResolver│ ──► │ Action │
│ (raw input) │     │  (calibration)   │     │     (keymap)      │     │        │
└─────────────┘     └──────────────────┘     └───────────────────┘     └────────┘
       │                    │                         │
       │                    │                         │
  "My terminal         "Normalize to              "Map key to
   sends 0x7F"          Backspace"                 action"
```

### Why a Translation Layer (not Action Overrides)

**Option A (Rejected): Override actions directly**
```
Raw Key → Action
0x7F → DeleteBackward
```
Problems:
- Bypasses the keymap entirely
- Can't combine with user keybinding customization
- Different behavior per keybinding map (emacs vs vscode)

**Option B (Chosen): Translate keys, then use keymap**
```
Raw Key → Normalized Key → Keymap → Action
0x7F → Backspace → (keymap lookup) → DeleteBackward
```
Benefits:
- Calibration is purely "terminal compatibility"
- Keymap customization still works on top
- Same calibration works with all keybinding maps
- Users can still rebind Backspace to something else if they want

### Translation Table Format

The calibration maps **raw terminal events** to **expected key codes**:

| Raw Event (what terminal sends) | Expected Key (what Fresh expects) |
|--------------------------------|----------------------------------|
| `Char('\x7f')` | `KeyCode::Backspace` |
| `Char('\x08')` | `KeyCode::Backspace` |
| `Alt('b')` | `KeyCode::Left` + `Alt` modifier |
| `Esc, '[', '1', '~'` | `KeyCode::Home` |

This translation happens **before** the KeybindingResolver sees the event.

## The Fail-Safe Control Scheme

During the wizard, specific lowercase letters are **reserved commands**:

| Key | Action | Description |
|-----|--------|-------------|
| `s` | **Skip** | Skip current key calibration (keep default) |
| `g` | **Skip Group** | Skip all remaining keys in current group |
| `a` | **Abort** | Exit wizard, discard all changes |
| `y` | **Yes** / Confirm | Save settings and exit (verification phase only) |
| `r` | **Retry** | Restart the wizard from the beginning |

**Why these keys are safe to reserve**: The wizard calibrates control keys (Backspace, Delete, Home, End, Alt+Arrow) not alphanumeric keys. It is extremely unlikely that a physical "Home" key sends the letter "s".

## Keys to Calibrate

The wizard calibrates the "problem children" - keys most likely to be broken in hostile terminals. Based on [issue #219](https://github.com/sinelaw/fresh/issues/219), these are organized into categories:

### Group 1: Basic Editing

| # | Key | Action | Common Issues |
|---|-----|--------|---------------|
| 1 | **BACKSPACE** | Delete backward | Sends `0x7F`, `0x08`, `Delete`, or `Ctrl+H` |
| 2 | **DELETE** | Delete forward | Sometimes confused with Backspace |
| 3 | **TAB** | Indent / Next field | May be intercepted for completion |
| 4 | **SHIFT + TAB** | Dedent / Prev field | Modifier combo issues |

### Group 2: Line Navigation

| # | Key | Action | Common Issues |
|---|-----|--------|---------------|
| 5 | **HOME** | Line start | Often not forwarded by terminals |
| 6 | **END** | Line end | Often not forwarded by terminals |
| 7 | **SHIFT + HOME** | Select to line start | Modifier combo issues |
| 8 | **SHIFT + END** | Select to line end | Modifier combo issues |

### Group 3: Word Navigation

| # | Key | Action | Common Issues |
|---|-----|--------|---------------|
| 9 | **ALT + LEFT** | Word left | Produces special chars on intl keyboards |
| 10 | **ALT + RIGHT** | Word right | Produces special chars on intl keyboards |
| 11 | **ALT + SHIFT + LEFT** | Select word left | Modifier combo issues |
| 12 | **ALT + SHIFT + RIGHT** | Select word right | Modifier combo issues |
| 13 | **CTRL + LEFT** | Word left (alt) | Alternative for broken Alt |
| 14 | **CTRL + RIGHT** | Word right (alt) | Alternative for broken Alt |
| 15 | **CTRL + SHIFT + LEFT** | Select word left (alt) | Alternative for broken Alt |
| 16 | **CTRL + SHIFT + RIGHT** | Select word right (alt) | Alternative for broken Alt |

### Group 4: Document Navigation

| # | Key | Action | Common Issues |
|---|-----|--------|---------------|
| 17 | **PAGE UP** | Page up | Sometimes intercepted |
| 18 | **PAGE DOWN** | Page down | Sometimes intercepted |
| 19 | **CTRL + HOME** | Document start | Modifier combo issues |
| 20 | **CTRL + END** | Document end | Modifier combo issues |

### Group 5: Emacs-Style Navigation

| # | Key | Action | Notes |
|---|-----|--------|-------|
| 21 | **CTRL + A** | Line start | Emacs-style, useful when Home broken |
| 22 | **CTRL + E** | Line end | Emacs-style, useful when End broken |
| 23 | **CTRL + K** | Delete to line end | Emacs kill-line |
| 24 | **CTRL + Y** | Paste (yank) | Emacs yank, also used for redo |

**Total: 24 keys** across 5 groups.

Users can skip any group with `g` or any individual key with `s`.

## User Flow

### Phase 1: Capture Loop (Steps 1-24)

The UI is stripped down - no fancy boxes that require specific rendering support:

```
[ FRESH EDITOR INPUT CALIBRATION ]
--------------------------------------------------
STEP 1 / 24 : Calibrating [ BACKSPACE ]
(Group 1: Basic Editing - 1 of 4)

Please press your physical BACKSPACE key now.

 Controls:
 [ s ] Skip this key (keep default)
 [ g ] Skip entire group (Basic Editing)
 [ a ] Abort wizard
--------------------------------------------------
 waiting for input...
```

**Logic:**
1. Wait for any key event
2. **Check for control keys:**
   - If `key == 's'` → Skip to next step
   - If `key == 'g'` → Skip all remaining keys in current group, advance to next group
   - If `key == 'a'` → Exit wizard, restore original state
   - If `key == 'y'`, `'n'`, or `'r'` → Block with message: "Reserved key, please press the target key or [s] to skip"
3. **Capture:** Any other key is captured and mapped to the current action
4. **Auto-advance** to the next step

### Phase 2: Verification Mode

After capturing all keys, users verify their mappings work:

```
[ VERIFICATION MODE ]
--------------------------------------------------
We captured your keys. Let's test them.

== Group 1: Basic Editing ==
 1. [ BACKSPACE ]:       [   ] Waiting...
 2. [ DELETE ]:          [ OK ] Detected!
 3. [ TAB ]:             [SKIP] (using default)
 4. [ SHIFT+TAB ]:       [SKIP] (using default)

== Group 2: Line Navigation ==
 5. [ HOME ]:            [ OK ] Detected!
 6. [ END ]:             [ OK ] Detected!
 7. [ SHIFT+HOME ]:      [   ] Waiting...
 8. [ SHIFT+END ]:       [   ] Waiting...

== Group 3: Word Navigation ==
 9. [ ALT+LEFT ]:        [ OK ] Detected!
10. [ ALT+RIGHT ]:       [ OK ] Detected!
    ... (8 more keys)

== Group 4: Document Navigation == (group skipped)

== Group 5: Emacs-Style == (group skipped)

--------------------------------------------------
COMMANDS:
 [ y ] SAVE these settings and exit
 [ r ] RETRY (restart wizard from beginning)
 [ a ] ABORT (discard all changes)
```

**Logic:**
- User can press each mapped key to verify it works
- UI updates each line to show `[ OK ]` when the correct input is detected
- User presses `y` to save and exit

## Integration Points

### 1. CLI Parameter

```bash
fresh --calibrate
```

Launches the editor directly into the calibration wizard. This is useful for first-time setup or when the terminal is so broken that the user can't navigate to the menu.

### 2. Command Palette

- **Command Name**: "Calibrate Input Keys"
- **Description**: "Open the input calibration wizard for problematic terminals"
- **Action**: `Action::CalibrateInput`
- **Contexts**: Available globally (empty contexts)

### 3. Menu Item

Add to the **View** menu (near Settings):

```
View
├── ...
├── Settings
├── Calibrate Input Keys...
├── ───────────
└── ...
```

## State Machine

```rust
use crossterm::event::{KeyCode, KeyModifiers, KeyEvent};

pub enum CalibrationStep {
    /// Capturing key for a specific target
    Capture {
        group_idx: usize,  // Index into CALIBRATION_GROUPS
        key_idx: usize,    // Index into group's key list
    },
    /// Verification phase
    Verify,
}

/// What the user's key SHOULD produce (the expected/normalized key)
pub struct ExpectedKey {
    pub code: KeyCode,
    pub modifiers: KeyModifiers,
}

pub struct CalibrationTarget {
    pub name: &'static str,
    pub expected: ExpectedKey,  // What Fresh expects to receive
}

pub struct CalibrationGroup {
    pub name: &'static str,
    pub targets: &'static [CalibrationTarget],
}

// 5 groups, 24 keys total
pub const CALIBRATION_GROUPS: &[CalibrationGroup] = &[
    // Group 1: Basic Editing (4 keys)
    CalibrationGroup {
        name: "Basic Editing",
        targets: &[
            CalibrationTarget { name: "BACKSPACE", expected: ExpectedKey { code: KeyCode::Backspace, modifiers: KeyModifiers::NONE } },
            CalibrationTarget { name: "DELETE", expected: ExpectedKey { code: KeyCode::Delete, modifiers: KeyModifiers::NONE } },
            CalibrationTarget { name: "TAB", expected: ExpectedKey { code: KeyCode::Tab, modifiers: KeyModifiers::NONE } },
            CalibrationTarget { name: "SHIFT+TAB", expected: ExpectedKey { code: KeyCode::BackTab, modifiers: KeyModifiers::SHIFT } },
        ],
    },
    // Group 2: Line Navigation (4 keys)
    CalibrationGroup {
        name: "Line Navigation",
        targets: &[
            CalibrationTarget { name: "HOME", expected: ExpectedKey { code: KeyCode::Home, modifiers: KeyModifiers::NONE } },
            CalibrationTarget { name: "END", expected: ExpectedKey { code: KeyCode::End, modifiers: KeyModifiers::NONE } },
            CalibrationTarget { name: "SHIFT+HOME", expected: ExpectedKey { code: KeyCode::Home, modifiers: KeyModifiers::SHIFT } },
            CalibrationTarget { name: "SHIFT+END", expected: ExpectedKey { code: KeyCode::End, modifiers: KeyModifiers::SHIFT } },
        ],
    },
    // Group 3: Word Navigation (8 keys)
    CalibrationGroup {
        name: "Word Navigation",
        targets: &[
            CalibrationTarget { name: "ALT+LEFT", expected: ExpectedKey { code: KeyCode::Left, modifiers: KeyModifiers::ALT } },
            CalibrationTarget { name: "ALT+RIGHT", expected: ExpectedKey { code: KeyCode::Right, modifiers: KeyModifiers::ALT } },
            CalibrationTarget { name: "ALT+SHIFT+LEFT", expected: ExpectedKey { code: KeyCode::Left, modifiers: KeyModifiers::ALT | KeyModifiers::SHIFT } },
            CalibrationTarget { name: "ALT+SHIFT+RIGHT", expected: ExpectedKey { code: KeyCode::Right, modifiers: KeyModifiers::ALT | KeyModifiers::SHIFT } },
            CalibrationTarget { name: "CTRL+LEFT", expected: ExpectedKey { code: KeyCode::Left, modifiers: KeyModifiers::CONTROL } },
            CalibrationTarget { name: "CTRL+RIGHT", expected: ExpectedKey { code: KeyCode::Right, modifiers: KeyModifiers::CONTROL } },
            CalibrationTarget { name: "CTRL+SHIFT+LEFT", expected: ExpectedKey { code: KeyCode::Left, modifiers: KeyModifiers::CONTROL | KeyModifiers::SHIFT } },
            CalibrationTarget { name: "CTRL+SHIFT+RIGHT", expected: ExpectedKey { code: KeyCode::Right, modifiers: KeyModifiers::CONTROL | KeyModifiers::SHIFT } },
        ],
    },
    // Group 4: Document Navigation (4 keys)
    CalibrationGroup {
        name: "Document Navigation",
        targets: &[
            CalibrationTarget { name: "PAGE UP", expected: ExpectedKey { code: KeyCode::PageUp, modifiers: KeyModifiers::NONE } },
            CalibrationTarget { name: "PAGE DOWN", expected: ExpectedKey { code: KeyCode::PageDown, modifiers: KeyModifiers::NONE } },
            CalibrationTarget { name: "CTRL+HOME", expected: ExpectedKey { code: KeyCode::Home, modifiers: KeyModifiers::CONTROL } },
            CalibrationTarget { name: "CTRL+END", expected: ExpectedKey { code: KeyCode::End, modifiers: KeyModifiers::CONTROL } },
        ],
    },
    // Group 5: Emacs-Style Navigation (4 keys)
    CalibrationGroup {
        name: "Emacs-Style",
        targets: &[
            CalibrationTarget { name: "CTRL+A", expected: ExpectedKey { code: KeyCode::Char('a'), modifiers: KeyModifiers::CONTROL } },
            CalibrationTarget { name: "CTRL+E", expected: ExpectedKey { code: KeyCode::Char('e'), modifiers: KeyModifiers::CONTROL } },
            CalibrationTarget { name: "CTRL+K", expected: ExpectedKey { code: KeyCode::Char('k'), modifiers: KeyModifiers::CONTROL } },
            CalibrationTarget { name: "CTRL+Y", expected: ExpectedKey { code: KeyCode::Char('y'), modifiers: KeyModifiers::CONTROL } },
        ],
    },
];

pub struct CalibrationWizard {
    /// Current step
    step: CalibrationStep,
    /// Translation table: raw terminal event → expected normalized event
    /// Key: what the terminal actually sends
    /// Value: what Fresh expects (the "correct" key)
    pending_translations: HashMap<KeyEvent, KeyEvent>,
    /// Which keys have been verified in the verification phase
    verified: HashSet<usize>,  // Index into flattened key list
    /// Groups that were skipped entirely
    skipped_groups: HashSet<usize>,
}

/// The key translator that applies calibration
pub struct KeyTranslator {
    /// Translation table loaded from config
    translations: HashMap<KeyEvent, KeyEvent>,
}

impl KeyTranslator {
    /// Translate a raw terminal event to a normalized event
    pub fn translate(&self, raw: KeyEvent) -> KeyEvent {
        self.translations.get(&raw).cloned().unwrap_or(raw)
    }
}
```

## Configuration Storage

When the user presses `y` to save, mappings are written to `~/.config/fresh/key_calibration.json`:

```json
{
  "_comment": "Generated by 'Calibrate Input Keys' wizard",
  "_format": "raw_key → expected_key",
  "translations": {
    "Char(0x7f)": "Backspace",
    "Char(0x08)": "Backspace",
    "Alt(b)": "Alt+Left",
    "Esc,[,1,~": "Home"
  }
}
```

Keys not listed use default behavior (no translation).

### Processing Order

The translation layer runs **before** keybinding resolution:

```
1. Terminal sends raw KeyEvent
2. KeyTranslator.translate(raw) → normalized KeyEvent  ← CALIBRATION
3. KeybindingResolver.resolve(normalized) → Action     ← KEYMAP
4. Editor executes Action
```

This means:
- Calibration is loaded once at startup (or reloaded after wizard saves)
- Keybindings still work normally on the normalized keys
- User can customize keybindings independently of calibration

## File Structure

```
src/
├── input/
│   ├── keybindings.rs           # Add CalibrateInput action
│   ├── key_translator.rs        # NEW: KeyTranslator + load/save calibration
│   └── mod.rs                   # Wire translator into input pipeline
├── app/
│   ├── calibration_wizard.rs    # NEW: Wizard state machine and logic
│   └── mod.rs                   # Integration with Editor
└── view/
    └── ui/
        └── calibration_ui.rs    # NEW: Wizard UI rendering
```

## Implementation Plan

### Phase 1: Core Infrastructure
1. Add `Action::CalibrateInput` to keybindings.rs
2. Add command to commands.rs
3. Add CLI parameter `--calibrate` to main.rs
4. Add menu item to View menu
5. Add i18n strings

### Phase 2: Translation Layer
6. Create `KeyTranslator` struct with `translate()` method
7. Implement loading from `key_calibration.json`
8. Wire translator into main event loop (before keymap resolution)

### Phase 3: Wizard Logic
9. Create `CalibrationWizard` state machine
10. Implement capture phase input handling
11. Implement verification phase input handling
12. Integrate with Editor (modal state)

### Phase 4: Persistence & UI
13. Implement key_calibration.json writing
14. Implement reload after wizard saves
15. Implement capture phase UI rendering
16. Implement verification phase UI rendering
17. Add status messages and error handling

## Security Considerations

- The wizard only remaps navigation/editing keys, not commands that could execute actions
- No file system access beyond the config directory
- No network access
- Abort (`a`) always works and discards all changes

## Accessibility

- All controls are single lowercase letters
- No timing requirements (wizard waits indefinitely)
- Clear text prompts with no color-dependent information
- Screen reader compatible (plain text UI)

## Testing Strategy

1. **Unit tests**: State machine transitions, key capture logic
2. **Integration tests**: Full wizard flow with mock input
3. **E2E tests**: CLI parameter, menu activation, save/load cycle

## Open Questions

1. Should we detect when a user's terminal seems "hostile" and suggest the wizard automatically?
2. Should the wizard also calibrate Shift+Arrow keys for selection?
3. Should there be a "reset to defaults" option in the wizard?

## References

- [Terminal Input Handling](https://invisible-island.net/xterm/ctlseqs/ctlseqs.html)
- [ANSI Escape Codes](https://en.wikipedia.org/wiki/ANSI_escape_code)
- [Crossterm Key Events](https://docs.rs/crossterm/latest/crossterm/event/enum.KeyCode.html)
