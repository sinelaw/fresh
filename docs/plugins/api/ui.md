<!-- Generated from the plugin API source. Do not edit: change the doc comments in the Rust source, then run `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`. -->

# Commands, Prompts & Dialogs

Add commands and key bindings, and ask the user for input with prompts, dialogs, popups and single keys. For a tour of the input options with screenshots, see [Asking for Input](../examples/asking-for-input/).

::: v-pre

## Commands

### `registerCommand`

Register a command in the command palette (Ctrl+P).

Usually you should omit `context` so the command is always visible.
If provided, the command is **hidden** unless your plugin has activated
that context with `editor.setContext(name, true)` or the focused buffer's
virtual mode (from `defineMode()`) matches. This is for plugin-defined
contexts only (e.g. `"tour-active"`, `"review-mode"`), not built-in
editor modes.

```typescript
registerCommand(name: string, description: string, handlerName: string, context?: string | null, options?: {
  terminalBypass?: boolean;
} | null): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `name` | Display name shown in the command palette |
| `description` | Description shown alongside the command |
| `handlerName` | Name of the `globalThis` function to call |

### `unregisterCommand`

Unregister a command by name

```typescript
unregisterCommand(name: string): boolean;
```

### `setContext`

Set a context (for keybinding conditions)
Custom contexts also control command visibility: a command registered
with a context is shown only while that context is active. For example,
setting "config-editor" makes the config editor commands visible.
Contexts a plugin sets are cleared when the plugin unloads.

```typescript
setContext(name: string, active: boolean): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `name` | Context name (e.g. "config-editor") |
| `active` | Whether the context is active (true = set, false = unset) |

### `executeAction`

Execute a built-in action
The action is given by name, e.g. "move_word_right" or "move_line_end".
The vi mode plugin uses this to run motions. The action is queued and
runs after the call returns; the result says whether it was queued.
An unknown action name is only logged as a warning.

```typescript
executeAction(actionName: string): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `actionName` | Action name (e.g. "move_word_right", "move_line_end") |

### `completeCommand`

Answer a command that was dispatched with a request id (a `RunCommand`
from the agent command channel).

Plugins do not normally call this: the host wraps every such dispatch so
that whatever the handler *returns* — or the promise it returns, once it
resolves — becomes the answer, and a throw becomes the failure. Call it
directly only to answer early, or to answer from somewhere other than
the handler's own return path. `output` is the JSON-encoded result the
caller prints; answering an unknown or already-answered id is a no-op.

```typescript
completeCommand(requestId: number, ok: boolean, output: string | null, error: string | null): boolean;
```

### `executeActions`

Execute multiple actions in sequence

Takes typed ActionSpec array - serde validates field names at runtime

Each action has an optional repeat count. Vi mode uses this for count
prefixes (e.g., "3dw" deletes 3 words). All actions run in one batch,
with no plugin round trips between them. Execution stops at the first
unknown or failing action.

```typescript
executeActions(actions: ActionSpec[]): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `actions` | Array of `{action: string, count?: number}` objects |

### `runCommand`

Run a registered command by the exact name it shows in the command
palette — the same dispatch the palette performs on that row, so a
command handler is exercised through its real path rather than by
calling the plugin function directly.

Resolves `true` when the command was found and dispatched; rejects
when no command carries that name (so a typo is an error, not a
silent no-op). The command's *own* async work is not awaited — this
resolves once dispatch happened, exactly like a keypress would.

`fresh --cmd command run "<name>"` is this call from a shell.

```typescript
runCommand(name: string): Promise<boolean>;
```

### `listCommands`

Every registered command — built-ins and plugin commands together —
as `{ name, description, source, plugin }`, where `source` is
`"builtin"` or `"plugin"` and `plugin` names the owner (empty for
built-ins).

The point of this from a plugin's own script: confirming that your
`registerCommand` actually landed, under the name you expect, before
hunting for why the palette "doesn't show it".

```typescript
listCommands(): Promise<Array<{
  name: string;
  description: string;
  source: string;
  plugin: string;
}>>;
```

## Modes & Keybindings

### `getKeybindingLabel`

Get the display label for a keybinding by action name and optional mode.
Returns null if no binding is found.

```typescript
getKeybindingLabel(action: string, mode: string | null): string | null;
```

### `defineMode`

Define a buffer mode (takes bindings as array of [key, command] pairs)

On a widget panel whose keymap is this mode, **the focused control
handles a key first** — a field types and moves its caret, an open
list takes the arrows and Enter, a button takes Enter and Space, Esc
closes a pop-up before the dialog — and a binding gets only the keys
the control does not use. Bind commands ("submit", "close"), not the
controls' own keys.

A binding whose third element is `"shortcut"` —
`["C-Enter", "submit", "shortcut"]` — is a **dialog-wide shortcut**
instead: it runs ahead of any control, wherever focus is. Keep that
list short and made of chords no control uses.

A binding whose third element is `"on:a,b"` —
`["Up", "history_prev", "on:name,cmd"]` — belongs to the controls
named: it applies only while one of those widgets holds the panel's
focus, and on any other control the key is left to the panel's
defaults (↑/↓ move focus to the control above or below). Use it for a
command that is about one field, rather than binding the key for the
whole dialog and forwarding it back.

Example:

```ts
editor.defineMode("diagnostics-list", [
  ["Return", "diagnostics_goto"],
  ["q", "close_buffer"],
], true);
```

```typescript
defineMode(name: string, bindingsArr: string[][], readOnly?: boolean, allowTextInput?: boolean, inheritNormalBindings?: boolean): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `name` | Mode name (e.g., "diagnostics-list") |
| `bindingsArr` | Array of [key_string, command_name] pairs |
| `readOnly` | Whether buffers in this mode are read-only |

### `setEditorMode`

Set the active window's editor mode — a plugin mode scoped to that
window, such as `markdown-source`. It outranks the editor-wide input
mode in that window and is invisible in every other. `null` clears
it. A modal-editing personality that should apply everywhere (vi)
belongs in `setInputMode` instead.

```typescript
setEditorMode(mode: string | null): boolean;
```

### `setInputMode`

Set the editor-wide input mode — a modal-editing personality such as
vi (`"vi-normal"`, `"vi-insert"`, …). It applies in every window,
including windows created later, and nothing window-scoped clears it.
Keys resolve against a focused panel's mode, then the buffer's, then
the window's editor mode, then this, then the base keymap. `null`
turns it off.

A change fires the `input_mode_changed` hook. Setting the mode it
already has does nothing.

```typescript
setInputMode(mode: string | null): boolean;
```

### `getEditorMode`

Get the active window's editor mode (see `setEditorMode`)

```typescript
getEditorMode(): string | null;
```

### `getInputMode`

Get the editor-wide input mode (see `setInputMode`)

```typescript
getInputMode(): string | null;
```

## Macros

### `listMacros`

Register keys of all recorded macros in the active session, sorted.
Reads the per-tick snapshot, so it never crosses the IPC boundary.

```typescript
listMacros(): string[];
```

### `getMacro`

The recorded steps of the macro under `register` as `ActionSpec[]`, or
`null` if no macro is stored there. The returned array is the exact
shape `editor.executeActions` accepts, so a macro round-trips into a
replay script with no translation — this equivalence is the core of the
macro&lt;->code bridge.

```typescript
getMacro(register: string): ActionSpec[] | null;
```

### `defineMacro`

Define (or replace) the macro under `register` from a step list. Lets
`init.ts` seed registers at startup so a saved macro plays back exactly
like a hand-recorded one. Returns true if the command was queued.

```typescript
defineMacro(register: string, steps: ActionSpec[]): boolean;
```

### `playMacro`

Play the macro stored under `register` (same effect as the built-in
"play macro" action). Returns true if the command was queued.

```typescript
playMacro(register: string): boolean;
```

## Prompts

### `cancelPrompt`

Cancel the active prompt / overlay — the same teardown the
Escape key triggers. Lets a plugin dismiss a prompt it opened
(e.g. exporting Live Grep results to a dock panel) without
routing a synthetic keypress.

```typescript
cancelPrompt(): boolean;
```

### `prompt`

Show a prompt and wait for user input (async)
Returns the user input or null if cancelled

```typescript
prompt(label: string, initialValue: string): Promise<string | null>;
```

| Parameter | Description |
|-----------|-------------|
| `label` | Text shown before the input |
| `initialValue` | Text already in the input when it opens |

### `pickFile`

Open the editor's native Open File browser and wait for a pick
(async) — the terminal analogue of a browser's file-input dialog.
Resolves with the chosen file's absolute path, or null if the
user cancels. The browser anchors where Open File does (the
active file's directory, else the window's working directory),
with the same navigation: Backspace walks up the tree, Tab
descends into directories, and typed input filters or resolves
as a path. No buffer is opened — the path is only returned.

`directory` anchors the browser somewhere else (a relative path
resolves against the window's working directory) and typed
relative input then resolves there too. `showHidden` overrides
the config's dotfile visibility for this pick — pass `true` when
the file being picked is itself a dotfile (a tour manifest, an
editorconfig), which the default would hide.

```typescript
pickFile(label: string, directory?: string | null, showHidden?: boolean | null): Promise<string | null>;
```

| Parameter | Description |
|-----------|-------------|
| `label` | Text shown before the input |

### `startPrompt`

Start an interactive prompt.

When `floatingOverlay` is true, the editor renders the prompt
and its suggestions inside a centred floating frame instead of
the bottom minibuffer row (issue #1796 — Live Grep). The flag
is rendering-only; confirm/cancel/hooks behave identically to a
non-overlay prompt of the same `promptType`.

```typescript
startPrompt(label: string, promptType: string, floatingOverlay?: boolean): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `label` | Label to display (e.g., "Git grep: ") |
| `promptType` | Type identifier (e.g., "git-grep") |

### `startPromptWithInitial`

Start a prompt with initial value. See `startPrompt` for the
meaning of `floatingOverlay`.

```typescript
startPromptWithInitial(label: string, promptType: string, initialValue: string, floatingOverlay?: boolean): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `label` | Label to display (e.g., "Git grep: ") |
| `promptType` | Type identifier (e.g., "git-grep") |
| `initialValue` | Initial text to pre-fill in the prompt |

### `setPromptSuggestions`

Set suggestions for the current prompt

Uses typed Vec&lt;Suggestion> - serde validates field names at runtime

Every suggestion's `id` must be unique in the array; a list that repeats one
throws instead of being shown.

```typescript
setPromptSuggestions(suggestions: PromptSuggestion[], selectedIndex?: number | null): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `suggestions` | Array of suggestions to display |

### `setPromptInputSync`

```typescript
setPromptInputSync(sync: boolean): boolean;
```

### `setPromptTitle`

Set the title shown in the floating-overlay prompt's frame
header (issue #1796) as styled segments. Each segment
carries optional `Partial<OverlayOptions>`, the same
styling primitive used by virtual text — plugins mark
keybinding hints with `{ fg: "ui.help_key_fg" }`,
separators with `{ fg: "ui.popup_border_fg" }`, etc. Pass
an empty array to clear the title and fall back to the
prompt-type default. Has no visible effect on non-overlay
prompts.

```typescript
setPromptTitle(title: StyledText[]): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `title` | Styled segments rendered along the overlay's top toolbar row |

### `setPromptFooter`

Set the footer chrome row of the floating-overlay prompt's
results pane. Plugins use this for hotkey-hint banners
(Orchestrator's `[n] new   [d] dive   [Esc] close` row).
Empty array clears the footer. Has no visible effect on
non-overlay prompts.

```typescript
setPromptFooter(footer: StyledText[]): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `footer` | Styled segments rendered along the overlay's bottom row |

### `setPromptFullscreen`

Centre the floating-overlay prompt's card on the whole frame — 90%
of it, over the dock and sidebar, as the Settings dialog is — instead
of on the chrome area beside the dock. `false` puts it back. Has no
visible effect on non-overlay prompts.

Live Grep uses it so its results, preview and toolbar get the room.

```typescript
setPromptFullscreen(fullscreen: boolean): boolean;
```

### `setPromptStatus`

Set the floating-overlay prompt's input-row status text (right-aligned,
left of the match count). Empty string clears it.

```typescript
setPromptStatus(status: string): boolean;
```

### `setPromptToolbar`

Set the floating-overlay prompt's toolbar as a `WidgetSpec` (real,
clickable `Toggle`/`Button` widgets rendered in the header band, in
place of the styled-text title). Pass `null`/`undefined` to clear it.

```typescript
setPromptToolbar(specObj: unknown): boolean;
```

### `toggleOverlayToolbarWidget`

Toggle a floating-overlay toolbar control by its widget `key`. The host
owns the toggle's checked state, flips it, and emits a `widget_event`
the plugin can listen for. Lets a plugin route its own Alt+… shortcut
through the same host path as a click / Space on the toggle.

```typescript
toggleOverlayToolbarWidget(key: string): boolean;
```

### `setPromptSelectedIndex`

Override the currently-highlighted suggestion row in the
open prompt. The editor clamps `index` to the suggestion
list's bounds and the renderer scrolls it into view on
the next frame. No-op when no prompt is open or the
suggestion list is empty. Typical use: re-opening a
picker and pre-selecting the entry the user last acted on
(Orchestrator highlights the active session).

```typescript
setPromptSelectedIndex(index: number): boolean;
```

## Reading Keys

### `beginKeyCapture`

Begin a key-capture window for the calling plugin.

Pair with `endKeyCapture()` around any `getNextKey()` loop.
While capture is active, keys arriving between two
`getNextKey()` calls are buffered in-order rather than
falling through to the buffer / mode bindings, so fast typing,
pastes, or held-key auto-repeat are delivered losslessly.
Without this, a plugin's input loop has a race where keys
typed while the plugin is mid-redraw can leak into the editor.

```typescript
beginKeyCapture(): boolean;
```

### `endKeyCapture`

End the key-capture window and discard any unconsumed buffered
keys.  Call from a `finally` block so capture is released even
if the plugin's loop throws.

```typescript
endKeyCapture(): boolean;
```

### `getNextKey`

Wait for the next keypress and resolve with a `KeyEventPayload`.

While the returned promise is pending the editor consumes the
next key and resolves it; the key does not propagate to mode
bindings or other dispatch. Multiple in-flight requests across
plugins are FIFO. Designed for short input loops (flash labels,
vi find-char, replace-char) that would otherwise need to bind
every printable key in `defineMode`.

For lossless capture against fast typing or paste, wrap the
loop with `beginKeyCapture()` / `endKeyCapture()`.

`KeyEventPayload` has `key` (e.g. `"a"`, `"escape"`, `"f1"`) and the
modifier flags `ctrl`, `alt`, `shift` and `meta`.

```typescript
getNextKey(): Promise<KeyEventPayload>;
```

## Dialogs & Widgets

A dialog is a floating panel built from widgets: text fields, checkboxes,
dropdowns, buttons and labels. The editor draws it and handles typing, focus
and Tab. The plugin hears what the user does through the
[`widget_event`](./events#widget-event) hook. The bundled plugins build the
widget spec with the helpers in `plugins/lib/widgets.ts`.

### `getPanelFocusKey`

Which widget holds focus in one of this plugin's mounted panels —
its key, or `""` when nothing is focused or the panel is not mounted.

The host owns a panel's focus: Tab, a click, a control's own move and
`setFocusKey` all write the one fact this reads. Read it rather than
mirroring focus from `focus` events — every `widget_event` also carries
it, as `focus_key`.

```typescript
getPanelFocusKey(panelId: number): string;
```

### `scrollToWidget`

Scroll a widget-panel buffer so the widget with `key` sits at the
top of its split, with the cursor on it.

The panel already knows where it painted every keyed widget, so
a page navigating to its own content asks rather than derives.
Deriving means painting, reading the buffer text back, matching
your own captions as strings and converting line numbers to byte
offsets — which is what this replaces, and which broke twice in
the welcome screen before it did.

A widget spanning several rows (a card whose rows share one key)
anchors at its top. Unknown keys are a no-op.

Queued like every layout mutation: `await editor.flush()` before
reading back.

```typescript
scrollToWidget(bufferId: number, key: string, align?: ScrollAlign): boolean;
```

### `mountWidgetPanel`

Mount a declarative widget panel inside a virtual buffer.

`spec` is a `WidgetSpec` JSON tree (see fresh.d.ts for the
shape). The host renders the spec into the buffer; subsequent
`updateWidgetPanel` calls re-render the panel against the
previously-mounted spec.

Returns true on successful queue, false if the IPC channel is
closed.

```typescript
mountWidgetPanel(panelId: number, bufferId: number, specObj: unknown, optionsObj?: WidgetPanelOptions): boolean;
```

### `updateWidgetPanel`

Replace the spec of a previously-mounted widget panel.
No-op if the panel id was never mounted.

```typescript
updateWidgetPanel(panelId: number, specObj: unknown): boolean;
```

### `unmountWidgetPanel`

Unmount a previously-mounted widget panel. The plugin retains
ownership of the underlying virtual buffer.

```typescript
unmountWidgetPanel(panelId: number): boolean;
```

### `widgetCommand`

Route a keystroke / nav action to the panel's focused widget.

`action` is a `WidgetAction` JSON object — see fresh.d.ts for
the shapes (`{kind: "focusAdvance", delta: 1}` etc.). Plugin's
`defineMode` bindings dispatch into here for keys handled by
the widget layer; the host runtime acts on the panel's
currently focused widget and fires `widget_event` as
appropriate.

```typescript
widgetCommand(panelId: number, actionObj: unknown): boolean;
```

### `widgetMutate`

Apply a targeted mutation to a mounted widget panel — the
IPC fast path. Use instead of `updateWidgetPanel` when the
model change touches a single widget; the host applies the
mutation in place without re-transmitting the full spec.
See `WidgetMutation` in fresh.d.ts for the shapes.

```typescript
widgetMutate(panelId: number, mutationObj: unknown): boolean;
```

### `mountFloatingWidget`

Mount a declarative widget panel as a centered floating
overlay (not bound to any virtual buffer).

```typescript
mountFloatingWidget(panelId: number, specObj: unknown, widthPct: number, heightPct: number, asDock?: boolean, focusMarker?: boolean, title?: string, closable?: boolean, startBlurred?: boolean, mode?: string, labelAlign?: string): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `panelId` | Your id for this panel. Events for it carry the same id |
| `specObj` | The `WidgetSpec` widgets to show |
| `widthPct` | Width, as a percent of the screen (1-100) |
| `heightPct` | Height, as a percent of the screen (1-100) |
| `asDock` | Dock it at the side instead of centring it |
| `focusMarker` | Draw `▸` next to the focused control |
| `title` | Title in the frame |
| `closable` | Show a `[×]` that closes the dialog like Esc |
| `startBlurred` | Open without taking the keyboard |
| `mode` | A `defineMode` keymap for the dialog (e.g. Enter to submit) |
| `labelAlign` | `"left"` (default) or `"right"` alignment of field labels |

### `mountSidebarSection`

Mount a declarative widget panel as a **sidebar section**: a titled,
collapsible section of the file explorer's column, appended after
the explorer and any section already there. The sidebar is shown if
it was hidden.

`rows` is the section's requested body height in rows (`0` shares the
column with the explorer); a divider the user has dragged overrides
it. `opts.closable` (default `true`) puts a `×` on the header that
removes the section and fires the panel's `cancel` `widget_event`;
`opts.startBlurred` (default `false`) mounts without taking keyboard
focus.

The section is an ordinary panel: `updateFloatingWidget(panelId, spec)`
replaces its content, `unmountFloatingWidget(panelId)` removes the
section, `widgetMutate` / `widgetCommand` apply, and its hits arrive
through the `widget_event` hook with this `panelId` unchanged.
`floatingPanelControl(panelId, "sidebar_rows", n)` changes the
requested rows, `"focus"` / `"blur"` work as for the dock, and
`"dock"` / `"center"` re-anchor the panel out of the sidebar (with
`"sidebar"` bringing a dock or centered panel in). Mounting an id that
is already a section replaces its content in place.

```typescript
mountSidebarSection(panelId: number, specObj: unknown, title: string, rows: number, opts?: {
  closable?: boolean;
  startBlurred?: boolean;
  scope?: {
    buffer: number;
  } | {
    window: number;
  } | "editor";
}): boolean;
```

### `updateFloatingWidget`

Replace the spec of the currently-mounted floating widget panel.

What the user typed into keyed fields is kept.

```typescript
updateFloatingWidget(panelId: number, specObj: unknown): boolean;
```

### `unmountFloatingWidget`

Tear down the floating widget panel.

```typescript
unmountFloatingWidget(panelId: number): boolean;
```

### `floatingPanelControl`

Control a mounted floating panel's placement / focus without
re-sending its spec. `op`: "dock" (re-anchor as the left dock and
focus; `arg` unused — the width is the editor's), "dock_width"
(`arg` = width in columns; sticks like a drag, across resizes and
launches), "center", "focus", "blur", "fullscreen" (`arg != 0` makes
a centered panel cover the whole frame over the dock), "sidebar"
(`arg` = requested rows; re-anchors the panel as a sidebar section
under the file explorer — "dock" / "center" re-anchor it back out),
"sidebar_rows" (`arg` = requested rows for a section; a divider the
user has dragged wins). See `PluginCommand::FloatingPanelControl`.

```typescript
floatingPanelControl(panelId: number, op: string, arg: number): boolean;
```

## Popups & Menus

### `showActionPopup`

Show an action popup

Takes a typed ActionPopupOptions struct - serde validates field names at runtime

Each action's `id` is its row's key and must be unique among the
actions; a repeated one throws.

The popup shows buttons for user interaction. When the user selects an
action, the `action_popup_result` hook is fired.

```typescript
showActionPopup(opts: ActionPopupOptions): boolean;
```

| Parameter | Description |
|-----------|-------------|
| `opts` | Popup configuration with id, title, message, and actions |

### `addMenuItem`

Contribute a row to one of the menu bar's menus (e.g. a "Show Dock"
toggle under "View"). The target menu and the neighbour named by
`after` / `before` are matched by stable id (a menu `id`, an item's
`action`) as well as by display label, so the placement survives a
locale change. Naming a menu that doesn't exist is a no-op.

Takes a typed AddMenuItemOptions struct - serde validates field
names at runtime.

```typescript
addMenuItem(opts: AddMenuItemOptions): boolean;
```

:::
