# Orchestrator dock: a visible startup policy (View ▸ Orchestrator Dock ▸)

Status: **draft spec — not implemented.**

## Problem

Whether the dock opens at startup is decided by `autoOpenDock`
(`auto` / `always` / `never`) and, under `auto`, by what `<data>/chrome.json`
remembers from the last quit (`Editor::apply_startup_dock_chrome`,
`app/chrome/dock.rs`).

Under `never` (and `always`) the remembered state is ignored, so opening or
closing the dock does not survive a restart. Nothing in the UI says so.
The View row `☑ Orchestrator Dock` looks like it controls the dock; it only
controls it *for this session*.

This is reachable without anyone choosing `never`: before #3442/#3465 a bare
`fresh` ignored `autoOpenDock: false`, so a `false` could sit in a config
unnoticed. #3465 reads it as `never` and rewrites it to `"never"` on disk.
The user then sees: dock missing at every launch; View ▸ Orchestrator Dock
opens it; quit; relaunch; missing again.

## Model

The dock has two independent pieces of state, and every control belongs to
exactly one of them:

| State | Meaning | Changed by |
|---|---|---|
| **Shown** | Is the dock on screen right now | The `×` in the dock header, `Orchestrator: Toggle Dock`, `Show Dock` in the menu, Alt+O when the dock is hidden |
| **Startup policy** | What the next start does | The new `On Startup` options in the menu, the Settings UI (`autoOpenDock`) |

"Startup" means the editor process starting. With Orchestrator mode that is
the daemon: **Quit and Stop All** followed by `fresh`. A Detach and reattach
is not a startup and applies nothing.

Policies:

| Policy (`autoOpenDock`) | Menu label | Next startup |
|---|---|---|
| `auto` (default) | Restore Last State | Whatever **Shown** was at quit (`chrome.json`). First launch ever: open in Orchestrator mode, else the manifest's `open`. Unchanged from today. |
| `always` | Always Open | Open |
| `never` | Never Open | Closed |

## Menu

```
View ▸ Orchestrator Dock ▸
        ☑ Show Dock
        ─────────────────────
        On Startup                    (label row, not selectable)
        ☑ Restore Last State
        ☐ Always Open
        ☐ Never Open
```

- Replaces the current one-row `☑ Orchestrator Dock`, in the same position
  (after File Explorer).
- `Show Dock` is today's row: the same action (`orchestrator_dock_toggle`)
  and checkmark (`dock`, "a dock is mounted").
- The three options render with the same check glyphs as View ▸ Keybinding
  Style: exactly one is checked. No new radio glyph.
- Choosing an option writes the setting and **does not show or hide the dock
  now**.
- Settings UI is unchanged: it edits the same key, so the menu and Settings
  always agree.

## Behaviour of × and the toggle

Neither ever writes the policy. The result at the next startup:

| Policy | Dock hidden (× / toggle), then quit | Dock shown (toggle / menu), then quit |
|---|---|---|
| Restore Last State | Starts hidden | Starts shown |
| Always Open | **Starts shown** | Starts shown |
| Never Open | Starts hidden | **Starts hidden** |

The bold cells are where the action does not carry over. In exactly those two
cases a status-bar message says so at the moment of the action:

- Hidden under Always Open:
  `Dock hidden until restart. It opens on startup (View ▸ Orchestrator Dock ▸ On Startup).`
- Shown under Never Open:
  `Dock shown until restart. It stays closed on startup (View ▸ Orchestrator Dock ▸ On Startup).`

Under Restore Last State there is no message: what you see is what you get.

The message is raised only on the user-initiated routes in the **Model**
table. The plugin's own closes and reopens (a worktree dive, a new or
recovered placeholder via `showDockUnfocused`) say nothing.

## Implementation

### Host (`fresh-editor`)

1. **Remember the claimant.** `apply_startup_dock_chrome` already resolves
   the dock-declaring plugin and its `open_setting`. Store both on `Editor`
   (e.g. `dock_open_setting: Option<(plugin, key)>`) instead of discarding
   them.
2. **Context keys.** In `update_menu_context` set `dock_startup_auto`,
   `dock_startup_always` and `dock_startup_never` from the *current* config
   through `DockOpenPolicy::read`. Legacy booleans therefore show correctly:
   `false` shows Never Open and `true` shows Restore Last State. Add the names
   to `context_keys`.
3. **Action.** Add `set_dock_startup_policy` with args `{ "mode": "auto" |
   "always" | "never" }`. It updates `config.plugins.<plugin>.settings.<key>`
   in memory and calls `persist_config_pointer`, which writes to whichever
   layer already defines the key (user by default, project if the project
   set it), then fires `config_changed`. This is the same write as
   `rewrite_legacy_dock_open_setting`, which should share it. With no dock
   claimant it is a no-op.

   This lives in the host because the dock column and its startup decision
   are the host's (`chrome/dock.rs`). It is generic over whichever plugin
   declares the dock.

### Plugin API (`fresh-core`, `fresh-plugin-runtime`)

4. `editor.addMenuItem` can only add one flat row with no args today
   (`AddMenuItemOptions`). Extend it with:
   - `items?: MenuRow[]`: when present, the contribution is a
     `MenuItem::Submenu` labelled `label`, and `action` / `checkbox` are not
     allowed.
   - `MenuRow`: `{ label, action?, args?, checkbox?, when? }`, or
     `{ separator: true }`, or `{ info }` for a label row.

   Regenerate `fresh.d.ts` and the API docs (CONTRIBUTING §6).

### Orchestrator plugin

5. Replace the `addMenuItem` call (`orchestrator.ts`, "View ▸ Orchestrator
   Dock") with the submenu above: `Show Dock` → `orchestrator_dock_toggle` /
   `dock`, and each option → `set_dock_startup_policy {mode}` /
   `dock_startup_<mode>`.
6. Status messages: in `toggleDock` and the `dock-close` activate handler,
   after the show or hide, read `autoOpenDock` (normalised the same way as
   the host: `false` → never, `true` / unknown → auto) and raise the matching
   message.
7. Strings (`menu.dock_show`, `menu.dock_on_startup`,
   `menu.dock_startup_auto|always|never`, `status.dock_hidden_until_restart`,
   `status.dock_shown_until_restart`) go in `orchestrator.i18n.json` for
   every locale, with real translations (CONTRIBUTING §10).

### Unchanged

- `chrome.json` and the quit-time `save_dock_chrome`.
- The legacy rewrite (`false` → `"never"`, `true` → `"auto"`). The menu now
  shows its result, which was the missing piece. See Q3.
- The Settings UI entry and its description text. (Optional: mention the menu
  in the description.)

## Tests

Per CONTRIBUTING §1–2: each behaviour has an e2e test that drives keys or
mouse and asserts only on the rendered screen; each fails without the change.

E2E (extend `tests/e2e/orchestrator_dock_startup.rs`; launches share a
`DirectoryContext` as `the_dock_is_remembered_across_launches` does):

- **The reported bug.** Config `autoOpenDock: false`. View ▸ Orchestrator
  Dock ▸ shows `☑ Never Open`. Show Dock opens it and the "until restart"
  message is on screen. Shut down, relaunch: no dock.
- **Choosing an option sticks.** Choose Always Open. The dock is not changed
  now. Hide with `×`: message on screen. Relaunch: dock open, and the menu
  shows `☑ Always Open`.
- **Restore is silent.** Under Restore Last State, hiding and showing raise
  no message, and the next launch matches what was left.
- **Settings and menu agree.** Change `autoOpenDock` in the Settings UI, then
  open the submenu: the check moved.

Unit:

- Context keys from config: each string, each legacy boolean, absent →
  auto, no dock claimant → all false.
- `set_dock_startup_policy` writes the pointer to the defining layer, and is
  a no-op without a claimant.
- `addMenuItem` with `items` produces a `Submenu` with args carried through,
  and rejects `items` combined with `action`.

Existing tests that open the View row by its old label
(`tests/e2e/orchestrator_dock.rs`, `tests/e2e/blog_showcases.rs`) move to the
submenu path.

## Open questions

- **Q1. Layout.** A submenu makes show/hide two clicks instead of one; Alt+O
  and the palette stay one step. Alternative: keep the one-click
  `☑ Orchestrator Dock` row and add a sibling `Orchestrator Dock on
  Startup ▸` submenu below it. The behaviour above is identical either way.
- **Q2. Should choosing an option also apply now?** For example, Never Open
  also hides the dock and Always Open also shows it. This spec says no, to
  keep the two states independent. Applying it would feel more direct, but
  then "Never Open" and "hide" look the same until a restart.
- **Q3. Legacy `false`.** It keeps meaning `never`. Alternatively, map it to
  `auto` for users who are in Orchestrator mode, where `false` never had an
  effect before #3465. Not proposed: it is a one-time migration, and the menu
  now makes the value visible and one click to change.
- **Q4. Message frequency.** Every time, or once per session? Status messages
  are transient, so the spec says every time.
