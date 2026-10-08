# Orchestrator dock: a start follows the user's last instruction

**At startup the dock follows your last explicit instruction (showing or
hiding it, or changing the Settings value), which overwrites the
`autoOpenDock` setting each time.**

## Problem

Whether the dock opens at startup is `autoOpenDock` (`auto` / `always` /
`never`), read by the host before the first frame
(`Editor::apply_startup_dock_chrome`, `app/chrome/dock.rs`). Under `auto` the
host restores what `<data>/chrome.json` remembered at quit. `always` and
`never` ignore that.

So under `never` an open from `View ▸ Orchestrator Dock` lasted only until the
restart, and under `always` a close did the same, with nothing on screen
explaining why. It is reachable without anyone choosing `never`: a bare
`fresh` used to ignore `autoOpenDock: false`, so a `false` could sit in a
config unnoticed. #3442 reads it as `never` and rewrites it on disk.

## Rule

Explicit instructions are:

- **Show:** View ▸ Orchestrator Dock, `Orchestrator: Toggle Dock`, or
  `toggle_dock_focus` (Alt+O) on a hidden dock.
- **Hide:** the dock header's `×`, View ▸ Orchestrator Dock, or
  `Orchestrator: Toggle Dock`.
- **Settings:** an edit to `autoOpenDock`.

A show writes `autoOpenDock: "always"` and a hide writes `"never"`. A Settings
edit to `always` / `never` also opens or closes the dock now; `auto` changes
nothing now. The most recent instruction wins.

These are not instructions and write nothing: the plugin's own opens and
closes (attaching to a discovered worktree, a new or recovered workspace via
`showDockUnfocused`), Detach and reattach, and quitting.

`auto` therefore means "no instruction yet" (a fresh install, or a Settings
reset). Under it the host behaves as before: what `chrome.json` remembered,
else on a first launch the launch mode, then the manifest's `open`.

## Matrix

| # | Setting at start | Dock at start | Last explicit instruction this session | Setting after | Next start |
|---|---|---|---|---|---|
| 1 | `auto` | Remembered; first run: open in Orchestrator mode | None | `auto` | As left at quit (`chrome.json`) |
| 2 | `auto` | (as above) | Show | `always` | Open |
| 3 | `auto` | (as above) | Hide | `never` | Closed |
| 4 | `always` | Open | None (only plugin closes, or Detach) | `always` | Open |
| 5 | `always` | Open | Hide (`×`) | `never` | Closed |
| 6 | `always` | Open | Hide, then Show | `always` | Open |
| 7 | `never` | Closed | None (the plugin may still show it for a new workspace) | `never` | Closed |
| 8 | `never` (or a legacy `false`) | Closed | Show (View menu) | `always` | Open |
| 9 | `never` | Closed | Show, then Hide | `never` | Closed |
| 10 | Any | Any | Settings: `always` | `always` | Open; the dock opens now too |
| 11 | Any | Any | Settings: `never` | `never` | Closed; the dock closes now too |
| 12 | Any | Any | Settings: `auto` | `auto` | As left at quit; nothing changes now |
| 13 | Any | Any | Show in one client, Hide in another (same daemon) | `never` | Closed: one editor, the later instruction wins |
| 14 | Any | Any | Detach, then `fresh` again | Unchanged | Not a start: the dock is as it was left |

## Implementation

All in the orchestrator plugin (`plugins/orchestrator.ts`); the host is
unchanged and still only reads the setting at startup.

- `rememberDockOpen(open)` writes `always` / `never` with `editor.saveSetting`.
  It is called from `toggleDock` (the command behind the View row, the palette
  and `toggle_dock_focus` on a hidden dock) and the `dock-close` activate
  handler, and from nowhere else.
- Writes are deduped against `lastDockMode`, the plugin's own last-written or
  adopted value, never the config snapshot. `saveSetting` only queues the
  write, so the snapshot reads the old value for a tick or two, and a second
  toggle inside that window would otherwise skip its write.
- `config_changed` compares `autoOpenDock` (normalised as the host reads it:
  `false` → never, `true` / unknown → auto) with `lastDockMode`. On a change
  to `always` it shows the dock unfocused; to `never` it closes it. Our own
  write arrives already matching and is a no-op. A modal picker on screen
  defers the change and leaves `lastDockMode` behind, so the next save
  reconsiders.

`chrome.json`'s `open` and the quit-time `save_dock_chrome` stay: they serve
row 1, including users upgrading with a dock state they left before this
change.

## Tests

- `orchestrator_dock_startup::an_open_from_the_view_menu_outlives_never`: the
  reported bug (row 8), across two launches that read the config from disk.
- `orchestrator_dock_startup::a_close_with_the_x_outlives_always`: row 5.
- `orchestrator_dock_settings::a_settings_edit_opens_and_closes_the_dock`:
  rows 10–11 through the Settings UI.
- The existing across-launch walks (`*_remembers_the_dock_across_launches`)
  cover rows 1–3 and 6/9.
