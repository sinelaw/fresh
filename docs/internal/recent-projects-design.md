# Recent / Pinned Projects — design

Implements [#1895](https://github.com/sinelaw/fresh/issues/1895): a fast way to
re-open recently used project directories without re-typing or browsing to them,
plus a user-curated set of *pinned* projects that always show first.

## Goals

- **Recent Projects** — auto-populated, most-recently-used list of project roots
  (working directories). Selecting one switches to it.
- **Pinned Projects** — a project the user explicitly pins is kept at the top of
  the list regardless of recency, and is never evicted by the recency cap.
- Reachable from **both** the command palette and the **File** menu (the issue
  asks for "the File menu and/or the command palette").

## Why this shape

A project switch in Fresh is implemented as a *restart* of the editor against the
new working directory (`change_working_dir` → `request_restart`). That gives us a
single, reliable recording point: **every** time the real editor finishes
booting against a working directory — initial launch *or* a switch — we record
that directory as "opened". One hook covers initial open and all subsequent
switches; there is no need to instrument each switch path.

The picker reuses the existing suggestion-prompt infrastructure (the same
machinery behind "Select Theme", "Stop LSP Server", …): a `PromptType` with a
pre-built suggestion list whose `value` carries the absolute path. On confirm the
selected suggestion's value is fed straight into `change_working_dir`, so the
actual switch reuses the well-tested `SwitchProject` code path.

## Storage

A single JSON file under the platform data dir, alongside the existing
`workspaces/` store:

```
$XDG_DATA_HOME/fresh/recent_projects.json
```

```jsonc
{
  "version": 1,
  "projects": [
    { "path": "/home/user/fresh",      "pinned": true,  "last_opened": 1718000000 },
    { "path": "/home/user/other-repo", "pinned": false, "last_opened": 1717900000 }
  ]
}
```

- Persisted in the data dir (not `config.json`) so it is mutable runtime state and
  does not churn the config schema.
- Atomic write (temp file + `sync_all` + rename), mirroring `workspace.rs`.
- Recency cap (`MAX_RECENT = 20`) applies only to **unpinned** entries; pinned
  entries are always retained.
- Paths are canonicalized and de-duplicated on insert.
- Corrupt / unreadable / version-too-new files degrade gracefully to an empty
  list — recent projects is a convenience, never load-bearing.

## Ordering shown in the picker / menu

1. Pinned entries first (marked with a `★`), each by recency.
2. Then unpinned recent entries by recency (most recent first).

The current project is skipped in the list (you are already in it).

## User-facing surface

- **Command palette**: `Open Recent Project…` and `Pin / Unpin Current Project`.
- **File menu**: an `Open Recent Project…` entry next to `Switch Project`.
- Pinning is a toggle on the *current* project (simple, discoverable, no extra UI
  surface). The picker shows which projects are pinned via the `★` marker.

## Touch points

| Area | Change |
|------|--------|
| `services/recent_projects.rs` | New store: types, atomic load/save, MRU/pin ops, unit tests |
| `input/keybindings.rs` | `Action::OpenRecentProject`, `Action::TogglePinProject` + name/parse maps |
| `input/actions.rs` | Add the two actions to the no-event group |
| `app/input.rs` | Dispatch the two actions |
| `view/prompt.rs` | `PromptType::OpenRecentProject` |
| `app/prompt_lifecycle.rs` | Resolve selected suggestion `value` on confirm |
| `app/prompt_actions.rs` | Confirm handler → `change_working_dir` |
| `app/recent_projects_actions.rs` | New: build picker, record-on-open, toggle pin |
| `input/commands.rs` | Two command-palette entries |
| `config.rs` | File-menu entry |
| `main.rs`, `server/editor_server.rs` | Record-on-open at the real boot points |
| `locales/*.json` | New i18n keys (all 14 locales) |

## Testing

- **Unit** (`recent_projects.rs`): MRU ordering, de-dup, cap excludes pinned,
  pin/unpin, serde round-trip, graceful handling of corrupt input.
- **E2E**: open the palette, run *Open Recent Project*, assert the rendered
  suggestion list shows the seeded project names (observe rendered output only).
- **Record-on-open**: integration assertion that after booting against a dir, the
  store file lists it (observing the persisted artifact).
</content>
</invoke>
