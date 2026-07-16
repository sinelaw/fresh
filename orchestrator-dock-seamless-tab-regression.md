# Orchestrator dock: active-session "seamless tab" border lost in the tree redesign

Interactive tmux reproduction of the UX regression where the current session's
dock card no longer visually connects to the editor area. Before the dock's
folder-tree redesign, the active card's border merged into the editor (the
dock's right-edge divider was scooped away across the card); now the card is
only marked by a background highlight.

## Root cause

The "connected border" is the dock's **seamless active tab**, painted by
`paint_dock_seamless_active_tab` (`crates/fresh-editor/src/app/render.rs`).
It erases the dock's right-edge divider (the "wall") across the active card's
rows and draws `╭──…──╯` / `╰──…──╮` scoop corners so the card flows into the
editor, like a file-folder tab.

That painter locates the active card by the **heavy box glyphs**
(`┏ ┓ ┗ ┛ ┃`) that the old *list*-widget selection styling
(`mark_list_card_selected` in `crates/fresh-editor/src/widgets/render.rs`)
stamped onto exactly one card's rows.

The dock redesign (`71d7428` "redesign dock as a hierarchical folder tree")
replaced the list with a *tree* widget. The tree's selection styling is a
plain background fill (`ui.popup_selection_bg`, applied in `collect_tree` /
`select_style`) — it never promotes border glyphs to their heavy forms. So
`paint_dock_seamless_active_tab` finds no `┏┓┗┛┃` rows and silently no-ops.
`251dccd` ("bordered dock cards…", issue #2703) restored the rounded card
borders via `render_tree_card`, but not the heavy-glyph selection marking the
seamless-tab painter keys on — which is why cards look right again but the
active one is "just highlighted".

The painter and its doc comments (and the doc comment on
`creating_session_moves_dock_highlight_to_new_session` in
`crates/fresh-editor/tests/e2e/orchestrator_dock.rs`, which still *describes*
reading state off the seamless tab) are now dead letters: no e2e test asserts
the scooped wall, which is why the regression went unnoticed.

## Interactive reproduction (tmux)

Debug build at HEAD (`6f5f4a1`), tmux 160×45, fresh `XDG_*` dirs, three git
projects (`alpha-proj`, `beta-proj`, `gamma-proj`):

1. `tmux new-session -d -x 160 -y 45 -c .../alpha-proj` then run
   `target/debug/fresh`.
2. `Ctrl+P`, type `Toggle Dock`, `Enter` — the dock opens with the launch
   session's card. (Gotcha for scripted driving: don't type the full
   `Orchestrator: Toggle Dock` — the `:` switches the palette to `:line`
   mode and swallows the rest.)
3. `Ctrl+P`, `New Workspace`, `Enter`, type the `beta-proj` path, raw
   `Ctrl+Enter` (`\033[13;5u`) to submit; repeat for `gamma-proj`.
4. `Alt+O` to focus the dock, `↑` to live-switch to `beta-proj`.

### HEAD (regressed) — wall unbroken, selection is bg-fill only

`beta-proj-2` is the selected/active session here; its highlight is a pure
background fill (`48;2;0;100;200` in the ANSI capture), invisible in plain
text, and the wall `│` runs past every card:

```text
╭────────────────────────────────────╮ │
│· alpha-proj  ▣ alpha-proj          │ │
│▸ master   clean                    │ │
│                                    │ │
╰────────────────────────────────────╯ │
╭────────────────────────────────────╮ │
│· beta-proj-2  ▣ beta-proj          │ │   <- ACTIVE (bg highlight only)
│▸ beta-proj-2   clean               │ │
│                                    │ │
╰────────────────────────────────────╯ │
╭────────────────────────────────────╮ │
│· gamma-proj-3  ▣ gamma-proj        │ │
│▸ gamma-proj-3   clean              │ │
│                                    │ │
╰────────────────────────────────────╯ │
```

No row of the dock contains a heavy glyph (`grep -c '┏\|┃\|┗\|━'` over the
captures = 0), confirming the painter's trigger can never fire.

### Pre-tree (`923056c`, parent of `71d7428`) — same scenario, same keys

The active card's top edge runs to the wall and scoops up (`╯`), its right
side is open (no card border, no wall), and the bottom edge scoops down
(`╮`); the tab follows ↑/↓ live-switching:

```text
╭────────────────────────────────────╮ │
│ · alpha-proj          ▣ alpha-proj │ │
│ ▸ master                     clean │ │
│                                    │ │
╰────────────────────────────────────╯ │
╭──────────────────────────────────────╯
│ · beta-proj-3          ▣ beta-proj        <- ACTIVE: merges into editor
│ ▸ beta-proj-3                clean
│
╰──────────────────────────────────────╮
╭────────────────────────────────────╮ │
│ · gamma-proj-4        ▣ gamma-proj │ │
│ ▸ gamma-proj-4               clean │ │
│                                    │ │
╰────────────────────────────────────╯ │
```

## Fix (implemented)

The tree's bordered-card path now marks the selected card the way the
pre-tree card list did: `render_widget_tree` applies
`mark_list_card_selected` (heavy box frame, bold, no background band) to a
selected *card* node's rows instead of the bg fill, which non-card rows
(folder headers, plain single-line trees) keep. The heavy glyphs restore the
marker `paint_dock_seamless_active_tab` keys on, so the active card merges
into the editor again; `mark_list_card_selected` also learned to recognise
border rows behind the tree's depth indent.

The e2e test `active_session_card_is_a_seamless_tab_and_follows_focus` now
asserts the rendered scooped wall (`╯`/`╮` at the wall column, no `│` across
the active card's content rows, inactive cards keeping the divider, the tab
following ↑/↓ live-switches). It times out without the fix and passes with
it. Verified interactively in tmux with the same scenario as above — the
active card scoops into the wall and the tab follows live-switching in both
directions.
