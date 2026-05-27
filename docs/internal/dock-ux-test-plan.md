# Orchestrator Dock — Interactive UX Test Plan

> Manual tmux test checklist for the global orchestrator dock and its
> interaction with the rest of the editor. Run each flow interactively
> (`tmux` + `send-keys` + `capture-pane`) and record PASS/FAIL. This is
> the script behind the bug sweep in `orchestrator-dock-gaps.md`.
>
> **Harness**: `tmux new-session -d -s v -x 160 -y 42`, launch
> `./target/release/fresh --log-file /tmp/v.log .`. The dock toggle is
> reachable from an editor session via `Ctrl+P → "Orchestrator: Toggle
> Dock"`. From a terminal session, exit terminal-input first (`Ctrl+]`).

## A. Dock lifecycle
1. Toggle **show**: dock appears as a full-height left column; the menu
   bar / explorer / splits / status bar all sit to its right.
2. Toggle **hide**: dock disappears; chrome reclaims the left edge.
3. Re-show after hide: dock returns, focused.
4. Open with 1 session, 2 sessions, many (list fills height; hint pinned
   at the bottom).

## B. Session switching (the core)
5. `↑`/`↓` moves the selection and **live-switches** the active window
   (the pane to the right re-roots) with a directional wipe.
6. List **order is stable** as the active window changes.
7. `Enter` on a session **dives**: focus moves to that window, dock stays
   visible (blurred).
8. `Esc` **leaves** the dock: focus to editor, dock stays visible.
9. **Mouse click** on a session row selects + switches + focuses the dock
   (keyboard `↑↓` work afterward).
10. Mouse **wheel** over the dock scrolls the list and does NOT scroll the
    window underneath.

## C. Editing while the dock is open
11. Dive into an editor session → **type** → text inserts in the buffer;
    dock stays open.
12. After diving, the **cursor is visible** and moves as you type/arrow.
13. While the dock is **focused**, typing does NOT leak into the buffer.
14. Switch session → dive → edit a *different* session's buffer.
15. Save (`Ctrl+S`) in a dived session works.

## D. Filter / search
16. `/` focuses the filter input.
17. Typing filters the session list live.
18. `↑`/`↓` navigate the **filtered** results (and live-switch).
19. `Enter` in the filter returns to the list (does NOT dive/blur); a
    second `Enter` then dives.
20. `Esc` in the filter returns to the list; `Esc` again leaves the dock.

## E. Multi-select / actions
21. `Space` toggles the highlighted row's checkbox (`[x]`).
22. Action buttons (Stop / Archive / Delete) reachable (Tab or mouse) and
    fire; disabled states correct (launch session, last window).
23. `Delete` shows an in-place confirm; Cancel/Confirm work.
24. `+ New` (button or `Alt+N`) opens the new-session form.
25. Scope toggle (`Alt+P` / button) flips current-project ↔ all.
26. Worktrees toggle (`Alt+T`) shows/hides discovered on-disk worktrees.

## F. Command palette / menu / popups with the dock open
27. `Ctrl+P` while the dock is focused: **blurs** the dock and opens the
    palette (key falls through); dock stays visible.
28. Command palette expand is **constrained to the window** (does not
    overlap the dock column).
29. Menu bar (`F10` / `Alt+F`) opens; dropdowns align to the chrome (not
    offset by the dock) — *known gap if not.*
30. LSP / hover / completion popups position within the window.
31. Full-screen modals (Settings, keybinding editor) — *known gap: may
    overlap the dock.*

## G. File explorer with the dock open
32. `Ctrl+E` focuses the file explorer; the dock stays put.
33. Open a file from the explorer → it opens in the dived window's buffer.
34. Explorer filter (`/` inside explorer) works independently of the dock.

## H. Cross-cutting
35. Terminal session: dock visible while a terminal is the active window;
    `Ctrl+]` then `Ctrl+P` reaches the palette.
36. Resize the terminal: dock width clamps; chrome reflows; hint stays
    pinned.
37. Closing the last non-dock window / session edge cases don't panic.
38. No stray rendering artifacts in the dock column when scrolling the
    window.

## Results log

### Run 2026-05-27 (after host-level dock-key rework)
- 1 (show)           : PASS — full-height left column, chrome to its right.
- 4 (fills/pins)     : PASS — list fills, hint pinned.
- 5 (↑↓ live-switch) : PASS — right pane re-roots to each session.
- 6 (order stable)   : PASS.
- 7 (Enter dive)     : PASS — focus to editor, dock stays; Down no longer
                       moves the dock.
- 8 (Esc leave)      : PASS.
- C12 (cursor)       : PASS — with the buffer focused + dock blurred the
                       caret renders in the buffer (x=76); earlier "parked"
                       reading was the explorer-focus confound.
- C11/13 (edit)      : PASS — typing reaches the buffer after dive; while
                       focused, typing does NOT leak to the buffer.
- 16/17 (`/`, filter): PASS — `/` focuses filter, typing filters live.
- 18 (filtered ↑↓)   : PASS — navigate filtered results.
- 19 (Enter in filt) : PASS — returns to list (no dive); ↑↓ keep working.
- 21 (Space select)  : PASS — toggles `[x]` on the highlighted row.
- 27 (Ctrl+P)        : PASS — blurs dock, opens palette, dock stays.
- 28 (palette width) : PASS — palette renders right of the dock; dock
                       column intact.
- A2 (toggle hide)   : PASS.

### Not verifiable via tmux (terminal-mouse limitation here)
- 9 (mouse click row): SGR mouse events don't reach the app under this
  tmux harness. Covered instead by the e2e `mouse_click_on_dock_new_
  button_opens_form` (hit-test) + mode-independent keys (click re-focuses
  → keyboard works). Needs a real terminal for full manual confirmation.

### Key root-cause note
Dock keys are handled at the floating-panel layer (host
`dispatch_floating_widget_key`), NOT via `editor.defineMode` — mode
bindings resolve against the *active buffer's* mode, which the dock floats
over, so a session whose buffer has a local mode (terminal, markdown, …)
would shadow them. This is why earlier mode-based Space/`/` never worked.
