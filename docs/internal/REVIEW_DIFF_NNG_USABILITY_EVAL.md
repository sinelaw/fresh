# Review Diff — Open Defects

**Feature:** `fresh` Review Diff mode (launched via `Ctrl+P → "Review Diff"`).
**Branch:** `claude/tui-editor-usability-eval-0LHgo` · **Editor:** 0.2.23 debug build.
**Evidence:** Screen captures under `/tmp/validate/c_*.txt` (pass 3, interactive),
`/tmp/eval-workspace/pass2/p2_*.txt` (pass 2, scenario sweep), and
`/tmp/eval-workspace/screen_*.txt` (pass 1, initial walk-through).

All items below are capture-proven open defects. Everything that already
works is in the appendix, as a sanity check on what the feature is for.

> **Status (follow-up pass):** items 2, 3, 4, 6, 7, 8 are now fixed in
> this branch with e2e regression tests. Item 1 was not reproducible in
> the e2e harness (likely a terminal-emulator specific rendering
> artifact) — a stricter chrome-visibility guard is in place regardless.
> Items 5, 9 remain deferred: #9 is blocked on the plugin API lacking a
> way to set inline-overlay priority (whole-entry `extend_to_line_end`
> currently paints over inline bg overlays).

---

## P0 — Ship-blockers

### 1. Terminal resize is unrecoverable — **deferred (not reproducible)**

Shrink 160×45 → 80×24 → grow back leaves menu, toolbar, and tab row
hidden. `r` refresh doesn't fix it; resize-bump doesn't fix it;
close-and-reopen of the review tab leaves stale rendering on the right
pane. Only killing and relaunching the editor recovers.

*Evidence:* `c_40_at_80.txt`, `c_41_back.txt`, `c_42_after_r.txt`,
`c_44_bump.txt`, `c_46_reopen.txt`.

*Status:* not reproducible in the e2e harness. Added a stricter
`test_issue1_resize_cycle_restores_all_chrome` as a guard.

### 2. Side-by-side `n` / `p` leave status bar stale — **fixed**

`n` / `p` moved the viewport but did NOT update the status-bar `Ln` /
`Col`. `composite_{next,prev}_hunk` now call
`sync_editor_cursor_from_composite`, mirroring the arrow-key path.

*Evidence:* `c_18_sxs.txt` → `c_21_sxs_n3.txt` → `c_19_sxs_down1.txt`.
*Regression test:* `test_issue2_side_by_side_next_hunk_updates_status_bar`.

### 3. No "Hunk N of M" indicator — **fixed**

The status bar now shows `Review Diff: Hunk N of M` whenever a current
hunk is known (new i18n key `status.review_summary_indexed`), driven by
a `currentGlobalHunkIndex()` helper invoked from every site that
changes the current hunk.

*Evidence:* `c_03`–`c_09`.
*Regression test:* `test_issue3_status_bar_shows_current_hunk_index`.

### 4. Empty state is ambiguous — **fixed**

`getGitStatus()` now returns an `EmptyStateReason` and the files / diff
panels render a labelled line ("Not a git repository." /
"No changes to review.") so the two cases are no longer byte-identical.

*Evidence:* `c_22_nogit.txt`, `c_23_clean.txt`.
*Regression test:* `test_issue4_empty_state_distinguishes_not_git_from_clean_repo`.

---

## P1 — High-impact UX gaps

### 5. Unified pane has no per-keyword syntax highlighting

Side-by-side does (`def` → fg 207, `return` → fg 51); unified pane uses
one foreground color per `+` / `-` line, no language tokens.

*Evidence:* `c_33_unified_syntax.txt` shows `[1m[38;5;51mdef add(a:
int, b: int) -> int:[0m` — single color for the whole keyword-rich
line. `c_34_sxs_syntax.txt` shows `[38;5;207mdef`, `[38;5;51mreturn`
with per-token colors.

### 6. `n` / `p` are dead in the files pane — **fixed**

`review_next_hunk` / `review_prev_hunk` used to gate on
`focusPanel === 'diff'`; the guard is removed because
`jumpDiffCursorToRow` already handles the unfocused diff panel via
`setBufferCursor`.

*Evidence:* `c_36_n_filespane.txt`.
*Regression test:* `test_issue6_n_from_files_pane_advances_hunks`.

### 7. `n` / `p` do not cross file boundaries — **fixed**

When the cursor is on the last hunk of the current file, `n` now walks
into the next file with hunks and lands on its first hunk (and mirror
for `p`). See `jumpToAdjacentFileHunk`.

*Evidence:* `c_37_n_pastend.txt`.
*Regression test:* `test_issue7_next_hunk_crosses_file_boundaries`.

### 8. `n` / `p` hints appear in the toolbar only after `Tab` — **fixed**

Hunk-navigation hints are now advertised on both the files-pane and
diff-pane toolbars. On the files pane Export/Close retain priority so
their labels survive the narrow viewports documented by bug10.

*Evidence:* `c_01_review.txt` vs `c_13_diff_start.txt`.
*Regression test:* `test_issue8_n_and_p_hints_visible_on_files_pane_toolbar`.

### 9. Whitespace-only changes have no per-character highlight — **deferred**

Trailing-space and double-space edits look identical on the `-` and `+`
lines; only the leading marker differs. The plugin already computes
per-char diff parts, but the inline-overlay bg it wants to paint is
overwritten by the whole-entry `extend_to_line_end` bg because both
overlays share the same priority. Fixing this needs a plugin-API
addition (overlay priority on `OverlayOptions`).

*Evidence:* `screen_13_whitespace_ansi.txt` — full-line bg, no
intra-line spans.

---

## P2 — Standards & discoverability

### 10. Non-standard hunk header

Renders as `@@ L006 @@` (a context-line preview) instead of git-standard
`@@ -X,Y +X,Y @@ <signature>`. Breaks muscle memory from `git diff` /
GitHub / `vimdiff` and prevents counting added/removed lines per hunk.

*Evidence:* `c_15_start.txt`.

### 11. Review Diff is in zero top-level menus

Walked every menu (File / Edit / View / Selection / Go / LSP / Help) —
the feature is not present. Only `Go → Command Palette…` exists, which
delegates back to `Ctrl+P`.

*Evidence:* `c_26_menu_file.txt`–`c_32_menu_help.txt`.

### 12. F1 in-app Manual lacks the feature

Searching the Manual for "review diff" returns `No matches found`.

*Evidence:* `screen_25_help_search.txt`.

### 13. Fuzzy palette is subsequence-only, no typo tolerance

"revw difff" returns `Markdown: Toggle Compose/Preview` instead of
`Review Diff`.

*Evidence:* `screen_22_typo.txt`.

### 14. `\ No newline at end of file` marker is dropped

A file stripped of its trailing newline shows only the normal
`+modified` line with no marker — reviewers will miss newline
regressions in shell scripts / fixtures.

*Evidence:* `p2_09_nonl.txt`.

### 15. Merge-conflict files appear twice

`UU conf.txt` shows in both `Staged` and `Changes` sections with `(no
diff available)` and no resolution affordance.

*Evidence:* `p2_43_conflict.txt`.

---

## P3 — Polish & edge cases

### 16. `N` / `n` key collision

Lowercase `n` = next hunk; capital `N` = open Note prompt. Distinct but
easy to mis-fire; no other toolbar key pairs rely on case sensitivity.

*Evidence:* `p2_22_lower_n.txt` vs `p2_23_upper_N.txt`.

### 17. Chrome vanishes at 80 × 24

Menu, tab row, and toolbar all disappear at narrow widths. No graceful
degradation to a single-glyph legend.

*Evidence:* `c_40_at_80.txt`.

### 18. No overflow indicator for truncated lines

`End` scrolls horizontally but nothing signals that a line continues.

*Evidence:* `screen_04_review_plain.txt`, `p2_37_end.txt`.

### 19. Files list sorts alphabetically, not naturally

`many/f10.txt` precedes `many/f2.txt`.

*Evidence:* `p2_02_review_open.txt`.

### 20. No line numbers in the unified diff gutter

Side-by-side has them; unified does not. Makes "which line is that?"
conversations awkward.

*Evidence:* `c_15_start.txt`.

### 21. No "reopen last review" command

After `q`, every re-entry requires the 4-keystroke palette round-trip.

---

## Suggested sprint bundle

- **A — stabilise:** 1, 2, 3, 4.
- **B — navigation ergonomics:** 5, 6, 7, 8.
- **C — standards & a11y:** 9, 10, 14.
- **D — discoverability & polish:** 11, 12, 13, 15, 16–21.

---

## Appendix A — What Review Diff is trying to be

A single keyboard-driven review surface that lets a developer do a PR-
style read-through of local changes without leaving the editor:

- List every changed file, grouped by `Staged` / `Changes` / `Untracked`.
- Show a unified colour diff on the right; drill into side-by-side with
  `Enter`.
- Stage / unstage / discard hunks with `s` / `u` / `d`.
- Attach inline `c`omments and a per-session `N`ote.
- Export the whole session to Markdown or JSON for sharing.

## Appendix B — Verified working (reference)

These behaviours were explicitly tested and do not need changes:

- `n` / `p` hunk navigation in the unified pane (jumps between hunk
  headers correctly).
- Viewport auto-scrolls to follow the cursor in the unified pane.
- Current hunk header is highlighted (bg `256:17`, dark blue).
- `NO_COLOR=1` env var is honoured (only `[0m` / `[4m` emitted).
- Cursor position is preserved across `Tab` between panes.
- Inline comments render as `» [hunk] …` and persist across close /
  reopen (backed by `.review/session.md`).
- `d` discard shows a confirmation dialog with an "undone" warning.
- Rename detection renders as `R old → new`.
- Unicode and emoji align correctly in both panes.
- Deleted-file drill-down into side-by-side works (no hang).
- `q` cleanly closes the review; the editor survives all tested keys
  including `Ctrl+C`.
- Debug-build input responsiveness is adequate (50 `PageDown` presses
  in ~3 s with no lag).
