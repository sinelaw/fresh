# Project Search/Replace — Reproduction Report

**Date:** 2026-04-15
**Branch:** `claude/fix-search-replace-bugs-rmhHU`
**Build:** `cargo build --bin fresh` (debug, no `--release`)
**Harness:** manual tmux (`send-keys` / `capture-pane`) against `/tmp/srtest`
**Upstream bug list:** https://github.com/mandolyte/fresh-ideas-testing/blob/main/Project-search-replace.md

## Test fixture

```
/tmp/srtest/
├── .git/                 (initialized, one commit)
├── a.txt                 "hello world\nhello there\nthis is a hello test\nfinal hello line\n"
├── b.txt                 "hello from b\nanother hello\nno match here\nhello hello hello\n"
└── sub/
    └── c.txt             "nested hello\nhello nested\n"
```

Total: 11 occurrences of `hello` across 3 files.

## Summary table

| # | Bug (from doc)                                    | Reproduced?                                  |
|---|---------------------------------------------------|----------------------------------------------|
| 1 | Ctrl+Z does not undo project replacements         | ✅ Confirmed — bug is in the `undo` command itself, not key handling |
| 2 | Match list not refreshed after replacement        | ✅ Confirmed                                 |
| 3 | Panel duplication                                 | ✅ Confirmed                                 |
| 4 | `Alt+Ret` repeats replacement on stale matches    | ✅ Confirmed — worse than documented (produces file corruption) |
| 5 | Split panel persists after restart w/ random file | ✅ Confirmed                                 |
| 6 | Buffer content mixup after restart                | ⚠️ Partially — split/tab serialization is broken; literal "wrong content under right label" not observed |

Additional issues discovered while reproducing (`A`–`D`) are at the bottom.

---

## Bug 1 — `Undo` does not revert project replacements

**Severity:** High. Data loss if user relies on undo after a mistaken project-wide replace.

**Observed:** The `Undo` command (whether triggered via `Ctrl+Z` key or via command palette) has no effect on changes made by project search/replace. Both the in-memory buffer and the file on disk remain in the post-replace state.

### Reproduction

Prerequisites: run `stty susp undef` before launching fresh, otherwise `Ctrl+Z` is swallowed by the shell as SIGTSTP (see issue D below).

1. `cd /tmp/srtest && /home/user/fresh/target/debug/fresh --no-restore a.txt`
2. `Ctrl+P`, type `Search and Replace in Project`, `Enter`
3. `Alt+]` (focus the panel — required, see issue A)
4. Type `hello`, `Enter`, type `XYZ`, `Enter`
5. `Alt+Enter` — status bar: `Replaced 11 occurrences in 3 files`. Verify on disk: `cat a.txt` → `XYZ world / XYZ there / this is a XYZ test / final XYZ line`.
6. `Escape`, `Escape` to close the panel.
7. `Alt+[` to focus the main split (`a.txt`).
8. `Ctrl+P`, type `Undo`, `Enter`.

**Expected:** `a.txt` reverts to `hello world / …`.
**Actual:** `a.txt` remains `XYZ world / …`, in buffer and on disk. Repeated `Undo` invocations also no-op.

### Likely root cause

`doReplaceAll` in `crates/fresh-editor/plugins/search_replace.ts` writes through `editor.replaceInFile(filePath, matches, panel.replaceText)`, which bypasses per-buffer undo-stack entries. See `doReplaceAll` (~line 799) and the backend path in `crates/fresh-editor/src/app/plugin_commands.rs`.

---

## Bug 2 — Match list is not refreshed after replacement

**Severity:** Medium. User cannot tell what has actually been modified; amplifies Bug 4.

**Observed:** After `Alt+Enter`, files are updated but the panel still lists the pre-replacement matches with the old context strings.

### Reproduction

1. `cd /tmp/srtest && /home/user/fresh/target/debug/fresh --no-restore a.txt`
2. `Ctrl+P`, `Search and Replace in Project`, `Enter`, `Alt+]`.
3. `hello`, `Enter`, `HI`, `Enter`. Panel shows `(11 matches / 3 files)` listing, e.g. `a.txt:1 - hello world`.
4. `Alt+Enter`. Status bar: `Replaced 11 occurrences in 3 files`. Verify: `cat a.txt` shows `HI …`.
5. Look at the panel.

**Expected:** Match list clears (or re-runs search and shows new matches for `hello` = 0).
**Actual:** Panel still reads `(11 matches / 3 files)` with stale entries `a.txt:1 - hello world`, `b.txt:4 - hello hello hello`, etc.

### Likely root cause

No post-replace refresh. `doReplaceAll` does not call `rerunSearch()`/`rerunSearchQuiet()` after writing files, so `panel.searchResults` and `panel.fileGroups` remain stale.

---

## Bug 3 — Panel duplication

**Severity:** Medium. Confusing layout; contributes to the persistence/focus bugs.

**Observed:** Invoking the feature creates `*Search/Replace*` in two places at once — as a **tab in the currently focused split** and as a **new split** with the actual panel UI.

### Reproduction

1. `cd /tmp/srtest && /home/user/fresh/target/debug/fresh --no-restore .`
2. `Ctrl+P`, `Search and Replace in Project`, `Enter`.
3. Inspect the tab bar and splits.

**Observed pane layout (captured):**

```
File Explorer  | [No Name] ×   *Search/Replace* ×
               |    ...
               |    ────────────────────────
               | *Search/Replace* ×
               | Search: []  Replace: []
               | [ ] Case  [ ] Regex  [ ] Whole  [Replace All]
               | ─── Matches ───
               |  Type a search pattern above
```

Note `*Search/Replace*` appears twice: once as a tab next to `[No Name]` and once as its own split.

---

## Bug 4 — `Alt+Ret` repeats replacement on stale matches (file corruption)

**Severity:** Critical. Silent file corruption with no warning and no undo.

**Observed:** Pressing `Alt+Enter` a second time uses the **original** byte offsets stored in `panel.searchResults` against the now-modified file content, writing garbled text.

### Reproduction

1. `cd /tmp/srtest && git checkout .` (reset fixture)
2. Launch fresh, open panel, search `hello`, replace `XYZ`, run the search, `Alt+Enter` — file becomes:
   ```
   XYZ world
   XYZ there
   this is a XYZ test
   final XYZ line
   ```
3. Press `Alt+Enter` again (no intermediate actions — panel still shows the stale match list from Bug 2).
4. `cat /tmp/srtest/a.txt`.

**Expected:** Either a warning/no-op (no `hello` in files anymore) or the replace re-runs a fresh search and does nothing.
**Actual observed output:**

```
hhXYZ world
hXYZ there
this is a hXYZ tesHIal HI line
hXYZ
```

The second replace took the byte offsets recorded for `hello` (offsets 0, 12, 24, … in the *original* a.txt), looked up 5 bytes at each offset (whatever happens to be there in the modified file), and replaced them with `XYZ`. Result: garbage.

### Likely root cause

Replace path iterates `panel.searchResults` (a `SearchResult[]` captured at search time) and uses each result's `byte_offset` / `length` without verifying that the bytes at those offsets still match the search pattern. See `doReplaceAll` around `search_replace.ts:799` and the server-side `editor.replaceInFile` handler.

### Mitigation ideas

- Re-run the search (or a per-file byte-check) immediately before applying replacements.
- Disable the `[Replace All]` action and clear `panel.searchResults` after a successful replace.
- Validate that the bytes at each stored offset still equal the original match before overwriting.

---

## Bug 5 — Split persists across restart, but with a "random" file

**Severity:** Medium. User reopens the editor and finds a stray split showing the wrong buffer.

**Observed:** Workspace serialization preserves the split that held `*Search/Replace*`, but since the virtual buffer is not restorable, the restored split ends up showing whichever file happened to be active in the neighboring split.

### Reproduction

1. `cd /tmp/srtest && /home/user/fresh/target/debug/fresh a.txt` (no `--no-restore`).
2. `Ctrl+P`, `Search and Replace in Project`, `Enter`. Confirm bottom split shows the panel.
3. `Ctrl+Q` to quit cleanly (saves workspace).
4. Relaunch: `/home/user/fresh/target/debug/fresh` (no arguments).

**Expected:** Either no stray split, or the `*Search/Replace*` panel is reopened.
**Actual:** Two vertical splits appear; the bottom split that previously held `*Search/Replace*` now shows `a.txt` (the last active file). Repeating the quit/restart cycle accumulates further copies of this split.

---

## Bug 6 — Tab restoration for the search/replace split is broken (partial)

**Severity:** Medium. Related to Bug 5; the workspace save/restore round-trip loses the virtual panel but keeps its neighbors.

**Observed:** When extra file tabs are opened next to `*Search/Replace*` in the same split, the panel tab is silently dropped on restart and only the file tabs remain.

### Reproduction

1. Launch fresh with session restore on (`fresh .`).
2. `Ctrl+P`, `Search and Replace in Project`, `Enter`. Bottom split now has `*Search/Replace*`.
3. `Ctrl+P`, type `b.txt`, `Enter` to open `b.txt` — it attaches as a second tab in the same bottom split: `*Search/Replace* ×   b.txt ×`.
4. `Ctrl+Q` to quit.
5. Relaunch with session restore.

**Expected:** Either both tabs restored, or only the real file tab restored with no stale splits.
**Actual:** The `*Search/Replace*` tab is gone, `b.txt` tab remains with correct content, and — combined with Bug 5 — the split stacking accumulates on subsequent quit/relaunch cycles (I ended with four vertical splits across two sessions: `a.txt` ×3 and `b.txt` ×1).

The "wrong content under the right label" symptom described in the original doc was not observed in this session, but the underlying serializer clearly mishandles virtual buffers, so it is plausible that a different tab order produces mismatched labels.

---

## Additional issues discovered while reproducing

### A — Focus does not land on the search field after opening the panel

**Observed:** Immediately after `Ctrl+P → Search and Replace in Project → Enter`, keystrokes leak into whichever pane was previously focused.

**Symptoms seen:**

- Typing `hello` when the file explorer had focus inserted `/hello` into the explorer's filter field (title bar shows `/hello`).
- Typing `hello` when `a.txt` had focus inserted `h` into `a.txt` (buffer became `hhello world`) before focus caught up, leaving the search box with `ello`.

**Workaround:** always press `Alt+]` (next_split) after opening the panel to move focus onto the search field.

### B — Typing in the search field does NOT auto-run the search

**Observed:** `insertCharAtCursor` in `search_replace.ts` (line 193) updates `panel.searchPattern` and redraws, but does not call `rerunSearchDebounced()`. Same for `search_replace_backspace` / `search_replace_delete`.

**Evidence:** Ran with `--log-file /tmp/fresh.log`; `handle_grep_project_streaming` does not fire while typing. The "No matches found" label shown next to a populated search box is a placeholder (`searchPattern && !results` branch in the renderer), not a search result.

**Actual flow required:**

1. Type pattern → no search.
2. `Enter` → moves focus from search field to replace field.
3. Type replacement (can be empty).
4. `Enter` → runs the search.

### C — `Escape` does not always close the panel

Escape in the wrong sub-pane (e.g. in the split that duplicates the panel as a tab) only closes one instance or the file-explorer filter. Repeated Escape presses are needed, and order matters.

### D — `Ctrl+Z` is caught by the shell, not by fresh

Fresh does not disable the terminal's `VSUSP` character (default `^Z`), so pressing `Ctrl+Z` inside the editor sends SIGTSTP and stops the process (`[1]+ Stopped fresh`). A real terminal user would likely see the same behavior. Worked around in testing with `stty susp undef` before launching.

---

## Code hotspots

| Concern | File | Notes |
|---|---|---|
| Replace path / stale offsets / no undo push | `crates/fresh-editor/plugins/search_replace.ts` — `doReplaceAll` (~L799) | Uses cached `panel.searchResults`; does not validate offsets, does not refresh, does not record undo. |
| No search-on-type | `crates/fresh-editor/plugins/search_replace.ts` — `insertCharAtCursor` (L193), `mode_text_input` (L203) | Neither calls `rerunSearchDebounced()`. |
| Backend grep (seems fine) | `crates/fresh-editor/src/app/plugin_commands.rs:2112` `handle_grep_project_streaming` | Streams results correctly; log confirms 11 matches found. |
| Panel open duplicates | `crates/fresh-editor/plugins/search_replace.ts` — `openPanel` (~L694) | Creates split AND registers tab in existing split. |
| Workspace serialization of virtual buffers | (grep for `*Search/Replace*` save paths) | Loses the panel on restart while keeping the split shape. |
| Key-binding / TTY SUSP | `crates/fresh-editor/keymaps/default.json` (`Ctrl+Z`→`undo`), terminal setup in `fresh-winterm` | Raw mode does not clear `VSUSP`. |

## Environment

- OS: Linux 4.4.0 (gvisor sandbox), shell `bash`, TTY via `tmux` 240×60.
- Rust toolchain as declared in `rust-toolchain.toml`.
- Build command: `cargo build --bin fresh` (debug profile).
- Launch: `/home/user/fresh/target/debug/fresh --log-file /tmp/fresh.log [--no-restore] <args>`.
- Logging: `--log-file` writes the full `tracing` output; `handle_grep_project_streaming` logs each search.
