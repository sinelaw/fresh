# LSP UX Heuristic Evaluation — clangd / fmt

Heuristic evaluation + technical audit of Fresh's **LSP Status Indicator**, **Notification Bar**, and **Hover Pop-ups**, using clangd as the language server against a real C++ codebase.

## Methodology

- **Editor build:** Fresh v0.2.23 (`d4cdedc`), compiled with `cargo build` (debug, **no** `--release`).
- **Language server:** `clangd` 18.1.3 (Ubuntu 24.04).
- **Test codebase:** [`fmt`](https://github.com/fmtlib/fmt) cloned to `/tmp/fmt`. `compile_commands.json` generated with `cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DFMT_TEST=OFF ..` from a `build/` subdirectory, then copied to the project root.
- **Files exercised:**
  - `src/format.cc` — 43 lines; in compile DB; template instantiations (dragonbox, locale_ref).
  - `test/format-test.cc` — 2 618 lines; **not** in compile DB (FMT_TEST=OFF); gtest macros.
  - `include/fmt/format.h` — 4 428 lines; C++ template header used for latency testing.
- **Environment:** tmux 3.4, pane size 200×50.
- **Visual inspection:** `tmux capture-pane -e -p` (to keep ANSI) and `tmux capture-pane -p` (for layout verification).
- **Framework:** NN/g Usability Heuristics with severity 0 – 4.

## 1. Heuristic Violation Log

| # | Finding | NN/g Heuristic | Severity |
|---|---|---|---|
| H-1 | **LSP does not auto-start.** Log line: `LSP for cpp not auto-starting (auto_start=false). Use command palette to start manually.` The status bar shows the language (`C++`) but no indication that clangd is available-but-dormant. | Visibility of System Status | **3 — Major** |
| H-2 | **Progress indicator did not surface during clangd indexing.** ~8 s elapsed between `Start/Restart LSP Server` and the first diagnostic flush; the only UI signal was the post-hoc `LSP (cpp) ready` token. No spinner, no percentage. *Code note: a `$/progress` relay does exist (`handle_lsp_progress` → `update_lsp_status_from_progress` in `app/mod.rs`). What was observed is that the pipeline did not render progress for this clangd/fmt session — root cause still to be identified (token registration, render-trigger, or clangd-specific throttling).* | Visibility of System Status | **3 — Major** |
| H-3 | **Dual, inconsistent diagnostic counters.** Status bar simultaneously shows `E:21 W:2` (LSP) and `[⚠ 1]` (editor/plugin). `Clear Warnings` removes the badge but not the LSP counts. Their relationship is undocumented. | Consistency & Standards; Minimalist Design | **3 — Major** |
| H-4 | **Diagnostics panel counter / title mismatch.** Panel header: `Diagnostics (Current File):`. Panel status line: `Diagnostics: 26 items`. Only 3 rows are visible for the current file; the 26 is a project-wide count. | Match Between System and Real World | **2 — Minor** |
| H-5 | **Hover does not surface diagnostic text.** With `zzz` appended at `format.cc:14:77` (clangd: `Unknown type name 'zzz'`), `Alt+K` returned enclosing-namespace info only. Diagnostic text is reachable only via `F8` → status bar, or the separate Diagnostics panel. | Recognition Rather than Recall; Error Prevention & Recovery | **3 — Major** |
| H-6 | **Hover under-informative on qualified names / templates.** Hovering `locale_ref::get<std::locale>` at `format.cc:14` returned `namespace v12 { inline namespace v12 {} }` — the enclosing inline namespace, never the function signature. "Go to Definition" is still required. | Recognition Rather than Recall | **3 — Major** |
| H-7 | **Hover silently fails on files outside `compile_commands.json`.** `test/format-test.cc` produced `No hover information available` on every identifier, with `E:21` displayed and no explanation. clangd's standard "no compile command available" guidance is not surfaced. | Help Users Recognize, Diagnose, Recover | **3 — Major** |
| H-8 | **`.h` files default to language `C`, preventing C++ LSP binding.** Opening `include/fmt/format.h` displayed `C` (not `C++`) in the status bar and no `LSP` badge. No hint is given that the file is being routed to a C language definition. | Match Between System and Real World | **2 — Minor** |
| H-9 | **Hover popup has no filled background.** The border (`38;5;51` cyan) floats over "transparent" content because the interior uses the editor background (`48;5;16`). Only syntax-highlighted tokens carry their own bg, yielding visible rectangles inside the card. | Aesthetic & Minimalist Design | **2 — Minor** |
| H-10 | **Low-contrast warning severity text in Diagnostics panel.** `[W]` body text uses `38;5;59` (#5f5f5f) on `48;5;16` (#000) ≈ **3.2 : 1** — below WCAG AA 4.5 : 1. `[E]` body is `38;5;231` (#fff) ≈ 21 : 1. Hierarchy is achieved by dimming warnings into near-illegibility. | Accessibility / Aesthetic Design | **2 — Minor** |
| H-11 | **Transient notifications share the status strip with persistent metadata.** After `Ctrl+S` the strip read `… W:1 \| Saved`. `Saved` never auto-clears; it is overwritten by the next notification. No toast area; no log of recent events. | Minimalist Design; Visibility of System Status | **2 — Minor** |
| H-12 | **Dirty buffer after `--no-restore` on a never-edited file.** Launching `fresh --no-restore test/format-test.cc` opened the file with a stray `:` at line 100 col 1 from a prior workspace, and `[+]` was set. The flag does not fully prevent session content leakage. | Consistency & User Control | **3 — Major** |
| H-13 | **SIGTSTP leaves a ghost status bar on the shell.** After an accidental editor suspend, `tmux capture-pane -p` showed the editor's bottom status line composed with the shell prompt; no screen clear on suspend. On tmux `kill-server`, the spawned clangd became `<defunct>` parented to PID 1. | Error Prevention; Aesthetic Integrity | **2 — Minor** |
| H-14 | **Popups are mutually exclusive, but silently.** `Ctrl+S`, `Ctrl+P`, a second `Alt+K`, or any command-palette action dismisses an active hover with no visual cue. Good for z-index; bad for discoverability. | User Control & Freedom | **1 — Cosmetic** |
| H-15 | **Latency under load is acceptable (strength).** `Ctrl+End` / `Ctrl+Home` on a 4 429-line `format.h` completed in ~1 s. clangd indexing did not block keystrokes. | Visibility of System Status | **0 — Strength** |

## 2. Technical Audit Report (`tmux capture-pane -e -p`)

### 2.1 Status bar palette (idle)

```
\x1b[38;5;231m\x1b[48;5;233m    # white on near-black, contrast ≈ 19:1  (OK)
\x1b[38;5;16m \x1b[48;5;226m   # black on bright yellow for [⚠ 1]       (OK, loud)
\x1b[38;5;203m\x1b[48;5;16m    # salmon on black for "Palette: Ctrl+P"  (OK)
```

### 2.2 Hover popup

```
border:   \x1b[38;5;51m                    # cyan box-drawing U+2500/2502/2510/2518
title:    "Hover"                          # same cyan; no separate title bg
content:  inline tokens highlighted at 48;5;235 (#262626)
          interior otherwise falls through to editor bg 48;5;16 (#000)
```

- Interior is **not fill-rasterized**. Only syntax-highlighted tokens carry a background.
- No ghost or trailing characters inside the frame; box-drawing cells align.
- Right edge rendered at column 110 in a 200-col pane — no wrap-around.

### 2.3 Diagnostics panel

```
Tab title: bold \x1b[38;5;16m on \x1b[48;5;226m   # black on bright yellow, shouts
Section:   bold \x1b[38;5;147m on \x1b[48;5;16m   # lavender on black, OK
[W] tag:   bold \x1b[38;5;215m                    # orange
[E] tag:   bold \x1b[38;5;203m                    # red-salmon
[W] text:  \x1b[38;5;59m  (#5f5f5f on #000)       # 3.2:1 — BELOW WCAG AA
[E] text:  \x1b[38;5;231m (#fff    on #000)       # 21:1 — OK
```

### 2.4 Layout integrity

- No stray multi-byte trailers inside popup frames (all U+25xx box cells are complete).
- After `SIGTSTP`, the editor's status line persisted on the shell screen until the next full repaint. Ghost-rendering confirmed on suspend.
- On tmux `kill-server`, clangd was left as `<defunct>` parented to PID 1 until manual `kill -9`. Fresh does not always reap its children on abnormal exit.

### 2.5 Language detection (extension-based)

| Extension | Detected | Encoding | LSP |
|---|---|---|---|
| `.cc`  | `C++` | `UTF-8` | starts after manual trigger |
| `.h`   | `C`   | `ASCII` | **does not start** |

Fresh does not treat ambiguous headers as C++ even inside a CMake C++ project.

## 3. Remediation Plan (Actionable Recommendations)

Ordered by impact. Each item lists the finding(s) it addresses.

### P0 — LSP lifecycle visibility (highest leverage; blocks H-5 / H-6 / H-7 triage)

1. **Auto-start LSP per-language by default** (or make `auto_start=true` the documented default for servers the user has actually installed). If `auto_start=false`, render a persistent clickable `LSP: off` badge in the status bar whenever the buffer's language has a configured server. *Fixes H-1.*
2. **Wire `$/progress` into the status bar.** clangd emits `WorkDoneProgressBegin/Report/End` during background indexing. Surface it as a Braille-spinner token with a percentage, e.g. `LSP (cpp) indexing ⠇ 42%`. Show `ready` only on `End`. *Fixes H-2.*

### P1 — Error-recovery affordances

3. **Fuse hover + diagnostic on symbols carrying errors.** When `publishDiagnostics` range overlaps the cursor, prepend the diagnostic (with its severity color) to the hover card above the `textDocument/hover` content. Matches VS Code's `hover.showDiagnostic`. *Fixes H-5, partially H-7.*
4. **Explicit banner when clangd is running without a compile DB.** On `Failed to parse` / missing `compile_commands.json`, show a one-time notification with a link to the existing `Clangd: Project Setup` command. *Fixes H-7.*
5. **Treat `.h/.hpp` inside a C++ tree as C++.** Use `compile_commands.json` coverage or a sibling `.cc` as a hint; fall back to a per-project override. *Fixes H-8.*

### P2 — Visual hierarchy & accessibility

6. **Unify diagnostic counters** — either move editor-internal warnings into `E:/W:` or relabel the global badge `[plugin: 1]` so origin is unambiguous. *Fixes H-3.*
7. **Align Diagnostics panel counter with its title** (either filter count to current file, or retitle to `All Diagnostics (N)`). *Fixes H-4.*
8. **Fill the hover popup interior** with a distinct background (e.g. `48;5;235`) rather than leaving it transparent. *Fixes H-9.*
9. **Raise warning-row luminance in the Diagnostics panel** to at least `38;5;250` (#bcbcbc ≈ 9 : 1). Keep the orange `[W]` tag so severity is encoded redundantly, not by dimming alone. *Fixes H-10.*

### P3 — Hygiene

10. **Introduce a short-lived toast row** above the status bar (auto-dismiss ~3 s, with a back-scroll viewer such as `:messages`). Keep the status bar for persistent state only. *Fixes H-11.*
11. **Honor `--no-restore` strictly** — do not reapply cached buffer content, including unsaved edits from prior workspaces. Emit `Session restore skipped (--no-restore)` once at startup. *Fixes H-12.*
12. **SIGTSTP / exit handling** — reset the terminal (`tput rmcup`) before suspending so the shell comes back clean, and `SIGTERM` clangd on editor exit. *Fixes H-13 and the defunct-clangd leak.*
13. **Announce popup dismissal** — when `Ctrl+P`/save dismisses an active hover, flash a one-line toast ("Hover dismissed") or re-render on subsequent idle. *Fixes H-14.*

## 4. Severity Roll-up

- **Major (3):** 7 findings — H-1, H-2, H-3, H-5, H-6, H-7, H-12. All block the "LSP should just work" promise.
- **Minor (2):** 6 findings — H-4, H-8, H-9, H-10, H-11, H-13.
- **Cosmetic (1):** 1 finding — H-14.
- **Strengths (0):** H-15 plus: responsive under load, clear `●` gutter markers, well-structured Diagnostics panel, clean box-drawing in hover frames.

**Single highest-impact fix: surfacing LSP lifecycle state (P0 items 1 + 2).** A developer opening a C++ file today cannot tell whether clangd is off, starting, indexing, or idle — and every downstream finding (H-5 / H-6 / H-7) gets blamed on the LSP rather than on the missing status signal.

## 5. Reproduction Notes

```bash
# Prerequisites
sudo apt-get install -y clangd
git clone --depth 1 https://github.com/fmtlib/fmt /tmp/fmt
mkdir -p /tmp/fmt/build && (cd /tmp/fmt/build && \
  cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DFMT_TEST=OFF ..)
cp /tmp/fmt/build/compile_commands.json /tmp/fmt/

# Build editor in debug mode (no --release)
cd <fresh-repo>
cargo build

# Run in tmux
tmux new-session -d -s eval -x 200 -y 50
tmux send-keys -t eval "cd /tmp/fmt && \
  <fresh-repo>/target/debug/fresh --no-restore --log-file /tmp/fresh.log \
  src/format.cc" Enter

# Inspect
tmux capture-pane -t eval -e -p   # with ANSI
tmux capture-pane -t eval -p      # layout only
```

Key commands exercised inside the editor:

| Action | Input |
|---|---|
| Start LSP | `Ctrl+P` → `Start/Restart LSP Server` → `Enter` |
| LSP status | `Ctrl+P` → `Show LSP Status` → `Enter` |
| Hover | `Alt+K` |
| Go to line | `Ctrl+G` |
| Next / Prev diagnostic | `F8` / `Shift+F8` |
| Diagnostics panel | `Ctrl+P` → `Show Diagnostics Panel` → `Enter` |
| Clear warning badge | `Ctrl+P` → `Clear Warnings` → `Enter` |
