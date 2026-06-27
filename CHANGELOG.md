# Release Notes

## 0.4.3

For live updates on Fresh, [follow me on X](https://x.com/TheNoamLewis).

> Most options below can be changed in the **Settings UI** — run **Open Settings** from the command palette (`Ctrl+P`).

### Features

* **Virtual space** - the cursor can move beyond the end of a line, like in Visual Studio and Vim's `virtualedit`. Set `editor.virtual_space` to `on` (arrow keys, clicks, and block selections go past line ends — including below the last line: clicking there — or pressing ArrowDown at the bottom — parks the cursor on a virtual line, and typing anywhere in the void fills the gap with newlines and spaces) or `block` (only block selections extend past line ends, making rectangular copy/insert operate on true rectangles). Off by default, toggleable per buffer via **Toggle Virtual Space (Current Buffer)** in the command palette (persisted with the workspace).
* **Slang shader support** - syntax highlighting and [slangd](https://github.com/shader-slang/slang) LSP integration, with **Go to Definition** into read-only builtin modules (#2536, #2539, requested by @batoripX in #2517).
* **NetBSD builds** - Fresh now compiles on NetBSD (#2534, by @ci4ic4).

### Bug Fixes

* **Cursor column** is preserved on vertical moves that used to drift it - indented soft-wrapped lines, off-screen up/down over short lines (#2565), and blank lines with indentation guides (#2564).
* **LSP args** - an empty `args` list now overrides a built-in server's defaults, so you can use one that takes no arguments (e.g. markdown-oxide for marksman) (#2549, reported by @alex-ball).
* **Settings icons** now render on all terminals by default - standard Unicode symbols replace the Nerd Font glyphs that showed as `?`; opt back in with `editor.nerd_font_icons` (#2032).
* **Markdown tables** no longer corrupt under rapid edits (#2479, #2484).
* **Orchestrator** - clicking a dock row focuses the activated window (#2521).
* **Wave animation** is now dismissable in daemon mode (#2530, reported by @muesli).
* **Build** - no more spurious `failed to parse serde attribute` warnings on `cargo install` / `cargo build` (#2519, reported by @puphubv).
* **Large files** - modestly-sized files (e.g. a ~2 MB file, or jumping into slangd's builtin module) no longer wrongly enter large-file mode, where the gutter/status showed byte offsets instead of line numbers and LSP was disabled; the threshold is now a single **10 MB** setting instead of several overlapping ones (#2540).
* **Remote workspaces (SSH)** (#2525) - file explorer appears immediately when toggled (#2522), a boot hang is fixed with idle-based read timeouts, and per-keystroke lag on very long lines is gone (#2529).
* **Indentation guides** continue through soft-wrapped rows (#2538), respect per-buffer overrides and buffer kind (#2523), and stay visible when opening a file at a commit from **Git Log**.
* **Settings fields** - language-entry edits (incl. **Tab Size**) keep their committed value, with `Esc` to cancel and `Enter`/`Tab` to commit (#2537, #2515); the search filter gains full cursor editing and ignores `Ctrl`/`Alt` chords; the **Env** list labels rows by name instead of `(no action)`.

### Internals

* Internal architecture docs were replaced with a leaner, code-verified set, new contributing guidelines were added, and a broad clippy / compiler-warning cleanup and function-decomposition pass landed across rendering, settings, and the plugin runtime.

## 0.4.2

For live updates on Fresh, [follow me on X](https://x.com/TheNoamLewis).

> Most options below can be changed in the **Settings UI** — run **Open Settings** from the command palette (`Ctrl+P`). Hand-editing config files is rarely necessary.

### Features

* **Indentation guides** — optional vertical guides for leading whitespace (off, all levels, or just the active block), with a customizable glyph and theme color. Enable and tune them in Settings (#2388, by @maupin).
* **Persistable, editable macros** — recorded macros can now leave the in-memory register: **Macro: Save to init.ts** writes an editable block, and **Macro: Promote to command** turns a macro into a real command you can extend with code (#2487).
* **JSONC config files** — config files now accept comments and trailing commas, a stray syntax slip no longer silently resets you to defaults, and saves never clobber an unparseable file (the error is surfaced instead) (#2497).
* **Per-buffer view toggles** — *Toggle Line Numbers (Current Buffer)* and *Toggle Line Wrap (Current Buffer)* scope to the active buffer and persist across restarts.
* **LSP Go to Implementation** (`textDocument/implementation`) (#2384, by @Crocmagnon).
* **Step through Search & Replace matches from the editor** — `Ctrl+Alt+→` / `Ctrl+Alt+←` (also available as palette commands and inside the panel) jump to the next/previous match, opening each in the source split so you can review or edit it without the Down → Enter → `Alt+]` round-trip back to the panel (#2434, requested by @mandolyte).

### Improvements

* **Project Search & Replace** lists files with matches in natural path order (so `file2` precedes `file10`) instead of the random order the parallel project walk produced, making it easy to track progress while reviewing matches (#2435, requested by @mandolyte).
* **Open Terminal to the Right / Below** — open a new terminal in a fresh split beside or below the active pane.
* **File Open: reveal hidden files** when the filter starts with `.` (#2407, requested by @dragonfyre13).
* **Vim compatibility options** for vi mode — extra compatibility motions and word search (#2398, by @NihilDigit).
* **Read-only `[RO]` indicator** now actually renders in the status bar, and is clickable (#2309).
* **Terminal mode** hides the unused scrollbar and reclaims its column for the live grid.
* **Copy** strips ANSI escape codes, so text yanked from terminal output pastes clean (#2408).
* Settings UI: smoother mouse interaction and clearer dirty-state markers (#2395, by @NihilDigit).
* **Remote workspaces (SSH / Kubernetes)**:
  * A dropped connection offers **Reconnect** from the status indicator, and reconnecting respawns the workspace's embedded terminals (#2413, #2482).
  * Remote sessions stay in the Orchestrator dock across restarts, and reconnect failures surface on the status line (#2412).
  * Agents (e.g. `claude`) launched in a remote workspace now run on the remote host / in the pod, not silently on the local machine (#2409).

### Bug Fixes

* **Vi mode**: moving with `j` / `k` onto a shorter line now clamps the cursor to that line's last character (like Vim) instead of parking it one past the end, so a following `x` deletes a character rather than the newline and joins lines. The desired column is still remembered for the next vertical move (#2442).
* **Regex search**: `^` / `$` now anchor per line in multi-line `Ctrl+F` search (#2495).
* **Splits**: closing a buffer shown in two splits no longer desyncs the surviving cursor (#2496).
* **Terminal**: a terminal restores its live/scrollback mode on refocus, and the last split is no longer left read-only after closing a second terminal (#2485).
* **Vi mode**: `cw` on a non-blank now changes up to the end of the word like Vim (matching `ce`) instead of swallowing the trailing whitespace (#2437).
* **Per-language indentation rules** are now actually applied — previously custom patterns were ignored (#2314).
* **Brackets** are no longer highlighted inside comments and strings (#2405, reported by @TakemiSora).
* **Quick-open `#` buffer switcher** now lists virtual buffers (#2373).
* **Review Diff**: line-level visual stage/unstage/discard (`v` then `s`/`u`/`d`) works, and discard shows a localized message (#2317, #2420).
* **Project search & replace**: plugin edits no longer leave phantom match highlights (#2414, reported by @mandolyte).
* **Widget panels (Search & Replace and others)**: the match list highlight now follows programmatic and mouse selection — clicking a result selects it *and* opens it, and a panel that manages its own scrolling no longer shows a stray buffer scrollbar or lets a drag push its header off-screen (#2434).
* **Keybindings**: switching keybinding maps no longer hides plugin bindings until restart (#2307); `Shift`+letter bindings match even when the terminal omits the `SHIFT` modifier (#1899, reported by @RandomGHUser).
* **Git Blame** lands on the user's current source line, including files with multi-byte characters (#1957).
* **Mark mode**: movement commands (bracket jump, Home) extend the selection instead of dropping it (#2489).
* **File explorer**: a deliberately-closed explorer no longer reopens on relaunch (#2476); configured ignore patterns are applied to the tree (#2404, by @sfjohansson).
* **Switch Project** re-roots the active window in place instead of restarting the editor (#2472).
* **New Workspace (Local)** seeds its path from a local root even when the active window is remote (#2480).
* **Markdown compose**: table cells are measured by display width (fixing emoji-row border flicker), and table borders clear when compose mode is disabled (#2475, #2478).
* **Live Diff**: a *vs HEAD* (or *vs branch*) diff now clears itself after you commit the buffer's changes — the plugin watches the HEAD reflog and refreshes its reference on every commit/checkout/reset, instead of leaving a stale diff until a manual *Live Diff: Refresh* (#2503).
* **Terminal scrollback**: ANSI colors survive soft-wrapped rows (#2449); the backing file stays local in remote mode, fixing a hang when toggling scrollback over SSH (#2424).
* **Shebang language detection** now covers interpreters with no first-line regex (Fish, Lua, PowerShell, Tcl, Elixir, R, Julia, …), handling `env` indirection and versioned names; an existing extension match still wins (#2357, reported by @shemgp).
* The asm-lsp "no `.asm-lsp.toml`" offer is scoped to its triggering buffer instead of floating over every buffer.
* The tab bar's "+" popup and tab context menu grab the keyboard while open, so keys no longer leak into the buffer underneath.

### Internals

* Plugin API: core unicode display-width is exposed to plugins (`charWidth`/`stringWidth`) (#2401, by @Agrejus), and conceal ranges can be cleared by namespace (#2399, by @Agrejus).
* A large refactor decomposed many oversized functions across rendering, settings, the plugin runtime, and terminals, and reworked the UI rendering pipeline.
* `fresh-gui` now uses the published `ratatui-wgpu` from crates.io, making it publishable (#2488).
* Docs: dropped remaining "experimental" disclaimers, added the 0.4.0 "What's New" blog post, and documented the SSH "jump box" pattern (#2377, by @Monear).

## 0.4.1

This is mostly a bug-fix release.

For live updates on Fresh, [follow me on X](https://x.com/TheNoamLewis).

### Renamed features and clarified docs

This releases introduces new names (in docs, cli, etc) to clarify the ambiguously used word: "session". The new vocabulary:

* **daemon** — the persistent background process you attach to and detach from.
* **workspace** — the editor's per-project unit. Multiple workspaces are managed by the Orchestrator.
* **backend** — where a workspace runs (local / SSH / dev container / Kubernetes).

The cli now supports `--cmd daemon`, but still accepts the now-deprecated `--cmd session` as an alias. The new vocabulary also reaches the user-facing surfaces of the editor: the Orchestrator now lists, dives into, and manages **workspaces** (commands, dialogs, dock chrome, and status messages), and the replace-in-project confirmation refers to files open "in this workspace". Localized strings were updated to match in every supported locale.

### Features

* **New language support**:
  * **Assembly** via [asm-lsp](https://github.com/bergercookie/asm-lsp) (opt-in) — GAS and NASM/Intel across x86/x86_64/ARM/RISC-V, with an offer to generate `.asm-lsp.toml` from the detected dialect (#1964, requested by @viti95).
  * **Fish** highlighting and auto-indentation (#2272), by @asukaminato0721.
  * **Smali** highlighting (#2265), by @asukaminato0721.
  * `yarn.lock` and other well-known lock/config files now highlight by their real format (#2326, reported by @asukaminato0721).
* **Windows on ARM** release artifacts (#784, requested by @teobugslayer; by @NihilDigit).
* `CancelMark` / `ClearMark` actions for fine-grained selection-anchor management (#2371, by @masmu).
* **Git Log (Current File)** command, plus concurrent git-blame buffers.

### Improvements

* **Workspace trust & environments**:
  * A single trust prompt for every project — folders with a shell environment (`.envrc` / `mise` / `.tool-versions`) no longer get the env-manager plugin's separate popup; the one prompt names the detected markers and activates on trust.
  * Environment detection is defined once in core and user-extensible (`env.detectors`, now also covering pipenv and poetry); the activated environment applies uniformly across every backend — integrated terminal, Docker, Kubernetes, SSH.
  * Hardening: plugins can request but never grant trust; a lone `.venv` no longer silently auto-trusts; venv activation uses a relative `source .venv/bin/activate` snippet to avoid shell injection (cf. CVE-2024-9287).
* **Settings**: distinct, keyboard-reachable `[Inherit]` / `[Reset]` / `[Clear]` per field; the language entry dialog no longer clobbers inherited fields (#2345, reported by @ren-lv).
* **Orchestrator localization**: the Orchestrator plugin is now fully internationalized — all 225 user-facing strings go through the editor's i18n mechanism, with translations for the 14 supported locales (cs, de, es, fr, it, ja, ko, pt-BR, ru, th, uk, vi, zh-CN, and en).
* The Orchestrator New Workspace dialog has a clearer, keyboard-linear focus model (Tab accepts the highlighted completion).

### Bug Fixes

* **vi-mode**: delete/change/yank update the unnamed register (#2368, by @NihilDigit); the cursor moves one column left on leaving insert mode (#2349, by @ianyepan); `%` jumps to the matching bracket (#2346, by @ianyepan).
* **Theme inspector & plugin panels** (#2321):
  * Opening the theme editor no longer breaks Orchestrator dock clicks/scrolling — plugin panels are scoped to their owning plugin.
  * Ctrl+Right-Click reports accurate theme keys for the status bar, tab bar, scrollbar, file explorer, menus, and dock, drawing above the dock.
  * The plugin bridge no longer corrupts integers larger than 32 bits (timestamps, byte offsets).
* Java (jdtls) and other Eclipse LSP4J servers: features registered via `client/registerCapability` now work — their string JSON-RPC ids were misparsed as notifications and dropped (#2340, reported by @maxandersen).
* LSP/plugin popups follow the active theme's `popup_*` colors instead of a hard-wired dark background (#2379, by @peanball).
* Paste falls back to the internal clipboard and works again on Termux (#2343, reported by @nightshade427).
* npm `.cmd`/`.bat` shims resolve on Windows, so npm-installed language servers spawn (#2324, reported by @SupertigerDev).
* Occurrence highlighting uses a theme-appropriate background in every shipped theme (#2312).
* Review Diff lists files inside untracked directories (#2315).
* The embedded terminal forwards the wheel as a mouse report to mouse-tracking programs, so scrolling no longer cycles their history (#2366).
* Replace toolbar: checked search options are visible in every theme (#2363).
* Alt+W and other search toggles no longer leak into the close prompt (#2359).
* Fixed a crash on a stale soft-wrap position in multi-byte text (#2320).
* Trusting a workspace with a shell or virtualenv environment (`.envrc` / `mise` / `.venv`) no longer restarts the whole editor: other windows keep their running terminals, language servers, and Orchestrator dock; only the active window refreshes to pick up the new environment.
* A file opened while a split is maximized — for example via an embedded `fresh <file>` forwarded from a maximized terminal dock — is now revealed instead of rendering hidden behind the maximized split (which previously looked like the terminal had hung).

### Internals

* Refactored the UI element rendering pipeline to make it easier to introduce alternative rendering frontends.

## 0.4.0

### Features

* **Review Diff, reworked**: a file sidebar with per-file status, change counts, and comment badges, grouped by directory; an in-panel side-by-side view that keeps the sidebar; a multi-line comments panel, with commenting available anywhere in the diff; a watch mode (`W`) that reloads the diff on save; reviewing a git stash; split/stack/auto layout toggles, a `/` file filter, and reworked keyboard navigation (Tab focus model, cross-file `n`/`p`).
* **Workspace trust & environments**: a first-class, clickable `{trust}` element now leads the status bar; virtualenvs activate without a prompt, shell envs get a single combined trust-and-activate prompt, and env pills in the status bar are clickable. Changing the trust level resets only the active session instead of the whole editor.
* **Per-session backends**: each session now owns its environment, trust level, and backend (local / SSH / Kubernetes / devcontainer). Remote sessions survive a restart and reconnect when activated; restored agent sessions re-run their agent instead of coming back as a blank terminal, and the New Session dialog gained an agent-command dropdown.
* **Terminal**: send the selection (or current line) to the terminal (#1871, requested by @aquasync); Ctrl+Click or Ctrl+hover opens file paths from terminal output, including scrollback; the shell working directory is tracked via OSC 7.
* **Configurable indentation rules** per language via `[languages.<id>.indent]`, including VS Code-style regex rules. 14 indent-only tree-sitter grammars were replaced by these rules, shrinking the binary by ~18 MB.
* **GDScript** language support (#2238, by @richiehowelll).
* **`lsp_enabled` master switch** to disable all LSP features globally (#1770, requested by @XhstormR).
* **`auto_read_only` option** to turn off automatic read-only mode for foreign files (#2048).
* **Occurrence highlighting toggle** with configuration (#2154, by @masmu), and the current-line highlight now hides while text is selected (#2153, by @masmu).
* **Clear Search action** and a plugin API exposing the active search state (#2152, by @masmu).
* **File Explorer slot override API** for plugins: icons, status, and name color (#2241, by @Agrejus).
* **'+' new-tab button** in the tab bar, with a New Terminal / New File popup.
* A **color-transition animation** on theme switch, and a **wave animation** — available as a command and as an optional idle screensaver.

### Improvements

* **Live Diff**: word-level highlighting inside changed line pairs (#1949, requested by @masapu).
* **Status bar**: configurable separator with dedicated theme keys; the default separator is now padding-only and the default left side leads with `{trust}`.
* **Orchestrator dock**: seamless-tab look for the active session, a hover-only overlay scrollbar, a stable session order, keyboard-navigable project dropdown, and clicking an inactive worktree opens it without an extra confirmation.
* **File Explorer**: natural-order filename sorting (#2073, requested by @mandolyte).
* `q` closes the *Keyboard Shortcuts* and *Fresh Manual* viewers (#2165).
* Undoing a format-on-save or trim rewrite keeps the view in place (#2027, requested by @SolarLune).

### Bug Fixes

* Windows: the TUI attaches to the parent process's stdio instead of failing in `-gui` builds (#2276, reported and fixed by @mokurin000 in #2277).
* Right-side status bar elements render with configurable separators (#2088, reported and fixed by @PavelLoparev), and cursor column numbers are grapheme-correct (#2090, reported and fixed by @PavelLoparev).
* Scroll panels compute focus offsets from the render width when a scrollbar is present (#2175, by @masmu).
* Restored windows no longer show a blank editor pane from an orphaned split leaf (#2267).
* Paste reaches Settings/dialog text fields instead of the buffer behind them, and buffer pastes work again after closing the dialog (#2246).
* Tab bar scroll re-anchors after closing many tabs, keeping the surviving tab visible (#2229, reported by @dmknght).
* LSP code actions now include diagnostics context, so diagnostic-based fixes appear (#2212).
* Devcontainer CLI detection works when the CLI is installed via Bun on Windows (#2201, reported by @steve-price-immybot).
* `install.sh` / AUR `fresh-editor-bin` packaging fixed (#2249, reported by @asukaminato0721; #2250, reported by @bandrefilipe).
* Focusing a restored terminal tab activates terminal mode.

## 0.3.12

### Features

* **Orchestrator Dock**: a persistent, non-modal left-column session switcher. Alt+O toggles focus; arrows live-switch the active session. Each row shows status (working/idle), project, branch, git summary, and a PR badge. Project dropdown, card/compact view toggle, and a Manage button for the Orchestrator dialog.

* **Create SSH sessions from within the UI**: You could already open a remote host from the cli, now you can do it via the Orchestrator: New dialog - it has a type selector (Local / SSH, and experimental Kubernetes / Devcontainer). Like the cli invocation - SSH attaches a full remote session:filesystem, LSP, process spawners, and a terminal on the remote host; switching retargets without a restart.

* **Kubernetes sessions** (initial, experimental): connect over `kubectl exec` to any cluster, with a keepalive heartbeat and reconnect.

* **Go to LSP Symbol**: a symbol finder with live preview, a precise jump to the symbol name (line and column), source-line snippets, and preselection of the symbol under the cursor (#1886, by @PavelLoparev).

* **Terminal tab auto-naming**: tabs follow the foreground process and OSC title. Setting `editor.terminal_auto_title` (on by default).

* **Open file from a diff**: in the side-by-side and review-diff views, Enter opens the working-tree file (NEW pane) or the read-only HEAD version (OLD pane) at that line.

* **Rainbow bracket colorization** for matching brackets across the viewport (#1088, by @asukaminato0721).

* **Minimal static Linux binary** release artifact (musl, x86_64 and aarch64) optimized for a small binary size.

### Improvements

* **Orchestrator Dock right-click menu**: right-click a session row for an unobtrusive popup anchored at the cursor with Visit / Archive / Delete. Archive and Delete open a centered, full-screen-dimmed confirmation before they run.
* **`variable.builtin` syntax category** for `this` / `self` / `super` (#2150, by @masmu).
* **`storage.type`** keywords highlight as keywords rather than types (#2151, by @masmu).
* **Async clipboard paste**: paste no longer blocks the editor, and a hung X11/Wayland clipboard owner should no longer freeze it (#2155).
* **Nested `fresh` launches** (`$EDITOR`, `git commit`) inside Fresh's terminal open in the parent editor instead of a second editor; falls back to inline if the parent is unreachable.
* **Python indentation**: Enter after `return` / `pass` / `raise` / `break` / `continue` dedents out of the block, and an extra indent level after a statement inside a block is fixed (#2192, reported by @WorldChallenge1).
* **Terminal scrollback** survives resize, `clear`, and alternate-screen programs; the scroll-back view soft-wraps long lines.
* **SSH remote editing** is documented in `fresh --help`.

### Bug Fixes

* **Inlay hints** participate in line wrapping and horizontal scroll (#2190, reported by @TakemiSora).
* **LSP capabilities**: dynamic `client/registerCapability` is now applied, and `workspace/configuration` and the diagnostic / inlay-hint / semantic-token refresh capabilities are advertised, so more servers pull config and re-pull stale results.
* No crash on a malformed mouse input sequence (#2234).
* Failed SSH/Kubernetes connects show their error instead of corrupting the screen.
* Fixed stuck keyboard input after using a session terminal and then opening a file (#2237, #2234).
* Fixed a client/server freeze when copying a large terminal-scrollback selection.
* File Explorer no longer re-roots the wrong window when previewing a restored session.
* Every session is archivable and deletable, including the launch session and the last window.
* The dock's working/idle indicator tracks the right session and idles when output stops.
* A worktree session in a repo with no commits reports git's real error.
* Text-input fields in dialogs focus on click (#2234).
* Diff "removed" lines no longer wrap one character per row at narrow widths, or when a language overrides the wrap column to 0 (#2177, reported by @biscuitvicious).
* LSP hover popups no longer appear while the LSP status popup is open (#2244, reported by @biscuitvicious).
* `editor.scroll_offset` is respected (#2162, reported and fixed by @PavelLoparev).
* Windows: long paths middle-truncate, and short-name / `\\?\` path differences no longer reorder the dock.
* Fixed a marker-tree corruption that could drop edits to markers and highlights.

## 0.3.10

A lot of work on bug fixes and polish in this release.

### Improvements

* **Live Grep**: previews highlight all matches and center on the match in wrapped documents; keyboard nav re-reveals the selection after a wheel scroll; prompt text is readable on all themes (new `suggestion_fg` theme key) and toolbar labels readable on light themes.
* The **Workspace Trust** prompt and the **env-manager** plugin's commands and status messages are now localized across all 14 supported languages (#2158).
* Session model reworked: **one session per directory**, `windows.json` dropped (sessions are discovered from the per-dir workspace cache), only the foreground window is materialized at startup with the rest lazy-restored, and the **base session is no longer magic — it can be deleted** like any other.
* Orchestrator picker hides **empty / single-file sessions** by default, with a toggle to show them (#2137).
* The Live Grep => Quickfix (persisted) list is now a **Finder dock panel** instead of a bespoke buffer: **Enter navigates** to the match, the dock stays open when jumping, and `live_grep_export_quickfix` is bound by default (#2124).

### Bug Fixes

* **Mouse-wheel scrolling** fixed in File Explorer, Live Grep, and Git Log panels (#2119, reported by @brunnerh in #2107).
* **Search highlights** no longer grow over adjacent typing (#2053, reported by @mandolyte) and stale matches now clear after edits inside a match (#2133).
* **Search / Replace**: panel no longer gets stuck on "Searching…" with zero matches; *Search and Replace in Current File* now works for unnamed buffers and for files outside the workspace root (#2112).
* **LSP**: diagnostics now display in Markdown compose/preview mode (#2146); `workspace/configuration` is answered per-server instead of with a fixed rust-analyzer blob; newly configured servers default to `auto_start = true`.
* **Diagnostics panel** `q` / `a` / `r` shortcuts work again instead of tripping "Editing disabled" (#2125).
* **Review Diff**: *Discard hunk* now succeeds for files with an unterminated final line (#2117).
* **Macro replay** is now a **single undo unit** instead of per-char (#2062).
* **Submenus** align so the first item is inline with the selected parent item (#2118, reported by @RandomGHUser).
* **File Explorer**: `Ctrl+O` works and keeps focus on the opened buffer.
* **Settings**: text fields commit and advance focus on `Enter`; `Ctrl+U` clears the prompt.
* **Gutter** no longer eats text width in non-compose mode when line numbers are off.
* **Windows**: the Microsoft Store `pwsh` App Execution Alias is skipped when opening a terminal (#2077, reported by @ket000), with an opt-out flag.
* Session working trees stored under the data dir are now editable.

## 0.3.9

### Features

#### Universal Search (multi-scope Live Grep)

Live Grep grows into a universal search overlay: search across multiple **scopes** — project files, open **Buffers**, and **Terminals** scrollback — with **Word** and **Regex** search modes (each keybindable). The overlay gets a clickable widget toolbar (focusable provider button, toggle controls), a full-width header band, and inline status shown in the overlay itself. Closed-terminal scrollback is retained so it stays searchable (#2099).

#### Orchestrator & git worktrees

* **Attach sessions to existing git worktrees** and discover them automatically (#2095).
* Multi-select **bulk actions**, worktree ordering, and a *Show all worktrees* filter toggle.
* Draggable scrollbars on the picker lists, with bulk-pane reorder.

#### Editor

* **Move to Next / Previous Paragraph** actions, including translations (#2084, contributed by @PavelLoparev; requested in #2083).

#### Language Support

* **C3** (`.c3`) syntax highlighting and LSP (#2101, requested by @data-man).

#### Plugins & API

* New `getWorkingDataDir()` (per-working-dir data root) and `getTerminalDir()` plugin APIs.
* Overlay toolbar widget APIs — clickable `Toggle`/`Button` controls, `toggleOverlayToolbarWidget`, and a `Row { wrap }` layout that reflows toolbars across lines.

### Improvements

* The **dashboard no longer opens automatically** on startup by default. To re-enable it, open **Settings**, select **Plugin: dashboard**, and turn on **AutoOpen** ("Show the dashboard automatically when Fresh starts with no real files open."). The dashboard remains available any time via the **Show Dashboard** command.
* Homebrew install simplified now that fresh-editor is in homebrew-core — `brew install fresh-editor`, no tap step.

### Bug Fixes

* Plugin `getCursorLine()` now returns the real cursor line instead of `0` (#2076, reported by @pmburov).
* **Reduced serial-console lag** by not repainting on non-visual plugin async events (#2100, reported by @jetpax).
* **Custom theme colours** no longer silently ignored for many UI fields (#2080, contributed by @flexiondotorg; reported in #2079).
* `PageDown`/`PageUp` no longer overshoot on a single soft-wrapped line (#2085).
* **Workspace restore** no longer mixes tabs from multiple projects in one window (#2056, reported by @mandolyte).
* **Live Grep**: *Resume* keeps the query and opens the selected result; overlay preview and input-box undo/redo corrected; Buffers scope finds unmodified open buffers; non-file-scope previews and read-only data-dir files fixed.
* `getKeybindingLabel` resolves correctly for bound plugin actions.

### Internal

* Major window refactor: window-scoped save/restore (`Window::from_workspace`), `WindowId`-parameterized persistence, `working_dir` derived from the active window, and `open_file`/LSP/watch helpers moved onto `Window`.
* Orchestrator bring-up characterization tests and fixtures across persistence layouts.
* Serial-console lag benchmark/diagnostic scripts; theme-key resolver schema-drift guard.
* Internal docs excluded from the public docs build.

## 0.3.8

### Features / Improvements

#### Environment Managers (venv / direnv / mise)

New built-in **environment-manager** plugin: it detects a project's environment manager — `.venv`/`venv`, `.envrc` (direnv), or `mise.toml`/`.tool-versions` — and, via **Env: Activate**, injects that environment into every editor-spawned process (LSP, formatters, terminals, `spawnProcess`). **Env: Use System (Deactivate)** restores the system environment, and **Env: Show Status** reports the current state. Activation is on-demand (not automatic) and respects Workspace Trust. Adds an opt-in `env` status-bar element.

#### Remote Development

* **LSP over SSH** now runs the language server *on the remote host* instead of falling back to a host-local process, so completions and diagnostics reflect the remote toolchain.

#### Workspace Trust (groundwork)

Foundational **Workspace Trust** support — a per-project trust level (persisted in the project state dir), a *Set Workspace Trust Level* command-palette control, and a `workspaceTrustLevel()` plugin API. The enforcement gate is **off by default this release** (no prompt on open; undecided folders are treated as Trusted) while the trust UX is reworked around sandboxed execution — so there is no behavior change unless you set a trust level yourself.

#### Orchestrator

* The **Open** dialog is now scoped to the current project by default, with a visible scope toggle to reach other projects, a tabular picker, and `/` to filter.
* New windows open atomically with their terminal (no `[No Name]` placeholder), and terminal output shows immediately.

#### Settings UI

A broad pass on the Settings editor: **number fields now accept direct typing** instead of `[-]`/`[+]` spinners; list editing gets inline `[+] Add new` / `[x]` rows, mouse support, and `Del` to remove; **`Ctrl+R` resets a field to its default**; the focused field's description is shown above the dialog buttons; and confirmations are required before deleting or discarding dirty edits.

#### Editor

* **Next / Previous Window** commands to cycle open windows without the Switch-Project picker (#2031).
* **Move File Explorer to Other Side** command, persisted to config (#1468, requested by @asukaminato0721).
* **Templ** (`.templ`) syntax highlighting (#463, requested by @TS22082).
* New `editor.confirm_quit` setting (default off) to prompt before quitting a clean session.

#### Plugins & API

* Built-in `editor.httpFetch(url, path)` for downloading files — `curl` is no longer bundled.
* "Install from URL" accepts direct `file://` URLs.

### Bug Fixes

* **Quit binding overrides honored** (#2030): binding `Ctrl+Q` to `noop`/`none` now actually disables it in every context.
* **Terminals**: line-number gutter and current-line highlight no longer leak on terminal exit; scroll-back viewport anchors correctly; `Shift+Tab` is forwarded to the child as `ESC[Z` (#2029).
* **File explorer keeps keyboard focus** when leaving a live terminal (#2029).
* **Closed terminals no longer reappear**, and buffer groups stay visible when a split closes (#2027, reported by @SolarLune).
* **Git Log**: selected commit stays aligned with the cursor; selection follows scrolling and row clicks.
* **Adjacent tab indicators** no longer double up (#1997, reported by @brunnerh).
* **Theming / Live Diff**: light-theme dim fixes, markdown popup body text inherits the terminal fg, and diff lines preserve syntax foreground.
* Tree-sitter `.scm` query files are included in the Nix source filter, fixing a source-build failure (#2055, reported by @melekbadreddine).

## 0.3.7

Introduces an experimental **multi-window orchestrator** command (`Alt+Q`) - a UI that manages multiple Fresh windows side by side in a single Fresh process.

### Features / Improvements

#### Orchestrator (early experiment)

The new orchestrator command (`Alt+Q`) lets one Fresh process juggle several independent windows — switch between sessions, open new ones from a project path / branch, or visit one in a separate Fresh process. This is an early experiment; expect rough edges. Please share your ideas and bug reports!

#### Search and Replace

Made an effort to improve the search & replace command, tackling several issues:

* **All Files** scope toggle + dedicated current-file command.
* Editor shortcuts (`Ctrl+P` etc.) pass through the panel.
* Proper empty / searching / no-results states.
* Large result sets stay responsive — incremental tree, throttled streaming, capped per-iteration work.

#### Git Log / Review Diff

* **Performance**: Per-commit diffs **stream into file-backed buffers** with full syntax highlighting, per-line add/delete backgrounds, working scrollbar, and `Fold by File` / `Fold by Hunk` commands. Now opens the famous "rewrite bun in rust" commit instantly!
* **Open file at commit** (#1962): jump from a diff line in the review-branch detail panel to the file at that commit (read-only, `q` to go back).

#### File Explorer

* **Compact directories** (#1406, thanks @Nandaleio!): a directory containing a single child directory renders inline as `com.example.name`, VSCode/IntelliJ-style. On by default; toggle with `file_explorer.compact_directories`.
* **Configurable tree indicators** (#1940, reported by @MicTheSquid): expand/collapse glyphs themable via *File Explorer → Tree Indicator Expanded / Collapsed* in the Settings UI.

#### Plugins & API

* **`git_statusbar` plugin + status-bar token registration API** (thanks @PavelLoparev!): show the current git branch in the status bar; plugins register their own tokens via `editor.registerStatusBarElement`. Add via *Settings → Editor → Status Bar → Left / Right*.
* **`tab_actions` plugin** (#1844, thanks @PavelLoparev!): close all / to-the-left / to-the-right and move tabs within the current split.
* **Plugins can register their own config items**: a new typed registration API lets plugins declare settings that appear in the Settings UI alongside built-in ones (sorted at the bottom of the category list), so users can toggle plugin behaviour from the same place as everything else. The bundled `vi_mode` plugin uses it to expose an **autoStart** option.

#### Editing & Widgets

* **Add Cursors to Line Ends** (#1870, reported by @aquasync): places a cursor at the end of every line in the selection.
* Widget text inputs gain real selection rendering, `Shift+arrow` / `Ctrl+Shift+arrow` extension, and `Ctrl+C/V/X/A` — used by Search & Replace and plugin dialogs.
* Floating completion popup on widget `Text` inputs.

#### Live Diff

* Word-level underline highlights on deletion lines; similarity threshold and size caps are runtime-tunable, so large diffs render as modified lines rather than paired add/remove blocks.

### Bug Fixes

* **`init.ts` `setAutoOpen(false)` ignored**: dashboard auto-open is now driven by the `ready` hook so user `init.ts` runs first.

#### Git Log / Review Diff

* **Detail panel `PageUp` / `PageDown`** (action name was wrong); scrollbar now functional on the streamed buffer.

#### File Explorer

* **UI / navigation shortcuts** (#1903): `Ctrl+P` and friends are no longer swallowed when the file explorer has focus.
* **Switch Project double-click** (#1931, reported by @SolarLune): double-clicking a directory navigates into it instead of dismissing the dialog.

#### Plugins

* **Plugin init scaffold** (#1986, @PavelLoparev): new plugins now generate `fresh.entry` (the current entry-point name) instead of stale `fresh.main`.
* **Idle CPU from git plugins** (#2009, #2012): `git_statusbar` and `merge_conflict` no longer spawn idle subprocesses; `git_statusbar` is driven off `watchPath(.git/HEAD)`.

#### Editing

* **Smart Home with multiple cursors**: now runs for every cursor, not only the primary.
* **Soft wrap at trailing space** (#1363, reported by @dragonfyre13): when a trailing space would overflow the wrap column, wrap backs up one word instead of breaking mid-token.
* **Horizontal scroll-to-view on long lines** (#1873): the viewport now follows the cursor on long lines.

#### Live Diff

* **Wrapped overlay backgrounds** now extend across every visual row of a wrapped line.

#### Rendering

* **Highlighter cache preserved across bulk edits** (#1958): large pastes and replace-all no longer trigger a whole-file reparse.
* **Phantom cursor offscreen** (#1965): a buffer cursor scrolled past the viewport no longer leaves a stray block at the screen edge.
* **Split separator background** (#1963): separator cells now paint with `editor_bg` so split borders blend with the panes.
* **Next / Prev Split un-maximizes** (#1961): navigating to another split un-maximizes first.
* **Stable cursor-position indicator** (#1967): `Ln L, Col C` no longer jitters as the column count grows.
* **PTY terminal background** now fills the render rect with the editor bg.
* **Nostalgia theme** (#1890, reported by @NGRIT41): terminal-pane blue now covers full pane.
* **Column rulers** suppressed on virtual buffers (dashboard, PTY panes, etc.).

#### Misc

* **No more stray `.fresh/` directory in your working dir** (#1991): orchestrator/cross-restart state now lives under `data_dir` instead of being dropped next to your project files.
* **Buffer "library path" detection** (#1970, reported by @FF-AntiK): only `.cargo/registry` and `.cargo/git` are treated as Cargo library paths — local `.cargo/config.toml` etc. are no longer flagged.
* **Julia grammar** (#1852, reported by @goszlanyi): the adjoint operator (`'`) no longer flips the rest of the line into string mode.
* **`move_word_end` / `select_word_end` labels** (#1878, reported by @sour-dani): translated in all locales.

## 0.3.6

This version includes a major internal refactoring to support multiple windows in a single Fresh process. The work will be used to add a multi-window orchestrator in a future version.

### Features

* **Go to line with selection** (#1764, thanks @PavelLoparev!): New action extends the selection from cursor to target line. Supports absolute and relative (`+10`) jumps.

* **File explorer "follow active buffer"** (#1569, #1803, thanks @ko3n1g!): New `file_explorer.follow_active_buffer` setting (default off). When on, switching tabs expands and highlights the corresponding file without stealing focus.

* **Live Diff**: Near-identical lines now render as modified lines, with new words decorated as underline (instead of two separate add/remove rows). Deletion virtual lines get gutter markers. Fixed live diff colors in `terminal` theme.

* **Auto-start `vi_mode`** (#1086, reported by @jmcblane): Vi mode plugin now exposes `toggle / enable / disable` so `init.ts` can flip vi mode at startup. Add this to your `init.ts` to enable: `editor.getPluginApi("vi-mode")?.enable();`.

In the future I plan to add a way for plugins to register custom config sections in the Settings UI, and then enabling vi-mode auto-start will be available through the UI.

### Improvements

* **openSUSE install instructions** (#1897, thanks @ilmanzo!) added to the README.

* **Quick Open ranking**: Matches at the start of a path segment now beat arbitrary substring hits — `ts` surfaces `tsconfig.json` and `tsc/...` before random `.ts` files.

* **Search & Replace no longer freezes** on result sets in the thousands.

* **Rust code actions** (#1915): `rust-analyzer` now returns `WorkspaceEdit`-based assists like "Fill struct fields" instead of "No code actions available" — Fresh wasn't advertising the capability.

* **Status-bar indicator menus**: Clicking another indicator's icon dismisses the currently-open menu instead of stacking a second one on top.

* **LSP-Servers popup is extensible**: Plugins (starting with the bundled `rust-lsp`) can contribute their own rows.

* **`terminal` built-in theme** (#1914): Diff backgrounds no longer collide with syntax foregrounds, so keywords on deletion lines and strings on addition lines stay readable on terminals (e.g. xfce-terminal) where each ANSI palette index renders identically for fg and bg.

* **Dashboard** matches the editor background and no longer slides in.

* **AppImage install script** has error handling and a `/tmp` fallback so it no longer silently fails when the target directory isn't writable.

### Bug Fixes

* **Crash on startup from a corrupt workspace** (#1939, reported by @zdooder): Restoring a session whose persisted split pointed at a buffer no longer in the buffer map crashed in `render.rs`. The active-buffer pointer is now validated and falls back to a live buffer if it's stale.

* **Completion popup + multicursor** (#1901, reported by @dtwilliamson): Typing a word character or `Backspace` while the popup was open only edited the primary cursor — secondary cursors silently went out of sync after the first keystroke. Accepting a completion now also applies at every cursor.

* **LSP indicator click** (#1941): Bugs fixed: plugin popup stacked under the built-in one; plugin popup ignored the active theme's `popup_bg`; state stayed stale after the LSP process was killed externally; "Disable LSP" didn't actually stop the running server; repeated clicks stacked further popups.

* **Scrollbar theme overrides** (#1554, reported by @klonuo): User themes setting `ui.scrollbar_track_fg`, `ui.scrollbar_track_hover_fg`, or `ui.scrollbar_thumb_hover_fg` had their overrides silently dropped.

* **Utility dock routing** (#1932): `Ctrl+P` → filename now opens the file in the editor area instead of inside the bottom dock when the dock has focus. The split tab-bar `□` button now maximizes the split you clicked, not the active one.

* **Tab drag**: Dropping a tab onto a fresh split (e.g. a Search & Replace result row) no longer panics on the next keystroke.

* **File explorer**: A directory whose `.gitignore` filters out all its contents (e.g. `build/` with `*` inside) stays visible when expanded.

* **Live Diff visual-line motion**: `Down`/`Up` no longer freezes when traversing a deletion block that starts with a blank line.

* **`S-Tab` plugin bindings** now register correctly (terminals deliver Shift+Tab as `BackTab`).

## 0.3.5

### Improvements

* **Live Grep overlay polish**: Surrounding editor is now visibly dimmed; shortcut hints and the active provider moved into a toolbar row with plainer labels; match cap raised from 100 to 1000 (with a `1000+ matches` indicator); provider errors render as a disabled result entry instead of a silent "0 matches"; `ripgrep` provider renamed to `rg`.

### Bug Fixes

* **Live Grep on Windows returned no results**: `git grep` outputs `\r\n`; splitting on `\n` left a trailing `\r` that broke the result regex. Split on `\r?\n` instead.

* **Plain `grep` provider was broken**: `--column` is a ripgrep-only flag and was being passed unconditionally. Removed.

### Under the Hood

* **Plugin API: `setPromptTitle` takes styled segments** (`{ text, style? }[]`) instead of a single string, so plugins control hint colouring directly instead of the renderer guessing structure from punctuation.

## 0.3.4

### Features

* **Live Grep floating overlay + Utility Dock** (#1796): Live Grep now opens as a centered floating overlay with results on the left and a real-buffer file preview on the right (full syntax highlighting, gutter, soft-wrap). `Esc` returns you to your prior layout exactly. **Resume** (`Alt+r`) reopens the last query with cached results. **Export to Quickfix** (`Alt+M`) sends results into a dockable list.

* New **Utility Dock** at the workspace root hosts the terminal (`` Alt+` ``), Quickfix, Diagnostics, and Find References — they share one pane spanning the full width instead of nesting under whichever split was focused.

* **Pluggable Live Grep providers**: Built-in chain is now ripgrep → **git-grep (default in repos)** → grep, with `ag` / `ack` available via plugin registration. `Alt+P` cycles to the next available provider; the active one shows in the overlay's title bar. Plugins can register custom backends via `editor.getPluginApi("live-grep")`.

* **Settings tree-view**: The left category list is now an expandable tree — categories with multiple sections show chevrons, expanding reveals jumpable section rows, and the tree cursor follows scrolling so you can see where you are in the body. Section jumps snap to the top of the section. Toggle controls render as a chip-style `[ ✓ ACTIVE ]` indicator.

* **HDL language support** (#1528, reported by @bqinTT): Syntax highlighting for **Verilog** (`.v` left mapped to vlang for compatibility, `.vh`/`.verilog`), **SystemVerilog** (`.sv`/`.svh`/`.svi`/`.svp`), and **VHDL** (`.vhd`/`.vhdl`/`.vho`). `svls` wired as the default LSP for Verilog/SystemVerilog (opt-in per project).

* **New `terminal` built-in theme** (#1457, #1798, reported by @AmethystGosling169 and @BrettKinny): Colors come from your terminal's own palette instead of hard-coded RGB — backgrounds use `Default` so transparency and your terminal's background show through; accents use ANSI named colors that remap to whatever your terminal colorscheme defines. Selection uses reverse-video so it inverts whatever colors are already on screen.

* **Theme inheritance with `extends`**: User themes can now `extends: "builtin://light"` (or `dark` / `high-contrast` / `nostalgia` / `terminal`) and layer overrides on top — the same model VSCode/Helix/Sublime/Zed use. With no `extends`, an explicit `editor.bg` triggers luminance-based auto-inference (bright bg → light base, dim → dark), so partial light themes no longer end up with dark UI chrome (#1281, reported by @nico2004444).

* **File explorer context-menu additions** (#1576, reported by @RandomGHUser): **Duplicate** (creates `name copy[.ext]` next to the source, multi-select supported), **Copy Full Path** and **Copy Relative Path** (newline-joined for multi-select). The new entries don't appear on the project root.

* **Distribute clipboard across cursors** (#1057, reported by @graphixillusion): With N cursors and an N-line clipboard, paste now gives each cursor one line in top-to-bottom order — VSCode / Notepad++ "column-mode paste" semantics. A block-selected copy/paste round-trip preserves its rectangular shape. Behavior is unchanged when counts don't match.

* **Discard option in quit prompt** (#1839, reported by @turkishmaid): With `hot_exit` on (the default), the unsaved-changes prompt now offers "(d)iscard and quit" so accidental edits no longer require disabling hot_exit globally to throw away. Picking "save" on quit also chains a Save As prompt for each dirty unnamed buffer instead of silently dropping it.

* **Project name in window title** (#1793, reported by @dAnjou): Title is now `<file> — <project> — Fresh` so multiple Fresh sessions in different projects are distinguishable in your taskbar/window list.

* **Cursor-jump animation toggle** (#1788): New `editor.cursor_jump_animation` setting lets you keep ambient animations (tab slides, dashboard) while disabling just the cursor-jump trail. The master `editor.animations` setting still wins.

### Improvements

* **Search & Replace across project no longer hangs on large binary files** (#1342, reported by @dragonfyre13): Hardcoded extension fast-path skips known-binary files (compiled artifacts, archives, media, ML weights, fonts) before any I/O. Per-file size cap and stronger header sniff (PNG, ZIP-based archives like `.pth`, ELF) catch the formats whose first bytes can look text-like.

* **POSIX ACL writability** (#1765, reported by @cherouize): A file granted write access via `setfacl -m u:NAME:rw` is no longer reported read-only — Fresh now asks the kernel via `faccessat(W_OK)` instead of walking inode mode bits, so ACLs, capabilities, and read-only mounts are all honored.

* **LSP status popup doesn't auto-show**: Auto-popping on first file open stole focus and swallowed keystrokes for users who hadn't asked to enable LSP. The `LSP` indicator is now a manual click target; its `Off` state (configured but not running) is rendered with a more prominent attention-grabbing color so discoverability isn't lost.

* **Hover popup no longer flickers on mouse moves** (#692) inside the editor (gutter, end-of-line, between words). It only dismisses when the mouse leaves the editor area entirely. New hover responses replace the existing popup instead of stacking.

* **Multi-cursor `Ctrl-D` after substring search** (#1697, reported by @dtwilliamson): When the cursor is inside an active search match, "Add cursor at next match" selects the next *search match* instead of expanding to the surrounding word.

* **JavaScript syntax highlighting** (#899, reported by @comesuccingfuccsloot): Routed through tree-sitter, so template literals containing arrow functions or `${expr}` no longer leak `@string` styling across the rest of the file.

* **Smarter auto-indent for Lua / Ruby / Bash / Pascal**: Tree-sitter `indents.scm` is now the single source of truth for keyword-delimited languages, so `(` opening a function call no longer gets treated as a block-opening delimiter.

* **Enter at column 0 doesn't push the line right anymore** (#1425, reported by @goszlanyi): Auto-indent now detects "cursor at column 0 of a non-empty line" and inserts a bare newline. Closing-delimiter lines still get the established indent-before-close behavior.

* **Live Diff virtual lines soft-wrap** (#1787) instead of being truncated at the right edge.

* **Per-workspace hot-exit recovery** (#1550, reported by @goszlanyi): Standalone-mode recovery files are now scoped per working directory. Quitting Fresh in folder B no longer wipes folder A's recovered unnamed-buffer state.

* **Terminal PTY resyncs on tab reveal** (#1795): Resizing the host while a terminal was hidden behind another tab now correctly forwards `SIGWINCH` when you switch back — `$COLUMNS` / `stty size` stay accurate.

* **Open File dialog scrolls correctly on small terminals** (#245): Selection no longer slides past the bottom of the visible list.

* **Keybinding editor scrollbar responds to mouse** (#1593, reported by @Kodiak-01): Click and drag both work; wheel scrolls the viewport instead of moving the selection (so a scrollbar drag isn't undone by the next wheel tick).

* **Settings Number controls** (#1825, e.g. Tab Size): Tab now commits and exits the input; clicking the value cell enters edit mode (matches Enter).

* **Plugin keybinding labels refresh on every prompt open** so plugins surfacing key hints ("`Alt+P` to cycle", overlay headers, etc.) reflect mid-session rebinds without restart.

* **New plugin hook `after_file_explorer_change`** fires on FS-mutating explorer actions (Duplicate, Paste, New File, Rename, Delete) so plugins like git_explorer can refresh badges immediately.

* **Theme picker consistency**: The Theme Editor plugin's picker now shows the same set of themes as the native `Select Theme` prompt — no more divergence from a separate `cwd/themes` scan or normalization mismatches. New plugin API `editor.getAllThemes()` returns the cached map directly so plugins can drop their own filesystem walks.

### Bug Fixes

* **Crash fixes**: `Option::unwrap()` panic when pasting in the Theme Editor (event apply used the wrong split). `DeleteBackward` panics on stale cursor state in vi-mode count prefixes and plugin action batches. Theme editor crash on the new `terminal` theme's modifier fields. Embedded-plugin extraction race across concurrent test processes.

* **Search** (#1537, reported by @pstahle): `Find Selection Next/Previous` on a non-word character (e.g. `}` after goto-matching-bracket) no longer hijacks the search query — it now navigates the existing search instead.

* **OpenLine (Emacs `C-o`)**: Cursor stays on the original line instead of advancing — was previously indistinguishable from Enter.

* **Markdown compose** (#1789, #1790): Wrap budget widened by one column to prevent orphan-word re-wrapping on Windows; current-line highlight now extends across soft-wrapped sub-rows.

* **Viewport** (#1794): Popup anchoring counts true visual rows under wrap, so completion popups appear next to the cursor instead of several rows above in heavily-wrapped buffers.

### Under the Hood

* **Smaller release binaries**: New `dist` cargo profile (`strip=true`, `lto=fat`, `codegen-units=1`, `opt-level=z`) is now applied to release builds. Binary shrinks from 62.4 MB → ~46 MB (-26%). Backtraces are still included in panics.

* **Build performance**: `oxc` and `rquickjs` now build at `opt-level=3` in dev/test profiles to keep iteration fast despite their size.

* **Semantic test framework + ~250 migrated cases**: A new "scenario" test layer dispatches `Action`s straight against an isolated editor instance, skipping plugin loading where the assertion surface (buffer text + caret) can't observe it — about 440 ms saved per test harness. ~250 e2e claims have been migrated across multicursor, block selection, auto-indent, paste, undo/redo, search-modal flows, multibyte handling, and many regression repros (issues #191, #1147, #1305, #1574, #1697, etc.). Property-based theorems over generated action sequences caught two real production crashes in no-render dispatch paths (vi-mode count prefixes, plugin action batches) — both fixed in this release. A pre-existing race in concurrent embedded-plugin extraction (could leave half-written plugins for parallel test processes) is also fixed.

## 0.3.2

### Features

* **Live Diff plugin** (experimental): Unified-diff overlay rendered live in the editable buffer. If your file is unmodified in the editor, it updates as the file changes on disk when auto-revert kicks in - great for watching an agent edit your file. Opt-in via `Live Diff: Toggle (Global)` / `Live Diff: Toggle (Buffer)`. Reference selectable per buffer: `vs HEAD` / `vs Disk` / `vs Branch...` / `vs Default Branch`.

* **New Startup section in Settings** (open Settings and search "Startup") groups everything that fires on launch:
    - **Blank-workspace flow** (#1753) — *Auto Create Empty Buffer On Last Buffer Close* (Editor) and *Auto Open On Last Buffer Close* (File Explorer). With both off, closing the last buffer leaves a truly blank pane (no `[No Name]`, no gutter, no `~`); buffer-specific status-bar items and menu entries are suppressed, and a subdued centered hint shows the keys to escape (`Ctrl+P` / `Ctrl+O` / `Ctrl+E`).
    - *Skip Session Restore When Files Passed* — `fresh src/main.rs` opens just that file; bare `fresh` and `fresh some/dir` still restore. Hot-exit recovery still runs. `--restore` overrides.
    - *Restore Previous Session* (existing, moved into Startup).

* **File explorer side** (thanks @paveloparev!): *Side* under File Explorer in Settings — left or right.

* **Prompt Line now hidden by default**: *Show Prompt Line* now defaults off — the prompt line only appears while a prompt is active. Turn it back on via Settings.

* **Mark mode preserved through Go to Line** so you can extend selections across the jump. Use **Set Mark** command followed by **Goto Line** to start a selection and extend it to the target line.

* **Copy File Path commands**: New commands: "Copy File Path" and "Copy Relative File Path" to copy current buffer's path to your clipboard. Also available by right-clicking on a tab name.

* **CLI Help Localization**: The `--help` output is now fully localized using runtime i18n lookups.

* **Relative +/- Goto Line**: Infers absolute vs relative jumps from a leading sign (e.g., `:+10` jumps 10 lines down, `:10` jumps to line 10).

* **Rust Toolchain Update**: Updated to Rust 1.95 in `rust-toolchain.toml` to fix compatibility issues with newer LLVM/clang versions on systems like Arch Linux (#1782).

### Improvements

* **Plugin loading deferred off the boot critical path** — another ~225 ms saved. Same load order, same hooks, just async.

* **Popup focus**: LSP popups that auto-show on file open (status popup, hover, signature help, plugin Text overlays) no longer steal the next keystroke. They show unfocused with an `[Alt+T to focus]` hint; user-invoked popups (Completion, code actions, status-bar `{remote}`, LSP-status menu) still grab focus on show. Settings / Menu / Prompt modals take precedence over unfocused buffer popups for `Esc` / `popup_focus`.

* **Status Bar visual integration** (#1711): The "Palette: Ctrl+P" hint and "LSP (on)" indicator now blend into the status bar by default. Built-in themes have been updated with coherent prominent colors for these indicators.

* **Prompt interaction and scrolling** (#1660):
    - Minimal scrolling: the suggestion list no longer recenters the selection on every move, preventing "row jumping" during navigation.
    - Clicks no longer cause accidental list shifts; double-click correctly confirms selections.
    - Preview-on-click supported for "Reload with encoding".

* **Global Menu Bar**: "Toggle Menu Bar" state is now persisted globally across all workspaces.

* **Windows integration**: High-quality app icon applied to the running window; app manifest and version info embedded in the binary.

### Bug Fixes

* **Windows subprocesses**: Transient console windows are now hidden when spawning subprocesses (e.g., formatters, linters).

* **Terminal CWD**: Fixed shell spawning failure on Windows when the current directory has a `\\?\` UNC prefix.

* **Live Diff stability**: Fixed crashes on surrogate-pair content (emojis) and corrected gutter rendering for empty lines inside added blocks.

### Under the Hood

* **Syntax Highlight Caching**: New multi-phase caching system (memoised scope lookups and whole-file cache for small files) significantly reduces CPU usage during rendering.

## 0.3.1

### Features

* **Animations framework**: tab-switch slide; cursor-jump trail animation. Disable it via Settings UI. Animations available to plugins via API.

* **Flash plugin** (label-jump, à la flash.nvim): bundled plugin — type a pattern, press the displayed label to jump, even across split panes.

* **Devcontainer fixes**: See below

### Improvements

* **Relative-numbers go-to negative** (thanks @paveloparev!): `g<-N>` jumps N lines up in relative-numbers mode.

* **Racket language support**: `.rkt` / `.rktd` / `.rktl` / `.scrbl` highlight out of the box; LSP via `racket-langserver`.

* **`{remote}` indicator default-on**: rendered on bottom-left for fresh installs (`F6` default keybinding, palette command).

* **Quick Open keybindings** (thanks @paveloparev!): `Ctrl+'` for files, `Ctrl+;` for buffers. Some terminals don't like passing these keys, you can rebind in the Keybinding UI (future version will probably change the default shortcuts to be more terminal-friendly across platforms).

* **Completion popup rebindable** (#1705): Allow modifying key bindings for the non-lsp completions popup.

* **Better scrolling** on markdown preview buffers and very-long-line buffers via a new two-tier line-wrap cache.

* **LSP**:
    - Stuck request no longer blocks others (#1679): per-server handlers on independent tokio tasks + 30 s timeout. R `languageserver` 0.3.17 no longer wedges completion / signature help.
    - Empty-server completion fallback to buffer-word completions.
    - Failure-stub log so "View Log" works after a failed spawn.

* **Devcontainer** spec-conformance + UX:
    - Lifecycle `cwd = remoteWorkspaceFolder`; object-form entries run in parallel and continue past failures; `remoteEnv` propagated; `shutdownAction: stopContainer` honoured on Detach; `userEnvProbe` captured and merged; `remoteUser` falls back per spec.
    - Lifecycle stdout/stderr surface in the panel; `Show *` commands reuse a single panel; build log split reused not stacked; parse errors surface (and `Open Config` stays registered); `onAutoForward: notify` toasts; state-relevant commands gated by authority; no re-prompt after restart.
    - **Devcontainer Goto-Definition across host / container**: LSP URIs translate at the boundary; container-only paths (e.g. `flask/app.py` under the venv) are fetched into a buffer.

* **File explorer preview** no longer loses focus to LSP popups in the editor pane.

* **Plugin types**: `tsconfig.json` for `tsc --noEmit` in CI; `editor.on(fn)` infers payload types from `HookEventMap`; `HookArgs` derives serde.

* **Conceal substitution** now emits the replacement glyph for whitespace tokens (Space / Newline / Break).

### Bug Fixes

* **`plugins/` folder in your project no longer hides bundled commands** (#1722): Fresh stops scanning the working directory for plugins.

* **Scroll & viewport**:
    - Mouse-wheel / PageDown / scrollbar-drag now reach EOF on word-wrapped buffers, including compose-mode markdown ending in a table. Within-line scroll re-clamps; gutter calc unified.
    - Search wrap-around Down-arrow stall: stale `scrolled_up_in_wrap` cleared on recenter.

* **Non-ASCII truncation panics** (#1715, #1718): settings search/preview/description/changelog, file-browser sort-arrow header, status-bar `truncate_path`, shell `truncate_command`, map-input value previews. Text search inside rows with multi-byte glyphs no longer panics either.

* **`setLayoutHints`** binds `compose_width` to the buffer, not whichever buffer is active.

### Under the Hood

* **Line-wrap cache** (`LineWrapCache` + tier-2 `VisualRowIndex`) becomes the single source of truth — `wrap_line` / `WrappedSegment` deleted; scroll math, cursor position, and scrollbar thumb share one pipeline.

* **Marker-tree `remove_in_range`** for `SoftBreakManager` / `ConcealManager` / `OverlayManager`: O(log N + k), proptest invariants.

* **`LspUri` newtype** enforces host / container URI translation at the type level.

* **Refactors**: `handle_mouse_click` (764 lines) → 14 helpers; `real_main` decomposed; `plugin_dispatch` match arms extracted; `quickjs_backend` ID-allocation boilerplate collapsed; `FromJs` impls into a macro; mouse multi-click + scroll dispatch deduped.

* **Test infrastructure**: fake `devcontainer` / `docker` / `pylsp` CLIs drive e2es without real binaries; plugin fixtures load from `<config_dir>/plugins/` instead of the cwd; animations default off in tests.

## 0.3.0

This version brings major features and many quality-of-life improvements and bug fixes:

- A cool dashboard plugin
- Devcontainer support
- init.ts

And more (see below). A large version is more likely to contain regression bugs, so please bear with me if you encounter problems, and open github issues without hesitation.

### Features

* **Dashboard plugin**: Built-in TUI dashboard that replaces the usual "[No Name]" with useful at-a-glance info.
    - Default widgets: git status + repo URL, a "vs master" row (commits ahead/behind), and disk usage for common mounts.
    - Opt-in widgets: weather, and open GitHub PRs for the current repo.
    - Auto-open (on startup / last-buffer-close) is configurable — e.g. `editor.getPluginApi("dashboard")?.setAutoOpen(false)` in `init.ts`. When off, use the "Show Dashboard" command in the palette.
    - Third-party plugins and `init.ts` can contribute their own rows via the `registerSection()` API. The `init.ts` starter template includes ready-to-paste snippets for enabling the opt-in widgets, toggling auto-open, and registering custom sections (see below).

* **Devcontainer support** (thanks @masak1yu!): Fresh integrates with the [devcontainer CLI](https://github.com/devcontainers/cli) (install it yourself).
    - Detects `.devcontainer/devcontainer.json` and offers Attach / Rebuild / Detach.
    - Embedded terminal, filesystem, and LSP servers all run inside the devcontainer.
    - `Dev Container: Create Config` scaffolds a config for projects that don't have one.
    - `Dev Container: Show Ports` merges configured `forwardPorts` with live `docker port` output.
    - `Dev Container: Show Logs` captures the container's recent stdout/stderr.
    - Build log streams into a workspace split; failed attaches offer Retry / Show Logs / Detach via a recovery popup.
    - `initializeCommand` runs on attach.

* **`init.ts`**: Fresh now auto-loads `~/.config/fresh/init.ts`! Allows you to run plugin code on startup, which complements the purely declarative config system with imperative, environment-aware logic. Use command palette `init: Edit` to generate a template with some examples. Use `init: Reload` to run it after editing. Use `--no-init` / `--safe` to skip loading.
    - Tip: *Enable LSP* when editing `init.ts` to get help and completions.
    - Example (for the Dashboard plugin):
    ```typescript
    // in your init.ts file:
    const dash = editor.getPluginApi("dashboard");
    if (dash) {
      dash.registerSection("env", async (ctx) => {
        ctx.kv("USER", editor.getEnv("USER") || "?");
      });
    }
    ```
    Will add a line like this to your dashboard:
    ```
    │ ▎  ENV                              │
    │    USER      someone                │
    ```

* **`{remote}` status-bar indicator**: Clickable status-bar element that lights up when you're attached to an SSH remote or devcontainer, with a context-aware menu (detach, show logs, retry attach, …). Surfaces `Connecting` / `Connected` / `FailedAttach` states. Fresh's config v1→v2 migration injects `{remote}` into customized `status_bar.left`.

* **Hot-exit restore split from session restore**: `editor.restore_previous_session` config and the `--no-restore` / `--restore` CLI flags now control workspace/tab restoration separately from hot-exit content — unsaved scratch buffers come back even when you opt out of full session restore (#1404).

* **File explorer — cut/copy/paste + multi-selection + right-click context menu** (thanks @theogravity!):
    - `Ctrl+C` / `Ctrl+X` / `Ctrl+V` with same-dir auto-rename and per-file conflict prompt on cross-dir paste.
    - `Shift+Up/Down` for multi-select.
    - Right-click context menu (#1684) with the usual file operations, honoring the active multi-selection.
    - Cut-pending items are dimmed until pasted; cancel a pending cut with Escape or by pasting back into the same directory.
    - Renaming a file or directory relocates any open buffers inside it; deleting a file closes its buffer.

* **File explorer — keyboard preview**: Moving the cursor with Up/Down in the explorer previews the highlighted file in a preview tab (#1570), so you can scan files without leaving the keyboard.

* **Quick Open / Go-to Line live preview**: Typing `:<N>` in the file finder (or in the standalone `:` mode) scrolls the cursor to the target line live as you type; Enter commits, Escape reverts, mouse movement or clicks also commit.

* **Terminal shell override (#1637)**: New `terminal.shell` config option lets you pick a different shell for the integrated terminal without reassigning `$SHELL` (which affects `format_on_save` and other features).

* **Suspend process (Unix)**: New `Suspend Process` action sends Fresh to the background like Ctrl+Z in a shell. Routed through the client in session mode so the server stays up.

* **Current-column highlight**: New `highlight_current_column` / `Toggle Current Column Highlight` — highlights the cursor's column for alignment work.

* **Post-EOF shading** (#779): Rows past end-of-file render with a distinct background so the boundary is obvious; works alongside `show_tilde`.

* **Regex replacement escapes**: `\n`, `\t`, `\r`, and `\\` in the replacement string are now interpreted when regex mode is on.

### Improvements

* **SSH URLs on the CLI**: `fresh ssh://user@host:port/path` launches a session whose filesystem and process authority point at the remote host.

* **Redraw Screen command** (#1070): Added a `redraw_screen` action and palette entry that clears the terminal and fully repaints the UI, useful when an external program scribbles over the TUI.

* **Terminal window title** (#1618): Fresh sets the terminal window title from the active buffer's filename, matching other editors.

* **LSP status popup upgrades**: LSP popup now shows better options for enabling/disabling the nudge.

* **Find Next centers vertically** (#1251): When the next match is off-screen, scroll it to roughly the middle of the viewport so you keep context above and below it. Matches that are already visible are not re-scrolled.

* **Adaptive line-number gutter** (#1204): The gutter now grows with the buffer's line count rather than reserving 4 digits by default — a small file reclaims 2–3 columns of editor width.

* **File explorer width in percent or columns** (#1118, #1212, #1213): `file_explorer.width` now accepts `"30%"` (percent of terminal width) or `"24"` (absolute columns). Dragging the divider preserves whichever form you configured. Legacy integer/fraction values keep working.

* **Relative paths to theme files** (#1621): User themes in `config.json` can be spelled out as relative to your themes directory:
  - "dark" or "builtin://dark" — any built-in by name
  - "my-theme.json" or "subdir/dark.json" — nested relative path in your user themes dir - useful for sharing Fresh config.json in a dotfiles repo
  - "file://${HOME}/themes/x.json" — absolute path; ${HOME}, ${XDG_CONFIG_HOME} are expanded
  - "https://github.com/foo/themes#dark" — URL-packaged theme

* **Plugin API additions**:
    - `editor.overrideThemeColors(...)` for in-memory theme mutation.
    - `editor.parseJsonc(...)` for host-side JSONC parsing.
    - Plugin-created terminals now have an ephemeral lifetime — they close cleanly when the action that spawned them finishes.
    - Plugin authors can augment `FreshPluginRegistry` to make `editor.getPluginApi("name")` return a typed interface (no `as`-cast needed). Augmentations are emitted to `~/.config/fresh/types/plugins.d.ts` at load time.
    - `spawnHostProcess` now returns a handle with `kill()` (and a matching `KillHostProcess` command).
    - `BufferInfo.splits` surfaces which splits display a buffer, for "focus-if-visible" dedupe.
    - `editor.setRemoteIndicatorState(...)` / `clearRemoteIndicatorState()` let remote plugins drive the status-bar `{remote}` element.
    - Dashboard gains `dash.registerSection()` (with a returned remover) and `dash.clearAllSections()` for plugin extension.

* **JSONC language**: `.jsonc` files and well-known JSONC-with-`.json`-suffix files (`devcontainer.json`, `tsconfig.json`, `.eslintrc.json`, `.babelrc`, VS Code settings files) now get a dedicated `jsonc` language with comment-tolerant highlighting and LSP routing through `vscode-json-language-server` with the correct `languageId`.

* **macOS Alt+Right / Option+Right stops at word end** (#1288): Selection no longer extends past trailing whitespace, matching TextEdit / VS Code on Mac.

### Bug Fixes

* **File Explorer `.gitignore` improvements** (#1388): Files are now visible only if they aren't hidden by ANY of the filters (hidden files, `.gitignore` files). Also, File Explorer will do a better job of auto-reloading when `.gitignore` changes.

* **Scrollbar theme colours** (#1554): The scrollbar now honours `theme.scrollbar_track_fg` / `scrollbar_thumb_fg`. A few themes were updated to define this missing value.

* **Fixed panic when clicking split + terminal** (#1620).

* **Fixed LSP server crash loop** (#1612): When LSP fails on startup, restart bypassed the normal restart count limiter, now fixed.

* **Fixed Markdown preview/compose wrapping when File Explorer is open**: When compose width was set (e.g. 80), opening the File Explorer sidebar pushed tables off the right edge. Separator rows no longer overflow when table cells are truncated.

* **More settings propagate live**: File-explorer width and flag changes made in the Settings UI apply immediately on save, without a restart.

* **Devcontainer: no re-prompt after restart**: Fresh no longer shows the "Attach?" prompt again after the post-attach self-restart.

* **Dashboard polish**: Doesn't steal focus from a CLI-supplied file, underline only on clickable spans (not trailing padding), clicks dispatch only from underlined column ranges, immediate repaint on split resize.

* **Quieter LSP**: Suppress `MethodNotFound` errors from LSP servers (#1649) — servers that don't implement an optional method no longer spam the log.

* **Plugin action popups survive buffer switches**: Popups stay visible when the active buffer changes, and concurrent popups queue LIFO so the newest shows first.

* **Encoding detection on CJK files** (#1635): Files whose only non-ASCII bytes sat past the 8 KB sample window were mis-detected; the sample boundary is now treated as truncation so the full file is considered before the encoding is guessed.

* **Review diff — no fold jitter**: Toggling a fold no longer re-centers the viewport.

* **LSP — cleaner disables**: No spurious warning when opening a file for a language whose LSP is explicitly disabled in config. The indicator shows buffer-skip state (e.g. file too large) instead of a generic warning.

* **Windows — preserve UNC paths**: `pathJoin` plugin API now preserves `\\?\` UNC prefixes on Windows.

* **Hardware cursor no longer bleeds through popups**: The terminal hardware cursor is hidden when an overlay popup covers it.

* **Focus — tab clicks reset explorer context** (#1540): Clicking a tab or buffer no longer leaves the FileExplorer key context active.

* **File explorer poll fixes**: Background refresh no longer collapses folders you've expanded, and resets the cursor to the root only when the selected path is genuinely gone.

* **Review PR Branch — default-branch detection**: The prompt now pre-fills the repo's actual default branch (via `origin/HEAD`) instead of hard-coding `main`.

* **Review: PageUp/PageDown**: Paging in review-branch mode now scrolls the commit list instead of moving the cursor by one row.

### Under the Hood

* **Authority abstraction**: Filesystem, process-spawning, and LSP routing are now consolidated behind a single `Authority` slot, with plugin ops (`editor.setAuthority` / `clearAuthority` / `spawnHostProcess`) for plugins that want to target the host even while attached elsewhere. This is what makes the devcontainer and `ssh://` flows work uniformly.

## 0.2.25

### Improvements

* **Redraw Screen command** (#1070): Added a "Redraw Screen" entry to the command palette (action `redraw_screen`) that clears the terminal and fully repaints the UI. Useful when an external program (e.g. a macOS pasteboard diagnostic leaked by the host terminal on Ctrl+C) scribbles over the TUI and leaves ghost text behind.

* **PageUp/PageDown in wrapped buffers**: Page motion is now view-row-aware, so paging through heavily wrapped text no longer stalls mid-buffer and the cursor stays visible after every press. Each page also keeps 3 rows of overlap with the previous page (matching vim / less) so you don't lose context across the jump.

* **Smarter char-wrapping of long tokens**: When a token has to be split mid-word because it doesn't fit on a fresh line, the break now prefers a UAX #29 word boundary within a lookback window instead of an arbitrary grapheme position — e.g. `dialog.getButton(DialogInterface.BUTTON_NEUTRAL).setOnClickListener` now wraps after `BUTTON_NEUTRAL` rather than mid-identifier.

### Bug Fixes

* Fixed language detection for extensionless files (#1598, #1607): files like `test` with a `#!/usr/bin/zsh` shebang, or extensionless bash scripts with `#!/bin/bash`, now detect the language from the shebang instead of falling through to plain text — restoring 0.2.23 behaviour.

* Fixed missing characters and blank first rows when wrapping indented lines (#1597) — e.g. the `:` between `with` and ` a` in `someObject.doSomething(with: a, and: b)` was being dropped at the wrap boundary, and quoted strings / code content could be pushed below a row of pure indent whitespace instead of wrapping in place.

* Fixed the end-of-line cursor overlapping the vertical scrollbar on wrapped rows that exactly filled the content width.

* Reduced idle CPU by avoiding per-tick serialization of config and diagnostics in the plugin snapshot — an idle editor with LSP active no longer reserializes this state dozens to hundreds of times per second.

* Silenced a bogus `ts -> TypeScript` alias warning at startup (#1601, #1603); the alias itself already worked.

## 0.2.24

### Features

* **Review Diff Rewrite**: Files list and diff are now one scrollable unified buffer. Use `n` / `p` to jump to       z next/previous hunk. You can collapse per-file, and stage or unstage content on the cursor row (hunk, whole file, or a line-level visual selection). **Review comments now persist per-repo** across sessions, and a dedicated Comments panel makes them navigable. Two new entry points: `Review: Commit Range` for any `A..B` / `A...B` range, and `Review: PR Branch` for walking a branch's commits with a live `git show` side-by-side. In the future I plan to make a new UX for picking the diff target (PR, branch, etc.)

* **Git Log Rewrite**: Live-preview right panel updates as you move through the log, clickable toolbar, theme-aware colours, aligned columns, wrapped commit messages.

* **Rendering Performance Improvements**: see the bugfix section below.

### Improvements

* **Preview Tabs in File Explorer** (#1403): Single-clicking a file opens it in an ephemeral "preview" tab that the next single-click replaces, instead of accumulating tabs. Any real commitment — editing, double-click / Enter, clicking the tab, or a layout action — promotes the preview to a permanent tab. Enabled by default; disable via Settings UI.

* **LSP**:
    - **LSP status-bar indicator** with a spinner during start-up and indexing (no more jitter as progress messages arrive); configured-but-dormant servers show as `LSP (off)`. Clicking opens a popup with per-server status, live progress, a "binary not in PATH" label for missing servers like `pylsp` / `gopls` (so they don't spawn failing processes), and a per-language mute option.
    - The LSP hover popup now fuses any overlapping diagnostic (severity-coloured, source-tagged like `rustc` / `clippy` / `clangd`) above the hover body.
    - `.h` files in C++ projects now route to the C++ LSP when there's a real signal (sibling `.cpp` / `.hpp` / `.hxx`).
    - **LSP Correctness fixes**: inlay hints re-request after every edit, anchor correctly at end of line (#1572), vanish when their anchor is deleted, track theme changes, and populate on every open buffer when the server becomes quiescent (previously only one); fold indicators no longer drift after edits around a fold (#1571); and diagnostic / inlay-hint refresh no longer pulls from buffers of the wrong language.

* **Markdown Table Frames**: Tables in Page View now render full box borders — top and bottom frames plus a separator between every data row.

* **Read-only state persists across sessions**: Buffers marked read-only stay read-only after restart.

* **Narrow-terminal status bar**: The right side drops low-priority elements (palette hint, warning counts, encoding, …) in order so the filename and cursor position stay visible.

* **Shift+Mouse-wheel** now scrolls horizontally even when the currently visible lines fit the viewport (previously required a long line to have been rendered first).

* **Explorer auto-expands on first open** (#1569): `Ctrl+B` with a nested file active now reveals the file on the first toggle — no more double-toggle.

* **Grammar / language fixes**: Bare-filename scripts (`Gemfile`, `Rakefile`, `Makefile`) highlight correctly; `.jsx` / `.mjs` / `.cjs` route to JavaScript; TypeScript chosen via Set Language now highlights and appears in `fresh --cmd grammar list`.

* **Plugin API**: Virtual lines accept theme keys for `fg` / `bg` and follow theme changes live. Plugin modes can inherit Normal-mode bindings instead of redeclaring motions. The `mouse_click` hook payload now carries buffer coordinates.

### Bug Fixes

* Fixed scrolling in heavily wrapped buffers (#1574): Up/Down no longer drifts the viewport one row per keystroke, and mouse-wheel scroll in Page View on long markdown lists now reaches EOF reliably.

* Fixed multi-byte / grapheme handling in the view pipeline: fullwidth and CJK text, ZWJ emoji families (`👨‍👩‍👧‍👦`), and long combining-mark clusters now render, advance by cursor, and participate in Home/End as a single unit (#1577). Note: different terminals handle ZWJ differently, I recommend ghostty if you need these characters for some reason...

* Fixed `Format Buffer` hanging on formatters that emit more than ~64KB — stdout / stderr are now drained concurrently with the write (#1573).

* Fixed arrow keys with an active selection (#1566): Left/Right now collapse to the selection's start/end edge (matching VSCode, Sublime, IntelliJ); Up/Down start from the appropriate edge.

* Fixed `Shift+Backspace` now behaves like Backspace (previously silently dropped on many terminals) (#1588).

* Fixed session-restored folds landing on unrelated lines after external edits — they now relocate by header text or are dropped (#1568).

* Fixed project-wide Search & Replace: replace is now undoable with a confirm prompt, repeated `Alt+Enter` no longer corrupts files, the file-watch mtime is refreshed after save, and the 100% CPU hang on large-repo scans is fixed (#1575).

* Fixed 100% CPU when a fold hid thousands of lines — fold-indicator detection no longer scans the whole buffer per visible row.

* Fixed plugin-panel buffers (audit mode, git log, review diff): cursor positions preserved across tab switches, clicks on scrollable panels register, `q` closes the group from any panel, and the active tab in an inactive split is now visible under the high-contrast theme.

* Fixed cursor being able to land on plugin-virtual rows (markdown table borders, gutters) when moving up/down.

## 0.2.23

### Improvements

* **Windows-1251 Encoding**: Added support for Windows-1251 (Cyrillic) encoding for loading and saving Cyrillic-script text files (#1453). Available in the encoding selector; auto-detected for text mixing uppercase and lowercase Cyrillic letters.

* **Theme Editor and Package Manager**: Multi-panel plugin UIs now behave like native splits — per-panel mouse-wheel scrolling and scrollbars, draggable panel dividers, and the theme editor's own colors now use the active theme.

* **File Finder in Command Palette (Ctrl+P)**: Much faster and more responsive on large local and remote trees — file enumeration runs in the background with results streaming in as they're found, typing a path like `etc/hosts` produces instant filesystem-confirmed matches, and ranking now reliably prefers contiguous matches (`results` finds `results.json` first) including multi-term queries that reconstruct a path or identifier (`etc hosts` → `/etc/hosts`, `save file` → `save_file.rs`).

* **Review Diff**: Brought back features that were dropped in the rewrite in version 0.2.22: stage, unstage, and discard individual hunks; jump between hunks with `n`/`p`; leave line comments (`c`) and overall session notes (`N`), edit or delete them with confirmation, see notes in the file list panel, and export your review notes to a markdown file. Redesigned toolbar of styled key hints that adapts to the focused panel.

* **Keybinding Editor**: Special keys like Esc, Tab, and Enter can now be bound — press Enter on the key field to enter capture mode, then the next keypress is recorded as-is (#1501). Fixed parent modal to be dimmed while a sub-dialog is open.

* **Customizable Status Bar**: The left and right sides of the status bar are now configurable via the Settings UI using a new DualList picker (transfer items between Available/Included columns, reorder with arrows). Includes a new `{clock}` element that displays HH:MM with a blinking colon. Thanks @1612elphi!

* **LSP Status Bar Indicator**: Simplified to a single color-coded "LSP" label — clicking LSP in the status bar now opens an interactive popup with per-server status and actions (restart, stop, view log).

* **Universal LSP Servers**: LSP servers configured for all languages are now spawned once per project instead of once per opened language, eliminating duplicate processes.

### Bug Fixes

* **Regression** - Fixed multi-byte UTF-8 characters not being parsed correctly in the input handler, and IME-composed characters delivered as key-up events being ignored on Windows (#1538). Thanks @wellorbetter! Reported by @AugustusZane.

* Fixed blank panels appearing after terminal resize.

* Fixed terminal mode not being exited when the active buffer is switched to a non-terminal.

* Fixed Review Diff hunk navigation (`n`/`p`) not working in the diff panel, files panel not receiving focus on launch, hunk-level comments not displaying (#1503), and deleted-file drill-down crashing.

* Fixed Settings UI section headers being invisible in the high-contrast theme.

* Fixed word wrap producing single-character-per-line output on narrow terminals with deeply indented code — the hanging indent was being double-counted (#1502).

* Fixed LSP completion popup showing duplicate entries when reopened (#1514).

* Fixed LSP `auto_start` being ignored on a per-server basis when multiple servers are configured for one language — opening a file no longer drags in every enabled server, only those individually marked `auto_start`.

* Fixed mouse input issue - mouse state not being restored in the terminal - after leaving Fresh (Windows only, #1530).


## 0.2.22

### Features

* **Review Diff Rewrite**: The review diff view has been rewritten with a magit-style split-panel UI. The left panel lists files grouped by staged/unstaged/untracked sections (sorted by category), and the right panel shows the diff for the selected file. Navigate with arrow keys, switch focus between panels with Tab, and drill down into individual files. Hunk navigation jumps between changes with auto-centering. Untracked and newly added files are now shown correctly. Diff colors are now theme-aware with per-theme highlight overrides.

* **Remote Mode**: SSH connections now auto-reconnect in the background with a disconnected indicator in the status bar. Filesystem operations no longer block the event loop. File explorer roots at the provided remote path instead of the home directory. File finder (Ctrl+P) works on remote filesystems. Error messages are cleaner — hints about SSH installation, and a "Connecting via SSH to ..." message on startup.

### Improvements

* **Create Directories on Save**: When saving a file to a path where the parent directory doesn't exist, Fresh now prompts to create the directory instead of failing (#1434).

* **Grammar Short Name Aliases**: Grammars can now be referenced by short names (e.g., `"bash"` instead of `"Bourne Again Shell (bash)"`) in config and the Set Language popup. Packages can declare a `shortName` in their grammar definition.

* **Default Language Setting**: The `default_language` setting replaces the previous `fallback` object. Set it to a language key (e.g., `"bash"`) so unrecognized file types use that language's full configuration (#1219).

* **AutoHotkey Syntax Highlighting**: Built-in grammar for `.ahk` and `.ahk2` files with v1/v2 command coverage.

* **Settings UI**: Added inherit/unset support for nullable settings with an Inherit button and inherited badge. The Delete key now unsets a setting override.

* **Theme Selector**: Installed theme packages now appear correctly even when multiple themes share the same name. The selector strips URL schemes and sizes the name column to content.

* **File Finder (Ctrl+P)**: Fixed showing no files on Windows when git isn't being used.

* **Selection Prompts**: Pre-filled text is now selected so typing immediately replaces it.

* **Theme Fixes**: Fixed low contrast in Nord, Solarized Dark, Light, and Dracula themes. Fixed command palette selected row using wrong foreground color. Syntax highlighting colors are now preserved in text selections.

### Bug Fixes

* Fixed out-of-memory crash caused by an infinite loop in the line-wrapping transform when indentation exceeds half the available width (#1454).

* Fixed `didOpen` notification only being sent to the first LSP server when multiple are configured for a language.

* Fixed status bar line number not updating when stepping through search matches with F3.

* Fixed `.bash_profile` appearing read-only when symlinked to a macOS library path (#1469).

* Fixed session `open-file` command failing when a session exists but its name doesn't match the socket.

* Fixed scrollbar track hover highlighting more than the hovered cell.

* Fixed self-update URL pattern not matching all release URLs.

## 0.2.21

### Features

* **Fast Completions without LSP**: New basic completions providers without language server — buffer-word candidates appear below LSP results in the popup. Also, a new setting (config) controls auto-trigger vs explicit Ctrl+Space (default: explicit). Enter dismisses the popup (Tab accepts). I plan to further improve it (make it more intelligent) in future releases.

* **Current Line Highlighting**: Highlights the cursor line. Enabled by default, togglable from the command palette and Settings UI (caveat: wrapped lines are currently highlighted in their entirety, this should probably be changed to visual lines).

* **LSP Code Actions**: Code action modal now actually works! Select an action by number or up/down arrows and enter (#1405). Supports resolve, execute command, and server-initiated workspace edits (previously dropped silently). File create/rename/delete operations handled. Actions from multiple servers unified into one popup. Default keybinding changed to Alt+. - because Ctrl+. is filtered by many terminals.

* **LSP Completion Resolve and Formatting**: Auto-imports applied on completion accept. Format Buffer falls back to LSP when no external formatter is configured. Also adds range formatting and pre-rename validation.

* **LSP Server Selection for Restart/Stop**: Popup to choose which server to restart/stop individually, or all at once.

* **Grammar Listing**: `fresh --cmd grammar list` and `editor.listGrammars()` plugin API show all available grammars with source and extensions. When specifying a grammar in a `languages` entry in the config, you must currently use a full name from this list - for example "Bourne Again Shell (bash)" rather than "bash". This will be improved once I add grammar aliases.

### Improvements

* **Theme Contrast**: Replaced all named ANSI colors with explicit RGB in built-in themes for deterministic rendering. Improved contrast ratios across both high-contrast and light themes. Diagnostic and semantic overlays now re-apply correctly on theme change, including during live preview.

* **Git Status Marker Refresh**: File explorer markers update on terminal focus gain and by polling for git index changes (#1431).

* **Config-Only Languages**: Custom languages without a built-in grammar (e.g., "fish") appear in the Set Language popup and are detected correctly — no more fallthrough to wrong built-in grammars.

* **Theme Inspector**: Records exact theme keys during rendering instead of reverse-mapping via heuristics. Theme editor Save As improved for built-in themes.

* **LSP Reliability**: Diagnostics cleared on server stop/crash, buffers re-opened on server start, document version checking for workspace edits, LSP notified after undo/redo of bulk edits, pending requests drained on server death to prevent deadlocks, hover suppressed while popups are visible.

### Vim Mode

22 bug fixes: C/D/S/cc, e motion, nG, h/l line clamping, ^, $, J with space, f/t special chars, r replace, ~ toggle case, visual mode entry/switching, count display. Key motions moved from async plugin commands to native Rust actions, eliminating race conditions.

If you use the Vim plugin please drop a note at https://github.com/sinelaw/fresh/discussions/417 - I need feedback on this feature.

### Bug Fixes

* Fixed Ctrl+W panic on accented/multi-byte characters (#1332).

* Fixed LSP diagnostics from stopped servers reappearing from queued messages.

## 0.2.20

### Features

* **Multi-LSP Server Support**: Configure multiple LSP servers per language (e.g., pylsp + pyright for Python). Servers are routed by feature using `only_features`/`except_features` filters, completions are merged from all eligible servers, and diagnostics are tracked per-server. Per-server status is shown in the status bar (#971).

* **Per-Language Editor Settings**: `line_wrap`, `wrap_column`, `page_view`, and `page_width` can now be configured per-language. For example, wrap Markdown at 80 columns while keeping code unwrapped (#1371).

* **Diff Chunk Navigation Plugin**: New built-in plugin for navigating between diff chunks, merging git and saved-diff sources.

### Improvements

* **Faster Startup (~350ms → ~170ms)**: Syntax grammars are pre-compiled at build time, package loading moved from JavaScript to Rust, plugin I/O and transpilation run in parallel, and redundant grammar rebuilds are eliminated. Plugins can now declare dependencies via `import type` from `"fresh:plugin/..."` and are topologically sorted.

* **Settings UI Overhaul**: Modernized visual design with wider modal (160 cols), rounded corner borders, Nerd Font category icons, styled `[✓]` toggles, and reverse-video key hints. Keyboard navigation rewritten: Tab cycles sequentially through all fields and buttons, composite controls (Map, ObjectArray, TextList) support internal navigation, entry dialogs have section headers with explicit field ordering, PageDown/PageUp work in the main panel, and TextList edits auto-accept on navigation. Focus indicator now highlights per-row in composite controls.

* **Settings Deep Search**: Also in the Settings UI: Search now walks into Map entries, TextList items, and nested JSON values. Searching "python" finds the "python" key in language/LSP maps. Results show hierarchical breadcrumbs (e.g., "Languages > python") and auto-focus the matching entry.

* **Per-Language Workspace Root Detection**: New `root_markers` field on LSP server configs. The editor walks upward from the file's directory looking for configured markers (e.g., `Cargo.toml`, `package.json`), replacing the old cwd-based root (#1360).

* **Page View Mode**: "Compose" mode renamed to "Page View". Can now auto-activate per language via `page_view: true` in language config. Old keybinding names continue to work.

* **256-Color Contrast Enforcement**: When running in a 256-color terminal, foreground colors are automatically adjusted to meet WCAG 3.0:1 minimum contrast ratio against their background. Fixes illegible text in Solarized Dark, Nord, Dracula, and light themes under tmux without truecolor.

* **LSP in Library Files**: Files in library paths (site-packages, node_modules, .cargo) now keep LSP enabled for Goto Definition, Hover, and Find References while remaining read-only (#1344).

* **Goto Matching Bracket**: Works inside bracket bodies by searching backward for the nearest enclosing bracket, matching VS Code and JetBrains behavior. All bracket searches are bounded to prevent hangs on huge files (#1258).

* **LSP Head-of-Line Blocking Fix**: LSP notifications (didClose, didChange, shutdown) are no longer blocked behind pending request responses.

* **New Settings**: `show_tilde` to hide EOF tilde markers (#1290), `menu_bar_mnemonics` to disable Alt+key menu shortcuts (#1257).

* **`getPluginDir()` Plugin API**: Plugins can now locate their own package directory to find bundled scripts or install local dependencies.

### Bug Fixes

* Fixed CSI u and xterm modifyOtherKeys key sequences inserted as literal text in terminal session mode (#1113).

* Fixed word selection (Ctrl+W) stopping at accented/Unicode characters (#1332).

* Fixed double-click backward drag losing the initial word selection (#1334).

* Fixed block cursor invisible in zellij due to double cursor-inversion (#1338).

* Fixed cursor visibility and command palette rendering in zellij (#1255).

* Fixed undo incorrectly clearing the modified flag after hot exit recovery, which could cause data loss.

* Fixed bulk edit (e.g., toggle comment) displacing inlay hints on subsequent lines (#1263). Displaced markers are now restored to exact positions on undo.

* Fixed large file syntax highlighting lost when revisiting a file, caused by checkpoint offset drift during partial cache updates.

* Fixed embedded language highlighting (e.g., CSS in HTML) breaking at large file offsets.

* Fixed Enter key leaking into the markdown buffer when the file explorer panel is focused.

* Fixed large file recovery saving the entire file as individual chunks instead of using the recovery format.

* Fixed read-only detection for files not owned by the current user (now checks effective uid/gid instead of file mode bits).

## 0.2.18

### Features

* **Bracketed Paste on Windows & Input Overhaul**: Bracketed paste now works on Windows Terminal (reverted in v0.2.17 due to #1284), and keyboard issues are resolved (#1054). **Mouse hover is disabled by default on Windows** because reliable bracketed paste requires cell-motion tracking; enabling hover switches to all-motion tracking which can insert corrupt text under heavy mouse movement or slow CPU. Re-enable it in Settings UI under Editor → Mouse Hover Enabled. Under the hood, crossterm's Windows input handling is replaced with a new `fresh-winterm` crate using direct VT input reads, with corrupt mouse sequence detection, UTF-16 surrogate handling, and console mode heartbeat to counteract ConPTY drift.

* **30 New Syntax Grammars**: Dockerfile, CMake, INI, SCSS, LESS, PowerShell, Kotlin, Swift, Dart, Elixir, F#, Nix, Terraform/HCL, Protobuf, GraphQL, Julia, Nim, Gleam, V, Solidity, KDL, Nushell, Starlark, Justfile, Earthfile, Go Module, Vue, Svelte, Astro, Hyprlang (#1266). These grammars are preliminary — please report highlighting issues for your language so we can improve them.

* **Broad LSP Support**: Added LSP configs and helper plugins (with install instructions) for Nix, Kotlin, Swift, Scala, Elixir, Erlang, Haskell, OCaml, Clojure, R, Julia, Perl, Nim, Gleam, F#, Dart (#1252), Nushell (#1031), Solidity (#857), Vue, Svelte, Astro, Tailwind CSS, Terraform/HCL, CMake, Protobuf, GraphQL, SQL, Bash, Lua, Ruby, PHP, YAML, TOML, and Typst. LSP integration for these languages is early-stage — feedback from users of these languages is welcome.

* **Deno LSP Auto-Detection**: Automatically detects and uses the Deno language server for JS/TS projects (#1191).

* **`show_prompt_line` Setting**: New config option to auto-hide the prompt line. Applied immediately from Settings UI (#1273).

* **`use_tabs` Setting**: Global `editor.use_tabs` config option for default tab indentation (#1295).

### Improvements

* **Plugin Commands in Keybinding Editor**: Plugin-registered commands are now shown and searchable in the keybinding editor.

* **Theme Editor ANSI Colors**: Named ANSI colors display as "terminal native" instead of misleading RGB values, with correct native color swatches (#1301).

* **Status Bar Language Info**: Shows "[syntax only]" when a language has no LSP config entry.

* **Default Language**: Set `default_language` to a language key (e.g., `"bash"`) so undetected file types use that language's full configuration (#1219). Replaces the previous `fallback` object; the old key is still accepted for backwards compatibility.

* **File Deletion Uses Trash**: `removePath` now uses the system trash instead of permanent deletion.

* **Package Manager Cross-Platform**: Plugin package manager uses cross-platform APIs instead of Unix-specific commands on Windows (#1215).

### Bug Fixes

* Fixed arrow keys not working in `less`/`git log` in the embedded terminal, including `TERM` env var not being set on Unix.

* Fixed Tab key getting trapped in TextList editing mode in Settings UI.

* Fixed `{`, `}`, `;` highlighted as operators instead of punctuation in C/C++ (#1318, #1319).

* Fixed auto-dedent for languages without tree-sitter, e.g. Dart.

* Fixed auto-indent after closing brace in nested C++ blocks.

* Fixed mouse click selecting wrong item in scrolled settings list.

* Fixed keybindings for plugin-registered commands not executing (#1312).

* Fixed Find Next/Find Previous ignoring cursor position (#1305).

* Fixed Tab indent affecting lines outside selection (#1304).

* Fixed Shift+letter keybinding deletion not persisting (#1303).

* Fixed word selection not preserved when dragging after double-click (#1202, #1317).

* Fixed `removePath` failing on Windows due to UNC path mismatch.

* Fixed external files missing from tab bar after session restore.

* Fixed scroll wheel targeting focused split instead of split under pointer (#1270).

* Fixed wrap indent not working with tab indentation (#1283).

* Fixed LSP "no server configured" for Kotlin and 30+ other languages.

* Fixed Diff syntax highlighting scope-to-category mappings.

* Fixed extension mappings for `.cjs`, `.mjs`, `.mts`, `.cts`, `Jenkinsfile`, `Brewfile`.

* Fixed LSP initialization timeout too short (increased from 10s to 60s).

### Internal

* Added syntax highlighting validation suite with 93 fixture files and e2e tests.

* Added e2e tests for Settings UI, keybinding editor, search/replace, and plugin commands.

* Fixed multiple flaky e2e tests (search/replace, plugin uninstall, Settings UI).

* Removed redundant `SIGUSR1` handler; relies on harness signal handler for backtraces.

* Cleaned up completed design docs.

---

## 0.2.17

### Bug Fixes

* **Reverted Windows Bracketed Paste Fix**: Reverted the bracketed paste fix for Windows Terminal (#1218) as it broke mouse input (#1284). The fix enabled `ENABLE_VIRTUAL_TERMINAL_INPUT` which interfered with mouse event handling.

---
## 0.2.16

### Features

* **Project-Wide Search & Replace**: Search and replace across the entire project. Works reliably with unsaved buffers, large files, and up to 10,000 results. Alt+Enter to replace in project.

* **Hot Exit**: All buffers — including unnamed scratch buffers — persist across sessions automatically. Configurable via `hot_exit` setting (#1148, #1233).

* **Workspace Storage**: Session state always restored on startup, even when opening specific files from CLI. Plugin state also persists across sessions.

### Improvements

* **Keybinding Editor**: Collapsible section headers and plugin mode bindings shown as first-class entries.

* **Markdown Compose Mode**: Easier to discover via global toggle and searchable command palette entries.

* **Tab Naming**: Duplicate tab names are disambiguated with appended numbers.

* **View...Keybinding Style: Menu Checkboxes**: Submenu items now show checkbox indicators for toggled settings.

### Bug Fixes

* Fixed crash when workspace references deleted files (#1278).

* Fixed CapsLock breaking keyboard shortcuts like Ctrl+A, Ctrl+C, etc.

* Fixed bracketed paste not working on Windows Terminal.

* Fixed clipboard not working in client-server session mode.

* Fixed Latin-1 files misdetected as UTF-8 for short files with trailing high bytes.

* Fixed line number bugs: Delete key not updating status bar (#1261), relative line numbers miscounting (#1262).

* Fixed C# LSP not working due to language ID mismatch.

* Fixed remote editing using hardcoded `/tmp` instead of querying the remote system.

* Fixed high memory usage on Windows (#1205).

* Fixed PageUp/PageDown not working in Theme Editor sidebar.

* Fixed unbound keys being swallowed in plugin modes.

### Packaging

* **Linux**: Icons and desktop files added to all packaging methods (deb, rpm, Flatpak, AppImage). Fixed Flatpak AppStream metadata for app stores.

---
## 0.2.14

### Improvements

* **Keybinding Map Checkboxes**: Submenu items in the keybinding map now show checkbox indicators for toggled settings.

### Bug Fixes

* **Windows Memory Usage**: Fixed high memory usage on Windows caused by buffered input event draining before render (#1205).

---

## 0.2.13

### Features

* **Inline Diagnostics**: Diagnostic text displayed at end of lines, right-aligned, with version-aware staleness dropping. Disabled by default — enable "diagnostics inline text" in the Settings UI (#1175).

* **Hanging Line Wrap**: Wrapped continuation lines preserve the indentation of their parent logical line (#1169).

* **Theme Editor Redesign**: Virtual scrolling, mouse support, flicker-free inline styling. New "Inspect Theme at Cursor" command and Ctrl+Right-Click theme info popup.

* **Open File Jump**: `path:line[:col]` syntax in Open File prompt and Quick Open (#1081, #1149).

### Improvements

* **Plugin API**: `registerHandler()` replacing `globalThis` pattern, `restartLspForLanguage`, process-limits for `registerLspServer`, async `reloadGrammars()`. Strict TypeScript across all plugins.

* **Load Plugin from Buffer**: Run and hot-reload plugins directly from an open buffer, with LSP support for plugin dev buffers.

* **Status Bar Toggle**: Command palette command and config option to show/hide the status bar.

* **LSP Environment Variables**: Pass environment variables to LSP server binaries via config (#1159).

* **LSP Language ID Overrides**: Configurable `language_id_overrides` in LSP server config.

* **Rust LSP Mode Switching**: Command palette action to switch between Full and Reduced Memory modes for rust-analyzer.

* **Signature Help Rendering**: Markdown rendering for signature help popups with hanging indent and paragraph spacing.

* **Non-Blocking Grammar Builds**: `SyntaxSet::build()` moved to a background thread. Buffered input events drained before render for CPU-constrained responsiveness.

* Disabled LSP start/restart commands for languages without LSP config (#1168).

### Bug Fixes

* **LSP Bracket Paths**: Fixed LSP failing for file paths containing `[` or `]` (#953).

* **Search F3 Navigation**: Fixed F3 losing matches after viewport scroll (#1155).

* **Settings JSON Copy**: Fixed Ctrl+C not working in settings JSON editor (#1159).

* **Line Numbers on New Files**: Fixed line numbers showing when disabled in settings for newly opened files (#1181).

* **Client/Server Paste**: Fixed bracketed paste mode and terminal feature parity in client/server sessions (#1168).

* **Popup Selection**: Fixed popup text selection copying wrong text when lines wrap (#1170).

* **Suggestions Popup Border**: Fixed bottom border overwritten by status bar (#1174).

* **TSX/JSX Language ID**: Fixed wrong `languageId` sent to LSP for TSX/JSX files (#1174).

* **LSP Error Suppression**: Suppress ContentModified/ServerCancelled errors per LSP spec instead of logging as errors.

* **Semantic Tokens**: Skip degraded semantic token responses to preserve syntax highlighting.

* **Theme Save**: Fixed save failing when themes directory doesn't exist (#1180). Fixed saving incomplete theme files.

* **LSP Completion**: Fixed completion debounce, cleanup-on-disable, and popup positioning issues.

---

## 0.2.12

### Features

* **Auto-Close Config**: Separate `auto_close` toggle (default: true) to independently control bracket/quote auto-close, skip-over, and pair deletion — previously coupled to `auto_indent`. Per-language overrides via `languages.<lang>.auto_close` (#1144).

* **Surround Selection**: Typing an opening delimiter with a selection wraps it instead of replacing it (e.g. selecting `hello` and typing `(` produces `(hello)`). Controlled by `auto_surround` config with per-language overrides.

* **Smart Quote Suppression**: Quotes typed inside an existing string no longer auto-close, preventing unwanted doubled quotes (#1142).

* **Read-Only Mode**: Files without write permission and library/toolchain paths (rustup, /usr/include, /nix/store, Homebrew Cellar, .nuget, Xcode SDKs) automatically open as read-only. New "Toggle Read Only" command to override. Status bar shows `[RO]` indicator.

### Bug Fixes

* **Multi-Cursor Enter**: Fixed Enter key in markdown mode only inserting a newline at the last cursor, ignoring secondary cursors. Falls back to built-in insert_newline when multiple cursors are active (#1140).

* **Multi-Cursor Position Drift**: Fixed cursors with no events during bulk edits (e.g. Delete at end of buffer) drifting to stale positions. Uses saturating arithmetic to prevent overflow with overlapping selections (#1140).

### Improvements

* **Log Noise Reduction**: Disabled span close events (~90% of log volume) by default and moved 12 high-frequency log sites to trace level. Typical log size reduced from ~266MB to ~5-10MB. Set `FRESH_LOG_SPANS=1` to re-enable (#1154).

### Internal

* Added multi-cursor shadow model property-based tests with random operation sequences across 2-3 cursors.
* Added e2e regression tests for multi-cursor Enter with auto_indent, Ctrl+D selection, tree-sitter, and markdown grammar.

---

## 0.2.11

### Features

* **Whitespace Indicators**: Granular control over whitespace visibility — configure space (·) and tab (→) indicators independently for leading, inner, and trailing positions. Master toggle, per-language overrides, and a new `whitespace_indicator_fg` theme color.

* **Indent-Based Code Folding**: Code folding now works in large file mode and for files without LSP folding ranges, using indentation analysis as a fallback. Fold from any line within a block (not just the header). Unified byte-offset pipeline for consistent gutter indicators.

* **Session Open-File Enhancements**: `--wait` flag blocks the CLI until the user dismisses a popup or closes the buffer — enables use as `git core.editor`. Range selection syntax (`file:L-EL`, `file:L:C-EL:EC`) and hover messages (`file:L@"markdown msg"`) for annotated file opening. Auto-attaches a client when `open-file` starts a new session.

* **GUI: macOS Native Integration** (experimental): Native menu bar with dynamic when/checkbox conditions, Cmd keybindings (`macos-gui` keymap), app icon, and `.app` bundle resources. Menu tracking detection prevents state mutations from causing menu jumps.

* **Platform Icons**: Application icons for Windows `.exe`, Linux `.deb`/`.rpm` packages, and macOS app bundles.

### Bug Fixes

* **Bracket Highlight Hanging on Large Files**: Bracket matching now caps scanning at 1MB and uses 16KB bulk reads instead of byte-at-a-time, preventing hangs on large files.

* **Markdown Plugin Activation**: Plugin now activates based on buffer language (not just file extension), fixing cases where `Set Language` to markdown didn't enable smart editing (#1117). Reverse bullet cycling on Shift+Tab now works correctly (#1116).

* **Settings UI**: Fixed Save button mouse click not closing the dialog. Fixed Reset button not showing confirmation dialog. Fixed Discard dialog persisting on reopen.

* **Active Tab Styling Bleed**: Fixed active tab border color bleeding through dropdown menus.

* **Cursor Corruption on Tab Click**: Fixed hardware cursor appearing at wrong position when clicking a tab in an inactive split.

* **Comment Delimiter Colors**: Fixed comment delimiter characters (e.g. `//`) using the wrong color in syntax highlighting.

* **Scroll Events Routing**: Fixed mouse scroll events going to the file explorer panel regardless of mouse position.

* **File Explorer Border**: Fixed hover/drag bugs on the file explorer resize border.

* **Windows Named Pipe Crash**: Fixed crash in `Server::handle_new_connection` on Windows.

* **Bar/Underline Cursor Invisible**: Fixed bar and underline cursor styles being invisible on characters due to REVERSED modifier creating a block-like highlight (#851).

* **Wrapped Line Viewport Scroll**: Fixed viewport scroll limit counting logical lines instead of visual rows, causing erratic scrolling, skipped wrapped rows, and stuck End key with line wrap enabled (#1147).

* **Search on Large Files**: Fixed multi-GB memory consumption, O(N²) offset accumulation, and search scan never completing when capped at max matches. Chunked incremental search, viewport-only overlays, and 100K match cap (#1146).

* **macOS Menu Hover Jump**: Fixed menu bar jumping to leftmost menu during hover by using `WaitUntil` instead of `Poll` and caching menu item states.

### Improvements

* Status log and warning log buffers are now read-only.
* Replaced `buffer_modified` JS plugin with native Rust diff indicators, eliminating JS↔Rust round-trips on every edit/scroll.

### Internal

* Folding system refactored to use byte offsets instead of line numbers for gutter indicators, fixing consistency issues in large file mode.
* Unified fold indicator pipeline shared between LSP-based and indent-based folding.
* Fixed Nix build: include PNG files in source filter for GUI icon resources.

---

## 0.2.9

### Features

* **Code Folding**: Fold/unfold code blocks via LSP foldingRange. Per-view fold state, gutter indicators for collapsed ranges, fold-aware scrolling. Toggle via command palette ("Toggle Fold") (#900). Thanks @asukaminato0721 !

* **Large File Line Numbers**: Large files show byte offsets in gutter/status bar until scanned. On-demand parallel line index scanning (via Ctrl+G prompt or "Scan Line Index" command) gives exact line numbers with progress indicator. Remote scanning counts newlines server-side without data transfer.

* **Markdown Source Editing**: New plugin for smart Markdown source-mode editing — auto-continues list items on Enter (bullets, ordered lists, checkboxes), removes empty markers, Tab indents + cycles bullet style (#1095).

* **GUI mode - can run without terminal** (highly experimental): GPU-accelerated windowed mode via winit + wgpu. Build with `--features gui` and run with `--gui` to try it.

### Improvements

* **Smart Backspace Dedent**: Backspace in leading whitespace removes one indent unit (tab_size spaces or 1 tab) instead of a single character.

* **Diagnostics Panel**: Up/Down now scrolls the editor to preview diagnostic location. Enter jumps and focuses the editor.

* **Glob Patterns in Language Config**: `filenames` field now supports glob patterns (`*.conf`, `*rc`, `/etc/**/rc.*`) for extensionless file detection (#1083).

* Disabled single-quote auto-close in Markdown files (interferes with apostrophes).

### Bug Fixes

* **Auto-Indent**: Fixed `tab_size` setting ignored for auto-indent; fixed indent level lost on normal statement lines; fixed Go auto-dedent using spaces instead of tabs (#1068); fixed Python nested indent after consecutive `:` lines (#1069).

* **File Explorer Dotfiles**: Fixed dotfiles always visible regardless of "Show hidden files" toggle. Config `show_hidden`/`show_gitignored` now applied on init (#1079).

* **LSP Toggle Desync**: Fixed state corruption when toggling LSP off/on — now sends `didClose` so re-enable gets fresh `didOpen` (#952).

* **LSP Client Capabilities**: Now advertises all supported capabilities including `publishDiagnostics`, enabling diagnostics from strict servers like pyright (#1006).

* **LSP Status Indicator**: Fixed status bar indicator disappearing after each request completion (#952).

* **Set Language**: Fixed command storing display name instead of canonical ID, breaking LSP config lookups (#1078).

* **Escape Sequences in Client Mode**: Fixed mouse codes, Shift+Tab, and standalone ESC not working in `fresh -a` attach mode (#1089).

* **Client Mode Terminal Reset**: Fixed terminal not fully restored on exit in client mode (#1089).

* **Ctrl+End with Line Wrap**: Fixed viewport not scrolling to trailing empty line; fixed Down arrow not reaching it (#992).

* **Diagnostics Panel Windows Paths**: Fixed file URIs not decoded properly on Windows (#1071).

* **Debug Keyboard Dialog**: Fixed not capturing keys in client/server mode (#1089).

### Performance

* Replaced linear span lookups in syntax highlighting with O(1) amortized cursor.
* Eliminated JSON round-trip and JS re-parsing in plugin hook dispatch (~16% CPU reduction).
* Path-copying PieceTree mutations with structural diff via `Arc::ptr_eq` — O(edit regions) instead of O(all leaves).
* Viewport-aware filtering and batch API for large file gutter indicators (~780K IPC commands → ~50 per edit).

### Internal

* Update flake.nix to rust 1.92.0
* Split GUI backend into separate `fresh-gui` crate.
* Unified language detection with `DetectedLanguage` struct and single `apply_language()` mutation point.
* CI now runs clippy with `--all-features` to lint GUI code.

---

## 0.2.5

### Features

* **Persistent Auto-Save**: New `auto_save_enabled` config option (default: false) to automatically save modified buffers to their original file at a configurable interval (`auto_save_interval_secs`, default: 30s) (#542)

* **Smart Home**: Home key now uses smart home behavior by default, toggling between the first non-whitespace character and column 0. On soft-wrapped lines, smart home respects visual line boundaries instead of jumping to the physical line start (#1064).

### Bug Fixes

* **Diff View Scrollbar**: Fixed scrollbar click-to-jump and thumb drag not working in side-by-side diff views. Composite buffer views now use row-based scrolling via CompositeViewState.

* **Terminal Bracket Paste**: Fixed pasted text going into the editor buffer instead of the terminal PTY when in terminal mode (#1056).

* **LSP did_open Reliability**: Fixed buffer being incorrectly marked as LSP-opened when the did_open send fails, which prevented retry and could corrupt server document state.

* **Remote Editing Data Loss**: Fixed intermittent data loss when loading large files via SSH remote editing on macOS. The bounded channel now uses backpressure instead of silently dropping data when the buffer overflows (#1059).

### Configuration

* Renamed `auto_save_interval_secs` (recovery) to `auto_recovery_save_interval_secs` to distinguish it from the new persistent auto-save feature. Added `auto_recovery_save_interval_secs` config option (default: 2s).

### Internal

* Introduced typed `LeafId` and `ContainerId` wrappers around `SplitId` to enforce leaf-vs-container constraints at compile time.
* Enabled `#![deny(clippy::let_underscore_must_use)]` crate-wide; all ignored `Result` values now have explicit annotations or proper error handling.
* Made `request_completion` and `request_signature_help` infallible, removing dead `Result` return types.
* Added CONTRIBUTING.md with development guidelines.

---

## 0.2.4

### Features

* **Markdown Compose Mode**: Distraction-free writing mode with concealed markup, soft breaks, table rendering, and mouse support. Split-view allows editing source and rendered markdown side-by-side with synchronized scrolling.

* **Vertical Rulers**: Configurable column rulers with add/remove commands via command palette. Per-buffer state and Settings UI JSON editor support (#1028).

* **Horizontal Scrollbar**: New horizontal scrollbar with drag support and toggle commands (#972).

* **Smooth Scrolling**: Cursor movement now scrolls one line at a time instead of jumping/recentering the viewport (#1040).

### Improvements

* **Macro Keybinding**: F4 shortcut for Play Last Macro. Removed Ctrl+0-9 and Alt+Shift+0-9 macro keybindings (#700).

* **Configurable Clipboard**: New `clipboard` config with `use_osc52` and `use_system_clipboard` toggles to prevent hangs in certain terminals (#964). Useful for Putty and other terminals that sometimes cause Fresh to hang on OSC 52.

* **Scrollbar Visibility**: New `show_vertical_scrollbar` and `show_horizontal_scrollbar` config options (#974).

* **Package Manager**: Reinstall support for local-path packages.

* **File Explorer Persistence**: Show hidden and show gitignored toggles now persist to config immediately (#569).

### Bug Fixes

* **Macro correctness**: Replaying a macro now respects the exact same effect as interactive flow.

* **Cursor Navigation**: Cursor up/down now lands at end-of-line when goal column is past content (#514).

* **Line Numbers**: Fixed line numbers leaking between splits due to shared margins state. Line numbers now live exclusively in per-split BufferViewState.

### Plugin API

* **Plugin API v2**: Versioned plugin API with `createTerminal`, `sendTerminalInput`, `closeTerminal`, `getAllCursors`, and plugin state API. Improved type declarations in `fresh.d.ts` (#1045).

* **Split Labels**: Splits can be labeled to prevent files opening in managed splits. Labels persist across save/restore. New `before` option to place buffers left/top.

### Internal

* Refactored per-buffer view state: cursors owned by SplitViewState, ComposeState extracted from EditorState.
* Conceal ranges, soft breaks, and overlay filtering by view_mode at render time.
* Plugin state snapshot reports active split's view_mode and compose flag.
* i18n updates for vertical rulers and macro shortcuts across all locales.
* PieceTree Performance: Use binary search instead of linear scan in line lookup.

---

## 0.2.3

### Bug Fixes

* **Undo Corruption After Save**: Fixed undo corrupting text after a bulk edit (e.g. multi-cursor or replace-all) followed by save. Piece tree buffers are now restored alongside the tree during undo.

* **Theme Not Found After Restart**: Fixed theme reverting to default when the JSON `name` field differs from the filename (#1001).

* **AltGr+Shift Input on Windows**: Fixed AltGr+Shift key combinations not being recognized as text input (#993).

* **Winget Manifest**: Fixed `UpgradeBehavior` indentation causing schema validation failure in winget publishing.

### Internal

* Added shadow model undo/redo property-based tests for increased piece tree correctness confidence.
* Added blog showcase system with animated GIF demos for the documentation site.

---

## 0.2.2

### Bug Fixes

* **Regex Find-and-Replace**: Fixed regex mode not working. Added capture group support (`$1`, `$2`, `${name}`) and a toolbar hint when regex mode is active.

* **Keybinding Editor**: Fixed actions with no default keybinding missing from the editor; all actions are now listed. Fixed inability to delete built-in keymap bindings. Fixed deleted bindings reappearing after save.

* **LSP Completion Popup**: Fixed completion popup blocking typing when not working in non-English locales.

* **Revised in-editor help**: Rewrote the in-memory help manual, should be a bit more useful.

### Internal

* Refactored keybinding editor into multi-file module.
* Locked Cargo.toml dependency versions to minor; docs recommend `--locked`. Dependency updates.
* ~53 documentation clarity fixes.

---

## 0.2.0

### Features

* Experimental **Session Persistence**: Detach from and reattach to editor sessions with full state preservation. Start with `fresh -a <name>` or `fresh -a` (directory-based), detach via File menu or command palette. Sessions persist across terminal disconnections. Use `fresh --cmd session list/kill/attach` and `fresh --cmd session open-file NAME FILES` to manage sessions from the command line. Allows using Fresh across other applications, e.g. yazi edit action triggers a file open in Fresh.

* **Keybinding Editor**: Full-featured editor for customizing keybindings. Search by text or record key, filter by context/source, add/edit/delete bindings with conflict detection and autocomplete. Try menus: Edit..Keybinding Editor, or command palette. Changes are saved in config.json

### Improvements

* **Line Editing**: Move lines up/down and duplicate lines, matching modern editor behavior. Multi-cursor support (@Asuka-Minato).

* **Triple-Click Selection**: Triple-click selects entire line (#597).

* **Vietnamese Localization**: Full Vietnamese (Tiếng Việt) language support.

* **Typst Language Support**: Syntax highlighting and tinymist LSP configuration for `.typ` files (#944).

* **LSP Improvements**:
  - Per-buffer LSP toggle command to enable/disable LSP for individual files
  - Default LSP configs for bash, lua, ruby, php, yaml, toml (#946)

### Bug Fixes

* **LSP Document Sync**: Fixed document corruption when LSP servers received didChange after didOpen, and when bulk edits (selection replacement, multi-cursor) bypassed LSP notifications.

* **LSP Completion Popup**: Fixed popup swallowing non-word characters, arrow keys, and other keys. Popup now dismisses correctly allowing keystrokes to pass through (#931)

* **LSP Diagnostics**: Fixed diagnostic gutter markers not appearing on implicit trailing lines with zero-width ranges (clangd-style diagnostics).

* **Line Wrapping**: End/Home keys now navigate by visual line when wrapping is enabled, matching VS Code/Notepad behavior (#979).

* **Syntax Highlighting**: Fixed highlighting lost when saving files without extension (shebang detection) outside working directory (#978).

* **Buffer Settings**: User-configured tab size, indentation, and line numbers now preserved across auto-revert.

* **Terminal Scrollback**: Any character key exits scrollback mode instead of just 'q' (#863).

* **32-bit ARM Build**: Fixed setrlimit type mismatch on ARMv7l platforms (#957).

### Configuration

* Added C++20 module extensions (.cppm, .ixx) for C++ syntax highlighting (#955).

### Documentation

* Added FreeBSD installation note (@lwhsu).

---

## 0.1.99

### Features

* **Windows Terminal Support**: Full terminal emulation on Windows using ConPTY (Windows 10 1809+). Handles PowerShell DSR cursor queries, prefers PowerShell over cmd.exe, and supports stdin piping (`type file | fresh`).

* **Text Encoding Support**: Detect and convert files in UTF-8, UTF-16 LE/BE, Latin-1, Windows-1252, Windows-1250, GBK, Shift-JIS, EUC-KR, and GB18030. Encoding shown in status bar (clickable to change). "Reload with Encoding..." command in File menu. Confirmation prompt for large files with non-resynchronizable encodings (#488).

* **Encoding Selection in File Browser**: Toggle "Detect Encoding" with Alt+E when opening files. When disabled, prompts for manual encoding selection.

* **Bundle Package Type**: New package type containing multiple languages, plugins, and themes in a single package. Shown with "B" tag in package manager.

* **Space-Separated Fuzzy Search**: Queries with spaces are now split into independent terms, all of which must match. For example, "features groups-view" now matches "/features/groups/groups-view.tsx" (#933).

### Bug Fixes

* Fixed Escape key not closing the manual and keyboard shortcuts pages (#840).

* Fixed scrollbar and mouse wheel scrolling not working with line wrap enabled.

* Fixed scrollbar thumb drag jumping to mouse position instead of following drag movement.

* Fixed AltGr character input not working on Windows (#762).

* Fixed custom themes not appearing in "Select Theme" on macOS due to incorrect config path resolution.

* Fixed LSP servers registered via plugins being disabled by default.

* Fixed language packs being installed to plugins directory instead of languages directory.

* Fixed theme changes not persisting when selecting the default theme.

* Fixed popup positioning not accounting for file explorer width (#898).

* Fixed LSP did_open sending wrong language for multi-language LSP servers.

* Fixed manual LSP start not working when LSP config was disabled; settings now sync immediately.

### Internal

* Refactored config path handling to pass DirectoryContext via call chain instead of static methods.

* Added shadow model property-based tests for TextBuffer.

* Bumped tree-sitter (0.26.5), actions/checkout (v6), actions/upload-pages-artifact (v4) (@dependabot).

---

## 0.1.98

### Features

* **File Explorer Quick Search**: Type to filter files/directories with fuzzy matching. ESC or Backspace clears the search (#892).

* **Sort Lines Command**: New command to alphabetically sort selected lines.

* **Paragraph Selection**: Ctrl+Shift+Up/Down extends selection to previous/next empty line.

* **Local Package Install**: Package manager now supports installing plugins/themes from local file paths (e.g., `/path/to/package`, `~/repos/plugin`).

* **Plugin API**: Added `setLineWrap` for plugins to control line wrapping.

### Bug Fixes

* Fixed data corruption when saving large files with in-place writes.

* Fixed UI hang when loading shortcuts in Open File dialog (#903).

* Fixed file explorer failing to open at root path "/" (#902).

* Fixed Settings UI search results not scrolling properly (#905).

* Fixed multi-cursor cut operations not batching undo correctly.

---

## 0.1.96

### Features

* **Visual Line Movement**: Up/Down arrows now move by visual lines when line wrap is enabled, matching expected behavior in wrapped text.

### Bug Fixes

* Fixed excessive filesystem polling during render, especially on remote filesystems like rclone mounts (#886).

### Packaging

* **FreeBSD Release**: FreeBSD x86_64 binaries now included in releases (#887).

---

## 0.1.95

### Bug Fixes

* Fixed data corruption issue when saving a large file multiple times (#882)

* Fixed hidden menus showing up when using left/right arrow keys to move between menus

* Fixed language pack plugins not being loaded properly in some cases

## 0.1.94

### Documentation

* **New documentation site**: @radiorambo contributed a complete restructure and build for a documentation section in the website. Kudos, awesome work!

See [getfresh.dev/docs](https://getfresh.dev/docs)

### Features

* **Event Debug Dialog**: New diagnostic tool for troubleshooting keyboard and terminal issues. Shows raw key codes and modifiers as they are received, helping diagnose keybinding problems. Access via Command Palette → "Event Debug".

* **File Explorer Keybindings**: Reorganized the keys and updated the docs. Ctrl+E now toggles focus between file explorer and editor. Ctrl+B toggles sidebar visibility. Single-click opens files without leaving explorer; double-click or Enter opens and focuses editor (#748).

### Bug Fixes

* **Case Conversion Enhancement**: To Upper (Alt+U) and To Lower (Alt+L) now automatically select the current word when no text is selected, matching common editor behavior.

* **Block Selection Copy**: Fixed Ctrl+C copying entire lines instead of the rectangular region. Block selection (Alt+Shift+Arrow) now correctly copies only the characters within the column bounds for each line.

* **Block Selection Editing**: Block selection now converts to multiple cursors for editing actions (typing, delete, backspace), enabling proper rectangular editing.

* **Dropdown Menu Position**: Fixed Help menu dropdown appearing at wrong position when Explorer menu was hidden. Menu position calculation now correctly skips hidden menus.

* **Settings Access**: Moved Settings to Edit menu and removed broken Ctrl+, keybinding which doesn't work reliably in terminals. Settings remain accessible via Edit → Settings... and Command Palette.

* **Block Selection Rendering**: Fixed double rendering of block selections that could cause visual artifacts.

### Internal

* **Remote Save Optimization**: SSH remote editing now uses recipe-based patched saves. For large files with small edits, only the changed portions are transferred instead of the entire file. A 10MB file with a 100-byte edit now transfers ~200 bytes instead of 10MB.

---

## 0.1.93

### Experimental

* **SSH Remote Editing**: Edit files on remote machines via SSH using `fresh user@host:path`. Supports password/key auth, sudo save, and file explorer integration.

### Features

* **Bracket Matching**: Highlight matching brackets with rainbow colors based on nesting depth. Configurable via `highlight_matching_brackets` and `rainbow_brackets`.
* **Whitespace Cleanup**: New `trim_trailing_whitespace_on_save` and `ensure_final_newline_on_save` options, plus manual commands.
* **Shift+Click Selection**: Extend selection to clicked position with Shift+click or Ctrl+click.
* **Terminal Mouse Forwarding**: Mouse events forwarded to terminal in alternate screen mode (vim, htop, etc.) (#853).
* **Tab Bar Scroll Buttons**: Click `<`/`>` buttons to scroll through tabs.
* **Library Files Protection**: Files outside project root are read-only and have LSP disabled.
* **Buffer Focus History**: Closing a buffer returns to previously focused buffer instead of adjacent tab.

### Bug Fixes

* **Multi-Cursor Cut**: Fixed cut not deleting all selections with multiple cursors.
* **Tab Scroll**: Fixed tab scroll buttons and active tab visibility.

### Packaging

* **AUR aarch64**: Added aarch64 support for Arch Linux ARM (#856).

### Internal

* Nix: Switched to `flake.parts`, added `shell.nix`/`default.nix` compatibility (@drupol).

---

## 0.1.90

### Features

* **Package Manager**: Browse, install, and uninstall plugins, themes, and language packs from the [official registry](https://github.com/sinelaw/fresh-plugins-registry). Features search, package validation, background registry sync with local caching, and automatic theme reloading after install.
  - **Language packs** bundle syntax highlighting (`.sublime-syntax`), language settings, and LSP server configuration
  - Filter by package type: Plugins, Themes, Languages
  - See [fresh-plugins](https://github.com/sinelaw/fresh-plugins) for example packages

* **Command Palette** (Ctrl+P): Unified prompt for navigating files, commands, buffers, and lines. Use prefix characters to switch modes:
  - No prefix: fuzzy file finder
  - `>` prefix: commands
  - `#` prefix: switch open buffers by name
  - `:` prefix: go to line number

  Includes hints line showing available prefixes and Tab completion.

* **Status Message Log**: Click status bar messages to view full message history.

* **Package Scaffolding (`--init`)**: Create new plugin, theme, or language pack projects with `fresh --init`. Interactive wizard generates package.json, entry files, and proper directory structure.

* **Theme Schema**: JSON Schema for theme validation. Use `scripts/validate-theme.sh` or any JSON Schema validator.

### Bug Fixes

* **Bracket Expansion**: Pressing Enter between matching brackets expands them with proper indentation (#629).
* **Ctrl+D Word Selection**: Ctrl+D selects the entire word when no selection exists.
* **Ctrl+Right Word Jump**: Ctrl+Right jumps to word end, matching Ctrl+Shift+Right behavior.
* **Alt+N/P Search**: Search invalidates when cursor moves manually, preventing stale matches.
* **Theme Fallback**: Falls back to default theme when configured theme is not found.
* **Cross-Platform Theme Paths**: Theme path handling works correctly on Windows.

### Internal

* Moved calculator, color-highlighter, todo-highlighter plugins to external repository (installable via package manager).
* Moved catppuccin and xscriptor themes to external repository (installable via package manager).
* Added WASM feature flag for shared editor core modules.
* Italian translation update (#839).

---

## 0.1.88

### Features

* **Status Bar Language Indicator**: Click the language name in the status bar to change syntax highlighting. Supports mouse wheel scrolling and type-to-filter.
* **VS Code-like Completion UX**: Debounced completion triggers, Tab accepts completion, uppercase letters work in type-to-filter.
* **Per-Language LSP Root URI**: LSP servers can now have per-language root URI detection. Includes automatic C# project root detection via `.csproj` files.
* **Settings UI Improvements**: Settings organized by topic sections, improved focus colors, search navigates to setting, better Map control navigation.

### Bug Fixes

* **Tab Bar Mouse Events**: Fixed clicks on tabs not working when menu bar is hidden (#832).
* **LSP Deadlock**: Fixed deadlock when LSP server sends requests while client is awaiting a response.
* **LSP Root URI**: Include `root_uri` in LSP initialize params for server compatibility.
* **Terminal Scrollback**: Fixed race condition truncating terminal backing file when PTY already wrote content.
* **Plugin i18n**: Fixed placeholder format to use `%{variable}` syntax.
* **Settings UI**: Fixed confirm dialog mouse clicks/Tab navigation, dropdown option selection, search result navigation, and content bleeding into footer.

### Packaging

* **Winget**: Added Windows Package Manager (winget) publishing to release pipeline.

### Internal

* **FileSystem Trait**: New IO abstraction layer enabling different backends (local, remote, WASM). All filesystem operations now use injectable `FileSystem` trait.

---

## 0.1.87

### Features

* **Language Support**: Added LSP configurations and syntax highlighting for Zig, Java, LaTeX, Markdown, and Templ.
* **Git File Highlighting**: Syntax highlighting for git-related files (.gitignore, .gitattributes, .gitmodules).
* **Plugin Type Safety**: TypeScript type definitions for plugin API with compile-time validation.

### Bug Fixes

* **Hover Popup**: Fixed scrolling to bottom, dismiss on click outside, block clicks inside popup.
* **Settings UI**: Fixed overwriting manual config.json edits when saving from Settings UI (#806).
* **Windows Terminal**: Fixed truecolor detection and 256-color grayscale conversion overflow.
* **Composite Buffers**: Fixed mouse click sync, deserialization errors, and cursor positioning.
* **Plugin Stability**: Plugin thread panics now propagate to main thread for proper error handling.
* **Review Diff Plugin**: Fixed side-by-side diff commands not appearing in command palette.

---

## 0.1.86

### Features

* **Popup Text Selection**: Select and copy text from LSP hover popups and tooltips. Click and drag to select, Ctrl+C to copy.
* **File Explorer Status Tooltips**: Hover over git status indicators (M, U, A) to see detailed explanations and diff stats. Directory tooltips show list of modified files.
* **Terminal Background Transparency**: New `use_terminal_bg` config option allows terminal transparency or custom backgrounds to show through the editor (#640).
* **Vi Mode Improvements**: Added `:w filename` to save to path, `:wq filename` to save and quit, `:q!` to force quit without saving. Added Ctrl+P (command palette) and Ctrl+Q (quit) to all vi modes.

### Bug Fixes

* **Settings UI Add Button**: Fixed "Add New" button not appearing for LSP and Languages maps in Settings UI.
* **LSP Hover Markdown**: Improved markdown rendering - soft breaks now create newlines (fixing Python docstring formatting), inline code rendered without visible backticks.
* **Symlink Directories**: Fixed symlinks to directories not showing expand marker and causing "Is a directory" error when opened (#787).
* **Live Grep Preview**: Fixed preview not updating when navigating through search results (#636).
* **Terminal Keyboard State**: Fixed arrow keys and Enter not working after exiting the editor due to Kitty keyboard protocol cleanup issue (#773).
* **Plugin Commands Visibility**: Fixed many plugin commands (Toggle Vi Mode, Git Blame, Diagnostics Panel, etc.) not appearing in command palette.

### UI Changes

* **File Explorer Layout**: Git status indicators moved to rightmost column, matching VS Code's layout. Removed file size and item count for cleaner appearance.
* **Quieter Startup**: Removed plugin "ready/loaded" status messages that cluttered the status bar on startup.

### Internal

* Separated I/O from pure types in theme and grammar modules for better testability and future WASM compatibility.
* Fixed workspace crate dependencies for crates.io publishing.
* Improved install.sh reliability for containers and edge cases.

---

## 0.1.83

### Breaking Changes

* **QuickJS Plugin Runtime**: Replaced Deno with QuickJS for the plugin system. Each plugin now runs in its own isolated context.

### Features

* **Cargo Workspace Architecture**: Refactored into modular crates (fresh-core, fresh-editor, fresh-languages, fresh-parser-js, fresh-plugin-runtime, fresh-plugin-api-macros).

### Bug Fixes

* **Toggle Comment YAML**: Fixed toggle comment not working for YAML files by falling back to config-based language detection (#774).
* **Undo History Panic**: Fixed panic when undoing past a save point and making new edits caused out-of-bounds slice access (#776).
* **Sudo Save Prompt**: Fixed permission denied crash when saving files owned by another user; now shows sudo prompt correctly (#775).
* **Musl Plugin Support**: Plugins now work on musl target builds (x86_64/aarch64-unknown-linux-musl).
* **LSP Server Requests**: Fixed LSP server-to-client request handling not being dispatched to plugins.
* **Git Find File Selection**: Fixed race condition causing wrong file selection when pressing Enter quickly.
* **Plugin Cache**: Embedded plugins now cached in XDG cache dir instead of leaking temp directories.

### Internal

* Improved compile times via LLVM optimization flag.
* Cross-platform path handling fixes for Windows.
* Test reliability improvements.

---

## 0.1.77

### Documentation

* **macOS Terminal Tips**: Added keyboard enhancement flags configuration guide.

### Features

* **LSP Semantic Highlighting** (@Asuka-Minato).
* **macOS Keybinding Display**: Native symbols (⌃, ⌥, ⇧) instead of Ctrl+/Alt+/Shift+.
* **Odin Language Support**: Syntax highlighting (sublime-syntax from @Tetralux) and OLS LSP configuration (@xoxorwr).
* **File Explorer Git Indicators**: Shows modified/added status for files and folders via new plugin (#526) (@Asuka-Minato).
* **Keyboard Enhancement Flags Config**: New config options for more granular control over kitty protocol usage (`keyboard_disambiguate_escape_codes`, `keyboard_report_event_types`, `keyboard_report_alternate_keys`, `keyboard_report_all_keys_as_escape_codes`).

### Bug Fixes

* **Menu Keybinding Display**: Consistent keybinding symbols in menus on macOS (#703).
* **Git Find File Popup**: Smart path truncation preserving filename (#707).
* **File Owner Preservation**: Preserve owner when saving files with group write privileges (#743).

### Internal

* Telemetry and update checks now debounce to once per day.
* Terminal mode handling refactored into dedicated module.
* Resolved ~300+ clippy warnings.
* Bumped url (2.5.8), libc (0.2.180) (@dependabot).

---

## 0.1.76

### Features

* **Anonymous Telemetry**: Basic anonymous telemetry (version, OS, terminal type) sent with update checks. Disable via `check_for_updates` config or `--no-upgrade-check` flag.
* **Toggle Tab Bar/Menu Bar**: Hide or show tab bar and menu bar via command palette or View menu (#618).
* **Plugin Enable/Disable**: New config options to enable or disable individual plugins.
* **Improved Settings UI**: Layer-aware modified indicators, column headers for Map controls, visual indication for read-only fields in Settings UI entry dialogs.
* **Git Grep Preview**: Live preview panel with debouncing for Git Grep results.

### Bug Fixes

* **Map Control Click**: Fixed "Add new" button requiring double-click instead of single click (#604).
* **File Explorer Session**: Persist `show_hidden` and `show_gitignored` settings across sessions (#569).
* **Line Numbers Config**: Respect `line_numbers` config when launching without a file argument (#539).
* **Find References UX**: Now uses prompt mode for consistent search experience.
* **i18n Placeholders**: Fixed string interpolation format in plugin translations (#706).

### Internal

* ResultsPanel abstraction with VS Code-style Provider pattern for plugin UI.
* TypeScript type checking for plugins.
* Test reliability improvements for e2e tests.

---

## 0.1.75

This is mostly a bugfix release.

### Bug Fixes

* **Prompt History**: Generic prompt history system with Up/Down navigation, now available for Go to Line and other prompts.
* **Session External Files**: Files opened from outside the project directory are now restored in sessions.
* **Fuzzy Search Exact Match Priority**: Open File dialog now prioritizes exact filename matches over fuzzy matches.
* **Horizontal Scroll**: Fixed cursor position with horizontal scroll after Open File dialog and pressing Enter on long lines.
* **Multi-Cursor Bracket Skip**: Fixed bracket skip-over with multiple cursors in bulk edit.
* **F3 Search**: Fixed F3 to allow searching more after editing and to update positions correctly after buffer modifications.
* **File Explorer**: Removed plain letter shortcuts causing accidental actions, fixed focus after rename/delete, improved new file command behavior.
* **Terminal**: Fixed scrollback colors, mouse scroll now exits to scrollback mode, fixed viewport position bugs, persist exit message.
* **Theme Editor**: Fixed reopening after closing the theme editor, allow editing builtin themes (#696), store builtin themes as json instead of hardcoded inside rust.
* **LSP Diagnostics**: Made diagnostic cache per-buffer to prevent marker position bugs.
* **Cursor Visibility**: You can see the letter under the block cursor now! Apply REVERSED style to primary cursor for better visibility.
* **Open Terminal**: Command now available in all contexts.
* **Open File Dialog**: When run while a terminal is focused, use CWD instead of the internal backing file directory.

### Internal

* Refactored reference highlighting to use overlay system (#694).
* Built-in themes now loaded from JSON artifacts at build time instead of hardcoded Rust.
* Removed duplicate dead code from LspTask.

---

## 0.1.74

### Features

* **Italian Locale**: Full Italian translation support added across the editor and all core plugins (@fdefilippo).
* **Interactive Links in Popups**: Markdown popups (such as LSP hover) now support clickable hyperlinks (OSC 8). Clicking a link opens it in your default web browser (@Asuka-Minato).
* **Sudo Save Fallback**: When saving a file fails due to insufficient permissions, the editor now offers to save using `sudo` (Linux/macOS) (#301).
* **Improved Language Features**: Improved word navigation, auto-pairs, and multi-cursor behavior.

### Bug Fixes

* **LSP Hover Reliability**: Fixed multiple issues with hover popups, including race conditions during rapid mouse movement, incorrect positioning on empty lines, and popups triggering past the end of a line.
* **Popup Scrollbar Drag**: You can now click and drag the scrollbar in popups (like hover and completion) to scroll through long content.
* **Inlay Hint Positioning**: Corrected inlay hint placement in Rust files to prevent them from shifting line content (#626, @Asuka-Minato).
* **Theme Editor Path Resolution**: Fixed a bug where the theme editor couldn't find the correct configuration directory on some systems.

### Internal

* **Error Handling**: Migrated to `anyhow` for more robust error tracking and backtraces.
* **Plugin API**: Added `editor.getConfigDir()` and `editor.getThemesDir()` to the plugin API.
* **Dependency Updates**: Bumped `clap` to 4.5.54.

---

## 0.1.71

### Features

* **Side-by-Side Diff View**: Word-level highlighting, synchronized scrolling, cursor navigation.
* **Theme Editor**: JSON Schema API, color swatches, command palette integration, delete theme.
* **Create Files from Open Dialog**: Type non-existent filename to create new buffer.
* **Tilde Expansion**: `~/path` works in Save As, Open File, Switch Project.

### Bug Fixes

* **Toggle Comment**: Use language config for comment prefixes, preserve selection, don't hang (#681).
* **Split Close**: Close split when closing last buffer instead of empty buffer.
* **Terminal**: Resume mode on buffer switch, sync content, clean up on close.
* **Hidden Buffers**: Skip in next/prev buffer, fix tab click targets.

### Internal

* Plugin i18n completeness tests. Bumped libc, tokio, tree-sitter-lua.

---

## 0.1.70

### Features

* **Input Calibration Wizard**: New wizard to calibrate keyboard input for terminals with broken key sequences. Access via "Calibrate Keyboard" in command palette or View menu. Uses failsafe ASCII-only navigation (#219).

* **Terminal Cursor Color**: Cursor color now set via OSC 12 escape sequence for proper visibility across all themes, especially light theme.

### Bug Fixes

* **Dynamic Keybinding Hints**: Status messages now show actual keybindings from keymap instead of hardcoded shortcuts (#659).

* **Search in Large Files**: Fixed "Buffer not fully loaded" error when searching in large plain text files (#657).

* **LSP Config Preservation**: Fixed LSP command field becoming empty when toggling enabled state. Partial config now merges with defaults (#630, #631).

* **Multi-Cursor End of Line**: Fixed secondary cursors rendering at line start instead of end (#632).

* **Selection at Cursor**: Fixed selection background not showing at primary cursor position with bar/underline cursor styles (#614).

* **Locale Interpolation**: Fixed locale name not appearing in "Locale changed" message (#624).

* **Cursor Past Trailing Newline**: Allow cursor to navigate to the empty line after trailing newline (#622, @Asuka-Minato).

* **.env Syntax Highlighting**: Added .env to default shell syntax patterns (#559).

* **Spanish Translation**: Fixed typo in menu bar (@osniel).

* **Audit Mode Keybindings**: Use Emacs-style key notation in diff-view bindings (@xunzhou).

### Internal

* Refactored config system to use layered PartialConfig resolution everywhere.
* Code cleanup: use `Self` where possible, merge match arms (@adamnemecek).
* Clean up log output by resetting to column zero (@Martin-Häcker).
* Bumped windows-sys to 0.61.2 (@dependabot).

---

## 0.1.69

> **macOS Users**: This release includes significant improvements for macOS terminal compatibility. See the new [macOS Terminal Tips](docs/USER_GUIDE.md#macos-terminal-tips) guide for recommended terminal emulators and keyboard configuration. The macOS keymap ([`keymaps/macos.json`](keymaps/macos.json)) is a work in progress—please submit patches based on your experience with different terminals and keyboard layouts!

### Features

* **macOS Keymap**: Terminal-friendly keybindings that avoid broken Ctrl+Shift combinations, ASCII control char collisions (Ctrl+J=LF), and international keyboard conflicts (Ctrl+Alt+L=@ on German). Key bindings: Ctrl+R (redo), Ctrl+G (find next), Ctrl+L (go to line), Ctrl+T (go to symbol), Alt+B/F (word movement). See [macOS Terminal Tips](docs/USER_GUIDE.md#macos-terminal-tips) (#219).

* **4-Level Config System**: Configuration now merges user, platform, project, and session layers. Settings UI shows layer indicators and allows editing specific config files.

* **Tab Context Menu**: Right-click tabs for Close, Close Others, Close All, Close to Right options.

* **Drag-to-Split Tabs**: Drag tabs to screen edges to create new splits.

* **Plugin Logging**: New `editor.error()`, `editor.warn()`, `editor.info()`, `editor.debug()` methods route plugin output through the editor's logging system.

* **Log Management**: Logs moved to XDG state directory with automatic 24-hour cleanup. Use `--show-paths` to see log locations.

### Experimental

*These features are work-in-progress. Expect rough edges and breaking changes.*

* **Internationalization (i18n)**: Full i18n support with 11 languages (German, French, Spanish, Japanese, Korean, Chinese, Russian, Ukrainian, Czech, Portuguese, Thai). Select locale via command palette or Settings UI. Plugins support translation via `editor.t()` and `.i18n.json` files. *Note*: Keybinding shortcuts do not take the active layout into account, which is why this feature is still experimental. Also I need you to provide feedback on the translations since they were all machine-generated and I don't speak any of the languages added.

* **Vi Mode Plugin**: Added `.` repeat command, visual block mode, and colon command mode with comprehensive vim commands (`:w`, `:q`, `:wq`, `:e`, `:split`, etc.).

* **Review Diff Plugin**: Side-by-side diff view with synchronized scrolling, line alignment, and word-level highlighting. Access via "Side-by-Side Diff" command.

### Bug Fixes

* **Tab Size Zero Panic**: Fixed division by zero when tab_size is 0 (#580).

* **Hidden Cursor Panic**: Fixed crash when rendering buffers with hidden cursors (#607, yoooughtul).

* **Settings Paste**: Fixed clipboard paste not working in Settings UI edit dialogs (#605, Tyooughtul).

* **Show Hidden Truncation**: Fixed "Show Hidden" checkbox label truncated in file dialog (#558).

* **Syntax Highlighting Config**: User-configured filename patterns now work for syntax highlighting (#565).

* **Replace All Performance**: Fixed O(n²) performance issue causing hangs on large files (#564).

* **Plugin Thread Hang**: Fixed plugin thread hanging on shutdown.

* **File Explorer Crash**: Fixed crash when scroll_offset exceeds tree size (#562).

* **Background Revert Jump**: Fixed viewport jumping when auto-reverting background files.

* **Scrollbar Gaps**: Render scrollbars with background fills to avoid glyph gaps in some terminals (Oleksii Smotrov).

### Performance

* **BulkEdit Operations**: Multi-cursor and replace-all now use O(n) algorithm instead of O(n²).

* **Semantic Highlighting**: Debounced to reduce CPU usage during rapid cursor movement.

---

## 0.1.67

### Features

* **Find Selection Next/Previous**: Search for word under cursor without opening find panel. Ctrl+F3/Ctrl+Shift+F3 or Alt+N/Alt+P (#489).

* **Cursor Style Configuration**: Configure terminal cursor style (block/bar/underline, blinking/steady) via command palette (#341).

* **Case Conversion**: Transform selected text to uppercase (Alt+U) or lowercase (Alt+L) (#522).

* **Folder Modified Indicators**: Parent folders show dot indicator when containing unsaved files (#526).

* **Line Ending Indicator**: Status bar shows LF/CRLF/CR, clickable to change. Conversion on save, configurable default (#487).

### Experimental

*These features are work-in-progress. Expect rough edges and breaking changes.*

* **LSP Helper Plugins**: Popup with install commands when LSP server not found for Python, Rust, TypeScript (#502).

* **Vi Mode Plugin**: Full vi-style modal editing with normal/insert/visual modes, operators (d/c/y), motions (hjkl, w/b/e, gg/G), text objects (iw, i", i(), etc.), counts, and find character (f/t).

* **Review Diff Plugin**: Code review for AI-generated changes or git diffs. Side-by-side view with synchronized scrolling, line comments, approve/reject/stage actions, export to Markdown/JSON.

### Bug Fixes

* **Line Numbers with Wrapped Lines**: Fixed line numbers desyncing when scrolling through wrapped lines (#552).

* **Click Past End of Line**: Now positions cursor at line end instead of next line start (#547).

* **Line Wrapping**: Fixed characters being clipped at wrap boundaries with tabs and grapheme clusters (#550).

* **Zsh Dotfiles**: .zshrc, .zprofile, .zshenv now highlighted as shell scripts (#537).

* **Cursor on Status Bar**: Fixed cursor jumping to status bar when scrolling to end of file (#468).

* **Large Single-Line Files**: Fixed memory exhaustion and 100% CPU on files like minified JSON (#481).

* **Config Editor Keys**: Fixed Delete, Home/End, Ctrl+A in JSON text box.

* **Search Term Persistence**: Alt+N/Alt+P keeps original search term when landing on longer word.

### Packaging

* **AUR**: Use stable source tarball to fix sha256sum validation failures.

---

## 0.1.65

### Features

* **Warning Indicators**: Non-intrusive warning notifications in the status bar. Click or use commands to view warnings, with domains for LSP and general warnings.

* **Format Buffer Command**: Explicit command to format the current buffer on demand.

* **Config Applied on Open**: `line_numbers` and `line_wrap` settings now properly apply when opening new buffers.

### Bug Fixes

* **Settings Persistence**: Fixed settings not persisting after save and reopen (#474, #457).

* **SaveAs Overwrite Confirmation**: Added confirmation dialog when SaveAs would overwrite an existing file (#476).

* **Multi-Byte Character Input**: Fixed panic when editing multi-byte characters in text inputs and prompts (#466).

* **TextList Dialog**: Fixed add-new input not rendering in entry dialogs.

---

## 0.1.64

* To prevent accidental deletion of files, removed 'd' / delete key bindings from File Explorer, changed the underlying delete to show a prompt and to move files to trash instead of really deleting.

## 0.1.63

### Features

* **Shell Command Prompt**: Pipe buffer or selection through shell commands (Alt+|).

* **On-Save Actions**: Run formatters/linters on save. Default formatters included for Rust (rustfmt), JavaScript/TypeScript (prettier), Python (ruff), C/C++ (clang-format), Go (gofmt).

* **Stdin Input**: Pipe content via stdin with background streaming (`echo "hello" | fresh -`).

* **Multi-File CLI**: Open multiple files from command line (#389).

* **Tab Indent Selection**: Tab indents selected lines, Shift+Tab dedents (#353).

* **Toggle Menu Bar**: Hide/show menu bar via command palette for extra screen space.

* **Global File Positions**: Cursor/scroll positions stored globally per file, not per project (#423).

* **Relative Line Numbers**: Show relative distances from cursor in gutter for easier vim-style navigation. Enable via `relative_line_numbers` config (#454).

### Bug Fixes

* **On-Save Missing Tools**: Graceful handling when formatter/linter command not found.

* **Settings UI Nested Dialogs**: Fixed nested ObjectArray navigation and save not persisting (e.g., editing on_save inside language config).

* **Live Grep Working Directory**: Fixed search plugins using process cwd instead of project working directory.

* **Open File Path Resolution**: Fixed relative paths resolving incorrectly when editor launched from different directory.

### Performance

* **Live Grep UI**: Fixed UI freezing for seconds during large codebase searches by making plugin event loop non-blocking.

### Internal

* Embedded plugins in binary as fallback for cargo-binstall (#416).

* Removed duplicate theme JSON files (#438).

* Extracted modules from mod.rs (file_operations, split_actions, clipboard, etc.).

* Pinned Rust 1.92 via rust-toolchain.toml (#338).

* Windows build switched from MSVC to GNU target.

---

## 0.1.59

### Features

* **Copy with Formatting**: Copy selected text as HTML with syntax highlighting. Works in Google Docs, Word, etc. Available via Edit menu submenu or command palette.

* **Pascal Language Support**: Auto-indentation and semantic highlighting for `.pas` and `.p` files (@casibbald).

* **Set Line Ending Command**: Change buffer line ending format (LF/CRLF/CR) via command palette.

* **Buffer Settings Commands**: Toggle auto_indent, use_tabs, and tab_size via command palette.

* **Settings UI**: Recursive dialog stack for nested arrays/maps, focus indicators, Ctrl+S to save, select-all on number input edit.

### Bug Fixes

* **Tab Size Config**: Fixed tab_size config not being respected (#384).

* **Windows Multi-Line Paste**: Fixed CRLF paste appearing as single line (#427).

* **CRLF Highlighting**: Fixed syntax highlighting offset drift in CRLF files.

* **CRLF Cursor**: Fixed cursor invisible at end of line in CRLF mode.

* **Menu Navigation**: Keyboard navigation now skips disabled items.

* **Cut/Copy Disabled**: Menu items grayed out when no selection.

### Internal

* Extracted CRLF helpers, consolidated TextMateHighlighter into TextMateEngine.

* Updated insta (1.45.0), deno_core (0.376.0).

---

## 0.1.57

### Bug Fixes

* **External Paste with Prompts**: Fixed paste via terminal (Ctrl+Shift+V / bracketed paste) going to editor instead of open prompt (#406).

* **Block Selection Escape**: Fixed Escape key not canceling block selection mode (#405).

* **CRLF Line Endings**: Fixed CRLF handling to preserve original line endings. Enter inserts correct line ending, End key positions before \r\n, backspace/delete treat \r\n as single unit (#401).

* **RPM Package**: Fixed /usr/bin/fresh entry missing from RPM package manifest.

* **Settings Percentage Values**: Fixed percentage settings saving as integers instead of floats.

* **Windows Unicode**: Fixed unicode character not supported on Windows (#400).

### Packaging

* **AUR Source Package**: Fixed sha256sum not being updated when publishing.

* **Nix Flake**: Fixed missing sublime-syntax grammar files in source filter.

* **Flatpak/AppImage**: Strip binaries before bundling for smaller package sizes.

### Internal

* **Test Reliability**: Fixed flaky e2e tests on macOS by removing timing sensitivity.

* **Release Workflow**: Added package upgrade tests and nix build test.

---

## 0.1.56

### Features

* **Per-Language Tab Settings**: Added `use_tabs` and `show_whitespace_tabs` config options per language. Go and Makefile default to tabs (#364).
* **AppImage Packaging**: AppImage bundles now included in GitHub releases (#365).
* **Terminal Color Detection**: Auto-detection of terminal color capabilities with fallback to 256 colors. Override via `FRESH_COLOR_MODE`.
* **TOML Syntax Highlighting**: Added embedded TextMate grammar for TOML files.
* **Language Detection by Filename**: Detect languages by filename (`.bashrc`, `Makefile`, `Dockerfile`, etc.) (#383).
* **Minimal Config Saves**: Config file only saves non-default fields.
* **Settings UI**: Mouse click/double-click support, hover effects, improved scrolling.

### Bug Fixes

* **LSP**: Improved error messages when server not found (#363). Fixed didOpen ordering (#399). Check diagnosticProvider capability before pull diagnostics (#399).
* **Terminal Mode Reset**: Fixed terminal_mode not being reset when closing a terminal buffer.
* **cargo-binstall**: Fixed missing binaries warning (#388).
* **macOS Keybinding Display**: Fixed showing ⌘ instead of Ctrl (#356).
* **tmux Truecolor**: Fixed detection when `COLORTERM=truecolor` is set.
* **RPM Upgrade**: Fixed upgrade failing when older version installed (#387).

## 0.1.54

### Features

* **Universal Install Script**: New `install.sh` script for easy installation across Linux and macOS.

* **Settings UI Enhancements**:
  - Entry dialogs for editing Languages, LSP servers, and keybindings
  - Schema-driven dialog builder with automatic field generation
  - Dimming effect for modal dialogs
  - Column-aligned controls for cleaner layout
  - Setting descriptions now displayed inline
  - Map controls with flat navigation, entry highlighting, and delete buttons

* **LSP Hover Improvements**: Hover popups now persist when moving within a symbol or hovering over the popup itself. Popups dismiss on focus loss.

* **Replace History**: Search & replace now supports history navigation for the replace field.

### Bug Fixes

* **Paste with Selection**: Fixed paste not replacing selected text - previously inserted without deleting selection.

* **Multi-Cursor Paste**: Fixed paste only working at primary cursor - now pastes at all cursor positions.

* **Bracketed Paste**: Enabled bracketed paste mode for proper handling of external paste (Ctrl+Shift+V). External pastes now arrive as single atomic events instead of character streams.

* **Settings Input Isolation**: Fixed keyboard input leaking between Settings UI panels.

* **Map Control Buttons**: Fixed [+] Add new buttons not working for Map controls.

* **File Browser Navigation**: Fixed input routing issues in file browser modal.

* **Config Loading**: Fixed config not loading from working directory; changes now apply to runtime state immediately.

### Configuration

* **rust-analyzer Defaults**: Added minimal performance defaults for rust-analyzer LSP.

### Internal

* **Input Handling Refactor**: New hierarchical `InputHandler` trait system for cleaner modal input routing.

* **Component Pattern**: Refactored all Settings UI controls (Button, Toggle, NumberInput, TextInput, Dropdown, TextList, MapInput, KeybindingList) to consistent component pattern.

* **Config Module**: Consolidated config path resolution and loading into `config_io` module. Config editor now saves only non-default values.

* **Code Organization**: Extracted action handlers into dedicated modules (menu_actions, lsp_actions, prompt_actions, undo_actions, mouse_input).

---

## 0.1.52

### Bug Fixes

* **musl Build**: Enabled the `runtime` feature for musl builds.
* **Flatpak**: Fixed CI and metainfo handling (official Flathub container + flatpak-builder action, appstream-compose deps, avoid corrupting XML declaration, remove invalid `launchable` tag).

### Internal

* **Version Bump Script**: Version bumps now skip `cargo check`.

---

## 0.1.45

### Features

* **Settings UI**: New graphical settings editor accessible via View menu or command palette. Features:
  - Two-panel layout with categories on left and settings on right
  - Fuzzy search to quickly find settings
  - Full keyboard navigation (Tab cycles through panels, arrow keys navigate items)
  - Mouse support with scrolling, scrollbar dragging, and hover indicators
  - Dropdown, number input, text list, and map editing controls
  - Reset to default functionality for individual settings
  - Confirmation dialog when discarding unsaved changes
  - Help overlay showing keyboard shortcuts

* **Default/Reset Color Support**: Theme colors can now use "Default" or "Reset" values for terminal transparency. The theme editor plugin shows these special colors with a "∅" placeholder swatch. Terminal background and foreground can inherit from the user's terminal emulator settings.

* **Flatpak Packaging**: Added Flatpak support for Linux installation (#340). Flatpak bundles are now included in releases.

### Bug Fixes

* **File Permissions Loss on Save**: Fixed file permissions/mode bits being lost when saving files (#329). Executable scripts and other special permissions are now preserved.

* **Polling File Watcher**: Replaced inotify/FSEvents-based file watching with a simple polling approach (#321). This fixes "too many open files" errors on large projects. Configurable via `auto_revert_poll_interval_ms` (default 2s) and `file_tree_poll_interval_ms` (default 3s).

* **Terminal Input Capture**: Fixed terminal capturing keyboard input when the Settings UI is opened while a terminal split is focused.

* **Search Result Scrolling**: Fixed settings UI not scrolling to show selected search results.

### Configuration

* **Memory Limit**: Changed `max_memory_mb` to `max_memory_percent` (default 50%) for consistent behavior across machines with different RAM.

### Packaging

* **AUR**: Updated package names to match conventions (fresh-editor vs fresh-editor-bin). Added `--syncdeps` to makepkg commands (#343).

### Internal

* **TimeSource Abstraction**: Added TimeSource trait for testability, making time-dependent behavior deterministic in tests (issue #314).

* **Test Reliability**: Replaced thread::sleep with testable time source in e2e tests. Fixed flaky tests on macOS and Windows.

* **Dependency Updates**: Updated deno_core, deno_error, actions/upload-artifact, actions/download-artifact, and actions/setup-node.

---

## 0.1.44

### Features

* **Double-Click Word Selection**: Double-click now selects the word under the cursor. Both clicks must be at the same position within the configurable time threshold (`double_click_time_ms`, default 500ms).

* **Multi-Byte Character Support**: Full support for CJK characters, emoji, and other double-width Unicode characters. Includes correct visual width calculation, cursor positioning, mouse click handling, line wrapping, and display across all UI components (status bar, tabs, file explorer, suggestions). (reported by @pm100)

* **Nix Flakes Support**: Added Nix flakes for reproducible builds and development. Includes crane-based Rust caching, dev shell with toolchain and dependencies, checks for clippy/tests/formatting, and direnv integration.

### Bug Fixes

* **Mouse Escape Codes After Panic**: Fixed mouse control codes littering the terminal after a crash by disabling mouse capture in the panic handler (#311, reported by @rluvaton).

* **Hover Popup Screen Edge**: Fixed panic when LSP hover popup appears near the edge of the screen.

* **File Explorer Click Focus**: Fixed typing not working after clicking on empty area in the file explorer and then clicking back on the editor.

### Infrastructure

* **npm Publish Workflow**: Consolidated npm publishing into a single workflow that works both standalone and when called from release.yml.

### Credits

Thanks to @blissartt, @dvchd, @jakoss, @pm100, @rluvaton, @sottey, and @Yousa-Mirage for bug reports, suggestions, and contributions.

---

## 0.1.40

### Features

* **Switch Project Command**: New "Switch Project" command (renamed from "Open Folder") to change project root with full context switch. Sessions are automatically saved and restored when switching projects, preserving open files, cursor positions, and split layouts.

* **Nested Submenu Support**: Menus now support nested submenus with proper arrow indicators and positioning.

* **Select Keybinding Map Command**: New popup selector to choose between different keybinding schemes.

* **Double-Click in File Dialog**: Can now double-click to open files in the file open dialog.

* **File Explorer UX Improvements**:
  - Ctrl+E now focuses the file explorer instead of toggling it
  - File explorer automatically focuses when closing the last tab
  - Menu checkboxes properly sync with file explorer visibility state

* **Split Auto-Close**: Closing the last tab in a split now automatically closes the split.

### Bug Fixes

* **Mouse Click Below Last Line**: Fixed mouse click below the last line incorrectly jumping to position 0,0.

* **Menu Checkbox Sync**: Fixed View menu checkboxes not syncing with file explorer visibility state.

* **Duplicate Buffer on Project Switch**: Fixed duplicate buffer creation when switching projects.

* **Wrong Upgrade Tip**: Fixed incorrect upgrade tip message (#293).

### Infrastructure

* **Build System Overhaul**: Replaced cargo-dist with direct cargo builds and custom packaging for more control over the release process.

* **npm OIDC Publishing**: Improved npm publish workflow with OIDC trusted publishing and provenance attestations.

* **GitHub Actions Updates**: Bumped actions/checkout to v6, actions/upload-artifact to v5, actions/download-artifact to v6, and actions/setup-node to v6.

* **Test Improvements**: Many test reliability improvements including Windows compatibility fixes, flaky test fixes, and better test isolation for session persistence tests.

---

## 0.1.35

### Features

* **XDG Config Paths**: Support standard XDG config paths for user configuration. On macOS, `~/.config/fresh/config.json` is now prioritized if it exists, in addition to the system default path. (@Yousa-Mirage)

### Packaging

* **cargo-binstall**: Added cargo-binstall as an installation method in documentation. (@dvchd)

* **npm OIDC Publishing**: Switched npm publish to OIDC trusted publishing with provenance attestations.

---

## 0.1.28

### Features

* **Integrated Terminal**: Full terminal emulation using alacritty_terminal. Open a terminal split with "Open Terminal" command, run shell commands, and interact with TUI applications. Supports:
  - Keyboard capture mode (F9) for sending all keys to terminal
  - Scrollback history with file-backed storage
  - Session persistence - terminals restore across editor restarts
  - Paste support (Ctrl+V)
  - Click to focus terminal splits
  - Auto-restore terminal mode when switching back to terminal tabs
  - Dimmed UI indication when keyboard capture is active

* **Mouse Hover for LSP**: Hover over symbols to see LSP hover information (type info, documentation). Configurable delay before showing hover popup.

* **Toggle Maximize Split**: New command to maximize/restore the current split view.

* **Close Tab Command**: New command to close a tab without closing the underlying buffer.

* **C# Language Support**: Added C# language configuration with LSP support (csharp-ls or csharp-language-server) and auto-indent. Includes proactive `dotnet restore` on C# file open.

* **Config Editor Improvements**: New `getConfig`/`getUserConfig` plugin APIs. Config editor now properly merges user config with defaults for LSP and languages sections. Timestamped backups created before saving config.

* **LSP Menu**: New LSP menu in menu bar with common LSP actions. Menu items are disabled when LSP server is not ready.

* **Common LSP Keybindings**: Added default keybindings for common LSP operations.

* **C/C++ Language Support**: Added C and C++ language configurations to defaults.

### Bug Fixes

* **LSP Focus Stealing**: Fixed LSP error and warning buffers stealing focus from the active buffer.

* **Terminal Scrollback**: Fixed multiple issues with terminal scrollback not being captured, restored, or displayed correctly after session restore and mode toggles.

* **Terminal View Following**: Fixed terminal view not following output when at the bottom of the screen.

* **Config Editor**: Fixed config editor saving null instead of user changes. Fixed undefined defaultValue reference.

* **Duplicate LSP didOpen**: Fixed duplicate didOpen notifications being sent to strict LSP servers.

* **LSP didChange Race**: Fixed LSP didChange notification being sent before didOpen.

### Internal

* **Musl Builds**: Added musl builds without plugins for fully static Linux binaries.

* **Plugin Build Flag**: Added cargo feature (`no-plugins`) to disable plugins at the dependency level, reducing binary size and startup time.

* **Test Organization**: Moved plugin-related and LSP find_references tests to dedicated plugins directory.

* **Test Reliability**: Fixed flaky e2e tests, skipped platform-specific tests on Windows/macOS where appropriate.

* **Terminal Architecture**: Implemented incremental streaming architecture for terminal scrollback with PTY logging and file-backed buffers.

---

## 0.1.27

### Features

* **Update Checker**: Automatically checks for new versions periodically (every 24 hours) and on quit, showing a notification when updates are available.

* **Diagnostics Panel**: New diagnostics panel plugin showing all errors/warnings in a dedicated split view. Opens in horizontal split, auto-updates on buffer change, and syncs cursor position with F8/Shift+F8 navigation. Includes help line with keybinding hints.

* **Diagnostics API**: New plugin API for accessing LSP diagnostics programmatically.

* **LSP Initialization Options**: Added support for `initialization_options` in LSP server configuration.

* **Warning Log Layer**: Captures WARN+ level logs to a file and can open them in the editor for debugging.

* **Plugin Hook**: Added `cursor_moved` hook for plugins to respond to cursor position changes. Standardized hook naming to use underscores.

### Bug Fixes

* **UTF-8 Status Bar**: Fixed panic when truncating status bar text mid-character.

* **Session Restore**: Fixed session restore when a plugin buffer was the active buffer.

* **Viewport Sync**: Fixed viewport sync issues after SplitViewState refactoring.

* **LSP Null Response**: Treat null LSP response as valid result instead of error.

* **LSP Auto-Start**: Persist LSP auto-start setting when manually stopping the server.

* **Safe String Slicing**: Use safe string slicing in get_text_to_end_of_line to prevent panics.

### Internal

* **SplitViewState Refactoring**: Made SplitViewState authoritative for viewport state.

* **Default Log Path**: Use system temp directory for default log file path.

* **Test Reliability**: Fixed flaky tests on macOS and Windows, improved diagnostics panel tests.

* **Dependency Updates**: Updated deno_core, schemars, libloading, and GitHub Actions dependencies.

* **Documentation**: Added macOS plugin location information, documented reloadConfig plugin API.

---

## 0.1.26

### Bug Fixes

* **aarch64 Build**: Fixed build on aarch64 Linux by enabling v8_use_custom_libcxx.

---

## 0.1.25

### Features

* **GPM Mouse Support**: Added mouse support in Linux virtual consoles (TTY) via the GPM daemon (#231). Uses dlopen to load libgpm.so at runtime, so the binary works on systems without GPM installed. Gracefully falls back to standard terminal mouse protocol when GPM is unavailable.

* **Configurable Highlight Context**: Syntax highlighting lookback/lookforward is now configurable via `highlight_context_bytes` in config (default increased from 1KB to 10KB). Fixes inaccurate highlighting when viewing the middle of files with long multi-line constructs.

### Bug Fixes

* **Mouse Wheel After Keyboard**: Fixed mouse wheel scroll not working in main editor after keyboard navigation (#248).

### Internal

* **Reduced Logging**: Reduced verbose debug logging in default config.

* **Signal Handling**: Removed ctrlc dependency, use nix sigaction directly.

* **Test Reliability**: Fixed flaky auto-revert tests on macOS (FSEvents latency) and filesystems with 1-second mtime granularity.

* **Dependency Updates**: Reduced and updated dependencies.

---

## 0.1.24

### Bug Fixes

* **Windows Build**: Fixed Windows build compatibility.

---

## 0.1.23

### Bug Fixes

* **Split Close Tab Preservation**: Fixed tabs not being preserved when closing a split.

### Performance

* **Diff Optimization**: Optimized diff_since_saved with two-phase algorithm.

---

## 0.1.22

### Features

* **CLI file:line:col**: Support `file:line:col` format on CLI command (#217).

* **LSP Error Logging**: LSP stderr is now piped to a file and opened as read-only buffer on error.

* **Config Languages**: Use config languages section for LSP language detection.

### Bug Fixes

* **TypeScript Highlighting**: Fixed TypeScript syntax highlighting by falling back to tree-sitter.

* **Plugin Race Condition**: Fixed race condition in plugin hooks reading stale state snapshot.

* **Long Path Truncation**: Truncate long paths in Open File prompt with styled [...].

* **Graceful Shutdown**: Prevent spurious LspError on graceful shutdown.

### Internal

* **Syntect for Highlighting**: Use syntect for syntax highlighting, retain tree-sitter for other features (#237).

---

## 0.1.21

### Packaging

* **AUR Package**: Added AUR package automation and installation instructions.

* **npm and crates.io**: Added automated npm and crates.io publishing.

---

## 0.1.20

### Features

* **Theme Editor Plugin**: New interactive theme editor for customizing colors. Allows editing all theme color values with a visual interface.

* **Drag-to-Select Mouse Support**: Click and drag to select text, similar to graphical editors.

* **Homebrew Distribution**: Preliminary setup for Homebrew distribution on macOS.

### Bug Fixes

* **File Open Dialog**: Fixed handling of pasted paths in the file open dialog. Previously pasting a full path would fail; now it correctly opens the file or navigates to the directory.

* **Mouse Click on Wrapped Lines**: Fixed mouse click positioning not working correctly on wrapped lines and empty lines.

### Packaging

* **Linux Packages**: `.deb` and `.rpm` packages are now available for Debian/Ubuntu and Fedora/RHEL distributions respectively.

* **Homepage**: Set official homepage to https://sinelaw.github.io/fresh/

---

## 0.1.19

### Packaging

* **Linux packages fix**: Fixed `.deb` and `.rpm` packages not being included in GitHub releases.

---

## 0.1.18

### Features

* **Auto-load user config**: Startup now loads the default config file (e.g. `~/.config/fresh/config.json`) so themes and preferences persist without needing `--config`.
* **Clearer confirmation prompts**: Destructive prompts now use action verbs (revert, overwrite, discard) instead of generic y/n, reducing misclicks.

### Bug Fixes

* **UTF-8 safe deletion**: Backspace/Delete operate on full Unicode characters (emojis, accented letters, currency symbols) instead of raw bytes.

### Packaging

* **Deb/RPM artifacts**: Release workflow now builds stripped `.deb` and `.rpm` packages for x86_64 and aarch64, with matrixed install/uninstall tests across Ubuntu 22.04/24.04, Debian 12, Fedora 39/40, and Rocky Linux 9.

---

## 0.1.15 - Unreleased

### Features

* **TextMate Grammar Support**: Syntax highlighting now uses TextMate grammars via syntect for languages without tree-sitter support. Includes proper highlighting for Markdown (headings, bold, italic, code, links, quotes, lists).

* **Fuzzy Matching**: Command palette and file browser now use fzf-style fuzzy matching. Matches are highlighted and scored by consecutive characters, word boundaries, and match position.

* **Tab Navigation Commands**: New commands "Go to Next Tab" and "Go to Previous Tab" in the command palette for keyboard-driven tab switching.

* **File Recovery**: Emacs-style auto-recovery for unsaved changes. Buffers are automatically saved every 2 seconds to `~/.local/share/fresh/recovery/`. On startup, automatically recovers unsaved changes from crashed sessions. Uses chunked storage for large files to avoid memory issues.

* **Explorer Menu**: New menu bar entry with file explorer actions (New File, New Folder, Rename, Delete) and keybindings. Disabled items shown in theme colors when not applicable.

* **File Explorer Rename**: Press F2 or use Explorer menu to rename files/folders. Project root is protected from renaming.

* **Emacs-Style Readline Bindings**: Added terminal key equivalents for common operations:
  - Ctrl+A: Home (beginning of line)
  - Ctrl+E: End (end of line)
  - Ctrl+K: Kill to end of line
  - Ctrl+U: Kill to beginning of line
  - Ctrl+W: Kill word backward
  - Alt+D: Kill word forward
  - Ctrl+Y: Yank (paste from kill ring)

### Bug Fixes

* **Multi-Cursor Selection**: Fixed Ctrl+D selection replacement not working correctly (issue #210).

* **LSP Auto-Restart**: Fixed stopped LSP server incorrectly auto-restarting on edit.

* **File Explorer Selection**: Fixed selection being lost after rename completes.

* **Markdown Highlighting**: Fixed markdown files not getting syntax highlighting for headers, bold, italic, links, etc.

### Performance

* **Recovery Write Performance**: Removed sync_all from recovery writes, reducing disk I/O overhead.

* **Large File Recovery**: Chunked recovery format applies edits directly without loading entire file into memory.

---

## 0.1.14

See git history for changes.

---

## 0.1.13

### Features

* **Git Gutter Plugin**: Shows git diff indicators in the gutter for lines changed vs HEAD:
  - │ (green): Added line
  - │ (yellow): Modified line
  - ▾ (red): Deleted line(s) below

* **Buffer Modified Plugin**: Shows unsaved changes with │ (blue) indicators for lines modified since last save.

* **Line Indicator System**: New plugin API for gutter indicators with automatic position tracking. Indicators use byte-position markers that shift automatically when text is inserted/deleted. Priority system allows multiple indicator types to coexist (diagnostics > git > buffer modified).

* **LCS-Based Line Diff**: Buffer modified indicators now use the classic LCS (Longest Common Subsequence) algorithm - the foundation of Unix diff - for accurate change detection. Correctly handles insertions without marking shifted lines as changed, and detects deletion points.

* **Content-Based Diff**: Diff comparison now uses actual byte content rather than piece tree structure. This means if you delete text and paste it back, the indicator correctly clears because the content matches the saved state.

### Bug Fixes

* **Save As Undo History**: Fixed undo history being cleared after Save As due to auto-revert triggered by file watcher detecting the newly created file. Uses optimistic concurrency with mtime comparison to avoid spurious reverts.

* **Save As Dirty State**: Fixed undo dirty state not being tracked correctly after Save As on unnamed buffers (issue #191).

### Performance

* **Large File Mode**: Diffing is now disabled in large file mode for performance. Uses the simpler is_modified() flag instead of expensive diff calculations for files with >10MB or unknown line counts.

---

## 0.1.12

### Features

* **Live Grep Plugin**: Project-wide search with ripgrep integration and live preview. Search results update as you type (minimum 2 characters), with a split pane showing file context and syntax highlighting. Press Enter to open file at location, ESC to close preview.

* **Calculator Plugin**: Scientific calculator with clickable buttons and keyboard input. Supports parentheses, exponents (^), sqrt, ln, log, trig functions, pi, and e. Mouse click/hover support, copy button for results, and ANSI-colored UI with Unicode box drawing. ESC to close, DEL to clear.

* **File Explorer Improvements**:
  - Shows file sizes (KB/MB/GB) and directory entry counts
  - Close button (×) in title bar to hide explorer
  - Left arrow on file/collapsed directory selects parent
  - Keybinding changed from Ctrl+B to Ctrl+E (avoids tmux conflict)

* **Split View Close Buttons**: Split views now show a × button on the right side of the tab row (only when multiple splits exist) for easy closing.

* **Close Last Buffer**: Closing the last buffer now creates a fresh anonymous buffer instead of blocking with "Cannot close last buffer".

* **Alt+W Keybinding**: New shortcut to close the current tab.

* **Command Palette Source Column**: Shows where each command comes from - "builtin" or the plugin filename - in a right-aligned column.

* **Relative Buffer Names**: Buffer display names are now shown relative to the working directory.

### Bug Fixes

* **File Explorer Toggle**: Fixed Ctrl+B/Ctrl+E toggle not working correctly - now properly opens/closes instead of just focusing.

* **Session Restore**: Fixed file explorer not initializing when restoring a session with explorer visible.

* **Open File Popup**: Hide status bar when file browser popup is shown; improved high-contrast theme colors (cyan instead of yellow).

---

## 0.1.11

See git history for changes.

---

## 0.1.10

### Features

* **Session Persistence**: Automatically saves per-project state (open files, tabs, split layout, cursor/scroll positions, file explorer state, search/replace history and options, bookmarks) to the XDG data dir and restores it on launch. Session restore is skipped when opening a specific file; use `--no-session` to start fresh.

* **Unified Search & Replace**: Replace (Ctrl+H) and Query Replace (Ctrl+Shift+H) now share the same interface with a "Confirm each" toggle (Alt+E). Query Replace enables confirmation by default; Replace uses the toggle state. Confirmation prompt shows `(y)es (n)o (a)ll (c)ancel` options.

### Bug Fixes

* **Session Restore Reliability**: Fixed session rehydration to reopen files/splits with the correct active buffer, cursor, and scroll position (including nested splits) instead of jumping back to the top on first render.

* **macOS Build**: Fixed Linux-specific `.init_array` by using cross-platform V8 initialization.

* **Syntax Highlighting**: Fixed invisible/hard-to-read highlighting in light and nostalgia themes by using theme-based color resolution instead of hardcoded colors.

* **Theme Colors**: Improved status bar and prompt colors across all themes (dark, high-contrast, light, nostalgia).

* **Search Prompt**: Search/replace prompts now cancel when focus leaves the editor (switching buffers or focusing file explorer).

---

## 0.1.9

### Features

* **Native File Browser**: New built-in file browser for Open File command (Ctrl+O) that works without plugins. Features sortable columns (name, size, modified), navigation shortcuts (parent, home, root), filtering with grayed non-matches, mouse support with hover indicators, and async directory loading.

* **CRLF Line Ending Support**: Transparent handling of Windows-style line endings. Files are detected and normalized internally, then saved with their original line ending format preserved.

* **CLI Enhancements**: Added `--version`, `--no-plugins` (skip JS runtime for faster startup), `--log-file`, and `--config` flags.

* **UI Improvements**:
  - Tab hover effects with close button changing to red on hover
  - Menu hover-to-switch when a menu is open
  - Buffer name shown in modified buffer confirmation prompts
  - Fixed column widths in command palette for stable layout

### Bug Fixes

* **V8 Segfault**: Fixed crash when creating multiple Editor instances (e.g., in tests) by initializing V8 platform once at library load.

* **Windows**: Fixed duplicate key presses caused by processing both Press and Release events.

---

## 0.1.8

### Bug Fixes

* **Open File Prompt**: Fixed completions not showing immediately (issue #193) by enabling ICU support for Unicode functions.

* **Keyboard Shortcuts Help**: Fixed crash when reopening keyboard shortcuts buffer (issue #192).

* **Undo Save Points**: Fixed extra undo step at beginning of save history (issue #191).

* **Scroll Keybindings**: Fixed Ctrl+Up/Down scroll not working by syncing viewport between SplitViewState and EditorState.

---

## 0.1.7

### Features

* **Select Theme Command**: New theme picker accessible from the command palette and View menu. Includes a new "nostalgia" theme inspired by Turbo Pascal 5 / WordPerfect 5.

* **Compose Mode Improvements**: Paper-on-desk visual effect with desk margin colors, and hanging indent support for markdown lists and blockquotes.

* **Binary File Detection**: Binary files are now detected and opened in read-only mode to prevent accidental corruption.

### Bug Fixes

* **Light Theme**: Fixed colors for status bar, prompt, scrollbar, tabs, and file explorer to use proper light theme colors.

* **Mouse Performance**: Fixed slow mouse movement on large terminals by skipping redundant renders when hover target hasn't changed. Added mouse event coalescing to skip stale positions.

* **UTF-8 Truncation**: Fixed panic when truncating suggestion descriptions mid-character.

### Internal Changes

* **Code Refactoring**: Major cleanup extracting helpers and reducing duplication across many modules including `process_async_messages`, `handle_plugin_command`, `render_view_lines`, `multi_cursor`, `highlight_color`, and more. Consolidated duplicate `hook_args_to_json` implementations.

* **Test Improvements**: Fixed flaky tests by removing timing assertions, made shortcut tests platform-aware for macOS.

* **Documentation**: Reorganized internal planning docs, updated plugin README from Lua to TypeScript, and added embedded help manual using `include_str!()`.
