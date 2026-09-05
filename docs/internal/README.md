# Fresh — Internal Architecture Documentation

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

This directory documents how Fresh works and why: the architecture of each
subsystem, the decisions and trade-offs behind it, the algorithms chosen (and
the ones rejected), and the UX alternatives that were considered. It is written
for contributors who need to understand or change the code, not for end users
(see [user docs](#user-facing-documentation) for that).

## Conventions

- **AI-generated.** These docs are derived from the source and the commit
  history. The source is authoritative; where a doc and the code disagree, the
  code wins.
- **No volatile detail.** Line numbers, exact constants, and source locations
  are omitted on purpose. Subsystems, types, and patterns are named
  conceptually rather than pinned to a file or line, because those drift.
- **IMPLEMENTED vs PLANNED.** Each doc labels what ships today versus what is
  forward-looking design. Several subsystems (k8s storage, orchestrator
  multi-session, parts of the universal search and diff-parity work) are partly
  planned; the docs say so explicitly.
- **Consolidated history.** This set replaces roughly 130 earlier design notes,
  plans, and evaluations that had accumulated here. Their rationale has been
  distilled into the docs below. The originals remain in git history for the
  full record.

Start with [`00-overview.md`](00-overview.md) for the runtime model and a map of
everything else, and keep [`glossary.md`](glossary.md) open for the naming
conventions (daemon / workspace / backend / `Authority`) and core vocabulary the
other docs assume.

## The documents

### Foundations
| Doc | What it covers |
|-----|----------------|
| [00-overview.md](00-overview.md) | The keystone map: the crate workspace and why it's split, cargo feature gating, the entrypoint and event loop, the threading model, the client/server and daemon architecture, async message flow, the `Editor` central object, and the Action-vs-Event split. |
| [glossary.md](glossary.md) | Naming conventions (the retired "session" → daemon/workspace/backend scheme) plus a core architecture vocabulary table. |

### Text & editing core
| Doc | What it covers |
|-----|----------------|
| [text-model.md](text-model.md) | The persistent path-copying piece tree (and why not a rope or gap buffer), lazy loading for multi-GB files, interval-tree markers with gravity, the `Event`/`BulkEdit` model with O(1) `Arc`-snapshot undo, composite buffers, and the encoding/save path. |
| [buffers-splits-undo.md](buffers-splits-undo.md) | App-layer buffer lifecycle and identity, buffer groups, the split/window tree, per-buffer vs per-view state, undo/redo with marker displacement, and hot-exit / crash recovery. |
| [input-keybindings-actions.md](input-keybindings-actions.md) | A keystroke end-to-end: terminal key normalization, the modal dispatch priority stack, the command→action→event pipeline and why it's separated, the unified keybinding resolver, multi-cursor, and mouse hit-testing. |
| [terminal-input-parsing.md](terminal-input-parsing.md) | The stage before that one: raw terminal bytes → events. Why Fresh parses input itself rather than using crossterm's parser, the DEC/ANSI state machine and the "control-sequence bytes are never emitted as text" invariant it protects, standalone-Escape resolution, the three input paths — and a register of the xterm/kitty protocol gaps that remain. |

### Rendering & language intelligence
| Doc | What it covers |
|-----|----------------|
| [rendering-and-layout.md](rendering-and-layout.md) | The per-frame render loop, the token→`ViewLine` pipeline, the line-wrap and visual-row caches that make huge files scroll cheaply, folding/conceal/virtual-text, split-pane layout, and the `Scene` projection shared with the web frontend. |
| [embedded-language-highlighting.md](embedded-language-highlighting.md) | Mixed/embedded-language files (Markdown fences, HTML+CSS, templating): the composite-snapshot region mechanism in the TextMate engine, and the rule of thumb for grammar-level embedding vs engine-level regions vs `highlight_string`. |
| [syntax-highlighting.md](syntax-highlighting.md) | The engine-selection rule (syntect TextMate grammars by default, tree-sitter for the gaps, and why), the checkpoint/convergence incremental-highlight algorithm, viewport-only scaling, category→theme mapping, and reference/bracket overlays. |
| [lsp.md](lsp.md) | The multi-server LSP client: `(language, feature)` routing, the gate-and-retry concurrency model, async result flow, diagnostics-as-markers, completion-source merging, and feature concessions. |
| [web-ui.md](web-ui.md) | The non-terminal (web) frontend: the unified-scene architecture and what ships today, plus the design gaps and implementation gaps between the current prototype and desktop-grade (VS Code-level) polish. |

### Extensibility & environment
| Doc | What it covers |
|-----|----------------|
| [plugins.md](plugins.md) | Sandboxed TypeScript plugins on a QuickJS thread, the `PluginCommand` protocol and one-frame lag, the provider pattern, the declarative widget runtime, parallel package loading, the git-based marketplace, and the sandbox/security trade-offs. |
| [remote-authority-trust.md](remote-authority-trust.md) | The `Authority` backend slot (local / SSH / docker-exec / kubectl-exec), the remote agent and filesystem, heartbeat/reconnect, devcontainers, the k8s transport, Workspace Trust, and the live env provider — with a clear shipped-vs-planned line. |
| [orchestrator-sessions.md](orchestrator-sessions.md) | The Orchestrator/Dock for many concurrent workspaces/agent sessions, session persistence and the Live/Dormant lifecycle, and the dock UX design versus what ships today. |
| [terminal.md](terminal.md) | The integrated terminal: PTY spawning, the embedded `fresh-winterm` VT emulator (and why custom), live/scrollback per-buffer state, mouse/links/OSC52, and restore-on-reconnect. |

### Configuration, features & quality
| Doc | What it covers |
|-----|----------------|
| [config-themes-settings.md](config-themes-settings.md) | The layered config overlay and resolution, JSONC with comment-preserving writes, schemars schema generation driving the Settings UI, the theme system and live preview, and the keybinding editor. |
| [search-and-diff.md](search-and-diff.md) | In-buffer search/replace, project-wide search and live grep, the diff/review (hunk) viewer and its `Arc::ptr_eq` piece-tree diff, git-log viewing, and the keyboard-macro system. |
| [editor-ux-features.md](editor-ux-features.md) | Smaller shipped features without their own doc: markdown compose/preview, code tour, the input calibration wizard, vi mode, i18n, the menu/command-palette/help/bookmarks cluster, and warning/notification UX. |
| [testing.md](testing.md) | The testing layers, the headless scenario framework (tests as data, replayed against `EditorTestApi`), the ANSI capture backend, the `TimeSource` determinism abstraction, and CI structure. |

### Plans (forward-looking)
| Doc | What it covers |
|-----|----------------|
| [widget-framework-v2-review.md](widget-framework-v2-review.md) | Critical review of the widget framework and the UI chrome around it, benchmarked against Dear ImGui, the CSS box model, Tailwind, htmx and TanStack. Argues that the recurring focus / event-propagation / wheel-targeting / text-input bugs share one root cause — the widget tree renders to a flat row list with no vertical axis, z-order or clipping — names five concrete places the codebase works around that, and proposes a phased v2 (constraint layout, one hit-test tree, one focus ring, one text engine, popups as stacking contexts). Analysis + planned work, not the system as built. |
| [chrome-event-model-plan.md](chrome-event-model-plan.md) | The app-level half of that arc: dissolving the central chrome enumeration into registered `ChromeComponent`s, tree dispatch over a per-event box arena, the keyboard registration slices, and the rulings behind the pre-band stages that stay outside the walk. |
| [widget-library-design.md](widget-library-design.md) | Where those two arcs point: spec + API for ONE widget library serving both plugin widgets and all editor chrome, built as a **retained reconciling tree** — immutable descriptions, persistent elements matched by `(type, key)` at a position, local state on the element, and expensive render objects holding geometry and focus registration. Covers dirty-marking with a depth-ordered flush, box-constraint layout with relayout boundaries, hit-test-derived pointer propagation, a parallel focus tree with pluggable traversal and Shortcuts→Intents→Actions, out-of-flow `Layer`s for floating panels, and a `LayoutSpec` display list consumed by TUI/web/test backends. Worked examples for the menu bar, split grid, context menus, command palette, transient popups and modals. Entirely planned. |
| [widget-library-implementation-plan.md](widget-library-implementation-plan.md) | How to build that library and adopt it, and the record of how far that has got. Part 1 stands up a `fresh-ui` crate from scratch (reconciler against a fake renderer, scheduler, layout, paint, hit-testing, focus, widget set), each phase with its exit criteria; Part 2 migrates every surface in nine waves — status bar, context menus, menus, popups, prompt, plugin panels, modals, settings, frame layout — swapping and deleting one at a time, with cell-identical output as the acceptance test and a per-wave deletion ledger. All of Part 1 is built — reconciler through widget set, plus a demo application with golden and property tests — and the doc carries a **deviation register**: a section-by-section audit of the design against the source, naming nine places the implemented model differs, ten specified pieces not yet built, and fourteen places the design itself needs correcting. Part 1b is the plan that closes them, and Part 2 does not start until it has. |
| [fresh-editor-ui-migration.md](fresh-editor-ui-migration.md) | The editor-side companion to the two library docs: a grounded survey of the editor's current UI as built (the `ChromeComponent` registry, the two decoupled precedence systems, the `Scene` projection, paint-recorded vs live geometry, the keyboard/pointer walks, `PointerGrab`, `capture_mouse`, the plugin `WidgetSpec` runtime, and the half-unified Settings controls), the crisp keep/migrate boundary at `SplitRenderer::render_content` (buffers stay behind a `Host` leaf), a target design mapping every surface onto `fresh-ui` primitives, and the M0–M9 migration refined with file-level moves. Survey is as-built; design and plan are forward-looking. |
| [welcome-screen-design.md](welcome-screen-design.md) | Design for an interactive **welcome buffer** replacing the `[No Name]` scratch buffer and the blank-pane hint as the default empty-workspace surface. Structures onboarding as a **ladder** — a scrollable virtual buffer whose first viewport is a zero-anxiety chooser and whose three bannered levels descend from everyday editing through IDE features to agent orchestration, with jump keys, a status-bar depth meter and foldable cards. Every feature is a live demo rather than a bullet, built on shipping primitives (`mountWidgetPanel` into a virtual buffer, `windowEmbed`, markdown `Text` with grammars). Covers the copy rules, the `editor.empty_workspace_screen` setting and its migration, the get-out-of-the-way lifecycle rule, the responsive floor, ASCII wireframes of all six states, and a phased build order. Entirely planned. |
| [settings-widget-unification-plan.md](settings-widget-unification-plan.md) | Phased plan to collapse the two separate UI-control systems — the Settings `view/controls/` library and the plugin `WidgetSpec` runtime — into the single declarative widget framework already exposed to plugins. Unlike the docs above, this describes intended work, not the system as built. |
| [finder-preview-highlighting-design.md](finder-preview-highlighting-design.md) | The answer to "why is the search preview plain?" (sinelaw/fresh#3196), the follow-up to the diff highlighting of sinelaw/fresh#3104: stop composing a snippet buffer and preview the real file as an ephemeral **preview tab**, through the File Explorer's own preview code rather than a second copy of it — colours, gutter, wrap, folds and LSP decoration then come for free. Splits `open_file_preview` into the explorer's policy header (its `preview_tabs` key, which the finders do not read, and its choice of split) and the shared preview discipline (replace, promote, deferred hook, suppressed history); targets a named split without a second open path, and without the focus handler that would commit every browse; drops the preview on cancel with no snapshot, because the preview tab is itself the record of what the browse added. Weighs the costs it does buy — LSP churn and the debounce it forces, and a browse that must skip rather than raise the large-file encoding dialog — and keeps `setSyntaxRegions` for the one surface that cannot be a file: the result list, whose rows are single lines from many files. Built, except the preview-session consolidation it sketches. |
| [sidebar-sections-design.md](sidebar-sections-design.md) | Turning the file-explorer sidebar from a single-occupancy column into a column of stacked, collapsible sections sharing one border row per boundary (sinelaw/fresh#3045), each hosting either the file tree or a plugin panel's `Interior` — the same value the dock and floating panels mount. Audits the four panel placements and the one grip mechanism the retained-mode merge left, specifies the section model in the `fresh-ui` shell's own vocabulary (`Frame.sidebar`, `Grip::SectionDivider`, `Slot::Sidebar`), and works the design through on a first consumer: an auto-syncing, clickable, foldable Markdown table-of-contents section that works in source and compose mode alike. Entirely planned. |
| [agent-fresh-cli-exposure-plan.md](agent-fresh-cli-exposure-plan.md) | How the Orchestrator launcher could teach a launched coding agent to drive the surrounding editor by shelling out to `fresh` (open a file, split, spawn a workspace) — the `FRESH_SESSION` seam, per-agent system-prompt injection mechanisms, the control-socket gap, and a phased plan. The launcher half (agent presets, resume, Start prompt, Auto mode) ships; the CLI-exposure half is planned. |
| [packaging-self-update.md](packaging-self-update.md) | A packaging paradigm built on deterministic **install provenance**: every distribution channel (brew, apt, dnf, AUR, winget, scoop, flatpak, npm, cargo, nix, AppImage, raw tarball, …) records at install time exactly how `fresh` was installed — via a packaged/sidecar `install-receipt.toml`, a compile-time channel, or a demoted path heuristic — so `fresh update` can self-update through the *same* mechanism (delegate to the manager, or verified in-place binary swap) across Linux/Windows/macOS. Replaces the current path-guessing update checker. Planned. |

## A note on history

This set replaces roughly 130 older design notes, plans, and evaluations that
had accumulated here. Most documented intent (often aspirational, or already
shipped and drifted) rather than the system as built. The rationale from them
has been consolidated into the docs above. The originals remain in git history
for the full record — for example:

```
git log --all --full-history -- 'docs/internal/orchestrator-sessions-design.md'
git show <commit>:docs/internal/<old-doc>.md
```

## User-facing documentation

See the parent [docs/](../) directory:
- [Architecture](../architecture.md) — user-facing system architecture overview
- [Getting Started](../getting-started/), [Features](../features/)
- [Plugins](../plugins/) and [Plugin Development](../plugins/development/)
