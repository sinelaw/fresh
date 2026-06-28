# Fresh — Internal Architecture Documentation

This directory documents **how Fresh actually works and why** — the
architecture of each subsystem, the decisions and trade-offs behind it, the
algorithms chosen (and the ones rejected), and the UX alternatives that were
considered. It is written for contributors who need to understand or change the
code, not for end users (see [user docs](#user-facing-documentation) for that).

## How to read these docs

- **Code is authoritative.** Every doc was written by reading the current
  implementation and the 7000-commit history, then verifying the older design
  notes against it. Where a claim and the code disagree, the code wins and the
  discrepancy is flagged inline.
- **`path:line` references** point into `crates/fresh-editor/src/` (or the named
  crate). They were accurate at the time of writing; treat them as signposts,
  not guarantees — line numbers drift.
- **IMPLEMENTED vs PLANNED.** Each doc labels what ships today versus what is
  forward-looking design. Several subsystems (k8s storage, orchestrator
  multi-session, parts of the universal search and diff-parity work) are
  partly aspirational; the docs say so explicitly.
- **Start with [`00-overview.md`](00-overview.md)** for the runtime model and a
  map of everything else, and keep [`glossary.md`](glossary.md) open for the
  naming conventions (daemon / workspace / backend / `Authority`) and core
  vocabulary the other docs assume.

## The documents

### Foundations
| Doc | What it covers |
|-----|----------------|
| [00-overview.md](00-overview.md) | The keystone map: the 8-crate workspace and why it's split, cargo feature gating, the `main` entrypoint and ~60fps event loop, the threading model, the client/server + daemon architecture, async message flow, the `Editor` god object, and the Action-vs-Event split. |
| [glossary.md](glossary.md) | Naming conventions (the retired "session" → daemon/workspace/backend scheme) plus a core architecture vocabulary table. |

### Text & editing core
| Doc | What it covers |
|-----|----------------|
| [text-model.md](text-model.md) | The persistent path-copying **piece tree** (and why not a rope/gap buffer), lazy loading for multi-GB files, interval-tree **markers** with gravity, the `Event`/`BulkEdit` model with O(1) `Arc`-snapshot undo, composite buffers, and the encoding/save path. |
| [buffers-splits-undo.md](buffers-splits-undo.md) | App-layer buffer lifecycle and identity, buffer groups, the split/window tree, per-buffer vs per-view state, undo/redo with marker displacement, and hot-exit / crash recovery. |
| [input-keybindings-actions.md](input-keybindings-actions.md) | A keystroke end-to-end: terminal key normalization, the modal dispatch priority stack, the command→action→event pipeline and why it's separated, the unified keybinding resolver, multi-cursor, and mouse hit-testing. |

### Rendering & language intelligence
| Doc | What it covers |
|-----|----------------|
| [rendering-and-layout.md](rendering-and-layout.md) | The per-frame render loop, the token→`ViewLine` pipeline, the line-wrap and visual-row caches that make huge files scroll cheaply, folding/conceal/virtual-text, split-pane layout, and the `Scene` projection shared with the web frontend. |
| [syntax-highlighting.md](syntax-highlighting.md) | The engine-selection rule (syntect TextMate grammars by default, tree-sitter for the gaps, and why), the checkpoint/convergence incremental-highlight algorithm, viewport-only scaling, category→theme mapping, and reference/bracket overlays. |
| [lsp.md](lsp.md) | The multi-server LSP client: `(language, feature)` routing, the gate-and-retry concurrency model, async result flow, diagnostics-as-markers, completion-source merging, and feature concessions. |

### Extensibility & environment
| Doc | What it covers |
|-----|----------------|
| [plugins.md](plugins.md) | Sandboxed TypeScript plugins on a QuickJS thread, the `PluginCommand` protocol and one-frame lag, the provider pattern, the declarative widget runtime, parallel package loading, the git-based marketplace, and the sandbox/security trade-offs. |
| [remote-authority-trust.md](remote-authority-trust.md) | The `Authority` backend slot (local / SSH / docker-exec / kubectl-exec), the remote agent + filesystem, heartbeat/reconnect, devcontainers, the k8s transport, Workspace Trust, and the live env provider — with a clear shipped-vs-planned line. |
| [orchestrator-sessions.md](orchestrator-sessions.md) | The Orchestrator/Dock for many concurrent workspaces/agent sessions, session persistence and the Live/Dormant lifecycle, and the large body of dock UX design vs what ships today. |
| [terminal.md](terminal.md) | The integrated terminal: PTY spawning, the embedded `fresh-winterm` VT emulator (and why custom), live/scrollback per-buffer state, mouse/links/OSC52, and restore-on-reconnect. |

### Configuration, features & quality
| Doc | What it covers |
|-----|----------------|
| [config-themes-settings.md](config-themes-settings.md) | The layered config overlay and resolution, JSONC + comment-preserving writes, schemars schema generation driving the Settings UI, the theme system and live preview, and the keybinding editor. |
| [search-and-diff.md](search-and-diff.md) | In-buffer search/replace, project-wide search and live grep, the diff/review (hunk) viewer and its `Arc::ptr_eq` piece-tree diff, git-log viewing, and the keyboard-macro system. |
| [editor-ux-features.md](editor-ux-features.md) | Smaller shipped features without their own doc: markdown compose/preview, code tour, the input calibration wizard, vi mode, i18n, the menu/command-palette/help/bookmarks cluster, and warning/notification UX. |
| [testing.md](testing.md) | The testing layers, the headless **scenario** framework (tests as data, replayed against `EditorTestApi`), the ANSI capture backend, the `TimeSource` determinism abstraction, and CI structure. |

## A note on history

This set replaces ~120 older design notes, plans, and usability evaluations
that had accumulated here. Most documented *intent* (often aspirational or
already shipped and drifted) rather than the system as built. The valuable
rationale from them has been distilled into the docs above, with each doc
listing the older notes it supersedes. The originals remain in **git history**
if you need the full archaeological record — for example:

```
git log --all --full-history -- 'docs/internal/orchestrator-sessions-design.md'
git show <commit>:docs/internal/<old-doc>.md
```

## User-facing documentation

See the parent [docs/](../) directory:
- [Architecture](../architecture.md) — user-facing system architecture overview
- [Getting Started](../getting-started/), [Features](../features/)
- [Plugins](../plugins/) and [Plugin Development](../plugins/development/)
