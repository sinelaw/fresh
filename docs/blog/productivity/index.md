---
title: "Productivity Features"
date: 2026-02-11
description: "Command palette, split view, file explorer, integrated terminal, and more."
outline: false
---

# Productivity Features

*February 11, 2026*

Beyond text editing — the tools that make Fresh a complete development environment in your terminal.

## Command Palette

**Ctrl+P** opens the unified command palette. Type to fuzzy-find files, prefix with `>` for commands, `#` for open buffers, `:` for go-to-line. Tab completion and hints guide you through.

<div class="showcase-demo">
  <img src="./command-palette/showcase.gif" alt="Command palette demo" />
</div>

## Split View

Split the editor horizontally or vertically. Each pane has its own tab bar, cursor, and scroll position. Navigate between panes with **Ctrl+K**.

<div class="showcase-demo">
  <img src="./split-view/showcase.gif" alt="Split view demo" />
</div>

## File Explorer

**Ctrl+E** toggles a sidebar file tree. Navigate with arrow keys, expand directories with Enter, and type to fuzzy-filter. Git status indicators show modified and untracked files.

<div class="showcase-demo">
  <img src="./file-explorer/showcase.gif" alt="File explorer demo" />
</div>

## Integrated Terminal

Open a terminal split inside the editor. Supports scrollback history, keyboard capture mode (F9), mouse forwarding, and session persistence across restarts.

<div class="showcase-demo">
  <img src="./terminal/showcase.gif" alt="Integrated terminal demo" />
</div>

## Also New

- **Session Persistence** — detach/reattach sessions with `fresh -a`, state preserved across terminal disconnections
- **Package Manager** — browse and install plugins, themes, and language packs from the registry
- **SSH Remote Editing** — `fresh user@host:path` with password/key auth and sudo save
- **Text Encoding** — UTF-16, Latin-1, GBK, Shift-JIS, EUC-KR, and more
- **Diagnostics Panel** — LSP errors/warnings in a dedicated split view
- **Side-by-Side Diff** — word-level diff with synchronized scrolling
- **Bracket Matching** — matching brackets highlighted automatically
- **Vi Mode** — modal editing with operators, motions, text objects
- **i18n** — 11 languages with plugin translation support

## Related

- [All features](/features/)
- [Getting started](/getting-started/)
- [Configuration](/configuration/)
- [Plugins](/plugins/)
