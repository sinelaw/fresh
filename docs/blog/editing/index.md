---
title: "Editing Features"
date: 2026-02-11
description: "Multi-cursor, search & replace, move lines, block selection, sort, case conversion, and more."
outline: false
---

# Editing Features

*February 11, 2026*

The core text editing capabilities that make Fresh feel like a modern editor in your terminal.

## Multi-Cursor Editing

Place your cursor on a word, press **Ctrl+W** to select it, then **Ctrl+D** to select the next occurrence. Repeat to select more. Type to replace them all simultaneously.

<div class="showcase-demo">
  <img src="./multi-cursor/showcase.gif" alt="Multi-cursor editing demo" />
</div>

## Search & Replace

**Ctrl+H** opens find-and-replace with live highlighting as you type. Supports regex with capture groups (`$1`, `$2`), find-in-selection, and a confirm-each toggle.

<div class="showcase-demo">
  <img src="./search-replace/showcase.gif" alt="Search and replace demo" />
</div>

## Move Lines

**Alt+↑** and **Alt+↓** move the current line (or selected lines) up and down. Works with multi-cursor selections.

<div class="showcase-demo">
  <img src="./line-move/showcase.gif" alt="Move lines demo" />
</div>

## Block Selection

**Alt+Shift+Arrow** creates rectangular column selections — useful for editing aligned data, CSV columns, or repetitive code patterns. Block selections convert to multi-cursors when you start typing.

<div class="showcase-demo">
  <img src="./block-selection/showcase.gif" alt="Block selection demo" />
</div>

## Triple-Click Selection

Triple-click to select an entire line — matching the behavior you'd expect from any modern editor.

<div class="showcase-demo">
  <img src="./triple-click/showcase.gif" alt="Triple-click selection demo" />
</div>

## Sort Lines

Select lines and sort them alphabetically via the command palette.

<div class="showcase-demo">
  <img src="./sort-lines/showcase.gif" alt="Sort lines demo" />
</div>

## Case Conversion

Select text and press **Alt+U** for uppercase or **Alt+L** for lowercase. When nothing is selected, it converts the word under the cursor.

<div class="showcase-demo">
  <img src="./case-conversion/showcase.gif" alt="Case conversion demo" />
</div>

## Duplicate Line

Duplicate the current line instantly via the command palette. Works with selections too.

<div class="showcase-demo">
  <img src="./duplicate-line/showcase.gif" alt="Duplicate line demo" />
</div>

## Tab Indent / Dedent

Select lines and press **Tab** to indent, **Shift+Tab** to dedent. Respects per-language tab settings (spaces vs. tabs).

<div class="showcase-demo">
  <img src="./tab-indent/showcase.gif" alt="Tab indent selection demo" />
</div>

## Related

- [Editing features](/features/editing)
- [Keyboard shortcuts](/configuration/keyboard)
