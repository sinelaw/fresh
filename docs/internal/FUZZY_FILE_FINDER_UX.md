# Fuzzy File Finder UX Research and Design

This document provides a comprehensive review of fuzzy file finder UX patterns across popular editors and proposes an implementation for Fresh Editor.

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Research: Editor Comparison](#research-editor-comparison)
3. [UX Pattern Analysis](#ux-pattern-analysis)
4. [Recommended Implementation](#recommended-implementation)
5. [Technical Design](#technical-design)
6. [Usage Guide](#usage-guide)

---

## Executive Summary

### Current State

Fresh Editor has a `git_find_file` plugin that:
- Uses `git ls-files` to find files
- Only works in git repositories
- Does not support non-tracked files
- Has no fallback for non-git directories

### Goals

1. **Platform-agnostic file finding** - Work in any directory, with or without git
2. **Unified entry point** - Single keybinding (`Ctrl+P`) with prefix-based mode switching
3. **Buffer/open file finder** - Quick switching between open files
4. **Robust performance** - Handle large codebases efficiently
5. **Familiar UX** - Follow conventions from VSCode/Sublime that users already know

### Key Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Entry point pattern | Unified with prefixes (VSCode model) | Most familiar, efficient once learned |
| Primary keybinding | `Ctrl+P` | Universal convention |
| Command prefix | `>` | VSCode/Sublime convention |
| Buffer prefix | `#` or none with separate binding | Fast access to open files |
| File discovery | Hierarchical: git → fd → find | Best performance where available |

---

## Research: Editor Comparison

### 1. VSCode - Quick Open and Command Palette

#### Key Bindings

| Shortcut | Function |
|----------|----------|
| `Ctrl+P` / `Cmd+P` | Quick Open (file finder) |
| `Ctrl+Shift+P` / `Cmd+Shift+P` | Command Palette |
| `Ctrl+Shift+O` / `Cmd+Shift+O` | Go to Symbol in File |
| `Ctrl+T` / `Cmd+T` | Go to Symbol in Workspace |
| `Ctrl+G` | Go to Line |
| `Ctrl+Tab` | Cycle through recent files |

#### Mode Switching via Prefix Characters

VSCode uses a **unified input field** with prefix characters to switch modes:

| Prefix | Mode | Description |
|--------|------|-------------|
| (none) | File Finder | Search for files by name |
| `>` | Command Palette | Execute editor commands |
| `@` | Symbol in File | Navigate to symbols in current file |
| `@:` | Symbol in File (grouped) | Symbols grouped by category |
| `#` | Symbol in Workspace | Search symbols across all files |
| `:` | Go to Line | Jump to specific line number |
| `?` | Help | View available prefix commands |

#### Strengths
- Single entry point (`Ctrl+P`) can access all navigation modes
- Typing `>` transforms file finder into command palette seamlessly
- Fuzzy matching works across all modes
- Prefix characters are discoverable via `?`
- Combining prefixes works (e.g., `filename@symbol` goes to symbol in specific file)

#### Weaknesses
- Users must learn/remember prefix characters
- The `>` prefix requires an extra keystroke for commands
- Less discoverable for new users without documentation

---

### 2. Sublime Text - Goto Anything

#### Key Bindings

| Shortcut | Function |
|----------|----------|
| `Ctrl+P` / `Cmd+P` | Goto Anything (main entry) |
| `Ctrl+R` / `Cmd+R` | Goto Symbol (pre-fills `@`) |
| `Ctrl+G` | Goto Line (pre-fills `:`) |
| `Ctrl+;` | Goto Word (pre-fills `#`) |

#### Mode Switching via Operators

| Operator | Mode | Description |
|----------|------|-------------|
| (none) | File Search | Files in project, recently closed, open files |
| `@` | Symbol Navigation | Browse symbols in file |
| `#` | Fuzzy Search in File | Search text within current file |
| `:` | Line Number | Jump to specific line |

#### Unique Features
- **Combinable operators**: Chain file + operator, e.g., `tp@rf` finds file matching "tp" then navigates to symbol matching "rf"
- **Instant navigation**: Selections happen instantly as you type
- **Performance**: Handles 50,000+ file projects smoothly

#### Strengths
- Pioneered the fuzzy finding pattern (influential on all others)
- Extremely fast fuzzy matching algorithm
- Clean, minimal interface
- Operators can be combined for powerful navigation

#### Weaknesses
- Limited to file/symbol/line navigation (no command palette integration)
- `#` for text search is less intuitive than `@` for symbols

---

### 3. Neovim with Plugins

#### Telescope.nvim

**Default Key Mappings (Common Configuration):**
```lua
<leader>ff  -- find_files (file finder)
<leader>fg  -- live_grep (content search)
<leader>fb  -- buffers (open buffers)
<leader>fh  -- help_tags (help documentation)
<leader>fo  -- oldfiles (recent files)
<leader>fs  -- lsp_document_symbols
<leader>fS  -- lsp_workspace_symbols
```

**Built-in Pickers:**

| Picker | Description |
|--------|-------------|
| `find_files` | List files in cwd |
| `git_files` | List git-tracked files |
| `live_grep` | Search content with ripgrep |
| `buffers` | Open buffers |
| `oldfiles` | Recently opened files |
| `commands` | Available commands |
| `keymaps` | Show keymaps |
| `lsp_references` | LSP references |
| `lsp_document_symbols` | Symbols in file |
| `lsp_workspace_symbols` | Symbols in workspace |

#### fzf.vim

**Common Commands:**
```vim
:Files [PATH]     " File finder
:GFiles           " Git files
:Buffers          " Open buffers
:Rg [PATTERN]     " Ripgrep search
:Lines            " Lines in loaded buffers
:BLines           " Lines in current buffer
:History          " File history (v:oldfiles + open buffers)
:Commands         " Commands
```

**Multi-select Features:**
- `Tab` / `Shift-Tab`: Toggle selection
- `Alt-A`: Select all matches
- `Alt-D`: Deselect all
- Multiple selections populate quickfix list

#### Strengths
- Highly customizable and extensible
- Native Lua (Telescope) feels more integrated
- Preview functionality is excellent
- Frecency-based ranking improves over time
- Multi-select to quickfix is powerful for bulk operations

#### Weaknesses
- Requires configuration/setup
- Multiple separate commands to learn
- No unified entry point like VSCode
- Plugin ecosystem fragmentation (Telescope vs fzf.vim vs fzf-lua)

---

### 4. JetBrains IDEs - Search Everywhere

#### Key Bindings

| Shortcut | Function |
|----------|----------|
| `Shift Shift` (double) | Search Everywhere |
| `Ctrl+N` | Go to Class |
| `Ctrl+Shift+N` | Go to File |
| `Ctrl+Alt+Shift+N` | Go to Symbol |
| `Ctrl+Shift+A` | Find Action (commands) |
| `Ctrl+E` | Recent Files |
| `Ctrl+Shift+E` | Recent Locations |

#### Tab-Based Mode Switching

Search Everywhere uses **tabs** to filter results:
- **All** - Everything (default)
- **Classes** - Class names
- **Files** - File names
- **Symbols** - Methods, fields, constants
- **Actions** - Commands and settings
- **Git** - Branches, commits, tags

Press `Tab` to cycle between tabs.

#### Strengths
- Double-Shift is highly memorable and ergonomic
- Tab-based filtering is visual and discoverable
- Shows recent files by default (most common use case)
- Deep integration with project structure

#### Weaknesses
- Multiple overlapping entry points can be confusing
- "Search Everywhere" can return too many results
- Heavier/slower than simpler fuzzy finders
- Tab navigation requires multiple keystrokes

---

### 5. Emacs - Helm, Ivy/Counsel, Vertico/Consult

#### Modern Stack (Vertico/Consult/Orderless)

| Package | Purpose |
|---------|---------|
| **Vertico** | Vertical completion UI |
| **Consult** | Enhanced completion commands |
| **Orderless** | Fuzzy/flexible matching |
| **Marginalia** | Annotations beside candidates |
| **Embark** | Context actions on candidates |

**Common Bindings:**
```elisp
C-x b       -- consult-buffer (buffers + recent files)
C-x p f     -- project-find-file
M-s l       -- consult-line (search in buffer)
M-s g       -- consult-ripgrep
M-g g       -- consult-goto-line
M-g i       -- consult-imenu (symbols)
```

**Orderless Matching:**
With Orderless, `car lo` matches `Cargo.lock` - space-separated terms match in any order.

#### Strengths
- Highly composable - mix and match packages
- Deep Emacs integration
- Embark provides powerful contextual actions
- Marginalia adds helpful context without clutter

#### Weaknesses
- Steep learning curve
- Requires configuration for optimal experience
- Multiple competing ecosystems
- Fuzzy matching behavior varies between packages

---

## UX Pattern Analysis

### Entry Point Patterns Comparison

| Pattern | Used By | Pros | Cons |
|---------|---------|------|------|
| **Unified with prefixes** | VSCode, Sublime | Single entry, composable, efficient | Learning curve for prefixes |
| **Separate commands** | Neovim, Emacs | Direct access, highly customizable | More bindings to remember |
| **Tabbed interface** | JetBrains | Visual, discoverable | More keystrokes to switch modes |
| **Hybrid** | JetBrains | Both unified and specific shortcuts | Can be confusing |

### Prefix Character Conventions

| Prefix | Common Meaning | Used By |
|--------|----------------|---------|
| `>` | Commands/Actions | VSCode |
| `@` | Symbols in file | VSCode, Sublime |
| `#` | Workspace symbols (VSCode) / Text search (Sublime) | VSCode, Sublime |
| `:` | Line number | VSCode, Sublime |
| `?` | Help | VSCode |

### Ranking Algorithms

| Algorithm | Description | Used By |
|-----------|-------------|---------|
| **Fuzzy match score** | Character order, gaps, bonuses | All |
| **Frecency** | Frequency + recency combined | Firefox, smart-open.nvim |
| **Recency only** | Most recently used first | JetBrains (recent files) |
| **Alphabetical** | Fallback when scores equal | Most editors |

**Frecency Algorithm (Mozilla):**
```
score = frequency_weight * recency_weight

recency_weight based on:
- Last 4 hours: 100
- Last day: 70
- Last week: 50
- Last month: 30
- Last 90 days: 10
```

### Best Practices Identified

1. **Default to most common use case** - Show file search or recent files
2. **Make modes discoverable** - Help prefix or visible tabs
3. **Allow combination** - `file@symbol` pattern is powerful
4. **Provide both unified and specific entry points** - Power users want direct access
5. **Preview is valuable** - Helps confirm selection before committing
6. **Frecency improves over time** - Personalized ranking based on usage
7. **Multi-select for bulk operations** - Populate lists for batch operations
8. **Respect .gitignore** - Don't show files users don't care about

---

## Recommended Implementation

### Phase 1: Core File Finder (MVP)

#### Keybinding: `Ctrl+P`

Opens unified finder that defaults to file search mode.

#### Prefix Support

| Prefix | Mode | Description |
|--------|------|-------------|
| (none) | File Finder | Search files in project/cwd |
| `>` | Command Palette | Execute commands (existing) |
| `#` | Buffer Finder | Search open buffers |
| `:` | Go to Line | Jump to line (if in file mode) |

#### File Discovery Strategy

Hierarchical approach for best performance:

```
1. If in git repo AND git available:
   → git ls-files (fast, respects .gitignore)
   → Optionally include untracked: git ls-files --others --exclude-standard

2. Else if `fd` available:
   → fd --type f --hidden --exclude .git
   (respects .gitignore by default, very fast)

3. Else if `find` available (Unix):
   → find . -type f -not -path '*/.git/*'
   (slower, doesn't respect .gitignore)

4. Fallback: directory traversal in plugin
   → Recursive readdir with exclusion patterns
```

#### Default Exclusions

When not using git/fd (which respect .gitignore):
```
.git/
node_modules/
target/
__pycache__/
*.pyc
.DS_Store
*.lock
dist/
build/
.next/
```

### Phase 2: Enhanced Features

#### Frecency Ranking

Store file access history:
```typescript
interface FileAccess {
  path: string;
  accessCount: number;
  lastAccess: number; // timestamp
}
```

Boost scores based on frecency when displaying results.

#### Preview Mode

When enabled, show file content in a preview pane as user navigates results.

#### Recent Files Integration

Show recently opened files at the top of results when query is empty.

### Phase 3: Advanced Features (Future)

- Symbol search (`@` prefix) - requires LSP integration
- Workspace symbol search (`#` prefix with different semantics)
- File path + symbol combination (`file@symbol`)
- Bookmarks integration

---

## Technical Design

### Plugin Structure

Create new plugin: `find_file.ts` (replaces/extends `git_find_file.ts`)

```typescript
// find_file.ts - Universal file finder

const finder = new Finder<FileEntry>(editor, {
  id: "find-file",
  format: formatFileEntry,
  preview: true,
  maxResults: 100,
});

interface FileEntry {
  path: string;
  relativePath: string;
  frecencyScore?: number;
}

// Hierarchical file discovery
async function discoverFiles(): Promise<FileEntry[]> {
  // Try git first
  const gitResult = await tryGitFiles();
  if (gitResult) return gitResult;

  // Try fd
  const fdResult = await tryFdFiles();
  if (fdResult) return fdResult;

  // Try find
  const findResult = await tryFindFiles();
  if (findResult) return findResult;

  // Fallback to manual traversal
  return await manualTraversal();
}
```

### Unified Entry Point

Handle prefix detection in the prompt input handler:

```typescript
function handleInput(query: string) {
  if (query.startsWith(">")) {
    // Switch to command palette mode
    switchToCommandPalette(query.slice(1));
  } else if (query.startsWith("#")) {
    // Switch to buffer finder mode
    switchToBufferFinder(query.slice(1));
  } else if (query.startsWith(":") && /^\d+$/.test(query.slice(1))) {
    // Go to line mode
    goToLine(parseInt(query.slice(1)));
  } else {
    // File finder mode
    filterFiles(query);
  }
}
```

### Buffer Finder

```typescript
const bufferFinder = new Finder<BufferInfo>(editor, {
  id: "buffer-finder",
  format: (buf) => ({
    label: buf.name,
    description: buf.path,
    location: { file: buf.path, line: 1, column: 1 }
  }),
  preview: false,
  maxResults: 50,
});

async function loadBuffers(): Promise<BufferInfo[]> {
  const buffers = await editor.getOpenBuffers();
  // Sort by most recently accessed
  return buffers.sort((a, b) => b.lastAccess - a.lastAccess);
}
```

### Keybinding Configuration

```json
{
  "keybindings": {
    "ctrl+p": "unified_finder",
    "ctrl+shift+p": "command_palette"
  }
}
```

### API Additions Needed

The plugin API may need:
1. `editor.getOpenBuffers()` - List open buffer info
2. `editor.getRecentFiles()` - List recently opened files
3. `editor.getCurrentDirectory()` - Get working directory
4. `editor.commandExists(cmd)` - Check if external command available

---

## Appendix: Fuzzy Matching Algorithm

Fresh already has a robust fuzzy matching implementation in `src/input/fuzzy.rs`:

**Scoring Bonuses:**
- `START_OF_STRING` (48): Match at start
- `WORD_BOUNDARY` (32): Match after separator (space, _, -, /, .)
- `CAMEL_CASE` (24): CamelCase transitions
- `CONSECUTIVE` (16): Consecutive character matches
- `EXACT_MATCH` (100): Query equals target
- `EXACT_BASENAME_MATCH` (80): Prefix match before extension
- `GAP_PENALTY` (-3): Per gap between matches
- `GAP_START_PENALTY` (-5): Starting a gap

This algorithm is already well-suited for file path matching with its basename and path separator awareness.

---

## References

- [VSCode Tips and Tricks](https://code.visualstudio.com/docs/getstarted/tips-and-tricks)
- [Sublime Text Navigation](https://docs.sublimetext.io/guide/usage/file-management/navigation.html)
- [Telescope.nvim](https://github.com/nvim-telescope/telescope.nvim)
- [fzf.vim](https://github.com/junegunn/fzf.vim)
- [JetBrains Search Everywhere](https://www.jetbrains.com/help/idea/searching-everywhere.html)
- [Reverse Engineering Sublime Text's Fuzzy Match](https://www.forrestthewoods.com/blog/reverse_engineering_sublime_texts_fuzzy_match/)
- [Mozilla Frecency Algorithm](https://developer.mozilla.org/en-US/docs/Mozilla/Tech/Places/Frecency_algorithm)

---

## Usage Guide

### Implementation Status

The universal file finder has been implemented in `plugins/find_file.ts` with full i18n support in `plugins/find_file.i18n.json`.

### Available Commands

Access these commands via the command palette (`Ctrl+P` then type the command name):

| Command | Description |
|---------|-------------|
| **Find File (Unified)** | Opens the unified finder with prefix support |
| **Find File** | Opens the file-only finder (no prefix modes) |
| **Find Buffer** | Opens the buffer/open files finder |
| **Reload File Index** | Refreshes the cached file list |

### Prefix Mode Support

When using the unified finder:

| Input | Behavior |
|-------|----------|
| `foo` | Search for files matching "foo" |
| `>save` | Switch to command palette, search for "save" commands |
| `#main` | Switch to buffer finder, search for buffers matching "main" |
| `:42` | Jump to line 42 in the current buffer |

### Rebinding Ctrl+P to the Unified Finder

By default, `Ctrl+P` opens the command palette. To make it open the unified file finder instead (VSCode-style), add this to your keybindings configuration:

```json
{
  "bindings": [
    {
      "key": "p",
      "modifiers": ["ctrl"],
      "action": "plugin_command",
      "args": {"command": "start_unified_finder"},
      "when": "global"
    }
  ]
}
```

Or, to keep both behaviors:
- `Ctrl+P` - Unified finder (files by default, `>` for commands)
- `Ctrl+Shift+P` - Command palette directly

### File Discovery Priority

The finder uses this hierarchy to find files:

1. **git ls-files** (if in a git repo) - Fastest, respects .gitignore
2. **fd** (if installed) - Very fast, respects .gitignore
3. **find** (Unix/Linux/macOS) - Slower, uses exclusion patterns
4. **Manual traversal** - Fallback, uses built-in exclusion list

### Frecency Ranking

Files you access frequently will appear higher in results. The ranking uses Mozilla's frecency algorithm:
- Recent accesses (< 4 hours) get maximum weight
- Older accesses get progressively less weight
- Access count is multiplied by recency weight

### Default Exclusions

When git/fd aren't available, these patterns are excluded:
- `.git/`, `node_modules/`, `target/`, `__pycache__/`
- `dist/`, `build/`, `.next/`, `.cache/`, `coverage/`
- `venv/`, `.venv/`, `env/`, `.env/`
- `*.lock`, `*.min.js`, `*.min.css`, `*.map`
- `vendor/`, `Pods/`
