# Search and Replace

*   **Search:** Press `Ctrl+F` to open the search prompt.
*   **Replace:** Press `Ctrl+R` to open the search and replace prompt.
*   **Query Replace:** Use "Query Replace" from the command palette for interactive replacement (y/n/!/q prompts for each match).

The search toolbar shows toggle buttons for:
- **Case Sensitive** (`Alt+C`) — match exact case
- **Whole Word** (`Alt+W`) — match complete words only
- **Regex** (`Alt+R`) — use regular expressions

## Case Sensitivity

Searches fold case by default: `todo` finds `TODO`. Every search surface can
say otherwise, and each remembers what you told it:

| Surface | Where the toggle is | How long the choice lasts |
| --- | --- | --- |
| Find / Replace prompt (`Ctrl+F`, `Ctrl+R`) | **Case Sensitive** on the options row, `Alt+C` | Saved with the workspace |
| Universal Search (Live Grep) | **Case** on the toolbar, `Alt+C` | The rest of the session |
| Search & Replace panel | **Case** checkbox on the panel, `Alt+C` | The rest of the session, panel reopens included |
| Git Grep | **Case** checkbox on the prompt toolbar (unbound; `git_grep_toggle_case`) | The rest of the session |

To start somewhere else, set the default in your config:

```json
{
  "editor": {
    "search": {
      "case_sensitive": true
    }
  }
}
```

`case_sensitive` seeds all four surfaces; flipping a toggle still wins from
there, and the Find prompt saves a choice that differs from it with the
workspace.

Workspaces saved by an earlier Fresh recorded the toggles on every save,
whether or not you had chosen anything, from a prompt that started
case-sensitive. Reopening one keeps the toggles you can be shown to have
set — a whole-word or regex search survives — but its case sensitivity
falls back to the setting above, because in those files "on" is
indistinguishable from "never touched". The section's other keys — `whole_word`, `regex`, `confirm_each` —
reach the Find/Replace prompt and the Search & Replace panel; see
[Configuration → Search](../configuration/index.md#search) for which applies
where.

## Stepping Through Matches

With the search bar open, `F3` jumps to the next match and `Shift+F3` to the
previous one **without closing the bar**, so the query stays editable and the
toggles stay reachable. The status bar reports `Match N of M` as you go. The
same keys work after the bar is closed, continuing from the last search.

`Enter` jumps to the current match and closes the bar; `Esc` cancels.

## Regex and Capture Groups

When regex mode is enabled, the replacement string supports capture groups: `$1`, `$2`, or `${name}` for named groups. For example, searching for `(\w+): (\w+)` and replacing with `$2: $1` swaps the two words around the colon.

The replacement also interprets the standard escape sequences `\n` (newline), `\t` (tab), `\r` (carriage return), and `\\` (literal backslash), so you can insert line breaks or indentation. Plain-text (non-regex) replacement treats these as literal characters.

In regex mode, `^` and `$` anchor at line boundaries, so an anchored pattern matches on every line.

## Clearing Highlights

Run **Clear Search Highlights** from the command palette to remove the active search highlights from the buffer.

## Project-Wide Search and Replace

Use "Search and Replace in Project" from the command palette to search across all git-tracked files in the project. Press `Alt+Enter` to replace all matches across the project. Works with unsaved buffers and large files, up to 10,000 results.

Use the **Files** field to limit project search to comma-separated globs. A
pattern without a directory separator matches file names at any depth (for
example, `*.rs`); a pattern with a separator matches workspace-relative paths
(for example, `src/**` or `tests/*.rs`).
