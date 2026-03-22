# Search and Replace

*   **Search:** Press `Ctrl+F` to open the search prompt.
*   **Replace:** Press `Ctrl+R` to open the search and replace prompt.
*   **Query Replace:** Use "Query Replace" from the command palette for interactive replacement (y/n/!/q prompts for each match).

The search toolbar shows toggle buttons for:
- **Case Sensitive** — match exact case
- **Whole Word** — match complete words only
- **Regex** — use regular expressions

## Regex and Capture Groups

When regex mode is enabled, the replacement string supports capture groups: `$1`, `$2`, or `${name}` for named groups. For example, searching for `(\w+): (\w+)` and replacing with `$2: $1` swaps the two words around the colon.

## Project-Wide Search and Replace

Use "Search and Replace in Project" from the command palette to search across all git-tracked files in the project. Press `Alt+Enter` to replace all matches across the project. Works with unsaved buffers and large files, up to 10,000 results.
