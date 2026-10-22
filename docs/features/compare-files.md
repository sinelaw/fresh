# Compare Files

> **Palette:** `Compare Files...`, `Compare Active File With...`
>
> **CLI:** `fresh --diff LEFT RIGHT`

Compare Files shows two files side by side in one tab, with the lines that match on the same row and the differences highlighted. Both sides are the files themselves, so you can edit either one. Typing, undo, save, search and language support all work as they do anywhere else, and the highlighting follows your edits.

```bash
fresh --diff old.conf new.conf
```

The two files can be any files, in a repository or not. Each one also keeps its own tab, which shows the same highlights while the comparison is open. Closing the comparison removes the highlights and leaves both files open.

## Reading a comparison

*   A line changed on both sides has a `~` in the gutter and a background on both sides, with the part of the line that differs picked out.
*   A line only the left file has is marked `-`, and a line only the right file has is marked `+`. The other side shows an empty filler row there, so the lines after the change stay level.
*   Each difference also gets a mark on the scrollbar, so you can see where the changes are in a long file.
*   The two sides scroll together.

Lines don't wrap inside a comparison, because a wrapped line would push one side out of step with the other.

## Commands

| Command | Default key | What it does |
|---------|-------------|--------------|
| **Compare: Next Difference** | `Alt+F5` | Moves both sides to the next difference, wrapping at the end |
| **Compare: Previous Difference** | `Shift+Alt+F5` | Moves both sides to the previous difference |
| **Compare: Switch Side** | `Ctrl+F6` | Moves the focus to the other file |
| **Compare: Copy Change to Other Side** | | Makes the other file's lines of the change under the cursor match this file (vimdiff's `dp`) |
| **Compare: Take Change from Other Side** | | Makes this file's lines of the change under the cursor match the other file (vimdiff's `do`) |
| **Compare: Close** | | Closes the comparison tab |

Copying a change edits the buffer and doesn't save it. Save the file when you're happy with it, or undo in that panel to put it back. To change the keys, run **Keybinding Editor**.

## As git's diff tool

`fresh --diff` runs in the terminal you typed it in and returns when you quit, which is what `git difftool` expects:

```bash
git config --global difftool.fresh.cmd 'fresh --diff "$LOCAL" "$REMOTE"'
git config --global diff.tool fresh
git difftool          # one comparison per changed file
```

`fresh --diff` is a [direct launch](./session-persistence.md#direct-vs-daemon-mode), like `fresh FILE`: no session is restored or saved, and no daemon is started. Run from Fresh's own integrated terminal, it also runs in that terminal instead of opening in the editor around it. That's because `git difftool` deletes its temporary copies as soon as the tool returns.

`fresh --diff` refuses to start if a path doesn't exist or is a directory, or with `--no-plugins` or `--safe`. In those cases it prints why and exits with status 2.

## For plugin authors

The comparison view comes from the bundled `compare_files` plugin. The editor's part is the command-line flag: it opens both files as `fresh LEFT RIGHT` would, and `editor.diffLaunch()` returns their absolute paths at `ready`. That means a plugin can provide a different view without any change to the command line. Scripts and other plugins can open a comparison with `editor.getPluginApi("compare-files").compare(left, right)`.
