# Compare Files

> **Palette:** `Compare Files...`, `Compare Active File With...`
>
> **CLI:** `fresh --diff LEFT RIGHT`

Compare Files shows two files side by side in one tab, with the lines that match on the same row and the differences highlighted. Both sides are the files themselves, so you can edit either one. Typing, undo, save, search and language support all work as they do anywhere else, and the highlighting follows your edits.

```bash
fresh --diff old.conf new.conf
```

The two files can be any text files, in a repository or not. The comparison is their only tab. If one of them was already open, it keeps its own tab as well, which shows the same highlights while the comparison is open.

Closing the comparison closes the files it opened, unless you've changed one without saving. That file stays open in a tab, so you can still save or discard your edits. Quitting with unsaved changes in a comparison asks about them, as it would for any file.

## Reading a comparison

*   A line changed on both sides has a `~` in the gutter and a background on both sides, with the part of the line that differs picked out.
*   A line only the left file has is marked `-`, and a line only the right file has is marked `+`. The other side shows an empty filler row there, so the lines after the change stay level.
*   Two lines that differ only in how they end are marked `~` with a note at the end, such as *no newline at end of file*, *CRLF line ending* or *LF line ending*.
*   Each difference also gets a mark on the scrollbar, so you can see where the changes are in a long file.
*   The two sides scroll together.

Lines don't wrap inside a comparison, because a wrapped line would push one side out of step with the other. The two sides scroll in whole lines, so while the top of one pane is inside a run of lines only that side has, the other pane holds at the edge of that run. They line up again as soon as the run scrolls past.

## Commands

| Command | Default key | What it does |
|---------|-------------|--------------|
| **Compare: Next Difference** | `Alt+F5` | Moves both sides to the next difference, wrapping at the end |
| **Compare: Previous Difference** | `Shift+Alt+F5` | Moves both sides to the previous difference |
| **Compare: Switch Side** | `Ctrl+F6` | Moves the focus to the other file |
| **Compare: Copy Change to Other Side** | | Makes the other file's lines of the change under the cursor the same as this file's (vimdiff's `dp`) |
| **Compare: Take Change from Other Side** | | Makes this file's lines of the change under the cursor the same as the other file's (vimdiff's `do`) |
| **Compare: Close** | | Closes the comparison tab |

Copying a change makes the lines byte-for-byte the same, line endings included, so the difference goes away. It edits the buffer without saving it, and a single undo in that panel puts it back. To change the keys, run **Keybinding Editor**.

## As git's diff tool

`fresh --diff` runs in the terminal you typed it in and returns when you quit, which is what `git difftool` expects:

```bash
git config --global difftool.fresh.cmd 'fresh --diff "$LOCAL" "$REMOTE"'
git config --global diff.tool fresh
git difftool          # one comparison per changed file
```

`fresh --diff` is a [direct launch](./session-persistence.md#direct-vs-daemon-mode), like `fresh FILE`: no session is restored or saved, no daemon is started, and it shows each file as it is on disk, not edits left unsaved by an earlier session. Run from Fresh's own integrated terminal, it also runs in that terminal instead of opening in the editor around it. That's because `git difftool` deletes its temporary copies as soon as the tool returns.

`fresh --diff` refuses to start, printing why and exiting with status 2, in these cases:

*   a path doesn't exist or is a directory;
*   both paths name the same file;
*   it's run with `--no-plugins` or `--safe`;
*   your config turns the `compare_files` plugin off.

A binary file, or a file large enough to open in large-file mode (10 MB by default, set by `large_file_threshold_bytes`), isn't compared: the status bar says so.

## For plugin authors

The comparison view comes from the bundled `compare_files` plugin. The editor's part is the command-line flag: it doesn't open the files itself. Instead, `editor.diffLaunch()` returns their absolute paths at `ready`, so a plugin can provide a different view without any change to the command line. Scripts and other plugins can open a comparison with `editor.getPluginApi("compare-files").compare(left, right)`.

The plugin builds on three host APIs, which any plugin can use:

*   `createBufferGroup` with `{type: "scrollable", id, path}` leaves shows a file in a panel without giving it a tab.
*   `compareBuffers(left, right)` diffs two buffers in the host and returns the byte ranges to paint from, so no file text is copied into the plugin. The diff runs off the editor thread.
*   `replaceRange(buffer, start, end, text, expectedVersion)` makes one edit as one undo step, and does nothing if the buffer has changed since that version.
