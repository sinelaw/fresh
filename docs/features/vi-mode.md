# Vi Mode

Fresh ships a vi mode for modal editing. Turn it on with **Toggle Vi mode** from
the command palette (`Ctrl+P`); run it again to go back to Fresh's normal
keys. Vi mode applies to the whole editor, in every window, and the status bar
shows the current mode.

Its settings are in the Settings UI under **Plugins → vi_mode**:

- **AutoStart** — turn vi mode on whenever Fresh starts (off by default).
- **ArrowKeys** — let the arrow keys move the cursor in vi's modes (on by default).
- **SearchWordUnderCursor** — `*` and `#` search for the word under the cursor (on by default).

`Ctrl+P` (command palette) and `Ctrl+Q` (quit) keep working in Normal, Insert
and Visual modes, so the rest of Fresh is always one key away.

## Modes

| Mode | Enter with | What it does |
|------|-----------|--------------|
| Normal | `Esc` | Keys move the cursor and run commands; typing doesn't insert text |
| Insert | `i` `a` `I` `A` `o` `O` | Typing inserts text, as in Fresh's normal editing |
| Visual | `v` | Select by character |
| Visual Line | `V` | Select whole lines |
| Command line | `:` | Type an ex command (see [below](#ex-commands)) |

In a visual mode, `v` and `V` switch between the two kinds of selection;
pressing the current one again, or `Esc`, returns to Normal.

## Motions

| Keys | Moves to |
|------|----------|
| `h` `j` `k` `l` | left, down, up, right |
| `w` `b` `e` / `W` `B` `E` | next word, previous word, end of word (word / WORD) |
| `0` `^` `$` | line start, first non-blank, line end |
| `_` `+` `-` | first non-blank of this, the next, the previous line |
| `gg` / `G` | first / last line (`5G` goes to line 5) |
| `{` / `}` | previous / next paragraph |
| `%` | matching bracket |
| `f` `t` `F` `T` + char | next / till next / previous / till previous character on the line |
| `;` / `,` | repeat the last `f`/`t`/`F`/`T` forward / backward |
| `/` then `n` `N` | search, next / previous match |
| `*` / `#` | search for the word under the cursor forward / backward |
| `Ctrl+D` / `Ctrl+U` | down / up ten lines |

## Operators

An operator followed by a motion or text object acts on that range: `dw`
deletes a word, `c$` changes to the end of the line, `y%` yanks to the matching
bracket, `dfx` deletes through the next `x`, `d;` repeats the last find as the
range. Doubling an operator acts on the whole line (`dd`, `cc`, `yy`, `>>`, `<<`).

| Key | Operator |
|-----|----------|
| `d` | delete |
| `c` | change (delete, then Insert) |
| `y` | yank (copy) |
| `>` / `<` | indent / dedent |

Shortcuts that act without a motion:

| Keys | Action |
|------|--------|
| `x` / `X` | delete the character under / before the cursor |
| `D` / `C` | delete / change to end of line |
| `s` / `S` | substitute a character / the whole line |
| `Y` | yank the line |
| `r` + char | replace the character under the cursor |
| `p` / `P` | paste after / before (line-wise yanks paste on their own lines) |
| `J` | join with the next line |
| `~` | toggle case |
| `u` / `Ctrl+R` | undo / redo |

Yank, delete and change all share one register, which is Fresh's clipboard,
so text yanked in vi mode can be pasted anywhere and vice versa.

## Text Objects

After an operator (`diw`, `ci"`, `ya(`) or in Visual mode (`viw`, `va{`), `i`
selects the inside of an object and `a` selects it with its surroundings.

| Key | Object |
|-----|--------|
| `w` / `W` | word / WORD |
| `"` `'` `` ` `` | quoted string |
| `(` `)` `b` | parentheses |
| `{` `}` `B` | braces |
| `[` `]` | brackets |
| `<` `>` | angle brackets |

## Counts and Repeat

- A count before a motion or command repeats it: `3w`, `5j`, `2fx`, `3x`.
- Counts work with operators too: `3dd` deletes three lines, `d3w` three words.
- `.` repeats the last change, including the text typed in Insert mode. A count before `.` replaces the original one (`3.`).

## Visual Mode Operators

With a selection, `d` (or `x`), `c` (or `s`), `y`, `>` and `<` act on it and
return to Normal. Visual and Visual Line modes also support `J` to join the
selected lines.

## Ex Commands

Press `:` to open the command line. Commands can be abbreviated as in Vim (`:w`,
`:wq`, `:bn`), and `!` forces one that would otherwise refuse because of unsaved
changes.

| Command | Action |
|---------|--------|
| `:w` / `:w file` | save / save as another file |
| `:q` / `:q!` | quit Fresh (refuses if anything is unsaved) / quit without saving |
| `:wq` | save and quit (always writes) |
| `:x`, `ZZ` | save if modified, then quit |
| `ZQ` | quit without saving |
| `:wa` / `:qa` / `:wqa` | save all / quit all / save all and quit |
| `:e file` / `:e!` | open a file / reload the current one, discarding changes |
| `:42` | go to line 42 |
| `:bn` `:bp` `:bd` `:b name` `:ls` | next / previous / close / switch buffer, list buffers |
| `:sp` `:vs` `:close` | split horizontally / vertically, close the split |
| `:tabclose` `:tabnext` `:tabprevious` | close / next / previous tab |
| `:noh` | clear search highlighting |
| `:set number` / `:set nonumber` | show / hide line numbers |

## Not Supported

Some Vim features have no vi-mode equivalent yet: named registers (`"a`), marks
(`m`), macro recording with `q` (use Fresh's own [macros](./editing.md#macros)),
Replace mode (`R`), and the `:s`, `:g` and `:!` commands (use Fresh's
[search and replace](./search-replace.md) and [terminal](./terminal.md) instead).
