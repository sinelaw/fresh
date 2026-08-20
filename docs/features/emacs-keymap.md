# Emacs Keymap

Fresh ships an Emacs keymap. Turn it on from **View → Keybinding Style → Emacs**,
or set it in `config.json`:

```json
{
  "active_keybinding_map": "emacs"
}
```

The keymap inherits `default`, so every key Emacs has no opinion about — the
settings editor, the completion popup, the integrated terminal, the dock, the
multi-cursor and bookmark keys — keeps working exactly as it does out of the
box. What follows is what the Emacs keymap changes.

## Movement

| Key | Command |
|-----|---------|
| `C-f` / `C-b` | forward / backward char |
| `C-n` / `C-p` | next / previous line |
| `C-a` / `C-e` | beginning / end of line |
| `M-f` / `M-b` | forward / backward word |
| `M-m` | back to indentation |
| `M-<` / `M->` | beginning / end of buffer |
| `M-{` / `M-}` | backward / forward paragraph |
| `C-v` / `M-v` | scroll down / up a page |
| `C-l` | recenter |
| `M-g g`, `M-g M-g` | goto line |

## Editing

| Key | Command |
|-----|---------|
| `C-d` | delete char forward |
| `M-d` / `M-DEL` | kill word forward / backward |
| `C-k` | kill to end of line |
| `C-S-DEL` | kill whole line |
| `C-t` | transpose chars |
| `C-o` | open line |
| `M-;` | comment / uncomment |
| `M-/` | dabbrev expand |
| `M-u` / `M-l` | upcase / downcase |
| `C-/`, `C-_`, `C-x u` | undo |
| `M-_` | redo |

## Mark and kill ring

| Key | Command |
|-----|---------|
| `C-SPC` | set mark |
| `C-w` / `M-w` | kill / copy region |
| `C-y` | yank |
| `C-g` | keyboard quit — cancels the mark and drops extra cursors |
| `C-x h` | mark whole buffer |

Once the mark is set, plain movement extends the region, as in Emacs.

`C-w`, `M-w` and `C-y` use the system clipboard. Fresh has no kill ring, so
`C-k` does not push the killed text onto it — text killed with `C-k` comes back
with undo, not with `C-y`.

## Search and replace

| Key | Command |
|-----|---------|
| `C-s` | search forward (opens the search prompt) |
| `C-s` / `C-r` in the prompt | repeat forward / backward |
| `C-r` | search backward |
| `M-%` | query replace |

## Files, buffers and windows (`C-x`)

| Key | Command |
|-----|---------|
| `C-x C-f` | find file |
| `C-x C-s` | save buffer |
| `C-x s` | save all buffers |
| `C-x C-w` | write file (save as) |
| `C-x k` | kill buffer |
| `C-x b`, `C-x C-b` | switch buffer |
| `C-x <right>` / `C-x <left>` | next / previous buffer |
| `C-x d` | file explorer (dired) |
| `C-x o` | other window |
| `C-x 0` | delete window |
| `C-x 2` / `C-x 3` | split below / right |
| `C-x C-c` | quit |

## Minibuffer, popups and menus

`M-x` opens the command palette. Inside any prompt, `C-a`/`C-e`, `C-f`/`C-b`,
`M-f`/`M-b`, `C-d`, `M-d`, `M-DEL`, `C-k`, `C-w`, `C-y` and `C-g` do what they
do in the Emacs minibuffer, and `C-n`/`C-p` move through the completion list.
`C-n`/`C-p` also move in popups, menus and the file explorer, and `C-g` closes
them.

## LSP

| Key | Command |
|-----|---------|
| `M-.` | go to definition |
| `M-,` | back to where you jumped from |
| `M-TAB` (`C-M-i`) | completion at point |

## Differences from GNU Emacs

- No kill ring — see the note above.
- `C-x` is a prefix, so it is not cut; `C-c` is *not* a prefix and copies.
- `C-q` quits (Fresh convention) rather than `quoted-insert`.
- `M-x` is the command palette, which also does file, buffer and line jumps.
- Emacs has no redo; Fresh's is on `M-_`.

Anything here can be rebound in the [keybinding editor](./keybinding-editor.md)
or in the `keybindings` array of your config.
