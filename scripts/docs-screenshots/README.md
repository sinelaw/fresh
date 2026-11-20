# Docs screenshots

`shoot.py` takes the editor screenshots under `docs/public/images/` from a
real Fresh, so they can be retaken whenever the UI changes instead of
going stale one by one.

```sh
cargo build --release -p fresh-editor
scripts/docs-screenshots/shoot.py                   # every shot
scripts/docs-screenshots/shoot.py settings-editor   # one shot (by id)
scripts/docs-screenshots/shoot.py --list            # ids and the images each writes
DOCSHOT_DISPLAY=:78 scripts/docs-screenshots/shoot.py ...   # run beside another run
```

## How a shot is taken

Every shot runs Fresh inside a real kitty window on a headless X server
(`Xvfb`), with **JetBrainsMono Nerd Font Mono at 24 pt**,
`TERM=xterm-kitty` and `COLORTERM=truecolor`. That is what makes the
images look like a terminal someone actually uses, rather than a
renderer's idea of one: real glyph shaping, Nerd Font icons, true colour.

- Keys go in through `xdotool key`; text through `kitty @ send-text`.
- After each step the script polls `kitty @ get-text --ansi` until the
  screen has not changed for half a second, so a shot never catches a
  half-painted frame.
- The window is captured with `import -window <id>` and cropped to
  columns × rows × cell size (or to a `crop` box, in cells).

Each shot gets its own throwaway `HOME` and `XDG_*` directories, so nothing
on your machine — config, sessions, a running daemon — leaks into an image,
and the shot cannot touch the editor you have open. The base config sets the
`dark` theme, turns on Nerd Font icons and turns off animations and update
checks; a shot can add to it.

## Requirements

```sh
sudo apt-get install -y xvfb kitty xdotool imagemagick x11-utils
```

and the **JetBrainsMono Nerd Font Mono** family
([nerd-fonts releases](https://github.com/ryanoasis/nerd-fonts/releases),
`JetBrainsMono.tar.xz`), installed where fontconfig finds it:

```sh
fc-list | grep "JetBrainsMono Nerd Font Mono"
```

A few shots need more than the editor:

- `lsp-*` start a real Python language server: `npm install -g pyright`.
- `markdown-preview-*` draw Mermaid with `termaid` (`pip install termaid`)
  and math with `utftex` (from [libtexprintf](https://github.com/bartp5/libtexprintf),
  built from source).
- `packages-browser` reads the package registry from github.com.

## Writing a shot

Shots live in `shots/*.py`, one file per area of the docs; each file defines
a `SHOTS` list, and `shots/_common.py` holds step helpers such as
`settings_jump(query)`. A shot is a dict:

| Key | Meaning |
|---|---|
| `id` | name used on the command line |
| `cols`, `rows` | terminal size in cells (default 120 × 34) |
| `project` | directory under `projects/` copied in as the working directory (default `default`); its `setup.sh`, if any, runs inside the copy, with `DOCSHOT_HOME`, `DOCSHOT_CONFIG_DIR` and `DOCSHOT_SHOTS` set |
| `config` | merged into the base user `config.json` |
| `config_files` | extra files to write into the project, e.g. `.fresh/config.json` |
| `args` | arguments to `fresh` |
| `prepare` | shell commands run in the project after its `setup.sh` and before Fresh starts, with `DOCSHOT_HOME` set (for state Fresh only reads at startup) |
| `steps` | what to do, in order |

Steps:

| Step | Does |
|---|---|
| `("key", "ctrl+p", ...)` | press keys (xdotool names: `Return`, `Escape`, `Down`, `ctrl+comma`, `slash`, ...) |
| `("text", "open settings")` | type text |
| `("click", col, row)` | click a cell (0-based) |
| `("rclick_text", "low-stock")` | right-click where this text is on screen |
| `("hover_text", "query-perf")` | put the pointer where this text is, without clicking |
| `("click_text", "[ User ]")` | click where this text is on screen |
| `("shell", "git stash")` | run a shell command in the project directory |
| `("wait", "Settings")` | wait until the text is on screen |
| `("sleep", 1.0)` | wait a fixed time — prefer `wait` |
| `("settle",)` | wait until the screen is still |
| `("crop_frame", "Settings [")` | crop later shots to the box-drawn frame whose top border holds this text |
| `("crop", (col, row, cols, rows))` | crop later shots to this box, in cells |
| `("dump",)` | print the screen as text (for writing a shot) |
| `("shot", "settings-editor")` | write `docs/public/images/settings-editor.png` |

`projects/default` is a small Python + JavaScript invoicing service; its
`setup.sh` commits it to git and leaves a couple of edits uncommitted so the
gutter, the explorer and Review Diff have something to show.
