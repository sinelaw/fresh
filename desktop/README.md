# Fresh Desktop

Fresh as a standalone desktop application: the editor in its own window, with
the `fresh` binary bundled inside, so it needs no browser and nothing else
installed. Released for Windows (NSIS installer), macOS (universal `.dmg`) and
Linux (`.AppImage`); see the main [README](../README.md#fresh-desktop).

## How it works

Fresh Desktop re-implements nothing. Each window runs one `fresh --web`
session — the same daemon, web bridge and page as the browser UI
(`crates/fresh-editor/src/webui`, `crates/fresh-editor/web-ui`) — and shows
that page in an Electron window:

- **One window, one project.** A window serves one working directory: the
  folder it was opened with, else the launch's current directory (when that
  is a shell's, not the home directory a launcher starts in), else the first
  file's directory, else your home directory. Launching again with another
  folder opens another window; launching with files of an open project sends
  them to its window, over the bridge it serves the page on.
- **Sessions are the terminal edition's.** The session is keyed by the
  directory like `fresh -a`, so workspaces, recovery and hot exit are shared
  with it. Closing the window quits the editor and closes once it has exited:
  unsaved files are asked about in the window as the editor's own quit asks
  (unnamed buffers are kept by hot exit), and cancelling keeps the window
  open. Quitting the editor closes the window.
- **Surviving a crash.** Running sessions are recorded in the app's data
  directory (`sessions.json`). If the app dies under an editor, the next
  window for that directory re-attaches to it instead of starting another.
- **The editor gets your environment.** Started from the Dock, Finder, a
  launcher or the Start menu, the app asks your login shell for its
  environment, so language servers and tools on your `PATH` are found.
- **Keys belong to the editor.** There is no application menu on Windows or
  Linux, and only a minimal one on macOS (with Edit ▸ Paste, which Cmd+V
  needs), so no accelerator takes a chord the editor is bound to.
- **The page is the editor, nothing else.** Navigation away from the local
  page is refused and links open in your browser; the only permission
  granted is the clipboard.
- **Updates** come from installing a newer Fresh Desktop. The bundled editor
  carries an install receipt saying so (`bin/install-receipt.toml`, written
  by `scripts/stage.js`), so its update notice points at the releases page
  rather than `fresh --cmd update`.

The editor's output goes to log files in the app's logs directory
(`~/.config/Fresh Desktop/logs` on Linux, `~/Library/Logs/Fresh Desktop` on
macOS, `%APPDATA%\Fresh Desktop\logs` on Windows; the newest 20 are kept); a
failure to start shows the end of the log.

On Linux the AppImage uses the static AppImage runtime, so it needs no
`libfuse2`. Its launcher (electron-builder's) adds `--no-sandbox` only where
Chromium's sandbox cannot work — when unprivileged user namespaces are
unavailable, as on Ubuntu 24.04+ — since an AppImage cannot carry the setuid
sandbox helper. The paths that launcher adds to `PATH`,
`LD_LIBRARY_PATH` and friends are removed again before the editor starts, so
its terminals see your system's environment, not the AppImage's.

## Building

You need Rust and Node.js. One command, from the repository root, builds
this platform's package into `desktop/dist/`:

```bash
node desktop/scripts/package.js
```

| Platform | Package |
|----------|---------|
| Linux | `fresh-desktop-<version>-linux-<x86_64\|aarch64>.AppImage` |
| macOS | `fresh-desktop-<version>-macos-<x86_64\|arm64>.dmg` |
| Windows | `fresh-desktop-<version>-windows-<x64\|arm64>-setup.exe` |

It builds the editor (`cargo build --release --features web`), installs the
npm dependencies on first use, stages the editor and icons, and runs
electron-builder for this OS and CPU. `--binary path/to/fresh` (or
`FRESH_BIN`) packages an existing build instead of building one, and
`--version` overrides the version stamped on it (default: the workspace's).
`npm run package` in `desktop/` does the same. Nothing is signed with a
developer certificate; macOS is ad-hoc signed, as the releases are. The
release workflow builds a universal macOS dmg instead (see below).

### Step by step

The same steps by hand, for working on the app itself. You need a `fresh`
built with the `web` feature (the release binaries are).

```bash
cargo build --release --features web        # from the repository root
cd desktop
npm ci
node scripts/stage.js                        # or: node scripts/stage.js path/to/fresh
npm start                                    # run it from source
npx electron-builder --publish never         # package for this OS → dist/
```

`scripts/stage.js` copies the binary into `bin/` and the icons into `build/`.
`npm start` runs the newest of the staged binary and the workspace's
`target/release` and `target/debug` builds (in `CARGO_TARGET_DIR`, when that
is set) — never a `fresh` from `PATH`; `FRESH_DESKTOP_BINARY` overrides it.

For a universal macOS build, `lipo` the `x86_64-apple-darwin` and
`aarch64-apple-darwin` binaries into one and stage that.

## Tests

```bash
xvfb-run -a npm run test:e2e        # or plain `npm run test:e2e` on a Linux desktop
```

drives the app from source with Playwright (`test/e2e.mjs`): a second launch
handing its files over, links staying out of the app, closing with and
without unsaved work, closing during startup, quitting from the editor, and
re-attaching to an editor after the app crashed. Each case runs in its own
project and `HOME`. CI runs it on Linux before packaging.

## Smoke test

```bash
FRESH_DESKTOP_SMOKE_TEST=/tmp/shot.png "<the app>" some-file.txt
```

starts the bundled editor, waits for the page to connect and render, types a
key and waits for the frame that answers it, saves a screenshot, quits the
editor cleanly and exits 0, or exits 1 with the reason. CI
(`.github/workflows/desktop-builds.yml`) runs it against every installer
before a release ships it.
