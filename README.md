# Fresh

A terminal-based text editor. [Official Website →](https://sinelaw.github.io/fresh/)

**[📦 Installation Instructions](#installation)**

**[Contributing](#contributing)**

**[Discord](https://discord.gg/qUutBj9t)**

## Why?

Why another text editor? Fresh brings the intuitive, conventional UX of editors like VS Code and Sublime Text to the terminal.

While veterans like Emacs and Vim - and newer editors like Neovim and Helix - are excellent for power users who prefer modal, highly specialized workflows, they often present a steep learning curve for those used to standard GUI interactions. Fresh is built for the developer who wants a familiar, non-modal experience out-of-the-box, without sacrificing the speed and portability of the command line. Keyboard bindings, mouse support, menus, command palette etc. are all designed to be familiar to most modern users.

Architecturally, Fresh is built to handle multi-gigabyte files or slow network streams efficiently, maintaining a negligible memory overhead regardless of file size. While traditional editors struggle with latency and RAM bloat on large files, Fresh delivers consistent, high-speed performance on any scale.

The goal for Fresh is to be an intuitive and accessible, high-performance terminal-based editor that "just works" on any hardware, for everyone.

## Discovery & Ease of Use

Fresh is designed for discovery. It features native UIs, a full Menu system, and a powerful Command Palette. With full mouse support, transitioning from graphical editors is seamless.

## Modern Extensibility

Extend Fresh easily using modern tools. Plugins are written in TypeScript and run securely in a sandboxed Deno environment, providing access to a modern JavaScript ecosystem without compromising stability.

## Low-Latency Performance

Fresh is engineered for speed. It delivers a low-latency experience, with text appearing instantly. The editor is designed to be light and fast, reliably opening and editing [huge files up to multi-gigabyte sizes](https://noamlewis.com/blog/2025/12/09/how-fresh-loads-huge-files-fast) without slowdown.

## Comprehensive Feature Set

- **File Management**: open/save/new/close, file explorer, tabs, auto-revert, git file finder
- **Editing**: undo/redo, multi-cursor, block selection, smart indent, comments, clipboard
- **Search & Replace**: incremental search, find in selection, query replace, git grep
- **Navigation**: go to line/bracket, word movement, position history, bookmarks, error navigation
- **Views & Layout**: split panes, line numbers, line wrap, backgrounds, markdown preview
- **Language Server (LSP)**: go to definition, references, hover, code actions, rename, diagnostics, autocompletion
- **Productivity**: command palette, menu bar, keyboard macros, git log, diagnostics panel
- **Plugins & Extensibility**: TypeScript plugins, color highlighter, TODO highlighter, merge conflicts, path complete, keymaps
- **Internationalization**: Multiple language support (see [`locales/`](locales/) for available languages), plugin translation system

![Fresh Demo](docs/fresh-demo2.gif)
![Fresh Screenshot](docs/public/images/screenshot1.png)
![Fresh Screenshot](docs/public/images/screenshot2.png)
![Fresh Screenshot](docs/public/images/screenshot3.png)

## Installation

Quick install (autodetect best method):

`curl https://raw.githubusercontent.com/sinelaw/fresh/refs/heads/master/scripts/install.sh | sh`

Or, pick your preferred method:

| Platform | Method |
|----------|--------|
| macOS | [brew](#brew) |
| Bazzite/Bluefin/Aurora Linux | [brew](#brew) |
| Windows | [winget](#windows-winget) |
| Arch Linux | [AUR](#arch-linux-aur) |
| Debian/Ubuntu | [.deb](#debianubuntu-deb) |
| Fedora/RHEL | [.rpm](#fedorarhelopensuse-rpm), [Terra](https://terra.fyralabs.com/) |
| FreeBSD | [ports / pkg](https://www.freshports.org/editors/fresh) |
| Linux (any distro) | [AppImage](#appimage), [Flatpak](#flatpak) |
| All platforms | [Pre-built binaries](#pre-built-binaries) |
| npm | [npm / npx](#npm) |
| Rust users (Fast) | [cargo-binstall](#using-cargo-binstall) |
| Rust users | [crates.io](#from-cratesio) |
| Nix | [Nix flakes](#nix-flakes) |
| Developers | [From source](#from-source) |

### Brew

On macOS and some linux distros (Bazzite/Bluefin/Aurora):

> **Note:** On macOS, see [macOS Terminal Tips](https://getfresh.dev/docs/configuration/keyboard#macos-terminal-tips) for recommended terminal configuration.

```bash
brew tap sinelaw/fresh
brew install fresh-editor
```

### Windows (winget)

```bash
winget install fresh-editor
```

Alternatively, Windows users can use [npm](#npm).

### Arch Linux ([AUR](https://aur.archlinux.org/packages/fresh-editor-bin))

**Binary package (recommended, faster install):**

```bash
git clone https://aur.archlinux.org/fresh-editor-bin.git
cd fresh-editor-bin
makepkg --syncdeps --install
```

**Build from source:**

```bash
git clone https://aur.archlinux.org/fresh-editor.git
cd fresh-editor
makepkg --syncdeps --install
```

**Using an AUR helper (such as `yay` or `paru`):**

```bash
# Binary package (recommended, faster install)
yay -S fresh-editor-bin

# Or build from source
yay -S fresh-editor
```

### Debian/Ubuntu (.deb)

Download and install the latest release:

```bash
curl -sL $(curl -s https://api.github.com/repos/sinelaw/fresh/releases/latest | grep "browser_download_url.*_$(dpkg --print-architecture)\.deb" | cut -d '"' -f 4) -o fresh-editor.deb && sudo dpkg -i fresh-editor.deb
```

Or download the `.deb` file manually from the [releases page](https://github.com/sinelaw/fresh/releases).

### Fedora/RHEL/openSUSE (.rpm)

Download and install the latest release:

```bash
curl -sL $(curl -s https://api.github.com/repos/sinelaw/fresh/releases/latest | grep "browser_download_url.*\.$(uname -m)\.rpm" | cut -d '"' -f 4) -o fresh-editor.rpm && sudo rpm -U fresh-editor.rpm
```

Or download the `.rpm` file manually from the [releases page](https://github.com/sinelaw/fresh/releases).

### AppImage

Download the `.AppImage` file from the [releases page](https://github.com/sinelaw/fresh/releases) and run:

```bash
chmod +x fresh-editor-VERSION-x86_64.AppImage
./fresh-editor-VERSION-x86_64.AppImage
```

**For faster startup** (recommended): Extract the AppImage instead of running it directly. This avoids the FUSE mount overhead on each launch (~10x faster):

```bash
./fresh-editor-VERSION-x86_64.AppImage --appimage-extract
mkdir -p ~/.local/share/fresh-editor ~/.local/bin
mv squashfs-root/* ~/.local/share/fresh-editor/
ln -sf ~/.local/share/fresh-editor/usr/bin/fresh ~/.local/bin/fresh
```

Ensure `~/.local/bin` is in your PATH. Available for x86_64 and aarch64 architectures.

### Flatpak

Download the `.flatpak` bundle from the [releases page](https://github.com/sinelaw/fresh/releases) and install:

```bash
flatpak install --user fresh-editor-VERSION-x86_64.flatpak
flatpak run io.github.sinelaw.fresh
```

See [flatpak/README.md](flatpak/README.md) for building from source.

### Pre-built binaries

Download the latest release for your platform from the [releases page](https://github.com/sinelaw/fresh/releases).

### npm

```bash
npm install -g @fresh-editor/fresh-editor
```

Or try it without installing:

```bash
npx @fresh-editor/fresh-editor
```

### Using cargo-binstall

To install the binary directly without compiling (much faster than crates.io):

First, install cargo-binstall if you haven't already

```bash
cargo install cargo-binstall
```

Then install fresh

```bash
cargo binstall fresh-editor
```

### Nix flakes

Run without installing:
```bash
nix run github:sinelaw/fresh
```

Or install to your profile:
```bash
nix profile add github:sinelaw/fresh
```

### From crates.io

```bash
cargo install --locked fresh-editor
```

### From source

```bash
git clone https://github.com/sinelaw/fresh.git
cd fresh
cargo build --release
./target/release/fresh [file]
```

## Documentation

- [User Guide](https://getfresh.dev/docs)
- [macOS Tips](https://getfresh.dev/docs/configuration/keyboard#macos-terminal-tips) - Terminal configuration, keyboard shortcuts, and troubleshooting for Mac users
- [Plugin Development](https://getfresh.dev/docs/plugins/development)

## Contributing

Thanks for contributing!

1. **Reproduce Before Fixing**: Always include a test case that reproduces the bug (fails) without the fix, and passes with the fix. This ensures the issue is verified and prevents future regressions.

2. **E2E Tests for New Flows**: Any new user flow or feature must include an end-to-end (e2e) test. E2E tests send keyboard/mouse events and examines the final rendered output, do not examine internal state.

3. **No timeouts or time-sensitive tests**: Use "semantic waiting" (waiting for specific state changes/events) instead of fixed timers to ensure test stability. Wait indefinitely, don't put timeouts inside tests (cargo nextest will timeout externally).

4. **Test isolation**: Tests should run in parallel. Use the internal clipboard mode in tests to isolate them from the host system and prevent flakiness in CI. Same for other external resources (temp files, etc. should all be isolated between tests, under a per-test temporary workdir).

5. **Required Formatting**: All code must be formatted with `cargo fmt` before submission. PRs that fail formatting checks will not be merged.

6. **Cross-Platform Consistency**: Avoid hard-coding newline or CRLF related logic, consider the buffer mode.

7. **LSP**: Ensure LSP interactions follow the correct lifecycle (e.g., `didOpen` must always precede other requests to avoid server-side errors). Use the appropriate existing helpers for this pattern.

8. **Regenerate plugin types and schemas**: After modifying the plugin API or config types:
   - **TypeScript definitions** (`plugins/lib/fresh.d.ts`): Auto-generated from Rust types with `#[derive(TS)]`. Run: `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored`
   - **JSON schemas** (`plugins/config-schema.json`, `plugins/schemas/theme.schema.json`): Auto-generated from Rust types with `#[derive(JsonSchema)]`. Run: `./scripts/gen_schema.sh`
   - **Package schema** (`plugins/schemas/package.schema.json`): Manually maintained - edit directly when adding new language pack fields

9. **Type check plugins**: Run `crates/fresh-editor/plugins/check-types.sh` (requires `tsc`)

**Tip**: You can use tmux + send-keys + render-pane to script ad-hoc tests on the UI, for example when trying to reproduce an issue.

## Privacy

Fresh checks for new versions daily to notify you of available upgrades. Alongside this, it sends basic anonymous telemetry (version, OS/architecture, terminal type) to help understand usage patterns. No personal data or file contents are collected.

To disable both upgrade checks and telemetry, use `--no-upgrade-check` or set `check_for_updates: false` in your config.

## License

Copyright (c) Noam Lewis

This project is licensed under the GNU General Public License v2.0 (GPL-2.0).
