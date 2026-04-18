# Fresh

A modern, full-featured terminal text editor, **with zero configuration**. Familiar keybindings, mouse support, and IDE-level features — no learning curve required.

[Official Website](https://sinelaw.github.io/fresh/) &nbsp;·&nbsp; [Documentation](https://getfresh.dev/docs) &nbsp;·&nbsp; [Discord](https://discord.gg/gqGh3K4uW3) &nbsp;·&nbsp; [Contributing](#contributing)

**[Quick Install](#installation):** &nbsp; `curl https://raw.githubusercontent.com/sinelaw/fresh/refs/heads/master/scripts/install.sh | sh`

---

![Fresh Demo](docs/fresh-demo2.gif)

Fresh brings the intuitive UX of VS Code and Sublime Text to the terminal. Standard keybindings, full mouse support, menus, and a command palette — everything works the way you'd expect, right out of the box. No modes, no memorizing shortcuts.

Built for real-world performance: Fresh handles [multi-gigabyte files](https://noamlewis.com/blog/2025/12/09/how-fresh-loads-huge-files-fast) with negligible memory overhead and delivers consistently low-latency input, regardless of file size.

### Command Palette & Fuzzy Finder

One shortcut to find files, run commands, switch buffers, and jump to any line.

![Command Palette](docs/blog/productivity/command-palette/showcase.gif)

### Multi-Cursor Editing

Select and edit multiple occurrences simultaneously — the same workflow you know from graphical editors.

![Multi-Cursor](docs/blog/editing/multi-cursor/showcase.gif)

### Themes & Customization

Browse and apply color themes instantly. Full settings UI and interactive keybinding editor included.

![Select Theme](docs/blog/themes/select-theme/showcase.gif)

See more feature demos: [Editing](https://getfresh.dev/docs/blog/editing) (search & replace, block selection, sort lines, ...) · [Productivity](https://getfresh.dev/docs/blog/productivity) (file explorer, split view, integrated terminal, ...) · [Themes](https://getfresh.dev/docs/blog/themes)

---

## Feature Overview

| Category | Features |
|----------|----------|
| **File Management** | open/save/new/close, file explorer, tabs, auto-revert, git file finder |
| **Editing** | undo/redo, multi-cursor, block selection, smart indent, comments, clipboard |
| **Search & Replace** | incremental search, find in selection, query replace, git grep |
| **Navigation** | go to line/bracket, word movement, position history, bookmarks, error navigation |
| **Views & Layout** | split panes, line numbers, line wrap, backgrounds, markdown preview |
| **Language Server (LSP)** | go to definition, references, hover, code actions, rename, diagnostics, autocompletion |
| **Productivity** | command palette, menu bar, keyboard macros, git log, diagnostics panel |
| **Extensibility** | TypeScript plugins (sandboxed QuickJS), color highlighter, TODO highlighter, merge conflicts, path complete, keymaps |
| **Internationalization** | Multiple language support (see [`locales/`](locales/)), plugin translation system |

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
| Gentoo | [GURU](#gentoo-guru) |
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

### Gentoo ([GURU](https://wiki.gentoo.org/wiki/Project:GURU))

Enable the repository as read in [Project:GURU/Information for End Users](https://wiki.gentoo.org/wiki/Project:GURU/Information_for_End_Users) then emerge the package:


```bash
emerge --ask app-editors/fresh
```

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

### Using mise

```bash
mise use github:sinelaw/fresh
```

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

See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## Privacy

Fresh checks for new versions daily to notify you of available upgrades. Alongside this, it sends basic anonymous telemetry (version, OS/architecture, terminal type) to help understand usage patterns. No personal data or file contents are collected.

To disable both upgrade checks and telemetry, use `--no-upgrade-check` or set `check_for_updates: false` in your config.

## License

Copyright (c) Noam Lewis

This project is licensed under the GNU General Public License v2.0 (GPL-2.0).
