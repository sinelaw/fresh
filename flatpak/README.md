# Flatpak Packaging for Fresh

This directory contains the Flatpak manifest and related files for building Fresh as a Flatpak package.

## Prerequisites

Install Flatpak and flatpak-builder:

```bash
# Debian/Ubuntu
sudo apt install flatpak flatpak-builder

# Fedora
sudo dnf install flatpak flatpak-builder

# Arch Linux
sudo pacman -S flatpak flatpak-builder
```

Add the Flathub repository and install the runtime:

```bash
flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
flatpak install flathub org.freedesktop.Platform//24.08 org.freedesktop.Sdk//24.08
```

## Building Locally

1. Build the release binary with cargo:

```bash
cargo build --release
```

2. Build and install the Flatpak:

```bash
flatpak-builder --force-clean --user --install build flatpak/io.github.sinelaw.fresh.yml
```

3. Run the installed Flatpak:

```bash
flatpak run io.github.sinelaw.fresh
```

## Files

- `io.github.sinelaw.fresh.yml` - Flatpak manifest
- `io.github.sinelaw.fresh.desktop` - Desktop entry file
- `io.github.sinelaw.fresh.metainfo.xml` - AppStream metadata
- `io.github.sinelaw.fresh.svg` - Application icon
