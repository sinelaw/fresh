# Packaging & Self-Update Paradigm

> Status: **Phases 1–8 landed**. This document specifies a new packaging
> paradigm for `fresh` whose defining property is **deterministic install
> provenance**: every distribution channel records — at install time — exactly
> which mechanism installed the binary, so the editor can self-update through
> the *same* mechanism without ever guessing. It supersedes the path-based
> heuristic in `services/release_checker.rs`.
>
> **Implemented:**
> - **Phase 1** — the `fresh-update` subcrate: `Channel` registry,
>   `install-receipt.toml` format, layered `resolve()` with confidence levels,
>   the update-command registry, checksum verification, the atomic binary swap,
>   and version comparison. `release_checker` delegates to it (the portable
>   check/parse/detect logic now lives in the crate).
> - **Phase 2** — every packaging pipeline writes a receipt (deb, rpm, AUR
>   bin+source, Homebrew, npm, Flatpak, Nix, install.sh AppImage, and the raw
>   release archive). CI asserts the deb/rpm/Flatpak receipts.
> - **Phase 3** — `fresh --cmd update [--check] [--yes] [--allow-downgrade]`
>   (behind the default `self-update` feature): delegate to the package manager, or
>   download → verify SHA-256 → extract → atomic swap for tarball/AppImage.
>   `fresh config paths` prints resolved provenance.
>
> - **Phase 4 (partial)** — interactive in-editor update: the status-bar
>   update indicator is clickable (and an "Update fresh" command exists);
>   clicking prompts "Update now?", and confirming runs the update **locally**
>   in the background (never on the window's remote `Authority`), logging to
>   `<log_dir>/self-update.log`. Gated by the `self_update` config (default on).
>   The **indicator itself relays state** (no transient status line): it shows
>   `Updating…` while the child runs, then `Updated — restart fresh` or
>   `Update failed — click for log` when a watcher thread reaps the process.
>   Clicking the indicator once an update is under way (or the "Open update log"
>   command) opens that log — read from the **local** filesystem via
>   `open_local_file`, so a window bound to a remote authority still shows the
>   right machine's log.
>
> - **Phase 5** — the unified Linux route: the static musl archive is the
>   default `install.sh` install (§7.6), it carries a `tarball` receipt, and
>   every downloaded artifact is checked against the release attestation on
>   `api.github.com` as well as its `.sha256` sidecar (§11).
>
> - **Phase 6** — the path heuristic is **gone**, and with it the `Heuristic`
>   confidence rung (§4.4). Provenance is now recorded or `Unknown`, never
>   inferred from where the binary happens to sit. `cargo` — the one channel
>   that genuinely depended on the guess — detects itself at build time from
>   the registry checkout it is compiled from (§7.5).
>
> - **Phase 7** — `fresh` **executes nothing**. Every channel but the
>   self-contained one prints its command and stops; release packages are still
>   downloaded and verified first (§9, §11). The `sudo` path is deleted.
>
> - **Phase 8** — a release **proves itself before anyone is offered it**. It is
>   assembled as a draft, published as a pre-release, then checked two ways —
>   every archive present/checksummed/attested, and the updater exercised from
>   both ends (the previous release updating to it, and the binary it ships
>   working against the real feed) — and only then promoted to
>   `latest`. Registry publishes hang off the promotion. This is what `-rc` tags
>   used to approximate (§15, Phase 8).
>
> **Not yet done:** full Sigstore chain verification of the attestation (the
> digest is cross-checked at a second origin; the DSSE certificate chain is not
> validated to a pinned root — see §11), and an optional `auto_update`
> (no-prompt) mode. See §15 and §17.

---

## 1. Why this exists

`fresh` already ships through an unusually large number of channels (see the
inventory in §3). What it does **not** have is a reliable way to answer one
question at runtime:

> *"How was **this** copy of `fresh` installed, and therefore how should it be
> updated?"*

Before this design, `services/release_checker.rs` answered that by **inspecting
`current_exe()` and pattern-matching the path** (`/opt/homebrew/…` → Homebrew,
`/.cargo/bin/…` → Cargo, `/usr/bin/…` + Arch → AUR, …). That is a guess, and
it is wrong or blind in many real cases:

| Situation | Heuristic result | Truth |
|---|---|---|
| AUR `fresh-editor-bin` vs `pacman`-official vs a manually-dropped `/usr/bin/fresh` | all → `Aur` | indistinguishable |
| `.deb` vs `.rpm` install | both → `PackageManager` | different upgrade command |
| winget / scoop / chocolatey | `Unknown` | no detection at all |
| Flatpak / snap / AppImage | `Unknown` / `PackageManager` | no detection at all |
| Homebrew with a custom `--prefix` | `Unknown` | should be `Homebrew` |
| Cargo with `CARGO_HOME` relocated | `Unknown` | should be `Cargo` |
| npm via `pnpm` / `volta` / `nvm` shims | often `Unknown` | should be `npm` |
| FreeBSD `pkg` | `PackageManager` | should be `pkg` |

And even when the guess is *right*, the checker only **prints a command** — it
never actually updates anything (`install-updater = false` in
`dist-workspace.toml`).

The user requirement driving this design is explicit: the editor must **know
for sure** which mechanism was used (AUR vs winget vs brew vs …) and self-update
via that same mechanism, on Linux, Windows, and macOS.

The only way to *know for sure* is to stop inferring and start **recording**.

---

## 2. Design goals & non-goals

**Goals**

1. **Deterministic provenance.** Each channel writes an authoritative record of
   how it installed `fresh`. Runtime reads the record; it does not guess.
2. **Same-mechanism updates.** Updating always uses the channel that installed:
   AUR → AUR helper, winget → `winget upgrade`, brew → `brew upgrade`, a raw
   tarball → in-place binary swap.
3. **Do no harm to package managers.** For OS/package-manager-owned installs the
   editor must **never** overwrite its own binary behind the manager's back
   (that corrupts the package DB, breaks signatures, and fights the next
   `apt upgrade`). It delegates instead.
4. **Real self-update where we own the bits.** For channels with no external
   manager (raw tarball, `install.sh` AppImage, direct download) the editor
   performs a verified, atomic, in-place update.
5. **All three platforms**, with per-OS specifics handled explicitly.
6. **Security first.** Every downloaded artifact is verified (SHA-256 +
   GitHub build attestation) before it is trusted.
7. **Privacy preserved.** Reuse the existing daily-debounced, opt-out check
   (`--no-upgrade-check`, `check_for_updates`); introduce no new phone-home.
8. **Honest degradation.** If nothing recorded the install, say so and route to
   the releases page. Do not guess from the executable path — an unknown
   install is a better answer than a plausible wrong one, and it never triggers
   a destructive action.

**Non-goals**

- Replacing package managers. We integrate with them, we do not reinvent them.
- Silent/background auto-installation of updates. Updating is always initiated
  by the user (a command or a confirmed prompt). We only *notify* in the
  background, exactly as today.
- Delta/binary-patch updates. Full-artifact replacement only, for now.

---

## 3. Current distribution channels (inventory)

`fresh` is a single self-contained Rust binary (`fresh`, crate `fresh-editor`,
`embed-plugins` bakes plugins + themes in). The release pipeline
(`.github/workflows/release.yml`, triggered on `v*` tags) produces one set of
per-target prebuilt archives and then feeds every downstream channel:

| # | Channel | Canonical id | Artifact / mechanism | Who owns updates |
|---|---|---|---|---|
| 1 | Homebrew (tap `sinelaw/homebrew-fresh`) | `homebrew` | `.tar.xz` via formula | brew |
| 2 | npm `@fresh-editor/fresh-editor` | `npm` | wrapper downloads archive | npm |
| 3 | crates.io | `cargo` | source build | cargo |
| 4 | cargo-binstall | `cargo-binstall` | prebuilt archive | cargo-binstall |
| 5 | AUR `fresh-editor-bin` | `aur-bin` | prebuilt `.tar.xz` | AUR helper |
| 6 | AUR `fresh-editor` (source) | `aur` | source build | AUR helper |
| 7 | Debian/Ubuntu `.deb` | `apt` | dpkg | apt/dpkg |
| 8 | Fedora/RHEL `.rpm` | `dnf` | rpm | dnf/rpm |
| 9 | openSUSE | `zypper` | the `.rpm`, installed by hand | *(no repo — see §6)* |
| 10 | Flatpak `io.github.sinelaw.fresh` | `flatpak` | flatpak bundle | flatpak |
| 11 | AppImage (`install.sh` / direct) | `appimage` | extracted to `~/.local` | **fresh (self)** |
| 12 | winget `sinelaw.fresh-editor` | `winget` | `.zip` | winget |
| 13 | Scoop *(planned)* | `scoop` | `.zip` | scoop |
| 14 | Chocolatey *(planned)* | `chocolatey` | `.zip` | choco |
| 15 | Nix flake | `nix` | source build | nix |
| 16 | FreeBSD pkg/ports | `freebsd-pkg` | pkg | pkg |
| 17 | mise (`github:sinelaw/fresh`) | `mise` | prebuilt archive | mise |
| 18 | Raw GitHub release archive | `tarball` | manual download | **fresh (self)** |
| 19 | Built from a git checkout | `source` | `cargo build` | developer |

The pipeline already emits per-archive `.sha256` files and enables **GitHub
artifact attestations** (`github-attestations = true`), plus npm OIDC
provenance — the raw materials for secure self-update already exist.

---

## 4. The provenance model

Provenance is resolved at runtime by a new module,
`services::provenance`, which returns a `Provenance { channel, confidence,
hints }`. It is computed from three sources, in strict precedence order.

### 4.1 Layer A — runtime override (highest precedence)

`FRESH_INSTALL_CHANNEL` env var, if set, wins. This is the escape hatch:
testing, CI, packagers overriding a wrong receipt, and power users. Confidence
= `Overridden`.

### 4.2 Layer B — install receipt (the authoritative layer)

A small file, **`install-receipt.toml`**, written *at install time* by whatever
performed the install. Two flavours, both authoritative:

- **Packaged receipt** — for OS/package-manager channels, the receipt is *a
  file inside the package* (`.deb`, `.rpm`, AUR, flatpak, winget, scoop, choco,
  nix, pkg). Because the package manager lays it down and removes it on
  uninstall, it is tamper-evident and always consistent with what is installed.
  It is read-only to the running editor — the editor can never accidentally
  "upgrade" a packaged receipt into claiming self-update capability.

- **Sidecar receipt** — for wrapper/manual channels that unpack the shared
  prebuilt archive (npm, homebrew-bin, cargo-binstall, AppImage, `install.sh`,
  raw tarball), the installer script writes the receipt next to the binary or
  into the per-user data dir.

Search order for the receipt (first hit wins):

```
1. <dir(exe)>/install-receipt.toml                        # sidecar, same dir
2. <dir(exe)>/../share/fresh/install-receipt.toml         # FHS, binary name
3. <dir(exe)>/../share/fresh-editor/install-receipt.toml  # FHS, package name
4. <dir(exe)>/../lib/fresh/install-receipt.toml           # npm/node layout
5. $XDG_DATA_HOME/fresh/install-receipt.toml    (Linux)   # per-user fallback
   ~/Library/Application Support/fresh/…        (macOS)
   %LOCALAPPDATA%\fresh\…                        (Windows)
```

Candidates 2 and 3 differ only in directory name, and both are required
because the binary and the package are not named the same thing. The binary is
`fresh`; the package is `fresh-editor`. OS packages put their data under
`/usr/share/<package>`, so `.deb` (`debian/rules`) and `.rpm`
(`[package.metadata.generate-rpm]`) ship `/usr/share/fresh-editor/`, while the
wrapper channels that install under a prefix they control — the Homebrew
formula, the Flatpak manifest, the Nix derivation — use `share/fresh/`.

Both spellings are already installed on real machines, so the resolver searches
both rather than picking a winner: renaming either side would strand every copy
installed before the change. Note this is a *search* path, not a write path —
installers must keep writing the one location their packaging convention
dictates, and never both.

Confidence = `Authoritative`.

### 4.3 Layer C — compile-time channel

`build.rs` embeds `FRESH_BUILD_CHANNEL` (from the build env var of the same
name) and the target triple. Source-building channels that own their build set
it to their id: crates.io → `cargo`, AUR-source → `aur`, Nix → `nix`, the
`.deb`/`.rpm` builders → `apt`/`dnf`, FreeBSD ports → `freebsd-pkg`. When unset
(the default for the shared prebuilt archive and for developer builds) it is
`prebuilt` (release CI) or `source` (dirty git tree), which tells the resolver
"trust the receipt, and if there is none you're a bare tarball."

Confidence = `Embedded`.

### 4.4 There is no Layer D

There used to be one: `detect_install_method_from_path`, kept but demoted —
`~/.cargo/bin` meant cargo, `/opt/homebrew` meant brew, `/usr/bin` on Arch
meant the AUR. It has been **removed**, and with it the `Heuristic` confidence
rung.

The layers above all record something at the moment it is true: the installer
writes the receipt as it installs, the build stamps the channel as it builds. A
path is read long afterwards and describes only where a file currently sits.
Copy the binary somewhere else and the guess changes although the install did
not; the guess could never separate apt from dnf from a file someone dropped in
`/usr/bin`; and a *nearly* right answer is worse than no answer, because it
produces a confident sentence about an update route that may not exist.

Removing it costs nothing real, because the one channel that depended on it now
records itself. See §7.6 for `cargo`.

An install that recorded nothing resolves to `Unknown`, which routes to the
releases page and says so. That is worse UX than a lucky guess and better
behaviour than a confident wrong one: the failure is visible instead of being a
command that quietly updates nothing.

### 4.5 Resolution & confidence

```
Overridden  > Authoritative > Embedded > Unknown
```

Every rung except `Unknown` is something that was recorded. `confidence` gates
behaviour:

| Confidence | Notify of update? | Show exact command? | Auto self-swap allowed? |
|---|---|---|---|
| Overridden / Authoritative | yes | yes | yes, if channel is self-managed |
| Embedded | yes | yes | yes, if channel is self-managed |
| Unknown | yes | generic (link to releases) | no |

---

## 5. The install-receipt format

```toml
# install-receipt.toml — written by the installer, read by `fresh` to self-update.
# This file is provenance metadata only. Do not edit unless you know what you're doing.
schema        = 1
channel       = "homebrew"           # canonical channel id (§3)
version       = "0.4.4"              # version this receipt was written for
package_name  = "fresh-editor"
installed_at  = "2026-07-23T10:30:00Z"
managed       = true                 # true => an external manager owns updates
self_update   = false                # true => `fresh update` may swap the binary in place

# Optional, channel-specific hints used to build the exact update invocation:
[hints]
tap          = "sinelaw/homebrew-fresh"   # homebrew
formula      = "fresh-editor"             # homebrew
aur_pkg      = "fresh-editor-bin"         # aur-bin
winget_id    = "sinelaw.fresh-editor"     # winget
flatpak_ref  = "io.github.sinelaw.fresh"  # flatpak
npm_pkg      = "@fresh-editor/fresh-editor"
target       = "x86_64-unknown-linux-gnu" # self-update: which asset to fetch
asset        = "fresh-editor-x86_64-unknown-linux-gnu.tar.xz"
install_root = "/home/u/.local/share/fresh-editor"  # appimage/tarball: where to swap
```

`managed` and `self_update` are the two decision bits. Everything else is
either provenance (`channel`, `version`, `installed_at`) or a fact the update
path would otherwise have to re-derive.

That last category is not an optimisation — it is the point. Anything the
*installer* knows for certain and the *running binary* could only infer belongs
in `[hints]`. `pkg_arch` is the clearest case: the `.deb` build knows it
produced `amd64`, while the running binary would have to map its own target
triple through a per-tool arch-spelling table and hope the two agree. The deb
and rpm pipelines record `target` and `pkg_arch` for exactly this reason, and
`registry::package_asset_with` prefers the recorded value outright.

The exception is anything that is a property of the *machine* rather than the
install, and can change after it: which AUR helper is on `PATH`, whether `sudo`
exists. Those are detected at runtime (`release_checker::fill_runtime_hints`)
and must not be baked into a receipt, or the receipt goes stale the first time
the user switches helper.

---

## 6. Channel registry — the single source of truth

A static table (`services::provenance::registry`) maps every `channel` id to
its update strategy. This is the one place that encodes "how do we update X".

```rust
pub enum UpdateStrategy {
    /// External manager owns it. We run (or print) its command; never swap.
    Delegated { command: &'static [&'static str], needs_privilege: bool },
    /// We own the bits: download the release asset, verify, swap in place.
    SelfContained,
    /// User-scoped toolchain manager (cargo/npm/mise): delegate, no sudo.
    Toolchain { command: &'static [&'static str] },
    /// A package manager owns the files but no repository serves them: fetch
    /// the release artifact and hand it to the local package tool.
    DownloadPackage,
    /// Unknown provenance: link to the releases page only.
    Manual,
}
```

`DownloadPackage` exists because a channel being *packaged* does not mean it is
*hosted*. We publish the `.deb`, `.rpm` and `.flatpak` as GitHub release
artifacts; there is no apt repo, no dnf repo and no Flathub remote, so
`apt-get install --only-upgrade` / `flatpak update` have nothing to upgrade
from and report "already up to date" forever. These channels download the new
artifact from the release, verify it against its `.sha256` sidecar, and install
it with `dpkg`/`rpm`/`flatpak`. An in-place binary swap is never an option for
them: it would leave the package database describing a file we replaced behind
its back. When the install needs root we print the command instead of running
it, and report `ActionRequired` so the editor's indicator doesn't claim an
update that hasn't landed yet (see the three-phase table in §15).

Two invariants govern this table.

**Never dead-end at the releases page.** A user who installed through one of our
channels must be able to keep updating through it. Telling them to go and
download a file by hand is the exact failure this mechanism exists to remove, so
every channel anything can resolve to names a route to the next version.
`registry.rs`'s `every_reachable_channel_has_a_same_channel_continuation` holds
this: only `unknown` — the honest "we have no idea" — is exempt. That is why
`zypper` and `pacman` are not `Manual` despite having no repository: an openSUSE
user installed the release `.rpm`, so the next one arrives the same way, and an
Arch user came via the AUR, so that is where they go back to. Only snap, scoop
and chocolatey remain `Manual`, and only because we publish nothing they could
install *and* nothing writes their receipts — they are unreachable, not
dead-ended.

**One mechanism per provenance class.** A proved provenance must imply exactly
one way to update — no alternates chosen at runtime from what happens to be on
the machine. Every such fallback was removed: the AUR helper chain (above),
`sudo`/`doas`/run-unprivileged for privileged installs (now `sudo`, full stop),
and the recorded-asset-name-else-look-it-up branches in the download paths (now
the release feed for packages, the compiled-in target triple for archives). The
`aur_helper` and `asset` hints survive in the receipt for wire compatibility but
are no longer read.

The cost is deliberate: a system with no `sudo` at all — a minimal container
running as root — now fails with `failed to run sudo` instead of quietly
installing by a different route. That is the intended trade. A fallback that
succeeds by doing something other than what the provenance says is precisely the
guessing this design exists to remove, and when it does fail it fails in a way
that looks like a packaging bug rather than a missing `sudo`.

**Never name a tool the channel doesn't guarantee.** For most channels the tool
*is* the channel: a `homebrew` receipt means `brew` exists, an `npm` receipt
means `npm` does. The AUR is the exception — it implies `pacman` and `makepkg`,
not any particular helper — and the registry used to default to `yay`, naming a
binary that is simply absent on the many Arch systems that build with plain
`makepkg`, with the failure arriving only after the user had confirmed. The
helper is now **detected** at runtime (`release_checker::fill_runtime_hints`,
mirroring `install.sh`'s list and order) and recorded in `hints.aur_helper`;
with none found, `registry::aur_command` falls back to the route that works
everywhere: clone the AUR repo and `makepkg --syncdeps --install`. Detection
happens before the plan is built, so the command shown in the popup is the one
that runs.

| channel | strategy | update invocation (templated with `hints`) |
|---|---|---|
| `homebrew` | Delegated | `brew upgrade {formula}` |
| `apt` | DownloadPackage (root) | fetch `.deb` from the release, verify, `dpkg -i` |
| `dnf` | DownloadPackage (root) | fetch `.rpm` from the release, verify, `rpm -U` |
| `zypper` | DownloadPackage (root) | fetch `.rpm` from the release, verify, `zypper install` |
| `aur-bin` / `aur` | Delegated | `git clone` + `makepkg --syncdeps --install` |
| `pacman` | Delegated | AUR: `git clone` + `makepkg --syncdeps --install` |
| `winget` | Delegated | `winget upgrade --id {winget_id}` |
| `scoop` | **Manual** | unreachable: no scoop manifest, nothing writes this receipt |
| `chocolatey` | **Manual** | unreachable: no package, nothing writes this receipt |
| `flatpak` | DownloadPackage | fetch `.flatpak` bundle, verify, `flatpak install --user` |
| `snap` | **Manual** | unreachable: no snap published, nothing writes this receipt |
| `nix` | Delegated | `nix profile upgrade fresh` (matches `flake.nix`'s `pname`) |
| `freebsd-pkg` | Delegated (root) | `pkg upgrade fresh` |
| `cargo` | Toolchain | `cargo install --locked fresh-editor` |
| `cargo-binstall` | Toolchain | `cargo binstall fresh-editor` |
| `npm` | Toolchain | `npm update -g {npm_pkg}` |
| `mise` | Toolchain | `mise upgrade github:sinelaw/fresh` (the tool ref, not `fresh`) |
| `appimage` | SelfContained | fetch `.AppImage`, verify, replace file |
| `tarball` | SelfContained | fetch archive, verify, atomic binary swap |
| `source` | Manual | `git pull && cargo install --path …` (note) |
| `unknown` | Manual | open releases page |

---

## 7. How each channel stamps its receipt

The heart of the paradigm: teach every packaging pipeline to emit the receipt.
Concrete, per-channel changes:

### 7.1 Compile-time (source builds) — `build.rs`
Extend the existing `build.rs` (which already embeds `FRESH_GIT_HASH`) to emit:

```rust
let channel = std::env::var("FRESH_BUILD_CHANNEL").unwrap_or_else(|_| {
    if option_env!("FRESH_GIT_HASH").is_some() { "source" } else { "prebuilt" }.into()
});
println!("cargo::rustc-env=FRESH_BUILD_CHANNEL={channel}");
println!("cargo::rustc-env=FRESH_TARGET_TRIPLE={}", std::env::var("TARGET").unwrap());
println!("cargo::rerun-if-env-changed=FRESH_BUILD_CHANNEL");
```

- **crates.io / cargo-publish.yml**: set `FRESH_BUILD_CHANNEL=cargo`.
- **Nix (`flake.nix`)**: set it to `nix` in the build derivation env; also drop
  a packaged receipt in `$out/share/fresh/`.
- **AUR source PKGBUILD**: export `FRESH_BUILD_CHANNEL=aur` in `build()`, and
  `install -Dm644` a receipt with `channel=aur`.
- **`.deb` (`debian/rules`)** and **`.rpm` (`generate-rpm` assets)**: build with
  the env var and ship `/usr/share/fresh-editor/install-receipt.toml`
  (`channel=apt`/`dnf`, `managed=true`, `self_update=false`). Added to
  `fresh-editor.install` and the rpm asset list.
- **FreeBSD**: `freebsd-pkg` receipt shipped in the pkg plist.

### 7.2 Prebuilt-archive wrappers (write a sidecar receipt)
These consume the shared `fresh-editor-<triple>.tar.xz`. The receipt is written
by the wrapper's own install step, so it is authoritative for that wrapper:

- **npm** (`npm-package/binary-install.js`): after extracting the binary, write
  `install-receipt.toml` (`channel=npm`, `managed=true`) beside `run-fresh.js`.
- **Homebrew formula** (generated in `release.yml`): in `def install`, write a
  receipt into `share/fresh/` with `channel=homebrew` + `tap`/`formula` hints.
- **cargo-binstall**: `[package.metadata.binstall]` `pkg-fmt`/hooks — ship the
  receipt inside the archive under a known relative path, marked `cargo-binstall`.
- **AUR `fresh-editor-bin` PKGBUILD**: `install -Dm644` receipt
  (`channel=aur-bin`, `managed=true`, `aur_pkg=fresh-editor-bin`).
- **Flatpak manifest**: `install -Dm644` receipt into `/app/share/fresh/`
  (`channel=flatpak`, `flatpak_ref=…`).
- **winget / scoop / chocolatey manifests**: the installer drops a receipt file
  alongside `fresh.exe` (`channel=winget|scoop|chocolatey`).

### 7.3 The prebuilt archive itself carries a **self-update** receipt
The release build embeds a default receipt in the `.tar.xz`/`.zip` with
`channel=tarball`, `managed=false`, `self_update=true`, plus `target`/`asset`
hints. Any wrapper that repackages the archive **overwrites** this receipt with
its own (so npm's receipt wins over the tarball's). But a user who just
downloads and extracts the archive — or `install.sh`'s AppImage path — keeps
the `tarball`/`appimage` receipt and gets genuine self-update.

### 7.4 `install.sh`
The universal installer already knows exactly which branch it took. Each branch
writes the matching receipt (or relies on the package's own packaged receipt).
Its AppImage branch writes `channel=appimage`, `self_update=true`,
`install_root=~/.local/share/fresh-editor`.

#### Desktop integration (tarball branch only)
The musl archive carries `fresh.desktop` and a `hicolor` icon tree — the same
files `debian/fresh-editor.install` puts in `/usr/share`. Unpacked under
`INSTALL_DIR` they are inert, because no XDG lookup consults that path, so the
tarball branch copies them into `$XDG_DATA_HOME/{applications,icons/hicolor}`
after the move. Three details are load-bearing:

- **`Exec=` is rewritten** to the absolute `$BIN_DIR/fresh`. The shipped entry
  says `Exec=fresh`, correct for a package that lands in `/usr/bin`; a desktop
  environment launching the entry frequently does not have `~/.local/bin` on
  `PATH`.
- **A manifest** (`$INSTALL_DIR/installed-files.txt`) records every path written
  outside `INSTALL_DIR`. Everything else the installer creates is one directory
  plus one symlink; without the manifest these files could not be removed
  without guessing. The next run prunes what the previous one recorded, and
  refuses to act on any line that is not one of the two shapes it writes.
- **Nothing here fails the install.** Missing `$HOME`, an archive built without
  the assets, an unwritable data dir, absent `update-desktop-database` /
  `gtk-update-icon-cache`: all warn at most. `--no-desktop-integration` /
  `FRESH_NO_DESKTOP=1` skips the whole step (still pruning the previous run's
  files), which is the right default for servers and containers.

Self-update swaps the binary alone (§8), so these files are refreshed by
re-running `install.sh`, not by `fresh --cmd update`. That is why `README.md` is
*not* in the archive: it would freeze at the first-installed version. `LICENSE`
stays, because the archive redistributes a binary.

The AppImage branch deliberately does none of this — its assets live inside the
extracted AppDir and are its own concern.

### 7.5 Known limitations
- **winget / scoop / chocolatey.** These consume the same Windows `.zip` as a
  raw download. winget-pkgs zip installers can't run a post-extract hook, so a
  winget install currently inherits the archive's generic `tarball` receipt
  (i.e. it would self-update rather than defer to `winget upgrade`). Marking it
  `managed` needs a wrapper installer type; tracked for later. Scoop/Chocolatey
  manifests *can* write a receipt, but those channels don't ship yet.
- **cargo (crates.io).** A user's `cargo install` build doesn't see any
  build-time env we set, so nothing can hand it `channel=cargo` and no receipt
  is written. It used to lean on the path heuristic (`~/.cargo/bin`). It now
  detects itself instead: `build.rs` stamps `FRESH_BUILD_CHANNEL=cargo` when
  the source tree carries both markers cargo writes when it unpacks a `.crate`
  — `.cargo-ok` (written after extraction, never shipped inside the tarball)
  and `Cargo.toml.orig` (written by `cargo package`, so absent from a git
  checkout). Deliberately not the `registry/src` path shape: that is the same
  guessing this design removed elsewhere, and it would also accept a
  `cargo install --git`. An explicit env var always wins, so
  packagers are never second-guessed.

  This is a fact rather than a guess, and the difference matters: it is about
  where the *source* came from, established at the only moment it is knowable,
  and moving or copying the resulting binary cannot invalidate it. A workspace
  build, a `--path` install and a git checkout are all correctly not registry
  checkouts. The predicate lives in `src/registry_checkout.rs` (which `build.rs`
  `include!`s) rather than inside the build script, because `#[cfg(test)]`
  modules in a build script are never run by `cargo test` — so keeping it there
  would have meant tests that only looked like coverage.

### 7.6 The unified Linux route

Nineteen channels is a lot of ways to answer "how do I install this", and all
but two of them hand the update lifecycle to something else. The default
`install.sh` route is therefore the one channel that keeps it: the **static
musl archive**, unpacked under `~/.local`, owned by the user, needing no root
and self-updating in place.

Three things make that work, and all three had to be fixed together:

1. **One artifact that runs everywhere.** `musl-builds.yml` already produced
   fully-featured static binaries for `x86_64` and `aarch64` (QuickJS and all).
   Static linking is what sidesteps the glibc-version problem that makes a
   single portable binary hard, and it is the same property that makes `fresh`
   awkward for distro packaging in the first place — so leaning into it rather
   than fighting it is the cheaper direction.
2. **A receipt, unconditionally.** The musl archive shipped without one, so a
   musl install resolved at `Unknown` confidence and `can_self_update`
   refused the swap (§4.5). The single most-portable artifact was the one
   artifact that could not update itself. It now carries the same
   `channel=tarball` receipt the dist archives do, and CI asserts it.

   `install.sh` does not *depend* on that, though. It knows exactly what it
   just did, so if the archive has no receipt — every archive published before
   this landed — it writes one itself, exactly as the AppImage branch always
   has. The build-time receipt wins when present, because it carries the
   version and timestamp the installer would have to invent. The result is that
   provenance for this channel is recorded rather than inferred *today*, not
   from the next release onward.
3. **gzip, not xz — the most portable compression wins.** The dist archives
   are `.tar.xz`, and matching them would have kept the engine's single
   extraction path. But this is the archive `install.sh` unpacks, and
   `install.sh` shells out to the *system* `tar`: `.tar.xz` needs the `xz`
   binary (`xz-utils`), which minimal images and busybox routinely lack, while
   gzip is universal. An artifact whose entire claim is "runs anywhere" cannot
   require a package to unpack.

   Nothing is lost on the update side, because **fresh decompresses in its own
   process** — `archive::from_tar_gz`, no subprocess — so the host needs no
   archiver at all there. If anything gzip is cheaper: flate2's default backend
   is pure Rust, whereas `xz2` links liblzma (`ldd` on a released glibc build
   shows `liblzma.so.5`). The cost is archive size: 13.2 MB gzip against 9.9 MB
   xz for the same x86_64 build, so about a third larger — ~3 MB, paid once per
   install, to drop a dependency the install path cannot assume.

   For one release cycle the musl job publishes **both** `.tar.gz` and
   `.tar.xz`. Binaries built before `archive_ext` existed ask for the xz name
   unconditionally, and that became reachable the moment `install.sh` started
   writing a receipt for archives that lack one: those installs previously had
   no receipt and the updater declined to act, so "no update offered" turned
   into "update offered" for a binary requesting a name that would not exist.
   Shipping both makes that request resolve. Droppable once no published
   binary asks for the xz name.

   `engine::archive_ext` maps the compile-time target triple to its extension
   (windows → `zip`, musl → `tar.gz`, else `tar.xz`). That stays a fact derived
   from a compile-time value rather than a lookup, so §6's no-guessing rule
   holds; a test pins every published target, since a mismatch here is a 404 at
   update time on a path nobody exercises until a user tries to update.

Distro packages remain fully supported and are **chosen, not defaulted to**:
`install.sh --method=deb|rpm|aur|nix|cargo|npm|brew|appimage`, or
`FRESH_INSTALL_METHOD`. When auto-selection picks the universal build on a
system that has a native option, it says so and names the flag. The reasoning
is not that packages are worse — it is that a package install needs root and
moves updates to that package manager, which is a decision worth making
deliberately rather than inheriting from whatever distro the script detected.

`install.sh` also verifies every artifact it downloads against the published
`.sha256` (it previously verified nothing) and stages anything destined for
`dpkg`/`rpm` under `sudo` in a `mktemp -d` 0700 directory, so there is no
window in which another local user can swap a payload between the checksum
check and the privileged install.

---

## 8. The self-update engine (SelfContained channels)

Only `tarball` and `appimage` (and future direct-download channels) ever swap
their own binary. Flow implemented in `services::self_update`:

1. **Resolve** provenance; assert `strategy == SelfContained` and
   `confidence >= Embedded`.
2. **Check** latest version via the existing `release_checker`
   (GitHub releases API, daily-debounced logic reused).
3. **Select asset** for `FRESH_TARGET_TRIPLE` using the `asset` hint
   (`fresh-editor-<triple>.{tar.xz,tar.gz,zip}` per `archive_ext`, or the
   `.AppImage`).
4. **Download** to a temp file *on the same filesystem* as the target
   (so the final rename is atomic), via `services::http::download_to_file`.
5. **Verify — mandatory, fail-closed, at two origins:**
   - fetch the `<asset>.sha256` from the release CDN and compare;
   - confirm the artifact's digest is attested under this asset name in the
     release attestation on `api.github.com` (§11). Either failure aborts the
     update and leaves the current binary untouched.
6. **Swap atomically:**
   - *Unix (tarball):* unpack, `chmod +x`, `rename()` the new binary over the
     old one (atomic on the same fs). If `EXDEV`/permission denied → this
     wasn't really a self-owned install; abort with guidance.
   - *AppImage:* replace the single `.AppImage` file (or the extracted
     `install_root`) the same way, preserving the `~/.local/bin/fresh` symlink.
   - *Windows:* a running `.exe` can't be deleted but *can* be renamed. Rename
     `fresh.exe → fresh.exe.old`, move the new exe into place, and schedule the
     `.old` for deletion on next launch (the `self-replace` crate encapsulates
     this; documented as the accepted approach).
7. **Finalise:** print the new version and offer to re-exec (`fresh` restarts
   into the new binary) or exit for the user to relaunch.

Rollback: because the swap is a single atomic rename after full verification, a
crash before the rename leaves the old binary; a crash after leaves the new one.
The `.old` file (Windows) / a kept backup (opt-in) allows manual revert.

---

## 9. Delegated & toolchain updates

For every non-SelfContained channel, `fresh update`:

1. Confirms an update exists.
2. Builds the exact command from the registry + `hints`.
3. **Prints it, and stops** — returning `ActionRequired`. For a release package
   (`apt`/`dnf`/`zypper`) it first downloads and verifies the artifact, so the
   printed command names a file already checked against its checksum and its
   attestation.
4. Never touches the binary directly — the manager does, keeping its package DB
   and signatures intact.

**Nothing is executed on the user's behalf.** Not `brew upgrade`, not
`winget upgrade`, not `cargo install`, and above all not `sudo dpkg -i`. This
replaces the earlier graded scheme, where user-scoped managers were run
outright, privileged ones were run under `sudo` in the update terminal, and
which of those happened depended on the channel, on `--yes`, on whether the
endpoint was trusted and on whether the tool needed root.

Two reasons to collapse it. The first is that spawning a package installer as
root was the most dangerous thing in the updater, and the entire
privilege-escalation surface disappears with the call rather than being
defended. The second is comprehensibility: a rule with no exceptions is one
nobody has to hold in their head, and "fresh only writes files it owns" is that
rule. `fresh` is not a package manager and should not act like a front-end for
five of them.

What it deliberately does **not** collapse is the download for repo-less
package channels. There is no apt or dnf repository serving these; a user told
only "go install the .deb" downloads it with no verification at all, whereas we
fetch it, checksum it and attestation-check it. Stopping before `dpkg` costs a
keystroke. Stopping before the download would cost them their only verification.

The peer evidence points the same way: herdr's docs say `herdr update` is for
its own installer and "Homebrew, mise, and Nix installs are updated through
those package managers instead"; Zed ships `ZED_UPDATE_EXPLANATION` so packagers
can replace the updater with a message; hunk has no updater at all. The
outlier is opencode, which shells out to whichever of seven managers it can
detect — and which also resolves provenance by pattern-matching the executable
path, the design §4.4 removed.

AUR is the one channel whose identity does *not* imply a tool: it means
`pacman` + `makepkg`, not any particular helper. It uses exactly one command —
`git clone` + `makepkg --syncdeps --install` — on every machine. Preferring
`yay`/`paru` when present was tried and removed: it made the upgrade mechanism a
property of the machine rather than of the proved provenance, so two users with
identical receipts would update by different routes and neither route could be
predicted from the receipt. It is not separately elevated: `makepkg -si` invokes
`sudo` itself for the `pacman` step, and wrapping it would nest prompts.

---

## 10. CLI & UX surface

New user-facing surface, consistent with the existing clap `--cmd` subcommand
convention (`daemon`, `config`, `grammar`, `init`):

- `fresh --cmd update` — check + update via the resolved strategy (prompts
  before doing anything).
- `fresh --cmd update --check` — report status only (current, latest, channel,
  what the update command would be); exit non-zero if outdated. Scriptable.
- `fresh --cmd update --yes` — non-interactive (CI, dotfiles).
- `fresh --cmd config paths` — extend to print resolved provenance + receipt
  path (for debugging "why does it think I installed via X").

Background notification (unchanged in spirit): the daily check still surfaces
the status-bar `Update: vX.Y.Z` indicator. Clicking / `Ctrl+P → "Update fresh"`
runs the same flow. For `Unknown` confidence the notification links
to instructions rather than offering a one-key update.

Config (`config.rs`): keep `check_for_updates` and `--no-upgrade-check`. Add
`editor.auto_update = false` (default) — reserved for a future opt-in that lets
SelfContained installs update without a prompt; **off by default** per the
non-goal on silent installs.

---

## 11. Security

- **Transport:** HTTPS only, via the existing `ureq + rustls` stack in
  `services::http`.
- **Integrity: two origins, not one.** A `.sha256` sidecar on its own proves
  very little, because it is served from the same place as the artifact —
  whoever can substitute the payload can substitute the digest beside it, and
  the comparison then only shows the server can do arithmetic. So every
  downloaded artifact is checked twice: against its sidecar, and against the
  **release attestation fetched from `api.github.com`**, which is a separate
  pinned origin. The attestation is the in-toto statement the release workflow
  produced (`github-attestations = true`); GitHub indexes it by subject digest,
  so a tampered artifact has no attestation to find, and a genuine artifact
  swapped in under another name fails the name check. Both fail-closed.
  `fresh_update::attestation` implements it, with the real GitHub payload
  checked into `tests/fixtures/` as a regression test.

  What this is **not** is full Sigstore verification. The bundle's DSSE
  envelope is signed by a Fulcio-issued certificate, but verifying that
  signature only means something once the chain is validated to a pinned root
  — otherwise anyone who can forge the `api.github.com` response forges the
  certificate with it. GitHub distributes that root through a TUF repository
  whose traversal is a client in its own right, and pinning the root without
  the TUF machinery would buy assurance at the price of a hard failure at every
  rotation. The trust anchor today is therefore TLS to a second pinned origin;
  chain validation slots in above it rather than replacing it.

  An overridden endpoint skips the attestation check — a local test server has
  no attestations and never could — which is the same line already drawn for
  privilege: bytes from an overridden endpoint never reach `sudo`.
- **No privilege escalation, because nothing is executed.** `fresh` spawns no
  package manager, so there is no `sudo` to consent to and no credential path to
  get wrong. Where root is genuinely required it appears only as the `sudo` in
  the command we *print* (`elevate::elevated` is a rendering helper — it is the
  only thing left of the old escalation code). The self-swap is unchanged and
  was never elevated: it writes one file the current user already owns, which is
  precisely what `self_update=true` asserts.

  This reverses the previous rule, which ran privileged commands in an
  interactive terminal on the grounds that printing them "did not remove the
  root command, it just moved it somewhere the user got no help with". That was
  true and is now outweighed: the help can be a printed command with the file
  already downloaded and verified, and the class of bug that comes with
  spawning an installer as root does not survive removing the spawn. See §9.
- **Downgrade protection:** refuse to "update" to a version `<= current` unless
  `--allow-downgrade` is passed.
- **Receipt trust:** packaged receipts are read-only, manager-owned. A
  malicious writable sidecar could at most *claim* `self_update=true`, but the
  swap still requires signature/checksum verification against the official
  release, so a forged receipt cannot cause arbitrary code to be installed — at
  worst it points the user at the genuine latest release.
- **macOS Gatekeeper / notarization:** a self-swapped binary inherits the
  quarantine attribute of the download. The engine strips `com.apple.quarantine`
  from the freshly downloaded, checksum-verified artifact; for the signed GUI
  `.pkg` path we stay Delegated (installer package), never self-swap.
- **Windows Authenticode:** self-swap preserves the signed `.exe` from the
  release (we don't re-sign locally); SmartScreen reputation carries over from
  the release artifact.

---

## 12. Privacy

No change to the privacy posture documented in `README.md`/`docs/privacy.md`:

- The update check is the same daily-debounced, anonymous request already
  gated by `should_run_daily_check` + `check_for_updates`.
- `--no-upgrade-check` disables both the check and any notification.
- The receipt is written locally and never transmitted. Telemetry's existing
  payload (version, OS/arch) *may optionally* include the resolved `channel`
  to help prioritise packaging work — but only under the existing opt-out, and
  it is not required for self-update to function.

---

## 13. Cross-platform summary

| | Linux | macOS | Windows |
|---|---|---|---|
| Packaged-receipt channels | apt, dnf, zypper, aur, pacman, flatpak, snap, nix, freebsd-pkg | homebrew (Cellar), nix | winget, scoop, chocolatey |
| Sidecar-receipt channels | npm, homebrew(linuxbrew), cargo-binstall, appimage, tarball | npm, cargo-binstall, tarball | npm, tarball |
| Self-update channels | tarball, appimage | tarball | tarball |
| Swap mechanism | atomic `rename()` | atomic `rename()` + de-quarantine | rename-running-exe + deferred delete |
| Receipt data dir fallback | `$XDG_DATA_HOME/fresh` | `~/Library/Application Support/fresh` | `%LOCALAPPDATA%\fresh` |

---

## 14. Rust module design

The paradigm ships as its own workspace subcrate, **`fresh-update`**, so the
provenance/update logic is reusable (CLI, installers, tests) and testable in
isolation without compiling the whole editor:

```
crates/fresh-update/
  build.rs        // emits FRESH_UPDATE_TARGET (target triple);
                  // rerun-if-env-changed FRESH_BUILD_CHANNEL
  src/
    lib.rs        // re-exports + TARGET_TRIPLE + embedded_channel()
    channel.rs    // Channel enum <-> stable string ids (+ aliases)
    confidence.rs // Confidence ladder (Unknown<Embedded<Authoritative<Overridden)
    receipt.rs    // InstallReceipt + Hints (serde/toml), candidate_paths(), find()
    registry_checkout.rs // is this a crates.io source build? (shared with build.rs)
    registry.rs   // Channel -> UpdateKind + UpdatePlan command templating
    provenance.rs // Provenance + resolve_from() (pure) + resolve() (env/fs)
    self_update.rs// verify_sha256 + atomic_replace (+ Windows deferred delete)
    version.rs    // is_newer() + parse_tag_name()

crates/fresh-editor/src/services/
  release_checker.rs  // keeps version-check + notification; its
                      // detect_provenance() delegates entirely to
                      // fresh_update::resolve() — there is no fallback.
```

Dependencies are deliberately minimal (`serde`, `toml`, `sha2`, `tracing`) so
the crate builds fast and offline. **Extraction and network I/O stay out of the
crate**: the caller (the editor, which already has a `ureq`/`rustls` stack in
`services::http`) fetches the release asset and, for `.tar.xz`/`.tar.gz`/`.zip`, extracts
the inner binary, then hands the verified executable bytes to
`self_update::atomic_replace`. AppImages need no extraction, so the flow is
usable end-to-end for them today.

Key types:

Key types:

```rust
pub enum Confidence { Overridden, Authoritative, Embedded, Unknown }

pub struct Provenance {
    pub channel: Channel,          // enum over the §3 ids
    pub confidence: Confidence,
    pub hints: Hints,              // tap, formula, winget_id, target, asset, …
    pub managed: bool,
    pub self_update: bool,
}

pub fn resolve() -> Provenance;               // A → B → C → D
pub fn update_command(p: &Provenance) -> UpdatePlan;
```

`release_checker::InstallMethod::update_command` is reimplemented on top of
`registry` so there is exactly one table of update commands. The existing
`release_checker` public API (used by `main.rs` and the e2e tests) stays
source-compatible; internally it calls `provenance::resolve()` instead of the
path heuristic, which no longer exists (§4.4).

---

## 15. Rollout plan

**Phase 1 — provenance plumbing (no behaviour change). ✅ landed.**
Added the `fresh-update` subcrate: receipt schema, layered resolver, registry,
confidence, a (then still present) demoted path heuristic, checksum verify +
atomic swap, and version compare — all unit-tested. `build.rs` embeds the target
triple and reruns on `FRESH_BUILD_CHANNEL`; `release_checker` delegates to
`fresh_update::resolve()`. Even with zero receipts written yet, this was already
strictly better (embedded channel + honest confidence). The heuristic layer was
removed in Phase 6 once every channel recorded itself.

**Phase 2 — receipts everywhere. ✅ landed.**
Each packaging pipeline (§7) writes its receipt via
`scripts/write-install-receipt.sh` (shell/CI channels) or an inline literal
(Ruby formula, npm JS, PKGBUILD, Nix). The deb/rpm install tests in
`linux-packages.yml` and the Flatpak install test assert the receipt exists
with the right `channel` (and, for deb/rpm, that nothing else leaked under
`/usr/share/fresh-editor`). This is the phase that delivers "know for sure."

**Phase 3 — `fresh update`. ✅ landed.**
`services/updater.rs` (feature `self-update`, in `default`). Delegated +
Toolchain paths run/print the known command; the SelfContained engine
downloads, verifies the SHA-256 sidecar (fail-closed), extracts (tar.xz/tar.gz/zip)
or `--appimage-extract`s, and atomically swaps — gated on
`confidence >= Embedded` via `self_update::can_self_update`. `config paths`
prints provenance. Covered by extraction unit tests and a mock-server
download→verify→extract integration test.

**Phase 4 — interactive UX. ✅ landed (attestation remaining).**
The status-bar `Update: vX.Y.Z` indicator is clickable (`StatusBarClickable::
Update` → `Action::UpdateFresh`), and a command-palette entry "Update fresh"
does the same. When `self_update` is on and an update is available, it prompts
`ConfirmUpdate`; confirming calls `updater::spawn_background_update`, which
re-invokes `fresh --cmd update --yes` as a **local** detached child (never the
window's `Authority`) and streams its output to `<log_dir>/self-update.log`.
The editor keeps running on the old inode until restart. Unknown/source
installs point at the releases page instead of prompting.

The result is surfaced on the **indicator**, not a transient status message
(which would scroll away and can't relay a "restart now" cue). App state
carries a `SelfUpdatePhase` with **three** terminal states, because
`fresh --cmd update` has three outcomes, not two:

| Child exit | Phase | Indicator |
|---|---|---|
| `0` | `Succeeded` | `Updated — restart fresh` |
| `EXIT_ACTION_REQUIRED` (2) | `ActionRequired` | `Update needs a command — click for details` |
| anything else | `Failed` | `Update failed — click for details` |

The third state exists because two channel groups legitimately end with the
update *not* applied and nothing having gone wrong: `DownloadPackage` channels
that need root (`dpkg -i`, `rpm -U`) download and verify the package and then
stop, and `Delegated`/`Toolchain` channels whose command we only print. Folding
those into the pair produced a wrong indicator in both directions — the
privileged path exited 1 and read as "Update failed" when nothing had failed,
and the print-only path exited 0 and read as "Updated — restart fresh" when
nothing had been installed. `UpdateStatus::ActionRequired` and the distinct
exit code are what keep them apart.

On launch the phase flips to `Running` (indicator: `Updating…`), and the
terminal's exit code is mapped by `SelfUpdatePhase::from_exit_code`.

Clicking the indicator is state-dependent: `Failed` offers retry / show-log,
`ActionRequired` surfaces the pending command (it does **not** re-offer an
update that has already been downloaded and verified), and `Running` /
`Succeeded` jump to the update output.

The confirmation prompt itself is built from the resolved `UpdatePlan` rather
than being one fixed row, since "Update to vX" means five different things
across the channels. `app::update_prompt::offer_for` maps the plan to an
`UpdateOffer` — in-place swap, download-and-install, download-then-hand-over,
run-the-command, show-the-command, or nothing-we-can-do — and the popup states
which one applies **before** the user confirms. For the offers that leave a
root command behind, the body says so up front.
Once a run has started, clicking the indicator — or the **"Open update log"**
command (`Action::OpenUpdateLog`) — opens the log via `open_local_file`, i.e.
from the machine `fresh` runs on, never the window's (possibly remote)
authority. Still remaining: an optional no-prompt `auto_update` mode.

**Phase 5 — one unified Linux route, verified at two origins. ✅ landed.**
The static musl archive became the default `install.sh` install and gained the
`tarball` receipt that lets it actually self-update (and stayed `.tar.gz`, the
compression any stock `tar` can read);
distro packages moved behind an explicit `--method` (§7.6). Every artifact the
engine downloads is now cross-checked against the release attestation on
`api.github.com` in addition to its `.sha256` sidecar, fail-closed, with the
real GitHub bundle checked in as a test fixture (§11).

**Phase 7 — fresh runs nothing but itself. ✅ landed.**
`Delegated`/`Toolchain` channels now print their command and return
`ActionRequired`; `DownloadPackage` downloads and verifies, then prints. The
`run_install` executor and the `sudo` invocation are deleted, `elevate` survives
only to render the `sudo` in printed text, and the popup offers "Update now"
solely for a self-contained install. Asserted end-to-end by putting a
sentinel-writing `brew`/`dpkg`/`sudo` on `PATH` and requiring they are never
called (`fresh-editor/tests/self_update_spine.rs`).

Also fixed there: `package()` enforced the pinned-host allowlist on the asset
URL taken from the feed, which made `--releases-url` unusable for the
air-gapped mirror it exists for — a mirror's feed names assets on the mirror.
The check now applies to the production endpoint only, where the feed is not
ours to trust.

**Phase 6 — no more guessing. ✅ landed.**
Deleted `heuristic.rs` and the `Heuristic` confidence rung; `ResolveInputs` no
longer takes the executable path or the host distro, so the resolver physically
cannot see them. `cargo` moved from a runtime path guess to a build-time fact
(§7.5). Provenance is now recorded or `Unknown`.

**Phase 8 — a release proves itself before anyone is offered it. ✅ landed.**
`release.yml` no longer publishes and hopes. One tag push, five steps:

1. **Check, then assemble as a draft.** Publishing is irreversible — assets
   freeze, the tag locks — so everything checkable without a published release
   happens first: `verify-staged-artifacts.sh` asserts the self-updatable
   archives are all present (including the musl `.tar.xz` the shim exists for)
   and that each `.sha256` describes the bytes beside it, while there is still
   nothing on GitHub to undo. Only then is a draft created and the assets
   attached — GitHub's own guidance for immutable-release repositories — and the
   attached list is diffed against the staged one, because `gh release create`
   uploads *after* creating the release and a partial upload leaves a real
   release missing files.

   The upload used to end in `2>/dev/null || true`, which turned a failed upload
   into a successful-looking release; errors are fatal now. (That `|| true` was
   doing a second job — an unmatched glob reaches `gh` as a literal path — so
   the asset list is expanded through `nullglob` instead.)
2. **Publish it as a pre-release.** This is the pivot: assets freeze, the
   release attestation is minted, and the bytes become downloadable.
   `/releases/latest` still names the previous release, and `feed::select`
   refuses a pre-release without `--pre` (§11) — so the release is reachable for
   testing and offered to nobody.
3. **`verify-release`** — every self-updatable archive is present, checksummed
   and attested (`verify-release-assets.sh`).
4. **`rehearse-update`** and **`verify-shipped-updater`** — the updater is
   exercised from both ends: the previous release actually self-updates to this
   one, and the binary in this release discovers, gates, verifies and swaps
   correctly against the real feed (§16).
5. **`promote`** — `--prerelease=false`, then `--latest`, then read the state
   back. Two calls in that order because the API refuses to make a pre-release
   latest; `=false` because these are boolean flags. The read-back is not
   ceremony: a promotion that silently failed would leave a finished release
   parked as a pre-release, visible to no one.

Registry publishes (Homebrew, npm, AUR, crates.io, winget) hang off `promote`
rather than off `release`, so no registry can ship a version that failed its own
upgrade rehearsal. They previously ran in parallel with an unverified release.

**When validation fails**, `mark-failed` retitles the release and prepends a
caution to its notes pointing at the run that rejected it. It cannot be
withdrawn — the tag is locked and the assets are frozen — so the remaining
choice is between an unexplained pre-release at the top of the list and one that
says what happened. It stays a pre-release either way: no updater offers it,
`/releases/latest` still names the last good release, and the fix is forward.

**Why the release is published before it is validated**, which reads backwards
and is not: a draft cannot be validated. Its assets are not downloadable (404
even with a token), and the release attestation is minted *at publish*, so a
draft has none. There is nothing to fetch and nothing to verify until the
release is real. Publishing as a pre-release is the least-exposed state GitHub
offers in which the artifact exists at all — and `promote` is what "published"
means to a user, since nothing offers them a release that is not `latest`.

**This replaces `-rc` tags.** Rehearsing used to mean cutting a throwaway
version — its own tag, its own changelog entry, a number nobody installs — and
it never actually rehearsed the real thing: the release attestation names the
tag it was minted for, so an rc proves the rc. Now the artifact under test is
the artifact that ships, and the only difference between a rehearsal and a
release is a flag that clears when it passes. `-rc` tags still work if wanted;
`promote` skips them, so they stay pre-releases forever.

---

## 16. Testing strategy

- **Unit:** receipt round-trip (serde), resolver precedence (A>B>C>D),
  registry command templating, semver downgrade guard.
- **Provenance matrix:** table-driven — for each `(receipt, embedded, path)`
  input triple, assert the resolved `(channel, confidence)`.
- **Package install tests (CI):** extend the existing `.deb`/`.rpm`/AppImage/
  flatpak install jobs to assert the packaged receipt is present and correct
  after install, and gone after uninstall.
- **Self-update spine (E2E). ✅ `crates/fresh-update/tests/self_update_spine.rs`.**
  Hermetic: `tiny_http` serves a fabricated feed announcing a version that does
  not exist, a tarball built in the test, and a `.sha256` over it, with
  `FRESH_RELEASES_URL` / `FRESH_DOWNLOAD_BASE` pointed at it. Nothing touches
  the network and no release has to be cut — the "new version" is just a string,
  which is what makes an upgrade testable without building a second binary.

  The thing being updated has to be a real process, since the engine replaces
  `current_exe()`, so `src/bin/update-harness.rs` is a stand-in for `fresh` that
  runs the engine against itself. It is gated on `insecure-endpoints`, which is
  never enabled in a published build.

  Asserts: the swap happens and the result is runnable; a bad checksum aborts
  and leaves the binary byte-identical; a missing asset fails without touching
  it; and an install with **no receipt** refuses to swap at all (the `Unknown`
  half of §4.4).

  **What it cannot see.** It derives the asset name from `archive_ext` and then
  builds the archive under that name, so it is self-consistent and blind to the
  mapping being *wrong* — verified by mutation: reintroducing the musl/xz
  mismatch left all four green. The seam is instead held by
  `archive_ext_matches_the_release_workflows`, which reads
  `musl-builds.yml`/`release.yml` and asserts the engine asks for what those
  files actually publish, sidecar included. That one does fail on the mutation.
  The lesson generalises: a test that derives its expectation from the code
  under test cannot hold a contract with something outside it.

- **Attestation:** covered against a real captured GitHub payload
  (`tests/fixtures/github-release-attestation.json`), not a synthetic one. The
  E2E skips the attestation gate by design — an overridden endpoint is
  untrusted, and a local server has no attestations.

- **The release itself** (`scripts/rehearse-self-update.sh`, run by the
  `rehearse-update` job): the one check the hermetic tests structurally cannot
  make. It downloads the *previously shipped* archive, installs it the way
  `install.sh` would, and drives that binary through `fresh --cmd update`
  against real GitHub — real feed, real bytes, real attestation, real swap.

  It tests the updater users are actually running, which is the one that
  matters: a bug in the *current* build can be fixed in the next release, while
  a bug in the shipped one strands everybody on it. Every release is therefore
  its own rehearsal, and nothing is promoted until it passes.

  A side effect worth keeping: this job is what decides when the musl `.tar.xz`
  transition shim can go. Binaries built before `archive_ext` existed ask for
  the xz name unconditionally, so while any such binary is still the *previous*
  release, dropping the shim breaks the rehearsal — which is exactly what it
  would do to those users. The shim's lifetime stops being a note in a comment
  and becomes a red job.

- **The updater being shipped** (`scripts/verify-shipped-updater.sh`, run by the
  `verify-shipped-updater` job): the same seam from the other end, and the
  reason the rehearsal alone is not enough. A rehearsal driven by the last
  release can only exercise what the last release could already do — for the
  first run of this flow, a binary with neither `--pre` nor an attestation
  check — so on its own it would leave the code being shipped untested until
  the release after next.

  This drives the archive just published (not a rebuild of the same commit:
  same code, and it also proves the artifact users download is sound), during
  the window where the release is public but not yet latest — the only moment
  these four questions have observable answers:

  1. a stable client is *not* offered it (`Latest version` is the previous one)
  2. `--pre` *is* offered it, through the real list endpoint
  3. the guard refuses a pre-release when a feed actually offers one — which
     `/releases/latest` never does, so this needs the tag endpoint; without it a
     build with the guard removed would still pass 1 and 2
  4. a real download verifies checksum **and** attestation, then swaps

  It cannot update *to* this release — it is already that version — so (4) runs
  downwards. The direction changes nothing: the same feed, download, checksum,
  attestation and swap execute. The endpoint override keeps it on an allowlisted
  host so the endpoint stays trusted and the attestation check stays on, and the
  engine's own "Verifying release attestation" line is asserted rather than the
  absence of the skip notice — a build that quietly stopped attesting would pass
  the negative test.

  Why this cannot be done before publishing, which is what makes the ordering
  in §15 (Phase 8) load-bearing rather than stylistic: the attestation the updater
  checks is a *release* attestation
  (`in-toto.io/attestation/release/v0.2`) minted by GitHub's immutable-releases
  setting when a release is **published**. A draft has none, and a draft's
  assets are not downloadable even with a token. There is no artifact to verify
  and no bytes to fetch until the release is real.
- **Windows swap:** test the rename-running-exe + deferred-delete path in a
  Windows CI runner.
- **Negative:** forged receipt claiming `self_update=true` must still fail the
  checksum/attestation gate.

---

## 17. Open questions / risks

1. **Homebrew Cellar receipts** are versioned paths; the formula must write the
   receipt into a stable `share/fresh/` location that survives `brew upgrade`.
2. **cargo-binstall** ships whatever is in the archive — confirm the embedded
   `tarball` receipt is *overwritten* to `cargo-binstall`, or accept that
   binstalled copies self-update as `tarball` (arguably fine, since the user has
   cargo — Toolchain would be nicer).
3. **Attestation verification offline / air-gapped:** the check is fail-closed
   and needs `api.github.com`, so a machine that can reach the release CDN but
   not the API cannot update. No escape hatch exists deliberately — a
   `--skip-attestation` flag is exactly the thing an attacker would tell the
   user to pass — but if this bites real users the answer is probably to accept
   a cached attestation, not to add the flag.
4. **Sigstore chain validation** (§11) needs GitHub's Fulcio root, which is
   distributed over TUF. Doing it properly means a TUF client; doing it
   improperly (pinning a root that rotates) trades availability for assurance.
   Deferred, with the digest cross-check standing in.
5. **mise/asdf** manage their own shims. The invocation is now
   `mise upgrade github:sinelaw/fresh` (the tool ref, matching the README's
   `mise use`); a bare `fresh` matched nothing. Nothing writes a `mise` receipt
   yet, so the channel is unreachable in practice — resolved as latent, not
   fixed by observation.
6. **Snap/Scoop/Chocolatey/zypper/pacman** have no pipeline, so nothing writes
   their receipts and nothing else resolves to them — they are unreachable, and
   §6 now routes them to `Manual` rather than naming an invented command. Their
   `Channel` variants are kept because the ids are receipt wire format, so a
   receipt written by a future pipeline still parses. **Open:** whether to drop
   the variants outright once it is clear those pipelines will not ship. That is
   a wire-format change and needs a deliberate call, not a cleanup.
7. **Two binaries, one machine** (e.g. a brew `fresh` and a cargo `fresh`): the
   receipt is resolved relative to `current_exe()`, so each updates itself
   correctly — anchoring the *search* on the executable is what makes that work,
   which is not the same as inferring the channel from the path.
