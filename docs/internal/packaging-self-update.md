# Packaging & Self-Update Paradigm

> Status: **design + Phase 1 landed**. This document specifies a new packaging
> paradigm for `fresh` whose defining property is **deterministic install
> provenance**: every distribution channel records — at install time — exactly
> which mechanism installed the binary, so the editor can self-update through
> the *same* mechanism without ever guessing. It supersedes the path-based
> heuristic in `services/release_checker.rs`.
>
> **Implemented (Phases 1–3):**
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
> **Not yet done (Phase 4):** the one-key in-editor update from the status-bar
> notification, and GitHub build-attestation verification (SHA-256 is enforced;
> attestation is still a follow-up). See §15 and §17.

---

## 1. Why this exists

`fresh` already ships through an unusually large number of channels (see the
inventory in §3). What it does **not** have is a reliable way to answer one
question at runtime:

> *"How was **this** copy of `fresh` installed, and therefore how should it be
> updated?"*

Today `services/release_checker.rs` answers that by **inspecting
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
8. **Graceful degradation.** If no receipt exists (old installs, exotic
   channels), fall back to the current heuristic but *label it low-confidence*
   and refuse destructive actions.

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
| 9 | openSUSE (zypper) | `zypper` | rpm | zypper |
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
1. <dir(exe)>/install-receipt.toml                       # sidecar, same dir
2. <dir(exe)>/../share/fresh/install-receipt.toml        # FHS-style sidecar
3. <dir(exe)>/../lib/fresh/install-receipt.toml          # npm/node layout
4. $XDG_DATA_HOME/fresh/install-receipt.toml   (Linux)   # per-user fallback
   ~/Library/Application Support/fresh/…       (macOS)
   %LOCALAPPDATA%\fresh\…                       (Windows)
```

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

### 4.4 Layer D — path heuristic (last resort)

The existing `detect_install_method_from_path` logic, retained but demoted. It
only runs when A–C produce nothing. Confidence = `Heuristic`. Results at this
confidence are shown to the user as a *suggestion* and **never** trigger an
automatic binary swap.

### 4.5 Resolution & confidence

```
Overridden  > Authoritative > Embedded > Heuristic > Unknown
```

`confidence` gates behaviour:

| Confidence | Notify of update? | Show exact command? | Auto self-swap allowed? |
|---|---|---|---|
| Overridden / Authoritative | yes | yes | yes, if channel is self-managed |
| Embedded | yes | yes | yes, if channel is self-managed |
| Heuristic | yes | yes (with "detected" caveat) | **no** |
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
either provenance (`channel`, `version`, `installed_at`) or an optimisation so
the update path doesn't have to re-derive it.

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
    /// Unknown provenance: link to the releases page only.
    Manual,
}
```

| channel | strategy | update invocation (templated with `hints`) |
|---|---|---|
| `homebrew` | Delegated | `brew upgrade {formula}` |
| `apt` | Delegated (sudo) | `apt-get install --only-upgrade {package_name}` |
| `dnf` | Delegated (sudo) | `dnf upgrade {package_name}` |
| `zypper` | Delegated (sudo) | `zypper update {package_name}` |
| `aur-bin` / `aur` | Delegated | `{aur_helper} -S {aur_pkg}` (detect yay/paru) |
| `pacman` | Delegated (sudo) | `pacman -Syu {package_name}` |
| `winget` | Delegated | `winget upgrade --id {winget_id}` |
| `scoop` | Delegated | `scoop update fresh` |
| `chocolatey` | Delegated (admin) | `choco upgrade fresh` |
| `flatpak` | Delegated | `flatpak update {flatpak_ref}` |
| `snap` | Delegated | `snap refresh fresh` |
| `nix` | Delegated | `nix profile upgrade` (or flake rebuild note) |
| `freebsd-pkg` | Delegated (sudo) | `pkg upgrade fresh` |
| `cargo` | Toolchain | `cargo install --locked fresh-editor` |
| `cargo-binstall` | Toolchain | `cargo binstall fresh-editor` |
| `npm` | Toolchain | `npm update -g {npm_pkg}` |
| `mise` | Toolchain | `mise upgrade fresh` |
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

### 7.5 Known limitations
- **winget / scoop / chocolatey.** These consume the same Windows `.zip` as a
  raw download. winget-pkgs zip installers can't run a post-extract hook, so a
  winget install currently inherits the archive's generic `tarball` receipt
  (i.e. it would self-update rather than defer to `winget upgrade`). Marking it
  `managed` needs a wrapper installer type; tracked for later. Scoop/Chocolatey
  manifests *can* write a receipt, but those channels don't ship yet.
- **cargo (crates.io).** A user's `cargo install` build doesn't see any
  build-time env, so it can't embed `channel=cargo`. It relies on the path
  heuristic (`~/.cargo/bin`), which is reliable enough; a receipt isn't written.

---

## 8. The self-update engine (SelfContained channels)

Only `tarball` and `appimage` (and future direct-download channels) ever swap
their own binary. Flow implemented in `services::self_update`:

1. **Resolve** provenance; assert `strategy == SelfContained` and
   `confidence >= Embedded`.
2. **Check** latest version via the existing `release_checker`
   (GitHub releases API, daily-debounced logic reused).
3. **Select asset** for `FRESH_TARGET_TRIPLE` using the `asset` hint
   (`fresh-editor-<triple>.tar.xz` / `.zip`, or the `.AppImage`).
4. **Download** to a temp file *on the same filesystem* as the target
   (so the final rename is atomic), via `services::http::download_to_file`.
5. **Verify — mandatory, fail-closed:**
   - fetch the `<asset>.sha256` and compare;
   - verify the GitHub build **attestation** for the asset (attestations are
     already produced in CI). A verification failure aborts the update and
     leaves the current binary untouched.
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
3. Either **runs it** (default when the tool is on `PATH` and no elevation is
   needed, e.g. `brew upgrade`, `flatpak update`, `winget upgrade`) after a
   confirmation prompt, or **prints it** for the user to run (when it needs
   `sudo`/admin, or the tool isn't found). We never invoke `sudo` ourselves.
4. Never touches the binary directly — the manager does, keeping its package DB
   and signatures intact.

AUR is special-cased: detect the installed helper (`yay`, `paru`, else
`makepkg`) exactly as `install.sh` already does, and template the command.

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
runs the same flow. For `Heuristic`/`Unknown` confidence the notification links
to instructions rather than offering a one-key update.

Config (`config.rs`): keep `check_for_updates` and `--no-upgrade-check`. Add
`editor.auto_update = false` (default) — reserved for a future opt-in that lets
SelfContained installs update without a prompt; **off by default** per the
non-goal on silent installs.

---

## 11. Security

- **Transport:** HTTPS only, via the existing `ureq + rustls` stack in
  `services::http`.
- **Integrity:** SHA-256 comparison against the published `.sha256` **and**
  GitHub artifact-attestation verification before any swap. Fail-closed.
- **No privilege escalation by the editor.** Delegated commands that need root
  are printed, not run. Self-swap only ever writes files the current user
  already owns (that's precisely what `self_update=true` asserts).
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
    confidence.rs // Confidence ladder (Unknown<Heuristic<Embedded<Authoritative<Overridden)
    receipt.rs    // InstallReceipt + Hints (serde/toml), candidate_paths(), find()
    registry.rs   // Channel -> UpdateKind + UpdatePlan command templating
    provenance.rs // Provenance + resolve_from() (pure) + resolve() (env/fs)
    heuristic.rs  // the demoted path-based detection (Layer D)
    self_update.rs// verify_sha256 + atomic_replace (+ Windows deferred delete)
    version.rs    // is_newer() + parse_tag_name()

crates/fresh-editor/src/services/
  release_checker.rs  // keeps version-check + notification; its
                      // detect_install_method() now delegates to
                      // fresh_update::resolve(), falling back to the legacy
                      // path heuristic only when provenance is Unknown.
```

Dependencies are deliberately minimal (`serde`, `toml`, `sha2`, `tracing`) so
the crate builds fast and offline. **Extraction and network I/O stay out of the
crate**: the caller (the editor, which already has a `ureq`/`rustls` stack in
`services::http`) fetches the release asset and, for `.tar.xz`/`.zip`, extracts
the inner binary, then hands the verified executable bytes to
`self_update::atomic_replace`. AppImages need no extraction, so the flow is
usable end-to-end for them today.

Key types:

Key types:

```rust
pub enum Confidence { Overridden, Authoritative, Embedded, Heuristic, Unknown }

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
path heuristic.

---

## 15. Rollout plan

**Phase 1 — provenance plumbing (no behaviour change). ✅ landed.**
Added the `fresh-update` subcrate: receipt schema, layered resolver, registry,
confidence, the demoted heuristic, checksum verify + atomic swap, and version
compare — all unit-tested. `build.rs` embeds the target triple and reruns on
`FRESH_BUILD_CHANNEL`; `release_checker::detect_install_method()` delegates to
`fresh_update::resolve()` and only falls back to the legacy path heuristic when
provenance is Unknown. Even with zero receipts written yet, this is already
strictly better (embedded channel + honest confidence).

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
downloads, verifies the SHA-256 sidecar (fail-closed), extracts (tar.xz/zip)
or `--appimage-extract`s, and atomically swaps — gated on
`confidence >= Embedded` via `self_update::can_self_update`. `config paths`
prints provenance. Covered by extraction unit tests and a mock-server
download→verify→extract integration test.

**Phase 4 — UX polish (remaining).**
One-key update from the status-bar notification for self-capable installs; and
GitHub build-attestation verification alongside the SHA-256 check. Optional
`editor.auto_update` opt-in. The exit-time notification already prints the
resolved command / `fresh --cmd update` hint.

---

## 16. Testing strategy

- **Unit:** receipt round-trip (serde), resolver precedence (A>B>C>D),
  registry command templating, semver downgrade guard.
- **Provenance matrix:** table-driven — for each `(receipt, embedded, path)`
  input triple, assert the resolved `(channel, confidence)`.
- **Package install tests (CI):** extend the existing `.deb`/`.rpm`/AppImage/
  flatpak install jobs to assert the packaged receipt is present and correct
  after install, and gone after uninstall.
- **Self-update integration:** spin up a mock release server (the pattern
  already used in `release_checker.rs` / `update_notification.rs` tests) serving
  a signed+checksummed fake archive; assert download → verify → atomic swap →
  new `--version`, and that a bad checksum aborts without touching the binary.
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
3. **Attestation verification offline / air-gapped:** provide a
   `--skip-attestation` (checksum-only) escape hatch, clearly warned.
4. **mise/asdf** manage their own shims; confirm `mise upgrade fresh` is the
   right invocation and that a receipt even makes sense there (mise may prefer
   we stay `Unknown` → Manual).
5. **Snap/Chocolatey** are listed as planned; receipts are specified but the
   channels ship only once those pipelines exist.
6. **Two binaries, one machine** (e.g. a brew `fresh` and a cargo `fresh`): the
   receipt is resolved relative to `current_exe()`, so each updates itself
   correctly — this is a feature of anchoring on the executable path, not the
   heuristic's global guess.
