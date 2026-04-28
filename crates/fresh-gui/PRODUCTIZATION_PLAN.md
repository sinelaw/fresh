# Fresh GUI — Productization Plan

This plan turns the `fresh-gui` crate (winit + wgpu + ratatui-wgpu + muda) from
a working prototype into a production-grade, signed, auto-updating desktop
application on macOS and Windows (with Linux as a secondary target via the
existing AppImage flow).

It complements `MACOS_TODO.md`, which tracks platform-specific UX details.
This document is the cross-cutting roadmap — the work that has to land for the
GUI to be a polished product, not just a working binary.

---

## 0. Current state — audit

What we already have:

- **Architecture**: `crates/fresh-gui` encapsulates all windowing/GPU deps;
  `fresh-editor` opts in via the `gui` feature flag (`cargo build --features
  gui` produces a single `fresh` binary that runs as either TUI or windowed
  GUI based on `--gui`).
- **Lifecycle**: `crates/fresh-gui/src/lib.rs` already implements the winit
  0.30 `ApplicationHandler` trait and defers window/wgpu init to `resumed()`.
- **Native menus**: `muda` is wired up for macOS via
  `crates/fresh-gui/src/macos/{menu.rs,menu_tracking.rs}`, including
  `NSNotificationCenter` integration via `objc2-foundation` + `block2` to
  prevent the event loop from freezing while a menu is open.
- **macOS bundle**: `Info.plist`, `Fresh.entitlements`, and
  `create-app-bundle.sh` exist under `crates/fresh-gui/resources/macos/`.
- **CI**: `.github/workflows/gui-builds.yml` builds GUI binaries for the five
  primary targets (x86_64/aarch64 × {linux,darwin}, x86_64-windows-msvc),
  produces an ad-hoc-signed `.pkg` on macOS and an `AppImage` on Linux, and
  ships a raw `.exe` on Windows.

What is **missing or incomplete** vs. the reference framework:

| Area | Gap |
|---|---|
| Windows manifest | No DPI awareness, no Common Controls v6, no embedded version info |
| Windows subsystem | No `#![windows_subsystem = "windows"]` — GUI launch from Explorer flashes a console |
| Windows icon | No `.ico` embedded via `winresource` (only the in-window winit icon) |
| Code signing | macOS uses ad-hoc only; Windows is unsigned → SmartScreen warnings |
| Notarization | No `notarytool` step; `.pkg` won't pass Gatekeeper on a fresh Mac |
| Universal binary | macOS x86_64 and aarch64 ship as separate `.pkg`s; no `lipo` step |
| Installers | Windows ships a bare `.exe`; no MSI / NSIS; no DMG on macOS |
| Single-instance | Opening a second file launches a second process, no IPC handoff |
| Auto-update | No update channel, no signed manifest, no in-app updater |
| Observability | No `sentry` crate panic handler; `tracing` not wired to a file sink in GUI mode |
| HiDPI text | `ScaleFactorChanged` not handled; surface is not reconfigured on monitor switch |
| File handling | Finder double-click / `open -a` / drag-and-drop not routed into the running app |
| Dual-mode console | `hide_console_ng` not used; CLI invocations of `fresh.exe` would lose stdout if we set the windows subsystem naively |

The reference document's stack converges with what we already use, so the work
below is mostly **filling in the production gaps around an existing
architecture** rather than rewriting it.

---

## 1. Phase 1 — Cross-platform build polish

Goal: a `cargo build --release --features gui` binary that, when launched from
the OS shell, looks and behaves like a real native app — without yet worrying
about signing or auto-update.

### 1.1 Windows application manifest

Add a `build.rs` to `crates/fresh-editor` (or a new dedicated `gui` build
script gated on `cfg(target_os = "windows")` + the `gui` feature) that uses
[`embed-manifest`](https://crates.io/crates/embed-manifest) to embed an XML
manifest declaring:

- `<dpiAware>PerMonitorV2</dpiAware>` and `<dpiAwareness>` — prevents Windows
  from bitmap-scaling the window on a 4K monitor (the cause of "blurry
  Electron" complaints).
- Common Controls v6 (`<dependentAssembly>` for
  `Microsoft.Windows.Common-Controls`) — gives any native dialogs a modern
  look.
- `requestedExecutionLevel level="asInvoker"` — prevents the heuristic UAC
  prompt that fires on binaries whose names contain "setup", "install",
  "patch", etc.

### 1.2 Windows subsystem and dual-mode console

Today the binary is a single `fresh.exe` that switches between TUI and GUI at
runtime. We cannot unconditionally set `#![windows_subsystem = "windows"]`
because that would also suppress the console for `fresh --help` and TUI
sessions launched from `cmd`/PowerShell.

Plan:

1. Use `#![cfg_attr(all(windows, feature = "gui"), windows_subsystem =
   "windows")]` on the `fresh` binary so the GUI build defaults to no console.
2. Add `hide_console_ng` (or a small custom wrapper around
   `AttachConsole(ATTACH_PARENT_PROCESS)` + `FreeConsole`) so that:
   - When `fresh.exe` is launched from a terminal with no `--gui`, it
     re-attaches the parent console and behaves as a CLI.
   - When launched from Explorer with no args, it stays consoleless and
     enters GUI mode.
3. Document the trade-off: shipping two binaries (`fresh.exe`, `fresh-gui.exe`)
   is the alternative if `hide_console_ng` proves flaky on older Windows; keep
   that as the fallback.

### 1.3 Windows icon and version resource

Add `winresource` to the editor crate and wire it up in `build.rs`:

- Generate `crates/fresh-gui/resources/windows/fresh.ico` from the existing
  `crates/fresh-gui/resources/icon_*.png` set (16/32/48/256 in one container).
- Embed it as the app icon, plus `FileVersion`, `ProductVersion`,
  `CompanyName`, `LegalCopyright`, `OriginalFilename`. These show up in
  Explorer's Properties dialog and in SmartScreen's "do you want to run this"
  prompt.

### 1.4 macOS universal binary

Today `gui-builds.yml` produces two separate `.pkg` files (x86_64, aarch64).
For a polished release we should also ship a single universal `.pkg`:

- Add a job downstream of the two macOS matrix jobs that consumes both target
  binaries via `actions/download-artifact` and runs:

  ```sh
  lipo -create -output Fresh.app/Contents/MacOS/fresh \
       x86_64-apple-darwin/release/fresh \
       aarch64-apple-darwin/release/fresh
  ```

- Run `pkgbuild` against the merged bundle to produce
  `fresh-editor-gui-universal-${VERSION}.pkg`.
- Keep the per-arch `.pkg`s as well, but make the universal build the
  default download in the release notes.

### 1.5 Bundle metadata sync

`crates/fresh-gui/resources/macos/Info.plist` currently hard-codes
`<string>0.2.5</string>` and the CI patches it via `sed`. Replace with a
template `Info.plist.in` containing `__VERSION__` placeholders and a small
`xtask`/script that fills it from `cargo metadata`. Same for the AppStream
metainfo XML in the AppImage flow. This eliminates the silent drift we
already have between `Cargo.toml` (0.3.1) and `Info.plist` (0.2.5).

---

## 2. Phase 2 — macOS productization

Goal: a notarized `.dmg` that double-clicks open on a fresh Mac with no
right-click "Open Anyway" workaround, and a universal binary that runs
natively on Apple Silicon and Intel.

### 2.1 Code signing with Developer ID

Pre-requisites (one-time, owner-action):

- Apple Developer Program enrollment (~$99/yr).
- Generate a `Developer ID Application` certificate and a `Developer ID
  Installer` certificate; export both as `.p12`.
- Store as GitHub Actions repository secrets:
  `APPLE_CERT_P12_BASE64`, `APPLE_CERT_PASSWORD`,
  `APPLE_INSTALLER_CERT_P12_BASE64`, `APPLE_INSTALLER_CERT_PASSWORD`,
  `APPLE_TEAM_ID`, `APPLE_API_KEY_ID`, `APPLE_API_ISSUER_ID`,
  `APPLE_API_KEY_P8_BASE64` (for `notarytool`'s App Store Connect API auth —
  preferred over an app-specific password).

CI changes in `gui-builds.yml`:

1. Decode the `.p12` files and import into a temporary keychain (`security
   create-keychain` + `security import` + `security set-key-partition-list`).
2. Replace the `codesign --force --deep --sign -` ad-hoc step with:

   ```sh
   codesign --force --deep --options=runtime \
            --entitlements crates/fresh-gui/resources/macos/Fresh.entitlements \
            --sign "Developer ID Application: <Team Name> (<TEAM_ID>)" \
            Fresh.app
   ```

   `--options=runtime` enables Hardened Runtime, which is required for
   notarization.
3. Sign the `.pkg` with the installer cert: `productsign` (or sign the
   `pkgbuild` output with `--sign "Developer ID Installer: ..."`).

### 2.2 Hardened Runtime entitlements audit

The current `Fresh.entitlements` was written before signing was real. Audit
it before flipping on `--options=runtime`:

- `com.apple.security.cs.allow-jit` — only if `rquickjs`/QuickJS or any
  embedded interpreter actually needs JIT. QuickJS is an interpreter, not a
  JIT, so this should be **removed**.
- `com.apple.security.cs.allow-unsigned-executable-memory` — same as above,
  remove unless proven necessary by a runtime crash on a notarized build.
- `com.apple.security.cs.disable-library-validation` — keep only if we plan
  to load unsigned plugins; if plugins are embedded into the binary, remove.
- `com.apple.security.network.client` — keep (auto-update + LSP downloads).
- `com.apple.security.files.user-selected.read-write` — keep.

The smaller the entitlements set, the smoother notarization is.

### 2.3 Notarization

Add a CI step after signing:

```sh
ditto -c -k --keepParent Fresh.app Fresh.zip   # or use the .pkg
xcrun notarytool submit Fresh.zip \
    --key   ~/private_keys/AuthKey_${APPLE_API_KEY_ID}.p8 \
    --key-id "${APPLE_API_KEY_ID}" \
    --issuer "${APPLE_API_ISSUER_ID}" \
    --wait
xcrun stapler staple Fresh.app
```

Stapling the ticket onto the bundle is essential — it lets Gatekeeper verify
the notarization offline, so a user without internet can still launch the
app the first time.

### 2.4 DMG distribution

`.pkg` is fine for unattended installs but most users expect a `.dmg` with a
drag-to-Applications layout. Add `create-dmg` (Homebrew) or
[`dmgbuild`](https://github.com/dmgbuild/dmgbuild) to the macOS matrix:

- Background image with an arrow pointing to the Applications symlink.
- Sign the DMG itself (`codesign` works on DMGs).
- Notarize and staple the DMG (notarytool accepts DMGs directly).
- Output: `Fresh-${VERSION}-universal.dmg` as the headline macOS download.

### 2.5 Acceptance test

Manual checklist before tagging a release:

- [ ] Download the DMG on a Mac that has never run Fresh.
- [ ] Open it; double-click `Fresh.app`. **No** Gatekeeper dialog should
      appear (or at most a one-shot "downloaded from internet" prompt that
      resolves on its own).
- [ ] `spctl --assess --type execute -vv /Applications/Fresh.app` reports
      `accepted` and `source=Notarized Developer ID`.
- [ ] `codesign --verify --deep --strict /Applications/Fresh.app` exits 0.
- [ ] `lipo -info /Applications/Fresh.app/Contents/MacOS/fresh` lists both
      `x86_64` and `arm64`.

---

## 3. Phase 3 — Windows productization

Goal: a signed installer that doesn't trigger SmartScreen on a fresh Windows
machine, looks like a real app in Start Menu / Add-Remove Programs, and
upgrades cleanly across versions.

### 3.1 Installer format

We currently ship a bare `fresh.exe`. That is not a product — it has no
uninstaller, no Start Menu entry, no per-user vs. per-machine choice, and no
upgrade path. Pick **one** primary installer format and stick to it:

- **MSI via [`cargo-wix`](https://crates.io/crates/cargo-wix)** — recommended.
  Native Windows Installer, integrates with group policy and SCCM, supports
  silent install (`msiexec /i Fresh.msi /qn`), produces a stable
  `ProductCode` GUID for upgrades.
- **NSIS via `cargo-packager`** — alternative if we also want a single-exe
  installer with a custom UI. Smaller footprint, but no per-machine GPO story.

Action: add `wix/main.wxs` under `crates/fresh-editor/`, configure
`cargo-wix` with:

- `UpgradeCode` GUID — generate once, never change. Drives the upgrade
  story across every future release.
- Per-user install by default (no admin prompt), with an opt-in per-machine
  flag.
- `INSTALLDIR` Start Menu shortcut + Desktop shortcut (opt-in).
- `ARPPRODUCTICON` so the entry in "Apps & Features" shows the Fresh icon.
- File-association registry entries (see §6.3 below).

### 3.2 Authenticode signing

Ad-hoc / unsigned binaries are fine for development; for distribution they
trigger the "Windows Protected your PC" SmartScreen wall, which kills
adoption.

Recommended path: **Azure Trusted Signing**. It's a managed service that:

- Issues short-lived (3-day) certificates, so a leaked GitHub Actions
  secret can only be abused for ~72 hours.
- Costs ~$10/month vs. a multi-year EV cert at ~$300/yr.
- Builds SmartScreen reputation against the *Microsoft Identity Verification
  Service* root, which is pre-trusted and starts trusted on day one.

CI integration via `trusted-signing-cli` (or the official
`Azure/trusted-signing-action`):

1. Set up an Azure subscription, a Trusted Signing account, an identity
   validation, and a certificate profile (one-time, owner action).
2. Store as GitHub Actions secrets:
   `AZURE_TENANT_ID`, `AZURE_CLIENT_ID`, `AZURE_CLIENT_SECRET`,
   `AZURE_TS_ENDPOINT`, `AZURE_TS_ACCOUNT`, `AZURE_TS_PROFILE`.
3. After the MSI is built, run:

   ```sh
   trusted-signing-cli sign \
       --endpoint "$AZURE_TS_ENDPOINT" \
       --account "$AZURE_TS_ACCOUNT" \
       --certificate-profile "$AZURE_TS_PROFILE" \
       Fresh-${VERSION}-x64.msi
   ```

4. Sign the inner `fresh.exe` **before** packaging it into the MSI, then
   sign the MSI itself. Otherwise SmartScreen will warn on the unsigned exe
   the moment the installer extracts it.

Fallback if Trusted Signing isn't approved in time for v1.0: use a
self-purchased OV code-signing cert (DigiCert / SSL.com / Certum). It will
work, but reputation has to be earned over weeks of installs.

### 3.3 Windows portable / chocolatey

For users who don't want an installer, also ship:

- A **portable** `.zip` containing `fresh.exe` + an empty `data\` subfolder
  whose presence flips the app into "portable mode" (config + plugins read
  relative to the exe instead of `%APPDATA%`).
- A **Chocolatey** package — `chocolatey/fresh.nuspec` + `chocolateyInstall.ps1`
  that downloads the signed MSI from the GitHub release. The `winget`
  publisher script already exists at `scripts/winget-publish.py`; mirror that
  for choco. Both are nice-to-have, not blocking for v1.0.

### 3.4 Acceptance test

Manual checklist on a fresh Windows 11 VM:

- [ ] Download the MSI; SmartScreen does not show "unrecognized publisher".
- [ ] Install completes without UAC prompts (per-user mode).
- [ ] Start Menu has a "Fresh" entry with the correct icon.
- [ ] `Get-AuthenticodeSignature .\fresh.exe` reports `Valid`.
- [ ] Launching from Explorer does **not** flash a console window.
- [ ] Launching `fresh.exe` from `cmd.exe` with no args still prints help
      to the parent console (dual-mode behaviour from §1.2).
- [ ] Uninstall from Apps & Features removes the binary and shortcuts;
      user config under `%APPDATA%\Fresh` is preserved.
- [ ] Installing v(N+1) over v(N) keeps user settings.

---

## 4. Phase 4 — Single-instance and OS file handoff

Goal: when a user double-clicks `foo.rs` in Finder/Explorer (or runs `fresh
foo.rs` from a shell) and Fresh is already open, the file opens as a new
buffer in the existing window instead of spawning a second app.

### 4.1 IPC channel

Use [`ipc-channel`](https://crates.io/crates/ipc-channel) — the same crate
Servo uses. It picks the fastest native primitive automatically: Mach ports
on macOS, named pipes on Windows, Unix domain sockets on Linux.

Architecture (in `fresh-gui` or a new `fresh-ipc` crate):

1. On startup, before creating the winit `EventLoop`, try to **bind** a
   well-known channel name:
   - macOS: `dev.getfresh.Fresh.ipc` (Mach service name).
   - Windows: `\\.\pipe\Fresh-${USERSID}`.
   - Linux: `${XDG_RUNTIME_DIR}/fresh.sock`.
2. If the bind succeeds → we're the **primary** instance. Spawn a tokio task
   that accepts connections and forwards `OpenFile { path }` /
   `Activate {}` messages to the editor via an `EventLoopProxy::send_event`
   custom user event.
3. If the bind fails with "already in use" → we're a **secondary** instance.
   Connect, send `OpenFile { path: argv[1..] }` plus `Activate`, and `exit(0)`
   without ever creating a window.
4. On primary-instance shutdown, unlink the socket file (Linux) — Mach ports
   and named pipes clean up automatically.

### 4.2 macOS file open events

Finder double-click and `open -a Fresh foo.rs` do **not** pass the file as
`argv`. They send the running app an `NSApplicationDelegate
application:openURLs:` event. With our current setup we miss those events
entirely.

Plan: in `crates/fresh-gui/src/macos/`, install an `NSApplicationDelegate`
shim (using `objc2-app-kit`, which we'd add alongside `objc2-foundation`)
that overrides:

- `application:openURLs:` — extract `NSURL` paths, push them into the same
  `OpenFile` channel used by the IPC layer.
- `applicationShouldHandleReopen:hasVisibleWindows:` — when the user clicks
  the dock icon and we have no visible window, recreate one.

This makes "drag a file onto the dock icon" and "double-click `.rs` in
Finder" work the same way as the IPC handoff in §4.1.

### 4.3 Windows file association

The MSI from §3.1 should register `fresh.exe` as a handler for an opt-in
list of extensions (`.md`, `.rs`, `.ts`, `.json`, …). Explorer will then
launch `fresh.exe "C:\path\to\foo.rs"`. Combined with §4.1, this routes the
path into the running instance.

Use `HKCU\Software\Classes\Applications\fresh.exe\shell\open\command` rather
than hijacking the global `HKCR\.rs` mapping — users hate editors that
"steal" file associations on install.

### 4.4 Acceptance test

- [ ] Launch Fresh, leave it open. From a second terminal: `fresh README.md`.
      The existing window focuses and opens `README.md` as a new buffer; no
      second process appears in Activity Monitor / Task Manager.
- [ ] macOS: drag a file onto the dock icon → opens in the existing window.
- [ ] Windows: right-click a `.rs` file → "Open with Fresh" → opens in the
      existing window.
- [ ] Quit Fresh, repeat the same actions → Fresh launches fresh and opens
      the file.

