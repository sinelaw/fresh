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

## 0.1 Cross-cutting concern: icon coverage matrix

Surfaced early because empirical testing already showed icons appearing in
some contexts but not others. "Set the window icon" is not a complete
solution on either platform — each OS surface has a distinct source-of-truth
and a distinct failure mode. Phases 1, 2, and 3 each touch part of this; the
matrix below is the contract those phases must satisfy together.

### Windows surfaces

| Surface | Driven by | Failure mode |
|---|---|---|
| Explorer / Desktop / Properties dialog | `RT_GROUP_ICON` resource embedded in `.exe` (via `winresource`) | `.ico` missing sizes → stretched fallback at that size |
| Window title bar (small icon) + Alt-Tab thumbnail (small) | winit `Window::set_window_icon` → `WM_SETICON(ICON_SMALL)` | If unset, falls back to exe resource — usually fine |
| Taskbar (large icon) + Alt-Tab thumbnail (large) | `WM_SETICON(ICON_BIG)`; falls back to exe resource | winit historically only sets `ICON_SMALL`; `ICON_BIG` *must* be in the exe resource or set explicitly via Win32 platform extensions |
| Pinned-taskbar shortcut + Start Menu tile | The `.lnk`'s `Icon=` attribute, set by the MSI shortcut definition | MSI omits the icon attribute → generic exe icon on the pin |
| Jump list, taskbar grouping, notifications | **AppUserModelID** — `SetCurrentProcessExplicitAppUserModelID(L"dev.getfresh.Fresh")` called early in `main` | Not set → Windows infers an AUMID from the exe path, groups runs of Fresh under the wrong identity, and the pinned shortcut points to a different AUMID than the running window |
| Apps & Features uninstall entry | MSI `ARPPRODUCTICON` property | Omitted → generic Windows Installer icon |
| SmartScreen "do you want to run this" prompt | Exe resource + version info block | Missing version info → "Unknown publisher: Unknown" even ignoring signing |

**Required `.ico` size set**: 16, 24, 32, 48, 64, 256 (the 256 entry must be
PNG-compressed, not BMP, or older Windows fails to render it). Generate from
the existing `crates/fresh-gui/resources/icon_*.png` set; `winresource` will
embed the result as both `RT_GROUP_ICON/1` and the small/large pair Windows
expects.

**Required code-side calls**:

```rust
#[cfg(windows)]
unsafe {
    use windows::Win32::UI::Shell::SetCurrentProcessExplicitAppUserModelID;
    use windows::core::w;
    let _ = SetCurrentProcessExplicitAppUserModelID(w!("dev.getfresh.Fresh"));
}
// ... before creating the winit EventLoop
```

The AUMID string must match the one the MSI uses for its shortcut, or the
"running window" and "pinned shortcut" stay separate in the taskbar.

### macOS surfaces

| Surface | Driven by | Failure mode |
|---|---|---|
| Finder / Get Info / drag-out from title bar | `CFBundleIconFile` → `Resources/Fresh.icns` | `.icns` missing required types (`ic07/ic08/ic09/ic10/ic11/ic12/ic13/ic14`) → blank for that size |
| Dock (running app) + Cmd-Tab switcher | Same `CFBundleIconFile`, unless overridden via `[NSApp setApplicationIconImage:]` | Stale Launch Services icon cache — Finder/Dock keeps the old icon across rebuilds |
| About dialog | `CFBundleIconFile` | — |
| Notification Center | `CFBundleIconFile` | — |
| Window proxy icon (the file glyph in the title bar) | Per-document, `[NSWindow setRepresentedFilename:]` | This is **not** the app icon — common confusion source |

**Required `.icns` type set**: `iconutil -c iconset Fresh.icns` must list all
of `icon_16x16`, `icon_16x16@2x`, `icon_32x32`, `icon_32x32@2x`,
`icon_128x128`, `icon_128x128@2x`, `icon_256x256`, `icon_256x256@2x`,
`icon_512x512`, `icon_512x512@2x`. Missing any of those → blank icon at that
size in some surfaces. The current `create-app-bundle.sh` *does* generate
the full set; verify the output, don't just trust the script.

**Bundle-level requirements**:

- `CFBundleIconFile` set in `Info.plist` (currently: `Fresh.icns` ✓).
- `CFBundleIdentifier` stable across releases — Launch Services keys icon
  cache by bundle ID, not path. Changing it strands the cached icon.
- `LSApplicationCategoryType` set so Finder picks the right "kind" badge.

### Cache invalidation during development

Both OSes aggressively cache icons; a "wrong icon after rebuild" report is
50/50 a real bug vs. a stale cache. Document these in `RELEASING.md` as
sanity steps before signing off on a build:

- **macOS**: `sudo rm -rf /Library/Caches/com.apple.iconservices.store &&
  killall Dock Finder`. Bumping `CFBundleVersion` between dev rebuilds also
  forces re-cache.
- **Windows**: `ie4uinit.exe -show`, or delete `%LOCALAPPDATA%\IconCache.db`
  and `%LOCALAPPDATA%\Microsoft\Windows\Explorer\iconcache_*.db`, then
  restart `explorer.exe`.

### Acceptance test (icon-specific)

Run on a clean machine after a fresh install, with caches invalidated:

- [ ] **Windows — Explorer**: navigating to the install dir shows the icon
      at every Explorer view size (Small / Medium / Large / Extra Large
      Icons in the View menu). At Extra Large the 256-px PNG entry must
      render without aliasing.
- [ ] **Windows — Alt-Tab**: holding Alt-Tab shows the large icon, not a
      blurry upscale of the 16-px one.
- [ ] **Windows — Taskbar**: the running-app icon and a pinned-shortcut
      icon are visually identical and group into the same taskbar entry
      (proves AUMID is correct).
- [ ] **Windows — Apps & Features**: uninstall entry shows the Fresh icon.
- [ ] **macOS — Finder**: `/Applications/Fresh.app` shows the icon at
      every Finder icon size (toggle View → as Icons → slider).
- [ ] **macOS — Dock**: launching shows the correct icon; quit and
      relaunch keeps it (catches an icon-cache miss on first launch only).
- [ ] **macOS — Cmd-Tab**: the switcher shows the correct icon.
- [ ] **macOS — Get Info**: the icon in the top-left of the Get Info pane
      matches the Dock icon (different size buckets, same image).

Subsections that own pieces of this matrix: §1.3 (Windows `.ico` +
`winresource`), §1.4 (macOS bundle assembly), §3.1 (MSI `ARPPRODUCTICON` +
shortcut icon + AUMID registration), §2.5 + §3.4 (acceptance tests roll up
the icon checklist).

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

Add `winresource` to the editor crate and wire it up in `build.rs`. Owns
the **Explorer / Alt-Tab / taskbar** rows of the icon matrix in §0.1.

- Generate `crates/fresh-gui/resources/windows/fresh.ico` from the existing
  `crates/fresh-gui/resources/icon_*.png` set, containing **all** of
  16/24/32/48/64/256 (256 must be PNG-compressed, not BMP). Missing sizes
  are the most common cause of the "right in Explorer, wrong in Alt-Tab"
  problem.
- Embed it as the app icon, plus `FileVersion`, `ProductVersion`,
  `CompanyName`, `LegalCopyright`, `OriginalFilename`. These show up in
  Explorer's Properties dialog and in SmartScreen's "do you want to run this"
  prompt.
- Call `SetCurrentProcessExplicitAppUserModelID(L"dev.getfresh.Fresh")` at
  the very top of `main` (before the winit `EventLoop` is constructed). This
  is what makes the running window and the pinned-taskbar shortcut group
  under the same icon — without it, Windows generates an AUMID from the exe
  path and the two desync. The string must match the AUMID the MSI sets on
  the shortcut in §3.1.

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
- `INSTALLDIR` Start Menu shortcut + Desktop shortcut (opt-in). Each
  shortcut element must set `Icon="fresh.ico"` and
  `Arguments="--gui"` (so a Start Menu launch goes to GUI mode). The
  shortcut also needs an `<MsiShortcutProperty Id="AppUserModelID"
  Value="dev.getfresh.Fresh"/>` — same string as the runtime call in §1.3,
  or the pinned shortcut and the running window stay un-grouped on the
  taskbar.
- `ARPPRODUCTICON` so the entry in "Apps & Features" shows the Fresh icon.
- File-association registry entries (see §6.3 below).

This block owns the **Start Menu / pinned shortcut / taskbar grouping /
Apps & Features** rows of the icon matrix in §0.1. §1.3 owns the rest.

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

---

## 5. Phase 5 — Observability

Goal: when a production user hits a crash, we know within minutes — with a
backtrace, OS, GPU adapter, and a breadcrumb trail of recent events — and
we can ship a fix before they file an issue.

### 5.1 `tracing` to a rotating file sink

In TUI mode, `tracing` output goes to stderr. In GUI mode there is no
stderr — Explorer/Finder swallows it. Wire `tracing-appender` to a daily
rotating file:

- macOS: `~/Library/Logs/Fresh/fresh.log` (Console.app picks this up
  automatically).
- Windows: `%LOCALAPPDATA%\Fresh\logs\fresh.log`.
- Linux: `${XDG_STATE_HOME:-~/.local/state}/fresh/fresh.log`.

Initialize the sink in `main.rs` **before** `tokio::runtime::Runtime::new()`
so wgpu adapter selection logs are captured. Cap retention at 7 days /
50 MB so the log directory doesn't grow unbounded.

Add a "Help → Reveal Log File" menu item (already an open slot in `muda`)
that calls `open` / `Finder.app` / `explorer.exe` on the log directory —
makes "send me your log" a one-click ask in bug reports.

### 5.2 `sentry` panic and error reporting

Add `sentry` + `sentry-tracing` as **optional** deps gated on a `telemetry`
feature flag (default-on for release builds, default-off for dev builds and
for users who set `FRESH_TELEMETRY=0`).

Initialization order matters:

```rust
fn main() {
    let _sentry = std::env::var("FRESH_TELEMETRY").map(|v| v != "0").unwrap_or(true)
        .then(|| sentry::init((SENTRY_DSN, sentry::ClientOptions {
            release: sentry::release_name!(),
            traces_sample_rate: 0.0,        // no perf tracing for now
            send_default_pii: false,        // no usernames, no file paths
            attach_stacktrace: true,
            ..Default::default()
        })));

    // tracing → sentry breadcrumbs
    tracing_subscriber::registry()
        .with(file_layer)
        .with(sentry_tracing::layer())
        .init();

    let rt = tokio::runtime::Builder::new_multi_thread().enable_all().build().unwrap();
    rt.block_on(run_gui());
}
```

The `_sentry` guard must outlive `main` so its async flush runs on Drop.

### 5.3 Privacy

Productizing telemetry is a trust contract. Document it explicitly:

- **First-run dialog**: a one-time modal in the GUI ("Help us catch crashes
  — send anonymous error reports? [Yes] [No]"). Persist the choice to
  `config.json`. Default is opt-in but the user can revoke any time from
  Settings.
- **Scrub**: register a `sentry::ClientOptions::before_send` hook that
  strips `$HOME` and `$USERNAME` from any captured paths and replaces them
  with `~` / `<user>`. File contents must never be uploaded.
- **Document**: extend `docs/privacy.md` with the exact list of fields
  Sentry receives — release version, OS, GPU adapter name, panic message,
  redacted stack frames, breadcrumb log lines.

### 5.4 GPU diagnostics

Add a "Help → System Info" menu item that opens a modal with:

- Fresh version + git SHA.
- OS + version.
- `wgpu::AdapterInfo` (vendor, device, driver, backend).
- Active scale factor + window size in physical/logical pixels.
- A "Copy" button.

This is a force-multiplier on bug reports: 90 % of GPU-related issues are
"Intel UHD on Windows 10 with driver X" and the user has no way to know
that without it.

---

## 6. Phase 6 — Auto-update

Goal: a user running v(N) sees a non-blocking notification within hours of
v(N+1) shipping, and one click installs it.

### 6.1 Update channel design

Two channels: `stable` and `beta`. Each is a small JSON manifest hosted on
GitHub Pages (the existing `homepage/` deploy target works fine):

```json
{
  "version": "0.4.0",
  "pub_date": "2026-05-12T15:00:00Z",
  "platforms": {
    "darwin-universal": {
      "url": "https://github.com/sinelaw/fresh/releases/download/v0.4.0/Fresh-0.4.0-universal.dmg",
      "signature": "<minisign signature over the file>"
    },
    "windows-x86_64": {
      "url": "https://github.com/sinelaw/fresh/releases/download/v0.4.0/Fresh-0.4.0-x64.msi",
      "signature": "<minisign signature over the file>"
    }
  }
}
```

The signature is verified against a public key **embedded in the binary at
compile time**. This is the single most important property of an
auto-updater: even if GitHub's CDN is compromised, an attacker cannot push
a malicious update because they don't have the minisign secret key.

### 6.2 Implementation

Use [`cargo-packager-updater`](https://crates.io/crates/cargo-packager-updater)
or roll a thin wrapper using `minisign-verify` + `reqwest` + the OS-native
installer launcher. The crate is the safer default — it already handles:

- Manifest fetch + parse.
- Background download with `If-Modified-Since`.
- Signature verification.
- Platform-appropriate install:
  - macOS: replace the `.app` bundle in place via `mv` then relaunch.
  - Windows: launch the new MSI with `msiexec /i Fresh.msi /qb /norestart`
    (passive UI; small progress bar, no prompts), then exit so Windows
    Installer can replace the running exe.

Update check schedule: on startup (delayed 30s so it doesn't slow launch)
and every 6 hours thereafter via a tokio `interval`. Network errors are
silent — failed checks must never prompt the user or block the UI.

### 6.3 In-app UX

- A non-modal banner at the top of the window: "Fresh 0.4.0 is available. •
  [Install on Quit] • [Install Now] • [Skip This Version]".
- "Install on Quit" is the default — patches the bundle the next time the
  user closes the app, no interruption to their flow.
- Skipped versions are tracked in `config.json` so the banner doesn't
  re-appear for the same release.
- A "Help → Check for Updates…" menu item for manual checks.

### 6.4 Acceptance test

- [ ] Cut a fake v(N+1) release with a tweaked manifest.
- [ ] An installed v(N) shows the banner within one update interval.
- [ ] "Install on Quit" replaces the binary; relaunching shows the new
      version in About.
- [ ] Tampering with the downloaded artifact (flip one byte) causes the
      updater to refuse the install and log a signature-mismatch error.
- [ ] Disabling the network mid-download leaves the installed version
      untouched on next launch.

---

## 7. Phase 7 — HiDPI text quality

Goal: text is crisp on every supported monitor configuration, including
the awkward cases (mixed-DPI multi-monitor setups, dragging the window
between a 1080p panel and a 4K external display).

### 7.1 Surface reconfiguration on scale change

`crates/fresh-gui/src/lib.rs` currently handles `WindowEvent::Resized` but
not `WindowEvent::ScaleFactorChanged`. Add a handler:

```rust
WindowEvent::ScaleFactorChanged { scale_factor, .. } => {
    let physical = self.window.inner_size();
    self.surface.configure(&self.device, &SurfaceConfiguration {
        width:  physical.width,
        height: physical.height,
        ..self.surface_config.clone()
    });
    let cell_px = self.font.cell_size_for_scale(scale_factor);
    let cols = (physical.width  as f32 / cell_px.width ) as u16;
    let rows = (physical.height as f32 / cell_px.height) as u16;
    self.app.resize(cols, rows);
    self.window.request_redraw();
}
```

Two invariants must hold:

1. The wgpu surface is configured in **physical pixels** (`PhysicalSize`).
2. The ratatui grid (cols × rows) is computed from physical pixels divided
   by the **per-scale-factor** cell size, not the logical-pixel cell size.
   Otherwise a 4K monitor renders 80 columns of 30 px text instead of
   the expected ~250 columns.

### 7.2 Font cell sizing

`ratatui-wgpu` exposes a `Font` builder; we currently build it once at
startup. For HiDPI we need to either:

- Re-rasterize the font atlas at the new scale factor (cleaner, more
  memory), or
- Render at 2× and downsample with a known-good filter (simpler, slightly
  worse on fractional scales like 1.5× / 1.75×).

Pick the first option for v1.0 — it's the only one that produces crisp
text on Windows's common 125% / 150% scales. Cache the atlas keyed by
`(font_size_px.round() as u32)` so toggling between two monitors doesn't
re-rasterize on every frame.

### 7.3 Subpixel positioning

ratatui's character grid is integer-aligned by definition, so subpixel
glyph positioning is not in scope — but **subpixel anti-aliasing** is.
Confirm that the embedded JetBrains Mono atlas is rendered with the
glyphon / fontdue settings that produce LCD-subpixel output on Windows
(where ClearType is the user expectation) and grayscale AA on macOS
(where Apple removed subpixel AA in 10.14).

### 7.4 Acceptance test

- [ ] Single 1080p monitor at 100%: text is crisp, baseline-aligned.
- [ ] Single 4K monitor at 200%: text is crisp; column count roughly
      doubles vs. the same window size at 100%.
- [ ] Dragging the window between a 100% monitor and a 200% monitor:
      glyphs re-rasterize, no visible tearing or column reflow lag > 1
      frame.
- [ ] Windows at 125% scale: no fractional-pixel blur (the most common
      complaint about non-DPI-aware apps).

---

## 8. Phase 8 — Native UX polish

Goal: hit the threshold where the app feels native rather than ported.
Most of the macOS-specific items live in `MACOS_TODO.md`; this section
captures the cross-platform parity work.

### 8.1 System theme detection

Use [`dark-light`](https://crates.io/crates/dark-light) (cross-platform,
~20 LOC of glue) to read the OS appearance and forward it to the editor's
theme system. Subscribe to changes:

- macOS: `NSDistributedNotificationCenter` for
  `AppleInterfaceThemeChangedNotification`.
- Windows: `WM_SETTINGCHANGE` with `SPI_SETCLIENTAREAANIMATION`.
- Linux: D-Bus `org.freedesktop.portal.Settings`.

When the user has `theme: "system"` in `config.json`, switch palettes live
without a restart.

### 8.2 Native chrome

- **Window title**: show `<filename> — Fresh` and a "modified" indicator
  (a bullet on macOS via `setDocumentEdited:`, a `*` prefix on Windows).
- **Proxy icon** (macOS): the file icon next to the title that the user
  can drag out — `setRepresentedFilename:` does this.
- **Recent files**: hook into `NSDocumentController`'s
  `noteNewRecentDocumentURL:` on macOS and the Windows
  `SHAddToRecentDocs` API. The "Open Recent" submenu (already a TODO in
  `MACOS_TODO.md`) reads from these system stores.

### 8.3 Keyboard accelerator coalescing

Right now `muda` accelerators on Windows fire through a separate event
loop and can race with winit's keyboard input. Adopt the
`EventLoopProxy<UserEvent>` pattern:

- All `MenuEvent`s are forwarded to a single `UserEvent::Menu(id)`.
- All winit keyboard events that match a registered accelerator are
  **swallowed** (don't reach the editor as raw key events).
- The editor sees one and only one path for a given action: the
  `UserEvent::Menu` dispatch.

This eliminates the class of bugs where Cmd-S triggers Save twice (once
from the menu, once from the keymap).

### 8.4 First-run experience

- A welcome screen on first launch that links to the existing tour
  (`.fresh-tour.json`) and the docs.
- Skip the first-run dialog if the binary was launched with a file path.

---

## 9. Phase 9 — CI/CD release pipeline

Goal: a single `git tag v0.4.0 && git push --tags` produces signed,
notarized, downloadable artifacts on every supported platform within ~30
minutes, with no human in the loop except the secret-holder for emergency
overrides.

### 9.1 Pipeline shape

Extend the existing `.github/workflows/release.yml` so that on a tag push:

1. **Plan** (existing) — read the tag, compute version, emit a matrix.
2. **Build** (existing `gui-builds.yml`, extended):
   - macOS x86_64 → unsigned binary artifact.
   - macOS aarch64 → unsigned binary artifact.
   - Windows x86_64 → unsigned `fresh.exe`.
   - Linux x86_64 / aarch64 → AppImage (existing flow).
3. **Sign** (new):
   - macOS-merge job: download both arch artifacts, `lipo` them into a
     universal binary, build the `.app`, sign with Developer ID, build
     `.pkg` and `.dmg`, sign the DMG, notarize, staple.
   - Windows-sign job: build the MSI, sign the inner `.exe` and the MSI
     itself via Trusted Signing.
4. **Manifest** (new): emit `latest-stable.json` with version + signed
   download URLs + minisign signatures, sign it with the update key,
   upload to the GitHub Pages site.
5. **Publish** (existing `release.yml`): create the GitHub release, attach
   all artifacts, run downstream homebrew/winget/AUR/npm publish jobs.
6. **Smoke test** (new, post-release): a separate workflow that downloads
   each artifact on a clean runner, installs it, runs `fresh --version`,
   and reports green/red. Catches "we forgot to sign one of them" the
   same hour the release ships.

### 9.2 Secret hygiene

All signing secrets live as GitHub Actions repository secrets, never in
the repo. Group them under environment protection rules (`production`)
that require manual approval on tag pushes — so a compromised PR can't
exfiltrate them via a workflow change.

Document the rotation procedure in `docs/internal/`:

- Apple cert: rotate annually before expiry; update both `.p12` secrets
  and the `APPLE_TEAM_ID` reference.
- Trusted Signing: managed by Azure, no manual rotation needed.
- Minisign updater key: kept offline (encrypted on a hardware key); the
  matching public key is committed to the repo at
  `crates/fresh-gui/src/updater_pubkey.minisig.pub` and embedded into the
  binary via `include_bytes!`. Compromise → cut a new key, ship a
  bridge-release signed with both old and new keys.

### 9.3 Pre-release channel

For risk reduction, run every commit to `main` through the same pipeline
but publish to `latest-beta.json` (signed with the same key, published to
the beta channel). The auto-updater opt-in makes this safe — only users
who flip "Settings → Updates → Use beta channel" see them. Internal
dogfooding catches signing/notarization regressions before they hit the
stable channel.

### 9.4 Release checklist

A minimal `RELEASING.md` lives next to this plan once Phase 9 lands.
Per-release manual gates:

- [ ] CHANGELOG.md updated and stamped with the version + date.
- [ ] `Cargo.toml` workspace version bumped.
- [ ] Smoke-test workflow green on the previous tag.
- [ ] Run §2.5 + §3.4 + §4.4 acceptance checklists on the staging
      artifacts (the universal DMG, the signed MSI, the AppImage).
- [ ] Tag pushed, release notes drafted from the CHANGELOG entry.

---

## Sequencing and milestones

The phases are listed in priority order, but they parallelize naturally:

| Milestone | Phases | Outcome |
|---|---|---|
| **M1 — Native polish** | 1, 7 | A `cargo build --release --features gui` binary that looks native on every monitor and doesn't flash a console. No signing, no auto-update. Internal alpha. |
| **M2 — Signed installers** | 2, 3 | Notarized DMG + signed MSI. Users can install Fresh from a download link without scary OS warnings. Public beta. |
| **M3 — Production runtime** | 4, 5 | Single-instance + telemetry. We can debug user crashes and the app feels like a real editor when files are double-clicked. v1.0. |
| **M4 — Continuous delivery** | 6, 8, 9 | Auto-updates, system theme, recent files, automated release pipeline. Steady-state product. |

The two phase-spanning blockers worth surfacing early are **secret
provisioning** (Apple Developer enrollment, Azure Trusted Signing setup,
minisign keypair generation — all owner-only actions, all multi-day
turnaround) and the **GPU regression risk** in Phase 7 — `ratatui-wgpu` is
a git-only dependency on a young commit, and changing its font/scale
plumbing may require upstream patches before HiDPI is solid.

Both should be kicked off in parallel with Phase 1, not deferred to their
own milestones.
