# macOS Native GUI — TODO List

Comprehensive list of work needed for best macOS native behavior in GUI mode,
following Apple Human Interface Guidelines and macOS user expectations.

## Legend
- [x] Done in this initial PR
- [ ] Future work

---

## 1. Native Menu Bar
- [x] Create `muda`-based native macOS menu bar mirroring editor menus
- [x] App menu ("Fresh") with About, Settings, Quit, Services, Hide/Show
- [x] File, Edit, View, Selection, Go, LSP, Help menus
- [x] Window menu with Minimize, Maximize, Fullscreen, Bring All to Front
- [ ] Dynamic menu item enable/disable based on editor context (e.g. disable "Paste" when clipboard empty)
- [ ] Checkbox state sync for toggle items (line numbers, word wrap, etc.)
- [ ] Keyboard shortcut annotations on menu items (show ⌘S next to "Save")
- [ ] "Open Recent" submenu (track recently opened files)
- [ ] Dynamic theme submenu under View (like the built-in Copy with Formatting)
- [ ] Localized menu labels (use the same i18n system as built-in menus)
- [ ] Context menus (right-click) via native popup menus

## 2. Keyboard Shortcuts (Cmd Key)
- [x] `macos-gui` keymap with Super (⌘) modifier bindings
- [x] Parse `"super"` / `"cmd"` / `"command"` in keybinding JSON
- [x] Display ⌘ symbol in keybinding hints on macOS
- [x] Auto-select `macos-gui` keymap when launching in GUI mode on macOS
- [ ] Cmd+Z / Cmd+Shift+Z for undo/redo (system-wide expectation) — **done in keymap**
- [ ] Cmd+Tab should not be intercepted (let macOS handle app switching)
- [ ] Cmd+H should hide window (currently mapped to replace — consider moving replace to Cmd+Shift+H or Cmd+Option+F)
- [ ] Cmd+M should minimize window
- [ ] Cmd+` (backtick) for window cycling
- [ ] Cmd+Comma for Preferences (standard macOS) — **done in keymap**
- [ ] Dead key / compose key handling for international input (accents, umlauts)
- [ ] Input method editor (IME) support for CJK input
- [ ] Fn key support (Fn+Backspace → Forward Delete, Fn+Up → Page Up, etc.)
- [ ] Touch Bar support (if applicable)

## 3. App Icon & Branding
- [x] Green leaf placeholder icon (PNG at multiple sizes: 16–1024px)
- [x] SVG source for the icon
- [ ] Replace placeholder with final designed icon
- [ ] Generate proper `.icns` file for the app bundle (requires macOS `iconutil`)
- [ ] Dock icon (comes from app bundle's `CFBundleIconFile`)
- [ ] Dock icon badge support (e.g. unsaved changes indicator)
- [ ] Custom document icons for associated file types

## 4. App Bundle & Distribution
- [x] `Info.plist` with bundle ID, version, document types, privacy descriptions
- [x] `Fresh.entitlements` for network, file access, JIT
- [x] `create-app-bundle.sh` script to assemble `.app` directory
- [ ] Proper code signing with Developer ID certificate
- [ ] Notarization for Gatekeeper approval
- [ ] DMG disk image creation for distribution
- [ ] Homebrew Cask formula
- [ ] Sparkle update framework integration (or custom updater)
- [ ] Universal binary (arm64 + x86_64) via `lipo`
- [ ] App Store submission (requires sandbox compliance)

## 5. Window Management
- [ ] Native title bar integration (traffic light buttons: close/minimize/zoom)
- [ ] Full-screen support (native macOS full-screen, not just maximized)
- [ ] Split View support (macOS split-screen with other apps)
- [ ] Tab bar support (native macOS window tabbing via Cmd+T)
- [ ] Window restoration on relaunch (NSRestorable)
- [ ] Proper window minimum size constraints
- [ ] Window title shows filename + modified indicator (dot in close button)
- [ ] Proxy icon in title bar (drag file icon from title)
- [ ] Transparent title bar option (for unified toolbar look)
- [ ] Vibrancy / translucency effects (NSVisualEffectView)

## 6. File Handling & System Integration
- [ ] Open files via Finder (double-click → launches app with file)
- [ ] Open files via `open -a Fresh file.txt` command
- [ ] Drag-and-drop files onto the dock icon to open
- [ ] Drag-and-drop files onto the editor window
- [ ] "Open With" integration in Finder context menu
- [ ] File change monitoring with FSEvents (native macOS file watcher)
- [ ] Trash integration (`NSWorkspace.recycleURLs` instead of `rm`)
- [ ] iCloud Drive support
- [ ] File bookmarks / security-scoped bookmarks for sandboxed access
- [ ] Handoff support (continue editing on another device)

## 7. System Services & Integration
- [ ] macOS Services menu integration (text processing services)
- [ ] Share menu support (share file/selection via system share sheet)
- [ ] Spotlight metadata importer (index file contents for search)
- [ ] Quick Look plugin (preview files in Finder)
- [ ] AppleScript / Scripting Bridge support
- [ ] Automator actions
- [ ] Accessibility (VoiceOver support, accessibility labels)
- [ ] System Preferences integration (e.g. keyboard shortcuts pane)
- [ ] Login items (auto-start option)

## 8. Clipboard & Text System
- [ ] Rich text clipboard support (paste formatted code into other apps)
- [ ] Pasteboard types (NSStringPboardType, public.utf8-plain-text)
- [ ] Universal Clipboard (Handoff-based clipboard sharing across Apple devices)
- [ ] Spell checking integration (NSSpellChecker)
- [ ] Grammar checking integration
- [ ] Text substitution support (System Preferences → Keyboard → Text)
- [ ] Emoji & Symbol picker (Cmd+Ctrl+Space → Character Viewer)

## 9. Appearance & Theming
- [ ] Dark mode / Light mode detection and auto-switching
- [ ] Follow system accent color for UI highlights
- [ ] Respect "Reduce motion" accessibility setting
- [ ] Respect "Reduce transparency" accessibility setting
- [ ] Respect "Increase contrast" accessibility setting
- [ ] Dynamic Type / text size accessibility setting
- [ ] System font support (SF Pro, SF Mono)
- [ ] Native scrollbar appearance (overlay scrollbars)
- [ ] Smooth scrolling (momentum / inertial scrolling from trackpad)
- [ ] Pinch-to-zoom for font size adjustment
- [ ] Cursor blink rate from system preferences

## 10. Notifications & Alerts
- [ ] Native alert dialogs (NSAlert) for "Save before closing?"
- [ ] User Notification Center integration (build complete, LSP errors, etc.)
- [ ] Sound effects (system alert sound on error)
- [ ] Bounce dock icon for attention

## 11. Performance & Power
- [ ] Metal backend support via wgpu (preferred over OpenGL on macOS)
- [ ] ProMotion display support (120Hz rendering on newer MacBooks)
- [ ] App Nap support (reduce power usage when occluded)
- [ ] Thermal state monitoring (reduce work under thermal pressure)
- [ ] Energy efficiency for battery life (coalesce timers, reduce wake-ups)

## 12. Printing
- [ ] Native print dialog (Cmd+P → NSPrintOperation)
- [ ] Print preview
- [ ] Syntax-highlighted printing
- [ ] PDF export via print dialog

## 13. Testing & CI
- [ ] macOS-specific unit tests for menu bar
- [ ] macOS-specific unit tests for Cmd key translation
- [ ] Integration test: app bundle launches correctly
- [ ] CI: build macOS app bundle on GitHub Actions (macOS runner)
- [ ] CI: notarization step
- [ ] Visual regression tests on macOS
