# Clangd Helper Plugin

Fresh bundles `plugins/clangd_support.ts` so clangd users get a small helper plugin out of the box.

## Commands

* `Clangd: Switch Source/Header` calls `textDocument/switchSourceHeader` for the active cpp-style buffer and opens the returned URI if there is a match.
* `Clangd: Open Project Config` searches the current directory tree for a `.clangd` file and opens it in the editor.

Those commands are registered in the command palette after the plugin loads; TypeScript plugins can register their own commands by calling `editor.registerCommand`.

## Notifications

The plugin listens for `lsp/custom_notification` events emitted by the core and filters for clangd-specific methods (`textDocument/clangd.fileStatus`, `$/memoryUsage`, etc.). When clangd sends `textDocument/clangd.fileStatus`, the plugin surfaces it as a status message (`Clangd file status: …`). The editor renders this plugin-provided status slot alongside the usual diagnostics/cursor info, so the notification stays visible without overwriting core messages.

Use `editor.setStatus` to set a plugin status message and `editor.setStatus("")` to clear it; the core `Editor::set_status_message` call clears the plugin slot so core actions regain priority.

## Project setup heuristic

`Clangd: Project Setup` opens a readonly panel that inspects the current workspace root and reports whether the files clangd needs are present (`compile_commands.json`, `.clangd`, etc.). The panel also guesses the build system (CMake, Bazel, Make) by looking for markers like `CMakeLists.txt` or `WORKSPACE` and prints quick tips for generating the missing artifacts (e.g., `cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON build`, `bear -- make`). This panel gives you a quick readiness check before enabling heavier clangd features on projects such as Lustre or other Makefile-heavy trees.
