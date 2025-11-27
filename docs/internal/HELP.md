# Help/Manual Plan

> **STATUS: COMPLETED** - This plan has been implemented. See `plugins/manual_help.ts` for the implementation.

This document captures the analysis and plan for revamping the Help experience.

## Goal
- Replace the bespoke `HelpRenderer` view with a "Keyboard Shortcuts" virtual buffer.
- Introduce a new manual page virtual buffer with links to the documentation.
- Implement both experiences via plugins, so the core can simply show virtual buffers instead of custom UI.

## Implementation

The help system is now plugin-driven:

- **`Action::ShowHelp`** dispatches the `manual_page` hook to plugins
- **`Action::ShowKeyboardShortcuts`** dispatches the `keyboard_shortcuts` hook with keybinding data
- **`plugins/manual_help.ts`** handles both hooks and creates virtual buffers

## Original Key Tasks (all completed)
1. **Command integration** - Core dispatches hooks instead of rendering custom UI
2. **Keyboard shortcuts plugin** - Implemented in `manual_help.ts`
3. **Manual page plugin** - Implemented in `manual_help.ts`
4. **Core/Docs updates** - Completed
