# Refactoring Plan: src/app/mod.rs

## Overview

The file `src/app/mod.rs` has grown to **10,233 lines**, making it the largest source file in the codebase. This plan outlines how to split it into smaller, focused modules while maintaining the existing architecture.

## Current Structure Analysis

### Editor Struct (lines 113-478)
- 365 lines defining 80+ fields
- Fields cover: buffers, splits, LSP, plugins, recovery, terminals, mouse, settings, etc.

### Method Categories Identified

| Category | Approx Lines | Line Range | Description |
|----------|--------------|------------|-------------|
| Constructor/Core | 720 | 480-1200 | `new()`, `with_options()`, accessors |
| Buffer Management | 600 | 1200-2200 | Open, close, switch, virtual buffers |
| Split Management | 400 | 2600-2900 | Horizontal/vertical splits, navigation |
| UI Toggles | 200 | 2900-3100 | Toggle line numbers, menu, mouse |
| Event Handling | 400 | 3300-3700 | `apply_event`, plugin hooks |
| Clipboard | 400 | 3760-4130 | Copy, cut, paste, multi-cursor |
| File Operations | 600 | 4220-4850 | Save, revert, file watching |
| Recovery/Auto-save | 350 | 4910-5180 | Recovery service methods |
| Prompts | 550 | 5190-5750 | Prompt handling, file open state |
| Async Processing | 450 | 5750-6200 | `process_async_messages`, LSP status |
| Plugin Commands | 600 | 6200-6830 | `handle_plugin_command` |
| LSP Operations | 1700 | 6830-8500 | All LSP request/response handling |
| Tests | 1700 | 8500-10233 | Unit tests |

### Existing Submodules (already extracted)
- `input.rs` (2,205 lines) - Input handling
- `render.rs` (3,355 lines) - Rendering
- `mouse_input.rs` (1,220 lines) - Mouse input
- `async_messages.rs` (704 lines) - Async message types
- `terminal.rs` (728 lines) - Terminal operations
- `session.rs` (1,213 lines) - Session management
- `plugin_commands.rs` (1,017 lines) - Plugin command handling
- And 12 other smaller modules

## Refactoring Plan

### Phase 1: LSP Operations (Highest Impact ~1,700 lines)

**New file: `src/app/lsp_requests.rs`**

Methods to extract:
- `request_completion()` (line 7151)
- `request_goto_definition()` (line 7188)
- `request_hover()` (line 7224)
- `request_hover_at_position()` (line 7274)
- `request_references()` (line 7483)
- `request_signature_help()` (line 7582)
- `request_code_actions()` (line 7736)
- `handle_completion_response()` (line 6830)
- `handle_goto_definition_response()` (line 6964)
- `handle_hover_response()` (line 7322)
- `handle_signature_help_response()` (line 7620)
- `handle_code_actions_response()` (line 7797)
- `handle_references_response()` (line 7859)
- `handle_rename_response()` (line 8025)
- `apply_lsp_text_edits()` (line 7928)
- `apply_rename_batch_to_buffer()` (line 8143)
- `apply_inlay_hints_to_state()` (line 7426)
- `with_lsp_for_buffer()` (line 7089)
- `has_pending_lsp_requests()` (line 7034)
- `cancel_pending_lsp_requests()` (line 7041)
- `send_lsp_cancel_request()` (line 7060)
- `start_rename()` (line 8369)
- `cancel_rename_overlay()` (line 8423)
- `perform_lsp_rename()` (line 8428)
- `send_lsp_changes_for_buffer()` (line 8232)
- `notify_lsp_file_opened()` (line 4500)
- `notify_lsp_file_changed()` (line 4654)
- `request_inlay_hints_for_active_buffer()` (line 3051)

### Phase 2: Buffer Management (~600 lines)

**New file: `src/app/buffer_management.rs`**

Methods to extract:
- `open_file()` (line 1199)
- `open_file_no_focus()` (line 1252)
- `new_buffer()` (line 1595)
- `create_virtual_buffer()` (line 1634)
- `set_virtual_buffer_content()` (line 1680)
- `close_buffer()` (line 2207)
- `force_close_buffer()` (line 2219)
- `close_buffer_internal()` (line 2224)
- `switch_buffer()` (line 2279)
- `close_tab()` (line 2299)
- `close_tab_in_split()` (line 2370)
- `next_buffer()` (line 2443)
- `prev_buffer()` (line 2479)
- `navigate_back()` (line 2515)
- `navigate_forward()` (line 2567)
- `restore_global_file_state()` (line 1416)
- `save_file_state_on_close()` (line 1474)
- `goto_line_col()` (line 1534)

### Phase 3: Split Management (~400 lines)

**New file: `src/app/split_actions.rs`**

Methods to extract:
- `split_pane_horizontal()` (line 2604)
- `split_pane_vertical()` (line 2637)
- `close_active_split()` (line 2670)
- `next_split()` (line 2712)
- `prev_split()` (line 2718)
- `switch_split()` (line 2724)
- `save_current_split_view_state()` (line 2740)
- `restore_current_split_view_state()` (line 2751)
- `sync_split_view_state_to_editor_state()` (line 2764)
- `adjust_other_split_cursors_for_event()` (line 2775)
- `sync_editor_state_to_split_view_state()` (line 3707)
- `adjust_split_size()` (line 2826)
- `toggle_maximize_split()` (line 2838)
- `get_separator_areas()` (line 2855)
- `get_split_ratio()` (line 2860)

### Phase 4: Clipboard Operations (~400 lines)

**New file: `src/app/clipboard.rs`**

Methods to extract:
- `copy_selection()` (line 3761)
- `copy_selection_with_theme()` (line 3792)
- `start_copy_with_formatting_prompt()` (line 3908)
- `cut_selection()` (line 3955)
- `paste()` (line 4002)
- `paste_text()` (line 4022)
- `set_clipboard_for_test()` (line 4115)
- `paste_for_test()` (line 4123)
- `add_cursor_at_next_match()` (line 4136)
- `add_cursor_above()` (line 4164)
- `add_cursor_below()` (line 4192)

### Phase 5: Recovery/Auto-save (~350 lines)

**New file: `src/app/recovery_actions.rs`**

Methods to extract:
- `start_recovery_session()` (line 4912)
- `end_recovery_session()` (line 4917)
- `has_recovery_files()` (line 4922)
- `list_recoverable_files()` (line 4927)
- `recover_all_buffers()` (line 4935)
- `discard_all_recovery()` (line 5024)
- `auto_save_dirty_buffers()` (line 5036)
- `is_active_buffer_recovery_dirty()` (line 5170)
- `delete_buffer_recovery()` (line 5179)

### Phase 6: Settings UI (~400 lines)

**New file: `src/app/settings_actions.rs`**

Methods to extract:
- `open_settings()` (line 1830)
- `close_settings()` (line 1854)
- `save_settings()` (line 1868)
- `settings_navigate_up()` (line 1923)
- `settings_navigate_down()` (line 1930)
- `settings_activate_current()` (line 1937)
- `settings_increment_current()` (line 2070)
- `settings_decrement_current()` (line 2132)
- `open_help_manual()` (line 1724)
- `open_keyboard_shortcuts()` (line 1762)

## Implementation Approach

For each new module:

1. **Create the new file** with appropriate imports
2. **Add `impl Editor` block** in the new file (Rust allows multiple impl blocks)
3. **Move methods** one at a time
4. **Add `mod` declaration** in `mod.rs`
5. **Run tests** to verify nothing is broken
6. **Commit** after each module extraction

## Expected Results

After refactoring:
- `mod.rs`: ~4,000 lines (reduced from 10,233)
- New focused modules for each domain
- Tests can stay with the code they test or be moved later

## Notes

- The `Editor` struct definition stays in `mod.rs` as it's the central type
- All methods remain on `impl Editor` - they're just in different files
- Rust allows `impl` blocks in multiple files for the same type
- This follows the existing pattern used by `input.rs`, `render.rs`, etc.
