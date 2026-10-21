<!-- Generated from the action tables in the source. Do not edit: change `crates/fresh-editor/src/input/keybindings.rs`, then run `UPDATE_DOCS=1 cargo test -p fresh-editor --test all_tests -- action_name_docs::`. -->

# Actions

Every built-in action, by the name Fresh reads it under. The same names are used everywhere an action is written down:

- a keybinding's `"action"` in `config.json` (see [Keyboard](./keyboard.md));
- a macro step in [`init.ts`](./init.md) — what **Macro: Save to init.ts** writes as `editor.defineMacro(...)`;
- a step passed to `editor.executeActions` from a plugin.

## Step format

In `init.ts` and plugins, each step is an object with the action's name, an optional `count`, and `args` for the few actions that carry data:

```typescript
{ action: "move_word_right" }                  // no args
{ action: "move_down", count: 3 }              // repeated 3 times
{ action: "insert_char", args: { char: "x" } } // with args
```

In `config.json`, a keybinding takes the same `args` object next to its `"action"`.

**A name that isn't listed here is treated as a plugin action** — the name of a handler a plugin registered with `registerHandler` (or `init.ts` did). That is what lets you bind keys to plugin commands, and it is also why a misspelled built-in name raises no error: it becomes a plugin action nothing handles, and does nothing when run.

## Actions that take args

| Action | Args | Example |
|---|---|---|
| `clear_bookmark` | `{ char: "x" }` | Clear bookmark 'x' |
| `copy_with_theme` | `{ theme: "dracula" }` | Copy with dracula theme |
| `insert_char` | `{ char: "x" }` | Insert character 'x' |
| `jump_to_bookmark` | `{ char: "x" }` | Jump to bookmark 'x' |
| `menu_open` | `{ name: "File" }` | Open File menu |
| `play_macro` | `{ char: "x" }` | Play macro 'x' |
| `plugin_next_key` | `{ key: "x" }` | Key for a plugin (x) |
| `prompt_confirm_with_text` | `{ text: "hello" }` | Confirm prompt (hello) |
| `set_bookmark` | `{ char: "x" }` | Set bookmark 'x' |
| `show_macro` | `{ char: "x" }` | Show macro 'x' in buffer |
| `switch_keybinding_map` | `{ map: "emacs" }` | Switch to 'emacs' keybindings |
| `toggle_macro_recording` | `{ char: "x" }` | Toggle macro recording for 'x' |

## All actions

| Action | Description |
|---|---|
| `add_cursor_above` | Add cursor above |
| `add_cursor_below` | Add cursor below |
| `add_cursor_next_match` | Add cursor at next match |
| `add_cursors_to_line_ends` | Add cursors to line ends |
| `add_ruler` | Add ruler |
| `block_select_down` | Block select down |
| `block_select_left` | Block select left |
| `block_select_right` | Block select right |
| `block_select_up` | Block select up |
| `calibrate_input` | Calibrate keyboard input |
| `cancel_mark` | Cancel mark (soft exit, keeps anchor) |
| `clear_bookmark` | Clear bookmark 'x' (args: `{ char: "x" }`) |
| `clear_mark` | Clear mark (hard exit, removes anchor) |
| `clear_search` | Clear Search Highlights |
| `clear_warnings` | Clear warnings |
| `close` | Close file |
| `close_settings` | Close settings |
| `close_split` | Close split |
| `close_tab` | Close tab |
| `close_terminal` | Close terminal |
| `command_palette` | Command palette |
| `completion_accept` | Accept completion |
| `completion_dismiss` | Dismiss completion popup |
| `composite_next_hunk` | Next Hunk (Side-by-Side Diff) |
| `composite_prev_hunk` | Previous Hunk (Side-by-Side Diff) |
| `copy` | Copy |
| `copy_file_path` | Copy file path |
| `copy_relative_file_path` | Copy relative file path |
| `copy_with_theme` | Copy with dracula theme (args: `{ theme: "dracula" }`) |
| `cut` | Cut |
| `cycle_live_grep_provider` | Cycle Live Grep provider (git-grep → rg → ag → ack → grep) |
| `dabbrev_expand` | Expand abbreviation (dabbrev) |
| `decrease_split_size` | Decrease split size |
| `dedent_selection` | Dedent selection |
| `delete_backward` | Delete backward |
| `delete_forward` | Delete forward |
| `delete_line` | Delete line |
| `delete_to_line_end` | Delete to end of line |
| `delete_to_line_start` | Delete to start of line |
| `delete_vi_word_end` | Delete word forward |
| `delete_word_backward` | Delete word backward |
| `delete_word_forward` | Delete word forward |
| `detach` | Detach from daemon |
| `dump_config` | Dump config to file |
| `dump_ui_tree` | Dump UI tree |
| `duplicate_line` | Duplicate line |
| `duplicate_line_above` | Duplicate line above |
| `ensure_final_newline` | Ensure file ends with a newline |
| `event_debug` | Debug keyboard events |
| `expand_line_selection` | Expand line selection |
| `expand_selection` | Expand selection |
| `extract_tab_to_new_workspace` | Extract tab to new workspace |
| `file_browser_toggle_detect_encoding` | Toggle encoding auto-detection |
| `file_browser_toggle_hidden` | Toggle hidden files visibility |
| `file_explorer_collapse` | File explorer: collapse directory |
| `file_explorer_copy` | File explorer: copy |
| `file_explorer_copy_full_path` | File explorer: copy full path |
| `file_explorer_copy_relative_path` | File explorer: copy relative path |
| `file_explorer_cut` | File explorer: cut |
| `file_explorer_delete` | File explorer: delete |
| `file_explorer_down` | File explorer: navigate down |
| `file_explorer_duplicate` | File explorer: duplicate |
| `file_explorer_expand` | File explorer: expand directory |
| `file_explorer_extend_selection_down` | File explorer: extend selection down |
| `file_explorer_extend_selection_up` | File explorer: extend selection up |
| `file_explorer_new_directory` | File explorer: new directory |
| `file_explorer_new_file` | File explorer: new file |
| `file_explorer_open` | File explorer: open file |
| `file_explorer_page_down` | File explorer: page down |
| `file_explorer_page_up` | File explorer: page up |
| `file_explorer_paste` | File explorer: paste |
| `file_explorer_refresh` | File explorer: refresh |
| `file_explorer_rename` | File explorer: rename |
| `file_explorer_search_backspace` | File explorer: delete search character |
| `file_explorer_search_clear` | File explorer: clear search |
| `file_explorer_select_all` | File explorer: select all |
| `file_explorer_toggle_gitignored` | File explorer: toggle gitignored files |
| `file_explorer_toggle_hidden` | File explorer: toggle hidden files |
| `file_explorer_toggle_select` | File explorer: toggle selection |
| `file_explorer_up` | File explorer: navigate up |
| `find_in_selection` | Search within selection |
| `find_next` | Find next search match |
| `find_previous` | Find previous search match |
| `find_selection_next` | Find next occurrence of selection |
| `find_selection_previous` | Find previous occurrence of selection |
| `focus_editor` | Focus editor |
| `focus_file_explorer` | Focus file explorer |
| `focus_next_sidebar_section` | Focus next sidebar section |
| `focus_prev_sidebar_section` | Focus previous sidebar section |
| `focus_terminal` | Focus terminal |
| `force_quit` | Quit editor (discard unsaved changes) |
| `format_buffer` | Format buffer with configured formatter |
| `goto_line` | Go to line number |
| `goto_matching_bracket` | Go to matching bracket |
| `increase_split_size` | Increase split size |
| `init_check` | Check init.ts |
| `init_edit` | Edit init.ts |
| `init_reload` | Reload init.ts |
| `insert_char` | Insert character 'x' (args: `{ char: "x" }`) |
| `insert_newline` | Insert newline |
| `insert_tab` | Insert tab |
| `inspect_theme_at_cursor` | Inspect theme at cursor |
| `jump_to_bookmark` | Jump to bookmark 'x' (args: `{ char: "x" }`) |
| `jump_to_next_error` | Jump to next error/diagnostic |
| `jump_to_previous_error` | Jump to previous error/diagnostic |
| `keyboard_shortcuts` | Show keyboard shortcuts |
| `list_bookmarks` | List all bookmarks |
| `list_macros` | List all recorded macros |
| `load_plugin_from_buffer` | Load Plugin from Buffer |
| `lsp_code_actions` | LSP: Show code actions |
| `lsp_completion` | LSP: Show completion suggestions |
| `lsp_goto_definition` | LSP: Go to definition |
| `lsp_hover` | LSP: Show hover documentation |
| `lsp_implementation` | LSP: Go to implementation |
| `lsp_references` | LSP: Find references |
| `lsp_rename` | LSP: Rename symbol |
| `lsp_restart` | LSP: Start/restart server for current language |
| `lsp_signature_help` | LSP: Show signature help |
| `lsp_stop` | LSP: Stop a running server |
| `lsp_toggle_for_buffer` | LSP: Toggle LSP for current buffer |
| `menu_activate` | Activate menu bar |
| `menu_close` | Close menu |
| `menu_down` | Navigate to next menu item |
| `menu_execute` | Execute selected menu item |
| `menu_left` | Navigate to previous menu |
| `menu_open` | Open File menu (args: `{ name: "File" }`) |
| `menu_right` | Navigate to next menu |
| `menu_up` | Navigate to previous menu item |
| `move_document_end` | Move to document end |
| `move_document_start` | Move to document start |
| `move_down` | Move cursor down |
| `move_left` | Move cursor left |
| `move_left_in_line` | Move cursor left |
| `move_line_down` | Move line down |
| `move_line_end` | Move to line end |
| `move_line_start` | Move to line start |
| `move_line_up` | Move line up |
| `move_page_down` | Move page down |
| `move_page_up` | Move page up |
| `move_right` | Move cursor right |
| `move_right_in_line` | Move cursor right |
| `move_to_paragraph_down` | Move to next empty line |
| `move_to_paragraph_up` | Move to previous empty line |
| `move_up` | Move cursor up |
| `move_word_end` | Move to word end |
| `move_word_left` | Move word left |
| `move_word_right` | Move word right |
| `navigate_back` | Navigate back in history |
| `navigate_forward` | Navigate forward in history |
| `new` | New file |
| `next_buffer` | Next buffer |
| `next_pane` | Next pane |
| `next_split` | Next split |
| `next_window` | Next window |
| `none` | Alias of `noop`. No action |
| `noop` | No action |
| `open` | Open file |
| `open_keybinding_editor` | Keybinding Editor |
| `open_line` | Open line below |
| `open_live_grep` | Live Grep (Find in Files) |
| `open_settings` | Open settings |
| `open_terminal` | Open terminal |
| `open_terminal_below` | Open terminal below |
| `open_terminal_in_dock` | Open terminal in Utility Dock |
| `open_terminal_right` | Open terminal to the right |
| `open_update_log` | Show update output |
| `paste` | Paste |
| `play_last_macro` | Play last recorded macro |
| `play_macro` | Play macro 'x' (args: `{ char: "x" }`) |
| `plugin_next_key` | Key for a plugin (x) (args: `{ key: "x" }`) |
| `popup_cancel` | Popup cancel |
| `popup_confirm` | Popup confirm |
| `popup_focus` | Focus popup |
| `popup_page_down` | Popup page down |
| `popup_page_up` | Popup page up |
| `popup_select_next` | Popup select next |
| `popup_select_prev` | Popup select previous |
| `prev_buffer` | Previous buffer |
| `prev_pane` | Previous pane |
| `prev_split` | Previous split |
| `prev_window` | Previous window |
| `prompt_accept_suggestion` | Prompt accept suggestion |
| `prompt_backspace` | Prompt backspace |
| `prompt_cancel` | Cancel prompt |
| `prompt_confirm` | Confirm prompt |
| `prompt_confirm_with_text` | Confirm prompt (hello) (args: `{ text: "hello" }`) |
| `prompt_copy` | Prompt copy |
| `prompt_cut` | Prompt cut |
| `prompt_delete` | Prompt delete |
| `prompt_delete_to_line_end` | Prompt delete to end of line |
| `prompt_delete_word_backward` | Prompt delete word backward |
| `prompt_delete_word_forward` | Prompt delete word forward |
| `prompt_jump_to_bookmark` | Jump to bookmark (prompts for register) |
| `prompt_move_end` | Prompt move to end |
| `prompt_move_end_selecting` | Prompt move to end selecting |
| `prompt_move_home_selecting` | Prompt move to start selecting |
| `prompt_move_left` | Prompt move left |
| `prompt_move_left_selecting` | Prompt move left selecting |
| `prompt_move_right` | Prompt move right |
| `prompt_move_right_selecting` | Prompt move right selecting |
| `prompt_move_start` | Prompt move to start |
| `prompt_move_word_left` | Prompt move word left |
| `prompt_move_word_right` | Prompt move word right |
| `prompt_page_down` | Prompt page down |
| `prompt_page_up` | Prompt page up |
| `prompt_paste` | Prompt paste |
| `prompt_play_macro` | Play macro (prompts for register) |
| `prompt_promote_macro` | Promote macro to an init.ts command (prompts for register) |
| `prompt_record_macro` | Record macro (prompts for register) |
| `prompt_save_macro_to_init` | Save macro to init.ts (prompts for register) |
| `prompt_select_all` | Prompt select all |
| `prompt_select_next` | Prompt select next |
| `prompt_select_prev` | Prompt select previous |
| `prompt_select_word_left` | Prompt select word left |
| `prompt_select_word_right` | Prompt select word right |
| `prompt_set_bookmark` | Set bookmark (prompts for register) |
| `query_replace` | Interactive replace (y/n/!/q for each match) |
| `quick_open` | Quick Open (files, commands, buffers) |
| `quick_open_buffers` | Quick Open Buffers |
| `quick_open_files` | Quick Open Files |
| `quit` | Quit editor |
| `recenter` | Recenter view on cursor |
| `redo` | Redo |
| `redraw_screen` | Redraw screen |
| `reload_with_encoding` | Reload file with specific encoding |
| `remove_ruler` | Remove ruler |
| `remove_secondary_cursors` | Remove secondary cursors |
| `replace` | Replace text in buffer |
| `reset_buffer_settings` | Reset buffer settings to config |
| `restart_terminal` | Restart terminal process |
| `resume_live_grep` | Resume Live Grep |
| `revert` | Revert to saved file |
| `review_interrupted_saves` | Review copies kept by interrupted saves |
| `save` | Save file |
| `save_all` | Save all files |
| `save_as` | Save file as... |
| `scan_line_index` | Scan line index |
| `scroll_down` | Scroll down |
| `scroll_tabs_left` | Scroll tabs left |
| `scroll_tabs_right` | Scroll tabs right |
| `scroll_up` | Scroll up |
| `search` | Search for text in buffer |
| `select_all` | Select all |
| `select_cursor_style` | Select cursor style |
| `select_document_end` | Select to document end |
| `select_document_start` | Select to document start |
| `select_down` | Select down |
| `select_keybinding_map` | Select keybinding map |
| `select_left` | Select left |
| `select_line` | Select current line |
| `select_line_end` | Select to line end |
| `select_line_start` | Select to line start |
| `select_locale` | Select locale |
| `select_page_down` | Select page down |
| `select_page_up` | Select page up |
| `select_right` | Select right |
| `select_theme` | Select theme |
| `select_to_paragraph_down` | Select to next empty line |
| `select_to_paragraph_up` | Select to previous empty line |
| `select_up` | Select up |
| `select_word` | Select word under cursor |
| `select_word_end` | Select to word end |
| `select_word_left` | Select word left |
| `select_word_right` | Select word right |
| `send_selection_to_terminal` | Send selection to terminal |
| `set_background` | Set ANSI background file |
| `set_background_blend` | Set background blend ratio |
| `set_bookmark` | Set bookmark 'x' (args: `{ char: "x" }`) |
| `set_compose_width` | Alias of `set_page_width`. Set page width (compose width) |
| `set_encoding` | Set text encoding (UTF-8, Latin-1, etc.) |
| `set_language` | Set language/syntax highlighting |
| `set_line_ending` | Set line ending format (LF/CRLF) |
| `set_mark` | Set mark (start selection) |
| `set_page_width` | Set page width (compose width) |
| `set_tab_size` | Set tab size for current buffer |
| `settings_activate` | Activate setting |
| `settings_decrement` | Decrement value |
| `settings_help` | Show settings help |
| `settings_increment` | Increment value |
| `settings_inherit` | Inherit value (unset override) |
| `settings_reset` | Reset setting to default |
| `settings_save` | Save settings |
| `settings_search` | Search settings |
| `settings_toggle_focus` | Toggle settings panel focus |
| `shell_command` | Run shell command on buffer/selection |
| `shell_command_replace` | Run shell command and replace |
| `show_help` | Show manual |
| `show_lsp_status` | Show LSP status |
| `show_macro` | Show macro 'x' in buffer (args: `{ char: "x" }`) |
| `show_read_only_menu` | Show read-only menu |
| `show_remote_indicator_menu` | Show remote authority menu |
| `show_status_log` | Show status message log |
| `show_warnings` | Show warnings |
| `smart_home` | Smart home (toggle line start / first non-whitespace) |
| `sort_lines` | Sort lines |
| `split_horizontal` | Split horizontally |
| `split_vertical` | Split vertically |
| `start_macro_recording` | Start macro recording |
| `stop_macro_recording` | Stop macro recording |
| `suspend_process` | Suspend editor process (resume with `fg`) |
| `switch_keybinding_map` | Switch to 'emacs' keybindings (args: `{ map: "emacs" }`) |
| `switch_project` | Switch project |
| `switch_to_previous_tab` | Switch to previous tab |
| `switch_to_tab_by_name` | Switch to tab by name |
| `terminal_escape` | Exit terminal mode |
| `terminal_paste` | Paste into terminal |
| `to_lower_case` | Convert to lowercase |
| `to_upper_case` | Convert to uppercase |
| `toggle_auto_revert` | Toggle auto-revert mode (current buffer) |
| `toggle_case` | Convert to uppercase |
| `toggle_comment` | Toggle comment |
| `toggle_compose_mode` | Alias of `toggle_page_view`. Toggle page view (compose) |
| `toggle_current_line_highlight` | Toggle current line highlight |
| `toggle_current_line_highlight_current_buffer` | Toggle current line highlight (current buffer) |
| `toggle_debug_highlights` | Toggle debug highlight mode (show byte ranges) |
| `toggle_dock_focus` | Toggle orchestrator dock focus |
| `toggle_file_explorer` | Toggle file explorer |
| `toggle_file_explorer_side` | Toggle file explorer side (left/right) |
| `toggle_fold` | Toggle fold |
| `toggle_fold_indicators_current_buffer` | Toggle folding indicators (current buffer) |
| `toggle_horizontal_scrollbar` | Toggle horizontal scrollbar visibility |
| `toggle_indentation_guide_current_buffer` | Toggle indentation guides (current buffer) |
| `toggle_indentation_style` | Toggle indentation style: spaces/tabs (current buffer) |
| `toggle_inlay_hints` | Toggle inlay hints |
| `toggle_keyboard_capture` | Toggle keyboard capture (terminal) |
| `toggle_line_numbers` | Toggle line numbers |
| `toggle_line_numbers_current_buffer` | Toggle line numbers (current buffer) |
| `toggle_line_wrap` | Toggle line wrap |
| `toggle_line_wrap_current_buffer` | Toggle line wrap (current buffer) |
| `toggle_macro_recording` | Toggle macro recording for 'x' (args: `{ char: "x" }`) |
| `toggle_maximize_split` | Toggle maximize split |
| `toggle_menu_bar` | Toggle menu bar visibility |
| `toggle_mouse_capture` | Toggle mouse support |
| `toggle_mouse_hover` | Toggle LSP hover on mouse |
| `toggle_occurrence_highlight` | Toggle occurrence highlight |
| `toggle_occurrence_highlight_current_buffer` | Toggle occurrence highlight (current buffer) |
| `toggle_page_view` | Toggle page view (compose) |
| `toggle_prompt_line` | Toggle prompt line visibility |
| `toggle_read_only` | Toggle read-only mode (current buffer) |
| `toggle_scroll_sync` | Toggle scroll sync |
| `toggle_search_case_sensitive` | Toggle search case sensitivity |
| `toggle_search_confirm_each` | Toggle confirm each replacement |
| `toggle_search_regex` | Toggle search regex mode |
| `toggle_search_whole_word` | Toggle search whole word matching |
| `toggle_status_bar` | Toggle status bar visibility |
| `toggle_tab_bar` | Toggle tab bar visibility |
| `toggle_tab_indicators` | Toggle tab indicator visibility (current buffer) |
| `toggle_utility_dock` | Toggle Utility Dock |
| `toggle_vertical_scrollbar` | Toggle vertical scrollbar visibility |
| `toggle_virtual_space_current_buffer` | Toggle virtual space (current buffer) |
| `toggle_whitespace_indicators` | Toggle whitespace indicator visibility (current buffer) |
| `transpose_chars` | Transpose characters |
| `trigger_wave_animation` | Wave animation |
| `trim_trailing_whitespace` | Remove trailing whitespace from all lines |
| `undo` | Undo |
| `update_fresh` | Update fresh |
| `vi_move_down` | Move cursor down |
| `vi_move_up` | Move cursor up |
| `vi_move_word_end` | Move to word end |
| `vi_select_word_end` | Select to word end |
| `workspace_trust_block` | Block Process Execution |
| `workspace_trust_prompt` | Workspace Trust… |
| `workspace_trust_restrict` | Restrict This Folder |
| `workspace_trust_trust` | Trust This Folder |
| `yank_to_line_end` | Yank to end of line |
| `yank_to_line_start` | Yank to start of line |
| `yank_vi_word_end` | Yank word forward |
| `yank_word_backward` | Yank word backward |
| `yank_word_forward` | Yank word forward |
