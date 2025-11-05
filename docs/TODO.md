# TODO

## Completed Features ✅

Core editing, multi-cursor, event-driven architecture, LSP integration (diagnostics, completion, go-to-def), file explorer with gitignore support, split views, syntax highlighting, command palette, configuration system, themes, position history, comprehensive testing.

## Current Focus

### File Explorer Polish
- [ ] Input dialog system for custom file/directory names
- [ ] Copy/move operations
- [ ] File watching for auto-refresh
- [ ] Search/filter within explorer

### LSP Features
- [ ] Hover documentation
- [ ] Code actions
- [ ] Find references
- [ ] Rename refactoring
- [ ] Signature help
- [ ] Inlay hints

### Editor Features
- [ ] Search & replace with regex
- [ ] Rectangular selection (Alt+drag)
- [ ] Auto-indent on newline
- [ ] Bracket matching/auto-close
- [ ] Smart home key
- [ ] Toggle comment (language-aware)

### Test Infrastructure
- [ ] **Fix async file loading in test harness**: Currently 6 tests are ignored due to async file loading not working properly in tests:
  - `test_file_explorer_displays_opened_file_content` - file explorer doesn't load file content synchronously
  - `test_git_find_file_actually_opens_file` - git find file doesn't load buffer content
  - `test_git_grep_opens_correct_file_and_jumps_to_line` - git grep doesn't load file
  - `test_git_grep_cursor_position_accuracy` - git grep doesn't load file
  - `test_git_grep_shows_results` - git grep doesn't show file content
  - The test harness needs a way to properly wait for/force async file operations to complete

- [ ] **Fix BIG.txt generation timing**: 2 scrolling tests fail when run with other tests:
  - `test_jump_to_eof_large_file` - passes individually, fails in suite
  - `test_line_numbers_absolute_after_jump_to_beginning` - passes individually, fails in suite
  - Issue: BIG.txt (61MB test file) generation interferes with other tests
  - Solution: Better test isolation or pre-generated fixtures

- [ ] **Support independent buffers per split**: Currently architectural limitation:
  - `test_margin_per_buffer_in_split_view` - expects different files in different splits
  - Current behavior: All splits display the same active buffer
  - Need to implement per-split buffer management if this is desired functionality

### Code Organization
- [x] Extract UI rendering (~430 lines → 6 modules)
- [x] Extract commands & prompts (~335 lines → 2 modules)
- [ ] Create BufferView (~500 lines)
- [ ] Extract multi-cursor operations (~200 lines)

### Polish
- [ ] Improve error messages
- [ ] Confirmation dialogs
- [ ] Progress indicators
- [ ] Welcome screen
- [ ] More themes

## Future Ideas

- Macros (record/play)
- Git integration (status, blame, stage hunks)
- Remote file editing (SSH, SFTP)
- Collaborative editing
- Plugin system
