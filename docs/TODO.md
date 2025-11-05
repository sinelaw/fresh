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
