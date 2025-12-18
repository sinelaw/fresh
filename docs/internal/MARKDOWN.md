# Markdown Compose Mode - Remaining Work

Plugin: `plugins/markdown_compose.ts`

## Pending Features

### Not Yet Started
- **Multi-pass transforms**: design allows chaining; current implementation supports single transform per viewport
- **Visual-line navigation**: up/down should operate on display lines in Compose mode; currently behaves like Source mode
- **Column guides rendering**: stored in state but not drawn
- **Context-sensitive Enter**: Enter in compose mode should be context-aware (continue lists, add bullets, double-newline for paragraphs). Likely implemented via a compose-specific keybinding (e.g. `Action::PluginAction`) rather than “intercepting raw keys”.

### Partial Implementation
- **Wrapping as transform**: wrapping happens in renderer, not as a token-inserting transform step. Plugins cannot control wrapping strategy
- **Base token stream**: renderer can build a base token stream for the viewport (used for plugin view transforms), but the “identity path” does not force tokens through an explicit multi-stage pipeline.

## Architecture Gap

The design envisions:
1. Source → base token stream (Text/Newline/Space)
2. Plugin transforms rewrite tokens (Newline → Space for soft breaks)
3. Layout transform inserts break tokens for wrapping
4. Renderer draws final token stream

**Current reality (as of current code)**:
- The renderer fires `view_transform_request` during render and provides viewport tokens.
- Plugin hooks are fire-and-forget (non-blocking); plugins respond by sending `PluginCommand::SubmitViewTransform`.
- Plugin commands are drained in `Editor::process_async_messages()` before a later render, so view transforms are effectively applied on a subsequent frame (and may lag by a frame under load).
- Line wrapping is still a renderer concern; plugins can shape tokens (e.g., inject soft breaks) but don’t own the wrapping algorithm end-to-end.

## Next Steps
1. **Column guides**: render vertical lines at `compose_column_guides` positions
2. **Visual navigation**: bind up/down to visual-line movement in Compose mode
3. **Markdown plugin**: parse incrementally, rewrite paragraph newlines to spaces, emit structure styling, detect hard breaks
