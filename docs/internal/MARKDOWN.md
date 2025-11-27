# Markdown Compose Mode - Remaining Work

Plugin: `plugins/markdown_compose.ts`

## Pending Features

### Not Yet Started
- **Multi-pass transforms**: design allows chaining; current implementation supports single transform per viewport
- **Visual-line navigation**: up/down should operate on display lines in Compose mode; currently behaves like Source mode
- **Column guides rendering**: stored in state but not drawn
- **Context-sensitive Enter**: Enter in compose mode should be context-aware (continue lists, add bullets, double-newline for paragraphs). Requires plugin hook for key interception

### Partial Implementation
- **Wrapping as transform**: wrapping happens in renderer, not as a token-inserting transform step. Plugins cannot control wrapping strategy
- **Base token stream**: identity view uses raw string, not token format. Only plugin transforms use tokens

## Architecture Gap

The design envisions:
1. Source → base token stream (Text/Newline/Space)
2. Plugin transforms rewrite tokens (Newline → Space for soft breaks)
3. Layout transform inserts break tokens for wrapping
4. Renderer draws final token stream

**Current reality**: source → raw string (identity) OR plugin tokens, then renderer wraps during line construction. Plugins can't fully control text flow.

## Next Steps
1. **Column guides**: render vertical lines at `compose_column_guides` positions
2. **Visual navigation**: bind up/down to visual-line movement in Compose mode
3. **Markdown plugin**: parse incrementally, rewrite paragraph newlines to spaces, emit structure styling, detect hard breaks
