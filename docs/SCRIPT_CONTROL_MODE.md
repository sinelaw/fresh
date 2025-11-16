# Script Control Mode

Script Control Mode is a special run mode for the Fresh editor that enables programmatic control via JSON commands through stdin/stdout. This is particularly useful for:

- **LLM Integration**: Allow AI assistants to interact with the editor
- **Automated Testing**: Script complex editor interactions
- **Test Generation**: Convert interactions into reproducible test cases
- **Integration Testing**: Control the editor from external tools

## Quick Start

```bash
# Start editor in script mode
cargo run -- --script-mode

# With custom terminal size
cargo run -- --script-mode --script-width 120 --script-height 40

# Open a file
cargo run -- --script-mode myfile.txt

# View available commands
cargo run -- --script-schema
```

## Command Reference

Send JSON commands to stdin, one per line. Each command returns a JSON response.

### Rendering

#### `render`
Render the current screen state.

```json
{"type": "render"}
```

Response:
```json
{
  "type": "screen",
  "content": "... screen content as string ...",
  "width": 80,
  "height": 24,
  "cursor": [8, 2]
}
```

### Input Events

#### `key`
Send a keyboard event.

```json
{"type": "key", "code": "a"}
{"type": "key", "code": "s", "modifiers": ["ctrl"]}
{"type": "key", "code": "Enter"}
{"type": "key", "code": "D", "modifiers": ["shift"]}
```

**Key codes**:
- Single characters: `"a"`, `"Z"`, `"1"`, `"@"`
- Special keys: `"Enter"`, `"Backspace"`, `"Delete"`, `"Tab"`, `"Escape"`
- Navigation: `"Left"`, `"Right"`, `"Up"`, `"Down"`, `"Home"`, `"End"`, `"PageUp"`, `"PageDown"`
- Function keys: `"F1"` through `"F12"`
- Space: `"space"`

**Modifiers**: `"ctrl"`, `"alt"`, `"shift"`, `"super"`

#### `type_text`
Type a string of text (convenience for multiple key presses).

```json
{"type": "type_text", "text": "Hello, World!"}
```

#### `mouse_click`
Click at a screen position.

```json
{"type": "mouse_click", "col": 10, "row": 5}
{"type": "mouse_click", "col": 10, "row": 5, "button": "right"}
```

**Buttons**: `"left"` (default), `"right"`, `"middle"`

#### `mouse_drag`
Drag from one position to another (for selection).

```json
{
  "type": "mouse_drag",
  "start_col": 10,
  "start_row": 5,
  "end_col": 20,
  "end_row": 5
}
```

#### `mouse_scroll`
Scroll at a position.

```json
{"type": "mouse_scroll", "col": 40, "row": 12, "direction": "down"}
{"type": "mouse_scroll", "col": 40, "row": 12, "direction": "up", "amount": 5}
```

### Editor State

#### `status`
Get editor status information.

```json
{"type": "status"}
```

Response:
```json
{
  "type": "status",
  "cursor_position": 13,
  "cursor_count": 1,
  "has_selection": false,
  "buffer_len": 13,
  "file_path": null,
  "is_modified": true
}
```

#### `get_buffer`
Get the actual buffer content.

```json
{"type": "get_buffer"}
```

Response:
```json
{
  "type": "buffer",
  "content": "Hello, World!"
}
```

#### `get_keybindings`
Get all keyboard bindings (key combinations mapped to actions).

```json
{"type": "get_keybindings"}
```

Response:
```json
{
  "type": "keybindings",
  "bindings": [
    {"key": "Ctrl+S", "action": "Save file"},
    {"key": "Ctrl+Q", "action": "Quit editor"},
    {"key": "Ctrl+F", "action": "Search for text in buffer"},
    {"key": "Esc", "action": "[fileExplorer] Focus editor"},
    {"key": "Enter", "action": "[menu] Execute selected menu item"}
  ]
}
```

Actions with context prefixes like `[menu]` or `[fileExplorer]` indicate keybindings that are only active in that specific mode.

### File Operations

#### `open_file`
Open a file in the editor.

```json
{"type": "open_file", "path": "/path/to/file.txt"}
```

### Terminal Control

#### `resize`
Resize the virtual terminal.

```json
{"type": "resize", "width": 120, "height": 40}
```

### Test Generation

#### `export_test`
Export the interaction history as Rust test code.

```json
{"type": "export_test", "test_name": "test_basic_editing"}
```

Response:
```json
{
  "type": "test_code",
  "code": "#[test]\nfn test_basic_editing() -> std::io::Result<()> {\n    let mut harness = EditorTestHarness::new(80, 24)?;\n    harness.render()?;\n\n    harness.type_text(\"Hello, World!\")?;\n    harness.render()?;\n\n    Ok(())\n}\n"
}
```

### Waiting for Conditions

#### `wait_for`
Wait for a condition to be met. Supports two approaches:

1. **Event-based** - Wait for specific ControlEvents (clean, event-driven)
2. **State-based** - Poll current state (fallback for simple checks)

```json
{"type": "wait_for", "condition": {"type": "event", "name": "lsp:status_changed", "data": {"language": "rust", "status": "running"}}}
{"type": "wait_for", "condition": {"type": "screen_contains", "text": "Error"}, "timeout_ms": 10000}
{"type": "wait_for", "condition": {"type": "popup_visible"}, "poll_interval_ms": 50}
```

**Event-based conditions** (wait for ControlEvents):

- `event` - Wait for event matching name pattern and optional data
  ```json
  {"type": "event", "name": "editor:file_saved", "data": {"path": "/tmp/test.rs"}}
  {"type": "event", "name": "editor:*", "data": {}}
  {"type": "event", "name": "lsp:status_changed", "data": {"status": "running"}}
  {"type": "event", "name": "*:error", "data": {}}
  ```

**Event name patterns**:
- Exact match: `"editor:file_opened"`, `"lsp:status_changed"`
- Prefix wildcard: `"editor:*"` matches any editor event
- Suffix wildcard: `"*:error"` matches any error event
- Any event: `"*"` matches everything

**Data pattern matching**:
- Empty object `{}` matches any data
- Partial match: `{"status": "running"}` matches if data contains that key/value
- Null values mean "key exists": `{"path": null}` matches if path key exists

**Currently emitted events**:
- `editor:file_opened` - data: `{path, buffer_id}`
- `editor:file_saved` - data: `{path}`
- `lsp:status_changed` - data: `{language, old_status, status}`

**State-based conditions** (polling fallback):

- `screen_contains` - Wait for screen to contain specific text
  ```json
  {"type": "screen_contains", "text": "Error"}
  ```

- `screen_not_contains` - Wait for screen to NOT contain specific text
  ```json
  {"type": "screen_not_contains", "text": "Loading"}
  ```

- `buffer_contains` - Wait for buffer to contain specific text
  ```json
  {"type": "buffer_contains", "text": "fn main"}
  ```

- `popup_visible` - Wait for a popup to be visible
  ```json
  {"type": "popup_visible"}
  ```

- `popup_hidden` - Wait for popup to be hidden
  ```json
  {"type": "popup_hidden"}
  ```

**Parameters**:
- `timeout_ms` - Timeout in milliseconds (default: 5000)
- `poll_interval_ms` - Poll interval in milliseconds (default: 100)

Response (success):
```json
{"type": "ok", "message": "Condition met after 150ms"}
```

Response (timeout):
```json
{"type": "error", "message": "Timeout after 5000ms waiting for condition: ..."}
```

### Session Control

#### `quit`
Quit the editor.

```json
{"type": "quit"}
```

## Response Types

All commands return JSON responses:

### Success
```json
{"type": "ok"}
{"type": "ok", "message": "Optional message"}
```

### Error
```json
{"type": "error", "message": "Error description"}
```

### Data Responses
- `screen` - Screen content and cursor position
- `status` - Editor state information
- `buffer` - Buffer text content
- `test_code` - Generated test code

## Example Session

Here's a complete example session that types some code and generates a test:

```bash
cat << EOF | cargo run -- --script-mode
{"type": "type_text", "text": "fn main() {"}
{"type": "key", "code": "Enter"}
{"type": "type_text", "text": "    println!(\"Hello!\");"}
{"type": "key", "code": "Enter"}
{"type": "type_text", "text": "}"}
{"type": "render"}
{"type": "status"}
{"type": "get_buffer"}
{"type": "export_test", "test_name": "test_write_rust_function"}
{"type": "quit"}
EOF
```

Output:
```json
{"type":"ok","message":"Script Control Mode ready. Send JSON commands to stdin."}
{"type":"ok"}
{"type":"ok"}
{"type":"ok"}
{"type":"ok"}
{"type":"ok"}
{"type":"screen","content":"... editor showing the Rust code ...","width":80,"height":24,"cursor":[9,4]}
{"type":"status","cursor_position":45,"cursor_count":1,"has_selection":false,"buffer_len":45,"file_path":null,"is_modified":true}
{"type":"buffer","content":"fn main() {\n    println!(\"Hello!\");\n}"}
{"type":"test_code","code":"#[test]\nfn test_write_rust_function() -> std::io::Result<()> {\n    let mut harness = EditorTestHarness::new(80, 24)?;\n    harness.render()?;\n\n    harness.type_text(\"fn main() {\");\n    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;\n    ...\n}\n"}
{"type":"ok","message":"Quitting editor"}
```

## Using with LLMs

Script Control Mode is designed to be used by LLMs. Here's a typical workflow:

1. **Start the editor**: Launch in script mode
2. **Get initial state**: Send `{"type": "render"}` to see the screen
3. **Interact**: Send key/mouse commands based on what the LLM sees
4. **Check results**: Render after each action to see the effect
5. **Generate tests**: Export interactions as test code

### Tips for LLM Integration

- Always render after actions to see the updated screen
- Use `status` to check cursor position and selection state
- Use `get_buffer` to verify the actual text content
- The screen output includes line numbers, menu bar, and status bar
- Cursor position in the response is the screen coordinate (x, y)
- Cursor position in status is the byte offset in the buffer

## Test Generation

One of the most powerful features is automatic test generation. Every interaction is recorded, and you can export them as Rust test code that uses the `EditorTestHarness`.

This allows you to:
1. Explore the editor behavior interactively
2. Export the exact sequence of actions
3. Save as a regression test

The generated test code is ready to be added to the test suite in `tests/e2e/`.

## Architecture

Script Control Mode uses:
- **Virtual Terminal**: `ratatui::backend::TestBackend` for screen capture
- **JSON Protocol**: `serde_json` for serialization
- **Same Editor Core**: Full editor functionality including LSP, syntax highlighting, etc.
- **Interaction Recording**: Tracks all commands with timestamps for test generation

## Command-Line Options

```
--script-mode                    Enable script control mode
--script-width <WIDTH>           Terminal width (default: 80)
--script-height <HEIGHT>         Terminal height (default: 24)
--script-schema                  Print command schema and exit
```

## Schema

To see the full command schema with all parameters and examples:

```bash
cargo run -- --script-schema | python3 -m json.tool
```

This provides documentation of all available commands in JSON format.
