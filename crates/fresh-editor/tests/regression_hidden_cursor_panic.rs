mod common;
use common::harness::EditorTestHarness;
use fresh::primitives::text_property::TextPropertyEntry;
use fresh::services::plugins::api::PluginCommand;
use std::collections::HashMap;

#[test]
fn reproduce_cursor_panic() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create a virtual buffer with show_cursors = false
    // This simulates a plugin (like file explorer or diagnostics) creating a buffer
    // that hides cursors, but is then rendered in a way that triggers the panic.
    let cmd = PluginCommand::CreateVirtualBufferWithContent {
        name: "*Test*".to_string(),
        mode: "test".to_string(),
        read_only: false,
        entries: vec![TextPropertyEntry {
            text: "Hello World".to_string(),
            properties: HashMap::new(),
        }],
        show_line_numbers: true,
        show_cursors: false, // <--- The trigger: hiding cursors
        editing_disabled: false,
        hidden_from_tabs: false,
        request_id: None,
    };

    // Execute the command to create and open the buffer
    harness.editor_mut().handle_plugin_command(cmd).unwrap();

    // Trigger a render. This should panic because the active buffer has show_cursors=false,
    // causing cursor_positions to be empty, but primary_cursor_position is still valid.
    harness.render().unwrap();
}
