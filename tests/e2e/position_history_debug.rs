use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

#[test]
fn test_debug_position_history() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Buffer 1
    harness.type_text("Buffer 1").unwrap();
    println!("Buffer 1: cursor at {}", harness.cursor_position());

    // Create Buffer 2
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    println!(
        "After Ctrl+N: cursor at {}, content: {:?}",
        harness.cursor_position(),
        harness.get_buffer_content().unwrap()
    );

    harness.type_text("Buffer 2").unwrap();
    println!("Buffer 2: cursor at {}", harness.cursor_position());

    // Create Buffer 3
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    println!(
        "After Ctrl+N: cursor at {}, content: {:?}",
        harness.cursor_position(),
        harness.get_buffer_content().unwrap()
    );

    harness.type_text("Buffer 3").unwrap();
    println!(
        "Buffer 3: cursor at {}, content: {:?}",
        harness.cursor_position(),
        harness.get_buffer_content().unwrap()
    );

    // Navigate back
    println!("\nNavigating back...");
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    println!(
        "After Alt+Left: cursor at {}, content: {:?}",
        harness.cursor_position(),
        harness.get_buffer_content().unwrap()
    );

    // Check content
    let content = harness.get_buffer_content().unwrap();
    println!("Final content: {content:?}");
    assert_eq!(
        content, "Buffer 2",
        "Should be in Buffer 2 after navigating back"
    );
}
