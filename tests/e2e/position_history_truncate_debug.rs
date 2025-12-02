use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

#[test]
fn test_debug_truncate() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Create 3 buffers
    println!("Creating Buffer 1");
    harness.type_text("Buffer 1").unwrap();
    println!(
        "Buffer 1 content: {:?}",
        harness.get_buffer_content().unwrap()
    );
    let hist = &harness.editor().position_history;
    println!(
        "History: len={}, current_idx={:?}, can_back={}, can_fwd={}",
        hist.len(),
        hist.current_index(),
        hist.can_go_back(),
        hist.can_go_forward()
    );

    println!("\nCreating Buffer 2");
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Buffer 2").unwrap();
    println!(
        "Buffer 2 content: {:?}",
        harness.get_buffer_content().unwrap()
    );
    let hist = &harness.editor().position_history;
    println!(
        "History: len={}, current_idx={:?}, can_back={}, can_fwd={}",
        hist.len(),
        hist.current_index(),
        hist.can_go_back(),
        hist.can_go_forward()
    );

    println!("\nCreating Buffer 3");
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Buffer 3").unwrap();
    println!(
        "Buffer 3 content: {:?}",
        harness.get_buffer_content().unwrap()
    );
    let hist = &harness.editor().position_history;
    println!(
        "History: len={}, current_idx={:?}, can_back={}, can_fwd={}",
        hist.len(),
        hist.current_index(),
        hist.can_go_back(),
        hist.can_go_forward()
    );

    // Navigate back twice
    println!("\n=== Navigate back (first) ===");
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    println!(
        "After first back: content = {:?}",
        harness.get_buffer_content().unwrap()
    );
    let hist = &harness.editor().position_history;
    println!(
        "History: len={}, current_idx={:?}, can_back={}, can_fwd={}",
        hist.len(),
        hist.current_index(),
        hist.can_go_back(),
        hist.can_go_forward()
    );

    println!("\n=== Navigate back (second) ===");
    harness.send_key(KeyCode::Left, KeyModifiers::ALT).unwrap();
    println!(
        "After second back: content = {:?}",
        harness.get_buffer_content().unwrap()
    );
    let hist = &harness.editor().position_history;
    println!(
        "History: len={}, current_idx={:?}, can_back={}, can_fwd={}",
        hist.len(),
        hist.current_index(),
        hist.can_go_back(),
        hist.can_go_forward()
    );
    assert_eq!(harness.get_buffer_content().unwrap(), "Buffer 1");

    // Create a new buffer - this should truncate forward history
    println!("\n=== Create Buffer 4 (should truncate forward history) ===");
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Buffer 4").unwrap();
    println!(
        "Buffer 4 content: {:?}",
        harness.get_buffer_content().unwrap()
    );
    let hist = &harness.editor().position_history;
    println!(
        "History: len={}, current_idx={:?}, can_back={}, can_fwd={}",
        hist.len(),
        hist.current_index(),
        hist.can_go_back(),
        hist.can_go_forward()
    );

    // Try to navigate forward - should not be able to go to Buffer 2 or 3
    println!("\n=== Navigate forward (should stay in Buffer 4) ===");
    harness.send_key(KeyCode::Right, KeyModifiers::ALT).unwrap();
    println!(
        "After forward: content = {:?}",
        harness.get_buffer_content().unwrap()
    );
    let hist = &harness.editor().position_history;
    println!(
        "History: len={}, current_idx={:?}, can_back={}, can_fwd={}",
        hist.len(),
        hist.current_index(),
        hist.can_go_back(),
        hist.can_go_forward()
    );

    // Should still be in Buffer 4 (at the end of history)
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "Buffer 4",
        "Should still be in Buffer 4 after forward"
    );
}
