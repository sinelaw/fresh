// Pane Navigation Tests (NextPane / PrevPane)
//
// These tests verify that NextPane/PrevPane cycle through every
// (split, tab) pair as if they were laid out in a single flat list.
//
// Key distinction from existing actions:
// - NextSplit/PrevSplit only moves between splits, staying on the tab
//   the split was last showing.
// - NextPane/PrevPane treats every (split, tab) combination as a unique
//   step and cycles through them in order.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

/// Helper: Open a file via command palette
fn open_file(harness: &mut EditorTestHarness, path: &std::path::Path) {
    harness.open_file(path).unwrap();
    harness.render().unwrap();
}

/// Helper: Create a vertical split via command palette
fn split_vertical(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("split vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Helper: Cycle to the next pane via command palette
fn next_pane(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("next pane").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Helper: Cycle to the previous pane via command palette
fn prev_pane(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("previous pane").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Test that NextPane cycles through tabs within the same split
#[test]
fn test_next_pane_cycles_tabs_within_split_first() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Create initial content (tab 0)
    harness.type_text("alpha content").unwrap();

    // Open a second tab with different content — this becomes the new active buffer
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("new file").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("beta content").unwrap();
    // At this point we're on tab 1 ("beta content")

    // NextPane wraps around: from tab 1 → tab 0
    next_pane(&mut harness);
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "alpha content",
        "NextPane should wrap from the last tab back to the first tab"
    );

    // The flat list: (split0, alpha), (split0, beta)
    // Next pane should go to beta
    next_pane(&mut harness);
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "beta content",
        "NextPane should cycle to the next tab in the split"
    );

    // Next pane should wrap back to alpha
    next_pane(&mut harness);
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "alpha content",
        "NextPane should wrap back to the first tab"
    );

    // Next pane should go to beta again
    next_pane(&mut harness);
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "beta content",
        "NextPane should cycle forward again"
    );
}

/// Test that NextPane cycles through splits when all tabs are exhausted
#[test]
fn test_next_pane_cycles_across_splits() {
    let temp_dir = TempDir::new().unwrap();
    let file_a = temp_dir.path().join("a.txt");
    let file_b = temp_dir.path().join("b.txt");

    std::fs::write(&file_a, "content of A").unwrap();
    std::fs::write(&file_b, "content of B").unwrap();

    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_file(&mut harness, &file_a);
    assert_eq!(harness.get_buffer_content().unwrap(), "content of A");

    // Create a vertical split — both splits initially show the same buffer (A)
    split_vertical(&mut harness);

    // Open file B in the new split
    open_file(&mut harness, &file_b);
    assert_eq!(harness.get_buffer_content().unwrap(), "content of B");

    // Navigate back to split 0 (showing A)
    prev_pane(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), "content of A");

    // The flat list is: (split0, A), (split1, B)
    // We're on (split0, A), so next pane should go to (split1, B)
    next_pane(&mut harness);
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "content of B",
        "NextPane should move to the next split's tab"
    );

    // Next pane should wrap back to (split0, A)
    next_pane(&mut harness);
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "content of A",
        "NextPane should wrap back to the first pane"
    );
}

/// Test that PrevPane cycles in reverse order with two splits.
///
/// After creating 2 splits with files A, B, the flat list of (split, tab)
/// pairs includes all tabs per split, giving 3 entries:
///
///   [(split0, A), (split1, A), (split1, B)]
///
/// Prev/NextPane cycle through this flat list circularly.
#[test]
fn test_prev_pane_two_splits_reverse() {
    let temp_dir = TempDir::new().unwrap();
    let file_a = temp_dir.path().join("a.txt");
    let file_b = temp_dir.path().join("b.txt");

    std::fs::write(&file_a, "content of A").unwrap();
    std::fs::write(&file_b, "content of B").unwrap();

    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_file(&mut harness, &file_a);

    // Create vertical split and open B in the new split
    split_vertical(&mut harness);
    open_file(&mut harness, &file_b);

    // Flat list: (split0, A), (split1, A), (split1, B)
    // We're on split1 (B). PrevPane goes to (split1, A).
    prev_pane(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), "content of A");

    // From (split1, A), PrevPane wraps to (split0, A).
    prev_pane(&mut harness);
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "content of A",
        "PrevPane wraps to (split0, A)"
    );

    // From (split0, A), PrevPane goes to (split1, B).
    prev_pane(&mut harness);
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "content of B",
        "PrevPane cycles back to B"
    );

    // From (split1, B), PrevPane goes to (split1, A) again.
    prev_pane(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), "content of A");
}

/// Test NextPane cycling through multiple splits.
///
/// After creating 3 splits with files a, b, c, the flat list of
/// (split, tab) pairs includes all tabs per split (some splits have
/// extra tabs inherited at split time), so the list has 5 entries:
///
///   [(split0, a), (split1, a), (split1, b), (split2, b), (split2, c)]
///
/// Prev/NextPane cycle through this flat list circularly.
#[test]
fn test_next_pane_three_splits() {
    let temp_dir = TempDir::new().unwrap();
    let files: Vec<_> = ["a.txt", "b.txt", "c.txt"]
        .iter()
        .map(|name| {
            let path = temp_dir.path().join(name);
            std::fs::write(&path, format!("content of {name}")).unwrap();
            path
        })
        .collect();

    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_file(&mut harness, &files[0]);

    // Create vertical split -> 2 splits
    split_vertical(&mut harness);
    open_file(&mut harness, &files[1]);

    // Create another vertical split -> 3 splits
    split_vertical(&mut harness);
    open_file(&mut harness, &files[2]);

    // Flat list: (split0, a), (split1, a), (split1, b), (split2, b), (split2, c)
    // Currently on split2 (c), prev_pane goes to (split2, b)
    prev_pane(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), "content of b.txt");

    // From (split2, b), prev_pane goes to (split1, b)
    prev_pane(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), "content of b.txt");

    // From (split1, b), prev_pane goes to (split1, a)
    prev_pane(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), "content of a.txt");

    // From (split1, a), prev_pane goes to (split0, a)
    prev_pane(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), "content of a.txt");

    // From (split0, a), prev_pane wraps to (split2, c)
    prev_pane(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), "content of c.txt");

    // From (split2, c), next_pane wraps to (split0, a)
    next_pane(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), "content of a.txt");

    // From (split0, a), next_pane goes to (split1, a)
    next_pane(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), "content of a.txt");

    // From (split1, a), next_pane goes to (split1, b)
    next_pane(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), "content of b.txt");

    // From (split1, b), next_pane goes to (split2, b)
    next_pane(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), "content of b.txt");

    // From (split2, b), next_pane goes to (split2, c)
    next_pane(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), "content of c.txt");
}

/// Test that NextPane distinguishes itself from NextSplit
#[test]
fn test_next_pane_differs_from_next_split() {
    let temp_dir = TempDir::new().unwrap();
    let file_a = temp_dir.path().join("a.txt");
    let file_b = temp_dir.path().join("b.txt");

    std::fs::write(&file_a, "content of A").unwrap();
    std::fs::write(&file_b, "content of B").unwrap();

    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_file(&mut harness, &file_a);

    // Create vertical split and open B in the new split
    split_vertical(&mut harness);
    open_file(&mut harness, &file_b);

    // Navigate back to A (split 0)
    prev_pane(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), "content of A");

    // NextPane should move to B (split 1)
    next_pane(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), "content of B");
}

/// Test prev_pane with multiple tabs in one split.
///
/// With 3 tabs in a single split, the flat list is:
///   [(split0, first), (split0, second), (split0, third)]
///
/// PrevPane cycles backwards: third → second → first → third (wrap).
#[test]
fn test_prev_pane_multiple_tabs_in_split() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();

    // Create initial content (tab 0)
    harness.type_text("first tab content").unwrap();

    // Open second tab
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("new file").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("second tab content").unwrap();

    // Open third tab
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("new file").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("third tab content").unwrap();
    // At this point we're on tab 2 ("third tab content")

    // PrevPane from third (pos 2) goes to second (pos 1)
    prev_pane(&mut harness);
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "second tab content",
        "PrevPane goes to the previous tab"
    );

    // From second (pos 1), prev_pane goes to first (pos 0)
    prev_pane(&mut harness);
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "first tab content",
        "PrevPane goes to the first tab"
    );

    // From first (pos 0), prev_pane wraps to third (pos 2)
    prev_pane(&mut harness);
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "third tab content",
        "PrevPane wraps to the last tab"
    );

    // From third (pos 2), prev_pane goes to second (pos 1)
    prev_pane(&mut harness);
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "second tab content",
        "PrevPane cycles forward again"
    );
}

/// Test that screen assertions reflect the correct file name after pane cycling
#[test]
fn test_pane_navigation_reflects_in_screen() {
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("alpha.txt");
    let file2 = temp_dir.path().join("beta.txt");

    std::fs::write(&file1, "Alpha content").unwrap();
    std::fs::write(&file2, "Beta content").unwrap();

    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    open_file(&mut harness, &file1);
    harness.assert_screen_contains("alpha.txt");

    // Create vertical split and open file2
    split_vertical(&mut harness);
    open_file(&mut harness, &file2);
    harness.assert_screen_contains("beta.txt");

    // Navigate back to file1
    prev_pane(&mut harness);
    harness.assert_screen_contains("alpha.txt");

    // Navigate forward to file2
    next_pane(&mut harness);
    harness.assert_screen_contains("beta.txt");
}
