use crate::common::harness::EditorTestHarness;
use tempfile::TempDir;

/// Test that PNG files are detected as binary and opened in read-only mode
#[test]
fn test_png_file_detected_as_binary() {
    let temp_dir = TempDir::new().unwrap();
    let png_path = temp_dir.path().join("test.png");

    // Write a minimal valid PNG file
    // PNG signature: 89 50 4E 47 0D 0A 1A 0A
    // The 0x1A byte (SUB control character) triggers binary detection
    let png_data: &[u8] = &[
        0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, // PNG signature
        0x00, 0x00, 0x00, 0x0D, // IHDR chunk length
        0x49, 0x48, 0x44, 0x52, // "IHDR"
        0x00, 0x00, 0x00, 0x01, // width = 1
        0x00, 0x00, 0x00, 0x01, // height = 1
        0x08, 0x02, // bit depth = 8, color type = 2 (RGB)
        0x00, 0x00, 0x00, // compression, filter, interlace
        0x90, 0x77, 0x53, 0xDE, // CRC
    ];
    std::fs::write(&png_path, png_data).unwrap();

    // Use wider terminal to see full status message
    let mut harness = EditorTestHarness::new(120, 24).unwrap();
    harness.open_file(&png_path).unwrap();
    harness.render().unwrap();

    // Verify the file is detected as binary by checking editing is disabled
    assert!(
        harness.editor().is_editing_disabled(),
        "Binary file should have editing disabled"
    );

    // Verify tab shows [BIN] indicator
    harness.assert_screen_contains("[BIN]");

    // Verify status bar shows binary file indicator
    harness.assert_screen_contains("[BIN]");
}

/// Test that JPEG files are detected as binary
#[test]
fn test_jpeg_file_detected_as_binary() {
    let temp_dir = TempDir::new().unwrap();
    let jpeg_path = temp_dir.path().join("test.jpg");

    // JPEG signature with null bytes that trigger binary detection
    let jpeg_data: &[u8] = &[
        0xFF, 0xD8, 0xFF, 0xE0, // JPEG SOI + APP0 marker
        0x00, 0x10, // APP0 length (16 bytes) - null byte triggers binary
        0x4A, 0x46, 0x49, 0x46, 0x00, // "JFIF\0"
        0x01, 0x01, // version
        0x00, // aspect ratio units
        0x00, 0x01, // X density
        0x00, 0x01, // Y density
        0x00, 0x00, // thumbnail dimensions
    ];
    std::fs::write(&jpeg_path, jpeg_data).unwrap();

    let mut harness = EditorTestHarness::new(120, 24).unwrap();
    harness.open_file(&jpeg_path).unwrap();
    harness.render().unwrap();

    assert!(
        harness.editor().is_editing_disabled(),
        "JPEG file should have editing disabled"
    );
    harness.assert_screen_contains("[BIN]");
}

/// Test that ELF executables are detected as binary
#[test]
fn test_elf_executable_detected_as_binary() {
    let temp_dir = TempDir::new().unwrap();
    let elf_path = temp_dir.path().join("test_binary");

    // ELF header with null bytes
    let elf_data: &[u8] = &[
        0x7F, 0x45, 0x4C, 0x46, // ELF magic
        0x02, // 64-bit
        0x01, // little endian
        0x01, // ELF version
        0x00, // OS/ABI - null byte triggers binary detection
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // padding
    ];
    std::fs::write(&elf_path, elf_data).unwrap();

    let mut harness = EditorTestHarness::new(120, 24).unwrap();
    harness.open_file(&elf_path).unwrap();
    harness.render().unwrap();

    assert!(
        harness.editor().is_editing_disabled(),
        "ELF binary should have editing disabled"
    );
    harness.assert_screen_contains("[BIN]");
}

/// Test that regular text files are NOT detected as binary
#[test]
fn test_text_file_not_detected_as_binary() {
    let temp_dir = TempDir::new().unwrap();
    let text_path = temp_dir.path().join("test.txt");

    std::fs::write(&text_path, "Hello, world!\nThis is a text file.\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&text_path).unwrap();
    harness.render().unwrap();

    // Text files should allow editing
    assert!(
        !harness.editor().is_editing_disabled(),
        "Text file should allow editing"
    );

    // Status bar should NOT show binary indicator
    harness.assert_screen_not_contains("binary");
}

/// Test that files with ANSI escape sequences are NOT detected as binary
#[test]
fn test_ansi_escape_sequences_not_binary() {
    let temp_dir = TempDir::new().unwrap();
    let ansi_path = temp_dir.path().join("colored.txt");

    // File with ANSI color codes (CSI sequences)
    let ansi_content = "\x1b[31mRed text\x1b[0m\n\x1b[32mGreen text\x1b[0m\n";
    std::fs::write(&ansi_path, ansi_content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&ansi_path).unwrap();
    harness.render().unwrap();

    // ANSI files should allow editing (not binary)
    assert!(
        !harness.editor().is_editing_disabled(),
        "File with ANSI escape sequences should allow editing"
    );
}

/// Test that typing is blocked in binary files
#[test]
fn test_typing_blocked_in_binary_file() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let temp_dir = TempDir::new().unwrap();
    let png_path = temp_dir.path().join("test.png");

    // Minimal PNG data
    let png_data: &[u8] = &[
        0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, 0x00, 0x00, 0x00, 0x00,
    ];
    std::fs::write(&png_path, png_data).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&png_path).unwrap();

    let initial_len = harness.buffer_len();

    // Try to type - should be blocked
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::NONE)
        .unwrap();

    // Buffer length should not change
    assert_eq!(
        harness.buffer_len(),
        initial_len,
        "Typing should be blocked in binary files"
    );
}

/// Test that binary bytes are rendered as <XX> format
#[test]
fn test_binary_bytes_rendered_as_hex() {
    let temp_dir = TempDir::new().unwrap();
    let bin_path = temp_dir.path().join("test.bin");

    // Create a file with specific bytes that we can verify in the rendering:
    // 0x89 (high byte), 0x50 ('P'), 0x4E ('N'), 0x47 ('G'), 0x0D (CR), 0x0A (LF), 0x1A (SUB), 0x0A (LF)
    // This is the PNG signature - we should see <89>PNG<0D><0A><1A><0A>
    // Note: 0x0D (CR) and 0x0A (newline) are allowed whitespace, so they won't be rendered as hex
    let bin_data: &[u8] = &[0x89, 0x50, 0x4E, 0x47, 0x00, 0x01, 0x7F];
    std::fs::write(&bin_path, bin_data).unwrap();

    let mut harness = EditorTestHarness::new(120, 24).unwrap();
    harness.open_file(&bin_path).unwrap();
    harness.render().unwrap();

    // The screen should contain <89> for the first byte (high byte, not valid UTF-8)
    harness.assert_screen_contains("<89>");

    // The screen should contain PNG (printable ASCII is shown as-is)
    harness.assert_screen_contains("PNG");

    // The screen should contain <00> for the null byte
    harness.assert_screen_contains("<00>");

    // The screen should contain <01> for the SOH control character
    harness.assert_screen_contains("<01>");

    // The screen should contain <7F> for the DEL character
    harness.assert_screen_contains("<7F>");
}

/// Test that scrolling through binary files doesn't cause rendering artifacts
/// This validates:
/// 1. Gutter line numbers remain consistent (format: "    N │")
/// 2. Content doesn't overflow into the gutter
/// 3. Screen is identical after scrolling down and back up
/// 4. VT100 parser sees correct output (catches ANSI escape sequence bugs)
#[test]
fn test_binary_file_scrolling_no_artifacts() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let temp_dir = TempDir::new().unwrap();
    let png_path = temp_dir.path().join("test.png");

    // Create a realistic PNG-like file with multiple lines of binary data
    // This simulates a real binary file with multiple newlines creating many "lines"
    let mut png_data = Vec::new();

    // PNG signature
    png_data.extend_from_slice(&[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]);

    // Generate enough binary data to require scrolling (multiple "lines")
    // Add newlines periodically to create multiple lines of binary content
    for i in 0..100 {
        // Add some binary bytes
        for j in 0..20 {
            png_data.push(((i * 20 + j) % 256) as u8);
        }
        // Add a newline to create a new "line"
        png_data.push(0x0A);
    }

    std::fs::write(&png_path, &png_data).unwrap();

    // Use a standard terminal size
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&png_path).unwrap();

    // Use render_real() which processes through VT100 parser for accurate terminal simulation
    harness.render_real().unwrap();

    // Capture initial screen using VT100 parser (more accurate than test backend)
    let initial_screen = harness.vt100_screen_to_string();

    // Validate initial gutter format
    validate_gutter_format(&initial_screen, "initial");

    // Verify test backend matches VT100 output
    harness.assert_test_matches_real();

    // Scroll down several times
    for i in 0..5 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        harness.render_real().unwrap();
        let screen = harness.vt100_screen_to_string();
        validate_gutter_format(&screen, &format!("after PageDown #{}", i + 1));
        harness.assert_test_matches_real();
    }

    // Scroll back up to the beginning
    for i in 0..5 {
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::NONE)
            .unwrap();
        harness.render_real().unwrap();
        let screen = harness.vt100_screen_to_string();
        validate_gutter_format(&screen, &format!("after PageUp #{}", i + 1));
        harness.assert_test_matches_real();
    }

    // Go to beginning of file using Home key
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render_real().unwrap();

    // Capture final screen after scrolling back
    let final_screen = harness.vt100_screen_to_string();

    // Validate final gutter format
    validate_gutter_format(&final_screen, "final");
    harness.assert_test_matches_real();

    // Compare screens cell by cell for exact match
    compare_screens_cell_by_cell(&initial_screen, &final_screen, 80, 24);
}

/// Compare two screens cell by cell to catch any rendering differences
fn compare_screens_cell_by_cell(initial: &str, final_screen: &str, width: usize, height: usize) {
    let initial_lines: Vec<&str> = initial.lines().collect();
    let final_lines: Vec<&str> = final_screen.lines().collect();

    assert_eq!(
        initial_lines.len(),
        final_lines.len(),
        "Screen height mismatch: initial has {} lines, final has {} lines",
        initial_lines.len(),
        final_lines.len()
    );

    let mut differences = Vec::new();

    for row in 0..height.min(initial_lines.len()) {
        let initial_line = initial_lines.get(row).unwrap_or(&"");
        let final_line = final_lines.get(row).unwrap_or(&"");

        if initial_line != final_line {
            let initial_chars: Vec<char> = initial_line.chars().collect();
            let final_chars: Vec<char> = final_line.chars().collect();

            for col in 0..width.max(initial_chars.len()).max(final_chars.len()) {
                let ic = initial_chars.get(col).copied().unwrap_or(' ');
                let fc = final_chars.get(col).copied().unwrap_or(' ');
                if ic != fc {
                    differences.push(format!(
                        "  Row {}, Col {}: initial '{}' (U+{:04X}) vs final '{}' (U+{:04X})",
                        row, col, ic, ic as u32, fc, fc as u32
                    ));
                }
            }
        }
    }

    if !differences.is_empty() {
        panic!(
            "Screen differs after scrolling!\n\nDifferences:\n{}\n\nInitial screen:\n{}\n\nFinal screen:\n{}",
            differences.join("\n"),
            initial,
            final_screen
        );
    }
}

/// Helper to validate gutter format in screen output
fn validate_gutter_format(screen: &str, context: &str) {
    let lines: Vec<&str> = screen.lines().collect();

    // Skip menu bar (line 0), tab bar (line 1), and check content lines
    // Also skip status bar and prompt line at the bottom
    let content_start = 2;
    let content_end = lines.len().saturating_sub(2);

    for (i, line) in lines.iter().enumerate() {
        if i < content_start || i >= content_end {
            continue;
        }

        // Skip empty lines and tilde lines (EOF markers)
        if line.trim().is_empty() || line.trim().starts_with('~') {
            continue;
        }

        // Each content line should have a gutter separator "│"
        let bar_pos = line.find('│');
        assert!(
            bar_pos.is_some(),
            "{}: Line {} is missing gutter separator │.\nLine: '{}'\n\nFull screen:\n{}",
            context,
            i,
            line,
            screen
        );

        let bar_pos = bar_pos.unwrap();
        let before_bar = &line[..bar_pos];

        // Before the bar should only contain spaces and optionally digits (line number)
        let invalid_chars: Vec<char> = before_bar
            .chars()
            .filter(|c| !c.is_ascii_whitespace() && !c.is_ascii_digit())
            .collect();
        assert!(
            invalid_chars.is_empty(),
            "{}: Line {} has content overflowing into gutter.\nGutter area: '{}'\nInvalid chars: {:?}\nLine: '{}'\n\nFull screen:\n{}",
            context, i, before_bar, invalid_chars, line, screen
        );
    }
}
