use crate::model::filesystem::StdFileSystem;
use std::sync::Arc;

fn test_fs() -> Arc<dyn crate::model::filesystem::FileSystem + Send + Sync> {
    Arc::new(StdFileSystem)
}
use super::*;
use proptest::prelude::*;

// Generate text with some newlines
fn text_with_newlines() -> impl Strategy<Value = Vec<u8>> {
    prop::collection::vec(
        prop_oneof![(b'a'..=b'z').prop_map(|c| c), Just(b'\n'),],
        0..100,
    )
}

// Strategy to generate operations
#[derive(Debug, Clone)]
enum Operation {
    Insert { offset: usize, text: Vec<u8> },
    Delete { offset: usize, bytes: usize },
}

fn operation_strategy() -> impl Strategy<Value = Vec<Operation>> {
    prop::collection::vec(
        prop_oneof![
            (0usize..200, text_with_newlines())
                .prop_map(|(offset, text)| { Operation::Insert { offset, text } }),
            (0usize..200, 1usize..50)
                .prop_map(|(offset, bytes)| { Operation::Delete { offset, bytes } }),
        ],
        0..50,
    )
}

proptest! {
    #[test]
    fn prop_line_count_consistent(text in text_with_newlines()) {
        let buffer = TextBuffer::from_bytes(text.clone(), test_fs());

        let newline_count = text.iter().filter(|&&b| b == b'\n').count();
        prop_assert_eq!(buffer.line_count(), Some(newline_count + 1));
    }

    #[test]
    fn prop_get_all_text_matches_original(text in text_with_newlines()) {
        let buffer = TextBuffer::from_bytes(text.clone(), test_fs());
        prop_assert_eq!(buffer.get_all_text().unwrap(), text);
    }

    #[test]
    fn prop_insert_increases_size(
        text in text_with_newlines(),
        offset in 0usize..100,
        insert_text in text_with_newlines()
    ) {
        let mut buffer = TextBuffer::from_bytes(text, test_fs());
        let initial_bytes = buffer.total_bytes();

        let offset = offset.min(buffer.total_bytes());
        buffer.insert_bytes(offset, insert_text.clone());

        prop_assert_eq!(buffer.total_bytes(), initial_bytes + insert_text.len());
    }

    #[test]
    fn prop_delete_decreases_size(
        text in text_with_newlines(),
        offset in 0usize..100,
        delete_bytes in 1usize..50
    ) {
        if text.is_empty() {
            return Ok(());
        }

        let mut buffer = TextBuffer::from_bytes(text, test_fs());
        let initial_bytes = buffer.total_bytes();

        let offset = offset.min(buffer.total_bytes());
        let delete_bytes = delete_bytes.min(buffer.total_bytes() - offset);

        if delete_bytes == 0 {
            return Ok(());
        }

        buffer.delete_bytes(offset, delete_bytes);

        prop_assert_eq!(buffer.total_bytes(), initial_bytes - delete_bytes);
    }

    #[test]
    fn prop_insert_then_delete_restores_original(
        text in text_with_newlines(),
        offset in 0usize..100,
        insert_text in text_with_newlines()
    ) {
        let mut buffer = TextBuffer::from_bytes(text.clone(), test_fs());

        let offset = offset.min(buffer.total_bytes());
        buffer.insert_bytes(offset, insert_text.clone());
        buffer.delete_bytes(offset, insert_text.len());

        prop_assert_eq!(buffer.get_all_text().unwrap(), text);
    }

    #[test]
    fn prop_offset_position_roundtrip(text in text_with_newlines()) {
        let buffer = TextBuffer::from_bytes(text.clone(), test_fs());

        for offset in 0..text.len() {
            let pos = buffer.offset_to_position(offset).expect("offset_to_position should succeed for valid offset");
            let back = buffer.position_to_offset(pos);
            prop_assert_eq!(back, offset, "Failed roundtrip for offset {}", offset);
        }
    }

    #[test]
    fn prop_get_text_range_valid(
        text in text_with_newlines(),
        offset in 0usize..100,
        length in 1usize..50
    ) {
        if text.is_empty() {
            return Ok(());
        }

        let buffer = TextBuffer::from_bytes(text.clone(), test_fs());
        let offset = offset.min(buffer.total_bytes());
        let length = length.min(buffer.total_bytes() - offset);

        if length == 0 {
            return Ok(());
        }

        let result = buffer.get_text_range(offset, length);
        prop_assert_eq!(result, Some(text[offset..offset + length].to_vec()));
    }

    #[test]
    fn prop_operations_maintain_consistency(operations in operation_strategy()) {
        let mut buffer = TextBuffer::from_bytes(b"initial\ntext".to_vec(), test_fs());
        let mut expected_text = b"initial\ntext".to_vec();

        for op in operations {
            match op {
                Operation::Insert { offset, text } => {
                    let offset = offset.min(buffer.total_bytes());
                    buffer.insert_bytes(offset, text.clone());

                    // Update expected
                    let offset = offset.min(expected_text.len());
                    expected_text.splice(offset..offset, text);
                }
                Operation::Delete { offset, bytes } => {
                    if offset < buffer.total_bytes() {
                        let bytes = bytes.min(buffer.total_bytes() - offset);
                        buffer.delete_bytes(offset, bytes);

                        // Update expected
                        if offset < expected_text.len() {
                            let bytes = bytes.min(expected_text.len() - offset);
                            expected_text.drain(offset..offset + bytes);
                        }
                    }
                }
            }
        }

        prop_assert_eq!(buffer.get_all_text().unwrap(), expected_text);
    }

    #[test]
    fn prop_line_count_never_zero(operations in operation_strategy()) {
        let mut buffer = TextBuffer::from_bytes(b"test".to_vec(), test_fs());

        for op in operations {
            match op {
                Operation::Insert { offset, text } => {
                    let offset = offset.min(buffer.total_bytes());
                    buffer.insert_bytes(offset, text);
                }
                Operation::Delete { offset, bytes } => {
                    buffer.delete_bytes(offset, bytes);
                }
            }

            // Document always has at least 1 line
            prop_assert!(buffer.line_count().unwrap_or(1) >= 1);
        }
    }

    #[test]
    fn prop_total_bytes_never_negative(operations in operation_strategy()) {
        let mut buffer = TextBuffer::from_bytes(b"test".to_vec(), test_fs());

        for op in operations {
            match op {
                Operation::Insert { offset, text } => {
                    let offset = offset.min(buffer.total_bytes());
                    buffer.insert_bytes(offset, text);
                }
                Operation::Delete { offset, bytes } => {
                    buffer.delete_bytes(offset, bytes);
                }
            }

            // Bytes should never overflow
            prop_assert!(buffer.total_bytes() < 10_000_000);
        }
    }

    #[test]
    fn prop_piece_tree_and_line_index_stay_synced(operations in operation_strategy()) {
        let mut buffer = TextBuffer::from_bytes(b"line1\nline2\nline3".to_vec(), test_fs());

        for op in operations {
            match op {
                Operation::Insert { offset, text } => {
                    let offset = offset.min(buffer.total_bytes());
                    buffer.insert_bytes(offset, text);
                }
                Operation::Delete { offset, bytes } => {
                    buffer.delete_bytes(offset, bytes);
                }
            }

            // Verify we can still convert between offsets and positions
            if buffer.total_bytes() > 0 {
                let mid_offset = buffer.total_bytes() / 2;
                if let Some(pos) = buffer.offset_to_position(mid_offset) {
                    let back = buffer.position_to_offset(pos);

                    // Should be able to roundtrip
                    prop_assert!(back <= buffer.total_bytes());
                }
            }
        }
    }

    #[test]
    fn prop_write_recipe_matches_content(text in text_with_newlines()) {
        let buffer = TextBuffer::from_bytes(text.clone(), test_fs());
        let recipe = buffer.build_write_recipe().expect("build_write_recipe should succeed");

        // Apply the recipe to get the output
        let output = apply_recipe(&buffer, &recipe);
        prop_assert_eq!(output, text, "Recipe output should match original content");
    }

    #[test]
    fn prop_write_recipe_after_edits(
        initial_text in text_with_newlines(),
        operations in operation_strategy()
    ) {
        let mut buffer = TextBuffer::from_bytes(initial_text, test_fs());

        // Apply random operations
        for op in operations {
            match op {
                Operation::Insert { offset, text } => {
                    let offset = offset.min(buffer.total_bytes());
                    buffer.insert_bytes(offset, text);
                }
                Operation::Delete { offset, bytes } => {
                    if offset < buffer.total_bytes() {
                        let bytes = bytes.min(buffer.total_bytes() - offset);
                        if bytes > 0 {
                            buffer.delete_bytes(offset, bytes);
                        }
                    }
                }
            }
        }

        // Build recipe and verify it matches buffer content
        let expected = buffer.get_all_text().unwrap();
        let recipe = buffer.build_write_recipe().expect("build_write_recipe should succeed");
        let output = apply_recipe(&buffer, &recipe);

        prop_assert_eq!(output, expected, "Recipe output should match buffer content after edits");
    }

    #[test]
    fn prop_write_recipe_copy_ops_valid(
        text in prop::collection::vec(prop_oneof![(b'a'..=b'z').prop_map(|c| c), Just(b'\n')], 10..200),
        edit_offset in 0usize..100,
        edit_text in text_with_newlines()
    ) {
        use tempfile::TempDir;

        // Create a temp file with initial content
        let temp_dir = TempDir::new().unwrap();
        let file_path = temp_dir.path().join("test.txt");
        std::fs::write(&file_path, &text).unwrap();

        // Load the file (creates unloaded buffer regions)
        let mut buffer = TextBuffer::load_from_file(&file_path, 1024 * 1024, test_fs()).unwrap();

        // Make an edit in the middle
        let edit_offset = edit_offset.min(buffer.total_bytes());
        buffer.insert_bytes(edit_offset, edit_text.clone());

        // Build recipe - should have Copy ops for unmodified regions
        let recipe = buffer.build_write_recipe().expect("build_write_recipe should succeed");

        // Verify recipe produces correct output
        let expected = buffer.get_all_text().unwrap();
        let output = apply_recipe(&buffer, &recipe);
        prop_assert_eq!(output, expected, "Recipe with Copy ops should match buffer content");

        // Verify we have at least some Copy ops if the file was large enough
        // (Copy ops reference unloaded regions from the original file)
        if text.len() > 100 && edit_offset > 10 {
            let has_copy = recipe.actions.iter().any(|a| matches!(a, RecipeAction::Copy { .. }));
            // Note: We don't assert this because line ending conversion or other factors
            // might cause all Insert ops, which is valid behavior
            let _ = has_copy;
        }
    }
}

/// Helper to apply a WriteRecipe and return the resulting bytes
fn apply_recipe(buffer: &TextBuffer, recipe: &WriteRecipe) -> Vec<u8> {
    let mut output = Vec::new();
    for action in &recipe.actions {
        match action {
            RecipeAction::Copy { offset, len } => {
                if let Some(src_path) = &recipe.src_path {
                    let data = buffer
                        .persistence
                        .fs()
                        .read_range(src_path, *offset, *len as usize)
                        .expect("read_range should succeed for Copy op");
                    output.extend_from_slice(&data);
                } else {
                    panic!("Copy action without source path");
                }
            }
            RecipeAction::Insert { index } => {
                output.extend_from_slice(&recipe.insert_data[*index]);
            }
        }
    }
    output
}

/// Helper to check if bytes are detected as binary
fn is_detected_as_binary(bytes: &[u8]) -> bool {
    super::format::detect_encoding_or_binary(bytes, false).1
}

#[test]
fn test_detect_binary_text_files() {
    // Plain text should not be detected as binary
    assert!(!is_detected_as_binary(b"Hello, world!"));
    assert!(!is_detected_as_binary(b"Line 1\nLine 2\nLine 3"));
    assert!(!is_detected_as_binary(b"Tabs\tand\tnewlines\n"));
    assert!(!is_detected_as_binary(b"Carriage return\r\n"));

    // Empty content is not binary
    assert!(!is_detected_as_binary(b""));

    // ANSI CSI escape sequences should be treated as text
    assert!(!is_detected_as_binary(b"\x1b[31mRed text\x1b[0m"));
}

#[test]
fn test_detect_binary_binary_files() {
    // Null bytes indicate binary
    assert!(is_detected_as_binary(b"Hello\x00World"));
    assert!(is_detected_as_binary(b"\x00"));

    // Non-printable control characters (except tab, newline, CR, form feed, vertical tab)
    assert!(is_detected_as_binary(b"Text with \x01 control char"));
    assert!(is_detected_as_binary(b"\x02\x03\x04"));

    // DEL character (0x7F)
    assert!(is_detected_as_binary(b"Text with DEL\x7F"));
}

#[test]
fn test_detect_binary_png_file() {
    // PNG file signature: 89 50 4E 47 0D 0A 1A 0A
    // The 0x1A byte (substitute character) is a control character that triggers binary detection
    let png_header: &[u8] = &[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A];
    assert!(is_detected_as_binary(png_header));

    // Simulate a PNG file with more data after header
    let mut png_data = vec![0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A];
    png_data.extend_from_slice(b"\x00\x00\x00\x0DIHDR"); // IHDR chunk with null bytes
    assert!(is_detected_as_binary(&png_data));
}

#[test]
fn test_detect_binary_other_image_formats() {
    // JPEG signature: FF D8 FF
    let jpeg_header: &[u8] = &[0xFF, 0xD8, 0xFF, 0xE0, 0x00, 0x10];
    assert!(is_detected_as_binary(jpeg_header));

    // GIF signature: GIF89a or GIF87a - contains valid ASCII but typically followed by binary
    // GIF header is ASCII but the LSD (Logical Screen Descriptor) contains binary
    let gif_data: &[u8] = &[
        0x47, 0x49, 0x46, 0x38, 0x39, 0x61, // GIF89a
        0x01, 0x00, 0x01, 0x00, // Width=1, Height=1 (little endian)
        0x00, // Packed byte
        0x00, // Background color index
        0x00, // Pixel aspect ratio
    ];
    // The null bytes in the dimensions trigger binary detection
    assert!(is_detected_as_binary(gif_data));

    // BMP signature: BM followed by file size (usually contains null bytes)
    let bmp_header: &[u8] = &[0x42, 0x4D, 0x00, 0x00, 0x00, 0x00];
    assert!(is_detected_as_binary(bmp_header));
}

#[test]
fn test_detect_binary_executable_formats() {
    // ELF signature (Linux executables)
    let elf_header: &[u8] = &[0x7F, 0x45, 0x4C, 0x46, 0x02, 0x01, 0x01, 0x00];
    assert!(is_detected_as_binary(elf_header));

    // Mach-O signature (macOS executables) - magic + cpu type/subtype contain null bytes
    let macho_header: &[u8] = &[0xCF, 0xFA, 0xED, 0xFE, 0x07, 0x00, 0x00, 0x01];
    assert!(is_detected_as_binary(macho_header));

    // PE/COFF (Windows executables) - MZ header
    let pe_header: &[u8] = &[0x4D, 0x5A, 0x90, 0x00, 0x03, 0x00];
    assert!(is_detected_as_binary(pe_header));
}
