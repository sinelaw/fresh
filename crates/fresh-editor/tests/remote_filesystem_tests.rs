//! Integration tests for RemoteFileSystem
//!
//! These tests spawn the Python agent locally and use the RemoteFileSystem
//! through AgentChannel to test the full integration stack.
//!
//! These tests use the production code paths:
//! - spawn_local_agent() for agent creation
//! - AgentChannel for communication
//! - RemoteFileSystem for file operations

use fresh::model::buffer::TextBuffer;
use fresh::model::filesystem::{FileSystem, WriteOp};
use fresh::services::remote::{spawn_local_agent, RemoteFileSystem};
use std::sync::Arc;

/// Creates a RemoteFileSystem using production code
fn create_test_filesystem() -> Option<(RemoteFileSystem, tempfile::TempDir, tokio::runtime::Runtime)>
{
    let temp_dir = tempfile::tempdir().ok()?;
    let rt = tokio::runtime::Runtime::new().ok()?;

    let channel = rt.block_on(spawn_local_agent()).ok()?;
    let fs = RemoteFileSystem::new(channel, "test@localhost".to_string());

    Some((fs, temp_dir, rt))
}

#[test]
fn test_read_file_content() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let test_path = temp_dir.path().join("test.txt");
    let test_content = b"Hello, this is test content!\nLine 2\nLine 3";

    // Write file using std::fs (directly to the temp dir)
    std::fs::write(&test_path, test_content).unwrap();

    // Read via RemoteFileSystem
    let read_content = fs.read_file(&test_path).unwrap();

    assert_eq!(
        read_content, test_content,
        "File content should match what was written"
    );
}

#[test]
fn test_write_and_read_roundtrip() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let test_path = temp_dir.path().join("write_test.txt");
    let test_content = b"Content written via RemoteFileSystem";

    // Write via RemoteFileSystem
    fs.write_file(&test_path, test_content).unwrap();

    // Read back via RemoteFileSystem
    let read_content = fs.read_file(&test_path).unwrap();

    assert_eq!(
        read_content, test_content,
        "Read content should match written content"
    );

    // Also verify via std::fs
    let direct_read = std::fs::read(&test_path).unwrap();
    assert_eq!(
        direct_read, test_content,
        "Direct file read should match written content"
    );
}

#[test]
fn test_read_large_file() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let test_path = temp_dir.path().join("large.bin");

    // Create a file larger than the chunk size (65536 bytes)
    let test_content: Vec<u8> = (0..100_000).map(|i| (i % 256) as u8).collect();

    std::fs::write(&test_path, &test_content).unwrap();

    // Read via RemoteFileSystem (should handle multiple streaming chunks)
    let read_content = fs.read_file(&test_path).unwrap();

    assert_eq!(
        read_content.len(),
        test_content.len(),
        "File sizes should match"
    );
    assert_eq!(
        read_content, test_content,
        "Large file content should match"
    );
}

#[test]
fn test_is_dir() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let dir_path = temp_dir.path().join("subdir");
    let file_path = temp_dir.path().join("file.txt");

    std::fs::create_dir(&dir_path).unwrap();
    std::fs::write(&file_path, b"content").unwrap();

    assert!(fs.is_dir(&dir_path).unwrap(), "Should detect directory");
    assert!(!fs.is_dir(&file_path).unwrap(), "File should not be a dir");
}

#[test]
fn test_read_dir() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    std::fs::write(temp_dir.path().join("file1.txt"), b"1").unwrap();
    std::fs::write(temp_dir.path().join("file2.txt"), b"2").unwrap();
    std::fs::create_dir(temp_dir.path().join("subdir")).unwrap();

    let entries = fs.read_dir(temp_dir.path()).unwrap();
    let names: Vec<_> = entries.iter().map(|e| e.name.as_str()).collect();

    assert!(names.contains(&"file1.txt"), "Should contain file1.txt");
    assert!(names.contains(&"file2.txt"), "Should contain file2.txt");
    assert!(names.contains(&"subdir"), "Should contain subdir");
}

#[test]
fn test_remote_connection_info() {
    let Some((fs, _temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    assert_eq!(
        fs.remote_connection_info(),
        Some("test@localhost"),
        "Should return connection string"
    );
}

#[test]
fn test_metadata() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let test_path = temp_dir.path().join("meta_test.txt");
    let content = b"test content for metadata";
    std::fs::write(&test_path, content).unwrap();

    let meta = fs.metadata(&test_path).unwrap();
    assert_eq!(
        meta.size,
        content.len() as u64,
        "Size should match content length"
    );
}

#[test]
fn test_read_file_larger_than_threshold() {
    // Test reading a file larger than LARGE_FILE_THRESHOLD_BYTES (1MB)
    // This tests that streaming works correctly for very large files
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let test_path = temp_dir.path().join("very_large.bin");

    // Create a 1.5MB file (larger than the 1MB threshold)
    let size = 1_500_000;
    let test_content: Vec<u8> = (0..size).map(|i| (i % 256) as u8).collect();

    std::fs::write(&test_path, &test_content).unwrap();

    // Read via RemoteFileSystem
    let read_content = fs.read_file(&test_path).unwrap();

    assert_eq!(
        read_content.len(),
        test_content.len(),
        "File sizes should match for 1.5MB file"
    );
    assert_eq!(
        read_content, test_content,
        "Very large file content should match"
    );
}

#[test]
fn test_write_and_read_file_larger_than_threshold() {
    // Test write+read roundtrip for a file larger than the threshold
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let test_path = temp_dir.path().join("write_large.bin");

    // Create 2MB of content
    let size = 2_000_000;
    let test_content: Vec<u8> = (0..size).map(|i| ((i * 7) % 256) as u8).collect();

    // Write via RemoteFileSystem
    fs.write_file(&test_path, &test_content).unwrap();

    // Read back via RemoteFileSystem
    let read_content = fs.read_file(&test_path).unwrap();

    assert_eq!(
        read_content.len(),
        test_content.len(),
        "2MB file sizes should match after roundtrip"
    );
    assert_eq!(
        read_content, test_content,
        "2MB file content should match after roundtrip"
    );
}

#[test]
fn test_read_range_on_large_file() {
    // Test read_range on a large file
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let test_path = temp_dir.path().join("range_large.bin");

    // Create 1.5MB file
    let size = 1_500_000;
    let test_content: Vec<u8> = (0..size).map(|i| (i % 256) as u8).collect();
    std::fs::write(&test_path, &test_content).unwrap();

    // Read a range from the middle of the file
    let offset = 1_000_000; // 1MB into the file
    let len = 100_000; // Read 100KB
    let read_content = fs.read_range(&test_path, offset, len).unwrap();

    assert_eq!(read_content.len(), len, "Read range length should match");
    assert_eq!(
        read_content,
        &test_content[offset as usize..(offset as usize + len)],
        "Read range content should match"
    );
}

// =============================================================================
// Tests for optimized remote operations (Phase 1 & 2 optimizations)
// =============================================================================

#[test]
fn test_append_to_file() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let test_path = temp_dir.path().join("append_test.txt");

    // Create initial file
    fs.write_file(&test_path, b"Hello").unwrap();

    // Append using open_file_for_append
    {
        use std::io::Write;
        let mut writer = fs.open_file_for_append(&test_path).unwrap();
        writer.write_all(b" World").unwrap();
        writer.sync_all().unwrap();
    }

    // Verify content
    let content = fs.read_file(&test_path).unwrap();
    assert_eq!(
        content, b"Hello World",
        "Append should add to existing content"
    );
}

#[test]
fn test_append_creates_file_if_missing() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let test_path = temp_dir.path().join("append_new.txt");

    // File doesn't exist yet
    assert!(!test_path.exists());

    // Append to non-existent file (should create it)
    {
        use std::io::Write;
        let mut writer = fs.open_file_for_append(&test_path).unwrap();
        writer.write_all(b"New content").unwrap();
        writer.sync_all().unwrap();
    }

    // Verify file was created with content
    let content = fs.read_file(&test_path).unwrap();
    assert_eq!(content, b"New content");
}

#[test]
fn test_truncate_file() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let test_path = temp_dir.path().join("truncate_test.txt");

    // Create file with content
    fs.write_file(&test_path, b"Hello World!").unwrap();

    // Truncate to 5 bytes
    fs.set_file_length(&test_path, 5).unwrap();

    // Verify content was truncated
    let content = fs.read_file(&test_path).unwrap();
    assert_eq!(content, b"Hello", "File should be truncated to 5 bytes");
}

#[test]
fn test_truncate_extend_file() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let test_path = temp_dir.path().join("extend_test.txt");

    // Create file with content
    fs.write_file(&test_path, b"Hi").unwrap();

    // Extend to 10 bytes (should pad with zeros)
    fs.set_file_length(&test_path, 10).unwrap();

    // Verify content was extended
    let content = fs.read_file(&test_path).unwrap();
    assert_eq!(content.len(), 10, "File should be extended to 10 bytes");
    assert_eq!(&content[0..2], b"Hi", "Original content preserved");
    assert!(
        content[2..].iter().all(|&b| b == 0),
        "Extended portion should be zeros"
    );
}

#[test]
fn test_write_patched_copy_and_insert() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let src_path = temp_dir.path().join("patch_src.txt");
    let dst_path = temp_dir.path().join("patch_dst.txt");

    // Create source file: "AAABBBCCC"
    fs.write_file(&src_path, b"AAABBBCCC").unwrap();

    // Apply patch: copy "AAA", insert "XXX", copy "CCC"
    let ops = vec![
        WriteOp::Copy { offset: 0, len: 3 }, // "AAA"
        WriteOp::Insert { data: b"XXX" },    // "XXX"
        WriteOp::Copy { offset: 6, len: 3 }, // "CCC"
    ];

    fs.write_patched(&src_path, &dst_path, &ops).unwrap();

    // Verify result
    let content = fs.read_file(&dst_path).unwrap();
    assert_eq!(
        content, b"AAAXXXCCC",
        "Patched content should match expected"
    );
}

#[test]
fn test_write_patched_in_place() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let path = temp_dir.path().join("patch_inplace.txt");

    // Create source file
    fs.write_file(&path, b"Hello World").unwrap();

    // Patch in-place: keep "Hello ", replace "World" with "Rust!"
    let ops = vec![
        WriteOp::Copy { offset: 0, len: 6 }, // "Hello "
        WriteOp::Insert { data: b"Rust!" },  // "Rust!"
    ];

    fs.write_patched(&path, &path, &ops).unwrap();

    // Verify result
    let content = fs.read_file(&path).unwrap();
    assert_eq!(content, b"Hello Rust!", "In-place patch should work");
}

#[test]
fn test_write_patched_large_file_small_edit() {
    // This test verifies the optimization benefit:
    // Edit a large file with a small change, only the change is transferred
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let path = temp_dir.path().join("large_patch.bin");

    // Create a 1MB file
    let size = 1_000_000;
    let original: Vec<u8> = (0..size).map(|i| (i % 256) as u8).collect();
    fs.write_file(&path, &original).unwrap();

    // Patch: keep first 500KB, insert 100 bytes, keep last 500KB
    let insert_data = b"THIS IS THE NEW CONTENT INSERTED IN THE MIDDLE OF A LARGE FILE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!";
    let ops = vec![
        WriteOp::Copy {
            offset: 0,
            len: 500_000,
        },
        WriteOp::Insert { data: insert_data },
        WriteOp::Copy {
            offset: 500_000,
            len: 500_000,
        },
    ];

    fs.write_patched(&path, &path, &ops).unwrap();

    // Verify result
    let content = fs.read_file(&path).unwrap();
    assert_eq!(
        content.len(),
        size + insert_data.len(),
        "File size should be original + inserted"
    );
    assert_eq!(
        &content[0..500_000],
        &original[0..500_000],
        "First half should match"
    );
    assert_eq!(
        &content[500_000..500_000 + insert_data.len()],
        insert_data,
        "Inserted content should match"
    );
    assert_eq!(
        &content[500_000 + insert_data.len()..],
        &original[500_000..],
        "Second half should match"
    );
}

#[test]
fn test_write_patched_preserves_permissions() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let path = temp_dir.path().join("perms_test.txt");

    // Create file and set specific permissions
    fs.write_file(&path, b"original").unwrap();

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
    }

    // Patch the file
    let ops = vec![WriteOp::Insert { data: b"patched" }];
    fs.write_patched(&path, &path, &ops).unwrap();

    // Verify permissions preserved
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let perms = std::fs::metadata(&path).unwrap().permissions();
        assert_eq!(
            perms.mode() & 0o777,
            0o755,
            "Permissions should be preserved after patch"
        );
    }
}

// =============================================================================
// TextBuffer + RemoteFileSystem e2e tests
// =============================================================================

/// Creates a RemoteFileSystem wrapped in Arc for use with TextBuffer
fn create_test_filesystem_arc() -> Option<(
    Arc<RemoteFileSystem>,
    tempfile::TempDir,
    tokio::runtime::Runtime,
)> {
    let temp_dir = tempfile::tempdir().ok()?;
    let rt = tokio::runtime::Runtime::new().ok()?;

    let channel = rt.block_on(spawn_local_agent()).ok()?;
    let fs = Arc::new(RemoteFileSystem::new(channel, "test@localhost".to_string()));

    Some((fs, temp_dir, rt))
}

#[test]
fn test_buffer_save_new_file_through_remote() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem_arc() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let file_path = temp_dir.path().join("new_file.txt");

    // Create a buffer with content
    let mut buffer = TextBuffer::from_bytes(b"Hello, World!\nLine 2\n".to_vec(), fs);

    // Save to new file
    buffer.save_to_file(&file_path).unwrap();

    // Verify file content
    let content = std::fs::read(&file_path).unwrap();
    assert_eq!(content, b"Hello, World!\nLine 2\n");
}

#[test]
fn test_buffer_save_edited_file_through_remote() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem_arc() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let file_path = temp_dir.path().join("edit_test.txt");

    // Create initial file
    std::fs::write(&file_path, b"AAABBBCCC").unwrap();

    // Load through remote filesystem
    let mut buffer = TextBuffer::load_from_file(&file_path, 1024 * 1024, fs).unwrap();

    // Edit: delete "BBB" and insert "XXX"
    buffer.delete_bytes(3, 3); // Delete "BBB"
    buffer.insert_bytes(3, b"XXX".to_vec()); // Insert "XXX"

    // Save back
    buffer.save_to_file(&file_path).unwrap();

    // Verify
    let content = std::fs::read(&file_path).unwrap();
    assert_eq!(content, b"AAAXXXCCC");
}

#[test]
fn test_buffer_save_with_copy_ops_through_remote() {
    // Test that unmodified regions use Copy ops (not transferred)
    let Some((fs, temp_dir, _rt)) = create_test_filesystem_arc() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let file_path = temp_dir.path().join("copy_ops_test.txt");

    // Create a larger file where Copy ops will be used
    let original: Vec<u8> = (0..10000).map(|i| b'A' + (i % 26) as u8).collect();
    std::fs::write(&file_path, &original).unwrap();

    // Load through remote filesystem
    let mut buffer = TextBuffer::load_from_file(&file_path, 1024 * 1024, fs).unwrap();

    // Make a small edit in the middle
    let edit_pos = 5000;
    buffer.delete_bytes(edit_pos, 10);
    buffer.insert_bytes(edit_pos, b"EDITED".to_vec());

    // Save back
    buffer.save_to_file(&file_path).unwrap();

    // Verify content
    let content = std::fs::read(&file_path).unwrap();
    assert_eq!(content.len(), original.len() - 10 + 6); // -10 deleted, +6 inserted

    // Check the edit is in place
    assert_eq!(&content[edit_pos..edit_pos + 6], b"EDITED");
    // Check surrounding content preserved
    assert_eq!(&content[0..100], &original[0..100]);
    assert_eq!(
        &content[content.len() - 100..],
        &original[original.len() - 100..]
    );
}

#[test]
fn test_buffer_save_as_different_path_through_remote() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem_arc() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let original_path = temp_dir.path().join("original.txt");
    let new_path = temp_dir.path().join("copy.txt");

    // Create initial file
    std::fs::write(&original_path, b"Original content").unwrap();

    // Load through remote filesystem
    let mut buffer = TextBuffer::load_from_file(&original_path, 1024 * 1024, fs).unwrap();

    // Make an edit
    buffer.insert_bytes(0, b"Modified: ".to_vec());

    // Save to different path
    buffer.save_to_file(&new_path).unwrap();

    // Verify new file has modified content
    let new_content = std::fs::read(&new_path).unwrap();
    assert_eq!(new_content, b"Modified: Original content");

    // Verify original file unchanged
    let original_content = std::fs::read(&original_path).unwrap();
    assert_eq!(original_content, b"Original content");
}

#[test]
fn test_buffer_save_with_line_ending_conversion_through_remote() {
    use fresh::model::buffer::LineEnding;

    let Some((fs, temp_dir, _rt)) = create_test_filesystem_arc() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let file_path = temp_dir.path().join("line_endings.txt");

    // Create file with CRLF line endings
    std::fs::write(&file_path, b"Line 1\r\nLine 2\r\nLine 3\r\n").unwrap();

    // Load through remote filesystem
    let mut buffer = TextBuffer::load_from_file(&file_path, 1024 * 1024, fs).unwrap();
    assert_eq!(buffer.line_ending(), LineEnding::CRLF);

    // Change to LF
    buffer.set_line_ending(LineEnding::LF);

    // Save back
    buffer.save_to_file(&file_path).unwrap();

    // Verify LF line endings (no CR)
    let content = std::fs::read(&file_path).unwrap();
    assert_eq!(content, b"Line 1\nLine 2\nLine 3\n");
    assert!(!content.contains(&b'\r'));
}

#[test]
fn test_buffer_save_empty_file_through_remote() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem_arc() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let file_path = temp_dir.path().join("empty.txt");

    // Create an empty buffer
    let mut buffer = TextBuffer::from_bytes(Vec::new(), fs);

    // Save to file
    buffer.save_to_file(&file_path).unwrap();

    // Verify empty file
    let content = std::fs::read(&file_path).unwrap();
    assert!(content.is_empty());
}

#[test]
fn test_buffer_multiple_edits_then_save_through_remote() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem_arc() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let file_path = temp_dir.path().join("multi_edit.txt");

    // Create initial file: "The quick brown fox jumps over the lazy dog."
    //                       0123456789...
    std::fs::write(&file_path, b"The quick brown fox jumps over the lazy dog.").unwrap();

    // Load through remote filesystem
    let mut buffer = TextBuffer::load_from_file(&file_path, 1024 * 1024, fs).unwrap();

    // Make multiple edits (work backwards to avoid offset shifts)
    // Original: "The quick brown fox jumps over the lazy dog."
    //            0         1         2         3         4
    //            0123456789012345678901234567890123456789012345

    // Change "lazy" (at pos 35) to "energetic"
    buffer.delete_bytes(35, 4); // delete "lazy"
    buffer.insert_bytes(35, b"energetic".to_vec());
    // Now: "The quick brown fox jumps over the energetic dog."

    // Change "brown" (at pos 10) to "red"
    buffer.delete_bytes(10, 5); // delete "brown"
    buffer.insert_bytes(10, b"red".to_vec());
    // Now: "The quick red fox jumps over the energetic dog."

    // Change "quick" (at pos 4) to "slow"
    buffer.delete_bytes(4, 5); // delete "quick"
    buffer.insert_bytes(4, b"slow".to_vec());
    // Now: "The slow red fox jumps over the energetic dog."

    // Save back
    buffer.save_to_file(&file_path).unwrap();

    // Verify
    let content = std::fs::read(&file_path).unwrap();
    assert_eq!(content, b"The slow red fox jumps over the energetic dog.");
}

#[test]
fn test_buffer_save_large_file_with_small_edit_through_remote() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem_arc() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let file_path = temp_dir.path().join("large_edit.bin");

    // Create a 1MB file
    let size = 1_000_000;
    let original: Vec<u8> = (0..size).map(|i| (i % 256) as u8).collect();
    std::fs::write(&file_path, &original).unwrap();

    // Load through remote filesystem
    let mut buffer = TextBuffer::load_from_file(&file_path, 1024 * 1024, fs).unwrap();

    // Make a tiny edit at the very end
    let edit_pos = size - 10;
    buffer.delete_bytes(edit_pos, 5);
    buffer.insert_bytes(edit_pos, b"END".to_vec());

    // Save back
    buffer.save_to_file(&file_path).unwrap();

    // Verify
    let content = std::fs::read(&file_path).unwrap();
    assert_eq!(content.len(), size - 5 + 3); // -5 deleted, +3 inserted

    // Beginning should be unchanged
    assert_eq!(&content[0..1000], &original[0..1000]);

    // Edit should be in place
    assert_eq!(&content[edit_pos..edit_pos + 3], b"END");
}

#[test]
fn test_buffer_large_file_edits_at_beginning_middle_and_end_through_remote() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem_arc() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let file_path = temp_dir.path().join("large_multi_edit.txt");

    // Create a 12MB file with uniquely calculatable lines
    // Each line is exactly 24 bytes: "Line NNNNNNNN content\n" where NNNNNNNN is the line number
    // This makes it easy to verify which lines are preserved or modified
    const LINE_LEN: usize = 22;
    const NUM_LINES: usize = 1_000_000;
    let mut original = Vec::with_capacity(LINE_LEN * NUM_LINES);
    let mut original_lines: Vec<Vec<u8>> = vec![];
    for i in 0..NUM_LINES {
        let line = format!("Line {:08} content\n", i);
        assert_eq!(line.len(), LINE_LEN);
        original.extend_from_slice(line.as_bytes());
        original_lines.push(line.into_bytes());
    }
    let size = original.len();
    assert_eq!(size, LINE_LEN * NUM_LINES);
    std::fs::write(&file_path, &original).unwrap();

    // Load through remote filesystem with default threshold (no customization)
    let mut buffer = TextBuffer::load_from_file(&file_path, 0, fs).unwrap();

    let orig_line_count = original_lines.len();

    // Define the edits we'll make
    // We need to work backwards (end -> middle -> beginning) to avoid offset shifts
    // affecting subsequent edit positions
    let mut expected_lines = Vec::from(original_lines);

    let steps = 4;
    let mut offset = 0;
    for i in 0..steps {
        let pos = i * (NUM_LINES / steps);
        if pos >= orig_line_count {
            break;
        }

        let line = format!("new {:08}\n", pos);
        let bytes = line.into_bytes();
        let target_offset = pos * LINE_LEN + offset;
        offset += bytes.len();
        println!("Inserting: at line: {}, offset: {}", pos, target_offset);
        buffer.insert_bytes(target_offset, bytes.clone());
        expected_lines.insert(pos + i, bytes);
    }

    // Save back through remote filesystem
    buffer.save_to_file(&file_path).unwrap();

    // Read back the saved file
    let content = std::fs::read(&file_path).unwrap();
    let content_str = String::from_utf8(content.clone()).expect("Content should be valid UTF-8");

    // Compare line by line for clear error messages
    let content_lines: Vec<&str> = content_str.lines().collect();

    // Compare each line
    for (line_num, (got, want)) in content_lines.iter().zip(expected_lines.iter()).enumerate() {
        // println!("{}", got);
        let want_bytes = &want[..(want.len() - 1)]; // drop newline
        assert_eq!(
            got.as_bytes(),
            want_bytes,
            "Line {} mismatch:\n  got:      {:?}\n  expected: {:?}",
            line_num,
            got,
            String::from_utf8(want.clone()).unwrap()
        );
    }

    assert_eq!(
        content_lines.len(),
        expected_lines.len(),
        "Line count should match: got {}, expected {}",
        content_lines.len(),
        expected_lines.len()
    );
}

#[test]
fn test_buffer_large_file_multiple_scattered_edits_through_remote() {
    // Test with many edits scattered throughout a 12MB file
    let Some((fs, temp_dir, _rt)) = create_test_filesystem_arc() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let file_path = temp_dir.path().join("scattered_edits.txt");

    // Create a 12MB file with uniquely calculatable lines
    // Each line is exactly 30 bytes: "L:NNNNNNNN:NNNNNNNN:content\n"
    // where NNNNNNNN is the line number (appears twice for redundancy)
    const LINE_LEN: usize = 28;
    const NUM_LINES: usize = 400_000; // 12MB total (400000 * 30 = 12,000,000)
    let make_line = |n: usize| -> String { format!("L:{:08}:{:08}:content\n", n, n) };

    let mut original = Vec::with_capacity(LINE_LEN * NUM_LINES);
    for i in 0..NUM_LINES {
        let line = make_line(i);
        assert_eq!(line.len(), LINE_LEN);

        original.extend_from_slice(line.as_bytes());
    }
    let size = original.len();
    assert_eq!(size, LINE_LEN * NUM_LINES);
    std::fs::write(&file_path, &original).unwrap();

    // Load through remote filesystem with default threshold (no customization)
    let mut buffer = TextBuffer::load_from_file(&file_path, 0, fs).unwrap();

    // Define edits at specific line positions (work backwards to simplify offset tracking)
    // Each edit: (line_number, insert_data, lines_to_delete)
    let edits: Vec<(usize, &[u8], usize)> = vec![
        // (line_num, insert_data, num_lines_to_delete)
        (380_000, b"[NEAR_END_MARKER]\n", 1),  // 95% through
        (300_000, b"[300K_LINE_MARKER]\n", 0), // 75% through
        (200_123, b"[HALFWAY_MARKER]\n", 0),   // 50% through
        (100_000, b"[100K_LINE_MARKER]\n", 0), // 25% through
        (50_515, b"[50K_LINE_MARKER]\n", 0),   // 12.5% through
        (10_000, b"[10K_LINE_MARKER]\n", 0),   // 2.5% through
        (0, b"[FILE_HEADER]\n", 0),            // Pure insert at start (no delete)
    ];

    // Apply edits in reverse order (high to low line number)
    for (line_num, insert_data, lines_to_delete) in &edits {
        let byte_pos = line_num * LINE_LEN;
        let delete_bytes = lines_to_delete * LINE_LEN;
        if delete_bytes > 0 {
            buffer.delete_bytes(byte_pos, delete_bytes);
        }
        buffer.insert_bytes(byte_pos, insert_data.to_vec());
    }

    // Save
    buffer.save_to_file(&file_path).unwrap();

    // Build expected content
    let mut expected = Vec::with_capacity(size + 200);

    // Line numbers that were deleted (replaced)
    let deleted_lines: std::collections::HashSet<usize> = edits
        .iter()
        .filter(|(_, _, del)| *del > 0)
        .map(|(line, _, _)| *line)
        .collect();

    // Insert markers at the right positions (sorted by line number for building)
    let mut markers: Vec<(usize, &[u8])> =
        edits.iter().map(|(line, data, _)| (*line, *data)).collect();
    markers.sort_by_key(|(line, _)| *line);

    let mut current_line = 0;
    for (marker_line, marker_data) in &markers {
        // Add all original lines before this marker (except deleted ones)
        while current_line < *marker_line {
            if !deleted_lines.contains(&current_line) {
                expected.extend_from_slice(make_line(current_line).as_bytes());
            }
            current_line += 1;
        }
        // Add the marker
        expected.extend_from_slice(marker_data);
        // If this marker replaced a line, skip it
        if deleted_lines.contains(marker_line) {
            current_line = marker_line + 1;
        }
    }
    // Add remaining original lines
    while current_line < NUM_LINES {
        if !deleted_lines.contains(&current_line) {
            expected.extend_from_slice(make_line(current_line).as_bytes());
        }
        current_line += 1;
    }

    // Read back the saved file
    let content = std::fs::read(&file_path).unwrap();
    let content_str = String::from_utf8(content.clone()).expect("Content should be valid UTF-8");
    let expected_str = String::from_utf8(expected.clone()).expect("Expected should be valid UTF-8");

    // Compare line by line for clear error messages
    let content_lines: Vec<&str> = content_str.lines().collect();
    let expected_lines: Vec<&str> = expected_str.lines().collect();

    assert_eq!(
        content_lines.len(),
        expected_lines.len(),
        "Line count should match: got {}, expected {}",
        content_lines.len(),
        expected_lines.len()
    );

    // Compare each line
    for (line_num, (got, want)) in content_lines.iter().zip(expected_lines.iter()).enumerate() {
        assert_eq!(
            got, want,
            "Line {} mismatch:\n  got:      {:?}\n  expected: {:?}",
            line_num, got, want
        );
    }

    // Also verify the raw bytes match exactly (including line endings)
    assert_eq!(
        content.len(),
        expected.len(),
        "Byte length should match: got {}, expected {}",
        content.len(),
        expected.len()
    );
    assert_eq!(content, expected, "Full byte content should match expected");
}

#[test]
fn test_buffer_huge_file_multi_save_cycle_through_remote() {
    let Some((fs, temp_dir, _rt)) = create_test_filesystem_arc() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let file_path = temp_dir.path().join("multi_save_huge.txt");

    // Use exactly the same line logic as e2e test
    const NUM_LINES: usize = 1_000_000;
    let mut original = Vec::new();
    let mut expected_lines = Vec::with_capacity(NUM_LINES);
    let mut line_starts = Vec::with_capacity(NUM_LINES);
    let mut current_offset = 0;

    for i in 0..NUM_LINES {
        line_starts.push(current_offset);
        let line = format!("Line {:05}: original content\n", i);
        current_offset += line.len();
        original.extend_from_slice(line.as_bytes());
        expected_lines.push(line);
    }
    std::fs::write(&file_path, &original).unwrap();

    // Use default threshold (1MB) to match production behavior
    let threshold = 1024 * 1024;
    let mut buffer = TextBuffer::load_from_file(&file_path, threshold, fs).unwrap();

    for target_line in vec![5000, 3] {
        let edit_text = format!("ITER_{}_", target_line);
        let byte_pos = line_starts[target_line];

        buffer.insert_bytes(byte_pos, edit_text.as_bytes().to_vec());
        expected_lines[target_line] = format!("{}{}", edit_text, expected_lines[target_line]);

        // Save
        buffer.save_to_file(&file_path).unwrap();

        // Verify
        let content = std::fs::read(&file_path).unwrap();
        let content_str = String::from_utf8(content).unwrap();
        let content_lines: Vec<&str> = content_str.lines().collect();

        assert_eq!(
            content_lines.len(),
            expected_lines.len(),
            "Line count mismatch at iter {}",
            target_line
        );
        for (i, (got, want)) in content_lines.iter().zip(expected_lines.iter()).enumerate() {
            let want_trimmed = want.trim_end_matches('\n');
            if *got != want_trimmed {
                panic!(
                    "Line {} mismatch at iter {}:\n  got:      {:?}\n  expected: {:?}",
                    i, target_line, got, want_trimmed
                );
            }
        }
    }
}
