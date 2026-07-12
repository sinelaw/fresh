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
use fresh::services::remote::{
    spawn_local_agent, spawn_local_agent_with_capacity, RemoteFileSystem, TEST_RECV_DELAY_US,
};
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::time::Duration;

/// Effectively-unbounded per-request timeout for the test harness.
///
/// CONTRIBUTING.md §Testing rule 3 mandates "wait indefinitely, don't put
/// timeouts inside tests (cargo nextest will timeout externally)". The
/// production `AgentChannel` defaults to a 10s timeout that is fine for
/// real interactive use, but tests in this file move tens of MB through
/// the streaming protocol and a loaded CI runner can blow past 10s while
/// still making forward progress. Setting an essentially-unbounded
/// timeout removes the time dependency from the happy path; the
/// intentional-timeout test below configures its own short value.
const TEST_HARNESS_REQUEST_TIMEOUT: Duration = Duration::from_secs(60 * 60);

/// Simple pseudo-random number generator (xorshift64) to avoid external deps.
/// Not cryptographic — just deterministic and fast for test data generation.
struct Rng(u64);
impl Rng {
    fn new(seed: u64) -> Self {
        Self(seed)
    }
    fn next(&mut self) -> u64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        self.0
    }
    /// Random usize in [0, max) — panics if max == 0
    fn usize(&mut self, max: usize) -> usize {
        (self.next() % max as u64) as usize
    }
}

/// Creates a RemoteFileSystem using production code
fn create_test_filesystem() -> Option<(RemoteFileSystem, tempfile::TempDir, tokio::runtime::Runtime)>
{
    let temp_dir = tempfile::tempdir().ok()?;
    let rt = tokio::runtime::Runtime::new().ok()?;

    let channel = rt.block_on(spawn_local_agent()).ok()?;
    channel.set_request_timeout(TEST_HARNESS_REQUEST_TIMEOUT);
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
    channel.set_request_timeout(TEST_HARNESS_REQUEST_TIMEOUT);
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

    // Verify no data was lost during streaming read (#1059)
    assert_eq!(
        buffer.total_bytes(),
        size,
        "Loaded buffer size ({}) != original file size ({}). \
         {} bytes lost during streaming read!",
        buffer.total_bytes(),
        size,
        size.saturating_sub(buffer.total_bytes()),
    );

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

#[test]
fn test_buffer_shadow_random_ops_through_remote() {
    // Shadow buffer test: perform many random insert/delete operations on both a
    // TextBuffer (through RemoteFileSystem) and a plain Vec<u8> (the "shadow").
    // After each save-and-reload cycle, verify the saved file matches the shadow.
    //
    // This catches:
    // - Streaming data loss (try_send channel overflow, #1059)
    // - Piece tree corruption during split/rebalance
    // - Write recipe (Copy/Insert) offset miscalculation
    // - Base64 encoding/decoding errors
    // - Save path bugs (write_file vs write_patched)

    let Some((fs, temp_dir, _rt)) = create_test_filesystem_arc() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let file_path = temp_dir.path().join("shadow_random.txt");

    // Start with a ~2MB file of recognizable content.
    // Each line is 22 bytes: "Line NNNNNNNN content\n"
    const LINE_LEN: usize = 22;
    const NUM_LINES: usize = 100_000; // ~2.2MB — streamed as ~34 chunks
    let mut shadow: Vec<u8> = Vec::with_capacity(LINE_LEN * NUM_LINES);
    for i in 0..NUM_LINES {
        let line = format!("Line {:08} content\n", i);
        shadow.extend_from_slice(line.as_bytes());
    }
    std::fs::write(&file_path, &shadow).unwrap();

    // Load through remote filesystem
    let mut buffer = TextBuffer::load_from_file(&file_path, 0, fs.clone()).unwrap();

    // Verify load was lossless
    assert_eq!(
        buffer.total_bytes(),
        shadow.len(),
        "Initial load lost data: buffer={} shadow={}",
        buffer.total_bytes(),
        shadow.len(),
    );

    let mut rng = Rng::new(0xDEAD_BEEF_CAFE_1059);

    const NUM_OPS: usize = 2000;
    const SAVE_EVERY: usize = 50; // save + verify every N ops

    for op_idx in 0..NUM_OPS {
        let buf_len = buffer.total_bytes();
        assert_eq!(
            buf_len,
            shadow.len(),
            "Size mismatch before op {}: buffer={} shadow={}",
            op_idx,
            buf_len,
            shadow.len(),
        );

        // Pick operation: 50% insert, 30% delete, 20% replace (delete+insert)
        let op_kind = rng.usize(10);

        if buf_len == 0 || op_kind < 5 {
            // INSERT: random position, random small payload
            let pos = if buf_len == 0 {
                0
            } else {
                rng.usize(buf_len + 1)
            };
            let payload_len = 1 + rng.usize(200);
            let payload: Vec<u8> = (0..payload_len)
                .map(|j| b'A' + ((op_idx + j) % 26) as u8)
                .collect();

            buffer.insert_bytes(pos, payload.clone());
            shadow.splice(pos..pos, payload.iter().copied());
        } else if op_kind < 8 {
            // DELETE: random position, random length (up to 500 bytes)
            let pos = rng.usize(buf_len);
            let max_del = (buf_len - pos).min(500);
            if max_del == 0 {
                continue;
            }
            let del_len = 1 + rng.usize(max_del);

            buffer.delete_bytes(pos, del_len);
            shadow.drain(pos..pos + del_len);
        } else {
            // REPLACE: delete then insert at same position
            let pos = rng.usize(buf_len);
            let max_del = (buf_len - pos).min(300);
            if max_del == 0 {
                continue;
            }
            let del_len = 1 + rng.usize(max_del);

            buffer.delete_bytes(pos, del_len);
            shadow.drain(pos..pos + del_len);

            let payload_len = 1 + rng.usize(200);
            let payload: Vec<u8> = (0..payload_len)
                .map(|j| b'a' + ((op_idx + j) % 26) as u8)
                .collect();

            let insert_pos = pos.min(buffer.total_bytes());
            buffer.insert_bytes(insert_pos, payload.clone());
            shadow.splice(insert_pos..insert_pos, payload.iter().copied());
        }

        // Periodic save-and-verify cycle
        if (op_idx + 1) % SAVE_EVERY == 0 {
            // Save through remote filesystem
            buffer.save_to_file(&file_path).unwrap();

            // Read back directly from disk
            let on_disk = std::fs::read(&file_path).unwrap();

            // Compare sizes first (cheap)
            assert_eq!(
                on_disk.len(),
                shadow.len(),
                "Size mismatch after save at op {}: disk={} shadow={}",
                op_idx,
                on_disk.len(),
                shadow.len(),
            );

            // Find first differing byte for a useful error message
            if on_disk != shadow {
                let first_diff = on_disk
                    .iter()
                    .zip(shadow.iter())
                    .position(|(a, b)| a != b)
                    .unwrap_or(on_disk.len().min(shadow.len()));
                let context_start = first_diff.saturating_sub(20);
                let context_end = (first_diff + 20).min(on_disk.len()).min(shadow.len());
                panic!(
                    "Content mismatch after op {}! First diff at byte {}.\n\
                     disk[{}..{}]:   {:?}\n\
                     shadow[{}..{}]: {:?}",
                    op_idx,
                    first_diff,
                    context_start,
                    context_end,
                    &on_disk[context_start..context_end],
                    context_start,
                    context_end,
                    &shadow[context_start..context_end],
                );
            }

            println!("  verified after op {}: {} bytes OK", op_idx, shadow.len());

            // Reload from disk through remote fs to test the full round-trip
            buffer = TextBuffer::load_from_file(&file_path, 0, fs.clone()).unwrap();

            assert_eq!(
                buffer.total_bytes(),
                shadow.len(),
                "Reload lost data after op {}: buffer={} shadow={}",
                op_idx,
                buffer.total_bytes(),
                shadow.len(),
            );
        }
    }

    // Final save and verify
    buffer.save_to_file(&file_path).unwrap();
    let final_content = std::fs::read(&file_path).unwrap();
    assert_eq!(
        final_content.len(),
        shadow.len(),
        "Final size mismatch: disk={} shadow={}",
        final_content.len(),
        shadow.len(),
    );
    assert_eq!(
        final_content,
        shadow,
        "Final content mismatch (sizes matched at {} bytes)",
        shadow.len(),
    );
    println!(
        "Shadow test passed: {} ops, {} save/reload cycles, final size {} bytes",
        NUM_OPS,
        NUM_OPS / SAVE_EVERY,
        shadow.len(),
    );
}

/// Regression test for #1059: verifies that streaming reads don't lose data
/// even under extreme backpressure (tiny channel + slow consumer).
///
/// This test is deterministic on all platforms — it doesn't rely on OS
/// scheduling to trigger the bug. Before the fix (try_send → send().await),
/// this test fails 100% of the time.
#[test]
fn test_regression_1059_streaming_read_backpressure() {
    let temp_dir = tempfile::tempdir().unwrap();
    let rt = tokio::runtime::Runtime::new().unwrap();

    // Tiny channel capacity: only 2 slots before backpressure kicks in
    let Some(channel) = rt.block_on(spawn_local_agent_with_capacity(2)).ok() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };
    channel.set_request_timeout(TEST_HARNESS_REQUEST_TIMEOUT);
    let fs: Arc<dyn FileSystem + Send + Sync> =
        Arc::new(RemoteFileSystem::new(channel, "test@localhost".to_string()));

    // Slow down the consumer to guarantee the producer outruns it.
    // 1ms per chunk × ~34 chunks ≈ 34ms total overhead — fast enough for CI.
    TEST_RECV_DELAY_US.store(1000, Ordering::SeqCst);

    let file_path = temp_dir.path().join("backpressure_test.txt");

    // ~2.2MB file → ~34 streaming chunks of 65KB each.
    // With channel capacity 2 and a 1ms consumer delay, the producer WILL
    // fill the channel. Before the fix, this silently dropped chunks.
    const LINE_LEN: usize = 22;
    const NUM_LINES: usize = 100_000;
    let mut original = Vec::with_capacity(LINE_LEN * NUM_LINES);
    for i in 0..NUM_LINES {
        let line = format!("Line {:08} content\n", i);
        original.extend_from_slice(line.as_bytes());
    }
    std::fs::write(&file_path, &original).unwrap();

    // Load through remote filesystem (streams the full file)
    let mut buffer = TextBuffer::load_from_file(&file_path, 0, fs.clone()).unwrap();

    // This is the core assertion: with the old try_send, loaded bytes < original
    assert_eq!(
        buffer.total_bytes(),
        original.len(),
        "Streaming read lost data: loaded {} bytes, expected {} ({} bytes lost)",
        buffer.total_bytes(),
        original.len(),
        original.len().saturating_sub(buffer.total_bytes()),
    );

    // Also verify a save round-trip produces correct content
    let insert_pos = 50_000 * LINE_LEN;
    let insert_data = b"INSERTED LINE\n".to_vec();
    buffer.insert_bytes(insert_pos, insert_data.clone());
    buffer.save_to_file(&file_path).unwrap();

    let saved = std::fs::read(&file_path).unwrap();
    let mut expected = original.clone();
    expected.splice(insert_pos..insert_pos, insert_data.iter().copied());
    assert_eq!(
        saved.len(),
        expected.len(),
        "Save round-trip size mismatch: got {} expected {}",
        saved.len(),
        expected.len(),
    );
    assert_eq!(saved, expected, "Save round-trip content mismatch");

    // Reset the delay so other tests aren't affected
    TEST_RECV_DELAY_US.store(0, Ordering::SeqCst);

    println!("Regression test #1059 passed: streaming read + save correct under backpressure");
}

/// Test that the Python agent handles concurrent requests correctly.
///
/// Spawns the agent, creates a file with known newline distribution, then
/// fires many concurrent `count_line_feeds_in_range` requests and verifies
/// every result matches the expected count. This exercises:
/// - ThreadPoolExecutor dispatch in the Python agent
/// - write_lock serialization of stdout (no corrupted JSON lines)
/// - AgentChannel request-ID multiplexing on the Rust side
/// - Interleaved responses from different requests
#[test]
fn test_concurrent_count_lf_requests() {
    let temp_dir = tempfile::tempdir().unwrap();
    let rt = tokio::runtime::Runtime::new().unwrap();

    let Some(channel) = rt.block_on(spawn_local_agent()).ok() else {
        eprintln!("Skipping test: could not spawn agent");
        return;
    };
    channel.set_request_timeout(TEST_HARNESS_REQUEST_TIMEOUT);
    let fs = Arc::new(RemoteFileSystem::new(channel, "test@localhost".to_string()));

    // Create a file where we can predict newline counts exactly.
    // Pattern: 100-byte "lines" — 99 bytes of 'A' followed by '\n'.
    // This gives us exactly 1 newline per 100-byte chunk.
    let line_len = 100usize;
    let num_lines = 10_000; // 1MB file
    let mut content = Vec::with_capacity(line_len * num_lines);
    for _ in 0..num_lines {
        content.extend(std::iter::repeat(b'A').take(line_len - 1));
        content.push(b'\n');
    }
    let file_path = temp_dir.path().join("concurrent_lf.bin");
    std::fs::write(&file_path, &content).unwrap();

    // Fire 64 concurrent count_lf requests, each covering a different
    // non-overlapping range of the file.
    let num_requests = 64usize;
    let chunk_size = content.len() / num_requests; // ~15625 bytes each

    let results: Vec<std::io::Result<(usize, usize)>> = rt.block_on(async {
        let mut handles = Vec::with_capacity(num_requests);
        for i in 0..num_requests {
            let fs = fs.clone();
            let path = file_path.clone();
            let offset = (i * chunk_size) as u64;
            let len = if i == num_requests - 1 {
                content.len() - i * chunk_size // last chunk gets remainder
            } else {
                chunk_size
            };
            handles.push(tokio::task::spawn_blocking(move || {
                let count = fs.count_line_feeds_in_range(&path, offset, len)?;
                Ok((i, count))
            }));
        }

        let mut results = Vec::with_capacity(handles.len());
        for handle in handles {
            results.push(handle.await.unwrap());
        }
        results
    });

    // Verify every request succeeded and returned the correct count
    let mut total_lf = 0usize;
    for result in &results {
        let (idx, count) = result.as_ref().expect("count_lf request should succeed");

        let offset = idx * chunk_size;
        let len = if *idx == num_requests - 1 {
            content.len() - idx * chunk_size
        } else {
            chunk_size
        };
        let expected = content[offset..offset + len]
            .iter()
            .filter(|&&b| b == b'\n')
            .count();

        assert_eq!(
            *count, expected,
            "chunk {}: got {} newlines, expected {} (offset={}, len={})",
            idx, count, expected, offset, len
        );
        total_lf += count;
    }

    assert_eq!(
        total_lf, num_lines,
        "total newlines across all chunks should equal num_lines"
    );
    println!(
        "Concurrent count_lf test passed: {} requests, {} total newlines",
        num_requests, total_lf
    );
}

/// Test concurrent mixed operations (count_lf + read_range interleaved).
///
/// This verifies that the agent's ThreadPoolExecutor correctly handles
/// different request types concurrently without corruption.
#[test]
fn test_concurrent_mixed_requests() {
    let temp_dir = tempfile::tempdir().unwrap();
    let rt = tokio::runtime::Runtime::new().unwrap();

    let Some(channel) = rt.block_on(spawn_local_agent()).ok() else {
        eprintln!("Skipping test: could not spawn agent");
        return;
    };
    channel.set_request_timeout(TEST_HARNESS_REQUEST_TIMEOUT);
    let fs = Arc::new(RemoteFileSystem::new(channel, "test@localhost".to_string()));

    // Create a file with predictable content
    let line_len = 80usize;
    let num_lines = 5_000;
    let mut content = Vec::with_capacity(line_len * num_lines);
    for i in 0..num_lines {
        let line = format!("{:>079}\n", i); // 79 digits + newline = 80 bytes
        content.extend_from_slice(line.as_bytes());
    }
    let file_path = temp_dir.path().join("mixed_concurrent.bin");
    std::fs::write(&file_path, &content).unwrap();

    // Fire 32 count_lf requests and 32 read_range requests concurrently
    let num_each = 32usize;
    let chunk_size = content.len() / num_each;

    enum Expected {
        CountLf { idx: usize, expected: usize },
        ReadRange { idx: usize, expected: Vec<u8> },
    }

    let (expectations, results): (Vec<Expected>, Vec<std::io::Result<()>>) = rt.block_on(async {
        let mut handles: Vec<tokio::task::JoinHandle<std::io::Result<(usize, Vec<u8>, usize)>>> =
            Vec::new();
        let mut expectations = Vec::new();

        for i in 0..num_each {
            let offset = (i * chunk_size) as u64;
            let len = chunk_size;

            // count_lf request
            {
                let fs = fs.clone();
                let path = file_path.clone();
                let expected_lf = content[i * chunk_size..(i + 1) * chunk_size]
                    .iter()
                    .filter(|&&b| b == b'\n')
                    .count();
                expectations.push(Expected::CountLf {
                    idx: i,
                    expected: expected_lf,
                });
                handles.push(tokio::task::spawn_blocking(move || {
                    let count = fs.count_line_feeds_in_range(&path, offset, len)?;
                    Ok((i, Vec::new(), count))
                }));
            }

            // read_range request
            {
                let fs = fs.clone();
                let path = file_path.clone();
                let expected_data = content[i * chunk_size..(i + 1) * chunk_size].to_vec();
                expectations.push(Expected::ReadRange {
                    idx: i,
                    expected: expected_data,
                });
                handles.push(tokio::task::spawn_blocking(move || {
                    let data = fs.read_range(&path, offset, len)?;
                    Ok((i, data, 0))
                }));
            }
        }

        let mut results = Vec::new();
        let mut handle_results = Vec::new();
        for handle in handles {
            handle_results.push(handle.await.unwrap());
        }

        for (exp, result) in expectations.iter().zip(handle_results.iter()) {
            match (exp, result) {
                (Expected::CountLf { idx, expected }, Ok((_i, _data, count))) => {
                    if count != expected {
                        results.push(Err(std::io::Error::new(
                            std::io::ErrorKind::Other,
                            format!(
                                "count_lf chunk {}: got {}, expected {}",
                                idx, count, expected
                            ),
                        )));
                    } else {
                        results.push(Ok(()));
                    }
                }
                (Expected::ReadRange { idx, expected }, Ok((_i, data, _count))) => {
                    if data != expected {
                        results.push(Err(std::io::Error::new(
                            std::io::ErrorKind::Other,
                            format!(
                                "read_range chunk {}: got {} bytes, expected {} bytes",
                                idx,
                                data.len(),
                                expected.len()
                            ),
                        )));
                    } else {
                        results.push(Ok(()));
                    }
                }
                (_, Err(e)) => {
                    results.push(Err(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        format!("request failed: {}", e),
                    )));
                }
            }
        }

        (expectations, results)
    });

    // Check all results
    for (i, result) in results.iter().enumerate() {
        result
            .as_ref()
            .unwrap_or_else(|e| panic!("request {} failed: {}", i, e));
    }

    println!(
        "Mixed concurrent test passed: {} total requests ({} count_lf + {} read_range)",
        expectations.len(),
        num_each,
        num_each
    );
}

#[test]
fn test_unique_temp_path_uses_system_temp_dir() {
    let Some((fs, _temp_dir, _rt)) = create_test_filesystem() else {
        eprintln!("Skipping test: could not create test filesystem");
        return;
    };

    let dest = std::path::Path::new("/some/dir/myfile.txt");
    let temp_path = fs.unique_temp_path(dest);

    // The temp path should be under the system's temp directory (what Python's
    // tempfile.gettempdir() returns), NOT a hardcoded /tmp.
    let expected_temp_dir = std::env::temp_dir();
    assert!(
        temp_path.starts_with(&expected_temp_dir),
        "unique_temp_path should use system temp dir ({:?}), got {:?}",
        expected_temp_dir,
        temp_path,
    );

    // The temp file name should contain the original file's name
    let name = temp_path.file_name().unwrap().to_string_lossy();
    assert!(
        name.contains("myfile.txt"),
        "temp file name should contain the original file name, got {:?}",
        name,
    );

    // The temp file name should end with .tmp
    assert!(
        name.ends_with(".tmp"),
        "temp file name should end with .tmp, got {:?}",
        name,
    );
}

/// Regression test: an explicitly configured request timeout still fires when
/// the request flows through `RemoteFileSystem`, even though the integration
/// test harness raises the default timeout to "essentially unbounded".
///
/// Guards against accidentally short-circuiting the timeout path in
/// production code (e.g., by removing `tokio::time::timeout` from
/// `AgentChannel::request`). The flaky-on-CI fix that prompted this test
/// only changes the *test harness* default; production callers must still
/// observe `ChannelError::Timeout` (surfaced as `io::Error` with "timed
/// out" in the message) when the configured budget is exceeded.
#[test]
fn test_remote_filesystem_explicit_timeout_fires() {
    use fresh::services::remote::AgentChannel;
    use tokio::io::{AsyncBufReadExt, BufReader};
    use tokio::process::Command as TokioCommand;

    // Python script: send the ready handshake, then read stdin forever
    // without ever responding to a request. Any `request_blocking` call
    // on the resulting channel will block until the configured timeout.
    let script = r#"
import sys, json
sys.stdout.write(json.dumps({"id": 0, "ok": True, "v": 1}) + "\n")
sys.stdout.flush()
for line in sys.stdin:
    pass
"#;

    let rt = tokio::runtime::Runtime::new().unwrap();
    let _guard = rt.enter();

    let mut child = match TokioCommand::new("python3")
        .arg("-u")
        .arg("-c")
        .arg(script)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
    {
        Ok(c) => c,
        Err(_) => {
            eprintln!("Skipping test: python3 not available");
            return;
        }
    };

    let stdin = child.stdin.take().expect("stdin");
    let stdout = child.stdout.take().expect("stdout");
    let mut reader = BufReader::new(stdout);

    let mut ready_line = String::new();
    rt.block_on(reader.read_line(&mut ready_line))
        .expect("read ready");
    assert!(
        !ready_line.is_empty(),
        "silent agent did not send ready message"
    );

    let channel = Arc::new(AgentChannel::new(reader, stdin));
    // Short, but generous enough to avoid being mistaken for a successful
    // response on a fast runner. Wall-clock cost of this test is bounded
    // by this value.
    channel.set_request_timeout(Duration::from_millis(200));

    let fs = RemoteFileSystem::new(channel, "test@silent".to_string());

    // Any operation that goes through the channel will time out, since
    // the agent never replies. `metadata` is a single round-trip call.
    let result = fs.metadata(std::path::Path::new("/anything"));
    let err = result.expect_err("expected timeout error, got Ok");
    let msg = err.to_string().to_lowercase();
    assert!(
        msg.contains("timed out") || msg.contains("timeout"),
        "expected timeout-related error, got: {:?}",
        err,
    );

    // Tidy up so the python process doesn't outlive the test.
    let _ = child.start_kill();
}

/// Regression test for the "web editor freezes on switching to an unreachable
/// SSH workspace" bug. The freeze was `home_dir()` issuing a blocking `info`
/// request on the editor thread during a dormant session's workspace restore;
/// on a host that finished the SSH handshake but then can't answer, that hung
/// the whole single-threaded UI for the request timeout.
///
/// The fix is a connect-time liveness gate: [`RemoteFileSystem::prime_sys_info`]
/// must reject a link that can't answer `info`, so the dive fails the connect
/// (Disconnected + Retry) instead of promoting into a session whose restore
/// then blocks the editor. Against a live agent it succeeds and primes the
/// cache that `home_dir` is then served from.
#[test]
fn prime_sys_info_gates_unresponsive_link_and_caches_when_live() {
    // --- Live agent: the gate accepts it and primes the $HOME cache. ---
    if let Some((fs, _temp_dir, rt)) = create_test_filesystem() {
        rt.block_on(fs.prime_sys_info())
            .expect("liveness gate must accept a responsive agent");
        let home = fs
            .home_dir()
            .expect("home_dir is served from the primed cache");
        assert!(
            home.is_absolute(),
            "expected an absolute remote home, got {home:?}",
        );
    } else {
        eprintln!("Skipping live half: could not spawn local agent");
    }

    // --- Silent agent (ready handshake, then no responses): gate rejects it. ---
    use fresh::services::remote::AgentChannel;
    use tokio::io::{AsyncBufReadExt, BufReader};
    use tokio::process::Command as TokioCommand;

    let script = r#"
import sys, json
sys.stdout.write(json.dumps({"id": 0, "ok": True, "v": 1}) + "\n")
sys.stdout.flush()
for line in sys.stdin:
    pass
"#;

    let rt = tokio::runtime::Runtime::new().unwrap();
    let _guard = rt.enter();

    let mut child = match TokioCommand::new("python3")
        .arg("-u")
        .arg("-c")
        .arg(script)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
    {
        Ok(c) => c,
        Err(_) => {
            eprintln!("Skipping silent half: python3 not available");
            return;
        }
    };

    let stdin = child.stdin.take().expect("stdin");
    let stdout = child.stdout.take().expect("stdout");
    let mut reader = BufReader::new(stdout);
    let mut ready_line = String::new();
    rt.block_on(reader.read_line(&mut ready_line))
        .expect("read ready");
    assert!(!ready_line.is_empty(), "silent agent did not send ready");

    let channel = Arc::new(AgentChannel::new(reader, stdin));
    // Bound how long the unanswered `info` waits; the assertion is on the
    // *outcome* (an error), not on elapsed time — the agent never replies, so
    // the gate rejects it regardless of runner speed (matches
    // `test_remote_filesystem_explicit_timeout_fires`).
    channel.set_request_timeout(Duration::from_millis(200));
    let fs = RemoteFileSystem::new(channel, "test@silent".to_string());

    let gated = rt.block_on(fs.prime_sys_info());
    assert!(
        gated.is_err(),
        "liveness gate must reject a link that cannot answer info",
    );

    let _ = child.start_kill();
}
