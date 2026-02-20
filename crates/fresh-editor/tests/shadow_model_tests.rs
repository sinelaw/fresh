//! Shadow Model Property-Based Tests for TextBuffer
//!
//! # Architecture: "The Shadow Model"
//!
//! This test suite implements a rigorous Model-Based Testing strategy (also known as
//! "Shadow Testing" or "Oracle Testing"). The core concept is to run two systems in parallel:
//!
//! - **System Under Test (SUT)**: The complex `TextBuffer` API using PieceTree data structure
//! - **Model (Oracle)**: A "stupidly simple" implementation using a standard `Vec<u8>` that
//!   is correct by definition
//!
//! ## Phase 1: Static Analysis Summary
//!
//! ### Target Layer
//! - **Core Engine**: `TextBuffer` in `src/model/buffer.rs`
//! - **Data Structure**: `PieceTree` (balanced binary tree for efficient text manipulation)
//!
//! ### Mutation Operations
//! - `insert_bytes(offset, text: Vec<u8>)` - Insert bytes at position
//! - `delete_bytes(offset, bytes: usize)` - Delete bytes starting at position
//! - `save_to_file(path)` - Persist buffer to disk
//! - `load_from_file(path)` - Load buffer from disk
//!
//! ### Query Operations
//! - `to_string()` - Get full content as String (None for large files)
//! - `total_bytes()` / `len()` - Get document size in bytes
//! - `line_count()` - Get number of lines
//!
//! ## Test Properties Verified
//! - Content equality: Model content == SUT content after every operation
//! - Length invariant: Model length == SUT total_bytes
//! - Line count invariant: Model newline count + 1 == SUT line count
//! - Persistence roundtrip: Save → Load preserves content exactly

use fresh::model::buffer::TextBuffer;
use fresh::model::filesystem::{
    DirEntry, FileMetadata, FilePermissions, FileReader, FileSystem, FileWriter, StdFileSystem,
};
use proptest::prelude::*;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use tempfile::TempDir;

// ============================================================================
// 0. CONFIGURABLE FILESYSTEM (For testing different ownership scenarios)
// ============================================================================

/// A filesystem wrapper that allows controlling `is_owner()` behavior at runtime.
/// This enables testing both atomic write (owner=true) and in-place write (owner=false) paths.
struct ConfigurableFileSystem {
    inner: Arc<dyn FileSystem + Send + Sync>,
    /// When false, simulates files owned by another user, triggering in-place write path
    simulate_owner: Arc<AtomicBool>,
}

impl ConfigurableFileSystem {
    fn new(simulate_owner: bool) -> Self {
        Self {
            inner: Arc::new(StdFileSystem),
            simulate_owner: Arc::new(AtomicBool::new(simulate_owner)),
        }
    }

    fn set_owner(&self, is_owner: bool) {
        self.simulate_owner.store(is_owner, Ordering::SeqCst);
    }

    fn is_simulating_owner(&self) -> bool {
        self.simulate_owner.load(Ordering::SeqCst)
    }
}

impl FileSystem for ConfigurableFileSystem {
    fn is_owner(&self, _path: &Path) -> bool {
        self.simulate_owner.load(Ordering::SeqCst)
    }

    // Delegate all other methods to inner filesystem
    fn read_file(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.inner.read_file(path)
    }

    fn read_range(&self, path: &Path, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        self.inner.read_range(path, offset, len)
    }

    fn write_file(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        self.inner.write_file(path, data)
    }

    fn create_file(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.create_file(path)
    }

    fn open_file(&self, path: &Path) -> io::Result<Box<dyn FileReader>> {
        self.inner.open_file(path)
    }

    fn open_file_for_write(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.open_file_for_write(path)
    }

    fn open_file_for_append(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.open_file_for_append(path)
    }

    fn set_file_length(&self, path: &Path, len: u64) -> io::Result<()> {
        self.inner.set_file_length(path, len)
    }

    fn rename(&self, from: &Path, to: &Path) -> io::Result<()> {
        self.inner.rename(from, to)
    }

    fn copy(&self, from: &Path, to: &Path) -> io::Result<u64> {
        self.inner.copy(from, to)
    }

    fn remove_file(&self, path: &Path) -> io::Result<()> {
        self.inner.remove_file(path)
    }

    fn remove_dir(&self, path: &Path) -> io::Result<()> {
        self.inner.remove_dir(path)
    }

    fn metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.inner.metadata(path)
    }

    fn metadata_if_exists(&self, path: &Path) -> Option<FileMetadata> {
        self.inner.metadata_if_exists(path)
    }

    fn symlink_metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.inner.symlink_metadata(path)
    }

    fn is_dir(&self, path: &Path) -> io::Result<bool> {
        self.inner.is_dir(path)
    }

    fn is_file(&self, path: &Path) -> io::Result<bool> {
        self.inner.is_file(path)
    }

    fn set_permissions(&self, path: &Path, permissions: &FilePermissions) -> io::Result<()> {
        self.inner.set_permissions(path, permissions)
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        self.inner.read_dir(path)
    }

    fn create_dir(&self, path: &Path) -> io::Result<()> {
        self.inner.create_dir(path)
    }

    fn create_dir_all(&self, path: &Path) -> io::Result<()> {
        self.inner.create_dir_all(path)
    }

    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        self.inner.canonicalize(path)
    }

    fn current_uid(&self) -> u32 {
        self.inner.current_uid()
    }

    fn sudo_write(
        &self,
        path: &Path,
        data: &[u8],
        mode: u32,
        uid: u32,
        gid: u32,
    ) -> io::Result<()> {
        self.inner.sudo_write(path, data, mode, uid, gid)
    }

    fn write_patched(
        &self,
        src: &Path,
        dest: &Path,
        ops: &[fresh::model::filesystem::WriteOp],
    ) -> io::Result<()> {
        self.inner.write_patched(src, dest, ops)
    }

    fn remote_connection_info(&self) -> Option<&str> {
        self.inner.remote_connection_info()
    }
}

// ============================================================================
// 1. THE MODEL (The "Gold Standard" - Stupidly Simple Implementation)
// ============================================================================

/// A simple shadow model that mirrors TextBuffer operations.
/// This is deliberately simple and inefficient, but obviously correct.
#[derive(Debug, Clone)]
struct ShadowModel {
    /// The content as a simple byte vector
    content: Vec<u8>,
    /// History of operations for debugging failures
    history: Vec<String>,
}

impl ShadowModel {
    fn new() -> Self {
        Self {
            content: Vec::new(),
            history: Vec::new(),
        }
    }

    fn from_bytes(content: Vec<u8>) -> Self {
        Self {
            content,
            history: vec!["INIT".to_string()],
        }
    }

    /// Insert bytes at the given position
    /// Matches TextBuffer/PieceTree behavior:
    /// - offset <= len: insert at position (or append at end)
    /// - offset > len: silently ignored (no-op)
    fn insert(&mut self, offset: usize, text: &[u8]) {
        if offset > self.content.len() {
            self.history.push(format!(
                "INSERT({}, {:?}) -> NOOP (offset > len={})",
                offset,
                String::from_utf8_lossy(&text[..text.len().min(20)]),
                self.content.len()
            ));
            return;
        }
        self.history.push(format!(
            "INSERT({}, {:?})",
            offset,
            String::from_utf8_lossy(&text[..text.len().min(20)])
        ));
        self.content.splice(offset..offset, text.iter().cloned());
    }

    /// Delete bytes starting at offset for the given length
    fn delete(&mut self, offset: usize, len: usize) {
        if offset >= self.content.len() {
            self.history.push(format!(
                "DELETE({}, {}) -> NOOP (out of bounds)",
                offset, len
            ));
            return;
        }
        let len = len.min(self.content.len() - offset);
        if len == 0 {
            return;
        }
        self.history.push(format!("DELETE({}, {})", offset, len));
        self.content.drain(offset..offset + len);
    }

    /// Replace entire content with new content
    fn replace(&mut self, new_content: &[u8]) {
        self.history
            .push(format!("REPLACE(len={})", new_content.len()));
        self.content = new_content.to_vec();
    }

    /// Get a specific line (0-indexed), including trailing newline if present
    /// Matches TextBuffer::get_line behavior
    fn get_line(&self, line: usize) -> Option<Vec<u8>> {
        let mut current_line = 0;
        let mut line_start = 0;

        for (i, &byte) in self.content.iter().enumerate() {
            if byte == b'\n' {
                if current_line == line {
                    // Include the newline in the result
                    return Some(self.content[line_start..=i].to_vec());
                }
                current_line += 1;
                line_start = i + 1;
            }
        }

        // Handle last line (no trailing newline)
        if current_line == line && line_start <= self.content.len() {
            Some(self.content[line_start..].to_vec())
        } else {
            None
        }
    }

    fn to_string(&self) -> String {
        String::from_utf8_lossy(&self.content).to_string()
    }

    fn len(&self) -> usize {
        self.content.len()
    }

    fn line_count(&self) -> usize {
        if self.content.is_empty() {
            1
        } else {
            self.content.iter().filter(|&&b| b == b'\n').count() + 1
        }
    }
}

// ============================================================================
// 2. THE SYSTEM UNDER TEST WRAPPER
// ============================================================================

struct TextBufferSUT {
    buffer: TextBuffer,
    fs: Arc<ConfigurableFileSystem>,
}

impl TextBufferSUT {
    fn new(fs: Arc<ConfigurableFileSystem>) -> Self {
        Self {
            buffer: TextBuffer::new(0, fs.clone()),
            fs,
        }
    }

    fn from_bytes(content: Vec<u8>, fs: Arc<ConfigurableFileSystem>) -> Self {
        Self {
            buffer: TextBuffer::from_bytes(content, fs.clone()),
            fs,
        }
    }

    #[allow(dead_code)]
    fn load_from_file(
        path: &std::path::Path,
        fs: Arc<ConfigurableFileSystem>,
    ) -> anyhow::Result<Self> {
        let buffer = TextBuffer::load_from_file(path, 0, fs.clone())?;
        Ok(Self { buffer, fs })
    }

    fn load_from_file_with_threshold(
        path: &std::path::Path,
        threshold: usize,
        fs: Arc<ConfigurableFileSystem>,
    ) -> anyhow::Result<Self> {
        let buffer = TextBuffer::load_from_file(path, threshold, fs.clone())?;
        Ok(Self { buffer, fs })
    }

    fn insert(&mut self, offset: usize, text: Vec<u8>) {
        self.buffer.insert_bytes(offset, text);
    }

    fn delete(&mut self, offset: usize, len: usize) {
        self.buffer.delete_bytes(offset, len);
    }

    fn replace_content(&mut self, content: &str) {
        self.buffer.replace_content(content);
    }

    fn save_to_file(&mut self, path: &std::path::Path) -> anyhow::Result<()> {
        self.buffer.save_to_file(path)
    }

    fn to_string(&self) -> String {
        self.buffer
            .to_string()
            .expect("Buffer content should be available")
    }

    fn try_to_string(&self) -> Option<String> {
        self.buffer.to_string()
    }

    /// Force load content (for large files) and return it
    fn get_all_text(&mut self) -> anyhow::Result<Vec<u8>> {
        let len = self.buffer.len();
        if len == 0 {
            return Ok(Vec::new());
        }
        self.buffer.get_text_range_mut(0, len)
    }

    fn len(&self) -> usize {
        self.buffer.len()
    }

    fn line_count(&self) -> Option<usize> {
        self.buffer.line_count()
    }

    fn is_large_file(&self) -> bool {
        self.buffer.is_large_file()
    }

    fn get_line(&self, line: usize) -> Option<Vec<u8>> {
        self.buffer.get_line(line)
    }

    fn filesystem(&self) -> &Arc<ConfigurableFileSystem> {
        &self.fs
    }
}

// ============================================================================
// 3. OPERATION TYPES
// ============================================================================

/// All operations that can be performed on the TextBuffer API
#[derive(Debug, Clone)]
enum Op {
    /// Insert text at a position
    Insert { offset: usize, text: Vec<u8> },
    /// Delete a range of bytes
    Delete { offset: usize, len: usize },
    /// Replace entire content
    ReplaceContent { text: Vec<u8> },
    /// Save to file and reload (persistence roundtrip)
    SaveAndReload,
    /// Change simulated file ownership (affects save path: atomic vs in-place write)
    SetFileOwnership { owned: bool },
}

// ============================================================================
// 4. TEST CONTEXT
// ============================================================================

struct TestContext {
    model: ShadowModel,
    sut: TextBufferSUT,
    fs: Arc<ConfigurableFileSystem>,
    _temp_dir: TempDir,
    save_path: PathBuf,
    step: usize,
    /// Whether the file has been saved at least once (exists on disk)
    file_exists: bool,
    /// Threshold for triggering large file mode (0 = use default 100MB)
    large_file_threshold: usize,
}

impl TestContext {
    fn new() -> Self {
        Self::with_full_config(Vec::new(), true, 0)
    }

    fn with_initial_content(content: Vec<u8>) -> Self {
        Self::with_full_config(content, true, 0)
    }

    #[allow(dead_code)]
    fn with_config(content: Vec<u8>, simulate_owner: bool) -> Self {
        Self::with_full_config(content, simulate_owner, 0)
    }

    /// Create test context with full configuration:
    /// - content: initial buffer content
    /// - simulate_owner: whether to simulate owning the file (false = in-place write)
    /// - large_file_threshold: threshold for large file mode (0 = default 100MB)
    fn with_full_config(
        content: Vec<u8>,
        simulate_owner: bool,
        large_file_threshold: usize,
    ) -> Self {
        let temp_dir = TempDir::new().expect("Failed to create temp directory");
        let save_path = temp_dir.path().join("buffer.txt");
        let fs = Arc::new(ConfigurableFileSystem::new(simulate_owner));

        let (model, sut) = if content.is_empty() {
            (ShadowModel::new(), TextBufferSUT::new(fs.clone()))
        } else {
            (
                ShadowModel::from_bytes(content.clone()),
                TextBufferSUT::from_bytes(content, fs.clone()),
            )
        };

        Self {
            model,
            sut,
            fs,
            _temp_dir: temp_dir,
            save_path,
            step: 0,
            file_exists: false,
            large_file_threshold,
        }
    }

    /// Apply an operation, adjusting parameters to be valid for current state
    fn apply(&mut self, op: &Op) -> Result<(), String> {
        let current_len = self.model.len();

        match op {
            Op::Insert { offset, text } => {
                // Clamp offset to valid range (0..=len)
                let valid_offset = (*offset).min(current_len);
                self.model.insert(valid_offset, text);
                self.sut.insert(valid_offset, text.clone());
            }
            Op::Delete { offset, len } => {
                if current_len == 0 {
                    return Ok(()); // Skip delete on empty buffer
                }
                let valid_offset = (*offset).min(current_len.saturating_sub(1));
                let available = current_len.saturating_sub(valid_offset);
                let valid_len = (*len).min(available).max(1);
                self.model.delete(valid_offset, valid_len);
                self.sut.delete(valid_offset, valid_len);
            }
            Op::ReplaceContent { text } => {
                self.model.replace(text);
                self.sut.replace_content(&String::from_utf8_lossy(text));
            }
            Op::SaveAndReload => {
                if current_len == 0 {
                    return Ok(()); // Skip save/reload on empty buffer
                }

                // If file doesn't exist yet and we're simulating non-owner,
                // we need to create the file first (as owner), then switch back.
                // This matches real-world: you can't "not own" a file that doesn't exist.
                let was_owner = self.fs.is_simulating_owner();
                let threshold = self.large_file_threshold;
                let is_large = threshold > 0 && current_len >= threshold;

                if !self.file_exists && !was_owner {
                    // Create file as owner first
                    self.fs.set_owner(true);
                    self.sut
                        .save_to_file(&self.save_path)
                        .map_err(|e| format!("Initial save failed: {}", e))?;
                    self.file_exists = true;

                    // Verify saved content matches model BEFORE reload
                    self.verify_file_on_disk()?;

                    // Switch back to non-owner for reload
                    self.fs.set_owner(false);
                    self.sut = TextBufferSUT::load_from_file_with_threshold(
                        &self.save_path,
                        threshold,
                        self.fs.clone(),
                    )
                    .map_err(|e| format!("Load failed: {}", e))?;
                    self.model.history.push(format!(
                        "SAVE_AND_RELOAD(owner=true->false, file_created=true, large={})",
                        is_large
                    ));
                } else {
                    self.model.history.push(format!(
                        "SAVE_AND_RELOAD(owner={}, file_exists={}, large={})",
                        was_owner, self.file_exists, is_large
                    ));
                    self.sut
                        .save_to_file(&self.save_path)
                        .map_err(|e| format!("Save failed: {}", e))?;
                    self.file_exists = true;

                    // Verify saved content matches model BEFORE reload
                    self.verify_file_on_disk()?;

                    self.sut = TextBufferSUT::load_from_file_with_threshold(
                        &self.save_path,
                        threshold,
                        self.fs.clone(),
                    )
                    .map_err(|e| format!("Load failed: {}", e))?;
                }
            }
            Op::SetFileOwnership { owned } => {
                self.fs.set_owner(*owned);
                self.model
                    .history
                    .push(format!("SET_FILE_OWNERSHIP({})", owned));
                // No verification needed - ownership doesn't affect model state
                return Ok(());
            }
        }

        self.step += 1;
        self.verify()
    }

    /// Verify model and SUT are in identical states
    fn verify(&mut self) -> Result<(), String> {
        // Check length
        if self.model.len() != self.sut.len() {
            return Err(format!(
                "Step {}: Length mismatch - model={}, sut={}",
                self.step,
                self.model.len(),
                self.sut.len()
            ));
        }

        // Check content
        // For large files, to_string() returns None until content is loaded,
        // so we use get_all_text() which forces loading
        let model_content = self.model.content.clone();
        let sut_content = if self.sut.is_large_file() {
            self.sut
                .get_all_text()
                .map_err(|e| format!("Step {}: Failed to get SUT content: {}", self.step, e))?
        } else {
            self.sut.to_string().into_bytes()
        };

        if model_content != sut_content {
            let diff_pos = model_content
                .iter()
                .zip(sut_content.iter())
                .position(|(a, b)| a != b)
                .unwrap_or(model_content.len().min(sut_content.len()));
            return Err(format!(
                "Step {}: Content mismatch at byte {}\nModel: {:?}\nSUT: {:?}",
                self.step,
                diff_pos,
                String::from_utf8_lossy(&model_content[..model_content.len().min(100)]),
                String::from_utf8_lossy(&sut_content[..sut_content.len().min(100)])
            ));
        }

        // Check line count (only for non-large files, as large files don't have line indexing)
        if !self.sut.is_large_file() {
            if let Some(sut_lines) = self.sut.line_count() {
                let model_lines = self.model.line_count();
                if sut_lines != model_lines {
                    return Err(format!(
                        "Step {}: Line count mismatch - model={}, sut={}",
                        self.step, model_lines, sut_lines
                    ));
                }
            }
        }

        Ok(())
    }

    /// Verify that the file on disk matches the model's expected content.
    /// This catches bugs where save corrupts data but load might mask it.
    fn verify_file_on_disk(&self) -> Result<(), String> {
        let disk_content = std::fs::read(&self.save_path)
            .map_err(|e| format!("Step {}: Failed to read file from disk: {}", self.step, e))?;

        let model_content = &self.model.content;

        if disk_content.len() != model_content.len() {
            return Err(format!(
                "Step {}: Disk file size mismatch - expected {} bytes, got {} bytes on disk",
                self.step,
                model_content.len(),
                disk_content.len()
            ));
        }

        if &disk_content != model_content {
            let diff_pos = disk_content
                .iter()
                .zip(model_content.iter())
                .position(|(a, b)| a != b)
                .unwrap_or(0);
            return Err(format!(
                "Step {}: Disk content mismatch at byte {}\nExpected: {:?}\nOn disk: {:?}",
                self.step,
                diff_pos,
                String::from_utf8_lossy(&model_content[..model_content.len().min(100)]),
                String::from_utf8_lossy(&disk_content[..disk_content.len().min(100)])
            ));
        }

        Ok(())
    }

    fn history(&self) -> String {
        self.model
            .history
            .iter()
            .enumerate()
            .map(|(i, h)| format!("  {}: {}", i, h))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

// ============================================================================
// 5. OPERATION GENERATOR
// ============================================================================

/// Generate random text for insertions
fn text_strategy() -> impl Strategy<Value = Vec<u8>> {
    prop_oneof![
        // Single characters
        4 => prop::char::range('a', 'z').prop_map(|c| vec![c as u8]),
        // Short ASCII strings
        3 => "[a-zA-Z0-9 .,!?]{1,20}".prop_map(|s| s.into_bytes()),
        // Strings with newlines
        2 => "([a-z]+\n){1,5}".prop_map(|s| s.into_bytes()),
        // Longer strings
        1 => "[a-zA-Z0-9 \n\t]{20,100}".prop_map(|s| s.into_bytes()),
        // Whitespace
        1 => "[\n\r\t ]{1,10}".prop_map(|s| s.into_bytes()),
        // Empty (no-op)
        1 => Just(Vec::new()),
    ]
}

/// Generate a single operation
fn op_strategy() -> impl Strategy<Value = Op> {
    prop_oneof![
        // Insert (most common)
        5 => (0usize..2000, text_strategy())
            .prop_map(|(offset, text)| Op::Insert { offset, text }),
        // Delete
        3 => (0usize..2000, 1usize..100)
            .prop_map(|(offset, len)| Op::Delete { offset, len }),
        // Replace content (occasional)
        1 => text_strategy().prop_map(|text| Op::ReplaceContent { text }),
        // Save and reload (occasional)
        2 => Just(Op::SaveAndReload),
        // Toggle file ownership (tests both atomic and in-place write paths)
        1 => prop::bool::ANY.prop_map(|owned| Op::SetFileOwnership { owned }),
    ]
}

/// Generate a sequence of operations
fn ops_strategy(max_ops: usize) -> impl Strategy<Value = Vec<Op>> {
    prop::collection::vec(op_strategy(), 1..max_ops)
}

/// Generate initial content
fn initial_content_strategy() -> impl Strategy<Value = Vec<u8>> {
    prop_oneof![
        // Empty start
        2 => Just(Vec::new()),
        // Small content
        3 => "[a-zA-Z0-9 ]{1,50}".prop_map(|s| s.into_bytes()),
        // Content with newlines
        2 => "([a-z]+\n){1,10}".prop_map(|s| s.into_bytes()),
        // Larger content
        1 => "[a-zA-Z0-9 \n]{100,500}".prop_map(|s| s.into_bytes()),
    ]
}

// ============================================================================
// 6. THE SINGLE COMPREHENSIVE PROPERTY TEST
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 500,
        max_shrink_iters: 5000,
        verbose: 1,
        ..ProptestConfig::default()
    })]

    /// Comprehensive shadow model test for the entire TextBuffer API
    ///
    /// This single property test covers:
    /// - Insert operations at any valid position
    /// - Delete operations of any valid range
    /// - Save/Load persistence roundtrips
    /// - Empty, small, and large initial content
    /// - Newlines and line counting
    /// - Boundary conditions (start, end, middle)
    /// - Random operation sequences
    ///
    /// After every operation, verifies:
    /// - Content equality between model and SUT
    /// - Length invariant
    /// - Line count invariant
    #[test]
    fn prop_textbuffer_shadow_model(
        initial in initial_content_strategy(),
        ops in ops_strategy(100)
    ) {
        let mut ctx = if initial.is_empty() {
            TestContext::new()
        } else {
            TestContext::with_initial_content(initial)
        };

        // Verify initial state
        ctx.verify().map_err(|e| TestCaseError::fail(format!("Initial: {}", e)))?;

        // Apply all operations
        for op in &ops {
            if let Err(msg) = ctx.apply(op) {
                return Err(TestCaseError::fail(format!(
                    "{}\n\nOperation: {:?}\n\nHistory:\n{}",
                    msg,
                    op,
                    ctx.history()
                )));
            }
        }
    }
}

// ============================================================================
// 7. TARGETED REGRESSION TESTS
// ============================================================================

#[test]
fn test_empty_buffer() {
    let mut ctx = TestContext::new();
    assert_eq!(ctx.model.len(), 0);
    assert_eq!(ctx.sut.len(), 0);
    ctx.verify().unwrap();
}

#[test]
fn test_insert_delete_cycle() {
    let mut ctx = TestContext::new();

    ctx.apply(&Op::Insert {
        offset: 0,
        text: b"hello".to_vec(),
    })
    .unwrap();

    ctx.apply(&Op::Delete { offset: 0, len: 5 }).unwrap();

    assert_eq!(ctx.model.len(), 0);
}

#[test]
fn test_persistence_roundtrip() {
    let mut ctx = TestContext::with_initial_content(b"test content".to_vec());

    ctx.apply(&Op::Insert {
        offset: 5,
        text: b" new".to_vec(),
    })
    .unwrap();

    ctx.apply(&Op::SaveAndReload).unwrap();

    // "test content" with " new" inserted at offset 5 = "test " + " new" + "content"
    assert_eq!(ctx.model.to_string(), "test  newcontent");
}

#[test]
fn test_insert_past_end_clamps_to_append() {
    let mut ctx = TestContext::with_initial_content(b"hello".to_vec());

    // Insert at offset > len gets clamped to append at end
    ctx.apply(&Op::Insert {
        offset: 100,
        text: b"world".to_vec(),
    })
    .unwrap();

    assert_eq!(ctx.model.to_string(), "helloworld");
}

#[test]
fn test_newline_counting() {
    let mut ctx = TestContext::new();

    ctx.apply(&Op::Insert {
        offset: 0,
        text: b"a\nb\nc".to_vec(),
    })
    .unwrap();

    assert_eq!(ctx.model.line_count(), 3);
    assert_eq!(ctx.sut.line_count(), Some(3));
}

#[test]
fn test_large_content_with_edits() {
    let large: Vec<u8> = (0..5000).map(|i| b'a' + (i % 26) as u8).collect();
    let mut ctx = TestContext::with_initial_content(large);

    // Scattered edits
    for pos in [0, 1000, 2500, 4000, 4999] {
        ctx.apply(&Op::Insert {
            offset: pos.min(ctx.model.len()),
            text: b"X".to_vec(),
        })
        .unwrap();
    }

    ctx.apply(&Op::SaveAndReload).unwrap();
    ctx.verify().unwrap();
}

#[test]
fn test_multiple_save_cycles() {
    let mut ctx = TestContext::with_initial_content(b"start".to_vec());

    for i in 0..5 {
        ctx.apply(&Op::Insert {
            offset: ctx.model.len(),
            text: format!("-{}", i).into_bytes(),
        })
        .unwrap();

        ctx.apply(&Op::SaveAndReload).unwrap();
    }

    assert_eq!(ctx.model.to_string(), "start-0-1-2-3-4");
}

#[test]
fn test_replace_content() {
    let mut ctx = TestContext::with_initial_content(b"initial content".to_vec());

    ctx.apply(&Op::ReplaceContent {
        text: b"completely new".to_vec(),
    })
    .unwrap();

    assert_eq!(ctx.model.to_string(), "completely new");
    assert_eq!(ctx.sut.to_string(), "completely new");
}

#[test]
fn test_get_line() {
    let ctx = TestContext::with_initial_content(b"line0\nline1\nline2".to_vec());

    // Model get_line (includes trailing newline where present)
    assert_eq!(ctx.model.get_line(0), Some(b"line0\n".to_vec()));
    assert_eq!(ctx.model.get_line(1), Some(b"line1\n".to_vec()));
    assert_eq!(ctx.model.get_line(2), Some(b"line2".to_vec())); // No trailing newline
    assert_eq!(ctx.model.get_line(3), None);

    // SUT get_line should match model
    assert_eq!(ctx.sut.get_line(0), ctx.model.get_line(0));
    assert_eq!(ctx.sut.get_line(1), ctx.model.get_line(1));
    assert_eq!(ctx.sut.get_line(2), ctx.model.get_line(2));
    assert_eq!(ctx.sut.get_line(3), ctx.model.get_line(3));
}

// ============================================================================
// 8. LARGE FILE MODE TESTS
// ============================================================================

/// Test that large file mode works correctly with shadow model verification.
/// Large files use lazy loading, so to_string() returns None until content is accessed.
#[test]
fn test_large_file_mode_operations() {
    use std::fs::File;
    use std::io::Write;

    let temp_dir = TempDir::new().expect("Failed to create temp directory");
    let file_path = temp_dir.path().join("large_test.txt");
    let fs = Arc::new(ConfigurableFileSystem::new(true));

    // Create a file with known content
    let content = b"line1\nline2\nline3\nline4\nline5";
    File::create(&file_path)
        .unwrap()
        .write_all(content)
        .unwrap();

    // Load with small threshold to trigger large file mode
    let mut sut = TextBufferSUT::load_from_file_with_threshold(&file_path, 10, fs)
        .expect("Failed to load file");

    // Should be in large file mode
    assert!(sut.is_large_file(), "Should be in large file mode");

    // Length should still work
    assert_eq!(sut.len(), content.len());

    // to_string() returns None for large files with unloaded content
    assert!(
        sut.try_to_string().is_none(),
        "to_string should be None for large file"
    );

    // line_count returns None for large files
    assert!(
        sut.line_count().is_none(),
        "line_count should be None for large file"
    );

    // After forcing load, content should be available
    let loaded_content = sut.get_all_text().expect("Should be able to load content");
    assert_eq!(loaded_content, content.to_vec());

    // Now to_string should work
    assert_eq!(
        sut.try_to_string(),
        Some("line1\nline2\nline3\nline4\nline5".to_string())
    );
}

/// Test large file mode with edits and save/reload (as file owner - atomic write)
#[test]
fn test_large_file_mode_with_edits() {
    use std::fs::File;
    use std::io::Write;

    let temp_dir = TempDir::new().expect("Failed to create temp directory");
    let file_path = temp_dir.path().join("large_edit_test.txt");
    let fs = Arc::new(ConfigurableFileSystem::new(true)); // We "own" the file

    // Create initial file
    let initial = b"AAABBBCCC";
    File::create(&file_path)
        .unwrap()
        .write_all(initial)
        .unwrap();

    // Load as large file
    let mut sut = TextBufferSUT::load_from_file_with_threshold(&file_path, 5, fs)
        .expect("Failed to load file");
    assert!(sut.is_large_file());

    // Insert at the beginning
    sut.insert(0, b"START".to_vec());

    // Verify content after edit (forces load)
    let content = sut.get_all_text().unwrap();
    assert_eq!(content, b"STARTAAABBBCCC");

    // Save the file
    sut.save_to_file(&file_path).expect("Save should succeed");

    // Verify saved content
    let saved = std::fs::read(&file_path).unwrap();
    assert_eq!(saved, b"STARTAAABBBCCC");
}

/// Test large file mode with in-place write (simulates file owned by another user).
/// This exercises the code path that had the data corruption bug where the file
/// was truncated before Copy operations could read from it.
#[test]
#[cfg(unix)] // In-place write is only used on Unix for ownership preservation
fn test_large_file_inplace_write() {
    use std::fs::File;
    use std::io::Write;

    let temp_dir = TempDir::new().expect("Failed to create temp directory");
    let file_path = temp_dir.path().join("large_inplace_test.txt");
    // Simulate NOT owning the file - this triggers in-place write path
    let fs = Arc::new(ConfigurableFileSystem::new(false));

    // Create a large file with recognizable content
    let mut content = String::new();
    for i in 0..100 {
        content.push_str(&format!("Line {:03}: test content\n", i));
    }
    let original_len = content.len();
    File::create(&file_path)
        .unwrap()
        .write_all(content.as_bytes())
        .unwrap();

    // Load as large file (threshold smaller than file size)
    let mut sut = TextBufferSUT::load_from_file_with_threshold(&file_path, 100, fs)
        .expect("Failed to load file");

    assert!(sut.is_large_file(), "Should be in large file mode");
    assert!(
        !sut.filesystem().is_simulating_owner(),
        "Should simulate non-owner"
    );

    // Make a small edit at the beginning - this creates Insert + Copy operations
    sut.insert(0, b"EDITED: ".to_vec());

    // Save - this uses in-place write because we don't "own" the file
    // The bug would cause this to corrupt the file by truncating before reading Copy chunks
    sut.save_to_file(&file_path)
        .expect("Save should succeed without corruption");

    // Verify the saved content is correct
    let saved = std::fs::read_to_string(&file_path).unwrap();
    let expected_len = original_len + 8; // "EDITED: " is 8 bytes

    assert_eq!(
        saved.len(),
        expected_len,
        "File size mismatch! Expected {} but got {}. This indicates data corruption.",
        expected_len,
        saved.len()
    );
    assert!(
        saved.starts_with("EDITED: Line 000"),
        "Edit should be at the start"
    );
    assert!(
        saved.contains("Line 099"),
        "Original content should be preserved"
    );
}

/// Property test that exercises in-place write path with large file mode.
/// This tests the exact scenario that caused the data corruption bug:
/// - Large file mode (content >= threshold, triggers lazy loading)
/// - Non-owner mode (triggers in-place write path)
/// - Edit + Save (creates Copy operations that need to read from original file)
#[test]
#[cfg(unix)]
fn prop_inplace_write_preserves_content() {
    use proptest::test_runner::{Config, TestRunner};

    let config = Config::with_cases(100);
    let mut runner = TestRunner::new(config);

    runner
        .run(
            &(
                // Generate content larger than threshold to trigger large file mode
                "[a-zA-Z0-9 \n]{200,1000}".prop_map(|s| s.into_bytes()),
                // Generate operations including ownership toggles
                prop::collection::vec(op_strategy(), 10..50),
            ),
            |(initial, ops)| {
                // Use small threshold (100 bytes) to trigger large file mode
                // Start with non-owner to exercise in-place write path
                let mut ctx = TestContext::with_full_config(initial, false, 100);

                for op in ops {
                    if let Err(e) = ctx.apply(&op) {
                        return Err(proptest::test_runner::TestCaseError::Fail(
                            format!("Operation failed: {}\nHistory:\n{}", e, ctx.history()).into(),
                        ));
                    }
                }
                Ok(())
            },
        )
        .expect("Property test failed");
}
