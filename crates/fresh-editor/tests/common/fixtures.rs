// Test file fixtures

use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::sync::{Mutex, OnceLock};
use tempfile::TempDir;

/// Manages temporary test files
pub struct TestFixture {
    _temp_dir: TempDir,
    pub path: PathBuf,
}

impl TestFixture {
    /// Create a new temporary file with given content
    pub fn new(filename: &str, content: &str) -> anyhow::Result<Self> {
        let temp_dir = tempfile::tempdir()?;
        let path = temp_dir.path().join(filename);

        let mut file = fs::File::create(&path)?;
        file.write_all(content.as_bytes())?;
        file.flush()?;

        Ok(TestFixture {
            _temp_dir: temp_dir,
            path,
        })
    }

    /// Create an empty temporary file
    pub fn empty(filename: &str) -> anyhow::Result<Self> {
        Self::new(filename, "")
    }

    /// Read the current content of the file
    pub fn read_content(&self) -> anyhow::Result<String> {
        Ok(fs::read_to_string(&self.path)?)
    }

    /// Get or create a shared large file (61MB) for all tests.
    /// Uses locking to ensure only one test creates the file, even when tests run in parallel.
    /// All concurrent tests share the same file, which is much more efficient than creating
    /// separate files per test.
    ///
    /// The file persists across test runs in the system temp directory and is reused.
    ///
    /// Note: The test_name parameter is kept for API compatibility but is no longer used
    /// since all tests share the same file.
    pub fn big_txt_for_test(_test_name: &str) -> anyhow::Result<PathBuf> {
        // Global lock and path storage for thread-safe initialization
        static BIG_TXT_INIT: OnceLock<Mutex<PathBuf>> = OnceLock::new();

        let path_mutex = BIG_TXT_INIT.get_or_init(|| {
            // Create path in system temp directory with predictable name
            let path = std::env::temp_dir().join("fresh-test-BIG.txt");
            Mutex::new(path)
        });

        // Lock to ensure only one test creates the file
        let path = path_mutex.lock().unwrap().clone();

        // Check if file already exists
        if !path.exists() {
            eprintln!("Generating shared large test file (61MB, one-time)...");
            let mut file = fs::File::create(&path)?;

            // Each line: "@00000000: " + 'x' repeated to fill ~80 chars total + "\n"
            // Byte offset prefix is 12 chars ("@00000000: "), so ~68 x's per line
            let size_mb = 61;
            let target_bytes = size_mb * 1024 * 1024;

            let mut byte_offset = 0;

            while byte_offset < target_bytes {
                let line = format!("@{:08}: {}\n", byte_offset, "x".repeat(68));
                file.write_all(line.as_bytes())?;
                byte_offset += line.len();
            }

            file.flush()?;
            let line_count = byte_offset / 81; // Each line is 81 bytes
            eprintln!(
                "Generated shared large test file with ~{} lines ({} bytes) at {path:?}",
                line_count, byte_offset
            );
        }

        Ok(path)
    }
}

/// Create a consistent temporary directory for a test
/// This ensures snapshot tests use the same paths on each run
pub fn test_temp_dir(test_name: &str) -> anyhow::Result<PathBuf> {
    let path = std::env::temp_dir().join(format!("editor-test-{test_name}"));
    if path.exists() {
        fs::remove_dir_all(&path)?;
    }
    fs::create_dir_all(&path)?;
    Ok(path)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fixture_new() {
        let fixture = TestFixture::new("test.txt", "hello world").unwrap();
        assert_eq!(fixture.read_content().unwrap(), "hello world");
    }

    #[test]
    fn test_fixture_empty() {
        let fixture = TestFixture::empty("empty.txt").unwrap();
        assert_eq!(fixture.read_content().unwrap(), "");
    }
}
