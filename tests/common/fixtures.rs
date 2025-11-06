// Test file fixtures

use std::fs;
use std::io::Write;
use std::path::PathBuf;
use tempfile::TempDir;

/// Manages temporary test files
pub struct TestFixture {
    _temp_dir: TempDir,
    pub path: PathBuf,
}

impl TestFixture {
    /// Create a new temporary file with given content
    pub fn new(filename: &str, content: &str) -> std::io::Result<Self> {
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
    pub fn empty(filename: &str) -> std::io::Result<Self> {
        Self::new(filename, "")
    }

    /// Read the current content of the file
    pub fn read_content(&self) -> std::io::Result<String> {
        fs::read_to_string(&self.path)
    }

    /// Create a large file with repeated content (for performance testing)
    pub fn large(filename: &str, size_mb: usize) -> std::io::Result<Self> {
        let temp_dir = tempfile::tempdir()?;
        let path = temp_dir.path().join(filename);

        let mut file = fs::File::create(&path)?;
        let line = "x".repeat(80) + "\n";
        let lines_per_mb = 1024 * 1024 / line.len();

        for _ in 0..(size_mb * lines_per_mb) {
            file.write_all(line.as_bytes())?;
        }
        file.flush()?;

        Ok(TestFixture {
            _temp_dir: temp_dir,
            path,
        })
    }

    /// Get or create BIG.txt in tests/ directory (61MB, persistent across test runs)
    /// This file is gitignored and generated on first use
    ///
    /// **DEPRECATED**: Use `big_txt_for_test()` instead to avoid test interference.
    /// This method is kept for backwards compatibility but should not be used in new tests.
    pub fn big_txt() -> std::io::Result<PathBuf> {
        let path = PathBuf::from("tests/BIG.txt");

        // Only generate if it doesn't exist
        if !path.exists() {
            eprintln!("Generating tests/BIG.txt (61MB, one-time)...");
            let mut file = fs::File::create(&path)?;
            let line = "x".repeat(80) + "\n";
            let lines_per_mb = 1024 * 1024 / line.len();
            let size_mb = 61;

            for _ in 0..(size_mb * lines_per_mb) {
                file.write_all(line.as_bytes())?;
            }
            file.flush()?;
            eprintln!("Generated tests/BIG.txt successfully");
        }

        Ok(path)
    }

    /// Create a test-specific large file (61MB) in a temporary directory.
    /// This ensures tests are hermetic and don't interfere with each other.
    /// Each test gets its own copy, preventing race conditions during parallel test execution.
    ///
    /// The file is automatically cleaned up when the returned TestFixture is dropped.
    pub fn big_txt_for_test(test_name: &str) -> std::io::Result<Self> {
        let temp_dir = tempfile::tempdir()?;
        let filename = format!("BIG-{}.txt", test_name);
        let path = temp_dir.path().join(&filename);

        eprintln!("Generating temporary large file for test '{}' (61MB)...", test_name);
        let mut file = fs::File::create(&path)?;
        let line = "x".repeat(80) + "\n";
        let lines_per_mb = 1024 * 1024 / line.len();
        let size_mb = 61;

        for _ in 0..(size_mb * lines_per_mb) {
            file.write_all(line.as_bytes())?;
        }
        file.flush()?;
        eprintln!("Generated temporary large file for test '{}'", test_name);

        Ok(TestFixture {
            _temp_dir: temp_dir,
            path,
        })
    }
}

/// Create a consistent temporary directory for a test
/// This ensures snapshot tests use the same paths on each run
pub fn test_temp_dir(test_name: &str) -> std::io::Result<PathBuf> {
    let path = std::env::temp_dir().join(format!("editor-test-{}", test_name));
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
