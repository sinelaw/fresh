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
