//! Git test helper - creates hermetic git repositories for testing

use std::fs;
use std::path::PathBuf;
use std::process::Command;
use tempfile::TempDir;

/// A hermetic git repository for testing
pub struct GitTestRepo {
    /// Temporary directory containing the git repository
    _temp_dir: TempDir,
    /// Path to the git repository root
    pub path: PathBuf,
}

impl GitTestRepo {
    /// Create a new git test repository with test files
    pub fn new() -> Self {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let path = temp_dir.path().to_path_buf();

        // Initialize git repository
        let output = Command::new("git")
            .arg("init")
            .current_dir(&path)
            .output()
            .expect("Failed to initialize git repository");

        if !output.status.success() {
            panic!(
                "git init failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }

        // Configure git user for commits
        Command::new("git")
            .args(["config", "user.name", "Test User"])
            .current_dir(&path)
            .output()
            .expect("Failed to configure git user.name");

        Command::new("git")
            .args(["config", "user.email", "test@example.com"])
            .current_dir(&path)
            .output()
            .expect("Failed to configure git user.email");

        // Disable GPG signing for test commits
        Command::new("git")
            .args(["config", "commit.gpgsign", "false"])
            .current_dir(&path)
            .output()
            .expect("Failed to disable GPG signing");

        GitTestRepo {
            _temp_dir: temp_dir,
            path,
        }
    }

    /// Create a file with content
    pub fn create_file(&self, relative_path: &str, content: &str) -> PathBuf {
        let file_path = self.path.join(relative_path);

        // Create parent directories if needed
        if let Some(parent) = file_path.parent() {
            fs::create_dir_all(parent).expect("Failed to create parent directories");
        }

        fs::write(&file_path, content).expect("Failed to write file");
        file_path
    }

    /// Add files to git staging area
    pub fn git_add(&self, paths: &[&str]) {
        for path in paths {
            let output = Command::new("git")
                .args(["add", path])
                .current_dir(&self.path)
                .output()
                .expect("Failed to run git add");

            if !output.status.success() {
                panic!(
                    "git add failed: {}",
                    String::from_utf8_lossy(&output.stderr)
                );
            }
        }
    }

    /// Add all files to git
    pub fn git_add_all(&self) {
        let output = Command::new("git")
            .args(["add", "."])
            .current_dir(&self.path)
            .output()
            .expect("Failed to run git add .");

        if !output.status.success() {
            panic!(
                "git add . failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
    }

    /// Commit staged changes
    pub fn git_commit(&self, message: &str) {
        let output = Command::new("git")
            .args(["commit", "-m", message])
            .current_dir(&self.path)
            .output()
            .expect("Failed to run git commit");

        if !output.status.success() {
            panic!(
                "git commit failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
    }

    /// Set up a typical project structure for testing
    pub fn setup_typical_project(&self) {
        // Create source files with searchable content
        self.create_file(
            "src/main.rs",
            r#"fn main() {
    println!("Hello, world!");
    let config = load_config();
    start_server(config);
}

fn load_config() -> Config {
    Config::default()
}

fn start_server(config: Config) {
    println!("Starting server...");
}
"#,
        );

        self.create_file(
            "src/lib.rs",
            r#"pub struct Config {
    pub port: u16,
    pub host: String,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            port: 8080,
            host: "localhost".to_string(),
        }
    }
}

pub fn process_request(data: &str) -> String {
    format!("Processed: {}", data)
}
"#,
        );

        self.create_file(
            "src/utils.rs",
            r#"pub fn format_output(msg: &str) -> String {
    format!("[INFO] {}", msg)
}

pub fn validate_config(config: &Config) -> bool {
    config.port > 0 && !config.host.is_empty()
}
"#,
        );

        self.create_file(
            "tests/integration.rs",
            r#"#[test]
fn test_config_default() {
    let config = Config::default();
    assert_eq!(config.port, 8080);
}

#[test]
fn test_process_request() {
    let result = process_request("test");
    assert_eq!(result, "Processed: test");
}
"#,
        );

        self.create_file(
            "Cargo.toml",
            r#"[package]
name = "test-project"
version = "0.1.0"
edition = "2021"

[dependencies]
serde = "1.0"
tokio = { version = "1.0", features = ["full"] }
"#,
        );

        self.create_file(
            "README.md",
            r#"# Test Project

A sample project for testing.

## Features

- Configuration management
- Request processing
- Server functionality
"#,
        );

        // Add and commit all files
        self.git_add_all();
        self.git_commit("Initial commit");
    }

    /// Set up a project with many files for scrolling tests
    pub fn setup_many_files(&self, count: usize) {
        for i in 0..count {
            let dir_num = i / 10;
            let file_name = format!("dir{dir_num}/file{i}.txt");
            let content = format!("This is file number {i}\nSearchable content here\nLine 3");
            self.create_file(&file_name, &content);
        }

        self.git_add_all();
        self.git_commit("Add many files");
    }

    /// Change current directory to this repository
    pub fn change_to_repo_dir(&self) -> PathBuf {
        // Try to get current dir, but if it fails (e.g., directory was deleted),
        // use a safe fallback like /tmp
        let original_dir = std::env::current_dir().unwrap_or_else(|_| {
            // If current dir doesn't exist, use /tmp as a safe default
            std::path::PathBuf::from("/tmp")
        });
        std::env::set_current_dir(&self.path).expect("Failed to change directory");
        original_dir
    }
}

/// Helper to restore original directory
pub struct DirGuard {
    original_dir: PathBuf,
}

impl DirGuard {
    pub fn new(original_dir: PathBuf) -> Self {
        DirGuard { original_dir }
    }
}

impl Drop for DirGuard {
    fn drop(&mut self) {
        let _ = std::env::set_current_dir(&self.original_dir);
    }
}
