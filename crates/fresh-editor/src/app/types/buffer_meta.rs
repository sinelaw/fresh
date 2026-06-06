use super::lsp_uri::LspUri;
use rust_i18n::t;
use std::collections::HashSet;
use std::path::{Path, PathBuf};

/// The kind of buffer (file-backed or virtual)
#[derive(Debug, Clone, PartialEq)]
pub enum BufferKind {
    /// A buffer backed by a file on disk
    File {
        /// Host-side path to the file. Filesystem APIs and the
        /// editor's own buffer state always speak in host paths.
        path: PathBuf,
        /// LSP-facing URI for the file. Already translated for the
        /// active authority, so handing this to the LSP server is
        /// always correct. See [`LspUri`] for the why.
        uri: Option<LspUri>,
    },
    /// A virtual buffer (not backed by a file)
    /// Used for special buffers like *Diagnostics*, *Grep*, etc.
    Virtual {
        /// The buffer's mode (e.g., "diagnostics-list", "grep-results")
        mode: String,
    },
}

/// Metadata associated with a buffer
#[derive(Debug, Clone)]
pub struct BufferMetadata {
    /// The kind of buffer (file or virtual)
    pub kind: BufferKind,

    /// Display name for the buffer (project-relative path or filename or *BufferName*)
    pub display_name: String,

    /// Whether LSP is enabled for this buffer (always false for virtual buffers)
    pub lsp_enabled: bool,

    /// Reason LSP is disabled (if applicable)
    pub lsp_disabled_reason: Option<String>,

    /// Whether the buffer is read-only (typically true for virtual buffers)
    pub read_only: bool,

    /// Whether the buffer contains binary content
    /// Binary buffers are automatically read-only and render unprintable chars as code points
    pub binary: bool,

    /// LSP server instance IDs that have received didOpen for this buffer.
    /// Used to ensure didOpen is sent before any requests to a new/restarted server.
    /// When a server restarts, it gets a new ID, so didOpen is automatically resent.
    /// Old IDs are harmless - they just remain in the set but don't match any active server.
    pub lsp_opened_with: HashSet<u64>,

    /// Whether this buffer should be hidden from tabs (used for composite source buffers)
    pub hidden_from_tabs: bool,

    /// Whether auto-revert (reload on external file change) should
    /// fire for this buffer. Defaults to true for any user-opened
    /// file. Plugins that drive the buffer's contents themselves —
    /// `openFileStreaming` is the prototype — set this to false so
    /// the file-watcher's reload doesn't race with their own
    /// `extend_streaming` calls.
    pub auto_revert_enabled: bool,

    /// Whether this buffer is a synthetic placeholder created when the user
    /// closed their last buffer with `auto_create_empty_buffer_on_last_buffer_close`
    /// disabled. The editor's invariants require at least one buffer at all
    /// times, so we keep this one around but render the split pane as blank
    /// (no line numbers, no `~` filler) and hide it from tabs to give the
    /// user a truly empty workspace.
    pub synthetic_placeholder: bool,

    /// Whether this buffer is opened in "preview" mode (ephemeral).
    /// A preview buffer is one opened by a single-click in the file explorer
    /// (or a similar soft-open gesture). Its tab is rendered in italic and
    /// it is replaced the next time another file is opened the same way.
    /// The flag is cleared ("promoted") when the user edits the buffer,
    /// double-clicks the file, or otherwise signals commitment to the file.
    ///
    /// Intentionally ephemeral — never serialized into workspace or
    /// recovery state. Restarting the editor always brings buffers back
    /// as permanent tabs; preview status belongs to the current session's
    /// exploration flow only.
    pub is_preview: bool,

    /// Stable recovery ID for unnamed buffers.
    /// For file-backed buffers, recovery ID is computed from the path hash.
    /// For unnamed buffers, this is generated once and reused across auto-saves.
    pub recovery_id: Option<String>,
}

impl BufferMetadata {
    /// Get the file path if this is a file-backed buffer
    pub fn file_path(&self) -> Option<&PathBuf> {
        match &self.kind {
            BufferKind::File { path, .. } => Some(path),
            BufferKind::Virtual { .. } => None,
        }
    }

    /// Get the LSP-facing URI if this is a file-backed buffer.
    ///
    /// The URI is already translated for the active authority — i.e.
    /// it carries the in-container path on a devcontainer authority
    /// and the host path elsewhere. Hand it to the LSP server
    /// directly; do NOT pass it to filesystem APIs (use
    /// [`Self::file_path`] for that).
    pub fn file_uri(&self) -> Option<&LspUri> {
        match &self.kind {
            BufferKind::File { uri, .. } => uri.as_ref(),
            BufferKind::Virtual { .. } => None,
        }
    }

    /// Check if this is a virtual buffer
    pub fn is_virtual(&self) -> bool {
        matches!(self.kind, BufferKind::Virtual { .. })
    }

    /// Get the mode name for virtual buffers
    pub fn virtual_mode(&self) -> Option<&str> {
        match &self.kind {
            BufferKind::Virtual { mode } => Some(mode),
            BufferKind::File { .. } => None,
        }
    }
}

impl Default for BufferMetadata {
    fn default() -> Self {
        Self::new()
    }
}

impl BufferMetadata {
    /// Create new metadata for a buffer (unnamed, file-backed)
    pub fn new() -> Self {
        Self {
            kind: BufferKind::File {
                path: PathBuf::new(),
                uri: None,
            },
            display_name: t!("buffer.no_name").to_string(),
            lsp_enabled: true,
            lsp_disabled_reason: None,
            read_only: false,
            binary: false,
            lsp_opened_with: HashSet::new(),
            hidden_from_tabs: false,
            auto_revert_enabled: true,
            synthetic_placeholder: false,
            is_preview: false,
            recovery_id: None,
        }
    }

    /// Create new metadata for an unnamed buffer with a custom display name
    /// Used for buffers created from stdin or other non-file sources
    pub fn new_unnamed(display_name: String) -> Self {
        Self {
            kind: BufferKind::File {
                path: PathBuf::new(),
                uri: None,
            },
            display_name,
            lsp_enabled: false, // No file path, so no LSP
            lsp_disabled_reason: Some(t!("lsp.disabled.unnamed").to_string()),
            read_only: false,
            binary: false,
            lsp_opened_with: HashSet::new(),
            auto_revert_enabled: true,
            hidden_from_tabs: false,
            synthetic_placeholder: false,
            is_preview: false,
            recovery_id: None,
        }
    }

    /// Create metadata for a file-backed buffer
    ///
    /// # Arguments
    /// * `canonical_path` - The canonical (symlink-resolved) absolute path to the file
    /// * `display_path` - The user-visible path before canonicalization (for library detection)
    /// * `working_dir` - The canonical working directory for computing relative display name
    /// * `path_translation` - Active authority's host↔remote workspace mapping;
    ///   used to build the LSP-facing `file_uri` so an in-container LSP sees
    ///   in-container paths. `None` for local/SSH authorities.
    pub fn with_file(
        canonical_path: PathBuf,
        display_path: &Path,
        working_dir: &Path,
        path_translation: Option<&crate::services::authority::PathTranslation>,
    ) -> Self {
        // Compute URI from the absolute path. When the active authority
        // has a host↔remote mapping (devcontainer attach), this is
        // where the host path gets rewritten into the container path
        // the LSP server actually understands.
        let file_uri = LspUri::from_host_path(&canonical_path, path_translation);

        // Compute display name (project-relative when under working_dir, else absolute path).
        // Use canonicalized forms first to handle macOS /var -> /private/var differences.
        let display_name = Self::display_name_for_path(&canonical_path, working_dir);

        // Check if this is a library file (in vendor directories or standard libraries).
        // Library files are read-only (to prevent accidental edits) but LSP stays
        // enabled so that Goto Definition, Hover, Find References, etc. still work
        // when the user navigates into library source code (issue #1344).
        //
        // A file is only considered a library file if BOTH the canonical path and the
        // user-visible path are in a library directory. This prevents symlinked dotfiles
        // (e.g., ~/.bash_profile -> /nix/store/...) from being marked read-only when
        // the user explicitly opened a non-library path (issue #1469).
        let is_library = Self::is_library_path(&canonical_path, working_dir)
            && Self::is_library_path(display_path, working_dir);

        Self {
            kind: BufferKind::File {
                path: canonical_path,
                uri: file_uri,
            },
            display_name,
            lsp_enabled: true,
            lsp_disabled_reason: None,
            read_only: is_library,
            binary: false,
            auto_revert_enabled: true,
            lsp_opened_with: HashSet::new(),
            hidden_from_tabs: false,
            synthetic_placeholder: false,
            is_preview: false,
            recovery_id: None,
        }
    }

    /// Create metadata for a buffer fetched from inside a container.
    ///
    /// Used by `Editor::open_lsp_uri_target` when a Goto-Definition
    /// (or similar) URI lands on a path that exists only inside the
    /// container — typically a stdlib / site-packages entry that
    /// isn't bind-mounted onto the host. The buffer is read-only
    /// because there's no host-side writeback path; LSP stays enabled
    /// so further navigation from the fetched buffer (hover, more
    /// goto-defs) keeps working.
    ///
    /// The supplied `uri` is the wire URI the LSP returned (already
    /// in container-side coordinates) and is cached verbatim — no
    /// host→remote translation, because the path *is* the remote
    /// path. The display name is the file name, since the container
    /// path has nothing to relativize against the host working dir.
    pub fn with_container_file(container_path: PathBuf, uri: LspUri) -> Self {
        let display_name = container_path
            .file_name()
            .and_then(|n| n.to_str())
            .map(|n| n.to_string())
            .unwrap_or_else(|| container_path.to_string_lossy().to_string());
        Self {
            kind: BufferKind::File {
                path: container_path,
                uri: Some(uri),
            },
            display_name,
            lsp_enabled: true,
            lsp_disabled_reason: None,
            read_only: true,
            auto_revert_enabled: true,
            binary: false,
            lsp_opened_with: HashSet::new(),
            hidden_from_tabs: false,
            synthetic_placeholder: false,
            is_preview: false,
            recovery_id: None,
        }
    }

    /// Check if a path is a library file (in vendor directories or standard libraries)
    ///
    /// Library files include:
    /// - Files in common vendor/dependency directories (.cargo, node_modules, etc.)
    /// - Standard library / toolchain files (rustup toolchains, system includes, etc.)
    pub fn is_library_path(path: &Path, _working_dir: &Path) -> bool {
        let path_str = path.to_string_lossy();

        const CONTAINS_PATTERNS: &[&str] = &[
            "/.cargo/registry/",
            "\\.cargo\\registry\\",
            "/.cargo/git/",
            "\\.cargo\\git\\",
            "/rustup/toolchains/",
            "\\rustup\\toolchains\\",
            "/node_modules/",
            "\\node_modules\\",
            "/site-packages/",
            "\\site-packages\\",
            "/dist-packages/",
            "\\dist-packages\\",
            "/pkg/mod/",
            "\\pkg\\mod\\",
            "/gems/",
            "\\gems\\",
            "/.gradle/",
            "\\.gradle\\",
            "/.m2/",
            "\\.m2\\",
            "/.nuget/",
            "\\.nuget\\",
            "/Xcode.app/Contents/Developer/",
            "/CommandLineTools/SDKs/",
        ];
        const PREFIX_PATTERNS: &[&str] = &[
            "/usr/include/",
            "/usr/local/include/",
            "/nix/store/",
            "/opt/homebrew/Cellar/",
            "/usr/local/Cellar/",
        ];

        CONTAINS_PATTERNS.iter().any(|p| path_str.contains(*p))
            || PREFIX_PATTERNS.iter().any(|p| path_str.starts_with(*p))
    }

    /// Compute display name relative to working_dir when possible, otherwise absolute
    pub fn display_name_for_path(path: &Path, working_dir: &Path) -> String {
        // Canonicalize working_dir to normalize platform-specific prefixes
        let canonical_working_dir = working_dir
            .canonicalize()
            .unwrap_or_else(|_| working_dir.to_path_buf());

        // Try to canonicalize the file path; if it fails (e.g., new file), fall back to absolute
        let absolute_path = if path.is_absolute() {
            path.to_path_buf()
        } else {
            // If we were given a relative path, anchor it to working_dir
            canonical_working_dir.join(path)
        };
        let canonical_path = absolute_path
            .canonicalize()
            .unwrap_or_else(|_| absolute_path.clone());

        // Prefer canonical comparison first, then raw prefix as a fallback
        let relative = canonical_path
            .strip_prefix(&canonical_working_dir)
            .or_else(|_| path.strip_prefix(working_dir))
            .ok()
            .and_then(|rel| rel.to_str().map(|s| s.to_string()));

        relative
            .or_else(|| canonical_path.to_str().map(|s| s.to_string()))
            .unwrap_or_else(|| t!("buffer.unknown").to_string())
    }

    /// Create metadata for a virtual buffer (not backed by a file)
    ///
    /// # Arguments
    /// * `name` - Display name (e.g., "*Diagnostics*")
    /// * `mode` - Buffer mode for keybindings (e.g., "diagnostics-list")
    /// * `read_only` - Whether the buffer should be read-only
    pub fn virtual_buffer(name: String, mode: String, read_only: bool) -> Self {
        Self {
            kind: BufferKind::Virtual { mode },
            display_name: name,
            lsp_enabled: false, // Virtual buffers don't use LSP
            lsp_disabled_reason: Some(t!("lsp.disabled.virtual").to_string()),
            auto_revert_enabled: true,
            read_only,
            binary: false,
            lsp_opened_with: HashSet::new(),
            hidden_from_tabs: false,
            synthetic_placeholder: false,
            is_preview: false,
            recovery_id: None,
        }
    }

    /// Create metadata for a hidden virtual buffer (for composite source buffers)
    /// These buffers are not shown in tabs and are managed by their parent composite buffer.
    /// Hidden buffers are always read-only to prevent accidental edits.
    pub fn hidden_virtual_buffer(name: String, mode: String) -> Self {
        Self {
            kind: BufferKind::Virtual { mode },
            display_name: name,
            lsp_enabled: false,
            auto_revert_enabled: true,
            lsp_disabled_reason: Some(t!("lsp.disabled.virtual").to_string()),
            read_only: true, // Hidden buffers are always read-only
            binary: false,
            lsp_opened_with: HashSet::new(),
            hidden_from_tabs: true,
            synthetic_placeholder: false,
            is_preview: false,
            recovery_id: None,
        }
    }

    /// Disable LSP for this buffer with a reason
    pub fn disable_lsp(&mut self, reason: String) {
        self.lsp_enabled = false;
        self.lsp_disabled_reason = Some(reason);
    }
}

#[cfg(test)]
mod is_library_path_tests {
    use super::*;

    fn check(path: &str) -> bool {
        BufferMetadata::is_library_path(Path::new(path), Path::new("/working_dir"))
    }

    // Regression tests for issue #1970: .cargo/config.toml (and other user-editable
    // entries directly under .cargo/) must not be treated as library files.
    #[test]
    fn cargo_config_toml_is_not_a_library_file() {
        assert!(!check("/home/user/.cargo/config.toml"));
        assert!(!check("/home/user/project/.cargo/config.toml"));
    }

    #[test]
    fn cargo_credentials_and_env_are_not_library_files() {
        assert!(!check("/home/user/.cargo/credentials.toml"));
        assert!(!check("/home/user/.cargo/env"));
    }

    #[test]
    fn cargo_registry_sources_are_library_files() {
        assert!(check(
            "/home/user/.cargo/registry/src/index.crates.io-1cd66030c949c28d/serde-1.0.0/src/lib.rs"
        ));
    }

    #[test]
    fn cargo_git_checkouts_are_library_files() {
        assert!(check(
            "/home/user/.cargo/git/checkouts/some-dep-abcdef/abcdef/src/lib.rs"
        ));
    }

    #[test]
    fn cargo_config_toml_is_not_a_library_file_windows() {
        assert!(!check("C:\\Users\\user\\.cargo\\config.toml"));
    }

    #[test]
    fn cargo_registry_sources_are_library_files_windows() {
        assert!(check(
            "C:\\Users\\user\\.cargo\\registry\\src\\index.crates.io-1cd66030c949c28d\\serde-1.0.0\\src\\lib.rs"
        ));
    }
}
