//! E2E: `.h` headers highlight with the grammar that detection actually
//! resolved (issue #3009).
//!
//! `.h` is mapped to C by the default `[languages]` table, but the LSP-side
//! detection already promotes a header to `cpp` when the surrounding tree
//! smells like C++ (a sibling C++ source, an ancestor `compile_commands.json`).
//! That promotion used to rename the buffer's language id only: the grammar,
//! the status-bar label and every colour on screen still came from the C
//! extension table. These tests open the *same header bytes* in two trees and
//! assert on rendered output only.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::model::filesystem::{
    DirEntry, FileMetadata, FilePermissions, FileReader, FileSearchCursor, FileSearchOptions,
    FileSystem, FileWriter, SearchMatch, StdFileSystem,
};
use ratatui::style::Color;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Same bytes in both trees. `namespace` / `class` / `virtual` are C++
/// keywords and plain identifiers in C; `counter` is a plain identifier in
/// both, so it is the per-buffer reference for "unhighlighted".
const HEADER: &str = "\
namespace ui {
class Widget {
public:
    virtual void draw();
};
}
int counter;
";

fn create_harness() -> EditorTestHarness {
    EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_project_root()
            .with_full_grammar_registry(),
    )
    .unwrap()
}

/// Foreground colour of the first cell of `text` on screen.
fn fg_at(harness: &EditorTestHarness, text: &str) -> Color {
    let (col, row) = harness
        .find_text_on_screen(text)
        .unwrap_or_else(|| panic!("'{text}' not found on screen"));
    harness
        .get_cell_style(col, row)
        .and_then(|s| s.fg)
        .unwrap_or_else(|| panic!("no fg style at '{text}' ({col},{row})"))
}

/// A header next to a C++ source is C++: the status bar says so and the C++
/// keywords are painted with the keyword colour rather than plain foreground.
#[test]
fn test_h_header_beside_cpp_source_highlights_as_cpp() {
    let mut harness = create_harness();
    let dir = harness.project_dir().unwrap().join("cpp_tree");
    std::fs::create_dir_all(&dir).unwrap();
    let header = dir.join("widget.h");
    std::fs::write(&header, HEADER).unwrap();
    // The decisive C++ signal — a sibling translation unit.
    std::fs::write(dir.join("widget.cpp"), "#include \"widget.h\"\n").unwrap();

    harness.open_file(&header).unwrap();
    harness.render().unwrap();

    let status_bar = harness.get_status_bar();
    assert!(
        status_bar.contains("C++"),
        "status bar should report C++ for a header in a C++ tree. Got: {status_bar}"
    );

    let plain = fg_at(&harness, "counter");
    assert_ne!(
        fg_at(&harness, "namespace"),
        plain,
        "`namespace` must render as a keyword, not plain foreground"
    );
    assert_ne!(
        fg_at(&harness, "virtual"),
        plain,
        "`virtual` must render as a keyword, not plain foreground"
    );
}

/// The same bytes in a plain C project keep the C grammar: `namespace` and
/// `virtual` are ordinary identifiers there and must render unhighlighted.
#[test]
fn test_h_header_in_pure_c_project_stays_c() {
    let mut harness = create_harness();
    let dir = harness.project_dir().unwrap().join("c_tree");
    std::fs::create_dir_all(&dir).unwrap();
    let header = dir.join("widget.h");
    std::fs::write(&header, HEADER).unwrap();
    // Only C siblings — no C++ signal anywhere.
    std::fs::write(dir.join("widget.c"), "#include \"widget.h\"\n").unwrap();

    harness.open_file(&header).unwrap();
    harness.render().unwrap();

    let status_bar = harness.get_status_bar();
    assert!(
        !status_bar.contains("C++"),
        "status bar must not report C++ for a header in a pure C tree. Got: {status_bar}"
    );

    let plain = fg_at(&harness, "counter");
    assert_eq!(
        fg_at(&harness, "namespace"),
        plain,
        "`namespace` is an ordinary identifier in C and must not be highlighted"
    );
    assert_eq!(
        fg_at(&harness, "virtual"),
        plain,
        "`virtual` is an ordinary identifier in C and must not be highlighted"
    );
}

/// The directory name the fake host serves its C++ tree from. Only the
/// leaf name is a literal — the root it hangs off is discovered at runtime
/// (see [`fake_remote_root`]).
const REMOTE_PROJECT_DIR: &str = "remote-cpp-project";

/// An absolute path that exists on no machine: the filesystem root the test
/// process is already running on, plus a directory name nobody creates.
///
/// The root component is *discovered* rather than written down, because an
/// absolute path is not a portable literal. `"/remote-cpp-project"` is
/// absolute on Unix, but on Windows `Path::is_absolute` requires a prefix
/// (a drive letter or UNC share) in addition to the root, so the same
/// literal is a *relative* path there. `Window::open_file_no_focus_inner`
/// re-anchors relative paths to the session's base directory, so on Windows
/// the header opened as `C:\…\remote-cpp-project\widget.h`, `RemoteTreeFs`
/// no longer recognised the prefix, the tree was never served, and the
/// buffer came up as a brand-new empty file instead of a C++ header.
///
/// Taking the leading `Prefix`/`RootDir` components of a path the platform
/// itself produced yields `/` on Unix and `C:\` on Windows, so the result is
/// absolute under either convention by construction rather than by guess.
fn fake_remote_root() -> PathBuf {
    let anchor = std::env::temp_dir();
    let mut root = PathBuf::new();
    for component in anchor.components() {
        match component {
            std::path::Component::Prefix(_) | std::path::Component::RootDir => {
                root.push(component.as_os_str())
            }
            _ => break,
        }
    }
    let root = root.join(REMOTE_PROJECT_DIR);
    assert!(
        root.is_absolute(),
        "the fake remote root must be absolute on this platform, else the \
         editor re-anchors it to the working directory and the fake tree is \
         never consulted. Got: {}",
        root.display()
    );
    root
}

/// A filesystem that behaves like an SSH host: the tree it serves lives under
/// [`fake_remote_root`], a path that does **not** exist on the machine
/// running the test, and every operation is translated to a real temporary
/// directory before being delegated to `StdFileSystem`.
///
/// The translation is the whole point. It reproduces the one thing a plain
/// tempdir fixture cannot: a file whose siblings are invisible to `std::fs`.
/// The `.h` → C++ tree probe used to call `std::fs::read_dir` directly, so on
/// a real SSH session it listed the *local* machine, found nothing, and
/// silently left every header in a remote C++ tree highlighted as C. Against
/// this filesystem the pre-fix probe behaves exactly as it did over SSH.
///
/// `remote_connection_info` also reports a connection string, which is what
/// marks the backend as one whose sync calls are blocking round trips.
struct RemoteTreeFs {
    inner: StdFileSystem,
    /// Path prefix that exists only on the "remote host".
    remote_root: PathBuf,
    /// The real directory backing the fake remote root.
    local_root: PathBuf,
}

impl RemoteTreeFs {
    fn new(local_root: PathBuf) -> Self {
        Self {
            inner: StdFileSystem,
            remote_root: fake_remote_root(),
            local_root,
        }
    }

    /// Translate a remote path to the real directory serving it. Paths
    /// outside the fake root pass through unchanged, so the harness's own
    /// config and scratch files keep working.
    fn map(&self, path: &Path) -> PathBuf {
        match path.strip_prefix(&self.remote_root) {
            Ok(rest) => self.local_root.join(rest),
            Err(_) => path.to_path_buf(),
        }
    }

    /// A path under the fake remote root for `name`.
    fn remote_path(&self, name: &str) -> PathBuf {
        self.remote_root.join(name)
    }
}

impl FileSystem for RemoteTreeFs {
    fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        // Entries are reported back under the remote path, the way a real
        // agent reports the host's own paths.
        let entries = self.inner.read_dir(&self.map(path))?;
        Ok(entries
            .into_iter()
            .map(|e| DirEntry::new(path.join(&e.name), e.name, e.entry_type))
            .collect())
    }

    // `canonicalize` must stay in remote space — resolving to the local
    // backing directory would hand the editor a path the "host" never had.
    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        self.inner.canonicalize(&self.map(path))?;
        Ok(path.to_path_buf())
    }

    fn remote_connection_info(&self) -> Option<&str> {
        Some("user@build-host")
    }

    // ---- delegation, with path translation ----
    fn read_file(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.inner.read_file(&self.map(path))
    }
    fn read_range(&self, path: &Path, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        self.inner.read_range(&self.map(path), offset, len)
    }
    fn write_file(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        self.inner.write_file(&self.map(path), data)
    }
    fn create_file(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.create_file(&self.map(path))
    }
    fn open_file(&self, path: &Path) -> io::Result<Box<dyn FileReader>> {
        self.inner.open_file(&self.map(path))
    }
    fn open_file_for_write(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.open_file_for_write(&self.map(path))
    }
    fn open_file_for_append(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.open_file_for_append(&self.map(path))
    }
    fn set_file_length(&self, path: &Path, len: u64) -> io::Result<()> {
        self.inner.set_file_length(&self.map(path), len)
    }
    fn rename(&self, from: &Path, to: &Path) -> io::Result<()> {
        self.inner.rename(&self.map(from), &self.map(to))
    }
    fn copy(&self, from: &Path, to: &Path) -> io::Result<u64> {
        self.inner.copy(&self.map(from), &self.map(to))
    }
    fn remove_file(&self, path: &Path) -> io::Result<()> {
        self.inner.remove_file(&self.map(path))
    }
    fn remove_dir(&self, path: &Path) -> io::Result<()> {
        self.inner.remove_dir(&self.map(path))
    }
    fn metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.inner.metadata(&self.map(path))
    }
    fn symlink_metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.inner.symlink_metadata(&self.map(path))
    }
    fn is_dir(&self, path: &Path) -> io::Result<bool> {
        self.inner.is_dir(&self.map(path))
    }
    fn is_file(&self, path: &Path) -> io::Result<bool> {
        self.inner.is_file(&self.map(path))
    }
    fn set_permissions(&self, path: &Path, permissions: &FilePermissions) -> io::Result<()> {
        self.inner.set_permissions(&self.map(path), permissions)
    }
    fn create_dir(&self, path: &Path) -> io::Result<()> {
        self.inner.create_dir(&self.map(path))
    }
    fn create_dir_all(&self, path: &Path) -> io::Result<()> {
        self.inner.create_dir_all(&self.map(path))
    }
    fn current_uid(&self) -> u32 {
        self.inner.current_uid()
    }
    fn search_file(
        &self,
        path: &Path,
        pattern: &str,
        opts: &FileSearchOptions,
        cursor: &mut FileSearchCursor,
    ) -> io::Result<Vec<SearchMatch>> {
        self.inner
            .search_file(&self.map(path), pattern, opts, cursor)
    }
    fn sudo_write(
        &self,
        path: &Path,
        data: &[u8],
        mode: u32,
        uid: u32,
        gid: u32,
    ) -> io::Result<()> {
        self.inner.sudo_write(&self.map(path), data, mode, uid, gid)
    }
    fn walk_files(
        &self,
        root: &Path,
        skip_dirs: &[&str],
        cancel: &std::sync::atomic::AtomicBool,
        on_file: &mut dyn FnMut(&Path, &str) -> bool,
    ) -> io::Result<()> {
        self.inner
            .walk_files(&self.map(root), skip_dirs, cancel, on_file)
    }
}

/// The remote half of #3009: opening `widget.h` from an SSH host whose tree
/// contains `widget.cpp` must highlight as C++ on screen, exactly as the
/// local case does.
///
/// Before the `FileSystem` trait was threaded through language detection this
/// rendered as C — the promotion probe read the local disk, which has no
/// `<root>/remote-cpp-project` at all, so it found no C++ sibling and quietly
/// declined to promote.
#[test]
fn test_remote_h_header_beside_cpp_source_highlights_as_cpp() {
    let backing = tempfile::tempdir().unwrap();
    std::fs::write(backing.path().join("widget.h"), HEADER).unwrap();
    // The decisive C++ signal — visible only through the remote filesystem.
    std::fs::write(backing.path().join("widget.cpp"), "#include \"widget.h\"\n").unwrap();

    let fs = Arc::new(RemoteTreeFs::new(backing.path().to_path_buf()));
    let header = fs.remote_path("widget.h");
    // The header exists only on the fake host: if `std::fs` can see it, the
    // test would no longer be proving that detection went through the
    // injected filesystem.
    assert!(
        !header.exists(),
        "{} must not exist locally",
        header.display()
    );

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_full_grammar_registry()
            .with_filesystem(fs),
    )
    .unwrap();

    harness.open_file(&header).unwrap();
    harness.render().unwrap();

    let status_bar = harness.get_status_bar();
    assert!(
        status_bar.contains("C++"),
        "status bar should report C++ for a header in a *remote* C++ tree. Got: {status_bar}"
    );

    let plain = fg_at(&harness, "counter");
    assert_ne!(
        fg_at(&harness, "namespace"),
        plain,
        "`namespace` must render as a keyword in a remote C++ tree"
    );
    assert_ne!(
        fg_at(&harness, "virtual"),
        plain,
        "`virtual` must render as a keyword in a remote C++ tree"
    );
}
