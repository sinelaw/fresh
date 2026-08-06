//! Where downloaded artifacts are put before they are installed.
//!
//! Everything staged here is eventually handed to another program *by path*,
//! and for `.deb`/`.rpm` that program runs under `sudo`. So anything able to
//! write to the path between our checksum check and the package tool's read
//! gets its bytes installed as root. Staging under a predictable name in a
//! world-writable `/tmp` made that a one-liner: pre-create the file, or point a
//! symlink at something else, wait out the sudo prompt, swap the contents.
//!
//! Two directories, because the two lifetimes are different:
//!
//! * [`ephemeral`] — created for one run and removed at the end of it. Private
//!   to the user, unpredictable name, created atomically so there is no window
//!   where it exists and is writable by anyone else.
//!
//! * [`durable`] — for the artifact we *print a command for* and expect the
//!   user to install later, by hand. That one must outlive the process, and it
//!   must not be in a reapable temp directory: the path is printed to the
//!   terminal, so it is not a secret, and once `systemd-tmpfiles` ages the
//!   directory out any local user can recreate it with their own bytes. The
//!   command sitting in the user's scrollback then installs *those* as root.
//!   `$XDG_CACHE_HOME/fresh` is not swept on a timer and is owned by the user.

use std::path::{Path, PathBuf};

/// A staging directory that is removed when this value is dropped.
pub struct Ephemeral {
    path: PathBuf,
}

impl Ephemeral {
    /// The directory itself.
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Keep the directory instead of removing it on drop.
    pub fn keep(mut self) -> PathBuf {
        std::mem::take(&mut self.path)
    }
}

impl Drop for Ephemeral {
    fn drop(&mut self) {
        if !self.path.as_os_str().is_empty() {
            let _ = std::fs::remove_dir_all(&self.path);
        }
    }
}

/// Create a private, single-use staging directory under the system temp dir.
///
/// `DirBuilder::create` fails rather than reusing an existing path, and on Unix
/// the 0700 mode is applied at creation, so there is no moment where the
/// directory exists and is writable by anyone else. If an attacker pre-creates
/// every name we try we fail closed — a denial of service, not a compromise.
pub fn ephemeral() -> Result<Ephemeral, String> {
    let base = std::env::temp_dir();
    create_private_under(&base).map(|path| Ephemeral { path })
}

/// Create a private staging directory that survives the process, for an
/// artifact the user will install themselves.
///
/// Unlike [`ephemeral`] this is under the user's cache directory, so nothing
/// sweeps it out from under the command we printed.
pub fn durable() -> Result<PathBuf, String> {
    durable_under(&cache_root()?)
}

/// [`durable`], with the parent directory given explicitly. Split out so it can
/// be tested without mutating the environment, which the test harness shares
/// across threads.
fn durable_under(base: &Path) -> Result<PathBuf, String> {
    std::fs::create_dir_all(base).map_err(|e| format!("create {}: {e}", base.display()))?;
    restrict(base);
    create_private_under(base)
}

/// `$XDG_CACHE_HOME/fresh/updates`, or the platform equivalent.
fn cache_root() -> Result<PathBuf, String> {
    let base = if let Some(xdg) = env_path("XDG_CACHE_HOME") {
        xdg
    } else if let Some(home) = env_path("HOME") {
        home.join(".cache")
    } else if let Some(local) = env_path("LOCALAPPDATA") {
        local
    } else {
        return Err("no cache directory (set $XDG_CACHE_HOME or $HOME)".to_string());
    };
    Ok(base.join("fresh").join("updates"))
}

fn env_path(key: &str) -> Option<PathBuf> {
    std::env::var_os(key)
        .filter(|v| !v.is_empty())
        .map(PathBuf::from)
}

/// Make a directory we may not have created ourselves private, best effort.
fn restrict(dir: &Path) {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let _ = std::fs::set_permissions(dir, std::fs::Permissions::from_mode(0o700));
    }
    #[cfg(not(unix))]
    let _ = dir;
}

fn create_private_under(base: &Path) -> Result<PathBuf, String> {
    let pid = std::process::id();
    for attempt in 0..16u32 {
        let nonce = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.subsec_nanos())
            .unwrap_or(0)
            ^ attempt.wrapping_mul(0x9E37_79B9);
        let dir = base.join(format!("fresh-update-{pid}-{nonce:08x}"));
        match create_private_dir(&dir) {
            Ok(()) => return Ok(dir),
            Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(e) => return Err(format!("create staging dir {}: {e}", dir.display())),
        }
    }
    Err("could not create a private staging directory".to_string())
}

#[cfg(unix)]
fn create_private_dir(dir: &Path) -> std::io::Result<()> {
    use std::os::unix::fs::DirBuilderExt;
    std::fs::DirBuilder::new().mode(0o700).create(dir)
}

#[cfg(not(unix))]
fn create_private_dir(dir: &Path) -> std::io::Result<()> {
    // The per-user temp directory on Windows is not world-writable, and
    // `create` still refuses to reuse an existing path.
    std::fs::DirBuilder::new().create(dir)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_ephemeral_directory_is_private_and_goes_away() {
        let path;
        {
            let dir = ephemeral().unwrap();
            path = dir.path().to_path_buf();
            assert!(path.is_dir());
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                let mode = std::fs::metadata(&path).unwrap().permissions().mode();
                assert_eq!(mode & 0o777, 0o700, "staging dir is not private: {mode:o}");
            }
        }
        assert!(!path.exists(), "staging directory outlived its scope");
    }

    #[test]
    fn keeping_an_ephemeral_directory_suppresses_the_cleanup() {
        let dir = ephemeral().unwrap();
        let path = dir.keep();
        assert!(path.is_dir());
        let _ = std::fs::remove_dir_all(&path);
    }

    #[test]
    fn two_staging_directories_never_share_a_name() {
        let a = ephemeral().unwrap();
        let b = ephemeral().unwrap();
        assert_ne!(a.path(), b.path());
    }

    /// The printed-command artifact must not live anywhere a temp reaper will
    /// sweep it: the path is printed to the terminal, so recreating it is
    /// exactly as hard as reading the scrollback. Once the directory is gone,
    /// any local user can recreate that path with their own bytes, and the
    /// `sudo` command in the user's scrollback installs those instead.
    #[test]
    fn durable_staging_is_not_under_the_temp_directory() {
        let cache = cache_root().expect("a cache root must be resolvable");
        assert!(
            !cache.starts_with(std::env::temp_dir()),
            "durable staging resolved into the temp directory: {cache:?}"
        );
    }

    #[test]
    fn durable_staging_is_private_and_survives_the_call() {
        let base = tempfile::tempdir().unwrap();
        let dir = durable_under(base.path()).unwrap();
        assert!(dir.starts_with(base.path()), "{dir:?}");
        assert!(dir.is_dir(), "durable staging must outlive the call");
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mode = std::fs::metadata(&dir).unwrap().permissions().mode();
            assert_eq!(mode & 0o777, 0o700, "not private: {mode:o}");
        }
    }
}
