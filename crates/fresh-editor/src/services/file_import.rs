//! Local-file imports. Source and destination filesystems are separate:
//! an SSH workspace's `copy` would look for the source on the remote host.

use crate::model::filesystem::FileSystem;
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};

/// Parse the quoted/escaped paths terminals send for file drops. This is only
/// used by the focused file explorer and the explicit import prompt.
/// No shell is run and no variables, globs, or command substitutions expand.
pub fn parse_paths(input: &str) -> io::Result<Vec<PathBuf>> {
    let paths = parse_paths_for_platform(input, cfg!(windows))?;
    // Resolve source paths on this machine, never against a remote cwd.
    if paths.iter().any(|path| !path.is_absolute()) {
        return Err(invalid("use absolute local file paths"));
    }
    Ok(paths)
}

fn parse_paths_for_platform(input: &str, windows: bool) -> io::Result<Vec<PathBuf>> {
    let mut paths = Vec::new();
    let mut word = String::new();
    let mut quote = None;
    let mut chars = input.chars();
    while let Some(ch) = chars.next() {
        match (quote, ch) {
            (Some(q), c) if c == q => quote = None,
            (None, '\'' | '"') => quote = Some(ch),
            (None, c) if c.is_whitespace() => {
                if !word.is_empty() {
                    paths.push(PathBuf::from(std::mem::take(&mut word)));
                }
            }
            (q, '\\') if !windows && q != Some('\'') => {
                let next = chars
                    .next()
                    .ok_or_else(|| invalid("incomplete path escape"))?;
                // In double quotes, backslash only escapes shell metacharacters.
                if q == Some('"') && !matches!(next, '"' | '\\' | '$' | '`' | '\n') {
                    word.push('\\');
                }
                word.push(next);
            }
            (_, c) => word.push(c),
        }
    }
    if quote.is_some() {
        return Err(invalid("unclosed path quote"));
    }
    if !word.is_empty() {
        paths.push(PathBuf::from(word));
    }
    if paths.is_empty() {
        return Err(invalid("no file paths supplied"));
    }
    Ok(paths)
}

fn invalid(message: &str) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidInput, message)
}

fn check_cancel(cancel: &AtomicBool) -> io::Result<()> {
    if cancel.load(Ordering::Acquire) {
        Err(io::Error::new(
            io::ErrorKind::Interrupted,
            "file import cancelled",
        ))
    } else {
        Ok(())
    }
}

/// Copy bytes in bounded chunks, staging beside the destination so publication
/// is atomic. Cancellation is checked between I/O requests; the current remote
/// request must finish or fail before cleanup can run. Earlier completed files
/// in a batch are not rolled back.
pub fn import_file(
    source_fs: &dyn FileSystem,
    destination_fs: &dyn FileSystem,
    source: &Path,
    destination: &Path,
    overwrite: bool,
    cancel: &AtomicBool,
    mut progress: impl FnMut(u64, u64),
) -> io::Result<()> {
    check_cancel(cancel)?;
    if !source.is_absolute() || !source_fs.is_file(source)? {
        return Err(invalid("source must be an absolute path to a regular file"));
    }
    if overwrite
        && destination_fs.remote_connection_info().is_none()
        && destination_fs.canonicalize(destination).ok().as_ref()
            == Some(&source_fs.canonicalize(source)?)
    {
        return Err(invalid("source and destination are the same file"));
    }
    if !overwrite {
        match destination_fs.symlink_metadata(destination) {
            Ok(_) => {
                return Err(io::Error::new(
                    io::ErrorKind::AlreadyExists,
                    "destination already exists",
                ))
            }
            Err(error) if error.kind() == io::ErrorKind::NotFound => {}
            Err(error) => return Err(error),
        }
    }
    let parent = destination
        .parent()
        .ok_or_else(|| invalid("missing destination directory"))?;
    let total = source_fs.metadata(source)?.size;
    let mut reader = source_fs.open_file(source)?;
    // An exclusive directory reserves our staging name. Never truncate a
    // pre-existing temp file, and never put uploads in the local /tmp directory.
    let staging_dir = parent.join(format!(".fresh-import-{}", uuid::Uuid::new_v4()));
    destination_fs.create_dir(&staging_dir)?;
    let staging_file = staging_dir.join("data");
    let result = (|| {
        destination_fs.write_file(&staging_file, &[])?;
        let mut buffer = vec![0; 256 * 1024];
        let mut copied = 0;
        progress(0, total);
        loop {
            check_cancel(cancel)?;
            let count = reader.read(&mut buffer)?;
            if count == 0 {
                break;
            }
            // Remote append writers retain their buffer until sync_all, so
            // use one per chunk. Reusing one would resend previous chunks.
            let mut writer = destination_fs.open_file_for_append(&staging_file)?;
            writer.write_all(&buffer[..count])?;
            writer.flush()?;
            writer.sync_all()?;
            copied += count as u64;
            progress(copied, total);
        }
        check_cancel(cancel)?;
        destination_fs.publish_file(&staging_file, destination, overwrite)
    })();
    // Publication may move the staging file or leave a hard link. Surface
    // cleanup failures as well as transfer errors (e.g. a disconnected host).
    let cleanup = match destination_fs.remove_file(&staging_file) {
        Ok(()) => Ok(()),
        Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(()),
        Err(e) => Err(e),
    }
    .and_then(|()| destination_fs.remove_dir(&staging_dir));
    match (result, cleanup) {
        (result, Ok(())) => result,
        (Ok(()), Err(e)) => Err(io::Error::other(format!(
            "file imported, but staging cleanup failed at {}: {e}",
            staging_dir.display()
        ))),
        (Err(e), Err(cleanup)) => Err(io::Error::other(format!(
            "{e}; staging cleanup failed at {}: {cleanup}",
            staging_dir.display()
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_terminal_paths_without_shell_expansion() {
        let parsed = parse_paths_for_platform(
            "'/tmp/a b.png' /tmp/한글\\ 파일.txt \"/tmp/c d\" /tmp/\\$HOME",
            false,
        )
        .unwrap();
        assert_eq!(
            parsed,
            [
                "/tmp/a b.png",
                "/tmp/한글 파일.txt",
                "/tmp/c d",
                "/tmp/$HOME"
            ]
            .map(PathBuf::from)
        );
        assert_eq!(
            parse_paths_for_platform("'/tmp/it'\\''s.png'\r\n/tmp/b", false).unwrap(),
            ["/tmp/it's.png", "/tmp/b"].map(PathBuf::from)
        );
        assert!(parse_paths_for_platform("'/tmp/unfinished", false).is_err());
        assert!(parse_paths_for_platform("/tmp/unfinished\\", false).is_err());
        assert!(parse_paths("ordinary text").is_err());
        assert!(parse_paths_for_platform("", false).is_err());
        assert_eq!(
            parse_paths_for_platform(" /tmp/trailing\\  ", false).unwrap(),
            [PathBuf::from("/tmp/trailing ")]
        );
    }

    #[test]
    fn preserves_windows_path_separators() {
        assert_eq!(
            parse_paths_for_platform(
                r#""C:\Users\Me\a b.png" D:\한글.txt "\\server\share\c d""#,
                true
            )
            .unwrap(),
            [
                r"C:\Users\Me\a b.png",
                r"D:\한글.txt",
                r"\\server\share\c d"
            ]
            .map(PathBuf::from)
        );
    }
}
