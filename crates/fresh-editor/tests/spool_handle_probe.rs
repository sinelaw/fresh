//! What an unlinked-but-open spool file can still do (#3134 follow-up).
//!
//! `… | fresh -` spools stdin to a temp file and the buffer keeps reading
//! chunks back off it for as long as it lives, so today the file has to stay
//! *named* and the process has to delete it on the way out. That is what the
//! exit guard, the signal sweeps and the panic hook are all for, and none of
//! them can cover a `SIGKILL`, a segfault or a power loss.
//!
//! Unlinking at creation and holding the handle open removes the whole
//! problem: the kernel reclaims the inode when the last descriptor closes,
//! however the process dies. This file pins the two properties that decision
//! rests on, because they are platform behaviour rather than ours:
//!
//!   1. **Positional reads through the retained handle keep working after the
//!      name is gone.** This is the load-bearing one — `read_range` reads a
//!      chunk at an arbitrary offset, and `pread` has no shared file offset,
//!      so it is safe under concurrent chunk loads on every Unix.
//!   2. **What the per-platform magic path does.** Linux `/proc/self/fd/N`
//!      gives a genuinely fresh `open`; macOS documents `/dev/fd/N` as
//!      equivalent to `dup(N)`, which shares the offset and would make the
//!      current open→seek→read shape racy. The second test records which one
//!      this platform is, so the choice is made on evidence rather than on a
//!      man page.
#![cfg(unix)]

use std::os::unix::io::AsRawFd;

/// Create a spool-shaped file, write `contents`, unlink the name, and return
/// the still-open handle — the shape the fix would use.
fn unlinked_spool(dir: &std::path::Path, contents: &[u8]) -> std::fs::File {
    let path = dir.join("fresh-stdin-probe.tmp");
    let file = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(true)
        .open(&path)
        .expect("create spool file");

    use std::io::Write;
    (&file).write_all(contents).expect("write spool contents");

    std::fs::remove_file(&path).expect("unlink the spool file");
    assert!(!path.exists(), "the name should be gone immediately");

    file
}

/// The property the redesign rests on: an unlinked file is still fully
/// readable at arbitrary offsets through the descriptor we kept, which is
/// what a lazily-loaded chunk needs.
///
/// `pread` rather than seek+read on purpose: it takes the offset as an
/// argument instead of moving a shared cursor, so overlapping chunk loads
/// cannot corrupt each other's position regardless of how the platform
/// implements its `/dev/fd`-style paths.
#[test]
fn an_unlinked_spool_file_still_serves_positional_reads() {
    let dir = tempfile::tempdir().unwrap();
    let mut contents = b"chunk-A".to_vec();
    contents.extend(std::iter::repeat_n(b'x', 50));
    contents.extend_from_slice(b"chunk-B");
    let file = unlinked_spool(dir.path(), &contents);

    // Size is still readable, which is how the streaming poll notices growth.
    assert_eq!(
        file.metadata().unwrap().len() as usize,
        contents.len(),
        "an unlinked file still reports its size through the handle"
    );

    let read_at = |offset: i64, len: usize| -> Vec<u8> {
        let mut buf = vec![0u8; len];
        // SAFETY: `file` is open for reading and `buf` has `len` bytes.
        let n = unsafe {
            libc::pread(
                file.as_raw_fd(),
                buf.as_mut_ptr().cast(),
                len,
                offset as libc::off_t,
            )
        };
        assert!(n >= 0, "pread failed: {}", std::io::Error::last_os_error());
        buf.truncate(n as usize);
        buf
    };

    // Out of order, to show the reads are genuinely positional.
    assert_eq!(read_at(57, 7), b"chunk-B");
    assert_eq!(read_at(0, 7), b"chunk-A");
    assert_eq!(read_at(57, 7), b"chunk-B");

    // And growth after the unlink is visible too — stdin is still streaming
    // while the buffer is already open.
    use std::io::Write;
    (&file).write_all(b"-more").unwrap();
    assert_eq!(
        file.metadata().unwrap().len() as usize,
        contents.len() + 5,
        "appends after the unlink are visible through the same handle"
    );
    assert_eq!(read_at(contents.len() as i64, 5), b"-more");
}

/// Records what this platform's `/dev/fd`-style path does for an unlinked
/// file, which decides whether the path-based `read_range` could point at it
/// directly instead of going through the handle.
///
/// Linux is asserted, because `/proc/self/fd/N` is a genuinely fresh `open`
/// and we rely on that. macOS is only *reported*: `man 4 fd` says opening
/// `/dev/fd/N` is equivalent to `dup(N)`, and a shared offset would make the
/// existing open→seek→read shape racy under concurrent chunk loads. The
/// printed line is the empirical answer, read off the macOS CI runner.
#[test]
fn magic_fd_path_behaviour_for_an_unlinked_file() {
    let dir = tempfile::tempdir().unwrap();
    let file = unlinked_spool(dir.path(), b"0123456789");

    #[cfg(target_os = "linux")]
    let magic = format!("/proc/self/fd/{}", file.as_raw_fd());
    #[cfg(not(target_os = "linux"))]
    let magic = format!("/dev/fd/{}", file.as_raw_fd());

    use std::io::{Read, Seek, SeekFrom};

    let opened = std::fs::File::open(&magic);
    let readable = match opened {
        Ok(mut a) => {
            let mut buf = [0u8; 4];
            a.seek(SeekFrom::Start(6)).ok();
            a.read_exact(&mut buf).map(|()| buf).ok()
        }
        Err(_) => None,
    };

    // Two independent opens: if the platform gives back a `dup`, they share
    // one cursor and the second read continues where the first stopped.
    let independent_offsets = match (std::fs::File::open(&magic), std::fs::File::open(&magic)) {
        (Ok(mut a), Ok(mut b)) => {
            let (mut x, mut y) = ([0u8; 3], [0u8; 3]);
            a.read_exact(&mut x).is_ok() && b.read_exact(&mut y).is_ok() && x == y
        }
        _ => false,
    };

    eprintln!(
        "magic-path probe: platform={} path={magic} readable_at_offset={readable:?} \
         independent_offsets={independent_offsets}",
        std::env::consts::OS,
    );

    #[cfg(target_os = "linux")]
    {
        assert_eq!(
            readable,
            Some(*b"6789"),
            "/proc/self/fd should give a fresh open of the unlinked inode"
        );
        assert!(
            independent_offsets,
            "/proc/self/fd opens should each have their own file offset"
        );
    }
}
