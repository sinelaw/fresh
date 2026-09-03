//! The platform property the nameless spool rests on (#3134).
//!
//! `… | fresh -` drains stdin into a scratch file that is unlinked the
//! instant it is created (`services::stdin_spool`), so the kernel reclaims it
//! when the last descriptor closes — however the process dies. The buffer
//! still has to read chunks back off it for as long as it lives, which works
//! only because an unlinked file remains fully usable through the descriptor
//! that was kept.
//!
//! That is platform behaviour rather than ours, so it is pinned here rather
//! than assumed: reads at arbitrary offsets, out of order, and appends that
//! are visible through the same handle afterwards. `pread` rather than
//! seek+read on purpose — it takes the offset as an argument instead of
//! moving a shared cursor, so overlapping chunk loads cannot corrupt each
//! other's position.
//!
//! This is also what let the design skip the per-platform magic paths. A
//! `/proc/self/fd/N` route would need procfs mounted and does not exist on
//! macOS or Windows, and macOS's `man 4 fd` documents `/dev/fd/N` as
//! equivalent to `dup(N)` — a *shared* offset, which is exactly the race
//! positional reads avoid. Going through the retained handle needs none of
//! that to be true anywhere.
#![cfg(unix)]

use std::os::unix::io::AsRawFd;

/// Create a spool-shaped file, write `contents`, unlink the name, and return
/// the still-open handle — the shape `services::stdin_spool` uses.
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

/// An unlinked file is still fully readable at arbitrary offsets through the
/// descriptor we kept, which is what a lazily-loaded chunk needs.
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
