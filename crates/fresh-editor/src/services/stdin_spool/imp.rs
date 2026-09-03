//! Unix: create the spool, unlink it immediately, keep the descriptor.
//!
//! The unlink happens before the file is ever handed out, so there is no
//! window in which a name exists for anything to find or leave behind. What
//! survives is the descriptor, and the kernel frees the inode when the last
//! one closes — a `SIGKILL` or a power cut included.

use std::fs::File;
use std::io;
use std::os::unix::io::AsRawFd;

/// The write side, handed to the thread draining the pipe.
pub type SpoolWriter = File;

/// The read side, used only for positional reads.
#[derive(Debug)]
pub struct SpoolHandle {
    file: File,
}

/// Create the spool and return `(read side, write side)`.
///
/// Two descriptors rather than one shared handle: the drain thread appends
/// through its own cursor while chunk loads read positionally, so the two
/// never contend.
pub fn create() -> io::Result<(SpoolHandle, SpoolWriter)> {
    // A name is needed only for the instant between `open` and `unlink`.
    // `O_EXCL` keeps that instant from being hijacked, and the pid plus a
    // monotonic counter keeps concurrent editors from colliding.
    let dir = std::env::temp_dir();
    let mut attempt = 0u32;
    let file = loop {
        let candidate = dir.join(format!("fresh-stdin-{}-{attempt}.tmp", std::process::id()));
        match std::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create_new(true)
            .open(&candidate)
        {
            Ok(file) => {
                // The whole point: no name from here on.
                std::fs::remove_file(&candidate)?;
                break file;
            }
            Err(e) if e.kind() == io::ErrorKind::AlreadyExists && attempt < 64 => {
                attempt += 1;
            }
            Err(e) => return Err(e),
        }
    };

    let writer = file.try_clone()?;
    Ok((SpoolHandle { file }, writer))
}

impl SpoolHandle {
    /// `pread`: takes the offset as an argument, so it neither uses nor
    /// disturbs any cursor and overlapping chunk loads cannot race.
    pub fn read_at(&self, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        let mut buf = vec![0u8; len];
        let mut filled = 0usize;

        while filled < len {
            // SAFETY: `self.file` is open for reading, and the slice being
            // written is `buf[filled..]`, which is in bounds by the loop
            // condition.
            let n = unsafe {
                libc::pread(
                    self.file.as_raw_fd(),
                    buf[filled..].as_mut_ptr().cast(),
                    len - filled,
                    (offset + filled as u64) as libc::off_t,
                )
            };
            match n {
                -1 => {
                    let e = io::Error::last_os_error();
                    if e.kind() == io::ErrorKind::Interrupted {
                        continue;
                    }
                    return Err(e);
                }
                // End of the drained region: report what we got and let the
                // caller decide whether that was enough.
                0 => break,
                n => filled += n as usize,
            }
        }

        buf.truncate(filled);
        Ok(buf)
    }

    /// How many bytes have been drained so far.
    pub fn len(&self) -> io::Result<u64> {
        Ok(self.file.metadata()?.len())
    }
}
