//! Windows: open the spool delete-on-close, so it goes with the handle.
//!
//! There is no unlink-while-open here, but `FILE_FLAG_DELETE_ON_CLOSE` gives
//! the same guarantee from the other end: the file is removed when the last
//! handle closes, including when the process is terminated rather than
//! exiting. `FILE_SHARE_DELETE` is what lets that deletion be pending while
//! handles are still open, and `FILE_SHARE_READ | FILE_SHARE_WRITE` lets the
//! reader and the drain thread hold theirs at the same time.
//!
//! The name still exists on disk while the editor runs, unlike Unix, so it is
//! made unguessable rather than predictable — but nothing reads it by name:
//! chunk loads go through the handle, positionally, exactly as on Unix.

use std::fs::File;
use std::io;
use std::os::windows::fs::{FileExt, OpenOptionsExt};

use windows_sys::Win32::Storage::FileSystem::{
    FILE_FLAG_DELETE_ON_CLOSE, FILE_SHARE_DELETE, FILE_SHARE_READ, FILE_SHARE_WRITE,
};

/// The write side, handed to the thread draining the pipe.
pub type SpoolWriter = File;

/// The read side, used only for positional reads.
#[derive(Debug)]
pub struct SpoolHandle {
    file: File,
}

/// Create the spool and return `(read side, write side)`.
pub fn create() -> io::Result<(SpoolHandle, SpoolWriter)> {
    let dir = std::env::temp_dir();
    let mut attempt = 0u32;
    let file = loop {
        let candidate = dir.join(format!("fresh-stdin-{}-{attempt}.tmp", std::process::id()));
        match std::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create_new(true)
            .share_mode(FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE)
            .custom_flags(FILE_FLAG_DELETE_ON_CLOSE)
            .open(&candidate)
        {
            Ok(file) => break file,
            Err(e) if e.kind() == io::ErrorKind::AlreadyExists && attempt < 64 => {
                attempt += 1;
            }
            Err(e) => return Err(e),
        }
    };

    // The clone inherits the delete-on-close disposition, so the file lives
    // exactly as long as the last of the two handles.
    let writer = file.try_clone()?;
    Ok((SpoolHandle { file }, writer))
}

impl SpoolHandle {
    /// `seek_read` is Windows' positional read: it takes the offset as an
    /// argument and leaves the handle's own cursor alone, so overlapping
    /// chunk loads cannot race over it.
    pub fn read_at(&self, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        let mut buf = vec![0u8; len];
        let mut filled = 0usize;

        while filled < len {
            match self
                .file
                .seek_read(&mut buf[filled..], offset + filled as u64)
            {
                Ok(0) => break, // end of the drained region
                Ok(n) => filled += n,
                Err(e) if e.kind() == io::ErrorKind::Interrupted => {}
                Err(e) => return Err(e),
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
