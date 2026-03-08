//! Remote filesystem implementation
//!
//! Implements the FileSystem trait for remote operations via SSH agent.

use crate::model::filesystem::{
    DirEntry, EntryType, FileMetadata, FilePermissions, FileReader, FileSystem, FileWriter, WriteOp,
};
use crate::services::remote::channel::{AgentChannel, ChannelError};
use crate::services::remote::protocol::{
    append_params, count_lf_params, decode_base64, ls_params, patch_params, read_params,
    stat_params, sudo_write_params, truncate_params, write_params, PatchOp, RemoteDirEntry,
    RemoteMetadata,
};
use std::io::{self, Cursor, Read, Seek, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, UNIX_EPOCH};

/// Remote filesystem that communicates with the Python agent
pub struct RemoteFileSystem {
    channel: Arc<AgentChannel>,
    /// Display string for the connection
    connection_string: String,
}

impl RemoteFileSystem {
    /// Create a new remote filesystem from an agent channel
    pub fn new(channel: Arc<AgentChannel>, connection_string: String) -> Self {
        Self {
            channel,
            connection_string,
        }
    }

    /// Get the connection string for display
    pub fn connection_string(&self) -> &str {
        &self.connection_string
    }

    /// Check if connected
    pub fn is_connected(&self) -> bool {
        self.channel.is_connected()
    }

    /// Extract the remote temp directory from an agent `info` response.
    /// Falls back to `/tmp` if the response is missing or doesn't contain `temp_dir`.
    fn parse_temp_dir_from_info(info: Option<&serde_json::Value>) -> PathBuf {
        info.and_then(|r| {
            r.get("temp_dir")
                .and_then(|v| v.as_str())
                .map(PathBuf::from)
        })
        .unwrap_or_else(|| PathBuf::from("/tmp"))
    }

    /// Convert a ChannelError to io::Error
    fn to_io_error(e: ChannelError) -> io::Error {
        match e {
            ChannelError::Io(e) => e,
            ChannelError::Remote(msg) => {
                let kind = if msg.contains("not found") || msg.contains("No such file") {
                    io::ErrorKind::NotFound
                } else if msg.contains("permission denied") {
                    io::ErrorKind::PermissionDenied
                } else if msg.contains("is a directory") {
                    io::ErrorKind::IsADirectory
                } else if msg.contains("not a directory") {
                    io::ErrorKind::NotADirectory
                } else {
                    io::ErrorKind::Other
                };
                io::Error::new(kind, msg)
            }
            e => io::Error::other(e.to_string()),
        }
    }

    /// Convert remote metadata to FileMetadata
    fn convert_metadata(rm: &RemoteMetadata, name: &str) -> FileMetadata {
        let modified = if rm.mtime > 0 {
            Some(UNIX_EPOCH + Duration::from_secs(rm.mtime as u64))
        } else {
            None
        };

        let is_hidden = name.starts_with('.');
        let permissions = FilePermissions::from_mode(rm.mode);
        let is_readonly = permissions.is_readonly();

        let mut meta = FileMetadata::new(rm.size)
            .with_hidden(is_hidden)
            .with_readonly(is_readonly)
            .with_permissions(permissions);

        if let Some(m) = modified {
            meta = meta.with_modified(m);
        }

        #[cfg(unix)]
        {
            meta.uid = Some(rm.uid);
            meta.gid = Some(rm.gid);
        }

        meta
    }

    /// Convert remote dir entry to DirEntry
    fn convert_dir_entry(re: &RemoteDirEntry) -> DirEntry {
        let entry_type = if re.link {
            EntryType::Symlink
        } else if re.dir {
            EntryType::Directory
        } else {
            EntryType::File
        };

        let modified = if re.mtime > 0 {
            Some(UNIX_EPOCH + Duration::from_secs(re.mtime as u64))
        } else {
            None
        };

        let is_hidden = re.name.starts_with('.');
        let permissions = FilePermissions::from_mode(re.mode);
        let is_readonly = permissions.is_readonly();

        let mut metadata = FileMetadata::new(re.size)
            .with_hidden(is_hidden)
            .with_readonly(is_readonly)
            .with_permissions(permissions);

        if let Some(m) = modified {
            metadata = metadata.with_modified(m);
        }

        let mut entry = DirEntry::new(PathBuf::from(&re.path), re.name.clone(), entry_type);
        entry.metadata = Some(metadata);
        entry.symlink_target_is_dir = re.link_dir;

        entry
    }
}

impl FileSystem for RemoteFileSystem {
    fn read_file(&self, path: &Path) -> io::Result<Vec<u8>> {
        let path_str = path.to_string_lossy();
        let (data_chunks, _result) = self
            .channel
            .request_with_data_blocking("read", read_params(&path_str, None, None))
            .map_err(Self::to_io_error)?;

        // Collect all streaming data chunks
        let mut content = Vec::new();
        for chunk in data_chunks {
            if let Some(b64) = chunk.get("data").and_then(|v| v.as_str()) {
                let decoded = decode_base64(b64)
                    .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
                content.extend(decoded);
            }
        }

        Ok(content)
    }

    fn read_range(&self, path: &Path, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        let path_str = path.to_string_lossy();
        let (data_chunks, result) = self
            .channel
            .request_with_data_blocking("read", read_params(&path_str, Some(offset), Some(len)))
            .map_err(Self::to_io_error)?;

        // Collect all streaming data chunks
        let mut content = Vec::new();
        for chunk in data_chunks {
            if let Some(b64) = chunk.get("data").and_then(|v| v.as_str()) {
                let decoded = decode_base64(b64)
                    .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
                content.extend(decoded);
            }
        }

        // Get the size reported by the agent (how many bytes it actually read from the file)
        let agent_reported_size = result
            .get("size")
            .and_then(|v| v.as_u64())
            .map(|s| s as usize);

        // Validate that we received the expected number of bytes.
        // This matches LocalFileSystem::read_range which uses read_exact.
        // Short reads indicate file truncation, race conditions, or metadata mismatch.
        if content.len() != len {
            return Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                format!(
                    "read_range: expected {} bytes at offset {}, got {} (agent reported: {:?}, path: {})",
                    len,
                    offset,
                    content.len(),
                    agent_reported_size,
                    path_str
                ),
            ));
        }

        Ok(content)
    }

    fn count_line_feeds_in_range(&self, path: &Path, offset: u64, len: usize) -> io::Result<usize> {
        let path_str = path.to_string_lossy();
        let result = self
            .channel
            .request_blocking("count_lf", count_lf_params(&path_str, offset, len))
            .map_err(Self::to_io_error)?;

        result
            .get("count")
            .and_then(|v| v.as_u64())
            .map(|c| c as usize)
            .ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "missing count in count_lf response",
                )
            })
    }

    fn write_file(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        let path_str = path.to_string_lossy();
        self.channel
            .request_blocking("write", write_params(&path_str, data))
            .map_err(Self::to_io_error)?;
        Ok(())
    }

    fn create_file(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        // Create an empty file first
        self.write_file(path, &[])?;
        Ok(Box::new(RemoteFileWriter::new(
            self.channel.clone(),
            path.to_path_buf(),
        )))
    }

    fn open_file(&self, path: &Path) -> io::Result<Box<dyn FileReader>> {
        // Read the entire file into memory for seeking
        let data = self.read_file(path)?;
        Ok(Box::new(RemoteFileReader::new(data)))
    }

    fn open_file_for_write(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        Ok(Box::new(RemoteFileWriter::new(
            self.channel.clone(),
            path.to_path_buf(),
        )))
    }

    fn open_file_for_append(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        // Use append-only writer that sends only new data
        Ok(Box::new(AppendingRemoteFileWriter::new(
            self.channel.clone(),
            path.to_path_buf(),
        )))
    }

    fn set_file_length(&self, path: &Path, len: u64) -> io::Result<()> {
        let path_str = path.to_string_lossy();
        self.channel
            .request_blocking("truncate", truncate_params(&path_str, len))
            .map_err(Self::to_io_error)?;
        Ok(())
    }

    fn write_patched(&self, src_path: &Path, dst_path: &Path, ops: &[WriteOp]) -> io::Result<()> {
        // Convert WriteOps to protocol PatchOps
        let patch_ops: Vec<PatchOp> = ops
            .iter()
            .map(|op| match op {
                WriteOp::Copy { offset, len } => PatchOp::copy(*offset, *len),
                WriteOp::Insert { data } => PatchOp::insert(data),
            })
            .collect();

        let src_str = src_path.to_string_lossy();
        let dst_str = dst_path.to_string_lossy();
        let dst_param = if src_path == dst_path {
            None
        } else {
            Some(dst_str.as_ref())
        };

        self.channel
            .request_blocking("patch", patch_params(&src_str, dst_param, &patch_ops))
            .map_err(Self::to_io_error)?;
        Ok(())
    }

    fn rename(&self, from: &Path, to: &Path) -> io::Result<()> {
        let params = serde_json::json!({
            "from": from.to_string_lossy(),
            "to": to.to_string_lossy()
        });
        self.channel
            .request_blocking("mv", params)
            .map_err(Self::to_io_error)?;
        Ok(())
    }

    fn copy(&self, from: &Path, to: &Path) -> io::Result<u64> {
        let params = serde_json::json!({
            "from": from.to_string_lossy(),
            "to": to.to_string_lossy()
        });
        let result = self
            .channel
            .request_blocking("cp", params)
            .map_err(Self::to_io_error)?;

        Ok(result.get("size").and_then(|v| v.as_u64()).unwrap_or(0))
    }

    fn remove_file(&self, path: &Path) -> io::Result<()> {
        let params = serde_json::json!({"path": path.to_string_lossy()});
        self.channel
            .request_blocking("rm", params)
            .map_err(Self::to_io_error)?;
        Ok(())
    }

    fn remove_dir(&self, path: &Path) -> io::Result<()> {
        let params = serde_json::json!({"path": path.to_string_lossy()});
        self.channel
            .request_blocking("rmdir", params)
            .map_err(Self::to_io_error)?;
        Ok(())
    }

    fn metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        let path_str = path.to_string_lossy();
        let result = self
            .channel
            .request_blocking("stat", stat_params(&path_str, true))
            .map_err(Self::to_io_error)?;

        let rm: RemoteMetadata = serde_json::from_value(result)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_default();
        Ok(Self::convert_metadata(&rm, &name))
    }

    fn symlink_metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        let path_str = path.to_string_lossy();
        let result = self
            .channel
            .request_blocking("stat", stat_params(&path_str, false))
            .map_err(Self::to_io_error)?;

        let rm: RemoteMetadata = serde_json::from_value(result)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_default();
        Ok(Self::convert_metadata(&rm, &name))
    }

    fn is_dir(&self, path: &Path) -> io::Result<bool> {
        let path_str = path.to_string_lossy();
        let result = self
            .channel
            .request_blocking("stat", stat_params(&path_str, true))
            .map_err(Self::to_io_error)?;

        Ok(result.get("dir").and_then(|v| v.as_bool()).unwrap_or(false))
    }

    fn is_file(&self, path: &Path) -> io::Result<bool> {
        let path_str = path.to_string_lossy();
        let result = self
            .channel
            .request_blocking("stat", stat_params(&path_str, true))
            .map_err(Self::to_io_error)?;

        Ok(result
            .get("file")
            .and_then(|v| v.as_bool())
            .unwrap_or(false))
    }

    fn set_permissions(&self, path: &Path, permissions: &FilePermissions) -> io::Result<()> {
        #[cfg(unix)]
        {
            let params = serde_json::json!({
                "path": path.to_string_lossy(),
                "mode": permissions.mode()
            });
            self.channel
                .request_blocking("chmod", params)
                .map_err(Self::to_io_error)?;
        }
        #[cfg(not(unix))]
        {
            let _ = (path, permissions);
        }
        Ok(())
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        let path_str = path.to_string_lossy();
        let result = self
            .channel
            .request_blocking("ls", ls_params(&path_str))
            .map_err(Self::to_io_error)?;

        let entries: Vec<RemoteDirEntry> = result
            .get("entries")
            .and_then(|v| serde_json::from_value(v.clone()).ok())
            .unwrap_or_default();

        Ok(entries.iter().map(Self::convert_dir_entry).collect())
    }

    fn create_dir(&self, path: &Path) -> io::Result<()> {
        let params = serde_json::json!({"path": path.to_string_lossy()});
        self.channel
            .request_blocking("mkdir", params)
            .map_err(Self::to_io_error)?;
        Ok(())
    }

    fn create_dir_all(&self, path: &Path) -> io::Result<()> {
        let params = serde_json::json!({
            "path": path.to_string_lossy(),
            "parents": true
        });
        self.channel
            .request_blocking("mkdir", params)
            .map_err(Self::to_io_error)?;
        Ok(())
    }

    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        let params = serde_json::json!({"path": path.to_string_lossy()});
        let result = self
            .channel
            .request_blocking("realpath", params)
            .map_err(Self::to_io_error)?;

        let canonical = result.get("path").and_then(|v| v.as_str()).ok_or_else(|| {
            io::Error::new(io::ErrorKind::InvalidData, "missing path in response")
        })?;

        Ok(PathBuf::from(canonical))
    }

    fn current_uid(&self) -> u32 {
        // We don't know the remote user's UID easily, return 0
        // This is used for ownership checks which we skip for remote
        0
    }

    fn remote_connection_info(&self) -> Option<&str> {
        Some(&self.connection_string)
    }

    fn home_dir(&self) -> io::Result<PathBuf> {
        let result = self
            .channel
            .request_blocking("info", serde_json::json!({}))
            .map_err(Self::to_io_error)?;

        let home = result.get("home").and_then(|v| v.as_str()).ok_or_else(|| {
            io::Error::new(io::ErrorKind::InvalidData, "missing home in response")
        })?;

        Ok(PathBuf::from(home))
    }

    fn unique_temp_path(&self, dest_path: &Path) -> PathBuf {
        // Query the remote system's temp directory instead of hardcoding /tmp,
        // which doesn't exist on Windows remotes. Falls back to /tmp if the
        // info request fails (e.g. older agent without temp_dir support).
        let temp_dir = Self::parse_temp_dir_from_info(
            self.channel
                .request_blocking("info", serde_json::json!({}))
                .ok()
                .as_ref(),
        );
        let file_name = dest_path
            .file_name()
            .unwrap_or_else(|| std::ffi::OsStr::new("fresh-save"));
        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        temp_dir.join(format!(
            "{}-{}-{}.tmp",
            file_name.to_string_lossy(),
            std::process::id(),
            timestamp
        ))
    }

    fn search_file(
        &self,
        path: &Path,
        pattern: &str,
        opts: &crate::model::filesystem::FileSearchOptions,
        cursor: &mut crate::model::filesystem::FileSearchCursor,
    ) -> io::Result<Vec<crate::model::filesystem::SearchMatch>> {
        if cursor.done {
            return Ok(vec![]);
        }

        let path_str = path.to_string_lossy();
        let params = serde_json::json!({
            "path": path_str,
            "pattern": pattern,
            "fixed_string": opts.fixed_string,
            "case_sensitive": opts.case_sensitive,
            "whole_word": opts.whole_word,
            "max_matches": opts.max_matches,
            "offset": cursor.offset,
            "running_line": cursor.running_line,
        });

        let result = self
            .channel
            .request_blocking("search_file", params)
            .map_err(Self::to_io_error)?;

        cursor.offset = result
            .get("next_offset")
            .and_then(|v| v.as_u64())
            .unwrap_or(0) as usize;
        cursor.running_line = result
            .get("running_line")
            .and_then(|v| v.as_u64())
            .unwrap_or(1) as usize;
        cursor.done = result.get("done").and_then(|v| v.as_bool()).unwrap_or(true);

        let matches: Vec<crate::model::filesystem::SearchMatch> = result
            .get("matches")
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|m| {
                        Some(crate::model::filesystem::SearchMatch {
                            byte_offset: m.get("byte_offset")?.as_u64()? as usize,
                            length: m.get("length")?.as_u64()? as usize,
                            line: m.get("line")?.as_u64()? as usize,
                            column: m.get("column")?.as_u64()? as usize,
                            context: m.get("context")?.as_str()?.to_string(),
                        })
                    })
                    .collect()
            })
            .unwrap_or_default();

        Ok(matches)
    }

    fn sudo_write(
        &self,
        path: &Path,
        data: &[u8],
        mode: u32,
        uid: u32,
        gid: u32,
    ) -> io::Result<()> {
        let path_str = path.to_string_lossy();
        self.channel
            .request_blocking(
                "sudo_write",
                sudo_write_params(&path_str, data, mode, uid, gid),
            )
            .map_err(Self::to_io_error)?;
        Ok(())
    }
}

/// Remote file reader - wraps in-memory data
struct RemoteFileReader {
    cursor: Cursor<Vec<u8>>,
}

impl RemoteFileReader {
    fn new(data: Vec<u8>) -> Self {
        Self {
            cursor: Cursor::new(data),
        }
    }
}

impl Read for RemoteFileReader {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.cursor.read(buf)
    }
}

impl Seek for RemoteFileReader {
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        self.cursor.seek(pos)
    }
}

impl FileReader for RemoteFileReader {}

/// Remote file writer - buffers writes and flushes on sync
struct RemoteFileWriter {
    channel: Arc<AgentChannel>,
    path: PathBuf,
    buffer: Vec<u8>,
}

impl RemoteFileWriter {
    fn new(channel: Arc<AgentChannel>, path: PathBuf) -> Self {
        Self {
            channel,
            path,
            buffer: Vec::new(),
        }
    }
}

impl Write for RemoteFileWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.buffer.extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        // Flush is a no-op; actual write happens on sync_all
        Ok(())
    }
}

impl FileWriter for RemoteFileWriter {
    fn sync_all(&self) -> io::Result<()> {
        let path_str = self.path.to_string_lossy();
        self.channel
            .request_blocking("write", write_params(&path_str, &self.buffer))
            .map_err(RemoteFileSystem::to_io_error)?;
        Ok(())
    }
}

/// Remote file writer for append operations - only sends new data
struct AppendingRemoteFileWriter {
    channel: Arc<AgentChannel>,
    path: PathBuf,
    buffer: Vec<u8>,
}

impl AppendingRemoteFileWriter {
    fn new(channel: Arc<AgentChannel>, path: PathBuf) -> Self {
        Self {
            channel,
            path,
            buffer: Vec::new(),
        }
    }
}

impl Write for AppendingRemoteFileWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.buffer.extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl FileWriter for AppendingRemoteFileWriter {
    fn sync_all(&self) -> io::Result<()> {
        if self.buffer.is_empty() {
            return Ok(());
        }
        let path_str = self.path.to_string_lossy();
        self.channel
            .request_blocking("append", append_params(&path_str, &self.buffer))
            .map_err(RemoteFileSystem::to_io_error)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_convert_metadata() {
        let rm = RemoteMetadata {
            size: 1234,
            mtime: 1700000000,
            mode: 0o644,
            uid: 1000,
            gid: 1000,
            dir: false,
            file: true,
            link: false,
        };

        let meta = RemoteFileSystem::convert_metadata(&rm, "test.txt");
        assert_eq!(meta.size, 1234);
        assert!(!meta.is_hidden);
        assert!(!meta.is_readonly);

        let meta = RemoteFileSystem::convert_metadata(&rm, ".hidden");
        assert!(meta.is_hidden);
    }

    #[test]
    fn test_convert_dir_entry() {
        let re = RemoteDirEntry {
            name: "file.rs".to_string(),
            path: "/home/user/file.rs".to_string(),
            dir: false,
            file: true,
            link: false,
            link_dir: false,
            size: 100,
            mtime: 1700000000,
            mode: 0o644,
        };

        let entry = RemoteFileSystem::convert_dir_entry(&re);
        assert_eq!(entry.name, "file.rs");
        assert_eq!(entry.entry_type, EntryType::File);
        assert!(!entry.is_symlink());
    }
}
