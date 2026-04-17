//! Save/write-recipe logic for `TextBuffer`.
//!
//! Types: `SudoSaveRequired`, `WriteRecipe`, `RecipeAction`.
//! Free fns: `build_write_recipe`, save-to-disk helpers that only
//! need `&dyn FileSystem` + local arguments.

use crate::model::encoding::Encoding;
use crate::model::filesystem::{FileMetadata, FileSystem, FileWriter, WriteOp};
use crate::model::piece_tree::{BufferData, BufferLocation, PieceTree, StringBuffer};
use super::format::{self, BufferFormat};
use super::file_kind::BufferFileKind;
use super::persistence::Persistence;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;

// ---------------------------------------------------------------------------
// SudoSaveRequired
// ---------------------------------------------------------------------------

/// Error returned when a file save operation requires elevated privileges.
///
/// This error contains all the information needed to perform the save via sudo
/// in a single operation, preserving original file ownership and permissions.
#[derive(Debug, Clone, PartialEq)]
pub struct SudoSaveRequired {
    /// Path to the temporary file containing the new content
    pub temp_path: PathBuf,
    /// Destination path where the file should be saved
    pub dest_path: PathBuf,
    /// Original file owner (UID)
    pub uid: u32,
    /// Original file group (GID)
    pub gid: u32,
    /// Original file permissions (mode)
    pub mode: u32,
}

impl std::fmt::Display for SudoSaveRequired {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Permission denied saving to {}. Use sudo to complete the operation.",
            self.dest_path.display()
        )
    }
}

impl std::error::Error for SudoSaveRequired {}

// ---------------------------------------------------------------------------
// WriteRecipe / RecipeAction
// ---------------------------------------------------------------------------

/// A write recipe built from the piece tree for saving
pub(crate) struct WriteRecipe {
    /// The source file path for Copy operations (if any)
    pub(crate) src_path: Option<PathBuf>,
    /// Data chunks for Insert operations (owned to avoid lifetime issues)
    pub(crate) insert_data: Vec<Vec<u8>>,
    /// Sequence of actions to build the output file
    pub(crate) actions: Vec<RecipeAction>,
}

/// An action in a write recipe
#[derive(Debug, Clone, Copy)]
pub(crate) enum RecipeAction {
    /// Copy bytes from source file at offset
    Copy { offset: u64, len: u64 },
    /// Insert data from insert_data[index]
    Insert { index: usize },
}

impl WriteRecipe {
    /// Convert the recipe to WriteOp slice for use with filesystem write_patched
    pub(crate) fn to_write_ops(&self) -> Vec<WriteOp<'_>> {
        self.actions
            .iter()
            .map(|action| match action {
                RecipeAction::Copy { offset, len } => WriteOp::Copy {
                    offset: *offset,
                    len: *len,
                },
                RecipeAction::Insert { index } => WriteOp::Insert {
                    data: &self.insert_data[*index],
                },
            })
            .collect()
    }

    /// Check if this recipe has any Copy operations
    pub(crate) fn has_copy_ops(&self) -> bool {
        self.actions
            .iter()
            .any(|a| matches!(a, RecipeAction::Copy { .. }))
    }

    /// Flatten all Insert operations into a single buffer.
    /// Only valid when has_copy_ops() returns false.
    pub(crate) fn flatten_inserts(&self) -> Vec<u8> {
        let mut result = Vec::new();
        for action in &self.actions {
            if let RecipeAction::Insert { index } = action {
                result.extend_from_slice(&self.insert_data[*index]);
            }
        }
        result
    }
}

// ---------------------------------------------------------------------------
// Free functions (extracted from impl TextBuffer)
// ---------------------------------------------------------------------------

/// Check if we should use in-place writing to preserve file ownership.
/// Returns true if the file exists and is owned by a different user.
/// On Unix, only root or the file owner can change file ownership with chown.
/// When the current user is not the file owner, using atomic write (temp file + rename)
/// would change the file's ownership to the current user. To preserve ownership,
/// we must write directly to the existing file instead.
pub(super) fn should_use_inplace_write(
    fs: &Arc<dyn FileSystem + Send + Sync>,
    dest_path: &Path,
) -> bool {
    !fs.is_owner(dest_path)
}

/// Build a write recipe from the piece tree for saving.
///
/// This creates a recipe of Copy and Insert operations that can reconstruct
/// the buffer content. Copy operations reference unchanged regions in the
/// source file, while Insert operations contain new/modified data.
///
/// # Returns
/// A WriteRecipe with the source path, insert data, and sequence of actions.
pub(super) fn build_write_recipe(
    piece_tree: &PieceTree,
    buffers: &[StringBuffer],
    format: &BufferFormat,
    file_kind: &BufferFileKind,
    persistence: &Persistence,
) -> io::Result<WriteRecipe> {
    let total = piece_tree.total_bytes();

    // Determine the source file for Copy operations (if any)
    // We can only use Copy if:
    // 1. We have a source file path
    // 2. The source file exists
    // 3. No line ending conversion is needed
    // 4. No encoding conversion is needed
    let needs_line_ending_conversion = format.line_ending_changed_since_load();
    // We need encoding conversion if:
    // - NOT a binary file (binary files preserve raw bytes), AND
    // - Either the encoding changed from the original, OR
    // - The target encoding isn't plain UTF-8/ASCII (since internal storage is UTF-8)
    // For example: UTF-8 BOM files are stored as UTF-8, so we need to add BOM on save
    let needs_encoding_conversion = !file_kind.is_binary()
        && (format.encoding_changed_since_load()
            || !matches!(
                format.encoding(),
                Encoding::Utf8 | Encoding::Ascii
            ));
    let needs_conversion = needs_line_ending_conversion || needs_encoding_conversion;

    let src_path_for_copy: Option<&Path> = if needs_conversion {
        None
    } else {
        persistence.file_path().filter(|p| persistence.fs().exists(p))
    };
    let target_ending = format.line_ending();
    let target_encoding = format.encoding();

    let mut insert_data: Vec<Vec<u8>> = Vec::new();
    let mut actions: Vec<RecipeAction> = Vec::new();

    // Add BOM as the first piece if the target encoding has one
    if let Some(bom) = target_encoding.bom_bytes() {
        insert_data.push(bom.to_vec());
        actions.push(RecipeAction::Insert { index: 0 });
    }

    for piece_view in piece_tree.iter_pieces_in_range(0, total) {
        let buffer_id = piece_view.location.buffer_id();
        let buffer = buffers.get(buffer_id).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                std::format!("Buffer {} not found", buffer_id),
            )
        })?;

        match &buffer.data {
            // Unloaded buffer: can use Copy if same source file, else load and send
            BufferData::Unloaded {
                file_path,
                file_offset,
                ..
            } => {
                // Can only use Copy if:
                // - This is a Stored piece (original file content)
                // - We have a valid source for copying
                // - This buffer is from that source
                // - No line ending or encoding conversion needed
                let can_copy = matches!(piece_view.location, BufferLocation::Stored(_))
                    && src_path_for_copy.is_some_and(|src| file_path == src);

                if can_copy {
                    let src_offset = (*file_offset + piece_view.buffer_offset) as u64;
                    actions.push(RecipeAction::Copy {
                        offset: src_offset,
                        len: piece_view.bytes as u64,
                    });
                    continue;
                }

                // Need to load and send this unloaded region
                // This happens when: different source file, or conversion needed
                let data = persistence.fs().read_range(
                    file_path,
                    (*file_offset + piece_view.buffer_offset) as u64,
                    piece_view.bytes,
                )?;

                let data = if needs_line_ending_conversion {
                    format::convert_line_endings_to(&data, target_ending)
                } else {
                    data
                };

                // Convert encoding if needed
                let data = if needs_encoding_conversion {
                    format::convert_to_encoding(&data, target_encoding)
                } else {
                    data
                };

                let index = insert_data.len();
                insert_data.push(data);
                actions.push(RecipeAction::Insert { index });
            }

            // Loaded data: send as Insert
            BufferData::Loaded { data, .. } => {
                let start = piece_view.buffer_offset;
                let end = start + piece_view.bytes;
                let chunk = &data[start..end];

                let chunk = if needs_line_ending_conversion {
                    format::convert_line_endings_to(chunk, target_ending)
                } else {
                    chunk.to_vec()
                };

                // Convert encoding if needed
                let chunk = if needs_encoding_conversion {
                    format::convert_to_encoding(&chunk, target_encoding)
                } else {
                    chunk
                };

                let index = insert_data.len();
                insert_data.push(chunk);
                actions.push(RecipeAction::Insert { index });
            }
        }
    }

    Ok(WriteRecipe {
        src_path: src_path_for_copy.map(|p| p.to_path_buf()),
        insert_data,
        actions,
    })
}

/// Create a temporary file for saving.
///
/// Tries to create the file in the same directory as the destination file first
/// to allow for an atomic rename. If that fails (e.g., due to directory permissions),
/// falls back to the system temporary directory.
pub(super) fn create_temp_file(
    fs: &Arc<dyn FileSystem + Send + Sync>,
    dest_path: &Path,
) -> io::Result<(PathBuf, Box<dyn FileWriter>)> {
    // Try creating in same directory first
    let same_dir_temp = fs.temp_path_for(dest_path);
    match fs.create_file(&same_dir_temp) {
        Ok(file) => Ok((same_dir_temp, file)),
        Err(e) if e.kind() == io::ErrorKind::PermissionDenied => {
            // Fallback to system temp directory
            let temp_path = fs.unique_temp_path(dest_path);
            let file = fs.create_file(&temp_path)?;
            Ok((temp_path, file))
        }
        Err(e) => Err(e),
    }
}

/// Create a temporary file in the recovery directory for in-place writes.
/// This allows recovery if a crash occurs during the in-place write operation.
pub(super) fn create_recovery_temp_file(
    fs: &Arc<dyn FileSystem + Send + Sync>,
    dest_path: &Path,
) -> io::Result<(PathBuf, Box<dyn FileWriter>)> {
    // Get recovery directory: $XDG_DATA_HOME/fresh/recovery or ~/.local/share/fresh/recovery
    let recovery_dir = crate::input::input_history::get_data_dir()
        .map(|d| d.join("recovery"))
        .unwrap_or_else(|_| std::env::temp_dir());

    // Ensure directory exists
    fs.create_dir_all(&recovery_dir)?;

    // Create unique filename based on destination file and timestamp
    let file_name = dest_path
        .file_name()
        .unwrap_or_else(|| std::ffi::OsStr::new("fresh-save"));
    let timestamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let pid = std::process::id();

    let temp_name = format!(
        ".inplace-{}-{}-{}.tmp",
        file_name.to_string_lossy(),
        pid,
        timestamp
    );
    let temp_path = recovery_dir.join(temp_name);

    let file = fs.create_file(&temp_path)?;
    Ok((temp_path, file))
}

/// Get the path for in-place write recovery metadata.
/// Uses the same recovery directory as temp files.
pub(super) fn inplace_recovery_meta_path(dest_path: &Path) -> PathBuf {
    let recovery_dir = crate::input::input_history::get_data_dir()
        .map(|d| d.join("recovery"))
        .unwrap_or_else(|_| std::env::temp_dir());

    let hash = crate::services::recovery::path_hash(dest_path);
    recovery_dir.join(format!("{}.inplace.json", hash))
}

/// Write in-place recovery metadata using fs.
/// This is called before the dangerous streaming step so we can recover on crash.
pub(super) fn write_inplace_recovery_meta(
    fs: &Arc<dyn FileSystem + Send + Sync>,
    meta_path: &Path,
    dest_path: &Path,
    temp_path: &Path,
    original_metadata: &Option<FileMetadata>,
) -> io::Result<()> {
    #[cfg(unix)]
    let (uid, gid, mode) = original_metadata
        .as_ref()
        .map(|m| {
            (
                m.uid.unwrap_or(0),
                m.gid.unwrap_or(0),
                m.permissions.as_ref().map(|p| p.mode()).unwrap_or(0o644),
            )
        })
        .unwrap_or((0, 0, 0o644));
    #[cfg(not(unix))]
    let (uid, gid, mode) = (0u32, 0u32, 0o644u32);

    let recovery = crate::services::recovery::InplaceWriteRecovery::new(
        dest_path.to_path_buf(),
        temp_path.to_path_buf(),
        uid,
        gid,
        mode,
    );

    let json = serde_json::to_string_pretty(&recovery)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

    fs.write_file(meta_path, json.as_bytes())
}

/// Write using in-place mode to preserve file ownership.
///
/// This is used when the file is owned by a different user and we need
/// to write directly to the existing file to preserve its ownership.
///
/// The approach:
/// 1. Write the recipe to a temp file first (reads from original, writes to temp)
/// 2. Stream the temp file content to the destination file (truncates and writes)
/// 3. Delete the temp file
///
/// This avoids the bug where truncating the destination before reading Copy chunks
/// would corrupt the file. It also works for huge files since we stream in chunks.
pub(super) fn save_with_inplace_write(
    fs: &Arc<dyn FileSystem + Send + Sync>,
    dest_path: &Path,
    recipe: &WriteRecipe,
) -> anyhow::Result<()> {
    let original_metadata = fs.metadata_if_exists(dest_path);

    // Optimization: if no Copy ops, we can write directly without a temp file
    // (same as the non-inplace path for small files)
    if !recipe.has_copy_ops() {
        let data = recipe.flatten_inserts();
        return write_data_inplace(fs, dest_path, &data, original_metadata);
    }

    // Step 1: Write recipe to a temp file in the recovery directory
    // This reads Copy chunks from the original file (still intact) and writes to temp.
    // Using the recovery directory allows crash recovery if the operation fails.
    let (temp_path, mut temp_file) = create_recovery_temp_file(fs, dest_path)?;
    if let Err(e) = write_recipe_to_file(fs, &mut temp_file, recipe) {
        // Best-effort cleanup of temp file on write failure
        #[allow(clippy::let_underscore_must_use)]
        let _ = fs.remove_file(&temp_path);
        return Err(e.into());
    }
    temp_file.sync_all()?;
    drop(temp_file);

    // Step 1.5: Save recovery metadata before the dangerous step
    // If we crash during step 2, this metadata + temp file allows recovery
    let recovery_meta_path = inplace_recovery_meta_path(dest_path);
    // Best effort - don't fail the save if we can't write recovery metadata
    #[allow(clippy::let_underscore_must_use)]
    let _ = write_inplace_recovery_meta(
        fs,
        &recovery_meta_path,
        dest_path,
        &temp_path,
        &original_metadata,
    );

    // Step 2: Stream temp file content to destination
    // Now it's safe to truncate the destination since all data is in temp
    match fs.open_file_for_write(dest_path) {
        Ok(mut out_file) => {
            if let Err(e) = stream_file_to_writer(fs, &temp_path, &mut out_file) {
                // Don't delete temp file or recovery metadata - allow recovery
                return Err(e.into());
            }
            out_file.sync_all()?;
            // Success! Clean up temp file and recovery metadata (best-effort)
            #[allow(clippy::let_underscore_must_use)]
            let _ = fs.remove_file(&temp_path);
            #[allow(clippy::let_underscore_must_use)]
            let _ = fs.remove_file(&recovery_meta_path);
            Ok(())
        }
        Err(e) if e.kind() == io::ErrorKind::PermissionDenied => {
            // Can't write to destination - trigger sudo fallback
            // Keep temp file for sudo to use, clean up recovery metadata (best-effort)
            #[allow(clippy::let_underscore_must_use)]
            let _ = fs.remove_file(&recovery_meta_path);
            Err(make_sudo_error(temp_path, dest_path, original_metadata))
        }
        Err(e) => {
            // Don't delete temp file or recovery metadata - allow recovery
            Err(e.into())
        }
    }
}

/// Write data directly to a file in-place, with sudo fallback on permission denied.
pub(super) fn write_data_inplace(
    fs: &Arc<dyn FileSystem + Send + Sync>,
    dest_path: &Path,
    data: &[u8],
    original_metadata: Option<FileMetadata>,
) -> anyhow::Result<()> {
    match fs.open_file_for_write(dest_path) {
        Ok(mut out_file) => {
            out_file.write_all(data)?;
            out_file.sync_all()?;
            Ok(())
        }
        Err(e) if e.kind() == io::ErrorKind::PermissionDenied => {
            // Create temp file for sudo fallback
            let (temp_path, mut temp_file) = create_temp_file(fs, dest_path)?;
            temp_file.write_all(data)?;
            temp_file.sync_all()?;
            drop(temp_file);
            Err(make_sudo_error(temp_path, dest_path, original_metadata))
        }
        Err(e) => Err(e.into()),
    }
}

/// Stream a file's content to a writer in chunks to avoid memory issues with large files.
pub(super) fn stream_file_to_writer(
    fs: &Arc<dyn FileSystem + Send + Sync>,
    src_path: &Path,
    out_file: &mut Box<dyn FileWriter>,
) -> io::Result<()> {
    const CHUNK_SIZE: usize = 1024 * 1024; // 1MB chunks

    let file_size = fs.metadata(src_path)?.size;
    let mut offset = 0u64;

    while offset < file_size {
        let remaining = file_size - offset;
        let chunk_len = std::cmp::min(remaining, CHUNK_SIZE as u64) as usize;
        let chunk = fs.read_range(src_path, offset, chunk_len)?;
        out_file.write_all(&chunk)?;
        offset += chunk_len as u64;
    }

    Ok(())
}

/// Write the recipe content to a file writer.
pub(super) fn write_recipe_to_file(
    fs: &Arc<dyn FileSystem + Send + Sync>,
    out_file: &mut Box<dyn FileWriter>,
    recipe: &WriteRecipe,
) -> io::Result<()> {
    for action in &recipe.actions {
        match action {
            RecipeAction::Copy { offset, len } => {
                // Read from source and write to output
                let src_path = recipe.src_path.as_ref().ok_or_else(|| {
                    io::Error::new(io::ErrorKind::InvalidData, "Copy action without source")
                })?;
                let data = fs.read_range(src_path, *offset, *len as usize)?;
                out_file.write_all(&data)?;
            }
            RecipeAction::Insert { index } => {
                out_file.write_all(&recipe.insert_data[*index])?;
            }
        }
    }
    Ok(())
}

/// Internal helper to create a SudoSaveRequired error.
pub(super) fn make_sudo_error(
    temp_path: PathBuf,
    dest_path: &Path,
    original_metadata: Option<FileMetadata>,
) -> anyhow::Error {
    #[cfg(unix)]
    let (uid, gid, mode) = if let Some(ref meta) = original_metadata {
        (
            meta.uid.unwrap_or(0),
            meta.gid.unwrap_or(0),
            meta.permissions
                .as_ref()
                .map(|p| p.mode() & 0o7777)
                .unwrap_or(0),
        )
    } else {
        (0, 0, 0)
    };
    #[cfg(not(unix))]
    let (uid, gid, mode) = (0u32, 0u32, 0u32);

    let _ = original_metadata; // suppress unused warning on non-Unix

    anyhow::anyhow!(SudoSaveRequired {
        temp_path,
        dest_path: dest_path.to_path_buf(),
        uid,
        gid,
        mode,
    })
}
