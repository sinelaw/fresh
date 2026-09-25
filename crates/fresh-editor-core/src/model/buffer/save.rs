//! Save/write-recipe logic for `TextBuffer`.
//!
//! Types: `SudoSaveRequired`, `WriteRecipe`, `RecipeAction`.
//! Free fns: `build_write_recipe`, save-to-disk helpers that only
//! need `&dyn FileSystem` + local arguments.

use super::file_kind::BufferFileKind;
use super::format::{self, BufferFormat};
use super::persistence::Persistence;
use crate::model::encoding::Encoding;
use crate::model::filesystem::{FileMetadata, FileSystem, FileWriter, ReplaceError, WriteOp};
use crate::model::piece_tree::{BufferData, BufferLocation, PieceTree, StringBuffer};
use crate::recovery_types::InplaceWriteRecovery;
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
///
/// It owns the temp file holding the new content: the file is deleted when
/// the error is dropped, through the filesystem that created it. A caller
/// that offers the sudo prompt takes the error out of the `anyhow::Error`
/// (`downcast`) and keeps it for as long as the prompt is up; every other
/// caller just drops it.
#[derive(Debug, PartialEq)]
pub struct SudoSaveRequired {
    /// The temporary file containing the new content
    temp_file: SudoSaveTempFile,
    /// Destination path where the file should be saved
    pub dest_path: PathBuf,
    /// Original file owner (UID)
    pub uid: u32,
    /// Original file group (GID)
    pub gid: u32,
    /// Original file permissions (mode)
    pub mode: u32,
}

impl SudoSaveRequired {
    /// Path of the temporary file containing the new content.
    pub fn temp_path(&self) -> &Path {
        &self.temp_file.path
    }

    /// The new content, read back from the temp file through the filesystem
    /// that wrote it.
    pub fn read_content(&self) -> io::Result<Vec<u8>> {
        self.temp_file.fs.read_file(&self.temp_file.path)
    }

    /// Delete the temp file now, for an exit that runs no destructors (a
    /// terminating signal while the sudo prompt is open). Dropping this
    /// afterwards tries again, which is harmless.
    pub fn remove_temp_file(&self) -> io::Result<()> {
        self.temp_file.fs.remove_file(&self.temp_file.path)
    }
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

/// A temp file a save created, deleted on drop through the filesystem that
/// created it — never left to its holder to remember.
pub(super) struct SudoSaveTempFile {
    fs: Arc<dyn FileSystem + Send + Sync>,
    path: PathBuf,
}

impl SudoSaveTempFile {
    /// Own the file at `path`, which `fs` just created.
    pub(super) fn new(fs: &Arc<dyn FileSystem + Send + Sync>, path: PathBuf) -> Self {
        Self {
            fs: Arc::clone(fs),
            path,
        }
    }
}

impl Drop for SudoSaveTempFile {
    fn drop(&mut self) {
        if let Err(err) = self.fs.remove_file(&self.path) {
            tracing::debug!(
                "Failed to remove sudo-save temp file {}: {}",
                self.path.display(),
                err
            );
        }
    }
}

impl std::fmt::Debug for SudoSaveTempFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("SudoSaveTempFile").field(&self.path).finish()
    }
}

impl PartialEq for SudoSaveTempFile {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

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
    /// The recipe for an empty file.
    pub(crate) fn empty() -> Self {
        Self {
            src_path: None,
            insert_data: Vec::new(),
            actions: Vec::new(),
        }
    }

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

/// Write `recipe` to `dest_path` on the local filesystem.
///
/// The one place a save chooses between replacing the file atomically and
/// overwriting it in place. A write-then-rename is the default, since a
/// crash then leaves either the old content or the new; but it leaves a
/// *new* file behind, so when that can't stand in for the original the file
/// is overwritten in place instead, its content staged in the recovery
/// directory first (see [`write_in_place_staged`]):
///
/// * the file is owned by another user — only root may give a new file
///   their ownership. Known before anything is written, so the recipe is
///   streamed straight in (see [`save_with_inplace_write`]);
/// * the filesystem reports that a replacement wouldn't keep the file's
///   identity ([`crate::model::filesystem::IdentityLoss`]): other hard links, or an owner, group or
///   extended attribute it can't carry over (issue #3348).
///
/// When the file (or, for the atomic write, its directory) can't be written
/// at all, the content is staged for the sudo prompt instead
/// ([`SudoSaveRequired`]).
///
/// `recovery_dir` is the editor's recovery directory (its top level, not a
/// session's scoped one), where an in-place write stages its copy.
pub(super) fn save_local(
    fs: &Arc<dyn FileSystem + Send + Sync>,
    dest_path: &Path,
    recipe: &WriteRecipe,
    recovery_dir: &Path,
) -> anyhow::Result<()> {
    if recipe.has_copy_ops() {
        refuse_copy_from_torn_file(&**fs, recovery_dir, dest_path)?;
    }
    if !fs.is_owner(dest_path) {
        return save_with_inplace_write(fs, dest_path, recipe, recovery_dir);
    }

    let mut data = Vec::new();
    write_recipe(fs, &mut data, recipe)?;
    match fs.replace_file_preserving_identity(dest_path, &data) {
        Ok(()) => Ok(()),
        Err(ReplaceError::IdentityNotPreserved(loss)) => {
            tracing::debug!("Writing {} in place: {loss}", dest_path.display());
            let original_metadata = fs.metadata_if_exists(dest_path);
            write_data_inplace(fs, dest_path, &data, original_metadata, recovery_dir)
        }
        Err(ReplaceError::Io(e)) if e.kind() == io::ErrorKind::PermissionDenied => {
            let original_metadata = fs.metadata_if_exists(dest_path);
            Err(stage_for_sudo(fs, dest_path, &data, original_metadata)?)
        }
        Err(ReplaceError::Io(e)) => Err(e.into()),
    }
}

/// A recipe with Copy ops (a large file's) reads the unchanged parts of the
/// file back from the file itself, as it was when the buffer loaded it. An
/// in-place write that failed part-way (or a crash during one) leaves the
/// file torn: its start holds new content, so the same offsets now read the
/// wrong bytes, and a save would write a "complete" file with them
/// (issue #3382). Such a write leaves recovery metadata pointing at a
/// complete copy of what it was writing; while that is there and the file
/// doesn't match it, refuse, and say where the copy is. The copy is offered
/// to the user when the editor starts, and removed once they decide.
///
/// Metadata whose copy is gone, or matches the file (the write finished
/// after all), says nothing about the file.
fn refuse_copy_from_torn_file(
    fs: &dyn FileSystem,
    recovery_dir: &Path,
    dest_path: &Path,
) -> anyhow::Result<()> {
    let meta_path = InplaceWriteRecovery::meta_path(recovery_dir, dest_path);
    let Some(recovery) = fs
        .read_file(&meta_path)
        .ok()
        .and_then(|json| serde_json::from_slice::<InplaceWriteRecovery>(&json).ok())
    else {
        return Ok(());
    };
    if recovery.dest_path != dest_path
        || !fs.exists(&recovery.temp_path)
        || same_content(fs, &recovery.temp_path, dest_path).unwrap_or(false)
    {
        return Ok(());
    }
    Err(anyhow::anyhow!(
        "Not saved: an earlier save of {} was interrupted, so the file may be damaged, and this save would read from it. What that save was writing is kept in {}",
        dest_path.display(),
        recovery.temp_path.display()
    ))
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
    // A genuine Classic-Mac (CR) *text* buffer stores its line breaks as
    // `\n` internally (normalized on load / inserted on Enter), so saving it
    // needs the `\n` -> `\r` conversion even when the ending is unchanged
    // since load (issue #2736). This is gated on `line_endings_normalized`:
    // a CR-detected buffer whose raw `\r` bytes were preserved verbatim
    // (binary, mixed endings, or a large/lazy CR load) is NOT normalized, so
    // it must be written byte-for-byte — reconstructing `\r` there would
    // corrupt any real `\n` in the content and break round-trips. LF and
    // CRLF keep their raw bytes and only convert when the user actually
    // changed the ending.
    let needs_line_ending_conversion = format.line_ending_changed_since_load()
        || (format.line_ending() == super::format::LineEnding::CR
            && format.line_endings_normalized());
    // We need encoding conversion if:
    // - NOT a binary file (binary files preserve raw bytes), AND
    // - Either the encoding changed from the original, OR
    // - The target encoding isn't plain UTF-8/ASCII (since internal storage is UTF-8)
    // For example: UTF-8 BOM files are stored as UTF-8, so we need to add BOM on save
    let needs_encoding_conversion = !file_kind.is_binary()
        && (format.encoding_changed_since_load()
            || !matches!(format.encoding(), Encoding::Utf8 | Encoding::Ascii));
    let needs_conversion = needs_line_ending_conversion || needs_encoding_conversion;

    let src_path_for_copy: Option<&Path> = if needs_conversion {
        None
    } else {
        persistence
            .file_path()
            .filter(|p| persistence.fs().exists(p))
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
                format!("Buffer {} not found", buffer_id),
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

/// Create a temporary file holding a save's new content for the sudo prompt
/// ([`SudoSaveRequired::temp_path`]), deleted when the returned guard drops.
///
/// Tries to create the file in the same directory as the destination file first.
/// If that fails (e.g., due to directory permissions), falls back to the system
/// temporary directory.
///
/// Readable only by its owner either way: it holds the file's content until
/// the prompt is answered, and the file's own permissions don't carry over
/// to it (the sudo write sets them on the destination itself).
fn create_temp_file(
    fs: &Arc<dyn FileSystem + Send + Sync>,
    dest_path: &Path,
) -> io::Result<(SudoSaveTempFile, Box<dyn FileWriter>)> {
    // Try creating in same directory first
    let (temp_path, file) =
        match crate::model::filesystem::create_private_temp_file_for(&**fs, dest_path) {
            Ok(created) => created,
            Err(e) if e.kind() == io::ErrorKind::PermissionDenied => {
                // Fallback to system temp directory
                let temp_path = fs.unique_temp_path(dest_path);
                let file = fs.create_new_private_file(&temp_path)?;
                (temp_path, file)
            }
            Err(e) => return Err(e),
        };
    Ok((SudoSaveTempFile::new(fs, temp_path), file))
}

/// Create the file an in-place write stages its copy of the new content in
/// (see [`write_in_place_staged`]), readable only by its owner: it holds the
/// file's content where the file's own permissions don't guard it.
///
/// In the recovery directory, where a copy is found again after a crash —
/// or, when that can't be written (under `su`, `$HOME` may still be another
/// user's; its disk may be full), next to the file itself, or else in the
/// system temp directory (issue #3381): a large file's save can't go ahead
/// without a copy anywhere, since it reads the unchanged parts back from the
/// file it overwrites. The recovery metadata points at the copy wherever it
/// is.
///
/// If no place works, the error says so, and is out of space only if the
/// file's own directory is (so writing the file without a copy would
/// probably fail part-way too), or the recovery directory is and nothing
/// says the file's isn't.
fn create_staging_file(
    fs: &dyn FileSystem,
    recovery_dir: &Path,
    dest_path: &Path,
) -> io::Result<(PathBuf, Box<dyn FileWriter>)> {
    // Named after the destination; only the start of a long file name, so
    // the name stays within the limits of filesystems that allow short
    // names (issue #3409)
    let timestamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let temp_name = format!(
        "{STAGED_PREFIX}{}-{}-{timestamp}{STAGED_SUFFIX}",
        crate::model::filesystem::temp_name_stem(dest_path),
        std::process::id(),
    );

    let mut errors = Vec::new();
    for (i, dir) in staging_dirs(recovery_dir, dest_path)
        .into_iter()
        .enumerate()
    {
        let temp_path = dir.join(&temp_name);
        // Only the recovery directory is ours to create
        let created = if i == 0 {
            fs.create_dir_all(&dir)
        } else {
            Ok(())
        };
        match created.and_then(|()| fs.create_new_private_file(&temp_path)) {
            Ok(file) => {
                if !errors.is_empty() {
                    tracing::warn!(
                        "Staged the copy of {} for its in-place save in {} ({})",
                        dest_path.display(),
                        dir.display(),
                        describe_staging_errors(&errors)
                    );
                }
                return Ok((temp_path, file));
            }
            Err(e) => errors.push((dir, e)),
        }
    }
    let beside_file_full = errors.get(1).is_some_and(|(_, e)| is_out_of_space(e));
    let beside_file_not_full = errors
        .get(1)
        .is_some_and(|(_, e)| e.kind() != io::ErrorKind::PermissionDenied && !is_out_of_space(e));
    let kind = if beside_file_full
        || (!beside_file_not_full && errors.first().is_some_and(|(_, e)| is_out_of_space(e)))
    {
        io::ErrorKind::StorageFull
    } else {
        io::ErrorKind::Other
    };
    Err(io::Error::new(
        kind,
        format!(
            "couldn't stage a copy of the new content anywhere ({})",
            describe_staging_errors(&errors)
        ),
    ))
}

/// Where [`create_staging_file`] tries to stage a copy for `dest_path`, in
/// order: the recovery directory, the file's own directory, the system
/// temp directory.
fn staging_dirs(recovery_dir: &Path, dest_path: &Path) -> Vec<PathBuf> {
    let mut dirs = vec![recovery_dir.to_path_buf()];
    if let Some(parent) = dest_path.parent().filter(|p| !p.as_os_str().is_empty()) {
        dirs.push(parent.to_path_buf());
    }
    dirs.push(std::env::temp_dir());
    dirs
}

fn describe_staging_errors(errors: &[(PathBuf, io::Error)]) -> String {
    errors
        .iter()
        .map(|(dir, e)| format!("{}: {e}", dir.display()))
        .collect::<Vec<_>>()
        .join("; ")
}

const STAGED_PREFIX: &str = ".inplace-";
const STAGED_SUFFIX: &str = ".tmp";

/// Whether `path` is a copy [`create_staging_file`] staged for `dest_path`:
/// named like one, in one of the places it stages. Recovery metadata is
/// only trusted to name such a file for removal.
fn is_staged_copy(path: &Path, recovery_dir: &Path, dest_path: &Path) -> bool {
    path.file_name()
        .and_then(|n| n.to_str())
        .is_some_and(|n| n.starts_with(STAGED_PREFIX) && n.ends_with(STAGED_SUFFIX))
        && path.parent().is_some_and(|parent| {
            staging_dirs(recovery_dir, dest_path)
                .iter()
                .any(|dir| dir == parent)
        })
}

/// Write in-place recovery metadata using fs.
/// This is called before the dangerous streaming step so we can recover on crash.
///
/// Returns what the metadata held before, for [`StagedCopy`] to restore if
/// the write never touches the file, or to supersede once it does.
fn write_inplace_recovery_meta(
    fs: &dyn FileSystem,
    meta_path: &Path,
    dest_path: &Path,
    temp_path: &Path,
    original_metadata: &Option<FileMetadata>,
) -> io::Result<Option<PreviousMeta>> {
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

    let recovery = InplaceWriteRecovery::new(
        dest_path.to_path_buf(),
        temp_path.to_path_buf(),
        uid,
        gid,
        mode,
    );

    let json = serde_json::to_string_pretty(&recovery)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

    let previous = fs.read_file(meta_path).ok().map(|json| {
        // One staged copy per destination: the metadata is the only pointer
        // to the copy an earlier attempt left (a failed write, or a crash),
        // and a complete copy staged for the same file supersedes it once
        // the file is being overwritten with it. Without this, every
        // failing auto-save left one more copy behind.
        let superseded_copy = serde_json::from_slice::<InplaceWriteRecovery>(&json)
            .ok()
            .filter(|previous| {
                previous.temp_path != temp_path
                    && meta_path.parent().is_some_and(|recovery_dir| {
                        is_staged_copy(&previous.temp_path, recovery_dir, dest_path)
                    })
                    && (previous.is_ours() || !previous.is_in_progress())
            })
            .map(|previous| previous.temp_path);
        PreviousMeta {
            json,
            superseded_copy,
        }
    });
    fs.write_file(meta_path, json.as_bytes())?;
    Ok(previous)
}

/// The recovery metadata a [`StagedCopy`] replaced.
struct PreviousMeta {
    /// Its content, to put back if the new copy is discarded.
    json: Vec<u8>,
    /// The earlier staged copy it pointed at, if the new one may supersede
    /// it (see [`write_inplace_recovery_meta`]).
    superseded_copy: Option<PathBuf>,
}

/// A complete copy of what an in-place write is about to put in a file,
/// staged in the recovery directory, with the recovery metadata pointing at
/// it (when that could be written).
///
/// The file may already be torn by an earlier attempt, with the metadata
/// pointing at *that* attempt's copy — then the only complete copy on disk.
/// So the earlier copy is removed only once the file has been opened for
/// writing ([`StagedCopy::supersede_previous`]), when the new copy is what
/// the file needs to be recovered to; if the file is never opened, the new
/// copy goes and the metadata is put back as it was.
struct StagedCopy<'a> {
    fs: &'a dyn FileSystem,
    temp_path: PathBuf,
    meta_path: PathBuf,
    /// `None` if the metadata couldn't be written, so it still describes
    /// whatever was staged before; otherwise what it held before.
    meta: Option<Option<PreviousMeta>>,
}

impl<'a> StagedCopy<'a> {
    /// Point the recovery metadata of `dest_path` at `temp_path`, a complete
    /// copy of the content about to be written to it.
    fn new(
        fs: &'a dyn FileSystem,
        recovery_dir: &Path,
        dest_path: &Path,
        temp_path: PathBuf,
        original_metadata: &Option<FileMetadata>,
    ) -> Self {
        let meta_path = InplaceWriteRecovery::meta_path(recovery_dir, dest_path);
        // Best effort - the staged copy alone is still worth having
        let meta =
            write_inplace_recovery_meta(fs, &meta_path, dest_path, &temp_path, original_metadata)
                .ok();
        Self {
            fs,
            temp_path,
            meta_path,
            meta,
        }
    }

    /// The file is open for writing, so about to be truncated: from here on
    /// this copy is the one to recover it to, and the one staged before it
    /// can go.
    fn supersede_previous(&mut self) {
        if let Some(Some(previous)) = self.meta.as_mut() {
            if let Some(copy) = previous.superseded_copy.take() {
                // Best-effort cleanup of a copy nothing points at any more
                #[allow(clippy::let_underscore_must_use)]
                let _ = self.fs.remove_file(&copy);
            }
        }
    }

    /// The write completed: neither the copy nor its metadata is needed.
    fn finish(self) {
        // Best-effort cleanup of files that are no longer needed
        #[allow(clippy::let_underscore_must_use)]
        let _ = self.fs.remove_file(&self.temp_path);
        if self.meta.is_some() {
            #[allow(clippy::let_underscore_must_use)]
            let _ = self.fs.remove_file(&self.meta_path);
        }
    }

    /// The file was never opened, so this attempt truncated nothing: remove
    /// the copy and put the metadata back as it was.
    fn discard(self) {
        let fs = self.fs;
        let temp_path = self.release();
        // Best-effort cleanup of a copy that is no longer needed
        #[allow(clippy::let_underscore_must_use)]
        let _ = fs.remove_file(&temp_path);
    }

    /// Like [`StagedCopy::discard`], but keep the copy and hand it over
    /// (to the sudo fallback, which removes it when done).
    fn release(self) -> PathBuf {
        // Best effort: at worst the metadata keeps pointing at this copy
        #[allow(clippy::let_underscore_must_use)]
        match self.meta {
            Some(Some(previous)) => {
                let _ = self.fs.write_file(&self.meta_path, &previous.json);
            }
            Some(None) => {
                let _ = self.fs.remove_file(&self.meta_path);
            }
            None => {}
        }
        self.temp_path
    }
}

/// `dest_path` was just written in full by other means (the sudo fallback),
/// so an in-place recovery left for it by an earlier, interrupted attempt is
/// resolved: remove its metadata and the staged copy it points at. Without
/// this the copy outlives the save (a file that needs sudo never gets the
/// non-sudo in-place write that would clear it), and every session start
/// warns about it. Entries of another running process are left alone.
///
/// `recovery_dir` is the one the save staged in (see [`save_local`]).
pub fn resolve_inplace_write_recovery(fs: &dyn FileSystem, recovery_dir: &Path, dest_path: &Path) {
    let meta_path = InplaceWriteRecovery::meta_path(recovery_dir, dest_path);
    let Ok(json) = fs.read_file(&meta_path) else {
        return;
    };
    let Ok(recovery) = serde_json::from_slice::<InplaceWriteRecovery>(&json) else {
        return;
    };
    if recovery.dest_path != dest_path || (!recovery.is_ours() && recovery.is_in_progress()) {
        return;
    }
    // Best-effort cleanup of files the completed save made obsolete
    if is_staged_copy(&recovery.temp_path, recovery_dir, dest_path) {
        #[allow(clippy::let_underscore_must_use)]
        let _ = fs.remove_file(&recovery.temp_path);
    }
    #[allow(clippy::let_underscore_must_use)]
    let _ = fs.remove_file(&meta_path);
}

/// The copies interrupted in-place writes kept in `recovery_dir` (see
/// [`clean_up_inplace_write_recoveries`]) that the user has yet to decide
/// about: a staged copy that is still there and differs from its file, of a
/// write no longer in progress. Oldest first.
pub fn kept_inplace_write_recoveries(
    fs: &dyn FileSystem,
    recovery_dir: &Path,
) -> Vec<InplaceWriteRecovery> {
    InplaceWriteRecovery::scan(fs, recovery_dir)
        .into_iter()
        .filter(|(meta_path, recovery)| {
            *meta_path == InplaceWriteRecovery::meta_path(recovery_dir, &recovery.dest_path)
        })
        .map(|(_, recovery)| recovery)
        .filter(|recovery| {
            !recovery.is_in_progress()
                && is_staged_copy(&recovery.temp_path, recovery_dir, &recovery.dest_path)
                && fs.exists(&recovery.temp_path)
                && !same_content(fs, &recovery.temp_path, &recovery.dest_path).unwrap_or(false)
        })
        .collect()
}

/// Finish the interrupted in-place write of `dest_path` whose copy
/// `recovery_dir` kept: overwrite the file in place with the copy, as that
/// write would have, then remove the copy and its metadata.
pub fn restore_inplace_write_recovery(
    fs: &dyn FileSystem,
    recovery_dir: &Path,
    dest_path: &Path,
) -> io::Result<()> {
    let meta_path = InplaceWriteRecovery::meta_path(recovery_dir, dest_path);
    let recovery = serde_json::from_slice::<InplaceWriteRecovery>(&fs.read_file(&meta_path)?)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;
    if recovery.dest_path != dest_path
        || !is_staged_copy(&recovery.temp_path, recovery_dir, dest_path)
    {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "the recovery metadata doesn't point at a copy staged for this file",
        ));
    }
    let mut out_file = fs.open_file_for_write(dest_path)?;
    stream_file_to_writer(fs, &recovery.temp_path, &mut out_file)?;
    out_file.sync_all()?;
    drop(out_file);
    resolve_inplace_write_recovery(fs, recovery_dir, dest_path);
    Ok(())
}

/// Clean up after in-place writes (see [`write_in_place_staged`]) whose
/// process died before it could, in `recovery_dir`, where they stage:
///
/// * a staged copy whose destination now holds exactly its content (the
///   write got that far, or the file was saved again since), with its
///   metadata;
/// * metadata whose staged copy is gone;
/// * the temp files of metadata writes a crash interrupted
///   (see [`crate::recovery_types::remove_orphaned_temp_files`]).
///
/// A staged copy that differs from its destination may be the only intact
/// copy of what was being saved, so it is kept, never aged out;
/// the editor offers it to the user (see
/// [`kept_inplace_write_recoveries`]), who decides what becomes of it.
/// Entries of a process still running are left alone. Returns how many files
/// were removed.
pub fn clean_up_inplace_write_recoveries(fs: &dyn FileSystem, recovery_dir: &Path) -> usize {
    let mut removed =
        crate::recovery_types::remove_orphaned_temp_files(fs, recovery_dir).unwrap_or(0);
    let mut remove = |path: &Path| match fs.remove_file(path) {
        Ok(()) => removed += 1,
        Err(e) => tracing::debug!("Failed to remove {}: {}", path.display(), e),
    };
    for (meta_path, recovery) in InplaceWriteRecovery::scan(fs, recovery_dir) {
        if recovery.is_in_progress() {
            continue;
        }
        if !fs.exists(&recovery.temp_path) {
            remove(&meta_path);
        } else if is_staged_copy(&recovery.temp_path, recovery_dir, &recovery.dest_path)
            && same_content(fs, &recovery.temp_path, &recovery.dest_path).unwrap_or(false)
        {
            remove(&recovery.temp_path);
            remove(&meta_path);
        } else {
            // Not a warning: the editor asks the user about it
            tracing::info!(
                "An interrupted save of {} left the content being saved in {}",
                recovery.dest_path.display(),
                recovery.temp_path.display()
            );
        }
    }
    removed
}

/// Whether two files hold the same bytes.
fn same_content(fs: &dyn FileSystem, a: &Path, b: &Path) -> io::Result<bool> {
    const CHUNK: u64 = 64 * 1024;
    let len = fs.metadata(a)?.size;
    if fs.metadata(b)?.size != len {
        return Ok(false);
    }
    let mut offset = 0;
    while offset < len {
        let n = (len - offset).min(CHUNK) as usize;
        if fs.read_range(a, offset, n)? != fs.read_range(b, offset, n)? {
            return Ok(false);
        }
        offset += n as u64;
    }
    Ok(true)
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
fn save_with_inplace_write(
    fs: &Arc<dyn FileSystem + Send + Sync>,
    dest_path: &Path,
    recipe: &WriteRecipe,
    recovery_dir: &Path,
) -> anyhow::Result<()> {
    let original_metadata = fs.metadata_if_exists(dest_path);

    // Optimization: if no Copy ops, we can write directly without a temp file
    // (same as the non-inplace path for small files)
    if !recipe.has_copy_ops() {
        let data = recipe.flatten_inserts();
        return write_data_inplace(fs, dest_path, &data, original_metadata, recovery_dir);
    }

    // Step 1: Write recipe to a temp file in the recovery directory (or
    // wherever it can be staged, see `create_staging_file`)
    // This reads Copy chunks from the original file (still intact) and writes to temp.
    // Using the recovery directory allows crash recovery if the operation fails.
    // Unlike a fully loaded buffer's, this content can't be written without
    // a staged copy: the Copy chunks come from the file being overwritten.
    let (temp_path, mut temp_file) =
        create_staging_file(&**fs, recovery_dir, dest_path).map_err(|e| {
            anyhow::anyhow!(
                "Can't save {} in place: it is written from a copy staged first, and {e}",
                dest_path.display()
            )
        })?;
    if let Err(e) = write_recipe(fs, &mut temp_file, recipe) {
        // Best-effort cleanup of temp file on write failure
        #[allow(clippy::let_underscore_must_use)]
        let _ = fs.remove_file(&temp_path);
        return Err(e.into());
    }
    temp_file.sync_all()?;
    drop(temp_file);

    // Step 1.5: Save recovery metadata before the dangerous step
    // If we crash during step 2, this metadata + temp file allows recovery
    let mut staged = StagedCopy::new(
        &**fs,
        recovery_dir,
        dest_path,
        temp_path,
        &original_metadata,
    );

    // Step 2: Stream temp file content to destination
    // Now it's safe to truncate the destination since all data is in temp
    match fs.open_file_for_write(dest_path) {
        Ok(mut out_file) => {
            staged.supersede_previous();
            // On failure from here on, keep the staged copy for recovery
            stream_file_to_writer(&**fs, &staged.temp_path, &mut out_file)?;
            out_file.sync_all()?;
            drop(out_file);
            staged.finish();
            Ok(())
        }
        Err(e) if e.kind() == io::ErrorKind::PermissionDenied => {
            // Can't write to destination - trigger sudo fallback with the
            // staged copy, which it removes when done
            let temp_file = SudoSaveTempFile::new(fs, staged.release());
            Err(make_sudo_error(temp_file, dest_path, original_metadata))
        }
        Err(e) => {
            // Nothing was truncated, so nothing needs recovering
            staged.discard();
            Err(e.into())
        }
    }
}

/// Write data directly to a file in-place, with sudo fallback when the file
/// itself can't be written (see [`write_in_place_staged`] for why a
/// recovery directory that can't be written doesn't count).
fn write_data_inplace(
    fs: &Arc<dyn FileSystem + Send + Sync>,
    dest_path: &Path,
    data: &[u8],
    original_metadata: Option<FileMetadata>,
    recovery_dir: &Path,
) -> anyhow::Result<()> {
    match write_in_place_staged(&**fs, recovery_dir, dest_path, data) {
        Ok(()) => Ok(()),
        Err(e) if e.kind() == io::ErrorKind::PermissionDenied => {
            Err(stage_for_sudo(fs, dest_path, data, original_metadata)?)
        }
        Err(e) => Err(e.into()),
    }
}

/// `dest_path` can't be written: put `data` in a temp file for the sudo
/// prompt and return the [`SudoSaveRequired`] error that hands it over.
fn stage_for_sudo(
    fs: &Arc<dyn FileSystem + Send + Sync>,
    dest_path: &Path,
    data: &[u8],
    original_metadata: Option<FileMetadata>,
) -> io::Result<anyhow::Error> {
    let (temp_file, mut writer) = create_temp_file(fs, dest_path)?;
    writer.write_all(data)?;
    writer.sync_all()?;
    drop(writer);
    Ok(make_sudo_error(temp_file, dest_path, original_metadata))
}

/// Overwrite `dest_path` in place with `data`, keeping its inode and with it
/// the owner, group, hard links, xattrs and ACLs.
///
/// Between truncating the file and finishing the write it holds neither the
/// old content nor the new, so `data` is first staged in the recovery
/// directory with [`crate::recovery_types::InplaceWriteRecovery`] metadata
/// pointing at it, as [`save_with_inplace_write`] does. The staged copy is
/// removed once the write succeeds (or if the file can't be opened, so
/// nothing was truncated) and kept if the write fails part-way. A copy an
/// earlier attempt staged is only superseded once the file is open — until
/// then it may be the only complete copy of a file that attempt tore (see
/// [`StagedCopy`]).
///
/// The copy goes in the recovery directory, or wherever else it can be
/// staged ([`create_staging_file`]). If it can't be staged anywhere, the
/// file is written in place without one, like before staging existed: the
/// error is about where copies go, not about the file. Only running out of
/// space stops the write, since the file is probably on the same full disk,
/// and truncating it then would lose its content with no copy of the new
/// one anywhere.
fn write_in_place_staged(
    fs: &dyn FileSystem,
    recovery_dir: &Path,
    dest_path: &Path,
    data: &[u8],
) -> io::Result<()> {
    let mut staged = match stage_in_place_write(fs, recovery_dir, dest_path, data) {
        Ok(staged) => Some(staged),
        Err(e) if is_out_of_space(&e) => return Err(e),
        Err(e) => {
            tracing::warn!(
                "Can't stage a copy of {} ({e}); writing it in place without one",
                dest_path.display()
            );
            None
        }
    };

    let mut out_file = match fs.open_file_for_write(dest_path) {
        Ok(file) => file,
        Err(e) => {
            if let Some(staged) = staged {
                staged.discard();
            }
            return Err(e);
        }
    };
    if let Some(staged) = staged.as_mut() {
        staged.supersede_previous();
    }
    // On failure from here on, keep the staged copy for recovery.
    out_file.write_all(data)?;
    out_file.sync_all()?;
    drop(out_file);
    if let Some(staged) = staged {
        staged.finish();
    }
    Ok(())
}

/// Stage `data` for [`write_in_place_staged`]: write it to a new file in the
/// recovery directory and (best effort) the metadata pointing at it.
fn stage_in_place_write<'a>(
    fs: &'a dyn FileSystem,
    recovery_dir: &Path,
    dest_path: &Path,
    data: &[u8],
) -> io::Result<StagedCopy<'a>> {
    let original_metadata = fs.metadata_if_exists(dest_path);
    let (temp_path, mut temp_file) = create_staging_file(fs, recovery_dir, dest_path)?;
    let staged = temp_file
        .write_all(data)
        .and_then(|()| temp_file.sync_all());
    drop(temp_file);
    if let Err(e) = staged {
        // Best-effort cleanup; the write error is what the caller needs
        #[allow(clippy::let_underscore_must_use)]
        let _ = fs.remove_file(&temp_path);
        return Err(e);
    }
    Ok(StagedCopy::new(
        fs,
        recovery_dir,
        dest_path,
        temp_path,
        &original_metadata,
    ))
}

/// Whether `e` means the disk (or the user's quota on it) is full.
fn is_out_of_space(e: &io::Error) -> bool {
    matches!(
        e.kind(),
        io::ErrorKind::StorageFull | io::ErrorKind::QuotaExceeded
    )
}

/// Stream a file's content to a writer in chunks to avoid memory issues with large files.
fn stream_file_to_writer(
    fs: &dyn FileSystem,
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

/// Write the recipe's content to `out` (a file, or a buffer).
fn write_recipe<W: Write + ?Sized>(
    fs: &Arc<dyn FileSystem + Send + Sync>,
    out: &mut W,
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
                out.write_all(&data)?;
            }
            RecipeAction::Insert { index } => {
                out.write_all(&recipe.insert_data[*index])?;
            }
        }
    }
    Ok(())
}

/// Internal helper to create a SudoSaveRequired error.
fn make_sudo_error(
    temp_file: SudoSaveTempFile,
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
        temp_file,
        dest_path: dest_path.to_path_buf(),
        uid,
        gid,
        mode,
    })
}
