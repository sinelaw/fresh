//! Deterministic batch worker/allocator regression; no timing assertions.
use fresh::model::filesystem::{
    DirEntry, FileMetadata, FilePermissions, FileReader, FileSearchCursor, FileSearchOptions,
    FileSystem, FileUpload, FileWriter, SearchMatch, StdFileSystem,
};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
type Writes = Arc<Mutex<Vec<(std::thread::ThreadId, usize)>>>;
struct RecordingImportFs {
    inner: StdFileSystem,
    writes: Writes,
    finished: Option<std::sync::mpsc::Sender<()>>,
}
struct RecordingUpload {
    inner: Box<dyn FileUpload>,
    writes: Writes,
    finished: Option<std::sync::mpsc::Sender<()>>,
}
impl Write for RecordingUpload {
    fn write(&mut self, data: &[u8]) -> io::Result<usize> {
        self.writes
            .lock()
            .unwrap()
            .push((std::thread::current().id(), data.as_ptr() as usize));
        self.inner.write(data)
    }
    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}
impl FileUpload for RecordingUpload {
    fn finish(self: Box<Self>) -> io::Result<()> {
        self.inner.finish()?;
        if let Some(finished) = self.finished {
            finished.send(()).unwrap();
        }
        Ok(())
    }
}
impl FileSystem for RecordingImportFs {
    fn read_file(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.inner.read_file(path)
    }
    fn read_range(&self, path: &Path, offset: u64, len: usize) -> io::Result<Vec<u8>> {
        self.inner.read_range(path, offset, len)
    }
    fn write_file(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        self.inner.write_file(path, data)
    }
    fn create_file(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.create_file(path)
    }
    fn open_file(&self, path: &Path) -> io::Result<Box<dyn FileReader>> {
        self.inner.open_file(path)
    }
    fn open_file_for_write(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.open_file_for_write(path)
    }
    fn open_file_for_append(&self, path: &Path) -> io::Result<Box<dyn FileWriter>> {
        self.inner.open_file_for_append(path)
    }
    fn set_file_length(&self, path: &Path, len: u64) -> io::Result<()> {
        self.inner.set_file_length(path, len)
    }
    fn rename(&self, from: &Path, to: &Path) -> io::Result<()> {
        self.inner.rename(from, to)
    }
    fn copy(&self, from: &Path, to: &Path) -> io::Result<u64> {
        self.inner.copy(from, to)
    }
    fn remove_file(&self, path: &Path) -> io::Result<()> {
        self.inner.remove_file(path)
    }
    fn remove_dir(&self, path: &Path) -> io::Result<()> {
        self.inner.remove_dir(path)
    }
    fn metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.inner.metadata(path)
    }
    fn symlink_metadata(&self, path: &Path) -> io::Result<FileMetadata> {
        self.inner.symlink_metadata(path)
    }
    fn is_dir(&self, path: &Path) -> io::Result<bool> {
        self.inner.is_dir(path)
    }
    fn is_file(&self, path: &Path) -> io::Result<bool> {
        self.inner.is_file(path)
    }
    fn set_permissions(&self, path: &Path, permissions: &FilePermissions) -> io::Result<()> {
        self.inner.set_permissions(path, permissions)
    }
    fn read_dir(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        self.inner.read_dir(path)
    }
    fn create_dir(&self, path: &Path) -> io::Result<()> {
        self.inner.create_dir(path)
    }
    fn create_dir_all(&self, path: &Path) -> io::Result<()> {
        self.inner.create_dir_all(path)
    }
    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        self.inner.canonicalize(path)
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
        self.inner.search_file(path, pattern, opts, cursor)
    }
    fn sudo_write(
        &self,
        path: &Path,
        data: &[u8],
        mode: u32,
        uid: u32,
        gid: u32,
    ) -> io::Result<()> {
        self.inner.sudo_write(path, data, mode, uid, gid)
    }
    fn walk(
        &self,
        root: &Path,
        opts: &fresh_editor_core::model::filesystem::WalkOptions<'_>,
        cancel: &std::sync::atomic::AtomicBool,
        on_entry: &mut dyn FnMut(fresh_editor_core::model::filesystem::WalkEntry<'_>) -> bool,
    ) -> std::io::Result<()> {
        self.inner.walk(root, opts, cancel, on_entry)
    }

    fn create_file_for_upload(&self, path: &Path) -> io::Result<Box<dyn FileUpload>> {
        Ok(Box::new(RecordingUpload {
            inner: self.inner.create_file_for_upload(path)?,
            writes: self.writes.clone(),
            finished: self.finished.clone(),
        }))
    }
    fn publish_file(&self, from: &Path, to: &Path, overwrite: bool) -> io::Result<()> {
        self.inner.publish_file(from, to, overwrite)
    }
}

#[test]
fn optimized_import_batch_reuses_worker_and_copy_buffer() {
    use crate::common::harness::{EditorTestHarness, HarnessOptions};
    use crossterm::event::{KeyCode, KeyModifiers};
    let writes = Writes::default();
    let fs = Arc::new(RecordingImportFs {
        inner: StdFileSystem,
        writes: writes.clone(),
        finished: None,
    });
    let project = tempfile::tempdir().unwrap();
    let sources = tempfile::tempdir().unwrap();
    let mut paths = Vec::new();
    for i in 0..12 {
        let source = sources.path().join(format!("file-{i}.bin"));
        std::fs::write(&source, vec![i; 64 * 1024]).unwrap();
        paths.push(format!("\"{}\"", source.display()));
    }
    let mut h = EditorTestHarness::create(
        180,
        32,
        HarnessOptions::new()
            .with_working_dir(project.path().to_path_buf())
            .with_filesystem(fs),
    )
    .unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("File Explorer").unwrap();
    h.send_paste(&paths.join(" ")).unwrap();
    h.wait_for_screen_contains("Imported 12 files; skipped 0")
        .unwrap();
    let writes = writes.lock().unwrap();
    assert_eq!(writes.len(), 12);
    let threads: std::collections::HashSet<_> = writes.iter().map(|(thread, _)| *thread).collect();
    assert_eq!(threads.len(), 1, "one transfer thread for the batch");
    let buffers: std::collections::HashSet<_> = writes.iter().map(|(_, buffer)| *buffer).collect();
    assert_eq!(buffers.len(), 1, "reuse the same bounded copy buffer");
}

#[test]
fn import_worker_transfers_a_batch_without_per_file_ui_acknowledgements() {
    use fresh::services::file_import::batch::{ImportEvent, ImportWorker};
    let root = tempfile::tempdir().unwrap();
    let sources = tempfile::tempdir().unwrap();
    let (finished, completions) = std::sync::mpsc::channel();
    let fs = Arc::new(RecordingImportFs {
        inner: StdFileSystem,
        writes: Writes::default(),
        finished: Some(finished),
    });
    let mut pending = std::collections::VecDeque::new();
    for i in 0..12 {
        let source = sources.path().join(format!("file-{i}"));
        std::fs::write(&source, b"DATA").unwrap();
        pending.push_back((source, root.path().join(format!("file-{i}"))));
    }
    let worker = ImportWorker::start(Arc::new(StdFileSystem), fs, pending).unwrap();
    // No editor tick, progress poll, or completion acknowledgement is needed
    // for the next file to run. Wait on actual filesystem completions instead.
    for _ in 0..12 {
        completions.recv().unwrap();
    }
    let mut count = 0;
    loop {
        match worker.events.recv().unwrap() {
            ImportEvent::Imported { .. } => count += 1,
            ImportEvent::Finished(result) => {
                result.unwrap();
                break;
            }
            event => panic!("unexpected {event:?}"),
        }
    }
    assert_eq!(count, 12);
    for i in 0..12 {
        assert_eq!(
            std::fs::read(root.path().join(format!("file-{i}"))).unwrap(),
            b"DATA"
        );
    }
}

#[test]
fn import_worker_cancellation_wakes_a_pending_conflict() {
    use fresh::services::file_import::batch::{ImportEvent, ImportWorker};
    let root = tempfile::tempdir().unwrap();
    let source = root.path().join("source");
    let destination = root.path().join("destination");
    std::fs::write(&source, b"NEW").unwrap();
    std::fs::write(&destination, b"KEEP").unwrap();
    let worker = ImportWorker::start(
        Arc::new(StdFileSystem),
        Arc::new(StdFileSystem),
        [(source, destination.clone())].into(),
    )
    .unwrap();
    assert!(matches!(
        worker.events.recv().unwrap(),
        ImportEvent::Conflict(_)
    ));
    worker.cancel();
    match worker.events.recv().unwrap() {
        ImportEvent::Finished(Err(error)) => assert_eq!(error.kind(), io::ErrorKind::Interrupted),
        event => panic!("unexpected {event:?}"),
    }
    assert_eq!(std::fs::read(&destination).unwrap(), b"KEEP");
    assert_eq!(std::fs::read_dir(root.path()).unwrap().count(), 2);
}
