//! Exercise the same local-read / destination-write path with a real local
//! filesystem and the production Python agent, including atomic publication.
use fresh::model::filesystem::{FileSystem, StdFileSystem};
use fresh::services::file_import::{import_entry, import_file, ImportOutcome};
use fresh::services::remote::{spawn_local_agent, RemoteFileSystem};
use std::io;
use std::sync::atomic::{AtomicBool, Ordering};

fn exercise_import(destination_fs: &dyn FileSystem) {
    let source_dir = tempfile::tempdir().unwrap();
    let destination_dir = tempfile::tempdir().unwrap();
    let source = source_dir.path().join("한글 image.bin");
    let destination = destination_dir.path().join("한글 image.bin");
    // Cross several chunk boundaries, including arbitrary binary bytes.
    let data: Vec<u8> = (0..3 * 1024 * 1024 + 3).map(|i| (i % 251) as u8).collect();
    std::fs::write(&source, &data).unwrap();
    let cancel = AtomicBool::new(false);
    let mut progress = Vec::new();
    import_file(
        &StdFileSystem,
        destination_fs,
        &source,
        &destination,
        false,
        &cancel,
        |bytes, total| progress.push((bytes, total)),
    )
    .unwrap();
    assert_eq!(destination_fs.read_file(&destination).unwrap(), data);
    assert_eq!(
        std::fs::read(&source).unwrap(),
        data,
        "import preserves source"
    );
    assert!(progress.len() > 3);
    assert_eq!(
        progress.last(),
        Some(&(data.len() as u64, data.len() as u64))
    );
    assert_eq!(
        std::fs::read_dir(destination_dir.path()).unwrap().count(),
        1
    );

    // A conflict never silently overwrites an existing file.
    destination_fs
        .write_file(&destination, b"original")
        .unwrap();
    let error = import_file(
        &StdFileSystem,
        destination_fs,
        &source,
        &destination,
        false,
        &cancel,
        |_, _| {},
    )
    .unwrap_err();
    assert_eq!(error.kind(), io::ErrorKind::AlreadyExists);
    assert_eq!(destination_fs.read_file(&destination).unwrap(), b"original");

    // Cancel after the first chunk, while overwriting: old content is intact
    // and neither staging file nor directory remains. No timing assumptions.
    let error = import_file(
        &StdFileSystem,
        destination_fs,
        &source,
        &destination,
        true,
        &cancel,
        |bytes, _| {
            if bytes > 0 {
                cancel.store(true, Ordering::Release);
            }
        },
    )
    .unwrap_err();
    assert_eq!(error.kind(), io::ErrorKind::Interrupted);
    assert_eq!(destination_fs.read_file(&destination).unwrap(), b"original");
    assert_eq!(
        std::fs::read_dir(destination_dir.path()).unwrap().count(),
        1
    );

    cancel.store(false, Ordering::Release);
    import_file(
        &StdFileSystem,
        destination_fs,
        &source,
        &destination,
        true,
        &cancel,
        |_, _| {},
    )
    .unwrap();
    assert_eq!(destination_fs.read_file(&destination).unwrap(), data);

    // Another process creates a destination during the transfer. Publication
    // must fail atomically even though the initial conflict check passed.
    let raced = destination_dir.path().join("raced.bin");
    let mut raced_once = false;
    let error = import_file(
        &StdFileSystem,
        destination_fs,
        &source,
        &raced,
        false,
        &cancel,
        |bytes, _| {
            if bytes > 0 && !raced_once {
                destination_fs
                    .write_file(&raced, b"concurrent writer")
                    .unwrap();
                raced_once = true;
            }
        },
    )
    .unwrap_err();
    assert_eq!(error.kind(), io::ErrorKind::AlreadyExists);
    assert_eq!(
        destination_fs.read_file(&raced).unwrap(),
        b"concurrent writer"
    );
    assert_eq!(
        std::fs::read_dir(destination_dir.path()).unwrap().count(),
        2
    );

    // Empty files are still published.
    std::fs::write(&source, []).unwrap();
    let empty = destination_dir.path().join("empty");
    import_file(
        &StdFileSystem,
        destination_fs,
        &source,
        &empty,
        false,
        &cancel,
        |_, _| {},
    )
    .unwrap();
    assert_eq!(destination_fs.read_file(&empty).unwrap(), b"");

    // Directories and missing inputs produce errors, not empty destination files.
    let invalid = destination_dir.path().join("invalid");
    assert!(import_file(
        &StdFileSystem,
        destination_fs,
        source_dir.path(),
        &invalid,
        false,
        &cancel,
        |_, _| {}
    )
    .is_err());
    assert!(import_file(
        &StdFileSystem,
        destination_fs,
        &source_dir.path().join("missing"),
        &invalid,
        false,
        &cancel,
        |_, _| {}
    )
    .is_err());
    assert!(!destination_fs.exists(&invalid));
}

#[test]
fn local_import_preserves_bytes_conflicts_and_cancellation() {
    exercise_import(&StdFileSystem);
}

#[test]
fn remote_import_preserves_bytes_conflicts_and_cancellation() {
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let channel = runtime
        .block_on(spawn_local_agent())
        .expect("Python agent must start");
    channel.set_request_timeout(std::time::Duration::from_secs(60 * 60));
    let remote = RemoteFileSystem::new(channel, "test@localhost".into());
    exercise_import(&remote);
}

#[test]
fn cannot_import_a_file_over_itself() {
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("source");
    std::fs::write(&file, b"preserve me").unwrap();
    // A same-directory drop still offers a conflict so the user can rename.
    assert_eq!(
        import_file(
            &StdFileSystem,
            &StdFileSystem,
            &file,
            &file,
            false,
            &AtomicBool::new(false),
            |_, _| {}
        )
        .unwrap_err()
        .kind(),
        io::ErrorKind::AlreadyExists
    );
    assert_eq!(
        import_file(
            &StdFileSystem,
            &StdFileSystem,
            &file,
            &file,
            true,
            &AtomicBool::new(false),
            |_, _| {}
        )
        .unwrap_err()
        .kind(),
        io::ErrorKind::InvalidInput
    );
    assert_eq!(std::fs::read(&file).unwrap(), b"preserve me");
}

#[cfg(unix)]
#[test]
fn destination_symlinks_are_not_followed() {
    for remote in [false, true] {
        let runtime = tokio::runtime::Runtime::new().unwrap();
        let fs: Box<dyn FileSystem> = if remote {
            let channel = runtime.block_on(spawn_local_agent()).unwrap();
            Box::new(RemoteFileSystem::new(channel, "test@localhost".into()))
        } else {
            Box::new(StdFileSystem)
        };
        let dir = tempfile::tempdir().unwrap();
        let source = dir.path().join("source");
        let target = dir.path().join("target");
        let link = dir.path().join("link");
        std::fs::write(&source, b"imported").unwrap();
        std::fs::write(&target, b"untouched").unwrap();
        std::os::unix::fs::symlink(&target, &link).unwrap();
        let cancel = AtomicBool::new(false);
        assert_eq!(
            import_file(
                &StdFileSystem,
                fs.as_ref(),
                &source,
                &link,
                false,
                &cancel,
                |_, _| {}
            )
            .unwrap_err()
            .kind(),
            io::ErrorKind::AlreadyExists
        );
        import_file(
            &StdFileSystem,
            fs.as_ref(),
            &source,
            &link,
            true,
            &cancel,
            |_, _| {},
        )
        .unwrap();
        assert_eq!(std::fs::read(&target).unwrap(), b"untouched");
        assert_eq!(std::fs::read(&link).unwrap(), b"imported");
        assert!(!std::fs::symlink_metadata(&link).unwrap().is_symlink());
    }
}

#[test]
fn local_import_opens_destination_once() {
    use fresh::services::fs::{SlowFileSystem, SlowFsConfig};
    use std::sync::Arc;
    let fs = SlowFileSystem::new(Arc::new(StdFileSystem), SlowFsConfig::none());
    let dir = tempfile::tempdir().unwrap();
    let source = dir.path().join("source");
    let destination = dir.path().join("destination");
    let data = vec![42; 4 * 1024 * 1024 + 17];
    std::fs::write(&source, &data).unwrap();
    import_file(
        &StdFileSystem,
        &fs,
        &source,
        &destination,
        false,
        &AtomicBool::new(false),
        |_, _| {},
    )
    .unwrap();
    assert_eq!(std::fs::read(destination).unwrap(), data);
    assert_eq!(
        fs.metrics().write_file_calls.load(Ordering::SeqCst),
        1,
        "one destination writer for the whole transfer"
    );
}

// Instrument the real agent, keeping performance assertions independent of
// wall-clock speed. These hooks exist only in this subprocess, never production.
async fn instrumented_agent() -> (
    std::sync::Arc<fresh::services::remote::AgentChannel>,
    tokio::process::Child,
) {
    use tokio::io::{AsyncBufReadExt, BufReader};
    let script = format!(
        "scope = {{'__name__': 'test_agent'}}\nexec({}, scope)\n{}",
        serde_json::to_string(include_str!("../src/services/remote/agent.py")).unwrap(),
        r#"
counts = {'syncs': 0, 'data_requests': 0, 'opens': 0, 'requests': 0}
faults = {}
listing_started = scope['threading'].Event()
listing_release = scope['threading'].Event()
original_sync = scope['os'].fsync
def counted_sync(fd):
    counts['syncs'] += 1
    if faults.get('sync'):
        raise OSError('injected upload sync failure')
    return original_sync(fd)
scope['os'].fsync = counted_sync
import builtins
original_open = builtins.open
def counted_open(*args, **kwargs):
    counts['opens'] += 1
    return original_open(*args, **kwargs)
scope['open'] = counted_open
for name in ('append', 'upload_chunk'):
    if name in scope['METHODS']:
        def wrap(handler):
            def counted(id, params):
                counts['data_requests'] += 1
                return handler(id, params)
            return counted
        scope['METHODS'][name] = wrap(scope['METHODS'][name])
original_ls = scope['METHODS']['ls']
def gated_ls(id, params):
    if faults.get('block_ls'):
        listing_started.set()
        listing_release.wait()
    return original_ls(id, params)
scope['METHODS']['ls'] = gated_ls
def stats(id, params):
    faults.update(params)
    if params.get('release_ls'):
        listing_release.set()
    result = dict(counts)
    result['listing_started'] = listing_started.is_set()
    result['uploads_open'] = len(scope.get('uploads', {}))
    scope['send'](id, r=result)
for name, handler in list(scope['METHODS'].items()):
    def track(handler):
        def call(id, params):
            counts['requests'] += 1
            return handler(id, params)
        return call
    scope['METHODS'][name] = track(handler)
scope['METHODS']['test_stats'] = stats
scope['main']()
"#
    );
    let mut child = tokio::process::Command::new("python3")
        .args(["-u", "-c", &script])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .kill_on_drop(true)
        .spawn()
        .unwrap();
    let mut reader = BufReader::new(child.stdout.take().unwrap());
    let mut ready = String::new();
    reader.read_line(&mut ready).await.unwrap();
    assert!(
        serde_json::from_str::<serde_json::Value>(&ready).unwrap()["ok"]
            .as_bool()
            .unwrap()
    );
    let channel = std::sync::Arc::new(fresh::services::remote::AgentChannel::new(
        reader,
        child.stdin.take().unwrap(),
    ));
    channel.set_request_timeout(std::time::Duration::from_secs(60 * 60));
    (channel, child)
}

#[test]
fn remote_import_batches_requests_and_syncs_once() {
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let (channel, _agent) = runtime.block_on(instrumented_agent());
    let remote = RemoteFileSystem::new(channel.clone(), "test@localhost".into());
    let dir = tempfile::tempdir().unwrap();
    let source = dir.path().join("source");
    let destination = dir.path().join("destination");
    let data: Vec<u8> = (0..4 * 1024 * 1024 + 17).map(|i| (i % 251) as u8).collect();
    std::fs::write(&source, &data).unwrap();
    import_file(
        &StdFileSystem,
        &remote,
        &source,
        &destination,
        false,
        &AtomicBool::new(false),
        |_, _| {},
    )
    .unwrap();
    assert_eq!(std::fs::read(destination).unwrap(), data);
    let stats = channel
        .request_blocking("test_stats", serde_json::json!({}))
        .unwrap();
    assert_eq!(stats["syncs"], 1, "sync only before publishing");
    assert_eq!(stats["opens"], 1, "keep the agent file handle open");
    assert!(stats["data_requests"].as_u64().unwrap() <= 5, "{stats}");
}

#[test]
fn import_batch_lists_destination_once() {
    use crate::common::harness::{EditorTestHarness, HarnessOptions};
    use crossterm::event::{KeyCode, KeyModifiers};
    use fresh::services::fs::{SlowFileSystem, SlowFsConfig};
    use std::sync::Arc;
    let fs = Arc::new(SlowFileSystem::new(
        Arc::new(StdFileSystem),
        SlowFsConfig::none(),
    ));
    let project = tempfile::tempdir().unwrap();
    let sources = tempfile::tempdir().unwrap();
    let mut h = EditorTestHarness::create(
        180,
        32,
        HarnessOptions::new()
            .with_working_dir(project.path().to_path_buf())
            .with_filesystem(fs.clone()),
    )
    .unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("File Explorer").unwrap();
    let mut paths = Vec::new();
    for i in 0..12 {
        let path = sources.path().join(format!("file-{i}.txt"));
        std::fs::write(&path, format!("IMPORTED_{i}")).unwrap();
        paths.push(format!("\"{}\"", path.display()));
    }
    fs.reset_metrics();
    h.send_paste(&paths.join(" ")).unwrap();
    h.wait_for_screen_contains("Imported 12 files; skipped 0")
        .unwrap();
    h.assert_screen_contains("file-11.txt");
    assert_eq!(
        fs.metrics().read_dir_calls.load(Ordering::SeqCst),
        1,
        "refresh the batch once, not after each file"
    );
}

#[test]
fn remote_import_sync_failure_preserves_destination_and_closes_upload() {
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let (channel, _agent) = runtime.block_on(instrumented_agent());
    let remote = RemoteFileSystem::new(channel.clone(), "test@localhost".into());
    let dir = tempfile::tempdir().unwrap();
    let source = dir.path().join("source");
    let destination = dir.path().join("destination");
    std::fs::write(&source, vec![42; 2 * 1024 * 1024 + 3]).unwrap();
    std::fs::write(&destination, "KEEP_ORIGINAL").unwrap();
    channel
        .request_blocking("test_stats", serde_json::json!({"sync": true}))
        .unwrap();
    let error = import_file(
        &StdFileSystem,
        &remote,
        &source,
        &destination,
        true,
        &AtomicBool::new(false),
        |_, _| {},
    )
    .unwrap_err();
    assert!(error.to_string().contains("injected upload sync failure"));
    assert_eq!(std::fs::read(&destination).unwrap(), b"KEEP_ORIGINAL");
    assert_eq!(std::fs::read_dir(dir.path()).unwrap().count(), 2);
    let stats = channel
        .request_blocking("test_stats", serde_json::json!({}))
        .unwrap();
    assert_eq!(stats["uploads_open"], 0);
}

#[test]
fn remote_import_directory_refresh_does_not_block_cancel_input() {
    use crate::common::harness::{EditorTestHarness, HarnessOptions};
    use crossterm::event::{KeyCode, KeyModifiers};
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let (channel, _agent) = runtime.block_on(instrumented_agent());
    let remote = std::sync::Arc::new(RemoteFileSystem::new(
        channel.clone(),
        "test@localhost".into(),
    ));
    let project = tempfile::tempdir().unwrap();
    let sources = tempfile::tempdir().unwrap();
    let source = sources.path().join("refresh.txt");
    std::fs::write(&source, "REFRESHED_CONTENT").unwrap();
    let mut h = EditorTestHarness::create(
        180,
        32,
        HarnessOptions::new()
            .with_working_dir(project.path().to_path_buf())
            .with_filesystem(remote),
    )
    .unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("[localhost]").unwrap();
    channel
        .request_blocking("test_stats", serde_json::json!({"block_ls": true}))
        .unwrap();
    h.send_paste(&format!("\"{}\"", source.display())).unwrap();
    h.wait_until(|_| {
        channel
            .request_blocking("test_stats", serde_json::json!({}))
            .unwrap()["listing_started"]
            == true
    })
    .unwrap();
    // The listing remains blocked until the test releases it. A synchronous
    // UI refresh cannot reach this key event; no wall-clock assertion is needed.
    h.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    h.assert_screen_contains("Paste cancelled");
    channel
        .request_blocking("test_stats", serde_json::json!({"release_ls": true}))
        .unwrap();
    h.wait_for_screen_contains("Imported 1 files; skipped 0")
        .unwrap();
    h.assert_screen_contains("refresh.txt");
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_screen_contains("REFRESHED_CONTENT").unwrap();
}

fn exercise_directory_import(filesystem: &dyn FileSystem) {
    let sources = tempfile::tempdir().unwrap();
    let destinations = tempfile::tempdir().unwrap();
    let source = sources.path().join("folder");
    let destination = destinations.path().join("folder");
    std::fs::create_dir_all(source.join("nested/empty")).unwrap();
    let bytes: Vec<u8> = (0..1024 * 1024 + 7).map(|i| (i % 251) as u8).collect();
    std::fs::write(source.join("nested/data.bin"), &bytes).unwrap();
    std::fs::write(source.join(".hidden"), "HIDDEN").unwrap();
    std::fs::create_dir(&destination).unwrap();
    std::fs::write(destination.join("keep.txt"), "KEEP").unwrap();
    let cancel = AtomicBool::new(false);
    let mut pending = vec![(source.clone(), destination.clone())];
    while let Some((source, destination)) = pending.pop() {
        match import_entry(
            &StdFileSystem,
            filesystem,
            &source,
            &destination,
            false,
            &cancel,
            |_, _| {},
        )
        .unwrap()
        {
            ImportOutcome::File => {}
            ImportOutcome::Directory { children, skipped } => {
                assert_eq!(skipped, 0);
                for child in children {
                    let destination = destination.join(child.file_name().unwrap());
                    pending.push((child, destination));
                }
            }
        }
    }
    assert_eq!(
        filesystem
            .read_file(&destination.join("nested/data.bin"))
            .unwrap(),
        bytes
    );
    assert_eq!(
        filesystem.read_file(&destination.join("keep.txt")).unwrap(),
        b"KEEP"
    );
    assert_eq!(
        filesystem.read_file(&destination.join(".hidden")).unwrap(),
        b"HIDDEN"
    );
    assert!(filesystem
        .is_dir(&destination.join("nested/empty"))
        .unwrap());
    assert_eq!(
        std::fs::read(source.join("nested/data.bin")).unwrap(),
        bytes
    );
    // Cancellation before directory work must not create the target.
    cancel.store(true, Ordering::Release);
    let cancelled = destinations.path().join("cancelled");
    assert_eq!(
        import_entry(
            &StdFileSystem,
            filesystem,
            &source,
            &cancelled,
            false,
            &cancel,
            |_, _| {}
        )
        .unwrap_err()
        .kind(),
        io::ErrorKind::Interrupted
    );
    assert!(!filesystem.exists(&cancelled));
    cancel.store(false, Ordering::Release);
    // A directory must never replace an existing file, even with overwrite.
    let collision = destinations.path().join("file");
    std::fs::write(&collision, "PRESERVE_FILE").unwrap();
    for (overwrite, kind) in [
        (false, io::ErrorKind::AlreadyExists),
        (true, io::ErrorKind::InvalidInput),
    ] {
        assert_eq!(
            import_entry(
                &StdFileSystem,
                filesystem,
                &source,
                &collision,
                overwrite,
                &cancel,
                |_, _| {}
            )
            .unwrap_err()
            .kind(),
            kind
        );
    }
    assert_eq!(filesystem.read_file(&collision).unwrap(), b"PRESERVE_FILE");
}

#[test]
fn directory_import_preserves_tree_and_merges_locally_and_through_agent() {
    exercise_directory_import(&StdFileSystem);
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let channel = runtime.block_on(spawn_local_agent()).unwrap();
    channel.set_request_timeout(std::time::Duration::from_secs(60 * 60));
    exercise_directory_import(&RemoteFileSystem::new(channel, "test@localhost".into()));
}

#[test]
fn directory_import_rejects_self_and_descendants_before_creating_them() {
    let root = tempfile::tempdir().unwrap();
    let source = root.path().join("folder");
    std::fs::create_dir_all(source.join("child")).unwrap();
    for destination in [&source, &source.join("copy"), &source.join("child/copy")] {
        assert!(import_entry(
            &StdFileSystem,
            &StdFileSystem,
            &source,
            destination,
            true,
            &AtomicBool::new(false),
            |_, _| {}
        )
        .is_err());
    }
    assert!(!source.join("copy").exists());
    assert!(!source.join("child/copy").exists());
}

#[cfg(unix)]
#[test]
fn directory_import_skips_source_links_and_refuses_destination_links() {
    let root = tempfile::tempdir().unwrap();
    let source = root.path().join("source");
    let outside = root.path().join("outside");
    std::fs::create_dir(&source).unwrap();
    std::fs::create_dir(&outside).unwrap();
    std::fs::write(source.join("data"), "DATA").unwrap();
    std::os::unix::fs::symlink(&source, source.join("loop")).unwrap();
    std::os::unix::fs::symlink(source.join("data"), source.join("file-link")).unwrap();
    let linked_source = root.path().join("linked-source");
    std::os::unix::fs::symlink(&source, &linked_source).unwrap();
    let linked_destination = root.path().join("linked-destination");
    std::os::unix::fs::symlink(&outside, &linked_destination).unwrap();
    let dangling = root.path().join("dangling");
    let missing_target = root.path().join("must-not-create");
    std::os::unix::fs::symlink(&missing_target, &dangling).unwrap();
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let channel = runtime.block_on(spawn_local_agent()).unwrap();
    let remote = RemoteFileSystem::new(channel, "test@localhost".into());
    for filesystem in [&StdFileSystem as &dyn FileSystem, &remote] {
        assert!(filesystem.is_symlink(&dangling).unwrap());
        assert!(filesystem.is_symlink(&linked_source.join("")).unwrap());
        assert!(import_entry(
            &StdFileSystem,
            filesystem,
            &source,
            &dangling,
            false,
            &AtomicBool::new(false),
            |_, _| {}
        )
        .is_err());
        assert!(!missing_target.exists());
        let target = root.path().join("target");
        let outcome = import_entry(
            &StdFileSystem,
            filesystem,
            &source,
            &target,
            false,
            &AtomicBool::new(false),
            |_, _| {},
        )
        .unwrap();
        match outcome {
            ImportOutcome::Directory { children, skipped } => {
                assert_eq!(skipped, 2);
                assert_eq!(children, vec![source.join("data")]);
            }
            ImportOutcome::File => panic!("expected directory"),
        }
        assert!(import_entry(
            &StdFileSystem,
            filesystem,
            &linked_source,
            &root.path().join("no-link-copy"),
            false,
            &AtomicBool::new(false),
            |_, _| {}
        )
        .is_err());
        assert!(import_entry(
            &StdFileSystem,
            filesystem,
            &source,
            &linked_destination,
            true,
            &AtomicBool::new(false),
            |_, _| {}
        )
        .is_err());
        assert_eq!(std::fs::read_dir(&outside).unwrap().count(), 0);
    }
    // A symlinked ancestor must not bypass the self-copy guard.
    let alias = root.path().join("alias");
    std::os::unix::fs::symlink(&source, &alias).unwrap();
    assert!(import_entry(
        &StdFileSystem,
        &StdFileSystem,
        &source,
        &alias.join("copy"),
        false,
        &AtomicBool::new(false),
        |_, _| {}
    )
    .is_err());
    assert!(!source.join("copy").exists());
}

#[test]
fn optimized_remote_import_uses_two_control_requests() {
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let (channel, _agent) = runtime.block_on(instrumented_agent());
    let remote = RemoteFileSystem::new(channel.clone(), "test@localhost".into());
    let root = tempfile::tempdir().unwrap();
    let source = root.path().join("source");
    std::fs::write(&source, b"SMALL_FILE").unwrap();
    import_file(
        &StdFileSystem,
        &remote,
        &source,
        &root.path().join("destination"),
        false,
        &AtomicBool::new(false),
        |_, _| {},
    )
    .unwrap();
    let stats = channel
        .request_blocking("test_stats", serde_json::json!({}))
        .unwrap();
    assert_eq!(
        stats["requests"], 3,
        "prepare, one data chunk, commit+cleanup: {stats}"
    );
    assert_eq!(stats["syncs"], 1);
    assert_eq!(stats["uploads_open"], 0);
    assert_eq!(std::fs::read_dir(root.path()).unwrap().count(), 2);
}

#[test]
fn optimized_folder_import_does_not_load_collapsed_descendants() {
    use crate::common::harness::{EditorTestHarness, HarnessOptions};
    use crossterm::event::{KeyCode, KeyModifiers};
    use fresh::services::fs::{SlowFileSystem, SlowFsConfig};
    use std::sync::Arc;
    let fs = Arc::new(SlowFileSystem::new(
        Arc::new(StdFileSystem),
        SlowFsConfig::none(),
    ));
    let project = tempfile::tempdir().unwrap();
    let sources = tempfile::tempdir().unwrap();
    let source = sources.path().join("bundle");
    for i in 0..32 {
        std::fs::create_dir_all(source.join(format!("child-{i}"))).unwrap();
        std::fs::write(source.join(format!("child-{i}/data.txt")), "DATA").unwrap();
    }
    let mut h = EditorTestHarness::create(
        180,
        32,
        HarnessOptions::new()
            .with_working_dir(project.path().to_path_buf())
            .with_filesystem(fs.clone()),
    )
    .unwrap();
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_screen_contains("File Explorer").unwrap();
    let initial_nodes = h.editor().file_explorer().unwrap().tree().node_count();
    fs.reset_metrics();
    h.send_paste(&format!("\"{}\"", source.display())).unwrap();
    h.wait_for_screen_contains("Imported 32 files and 33 folders; skipped 0")
        .unwrap();
    assert_eq!(
        fs.metrics().read_dir_calls.load(Ordering::SeqCst),
        1,
        "only refresh the visible destination"
    );
    assert_eq!(
        h.editor().file_explorer().unwrap().tree().node_count(),
        initial_nodes + 1,
        "only add the collapsed imported folder"
    );
    for i in 0..32 {
        assert_eq!(
            std::fs::read(project.path().join(format!("bundle/child-{i}/data.txt"))).unwrap(),
            b"DATA"
        );
    }
}

#[test]
fn remote_import_transaction_drop_removes_staging_without_publishing() {
    use std::io::Write;
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let (channel, _agent) = runtime.block_on(instrumented_agent());
    let remote = RemoteFileSystem::new(channel.clone(), "test@localhost".into());
    let root = tempfile::tempdir().unwrap();
    let destination = root.path().join("destination");
    std::fs::write(&destination, b"KEEP").unwrap();
    let mut transaction = remote
        .begin_file_import(&destination, true)
        .unwrap()
        .unwrap();
    transaction.write_all(b"PARTIAL_DATA").unwrap();
    drop(transaction);
    let stats = channel
        .request_blocking("test_stats", serde_json::json!({}))
        .unwrap();
    assert_eq!(stats["uploads_open"], 0);
    assert_eq!(stats["syncs"], 0);
    assert_eq!(std::fs::read(&destination).unwrap(), b"KEEP");
    assert_eq!(std::fs::read_dir(root.path()).unwrap().count(), 1);
    assert!(remote.begin_file_import(&destination, false).is_err());
    assert_eq!(std::fs::read_dir(root.path()).unwrap().count(), 1);
}
