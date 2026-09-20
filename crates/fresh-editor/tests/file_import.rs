//! Exercise the same local-read / destination-write path with a real local
//! filesystem and the production Python agent, including atomic publication.
use fresh::model::filesystem::{FileSystem, StdFileSystem};
use fresh::services::file_import::import_file;
use fresh::services::remote::{spawn_local_agent, RemoteFileSystem};
use std::io;
use std::sync::atomic::{AtomicBool, Ordering};

fn exercise_import(destination_fs: &dyn FileSystem) {
    let source_dir = tempfile::tempdir().unwrap();
    let destination_dir = tempfile::tempdir().unwrap();
    let source = source_dir.path().join("한글 image.bin");
    let destination = destination_dir.path().join("한글 image.bin");
    // Cross several chunk boundaries, including arbitrary binary bytes.
    let data: Vec<u8> = (0..800_003).map(|i| (i % 251) as u8).collect();
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
