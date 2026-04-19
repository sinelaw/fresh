// Integration tests that exercise the actual EditorServer code paths
// for session lifecycle.
//
// These tests call EditorServer::initialize_editor() directly — the same
// production method called when the first client connects to a session server.
// No threads, no IPC, no client reimplementation.
//
// #1233: initialize_editor() must restore workspace and recovery on restart.
// #1237: The server's file-opening code path (queue_file_open + process_pending_file_opens)
//        must work after initialize_editor() — this is the code that runs when
//        the client sends OpenFiles.

use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh::server::editor_server::{EditorServer, EditorServerConfig};
use tempfile::TempDir;

fn unique_session_name(prefix: &str) -> String {
    format!(
        "{}-{}-{}",
        prefix,
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    )
}

fn make_server_config(
    session_name: &str,
    working_dir: std::path::PathBuf,
    dir_context: DirectoryContext,
) -> EditorServerConfig {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    EditorServerConfig {
        working_dir,
        session_name: Some(session_name.to_string()),
        idle_timeout: None,
        editor_config: config,
        dir_context,
        plugins_enabled: false,
        init_enabled: false,
    }
}

// ---------------------------------------------------------------------------
// #1233: EditorServer::initialize_editor() restores session state
// ---------------------------------------------------------------------------

/// Session 1: initialize_editor → open file → shutdown (production path).
/// Session 2: initialize_editor → verify file was restored from workspace.
///
/// Before the fix, initialize_editor() only created the editor and set the
/// session name. It never called try_restore_workspace() or recover_all_buffers().
#[test]
fn test_editor_server_initialize_restores_workspace() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("hello.txt");
    std::fs::write(&file1, "original content").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());
    let session_name = unique_session_name("restore-ws");

    // --- Session 1: initialize, open file, shutdown ---
    {
        let config = make_server_config(&session_name, project_dir.clone(), dir_context.clone());
        let mut server = EditorServer::new(config).unwrap();
        server.initialize_editor().unwrap();

        let editor = server.editor_mut().unwrap();
        editor.queue_file_open(file1.clone(), None, None, None, None, None, None);
        editor.process_pending_file_opens();

        // Verify file is open
        let content = editor.active_state().buffer.to_string().unwrap_or_default();
        assert!(
            content.contains("original content"),
            "Session 1 failed to open file"
        );

        // Production shutdown path (same as EditorServer::run's exit block)
        editor.end_recovery_session().unwrap();
        editor.save_workspace().unwrap();
    }

    // --- Session 2: initialize should restore ---
    {
        let config = make_server_config(&session_name, project_dir.clone(), dir_context.clone());
        let mut server = EditorServer::new(config).unwrap();

        // This is the production code path that was broken:
        server.initialize_editor().unwrap();

        let editor = server.editor().unwrap();
        let content = editor.active_state().buffer.to_string().unwrap_or_default();
        assert!(
            content.contains("original content"),
            "Session 2: initialize_editor should have restored hello.txt, got: {:?}",
            content
        );
    }
}

/// Same as above but with an unnamed buffer — verifies hot exit recovery
/// restores unsaved content that was never on disk.
#[test]
fn test_editor_server_initialize_restores_unnamed_buffers() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());
    let session_name = unique_session_name("restore-unnamed");

    // --- Session 1: type into unnamed buffer, shutdown ---
    {
        let config = make_server_config(&session_name, project_dir.clone(), dir_context.clone());
        let mut server = EditorServer::new(config).unwrap();
        server.initialize_editor().unwrap();

        let editor = server.editor_mut().unwrap();
        editor
            .handle_key(
                crossterm::event::KeyCode::Char('s'),
                crossterm::event::KeyModifiers::NONE,
            )
            .unwrap();
        editor
            .handle_key(
                crossterm::event::KeyCode::Char('c'),
                crossterm::event::KeyModifiers::NONE,
            )
            .unwrap();
        editor
            .handle_key(
                crossterm::event::KeyCode::Char('r'),
                crossterm::event::KeyModifiers::NONE,
            )
            .unwrap();

        let content = editor.active_state().buffer.to_string().unwrap_or_default();
        assert!(content.contains("scr"), "Session 1 should have typed text");

        editor.end_recovery_session().unwrap();
        editor.save_workspace().unwrap();
    }

    // --- Session 2: should restore unnamed buffer ---
    {
        let config = make_server_config(&session_name, project_dir.clone(), dir_context.clone());
        let mut server = EditorServer::new(config).unwrap();
        server.initialize_editor().unwrap();

        let editor = server.editor().unwrap();
        let content = editor.active_state().buffer.to_string().unwrap_or_default();
        assert!(
            content.contains("scr"),
            "Session 2: should have restored unnamed buffer content, got: {:?}",
            content
        );
    }
}

// ---------------------------------------------------------------------------
// #1237: Server file-opening code path works after initialize_editor
// ---------------------------------------------------------------------------

/// After initialize_editor(), the server processes OpenFiles by calling
/// queue_file_open + process_pending_file_opens on the editor. Verify this
/// works — this is the same code path the server's OpenFiles handler uses.
#[test]
fn test_editor_server_opens_files_after_initialize() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("first.txt");
    let file2 = project_dir.join("second.txt");
    std::fs::write(&file1, "content of first").unwrap();
    std::fs::write(&file2, "content of second").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());
    let session_name = unique_session_name("openfiles");

    let config = make_server_config(&session_name, project_dir.clone(), dir_context);
    let mut server = EditorServer::new(config).unwrap();
    server.initialize_editor().unwrap();

    // This is exactly what EditorServer::process_clients does when it
    // receives a ClientControl::OpenFiles message:
    let editor = server.editor_mut().unwrap();
    editor.queue_file_open(file1.clone(), None, None, None, None, None, None);
    editor.queue_file_open(file2.clone(), None, None, None, None, None, None);
    editor.process_pending_file_opens();

    // The active buffer should be the last file opened
    let content = editor.active_state().buffer.to_string().unwrap_or_default();
    assert!(
        content.contains("content of second"),
        "Active buffer should be second.txt, got: {:?}",
        content
    );
}

/// After restoring a workspace, the server must be able to open additional
/// files via the OpenFiles path. This is the #1237 scenario: user runs
/// `fresh -a session file.txt`, server restores workspace, then opens file.txt.
#[test]
fn test_editor_server_opens_files_on_top_of_restored_workspace() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file1 = project_dir.join("existing.txt");
    let file2 = project_dir.join("new_file.txt");
    std::fs::write(&file1, "existing content").unwrap();
    std::fs::write(&file2, "new file content").unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());
    let session_name = unique_session_name("restore-plus-open");

    // --- Session 1: open existing.txt, shutdown ---
    {
        let config = make_server_config(&session_name, project_dir.clone(), dir_context.clone());
        let mut server = EditorServer::new(config).unwrap();
        server.initialize_editor().unwrap();

        let editor = server.editor_mut().unwrap();
        editor.queue_file_open(file1.clone(), None, None, None, None, None, None);
        editor.process_pending_file_opens();

        editor.end_recovery_session().unwrap();
        editor.save_workspace().unwrap();
    }

    // --- Session 2: initialize (restores existing.txt), then open new_file.txt ---
    {
        let config = make_server_config(&session_name, project_dir.clone(), dir_context.clone());
        let mut server = EditorServer::new(config).unwrap();
        server.initialize_editor().unwrap();

        // Verify workspace restored existing.txt
        let editor = server.editor_mut().unwrap();
        let content = editor.active_state().buffer.to_string().unwrap_or_default();
        assert!(
            content.contains("existing content"),
            "Workspace should have restored existing.txt, got: {:?}",
            content
        );

        // Now open new_file.txt on top (same as OpenFiles handler)
        editor.queue_file_open(file2.clone(), None, None, None, None, None, None);
        editor.process_pending_file_opens();

        // Active buffer should now be new_file.txt
        let content = editor.active_state().buffer.to_string().unwrap_or_default();
        assert!(
            content.contains("new file content"),
            "Active buffer should be new_file.txt, got: {:?}",
            content
        );
    }
}
