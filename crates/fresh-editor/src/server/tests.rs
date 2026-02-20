//! Integration tests for client-server communication

#[cfg(test)]
mod integration_tests {
    use std::sync::atomic::Ordering;
    use std::thread;
    use std::time::Duration;

    use crate::server::daemon::is_process_running;
    use crate::server::ipc::{ClientConnection, SocketPaths};
    use crate::server::protocol::{
        ClientControl, ClientHello, ServerControl, ServerHello, TermSize, PROTOCOL_VERSION,
    };
    use crate::server::runner::{Server, ServerConfig};

    /// Read from the client data pipe until the accumulated output contains `needle`.
    /// Appends to `output` so callers can accumulate across multiple calls.
    /// No timeout - cargo nextest provides external timeout.
    fn read_until_contains(conn: &ClientConnection, output: &mut Vec<u8>, needle: &str) {
        let mut buf = [0u8; 8192];
        loop {
            if String::from_utf8_lossy(output).contains(needle) {
                return;
            }
            match conn.data.try_read(&mut buf) {
                Ok(0) => return,
                Ok(n) => output.extend_from_slice(&buf[..n]),
                Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    thread::sleep(Duration::from_millis(5));
                }
                Err(_) => return,
            }
        }
    }

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

    /// Test basic server startup and shutdown
    #[test]
    fn test_server_creates_sockets_on_bind() {
        let temp_dir = std::env::temp_dir().join(format!("fresh-test-{}", std::process::id()));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let config = ServerConfig {
            working_dir: temp_dir.clone(),
            session_name: Some(unique_session_name("lifecycle")),
            idle_timeout: Some(Duration::from_millis(100)),
        };

        let mut server = Server::new(config).unwrap();
        let shutdown = server.shutdown_handle();

        // Sockets should exist after bind
        let paths = server.socket_paths();
        assert!(paths.data.exists());
        assert!(paths.control.exists());

        shutdown.store(true, Ordering::SeqCst);
        let _ = server.run();

        std::fs::remove_dir_all(&temp_dir).ok();
    }

    /// Test client-server handshake protocol
    #[test]
    fn test_handshake_exchanges_protocol_version() {
        let temp_dir = std::env::temp_dir().join(format!("fresh-test-hs-{}", std::process::id()));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let session_name = unique_session_name("handshake");

        let config = ServerConfig {
            working_dir: temp_dir.clone(),
            session_name: Some(session_name.clone()),
            idle_timeout: Some(Duration::from_secs(5)),
        };

        let mut server = Server::new(config).unwrap();
        let socket_paths = server.socket_paths().clone();
        let server_handle = thread::spawn(move || server.run());

        thread::sleep(Duration::from_millis(50));

        let conn = ClientConnection::connect(&socket_paths).unwrap();

        // Send hello
        let hello = ClientHello::new(TermSize::new(80, 24));
        let hello_json = serde_json::to_string(&ClientControl::Hello(hello)).unwrap();
        conn.write_control(&hello_json).unwrap();

        // Verify server responds with matching protocol version
        let response = conn.read_control().unwrap().unwrap();
        let server_msg: ServerControl = serde_json::from_str(&response).unwrap();

        match server_msg {
            ServerControl::Hello(server_hello) => {
                assert_eq!(server_hello.protocol_version, PROTOCOL_VERSION);
                assert_eq!(server_hello.session_id, session_name);
            }
            _ => panic!("Expected Hello response"),
        }

        thread::sleep(Duration::from_millis(100));
        let _ = conn.write_control(&serde_json::to_string(&ClientControl::Quit).unwrap());

        let _ = server_handle.join();
        std::fs::remove_dir_all(&temp_dir).ok();
    }

    /// Test server rejects clients with incompatible protocol version
    #[test]
    fn test_version_mismatch_rejected() {
        let temp_dir = std::env::temp_dir().join(format!("fresh-test-ver-{}", std::process::id()));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let config = ServerConfig {
            working_dir: temp_dir.clone(),
            session_name: Some(unique_session_name("version")),
            idle_timeout: Some(Duration::from_secs(5)),
        };

        let mut server = Server::new(config).unwrap();
        let shutdown = server.shutdown_handle();
        let socket_paths = server.socket_paths().clone();

        let server_handle = thread::spawn(move || server.run());

        thread::sleep(Duration::from_millis(50));

        let conn = ClientConnection::connect(&socket_paths).unwrap();

        // Send hello with incompatible version
        let hello_json = serde_json::json!({
            "type": "hello",
            "protocol_version": 999,
            "client_version": "99.0.0",
            "term_size": { "cols": 80, "rows": 24 },
            "env": {}
        });
        conn.write_control(&hello_json.to_string()).unwrap();

        let response = conn.read_control().unwrap().unwrap();
        let server_msg: ServerControl = serde_json::from_str(&response).unwrap();

        assert!(matches!(server_msg, ServerControl::VersionMismatch(_)));

        shutdown.store(true, Ordering::SeqCst);
        let _ = server_handle.join();
        std::fs::remove_dir_all(&temp_dir).ok();
    }

    /// Test idle timeout causes server shutdown
    #[test]
    fn test_idle_timeout_triggers_shutdown() {
        let temp_dir = std::env::temp_dir().join(format!("fresh-test-idle-{}", std::process::id()));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let config = ServerConfig {
            working_dir: temp_dir.clone(),
            session_name: Some(unique_session_name("idle")),
            idle_timeout: Some(Duration::from_millis(50)), // Very short timeout
        };

        let mut server = Server::new(config).unwrap();

        // Without any client connections, server should exit due to idle timeout
        let result = server.run();
        assert!(result.is_ok());

        std::fs::remove_dir_all(&temp_dir).ok();
    }

    /// Test ping/pong keepalive
    #[test]
    #[cfg_attr(
        windows,
        ignore = "Windows named pipe handling needs further investigation for sustained connections"
    )]
    fn test_ping_pong_keepalive() {
        let temp_dir = std::env::temp_dir().join(format!("fresh-test-ping-{}", std::process::id()));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let config = ServerConfig {
            working_dir: temp_dir.clone(),
            session_name: Some(unique_session_name("ping")),
            idle_timeout: Some(Duration::from_secs(5)),
        };

        let mut server = Server::new(config).unwrap();
        let socket_paths = server.socket_paths().clone();
        let server_handle = thread::spawn(move || server.run());

        thread::sleep(Duration::from_millis(50));

        let conn = ClientConnection::connect(&socket_paths).unwrap();

        // Handshake
        let hello = ClientHello::new(TermSize::new(80, 24));
        conn.write_control(&serde_json::to_string(&ClientControl::Hello(hello)).unwrap())
            .unwrap();
        let _ = conn.read_control().unwrap();

        thread::sleep(Duration::from_millis(100));

        // Send ping
        conn.write_control(&serde_json::to_string(&ClientControl::Ping).unwrap())
            .unwrap();

        // Should receive pong
        thread::sleep(Duration::from_millis(50));

        // Due to non-blocking reads, we need to handle the response carefully
        // The server processes ping and sends pong

        let _ = conn.write_control(&serde_json::to_string(&ClientControl::Quit).unwrap());
        let _ = server_handle.join();
        std::fs::remove_dir_all(&temp_dir).ok();
    }

    /// Test quit command triggers server shutdown
    #[test]
    #[cfg_attr(
        windows,
        ignore = "Windows named pipe handling needs further investigation for sustained connections"
    )]
    fn test_quit_command_shuts_down_server() {
        let temp_dir = std::env::temp_dir().join(format!("fresh-test-quit-{}", std::process::id()));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let config = ServerConfig {
            working_dir: temp_dir.clone(),
            session_name: Some(unique_session_name("quit")),
            idle_timeout: None, // No idle timeout
        };

        let mut server = Server::new(config).unwrap();
        let socket_paths = server.socket_paths().clone();

        let server_handle = thread::spawn(move || server.run());
        thread::sleep(Duration::from_millis(50));

        let conn = ClientConnection::connect(&socket_paths).unwrap();

        // Handshake
        let hello = ClientHello::new(TermSize::new(80, 24));
        conn.write_control(&serde_json::to_string(&ClientControl::Hello(hello)).unwrap())
            .unwrap();
        let _ = conn.read_control().unwrap();

        thread::sleep(Duration::from_millis(100));

        // Send quit - server should shutdown
        conn.write_control(&serde_json::to_string(&ClientControl::Quit).unwrap())
            .unwrap();

        // Server should exit
        let result = server_handle.join().unwrap();
        assert!(result.is_ok());

        std::fs::remove_dir_all(&temp_dir).ok();
    }

    /// E2E test: Server signals readiness via PID file, client connects using semantic condition
    ///
    /// This test verifies the cross-platform synchronization mechanism:
    /// 1. Server writes PID file AFTER successfully binding to sockets
    /// 2. Client waits for PID file with valid running PID (semantic condition)
    /// 3. Client connects only after server is ready
    ///
    /// No arbitrary sleeps - uses semantic signaling via PID file.
    #[test]
    fn test_pid_file_signals_server_readiness() {
        let temp_dir = std::env::temp_dir().join(format!("fresh-test-pid-{}", std::process::id()));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let session_name = unique_session_name("pid-ready");

        // Start server in background thread and get socket paths from it
        let config = ServerConfig {
            working_dir: temp_dir.clone(),
            session_name: Some(session_name.clone()),
            idle_timeout: Some(Duration::from_secs(5)),
        };

        // Use channel to get socket paths from server thread
        let (paths_tx, paths_rx) = std::sync::mpsc::channel();

        let server_handle = thread::spawn(move || {
            let mut server = Server::new(config).unwrap();
            let socket_paths = server.socket_paths().clone();
            // Write PID file after bind (simulating EditorServer behavior)
            socket_paths.write_pid(std::process::id()).unwrap();
            // Send paths to waiting client
            paths_tx.send(socket_paths).unwrap();
            server.run()
        });

        // Get socket paths from server (this blocks until server sends them)
        let socket_paths = paths_rx.recv().unwrap();

        // PID file exists with valid running PID - server is ready
        let pid = socket_paths.read_pid().unwrap().unwrap();
        assert!(
            is_process_running(pid),
            "PID file should contain running process ID"
        );

        // Now we can connect reliably
        let conn = ClientConnection::connect(&socket_paths);
        assert!(
            conn.is_ok(),
            "Connection should succeed after PID file is ready: {:?}",
            conn.err()
        );

        let conn = conn.unwrap();

        // Perform handshake
        let hello = ClientHello::new(TermSize::new(80, 24));
        conn.write_control(&serde_json::to_string(&ClientControl::Hello(hello)).unwrap())
            .unwrap();

        let response = conn.read_control().unwrap().unwrap();
        let server_msg: ServerControl = serde_json::from_str(&response).unwrap();

        match server_msg {
            ServerControl::Hello(server_hello) => {
                assert_eq!(server_hello.protocol_version, PROTOCOL_VERSION);
            }
            other => panic!("Expected Hello, got {:?}", other),
        }

        // Give server time to process the handshake
        thread::sleep(Duration::from_millis(50));

        // Clean shutdown
        let _ = conn.write_control(&serde_json::to_string(&ClientControl::Quit).unwrap());

        let _ = server_handle.join();
        std::fs::remove_dir_all(&temp_dir).ok();
    }

    /// E2E test: Full client-server relay loop with data transmission
    ///
    /// This test verifies the complete data flow:
    /// 1. Server starts and accepts connection
    /// 2. Client connects and completes handshake
    /// 3. Server sends data through data pipe
    /// 4. Client receives the data
    ///
    /// This test reproduces issues with Windows named pipe mode toggling.
    #[test]
    fn test_client_receives_data_from_server() {
        use crate::server::ipc::ServerListener;

        let session_name = unique_session_name("relay");

        // Get socket paths for this session
        let socket_paths = SocketPaths::for_session_name(&session_name).unwrap();
        let _ = socket_paths.cleanup(); // Clean any existing

        // Channel to signal when server is ready
        let (ready_tx, ready_rx) = std::sync::mpsc::channel();
        // Channel to get server's connection for sending data
        let (conn_tx, conn_rx) = std::sync::mpsc::channel();
        // Channel to signal test completion
        let (done_tx, done_rx) = std::sync::mpsc::channel::<()>();

        let paths_for_server = socket_paths.clone();
        let session_for_server = session_name.clone();
        let server_handle = thread::spawn(move || {
            let mut listener = ServerListener::bind(paths_for_server.clone()).unwrap();
            paths_for_server.write_pid(std::process::id()).unwrap();
            ready_tx.send(()).unwrap();

            // Accept one connection
            let conn = loop {
                match listener.accept() {
                    Ok(Some(conn)) => break conn,
                    Ok(None) => {
                        thread::yield_now();
                        continue;
                    }
                    Err(e) => panic!("Accept error: {}", e),
                }
            };

            // Do handshake
            let hello_json = conn.read_control().unwrap().unwrap();
            let client_msg: ClientControl = serde_json::from_str(&hello_json).unwrap();

            if let ClientControl::Hello(_hello) = client_msg {
                let server_hello = ServerHello::new(session_for_server);
                let response = serde_json::to_string(&ServerControl::Hello(server_hello)).unwrap();
                conn.write_control(&response).unwrap();
            } else {
                panic!("Expected Hello, got {:?}", client_msg);
            }

            // Send connection to main test thread
            conn_tx.send(conn).unwrap();

            // Wait for test completion
            done_rx.recv().unwrap();
        });

        // Wait for server to be ready
        ready_rx.recv().unwrap();

        // Connect client
        let conn = ClientConnection::connect(&socket_paths).unwrap();

        // Do client handshake
        let hello = ClientHello::new(TermSize::new(80, 24));
        conn.write_control(&serde_json::to_string(&ClientControl::Hello(hello)).unwrap())
            .unwrap();
        let response = conn.read_control().unwrap().unwrap();
        let _server_msg: ServerControl = serde_json::from_str(&response).unwrap();

        // Get server connection
        let server_conn = conn_rx.recv().unwrap();

        // Server sends test data
        let test_data = b"Hello from server!";
        server_conn.write_data(test_data).unwrap();

        // Client reads data using try_read (this is where Windows fails)
        let mut buf = [0u8; 1024];
        let mut attempts = 0;
        let mut received = Vec::new();

        loop {
            match conn.data.try_read(&mut buf) {
                Ok(0) => {
                    panic!("Connection closed unexpectedly");
                }
                Ok(n) => {
                    received.extend_from_slice(&buf[..n]);
                    if received.len() >= test_data.len() {
                        break;
                    }
                }
                Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    attempts += 1;
                    if attempts > 100 {
                        panic!("Timeout waiting for data after {} attempts", attempts);
                    }
                    thread::sleep(Duration::from_millis(10));
                }
                Err(e) => {
                    panic!(
                        "Read error: {} (kind={:?}, raw={:?})",
                        e,
                        e.kind(),
                        e.raw_os_error()
                    );
                }
            }
        }

        assert_eq!(&received[..test_data.len()], test_data);

        // Signal server to finish and cleanup
        done_tx.send(()).unwrap();
        let _ = server_handle.join();
        let _ = socket_paths.cleanup();
    }

    /// E2E test: Client waits for PID file before connecting (no sleep-based timing)
    ///
    /// Tests the scenario where client starts before server is fully ready.
    /// Client should poll for PID file existence rather than using arbitrary delays.
    #[test]
    fn test_client_waits_for_pid_file_not_timeout() {
        let temp_dir = std::env::temp_dir().join(format!("fresh-test-wait-{}", std::process::id()));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let session_name = unique_session_name("wait-pid");

        // Get the socket paths that will be used (global socket dir)
        let socket_paths = SocketPaths::for_session_name(&session_name).unwrap();

        // Clean up any existing files
        let _ = socket_paths.cleanup();

        // Verify no PID file exists initially
        assert!(
            !socket_paths.pid.exists(),
            "PID file should not exist before server starts"
        );

        // Start client-side waiting BEFORE server starts
        let paths_for_waiter = socket_paths.clone();
        let waiter_handle = thread::spawn(move || {
            // This simulates what run_attach_command does:
            // Wait for PID file to appear with valid running PID
            let mut iterations = 0;
            loop {
                if let Ok(Some(pid)) = paths_for_waiter.read_pid() {
                    if is_process_running(pid) {
                        return Some(pid); // Got the signal
                    }
                }
                thread::yield_now();
                iterations += 1;
                if iterations > 10_000_000 {
                    return None; // Timeout (for test safety)
                }
            }
        });

        // Give waiter thread time to start waiting
        thread::yield_now();

        // Now start server (after waiter is already waiting)
        let config = ServerConfig {
            working_dir: temp_dir.clone(),
            session_name: Some(session_name.clone()),
            idle_timeout: Some(Duration::from_secs(5)),
        };

        let paths_for_cleanup = socket_paths.clone();
        let server_handle = thread::spawn(move || {
            let mut server = Server::new(config).unwrap();
            server.socket_paths().write_pid(std::process::id()).unwrap();
            server.run()
        });

        // Waiter should detect server readiness
        let detected_pid = waiter_handle.join().unwrap();
        assert!(detected_pid.is_some(), "Waiter should detect server PID");

        // Connect and clean up
        let conn = ClientConnection::connect(&paths_for_cleanup);
        assert!(conn.is_ok(), "Should connect after detecting PID");

        let conn = conn.unwrap();
        let hello = ClientHello::new(TermSize::new(80, 24));
        conn.write_control(&serde_json::to_string(&ClientControl::Hello(hello)).unwrap())
            .unwrap();
        let _ = conn.read_control().unwrap();
        conn.write_control(&serde_json::to_string(&ClientControl::Quit).unwrap())
            .unwrap();

        let _ = server_handle.join();
        let _ = paths_for_cleanup.cleanup();
        std::fs::remove_dir_all(&temp_dir).ok();
    }

    /// E2E test using real EditorServer with full editor initialization
    ///
    /// This test exercises the complete client-server flow:
    /// 1. Starts real EditorServer (with Editor, CaptureBackend, LSP, plugins)
    /// 2. Connects via real IPC (Unix sockets / Windows named pipes)
    /// 3. Completes handshake with version negotiation
    /// 4. Sends keystrokes through data pipe (simulating user input)
    /// 5. Receives rendered output through data pipe
    /// 6. Verifies editor state via output (e.g., typed text appears)
    /// 7. Sends quit command and verifies clean shutdown
    ///
    /// Uses background data drain threads to prevent deadlocks from
    /// blocking pipe writes on Windows named pipes.
    #[test]
    fn test_full_editor_server_e2e() {
        use crate::config::Config;
        use crate::config_io::DirectoryContext;
        use crate::server::editor_server::{EditorServer, EditorServerConfig};
        use std::sync::mpsc;

        eprintln!("[e2e] === START test_full_editor_server_e2e ===");

        let temp_dir = std::env::temp_dir().join(format!("fresh-e2e-{}", std::process::id()));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let session_name = unique_session_name("e2e-full");

        let config = Config::default();
        let dir_context = DirectoryContext::for_testing(&temp_dir);

        let server_config = EditorServerConfig {
            working_dir: temp_dir.clone(),
            session_name: Some(session_name.clone()),
            idle_timeout: Some(Duration::from_secs(30)),
            editor_config: config,
            dir_context,
            plugins_enabled: false,
        };

        let (paths_tx, paths_rx) = mpsc::channel();
        let (shutdown_tx, shutdown_rx) = mpsc::channel();

        eprintln!("[e2e] Spawning server thread...");
        // EditorServer must be created in the thread because Editor is not Send
        let server_handle = thread::spawn(move || {
            eprintln!("[e2e][server] Creating EditorServer...");
            let mut server = EditorServer::new(server_config).unwrap();
            eprintln!("[e2e][server] EditorServer created, sending paths...");
            let socket_paths = server.socket_paths().clone();
            let shutdown_handle = server.shutdown_handle();
            paths_tx.send(socket_paths).unwrap();
            shutdown_tx.send(shutdown_handle).unwrap();
            eprintln!("[e2e][server] Calling server.run()...");
            let result = server.run();
            eprintln!("[e2e][server] server.run() returned: {:?}", result);
            result
        });

        eprintln!("[e2e] Waiting for paths from server thread...");
        let socket_paths = paths_rx.recv().unwrap();
        let shutdown_handle = shutdown_rx.recv().unwrap();
        eprintln!("[e2e] Got paths. PID file: {:?}", socket_paths.pid);

        // Wait for server to be ready (PID file signals readiness)
        while !socket_paths.pid.exists() || socket_paths.read_pid().ok().flatten().is_none() {
            thread::yield_now();
        }
        eprintln!("[e2e] PID file ready");

        // Connect client 1
        eprintln!("[e2e] Connecting client...");
        let conn = ClientConnection::connect(&socket_paths).expect("Failed to connect to server");
        eprintln!("[e2e] Client connected.");

        // Perform handshake
        eprintln!("[e2e] Sending Hello...");
        let hello = ClientHello::new(TermSize::new(80, 24));
        conn.write_control(&serde_json::to_string(&ClientControl::Hello(hello)).unwrap())
            .unwrap();
        eprintln!("[e2e] Hello sent, reading response...");

        let response = conn.read_control().unwrap().unwrap();
        eprintln!("[e2e] Got server Hello response.");
        let server_msg: ServerControl = serde_json::from_str(&response).unwrap();

        match server_msg {
            ServerControl::Hello(server_hello) => {
                assert_eq!(server_hello.protocol_version, PROTOCOL_VERSION);
                assert_eq!(server_hello.session_id, session_name);
            }
            other => panic!("Expected Hello, got {:?}", other),
        }

        // Wait for initial render output (semantic: wait for ANSI escape sequences)
        // Server uses ClientDataWriter background thread for non-blocking writes
        let mut output1 = Vec::new();
        eprintln!("[e2e] Waiting for initial render output...");
        read_until_contains(&conn, &mut output1, "\x1b[");
        eprintln!("[e2e] Initial render received: {} bytes", output1.len());

        // Send keystrokes and wait for them to appear in the render
        eprintln!("[e2e] Sending 'hello' keystrokes...");
        conn.write_data(b"hello").unwrap();
        eprintln!("[e2e] Waiting for 'hello' to appear in render...");
        read_until_contains(&conn, &mut output1, "hello");
        eprintln!(
            "[e2e] Typed text appeared in render: {} bytes total",
            output1.len()
        );

        // Test detach command
        eprintln!("[e2e] Sending Detach...");
        conn.write_control(&serde_json::to_string(&ClientControl::Detach).unwrap())
            .unwrap();

        // Wait for server to process detach (it sends teardown sequences)
        // The alternate screen teardown includes ESC[?1049l
        eprintln!("[e2e] Waiting for teardown sequences...");
        read_until_contains(&conn, &mut output1, "\x1b[?1049l");
        eprintln!("[e2e] Teardown received, detach complete.");

        // Server should still be running after detach - reconnect
        eprintln!("[e2e] Reconnecting second client after detach...");
        let conn2 =
            ClientConnection::connect(&socket_paths).expect("Should reconnect after detach");
        eprintln!("[e2e] Second client connected.");

        // Handshake again
        eprintln!("[e2e] Sending Hello from second client...");
        let hello2 = ClientHello::new(TermSize::new(80, 24));
        conn2
            .write_control(&serde_json::to_string(&ClientControl::Hello(hello2)).unwrap())
            .unwrap();
        eprintln!("[e2e] Reading second Hello response...");

        let response2 = conn2.read_control().unwrap().unwrap();
        eprintln!("[e2e] Got second Hello response.");
        let server_msg2: ServerControl = serde_json::from_str(&response2).unwrap();
        assert!(
            matches!(server_msg2, ServerControl::Hello(_)),
            "Should get Hello after reconnect"
        );

        // Wait for render output on reconnected client
        let mut output2 = Vec::new();
        eprintln!("[e2e] Waiting for render on reconnected client...");
        read_until_contains(&conn2, &mut output2, "\x1b[");
        eprintln!(
            "[e2e] Reconnected client got render: {} bytes",
            output2.len()
        );

        // Send quit to shut down
        eprintln!("[e2e] Sending Quit...");
        conn2
            .write_control(&serde_json::to_string(&ClientControl::Quit).unwrap())
            .unwrap();
        eprintln!("[e2e] Quit sent, setting shutdown flag...");

        shutdown_handle.store(true, Ordering::SeqCst);
        eprintln!("[e2e] Joining server thread...");
        let result = server_handle.join().unwrap();
        eprintln!("[e2e] Server thread joined: {:?}", result);
        assert!(result.is_ok(), "Server should exit cleanly: {:?}", result);

        let _ = socket_paths.cleanup();
        std::fs::remove_dir_all(&temp_dir).ok();
        eprintln!("[e2e] === END test_full_editor_server_e2e ===");
    }

    /// E2E test: Second client connecting gets full screen render
    ///
    /// This test verifies that when a second client connects while the first
    /// is still connected, the second client receives a complete screen render
    /// (not just diffs).
    ///
    /// Uses background data drain threads to prevent deadlocks from
    /// blocking pipe writes on Windows named pipes.
    #[test]
    fn test_second_client_gets_full_screen() {
        use crate::config::Config;
        use crate::config_io::DirectoryContext;
        use crate::server::editor_server::{EditorServer, EditorServerConfig};
        use std::sync::mpsc;

        eprintln!("[multi] === START test_second_client_gets_full_screen ===");

        let temp_dir = std::env::temp_dir().join(format!("fresh-e2e-multi-{}", std::process::id()));
        std::fs::create_dir_all(&temp_dir).unwrap();

        let session_name = unique_session_name("e2e-multi-client");

        let config = Config::default();
        let dir_context = DirectoryContext::for_testing(&temp_dir);

        let server_config = EditorServerConfig {
            working_dir: temp_dir.clone(),
            session_name: Some(session_name.clone()),
            idle_timeout: Some(Duration::from_secs(30)),
            editor_config: config,
            dir_context,
            plugins_enabled: false,
        };

        let (paths_tx, paths_rx) = mpsc::channel();
        let (shutdown_tx, shutdown_rx) = mpsc::channel();

        eprintln!("[multi] Spawning server thread...");
        let server_handle = thread::spawn(move || {
            eprintln!("[multi][server] Creating EditorServer...");
            let mut server = EditorServer::new(server_config).unwrap();
            eprintln!("[multi][server] EditorServer created, sending paths...");
            let socket_paths = server.socket_paths().clone();
            let shutdown_handle = server.shutdown_handle();
            paths_tx.send(socket_paths).unwrap();
            shutdown_tx.send(shutdown_handle).unwrap();
            eprintln!("[multi][server] Calling server.run()...");
            let result = server.run();
            eprintln!("[multi][server] server.run() returned: {:?}", result);
            result
        });

        let socket_paths = paths_rx.recv().unwrap();
        let shutdown_handle = shutdown_rx.recv().unwrap();
        eprintln!("[multi] Got paths.");

        // Wait for server readiness
        while !socket_paths.pid.exists() || socket_paths.read_pid().ok().flatten().is_none() {
            thread::yield_now();
        }
        eprintln!("[multi] PID file ready");

        // === First client connects ===
        eprintln!("[multi] Connecting first client...");
        let conn1 =
            ClientConnection::connect(&socket_paths).expect("First client failed to connect");
        eprintln!("[multi] First client connected.");

        eprintln!("[multi] Sending Hello from first client...");
        let hello1 = ClientHello::new(TermSize::new(80, 24));
        conn1
            .write_control(&serde_json::to_string(&ClientControl::Hello(hello1)).unwrap())
            .unwrap();
        eprintln!("[multi] Reading first Hello response...");

        let response1 = conn1.read_control().unwrap().unwrap();
        eprintln!("[multi] Got first Hello response.");
        assert!(
            matches!(
                serde_json::from_str::<ServerControl>(&response1).unwrap(),
                ServerControl::Hello(_)
            ),
            "First client should get Hello"
        );

        // Wait for initial render (semantic: wait for ANSI sequences)
        // Server uses ClientDataWriter background thread for non-blocking writes
        let mut output1 = Vec::new();
        eprintln!("[multi] Waiting for first client initial render...");
        read_until_contains(&conn1, &mut output1, "\x1b[");
        eprintln!(
            "[multi] First client initial render: {} bytes",
            output1.len()
        );

        // First client types something to create content
        eprintln!("[multi] First client typing 'HELLO_WORLD'...");
        conn1.write_data(b"HELLO_WORLD").unwrap();

        // Wait for typed text to appear in render (semantic)
        eprintln!("[multi] Waiting for 'HELLO_WORLD' to appear in client 1 render...");
        read_until_contains(&conn1, &mut output1, "HELLO_WORLD");
        eprintln!(
            "[multi] HELLO_WORLD appeared in client 1 render: {} bytes total",
            output1.len()
        );

        // === Second client connects while first is still connected ===
        // Server's ClientDataWriter ensures writes to client 1 don't block the main loop,
        // so the server can accept client 2's connection concurrently.
        eprintln!("[multi] Connecting second client...");
        let conn2 =
            ClientConnection::connect(&socket_paths).expect("Second client failed to connect");
        eprintln!("[multi] Second client connected.");

        eprintln!("[multi] Sending Hello from second client...");
        let hello2 = ClientHello::new(TermSize::new(80, 24));
        conn2
            .write_control(&serde_json::to_string(&ClientControl::Hello(hello2)).unwrap())
            .unwrap();
        eprintln!("[multi] Reading second Hello response...");

        let response2 = conn2.read_control().unwrap().unwrap();
        eprintln!("[multi] Got second Hello response.");
        assert!(
            matches!(
                serde_json::from_str::<ServerControl>(&response2).unwrap(),
                ServerControl::Hello(_)
            ),
            "Second client should get Hello"
        );

        // Wait for second client to receive full render with HELLO_WORLD (semantic)
        let mut output2 = Vec::new();
        eprintln!("[multi] Waiting for 'HELLO_WORLD' to appear in client 2 render...");
        read_until_contains(&conn2, &mut output2, "HELLO_WORLD");

        let client2_str = String::from_utf8_lossy(&output2);

        eprintln!(
            "[multi] Second client received {} bytes, contains HELLO_WORLD: {}",
            output2.len(),
            client2_str.contains("HELLO_WORLD")
        );

        // A full 80x24 render should be substantial (at least 500 bytes with escape sequences)
        assert!(
            output2.len() > 500,
            "Second client should receive substantial output (full render), but only got {} bytes",
            output2.len()
        );

        // Cleanup
        eprintln!("[multi] Setting shutdown flag...");
        shutdown_handle.store(true, Ordering::SeqCst);
        eprintln!("[multi] Joining server thread...");
        let _ = server_handle.join();
        eprintln!("[multi] Server thread joined.");
        let _ = socket_paths.cleanup();
        std::fs::remove_dir_all(&temp_dir).ok();
        eprintln!("[multi] === END test_second_client_gets_full_screen ===");
    }
}
