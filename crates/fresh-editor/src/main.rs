use anyhow::{Context, Result as AnyhowResult};
use clap::Parser;
use crossterm::event::{
    poll as event_poll, read as event_read, Event as CrosstermEvent, KeyEvent, KeyEventKind,
    MouseEvent,
};
use fresh::input::key_translator::KeyTranslator;
#[cfg(target_os = "linux")]
use fresh::services::gpm::{gpm_to_crossterm, GpmClient};
use fresh::services::terminal_modes::{self, KeyboardConfig, TerminalModes};
use fresh::services::tracing_setup;
use fresh::{
    app::Editor,
    client, config,
    config_io::DirectoryContext,
    model::filesystem::{FileSystem, StdFileSystem},
    server::SocketPaths,
    services::release_checker,
    services::remote,
    services::signal_handler,
    services::tracing_setup::TracingHandles,
    workspace,
};
use ratatui::Terminal;
use std::{
    io::{self, stdout},
    path::PathBuf,
    time::Duration,
};

/// Exit code when open-file command starts a new session.
/// Callers can check for this to spawn a terminal with an attached client.
const EXIT_NEW_SESSION: i32 = 2;

/// A terminal text editor with multi-cursor support
#[derive(Parser, Debug)]
#[command(name = "fresh")]
#[command(version, propagate_version = true)]
#[command(after_help = concat!(
    "Commands (use --cmd):\n",
    "  config show               Print effective configuration\n",
    "  config paths              Show directories used by Fresh\n",
    "  init                      Initialize a new plugin/theme/language\n",
    "\n",
    "Session commands:\n",
    "  session list              List active sessions\n",
    "  session attach [NAME]     Attach to a session (NAME or current dir)\n",
    "  session new NAME          Start a new named session\n",
    "  session kill [NAME]       Terminate a session\n",
    "  session open-file NAME FILES   Open files in session (starts if needed, exit 2 = new)\n",
    "\n",
    "File location syntax:\n",
    "  file.txt:10                  Open at line 10\n",
    "  file.txt:10:5                Open at line 10, column 5\n",
    "  file.txt:10-20               Select lines 10 to 20\n",
    "  file.txt:10:5-20:1           Select from line 10 col 5 to line 20 col 1\n",
    "  file.txt:10@\"msg\"            Open at line 10 with markdown popup message\n",
    "  file.txt:10-20@\"msg\"         Select range with markdown popup message\n",
    "  Tip: use single quotes to avoid shell expansion, e.g. 'file.txt:10@\"msg\"'\n",
    "\n",
    "Examples:\n",
    "  fresh file.txt                               Open a file\n",
    "  fresh 'file.txt:10-20@\"Check this code\"'     Open with range selected and popup\n",
    "  fresh -a                                     Attach to session (current dir)\n",
    "  fresh -a mysession                           Attach to named session\n",
    "  fresh --cmd session new proj                 Start session named 'proj'\n",
    "  fresh --cmd session open-file . main.rs     Open file in current dir session\n",
    "  fresh --cmd session open-file proj a.rs     Open file in 'proj' session\n",
    "\n",
    "Documentation: https://getfresh.dev/docs"
))]
struct Cli {
    /// Run a command instead of opening files
    /// Commands: session (list|attach|new|kill|open-file), config (show|paths), init
    #[arg(long, num_args = 1.., value_name = "COMMAND")]
    cmd: Vec<String>,

    /// Files to open (supports file:line:col, ranges, and @"message" syntax)
    #[arg(value_name = "FILES")]
    files: Vec<String>,

    /// Attach to session. Use -a for current dir, -a NAME for named session
    #[arg(short = 'a', long, value_name = "NAME", num_args = 0..=1, default_missing_value = "")]
    attach: Option<String>,

    /// Read content from stdin (alternative to using "-" as filename)
    #[arg(long)]
    stdin: bool,

    /// Disable plugin loading
    #[arg(long)]
    no_plugins: bool,

    /// Path to configuration file
    #[arg(long, value_name = "PATH")]
    config: Option<PathBuf>,

    /// Path to log file for editor diagnostics
    #[arg(long, value_name = "PATH")]
    log_file: Option<PathBuf>,

    /// Enable event logging to the specified file
    #[arg(long, value_name = "LOG_FILE")]
    event_log: Option<PathBuf>,

    /// Don't restore previous workspace
    #[arg(long, alias = "no-session")]
    no_restore: bool,

    /// Disable upgrade checking and anonymous telemetry
    #[arg(long)]
    no_upgrade_check: bool,

    /// Override the locale (e.g., 'en', 'ja', 'zh-CN')
    #[arg(long, value_name = "LOCALE")]
    locale: Option<String>,

    // === Hidden internal flags ===
    /// Start as a daemon server (internal)
    #[arg(long, hide = true)]
    server: bool,

    /// Session name for server mode (internal, used by spawn_server_detached)
    #[arg(long, hide = true, value_name = "NAME")]
    session_name: Option<String>,

    // === Deprecated flags from pre-subcommand CLI (hidden, with warnings) ===
    /// [deprecated: use `fresh config show`]
    #[arg(long, hide = true)]
    dump_config: bool,

    /// [deprecated: use `fresh config paths`]
    #[arg(long, hide = true)]
    show_paths: bool,

    /// Check a plugin (for debugging)
    #[arg(long, hide = true, value_name = "PLUGIN_PATH")]
    check_plugin: Option<PathBuf>,

    /// [deprecated: use `fresh init`]
    #[arg(long, hide = true, value_name = "TYPE")]
    init: Option<Option<String>>,

    /// Launch in GUI mode (native window with GPU rendering)
    #[cfg(feature = "gui")]
    #[arg(long)]
    gui: bool,
}

// Internal Args struct - maps from new Cli to format used by rest of codebase
#[derive(Debug)]
#[allow(dead_code)]
struct Args {
    files: Vec<String>,
    stdin: bool,
    no_plugins: bool,
    config: Option<PathBuf>,
    log_file: Option<PathBuf>,
    event_log: Option<PathBuf>,
    no_session: bool,
    no_upgrade_check: bool,
    dump_config: bool,
    show_paths: bool,
    locale: Option<String>,
    check_plugin: Option<PathBuf>,
    init: Option<Option<String>>,
    server: bool,
    // Session-related fields (set via subcommands or -a shortcut)
    attach: bool,
    list_sessions: bool,
    session_name: Option<String>,
    kill: Option<Option<String>>,
    /// Open files in a session without attaching (session_name, files)
    open_files_in_session: Option<(Option<String>, Vec<String>)>,
    /// Launch in GUI mode
    #[cfg(feature = "gui")]
    gui: bool,
}

impl From<Cli> for Args {
    fn from(cli: Cli) -> Self {
        // Parse --cmd arguments to determine command
        let (
            list_sessions,
            kill,
            attach,
            session_name,
            dump_config,
            show_paths,
            init,
            files,
            open_files_in_session,
        ) = if !cli.cmd.is_empty() {
            // Parse command from --cmd arguments
            let cmd_args: Vec<&str> = cli.cmd.iter().map(|s| s.as_str()).collect();
            match cmd_args.as_slice() {
                // Session commands
                ["session", "list", ..]
                | ["s", "list", ..]
                | ["session", "ls", ..]
                | ["s", "ls", ..] => (true, None, false, None, false, false, None, cli.files, None),
                // Open file in session: fresh --cmd session open-file <name> <files...>
                ["session", "open-file", name, files @ ..]
                | ["s", "open-file", name, files @ ..] => {
                    let session = if *name == "." {
                        None
                    } else {
                        Some((*name).to_string())
                    };
                    let file_list: Vec<String> = files.iter().map(|s| (*s).to_string()).collect();
                    (
                        false,
                        None,
                        false,
                        None,
                        false,
                        false,
                        None,
                        vec![],
                        Some((session, file_list)),
                    )
                }
                ["session", "attach", name, ..]
                | ["s", "attach", name, ..]
                | ["session", "a", name, ..]
                | ["s", "a", name, ..] => (
                    false,
                    None,
                    true,
                    Some((*name).to_string()),
                    false,
                    false,
                    None,
                    cli.files,
                    None,
                ),
                ["session", "attach"] | ["s", "attach"] | ["session", "a"] | ["s", "a"] => {
                    (false, None, true, None, false, false, None, cli.files, None)
                }
                ["session", "new", name, rest @ ..]
                | ["s", "new", name, rest @ ..]
                | ["session", "n", name, rest @ ..]
                | ["s", "n", name, rest @ ..] => {
                    let files: Vec<String> = rest.iter().map(|s| (*s).to_string()).collect();
                    (
                        false,
                        None,
                        true,
                        Some((*name).to_string()),
                        false,
                        false,
                        None,
                        files,
                        None,
                    )
                }
                ["session", "kill", "--all"]
                | ["s", "kill", "--all"]
                | ["session", "k", "--all"]
                | ["s", "k", "--all"] => (
                    false,
                    Some(Some("--all".to_string())),
                    false,
                    None,
                    false,
                    false,
                    None,
                    cli.files,
                    None,
                ),
                ["session", "kill", name, ..]
                | ["s", "kill", name, ..]
                | ["session", "k", name, ..]
                | ["s", "k", name, ..] => (
                    false,
                    Some(Some((*name).to_string())),
                    false,
                    None,
                    false,
                    false,
                    None,
                    cli.files,
                    None,
                ),
                ["session", "kill"] | ["s", "kill"] | ["session", "k"] | ["s", "k"] => (
                    false,
                    Some(None),
                    false,
                    None,
                    false,
                    false,
                    None,
                    cli.files,
                    None,
                ),
                ["session", "info", name, ..] | ["s", "info", name, ..] => {
                    // Info not fully implemented, treat as list for now
                    let _ = name;
                    (true, None, false, None, false, false, None, cli.files, None)
                }
                ["session", "info"] | ["s", "info"] => {
                    (true, None, false, None, false, false, None, cli.files, None)
                }
                // Config commands
                ["config", "show"] | ["config", "dump"] => {
                    (false, None, false, None, true, false, None, cli.files, None)
                }
                ["config", "paths"] => {
                    (false, None, false, None, false, true, None, cli.files, None)
                }
                // Init command
                ["init", pkg_type, ..] => (
                    false,
                    None,
                    false,
                    None,
                    false,
                    false,
                    Some(Some((*pkg_type).to_string())),
                    cli.files,
                    None,
                ),
                ["init"] => (
                    false,
                    None,
                    false,
                    None,
                    false,
                    false,
                    Some(None),
                    cli.files,
                    None,
                ),
                // Unknown command
                _ => {
                    eprintln!("Unknown command: {}", cli.cmd.join(" "));
                    eprintln!("Available commands: session (list|attach|new|kill|info|open-file), config (show|paths), init");
                    std::process::exit(1);
                }
            }
        } else {
            // No --cmd - check for -a shortcut and internal flags
            let attach = cli.attach.is_some();
            let session_name = if attach {
                let name = cli.attach.unwrap();
                if name.is_empty() || name == "." {
                    cli.session_name
                } else {
                    Some(name)
                }
            } else {
                // Use --session-name if provided (for internal --server use)
                cli.session_name
            };

            (
                false,
                None,
                attach,
                session_name,
                cli.dump_config,
                cli.show_paths,
                cli.init,
                cli.files,
                None,
            )
        };

        Args {
            files,
            stdin: cli.stdin,
            no_plugins: cli.no_plugins,
            config: cli.config,
            log_file: cli.log_file,
            event_log: cli.event_log,
            no_session: cli.no_restore,
            no_upgrade_check: cli.no_upgrade_check,
            dump_config,
            show_paths,
            locale: cli.locale,
            check_plugin: cli.check_plugin,
            init,
            server: cli.server,
            attach,
            list_sessions,
            session_name,
            kill,
            open_files_in_session,
            #[cfg(feature = "gui")]
            gui: cli.gui,
        }
    }
}

/// Parsed file location from CLI argument in file:line:col format
/// Also supports range selections (file:L-EL or file:L:C-EL:EC) and
/// hover messages (file:L@"message").
#[derive(Debug)]
struct FileLocation {
    path: PathBuf,
    line: Option<usize>,
    column: Option<usize>,
    end_line: Option<usize>,
    end_column: Option<usize>,
    message: Option<String>,
}

/// Parsed remote location from CLI argument in user@host:path format
#[derive(Debug, Clone)]
struct RemoteLocation {
    user: String,
    host: String,
    path: String,
    line: Option<usize>,
    column: Option<usize>,
}

/// Either a local or remote file location
#[derive(Debug)]
enum ParsedLocation {
    Local(FileLocation),
    Remote(RemoteLocation),
}

struct IterationOutcome {
    loop_result: AnyhowResult<()>,
    update_result: Option<release_checker::ReleaseCheckResult>,
    restart_dir: Option<PathBuf>,
}

struct SetupState {
    config: config::Config,
    tracing_handles: Option<TracingHandles>,
    terminal: Terminal<ratatui::backend::CrosstermBackend<io::Stdout>>,
    terminal_size: (u16, u16),
    file_locations: Vec<FileLocation>,
    show_file_explorer: bool,
    dir_context: DirectoryContext,
    current_working_dir: Option<PathBuf>,
    /// Stdin streaming state (if --stdin flag or "-" file was used)
    /// Contains temp file path and background thread handle
    stdin_stream: Option<StdinStreamState>,
    /// Filesystem implementation (local or remote)
    filesystem: std::sync::Arc<dyn FileSystem + Send + Sync>,
    /// Process spawner for plugin command execution (local or remote)
    process_spawner: std::sync::Arc<dyn remote::ProcessSpawner>,
    /// Remote session resources - must be kept alive for remote editing
    _remote_session: Option<RemoteSession>,
    /// Key translator for input calibration
    key_translator: KeyTranslator,
    #[cfg(target_os = "linux")]
    gpm_client: Option<GpmClient>,
    #[cfg(not(target_os = "linux"))]
    gpm_client: Option<()>,
    /// Terminal mode state (raw mode, alternate screen, etc.)
    /// Drop impl restores terminal on cleanup
    terminal_modes: TerminalModes,
}

/// State for stdin streaming in background
#[cfg(unix)]
pub struct StdinStreamState {
    /// Path to temp file where stdin is being written
    pub temp_path: PathBuf,
    /// Handle to background thread (None if completed)
    pub thread_handle: Option<std::thread::JoinHandle<anyhow::Result<()>>>,
}

/// Start streaming stdin to temp file in background.
/// Returns immediately with streaming state. Editor can start while data streams in.
/// Must be called BEFORE enabling raw terminal mode.
#[cfg(unix)]
fn start_stdin_streaming() -> AnyhowResult<StdinStreamState> {
    use std::fs::File;
    use std::os::unix::io::{AsRawFd, FromRawFd};

    // Duplicate stdin fd BEFORE reopening it as TTY
    // This preserves access to the pipe for background reading
    let stdin_fd = io::stdin().as_raw_fd();
    let pipe_fd = unsafe { libc::dup(stdin_fd) };
    if pipe_fd == -1 {
        anyhow::bail!("Failed to dup stdin: {}", io::Error::last_os_error());
    }

    // Create empty temp file
    let temp_dir = std::env::temp_dir();
    let temp_path = temp_dir.join(format!("fresh-stdin-{}.tmp", std::process::id()));
    File::create(&temp_path)?;

    // Reopen stdin from /dev/tty so crossterm can use it for keyboard input
    reopen_stdin_from_tty()?;
    tracing::info!("Reopened stdin from /dev/tty for terminal input");

    // Spawn background thread to drain pipe into temp file
    let temp_path_clone = temp_path.clone();
    let thread_handle = std::thread::spawn(move || {
        use std::io::{Read, Write};

        // SAFETY: pipe_fd is a valid duplicated file descriptor
        let mut pipe_file = unsafe { File::from_raw_fd(pipe_fd) };
        let mut temp_file = std::fs::OpenOptions::new()
            .append(true)
            .open(&temp_path_clone)?;

        const CHUNK_SIZE: usize = 64 * 1024;
        let mut buffer = vec![0u8; CHUNK_SIZE];

        loop {
            let bytes_read = pipe_file.read(&mut buffer)?;
            if bytes_read == 0 {
                break; // EOF
            }
            temp_file.write_all(&buffer[..bytes_read])?;
            // Flush each chunk so main thread can see progress
            temp_file.flush()?;
        }

        tracing::info!("Stdin streaming complete");
        Ok(())
    });

    Ok(StdinStreamState {
        temp_path,
        thread_handle: Some(thread_handle),
    })
}

/// Windows stdin stream state
#[cfg(windows)]
pub struct StdinStreamState {
    pub temp_path: PathBuf,
    pub thread_handle: Option<std::thread::JoinHandle<anyhow::Result<()>>>,
}

/// Stream stdin content to a temp file on Windows.
/// This is called when stdin is a pipe (e.g., `cat file.txt | fresh`).
/// We duplicate the stdin handle, spawn a thread to read from it,
/// and then reopen stdin from CONIN$ for keyboard input.
#[cfg(windows)]
fn start_stdin_streaming() -> AnyhowResult<StdinStreamState> {
    use std::fs::File;
    use std::io::{Read, Write};
    use std::os::windows::io::{AsRawHandle, FromRawHandle, OwnedHandle};
    use windows_sys::Win32::Foundation::{
        CloseHandle, DuplicateHandle, DUPLICATE_SAME_ACCESS, HANDLE, INVALID_HANDLE_VALUE,
    };
    use windows_sys::Win32::System::Console::GetStdHandle;
    use windows_sys::Win32::System::Console::STD_INPUT_HANDLE;
    use windows_sys::Win32::System::Threading::GetCurrentProcess;

    // Get the current stdin handle (which is a pipe)
    let stdin_handle = unsafe { GetStdHandle(STD_INPUT_HANDLE) };
    if stdin_handle == INVALID_HANDLE_VALUE || stdin_handle == 0 as HANDLE {
        anyhow::bail!("Failed to get stdin handle");
    }

    // Duplicate the handle so we can read from it in a background thread
    // while we replace stdin with CONIN$ for keyboard input
    let mut duplicated_handle: HANDLE = 0 as HANDLE;
    let current_process = unsafe { GetCurrentProcess() };
    let success = unsafe {
        DuplicateHandle(
            current_process,
            stdin_handle,
            current_process,
            &mut duplicated_handle,
            0,
            0, // not inheritable
            DUPLICATE_SAME_ACCESS,
        )
    };

    if success == 0 {
        anyhow::bail!(
            "Failed to duplicate stdin handle: {}",
            io::Error::last_os_error()
        );
    }

    // Create a temp file to store the piped content
    let temp_dir = std::env::temp_dir();
    let temp_path = temp_dir.join(format!("fresh-stdin-{}.txt", std::process::id()));

    let temp_path_clone = temp_path.clone();

    // Cast handle to usize for Send across thread boundary
    // SAFETY: HANDLE is a pointer-sized value, usize preserves it exactly
    let handle_as_usize = duplicated_handle as usize;

    // Spawn a thread to read from the duplicated pipe handle
    let thread_handle = std::thread::spawn(move || -> AnyhowResult<()> {
        // SAFETY: We own this duplicated handle and will close it when done
        // Cast back from usize to raw handle
        let raw_handle = handle_as_usize as *mut std::ffi::c_void;
        let owned_handle = unsafe { OwnedHandle::from_raw_handle(raw_handle) };
        let mut pipe_reader = unsafe { File::from_raw_handle(owned_handle.as_raw_handle()) };
        // Forget the OwnedHandle since File now owns it
        std::mem::forget(owned_handle);

        let mut temp_file = File::create(&temp_path_clone)?;
        let mut buffer = [0u8; 8192];

        loop {
            match pipe_reader.read(&mut buffer) {
                Ok(0) => break, // EOF
                Ok(n) => {
                    temp_file.write_all(&buffer[..n])?;
                }
                Err(e) if e.kind() == io::ErrorKind::BrokenPipe => break,
                Err(e) => return Err(e.into()),
            }
        }

        temp_file.flush()?;
        Ok(())
    });

    Ok(StdinStreamState {
        temp_path,
        thread_handle: Some(thread_handle),
    })
}

/// Check if stdin has data available (is a pipe or redirect, not a TTY)
fn stdin_has_data() -> bool {
    use std::io::IsTerminal;
    !io::stdin().is_terminal()
}

/// Reopen stdin from /dev/tty after reading piped content.
/// This allows crossterm to use the terminal for keyboard input
/// even though the original stdin was a pipe.
#[cfg(unix)]
fn reopen_stdin_from_tty() -> AnyhowResult<()> {
    use std::fs::File;
    use std::os::unix::io::AsRawFd;

    // Open /dev/tty - the controlling terminal
    let tty = File::open("/dev/tty")?;

    // Duplicate /dev/tty to stdin (fd 0) using libc
    // SAFETY: dup2 is safe to call with valid file descriptors
    let result = unsafe { libc::dup2(tty.as_raw_fd(), libc::STDIN_FILENO) };

    if result == -1 {
        anyhow::bail!(io::Error::last_os_error());
    }

    Ok(())
}

/// Reopen stdin from CONIN$ on Windows.
/// This allows crossterm to receive keyboard events after stdin was a pipe.
#[cfg(windows)]
fn reopen_stdin_from_tty() -> AnyhowResult<()> {
    use windows_sys::Win32::Foundation::INVALID_HANDLE_VALUE;
    use windows_sys::Win32::Storage::FileSystem::{
        CreateFileW, FILE_GENERIC_READ, FILE_SHARE_READ, OPEN_EXISTING,
    };
    use windows_sys::Win32::System::Console::{SetStdHandle, STD_INPUT_HANDLE};

    // "CONIN$" is the console input device on Windows
    // This is analogous to /dev/tty on Unix
    let conin: Vec<u16> = "CONIN$\0".encode_utf16().collect();

    let conin_handle = unsafe {
        CreateFileW(
            conin.as_ptr(),
            FILE_GENERIC_READ,
            FILE_SHARE_READ,
            std::ptr::null(),
            OPEN_EXISTING,
            0,
            std::ptr::null_mut(),
        )
    };

    if conin_handle == INVALID_HANDLE_VALUE {
        anyhow::bail!("Failed to open CONIN$: {}", io::Error::last_os_error());
    }

    // Replace stdin with the console input handle
    let success = unsafe { SetStdHandle(STD_INPUT_HANDLE, conin_handle) };
    if success == 0 {
        anyhow::bail!(
            "Failed to set stdin to CONIN$: {}",
            io::Error::last_os_error()
        );
    }

    Ok(())
}

fn handle_first_run_setup(
    editor: &mut Editor,
    args: &Args,
    file_locations: &[FileLocation],
    show_file_explorer: bool,
    stdin_stream: &mut Option<StdinStreamState>,
    tracing_handles: &mut Option<TracingHandles>,
    workspace_enabled: bool,
) -> AnyhowResult<()> {
    if let Some(log_path) = &args.event_log {
        tracing::trace!("Event logging enabled: {}", log_path.display());
        editor.enable_event_streaming(log_path)?;
    }

    if let Some(handles) = tracing_handles.take() {
        editor.set_warning_log(handles.warning.receiver, handles.warning.path);
        editor.set_status_log_path(handles.status.path);
    }

    if workspace_enabled {
        match editor.try_restore_workspace() {
            Ok(true) => {
                tracing::info!("Workspace restored successfully");
            }
            Ok(false) => {
                tracing::debug!("No previous workspace found");
            }
            Err(e) => {
                tracing::warn!("Failed to restore workspace: {}", e);
            }
        }
    }

    // Handle stdin streaming (takes priority over files)
    // Opens with empty/partial buffer, content streams in background
    if let Some(mut stream_state) = stdin_stream.take() {
        tracing::info!("Opening stdin buffer from: {:?}", stream_state.temp_path);
        editor.open_stdin_buffer(&stream_state.temp_path, stream_state.thread_handle.take())?;
    }

    // Queue CLI files to be opened after the TUI starts
    // This ensures they go through the same code path as interactive file opens,
    // with consistent error handling (e.g., encoding confirmation prompts in the UI)
    for loc in file_locations {
        if loc.path.is_dir() {
            continue;
        }
        tracing::info!("[SYNTAX DEBUG] Queueing CLI file for open: {:?}", loc.path);
        editor.queue_file_open(
            loc.path.clone(),
            loc.line,
            loc.column,
            loc.end_line,
            loc.end_column,
            loc.message.clone(),
        );
    }

    if show_file_explorer {
        editor.show_file_explorer();
    }

    if editor.has_recovery_files().unwrap_or(false) {
        tracing::info!("Recovery files found from previous session, recovering...");
        match editor.recover_all_buffers() {
            Ok(count) if count > 0 => {
                tracing::info!("Recovered {} buffer(s)", count);
            }
            Ok(_) => {
                tracing::info!("No buffers to recover");
            }
            Err(e) => {
                tracing::warn!("Failed to recover buffers: {}", e);
            }
        }
    }

    Ok(())
}

/// Parse a file path that may include line/column, range, and message information.
/// Supports formats:
/// - file.txt
/// - file.txt:10
/// - file.txt:10:5
/// - file.txt:13-16           (line range)
/// - file.txt:13:17-21:1      (full range with columns)
/// - file.txt:10@"message"    (position + hover message)
/// - file.txt:13-16@"message" (range + hover message)
/// - /path/to/file.txt:10:5
///
/// For Windows paths like C:\path\file.txt:10:5, we handle the drive letter
/// prefix properly using std::path APIs.
///
/// If the full path exists as a file, it's used as-is (handles files with colons in name).
fn parse_file_location(input: &str) -> FileLocation {
    use std::path::{Component, Path};

    let empty = FileLocation {
        path: PathBuf::from(input),
        line: None,
        column: None,
        end_line: None,
        end_column: None,
        message: None,
    };

    let full_path = PathBuf::from(input);

    // If the full path exists as a file, use it directly
    // This handles edge cases like files named "foo:10"
    if full_path.is_file() {
        return FileLocation {
            path: full_path,
            ..empty
        };
    }

    // Extract message from @"..." suffix (before parsing positions)
    let (input_no_msg, message) = extract_message_suffix(input);

    // Check if the path has a Windows drive prefix using std::path
    let has_prefix = Path::new(input_no_msg)
        .components()
        .next()
        .map(|c| matches!(c, Component::Prefix(_)))
        .unwrap_or(false);

    // Calculate where to start looking for :line:col
    // For Windows paths with prefix (e.g., "C:"), skip past the drive letter and colon
    let search_start = if has_prefix {
        // Find the first colon (the drive letter separator) and skip it
        input_no_msg.find(':').map(|i| i + 1).unwrap_or(0)
    } else {
        0
    };

    // Find the last colon(s) that could be line:col
    let suffix = &input_no_msg[search_start..];

    // Check if there's a range (contains '-' in the location suffix, not in the path)
    // We need to find the first colon that starts the location suffix, then check for '-'
    if let Some(first_colon) = suffix.find(':') {
        let location_part = &suffix[first_colon + 1..];
        if location_part.contains('-') {
            // Range syntax: try to parse as L-EL or L:C-EL:EC
            let path_part = &suffix[..first_colon];
            let path_str = if has_prefix {
                format!("{}{}", &input_no_msg[..search_start], path_part)
            } else {
                path_part.to_string()
            };

            if let Some(result) =
                parse_range(location_part, PathBuf::from(path_str), message.clone())
            {
                return result;
            }
        }
    }

    // No range — fall back to standard :line or :line:col parsing
    let parts: Vec<&str> = suffix.rsplitn(3, ':').collect();

    match parts.as_slice() {
        [maybe_col, maybe_line, rest] => {
            if let (Ok(line), Ok(col)) = (maybe_line.parse::<usize>(), maybe_col.parse::<usize>()) {
                let path_str = if has_prefix {
                    format!("{}{}", &input_no_msg[..search_start], rest)
                } else {
                    rest.to_string()
                };
                return FileLocation {
                    path: PathBuf::from(path_str),
                    line: Some(line),
                    column: Some(col),
                    message,
                    ..empty
                };
            }
        }
        [maybe_line, rest] => {
            if let Ok(line) = maybe_line.parse::<usize>() {
                let path_str = if has_prefix {
                    format!("{}{}", &input_no_msg[..search_start], rest)
                } else {
                    rest.to_string()
                };
                return FileLocation {
                    path: PathBuf::from(path_str),
                    line: Some(line),
                    message,
                    ..empty
                };
            }
        }
        _ => {}
    }

    // No valid suffix found, treat the whole thing as a path
    FileLocation {
        path: PathBuf::from(input_no_msg),
        message,
        ..empty
    }
}

/// Extract a @"message" suffix from a file location string.
/// Returns (remaining_input, optional_message).
fn extract_message_suffix(input: &str) -> (&str, Option<String>) {
    // Look for @" pattern — the message is everything between the quotes
    if let Some(at_pos) = input.rfind("@\"") {
        if input.ends_with('"') && input.len() > at_pos + 2 {
            let msg = &input[at_pos + 2..input.len() - 1];
            // Unescape \" within the message
            let msg = msg.replace("\\\"", "\"");
            return (&input[..at_pos], Some(msg));
        }
    }
    (input, None)
}

/// Parse a range location suffix like "13-16" or "13:17-21:1".
/// Returns a FileLocation if successful.
fn parse_range(location: &str, path: PathBuf, message: Option<String>) -> Option<FileLocation> {
    let parts: Vec<&str> = location.splitn(2, '-').collect();
    if parts.len() != 2 {
        return None;
    }

    let start_part = parts[0];
    let end_part = parts[1];

    // Parse start: either "L" or "L:C"
    let (start_line, start_col) = parse_line_col(start_part)?;
    // Parse end: either "EL" or "EL:EC"
    let (end_line, end_col) = parse_line_col(end_part)?;

    Some(FileLocation {
        path,
        line: Some(start_line),
        column: start_col,
        end_line: Some(end_line),
        end_column: end_col,
        message,
    })
}

/// Parse "L" or "L:C" into (line, optional_column).
fn parse_line_col(s: &str) -> Option<(usize, Option<usize>)> {
    if let Some((line_str, col_str)) = s.split_once(':') {
        let line = line_str.parse::<usize>().ok()?;
        let col = col_str.parse::<usize>().ok()?;
        Some((line, Some(col)))
    } else {
        let line = s.parse::<usize>().ok()?;
        Some((line, None))
    }
}

/// Parse a location that may be local (file:line:col) or remote (user@host:path:line:col)
///
/// Remote format: user@host:path or user@host:path:line or user@host:path:line:col
/// The path can be absolute (/path) or relative (path)
fn parse_location(input: &str) -> ParsedLocation {
    // Check for SSH-style syntax: user@host:path
    // Must have @ before the first : to be considered remote
    // Also skip if it looks like a Windows path (single letter before :)
    if let Some(at_pos) = input.find('@') {
        // Everything before @ is the user
        let user = &input[..at_pos];

        // Everything after @ contains host:path[:line[:col]]
        let after_at = &input[at_pos + 1..];

        // Find the first : which separates host from path
        if let Some(colon_pos) = after_at.find(':') {
            let host = &after_at[..colon_pos];
            let path_and_rest = &after_at[colon_pos + 1..];

            // Validate: user and host must be non-empty and not contain spaces
            if !user.is_empty()
                && !host.is_empty()
                && !user.contains(' ')
                && !host.contains(' ')
                && !path_and_rest.is_empty()
            {
                // Now parse path:line:col from path_and_rest
                // We need to distinguish between path components and line:col suffixes
                // Strategy: work backwards, try to parse numeric suffixes

                let parts: Vec<&str> = path_and_rest.rsplitn(3, ':').collect();

                let (path, line, column) = match parts.as_slice() {
                    [maybe_col, maybe_line, rest] => {
                        if let (Ok(line), Ok(col)) =
                            (maybe_line.parse::<usize>(), maybe_col.parse::<usize>())
                        {
                            (rest.to_string(), Some(line), Some(col))
                        } else {
                            (path_and_rest.to_string(), None, None)
                        }
                    }
                    [maybe_line, rest] => {
                        if let Ok(line) = maybe_line.parse::<usize>() {
                            (rest.to_string(), Some(line), None)
                        } else {
                            (path_and_rest.to_string(), None, None)
                        }
                    }
                    _ => (path_and_rest.to_string(), None, None),
                };

                return ParsedLocation::Remote(RemoteLocation {
                    user: user.to_string(),
                    host: host.to_string(),
                    path,
                    line,
                    column,
                });
            }
        }
    }

    // Not a remote path, parse as local
    ParsedLocation::Local(parse_file_location(input))
}

/// Holds resources needed for remote editing (kept alive for duration of session)
struct RemoteSession {
    /// The SSH connection - dropping this closes the connection
    _connection: remote::SshConnection,
    /// Tokio runtime for async operations
    _runtime: tokio::runtime::Runtime,
}

/// Result of creating filesystem - includes optional remote session to keep alive
struct FilesystemResult {
    filesystem: std::sync::Arc<dyn FileSystem + Send + Sync>,
    /// Process spawner for plugin command execution
    process_spawner: std::sync::Arc<dyn remote::ProcessSpawner>,
    /// Remote session resources - must be kept alive for remote editing
    remote_session: Option<RemoteSession>,
}

/// Create filesystem for local or remote editing
fn create_filesystem(remote_info: &Option<RemoteLocation>) -> AnyhowResult<FilesystemResult> {
    if let Some(remote) = remote_info {
        connect_remote(remote)
    } else {
        Ok(FilesystemResult {
            filesystem: std::sync::Arc::new(StdFileSystem),
            process_spawner: std::sync::Arc::new(remote::LocalProcessSpawner),
            remote_session: None,
        })
    }
}

/// Establish SSH connection to remote host and return RemoteFileSystem
fn connect_remote(remote: &RemoteLocation) -> AnyhowResult<FilesystemResult> {
    // Create a Tokio runtime for the SSH connection
    let rt = tokio::runtime::Runtime::new()
        .context("Failed to create Tokio runtime for remote connection")?;

    let connection_params = remote::ConnectionParams {
        user: remote.user.clone(),
        host: remote.host.clone(),
        port: None, // TODO: support port in remote location parsing
        identity_file: None,
    };

    // Establish SSH connection (this is async, so we block on it)
    let connection = rt
        .block_on(remote::SshConnection::connect(connection_params))
        .context(format!(
            "Failed to connect to remote host {}@{}",
            remote.user, remote.host
        ))?;

    let connection_string = connection.connection_string();
    let channel = connection.channel();

    tracing::info!("Connected to remote host: {}", connection_string);

    let filesystem = std::sync::Arc::new(remote::RemoteFileSystem::new(
        channel.clone(),
        connection_string,
    ));
    let process_spawner = std::sync::Arc::new(remote::RemoteProcessSpawner::new(channel));

    Ok(FilesystemResult {
        filesystem,
        process_spawner,
        remote_session: Some(RemoteSession {
            _connection: connection,
            _runtime: rt,
        }),
    })
}

fn initialize_app(args: &Args) -> AnyhowResult<SetupState> {
    let log_file = args
        .log_file
        .clone()
        .unwrap_or_else(fresh::services::log_dirs::main_log_path);
    let tracing_handles = tracing_setup::init_global(&log_file);

    // Clean up stale log files from dead processes on startup
    fresh::services::log_dirs::cleanup_stale_logs();

    tracing::info!("Editor starting");

    signal_handler::install_signal_handlers();
    tracing::info!("Signal handlers installed");

    let original_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic| {
        terminal_modes::emergency_cleanup();
        original_hook(panic);
    }));

    // Check if we should read from stdin
    // This can be triggered by --stdin flag or by using "-" as a file argument
    let stdin_requested = args.stdin || args.files.iter().any(|f| f == "-");

    // Start stdin streaming in background BEFORE entering raw mode
    // This is critical - once raw mode is enabled, stdin is used for terminal events
    // Background thread streams pipe → temp file while editor runs
    let stdin_stream = if stdin_requested {
        if stdin_has_data() {
            tracing::info!("Starting background stdin streaming");
            match start_stdin_streaming() {
                Ok(stream_state) => {
                    tracing::info!(
                        "Stdin streaming started, temp file: {:?}",
                        stream_state.temp_path
                    );
                    Some(stream_state)
                }
                Err(e) => {
                    eprintln!("Error: Failed to start stdin streaming: {}", e);
                    return Err(e);
                }
            }
        } else {
            eprintln!("Error: --stdin or \"-\" specified but stdin is a terminal (no piped data)");
            anyhow::bail!(io::Error::new(
                io::ErrorKind::InvalidInput,
                "No data piped to stdin",
            ));
        }
    } else {
        None
    };

    // Determine working directory early for config loading
    // Filter out "-" from files list since it's handled via stdin_stream
    // Parse locations which may be local or remote (user@host:path)
    let parsed_locations: Vec<ParsedLocation> = args
        .files
        .iter()
        .filter(|f| *f != "-")
        .map(|f| parse_location(f))
        .collect();

    // Check for remote locations - for now, collect them separately
    let remote_locations: Vec<&RemoteLocation> = parsed_locations
        .iter()
        .filter_map(|loc| match loc {
            ParsedLocation::Remote(r) => Some(r),
            ParsedLocation::Local(_) => None,
        })
        .collect();

    // If there are remote locations, validate they're all on the same host
    let remote_info: Option<RemoteLocation> = if !remote_locations.is_empty() {
        let first = remote_locations[0];
        for r in &remote_locations[1..] {
            if r.user != first.user || r.host != first.host {
                anyhow::bail!(
                    "Cannot open files from multiple remote hosts. \
                     First: {}@{}, found: {}@{}",
                    first.user,
                    first.host,
                    r.user,
                    r.host
                );
            }
        }
        // Check that there are no local files mixed with remote
        let has_local = parsed_locations
            .iter()
            .any(|loc| matches!(loc, ParsedLocation::Local(_)));
        if has_local {
            anyhow::bail!(
                "Cannot mix local and remote files. Use either local paths or remote paths (user@host:path)."
            );
        }
        Some(first.clone())
    } else {
        None
    };

    // Convert to FileLocation for downstream code
    let file_locations: Vec<FileLocation> = parsed_locations
        .into_iter()
        .map(|loc| match loc {
            ParsedLocation::Local(fl) => fl,
            ParsedLocation::Remote(rl) => FileLocation {
                path: PathBuf::from(&rl.path),
                line: rl.line,
                column: rl.column,
                end_line: None,
                end_column: None,
                message: None,
            },
        })
        .collect();

    // Create filesystem early - needed for remote directory detection
    // For remote editing, this establishes the SSH connection
    let FilesystemResult {
        filesystem,
        process_spawner,
        remote_session,
    } = create_filesystem(&remote_info)?;

    let mut working_dir = None;
    let mut show_file_explorer = false;

    // Only set working_dir if exactly one parameter is passed and it's a directory
    if file_locations.len() == 1 {
        if let Some(first_loc) = file_locations.first() {
            // Use the filesystem to check if path is a directory
            // This works for both local and remote paths
            let is_directory = filesystem.is_dir(&first_loc.path).unwrap_or(false);
            if is_directory {
                working_dir = Some(first_loc.path.clone());
                show_file_explorer = true;
            }
        }
    }

    // Load config using the layered config system
    // For remote editing, use current local dir for config (remote doesn't have our config)
    let effective_working_dir = if remote_info.is_some() {
        std::env::current_dir().unwrap_or_default()
    } else {
        working_dir
            .as_ref()
            .cloned()
            .unwrap_or_else(|| std::env::current_dir().unwrap_or_default())
    };

    let dir_context = fresh::config_io::DirectoryContext::from_system()?;

    let mut config = if let Some(config_path) = &args.config {
        // Explicit config file overrides layered system
        match config::Config::load_from_file(config_path) {
            Ok(cfg) => cfg,
            Err(e) => {
                eprintln!(
                    "Error: Failed to load config from {}: {}",
                    config_path.display(),
                    e
                );
                anyhow::bail!(io::Error::new(io::ErrorKind::InvalidData, e.to_string()));
            }
        }
    } else {
        config::Config::load_with_layers(&dir_context, &effective_working_dir)
    };

    // CLI flag overrides config
    if args.no_upgrade_check {
        config.check_for_updates = false;
    }

    // Initialize i18n with locale: CLI arg > config > environment
    // This ensures menu defaults are created with the correct translations
    let locale_override = args.locale.as_deref().or(config.locale.as_option());
    fresh::i18n::init_with_config(locale_override);

    // Enable terminal modes (raw mode, alternate screen, mouse capture, etc.)
    // This checks support for each mode and tracks what was enabled
    let keyboard_config = KeyboardConfig {
        disambiguate_escape_codes: config.editor.keyboard_disambiguate_escape_codes,
        report_event_types: config.editor.keyboard_report_event_types,
        report_alternate_keys: config.editor.keyboard_report_alternate_keys,
        report_all_keys_as_escape_codes: config.editor.keyboard_report_all_keys_as_escape_codes,
    };
    let terminal_modes = TerminalModes::enable(Some(&keyboard_config))?;

    #[cfg(target_os = "linux")]
    let gpm_client = match GpmClient::connect() {
        Ok(client) => client,
        Err(e) => {
            tracing::warn!("Failed to connect to GPM: {}", e);
            None
        }
    };
    #[cfg(not(target_os = "linux"))]
    let gpm_client: Option<()> = None;

    if gpm_client.is_some() {
        tracing::info!("Using GPM for mouse capture");
    }

    // Set cursor style from config
    use crossterm::ExecutableCommand;
    // Best-effort cursor style set
    #[allow(clippy::let_underscore_must_use)]
    let _ = stdout().execute(config.editor.cursor_style.to_crossterm_style());
    tracing::info!("Set cursor style to {:?}", config.editor.cursor_style);

    let backend = ratatui::backend::CrosstermBackend::new(stdout());
    let mut terminal = Terminal::new(backend)?;
    terminal.clear()?;

    let size = terminal.size()?;
    tracing::info!("Terminal size: {}x{}", size.width, size.height);

    let dir_context = DirectoryContext::from_system()?;
    let current_working_dir = working_dir;

    // Load key translator for input calibration
    let key_translator = match KeyTranslator::load_from_config_dir(&dir_context.config_dir) {
        Ok(translator) => translator,
        Err(e) => {
            tracing::warn!("Failed to load key calibration: {}", e);
            KeyTranslator::new()
        }
    };

    Ok(SetupState {
        config,
        tracing_handles,
        terminal,
        terminal_size: (size.width, size.height),
        file_locations,
        show_file_explorer,
        dir_context,
        current_working_dir,
        stdin_stream,
        key_translator,
        gpm_client,
        terminal_modes,
        filesystem,
        process_spawner,
        _remote_session: remote_session,
    })
}

#[cfg_attr(not(target_os = "linux"), allow(unused_variables))]
fn run_editor_iteration(
    editor: &mut Editor,
    workspace_enabled: bool,
    terminal: &mut Terminal<ratatui::backend::CrosstermBackend<io::Stdout>>,
    key_translator: &KeyTranslator,
    #[cfg(target_os = "linux")] gpm_client: &Option<GpmClient>,
) -> AnyhowResult<IterationOutcome> {
    #[cfg(target_os = "linux")]
    let loop_result = run_event_loop(
        editor,
        terminal,
        workspace_enabled,
        key_translator,
        gpm_client,
    );
    #[cfg(not(target_os = "linux"))]
    let loop_result = run_event_loop(editor, terminal, workspace_enabled, key_translator);

    if let Err(e) = editor.end_recovery_session() {
        tracing::warn!("Failed to end recovery session: {}", e);
    }

    let update_result = editor.get_update_result().cloned();
    let restart_dir = editor.take_restart_dir();

    Ok(IterationOutcome {
        loop_result,
        update_result,
        restart_dir,
    })
}

/// Check a plugin by bundling it and printing the output
#[cfg(feature = "plugins")]
fn check_plugin_bundle(plugin_path: &std::path::Path) -> AnyhowResult<()> {
    use fresh_parser_js::{bundle_module, has_es_module_syntax, transpile_typescript};

    eprintln!("Checking plugin: {}", plugin_path.display());

    // Read the source
    let source = std::fs::read_to_string(plugin_path)
        .with_context(|| format!("Failed to read plugin file: {}", plugin_path.display()))?;

    eprintln!("Source length: {} bytes", source.len());

    // Check if it needs bundling
    if has_es_module_syntax(&source) {
        eprintln!("Plugin has ES module syntax, bundling...\n");

        match bundle_module(plugin_path) {
            Ok(bundled) => {
                eprintln!("=== BUNDLED OUTPUT ({} bytes) ===\n", bundled.len());
                println!("{}", bundled);
                eprintln!("\n=== END BUNDLED OUTPUT ===");
            }
            Err(e) => {
                eprintln!("ERROR bundling plugin: {}", e);
                return Err(e);
            }
        }
    } else {
        eprintln!("Plugin has no ES module syntax, transpiling directly...\n");

        let filename = plugin_path.to_str().unwrap_or("plugin.ts");
        match transpile_typescript(&source, filename) {
            Ok(transpiled) => {
                eprintln!("=== TRANSPILED OUTPUT ({} bytes) ===\n", transpiled.len());
                println!("{}", transpiled);
                eprintln!("\n=== END TRANSPILED OUTPUT ===");
            }
            Err(e) => {
                eprintln!("ERROR transpiling plugin: {}", e);
                return Err(e);
            }
        }
    }

    Ok(())
}

/// Initialize a new Fresh package (plugin, theme, or language pack)
fn init_package_command(package_type: Option<String>) -> AnyhowResult<()> {
    use std::io::{BufRead, Write};

    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();

    // Helper to prompt for input
    let mut prompt = |msg: &str| -> String {
        print!("{}", msg);
        // Best-effort flush for interactive prompt
        #[allow(clippy::let_underscore_must_use)]
        let _ = stdout.flush();
        let mut input = String::new();
        stdin.lock().read_line(&mut input).unwrap_or_default();
        input.trim().to_string()
    };

    println!("Fresh Package Initializer");
    println!("=========================\n");

    // Determine package type
    let pkg_type = match package_type.as_deref() {
        Some("plugin") | Some("p") => "plugin",
        Some("theme") | Some("t") => "theme",
        Some("language") | Some("lang") | Some("l") => "language",
        Some(other) => {
            eprintln!(
                "Unknown package type '{}'. Valid types: plugin, theme, language",
                other
            );
            std::process::exit(1);
        }
        None => {
            println!("Package types:");
            println!("  1. plugin   - Extend Fresh with custom commands and functionality");
            println!("  2. theme    - Custom color schemes and styling");
            println!("  3. language - Syntax highlighting, LSP, and language configuration\n");

            loop {
                let choice = prompt("Select type (1/2/3 or plugin/theme/language): ");
                match choice.as_str() {
                    "1" | "plugin" | "p" => break "plugin",
                    "2" | "theme" | "t" => break "theme",
                    "3" | "language" | "lang" | "l" => break "language",
                    "" => {
                        eprintln!("Please select a package type.");
                    }
                    _ => {
                        eprintln!("Invalid choice. Please enter 1, 2, 3, or the type name.");
                    }
                }
            }
        }
    };

    // Get package name
    let default_name = format!("my-fresh-{}", pkg_type);
    let name = loop {
        let input = prompt(&format!("Package name [{}]: ", default_name));
        let name = if input.is_empty() {
            default_name.clone()
        } else {
            input
        };

        // Validate name (lowercase, alphanumeric, dashes)
        if name
            .chars()
            .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-')
            && !name.starts_with('-')
            && !name.ends_with('-')
        {
            break name;
        }
        eprintln!("Invalid name. Use lowercase letters, numbers, and dashes only.");
    };

    // Get description
    let description = prompt("Description (optional): ");

    // Get author
    let author = prompt("Author (optional): ");

    // Create directory
    let pkg_dir = PathBuf::from(&name);
    if pkg_dir.exists() {
        eprintln!("Error: Directory '{}' already exists.", name);
        std::process::exit(1);
    }

    std::fs::create_dir_all(&pkg_dir)?;

    // Generate files based on package type
    match pkg_type {
        "plugin" => create_plugin_package(&pkg_dir, &name, &description, &author)?,
        "theme" => create_theme_package(&pkg_dir, &name, &description, &author)?,
        "language" => create_language_package(&pkg_dir, &name, &description, &author)?,
        _ => unreachable!(),
    }

    println!("\nPackage '{}' created successfully!", name);
    println!("\nNext steps:");
    println!("  1. cd {}", name);
    match pkg_type {
        "plugin" => {
            println!("  2. Edit plugin.ts to add your functionality");
            println!("  3. Test locally: fresh --check-plugin .");
            println!("  4. Validate manifest: ./validate.sh");
        }
        "theme" => {
            println!("  2. Edit theme.json to customize colors");
            println!("  3. Validate theme: ./validate.sh (requires: pip install jsonschema)");
        }
        "language" => {
            println!("  2. Edit grammars/syntax.sublime-syntax (YAML format)");
            println!("  3. Update package.json with file extensions and LSP command");
            println!("  4. Test by copying to ~/.config/fresh/grammars/");
            println!("  5. Validate manifest: ./validate.sh");
        }
        _ => unreachable!(),
    }
    println!("\nTo publish:");
    println!("  1. Push your package to a public Git repository");
    println!("  2. Submit a PR to: https://github.com/sinelaw/fresh-plugins-registry");
    println!("     Add your package to the appropriate registry file:");
    match pkg_type {
        "plugin" => println!("     - plugins.json"),
        "theme" => println!("     - themes.json"),
        "language" => println!("     - languages.json"),
        _ => unreachable!(),
    }
    println!("\nDocumentation: https://github.com/sinelaw/fresh-plugins-registry#readme");

    Ok(())
}

/// Write a validation script that checks package.json against the official schema
fn write_validate_script(dir: &PathBuf) -> AnyhowResult<()> {
    let validate_sh = r#"#!/bin/bash
# Validate package.json against the official Fresh package schema
#
# Prerequisite: pip install jsonschema
curl -sSL https://raw.githubusercontent.com/sinelaw/fresh/main/scripts/validate-package.sh | bash
"#;
    write_script_file(dir, "validate.sh", validate_sh)
}

/// Write a validation script for themes (validates both package.json and theme.json)
fn write_theme_validate_script(dir: &PathBuf) -> AnyhowResult<()> {
    let validate_sh = r#"#!/bin/bash
# Validate Fresh theme package
#
# Prerequisite: pip install jsonschema
set -e

echo "Validating package.json..."
curl -sSL https://raw.githubusercontent.com/sinelaw/fresh/main/scripts/validate-package.sh | bash

echo "Validating theme.json..."
python3 -c "
import json, jsonschema, urllib.request, sys

with open('theme.json') as f:
    data = json.load(f)

schema_url = 'https://raw.githubusercontent.com/sinelaw/fresh/main/crates/fresh-editor/plugins/schemas/theme.schema.json'
try:
    with urllib.request.urlopen(schema_url, timeout=5) as resp:
        schema = json.load(resp)
    jsonschema.validate(data, schema)
    print('✓ theme.json is valid')
except urllib.error.URLError:
    print('⚠ Could not fetch schema (URL may not exist yet)')
except jsonschema.ValidationError as e:
    print(f'✗ Validation error: {e.message}')
    sys.exit(1)
"
"#;
    write_script_file(dir, "validate.sh", validate_sh)
}

fn write_script_file(dir: &PathBuf, name: &str, content: &str) -> AnyhowResult<()> {
    std::fs::write(dir.join(name), content)?;

    // Make executable on Unix
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(dir.join(name))?.permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(dir.join(name), perms)?;
    }

    Ok(())
}

fn create_plugin_package(
    dir: &PathBuf,
    name: &str,
    description: &str,
    author: &str,
) -> AnyhowResult<()> {
    // package.json
    let package_json = format!(
        r#"{{
  "$schema": "https://raw.githubusercontent.com/sinelaw/fresh/main/crates/fresh-editor/plugins/schemas/package.schema.json",
  "name": "{}",
  "version": "0.1.0",
  "description": "{}",
  "type": "plugin",
  "author": "{}",
  "license": "MIT",
  "fresh": {{
    "main": "plugin.ts"
  }}
}}
"#,
        name,
        if description.is_empty() {
            format!("A Fresh plugin")
        } else {
            description.to_string()
        },
        author
    );
    std::fs::write(dir.join("package.json"), package_json)?;

    // validate.sh
    write_validate_script(dir)?;

    // plugin.ts
    let plugin_ts = r#"// Fresh Plugin
// Documentation: https://github.com/user/fresh/blob/main/docs/plugins.md

// Register a command that users can invoke
editor.registerCommand("hello", "Say Hello", async () => {
  editor.showStatusMessage("Hello from your plugin!");
});

// React to editor events
editor.on("buffer_opened", (event) => {
  const info = editor.getBufferInfo();
  if (info) {
    console.log(`Opened: ${info.path}`);
  }
});

// Example: Add a keybinding in your Fresh config:
// {
//   "keyBindings": {
//     "ctrl+alt+h": "command:hello"
//   }
// }
"#;
    std::fs::write(dir.join("plugin.ts"), plugin_ts)?;

    // README.md
    let readme = format!(
        r#"# {}

{}

## Installation

Install via Fresh's package manager:
```
:pkg install {}
```

Or install from this repository:
```
:pkg install https://github.com/YOUR_USERNAME/{}
```

## Usage

This plugin adds the following commands:
- `hello` - Say Hello

## License

MIT
"#,
        name,
        if description.is_empty() {
            "A Fresh plugin."
        } else {
            description
        },
        name,
        name
    );
    std::fs::write(dir.join("README.md"), readme)?;

    Ok(())
}

fn create_theme_package(
    dir: &PathBuf,
    name: &str,
    description: &str,
    author: &str,
) -> AnyhowResult<()> {
    // package.json
    let package_json = format!(
        r#"{{
  "$schema": "https://raw.githubusercontent.com/sinelaw/fresh/main/crates/fresh-editor/plugins/schemas/package.schema.json",
  "name": "{}",
  "version": "0.1.0",
  "description": "{}",
  "type": "theme",
  "author": "{}",
  "license": "MIT",
  "fresh": {{
    "theme": "theme.json"
  }}
}}
"#,
        name,
        if description.is_empty() {
            format!("A Fresh theme")
        } else {
            description.to_string()
        },
        author
    );
    std::fs::write(dir.join("package.json"), package_json)?;

    // validate.sh - validates both package.json and theme.json
    write_theme_validate_script(dir)?;

    // theme.json - minimal theme with essential colors
    let theme_json = r##"{
  "name": "My Theme",
  "colors": {
    "background": "#1e1e2e",
    "foreground": "#cdd6f4",
    "cursor": "#f5e0dc",
    "selection": "#45475a",
    "line_numbers": "#6c7086",
    "current_line": "#313244",
    "status_bar": {
      "background": "#181825",
      "foreground": "#cdd6f4"
    },
    "syntax": {
      "keyword": "#cba6f7",
      "string": "#a6e3a1",
      "number": "#fab387",
      "comment": "#6c7086",
      "function": "#89b4fa",
      "type": "#f9e2af",
      "variable": "#cdd6f4",
      "operator": "#89dceb"
    }
  }
}
"##;
    std::fs::write(dir.join("theme.json"), theme_json)?;

    // README.md
    let readme = format!(
        r#"# {}

{}

## Installation

Install via Fresh's package manager:
```
:pkg install {}
```

## Activation

After installation, activate the theme:
```
:theme {}
```

Or add to your Fresh config:
```json
{{
  "theme": "{}"
}}
```

## Preview

<!-- Add a screenshot of your theme here -->

## License

MIT
"#,
        name,
        if description.is_empty() {
            "A Fresh theme."
        } else {
            description
        },
        name,
        name,
        name
    );
    std::fs::write(dir.join("README.md"), readme)?;

    Ok(())
}

fn create_language_package(
    dir: &PathBuf,
    name: &str,
    description: &str,
    author: &str,
) -> AnyhowResult<()> {
    // Create grammars directory
    std::fs::create_dir_all(dir.join("grammars"))?;

    // package.json
    let package_json = format!(
        r#"{{
  "$schema": "https://raw.githubusercontent.com/sinelaw/fresh/main/crates/fresh-editor/plugins/schemas/package.schema.json",
  "name": "{}",
  "version": "0.1.0",
  "description": "{}",
  "type": "language",
  "author": "{}",
  "license": "MIT",
  "fresh": {{
    "grammar": {{
      "file": "grammars/syntax.sublime-syntax",
      "extensions": ["ext"]
    }},
    "language": {{
      "commentPrefix": "//",
      "tabSize": 4,
      "autoIndent": true
    }},
    "lsp": {{
      "command": "language-server",
      "args": ["--stdio"],
      "autoStart": true
    }}
  }}
}}
"#,
        name,
        if description.is_empty() {
            format!("Language support for Fresh")
        } else {
            description.to_string()
        },
        author
    );
    std::fs::write(dir.join("package.json"), package_json)?;

    // validate.sh
    write_validate_script(dir)?;

    // Sublime syntax grammar template (YAML format)
    let grammar = r#"%YAML 1.2
---
# Sublime syntax file for your language
# Documentation: https://www.sublimetext.com/docs/syntax.html
name: My Language
scope: source.mylang
file_extensions: [ext]

contexts:
  main:
    - include: comments
    - include: strings
    - include: keywords
    - include: numbers

  comments:
    # Line comments
    - match: //.*$
      scope: comment.line.double-slash

  strings:
    # Double-quoted strings with escape sequences
    - match: '"'
      scope: punctuation.definition.string.begin
      push:
        - meta_scope: string.quoted.double
        - match: \\.
          scope: constant.character.escape
        - match: '"'
          scope: punctuation.definition.string.end
          pop: true

  keywords:
    - match: \b(if|else|while|for|return)\b
      scope: keyword.control

  numbers:
    - match: \b[0-9]+(\.[0-9]+)?\b
      scope: constant.numeric
"#;
    std::fs::write(dir.join("grammars/syntax.sublime-syntax"), grammar)?;

    // README.md
    let readme = format!(
        r#"# {}

{}

## Features

- Syntax highlighting via Sublime syntax grammar
- Language configuration (comments, indentation)
- LSP integration (if configured)

## Installation

Install via Fresh's package manager:
```
:pkg install {}
```

## Configuration

This language pack provides:

### Grammar
- File extensions: `.ext` (update in package.json)
- Syntax highlighting rules in `grammars/syntax.sublime-syntax`

### Language Settings
- Comment prefix: `//`
- Tab size: 4 spaces
- Auto-indent: enabled

### LSP Server
- Command: `language-server --stdio`
- Auto-start: enabled

Update `package.json` to match your language's requirements.

## Development

1. Edit `grammars/syntax.sublime-syntax` for syntax highlighting
2. Update `package.json` with correct file extensions and LSP command
3. Test by copying to `~/.config/fresh/grammars/` and restarting Fresh

**Tip:** Search GitHub for existing `<language> sublime-syntax` files you can adapt.
If using an existing grammar, check its license and include a copy in `grammars/LICENSE`.

## Grammar Attribution

<!-- If you used an existing grammar, add attribution here: -->
<!-- The syntax grammar is derived from [original](https://github.com/user/repo) -->
<!-- by Original Author, licensed under MIT. See `grammars/LICENSE` for details. -->

## Resources

- [Sublime Text Syntax Documentation](https://www.sublimetext.com/docs/syntax.html)
- [Scope Naming Conventions](https://www.sublimetext.com/docs/scope_naming.html)

## License

MIT
"#,
        name,
        if description.is_empty() {
            "Language support for Fresh."
        } else {
            description
        },
        name
    );
    std::fs::write(dir.join("README.md"), readme)?;

    Ok(())
}

// === Session persistence commands ===

/// List active sessions
fn list_sessions_command() -> AnyhowResult<()> {
    let socket_dir = SocketPaths::socket_directory()?;

    if !socket_dir.exists() {
        println!("No active sessions.");
        return Ok(());
    }

    let mut sessions = Vec::new();
    let mut stale_cleaned = 0;

    for entry in std::fs::read_dir(&socket_dir)? {
        let entry = entry?;
        let path = entry.path();
        let filename = path.file_name().and_then(|s| s.to_str()).unwrap_or("");

        // Look for control sockets (*.ctrl.sock)
        if let Some(name) = filename.strip_suffix(".ctrl.sock") {
            // Get socket paths for this session to check if server is alive
            let socket_paths = SocketPaths::for_session_name(name)?;

            // Check if server is actually running, clean up if stale
            if socket_paths.cleanup_if_stale() {
                stale_cleaned += 1;
                continue;
            }

            // Only show sessions with running servers
            if !socket_paths.is_server_alive() {
                continue;
            }

            // Try to decode the session name (for working-dir based sessions)
            // Only show the decoded path if it looks like a real absolute path
            let display_name = if let Some(decoded_path) = workspace::decode_filename_to_path(name)
            {
                // Only use decoded path if it has more than one component
                // (i.e., not just "/<name>" which happens with simple session names)
                if decoded_path.components().count() > 2 {
                    decoded_path.display().to_string()
                } else {
                    name.to_string()
                }
            } else {
                name.to_string()
            };

            sessions.push((name.to_string(), display_name));
        }
    }

    if stale_cleaned > 0 {
        eprintln!("Cleaned up {} stale session(s).", stale_cleaned);
    }

    if sessions.is_empty() {
        println!("No active sessions.");
    } else {
        println!("Active sessions:");
        for (id, display) in sessions {
            println!("  {} ({})", display, id);
        }
        println!();
        println!("Attach with: fresh session attach [NAME]  or  fresh -a [NAME]");
    }

    Ok(())
}

/// Kill a session (terminate the server)
fn kill_session_command(session: Option<&str>, args: &Args) -> AnyhowResult<()> {
    use fresh::server::ipc::ClientConnection;
    use fresh::server::protocol::ClientControl;

    let working_dir = std::env::current_dir()?;

    // Determine session name: explicit arg > --session-name flag > working dir
    let socket_paths = match session.or(args.session_name.as_deref()) {
        Some(name) => SocketPaths::for_session_name(name)?,
        None => SocketPaths::for_working_dir(&working_dir)?,
    };

    if !socket_paths.data.exists() || !socket_paths.control.exists() {
        eprintln!("No session found to kill.");
        return Ok(());
    }

    // Connect and send quit command
    let conn = ClientConnection::connect(&socket_paths)?;

    // We need to do a minimal handshake first
    use fresh::server::protocol::{ClientHello, TermSize};
    let hello = ClientHello::new(TermSize::new(80, 24));
    let hello_json = serde_json::to_string(&ClientControl::Hello(hello))?;
    conn.write_control(&hello_json)?;

    // Read server response (we don't care about version mismatch here)
    let _ = conn.read_control()?;

    // Send quit command
    let quit_msg = serde_json::to_string(&ClientControl::Quit)?;
    conn.write_control(&quit_msg)?;

    // Wait for server to close the connection (indicates shutdown)
    conn.set_data_nonblocking(false)?;
    let mut buf = [0u8; 1024];
    let timeout = std::time::Duration::from_secs(5);
    let start = std::time::Instant::now();

    // Read until EOF or timeout
    while start.elapsed() < timeout {
        match conn.read_data(&mut buf) {
            Ok(0) => break,    // EOF - server closed connection
            Ok(_) => continue, // Keep draining
            Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                std::thread::sleep(std::time::Duration::from_millis(50));
            }
            Err(_) => break, // Error - connection closed
        }
    }

    // Clean up stale socket files if they still exist
    std::thread::sleep(std::time::Duration::from_millis(100));
    if socket_paths.data.exists() {
        // Best-effort cleanup of stale socket files
        #[allow(clippy::let_underscore_must_use)]
        let _ = std::fs::remove_file(&socket_paths.data);
    }
    if socket_paths.control.exists() {
        // Best-effort cleanup of stale socket files
        #[allow(clippy::let_underscore_must_use)]
        let _ = std::fs::remove_file(&socket_paths.control);
    }

    println!("Session terminated.");
    Ok(())
}

/// Run as a daemon server
fn run_server_command(args: &Args) -> AnyhowResult<()> {
    use fresh::server::{EditorServer, EditorServerConfig};

    // Initialize tracing to stderr (will go to log file when spawned detached)
    use tracing_subscriber::{fmt, EnvFilter};
    let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("debug"));
    fmt()
        .with_env_filter(filter)
        .with_writer(std::io::stderr)
        .with_ansi(false)
        .init();

    eprintln!(
        "[server] Starting server process for session {:?}",
        args.session_name
    );

    let working_dir = std::env::current_dir()?;
    eprintln!("[server] Working directory: {:?}", working_dir);

    let dir_context = fresh::config_io::DirectoryContext::from_system()?;

    // Load editor config
    eprintln!("[server] Loading editor config...");
    let editor_config = if let Some(config_path) = &args.config {
        config::Config::load_from_file(config_path)?
    } else {
        config::Config::load_with_layers(&dir_context, &working_dir)
    };
    eprintln!("[server] Editor config loaded");

    let config = EditorServerConfig {
        working_dir: working_dir.clone(),
        session_name: args.session_name.clone(),
        idle_timeout: Some(std::time::Duration::from_secs(3600)), // 1 hour default
        editor_config,
        dir_context,
        plugins_enabled: !args.no_plugins,
    };

    eprintln!("[server] Creating EditorServer...");
    let mut server = match EditorServer::new(config) {
        Ok(s) => {
            eprintln!("[server] EditorServer created successfully");
            s
        }
        Err(e) => {
            eprintln!("[server] EditorServer::new failed: {:?}", e);
            return Err(e.into());
        }
    };

    eprintln!("[server] Server ready at {:?}", server.socket_paths());
    tracing::info!("Editor server started at {:?}", server.socket_paths());

    // Run the server (blocking)
    eprintln!("[server] Entering main loop...");
    server.run()?;

    eprintln!("[server] Server shutting down");
    Ok(())
}

/// Open files in a running session without attaching
fn run_open_files_command(session_name: Option<&str>, files: &[String]) -> AnyhowResult<()> {
    use fresh::server::daemon::is_process_running;
    use fresh::server::protocol::{
        ClientControl, ClientHello, FileRequest, ServerControl, TermSize, PROTOCOL_VERSION,
    };
    use fresh::server::spawn_server_detached;

    if files.is_empty() {
        eprintln!("No files specified.");
        return Ok(());
    }

    let working_dir = std::env::current_dir()?;

    // Build file requests BEFORE starting server, filtering out directories
    let mut file_requests: Vec<FileRequest> = Vec::new();
    let mut skipped_dirs = 0;

    for f in files {
        let loc = parse_file_location(f);
        // Resolve relative paths to absolute paths based on client's working directory
        let abs_path = if loc.path.is_relative() {
            working_dir.join(&loc.path)
        } else {
            loc.path.clone()
        };
        // Canonicalize to resolve symlinks and normalize
        let canonical_path = abs_path.canonicalize().unwrap_or(abs_path);

        if canonical_path.is_dir() {
            skipped_dirs += 1;
            eprintln!("Skipping directory: {}", canonical_path.display());
            continue;
        }

        file_requests.push(FileRequest {
            path: canonical_path.to_string_lossy().to_string(),
            line: loc.line,
            column: loc.column,
            end_line: loc.end_line,
            end_column: loc.end_column,
            message: loc.message,
        });
    }

    // Check if we have any files to open BEFORE starting the server
    if file_requests.is_empty() {
        if skipped_dirs > 0 {
            eprintln!("No files to open (only directories were specified).");
        }
        return Ok(());
    }

    // Determine socket paths based on session name or working directory
    let socket_paths = if let Some(name) = session_name {
        SocketPaths::for_session_name(name)?
    } else {
        SocketPaths::for_working_dir(&working_dir)?
    };

    // Clean up stale sockets if server is dead
    socket_paths.cleanup_if_stale();

    // Start server if not running (like nvr does by default)
    let server_was_started = if !socket_paths.is_server_alive() {
        let _pid = spawn_server_detached(session_name)?;

        // Wait for server to be ready
        loop {
            if let Ok(Some(pid)) = socket_paths.read_pid() {
                if is_process_running(pid) {
                    break;
                }
            }
            std::thread::yield_now();
        }
        true
    } else {
        false
    };

    // Connect to server
    let conn = fresh::server::ipc::ClientConnection::connect(&socket_paths)?;

    // Perform handshake
    let hello = ClientHello::new(TermSize::new(80, 24)); // Size doesn't matter, we're not rendering
    let hello_json = serde_json::to_string(&ClientControl::Hello(hello))?;
    conn.write_control(&hello_json)?;

    // Read server response
    let response = conn
        .read_control()?
        .ok_or_else(|| anyhow::anyhow!("Server closed connection during handshake"))?;

    let server_msg: ServerControl = serde_json::from_str(&response)?;

    match server_msg {
        ServerControl::Hello(server_hello) => {
            if server_hello.protocol_version != PROTOCOL_VERSION {
                eprintln!(
                    "Version mismatch: server is v{}",
                    server_hello.server_version
                );
                return Ok(());
            }
        }
        ServerControl::VersionMismatch(mismatch) => {
            eprintln!("Version mismatch: server is v{}", mismatch.server_version);
            return Ok(());
        }
        ServerControl::Error { message } => {
            return Err(anyhow::anyhow!("Server error: {}", message));
        }
        _ => {
            return Err(anyhow::anyhow!("Unexpected server response"));
        }
    }

    // Send OpenFiles command
    let msg = serde_json::to_string(&ClientControl::OpenFiles {
        files: file_requests.clone(),
    })?;
    conn.write_control(&msg)?;

    if server_was_started {
        eprintln!(
            "Started new session and opened {} file(s).",
            file_requests.len()
        );
        // Exit code 2 signals caller to spawn a terminal with attached client
        std::process::exit(EXIT_NEW_SESSION);
    } else {
        eprintln!("Opened {} file(s) in session.", file_requests.len());
    }
    Ok(())
}

/// Attach to an existing session, starting a server if needed
fn run_attach_command(args: &Args) -> AnyhowResult<()> {
    use crossterm::terminal::enable_raw_mode;
    use fresh::server::protocol::{
        ClientControl, ClientHello, ServerControl, TermSize, PROTOCOL_VERSION,
    };
    use fresh::server::spawn_server_detached;

    // Initialize tracing to a file for debugging
    use tracing_subscriber::{fmt, EnvFilter};
    let log_path = fresh::services::log_dirs::log_dir()
        .join(format!("fresh-client-{}.log", std::process::id()));
    let log_file = std::fs::File::create(&log_path).ok();
    if let Some(file) = log_file {
        let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("debug"));
        // Best-effort: tracing subscriber may already be set
        #[allow(clippy::let_underscore_must_use)]
        let _ = fmt()
            .with_env_filter(filter)
            .with_writer(std::sync::Mutex::new(file))
            .with_ansi(false)
            .try_init();
    }

    let working_dir = std::env::current_dir()?;

    // Determine socket paths based on session name or working directory
    let socket_paths = if let Some(ref name) = args.session_name {
        SocketPaths::for_session_name(name)?
    } else {
        SocketPaths::for_working_dir(&working_dir)?
    };

    // Clean up stale sockets if server is dead
    if socket_paths.cleanup_if_stale() {
        eprintln!("Cleaned up stale session.");
    }

    // Check if a server is running, if not start one
    let server_was_started = if !socket_paths.is_server_alive() {
        eprintln!("Starting server...");

        // Spawn server in background
        let _pid = spawn_server_detached(args.session_name.as_deref())?;
        true
    } else {
        false
    };

    // Get terminal size
    let (cols, rows) = crossterm::terminal::size()?;

    // Wait for server to be ready - the PID file is the semantic signal
    // that the server has successfully bound and is ready to accept connections.
    if server_was_started {
        use fresh::server::daemon::is_process_running;

        // Wait for PID file to appear with a valid running PID
        // This is the semantic condition: server writes PID after bind() succeeds
        loop {
            if let Ok(Some(pid)) = socket_paths.read_pid() {
                if is_process_running(pid) {
                    break; // Server is ready
                }
            }
            // Yield to scheduler - we're waiting for an event (PID file creation),
            // not delaying for time. The yield is just to avoid busy-spinning.
            std::thread::yield_now();
        }
    }

    // Now connect - server is ready
    let conn = fresh::server::ipc::ClientConnection::connect(&socket_paths)?;

    if server_was_started {
        eprintln!("Server started.");
    }

    let term_size = TermSize::new(cols, rows);

    // Perform handshake
    let hello = ClientHello::new(term_size);
    let hello_json = serde_json::to_string(&ClientControl::Hello(hello))?;
    conn.write_control(&hello_json)?;

    // Read server response
    let response = conn
        .read_control()?
        .ok_or_else(|| anyhow::anyhow!("Server closed connection during handshake"))?;

    let server_msg: ServerControl = serde_json::from_str(&response)?;

    match server_msg {
        ServerControl::Hello(server_hello) => {
            if server_hello.protocol_version != PROTOCOL_VERSION {
                eprintln!(
                    "Version mismatch: server is v{}",
                    server_hello.server_version
                );
                eprintln!("Please restart the server with the same version as the client.");
                return Ok(());
            }
            tracing::info!(
                "Connected to session '{}' (server {})",
                server_hello.session_id,
                server_hello.server_version
            );
        }
        ServerControl::VersionMismatch(mismatch) => {
            eprintln!("Version mismatch: server is v{}", mismatch.server_version);
            eprintln!("Please restart the server with the same version as the client.");
            return Ok(());
        }
        ServerControl::Error { message } => {
            return Err(anyhow::anyhow!("Server error: {}", message));
        }
        _ => {
            return Err(anyhow::anyhow!("Unexpected server response"));
        }
    }

    // Continue to relay loop

    // Enable raw mode - the server sends terminal setup sequences (alternate screen, etc.)
    // but we need raw mode so key presses are forwarded immediately
    enable_raw_mode()?;

    // Run the client relay loop (handshake already done)
    let result = client::run_client_relay(conn);

    // Best-effort: restore terminal state before printing any messages.
    // The server sends terminal setup sequences (alternate screen, mouse capture, etc.)
    // through us, so we must undo all of them, not just raw mode.
    fresh::services::terminal_modes::emergency_cleanup();

    // Handle result
    match result {
        Ok(client::ClientExitReason::ServerQuit) => {
            tracing::debug!("Client exit: ServerQuit");
        }
        Err(e) => {
            tracing::debug!("Client error: {}", e);
            return Err(e.into());
        }
        Ok(client::ClientExitReason::Detached) => {
            tracing::debug!("Client exit: Detached");
            eprintln!("Detached from session. Server continues running.");
            eprintln!("Reattach with: fresh -a  or  fresh session attach");
        }
        Ok(client::ClientExitReason::VersionMismatch { server_version }) => {
            tracing::debug!("Client exit: VersionMismatch");
            eprintln!("Version mismatch: server is v{}", server_version);
            eprintln!("Please restart the server with the same version as the client.");
        }
        Ok(client::ClientExitReason::Error(e)) => {
            tracing::debug!("Client exit: Error({})", e);
            eprintln!("Connection error: {}", e);
            return Err(e.into());
        }
    }

    Ok(())
}

/// Print deprecation warnings for old CLI flags
fn print_deprecation_warnings(cli: &Cli) {
    // Only print warnings if no --cmd is used (i.e., using deprecated flags directly)
    if !cli.cmd.is_empty() {
        return;
    }

    // These flags existed in master and are now reorganized into --cmd commands
    if cli.dump_config {
        eprintln!("warning: --dump-config is deprecated, use `fresh --cmd config show` instead");
    }
    if cli.show_paths {
        eprintln!("warning: --show-paths is deprecated, use `fresh --cmd config paths` instead");
    }
    if cli.init.is_some() {
        eprintln!("warning: --init is deprecated, use `fresh --cmd init` instead");
    }
}

fn main() -> AnyhowResult<()> {
    real_main()
}

fn real_main() -> AnyhowResult<()> {
    let cli = Cli::parse();

    // Print deprecation warnings for old flags
    print_deprecation_warnings(&cli);

    // Convert to legacy Args format for compatibility
    let args: Args = cli.into();

    // Handle --show-paths early (no terminal setup needed)
    if args.show_paths {
        let dir_context = fresh::config_io::DirectoryContext::from_system()?;
        fresh::services::log_dirs::print_all_paths(&dir_context);
        return Ok(());
    }

    // Handle --dump-config early (no terminal setup needed)
    if args.dump_config {
        let dir_context = fresh::config_io::DirectoryContext::from_system()?;
        let working_dir = std::env::current_dir().unwrap_or_default();
        let config = if let Some(config_path) = &args.config {
            match config::Config::load_from_file(config_path) {
                Ok(cfg) => cfg,
                Err(e) => {
                    eprintln!(
                        "Error: Failed to load config from {}: {}",
                        config_path.display(),
                        e
                    );
                    anyhow::bail!(
                        "Failed to load config from {}: {}",
                        config_path.display(),
                        e
                    );
                }
            }
        } else {
            config::Config::load_with_layers(&dir_context, &working_dir)
        };

        // Pretty-print the config as JSON
        match serde_json::to_string_pretty(&config) {
            Ok(json) => {
                println!("{}", json);
                return Ok(());
            }
            Err(e) => {
                eprintln!("Error: Failed to serialize config: {}", e);
                anyhow::bail!("Failed to serialize config: {}", e);
            }
        }
    }

    // Handle --check-plugin early (no terminal setup needed)
    #[cfg(feature = "plugins")]
    if let Some(plugin_path) = &args.check_plugin {
        return check_plugin_bundle(plugin_path);
    }

    // Handle --init early (no terminal setup needed)
    if let Some(ref pkg_type) = args.init {
        return init_package_command(pkg_type.clone());
    }

    // Handle --list-sessions early (no terminal setup needed)
    if args.list_sessions {
        return list_sessions_command();
    }

    // Handle --kill: terminate a session
    if let Some(ref session) = args.kill {
        return kill_session_command(session.as_deref(), &args);
    }

    // Handle --server: run as daemon server
    if args.server {
        return run_server_command(&args);
    }

    // Handle open-file in session: send files to running session without attaching
    if let Some((session_name, files)) = &args.open_files_in_session {
        return run_open_files_command(session_name.as_deref(), files);
    }

    // Handle --attach: connect to existing session
    if args.attach {
        return run_attach_command(&args);
    }

    // Handle --gui: launch in native window mode (no terminal setup needed)
    #[cfg(feature = "gui")]
    if args.gui {
        return fresh::gui::run_gui(
            &args.files,
            args.no_plugins,
            args.config.as_ref(),
            args.locale.as_deref(),
            args.no_session,
            args.log_file.as_ref(),
        );
    }

    let SetupState {
        config,
        mut tracing_handles,
        mut terminal,
        terminal_size,
        file_locations,
        show_file_explorer,
        dir_context,
        current_working_dir: initial_working_dir,
        mut stdin_stream,
        key_translator,
        #[cfg(target_os = "linux")]
        gpm_client,
        #[cfg(not(target_os = "linux"))]
        gpm_client,
        mut terminal_modes,
        filesystem,
        process_spawner,
        _remote_session,
    } = initialize_app(&args).context("Failed to initialize application")?;

    let mut current_working_dir = initial_working_dir;
    let (terminal_width, terminal_height) = terminal_size;

    // Track whether this is the first run (for session restore, file open, etc.)
    let mut is_first_run = true;

    // Track whether we should restore workspace on restart (for project switching)
    let mut restore_workspace_on_restart = false;

    // Main editor loop - supports restarting with a new working directory
    // Returns (loop_result, last_update_result) tuple
    let (result, last_update_result) = loop {
        let first_run = is_first_run;
        let workspace_enabled = !args.no_session && file_locations.is_empty();

        // Detect terminal color capability
        let color_capability = fresh::view::color_support::ColorCapability::detect();

        // Use the filesystem created during initialization (supports both local and remote)
        let fs = filesystem.clone();

        let mut editor = Editor::with_working_dir(
            config.clone(),
            terminal_width,
            terminal_height,
            current_working_dir.clone(),
            dir_context.clone(),
            !args.no_plugins,
            color_capability,
            fs,
        )
        .context("Failed to create editor instance")?;

        // Set the process spawner (LocalProcessSpawner for local, RemoteProcessSpawner for remote)
        editor.set_process_spawner(process_spawner.clone());

        #[cfg(target_os = "linux")]
        if gpm_client.is_some() {
            editor.set_gpm_active(true);
        }

        if first_run {
            handle_first_run_setup(
                &mut editor,
                &args,
                &file_locations,
                show_file_explorer,
                &mut stdin_stream,
                &mut tracing_handles,
                workspace_enabled,
            )
            .context("Failed first run setup")?;
        } else {
            if restore_workspace_on_restart {
                match editor.try_restore_workspace() {
                    Ok(true) => {
                        tracing::info!("Workspace restored successfully");
                    }
                    Ok(false) => {
                        tracing::debug!("No previous workspace found");
                    }
                    Err(e) => {
                        tracing::warn!("Failed to restore workspace: {}", e);
                    }
                }
            }

            editor.show_file_explorer();
            let path = current_working_dir
                .as_ref()
                .map(|p| p.display().to_string())
                .unwrap_or_else(|| ".".to_string());
            editor.set_status_message(fresh::i18n::switched_to_project_message(&path));
        }

        if let Err(e) = editor.start_recovery_session() {
            tracing::warn!("Failed to start recovery session: {}", e);
        }

        let iteration = run_editor_iteration(
            &mut editor,
            workspace_enabled,
            &mut terminal,
            &key_translator,
            #[cfg(target_os = "linux")]
            &gpm_client,
        )
        .context("Editor iteration failed")?;

        let update_result = iteration.update_result;
        let restart_dir = iteration.restart_dir;
        let loop_result = iteration.loop_result;

        drop(editor);

        if let Some(new_dir) = restart_dir {
            tracing::info!(
                "Restarting editor with new working directory: {}",
                new_dir.display()
            );
            current_working_dir = Some(new_dir);
            is_first_run = false;
            restore_workspace_on_restart = true; // Restore workspace for the new project
            terminal
                .clear()
                .context("Failed to clear terminal for restart")?;
            continue;
        }

        break (loop_result, update_result);
    };

    // Restore terminal state
    terminal_modes.undo();

    // Check for updates after terminal is restored (using cached result)
    if let Some(update_result) = last_update_result {
        if update_result.update_available {
            eprintln!();
            eprintln!(
                "A new version of fresh is available: {} -> {}",
                release_checker::CURRENT_VERSION,
                update_result.latest_version
            );
            if let Some(cmd) = update_result.install_method.update_command() {
                eprintln!("Update with: {}", cmd);
            } else {
                eprintln!(
                    "Download from: https://github.com/sinelaw/fresh/releases/tag/v{}",
                    update_result.latest_version
                );
            }
            eprintln!();
        }
    }

    result.context("Editor loop returned an error")
}

/// Main event loop
#[cfg(target_os = "linux")]
fn run_event_loop(
    editor: &mut Editor,
    terminal: &mut Terminal<ratatui::backend::CrosstermBackend<io::Stdout>>,
    workspace_enabled: bool,
    key_translator: &KeyTranslator,
    gpm_client: &Option<GpmClient>,
) -> AnyhowResult<()> {
    run_event_loop_common(
        editor,
        terminal,
        workspace_enabled,
        key_translator,
        |timeout| poll_with_gpm(gpm_client.as_ref(), timeout),
    )
}

/// Main event loop (non-Linux version without GPM)
#[cfg(not(target_os = "linux"))]
fn run_event_loop(
    editor: &mut Editor,
    terminal: &mut Terminal<ratatui::backend::CrosstermBackend<io::Stdout>>,
    workspace_enabled: bool,
    key_translator: &KeyTranslator,
) -> AnyhowResult<()> {
    run_event_loop_common(
        editor,
        terminal,
        workspace_enabled,
        key_translator,
        |timeout| {
            if event_poll(timeout)? {
                Ok(Some(event_read()?))
            } else {
                Ok(None)
            }
        },
    )
}

fn run_event_loop_common<F>(
    editor: &mut Editor,
    terminal: &mut Terminal<ratatui::backend::CrosstermBackend<io::Stdout>>,
    workspace_enabled: bool,
    _key_translator: &KeyTranslator,
    mut poll_event: F,
) -> AnyhowResult<()>
where
    F: FnMut(Duration) -> AnyhowResult<Option<CrosstermEvent>>,
{
    use std::time::Instant;

    const FRAME_DURATION: Duration = Duration::from_millis(16); // 60fps
    let mut last_render = Instant::now();
    let mut needs_render = true;
    let mut pending_event: Option<CrosstermEvent> = None;

    loop {
        // Run shared per-tick housekeeping (async messages, timers, auto-save, etc.)
        {
            let _span = tracing::info_span!("editor_tick").entered();
            if fresh::app::editor_tick(editor, || {
                terminal.clear()?;
                Ok(())
            })? {
                needs_render = true;
            }
        }

        if editor.should_quit() {
            if workspace_enabled {
                if let Err(e) = editor.save_workspace() {
                    tracing::warn!("Failed to save workspace: {}", e);
                } else {
                    tracing::debug!("Workspace saved successfully");
                }
            }
            break;
        }

        if needs_render && last_render.elapsed() >= FRAME_DURATION {
            {
                let _span = tracing::info_span!("terminal_draw").entered();
                terminal.draw(|frame| editor.render(frame))?;
            }
            last_render = Instant::now();
            needs_render = false;
        }

        let event = if let Some(e) = pending_event.take() {
            Some(e)
        } else {
            let timeout = if needs_render {
                FRAME_DURATION.saturating_sub(last_render.elapsed())
            } else {
                Duration::from_millis(50)
            };

            poll_event(timeout)?
        };

        let Some(event) = event else { continue };

        let (event, next) = coalesce_mouse_moves(event)?;
        pending_event = next;

        // Event debug dialog receives ALL RAW events (before any translation or processing)
        // This is essential for diagnosing terminal keybinding issues
        if editor.is_event_debug_active() {
            if let CrosstermEvent::Key(key_event) = event {
                if key_event.kind == KeyEventKind::Press {
                    editor.handle_event_debug_input(&key_event);
                    needs_render = true;
                }
            }
            // Consume all events while event debug is active
            continue;
        }

        match event {
            CrosstermEvent::Key(key_event) => {
                if key_event.kind == KeyEventKind::Press {
                    let _span = tracing::trace_span!(
                        "handle_key",
                        code = ?key_event.code,
                        modifiers = ?key_event.modifiers,
                    )
                    .entered();
                    // Apply key translation (for input calibration)
                    // Use editor's translator so calibration changes take effect immediately
                    let translated_event = editor.key_translator().translate(key_event);
                    handle_key_event(editor, translated_event)?;
                    needs_render = true;
                }
            }
            CrosstermEvent::Mouse(mouse_event) => {
                if handle_mouse_event(editor, mouse_event)? {
                    needs_render = true;
                }
            }
            CrosstermEvent::Resize(w, h) => {
                editor.resize(w, h);
                needs_render = true;
            }
            CrosstermEvent::Paste(text) => {
                // External paste from terminal (bracketed paste mode)
                editor.paste_text(text);
                needs_render = true;
            }
            _ => {}
        }
    }

    Ok(())
}

/// Poll for events from both GPM and crossterm (Linux with libgpm available)
#[cfg(target_os = "linux")]
fn poll_with_gpm(
    gpm_client: Option<&GpmClient>,
    timeout: Duration,
) -> AnyhowResult<Option<CrosstermEvent>> {
    use nix::poll::{poll, PollFd, PollFlags, PollTimeout};
    use std::os::unix::io::{AsRawFd, BorrowedFd};

    // If no GPM client, just use crossterm polling
    let Some(gpm) = gpm_client else {
        return if event_poll(timeout)? {
            Ok(Some(event_read()?))
        } else {
            Ok(None)
        };
    };

    // Set up poll for both stdin (crossterm) and GPM fd
    let stdin_fd = std::io::stdin().as_raw_fd();
    let gpm_fd = gpm.fd();
    tracing::trace!("GPM poll: stdin_fd={}, gpm_fd={}", stdin_fd, gpm_fd);

    // SAFETY: We're borrowing the fds for the duration of the poll call
    let stdin_borrowed = unsafe { BorrowedFd::borrow_raw(stdin_fd) };
    let gpm_borrowed = unsafe { BorrowedFd::borrow_raw(gpm_fd) };

    let mut poll_fds = [
        PollFd::new(stdin_borrowed, PollFlags::POLLIN),
        PollFd::new(gpm_borrowed, PollFlags::POLLIN),
    ];

    // Convert timeout to milliseconds, clamping to u16::MAX (about 65 seconds)
    let timeout_ms = timeout.as_millis().min(u16::MAX as u128) as u16;
    let poll_timeout = PollTimeout::from(timeout_ms);
    let ready = poll(&mut poll_fds, poll_timeout)?;

    if ready == 0 {
        return Ok(None);
    }

    let stdin_revents = poll_fds[0].revents();
    let gpm_revents = poll_fds[1].revents();
    tracing::trace!(
        "GPM poll: ready={}, stdin_revents={:?}, gpm_revents={:?}",
        ready,
        stdin_revents,
        gpm_revents
    );

    // Check GPM first (mouse events are typically less frequent)
    if gpm_revents.is_some_and(|r| r.contains(PollFlags::POLLIN)) {
        tracing::trace!("GPM poll: GPM fd has data, reading event...");
        match gpm.read_event() {
            Ok(Some(gpm_event)) => {
                tracing::trace!(
                    "GPM event received: x={}, y={}, buttons={}, type=0x{:x}",
                    gpm_event.x,
                    gpm_event.y,
                    gpm_event.buttons.0,
                    gpm_event.event_type
                );
                if let Some(mouse_event) = gpm_to_crossterm(&gpm_event) {
                    tracing::trace!("GPM event converted to crossterm: {:?}", mouse_event);
                    return Ok(Some(CrosstermEvent::Mouse(mouse_event)));
                } else {
                    tracing::debug!("GPM event could not be converted to crossterm event");
                }
            }
            Ok(None) => {
                tracing::trace!("GPM poll: read_event returned None");
            }
            Err(e) => {
                tracing::warn!("GPM poll: read_event error: {}", e);
            }
        }
    }

    // Check stdin (crossterm events)
    if stdin_revents.is_some_and(|r| r.contains(PollFlags::POLLIN)) {
        // Use crossterm's read since it handles escape sequence parsing
        if event_poll(Duration::ZERO)? {
            return Ok(Some(event_read()?));
        }
    }

    Ok(None)
}

/// Handle a keyboard event
fn handle_key_event(editor: &mut Editor, key_event: KeyEvent) -> AnyhowResult<()> {
    // Trace the full key event
    tracing::trace!(
        "Key event received: code={:?}, modifiers={:?}, kind={:?}, state={:?}",
        key_event.code,
        key_event.modifiers,
        key_event.kind,
        key_event.state
    );

    // Log the keystroke
    let key_code = format!("{:?}", key_event.code);
    let modifiers = format!("{:?}", key_event.modifiers);
    editor.log_keystroke(&key_code, &modifiers);

    // Delegate to the editor's handle_key method
    editor.handle_key(key_event.code, key_event.modifiers)?;

    Ok(())
}

/// Handle a mouse event
/// Returns true if a re-render is needed
fn handle_mouse_event(editor: &mut Editor, mouse_event: MouseEvent) -> AnyhowResult<bool> {
    tracing::trace!(
        "Mouse event received: kind={:?}, column={}, row={}, modifiers={:?}",
        mouse_event.kind,
        mouse_event.column,
        mouse_event.row,
        mouse_event.modifiers
    );

    // Delegate to the editor's handle_mouse method
    editor
        .handle_mouse(mouse_event)
        .context("Failed to handle mouse event")
}

/// Skip stale mouse move events, return the latest one.
/// If we read a non-move event while draining, return it as pending.
fn coalesce_mouse_moves(
    event: CrosstermEvent,
) -> AnyhowResult<(CrosstermEvent, Option<CrosstermEvent>)> {
    use crossterm::event::MouseEventKind;

    // Only coalesce mouse moves
    if !matches!(&event, CrosstermEvent::Mouse(m) if m.kind == MouseEventKind::Moved) {
        return Ok((event, None));
    }

    let mut latest = event;
    while event_poll(Duration::ZERO)? {
        let next = event_read()?;
        if matches!(&next, CrosstermEvent::Mouse(m) if m.kind == MouseEventKind::Moved) {
            latest = next; // Newer move, skip the old one
        } else {
            return Ok((latest, Some(next))); // Hit a click/key, save it
        }
    }
    Ok((latest, None))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_file_location_simple_path() {
        let loc = parse_file_location("foo.txt");
        assert_eq!(loc.path, PathBuf::from("foo.txt"));
        assert_eq!(loc.line, None);
        assert_eq!(loc.column, None);
    }

    #[test]
    fn test_parse_multiple_files() {
        let inputs = ["file1.txt", "sub/file2.rs:10", "file3.cpp:20:5"];
        let locs: Vec<FileLocation> = inputs.iter().map(|i| parse_file_location(i)).collect();

        assert_eq!(locs.len(), 3);
        assert_eq!(locs[0].path, PathBuf::from("file1.txt"));
        assert_eq!(locs[0].line, None);
        assert_eq!(locs[0].column, None);
        assert_eq!(locs[1].path, PathBuf::from("sub/file2.rs"));
        assert_eq!(locs[1].line, Some(10));
        assert_eq!(locs[1].column, None);
        assert_eq!(locs[2].path, PathBuf::from("file3.cpp"));
        assert_eq!(locs[2].line, Some(20));
        assert_eq!(locs[2].column, Some(5));
    }

    #[test]
    fn test_parse_file_location_with_line() {
        let loc = parse_file_location("foo.txt:42");
        assert_eq!(loc.path, PathBuf::from("foo.txt"));
        assert_eq!(loc.line, Some(42));
        assert_eq!(loc.column, None);
    }

    #[test]
    fn test_parse_file_location_with_line_and_col() {
        let loc = parse_file_location("foo.txt:42:10");
        assert_eq!(loc.path, PathBuf::from("foo.txt"));
        assert_eq!(loc.line, Some(42));
        assert_eq!(loc.column, Some(10));
    }

    #[test]
    fn test_parse_file_location_absolute_path() {
        let loc = parse_file_location("/home/user/foo.txt:100:5");
        assert_eq!(loc.path, PathBuf::from("/home/user/foo.txt"));
        assert_eq!(loc.line, Some(100));
        assert_eq!(loc.column, Some(5));
    }

    #[test]
    fn test_parse_file_location_no_numbers_after_colon() {
        // If the suffix isn't a number, treat the whole thing as a path
        let loc = parse_file_location("foo:bar");
        assert_eq!(loc.path, PathBuf::from("foo:bar"));
        assert_eq!(loc.line, None);
        assert_eq!(loc.column, None);
    }

    #[test]
    fn test_parse_file_location_mixed_suffix() {
        // If only one part is a number, depends on position
        // "foo:10:bar" -> "bar" isn't a number, so no line:col parsing
        let loc = parse_file_location("foo:10:bar");
        assert_eq!(loc.path, PathBuf::from("foo:10:bar"));
        assert_eq!(loc.line, None);
        assert_eq!(loc.column, None);
    }

    #[test]
    fn test_parse_file_location_line_only_not_col() {
        // "foo:bar:10" -> "10" is col, "bar" isn't line, so no parsing
        let loc = parse_file_location("foo:bar:10");
        assert_eq!(loc.path, PathBuf::from("foo:bar:10"));
        assert_eq!(loc.line, None);
        assert_eq!(loc.column, None);
    }

    // Tests for parse_location (local vs remote detection)

    #[test]
    fn test_parse_location_local_simple() {
        let loc = parse_location("file.txt");
        match loc {
            ParsedLocation::Local(fl) => {
                assert_eq!(fl.path, PathBuf::from("file.txt"));
                assert_eq!(fl.line, None);
            }
            ParsedLocation::Remote(_) => panic!("Expected local, got remote"),
        }
    }

    #[test]
    fn test_parse_location_local_with_line() {
        let loc = parse_location("/path/to/file.rs:42");
        match loc {
            ParsedLocation::Local(fl) => {
                assert_eq!(fl.path, PathBuf::from("/path/to/file.rs"));
                assert_eq!(fl.line, Some(42));
            }
            ParsedLocation::Remote(_) => panic!("Expected local, got remote"),
        }
    }

    #[test]
    fn test_parse_location_remote_simple() {
        let loc = parse_location("user@host:/path/to/file.rs");
        match loc {
            ParsedLocation::Remote(rl) => {
                assert_eq!(rl.user, "user");
                assert_eq!(rl.host, "host");
                assert_eq!(rl.path, "/path/to/file.rs");
                assert_eq!(rl.line, None);
                assert_eq!(rl.column, None);
            }
            ParsedLocation::Local(_) => panic!("Expected remote, got local"),
        }
    }

    #[test]
    fn test_parse_location_remote_with_line() {
        let loc = parse_location("alice@server.com:/home/alice/project/main.rs:42");
        match loc {
            ParsedLocation::Remote(rl) => {
                assert_eq!(rl.user, "alice");
                assert_eq!(rl.host, "server.com");
                assert_eq!(rl.path, "/home/alice/project/main.rs");
                assert_eq!(rl.line, Some(42));
                assert_eq!(rl.column, None);
            }
            ParsedLocation::Local(_) => panic!("Expected remote, got local"),
        }
    }

    #[test]
    fn test_parse_location_remote_with_line_and_col() {
        let loc = parse_location("bob@example.org:src/lib.rs:100:25");
        match loc {
            ParsedLocation::Remote(rl) => {
                assert_eq!(rl.user, "bob");
                assert_eq!(rl.host, "example.org");
                assert_eq!(rl.path, "src/lib.rs");
                assert_eq!(rl.line, Some(100));
                assert_eq!(rl.column, Some(25));
            }
            ParsedLocation::Local(_) => panic!("Expected remote, got local"),
        }
    }

    #[test]
    fn test_parse_location_remote_relative_path() {
        let loc = parse_location("user@host:relative/path/file.txt");
        match loc {
            ParsedLocation::Remote(rl) => {
                assert_eq!(rl.user, "user");
                assert_eq!(rl.host, "host");
                assert_eq!(rl.path, "relative/path/file.txt");
            }
            ParsedLocation::Local(_) => panic!("Expected remote, got local"),
        }
    }

    #[test]
    fn test_parse_location_email_like_not_remote() {
        // An email-like string without a path should be treated as local
        let loc = parse_location("user@host");
        match loc {
            ParsedLocation::Local(fl) => {
                assert_eq!(fl.path, PathBuf::from("user@host"));
            }
            ParsedLocation::Remote(_) => panic!("Expected local, got remote"),
        }
    }

    #[test]
    fn test_parse_location_at_in_path_local() {
        // A local path that happens to contain @ should still be local
        let loc = parse_location("/path/with@sign/file.txt");
        match loc {
            ParsedLocation::Local(fl) => {
                assert_eq!(fl.path, PathBuf::from("/path/with@sign/file.txt"));
            }
            ParsedLocation::Remote(_) => panic!("Expected local, got remote"),
        }
    }

    // Tests for range selection and message parsing

    #[test]
    fn test_parse_file_location_line_range() {
        let loc = parse_file_location("file.txt:13-16");
        assert_eq!(loc.path, PathBuf::from("file.txt"));
        assert_eq!(loc.line, Some(13));
        assert_eq!(loc.column, None);
        assert_eq!(loc.end_line, Some(16));
        assert_eq!(loc.end_column, None);
        assert_eq!(loc.message, None);
    }

    #[test]
    fn test_parse_file_location_full_range() {
        let loc = parse_file_location("file.txt:13:17-21:1");
        assert_eq!(loc.path, PathBuf::from("file.txt"));
        assert_eq!(loc.line, Some(13));
        assert_eq!(loc.column, Some(17));
        assert_eq!(loc.end_line, Some(21));
        assert_eq!(loc.end_column, Some(1));
        assert_eq!(loc.message, None);
    }

    #[test]
    fn test_parse_file_location_line_range_with_message() {
        let loc = parse_file_location("file.txt:13-16@\"hello world\"");
        assert_eq!(loc.path, PathBuf::from("file.txt"));
        assert_eq!(loc.line, Some(13));
        assert_eq!(loc.end_line, Some(16));
        assert_eq!(loc.message, Some("hello world".to_string()));
    }

    #[test]
    fn test_parse_file_location_point_with_message() {
        let loc = parse_file_location("file.txt:13:5@\"msg\"");
        assert_eq!(loc.path, PathBuf::from("file.txt"));
        assert_eq!(loc.line, Some(13));
        assert_eq!(loc.column, Some(5));
        assert_eq!(loc.end_line, None);
        assert_eq!(loc.end_column, None);
        assert_eq!(loc.message, Some("msg".to_string()));
    }

    #[test]
    fn test_parse_file_location_full_range_with_message() {
        let loc = parse_file_location("file.txt:13:17-21:1@\"explanation\"");
        assert_eq!(loc.path, PathBuf::from("file.txt"));
        assert_eq!(loc.line, Some(13));
        assert_eq!(loc.column, Some(17));
        assert_eq!(loc.end_line, Some(21));
        assert_eq!(loc.end_column, Some(1));
        assert_eq!(loc.message, Some("explanation".to_string()));
    }

    #[test]
    fn test_parse_file_location_message_with_escaped_quotes() {
        let loc = parse_file_location(r#"file.txt:5@"say \"hello\"""#);
        assert_eq!(loc.path, PathBuf::from("file.txt"));
        assert_eq!(loc.line, Some(5));
        assert_eq!(loc.message, Some("say \"hello\"".to_string()));
    }

    #[test]
    fn test_parse_file_location_empty_message() {
        let loc = parse_file_location("file.txt:5@\"\"");
        assert_eq!(loc.path, PathBuf::from("file.txt"));
        assert_eq!(loc.line, Some(5));
        assert_eq!(loc.message, Some("".to_string()));
    }

    #[test]
    fn test_parse_file_location_line_only_with_message() {
        let loc = parse_file_location("file.txt:10@\"check this\"");
        assert_eq!(loc.path, PathBuf::from("file.txt"));
        assert_eq!(loc.line, Some(10));
        assert_eq!(loc.column, None);
        assert_eq!(loc.end_line, None);
        assert_eq!(loc.message, Some("check this".to_string()));
    }

    #[test]
    fn test_parse_file_location_absolute_path_with_range() {
        let loc = parse_file_location("/home/user/file.txt:5-10");
        assert_eq!(loc.path, PathBuf::from("/home/user/file.txt"));
        assert_eq!(loc.line, Some(5));
        assert_eq!(loc.end_line, Some(10));
    }

    #[test]
    fn test_parse_file_location_no_range_fields_for_simple() {
        let loc = parse_file_location("foo.txt:42:10");
        assert_eq!(loc.end_line, None);
        assert_eq!(loc.end_column, None);
        assert_eq!(loc.message, None);
    }

    #[test]
    fn test_extract_message_suffix() {
        let (rest, msg) = extract_message_suffix("file.txt:10@\"hello\"");
        assert_eq!(rest, "file.txt:10");
        assert_eq!(msg, Some("hello".to_string()));
    }

    #[test]
    fn test_extract_message_suffix_no_message() {
        let (rest, msg) = extract_message_suffix("file.txt:10");
        assert_eq!(rest, "file.txt:10");
        assert_eq!(msg, None);
    }
}

// Property tests use Unix-style path generation strategy, skip on Windows
// where path parsing differs (drive letters like C: conflict with :line:col parsing)
#[cfg(all(test, not(windows)))]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    /// Generate a valid Unix-style file path (no colons in path components)
    fn unix_path_strategy() -> impl Strategy<Value = String> {
        prop::collection::vec("[a-zA-Z0-9._-]+", 1..5).prop_map(|components| components.join("/"))
    }

    proptest! {
        /// Property: If we construct "path:line:col", we should get back the path, line, and col
        #[test]
        fn roundtrip_line_col(
            path in unix_path_strategy(),
            line in 1usize..10000,
            col in 1usize..1000
        ) {
            let input = format!("{}:{}:{}", path, line, col);
            let loc = parse_file_location(&input);

            prop_assert_eq!(loc.path, PathBuf::from(&path));
            prop_assert_eq!(loc.line, Some(line));
            prop_assert_eq!(loc.column, Some(col));
        }

        /// Property: If we construct "path:line", we should get back the path and line
        #[test]
        fn roundtrip_line_only(
            path in unix_path_strategy(),
            line in 1usize..10000
        ) {
            let input = format!("{}:{}", path, line);
            let loc = parse_file_location(&input);

            prop_assert_eq!(loc.path, PathBuf::from(&path));
            prop_assert_eq!(loc.line, Some(line));
            prop_assert_eq!(loc.column, None);
        }

        /// Property: A path without any colon-number suffix returns the full path
        #[test]
        fn path_without_numbers_unchanged(
            path in unix_path_strategy()
        ) {
            let loc = parse_file_location(&path);

            prop_assert_eq!(loc.path, PathBuf::from(&path));
            prop_assert_eq!(loc.line, None);
            prop_assert_eq!(loc.column, None);
        }

        /// Property: line and column should always be non-zero when present
        /// (we parse as usize so 0 is valid, but the function doesn't filter)
        #[test]
        fn parsed_values_match_input(
            path in unix_path_strategy(),
            line in 0usize..10000,
            col in 0usize..1000
        ) {
            let input = format!("{}:{}:{}", path, line, col);
            let loc = parse_file_location(&input);

            prop_assert_eq!(loc.line, Some(line));
            prop_assert_eq!(loc.column, Some(col));
        }
    }
}
