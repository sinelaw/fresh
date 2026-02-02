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
    config,
    config_io::DirectoryContext,
    model::filesystem::{FileSystem, StdFileSystem},
    services::release_checker,
    services::remote,
    services::signal_handler,
    services::tracing_setup::TracingHandles,
};
use ratatui::Terminal;
use std::{
    io::{self, stdout},
    path::PathBuf,
    time::Duration,
};

/// A high-performance terminal text editor
#[derive(Parser, Debug)]
#[command(name = "fresh")]
#[command(about = "A terminal text editor with multi-cursor support", long_about = None)]
#[command(version)]
struct Args {
    /// Files to open. Supports line:col syntax (e.g., file.txt:10:5), remote paths
    /// (user@host:path), and "-" for stdin.
    #[arg(value_name = "FILES")]
    files: Vec<String>,

    /// Read content from stdin (alternative to using "-" as filename)
    #[arg(long)]
    stdin: bool,

    /// Disable plugin loading
    #[arg(long)]
    no_plugins: bool,

    /// Path to configuration file
    #[arg(long, value_name = "PATH")]
    config: Option<PathBuf>,

    /// Path to log file for editor diagnostics (default: system temp dir)
    #[arg(long, value_name = "PATH")]
    log_file: Option<PathBuf>,

    /// Enable event logging to the specified file
    #[arg(long, value_name = "LOG_FILE")]
    event_log: Option<PathBuf>,

    /// Don't restore previous session (start fresh)
    #[arg(long)]
    no_session: bool,

    /// Disable upgrade checking and anonymous telemetry
    #[arg(long)]
    no_upgrade_check: bool,

    /// Print the effective configuration as JSON and exit
    #[arg(long)]
    dump_config: bool,

    /// Print the directories used by Fresh and exit
    #[arg(long)]
    show_paths: bool,

    /// Override the locale (e.g., 'en', 'ja', 'zh-CN')
    #[arg(long, value_name = "LOCALE")]
    locale: Option<String>,

    /// Check a plugin by bundling it and printing the output (for debugging)
    #[arg(long, value_name = "PLUGIN_PATH")]
    check_plugin: Option<PathBuf>,

    /// Initialize a new package (plugin, theme, or language pack)
    #[arg(long, value_name = "TYPE")]
    init: Option<Option<String>>,
}

/// Parsed file location from CLI argument in file:line:col format
#[derive(Debug)]
struct FileLocation {
    path: PathBuf,
    line: Option<usize>,
    column: Option<usize>,
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

/// Placeholder for Windows (not yet implemented)
#[cfg(windows)]
pub struct StdinStreamState {
    pub temp_path: PathBuf,
    pub thread_handle: Option<std::thread::JoinHandle<anyhow::Result<()>>>,
}

// TODO(windows): Implement stdin streaming for Windows
// - Use GetStdHandle(STD_INPUT_HANDLE) to get stdin handle
// - Use DuplicateHandle to duplicate the pipe handle before reopening as CONIN$
// - Spawn background thread to read from duplicated handle and write to temp file
// - Use SetStdHandle or reopen CONIN$ as stdin for keyboard input
#[cfg(windows)]
fn start_stdin_streaming() -> AnyhowResult<StdinStreamState> {
    anyhow::bail!(io::Error::new(
        io::ErrorKind::Unsupported,
        "Reading from stdin is not yet supported on Windows",
    ))
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

// TODO(windows): Implement reopening stdin from CONIN$
// - Open "CONIN$" which is the console input device
// - Use SetStdHandle(STD_INPUT_HANDLE, conin_handle) to replace stdin
// - This allows crossterm to receive keyboard events after stdin was a pipe
#[cfg(windows)]
fn reopen_stdin_from_tty() -> AnyhowResult<()> {
    anyhow::bail!(io::Error::new(
        io::ErrorKind::Unsupported,
        "Reading from stdin is not yet supported on Windows",
    ))
}

fn handle_first_run_setup(
    editor: &mut Editor,
    args: &Args,
    file_locations: &[FileLocation],
    show_file_explorer: bool,
    stdin_stream: &mut Option<StdinStreamState>,
    tracing_handles: &mut Option<TracingHandles>,
    session_enabled: bool,
) -> AnyhowResult<()> {
    if let Some(log_path) = &args.event_log {
        tracing::trace!("Event logging enabled: {}", log_path.display());
        editor.enable_event_streaming(log_path)?;
    }

    if let Some(handles) = tracing_handles.take() {
        editor.set_warning_log(handles.warning.receiver, handles.warning.path);
        editor.set_status_log_path(handles.status.path);
    }

    if session_enabled {
        match editor.try_restore_session() {
            Ok(true) => {
                tracing::info!("Session restored successfully");
            }
            Ok(false) => {
                tracing::debug!("No previous session found");
            }
            Err(e) => {
                tracing::warn!("Failed to restore session: {}", e);
            }
        }
    }

    // Handle stdin streaming (takes priority over files)
    // Opens with empty/partial buffer, content streams in background
    if let Some(mut stream_state) = stdin_stream.take() {
        tracing::info!("Opening stdin buffer from: {:?}", stream_state.temp_path);
        editor.open_stdin_buffer(&stream_state.temp_path, stream_state.thread_handle.take())?;
    }

    for loc in file_locations {
        if loc.path.is_dir() {
            continue;
        }
        tracing::info!("[SYNTAX DEBUG] CLI opening file: {:?}", loc.path);
        editor.open_file(&loc.path)?;

        if let Some(line) = loc.line {
            editor.goto_line_col(line, loc.column);
        }
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

/// Parse a file path that may include line and column information.
/// Supports formats:
/// - file.txt
/// - file.txt:10
/// - file.txt:10:5
/// - /path/to/file.txt:10:5
///
/// For Windows paths like C:\path\file.txt:10:5, we handle the drive letter
/// prefix properly using std::path APIs.
///
/// If the full path exists as a file, it's used as-is (handles files with colons in name).
fn parse_file_location(input: &str) -> FileLocation {
    use std::path::{Component, Path};

    let full_path = PathBuf::from(input);

    // If the full path exists as a file, use it directly
    // This handles edge cases like files named "foo:10"
    if full_path.is_file() {
        return FileLocation {
            path: full_path,
            line: None,
            column: None,
        };
    }

    // Check if the path has a Windows drive prefix using std::path
    let has_prefix = Path::new(input)
        .components()
        .next()
        .map(|c| matches!(c, Component::Prefix(_)))
        .unwrap_or(false);

    // Calculate where to start looking for :line:col
    // For Windows paths with prefix (e.g., "C:"), skip past the drive letter and colon
    let search_start = if has_prefix {
        // Find the first colon (the drive letter separator) and skip it
        input.find(':').map(|i| i + 1).unwrap_or(0)
    } else {
        0
    };

    // Find the last colon(s) that could be line:col
    let suffix = &input[search_start..];

    // Try to parse from the end: look for :col and :line patterns
    // We work backwards to find numeric suffixes
    let parts: Vec<&str> = suffix.rsplitn(3, ':').collect();

    match parts.as_slice() {
        // Could be "col", "line", "rest" or just parts of the path
        [maybe_col, maybe_line, rest] => {
            if let (Ok(line), Ok(col)) = (maybe_line.parse::<usize>(), maybe_col.parse::<usize>()) {
                // Both parsed as numbers: file:line:col
                let path_str = if has_prefix {
                    format!("{}{}", &input[..search_start], rest)
                } else {
                    rest.to_string()
                };
                return FileLocation {
                    path: PathBuf::from(path_str),
                    line: Some(line),
                    column: Some(col),
                };
            }
            // Fall through - not valid line:col format
        }
        // Could be "line", "rest" or just parts of the path
        [maybe_line, rest] => {
            if let Ok(line) = maybe_line.parse::<usize>() {
                // Parsed as number: file:line
                let path_str = if has_prefix {
                    format!("{}{}", &input[..search_start], rest)
                } else {
                    rest.to_string()
                };
                return FileLocation {
                    path: PathBuf::from(path_str),
                    line: Some(line),
                    column: None,
                };
            }
            // Fall through - not valid line format
        }
        _ => {}
    }

    // No valid line:col suffix found, treat the whole thing as a path
    FileLocation {
        path: full_path,
        line: None,
        column: None,
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
    session_enabled: bool,
    terminal: &mut Terminal<ratatui::backend::CrosstermBackend<io::Stdout>>,
    key_translator: &KeyTranslator,
    #[cfg(target_os = "linux")] gpm_client: &Option<GpmClient>,
) -> AnyhowResult<IterationOutcome> {
    #[cfg(target_os = "linux")]
    let loop_result = run_event_loop(
        editor,
        terminal,
        session_enabled,
        key_translator,
        gpm_client,
    );
    #[cfg(not(target_os = "linux"))]
    let loop_result = run_event_loop(editor, terminal, session_enabled, key_translator);

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

fn main() -> AnyhowResult<()> {
    // Parse command-line arguments
    let args = Args::parse();

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

    // Track whether we should restore session on restart (for project switching)
    let mut restore_session_on_restart = false;

    // Main editor loop - supports restarting with a new working directory
    // Returns (loop_result, last_update_result) tuple
    let (result, last_update_result) = loop {
        let first_run = is_first_run;
        let session_enabled = !args.no_session && file_locations.is_empty();

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
                session_enabled,
            )
            .context("Failed first run setup")?;
        } else {
            if restore_session_on_restart {
                match editor.try_restore_session() {
                    Ok(true) => {
                        tracing::info!("Session restored successfully");
                    }
                    Ok(false) => {
                        tracing::debug!("No previous session found");
                    }
                    Err(e) => {
                        tracing::warn!("Failed to restore session: {}", e);
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
            session_enabled,
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
            restore_session_on_restart = true; // Restore session for the new project
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
    session_enabled: bool,
    key_translator: &KeyTranslator,
    gpm_client: &Option<GpmClient>,
) -> AnyhowResult<()> {
    run_event_loop_common(
        editor,
        terminal,
        session_enabled,
        key_translator,
        |timeout| poll_with_gpm(gpm_client.as_ref(), timeout),
    )
}

/// Main event loop (non-Linux version without GPM)
#[cfg(not(target_os = "linux"))]
fn run_event_loop(
    editor: &mut Editor,
    terminal: &mut Terminal<ratatui::backend::CrosstermBackend<io::Stdout>>,
    session_enabled: bool,
    key_translator: &KeyTranslator,
) -> AnyhowResult<()> {
    run_event_loop_common(
        editor,
        terminal,
        session_enabled,
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
    session_enabled: bool,
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
        // Process async messages and poll for file changes (auto-revert, file tree)
        if editor.process_async_messages() {
            needs_render = true;
        }

        // Check mouse hover timer for LSP hover requests
        if editor.check_mouse_hover_timer() {
            needs_render = true;
        }

        // Check semantic highlight debounce timer
        if editor.check_semantic_highlight_timer() {
            needs_render = true;
        }

        // Check completion trigger timer (debounced quick suggestions)
        if editor.check_completion_trigger_timer() {
            needs_render = true;
        }

        // Check for warnings and open warning log if any occurred
        if editor.check_warning_log() {
            needs_render = true;
        }

        // Poll stdin streaming progress (if active)
        if editor.poll_stdin_streaming() {
            needs_render = true;
        }

        if let Err(e) = editor.auto_save_dirty_buffers() {
            tracing::debug!("Auto-save error: {}", e);
        }

        // Handle hard redraw requests (e.g. after returning from sudo)
        if editor.take_full_redraw_request() {
            terminal.clear()?;
            needs_render = true;
        }

        if editor.should_quit() {
            if session_enabled {
                if let Err(e) = editor.save_session() {
                    tracing::warn!("Failed to save session: {}", e);
                } else {
                    tracing::debug!("Session saved successfully");
                }
            }
            break;
        }

        if needs_render && last_render.elapsed() >= FRAME_DURATION {
            terminal.draw(|frame| editor.render(frame))?;
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
