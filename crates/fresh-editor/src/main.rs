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
    app::Editor, client, config, config_io::DirectoryContext, server::SocketPaths,
    services::release_checker, services::remote, services::signal_handler,
    services::tracing_setup::TracingHandles, workspace,
};
use ratatui::Terminal;
use std::{
    io::{self, stdout},
    path::{Path, PathBuf},
    time::Duration,
};

/// A terminal text editor with multi-cursor support
#[derive(Parser, Debug)]
#[command(name = "fresh")]
#[command(version, propagate_version = true)]
#[command(after_help = concat!(
    "Commands (use --cmd):\n",
    "  config show               Print effective configuration\n",
    "  config paths              Show directories used by Fresh\n",
    "  grammar list              List all available grammars (with source info)\n",
    "  init                      Initialize a new plugin/theme/language\n",
    "\n",
    "Session commands:\n",
    "  session list              List active sessions\n",
    "  session attach [NAME]     Attach to a session (NAME or current dir)\n",
    "  session new NAME          Start a new named session\n",
    "  session kill [NAME]       Terminate a session\n",
    "  session open-file NAME FILES [--wait]   Open files in session (--wait blocks until done)\n",
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
    "Guided walkthrough with --wait:\n",
    "  The --wait flag blocks the CLI process until the user dismisses the popup\n",
    "  (if @\"message\" was given) or closes the buffer (if no message). This lets\n",
    "  a script or tool open files sequentially, waiting for the user to finish\n",
    "  with each one before moving on.\n",
    "\n",
    "  Use NAME '.' to target the session for the current working directory.\n",
    "  A session is started automatically if one isn't already running. When a\n",
    "  new session is started, the client attaches interactively (--wait is ignored).\n",
    "\n",
    "  To show a file with an annotation, combine range selection with @\"msg\":\n",
    "    fresh --cmd session open-file . 'src/main.rs:10-25@\"msg\"' --wait\n",
    "\n",
    "  The message supports markdown. Use real newlines (not \\n literals) in\n",
    "  the shell string for multi-line messages. For example with $'...':\n",
    "    fresh --cmd session open-file . \\\n",
    "      $'src/main.rs:10-25@\"**Title**\\nBody text here\"' --wait\n",
    "\n",
    "  To walk through multiple locations, run commands sequentially — each\n",
    "  one blocks until the user presses Escape (popup) or closes the buffer:\n",
    "    fresh --cmd session open-file . 'a.rs:1-10@\"Step 1\"' --wait\n",
    "    fresh --cmd session open-file . 'b.rs:5-20@\"Step 2\"' --wait\n",
    "    fresh --cmd session open-file . 'c.rs:30@\"Step 3\"'   --wait\n",
    "\n",
    "  Use as git's editor:\n",
    "    git config core.editor 'fresh --cmd session open-file . --wait'\n",
    "\n",
    "Documentation: https://getfresh.dev/docs"
))]
struct Cli {
    /// Run a command instead of opening files
    /// Commands: session (list|attach|new|kill|open-file), config (show|paths), grammar (list), init
    #[arg(long, num_args = 1.., value_name = "COMMAND", allow_hyphen_values = true)]
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

    /// Skip `~/.config/fresh/init.ts` for this launch
    #[arg(long)]
    no_init: bool,

    /// Safe mode: skip init.ts AND all plugins (recovery from a bad config)
    #[arg(long)]
    safe: bool,

    /// Path to configuration file
    #[arg(long, value_name = "PATH")]
    config: Option<PathBuf>,

    /// Path to log file for editor diagnostics
    #[arg(long, value_name = "PATH")]
    log_file: Option<PathBuf>,

    /// Enable event logging to the specified file
    #[arg(long, value_name = "LOG_FILE")]
    event_log: Option<PathBuf>,

    /// Don't restore previous workspace (only hot-exit content — unsaved
    /// modified files and unnamed buffers with content — is still restored
    /// so in-progress work is not lost)
    #[arg(long, alias = "no-session", conflicts_with = "restore")]
    no_restore: bool,

    /// Force restore of previous workspace, overriding
    /// `editor.restore_previous_session = false` in the config.  Cannot be
    /// combined with `--no-restore`.
    #[arg(long)]
    restore: bool,

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

    /// Remote SSH URL for server mode (internal, used by spawn_server_detached
    /// when the client was launched with `ssh://…` or `user@host:path`).  The
    /// server parses this, connects, and installs the result as
    /// `EditorServerConfig.startup_authority`.
    #[arg(long, hide = true, value_name = "URL")]
    ssh_url: Option<String>,

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
    no_init: bool,
    safe: bool,
    config: Option<PathBuf>,
    log_file: Option<PathBuf>,
    event_log: Option<PathBuf>,
    no_session: bool,
    /// Force workspace restore even if `editor.restore_previous_session`
    /// is disabled in the config.
    force_restore: bool,
    no_upgrade_check: bool,
    dump_config: bool,
    show_paths: bool,
    list_grammars: bool,
    locale: Option<String>,
    check_plugin: Option<PathBuf>,
    init: Option<Option<String>>,
    server: bool,
    /// Forwarded to the detached daemon by `spawn_server_detached`
    /// when the client saw an `ssh://` / scp-style remote in
    /// `files`.  Populated only for the daemon side.
    ssh_url: Option<String>,
    // Session-related fields (set via subcommands or -a shortcut)
    attach: bool,
    list_sessions: bool,
    session_name: Option<String>,
    kill: Option<Option<String>>,
    /// Open files in a session without attaching (session_name, files, wait)
    open_files_in_session: Option<(Option<String>, Vec<String>, bool)>,
    /// Launch in GUI mode
    #[cfg(feature = "gui")]
    gui: bool,
}

impl From<Cli> for Args {
    fn from(cli: Cli) -> Self {
        // Check for grammar list command before the main tuple parsing
        let list_grammars = if !cli.cmd.is_empty() {
            let cmd_args: Vec<&str> = cli.cmd.iter().map(|s| s.as_str()).collect();
            matches!(
                cmd_args.as_slice(),
                ["grammar", "list"] | ["grammars", "list"] | ["grammar", "ls"] | ["grammars"]
            )
        } else {
            false
        };

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
                // Open file in session: fresh --cmd session open-file <name> <files...> [--wait]
                ["session", "open-file", name, files @ ..]
                | ["s", "open-file", name, files @ ..] => {
                    let session = if *name == "." {
                        None
                    } else {
                        Some((*name).to_string())
                    };
                    let wait = files.contains(&"--wait");
                    let file_list: Vec<String> = files
                        .iter()
                        .filter(|s| **s != "--wait")
                        .map(|s| (*s).to_string())
                        .collect();
                    (
                        false,
                        None,
                        false,
                        None,
                        false,
                        false,
                        None,
                        vec![],
                        Some((session, file_list, wait)),
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
                // Grammar commands (handled via list_grammars flag above)
                ["grammar", "list"] | ["grammars", "list"] | ["grammar", "ls"] | ["grammars"] => (
                    false, None, false, None, false, false, None, cli.files, None,
                ),
                // Unknown command
                _ => {
                    eprintln!("Unknown command: {}", cli.cmd.join(" "));
                    eprintln!("Available commands: session (list|attach|new|kill|info|open-file), config (show|paths), grammar (list), init");
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

        // Safe mode implies no_plugins and no_init.
        let safe = cli.safe;
        let no_plugins = cli.no_plugins || safe;
        let no_init = cli.no_init || safe;

        Args {
            files,
            stdin: cli.stdin,
            no_plugins,
            no_init,
            safe,
            config: cli.config,
            log_file: cli.log_file,
            event_log: cli.event_log,
            no_session: cli.no_restore,
            force_restore: cli.restore,
            no_upgrade_check: cli.no_upgrade_check,
            dump_config,
            show_paths,
            list_grammars,
            locale: cli.locale,
            check_plugin: cli.check_plugin,
            init,
            server: cli.server,
            ssh_url: cli.ssh_url,
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

/// Parsed remote location from CLI argument.
///
/// Accepts two wire forms — both produce the same struct:
///
/// - scp-style: `user@host:path[:line[:col]]`
/// - URL-style: `ssh://[user@]host[:port]/path[:line[:col]]`
///
/// `user` is mandatory in the scp-style form and optional in the URL
/// form (defaults to `$USER` / `$USERNAME`). `port` is only reachable
/// through the URL form.
#[derive(Debug, Clone)]
struct RemoteLocation {
    user: String,
    host: String,
    port: Option<u16>,
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
    /// Single backend slot for "where does the editor act?".
    ///
    /// The editor always boots with `Authority::local()`. The SSH
    /// startup form (`fresh user@host:path`) replaces it with
    /// `Authority::ssh(...)` here. Devcontainer attach is now a plugin
    /// concern (it calls `editor.setAuthority({...})` from
    /// `plugins/devcontainer.ts` after `devcontainer up` returns) so
    /// startup never blocks on a container.
    authority: fresh::services::authority::Authority,
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
        DuplicateHandle, DUPLICATE_SAME_ACCESS, HANDLE, INVALID_HANDLE_VALUE,
    };
    use windows_sys::Win32::System::Console::GetStdHandle;
    use windows_sys::Win32::System::Console::STD_INPUT_HANDLE;
    use windows_sys::Win32::System::Threading::GetCurrentProcess;

    // Get the current stdin handle (which is a pipe)
    let stdin_handle = unsafe { GetStdHandle(STD_INPUT_HANDLE) };
    if stdin_handle == INVALID_HANDLE_VALUE || stdin_handle.is_null() {
        anyhow::bail!("Failed to get stdin handle");
    }

    // Duplicate the handle so we can read from it in a background thread
    // while we replace stdin with CONIN$ for keyboard input
    let mut duplicated_handle: HANDLE = std::ptr::null_mut();
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
    workspace_enabled: bool,
) -> AnyhowResult<()> {
    if let Some(log_path) = &args.event_log {
        tracing::trace!("Event logging enabled: {}", log_path.display());
        editor.enable_event_streaming(log_path)?;
    }
    // The warning-log channel and status-log path used to be wired up
    // here from `tracing_handles`; that wiring now lives in the main
    // loop so it survives editor restarts (e.g. devcontainer attach).

    let restore_full_session = workspace_enabled
        && (args.force_restore || editor.config().editor.restore_previous_session);

    if restore_full_session {
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
    } else {
        if !workspace_enabled {
            tracing::info!("Skipping workspace restore: --no-restore was specified");
        } else {
            tracing::info!(
                "Skipping workspace restore: editor.restore_previous_session is disabled"
            );
        }
        // Session restore opted out, but hot-exit content (unsaved
        // modified files + unnamed buffers with content) is still
        // restored so the user does not lose in-progress work.
        match editor.try_restore_hot_exit_buffers() {
            Ok(n) if n > 0 => {
                tracing::info!(
                    "Restored {} hot-exit buffer(s) despite skipping session restore",
                    n
                );
            }
            Ok(_) => {}
            Err(e) => {
                tracing::warn!("Failed to restore hot-exit buffers: {}", e);
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
    let mut has_cli_files = false;
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
            None,
        );
        has_cli_files = true;
    }

    // Schedule hot exit recovery for CLI-opened files (not covered by workspace restore)
    if has_cli_files {
        editor.schedule_hot_exit_recovery();
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
/// Build FileRequest structs from CLI file arguments, resolving paths relative to `working_dir`.
/// Directories are silently skipped.
fn build_file_requests(
    files: &[String],
    working_dir: &std::path::Path,
) -> Vec<fresh::server::protocol::FileRequest> {
    use fresh::server::protocol::FileRequest;
    let mut requests = Vec::new();
    for f in files {
        let loc = parse_file_location(f);
        let abs_path = if loc.path.is_relative() {
            working_dir.join(&loc.path)
        } else {
            loc.path.clone()
        };
        let canonical_path = abs_path.canonicalize().unwrap_or(abs_path);
        if canonical_path.is_dir() {
            continue;
        }
        requests.push(FileRequest {
            path: canonical_path.to_string_lossy().to_string(),
            line: loc.line,
            column: loc.column,
            end_line: loc.end_line,
            end_column: loc.end_column,
            message: loc.message,
        });
    }
    requests
}

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

/// Split a remote `path[:line[:col]]` tail.  Only strips the numeric
/// suffixes — anything non-numeric keeps the full string as the path
/// (no partial stripping).  Shared between scp-style and `ssh://`
/// parsing so the two forms agree on what counts as a trailing
/// line/column.
fn parse_path_with_line_col(path_and_rest: &str) -> (String, Option<usize>, Option<usize>) {
    let parts: Vec<&str> = path_and_rest.rsplitn(3, ':').collect();
    match parts.as_slice() {
        [maybe_col, maybe_line, rest] => {
            if let (Ok(line), Ok(col)) = (maybe_line.parse::<usize>(), maybe_col.parse::<usize>()) {
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
    }
}

/// Resolve the default SSH user when the `ssh://` URL omits one.
/// Uses `$USER` on Unix, `$USERNAME` on Windows.  Returns `None` when
/// neither is set — the caller treats the URL as malformed in that
/// case.
fn default_ssh_user() -> Option<String> {
    std::env::var("USER")
        .ok()
        .or_else(|| std::env::var("USERNAME").ok())
        .filter(|u| !u.is_empty())
}

/// Parse the part of an `ssh://` URL after the `ssh://` prefix.
/// Returns `None` for any shape we don't recognise (missing `/path`,
/// empty host, bad port, missing user with no `$USER` fallback).
fn parse_ssh_url_rest(rest: &str) -> Option<RemoteLocation> {
    // Authority and path are separated by the first `/`.  Missing
    // slash means no path, which we reject — consistent with the
    // scp-style branch that requires a non-empty path component.
    let (authority, path_and_rest) = rest.split_once('/')?;
    if path_and_rest.is_empty() {
        return None;
    }

    // Optional `user@` prefix on the authority.
    let (user, host_and_port) = match authority.split_once('@') {
        Some((u, rest)) if !u.is_empty() && !u.contains(' ') => (u.to_string(), rest),
        Some(_) => return None, // empty or space-bearing user
        None => (default_ssh_user()?, authority),
    };

    // Optional `:port` on the host.
    let (host, port) = match host_and_port.rsplit_once(':') {
        Some((h, p)) => {
            let parsed_port = p.parse::<u16>().ok()?;
            (h, Some(parsed_port))
        }
        None => (host_and_port, None),
    };

    if host.is_empty() || host.contains(' ') {
        return None;
    }

    let (path_tail, line, column) = parse_path_with_line_col(path_and_rest);
    // URL paths are always absolute (we consumed exactly one `/`
    // between authority and path).  Re-add it so callers see the
    // same absolute path they'd get from `ssh://host/etc/hosts`.
    let path = format!("/{}", path_tail);

    Some(RemoteLocation {
        user,
        host: host.to_string(),
        port,
        path,
        line,
        column,
    })
}

/// Render a `RemoteLocation` back into the canonical `ssh://` URL
/// form (no line/column — that's per-file metadata, not part of the
/// authority).  Used to forward a client-side remote spec to the
/// detached daemon via `spawn_server_detached`.
fn remote_location_to_ssh_url(remote: &RemoteLocation) -> String {
    // Paths on the remote are absolute by convention; if someone
    // gave us a relative one (scp-style allows this) preserve it by
    // dropping any leading `/` duplication.
    let path = remote.path.trim_start_matches('/');
    match remote.port {
        Some(port) => format!("ssh://{}@{}:{}/{}", remote.user, remote.host, port, path),
        None => format!("ssh://{}@{}/{}", remote.user, remote.host, path),
    }
}

/// Scan CLI `files` for remote specs.  Returns `Ok(Some(url))` when
/// every remote entry agrees on user/host/port (passed as
/// `--ssh-url` to the detached daemon), `Ok(None)` when no file is
/// remote, or an error when files straddle hosts.  The check mirrors
/// `initialize_app` in standalone mode so both entry points reject
/// the same bad inputs with the same message.
fn extract_ssh_url_from_files(files: &[String]) -> AnyhowResult<Option<String>> {
    let parsed: Vec<ParsedLocation> = files
        .iter()
        .filter(|f| *f != "-")
        .map(|f| parse_location(f))
        .collect();

    let remotes: Vec<&RemoteLocation> = parsed
        .iter()
        .filter_map(|loc| match loc {
            ParsedLocation::Remote(r) => Some(r),
            ParsedLocation::Local(_) => None,
        })
        .collect();

    if remotes.is_empty() {
        return Ok(None);
    }

    let first = remotes[0];
    for r in &remotes[1..] {
        if r.user != first.user || r.host != first.host || r.port != first.port {
            anyhow::bail!(
                "Cannot open files from multiple remote hosts. First: {}@{}, found: {}@{}",
                first.user,
                first.host,
                r.user,
                r.host
            );
        }
    }
    if parsed
        .iter()
        .any(|loc| matches!(loc, ParsedLocation::Local(_)))
    {
        anyhow::bail!(
            "Cannot mix local and remote files. Use either local paths or remote paths (ssh:// or user@host:path)."
        );
    }

    Ok(Some(remote_location_to_ssh_url(first)))
}

/// Parse a standalone `ssh://…` URL passed via the internal
/// `--ssh-url` flag.  Accepts only the URL form (not scp-style) and
/// the URL must carry a path; anything else is a hard error because
/// this input comes from our own `spawn_server_detached` and a
/// malformed URL there means we corrupted it on the way over.
fn parse_ssh_url_arg(url: &str) -> AnyhowResult<RemoteLocation> {
    let rest = url
        .strip_prefix("ssh://")
        .ok_or_else(|| anyhow::anyhow!("--ssh-url expects an ssh:// URL, got {:?}", url))?;
    parse_ssh_url_rest(rest)
        .ok_or_else(|| anyhow::anyhow!("--ssh-url is not a valid ssh:// URL: {:?}", url))
}

/// Parse a location that may be local, scp-style remote, or an
/// `ssh://` URL.
///
/// Accepted forms:
/// - local: `file`, `file:line`, `file:line:col`
/// - scp-style remote: `user@host:path[:line[:col]]`
/// - URL-style remote: `ssh://[user@]host[:port]/path[:line[:col]]`
///
/// When `ssh://` omits the user, the current login name (`$USER` /
/// `$USERNAME`) is used.  The URL form is the only way to pass a
/// port.  The path must be non-empty in both remote forms.
fn parse_location(input: &str) -> ParsedLocation {
    if let Some(rest) = input.strip_prefix("ssh://") {
        return match parse_ssh_url_rest(rest) {
            Some(loc) => ParsedLocation::Remote(loc),
            // Malformed `ssh://` — treat the whole input as a local
            // filename rather than letting scp-style parsing match
            // the `ssh://user@host:bad-port/...` slice and produce a
            // nonsense user like `ssh://user`.
            None => ParsedLocation::Local(parse_file_location(input)),
        };
    }

    // scp-style: user@host:path. Must have @ before the first : to
    // count as remote (skips Windows drive letters like `C:\...`).
    if let Some(at_pos) = input.find('@') {
        let user = &input[..at_pos];
        let after_at = &input[at_pos + 1..];

        if let Some(colon_pos) = after_at.find(':') {
            let host = &after_at[..colon_pos];
            let path_and_rest = &after_at[colon_pos + 1..];

            if !user.is_empty()
                && !host.is_empty()
                && !user.contains(' ')
                && !host.contains(' ')
                && !path_and_rest.is_empty()
            {
                let (path, line, column) = parse_path_with_line_col(path_and_rest);

                return ParsedLocation::Remote(RemoteLocation {
                    user: user.to_string(),
                    host: host.to_string(),
                    port: None,
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
    /// Background reconnect task handle - dropping this aborts the task
    _reconnect_handle: tokio::task::JoinHandle<()>,
}

/// Bundle of the startup authority plus any resources that must stay
/// alive for the duration of the session (currently only the SSH
/// connection handle and its reconnect task).
struct StartupAuthority {
    authority: fresh::services::authority::Authority,
    remote_session: Option<RemoteSession>,
}

/// Pick the startup authority. Per principle 6, defaults to
/// `Authority::local()`; SSH CLI form constructs the remote authority
/// directly. Devcontainer attach is no longer a core startup concern —
/// the TS plugin handles it post-boot via `editor.setAuthority(...)`.
fn create_startup_authority(
    remote_info: &Option<RemoteLocation>,
) -> AnyhowResult<StartupAuthority> {
    if let Some(remote) = remote_info {
        connect_remote(remote)
    } else {
        Ok(StartupAuthority {
            authority: fresh::services::authority::Authority::local(),
            remote_session: None,
        })
    }
}

/// Establish SSH connection to remote host and return `Authority::ssh`.
fn connect_remote(remote: &RemoteLocation) -> AnyhowResult<StartupAuthority> {
    // Create a Tokio runtime for the SSH connection
    let rt = tokio::runtime::Runtime::new()
        .context("Failed to create Tokio runtime for remote connection")?;

    let connection_params = remote::ConnectionParams {
        user: remote.user.clone(),
        host: remote.host.clone(),
        port: remote.port,
        identity_file: None,
    };

    match remote.port {
        Some(port) => eprintln!(
            "Connecting via SSH to {}@{}:{}...",
            remote.user, remote.host, port
        ),
        None => eprintln!("Connecting via SSH to {}@{}...", remote.user, remote.host),
    }

    // Establish SSH connection (this is async, so we block on it)
    let connection = rt
        .block_on(remote::SshConnection::connect(connection_params))
        .context(format!(
            "Failed to connect to remote host {}@{}",
            remote.user, remote.host
        ))?;

    let connection_string = connection.connection_string();
    let channel = connection.channel();
    let reconnect_params = connection.params().clone();

    tracing::info!("Connected to remote host: {}", connection_string);

    let filesystem = std::sync::Arc::new(remote::RemoteFileSystem::new(
        channel.clone(),
        connection_string,
    ));
    let process_spawner = std::sync::Arc::new(remote::RemoteProcessSpawner::new(channel.clone()));

    // Spawn background reconnect task on the runtime.
    // We need a runtime context for tokio::spawn inside spawn_reconnect_task.
    let reconnect_handle = {
        let _guard = rt.enter();
        remote::spawn_reconnect_task(channel, reconnect_params)
    };

    // SSH authority: leave the display label empty so the status bar
    // falls back to `filesystem.remote_connection_info()`, which knows
    // how to annotate the disconnect state.
    Ok(StartupAuthority {
        authority: fresh::services::authority::Authority::ssh(filesystem, process_spawner),
        remote_session: Some(RemoteSession {
            _connection: connection,
            _runtime: rt,
            _reconnect_handle: reconnect_handle,
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

    tracing::info!(
        "Editor starting (v{} {})",
        env!("CARGO_PKG_VERSION"),
        env!("FRESH_GIT_HASH")
    );

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

    // Pick the startup authority. Per principle 6, this is `local()`
    // by default; SSH CLI form swaps in `Authority::ssh(...)` here.
    // Devcontainer detection is intentionally NOT done here — it moved
    // into the devcontainer TS plugin so container attach runs
    // post-boot (see `plugins/devcontainer.ts` and principle 8).
    tracing::info!("Building startup authority...");
    let StartupAuthority {
        authority,
        remote_session,
    } = create_startup_authority(&remote_info)?;
    tracing::info!("Startup authority ready");

    let mut working_dir = None;
    let mut show_file_explorer = false;

    // Only set working_dir if exactly one parameter is passed and it's a directory
    if file_locations.len() == 1 {
        if let Some(first_loc) = file_locations.first() {
            // Use the filesystem to check if path is a directory
            // This works for both local and remote paths
            let is_directory = authority
                .filesystem
                .is_dir(&first_loc.path)
                .unwrap_or(false);
            if is_directory {
                working_dir = Some(first_loc.path.clone());
                show_file_explorer = true;
            }
        }
    }

    // Load config using the layered config system
    // For remote editing, use current local dir for config (remote doesn't have our config)
    tracing::info!("Loading config...");
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

    tracing::info!("Config loaded");

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

    tracing::info!("Loading directory context...");
    let dir_context = DirectoryContext::from_system()?;
    tracing::info!("Directory context loaded");
    let current_working_dir = working_dir;

    // Load key translator for input calibration
    tracing::info!("Loading key translator...");
    let key_translator = match KeyTranslator::load_from_config_dir(&dir_context.config_dir) {
        Ok(translator) => translator,
        Err(e) => {
            tracing::warn!("Failed to load key calibration: {}", e);
            KeyTranslator::new()
        }
    };

    tracing::info!("Key translator loaded, returning SetupState");
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
        authority,
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
    terminal_modes: &mut TerminalModes,
) -> AnyhowResult<IterationOutcome> {
    #[cfg(target_os = "linux")]
    let loop_result = run_event_loop(
        editor,
        terminal,
        workspace_enabled,
        key_translator,
        gpm_client,
        terminal_modes,
    );
    #[cfg(not(target_os = "linux"))]
    let loop_result = run_event_loop(
        editor,
        terminal,
        workspace_enabled,
        key_translator,
        terminal_modes,
    );

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

/// `fresh --cmd init check` — syntax-check ~/.config/fresh/init.ts via oxc.
/// Exits 0 if the file is absent or parses cleanly, 1 on any parse error.
fn init_check_command() -> AnyhowResult<()> {
    let dir_context = fresh::config_io::DirectoryContext::from_system()
        .context("failed to resolve config directory")?;
    let report = fresh::init_script::check(&dir_context.config_dir);

    let path_display = report.path.display();
    if report.ok {
        if report.diagnostics.is_empty() {
            // File may or may not exist — either way, nothing to complain about.
            println!("init.ts: ok ({path_display})");
        } else {
            // Warnings without errors. Still exit 0.
            for d in &report.diagnostics {
                eprintln!(
                    "{path_display}:{}:{}  warning  {}",
                    d.line, d.column, d.message
                );
            }
        }
        return Ok(());
    }

    for d in &report.diagnostics {
        let tag = match d.severity {
            fresh::init_script::CheckSeverity::Error => "error",
            fresh::init_script::CheckSeverity::Warning => "warning",
        };
        eprintln!(
            "{path_display}:{}:{}  {tag}  {}",
            d.line, d.column, d.message
        );
    }
    let errors = report
        .diagnostics
        .iter()
        .filter(|d| d.severity == fresh::init_script::CheckSeverity::Error)
        .count();
    eprintln!(
        "\n{errors} error{}. init.ts will not be evaluated until fixed.",
        if errors == 1 { "" } else { "s" }
    );
    std::process::exit(1);
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
fn write_validate_script(dir: &Path) -> AnyhowResult<()> {
    let validate_sh = r#"#!/bin/bash
# Validate package.json against the official Fresh package schema
#
# Prerequisite: pip install jsonschema
curl -sSL https://raw.githubusercontent.com/sinelaw/fresh/main/scripts/validate-package.sh | bash
"#;
    write_script_file(dir, "validate.sh", validate_sh)
}

/// Write a validation script for themes (validates both package.json and theme.json)
fn write_theme_validate_script(dir: &Path) -> AnyhowResult<()> {
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

fn write_script_file(dir: &Path, name: &str, content: &str) -> AnyhowResult<()> {
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

fn write_package_json(
    dir: &Path,
    name: &str,
    description: &str,
    author: &str,
    pkg_type: &str,
    default_description: &str,
    fresh_section: &str,
) -> AnyhowResult<()> {
    let desc = if description.is_empty() {
        default_description
    } else {
        description
    };
    let content = format!(
        r#"{{
  "$schema": "https://raw.githubusercontent.com/sinelaw/fresh/main/crates/fresh-editor/plugins/schemas/package.schema.json",
  "name": "{name}",
  "version": "0.1.0",
  "description": "{desc}",
  "type": "{pkg_type}",
  "author": "{author}",
  "license": "MIT",
  "fresh": {fresh_section}
}}
"#
    );
    std::fs::write(dir.join("package.json"), content)?;
    Ok(())
}

fn create_plugin_package(
    dir: &Path,
    name: &str,
    description: &str,
    author: &str,
) -> AnyhowResult<()> {
    write_package_json(
        dir,
        name,
        description,
        author,
        "plugin",
        "A Fresh plugin",
        r#"{
    "main": "plugin.ts"
  }"#,
    )?;

    // validate.sh
    write_validate_script(dir)?;

    // plugin.ts
    let plugin_ts = r#"// Fresh Plugin
// Documentation: https://github.com/user/fresh/blob/main/docs/plugins.md

const editor = getEditor();

// Define a command handler and register it
function hello(): void {
  editor.setStatus("Hello from your plugin!");
}
registerHandler("hello", hello);
editor.registerCommand("hello", "Say Hello", "hello");

// React to editor events
function onBufferOpened(): void {
  const bufferId = editor.getActiveBufferId();
  const info = editor.getBufferInfo(bufferId);
  if (info) {
    editor.debug(`Opened: ${info.path}`);
  }
}
registerHandler("on_buffer_opened", onBufferOpened);
editor.on("buffer_opened", "on_buffer_opened");

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
    dir: &Path,
    name: &str,
    description: &str,
    author: &str,
) -> AnyhowResult<()> {
    write_package_json(
        dir,
        name,
        description,
        author,
        "theme",
        "A Fresh theme",
        r#"{
    "theme": "theme.json"
  }"#,
    )?;

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
    dir: &Path,
    name: &str,
    description: &str,
    author: &str,
) -> AnyhowResult<()> {
    // Create grammars directory
    std::fs::create_dir_all(dir.join("grammars"))?;

    write_package_json(
        dir,
        name,
        description,
        author,
        "language",
        "Language support for Fresh",
        r#"{
    "grammar": {
      "file": "grammars/syntax.sublime-syntax",
      "extensions": ["ext"]
    },
    "language": {
      "commentPrefix": "//",
      "tabSize": 4,
      "autoIndent": true
    },
    "lsp": {
      "command": "language-server",
      "args": ["--stdio"],
      "autoStart": true
    }
  }"#,
    )?;

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
fn list_grammars_command() -> AnyhowResult<()> {
    let dir_context = fresh::config_io::DirectoryContext::from_system()?;
    let config_dir = dir_context.config_dir.clone();
    let registry = fresh::primitives::grammar::GrammarRegistry::for_editor(config_dir);
    // The unified catalog already includes tree-sitter-only languages
    // (e.g. TypeScript) alongside syntect grammars.
    let grammars = registry.available_grammar_info();

    if grammars.is_empty() {
        println!("No grammars available.");
        return Ok(());
    }

    // Find the longest name and short name for alignment
    let max_name_len = grammars.iter().map(|g| g.name.len()).max().unwrap_or(0);
    let max_short_len = grammars
        .iter()
        .map(|g| g.short_name.as_ref().map_or(0, |s| s.len()))
        .max()
        .unwrap_or(0)
        .max("SHORT NAME".len());

    println!(
        "{:<nw$}  {:<sw$}  {:<12}  EXTENSIONS",
        "GRAMMAR",
        "SHORT NAME",
        "SOURCE",
        nw = max_name_len,
        sw = max_short_len
    );
    println!(
        "{:<nw$}  {:<sw$}  {:<12}  ----------",
        "-------",
        "----------",
        "------",
        nw = max_name_len,
        sw = max_short_len
    );
    for grammar in &grammars {
        let extensions = if grammar.file_extensions.is_empty() {
            String::new()
        } else {
            grammar
                .file_extensions
                .iter()
                .map(|e| format!(".{}", e))
                .collect::<Vec<_>>()
                .join(", ")
        };
        let short = grammar.short_name.as_deref().unwrap_or("");
        println!(
            "{:<nw$}  {:<sw$}  {:<12}  {}",
            grammar.name,
            short,
            grammar.source.to_string(),
            extensions,
            nw = max_name_len,
            sw = max_short_len
        );
    }

    println!("\n{} grammars available.", grammars.len());
    println!("Use the grammar name or short name in config: languages -> <language> -> grammar");
    Ok(())
}

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
        for (id, display) in &sessions {
            if display != id {
                // Working-directory session: show path and usable name
                println!("  {}  (name: {})", display, id);
            } else {
                // Named session
                println!("  {}", id);
            }
        }
        println!();
        // Show the most convenient attach form for each session type
        if sessions.len() == 1 {
            let (id, display) = &sessions[0];
            if display != id {
                println!("Attach with: fresh -a  (from that directory)");
                println!("         or: fresh -a {}", id);
            } else {
                println!("Attach with: fresh -a {}", id);
            }
        } else {
            println!("Attach with: fresh -a [NAME]");
        }
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

    // Parse the optional `--ssh-url` the client forwarded via
    // `spawn_server_detached`.  If present, we connect here — before
    // building any editor config — so config loading can key off the
    // remote working directory.
    let remote_info = match args.ssh_url.as_deref() {
        Some(url) => Some(parse_ssh_url_arg(url)?),
        None => None,
    };

    let StartupAuthority {
        authority,
        remote_session,
    } = create_startup_authority(&remote_info)?;

    // Working directory: local cwd by default; remote path when the
    // daemon was spawned with an `--ssh-url`.  Config layering is
    // always keyed off the local cwd (the remote host doesn't have
    // our config layout), matching the standalone path.
    let working_dir = match &remote_info {
        Some(remote) => PathBuf::from(&remote.path),
        None => std::env::current_dir()?,
    };
    let config_dir = std::env::current_dir()?;
    eprintln!("[server] Working directory: {:?}", working_dir);

    let dir_context = fresh::config_io::DirectoryContext::from_system()?;

    // Load editor config
    eprintln!("[server] Loading editor config...");
    let editor_config = if let Some(config_path) = &args.config {
        config::Config::load_from_file(config_path)?
    } else {
        config::Config::load_with_layers(&dir_context, &config_dir)
    };
    eprintln!("[server] Editor config loaded");

    let session_keepalive: Option<Box<dyn std::any::Any + Send>> =
        remote_session.map(|rs| Box::new(rs) as Box<dyn std::any::Any + Send>);
    let startup_authority = if remote_info.is_some() {
        Some(authority)
    } else {
        None
    };

    let config = EditorServerConfig {
        working_dir: working_dir.clone(),
        session_name: args.session_name.clone(),
        idle_timeout: Some(std::time::Duration::from_secs(3600)), // 1 hour default
        editor_config,
        dir_context,
        plugins_enabled: !args.no_plugins,
        init_enabled: !args.no_init,
        startup_authority,
        session_keepalive,
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

/// Resolve a session name to socket paths.
///
/// When `session_name` is `None`, uses the current working directory.
/// When it looks like a filesystem path (absolute or `.`-relative), tries to
/// resolve it as a working-directory session first, falling back to a literal
/// named session.  This lets users pass paths like `/home/user/project` to
/// target sessions that were started with `fresh -a` in that directory.
fn resolve_session(session_name: Option<&str>) -> anyhow::Result<SocketPaths> {
    let working_dir = std::env::current_dir()?;

    match session_name {
        None => Ok(SocketPaths::for_working_dir(&working_dir)?),
        Some(name) => {
            // If the name looks like a path, try working-dir resolution first.
            let path = std::path::Path::new(name);
            if path.is_absolute() || name.contains('/') || name.contains('\\') {
                // Canonicalize so that e.g. `/home/user/project/` and
                // `/home/user/project` both match.
                let canonical = std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
                let by_dir = SocketPaths::for_working_dir(&canonical)?;
                if by_dir.is_server_alive() {
                    return Ok(by_dir);
                }
            }

            // Fall back to literal session name lookup.
            Ok(SocketPaths::for_session_name(name)?)
        }
    }
}

/// Open files in a running session without attaching
fn run_open_files_command(
    session_name: Option<&str>,
    files: &[String],
    wait: bool,
) -> AnyhowResult<()> {
    use fresh::server::daemon::is_process_running;
    use fresh::server::protocol::{
        ClientControl, ClientHello, ServerControl, TermSize, PROTOCOL_VERSION,
    };
    use fresh::server::spawn_server_detached;

    if files.is_empty() {
        eprintln!("No files specified.");
        return Ok(());
    }

    // Strip any `ssh://` / `user@host:path` specs off the file list
    // before they reach `build_file_requests` — those entries become
    // the daemon's authority, not local path requests.  An existing
    // server keeps its authority; we don't re-attach through a
    // remote URL after the fact.
    let ssh_url = extract_ssh_url_from_files(files)?;
    let local_files: Vec<String> = if ssh_url.is_some() {
        files
            .iter()
            .filter_map(|f| match parse_location(f) {
                ParsedLocation::Remote(r) => Some(r.path),
                ParsedLocation::Local(_) => None,
            })
            .collect()
    } else {
        files.to_vec()
    };

    let working_dir = std::env::current_dir()?;
    let file_requests = build_file_requests(&local_files, &working_dir);

    if file_requests.is_empty() {
        eprintln!("No files to open (only directories were specified).");
        return Ok(());
    }

    // Determine socket paths based on session name or working directory
    let socket_paths = resolve_session(session_name)?;

    // Clean up stale sockets if server is dead
    socket_paths.cleanup_if_stale();

    // Start server if not running (like nvr does by default)
    let server_was_started = if !socket_paths.is_server_alive() {
        let _pid = spawn_server_detached(session_name, ssh_url.as_deref())?;

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
        wait,
    })?;
    conn.write_control(&msg)?;

    if server_was_started {
        // We just started the server and already sent the OpenFiles command
        // above.  If we have a controlling terminal, attach interactively so
        // the user can see the editor.  Otherwise (pipes, scripts, non-tty
        // contexts) just report success — the server is running headless and
        // the files have been queued.
        drop(conn);
        if std::io::IsTerminal::is_terminal(&std::io::stdin()) {
            return run_attach(session_name, &[]);
        } else {
            eprintln!(
                "Started new session and opened {} file(s). Attach with: fresh -a{}",
                file_requests.len(),
                session_name.map_or(String::new(), |n| format!(" {}", n)),
            );
            return Ok(());
        }
    } else if wait {
        // Existing session — block until the server sends WaitComplete
        loop {
            match conn.read_control() {
                Ok(Some(line)) => {
                    if let Ok(msg) = serde_json::from_str::<ServerControl>(&line) {
                        match msg {
                            ServerControl::WaitComplete => break,
                            ServerControl::Quit { .. } => break,
                            _ => {} // Ignore other messages
                        }
                    }
                }
                Ok(None) => break, // Server closed connection
                Err(_) => break,   // Connection error
            }
        }
    } else {
        eprintln!("Opened {} file(s) in session.", file_requests.len());
    }
    Ok(())
}

/// Attach to an existing session, starting a server if needed
fn run_attach_command(args: &Args) -> AnyhowResult<()> {
    run_attach(args.session_name.as_deref(), &args.files)
}

fn run_attach(session_name: Option<&str>, files: &[String]) -> AnyhowResult<()> {
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

    // If the user passed any remote specs, the URL becomes the
    // daemon's startup authority — and any subsequent `OpenFiles`
    // carries only the path components (see filtering below).  We
    // only consume the URL when *starting* a server; attaching to
    // an existing session ignores it.
    let ssh_url = extract_ssh_url_from_files(files)?;

    // Determine socket paths based on session name or working directory
    let socket_paths = resolve_session(session_name)?;

    // Clean up stale sockets if server is dead
    if socket_paths.cleanup_if_stale() {
        eprintln!("Cleaned up stale session.");
    }

    // Check if a server is running, if not start one
    let server_was_started = if !socket_paths.is_server_alive() {
        eprintln!("Starting server...");

        // Spawn server in background
        let _pid = spawn_server_detached(session_name, ssh_url.as_deref())?;
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

    // Send file open requests if any files were specified on the
    // command line.  When `ssh_url` was extracted above the file
    // list carried remote specs; strip them to bare paths so the
    // daemon opens them through its (SSH) authority.
    if !files.is_empty() {
        let local_files: Vec<String> = if ssh_url.is_some() {
            files
                .iter()
                .filter_map(|f| match parse_location(f) {
                    ParsedLocation::Remote(r) => Some(r.path),
                    ParsedLocation::Local(_) => None,
                })
                .collect()
        } else {
            files.to_vec()
        };
        let file_requests = build_file_requests(&local_files, &working_dir);
        if !file_requests.is_empty() {
            let msg = serde_json::to_string(&ClientControl::OpenFiles {
                files: file_requests,
                wait: false,
            })?;
            conn.write_control(&msg)?;
        }
    }

    // Continue to relay loop

    // Save original console mode before anything modifies it
    #[cfg(windows)]
    let original_console_mode = fresh_winterm::save_console_mode();

    // Enable raw mode - the server sends terminal setup sequences (alternate screen, etc.)
    // but we need raw mode so key presses are forwarded immediately
    enable_raw_mode()?;

    // Run the client relay loop (handshake already done)
    let result = client::run_client_relay(conn);

    // Best-effort: restore terminal state before printing any messages.
    // The server sends terminal setup sequences (alternate screen, mouse capture, etc.)
    // through us, so we must undo all of them, not just raw mode.
    fresh::services::terminal_modes::emergency_cleanup();

    // Restore original console mode AFTER all cleanup to ensure Quick Edit
    // mode is properly restored on Windows.
    #[cfg(windows)]
    let _ = fresh_winterm::restore_console_mode(original_console_mode);

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
    match real_main() {
        Ok(()) => Ok(()),
        Err(e) => {
            // SSH connection errors are expected user-facing failures, not bugs.
            // Print a clean error message without the stack backtrace.
            if e.downcast_ref::<remote::SshError>().is_some()
                || e.chain()
                    .any(|cause| cause.downcast_ref::<remote::SshError>().is_some())
            {
                eprintln!("Error: {:#}", e);
                std::process::exit(1);
            }
            Err(e)
        }
    }
}

fn real_main() -> AnyhowResult<()> {
    // Enable backtraces for error reporting if not already set.
    // Errors that crash the editor are bugs — backtraces help diagnose them.
    if std::env::var_os("RUST_BACKTRACE").is_none() {
        std::env::set_var("RUST_BACKTRACE", "1");
    }

    let cli = Cli::parse();

    // Print deprecation warnings for old flags
    print_deprecation_warnings(&cli);

    // Convert to legacy Args format for compatibility
    let args: Args = cli.into();

    // Expose `FRESH_INTERACTIVE=1` on the editor's process env when Fresh
    // is launched as a human-interactive editor (stdin is a TTY, not a
    // CLI sub-command, not --stdin / --attach / --server). init.ts (and
    // plugins in general) read this via getEnv to branch on "real"
    // launches — e.g., skip heavy workflow-setup under $GIT_EDITOR. See
    // design §5 / §6.2.
    let interactive = std::io::IsTerminal::is_terminal(&std::io::stdin())
        && !args.stdin
        && !args.attach
        && !args.server
        && !args.list_sessions
        && args.kill.is_none()
        && args.open_files_in_session.is_none()
        && args.init.is_none()
        && !args.list_grammars
        && !args.dump_config
        && !args.show_paths
        && args.check_plugin.is_none();
    if interactive {
        std::env::set_var("FRESH_INTERACTIVE", "1");
    }

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

    // Handle grammar list early (no terminal setup needed)
    if args.list_grammars {
        return list_grammars_command();
    }

    // Handle --check-plugin early (no terminal setup needed)
    #[cfg(feature = "plugins")]
    if let Some(plugin_path) = &args.check_plugin {
        return check_plugin_bundle(plugin_path);
    }

    // Handle --init early (no terminal setup needed).
    // `--cmd init check` is a hidden sub-route on the same flag.
    if let Some(ref pkg_type) = args.init {
        if let Some(subcmd) = pkg_type {
            if subcmd == "check" {
                return init_check_command();
            }
        }
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
    if let Some((session_name, files, wait)) = &args.open_files_in_session {
        return run_open_files_command(session_name.as_deref(), files, *wait);
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
            args.no_init,
            args.config.as_ref(),
            args.locale.as_deref(),
            args.no_session,
            args.log_file.as_ref(),
        );
    }

    // Save the original console mode BEFORE anything modifies it (raw mode,
    // enable_vt_input, etc.). Restored at the very end after all cleanup.
    #[cfg(windows)]
    let original_console_mode = fresh_winterm::save_console_mode();

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
        authority: startup_authority,
        _remote_session,
    } = initialize_app(&args).context("Failed to initialize application")?;

    let mut current_working_dir = initial_working_dir;
    let (terminal_width, terminal_height) = terminal_size;

    // Track whether this is the first run (for session restore, file open, etc.)
    let mut is_first_run = true;

    // Track whether we should restore workspace on restart (for project switching)
    let mut restore_workspace_on_restart = false;

    // Authority that will drive the next `Editor` constructed in the
    // loop. Starts from the startup authority (local or SSH); when a
    // plugin calls `editor.setAuthority(...)` the previous Editor
    // stashes the new authority in its `pending_authority` slot, which
    // we consume right before dropping it below.
    let mut current_authority = startup_authority;

    // Status-message log path is just a clone-able path — capture it
    // once and re-bind to every restarted editor instance. Without
    // this, the post-`setAuthority` editor has no path to point the
    // "click status bar to view log" action at, and the user sees
    // "status log not available" for every status message after the
    // restart.
    let status_log_path: Option<PathBuf> = tracing_handles.as_ref().map(|h| h.status.path.clone());

    // Warning-log channel survives across restarts the same way,
    // except the `Receiver<()>` is single-consumer and can't be
    // cloned: lift the whole `(receiver, path)` pair out of the
    // editor before we drop it, and reinstall it on the next one.
    // Seeded here from `tracing_handles` (which then no longer carries
    // the warning slot), and topped up post-iteration via
    // `editor.take_warning_log()`.
    let mut warning_log_slot: Option<(std::sync::mpsc::Receiver<()>, PathBuf)> = tracing_handles
        .take()
        .map(|h| (h.warning.receiver, h.warning.path));

    // Main editor loop - supports restarting with a new working directory
    // Returns (loop_result, last_update_result) tuple
    let (result, last_update_result) = loop {
        let first_run = is_first_run;
        let workspace_enabled = !args.no_session;

        // Detect terminal color capability
        let color_capability = fresh::view::color_support::ColorCapability::detect();

        // The editor constructor still takes a filesystem (tests use
        // it to inject mocks). The authority we want is installed
        // immediately after construction via `set_boot_authority`, so
        // that later init — plugin loading, session restore, the
        // first event-loop tick — sees the real backend.
        let fs = current_authority.filesystem.clone();

        tracing::info!("Creating editor instance...");
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
        tracing::info!("Editor instance created");

        // Install the real authority before any plugin / init.ts code
        // runs, so everything that loads below sees the correct
        // backend from the first tick.
        editor.set_boot_authority(current_authority.clone());

        // User init.ts: auto-load from ~/.config/fresh/init.ts through the
        // same pipeline as "Load Plugin from Buffer". Respects `--no-init`
        // and `--safe`, and is short-circuited by the crash fuse after
        // repeated failures.
        editor.load_init_script(!args.no_init);

        // All plugins (registry + init.ts) have loaded — fire the
        // plugins_loaded lifecycle hook so init.ts `on("plugins_loaded",
        // fn)` callbacks can configure plugins via getPluginApi.
        editor.fire_plugins_loaded_hook();

        #[cfg(target_os = "linux")]
        if gpm_client.is_some() {
            editor.set_gpm_active(true);
        }

        // Re-wire the tracing log paths into every editor instance,
        // not just the first. Status-bar click → open log, warning
        // indicator click → open log all break otherwise after the
        // first authority swap restart.
        if let Some(p) = status_log_path.as_ref() {
            editor.set_status_log_path(p.clone());
        }
        if let Some((rx, p)) = warning_log_slot.take() {
            editor.set_warning_log(rx, p);
        }

        if first_run {
            tracing::info!("Running first-run setup...");
            handle_first_run_setup(
                &mut editor,
                &args,
                &file_locations,
                show_file_explorer,
                &mut stdin_stream,
                workspace_enabled,
            )
            .context("Failed first run setup")?;
            tracing::info!("First-run setup complete");
        } else {
            if restore_workspace_on_restart {
                if args.force_restore || editor.config().editor.restore_previous_session {
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
                } else {
                    tracing::info!(
                        "Skipping workspace restore on restart: editor.restore_previous_session is disabled"
                    );
                    // Session restore opted out, but hot-exit content
                    // for the newly-switched project is still restored.
                    match editor.try_restore_hot_exit_buffers() {
                        Ok(n) if n > 0 => {
                            tracing::info!(
                                "Restored {} hot-exit buffer(s) on restart despite skipping session restore",
                                n
                            );
                        }
                        Ok(_) => {}
                        Err(e) => {
                            tracing::warn!("Failed to restore hot-exit buffers on restart: {}", e);
                        }
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

        // Drain any CLI file arguments that were queued by
        // `initialize_app` BEFORE firing the ready hook, so plugins
        // listening on `ready` see those buffers as already open. Left
        // to the event loop, ready would fire first and plugins that
        // branch on "is there a real file open?" (e.g. the dashboard)
        // would race the file in and open on top of it.
        editor.process_pending_file_opens();

        // Workspace restored, initial buffers opened, recovery session up —
        // fire the `ready` lifecycle hook (design M2, §3.3 phase 3) before
        // handing off to the event loop.
        editor.fire_ready_hook();

        let iteration = run_editor_iteration(
            &mut editor,
            workspace_enabled,
            &mut terminal,
            &key_translator,
            #[cfg(target_os = "linux")]
            &gpm_client,
            &mut terminal_modes,
        )
        .context("Editor iteration failed")?;

        let update_result = iteration.update_result;
        let restart_dir = iteration.restart_dir;
        let loop_result = iteration.loop_result;

        // If a plugin called `editor.setAuthority(...)` (or cleared it)
        // during this iteration, the editor parked the replacement in
        // `pending_authority` and triggered a restart. Move it into
        // the loop-local var *before* dropping the editor so the next
        // iteration builds against the new backend.
        if let Some(new_authority) = editor.take_pending_authority() {
            tracing::info!("Authority transition queued; restarting editor");
            current_authority = new_authority;
        }

        // Pluck the warning-log channel back out of the soon-to-be-
        // dropped editor so the next iteration can re-bind it.
        warning_log_slot = editor.take_warning_log();

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

    // Restore the original console mode AFTER all other cleanup (crossterm's
    // disable_raw_mode, DisableMouseCapture, etc.) to ensure Quick Edit mode
    // is properly restored. Without this, text selection with mouse doesn't
    // work in Windows Terminal after exiting fresh.
    #[cfg(windows)]
    let _ = fresh_winterm::restore_console_mode(original_console_mode);

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

/// Handle a pending suspend request from the editor.
///
/// Tears down the TUI, raises SIGTSTP so the user drops back to the shell,
/// and on resume (`fg`) re-enables the modes we just undid and asks for a
/// full redraw. Windows doesn't have Unix job control, so the action is a
/// no-op there beyond a status message.
fn handle_suspend_request(
    editor: &mut Editor,
    terminal_modes: &mut TerminalModes,
) -> AnyhowResult<()> {
    #[cfg(unix)]
    {
        let keyboard_config = KeyboardConfig {
            disambiguate_escape_codes: editor.config().editor.keyboard_disambiguate_escape_codes,
            report_event_types: editor.config().editor.keyboard_report_event_types,
            report_alternate_keys: editor.config().editor.keyboard_report_alternate_keys,
            report_all_keys_as_escape_codes: editor
                .config()
                .editor
                .keyboard_report_all_keys_as_escape_codes,
        };
        terminal_modes::suspend_and_resume(terminal_modes, Some(&keyboard_config))
            .context("Failed to suspend process")?;
        editor.request_full_redraw();
        editor.set_status_message(fresh::i18n::resumed_after_suspend_message());
    }
    #[cfg(not(unix))]
    {
        let _ = terminal_modes;
        editor.set_status_message(fresh::i18n::suspend_unsupported_message());
    }
    Ok(())
}

/// Main event loop
#[cfg(target_os = "linux")]
fn run_event_loop(
    editor: &mut Editor,
    terminal: &mut Terminal<ratatui::backend::CrosstermBackend<io::Stdout>>,
    workspace_enabled: bool,
    key_translator: &KeyTranslator,
    gpm_client: &Option<GpmClient>,
    terminal_modes: &mut TerminalModes,
) -> AnyhowResult<()> {
    run_event_loop_common(
        editor,
        terminal,
        workspace_enabled,
        key_translator,
        terminal_modes,
        |timeout| poll_with_gpm(gpm_client.as_ref(), timeout),
    )
}

/// Main event loop (Windows version with VT input for bracketed paste support)
#[cfg(windows)]
fn run_event_loop(
    editor: &mut Editor,
    terminal: &mut Terminal<ratatui::backend::CrosstermBackend<io::Stdout>>,
    workspace_enabled: bool,
    key_translator: &KeyTranslator,
    terminal_modes: &mut TerminalModes,
) -> AnyhowResult<()> {
    use fresh::server::input_parser::InputParser;
    use fresh_winterm::{VtInputEvent, VtInputReader};

    let old_console_mode = fresh_winterm::enable_vt_input()?;
    // Use the configured mouse mode: when mouse_hover_enabled is true, use
    // mode 1003 (all motion) for full hover support; otherwise use mode 1002
    // (cell motion) which avoids the high event volume that can cause input
    // corruption on Windows.
    let mouse_mode = if editor.config().editor.mouse_hover_enabled {
        fresh_winterm::MouseMode::AllMotion
    } else {
        fresh_winterm::MouseMode::CellMotion
    };
    fresh_winterm::enable_mouse_tracking(mouse_mode)?;

    // Spawn a dedicated reader thread to drain the console buffer as fast
    // as possible. This prevents the Windows console from dropping bytes
    // from VT mouse sequences under high event rates (1003 all-motion).
    let reader = VtInputReader::spawn();

    let mut input_parser = InputParser::new();
    let mut event_buffer: std::collections::VecDeque<CrosstermEvent> =
        std::collections::VecDeque::new();

    let result = run_event_loop_common(
        editor,
        terminal,
        workspace_enabled,
        key_translator,
        terminal_modes,
        |timeout| -> AnyhowResult<Option<CrosstermEvent>> {
            // Return buffered events first
            if let Some(event) = event_buffer.pop_front() {
                return Ok(Some(event));
            }

            // Drain all available events from the reader thread
            let mut got_any = false;
            loop {
                let event = if !got_any {
                    reader.poll(timeout)
                } else {
                    // Subsequent: non-blocking drain of anything queued
                    reader.try_recv()
                };

                match event {
                    Some(VtInputEvent::VtBytes(bytes)) => {
                        let parsed = input_parser.parse(&bytes);
                        for ev in parsed {
                            event_buffer.push_back(ev);
                        }
                        got_any = true;
                    }
                    Some(VtInputEvent::Resize) => {
                        if let Ok((cols, rows)) = crossterm::terminal::size() {
                            event_buffer.push_back(CrosstermEvent::Resize(cols, rows));
                        }
                        got_any = true;
                    }
                    Some(VtInputEvent::FocusGained) => {
                        event_buffer.push_back(CrosstermEvent::FocusGained);
                        got_any = true;
                    }
                    Some(VtInputEvent::FocusLost) => {
                        event_buffer.push_back(CrosstermEvent::FocusLost);
                        got_any = true;
                    }
                    None => break,
                }
            }

            if !got_any {
                // Timed out — flush standalone ESC if any (MS Edit pattern)
                let flushed = input_parser.parse(b"");
                for ev in flushed {
                    event_buffer.push_back(ev);
                }
            }

            Ok(event_buffer.pop_front())
        },
    );

    // Restore mouse tracking and console mode on exit
    let _ = fresh_winterm::disable_mouse_tracking();
    let _ = fresh_winterm::restore_console_mode(old_console_mode);

    result
}

/// Main event loop (non-Linux, non-Windows version — e.g., macOS)
#[cfg(not(any(target_os = "linux", windows)))]
fn run_event_loop(
    editor: &mut Editor,
    terminal: &mut Terminal<ratatui::backend::CrosstermBackend<io::Stdout>>,
    workspace_enabled: bool,
    key_translator: &KeyTranslator,
    terminal_modes: &mut TerminalModes,
) -> AnyhowResult<()> {
    run_event_loop_common(
        editor,
        terminal,
        workspace_enabled,
        key_translator,
        terminal_modes,
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
    terminal_modes: &mut TerminalModes,
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
            // Auto-save file-backed buffers to disk before exiting
            if editor.config().editor.auto_save_enabled {
                match editor.save_all_on_exit() {
                    Ok(count) if count > 0 => {
                        tracing::info!("Auto-saved {} buffer(s) on exit", count);
                    }
                    Ok(_) => {}
                    Err(e) => {
                        tracing::warn!("Failed to auto-save on exit: {}", e);
                    }
                }
            }

            // End recovery session first (flushes dirty buffers + assigns recovery IDs),
            // then save workspace (captures those IDs for next session restore).
            if let Err(e) = editor.end_recovery_session() {
                tracing::warn!("Failed to end recovery session: {}", e);
            }
            if workspace_enabled {
                if let Err(e) = editor.save_workspace() {
                    tracing::warn!("Failed to save workspace: {}", e);
                } else {
                    tracing::debug!("Workspace saved successfully");
                }
            }
            break;
        }

        if editor.take_suspend_request() {
            handle_suspend_request(editor, terminal_modes)?;
            needs_render = true;
            last_render = Instant::now() - FRAME_DURATION;
            continue;
        }

        // Active animations force a render every FRAME_DURATION.
        let animations_active = editor.animations.is_active();
        if animations_active {
            needs_render = true;
        }

        if needs_render && last_render.elapsed() >= FRAME_DURATION {
            {
                let _span = tracing::info_span!("terminal_draw").entered();
                use crossterm::ExecutableCommand;
                stdout().execute(crossterm::terminal::BeginSynchronizedUpdate)?;
                terminal.draw(|frame| editor.render(frame))?;
                stdout().execute(crossterm::terminal::EndSynchronizedUpdate)?;
            }
            last_render = Instant::now();
            needs_render = false;
        }

        let event = if let Some(e) = pending_event.take() {
            Some(e)
        } else {
            let mut timeout = if needs_render {
                FRAME_DURATION.saturating_sub(last_render.elapsed())
            } else {
                Duration::from_millis(50)
            };
            // While animations are running, cap the timeout so the next
            // iteration fires in time for the next frame — but never past
            // the earliest animation deadline.
            if editor.animations.is_active() {
                let until_next_frame = FRAME_DURATION.saturating_sub(last_render.elapsed());
                timeout = timeout.min(until_next_frame);
                if let Some(deadline) = editor.animations.next_deadline() {
                    let until_deadline = deadline.saturating_duration_since(Instant::now());
                    timeout = timeout.min(until_deadline);
                }
            }

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
            CrosstermEvent::FocusGained => {
                editor.focus_gained();
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

    // Tests for the URL-style `ssh://` remote form.  The `$USER`
    // fallback-dependent cases set the env var explicitly so they
    // don't depend on the test runner's environment.

    #[test]
    fn test_parse_location_ssh_url_user_and_path() {
        let loc = parse_location("ssh://alice@host.example/home/alice/main.rs");
        match loc {
            ParsedLocation::Remote(rl) => {
                assert_eq!(rl.user, "alice");
                assert_eq!(rl.host, "host.example");
                assert_eq!(rl.port, None);
                assert_eq!(rl.path, "/home/alice/main.rs");
                assert_eq!(rl.line, None);
                assert_eq!(rl.column, None);
            }
            ParsedLocation::Local(_) => panic!("Expected remote, got local"),
        }
    }

    #[test]
    fn test_parse_location_ssh_url_with_port() {
        let loc = parse_location("ssh://bob@server:2222/etc/hosts");
        match loc {
            ParsedLocation::Remote(rl) => {
                assert_eq!(rl.user, "bob");
                assert_eq!(rl.host, "server");
                assert_eq!(rl.port, Some(2222));
                assert_eq!(rl.path, "/etc/hosts");
            }
            ParsedLocation::Local(_) => panic!("Expected remote, got local"),
        }
    }

    #[test]
    fn test_parse_location_ssh_url_with_port_and_line_col() {
        let loc = parse_location("ssh://bob@server:2222/src/lib.rs:42:7");
        match loc {
            ParsedLocation::Remote(rl) => {
                assert_eq!(rl.user, "bob");
                assert_eq!(rl.host, "server");
                assert_eq!(rl.port, Some(2222));
                assert_eq!(rl.path, "/src/lib.rs");
                assert_eq!(rl.line, Some(42));
                assert_eq!(rl.column, Some(7));
            }
            ParsedLocation::Local(_) => panic!("Expected remote, got local"),
        }
    }

    #[test]
    fn test_parse_location_ssh_url_default_user_from_env() {
        // Temporarily override $USER so the test doesn't depend on
        // whatever the runner has set.
        let prev_user = std::env::var("USER").ok();
        let prev_username = std::env::var("USERNAME").ok();
        // SAFETY: single-threaded test; no other thread reads $USER.
        unsafe {
            std::env::set_var("USER", "envuser");
        }
        let loc = parse_location("ssh://host.example/tmp/file.txt");
        // Restore before asserting so a panic doesn't poison later tests.
        unsafe {
            match prev_user {
                Some(ref v) => std::env::set_var("USER", v),
                None => std::env::remove_var("USER"),
            }
            match prev_username {
                Some(ref v) => std::env::set_var("USERNAME", v),
                None => std::env::remove_var("USERNAME"),
            }
        }
        match loc {
            ParsedLocation::Remote(rl) => {
                assert_eq!(rl.user, "envuser");
                assert_eq!(rl.host, "host.example");
                assert_eq!(rl.port, None);
                assert_eq!(rl.path, "/tmp/file.txt");
            }
            ParsedLocation::Local(_) => panic!("Expected remote, got local"),
        }
    }

    #[test]
    fn test_parse_location_ssh_url_missing_path_is_local() {
        // `ssh://host` with no `/path` is malformed — fall through to
        // the local parser, which stores the whole thing as a filename.
        let loc = parse_location("ssh://host.example");
        match loc {
            ParsedLocation::Local(fl) => {
                assert_eq!(fl.path, PathBuf::from("ssh://host.example"));
            }
            ParsedLocation::Remote(_) => panic!("Expected local, got remote"),
        }
    }

    #[test]
    fn test_parse_location_ssh_url_bad_port_is_local() {
        // Non-numeric port → falls through to local.
        let loc = parse_location("ssh://alice@host:not-a-port/file");
        match loc {
            ParsedLocation::Local(fl) => {
                assert_eq!(fl.path, PathBuf::from("ssh://alice@host:not-a-port/file"));
            }
            ParsedLocation::Remote(_) => panic!("Expected local, got remote"),
        }
    }

    #[test]
    fn test_parse_location_ssh_url_empty_user_is_local() {
        // `@` with an empty user is malformed — fall through to local.
        let loc = parse_location("ssh://@host/path");
        match loc {
            ParsedLocation::Local(fl) => {
                assert_eq!(fl.path, PathBuf::from("ssh://@host/path"));
            }
            ParsedLocation::Remote(_) => panic!("Expected local, got remote"),
        }
    }

    // Tests for the client→daemon plumbing: a remote spec on the
    // command line becomes a `--ssh-url` forwarded to
    // `spawn_server_detached`, and the server side parses it back
    // into the same shape the standalone path uses.

    #[test]
    fn test_remote_location_to_ssh_url_with_port() {
        let remote = RemoteLocation {
            user: "alice".into(),
            host: "host.example".into(),
            port: Some(2222),
            path: "/etc/hosts".into(),
            line: Some(10),
            column: None,
        };
        // Line/column are per-file, not authority — they must not
        // appear in the URL we hand to the daemon.
        assert_eq!(
            remote_location_to_ssh_url(&remote),
            "ssh://alice@host.example:2222/etc/hosts"
        );
    }

    #[test]
    fn test_remote_location_to_ssh_url_no_port() {
        let remote = RemoteLocation {
            user: "bob".into(),
            host: "server".into(),
            port: None,
            path: "/home/bob".into(),
            line: None,
            column: None,
        };
        assert_eq!(
            remote_location_to_ssh_url(&remote),
            "ssh://bob@server/home/bob"
        );
    }

    #[test]
    fn test_extract_ssh_url_none_for_local_only() {
        let files = vec!["foo.txt".to_string(), "bar:42".to_string()];
        assert_eq!(extract_ssh_url_from_files(&files).unwrap(), None);
    }

    #[test]
    fn test_extract_ssh_url_from_ssh_urls() {
        let files = vec![
            "ssh://alice@host/a".to_string(),
            "ssh://alice@host/b:10".to_string(),
        ];
        assert_eq!(
            extract_ssh_url_from_files(&files).unwrap(),
            Some("ssh://alice@host/a".to_string())
        );
    }

    #[test]
    fn test_extract_ssh_url_from_scp_style() {
        let files = vec!["alice@host:/etc/hosts".to_string()];
        assert_eq!(
            extract_ssh_url_from_files(&files).unwrap(),
            Some("ssh://alice@host/etc/hosts".to_string())
        );
    }

    #[test]
    fn test_extract_ssh_url_rejects_mismatched_hosts() {
        let files = vec![
            "ssh://alice@host1/a".to_string(),
            "ssh://alice@host2/b".to_string(),
        ];
        assert!(extract_ssh_url_from_files(&files).is_err());
    }

    #[test]
    fn test_extract_ssh_url_rejects_mixed_local_and_remote() {
        let files = vec!["ssh://alice@host/a".to_string(), "local.txt".to_string()];
        assert!(extract_ssh_url_from_files(&files).is_err());
    }

    #[test]
    fn test_parse_ssh_url_arg_accepts_valid_url() {
        let rl = parse_ssh_url_arg("ssh://alice@host:2222/path").unwrap();
        assert_eq!(rl.user, "alice");
        assert_eq!(rl.host, "host");
        assert_eq!(rl.port, Some(2222));
        assert_eq!(rl.path, "/path");
    }

    #[test]
    fn test_parse_ssh_url_arg_rejects_scp_style() {
        // The `--ssh-url` flag is URL-form only; scp-style is an
        // error (we'd never send it over this flag).
        assert!(parse_ssh_url_arg("alice@host:/path").is_err());
    }

    #[test]
    fn test_parse_ssh_url_arg_rejects_malformed() {
        assert!(parse_ssh_url_arg("ssh://host").is_err()); // no path
        assert!(parse_ssh_url_arg("ssh://alice@host:bad/path").is_err()); // bad port
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
