# Fresh CLI Redesign

**Status**: Implemented (Experimental)

> **Note**: The session persistence features (`open`, `attach`, `list`, `kill`) are experimental. See [Session Persistence](../features/session-persistence.md) for user documentation.

---

## Previous Problems (Fixed)

The current CLI has several UX issues:

1. **Flat structure** - 20+ flags at top level, hard to scan
2. **Mixed metaphors** - `--attach`, `--kill`, `--list-sessions` are actions masquerading as options
3. **Confusing naming** - `--no-session` (don't restore workspace) vs `--list-sessions` (show daemons)
4. **Poor discoverability** - Users must read all flags to find session features
5. **Cognitive overload** - Too many options shown at once
6. **Inconsistent patterns** - Some features use `--flag`, others use `--flag <value>`, `--flag [value]`

## Design Principles (nngroup)

1. **Recognition over recall** - Subcommands are more discoverable than flags
2. **Consistency** - Follow git/cargo conventions users already know
3. **Progressive disclosure** - Simple use case first, advanced options available
4. **Flexibility** - Short aliases for power users
5. **Minimalist design** - Show only relevant options per context
6. **Match real world** - Use familiar terminology (session, attach, detach)

---

## Proposed CLI Structure

### Primary Usage (Editor)

```
fresh [OPTIONS] [FILES...]
```

Opening the editor remains the default. No subcommand needed.

```bash
fresh                           # Open editor in current directory
fresh file.txt                  # Open a file
fresh src/main.rs:42            # Open at line 42
fresh src/main.rs:42:10         # Open at line 42, column 10
fresh user@host:~/file.txt      # Open remote file
fresh -                         # Read from stdin
```

### Subcommands

```
fresh <COMMAND>

Commands:
  session   Manage persistent sessions (attach, detach, list, kill)
  init      Initialize a new plugin, theme, or language pack
  config    Show or validate configuration
  help      Print help for a command
```

### Session Management

```
fresh session <COMMAND>

Commands:
  list, ls     List active sessions
  attach, a    Attach to an existing session
  new, n       Start a new named session
  kill, k      Terminate a session
  info         Show details about a session
```

Examples:
```bash
fresh session list              # List all sessions
fresh session ls                # Short alias

fresh session attach            # Attach to session for current directory
fresh session attach myproject  # Attach to named session
fresh session a                 # Short alias

fresh session new myproject     # Start new named session
fresh session n myproject       # Short alias

fresh session kill              # Kill session for current directory
fresh session kill myproject    # Kill named session
fresh session kill --all        # Kill all sessions
fresh session k                 # Short alias

fresh session info              # Info about current directory's session
fresh session info myproject    # Info about named session
```

### Shortcuts for Common Operations

Power users can use top-level shortcuts:

```bash
fresh -a                        # Shortcut for: fresh session attach
fresh -a myproject              # Shortcut for: fresh session attach myproject
```

### Init Subcommand

```
fresh init <TYPE>

Types:
  plugin      Create a new plugin project
  theme       Create a new theme
  language    Create a new language pack
```

Examples:
```bash
fresh init plugin               # Interactive plugin creation
fresh init theme                # Interactive theme creation
fresh init language             # Interactive language pack creation
```

### Config Subcommand

```
fresh config <COMMAND>

Commands:
  show        Print effective configuration as JSON
  paths       Show directories used by Fresh
  check       Validate configuration file
  edit        Open configuration in Fresh
```

Examples:
```bash
fresh config show               # Dump effective config
fresh config paths              # Show config/data/cache directories
fresh config check              # Validate config syntax
fresh config edit               # Open config file for editing
```

---

## Complete Help Output

### Main Help (`fresh --help`)

```
fresh - A terminal text editor with multi-cursor support

Usage: fresh [OPTIONS] [FILES...]
       fresh <COMMAND>

Arguments:
  [FILES...]  Files to open (supports line:col syntax and remote paths)

Commands:
  session     Manage persistent sessions [aliases: s]
  init        Initialize a new plugin, theme, or language pack
  config      Configuration management
  help        Print help for a command

Options:
  -a, --attach [NAME]    Attach to session (shortcut for `session attach`)
      --stdin            Read from stdin (alternative to `-` filename)
      --no-plugins       Disable plugin loading
      --config <PATH>    Path to configuration file
      --log-file <PATH>  Path to log file
      --no-restore       Don't restore previous workspace
      --locale <LOCALE>  Override locale (e.g., 'en', 'ja')
  -h, --help             Print help
  -V, --version          Print version

Examples:
  fresh                         Open editor in current directory
  fresh file.txt                Edit a file
  fresh src/main.rs:42:10       Edit file at specific location
  fresh -a                      Reattach to persistent session
  fresh session list            List all sessions

Run `fresh <command> --help` for more information on a command.
```

### Session Help (`fresh session --help`)

```
Manage persistent sessions

Sessions allow editor state to persist across terminal sessions. The editor
runs as a background daemon, and you can attach/detach like tmux.

Usage: fresh session <COMMAND>

Commands:
  list, ls     List active sessions
  attach, a    Attach to a session
  new, n       Start a new named session
  kill, k      Terminate a session
  info         Show session details

Options:
  -h, --help   Print help

Examples:
  fresh session list              Show all running sessions
  fresh session attach            Attach to session for current directory
  fresh session attach myproject  Attach to named session
  fresh session kill --all        Terminate all sessions
```

### Session Attach Help (`fresh session attach --help`)

```
Attach to an existing session

If no session exists for the target, one will be created automatically.

Usage: fresh session attach [OPTIONS] [NAME]

Arguments:
  [NAME]  Session name (defaults to current directory)

Options:
      --create-only   Fail if session doesn't exist (don't auto-create)
  -h, --help          Print help

Examples:
  fresh session attach            Attach to session for current directory
  fresh session attach myproject  Attach to session named "myproject"
  fresh -a myproject              Shortcut syntax
```

### Session List Help (`fresh session list --help`)

```
List active sessions

Usage: fresh session list [OPTIONS]

Options:
      --json      Output as JSON
      --quiet     Only print session names
  -h, --help      Print help

Output columns:
  NAME      Session name or directory
  STATUS    Running, idle, or orphaned
  CLIENTS   Number of attached clients
  CREATED   When the session was started
```

### Session Kill Help (`fresh session kill --help`)

```
Terminate a session

Usage: fresh session kill [OPTIONS] [NAME]

Arguments:
  [NAME]  Session to kill (defaults to current directory's session)

Options:
      --all            Kill all sessions
      --idle <DURATION>  Kill sessions idle longer than DURATION (e.g., 7d, 24h)
      --force          Kill without prompting for unsaved changes
  -h, --help           Print help

Examples:
  fresh session kill              Kill current directory's session
  fresh session kill myproject    Kill named session
  fresh session kill --all        Kill all sessions
  fresh session kill --idle 7d    Kill sessions idle for 7+ days
```

---

## Renamed/Reorganized Options

### Deprecated Flags (existed in master, now reorganized)

| Old                   | New                          | Reason                                    |
|-----------------------|------------------------------|-------------------------------------------|
| `--dump-config`       | `fresh config show`          | Action → subcommand                       |
| `--show-paths`        | `fresh config paths`         | Action → subcommand                       |
| `--init [TYPE]`       | `fresh init [TYPE]`          | Action → subcommand                       |
| `--no-session`        | `--no-restore`               | Clearer naming (aliased for compat)       |

### New Session Commands (no backward compat needed)

| Command                           | Description                               |
|-----------------------------------|-------------------------------------------|
| `fresh session list`              | List active sessions                      |
| `fresh session attach [NAME]`     | Attach to session                         |
| `fresh session kill [NAME]`       | Terminate a session                       |
| `fresh session new NAME`          | Start new named session                   |
| `fresh -a [NAME]`                 | Shortcut for `session attach`             |

### Internal/Hidden Flags

| Flag        | Purpose                                    |
|-------------|--------------------------------------------|
| `--server`  | Start as daemon (used internally)          |

---

## Implementation Notes

### Clap Subcommands

```rust
#[derive(Parser)]
#[command(name = "fresh")]
#[command(about = "A terminal text editor with multi-cursor support")]
#[command(version, propagate_version = true)]
#[command(after_help = "Run `fresh <command> --help` for more information on a command.")]
struct Cli {
    #[command(subcommand)]
    command: Option<Command>,

    /// Files to open
    #[arg(value_name = "FILES")]
    files: Vec<String>,

    /// Attach to session (shortcut for `session attach`)
    #[arg(short = 'a', long, value_name = "NAME")]
    attach: Option<Option<String>>,

    // ... other editor options
}

#[derive(Subcommand)]
enum Command {
    /// Manage persistent sessions
    #[command(alias = "s")]
    Session(SessionArgs),

    /// Initialize a new plugin, theme, or language pack
    Init(InitArgs),

    /// Configuration management
    Config(ConfigArgs),
}

#[derive(Args)]
struct SessionArgs {
    #[command(subcommand)]
    command: SessionCommand,
}

#[derive(Subcommand)]
enum SessionCommand {
    /// List active sessions
    #[command(alias = "ls")]
    List {
        #[arg(long)]
        json: bool,
        #[arg(long)]
        quiet: bool,
    },

    /// Attach to a session
    #[command(alias = "a")]
    Attach {
        /// Session name (defaults to current directory)
        name: Option<String>,
        #[arg(long)]
        create_only: bool,
    },

    /// Start a new named session
    #[command(alias = "n")]
    New {
        /// Session name
        name: String,
        /// Files to open
        files: Vec<String>,
    },

    /// Terminate a session
    #[command(alias = "k")]
    Kill {
        /// Session to kill
        name: Option<String>,
        #[arg(long)]
        all: bool,
        #[arg(long, value_name = "DURATION")]
        idle: Option<String>,
        #[arg(long)]
        force: bool,
    },

    /// Show session details
    Info {
        /// Session name
        name: Option<String>,
    },
}
```

### Backward Compatibility

Only flags that existed in master have deprecation warnings:

```rust
/// [deprecated: use `fresh config show`]
#[arg(long, hide = true)]
dump_config: bool,

/// [deprecated: use `fresh config paths`]
#[arg(long, hide = true)]
show_paths: bool,

/// [deprecated: use `fresh init`]
#[arg(long, hide = true, value_name = "TYPE")]
init: Option<Option<String>>,
```

When used:
```
warning: --dump-config is deprecated, use `fresh config show` instead
```

Session commands (`fresh session list`, `fresh session attach`, etc.) are new
and have no deprecated equivalents since they didn't exist before.

---

## Migration Guide

For the v0.X release notes:

```markdown
## CLI Changes

The CLI has been reorganized for better discoverability and consistency.

### Session Management

Session commands are now under `fresh session`:

### New: Session Persistence

Fresh now supports persistent sessions that survive terminal disconnects:

```bash
fresh session list              # List running sessions
fresh session attach [NAME]     # Attach to a session
fresh session kill [NAME]       # Terminate a session
fresh -a [NAME]                 # Shortcut for attach
```

### Reorganized: Configuration Commands

| Before                | After                 |
|-----------------------|-----------------------|
| `fresh --dump-config` | `fresh config show`   |
| `fresh --show-paths`  | `fresh config paths`  |
| `fresh --init [TYPE]` | `fresh init [TYPE]`   |

### Renamed: Session Restore Flag

| Before            | After              | Notes                              |
|-------------------|--------------------|------------------------------------|
| `--no-session`    | `--no-restore`     | Clearer: don't restore workspace   |

The old configuration flags (`--dump-config`, `--show-paths`, `--init`) still
work but show deprecation warnings. `--no-session` is aliased to `--no-restore`.
```
