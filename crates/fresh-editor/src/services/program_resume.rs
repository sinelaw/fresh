//! Program Resume (OSC 7577): both sides of the protocol.
//!
//! The protocol (draft revision 0.5,
//! <https://www.superlogical.com/rex/docs/build/program-resume>) lets a program
//! running in a terminal tell that terminal how to start it again — `nvim -S
//! session.vim`, `fresh --restore` — so that after the terminal restarts it
//! can bring each pane back running what it ran, not as a bare shell.
//!
//! ```text
//! ESC ] 7577 ; pid=<pid>:time=<unix secs>:argv=<b64>,<b64>…[:cwd=<b64>]:mac=<hex> ST
//! ESC ] 7577 ; pid=<pid>:time=<unix secs>:clear=1:mac=<hex> ST
//! ```
//!
//! A terminal that implements it puts a random per-pane secret in
//! `OSC7577_TOKEN`; a report is only believed when its `mac` is
//! `sha256(<body up to ":mac="> ":" <token>)`, so output that merely *prints*
//! the sequence (a `cat` of a log, a remote host) cannot plant a command.
//!
//! Fresh plays both parts:
//!
//!   * **Program** ([`ProgramReporter`]): fresh itself, run in a terminal that
//!     sets the token, reports the command that reopens its workspace and
//!     clears it when the user quits.
//!   * **Terminal** ([`ResumeTracker`]): fresh's embedded terminals hand each
//!     local shell its own token, verify the reports its programs send, and
//!     the workspace persists the current one so a restored terminal re-runs
//!     it (see `Window::restore_terminal`).
//!
//! Everything protocol-shaped — encoding, the strict parse, the MAC — lives
//! here so both parts agree on it by construction.

use base64::engine::general_purpose::{GeneralPurpose, GeneralPurposeConfig, STANDARD};
use base64::engine::DecodePaddingMode;
use base64::Engine as _;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

/// The environment variable a terminal sets to advertise support and carry the
/// per-pane secret.
pub const TOKEN_ENV: &str = "OSC7577_TOKEN";

/// Reports whose `time` is further than this from the receiver's clock are
/// ignored (the spec's recommended window).
pub const TIME_WINDOW_SECS: u64 = 60;

/// The longest sequence a program may send and expect every terminal to take
/// (the spec's minimum), counted over the whole escape sequence. The program
/// side never sends more.
pub const MIN_ACCEPTED_SEQUENCE_BYTES: usize = 2048;

/// The longest report *payload* the terminal side buffers. Larger than the
/// spec's minimum, which is all a terminal owes; anything past it is abandoned
/// so a stray introducer without a terminator can't grow without bound.
const MAX_PAYLOAD_BYTES: usize = 8192;

/// The most argv elements the terminal side takes (the spec's minimum is 64).
const MAX_ARGV: usize = 256;

/// The keys this implementation understands, which a report may list in
/// `need`.
const IMPLEMENTED_KEYS: &[&str] = &["pid", "time", "mac", "argv", "cwd", "clear", "need"];

/// Standard alphabet, padding optional on decode, as the spec allows.
const B64_DECODE: GeneralPurpose = GeneralPurpose::new(
    &base64::alphabet::STANDARD,
    GeneralPurposeConfig::new().with_decode_padding_mode(DecodePaddingMode::Indifferent),
);

/// Seconds since the Unix epoch, now.
pub fn unix_now() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

/// Whether `token` has the shape the spec gives a token: 22–64 characters of
/// `[A-Za-z0-9_-]`.
pub fn is_valid_token(token: &str) -> bool {
    (22..=64).contains(&token.len())
        && token
            .bytes()
            .all(|b| b.is_ascii_alphanumeric() || b == b'_' || b == b'-')
}

/// A new random token for one terminal pane: 64 hex characters (two v4 UUIDs,
/// 244 random bits from the OS CSPRNG).
pub fn new_token() -> String {
    format!(
        "{}{}",
        uuid::Uuid::new_v4().simple(),
        uuid::Uuid::new_v4().simple()
    )
}

/// The report MAC: lowercase hex SHA-256 of `body ":" token`.
pub fn compute_mac(body: &str, token: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(body.as_bytes());
    hasher.update(b":");
    hasher.update(token.as_bytes());
    let digest = hasher.finalize();
    let mut out = String::with_capacity(64);
    for byte in digest {
        out.push_str(&format!("{byte:02x}"));
    }
    out
}

/// Equality that takes the same time wherever the inputs differ, so the MAC
/// check does not leak how much of a forged MAC was right.
fn constant_time_eq(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    a.iter().zip(b).fold(0u8, |acc, (x, y)| acc | (x ^ y)) == 0
}

/// A command to resume a program with: what a terminal records from a report,
/// and what a workspace persists for a terminal pane.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ResumeCommand {
    /// Program first (a bare name, resolved on `PATH` when it runs, or an
    /// absolute path), then its arguments, each passed unchanged as one
    /// argument — never joined into shell source.
    pub argv: Vec<String>,
    /// The directory to run it in, when the report named one (or, on the
    /// terminal side, the owner's own working directory at save time).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub cwd: Option<PathBuf>,
}

impl ResumeCommand {
    /// Whether `argv[0]` is a program the spec allows: a bare name that does
    /// not look like an option, or an absolute path. A relative path would
    /// depend on a directory nobody can promise at resume time.
    fn program_is_valid(argv: &[String]) -> bool {
        let Some(program) = argv.first() else {
            return false;
        };
        if program.is_empty() || program.starts_with('-') {
            return false;
        }
        !program.contains('/') || Path::new(program).is_absolute()
    }
}

// ============================================================================
// Program side
// ============================================================================

/// What a program asks of its terminal.
#[derive(Debug, Clone, Copy)]
pub enum Report<'a> {
    /// Run this to resume me.
    Resume(&'a ResumeCommand),
    /// Forget me: I was quit on purpose.
    Clear,
}

/// The body of a report (everything before `:mac=`), or `None` when the
/// command can't be expressed in the protocol: an element that isn't valid
/// text for it, a relative program path, a relative `cwd`.
pub fn encode_body(pid: u32, time: u64, report: Report<'_>) -> Option<String> {
    let mut body = format!("pid={pid}:time={time}");
    match report {
        Report::Clear => body.push_str(":clear=1"),
        Report::Resume(cmd) => {
            if !ResumeCommand::program_is_valid(&cmd.argv)
                || cmd.argv.len() > 64
                || cmd.argv.iter().any(|a| a.chars().any(char::is_control))
            {
                return None;
            }
            let encoded: Vec<String> = cmd.argv.iter().map(|a| STANDARD.encode(a)).collect();
            body.push_str(":argv=");
            body.push_str(&encoded.join(","));
            if let Some(cwd) = &cmd.cwd {
                let cwd = cwd.to_str()?;
                if !Path::new(cwd).is_absolute() || cwd.chars().any(char::is_control) {
                    return None;
                }
                body.push_str(":cwd=");
                body.push_str(&STANDARD.encode(cwd));
            }
        }
    }
    Some(body)
}

/// The whole escape sequence for `report`, signed with `token`, or `None` when
/// it can't be expressed or would be longer than every terminal must accept.
pub fn encode_sequence(pid: u32, time: u64, report: Report<'_>, token: &str) -> Option<String> {
    let body = encode_body(pid, time, report)?;
    let mac = compute_mac(&body, token);
    let seq = format!("\x1b]7577;{body}:mac={mac}\x1b\\");
    (seq.len() <= MIN_ACCEPTED_SEQUENCE_BYTES).then_some(seq)
}

/// Fresh's own reporting, as a program running in a terminal that implements
/// OSC 7577.
///
/// Holds the token (taken out of the environment at startup so nothing fresh
/// starts — the daemon, LSP servers, embedded shells — inherits it), the
/// command last reported, and the last `time` used, since a terminal ignores
/// a report that is not newer than the previous one from the same pid.
#[derive(Debug)]
pub struct ProgramReporter {
    token: String,
    pid: u32,
    last_time: u64,
    current: Option<ResumeCommand>,
}

impl ProgramReporter {
    /// Take `OSC7577_TOKEN` out of this process's environment, and return a
    /// reporter when it held a well-formed token.
    ///
    /// The variable is removed whatever it held: the spec has a program that
    /// reports unset it for the children it starts, and fresh starts many.
    /// Call this before any thread is spawned — environment writes race with
    /// concurrent reads on some platforms.
    pub fn take_from_env() -> Option<Self> {
        let token = std::env::var(TOKEN_ENV).ok();
        if std::env::var_os(TOKEN_ENV).is_some() {
            std::env::remove_var(TOKEN_ENV);
        }
        let token = token.filter(|t| is_valid_token(t))?;
        Some(Self::new(token, std::process::id()))
    }

    /// A reporter for `pid`, signing with `token` (which
    /// [`Self::take_from_env`] reads from the terminal).
    pub fn new(token: String, pid: u32) -> Self {
        Self {
            token,
            pid,
            last_time: 0,
            current: None,
        }
    }

    /// A `time` newer than any sent before: the clock, or one past the last
    /// report when two land in the same second.
    fn next_time(&mut self) -> u64 {
        let time = unix_now().max(self.last_time + 1);
        self.last_time = time;
        time
    }

    /// The sequence reporting `cmd`, remembering it so [`Self::refresh`] can
    /// send it again. When `cmd` can't be expressed this is a `clear`
    /// instead, so the terminal doesn't keep resuming an older command.
    pub fn report(&mut self, cmd: ResumeCommand) -> Option<String> {
        let time = self.next_time();
        match encode_sequence(self.pid, time, Report::Resume(&cmd), &self.token) {
            Some(seq) => {
                self.current = Some(cmd);
                Some(seq)
            }
            None => {
                tracing::debug!("OSC 7577: resume command not expressible; clearing instead");
                self.current = None;
                encode_sequence(self.pid, time, Report::Clear, &self.token)
            }
        }
    }

    /// The last reported command again, for when the program has the
    /// terminal back (after `fg`). `None` when nothing was reported.
    pub fn refresh(&mut self) -> Option<String> {
        let cmd = self.current.clone()?;
        self.report(cmd)
    }

    /// The sequence that withdraws the report, for a deliberate quit.
    pub fn clear(&mut self) -> Option<String> {
        self.current = None;
        let time = self.next_time();
        encode_sequence(self.pid, time, Report::Clear, &self.token)
    }
}

/// This process's reporter, when it runs in a terminal that implements the
/// protocol. Set once by [`init_from_env`], early in `main`; every other
/// entry point is a no-op until then, and stays one in a terminal without
/// the protocol.
static PROGRAM: std::sync::Mutex<Option<ProgramReporter>> = std::sync::Mutex::new(None);

/// Take `OSC7577_TOKEN` out of the environment (always) and keep a reporter
/// for this process when it held a token. Call first thing in `main`, before
/// any thread or child process exists.
pub fn init_from_env() {
    let reporter = ProgramReporter::take_from_env();
    if let Ok(mut slot) = PROGRAM.lock() {
        *slot = reporter;
    }
}

fn with_program(f: impl FnOnce(&mut ProgramReporter) -> Option<String>) {
    let seq = match PROGRAM.lock() {
        Ok(mut slot) => slot.as_mut().and_then(f),
        Err(_) => None,
    };
    write_to_terminal(seq);
}

/// Report `cmd` as the command that resumes this process.
pub fn report_resume(cmd: ResumeCommand) {
    with_program(|p| p.report(cmd));
}

/// Report the last command again: the process has the terminal back.
pub fn refresh_resume() {
    with_program(ProgramReporter::refresh);
}

/// Withdraw the report: the user quit on purpose.
pub fn clear_resume() {
    with_program(ProgramReporter::clear);
}

/// The program to put first in fresh's own resume command.
///
/// The name it was started by when that was a bare name found on `PATH`, so
/// the command keeps working after an upgrade replaces the binary; otherwise
/// the absolute path of the binary (the AppImage itself rather than its
/// temporary mount).
pub fn own_program() -> Option<String> {
    if let Some(appimage) = std::env::var_os("APPIMAGE") {
        return appimage.into_string().ok();
    }
    let argv0 = std::env::args().next()?;
    let path = Path::new(&argv0);
    if !argv0.is_empty() && path.components().count() == 1 && !argv0.starts_with('-') {
        return Some(argv0);
    }
    let exe = std::env::current_exe().ok()?;
    exe.to_str().map(str::to_owned)
}

/// Write a report sequence to the terminal on stdout. Best-effort: the
/// report is a convenience and must never disrupt editing.
pub fn write_to_terminal(seq: Option<String>) {
    use std::io::IsTerminal;
    let Some(seq) = seq else {
        return;
    };
    let mut out = std::io::stdout();
    if !out.is_terminal() {
        return;
    }
    if let Err(e) = out.write_all(seq.as_bytes()).and_then(|()| out.flush()) {
        tracing::debug!("OSC 7577: failed to write report: {e}");
    }
}

// ============================================================================
// Terminal side
// ============================================================================

/// Why a report was dropped without effect. Kept for logging and tests.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ignored {
    /// The `?` body the spec reserves.
    Reserved,
    /// No `mac`, or not the last key, or not 64 lowercase hex digits.
    MissingMac,
    /// The MAC does not match this pane's token.
    BadMac,
    /// `pid` or `time` missing or unreadable, so neither the time check nor a
    /// rejection could be applied.
    NoOwner,
    /// `time` outside the window around the receiver's clock.
    Stale,
    /// `time` not newer than the last report accepted from the same pid.
    Replayed,
}

/// What a well-signed, in-window report asks for: a record to keep, a record
/// to drop, or — when its content broke a rule — the rejection, which also
/// drops every record of its owner.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Verdict {
    Resume(ResumeCommand),
    Clear,
    Rejected(&'static str),
}

/// A report that passed the MAC and window checks.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Checked {
    pub pid: u32,
    pub time: u64,
    pub verdict: Verdict,
}

/// Check one report payload (the text between `ESC ] 7577 ;` and its
/// terminator) against `token`, at `now`.
///
/// The order is the spec's: a report that fails the MAC or the time window is
/// ignored outright; only then is the content parsed, and a content error
/// becomes [`Verdict::Rejected`] (which removes the owner's records) rather
/// than silence. The replay check needs per-pid state and is
/// [`ResumeTracker`]'s.
pub fn check_report(payload: &str, token: &str, now: u64) -> Result<Checked, Ignored> {
    if payload == "?" {
        return Err(Ignored::Reserved);
    }
    let (body, mac) = payload.rsplit_once(":mac=").ok_or(Ignored::MissingMac)?;
    if mac.len() != 64 || !mac.bytes().all(|b| matches!(b, b'0'..=b'9' | b'a'..=b'f')) {
        return Err(Ignored::MissingMac);
    }
    if !constant_time_eq(compute_mac(body, token).as_bytes(), mac.as_bytes()) {
        return Err(Ignored::BadMac);
    }

    // The owner and the time, read before the strict parse: a report whose
    // content is wrong is still *rejected* (its owner's records dropped), and
    // that needs both.
    let field = |key: &str| {
        body.split(':')
            .find_map(|pair| pair.strip_prefix(key)?.strip_prefix('='))
    };
    let number = |s: &str| -> Option<u64> {
        (!s.is_empty() && !s.starts_with('0') && s.bytes().all(|b| b.is_ascii_digit()))
            .then(|| s.parse().ok())
            .flatten()
    };
    let time = field("time").and_then(number).ok_or(Ignored::NoOwner)?;
    let pid = field("pid")
        .and_then(number)
        .and_then(|p| u32::try_from(p).ok())
        .ok_or(Ignored::NoOwner)?;
    if time.abs_diff(now) > TIME_WINDOW_SECS {
        return Err(Ignored::Stale);
    }

    let verdict = match parse_body(body) {
        Ok(verdict) => verdict,
        Err(reason) => Verdict::Rejected(reason),
    };
    Ok(Checked { pid, time, verdict })
}

/// The strict parse of a report body (MAC already stripped).
fn parse_body(body: &str) -> Result<Verdict, &'static str> {
    let mut fields: HashMap<&str, &str> = HashMap::new();
    for pair in body.split(':') {
        let (key, value) = pair.split_once('=').ok_or("malformed pair")?;
        if key.is_empty() || !key.bytes().all(|b| b.is_ascii_lowercase()) {
            return Err("malformed key");
        }
        if !value.bytes().all(|b| {
            b.is_ascii_alphanumeric() || matches!(b, b'_' | b'.' | b',' | b'+' | b'/' | b'=' | b'-')
        }) {
            return Err("malformed value");
        }
        if fields.insert(key, value).is_some() {
            return Err("repeated key");
        }
    }

    if let Some(need) = fields.get("need") {
        if need.is_empty() {
            return Err("empty need");
        }
        if need.split(',').any(|k| !IMPLEMENTED_KEYS.contains(&k)) {
            return Err("needs an unimplemented key");
        }
    }

    match (fields.get("argv"), fields.get("clear")) {
        (Some(_), Some(_)) => Err("both argv and clear"),
        (None, None) => Err("neither argv nor clear"),
        (None, Some(&clear)) => {
            if clear == "1" {
                Ok(Verdict::Clear)
            } else {
                Err("bad clear value")
            }
        }
        (Some(&argv), None) => {
            let argv = argv
                .split(',')
                .map(decode_text)
                .collect::<Result<Vec<_>, _>>()?;
            if argv.len() > MAX_ARGV {
                return Err("command too long");
            }
            if !ResumeCommand::program_is_valid(&argv) {
                return Err("program is not a bare name or absolute path");
            }
            let cwd = match fields.get("cwd") {
                Some(cwd) => {
                    let cwd = PathBuf::from(decode_text(cwd)?);
                    if !cwd.is_absolute() {
                        return Err("relative cwd");
                    }
                    Some(cwd)
                }
                None => None,
            };
            Ok(Verdict::Resume(ResumeCommand { argv, cwd }))
        }
    }
}

/// One base64 element: valid UTF-8 with no control characters.
fn decode_text(b64: &str) -> Result<String, &'static str> {
    let bytes = B64_DECODE.decode(b64).map_err(|_| "bad base64")?;
    let text = String::from_utf8(bytes).map_err(|_| "invalid UTF-8")?;
    if text.chars().any(char::is_control) {
        return Err("control character");
    }
    Ok(text)
}

/// Incremental scanner for `ESC ] 7577 ; <payload> ST` in a raw PTY byte
/// stream. The embedded emulator drops OSCs it doesn't know, and sequences
/// can straddle reads, so — like the OSC 7 scanner — this is a resumable
/// state machine fed every byte that flows to the emulator.
#[derive(Debug, Default)]
pub struct ReportScanner {
    intro_match: usize,
    collecting: bool,
    saw_esc: bool,
    buf: Vec<u8>,
}

const REPORT_INTRO: &[u8] = b"\x1b]7577;";

impl ReportScanner {
    /// Feed one chunk; returns each payload that completes in it.
    pub fn feed(&mut self, data: &[u8], out: &mut Vec<String>) {
        for &byte in data {
            if self.collecting {
                if self.saw_esc {
                    self.saw_esc = false;
                    if byte == b'\\' {
                        self.finish(out);
                    } else {
                        self.reset();
                    }
                } else if byte == 0x07 {
                    self.finish(out);
                } else if byte == 0x1b {
                    self.saw_esc = true;
                } else if self.buf.len() >= MAX_PAYLOAD_BYTES {
                    // Oversized: not received whole, so ignored.
                    self.reset();
                } else {
                    self.buf.push(byte);
                }
            } else if byte == REPORT_INTRO[self.intro_match] {
                self.intro_match += 1;
                if self.intro_match == REPORT_INTRO.len() {
                    self.collecting = true;
                    self.intro_match = 0;
                    self.buf.clear();
                }
            } else {
                // ESC is the introducer's only byte that also starts it.
                self.intro_match = usize::from(byte == REPORT_INTRO[0]);
            }
        }
    }

    fn finish(&mut self, out: &mut Vec<String>) {
        if let Ok(s) = std::str::from_utf8(&self.buf) {
            out.push(s.to_owned());
        }
        self.reset();
    }

    fn reset(&mut self) {
        self.collecting = false;
        self.saw_esc = false;
        self.intro_match = 0;
        self.buf.clear();
    }
}

/// A process's identity at the time a record was accepted, so a record is
/// dropped when its owner exits — and not inherited by an unrelated process
/// that later reuses the pid.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Owner {
    pid: u32,
    /// Start time (clock ticks since boot) where the platform reports one.
    start: Option<u64>,
}

#[derive(Debug, Clone)]
struct Record {
    owner: Owner,
    command: ResumeCommand,
    /// Acceptance order, newest highest.
    seq: u64,
}

/// One terminal pane's OSC 7577 state: its token and the records its programs
/// have reported.
#[derive(Debug)]
pub struct ResumeTracker {
    token: String,
    /// The pane's session leader (the shell the PTY was opened for). A report
    /// from a process outside its session is not this pane's, and is ignored.
    session_leader: Option<u32>,
    records: Vec<Record>,
    last_time: HashMap<u32, u64>,
    next_seq: u64,
}

impl ResumeTracker {
    pub fn new(token: String) -> Self {
        Self {
            token,
            session_leader: None,
            records: Vec::new(),
            last_time: HashMap::new(),
            next_seq: 0,
        }
    }

    /// Bind the tracker to the pane's shell, once its pid is known.
    pub fn set_session_leader(&mut self, pid: Option<u32>) {
        self.session_leader = pid;
    }

    /// Apply one report payload from the pane's output.
    pub fn handle_payload(&mut self, payload: &str, now: u64) {
        let checked = match check_report(payload, &self.token, now) {
            Ok(checked) => checked,
            Err(why) => {
                tracing::debug!("OSC 7577 report ignored: {why:?}");
                return;
            }
        };
        if self
            .last_time
            .get(&checked.pid)
            .is_some_and(|&last| checked.time <= last)
        {
            tracing::debug!("OSC 7577 report ignored: {:?}", Ignored::Replayed);
            return;
        }
        // The pid is not a credential (the token is), but a report naming a
        // process that isn't running in this pane would make a record nothing
        // ever clears.
        let Some(owner) = proc_info::owner_in_session(checked.pid, self.session_leader) else {
            tracing::debug!(
                "OSC 7577 report ignored: pid {} is not a live process in this pane",
                checked.pid
            );
            return;
        };
        self.last_time.insert(checked.pid, checked.time);
        self.records.retain(|r| r.owner.pid != checked.pid);
        match checked.verdict {
            Verdict::Resume(command) => {
                tracing::debug!("OSC 7577: pid {} reported {:?}", checked.pid, command.argv);
                self.next_seq += 1;
                self.records.push(Record {
                    owner,
                    command,
                    seq: self.next_seq,
                });
            }
            Verdict::Clear => {
                tracing::debug!("OSC 7577: pid {} cleared its record", checked.pid);
            }
            Verdict::Rejected(reason) => {
                tracing::debug!(
                    "OSC 7577 report from pid {} rejected: {reason}",
                    checked.pid
                );
            }
        }
    }

    /// Make `command` the record of `pid`, as if it had reported it: what a
    /// terminal does for a program it just resumed, which the spec has own
    /// the record it was resumed from until it reports for itself (or exits).
    pub fn adopt(&mut self, pid: u32, command: ResumeCommand) {
        let Some(owner) = proc_info::owner_in_session(pid, None) else {
            return;
        };
        self.records.retain(|r| r.owner.pid != pid);
        self.next_seq += 1;
        self.records.push(Record {
            owner,
            command,
            seq: self.next_seq,
        });
    }

    /// The command to resume this pane with, if any of its programs reported
    /// one and is still running.
    ///
    /// Records of exited owners are dropped first. Of those left, an
    /// ancestor's record wins over a descendant's (the outermost program is
    /// the one the pane was running; a child it started reports for itself
    /// only), and among unrelated ones the newest. A record without a `cwd`
    /// takes its owner's working directory, read now.
    pub fn current(&mut self) -> Option<ResumeCommand> {
        self.records.retain(|r| proc_info::is_alive(r.owner));
        let pids: Vec<u32> = self.records.iter().map(|r| r.owner.pid).collect();
        let record = self
            .records
            .iter()
            .filter(|r| {
                !pids.iter().any(|&other| {
                    other != r.owner.pid && proc_info::descends_from(r.owner.pid, other)
                })
            })
            .max_by_key(|r| r.seq)?;
        let mut command = record.command.clone();
        if command.cwd.is_none() {
            command.cwd = proc_info::cwd(record.owner.pid);
        }
        Some(command)
    }
}

/// The little of the process table the terminal side needs. Linux reads
/// `/proc`; other Unixes can only tell whether a pid is alive; Windows (where
/// the protocol's pids are rarely meaningful) trusts the report.
mod proc_info {
    use super::Owner;
    use std::path::PathBuf;

    /// Whether a process with this pid exists (and may be signalled by us).
    #[cfg(all(unix, not(target_os = "linux")))]
    fn process_exists(pid: u32) -> bool {
        // SAFETY: signal 0 only checks; it delivers nothing.
        unsafe { libc::kill(pid as libc::pid_t, 0) == 0 }
    }

    /// `(ppid, session, starttime)` from `/proc/<pid>/stat`.
    #[cfg(target_os = "linux")]
    fn stat(pid: u32) -> Option<(u32, u32, u64)> {
        let text = std::fs::read_to_string(format!("/proc/{pid}/stat")).ok()?;
        // The command name is in parentheses and may itself contain spaces
        // or parentheses; every field after the last `)` is plain.
        let rest = &text[text.rfind(')')? + 1..];
        let fields: Vec<&str> = rest.split_whitespace().collect();
        // rest[0] is field 3 (state): ppid is field 4, session field 6,
        // starttime field 22.
        let ppid = fields.get(1)?.parse().ok()?;
        let session = fields.get(3)?.parse().ok()?;
        let start = fields.get(19)?.parse().ok()?;
        Some((ppid, session, start))
    }

    /// `pid`'s identity, when it is a live process in `leader`'s session (or
    /// merely live, where the session can't be read or isn't known).
    pub fn owner_in_session(pid: u32, leader: Option<u32>) -> Option<Owner> {
        #[cfg(target_os = "linux")]
        {
            let (_, session, start) = stat(pid)?;
            if leader.is_some_and(|l| l != session) {
                return None;
            }
            Some(Owner {
                pid,
                start: Some(start),
            })
        }
        #[cfg(all(unix, not(target_os = "linux")))]
        {
            let _ = leader;
            let alive = process_exists(pid);
            alive.then_some(Owner { pid, start: None })
        }
        #[cfg(not(unix))]
        {
            let _ = leader;
            Some(Owner { pid, start: None })
        }
    }

    /// Whether the process `owner` names is still the one that reported.
    pub fn is_alive(owner: Owner) -> bool {
        #[cfg(target_os = "linux")]
        {
            stat(owner.pid).is_some_and(|(_, _, start)| Some(start) == owner.start)
        }
        #[cfg(all(unix, not(target_os = "linux")))]
        {
            process_exists(owner.pid)
        }
        #[cfg(not(unix))]
        {
            let _ = owner;
            true
        }
    }

    /// Whether `pid` is a (strict) descendant of `ancestor`.
    pub fn descends_from(pid: u32, ancestor: u32) -> bool {
        #[cfg(target_os = "linux")]
        {
            let mut current = pid;
            // Bounded: a cycle can't happen in a real process table, but a
            // racing read must not spin forever.
            for _ in 0..128 {
                match stat(current) {
                    Some((ppid, _, _)) if ppid == ancestor => return true,
                    Some((ppid, _, _)) if ppid > 1 => current = ppid,
                    _ => return false,
                }
            }
            false
        }
        #[cfg(not(target_os = "linux"))]
        {
            let _ = (pid, ancestor);
            false
        }
    }

    /// `pid`'s working directory, where the platform exposes it.
    pub fn cwd(pid: u32) -> Option<PathBuf> {
        #[cfg(target_os = "linux")]
        {
            std::fs::read_link(format!("/proc/{pid}/cwd")).ok()
        }
        #[cfg(not(target_os = "linux"))]
        {
            let _ = pid;
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The token the spec's examples are signed with.
    const SPEC_TOKEN: &str = "dummy-token-replace-me";
    const SPEC_TIME: u64 = 1795543200;

    fn cmd(argv: &[&str], cwd: Option<&str>) -> ResumeCommand {
        ResumeCommand {
            argv: argv.iter().map(|s| s.to_string()).collect(),
            cwd: cwd.map(PathBuf::from),
        }
    }

    /// The spec's example reports, byte for byte.
    #[test]
    fn encodes_the_spec_examples() {
        let nvim = cmd(
            &[
                "nvim",
                "-S",
                "/home/user/.local/state/nvim/sessions/8f2c1d.vim",
            ],
            None,
        );
        assert_eq!(
            encode_sequence(74877, SPEC_TIME, Report::Resume(&nvim), SPEC_TOKEN).unwrap(),
            "\x1b]7577;pid=74877:time=1795543200:argv=bnZpbQ==,LVM=,\
             L2hvbWUvdXNlci8ubG9jYWwvc3RhdGUvbnZpbS9zZXNzaW9ucy84ZjJjMWQudmlt:\
             mac=9dbb99bfb856749a87563839d7ce9062818d78cf25909db93f309e0cd3b1909b\x1b\\"
        );
        assert_eq!(
            encode_sequence(74877, SPEC_TIME, Report::Clear, SPEC_TOKEN).unwrap(),
            "\x1b]7577;pid=74877:time=1795543200:clear=1:\
             mac=c30603369f98fef581a4098be889175b0c74f0641e49f8b25f4326cb38f97c93\x1b\\"
        );
        let lazygit = cmd(&["lazygit"], Some("/home/user/src/project"));
        assert_eq!(
            encode_sequence(74877, SPEC_TIME, Report::Resume(&lazygit), SPEC_TOKEN).unwrap(),
            "\x1b]7577;pid=74877:time=1795543200:argv=bGF6eWdpdA==:\
             cwd=L2hvbWUvdXNlci9zcmMvcHJvamVjdA==:\
             mac=35cb909805c380336efd3689a125acc774a9dec8422a1d21492ebec06ca758e1\x1b\\"
        );
    }

    fn payload(seq: &str) -> &str {
        seq.strip_prefix("\x1b]7577;")
            .and_then(|s| s.strip_suffix("\x1b\\"))
            .unwrap()
    }

    #[test]
    fn checks_the_spec_examples() {
        let lazygit = cmd(&["lazygit"], Some("/home/user/src/project"));
        let seq = encode_sequence(74877, SPEC_TIME, Report::Resume(&lazygit), SPEC_TOKEN).unwrap();
        assert_eq!(
            check_report(payload(&seq), SPEC_TOKEN, SPEC_TIME + 30),
            Ok(Checked {
                pid: 74877,
                time: SPEC_TIME,
                verdict: Verdict::Resume(lazygit),
            })
        );
        let clear = encode_sequence(74877, SPEC_TIME, Report::Clear, SPEC_TOKEN).unwrap();
        assert_eq!(
            check_report(payload(&clear), SPEC_TOKEN, SPEC_TIME).map(|c| c.verdict),
            Ok(Verdict::Clear)
        );
    }

    #[test]
    fn ignores_reports_that_fail_the_mac_or_window() {
        let seq = encode_sequence(7, SPEC_TIME, Report::Clear, SPEC_TOKEN).unwrap();
        let p = payload(&seq);
        assert_eq!(
            check_report(p, "another-token-of-22-chars", SPEC_TIME),
            Err(Ignored::BadMac)
        );
        assert_eq!(
            check_report(p, SPEC_TOKEN, SPEC_TIME + TIME_WINDOW_SECS + 1),
            Err(Ignored::Stale)
        );
        assert_eq!(
            check_report("?", SPEC_TOKEN, SPEC_TIME),
            Err(Ignored::Reserved)
        );
        assert_eq!(
            check_report("pid=7:time=1795543200:clear=1", SPEC_TOKEN, SPEC_TIME),
            Err(Ignored::MissingMac)
        );
        // `mac` must be the last key.
        let body = "pid=7:time=1795543200:clear=1";
        let mac = compute_mac(body, SPEC_TOKEN);
        assert_eq!(
            check_report(&format!("mac={mac}:{body}"), SPEC_TOKEN, SPEC_TIME),
            Err(Ignored::MissingMac)
        );
    }

    /// Sign an arbitrary body, the way a (possibly buggy) program would.
    fn signed(body: &str) -> String {
        format!("{body}:mac={}", compute_mac(body, SPEC_TOKEN))
    }

    fn verdict(body: &str) -> Verdict {
        check_report(&signed(body), SPEC_TOKEN, SPEC_TIME)
            .unwrap()
            .verdict
    }

    #[test]
    fn rejects_well_signed_reports_that_break_the_rules() {
        let t = "pid=7:time=1795543200";
        let b64 = |s: &str| STANDARD.encode(s);
        let rejected = |body: String| {
            assert!(
                matches!(verdict(&body), Verdict::Rejected(_)),
                "should reject {body}"
            );
        };
        rejected(format!("{t}:argv={}:clear=1", b64("vim")));
        rejected(t.to_string());
        rejected(format!("{t}:argv={}", b64("bin/vim")));
        rejected(format!("{t}:argv={}", b64("-vim")));
        rejected(format!("{t}:argv={}:cwd={}", b64("vim"), b64("src")));
        rejected(format!("{t}:argv={}:need=env", b64("vim")));
        rejected(format!("{t}:argv={}:argv={}", b64("vim"), b64("vim")));
        rejected(format!("{t}:argv={}", b64("vim\nrm")));
        rejected(format!("{t}:argv=!!!"));
        rejected(format!("{t}:clear=2"));
        rejected(format!("{t}:argv={},{}", b64(""), b64("x")));
    }

    #[test]
    fn accepts_the_lenient_forms_the_spec_allows() {
        let t = "pid=7:time=1795543200";
        // Padding optional, empty non-program elements, unknown keys, a need
        // of implemented keys.
        assert_eq!(
            verdict(&format!("{t}:argv=dmlt,,LVM:future=x:need=argv,cwd")),
            Verdict::Resume(cmd(&["vim", "", "-S"], None))
        );
        assert_eq!(
            verdict(&format!("{t}:argv={}", STANDARD.encode("/usr/bin/vim"))),
            Verdict::Resume(cmd(&["/usr/bin/vim"], None))
        );
    }

    #[test]
    fn program_side_refuses_what_it_cannot_express() {
        let bad = cmd(&["./fresh"], None);
        assert!(encode_body(1, 1, Report::Resume(&bad)).is_none());
        let long = cmd(&["fresh", &"x".repeat(3000)], None);
        assert!(encode_sequence(1, 1, Report::Resume(&long), SPEC_TOKEN).is_none());
    }

    #[test]
    fn reporter_times_strictly_increase_and_fall_back_to_clear() {
        let mut reporter = ProgramReporter::new(SPEC_TOKEN.to_string(), 42);
        let first = reporter
            .report(cmd(&["fresh", "--restore"], Some("/tmp")))
            .unwrap();
        let second = reporter.refresh().unwrap();
        let time = |seq: &str| {
            check_report(payload(seq), SPEC_TOKEN, unix_now())
                .unwrap()
                .time
        };
        assert!(time(&second) > time(&first));
        // A command that can't be expressed withdraws the old one.
        let fallback = reporter.report(cmd(&["./relative"], None)).unwrap();
        assert_eq!(
            check_report(payload(&fallback), SPEC_TOKEN, unix_now())
                .unwrap()
                .verdict,
            Verdict::Clear
        );
        assert!(reporter.refresh().is_none());
    }

    #[test]
    fn scanner_finds_reports_split_across_reads_with_either_terminator() {
        let mut scanner = ReportScanner::default();
        let mut out = Vec::new();
        scanner.feed(b"hello \x1b]75", &mut out);
        scanner.feed(b"77;pid=1:mac=ab\x1b", &mut out);
        assert!(out.is_empty());
        scanner.feed(b"\\ world \x1b]7577;x\x07 \x1b]7;file:///tmp\x07", &mut out);
        assert_eq!(out, vec!["pid=1:mac=ab".to_string(), "x".to_string()]);
    }

    #[test]
    fn token_shape() {
        assert!(is_valid_token(&new_token()));
        assert_eq!(new_token().len(), 64);
        assert!(is_valid_token(SPEC_TOKEN));
        assert!(!is_valid_token("short"));
        assert!(!is_valid_token("has a space in it, which is not ok"));
    }

    /// The tracker end to end against this test process itself, which is a
    /// live pid (in whatever session the test runs in).
    #[test]
    fn tracker_records_replaces_and_clears() {
        let pid = std::process::id();
        let now = unix_now();
        let mut tracker = ResumeTracker::new(SPEC_TOKEN.to_string());
        let report = |cmd: &ResumeCommand, time| {
            payload(&encode_sequence(pid, time, Report::Resume(cmd), SPEC_TOKEN).unwrap())
                .to_string()
        };
        let vim = cmd(&["vim"], Some("/tmp"));
        tracker.handle_payload(&report(&vim, now), now);
        assert_eq!(tracker.current(), Some(vim.clone()));

        // A replay of an older time is ignored; a newer report replaces.
        let htop = cmd(&["htop"], Some("/"));
        tracker.handle_payload(&report(&htop, now), now);
        assert_eq!(tracker.current(), Some(vim));
        tracker.handle_payload(&report(&htop, now + 1), now);
        assert_eq!(tracker.current(), Some(htop));

        // A well-signed report that breaks a rule drops the owner's record.
        let body = format!("pid={pid}:time={}:argv=!!", now + 2);
        tracker.handle_payload(&signed(&body), now);
        assert_eq!(tracker.current(), None);

        let vim = cmd(&["vim"], None);
        tracker.handle_payload(&report(&vim, now + 3), now);
        // No cwd key: the owner's own working directory is used.
        #[cfg(target_os = "linux")]
        assert_eq!(
            tracker.current().and_then(|c| c.cwd),
            std::env::current_dir().ok()
        );
        let clear = encode_sequence(pid, now + 4, Report::Clear, SPEC_TOKEN).unwrap();
        tracker.handle_payload(payload(&clear), now);
        assert_eq!(tracker.current(), None);
    }

    #[test]
    fn tracker_adopts_a_resumed_programs_record_until_it_reports() {
        let pid = std::process::id();
        let now = unix_now();
        let mut tracker = ResumeTracker::new(SPEC_TOKEN.to_string());
        let resumed = cmd(&["vim", "-S", "/tmp/s.vim"], Some("/tmp"));
        tracker.adopt(pid, resumed.clone());
        assert_eq!(tracker.current(), Some(resumed));
        // Its own report supersedes the adopted record.
        let own = cmd(&["vim"], Some("/"));
        let seq = encode_sequence(pid, now, Report::Resume(&own), SPEC_TOKEN).unwrap();
        tracker.handle_payload(payload(&seq), now);
        assert_eq!(tracker.current(), Some(own));
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn tracker_ignores_processes_outside_the_pane() {
        let pid = std::process::id();
        let now = unix_now();
        let mut tracker = ResumeTracker::new(SPEC_TOKEN.to_string());
        // A session leader this process does not belong to.
        tracker.set_session_leader(Some(u32::MAX - 1));
        let seq =
            encode_sequence(pid, now, Report::Resume(&cmd(&["vim"], None)), SPEC_TOKEN).unwrap();
        tracker.handle_payload(payload(&seq), now);
        assert_eq!(tracker.current(), None);
    }
}
