//! Terminals: a PTY on the workspace's host, and its raw output.
//!
//! The server does not emulate a terminal. It spawns the host's shell on a
//! PTY (through the workspace's [`Authority`](crate::authority::Authority), so
//! the same code serves a local shell, an SSH session or a devcontainer) and
//! streams the bytes. Each client runs its own terminal emulator (xterm.js)
//! on them. A replay buffer of recent output lets a client that attaches
//! late, or reconnects, rebuild the screen: every chunk carries its offset in
//! the stream, and the replay says where it ends, so nothing is shown twice.
//!
//! A terminal may run a command instead of an interactive shell: an **agent
//! session** (a coding agent's CLI, or any long-running program). The server
//! watches every terminal's stream for what the orchestrator shows at a
//! glance: the title a program sets (OSC 0/2), the bell, and when output last
//! came, from which a terminal's state is read ([`State`]).

use crate::authority::{Authority, HostProc};
use crate::hub::Room;
use crate::workspace::Workspace;
use anyhow::{anyhow, Result};
use base64::Engine;
use portable_pty::{native_pty_system, Child, MasterPty, PtySize};
use serde_json::{json, Value};
use std::collections::{HashMap, VecDeque};
use std::io::{Read, Write};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

/// How much recent output is kept for replay.
const REPLAY_BYTES: usize = 1 << 20;

/// Output within this long means a terminal is working (the editor's
/// orchestrator's idle window).
pub const IDLE_AFTER: Duration = Duration::from_secs(5);

/// A run of output at least this long is work: once the terminal goes
/// quiet, it is finished work nobody has looked at yet.
pub const WORK_MIN: Duration = Duration::from_millis(1500);

/// How many of the screen's last lines are read for a question.
const RECENT_LINES: usize = 3;

/// The lines that read as the program waiting on the user: the editor's
/// orchestrator's built-in detection rules (case-insensitive).
const QUESTION_RULES: &[&str] = &[
    r"\(y/n\)|\[y/n\]|\(yes/no\)",
    r"\bdo you want to\b",
    r"\b(proceed|continue|allow|approve|confirm|accept)\?",
    r"\bpress (enter|return)\b",
    r"[❯>]\s*1[.)]\s*yes",
    r"\bdon'?t ask again\b",
    r"\bwaiting for (your )?(input|approval|confirmation)\b",
    r"\besc(ape)? to cancel\b",
    r"\benter to (confirm|select|continue)\b",
    r"\bsyntax theme: .*\(ctrl\+t to (disable|enable)\)",
    r"\bselect login method\b",
];

/// What a terminal is doing, as read from its stream (the editor's
/// orchestrator's states).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum State {
    /// Output within [`IDLE_AFTER`], or shell integration (OSC 133, OSC
    /// 9;4) says a command runs.
    Working,
    /// Quiet, and one of its last lines asks the user something.
    Waiting,
    /// Quiet after work nobody has seen yet (`workspace.seen` sees it).
    Done,
    /// Quiet.
    Idle,
    /// No output yet.
    Unknown,
    /// Its program ended.
    Exited,
}

impl State {
    pub fn as_str(self) -> &'static str {
        match self {
            State::Working => "working",
            State::Waiting => "waiting",
            State::Done => "done",
            State::Idle => "idle",
            State::Unknown => "unknown",
            State::Exited => "exited",
        }
    }
}

/// Where the scanner is in the stream: escape sequences are followed only
/// as far as telling a title (OSC 0/2) and a bell apart, since an OSC ends
/// with the same BEL byte that rings the bell.
#[derive(Default)]
enum Lexer {
    #[default]
    Text,
    Escape,
    Osc(Vec<u8>),
    OscEscape(Vec<u8>),
}

/// What the stream told so far.
struct Activity {
    esc: Lexer,
    title: String,
    bells: u64,
    last_output: Option<Instant>,
    /// When the current run of output began.
    burst: Instant,
    /// A run of output since the user last looked was work ([`WORK_MIN`]).
    worked: bool,
    /// Shell integration's word: a command runs (`Some(true)`), or the last
    /// one finished.
    running: Option<bool>,
}

impl Activity {
    fn new(now: Instant) -> Self {
        Activity {
            esc: Lexer::Text,
            title: String::new(),
            bells: 0,
            last_output: None,
            burst: now,
            worked: false,
            running: None,
        }
    }

    fn scan(&mut self, bytes: &[u8], now: Instant) {
        if self
            .last_output
            .is_none_or(|t| now.duration_since(t) >= IDLE_AFTER)
        {
            self.burst = now;
        }
        if now.duration_since(self.burst) >= WORK_MIN {
            self.worked = true;
        }
        self.last_output = Some(now);
        for &b in bytes {
            self.esc = match std::mem::take(&mut self.esc) {
                Lexer::Text => match b {
                    0x07 => {
                        self.bells += 1;
                        Lexer::Text
                    }
                    0x1b => Lexer::Escape,
                    _ => Lexer::Text,
                },
                Lexer::Escape => match b {
                    b']' => Lexer::Osc(Vec::new()),
                    0x1b => Lexer::Escape,
                    _ => Lexer::Text,
                },
                Lexer::Osc(mut s) => match b {
                    0x07 => {
                        self.osc(&s);
                        Lexer::Text
                    }
                    0x1b => Lexer::OscEscape(s),
                    _ => {
                        if s.len() < 4096 {
                            s.push(b);
                        }
                        Lexer::Osc(s)
                    }
                },
                Lexer::OscEscape(s) => {
                    // ST (ESC \) ends it; anything else abandons it.
                    if b == b'\\' {
                        self.osc(&s);
                    }
                    Lexer::Text
                }
            }
        }
    }

    /// A title (OSC 0/2), or shell integration's command marks (OSC 133:
    /// `C` a command starts, `D` it ended, `A` a prompt) and progress (OSC
    /// 9;4: state 0 ends it).
    fn osc(&mut self, s: &[u8]) {
        let s = String::from_utf8_lossy(s);
        if let Some(t) = s.strip_prefix("0;").or_else(|| s.strip_prefix("2;")) {
            self.title = t.chars().take(200).collect();
        } else if let Some(mark) = s.strip_prefix("133;") {
            match mark.chars().next() {
                Some('C') => self.running = Some(true),
                Some('D' | 'A') => self.running = Some(false),
                _ => {}
            }
        } else if let Some(progress) = s.strip_prefix("9;4;") {
            self.running = Some(!progress.starts_with('0'));
        }
    }

    fn state(&self, exited: bool, replay: &VecDeque<u8>, now: Instant) -> State {
        if exited {
            return State::Exited;
        }
        if self.running == Some(true) {
            return State::Working;
        }
        let Some(last) = self.last_output else {
            return State::Unknown;
        };
        if self.running != Some(false) && now.duration_since(last) < IDLE_AFTER {
            return State::Working;
        }
        if recent_lines(replay).iter().any(|l| asks(l)) {
            State::Waiting
        } else if self.worked {
            State::Done
        } else {
            State::Idle
        }
    }
}

struct Output {
    /// The last [`REPLAY_BYTES`] bytes, ending at stream offset `end`.
    replay: VecDeque<u8>,
    end: u64,
    exited: bool,
    activity: Activity,
}

/// The last lines of text on the screen, roughly: the tail of the stream
/// without escape sequences, its last [`RECENT_LINES`] lines holding
/// anything.
fn recent_lines(replay: &VecDeque<u8>) -> Vec<String> {
    let n = replay.len();
    let tail: Vec<u8> = replay.range(n.saturating_sub(4096)..).copied().collect();
    let text = String::from_utf8_lossy(&tail);
    let plain = strip_escapes(&text);
    let mut lines: Vec<String> = plain
        .split(['\n', '\r'])
        .map(str::trim)
        .filter(|l| !l.is_empty())
        .rev()
        .take(RECENT_LINES)
        .map(str::to_string)
        .collect();
    lines.reverse();
    lines
}

fn strip_escapes(s: &str) -> String {
    static RE: std::sync::OnceLock<regex::Regex> = std::sync::OnceLock::new();
    let re = RE.get_or_init(|| {
        regex::Regex::new(r"\x1b\][^\x07\x1b]*(\x07|\x1b\\)|\x1b\[[0-9;?<>=!]*[ -/]*[@-~]|\x1b[@-Z\\-_]|[\x00-\x08\x0b-\x0c\x0e-\x1f\x7f]")
            .unwrap()
    });
    re.replace_all(s, "").into_owned()
}

/// Whether a line asks the user something, by [`QUESTION_RULES`].
fn asks(line: &str) -> bool {
    static RE: std::sync::OnceLock<regex::RegexSet> = std::sync::OnceLock::new();
    let re = RE.get_or_init(|| {
        regex::RegexSetBuilder::new(QUESTION_RULES)
            .case_insensitive(true)
            .build()
            .unwrap()
    });
    re.is_match(line)
}

struct Term {
    out: Arc<Mutex<Output>>,
    writer: Box<dyn Write + Send>,
    master: Box<dyn MasterPty + Send>,
    child: Box<dyn Child + Send + Sync>,
    size: (u16, u16),
    authority: Arc<dyn Authority>,
    /// The shell on the host.
    host: HostProc,
    /// The name it was opened with.
    name: String,
    /// The command it runs, `None` for an interactive shell.
    command: Option<String>,
}

impl Term {
    fn describe(&self, id: u64, now: Instant) -> Value {
        let o = self.out.lock().unwrap();
        let state = o.activity.state(o.exited, &o.replay, now);
        json!({
            "id": id, "name": self.name, "command": self.command, "agent": self.command.is_some(),
            "title": o.activity.title, "state": state.as_str(), "bells": o.activity.bells,
        })
    }
}

#[derive(Default)]
pub struct Terms {
    map: Mutex<HashMap<u64, Term>>,
    next: AtomicU64,
}

/// A command line as a terminal's name: itself, cut to 32 characters.
fn short(command: &str) -> String {
    let c = command.split_whitespace().collect::<Vec<_>>().join(" ");
    if c.chars().count() <= 32 {
        c
    } else {
        format!("{}…", c.chars().take(31).collect::<String>())
    }
}

fn b64(bytes: &[u8]) -> String {
    base64::engine::general_purpose::STANDARD.encode(bytes)
}

fn not_found(id: u64) -> anyhow::Error {
    crate::error::err(crate::error::Code::NotFound, format!("no terminal {id}"))
}

impl Terms {
    /// A terminal running the user's shell, or `command` in it, in the
    /// workspace's root. `name` labels it (default: the command, or
    /// "Terminal").
    pub fn open(
        &self,
        room: Room,
        ws: &Workspace,
        size: (u16, u16),
        name: Option<&str>,
        command: Option<&str>,
    ) -> Result<u64> {
        let (cols, rows) = size;
        let pair = native_pty_system()
            .openpty(PtySize {
                rows,
                cols,
                pixel_width: 0,
                pixel_height: 0,
            })
            .map_err(|e| anyhow!("{e}"))?;
        let (builder, host) = ws.authority.shell_command(&ws.root, command);
        let child = pair
            .slave
            .spawn_command(builder)
            .map_err(|e| anyhow!("{e}"))?;
        drop(pair.slave);
        let mut reader = pair.master.try_clone_reader().map_err(|e| anyhow!("{e}"))?;
        let writer = pair.master.take_writer().map_err(|e| anyhow!("{e}"))?;
        let id = self.next.fetch_add(1, Ordering::SeqCst) + 1;
        let out = Arc::new(Mutex::new(Output {
            replay: VecDeque::new(),
            end: 0,
            exited: false,
            activity: Activity::new(Instant::now()),
        }));
        {
            let out = out.clone();
            let (authority, host) = (ws.authority.clone(), host.clone());
            std::thread::spawn(move || {
                let mut buf = [0u8; 32768];
                loop {
                    match reader.read(&mut buf) {
                        Ok(0) | Err(_) => break,
                        Ok(n) => {
                            // Recorded and sent under one lock, so the offsets
                            // the clients see are in order.
                            let mut o = out.lock().unwrap();
                            let at = o.end;
                            o.replay.extend(&buf[..n]);
                            let over = o.replay.len().saturating_sub(REPLAY_BYTES);
                            o.replay.drain(..over);
                            o.end += n as u64;
                            o.activity.scan(&buf[..n], Instant::now());
                            room.broadcast(
                                &json!({"ev": "term.data", "term": id, "at": at, "data": b64(&buf[..n])}),
                            );
                        }
                    }
                }
                out.lock().unwrap().exited = true;
                room.broadcast(&json!({"ev": "term.exit", "term": id}));
                // The shell's pid file outlives it (the shell replaced the
                // script that would have removed it).
                authority.release(&host);
            });
        }
        let name = name
            .map(str::to_string)
            .or_else(|| command.map(short))
            .unwrap_or_else(|| "Terminal".into());
        self.map.lock().unwrap().insert(
            id,
            Term {
                out,
                writer,
                master: pair.master,
                child,
                size: (cols, rows),
                authority: ws.authority.clone(),
                host,
                name,
                command: command.map(str::to_string),
            },
        );
        Ok(id)
    }

    /// The replay buffer, the stream offset it ends at, and what the
    /// terminal is.
    pub fn attach(&self, id: u64) -> Result<Value> {
        let m = self.map.lock().unwrap();
        let t = m.get(&id).ok_or_else(|| not_found(id))?;
        let mut v = t.describe(id, Instant::now());
        let o = t.out.lock().unwrap();
        let (a, b) = o.replay.as_slices();
        let mut bytes = a.to_vec();
        bytes.extend_from_slice(b);
        v["data"] = json!(b64(&bytes));
        v["end"] = json!(o.end);
        v["exited"] = json!(o.exited);
        v["cols"] = json!(t.size.0);
        v["rows"] = json!(t.size.1);
        Ok(v)
    }

    pub fn input(&self, id: u64, data: &str) -> Result<()> {
        let mut m = self.map.lock().unwrap();
        let t = m.get_mut(&id).ok_or_else(|| not_found(id))?;
        t.writer.write_all(data.as_bytes())?;
        t.writer.flush()?;
        Ok(())
    }

    /// Size the PTY. With several clients attached, the last to resize wins,
    /// as with a shared tmux window.
    pub fn resize(&self, room: &Room, id: u64, cols: u16, rows: u16) -> Result<()> {
        let mut m = self.map.lock().unwrap();
        let t = m.get_mut(&id).ok_or_else(|| not_found(id))?;
        if t.size != (cols, rows) {
            t.master
                .resize(PtySize {
                    rows,
                    cols,
                    pixel_width: 0,
                    pixel_height: 0,
                })
                .map_err(|e| anyhow!("{e}"))?;
            t.size = (cols, rows);
            room.broadcast(&json!({"ev": "term.size", "term": id, "cols": cols, "rows": rows}));
        }
        Ok(())
    }

    pub fn close(&self, id: u64) {
        let term = self.map.lock().unwrap().remove(&id);
        if let Some(mut t) = term {
            // A hang-up to the shell on the host (it passes it on to its
            // jobs), then the local end. The local child lives as long as
            // the shell (or the link to it): only a running one is stopped.
            let running = matches!(t.child.try_wait(), Ok(None));
            if running {
                t.authority.stop(&t.host);
            }
            let _ = t.child.kill();
            let _ = t.child.wait();
            if running {
                t.authority.release(&t.host);
            }
        }
    }

    pub fn close_all(&self) {
        for id in self.ids() {
            self.close(id);
        }
    }

    /// The user has seen what every terminal did: finished work is idle.
    pub fn seen(&self) {
        for t in self.map.lock().unwrap().values() {
            t.out.lock().unwrap().activity.worked = false;
        }
    }

    pub fn ids(&self) -> Vec<u64> {
        let mut v: Vec<u64> = self.map.lock().unwrap().keys().copied().collect();
        v.sort();
        v
    }

    /// Every terminal, in the order opened: `{id, name, command, agent,
    /// title, state, bells}`.
    pub fn list(&self) -> Vec<Value> {
        let now = Instant::now();
        let m = self.map.lock().unwrap();
        let mut ids: Vec<&u64> = m.keys().collect();
        ids.sort();
        ids.into_iter().map(|id| m[id].describe(*id, now)).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn activity() -> Activity {
        Activity::new(Instant::now())
    }

    #[test]
    fn titles_and_bells_are_told_apart() {
        let mut a = activity();
        let now = Instant::now();
        // A title ended by BEL is no bell; one split across chunks is read
        // whole; ST ends one too.
        a.scan(b"\x1b]0;build: running\x07$ ", now);
        assert_eq!((a.title.as_str(), a.bells), ("build: running", 0));
        a.scan(b"\x1b]2;half", now);
        a.scan(b" and half\x1b\\done\x07", now);
        assert_eq!((a.title.as_str(), a.bells), ("half and half", 1));
        a.scan(b"\x1b[31mred\x1b[0m\x07\x07", now);
        assert_eq!(a.bells, 3);
    }

    #[test]
    fn the_last_lines_are_read_without_escapes() {
        let replay: VecDeque<u8> =
            b"one\r\ntwo\r\nworking...\r\n\x1b[1mDo you want to proceed? \x1b[0m\r\n\x1b[K  \r\n"
                .iter()
                .copied()
                .collect();
        let lines = recent_lines(&replay);
        assert_eq!(lines, ["two", "working...", "Do you want to proceed?"]);
        assert!(asks(&lines[2]));
        assert!(asks("Overwrite file [y/N]"));
        assert!(asks("Continue (yes/no)"));
        assert!(asks("❯ 1. Yes"));
        assert!(asks("Enter to confirm · Esc to cancel"));
        assert!(!asks("user@host:~/src$"));
        assert!(!asks("done: 3 files"));
    }

    #[test]
    fn states_follow_output_work_and_shell_integration() {
        let t0 = Instant::now();
        let at = |ms: u64| t0 + Duration::from_millis(ms);
        let empty = VecDeque::new();
        let mut a = activity();
        assert_eq!(a.state(false, &empty, at(0)), State::Unknown);
        // A prompt: output, but no run of work.
        a.scan(b"$ ", at(0));
        assert_eq!(a.state(false, &empty, at(100)), State::Working);
        assert_eq!(a.state(false, &empty, at(6000)), State::Idle);
        // Output for two seconds: work; quiet after it, done until seen.
        for ms in (7000..=9000).step_by(500) {
            a.scan(b"x", at(ms));
        }
        assert_eq!(a.state(false, &empty, at(9500)), State::Working);
        assert_eq!(a.state(false, &empty, at(15000)), State::Done);
        a.worked = false;
        assert_eq!(a.state(false, &empty, at(15000)), State::Idle);
        // Shell integration: a running command works while silent; a
        // finished one is quiet at once.
        a.scan(b"\x1b]133;C\x07", at(20000));
        assert_eq!(a.state(false, &empty, at(60000)), State::Working);
        a.scan(b"\x1b]133;D;0\x07", at(60000));
        assert_eq!(a.state(false, &empty, at(60001)), State::Idle);
        a.scan(b"\x1b]9;4;1;50\x07", at(61000));
        assert_eq!(a.state(false, &empty, at(90000)), State::Working);
        a.scan(b"\x1b]9;4;0;0\x07", at(90000));
        assert_eq!(a.state(false, &empty, at(90001)), State::Idle);
        assert_eq!(a.state(true, &empty, at(90001)), State::Exited);
    }
}
