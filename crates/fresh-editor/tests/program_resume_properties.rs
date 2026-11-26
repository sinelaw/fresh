//! Property tests for both sides of OSC 7577 ("Program Resume").
//!
//! **Program side** (fresh reporting to the terminal it runs in): whatever
//! command fresh reports, a conforming terminal reads back exactly that
//! command, from exactly that pid, at exactly that time; any change to the
//! bytes on the wire, or a different token, gets the report ignored; and a
//! reporter's stream of reports, refreshes and clears always verifies, with
//! strictly increasing times, saying what a model of it says.
//!
//! **Terminal side** (fresh hosting programs in its terminals): a random
//! stream of valid, forged, replayed, clearing and malformed reports, mixed
//! with ordinary output and cut into arbitrary PTY reads, leaves the
//! terminal's resume record where a model of the spec's rules says it should
//! be.

use fresh::services::program_resume::{
    check_report, encode_body, encode_sequence, new_token, unix_now, Ignored, ProgramReporter,
    Report, ResumeCommand, ResumeRecord, ResumeTracker, Verdict, MIN_ACCEPTED_SEQUENCE_BYTES,
    TIME_WINDOW_SECS,
};
use fresh::services::terminal::TerminalState;
use proptest::prelude::*;
use std::path::PathBuf;

// ---------------------------------------------------------------------------
// Generators
// ---------------------------------------------------------------------------

/// A token of the shape a terminal hands out.
fn token() -> impl Strategy<Value = String> {
    "[A-Za-z0-9_-]{22,64}"
}

/// A program: a bare name or an absolute path.
fn program() -> impl Strategy<Value = String> {
    prop_oneof![
        "[a-z][a-z0-9_.+]{0,12}",
        "(/[a-zA-Z0-9_. -]{1,10}){1,4}",
    ]
}

/// Any argument text the protocol can carry: no control characters, but
/// otherwise anything (spaces, quotes, `:`/`=`/`,`, non-ASCII).
fn argument() -> impl Strategy<Value = String> {
    "\\PC{0,16}"
}

fn command() -> impl Strategy<Value = ResumeCommand> {
    (
        program(),
        prop::collection::vec(argument(), 0..6),
        prop::option::of("/\\PC{0,24}"),
    )
        .prop_map(|(program, args, cwd)| ResumeCommand {
            argv: std::iter::once(program).chain(args).collect(),
            cwd: cwd.map(PathBuf::from),
        })
}

/// The payload of a sequence: what a terminal's scanner hands its verifier.
fn payload(seq: &str) -> &str {
    seq.strip_prefix("\x1b]7577;")
        .and_then(|s| s.strip_suffix("\x1b\\"))
        .expect("a report is framed as ESC ] 7577 ; … ESC \\")
}

// ---------------------------------------------------------------------------
// Program side
// ---------------------------------------------------------------------------

/// What the program side is asked to do, for the reporter model.
#[derive(Debug, Clone)]
enum ProgramOp {
    Report(ResumeCommand),
    /// A command the protocol can't carry (relative program path).
    ReportInexpressible,
    Refresh,
    Clear,
}

fn program_op() -> impl Strategy<Value = ProgramOp> {
    prop_oneof![
        4 => command().prop_map(ProgramOp::Report),
        1 => Just(ProgramOp::ReportInexpressible),
        2 => Just(ProgramOp::Refresh),
        1 => Just(ProgramOp::Clear),
    ]
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(256))]

    /// Round trip: a command fresh can report is read back unchanged, and
    /// one it can't is one that would not fit in what every terminal takes.
    #[test]
    fn program_reports_round_trip(
        cmd in command(),
        pid in 1u32..,
        time in 1u64..4_000_000_000,
        token in token(),
    ) {
        match encode_sequence(pid, time, Report::Resume(&cmd), &token) {
            Some(seq) => {
                prop_assert!(seq.len() <= MIN_ACCEPTED_SEQUENCE_BYTES);
                prop_assert!(!payload(&seq).bytes().any(|b| b.is_ascii_control()));
                let checked = check_report(payload(&seq), &token, time).unwrap();
                prop_assert_eq!(checked.pid, pid);
                prop_assert_eq!(checked.time, time);
                prop_assert_eq!(checked.verdict, Verdict::Resume(cmd));
            }
            None => {
                let body = encode_body(pid, time, Report::Resume(&cmd))
                    .expect("generated commands are expressible");
                // ESC ] 7577 ; <body> :mac= <64 hex> ESC \
                prop_assert!(7 + body.len() + 5 + 64 + 2 > MIN_ACCEPTED_SEQUENCE_BYTES);
            }
        }
    }

    /// Integrity: changing any one byte of a report, or checking it with
    /// another pane's token, gets it ignored — never read as some other
    /// command.
    #[test]
    fn program_reports_cannot_be_altered_or_replayed_elsewhere(
        cmd in command(),
        pid in 1u32..,
        token in token(),
        other in token(),
        index in any::<prop::sample::Index>(),
        replacement in 0x20u8..0x7f,
    ) {
        let now = unix_now();
        let Some(seq) = encode_sequence(pid, now, Report::Resume(&cmd), &token) else {
            return Ok(());
        };
        let mut bytes = payload(&seq).as_bytes().to_vec();
        let i = index.index(bytes.len());
        prop_assume!(bytes[i] != replacement);
        bytes[i] = replacement;
        let tampered = String::from_utf8(bytes).unwrap();
        prop_assert!(check_report(&tampered, &token, now).is_err());

        prop_assume!(other != token);
        prop_assert_eq!(check_report(payload(&seq), &other, now), Err(Ignored::BadMac));
        prop_assert_eq!(
            check_report(payload(&seq), &token, now + TIME_WINDOW_SECS + 1),
            Err(Ignored::Stale)
        );
    }

    /// The reporter against a model: every sequence it emits verifies, times
    /// strictly increase (so a terminal never drops one as a replay), a
    /// refresh repeats the last command, and an inexpressible command or a
    /// quit withdraws it — and only it: with nothing reported, nothing is
    /// sent.
    #[test]
    fn program_reporter_matches_its_model(
        ops in prop::collection::vec(program_op(), 1..24),
        pid in 1u32..,
        token in token(),
    ) {
        let mut reporter = ProgramReporter::new(token.clone(), pid);
        let mut model: Option<ResumeCommand> = None;
        let mut last_time = 0u64;
        for op in ops {
            let (sent, expected) = match op {
                ProgramOp::Report(cmd) => {
                    let sent = reporter.report(cmd.clone());
                    // Too long to send: the reporter withdraws instead.
                    let fits = encode_sequence(pid, 1, Report::Resume(&cmd), &token).is_some();
                    let expected = if fits {
                        Some(Verdict::Resume(cmd.clone()))
                    } else {
                        model.as_ref().map(|_| Verdict::Clear)
                    };
                    model = fits.then_some(cmd);
                    (sent, expected)
                }
                ProgramOp::ReportInexpressible => {
                    let expected = model.take().map(|_| Verdict::Clear);
                    let cmd = ResumeCommand { argv: vec!["./relative".into()], cwd: None };
                    (reporter.report(cmd), expected)
                }
                ProgramOp::Refresh => {
                    (reporter.refresh(), model.clone().map(Verdict::Resume))
                }
                ProgramOp::Clear => {
                    let expected = model.take().map(|_| Verdict::Clear);
                    (reporter.clear(), expected)
                }
            };
            match (sent, expected) {
                (None, None) => {}
                (Some(seq), Some(expected)) => {
                    let checked = check_report(payload(&seq), &token, unix_now()).unwrap();
                    prop_assert_eq!(checked.pid, pid);
                    prop_assert!(checked.time > last_time, "times must strictly increase");
                    last_time = checked.time;
                    prop_assert_eq!(checked.verdict, expected);
                }
                (sent, expected) => {
                    prop_assert!(false, "sent {:?}, model expected {:?}", sent, expected);
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Terminal side
// ---------------------------------------------------------------------------

/// One thing that appears in a terminal's output.
#[derive(Debug, Clone)]
enum TerminalOp {
    /// A correctly signed report of this command, newer than any before.
    Report(ResumeCommand),
    /// The same, signed with some other token.
    Forged(ResumeCommand),
    /// An earlier report sent again (the program's own bytes, replayed).
    Replay(prop::sample::Index),
    /// A correctly signed `clear`.
    Clear,
    /// A correctly signed report whose content breaks a rule (a relative
    /// program path), which drops the owner's record.
    Malformed,
    /// Ordinary output: anything without ESC or BEL.
    Noise(Vec<u8>),
    /// An introducer that never gets its terminator — a program killed
    /// mid-write, or a `cat` of half a log. It must not swallow a report
    /// that follows.
    Stray,
}

fn terminal_op() -> impl Strategy<Value = TerminalOp> {
    prop_oneof![
        4 => command().prop_map(TerminalOp::Report),
        2 => command().prop_map(TerminalOp::Forged),
        2 => any::<prop::sample::Index>().prop_map(TerminalOp::Replay),
        1 => Just(TerminalOp::Clear),
        1 => Just(TerminalOp::Malformed),
        3 => prop::collection::vec(
            any::<u8>().prop_filter("no ESC or BEL", |b| *b != 0x1b && *b != 0x07),
            0..40,
        )
        .prop_map(TerminalOp::Noise),
        1 => Just(TerminalOp::Stray),
    ]
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(128))]

    /// The terminal against a model of the spec's rules: only reports signed
    /// with the pane's token and not older than the owner's last count; `clear`
    /// and a rejected report both drop the record; and none of it depends on
    /// how the output was cut into reads or what surrounds it.
    #[test]
    fn terminal_record_matches_its_model(
        ops in prop::collection::vec(terminal_op(), 1..30),
        cuts in prop::collection::vec(1usize..64, 1..200),
    ) {
        // The owner must be a live process; this test's own is.
        let pid = std::process::id();
        let token = new_token();
        let forger = new_token();
        let base = unix_now();
        let mut time = base;
        let mut next_time = || { time += 1; time };

        let mut stream: Vec<u8> = Vec::new();
        let mut sent: Vec<String> = Vec::new();
        let mut model: Option<ResumeCommand> = None;
        for op in ops {
            match op {
                TerminalOp::Report(cmd) => {
                    let Some(seq) = encode_sequence(pid, next_time(), Report::Resume(&cmd), &token)
                    else {
                        continue;
                    };
                    model = Some(cmd);
                    stream.extend_from_slice(seq.as_bytes());
                    sent.push(seq);
                }
                TerminalOp::Forged(cmd) => {
                    if let Some(seq) = encode_sequence(pid, next_time(), Report::Resume(&cmd), &forger) {
                        stream.extend_from_slice(seq.as_bytes());
                    }
                }
                TerminalOp::Replay(index) => {
                    if !sent.is_empty() {
                        stream.extend_from_slice(sent[index.index(sent.len())].as_bytes());
                    }
                }
                TerminalOp::Clear => {
                    let seq = encode_sequence(pid, next_time(), Report::Clear, &token).unwrap();
                    model = None;
                    stream.extend_from_slice(seq.as_bytes());
                    sent.push(seq);
                }
                TerminalOp::Malformed => {
                    let body = format!("pid={pid}:time={}:argv=Li92aW0=", next_time());
                    let mac = fresh::services::program_resume::compute_mac(&body, &token);
                    model = None;
                    stream.extend_from_slice(format!("\x1b]7577;{body}:mac={mac}\x07").as_bytes());
                }
                TerminalOp::Noise(bytes) => stream.extend_from_slice(&bytes),
                TerminalOp::Stray => stream.extend_from_slice(b"\x1b]7577;pid=1:time=1:argv=e"),
            }
        }
        // Every report in this run must still be inside the time window.
        prop_assume!(time - base < TIME_WINDOW_SECS);

        let mut state = TerminalState::with_scrollback(80, 24, 100);
        state.enable_program_resume(ResumeTracker::new(token));
        let mut rest = stream.as_slice();
        let mut cuts = cuts.into_iter().cycle();
        while !rest.is_empty() {
            let n = cuts.next().unwrap().min(rest.len());
            state.process_output(&rest[..n]);
            rest = &rest[n..];
        }
        // A record without a `cwd` keeps its owner's own working directory,
        // where the platform exposes it, apart from the command.
        let owner_cwd = if cfg!(target_os = "linux") {
            std::env::current_dir().ok()
        } else {
            None
        };
        let model = model.map(|command| ResumeRecord {
            owner_cwd: if command.cwd.is_none() { owner_cwd } else { None },
            command,
        });
        prop_assert_eq!(state.program_resume_command(), model);
    }
}
