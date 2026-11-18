//! `fresh --cmd init` (#3793): end of input on stdin ends the command with an
//! error instead of re-prompting forever, and creates nothing.
//!
//! Driven through the binary, Linux-gated like the other binary-driving tests.
#![cfg(target_os = "linux")]

use std::io::Read;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

fn run_init_with_closed_stdin(args: &[&str]) -> (Option<i32>, String, tempfile::TempDir) {
    let home = tempfile::tempdir().unwrap();
    let mut child = Command::new(env!("CARGO_BIN_EXE_fresh"))
        .args(["--cmd", "init"])
        .args(args)
        .current_dir(home.path())
        .env("HOME", home.path())
        .env("XDG_CONFIG_HOME", home.path().join("config"))
        .env("XDG_DATA_HOME", home.path().join("data"))
        .env("XDG_STATE_HOME", home.path().join("state"))
        .env("XDG_CACHE_HOME", home.path().join("cache"))
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn fresh");

    // The bug was an endless loop, so bound the wait rather than hang the suite.
    let deadline = Instant::now() + Duration::from_secs(20);
    let status = loop {
        if let Some(status) = child.try_wait().unwrap() {
            break status;
        }
        if Instant::now() > deadline {
            #[allow(clippy::let_underscore_must_use)]
            let _ = child.kill();
            panic!("`fresh --cmd init` did not exit at end of input");
        }
        std::thread::sleep(Duration::from_millis(20));
    };
    let mut stderr = String::new();
    child
        .stderr
        .take()
        .unwrap()
        .read_to_string(&mut stderr)
        .unwrap();
    (status.code(), stderr, home)
}

#[test]
fn init_type_prompt_exits_with_an_error_at_end_of_input() {
    let (code, stderr, home) = run_init_with_closed_stdin(&[]);
    assert_eq!(code, Some(1), "stderr:\n{stderr}");
    assert!(
        stderr.lines().count() < 10,
        "must not re-prompt in a loop. stderr:\n{stderr}"
    );
    assert!(stderr.contains("no input"), "stderr:\n{stderr}");
    assert_eq!(
        std::fs::read_dir(home.path())
            .unwrap()
            .filter(|e| e.as_ref().unwrap().file_name().to_string_lossy().starts_with("my-fresh"))
            .count(),
        0,
        "nothing may be created"
    );
}

#[test]
fn init_with_a_type_still_stops_at_end_of_input() {
    let (code, stderr, home) = run_init_with_closed_stdin(&["language"]);
    assert_eq!(code, Some(1), "stderr:\n{stderr}");
    assert!(
        !home.path().join("my-fresh-language").exists(),
        "a package must not be created from answers that were never given"
    );
}
