#!/usr/bin/env python3
"""Coding Agent — a *fake* interactive coding agent used by Fresh's showcase
GIFs. Every line of output is staged; it does not read, run, or change
anything.

It renders the shape a real terminal coding agent has: a transcript that
scrolls (a user turn, assistant bullets, tool calls with their `⎿` results)
under a spinner line, and an input box pinned to the bottom of the pane. Lines
are drawn from a large bank and seeded by the project name, so two instances
running side by side diverge. It loops forever, so it keeps producing output
for as long as a demo needs to film it.

Usage:  python3 coding_agent.py [--as <comm-name>] <project-name>

`--as` renames the process (prctl PR_SET_NAME on Linux) and sets the terminal
title (OSC 2), the two things Fresh's terminal auto-titling reads — the
foreground process' `/proc/<pgid>/comm` and the OSC title. A demo can then put
a shim called `claude` on `PATH` and get the tab a real agent launch produces,
instead of one named `python3`.

Any remaining argument that starts with `-` is ignored (a launcher may append
its own flags); the first bare one, or else the current directory's name, is
the seed.
"""
import itertools
import os
import random
import shutil
import signal
import sys
import time


def sgr(code, text):
    return f"\033[{code}m{text}\033[0m"


def announce_as(name):
    """Look like `name` to a terminal: rename the process (best-effort
    `/proc/self/comm`, a no-op off Linux) and set the OSC 2 window title."""
    try:
        import ctypes

        PR_SET_NAME = 15
        ctypes.CDLL("libc.so.6", use_errno=True).prctl(
            PR_SET_NAME, name.encode()[:15] + b"\0", 0, 0, 0
        )
    except Exception:
        pass
    sys.stdout.write(f"\033]2;{name}\007")


DIM, BOLD = "2", "1"
CYAN, GREEN, YELLOW, MAGENTA, ORANGE = "36", "32", "33", "35", "38;5;209"

argv = sys.argv[1:]
if len(argv) >= 2 and argv[0] == "--as":
    announce_as(argv[1])
    argv = argv[2:]

bare = [a for a in argv if not a.startswith("-")]
project = bare[0] if bare else os.path.basename(os.getcwd()) or "service"
# `random.Random` accepts a str seed and hashes it stably (sha512), so the
# stream is deterministic per project and differs between projects.
rng = random.Random(project)

SPIN = "✻✳✶✻✳✢"

FILES = [
    "src/auth.rs", "src/session.rs", "src/handlers.rs", "src/routes.rs",
    "src/db/pool.rs", "src/middleware.rs", "tests/api.rs", "src/token.rs",
    "src/ratelimit.rs", "src/config.rs", "src/errors.rs", "src/cache.rs",
    "src/worker.rs", "src/metrics.rs",
]
SYMS = [
    "validate_token", "Session::new", "connect_pool", "RateLimiter",
    "verify_signature", "refresh_claims", "decode_jwt", "Backoff",
    "with_deadline", "spawn_worker",
]

THINK = [
    "Reading {f}", "Scanning for call sites of {s}", "Planning the refactor",
    "Inferring lifetimes in {f}", "Checking the error paths",
    "Resolving trait bounds", "Diffing against main",
    "Summarising the test failures", "Grepping for stale TODOs",
    "Tracing the request through {f}", "Drafting a patch for {f}",
    "Re-running the failing case", "Reading the docs for {s}",
    "Narrowing the type of {s}", "Checking for a borrow conflict",
    "Looking for a simpler approach", "Cogitating", "Untangling",
]

TASKS = [
    ("harden token validation", "reject any token whose signature we can't verify"),
    ("cut p99 latency", "the /v1/sessions route spikes to 900ms under load"),
    ("fix the flaky test", "tests/api.rs::concurrent_refresh fails ~1 in 20"),
    ("migrate to the new pool", "swap the deadpool wrapper for the new Pool"),
    ("add per-route rate limiting", "60/min per API key, 429 with Retry-After"),
    ("close the auth bypass", "an expired refresh token still mints a session"),
]

SAYS = [
    "Found it — {s} returns before the expiry check runs.",
    "The lock is held across an await; that's the stall.",
    "There are {n} call sites; {n2} of them can take the fast path.",
    "That branch is unreachable — removing it simplifies the match.",
    "I'll extract a helper so the two paths can't drift again.",
    "The failure only reproduces when the bucket refills mid-request.",
    "Adding a regression test before the fix, so it's pinned.",
    "This is cheaper as a single pass — no intermediate Vec.",
]

# (command, how its `⎿` result line reads) — a grep doesn't report "tests
# passing", so each command carries its own shape of result.
CMDS = [
    ("cargo nextest run", lambda: f"{{ok}} {rng.choice([12, 18, 24, 31, 42])} tests passed"),
    ("cargo build", lambda: f"{{ok}} Finished in {rng.randint(2, 40)}.{rng.randint(0, 9)}s"),
    ("cargo clippy --all-targets", lambda: f"{{ok}} No warnings"),
    ("cargo fmt --check", lambda: f"{{ok}} Clean"),
    ("git diff --stat", lambda: f"{rng.randint(2, 6)} files changed, "
                                f"{rng.randint(12, 90)} insertions(+)"),
    ("rg 'unwrap\\(\\)' src/", lambda: f"{rng.randint(3, 24)} matches"),
]


def truncate(line, width):
    """Trim to `width` printable columns, ignoring SGR escapes, so a long
    line can't wrap and desync the cursor arithmetic below."""
    out, shown, i = [], 0, 0
    while i < len(line):
        if line[i] == "\033":
            end = line.find("m", i)
            if end == -1:
                break
            out.append(line[i:end + 1])
            i = end + 1
            continue
        if shown >= width:
            out.append("\033[0m")
            break
        out.append(line[i])
        shown += 1
        i += 1
    return "".join(out)


def think_line():
    return rng.choice(THINK).format(f=rng.choice(FILES), s=rng.choice(SYMS))


def step():
    """One committed transcript step: a list of lines, blank-separated from
    whatever came before."""
    roll = rng.random()
    f = rng.choice(FILES)
    if roll < 0.26:
        a, d = rng.randint(2, 48), rng.randint(0, 14)
        return [
            f"{sgr(GREEN, '●')} {sgr(BOLD, 'Update')}({f})",
            f"  {sgr(DIM, '⎿')}  Updated {f} with {a} additions and {d} removals",
        ]
    if roll < 0.44:
        cmd, result = rng.choice(CMDS)
        ms = rng.randint(80, 900)
        line = result().replace("{ok}", sgr(GREEN, "✓"))
        return [
            f"{sgr(GREEN, '●')} {sgr(BOLD, 'Bash')}({cmd})",
            f"  {sgr(DIM, '⎿')}  {line} {sgr(DIM, f'({ms}ms)')}",
        ]
    if roll < 0.58:
        return [
            f"{sgr(GREEN, '●')} {sgr(BOLD, 'Read')}({f})",
            f"  {sgr(DIM, '⎿')}  Read {rng.randint(24, 310)} lines",
        ]
    if roll < 0.68:
        s = rng.choice(SYMS)
        return [
            f"{sgr(GREEN, '●')} {sgr(BOLD, 'Search')}(pattern: \"{s}\")",
            f"  {sgr(DIM, '⎿')}  Found {rng.randint(2, 19)} matches across "
            f"{rng.randint(2, 7)} files",
        ]
    if roll < 0.78:
        nf = f.replace("src/", "src/new_")
        return [
            f"{sgr(GREEN, '●')} {sgr(BOLD, 'Write')}({nf})",
            f"  {sgr(DIM, '⎿')}  Wrote {rng.randint(18, 90)} lines",
        ]
    say = rng.choice(SAYS).format(
        s=rng.choice(SYMS), n=rng.randint(4, 17), n2=rng.randint(2, 4)
    )
    return [f"{sgr(GREEN, '●')} {say}"]


class Pane:
    """Transcript above, spinner + input box pinned below."""

    #  spinner, blank, rule, prompt, rule, hint
    LIVE_LINES = 6

    def __init__(self, out, header):
        self.out = out
        self.header = header
        self.width = 60
        self.rows = 24
        self.drawn = False
        self.resized = True  # first render lays the pane out from scratch

    def measure(self):
        size = shutil.get_terminal_size((80, 24))
        self.width = max(28, size.columns - 1)
        self.rows = max(8, size.lines)

    def on_resize(self, *_):
        self.resized = True

    def lay_out(self):
        """Clear the pane and park the header just above the input box, the
        way an agent CLI looks a moment after it starts — transcript lines
        then push it up. Also the recovery path after a resize: the redraw
        below rewinds by a fixed number of lines, which a reflow invalidates,
        so a resize starts the pane over rather than desyncing it."""
        self.measure()
        self.out.write("\033[H\033[2J")
        pad = max(0, self.rows - len(self.header) - self.LIVE_LINES)
        self.out.write("\n" * pad)
        for line in self.header:
            self.out.write(truncate(line, self.width) + "\n")
        self.drawn = False
        self.resized = False

    def live(self, spinner):
        rule = sgr(DIM, "─" * self.width)
        return [
            spinner,
            "",
            rule,
            f"{sgr(ORANGE, '❯')} {sgr(DIM, '▏')}",
            rule,
            f"  {sgr(DIM, '⏸ manual mode on · ? for shortcuts · ← for agents')}",
        ]

    def render(self, spinner, commit=()):
        """Erase the live block, append `commit` to the transcript, redraw."""
        if self.resized:
            self.lay_out()
        self.measure()
        if self.drawn:
            # Cursor sits on the last live line: rewind to the first and wipe.
            self.out.write(f"\r\033[{self.LIVE_LINES - 1}A\033[J")
        for line in commit:
            self.out.write(truncate(line, self.width) + "\n")
        block = self.live(spinner)
        self.out.write(
            "\n".join(truncate(line, self.width) for line in block)
        )
        self.out.flush()
        self.drawn = True


def main():
    out = sys.stdout
    task, ask = rng.choice(TASKS)
    header = [
        "",
        f" {sgr(f'1;{ORANGE}', '✻ Coding Agent')}{sgr(DIM, f'  ·  {project}')}",
        f" {sgr(DIM, 'task: ' + task)}",
        "",
        f"{sgr(ORANGE, '❯')} {ask}",
        "",
    ]
    pane = Pane(out, header)
    try:
        signal.signal(signal.SIGWINCH, pane.on_resize)
    except (AttributeError, ValueError):
        pass  # no SIGWINCH off Unix; the first render still lays out
    # A host that opens this in a split resizes the PTY right after spawning
    # it. SIGWINCH handles that, but waiting out the initial flurry keeps the
    # opening frames from being a redraw.
    time.sleep(0.5)

    for _ in itertools.count():
        msg = think_line()
        tokens = rng.randint(2, 19) * 100
        started = time.time()
        deadline = started + rng.uniform(1.6, 3.0)
        spin = 0
        commit = []
        while time.time() < deadline:
            glyph = SPIN[spin % len(SPIN)]
            elapsed = int(time.time() - started)
            spinner = (
                f"{sgr(ORANGE, glyph)} {sgr(DIM, msg + '…')} "
                f"{sgr(DIM, f'({elapsed}s · ↑ {tokens} tokens)')}"
            )
            pane.render(spinner, commit)
            commit = []
            time.sleep(0.22)
            spin += 1
        commit = step() + [""]
        pane.render(f"{sgr(ORANGE, SPIN[0])} {sgr(DIM, 'Cogitating…')}", commit)


if __name__ == "__main__":
    try:
        main()
    except (KeyboardInterrupt, BrokenPipeError):
        pass
