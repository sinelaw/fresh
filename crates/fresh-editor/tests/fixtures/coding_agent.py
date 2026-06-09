#!/usr/bin/env python3
"""Coding Agent — a *fake* autonomous coding agent used by Fresh's showcase
GIFs. Every line of output is staged; it does not read, run, or change
anything.

It streams an agent-style log: a braille spinner animates a "thinking" line
for about a second, then it commits a result line. Lines are drawn from a
large bank and seeded by the project name passed as argv[1], so two instances
running side by side diverge. It loops forever, so it keeps producing output
for as long as a demo needs to film it.

Usage:  python3 coding_agent.py <project-name>
"""
import itertools
import random
import sys
import time


def sgr(code, text):
    return f"\033[{code}m{text}\033[0m"


DIM, BOLD = "2", "1"
CYAN, GREEN, YELLOW, MAGENTA = "36", "32", "33", "35"

project = sys.argv[1] if len(sys.argv) > 1 else "service"
# `random.Random` accepts a str seed and hashes it stably (sha512), so the
# stream is deterministic per project and differs between projects.
rng = random.Random(project)

SPIN = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

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
    "reading {f}",
    "scanning for call sites of {s}",
    "planning the refactor",
    "inferring lifetimes in {f}",
    "checking the error paths",
    "resolving trait bounds",
    "diffing against main",
    "summarising the test failures",
    "grepping for stale TODOs",
    "tracing the request through {f}",
    "drafting a patch for {f}",
    "re-running the failing case",
    "reading the docs for {s}",
    "narrowing the type of {s}",
    "checking for a borrow conflict",
    "looking for a simpler approach",
]

CMDS = ["cargo build", "cargo clippy", "cargo fmt --check", "git status", "ruff check", "cargo nextest run"]
NOTES = [
    "{s} can be simplified",
    "found a missing .await",
    "this branch is never hit — removing it",
    "extracted a helper to cut duplication",
    "added a regression test for the panic",
    "the lock was held across an await — fixed",
]


def think_line():
    return rng.choice(THINK).format(f=rng.choice(FILES), s=rng.choice(SYMS))


def result_line():
    roll = rng.random()
    if roll < 0.42:
        a, d = rng.randint(2, 48), rng.randint(0, 14)
        return f"{sgr(GREEN, '✓')} {sgr(BOLD, 'edit')} {rng.choice(FILES)}  {sgr(DIM, f'+{a} -{d}')}"
    if roll < 0.62:
        n = rng.choice([12, 18, 24, 31, 42])
        ms = rng.randint(80, 900)
        return f"{sgr(GREEN, '✓')} {sgr(BOLD, 'tests')}  {sgr(GREEN, f'{n}/{n} passing')}{sgr(DIM, f'  ({ms}ms)')}"
    if roll < 0.78:
        return f"{sgr(CYAN, '→')} {sgr(BOLD, 'run')} {sgr(DIM, rng.choice(CMDS))}"
    if roll < 0.90:
        f = rng.choice(FILES).replace("src/", "src/new_")
        return f"{sgr(YELLOW, '✎')} {sgr(BOLD, 'create')} {f}"
    return f"{sgr(MAGENTA, '◆')} {sgr(DIM, rng.choice(NOTES).format(s=rng.choice(SYMS)))}"


def main():
    out = sys.stdout
    task = rng.choice([
        "harden token validation", "cut p99 latency", "fix the flaky test",
        "migrate to the new pool", "add per-route rate limiting",
        "close the auth bypass",
    ])
    out.write("\n")
    out.write(f" {sgr(f'1;{CYAN}', '⟁ Coding Agent')}{sgr(DIM, f'  ·  {project}')}\n")
    out.write(f" {sgr(DIM, 'task: ' + task)}\n\n")
    out.flush()

    for _ in itertools.count():
        msg = think_line()
        deadline = time.time() + rng.uniform(0.7, 1.2)
        spin = 0
        while time.time() < deadline:
            out.write(f"\r {sgr(YELLOW, SPIN[spin % len(SPIN)])} {sgr(DIM, msg)} …\033[K")
            out.flush()
            time.sleep(0.08)
            spin += 1
        out.write(f"\r {result_line()}\033[K\n")
        out.flush()


if __name__ == "__main__":
    try:
        main()
    except (KeyboardInterrupt, BrokenPipeError):
        pass
