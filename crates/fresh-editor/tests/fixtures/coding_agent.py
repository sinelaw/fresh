#!/usr/bin/env python3
"""Coding Agent — a *fake* interactive coding agent used by Fresh's showcase
GIFs and feature clips. Every line of output is staged; it does not read, run,
or change anything.

It renders the shape a real terminal coding agent has: a transcript that
scrolls (a user turn, assistant bullets, tool calls with their `⎿` results,
todo lists, edits with their diff hunks) under a spinner line, and an input box
pinned to the bottom of the pane. Lines are drawn from a large bank and seeded
by the project name, so two instances running side by side diverge. It loops
forever, so it keeps producing output for as long as a demo needs to film it.

Usage:  python3 coding_agent.py [--as <comm-name>] [--ask [N]]
                               [--dilate F] [--warm N] <project-name>

`--as` renames the process (prctl PR_SET_NAME on Linux) and sets the terminal
title (OSC 2), the two things Fresh's terminal auto-titling reads — the
foreground process' `/proc/<pgid>/comm` and the OSC title. A demo can then put
a shim on `PATH` and get the tab a real agent launch produces, instead of one
named `python3`. It also picks the skin — accent colour, spinner and the name
the pane calls itself — so agents launched under different names read as
different programs. The names it knows are invented ones (see `SKINS`): a
staged transcript should not run under a real agent's name.

`--ask` stops after N steps (default 4) on a permission prompt and stays there,
which is the state a demo about *noticing* an agent needs one: the other panes
keep moving, this one is waiting on a human.

`--dilate F` slows this pane's whole clock by F, and `--warm N` fills it with N
steps at once before that starts. Together they are how a filmed pane can look
like an agent working at a normal pace. A screenshot of a terminal costs a few
hundred milliseconds to take, so a filmed pane is sampled about three times a
second; at F=8 that is about thirty samples per second of *this* clock, which
played back at thirty frames a second is real-time motion, smooth. `--warm`
then covers the other half: a pane whose clock is eight times slow has printed
almost nothing by the time the camera arrives, and an agent with three lines in
it looks like an agent that has just started rather than one at work.

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
RED, BLUE, VIOLET = "38;5;203", "38;5;75", "38;5;141"
# Diff hunks are read as blocks of colour before they are read as text, so the
# add/remove rows carry a background rather than only coloured ink.
ADD_BG, DEL_BG = "48;5;22;38;5;158", "48;5;52;38;5;217"

# One skin per agent: a demo that runs four at once wants them to look like
# four programs, not one program in four windows. Keyed by the name the
# launcher used (`--as`), because that is the only thing that actually differs
# between the instances.
#
# The names are invented, and the pane says the invented name. A staged
# transcript under a real agent's name is a screenshot of that agent saying
# things it never said — so this one is nobody's: `quill`, `marlin`, `tern`
# and `scout` are coding agents that do not exist. A launcher that runs it
# under some other name gets the neutral "Coding Agent" and the default skin.
SKINS = {
    "quill":  ("38;5;209", "✻✳✶✻✳✢", "Quill"),
    "marlin": ("38;5;75",  "◐◓◑◒",   "Marlin"),
    "tern":   ("38;5;141", "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏", "Tern"),
    "scout":  ("38;5;115", "◜◠◝◞◡◟", "Scout"),
}
DEFAULT_SKIN = ("38;5;209", "✻✳✶✻✳✢", "Coding Agent")

argv = sys.argv[1:]
skin = "claude"
ask_after = None
dilate = 1.0
warm = 0
while argv:
    if len(argv) >= 2 and argv[0] == "--as":
        announce_as(argv[1])
        skin = argv[1]
        argv = argv[2:]
        continue
    if argv[0] == "--ask":
        ask_after = 4
        argv = argv[1:]
        if argv and argv[0].isdigit():
            ask_after = int(argv[0])
            argv = argv[1:]
        continue
    if len(argv) >= 2 and argv[0] == "--dilate":
        dilate = max(0.05, float(argv[1]))
        argv = argv[2:]
        continue
    if len(argv) >= 2 and argv[0] == "--warm":
        warm = max(0, int(argv[1]))
        argv = argv[2:]
        continue
    break

ACCENT, SPIN, BRAND = SKINS.get(skin, DEFAULT_SKIN)

bare = [a for a in argv if not a.startswith("-")]
project = bare[0] if bare else os.path.basename(os.getcwd()) or "service"
# `random.Random` accepts a str seed and hashes it stably (sha512), so the
# stream is deterministic per project and differs between projects.
rng = random.Random(project)

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

# Edits are the one thing an agent does that a reader wants to *see* rather
# than be told about, so each carries a hunk: the line it replaced and the line
# it wrote. Kept short — these render in a pane beside the code, not across a
# full-width terminal.
HUNKS = [
    ("src/auth.rs", "if claims.sub.is_some() {",
     "if claims.sub.is_some() && !claims.expired() {"),
    ("src/session.rs", "let now = SystemTime::now();",
     "let now = clock.now();  // injectable, for tests"),
    ("src/db/pool.rs", "let conn = pool.get().await?;",
     "let conn = pool.get().timeout(DEADLINE).await??;"),
    ("src/ratelimit.rs", "self.hits += 1;",
     "self.hits = self.hits.saturating_add(1);"),
    ("src/token.rs", "let key = self.keys[0].clone();",
     "let key = self.keys.by_kid(header.kid)?;"),
    ("src/middleware.rs", ".layer(TraceLayer::new())",
     ".layer(TraceLayer::new().on_failure(log_failure))"),
]

TODOS = [
    ["Reproduce the bypass in a test", "Reject expired refresh tokens",
     "Add a regression test"],
    ["Measure the hot path", "Move the lock off the await",
     "Re-run the load test"],
    ["Read the failing case", "Make the clock injectable",
     "Unignore the test"],
]


def visible(line):
    """Printable columns of `line`, ignoring SGR escapes."""
    out, i = 0, 0
    while i < len(line):
        if line[i] == "\033":
            end = line.find("m", i)
            if end == -1:
                break
            i = end + 1
            continue
        out += 1
        i += 1
    return out


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


def wrap(text, width, indent="  "):
    """Prose, broken to the pane. Truncating a sentence loses the half that
    said something; a real agent wraps, and the wrapped remainder is indented
    so the bullet column stays a column."""
    words, lines, cur = text.split(), [], ""
    for w in words:
        probe = f"{cur} {w}".strip()
        if len(probe) > width and cur:
            lines.append(cur)
            cur = w
        else:
            cur = probe
    if cur:
        lines.append(cur)
    return [lines[0]] + [indent + rest for rest in lines[1:]]


def spin_line(glyph, msg, elapsed, tokens, width):
    """The working line, fitted to the pane.

    Three things want the same row — what it is doing, how long it has been,
    and how to stop it — and a narrow pane cannot have all three. Drop the
    tail before the message, and shorten the message before losing it: a line
    the pane cuts mid-word reads as a rendering fault, which is the one thing
    a fake agent must not look like.
    """
    for meta in (f"({elapsed}s · ↑ {tokens} tokens · esc to interrupt)",
                 f"({elapsed}s · ↑ {tokens} tokens)",
                 f"({elapsed}s)",
                 ""):
        room = width - visible(f"{glyph} …") - (visible(meta) + 1 if meta else 0)
        if room >= 12:
            # The line ends in the ellipsis that says "still going", so a
            # message trimmed to fit borrows it rather than growing a second.
            text = msg if len(msg) <= room else msg[:room].rstrip()
            line = f"{sgr(ACCENT, glyph)} {sgr(DIM, text + '…')}"
            return f"{line} {sgr(DIM, meta)}" if meta else line
    return f"{sgr(ACCENT, glyph)}"


def think_line():
    return rng.choice(THINK).format(f=rng.choice(FILES), s=rng.choice(SYMS))


def diff_block(width):
    """An edit and the hunk it wrote, the way a coding agent shows one."""
    f, before, after = rng.choice(HUNKS)
    n = rng.randint(24, 180)
    add, rem = rng.randint(2, 22), rng.randint(0, 9)
    # The hunk rows are `     <line> <block>`, and the block is what is left
    # of the pane after that gutter — computed from it rather than guessed at,
    # since a block one cell too wide wraps into the next row.
    gutter = 5 + len(str(n)) + 1
    body = max(12, width - gutter)

    def hunk(bg, text):
        return f"{'':5}{sgr(DIM, str(n))} {sgr(bg, text[:body].ljust(body))}"

    return [
        f"{sgr(ACCENT, '⏺')} {sgr(BOLD, 'Update')}({f})",
        f"  {sgr(DIM, '⎿')}  Updated {f} "
        f"{sgr(GREEN, f'+{add}')} {sgr(RED, f'−{rem}')}",
        hunk(DEL_BG, "- " + before),
        hunk(ADD_BG, "+ " + after),
    ]


def todo_block(width):
    """The plan, with what is done struck off it — the block that says an
    agent is working to a plan rather than replying to a prompt."""
    items = rng.choice(TODOS)
    done = rng.randint(1, len(items) - 1)
    rows = [f"{sgr(ACCENT, '⏺')} {sgr(BOLD, 'Update Todos')}"]
    for i, item in enumerate(items):
        mark = "☒" if i < done else "☐"
        ink = DIM if i < done else ("1;" + ACCENT if i == done else "0")
        lead = f"  {sgr(DIM, '⎿')}  " if i == 0 else "     "
        rows.append(f"{lead}{sgr(ink, mark + ' ' + item)}"[:width + 40])
    return rows


def step(width):
    """One committed transcript step: a list of lines, blank-separated from
    whatever came before."""
    roll = rng.random()
    f = rng.choice(FILES)
    if roll < 0.24:
        return diff_block(width)
    if roll < 0.34:
        return todo_block(width)
    if roll < 0.50:
        cmd, result = rng.choice(CMDS)
        ms = rng.randint(80, 900)
        line = result().replace("{ok}", sgr(GREEN, "✓"))
        return [
            f"{sgr(ACCENT, '⏺')} {sgr(BOLD, 'Bash')}({cmd})",
            f"  {sgr(DIM, '⎿')}  {line} {sgr(DIM, f'({ms}ms)')}",
        ]
    if roll < 0.62:
        return [
            f"{sgr(ACCENT, '⏺')} {sgr(BOLD, 'Read')}({f})",
            f"  {sgr(DIM, '⎿')}  Read {rng.randint(24, 310)} lines",
        ]
    if roll < 0.72:
        s = rng.choice(SYMS)
        return [
            f"{sgr(ACCENT, '⏺')} {sgr(BOLD, 'Search')}(pattern: \"{s}\")",
            f"  {sgr(DIM, '⎿')}  Found {rng.randint(2, 19)} matches across "
            f"{rng.randint(2, 7)} files",
        ]
    if roll < 0.80:
        nf = f.replace("src/", "src/new_")
        return [
            f"{sgr(ACCENT, '⏺')} {sgr(BOLD, 'Write')}({nf})",
            f"  {sgr(DIM, '⎿')}  Wrote {rng.randint(18, 90)} lines",
        ]
    say = rng.choice(SAYS).format(
        s=rng.choice(SYMS), n=rng.randint(4, 17), n2=rng.randint(2, 4)
    )
    body = wrap(say, max(20, width - 2))
    return [f"{sgr(ACCENT, '⏺')} {body[0]}"] + body[1:]


class Pane:
    """Transcript above, spinner + input box pinned below."""

    #  spinner, blank, top rule, prompt, bottom rule, hint
    LIVE_LINES = 6
    #  the permission prompt is taller: it replaces the live block entirely
    ASK_LINES = 9

    def __init__(self, out, header):
        self.out = out
        self.header = header
        self.width = 60
        self.rows = 24
        self.drawn = 0        # lines the last live block occupied, 0 if none
        self.resized = True   # first render lays the pane out from scratch

    def measure(self):
        size = shutil.get_terminal_size((80, 24))
        # Three off the reported width. One is the column a terminal pane
        # keeps for its scrollbar; the other two are slack, because several
        # glyphs this pane is drawn with (⎿ ⏺ ☒ │) are East-Asian *ambiguous*
        # width and a host is entitled to render them two cells wide. A line
        # one cell too long wraps, and a wrapped line desyncs the fixed-height
        # rewind below — so the cost of being wrong is the whole pane, and the
        # cost of the slack is three columns nobody notices.
        self.width = max(28, size.columns - 3)
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
        self.drawn = 0
        self.resized = False

    def live(self, spinner, context):
        """The block pinned to the foot of the pane. Its two furniture lines
        are written to the width they have rather than to a fixed sentence: a
        hint the pane cuts in half reads as a rendering fault, and this pane is
        a third of a screen the moment a demo puts an editor beside it."""
        rule = sgr(DIM, "─" * self.width)
        hint = "⏵⏵ accept edits on · ? for shortcuts"
        tail = f" · {context}% context left"
        if visible(hint + tail) + 2 > self.width:
            hint = "⏵⏵ accept edits on"
        if visible(hint + tail) + 2 > self.width:
            hint = ""
            tail = tail.lstrip(" ·")
        return [
            spinner,
            "",
            rule,
            f"{sgr(ACCENT, '❯')} {sgr(DIM, '▏')}",
            rule,
            f"  {sgr(DIM, (hint + tail).strip())}",
        ]

    def ask(self, file):
        """The prompt a real agent stops on, and the reason a dock that says
        which session is waiting is worth having."""
        w = min(self.width - 1, 48)
        top = "╭" + "─" * (w - 2) + "╮"
        bot = "╰" + "─" * (w - 2) + "╯"

        def row(text, ink="0"):
            """One line of the box, fitted to it: the borders have to line up
            under each other, so the text is cut to the box rather than the
            box grown to the text."""
            room = w - 4
            if visible(text) > room:
                text = text[:room - 1].rstrip() + "…"
            pad = " " * max(0, room - visible(text))
            return f"{sgr(YELLOW, '│')} {sgr(ink, text)}{pad} {sgr(YELLOW, '│')}"

        hint = "waiting for you · ← the dock says which one"
        if visible(hint) + 2 > self.width:
            hint = "waiting for you · ← the dock knows"
        if visible(hint) + 2 > self.width:
            hint = "waiting for you"
        return [
            "",
            sgr(YELLOW, top),
            row(f"Edit {file}", BOLD),
            row("Do you want to make this edit?", DIM),
            row("❯ 1. Yes", "1;" + ACCENT),
            row("  2. Yes, and don't ask again"),
            row("  3. No, tell it what to do instead (esc)"),
            sgr(YELLOW, bot),
            f"  {sgr(DIM, hint)}",
        ]

    def render(self, block, commit=()):
        """Erase the live block, append `commit` to the transcript, redraw."""
        if self.resized:
            self.lay_out()
        self.measure()
        if self.drawn:
            # Cursor sits on the last live line: rewind to the first and wipe.
            self.out.write(f"\r\033[{self.drawn - 1}A\033[J")
        for line in commit:
            self.out.write(truncate(line, self.width) + "\n")
        self.out.write("\n".join(truncate(line, self.width) for line in block))
        self.out.flush()
        self.drawn = len(block)


def main():
    out = sys.stdout
    task, ask = rng.choice(TASKS)
    header = [
        "",
        f" {sgr('1;' + ACCENT, '✻ ' + BRAND)}{sgr(DIM, f'  ·  {project}')}",
        f" {sgr(DIM, 'task: ' + task)}",
        "",
        f"{sgr(ACCENT, '❯')} {ask}",
        "",
    ]
    if os.environ.get("CODING_AGENT_RULER"):
        # A width probe for whoever is framing a demo: one row of digits at
        # the width this process thinks it has, and one of the box-drawing
        # glyphs the pane furniture is made of. Where the two disagree, the
        # host is rendering a glyph wider than one cell.
        w = shutil.get_terminal_size((80, 24)).columns
        header.insert(0, "".join(str(i % 10) for i in range(w)))
        header.insert(1, "│" * w)
        header.insert(2, f"cols={w}")
    pane = Pane(out, header)
    try:
        signal.signal(signal.SIGWINCH, pane.on_resize)
    except (AttributeError, ValueError):
        pass  # no SIGWINCH off Unix; the first render still lays out
    # A host that opens this in a split resizes the PTY right after spawning
    # it. SIGWINCH handles that, but waiting out the initial flurry keeps the
    # opening frames from being a redraw.
    time.sleep(0.5)

    context = rng.randint(31, 74)
    # The backlog, all at once: what this session did before the camera got
    # here. Committed through `render` rather than printed, so the pane ends up
    # in the same state a slow run would have reached.
    for _ in range(warm):
        pane.render(pane.live(f"{sgr(ACCENT, SPIN[0])} {sgr(DIM, 'Cogitating…')}",
                              context),
                    step(pane.width) + [""])
        context = max(4, context - rng.randint(0, 3))

    for n in itertools.count():
        if ask_after is not None and n == ask_after:
            # Stop here, for good: an agent waiting on a person does not
            # carry on in the background, and the demo wants one pane that
            # is still asking when the camera comes back to it.
            pane.render(pane.ask(rng.choice(HUNKS)[0]))
            while True:
                time.sleep(3600)
        msg = think_line()
        tokens = rng.randint(2, 19) * 100
        started = time.time()
        deadline = started + rng.uniform(1.6, 3.0) * dilate
        spin = 0
        commit = []
        while time.time() < deadline:
            glyph = SPIN[spin % len(SPIN)]
            # Reported in the pane's own dilated seconds, so a slowed clock
            # does not also read as an agent that has been stuck for a minute.
            elapsed = int((time.time() - started) / dilate)
            spinner = spin_line(glyph, msg, elapsed, tokens, pane.width)
            pane.render(pane.live(spinner, context), commit)
            commit = []
            time.sleep(0.22 * dilate)
            spin += 1
        commit = step(pane.width) + [""]
        context = max(4, context - rng.randint(0, 3))
        pane.render(
            pane.live(f"{sgr(ACCENT, SPIN[0])} {sgr(DIM, 'Cogitating…')}", context),
            commit,
        )


if __name__ == "__main__":
    try:
        main()
    except (KeyboardInterrupt, BrokenPipeError):
        pass
