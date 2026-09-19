#!/usr/bin/env python3
"""
Runtime memory breakdown for the `fresh` binary.

Answers "what is the editor's memory actually spent on, at the moment it is
using the most of it" -- per allocation site, with the call stack that led
there -- rather than a single total.

How it works: `fresh` is an interactive TUI, so there is nothing to profile
unless something drives it. We allocate a pseudo-terminal (the editor
refuses to start without one), launch the binary under Valgrind on its
slave end, feed it a fixed, scripted workload -- open the files, walk them
end to end, type, switch buffers -- and quit with Ctrl+Q so the profiler
gets a clean exit and writes its output. The same script every run, so two
runs are comparable.

Three tools, all optional-dependency-free (no code changes, no allocator
shim, no crate added to the build):

  massif  (default)  Heap over time, and at the peak a tree of every
                     allocation site with its share of the total. This is
                     the breakdown.
  dhat               Every allocation's size, lifetime and access count.
                     Answers "what churns" and "what is read once and held
                     forever", which massif cannot see. Output loads into
                     https://nnethercote.github.io/dh_view/dh_view.html
  rss                No Valgrind: sample /proc/<pid>/{status,smaps_rollup}
                     while the same workload runs. Coarse, but it is real
                     resident memory at real speed, so it is the sanity
                     check on the other two (Valgrind's own heap layout is
                     not the native allocator's).

Usage:
    cargo build --profile profiling --bin fresh
    scripts/memory-profile.py                       # massif, default workload
    scripts/memory-profile.py --tool rss            # RSS timeline, fast
    scripts/memory-profile.py --tool dhat
    scripts/memory-profile.py --files a.rs b.ts     # profile your own files

Build with the `profiling` profile (release codegen + line tables, no LTO):
a stripped or fat-LTO binary profiles as a handful of anonymous frames.
"""

import argparse
import fcntl
import json
import os
import pty
import re
import select
import shutil
import signal
import struct
import subprocess
import sys
import tempfile
import termios
import time

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# Keystrokes, as an xterm-compatible terminal sends them.
CTRL_Q = b"\x11"
CTRL_B = b"\x02"
CTRL_P = b"\x10"
CTRL_S = b"\x13"
CTRL_END = b"\x1b[1;5F"
CTRL_HOME = b"\x1b[1;5H"
PAGE_DOWN = b"\x1b[6~"
PAGE_UP = b"\x1b[5~"
CTRL_PAGE_UP = b"\x1b[5;5~"
CTRL_PAGE_DOWN = b"\x1b[6;5~"
TAB = b"\t"
ENTER = b"\r"
BACKSPACE = b"\x7f"


def step(label, keys, reps=1, settle=1.0):
    """One workload step. `settle` scales the wait for the editor to go quiet,
    for steps that do far more work than a keystroke (creating a workspace
    forks git and a shell)."""
    return (label, keys, reps, settle)


def palette_file(name):
    """Open a file through the palette. It opens in `>command` mode, so the
    leading backspace drops the `>` and puts it in file mode."""
    return CTRL_P + BACKSPACE + name.encode() + ENTER


def palette_command(name):
    return CTRL_P + name.encode() + ENTER


# Workload 1: editing. Walk each buffer end to end, type, save, switch buffer.
EDIT_WORKLOAD = [
    step("walk to end of buffer", CTRL_END),
    step("page up through it", PAGE_UP, 6),
    step("page down through it", PAGE_DOWN, 6),
    step("back to the top", CTRL_HOME),
    step("type", b"fn memory_profile_probe() {}"),
    # Save so the quit at the end is not held up by a "buffer is modified"
    # prompt: the files are throwaway copies, so writing them costs nothing
    # and it keeps the run unattended.
    step("save", CTRL_S),
    step("next buffer", CTRL_PAGE_DOWN),
    step("walk to end of buffer", CTRL_END),
    step("page up through it", PAGE_UP, 6),
    step("next buffer", CTRL_PAGE_DOWN),
    step("walk to end of buffer", CTRL_END),
]

# Workload 2: the orchestrator. Several workspaces, each a git worktree with
# its own terminal, file explorer and buffers, over a real git repo so the git
# plugins have something to do.
#
# Every keystroke here was worked out against the running editor (the trust
# dialog's default is *not* trust, the palette opens in command mode, and the
# New Workspace dialog needs four tabs to reach its Create button), so treat
# the sequence as load-bearing rather than illustrative.
ORCHESTRATOR_FILES = ["README.md", "Cargo.toml", "main.rs"]


def orchestrator_workload(workspaces=3):
    steps = [
        # The security dialog owns the first keystrokes in a folder the editor
        # has not seen before. `t` selects "Trust folder & Allow Tooling",
        # Enter confirms -- without it the whole run profiles a modal dialog.
        step("trust the folder", b"t"),
        step("confirm trust", ENTER, settle=2.0),
        step("open the orchestrator doc", palette_file("orchestrator-sessions"), settle=1.5),
        step("open the file explorer", CTRL_B, settle=1.5),
    ]
    for i in range(workspaces):
        steps += [
            step("workspace %d: open dialog" % (i + 1), palette_command("orchestrator: new work"), settle=1.5),
            # Four tabs from the Project Path field to [ Create Workspace ].
            step("workspace %d: reach Create" % (i + 1), TAB, 4),
            # Creating one forks git for a worktree and spawns the shell, so
            # it needs far longer than a keystroke to go quiet.
            step("workspace %d: create" % (i + 1), ENTER, settle=6.0),
            step("workspace %d: file explorer" % (i + 1), CTRL_B, settle=1.5),
        ]
        for name in ORCHESTRATOR_FILES:
            steps.append(step("workspace %d: open %s" % (i + 1, name), palette_file(name), settle=2.0))
        steps += [
            step("workspace %d: highlight to end" % (i + 1), CTRL_END, settle=1.5),
            # Back to the workspace's terminal, and give git something to do
            # in it -- the git plugins watch the repo, and the terminal has to
            # emulate the output.
            step("workspace %d: back to terminal" % (i + 1), CTRL_PAGE_UP, len(ORCHESTRATOR_FILES) + 1),
            # --no-pager on purpose: an interactive pager would still own the
            # terminal at the end of the run and swallow the quit.
            step("workspace %d: run git in it" % (i + 1), b"git --no-pager log --oneline -20\r", settle=2.0),
        ]
    return steps


WORKLOADS = {
    "edit": lambda: EDIT_WORKLOAD,
    "orchestrator": orchestrator_workload,
}


def log(msg):
    print(msg, file=sys.stderr, flush=True)


def default_files():
    """A workload with something for each of the expensive subsystems:
    a big Rust file (syntect highlighting + the text model), a TypeScript
    file (tree-sitter grammar + parse tree), and JSON."""
    candidates = [
        "crates/fresh-editor/src/main.rs",
        "crates/fresh-editor/src/input/keybindings.rs",
        "crates/fresh-editor-core/keymaps/default.json",
    ]
    found = [os.path.join(REPO, c) for c in candidates if os.path.exists(os.path.join(REPO, c))]
    if not found:
        sys.exit("none of the default workload files exist; pass --files")
    return found


def spawn_pty(argv, env, cols=140, rows=45, cwd=None):
    """Fork the child onto the slave end of a new pty, and return (pid, master)."""
    pid, fd = pty.fork()
    if pid == 0:
        os.environ.clear()
        os.environ.update(env)
        try:
            if cwd:
                os.chdir(cwd)
            os.execvp(argv[0], argv)
        except Exception as exc:  # pragma: no cover - child side
            sys.stderr.write("exec failed: %r\n" % (exc,))
        os._exit(127)
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))
    return pid, fd


def write_keys(fd, data):
    """Type into the pty. False once the child is gone and the master hangs up."""
    try:
        os.write(fd, data)
        return True
    except OSError:
        return False


def drain(fd, quiet=0.5, maxwait=120.0, quiet_bytes=1024):
    """Read until the child stops doing visible work, and return what it wrote.

    "Stops" is not "emits nothing": parts of this UI animate -- the
    orchestrator dock repaints a spinner while a workspace is being prepared
    -- so a strict no-output test would wait out the whole cap on every step
    that starts one. A window of `quiet` seconds carrying less than
    `quiet_bytes` counts as idle, which a spinner satisfies and a real
    repaint does not.

    Waiting on output rather than sleeping a fixed amount is what keeps the
    workload honest under Valgrind, where every step is tens of times slower
    and a fixed sleep would race the editor.
    """
    buf = bytearray()
    start = time.time()
    window_start = start
    window_bytes = 0
    while True:
        r, _, _ = select.select([fd], [], [], quiet)
        now = time.time()
        if not r:
            return bytes(buf)
        try:
            data = os.read(fd, 65536)
        except OSError:
            return bytes(buf)
        if not data:
            return bytes(buf)
        buf += data
        window_bytes += len(data)
        if now - window_start >= quiet:
            if window_bytes < quiet_bytes:
                return bytes(buf)
            window_start, window_bytes = now, 0
        if now - start > maxwait:
            return bytes(buf)


def isolated_env(home):
    """A throwaway HOME so the profile reflects the editor, not this machine's
    config, plugins or restored session."""
    return {
        "TERM": "xterm-256color",
        "HOME": home,
        "XDG_CONFIG_HOME": os.path.join(home, ".config"),
        "XDG_DATA_HOME": os.path.join(home, ".local/share"),
        "XDG_STATE_HOME": os.path.join(home, ".local/state"),
        "XDG_CACHE_HOME": os.path.join(home, ".cache"),
        "PATH": os.environ.get("PATH", "/usr/bin:/bin"),
        "LANG": "C.UTF-8",
        "LC_ALL": "C.UTF-8",
    }


def run_workload(argv, env, quiet, steps, rss_samples=None, cols=140, rows=45, cwd=None,
                 max_step=30.0, verbose=False, screen_out=None, maps_out=None):
    """Drive one editor launch through `steps` and return its exit status.

    `rss_samples` (a list) turns on /proc sampling of the editor process.
    """
    pid, fd = spawn_pty(argv, env, cols, rows, cwd=cwd)
    sampler = ProcSampler(pid, rss_samples, exe=argv[0]) if rss_samples is not None else None
    last_screen = bytearray()
    if sampler is not None and maps_out is not None:
        maps_out.append(sampler)
    try:
        if sampler:
            sampler.sample("start")
        drain(fd, quiet=quiet, maxwait=max_step * 3)
        if sampler:
            sampler.sample("after startup")
        for label, keys, reps, settle in steps:
            started = time.time()
            nbytes = 0
            for _ in range(reps):
                if not write_keys(fd, keys):
                    break
                # The cap matters: parts of this UI animate (the dock's
                # "preparing workspace" spinner), and a screen that never goes
                # quiet would otherwise hold a step open indefinitely.
                nbytes += len(drain(fd, quiet=quiet * settle, maxwait=max_step * settle))
            if verbose:
                log("  %-42s %5.1fs %8d bytes" % (label[:42], time.time() - started, nbytes))
            if sampler:
                sampler.sample(label)
        # Quitting, defensively. The last step leaves focus in a terminal, so
        # step out of it first -- a terminal pane is entitled to swallow
        # Ctrl+Q. Then answer, in order, the prompts a loaded editor can raise
        # on the way out: unsaved buffers, and the quit confirmation. A key
        # that no prompt is waiting for lands in a buffer we are about to
        # discard, which costs nothing.
        for keys in (CTRL_PAGE_DOWN, CTRL_Q, b"d", b"y", ENTER):
            if not write_keys(fd, keys):
                break
            tail = drain(fd, quiet=quiet, maxwait=max_step)
            if tail:
                last_screen[:] = tail[-4096:]
            done, status = os.waitpid(pid, os.WNOHANG)
            if done:
                return status

        deadline = time.time() + max_step * 4
        while time.time() < deadline:
            done, status = os.waitpid(pid, os.WNOHANG)
            if done:
                return status
            tail = drain(fd, quiet=0.5, maxwait=2.0)
            if tail:
                last_screen[:] = tail[-4096:]
        # SIGTERM, not SIGKILL: Valgrind dumps its profile for a process
        # terminating on a signal it can see, and nothing at all for one that
        # is killed outright. The screen dump says what the editor was waiting
        # for, which is the only way to debug a workload from out here.
        log("editor did not exit; sending SIGTERM (the profile may be short)")
        os.kill(pid, signal.SIGTERM)
        for _ in range(20):
            done, status = os.waitpid(pid, os.WNOHANG)
            if done:
                return status
            drain(fd, quiet=0.5, maxwait=1.0)
        os.kill(pid, signal.SIGKILL)
        return os.waitpid(pid, 0)[1]
    finally:
        if screen_out and last_screen:
            with open(screen_out, "wb") as f:
                f.write(bytes(last_screen))
        try:
            os.close(fd)
        except OSError:
            pass



SMAPS_CATEGORIES = (
    # (label, predicate on (path, perms))
    ("editor binary: code", lambda path, perms, exe: path == exe and "x" in perms),
    ("editor binary: read-only data", lambda path, perms, exe: path == exe and perms.startswith("r--")),
    ("editor binary: data + relocations", lambda path, perms, exe: path == exe),
    ("shared libraries", lambda path, perms, exe: path.endswith(".so") or ".so." in path),
    ("heap (brk)", lambda path, perms, exe: path == "[heap]"),
    ("main thread stack", lambda path, perms, exe: path == "[stack]"),
    ("anonymous (malloc arenas, thread stacks)", lambda path, perms, exe: path == ""),
    ("other file mappings", lambda path, perms, exe: not path.startswith("[")),
    ("kernel mappings", lambda path, perms, exe: True),
)


def smaps_breakdown(pid, exe):
    """Split a live process's resident memory by what each mapping *is*.

    massif only ever sees the heap. This is the other question -- what the
    other two thirds of RSS are -- and the only place it can be answered is
    /proc/<pid>/smaps, mapping by mapping.
    """
    totals = {}          # label -> [rss_kb, pss_kb, count]
    path, perms = "", ""
    header = re.compile(r"^[0-9a-f]+-[0-9a-f]+ (\S{4}) \S+ \S+ \S+\s*(.*)$")
    try:
        with open("/proc/%d/smaps" % pid) as f:
            for line in f:
                m = header.match(line)
                if m:
                    perms, path = m.group(1), m.group(2).strip()
                    continue
                if not line.startswith(("Rss:", "Pss:")):
                    continue
                kb = int(line.split()[1])
                label = next(lbl for lbl, pred in SMAPS_CATEGORIES if pred(path, perms, exe))
                row = totals.setdefault(label, [0, 0, 0])
                if line.startswith("Rss:"):
                    row[0] += kb
                    row[2] += 1
                else:
                    row[1] += kb
    except OSError:
        return {}
    return totals


class ProcSampler:
    """Reads resident memory straight out of /proc for the live process."""

    def __init__(self, pid, out, exe=""):
        self.pid = pid
        self.out = out
        self.exe = exe
        self.maps = {}
        self.threads = 0

    def sample(self, label):
        pid = self.pid
        rec = {"label": label, "t": time.time()}
        try:
            with open("/proc/%d/status" % pid) as f:
                for line in f:
                    if line.startswith(("VmRSS:", "VmHWM:", "VmSize:")):
                        k, v = line.split(":", 1)
                        rec[k] = int(v.split()[0])  # kB
        except OSError:
            return
        try:
            with open("/proc/%d/smaps_rollup" % pid) as f:
                for line in f:
                    for key in ("Rss:", "Pss:", "Private_Dirty:", "Private_Clean:", "Shared_Clean:", "Anonymous:"):
                        if line.startswith(key):
                            rec[key.rstrip(":")] = int(line.split()[1])
        except OSError:
            pass
        try:
            self.threads = len(os.listdir("/proc/%d/task" % pid))
        except OSError:
            pass
        # Keep the most recent one: the run ends at its fullest, and reading
        # smaps is far too slow to do more often than once a step.
        breakdown = smaps_breakdown(pid, self.exe)
        if breakdown:
            self.maps = breakdown
        self.out.append(rec)


# ---------------------------------------------------------------- massif ---


def parse_massif(path):
    """Parse a massif.out file into snapshots.

    Format is documented in the Valgrind manual; the only parts we need are
    the per-snapshot totals and, for detailed snapshots, the heap tree whose
    lines look like `  n2: 1234 0x4A2B: foo (bar.rs:12)`.
    """
    snapshots = []
    cur = None
    tree_re = re.compile(r"^(\s*)n(\d+): (\d+) (.*)$")
    with open(path) as f:
        for line in f:
            line = line.rstrip("\n")
            if line.startswith("#-----------") or line.startswith("desc:") or line.startswith("cmd:") or line.startswith("time_unit:"):
                continue
            if line.startswith("snapshot="):
                cur = {"id": int(line.split("=")[1]), "tree": []}
                snapshots.append(cur)
                continue
            if cur is None:
                continue
            for key in ("time", "mem_heap_B", "mem_heap_extra_B", "mem_stacks_B"):
                if line.startswith(key + "="):
                    cur[key] = int(line.split("=")[1])
            if line.startswith("heap_tree="):
                cur["heap_tree"] = line.split("=")[1]
                continue
            m = tree_re.match(line)
            if m:
                indent, _children, nbytes, what = m.groups()
                cur["tree"].append({"depth": len(indent), "bytes": int(nbytes), "what": what})
    return snapshots


def human(nbytes):
    for unit in ("B", "KiB", "MiB", "GiB"):
        if abs(nbytes) < 1024 or unit == "GiB":
            return "%.1f %s" % (nbytes, unit) if unit != "B" else "%d B" % nbytes
        nbytes /= 1024.0


def clean_frame(what):
    """`0x4A2B3C: fresh::foo::bar (file.rs:12)` -> `fresh::foo::bar (file.rs:12)`."""
    what = re.sub(r"^0x[0-9A-Fa-f]+: ", "", what)
    what = re.sub(r"\(in /[^)]*\)", "", what)
    return what.strip()


PLUMBING = "std / allocator plumbing"


def attribute(frame):
    """Bucket a frame by the crate/subsystem it belongs to.

    This is the part that turns a call-stack tree into something you can act
    on: 30 stacks through `alloc::raw_vec` mean nothing, but "syntect: 18 MiB"
    does.
    """
    f = frame
    buckets = [
        ("tree-sitter", r"tree_sitter|ts_(parser|tree|query)|\bts_"),
        ("syntect (syntax defs + themes)", r"syntect|onig|SyntaxSet|ThemeSet"),
        ("QuickJS (plugin runtime)", r"rquickjs|\bJS_|quickjs|js_(malloc|calloc|realloc|strndup|parse|create)"),
        ("oxc (TS transpile)", r"\boxc_"),
        ("plugins (host side)", r"fresh_plugin"),
        ("retained-mode UI (fresh-ui)", r"fresh_ui::"),
        ("text model / buffers", r"fresh_editor_core::(text|buffer|rope)|fresh_core::text"),
        ("editor state", r"fresh_editor(_core)?::|\bfresh::(app|state|editor|view|server|services|workspace|input)"),
        ("terminal emulation", r"alacritty_terminal|\bvte\b|portable_pty"),
        ("rendering (ratatui/crossterm)", r"ratatui|crossterm"),
        ("language defs", r"fresh_languages"),
        ("i18n / locales", r"fresh_i18n|rust_i18n|locales"),
        ("file walking / watching", r"\bignore::|\bnotify::|globset"),
        ("tokio / async runtime", r"tokio|\bmio::"),
        ("regex", r"\bregex(_automata|_syntax)?::|fancy_regex"),
        ("serde / json", r"serde|serde_json|jsonc"),
        # Everything below names a container or the allocator itself, never an
        # owner: skipping it is what lets the roll-up walk out to the code that
        # actually wanted the memory.
        (PLUMBING, r"alloc::|core::|std::|__rust|malloc|realloc|calloc|hashbrown|heap allocation functions"),
    ]
    for name, pat in buckets:
        if re.search(pat, f):
            return name
    return None


def summarize_massif(path, top=25):
    snapshots = parse_massif(path)
    if not snapshots:
        sys.exit("massif produced no snapshots: %s" % path)
    detailed = [s for s in snapshots if s.get("tree")]
    peak = max(snapshots, key=lambda s: s.get("mem_heap_B", 0))
    # The peak snapshot is detailed only if massif chose it as such; fall back
    # to the largest detailed one, which is what its tree actually describes.
    tree_snap = max(detailed, key=lambda s: s.get("mem_heap_B", 0)) if detailed else None

    print()
    print("=" * 78)
    print("HEAP OVER TIME")
    print("=" * 78)
    print("%-8s %14s %14s %14s" % ("snapshot", "heap", "+admin/frag", "stacks"))
    for s in snapshots:
        marker = " <- peak" if s is peak else (" (detailed)" if s.get("tree") else "")
        print("%-8d %14s %14s %14s%s" % (
            s["id"],
            human(s.get("mem_heap_B", 0)),
            human(s.get("mem_heap_extra_B", 0)),
            human(s.get("mem_stacks_B", 0)),
            marker,
        ))

    print()
    print("Peak heap: %s (+%s allocator overhead/fragmentation)" % (
        human(peak.get("mem_heap_B", 0)), human(peak.get("mem_heap_extra_B", 0))))

    if not tree_snap:
        print("massif took no detailed snapshot, so there is no tree to break down.\n"
              "Rerun with a lower --detailed-freq (the harness passes 5).")
        return

    total = tree_snap.get("mem_heap_B", 0) or 1
    print()
    print("=" * 78)
    print("WHERE THAT MEMORY IS (snapshot %d, %s live)" % (tree_snap["id"], human(total)))
    print("=" * 78)

    # Per-subsystem roll-up.
    #
    # massif's tree is rooted at malloc and grows *outwards*: a node's
    # children are its callers, and a node's bytes include everything its
    # children account for. So the first frame on the way out that names a
    # real subsystem is the code that asked for the memory, and once a stack
    # has been charged there, nothing further out is charged again -- which is
    # what keeps the roll-up from double counting.
    rollup = {}
    charged = []          # (bytes, subsystem, frame) -- one entry per stack
    attributed = 0
    stack = []            # (depth, owner or None)
    for node in tree_snap["tree"]:
        while stack and stack[-1][0] >= node["depth"]:
            stack.pop()
        owner = next((o for _, o in reversed(stack) if o), None)
        if owner is None:
            frame = clean_frame(node["what"])
            name = attribute(frame)
            if name and name != PLUMBING:
                owner = name
                rollup[name] = rollup.get(name, 0) + node["bytes"]
                charged.append((node["bytes"], name, frame))
                attributed += node["bytes"]
        stack.append((node["depth"], owner))

    print()
    print("By subsystem (charged to the innermost frame that names one):")
    print("%-42s %12s %8s" % ("subsystem", "bytes", "share"))
    print("-" * 64)
    for name, nbytes in sorted(rollup.items(), key=lambda kv: -kv[1])[:top]:
        print("%-42s %12s %7.1f%%" % (name, human(nbytes), 100.0 * nbytes / total))
    rest = total - attributed
    if rest > 0:
        print("%-42s %12s %7.1f%%" % ("(unattributed: stack truncated at --depth,",
                                      human(rest), 100.0 * rest / total))
        print("%-42s %12s %8s" % (" or no frame named a subsystem)", "", ""))

    print()
    print("Largest allocation sites (the frame each stack was charged to):")
    print("%-52s %10s  %s" % ("site", "bytes", "subsystem"))
    print("-" * 78)
    for nbytes, name, frame in sorted(charged, reverse=True)[:top]:
        print("%-52s %10s  %s" % (frame[:52], human(nbytes), name))
    if not charged:
        print("(nothing matched: check the binary has symbols -- build with --profile profiling)")


# -------------------------------------------------------------------- dhat ---


def summarize_dhat(path, top=25):
    """Summarize a dhat profile: what is live at the peak, and what churns.

    dhat groups allocations by "program point" -- the stack that made them.
    Per point it records total bytes ever allocated (`tb`), bytes live at the
    moment the whole program peaked (`gb`), bytes still live at exit (`eb`),
    and how many bytes of that memory were ever read (`rb`) or written (`wb`).
    """
    with open(path) as f:
        data = json.load(f)
    pps = data.get("pps", [])
    ftbl = data.get("ftbl", [])
    if not pps:
        sys.exit("dhat profile has no program points: %s" % path)

    def site(pp):
        """The innermost frame of this program point that names real code."""
        for idx in pp.get("fs", []):
            frame = clean_frame(ftbl[idx] if idx < len(ftbl) else "?")
            if attribute(frame) not in (None, PLUMBING):
                return frame
        # Nothing matched a subsystem: fall back to the outermost real frame,
        # which at least says which part of `main` the stack came through.
        for idx in reversed(pp.get("fs", [])):
            frame = clean_frame(ftbl[idx] if idx < len(ftbl) else "?")
            if frame and frame != "[root]":
                return frame
        return "?"

    total_alloc = sum(p.get("tb", 0) for p in pps)
    total_blocks = sum(p.get("tbk", 0) for p in pps)
    peak_live = sum(p.get("gb", 0) for p in pps)
    at_exit = sum(p.get("eb", 0) for p in pps)
    never_read = sum(p.get("tb", 0) for p in pps if p.get("rb", 0) == 0)

    print()
    print("=" * 78)
    print("ALLOCATION CHURN AND LIFETIMES")
    print("=" * 78)
    print("Allocated over the whole run : %s in %s blocks" % (human(total_alloc), "{:,}".format(total_blocks)))
    print("Live at the peak             : %s" % human(peak_live))
    print("Still live at exit           : %s" % human(at_exit))
    print("Allocated and never read     : %s" % human(never_read))

    live = {}
    churn = {}
    churn_blocks = {}
    for pp in pps:
        name = attribute(site(pp)) or "unattributed"
        live[name] = live.get(name, 0) + pp.get("gb", 0)
        churn[name] = churn.get(name, 0) + pp.get("tb", 0)
        churn_blocks[name] = churn_blocks.get(name, 0) + pp.get("tbk", 0)

    print()
    print("Live at the peak, by subsystem:")
    print("%-42s %12s %8s" % ("subsystem", "bytes", "share"))
    print("-" * 64)
    for name, nbytes in sorted(live.items(), key=lambda kv: -kv[1])[:top]:
        if nbytes:
            print("%-42s %12s %7.1f%%" % (name, human(nbytes), 100.0 * nbytes / max(peak_live, 1)))

    # Churn is the dimension massif cannot show, and it ranks the subsystems
    # very differently from footprint: something can hold almost nothing and
    # still dominate the allocator.
    print()
    print("Allocated over the whole run, by subsystem (churn):")
    print("%-42s %12s %8s %12s" % ("subsystem", "bytes", "share", "blocks"))
    print("-" * 78)
    for name, nbytes in sorted(churn.items(), key=lambda kv: -kv[1])[:top]:
        if nbytes:
            print("%-42s %12s %7.1f%% %12s" % (name, human(nbytes), 100.0 * nbytes / max(total_alloc, 1),
                                               "{:,}".format(churn_blocks[name])))

    print()
    print("Biggest churn by site. `read`/`written` are how much of that memory")
    print("was ever loaded from or stored to -- allocated-and-written-but-never-read")
    print("is copying somebody never asked for.")
    print("%-46s %10s %9s %9s %9s" % ("site", "allocated", "blocks", "read", "written"))
    print("-" * 88)
    for pp in sorted(pps, key=lambda p: -p.get("tb", 0))[:top]:
        print("%-46s %10s %9s %9s %9s" % (
            site(pp)[:46],
            human(pp.get("tb", 0)),
            "{:,}".format(pp.get("tbk", 0)),
            human(pp.get("rb", 0)),
            human(pp.get("wb", 0)),
        ))


def print_maps_table(maps, threads):
    """The /proc/<pid>/smaps roll-up: what each part of RSS actually is."""
    total_rss = sum(v[0] for v in maps.values())
    print()
    print("=" * 78)
    print("WHAT THAT RESIDENT MEMORY *IS* (from /proc/<pid>/smaps)")
    print("=" * 78)
    print("%-44s %10s %10s %7s" % ("mapping", "RSS", "PSS", "share"))
    print("-" * 74)
    for label, (rss_kb, pss_kb, _count) in sorted(maps.items(), key=lambda kv: -kv[1][0]):
        print("%-44s %10s %10s %6.1f%%" % (
            label, human(rss_kb * 1024), human(pss_kb * 1024),
            100.0 * rss_kb / max(total_rss, 1)))
    print("-" * 74)
    print("%-44s %10s" % ("total", human(total_rss * 1024)))
    print()
    print("Threads: %d. Anonymous memory is the allocator's arenas plus one stack" % threads)
    print("per thread, which /proc does not separate; massif's heap total is the")
    print("part of it that is live allocations.")


def snapshot_pid(pid, top=25):
    """Break down a process that is already running -- your own session, as it
    is right now. No workload, nothing spawned, nothing slowed down: this only
    reads /proc, so the editor does not notice it happened."""
    try:
        exe = os.readlink("/proc/%d/exe" % pid)
    except OSError as exc:
        sys.exit("cannot read /proc/%d: %s" % (pid, exc))
    rec = {}
    with open("/proc/%d/status" % pid) as f:
        for line in f:
            if line.startswith(("Name:", "VmRSS:", "VmHWM:", "VmSize:", "Threads:")):
                key, val = line.split(":", 1)
                rec[key] = val.strip()
    print("pid %d  %s" % (pid, exe))
    print("RSS now %s, peak %s (VmHWM), %s threads" % (
        human(int(rec.get("VmRSS", "0 kB").split()[0]) * 1024),
        human(int(rec.get("VmHWM", "0 kB").split()[0]) * 1024),
        rec.get("Threads", "?")))
    maps = smaps_breakdown(pid, exe)
    if not maps:
        sys.exit("no mappings read; is the process still alive?")
    print_maps_table(maps, int(rec.get("Threads", 0)))


def analyze_profile(path, top=25):
    """Summarize a profile this run did not produce -- one left behind by a
    session you drove yourself under Valgrind."""
    with open(path, "rb") as f:
        head = f.read(2048)
    if head.lstrip().startswith(b"{"):
        summarize_dhat(path, top=top)
    elif b"massif" in head or b"mem_heap_B" in head:
        summarize_massif(path, top=top)
    else:
        sys.exit("%s looks like neither a massif nor a dhat profile" % path)


# ------------------------------------------------------------------ main ---


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--tool", choices=("massif", "dhat", "rss"), default="massif")
    ap.add_argument("--pid", type=int,
                    help="break down a process that is already running -- an interactive "
                         "session of your own -- and exit. Reads /proc only")
    ap.add_argument("--analyze", metavar="PROFILE",
                    help="summarize an existing massif or dhat profile and exit, e.g. one "
                         "left by a session you drove yourself under Valgrind")
    ap.add_argument("--binary", default=os.path.join(REPO, "target/profiling/fresh"))
    ap.add_argument("--out", default=os.path.join(REPO, "target/memory-profile"))
    ap.add_argument("--workload", choices=tuple(WORKLOADS), default="edit",
                    help="edit: open files and walk/type through them. "
                         "orchestrator: several workspaces, each a git worktree with its own "
                         "terminal, file explorer and buffers, over a throwaway clone of the repo")
    ap.add_argument("--workspaces", type=int, default=3,
                    help="orchestrator workload: how many workspaces to spawn (default 3)")
    ap.add_argument("--repo", default=REPO,
                    help="orchestrator workload: repository to clone and work in (default: this one)")
    ap.add_argument("--files", nargs="*", help="edit workload: files to open (default: a few repo sources)")
    ap.add_argument("--pages-as-heap", action="store_true",
                    help="massif: account every mapped page, not just malloc -- "
                         "totals then include the binary, stacks and mmap'd arenas")
    ap.add_argument("--quiet-window", type=float, default=None,
                    help="seconds of silence that counts as 'the editor is done repainting' "
                         "(default: 0.8 native, 2.5 under massif, 5 under dhat)")
    ap.add_argument("--max-step", type=float, default=None,
                    help="hard cap in seconds on how long one workload step may take "
                         "(default: 25 native, 120 under Valgrind)")
    ap.add_argument("--verbose", action="store_true", help="log each workload step as it runs")
    ap.add_argument("--env", action="append", default=[], metavar="KEY=VALUE",
                    help="extra environment for the editor, repeatable. The allocator knobs are "
                         "the interesting ones: MALLOC_ARENA_MAX=2 answers how much of RSS is "
                         "glibc's per-thread arenas rather than live data")
    ap.add_argument("--top", type=int, default=25)
    ap.add_argument("--json", help="also write the machine-readable numbers here")
    args = ap.parse_args()

    if args.pid:
        snapshot_pid(args.pid, top=args.top)
        return
    if args.analyze:
        analyze_profile(args.analyze, top=args.top)
        return

    if not os.path.exists(args.binary):
        sys.exit("no binary at %s\n  build it with: cargo build --profile profiling --bin fresh" % args.binary)
    if args.tool != "rss" and not shutil.which("valgrind"):
        sys.exit("valgrind is not installed; use --tool rss for a no-Valgrind RSS timeline")

    os.makedirs(args.out, exist_ok=True)
    stamp = time.strftime("%Y%m%d-%H%M%S")
    # Short, and deliberately not under a deep scratch path: the editor's
    # control socket lives under $XDG_RUNTIME_DIR, and a unix socket path over
    # ~108 bytes fails to bind with "exceeds capacity of sun_path", which costs
    # the editor its command socket and the profile its realism.
    home = tempfile.mkdtemp(prefix="fmp-", dir="/tmp")
    env = isolated_env(home)
    for entry in args.env:
        key, _, value = entry.partition("=")
        env[key] = value
    cwd = None

    if args.workload == "orchestrator":
        # Work in a throwaway clone: the workload creates worktree workspaces,
        # and each one cuts a branch in the repository it was started from.
        cwd = os.path.join(home, "repo")
        log("cloning %s into %s (workspaces cut branches; the real checkout stays clean)" % (args.repo, cwd))
        subprocess.run(["git", "clone", "--quiet", args.repo, cwd], check=True)
        steps = WORKLOADS[args.workload](args.workspaces)
        opened = ["README.md"]
    else:
        files = args.files or default_files()
        # A modified buffer would make quit prompt; the workload types, so copy
        # the files into the throwaway home and edit those instead of the repo's.
        workdir = os.path.join(home, "work")
        os.makedirs(workdir, exist_ok=True)
        opened = []
        for path in files:
            dest = os.path.join(workdir, os.path.basename(path))
            shutil.copyfile(path, dest)
            opened.append(dest)
        steps = WORKLOADS[args.workload]()

    editor_args = [args.binary, "--no-upgrade-check", "--no-restore"] + opened
    # How long a gap in the editor's output means "it has finished repainting".
    # It has to grow with the slowdown the tool imposes, or the workload starts
    # typing into an editor that is still painting the previous step: dhat
    # instruments every memory access and is markedly slower than massif.
    quiet = args.quiet_window
    if quiet is None:
        quiet = {"rss": 0.8, "massif": 2.5, "dhat": 5.0}[args.tool]

    if args.tool == "massif":
        out_file = os.path.join(args.out, "massif.out.%s" % stamp)
        argv = [
            "valgrind",
            "--tool=massif",
            "--massif-out-file=" + out_file,
            "--detailed-freq=5",
            "--max-snapshots=60",
            "--threshold=0.2",
            # Deep enough to reach past the allocator plumbing every Rust stack
            # starts with and into the code that actually asked for the memory.
            "--depth=20",
        ]
        if args.pages_as_heap:
            argv.append("--pages-as-heap=yes")
        argv += editor_args
    elif args.tool == "dhat":
        out_file = os.path.join(args.out, "dhat.out.%s.json" % stamp)
        argv = ["valgrind", "--tool=dhat", "--dhat-out-file=" + out_file, "--num-callers=20"] + editor_args
    else:
        out_file = os.path.join(args.out, "rss.%s.json" % stamp)
        argv = editor_args

    log("workload: %s (%d steps)" % (args.workload, len(steps)))
    log("running: %s" % " ".join(argv))
    if args.tool != "rss":
        log("Valgrind makes this 20-50x slower than native; a full run is minutes, not seconds.")

    samples = [] if args.tool == "rss" else None
    started = time.time()
    max_step = args.max_step if args.max_step is not None else (25.0 if args.tool == "rss" else 120.0)
    screen_out = os.path.join(args.out, "last-screen.%s.txt" % stamp)
    samplers = []
    status = run_workload(argv, env, quiet, steps, rss_samples=samples, cwd=cwd,
                          max_step=max_step, verbose=args.verbose, screen_out=screen_out,
                          maps_out=samplers)
    elapsed = time.time() - started
    log("editor exited (status %d) after %.1fs" % (status, elapsed))

    if args.tool == "massif":
        if not os.path.exists(out_file):
            sys.exit("massif wrote no output -- did the editor start? (%s)" % out_file)
        summarize_massif(out_file, top=args.top)
        print()
        print("Raw profile:   %s" % out_file)
        if shutil.which("ms_print"):
            report = out_file + ".txt"
            with open(report, "w") as f:
                subprocess.run(["ms_print", out_file], stdout=f, check=False)
            print("Full tree:     %s   (ms_print, with the allocation graph)" % report)
    elif args.tool == "dhat":
        if not os.path.exists(out_file):
            sys.exit("dhat wrote no output -- did the editor start? (%s)" % out_file)
        summarize_dhat(out_file, top=args.top)
        print()
        print("Raw profile:   %s" % out_file)
        print("Full detail:   load it at https://nnethercote.github.io/dh_view/dh_view.html")
        print("               (the viewer runs in the browser; the file is not uploaded)")
    else:
        with open(out_file, "w") as f:
            json.dump(samples, f, indent=2)
        print()
        print("=" * 78)
        print("RESIDENT MEMORY THROUGH THE WORKLOAD (no Valgrind, real speed)")
        print("=" * 78)
        print("%-28s %10s %10s %10s" % ("after", "RSS", "PSS", "anon"))
        for rec in samples:
            print("%-28s %10s %10s %10s" % (
                rec["label"][:28],
                human(rec.get("VmRSS", 0) * 1024),
                human(rec.get("Pss", 0) * 1024),
                human(rec.get("Anonymous", 0) * 1024),
            ))
        if samples:
            print()
            print("Peak RSS (VmHWM): %s" % human(max(r.get("VmHWM", 0) for r in samples) * 1024))
        maps = samplers[0].maps if samplers else {}
        if maps:
            print_maps_table(maps, samplers[0].threads)
        print()
        print("Samples:       %s" % out_file)

    if args.json:
        with open(args.json, "w") as f:
            json.dump({"tool": args.tool, "argv": argv, "elapsed_s": elapsed,
                       "out_file": out_file, "samples": samples}, f, indent=2)
    shutil.rmtree(home, ignore_errors=True)


if __name__ == "__main__":
    main()
