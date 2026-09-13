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
CTRL_END = b"\x1b[1;5F"
CTRL_HOME = b"\x1b[1;5H"
PAGE_DOWN = b"\x1b[6~"
PAGE_UP = b"\x1b[5~"
CTRL_PAGE_DOWN = b"\x1b[6;5~"
CTRL_S = b"\x13"

# The scripted workload: (label, bytes, repeats). Kept to keys whose default
# binding is stable (see crates/fresh-editor-core/keymaps/default.json) so
# the profile measures the editor, not a keymap change.
WORKLOAD = [
    ("walk to end of buffer", CTRL_END, 1),
    ("page up through it", PAGE_UP, 6),
    ("page down through it", PAGE_DOWN, 6),
    ("back to the top", CTRL_HOME, 1),
    ("type", b"fn memory_profile_probe() {}", 1),
    # Save so the quit at the end is not held up by a "buffer is modified"
    # prompt: the files are throwaway copies, so writing them costs nothing
    # and it keeps the run unattended.
    ("save", CTRL_S, 1),
    ("next buffer", CTRL_PAGE_DOWN, 1),
    ("walk to end of buffer", CTRL_END, 1),
    ("page up through it", PAGE_UP, 6),
    ("next buffer", CTRL_PAGE_DOWN, 1),
    ("walk to end of buffer", CTRL_END, 1),
]


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


def spawn_pty(argv, env, cols=120, rows=40):
    """Fork the child onto the slave end of a new pty, and return (pid, master)."""
    pid, fd = pty.fork()
    if pid == 0:
        os.environ.clear()
        os.environ.update(env)
        try:
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


def drain(fd, quiet=0.5, maxwait=120.0):
    """Read until the child stops writing for `quiet` seconds.

    Waiting on output rather than sleeping a fixed amount is what keeps the
    workload honest under Valgrind, where every step is 20-50x slower than
    native and a fixed sleep would race the editor's repaint.
    """
    buf = bytearray()
    start = time.time()
    while True:
        r, _, _ = select.select([fd], [], [], quiet)
        if not r:
            return bytes(buf)
        try:
            data = os.read(fd, 65536)
        except OSError:
            return bytes(buf)
        if not data:
            return bytes(buf)
        buf += data
        if time.time() - start > maxwait:
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


def run_workload(argv, env, quiet, rss_samples=None, cols=120, rows=40):
    """Drive one editor launch through WORKLOAD and return its exit status.

    `rss_samples` (a list) turns on /proc sampling of the editor process.
    """
    pid, fd = spawn_pty(argv, env, cols, rows)
    sampler = ProcSampler(pid, rss_samples) if rss_samples is not None else None
    try:
        if sampler:
            sampler.sample("start")
        drain(fd, quiet=quiet, maxwait=180.0)
        if sampler:
            sampler.sample("after startup")
        for label, keys, reps in WORKLOAD:
            for _ in range(reps):
                if not write_keys(fd, keys):
                    break
                drain(fd, quiet=quiet)
            if sampler:
                sampler.sample(label)
        write_keys(fd, CTRL_Q)
        drain(fd, quiet=quiet)
        # The workload saves before quitting, so there should be no prompt --
        # but if something else left a buffer dirty, answer "discard" rather
        # than hang. Valgrind only writes its profile on a clean exit, so a
        # run that ends in SIGKILL produces nothing at all.
        write_keys(fd, b"d")
        drain(fd, quiet=quiet)
        deadline = time.time() + 180.0
        while time.time() < deadline:
            done, status = os.waitpid(pid, os.WNOHANG)
            if done:
                return status
            drain(fd, quiet=0.2, maxwait=1.0)
        log("editor did not exit in time; killing it")
        os.kill(pid, signal.SIGKILL)
        return os.waitpid(pid, 0)[1]
    finally:
        try:
            os.close(fd)
        except OSError:
            pass


class ProcSampler:
    """Reads resident memory straight out of /proc for the live process."""

    def __init__(self, pid, out):
        self.pid = pid
        self.out = out

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


# ------------------------------------------------------------------ main ---


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--tool", choices=("massif", "dhat", "rss"), default="massif")
    ap.add_argument("--binary", default=os.path.join(REPO, "target/profiling/fresh"))
    ap.add_argument("--out", default=os.path.join(REPO, "target/memory-profile"))
    ap.add_argument("--files", nargs="*", help="files for the editor to open (default: a few repo sources)")
    ap.add_argument("--pages-as-heap", action="store_true",
                    help="massif: account every mapped page, not just malloc -- "
                         "totals then include the binary, stacks and mmap'd arenas")
    ap.add_argument("--quiet-window", type=float, default=None,
                    help="seconds of silence that counts as 'the editor is done repainting' "
                         "(default: 0.8 native, 2.5 under massif, 5 under dhat)")
    ap.add_argument("--top", type=int, default=25)
    ap.add_argument("--json", help="also write the machine-readable numbers here")
    args = ap.parse_args()

    if not os.path.exists(args.binary):
        sys.exit("no binary at %s\n  build it with: cargo build --profile profiling --bin fresh" % args.binary)
    if args.tool != "rss" and not shutil.which("valgrind"):
        sys.exit("valgrind is not installed; use --tool rss for a no-Valgrind RSS timeline")

    files = args.files or default_files()
    os.makedirs(args.out, exist_ok=True)
    stamp = time.strftime("%Y%m%d-%H%M%S")
    home = tempfile.mkdtemp(prefix="fresh-memprof-home-")
    env = isolated_env(home)
    # A modified buffer would make quit prompt; the workload types, so copy the
    # files into the throwaway home and edit those instead of the repo's.
    workdir = os.path.join(home, "work")
    os.makedirs(workdir, exist_ok=True)
    copies = []
    for path in files:
        dest = os.path.join(workdir, os.path.basename(path))
        shutil.copyfile(path, dest)
        copies.append(dest)

    editor_args = [args.binary, "--no-upgrade-check", "--no-restore"] + copies
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

    log("workload files: %s" % ", ".join(os.path.basename(c) for c in copies))
    log("running: %s" % " ".join(argv))
    if args.tool != "rss":
        log("Valgrind makes this 20-50x slower than native; a full run is minutes, not seconds.")

    samples = [] if args.tool == "rss" else None
    started = time.time()
    status = run_workload(argv, env, quiet, rss_samples=samples)
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
        print()
        print("Samples:       %s" % out_file)

    if args.json:
        with open(args.json, "w") as f:
            json.dump({"tool": args.tool, "argv": argv, "elapsed_s": elapsed,
                       "out_file": out_file, "samples": samples}, f, indent=2)
    shutil.rmtree(home, ignore_errors=True)


if __name__ == "__main__":
    main()
