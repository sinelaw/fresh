#!/usr/bin/env python3
"""
Diagnose *where* Fresh's per-keystroke serial traffic goes (companion to
serial_lag_bench.py). Two empirical probes, no serial hardware required:

  1. config sweep  -- toggle individual settings and measure bytes/keystroke,
                      isolating which feature drives the output volume.
  2. frame audit   -- count synchronized-update frames (ESC[?2026h..l) emitted
                      per single keystroke, splitting them into "content" frames
                      (real cell changes) vs "empty" frames (no-op repaints that
                      still cost ~48 bytes of wrapper + cursor reposition + SGR
                      reset). Empty frames are pure waste on a slow link.

Usage:
  python3 scripts/serial_lag_diagnose.py sweep
  python3 scripts/serial_lag_diagnose.py frames
"""
import os, pty, select, time, struct, fcntl, termios, signal, tempfile, json, re, sys

FRESH = os.environ.get("FRESH_BIN", "target/release/fresh")
CSI = re.compile(rb"\x1b\[[0-9;?]*[A-Za-z]")
OSC = re.compile(rb"\x1b\][^\x07]*\x07")


def spawn(config=None, rows=24, cols=80):
    home = tempfile.mkdtemp(prefix="freshdiag_")
    os.makedirs(os.path.join(home, ".config", "fresh"), exist_ok=True)
    if config is not None:
        with open(os.path.join(home, ".config", "fresh", "config.json"), "w") as f:
            json.dump(config, f)
    tf = os.path.join(home, "t.txt")
    with open(tf, "w") as f:
        for i in range(1, 401):
            f.write("line %4d: the quick brown fox jumps over the lazy dog 0123456789\n" % i)
    env = dict(TERM="xterm-256color", HOME=home, PATH=os.environ["PATH"],
               LANG="C.UTF-8", LC_ALL="C.UTF-8",
               XDG_STATE_HOME=os.path.join(home, "state"))
    pid, fd = pty.fork()
    if pid == 0:
        os.environ.update(env)
        os.execvp(FRESH, [FRESH, tf])
        os._exit(127)
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))
    return pid, fd


def drain(fd, quiet=0.4, maxwait=8.0):
    buf = bytearray()
    start = time.time()
    while True:
        r, _, _ = select.select([fd], [], [], quiet)
        if r:
            try:
                d = os.read(fd, 65536)
            except OSError:
                break
            if not d:
                break
            buf += d
        else:
            break
        if time.time() - start > maxwait:
            break
    return bytes(buf)


def kill(pid, fd):
    try:
        os.kill(pid, signal.SIGKILL); os.waitpid(pid, 0)
    except Exception:
        pass
    try:
        os.close(fd)
    except Exception:
        pass


def measure(config):
    pid, fd = spawn(config)
    try:
        drain(fd, 0.8, 10)

        def ev(data, reps):
            t = 0
            for _ in range(reps):
                os.write(fd, data); t += len(drain(fd))
            return t / reps
        return {"type": ev(b"a", 20), "down": ev(b"\x1b[B", 20),
                "right": ev(b"\x1b[C", 20), "pgdn": ev(b"\x1b[6~", 10)}
    finally:
        kill(pid, fd)


def sweep():
    configs = {
        "baseline(default)": None,
        "no_cursorline": {"editor": {"highlight_current_line": False}},
        "no_syntax": {"editor": {"syntax_highlighting": False}},
        "no_whitespace": {"editor": {"whitespace_show": False}},
        "no_linenum": {"editor": {"line_numbers": False}},
        "no_scrollbar": {"editor": {"show_vertical_scrollbar": False}},
        "minimal_all": {"editor": {
            "highlight_current_line": False, "syntax_highlighting": False,
            "whitespace_show": False, "line_numbers": False,
            "show_vertical_scrollbar": False, "show_status_bar": False,
            "show_menu_bar": False, "show_tab_bar": False}},
    }
    print("%-20s %8s %8s %8s %8s   (bytes/keystroke)" % ("config", "type", "down", "right", "pgdn"))
    print("-" * 64)
    for name, cfg in configs.items():
        r = measure(cfg)
        print("%-20s %8.0f %8.0f %8.0f %8.0f" % (name, r["type"], r["down"], r["right"], r["pgdn"]))


def frames():
    pid, fd = spawn(None)
    try:
        drain(fd, 0.8, 10)

        def strip(s):
            return OSC.sub(b"", CSI.sub(b"", s))

        def one(data, label):
            os.write(fd, data)
            out = drain(fd, 0.5)
            segs = re.findall(rb"\x1b\[\?2026h(.*?)\x1b\[\?2026l", out, re.S)
            content = empty = empty_bytes = 0
            for s in segs:
                if strip(s).strip():
                    content += 1
                else:
                    empty += 1
                    empty_bytes += len(s) + len(b"\x1b[?2026h\x1b[?2026l")
            print("%-9s total=%-4d frames=%-2d content=%-2d empty=%-2d empty_overhead=%dB"
                  % (label, len(out), len(segs), content, empty, empty_bytes))
        print("Per single keystroke: synchronized-update frames (ESC[?2026h..l)")
        print("content = real cell changes; empty = no-op repaint (pure waste)\n")
        for _ in range(3):
            one(b"a", "type a")
        for _ in range(3):
            one(b"\x1b[B", "arrow_dn")
        idle = drain(fd, 2.0, 2.5)
        print("\nidle 2s after input: bytes=%d (should be 0)" % len(idle))
    finally:
        kill(pid, fd)


if __name__ == "__main__":
    mode = sys.argv[1] if len(sys.argv) > 1 else "sweep"
    if mode == "sweep":
        sweep()
    elif mode == "frames":
        frames()
    else:
        print("usage: serial_lag_diagnose.py [sweep|frames]")
