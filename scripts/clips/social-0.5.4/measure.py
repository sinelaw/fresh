#!/usr/bin/env python3
"""Time `fresh notes.md` to its first full frame in a pty, and its RSS once idle.

    measure.py DEMO_DIR RUNS FRESH_BINARY

First frame = the moment the status bar's "Ln 1" has been written to the
terminal, which is the last row a full paint draws. Memory is the summed RSS of
every process running that binary after a 2s settle, so a daemon the launch
started would be counted too. Run it with XDG_* pointed at a scratch config.
"""
import os, pty, select, signal, statistics, struct, sys, time, fcntl, termios, json

FRESH = sys.argv[3]
DEMO = sys.argv[1]
RUNS = int(sys.argv[2]) if len(sys.argv) > 2 else 15
MARK = b"Ln 1"


def procs():
    out = []
    for p in os.listdir("/proc"):
        if p.isdigit():
            try:
                if os.readlink(f"/proc/{p}/exe") == os.path.realpath(FRESH):
                    out.append(int(p))
            except OSError:
                pass
    return out


def all_rss_kb():
    total, ps = 0, procs()
    for p in ps:
        try:
            for line in open(f"/proc/{p}/status"):
                if line.startswith("VmRSS:"):
                    total += int(line.split()[1])
        except OSError:
            pass
    return total, len(ps)


def kill_all():
    for p in procs():
        try:
            os.kill(p, signal.SIGKILL)
        except OSError:
            pass
    time.sleep(0.3)


def one():
    pid, fd = pty.fork()
    if pid == 0:
        os.chdir(DEMO)
        os.execv(FRESH, [FRESH, "--no-upgrade-check", "notes.md"])
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 36, 100, 0, 0))
    t0 = time.perf_counter()
    buf, first = b"", None
    while time.perf_counter() - t0 < 10:
        r, _, _ = select.select([fd], [], [], 0.05)
        if r:
            try:
                chunk = os.read(fd, 65536)
            except OSError:
                break
            buf += chunk
            if first is None and MARK in buf:
                first = time.perf_counter() - t0
                break
    # keep draining for 2s so it settles, then sample memory
    end = time.perf_counter() + 2.0
    while time.perf_counter() < end:
        r, _, _ = select.select([fd], [], [], 0.05)
        if r:
            try:
                os.read(fd, 65536)
            except OSError:
                break
    rss, nproc = all_rss_kb()
    os.kill(pid, signal.SIGKILL)
    kill_all()
    os.waitpid(pid, 0)
    os.close(fd)
    return first, rss, nproc


res = [one() for _ in range(RUNS)]
ff = [r[0] * 1000 for r in res if r[0]]
rss = [r[1] / 1024 for r in res]
print(json.dumps({
    "runs": RUNS, "painted": len(ff),
    "first_frame_ms": {"median": statistics.median(ff), "min": min(ff), "max": max(ff)},
    "rss_mb": {"median": statistics.median(rss), "min": min(rss), "max": max(rss)},
    "procs": sorted({r[2] for r in res}),
    "binary_mb": os.path.getsize(FRESH) / 1e6,
}, indent=1))
