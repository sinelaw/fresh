#!/usr/bin/env python3
"""
Empirically measure terminal output traffic for TUI editors, to reproduce and
quantify "lag over a serial console".

A serial line at 115200 8N1 carries ~11,520 bytes/sec (10 bits/byte incl.
start+stop). Therefore the number of bytes an editor writes to the terminal in
response to a keystroke is, to first order, the floor on the latency the user
feels on that link:

    serial_ms(n_bytes) = n_bytes * 10 / 115200 * 1000  ~=  n_bytes * 0.0868 ms

We run the editor under a pseudo-terminal, feed it a scripted, identical
sequence of input events, and for each event count the bytes it emits back to
the "terminal" (the pty master = what would physically go down the wire). We
also measure idle output (does it stream bytes with no input? e.g. cursor
blink / periodic repaint -- that alone saturates a slow serial link).
"""

import os, pty, select, time, sys, fcntl, struct, termios, json, argparse, tempfile, signal

BAUD = 115200
BITS_PER_BYTE = 10  # 8 data + 1 start + 1 stop, no parity


def serial_ms(nbytes, baud=BAUD):
    return nbytes * BITS_PER_BYTE / baud * 1000.0


def spawn(argv, env, cols=80, rows=24):
    pid, fd = pty.fork()
    if pid == 0:
        for k, v in env.items():
            os.environ[k] = v
        try:
            os.execvp(argv[0], argv)
        except Exception as e:
            sys.stderr.write("exec failed: %r\n" % (e,))
        os._exit(127)
    # parent: force a known window size on the tty
    winsize = struct.pack("HHHH", rows, cols, 0, 0)
    fcntl.ioctl(fd, termios.TIOCSWINSZ, winsize)
    return pid, fd


def drain(fd, quiet=0.35, maxwait=8.0):
    """Read until no bytes arrive for `quiet` seconds (or maxwait elapses)."""
    buf = bytearray()
    start = time.time()
    while True:
        timeout = quiet
        r, _, _ = select.select([fd], [], [], timeout)
        if r:
            try:
                data = os.read(fd, 65536)
            except OSError:
                break
            if not data:
                break
            buf += data
        else:
            break  # quiescent
        if time.time() - start > maxwait:
            break
    return bytes(buf)


def idle_collect(fd, seconds):
    """Collect everything emitted over a fixed window with no input."""
    buf = bytearray()
    end = time.time() + seconds
    while time.time() < end:
        r, _, _ = select.select([fd], [], [], end - time.time())
        if r:
            try:
                data = os.read(fd, 65536)
            except OSError:
                break
            if not data:
                break
            buf += data
    return bytes(buf)


import re
def analyze_escapes(data):
    """Characterize a chunk of terminal output: how many cursor moves, color
    changes, clears -- i.e. what the editor is spending bytes on."""
    text = data
    return {
        "bytes": len(data),
        "cursor_pos": len(re.findall(rb"\x1b\[[0-9;]*[Hf]", text)),   # CUP
        "sgr_color": len(re.findall(rb"\x1b\[[0-9;]*m", text)),       # set graphics
        "erase_line": len(re.findall(rb"\x1b\[[0-9]*K", text)),       # EL
        "erase_disp": len(re.findall(rb"\x1b\[[0-9]*J", text)),       # ED (clear screen)
        "cursor_updn": len(re.findall(rb"\x1b\[[0-9]*[ABCD]", text)), # rel moves
        "printable": sum(1 for b in text if 0x20 <= b < 0x7f),
    }


def run_scenario(name, argv, env, testfile, cols=80, rows=24):
    pid, fd = spawn(argv, env, cols, rows)
    try:
        # settle startup; ignore the initial paint for per-key numbers but record it
        startup = drain(fd, quiet=0.6, maxwait=10.0)
        results = {"editor": name, "argv": argv, "cols": cols, "rows": rows}
        results["startup_bytes"] = len(startup)

        captures = {}
        def event(label, data, quiet=0.35, reps=1):
            tot = 0
            last = b""
            for _ in range(reps):
                os.write(fd, data)
                out = drain(fd, quiet=quiet)
                tot += len(out)
                last = out
            captures[label] = last  # keep last sample for escape analysis
            return {"bytes_total": tot, "reps": reps,
                    "bytes_per_event": tot / reps,
                    "serial_ms_per_event": serial_ms(tot / reps),
                    "sample_analysis": analyze_escapes(last)}

        steps = {}
        # 1) Type characters one at a time (the dominant typing feel)
        steps["type_char"] = event("type_char", b"a", reps=20)
        # 2) Arrow-key cursor movement
        steps["arrow_down"] = event("arrow_down", b"\x1b[B", reps=20)
        steps["arrow_right"] = event("arrow_right", b"\x1b[C", reps=20)
        # 3) Page down (scrolling -- forces large region redraw)
        steps["page_down"] = event("page_down", b"\x1b[6~", reps=10)
        # 4) Backspace
        steps["backspace"] = event("backspace", b"\x7f", reps=20)
        results["steps"] = steps
        # keep a hex sample of one keystroke's output for the report
        results["sample_type_char_hex"] = captures.get("type_char", b"")[:400].hex()

        # 5) Idle: no input, measure streamed bytes over 3 s (cursor blink etc.)
        idle = idle_collect(fd, 3.0)
        results["idle_bytes_3s"] = len(idle)
        results["idle_bytes_per_sec"] = len(idle) / 3.0
        return results
    finally:
        try:
            os.kill(pid, signal.SIGKILL)
            os.waitpid(pid, 0)
        except Exception:
            pass
        try:
            os.close(fd)
        except Exception:
            pass


def make_testfile():
    fd, path = tempfile.mkstemp(suffix=".txt", prefix="seriallag_")
    with os.fdopen(fd, "w") as f:
        for i in range(1, 401):
            f.write("line %4d: the quick brown fox jumps over the lazy dog 0123456789\n" % i)
    return path


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--fresh", default="target/release/fresh")
    ap.add_argument("--nano", default="nano")
    ap.add_argument("--cols", type=int, default=80)
    ap.add_argument("--rows", type=int, default=24)
    args = ap.parse_args()

    testfile = make_testfile()
    home = tempfile.mkdtemp(prefix="seriallag_home_")
    base_env = {
        "TERM": "xterm-256color",
        "HOME": home,
        "PATH": os.environ.get("PATH", "/usr/bin:/bin"),
        "LANG": "C.UTF-8",
        "LC_ALL": "C.UTF-8",
        "NO_COLOR": "",  # leave colors on; serial terminals do support them
    }

    editors = []
    if os.path.exists(args.fresh):
        editors.append(("fresh", [args.fresh, testfile]))
    editors.append(("nano", [args.nano, testfile]))

    all_results = []
    for name, argv in editors:
        try:
            res = run_scenario(name, argv, base_env, testfile, args.cols, args.rows)
            all_results.append(res)
        except Exception as e:
            all_results.append({"editor": name, "error": repr(e)})

    print(json.dumps(all_results, indent=2))

    # Human-readable comparison table
    print("\n=== Serial-lag comparison @ %d baud (%.1f bytes/sec) ===" %
          (BAUD, BAUD / BITS_PER_BYTE))
    print("File: 400 lines, terminal %dx%d, TERM=xterm-256color\n" % (args.cols, args.rows))
    hdr = "%-12s %12s %12s %12s %12s %12s" % (
        "metric", "fresh B", "fresh ms", "nano B", "nano ms", "ratio")
    print(hdr)
    print("-" * len(hdr))

    def get(res, *keys):
        for k in keys:
            res = res.get(k, {}) if isinstance(res, dict) else {}
        return res

    rmap = {r["editor"]: r for r in all_results if "editor" in r and "steps" in r}
    fr = rmap.get("fresh"); na = rmap.get("nano")
    if fr and na:
        def row(label, fb, nb):
            ratio = (fb / nb) if nb else float("inf")
            print("%-12s %12.0f %12.1f %12.0f %12.1f %11.1fx" % (
                label, fb, serial_ms(fb), nb, serial_ms(nb), ratio))
        print("%-12s %12.0f %12.1f %12.0f %12.1f %11.1fx" % (
            "startup", fr["startup_bytes"], serial_ms(fr["startup_bytes"]),
            na["startup_bytes"], serial_ms(na["startup_bytes"]),
            fr["startup_bytes"] / max(na["startup_bytes"], 1)))
        for key in ["type_char", "arrow_down", "arrow_right", "page_down", "backspace"]:
            row(key, fr["steps"][key]["bytes_per_event"], na["steps"][key]["bytes_per_event"])
        print("%-12s %12.0f %12.1f %12.0f %12.1f %11s" % (
            "idle/sec", fr["idle_bytes_per_sec"], serial_ms(fr["idle_bytes_per_sec"]),
            na["idle_bytes_per_sec"], serial_ms(na["idle_bytes_per_sec"]), "-"))

        # Why: escape-sequence breakdown for a single keystroke / arrow press
        for key in ["type_char", "arrow_down", "page_down"]:
            fa = fr["steps"][key]["sample_analysis"]
            naa = na["steps"][key]["sample_analysis"]
            print("\n[%s] per-event escape breakdown (one sample):" % key)
            print("   %-10s fresh=%-6d nano=%-6d" % ("bytes", fa["bytes"], naa["bytes"]))
            for m in ["cursor_pos", "sgr_color", "erase_line", "erase_disp",
                      "cursor_updn", "printable"]:
                print("   %-10s fresh=%-6d nano=%-6d" % (m, fa[m], naa[m]))


if __name__ == "__main__":
    main()
