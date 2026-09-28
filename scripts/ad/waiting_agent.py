#!/usr/bin/env python3
"""A coding agent paused on a permission prompt: a short transcript, the
proposed edit, and a choice waiting for the user. Renders once and waits."""
import os, sys, time, ctypes, signal

name = sys.argv[1] if len(sys.argv) > 1 else "agent"
try:
    ctypes.CDLL("libc.so.6").prctl(15, name.encode()[:15] + b"\0", 0, 0, 0)
except Exception:
    pass

def draw(*_):
    cols, rows = os.get_terminal_size()
    w = min(cols, 100) - 2
    E = "\033["
    def c(code, s): return f"{E}{code}m{s}{E}0m"
    out = [f"\033]2;{name}\007{E}H{E}2J"]
    L = []
    L.append(c("1;38;5;141", " ┃ ") + c("1", "You"))
    L.append(c("38;5;141", " ┃ ") + "Expired tokens are still accepted on /admin. Find the bypass and fix it.")
    L.append("")
    L.append(c("38;5;79", " ┃ ") + c("1", "Agent") + c("2", "  · 3 files read · 41s"))
    L.append(c("38;5;79", " ┃ ") + "The admin router mounts " + c("38;5;215", "verify_session") + " before " + c("38;5;215", "check_expiry") + ",")
    L.append(c("38;5;79", " ┃ ") + "and verify_session returns early for service tokens, so the expiry")
    L.append(c("38;5;79", " ┃ ") + "check never runs for them. Moving the check into verify_session")
    L.append(c("38;5;79", " ┃ ") + "closes it for every route, not just /admin.")
    L.append("")
    top = c("38;5;215", " ╭─ " ) + c("1;38;5;215", "Permission required") + c("38;5;215", " " + "─" * (w - 25) + "╮")
    L.append(top)
    box = [
        c("1", "Edit src/auth.rs") + c("2", "  (+4 −1)"),
        "",
        c("2", " 41 ") + "    let claims = decode(token, &keys)?;",
        c("38;5;203", " 42 -    if claims.kind == Kind::Service { return Ok(claims); }"),
        c("38;5;114", " 42 +    if claims.exp < now() {"),
        c("38;5;114", " 43 +        return Err(AuthError::Expired);"),
        c("38;5;114", " 44 +    }"),
        c("38;5;114", " 45 +    if claims.kind == Kind::Service { return Ok(claims); }"),
        c("2", " 46 ") + "    Ok(claims)",
        "",
        "Allow this edit?",
        c("1;38;5;215", "❯ 1. Yes"),
        "  2. Yes, and allow edits to src/ for this session",
        "  3. No, and tell the agent what to do instead",
    ]
    import re
    vis = lambda s: len(re.sub(r"\033\[[0-9;]*m", "", s))
    for b in box:
        L.append(c("38;5;215", " │ ") + b + " " * max(0, w - 4 - vis(b)) + c("38;5;215", "│"))
    L.append(c("38;5;215", " ╰" + "─" * (w - 3) + "╯"))
    pad = max(0, rows - len(L) - 2)
    out.append("\r\n" + "\r\n".join(L) + "\r\n" * pad)
    out.append(c("2", "  ↑↓ select · enter confirm · esc cancel"))
    sys.stdout.write("".join(out)); sys.stdout.flush()

signal.signal(signal.SIGWINCH, draw)
draw()
while True:
    time.sleep(3600)
