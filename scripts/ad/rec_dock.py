#!/usr/bin/env python3
"""Record the landing page's Orchestrator loop: the dock walking its workspaces.

Unlike rec.py this drives a Fresh that is already running in tmux session `o`
(140x75, dock focused with Alt+O), since the scene is a daemon session built by
hand: a few workspaces with agents mid-task, filed into dock folders. Attaching
a client would only repaint what redraws, so it samples `tmux capture-pane`
instead and writes each change as a full-screen asciicast frame.

  python3 rec_dock.py casts/switch.cast && python3 cast2frames.py casts/switch.cast frames/switch.json
"""
import json, time, subprocess, threading, sys
out = sys.argv[1]; t0 = time.time(); ev = []; done = False
def keys():
    global done
    k = lambda *a: subprocess.run(["tmux", "send-keys", "-t", "o", *a])
    for _ in range(8): k("Up")
    time.sleep(2.5)
    for _ in range(7):
        k("Down"); time.sleep(2.4)
    done = True
threading.Thread(target=keys, daemon=True).start()
last = None
while not done:
    s = subprocess.run(["tmux", "capture-pane", "-p", "-e", "-t", "o"], capture_output=True, text=True).stdout
    if s != last:
        rows = s.rstrip("\n").split("\n")
        ev.append([time.time() - t0, "o", "\x1b[H\x1b[2J\x1b[m" + "\r\n".join(r + "\x1b[m" for r in rows)])
        last = s
    time.sleep(0.04)
with open(out, "w") as f:
    f.write(json.dumps({"version": 2, "width": 140, "height": 75}) + "\n")
    for e in ev: f.write(json.dumps(e) + "\n")
print(len(ev), ev[-1][0])
