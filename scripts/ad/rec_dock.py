#!/usr/bin/env python3
"""Record the landing page's Orchestrator loop: the dock hopping between workspaces.

Unlike rec.py this drives a Fresh that is already running in tmux session `o`
(140x75, dock focused with Alt+O), since the scene is a daemon session built by
hand: workspaces filed into dock folders, each showing something different
(a working agent, an agent waiting on a permission prompt from
waiting_agent.py, a Review Diff, a Rust buffer, markdown in compose mode). Attaching a client would only repaint what
redraws, so it samples `tmux capture-pane` instead and writes each change as a
full-screen asciicast frame.

The selection hops quickly across folder headers and holds on each
workspace; at the end it runs back up to the first workspace, so the clip
loops from its own last frame.

  python3 rec_dock.py casts/switch.cast && python3 cast2frames.py casts/switch.cast frames/switch.json
"""
import json
import subprocess
import sys
import threading
import time

# dock rows (1 = first row under the search box) that hold workspaces, in order
ROWS = [2, 3, 5, 6, 7, 8]
HOLD = 1.2     # seconds on each workspace
HOP = 0.09     # seconds per row when passing over a folder header or running back

def key(*k):
    subprocess.run(["tmux", "send-keys", "-t", "o", *k])

def screen():
    return subprocess.run(["tmux", "capture-pane", "-p", "-e", "-t", "o"],
                          capture_output=True, text=True).stdout

def frame(s):
    rows = s.rstrip("\n").split("\n")
    return "\x1b[H\x1b[2J\x1b[m" + "\r\n".join(r + "\x1b[m" for r in rows)

out = sys.argv[1]
for _ in range(12):
    key("Up")
key("Down")                      # onto the first workspace
time.sleep(1.0)

done = False
def drive():
    global done
    row = ROWS[0]
    for target in ROWS[1:]:
        time.sleep(HOLD)
        while row < target:
            key("Down")
            row += 1
            if row < target:
                time.sleep(HOP)
    time.sleep(HOLD)
    while row > ROWS[0]:
        key("Up")
        row -= 1
        time.sleep(HOP)
    time.sleep(0.25)             # let the first workspace land, then stop
    done = True

ev, last, t0 = [], None, time.time()
threading.Thread(target=drive, daemon=True).start()
while not done:
    s = screen()
    if s != last:
        ev.append([round(time.time() - t0, 3), "o", frame(s)])
        last = s
    time.sleep(0.02)
with open(out, "w") as f:
    f.write(json.dumps({"version": 2, "width": 140, "height": 75}) + "\n")
    for e in ev:
        f.write(json.dumps(e) + "\n")
print(len(ev), "frames,", round(time.time() - t0, 2), "s")
