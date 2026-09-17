#!/usr/bin/env python3
"""Block until W -- the moment every row starts working.

W is one instant shared by all eleven agents, so it is a deadline and never
a per-row delay: the rows are spawned over several seconds, and a delay would
stagger the work by however long staging took.

Under a camera, W is the camera's first shutter rather than a clock. A shot
writes its raw dump into the shots directory at the instant it grabs, so a
new `.xwd` appearing there is visible to any process that can read the
directory. Anchoring both the agents and the capture's absolute `at:` marks
to that one event is what keeps each badge landing on the shot meant to hold
it; two clocks joined by a guessed constant drift, and no value fixes it
because the error is not constant.

With no camera (`CLIP_SHOTS_DIR` unset -- staging by hand, or in tmux) it
falls back to the epoch second the stager chose.

    wait-for-start.py <epoch-second>

The timeout is not a fallback for a slow camera; it is for one that has gone
away. A crashed take should let the row carry on rather than hang a terminal
forever.
"""
import os, sys, time

TIMEOUT = 120.0
deadline = float(sys.argv[1]) if len(sys.argv) > 1 else 0.0
shots = os.environ.get("CLIP_SHOTS_DIR")

if shots:
    end = time.time() + TIMEOUT
    while time.time() < end:
        try:
            if any(f.endswith(".xwd") for f in os.listdir(shots)):
                break
        except OSError:
            pass
        time.sleep(0.02)
else:
    time.sleep(max(0.0, deadline - time.time()))
