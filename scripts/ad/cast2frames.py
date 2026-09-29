#!/usr/bin/env python3
"""Replay an asciicast through pyte and dump deduplicated screen snapshots.

Output JSON: {cols, rows, screens: [[row runs...]], timeline: [[t, screen_idx, cx, cy, cursor_visible]]}
Each row is a list of runs [text, fg, bg, bold]; colours are '#rrggbb'.
"""
import json
import sys

import pyte

NAMED = {
    "black": "#000000", "red": "#cd3131", "green": "#0dbc79", "brown": "#e5e510",
    "yellow": "#e5e510", "blue": "#2472c8", "magenta": "#bc3fbc", "cyan": "#11a8cd",
    "white": "#e5e5e5", "brightblack": "#666666", "brightred": "#f14c4c",
    "brightgreen": "#23d18b", "brightbrown": "#f5f543", "brightyellow": "#f5f543",
    "brightblue": "#3b8eea", "brightmagenta": "#d670d6", "brightcyan": "#29b8db",
    "brightwhite": "#ffffff",
}
DEF_FG, DEF_BG = "#d4d4d4", "#1e1e1e"


def col(c, default):
    if c == "default":
        return default
    if c in NAMED:
        return NAMED[c]
    if len(c) == 6:
        return "#" + c
    return default


def snapshot(screen):
    rows = []
    for y in range(screen.lines):
        line = screen.buffer[y]
        runs = []
        x = 0
        while x < screen.columns:
            ch = line[x]
            fg, bg = col(ch.fg, DEF_FG), col(ch.bg, DEF_BG)
            if ch.reverse:
                fg, bg = bg, fg
            data = ch.data or " "
            if data == "":
                data = " "
            key = (fg, bg, int(ch.bold) | (2 if ch.underscore else 0) | (4 if ch.italics else 0))
            if runs and tuple(runs[-1][1:]) == key and len(data) == 1:
                runs[-1][0] += data
            else:
                runs.append([data, fg, bg, key[2]])
            x += 1
        rows.append(runs)
    return rows


def main(cast, out, fps=30.0):
    with open(cast) as f:
        header = json.loads(f.readline())
        events = [json.loads(l) for l in f if l.strip()]
    end_o = max((e[0] for e in events if e[1] == "o"), default=0)
    cols, rows = header["width"], header["height"]
    screen = pyte.Screen(cols, rows)
    stream = pyte.ByteStream(screen)
    screens, index, timeline = [], {}, []
    end = max(e[0] for e in events)
    ei = 0
    t = 0.0
    while t <= end + 0.5:
        while ei < len(events) and events[ei][0] <= t:
            if events[ei][1] == "o":
                stream.feed(events[ei][2].encode("utf-8", "surrogateescape"))
            ei += 1
        snap = snapshot(screen)
        key = json.dumps(snap)
        if key not in index:
            index[key] = len(screens)
            screens.append(snap)
        timeline.append([round(t, 4), index[key], screen.cursor.x, screen.cursor.y,
                         0 if screen.cursor.hidden else 1])
        t += 1 / fps
    keys = [[round(e[0], 3), e[2]] for e in events if e[1] == "k"]
    mouse = [[round(e[0], 3)] + [int(v) for v in e[2].split(",")] for e in events if e[1] == "m"]
    with open(out, "w") as f:
        json.dump({"cols": cols, "rows": rows, "fps": fps, "screens": screens,
                   "timeline": timeline, "keys": keys, "mouse": mouse}, f, separators=(",", ":"))
    print(f"{out}: {len(timeline)} frames, {len(screens)} unique screens")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
