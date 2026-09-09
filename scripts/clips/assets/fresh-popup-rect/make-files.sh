#!/usr/bin/env bash
# Materialise the files the fresh-popup-rect clip films.
#
# One job — put a popup on screen near an edge, and take it away again — as
# it was written before #3028 and as it is written now. BEFORE is the commit
# that PR branched from; AFTER is what master has today, the file being
# unchanged since it merged, so "after" and "now" are the same thing.
#
# Whole files, straight out of git, with two mechanical edits.
#
# Comments are stripped: the clip makes its argument in its own captions, and
# a reader who stops to read a `// +2 for borders` has stopped looking at the
# shape of the code, which is the only thing the clip is about.
#
# Then rustfmt re-wraps at MAXCOL. A line wider than the terminal wraps on
# screen, which puts a continuation row in the middle of a beat and makes a
# line look like two — and a beat framed on a rect of rows cannot survive
# that. Re-wrapping is rustfmt's own judgement about the same code, not an
# edit to it; no excerpting, no rewriting, so every line on screen is a line
# that shipped.
#
# Basenames are distinct because the tab bar shows basenames, and two tabs
# reading `theme_info.rs` would be a puzzle rather than a comparison.
#
#   ./make-files.sh /path/to/dir
set -euo pipefail
OUT="${1:?usage: make-files.sh <dir>}"
REPO="${FRESH_REPO:-$HOME/repos/fresh}"
BEFORE_REF=93741f1e
MAXCOL=68
AFTER_REF=origin/master

rm -rf "$OUT"
mkdir -p "$OUT/before" "$OUT/after"

strip() {
  python3 -c '
import sys, re
out, blanks = [], 0
for line in sys.stdin.read().splitlines():
    t = line.strip()
    if t.startswith("//"):            # a whole line of comment: gone
        continue
    # a trailing comment, but only where the "//" is not inside a string
    i = line.find("//")
    while i != -1:
        if line[:i].count(chr(34)) % 2 == 0:
            line = line[:i].rstrip()
            break
        i = line.find("//", i + 2)
    if not line.strip():
        blanks += 1
        if blanks > 1:                 # the gaps comments leave behind
            continue
    else:
        blanks = 0
    out.append(line)
while out and not out[-1].strip():
    out.pop()
print("\n".join(out))
'
}

emit() {   # <ref>:<path> <out>
  git -C "$REPO" show "$1" | strip > "$2"
  rustfmt --edition 2021 --config max_width=$MAXCOL "$2"
  # A handful of lines have nothing to break on -- a long string literal, a
  # deep path -- and rustfmt leaves them. They only matter if one lands in a
  # filmed region, where a wrap would put a continuation row in the middle of
  # a beat, so this reports them and the anchors are checked separately.
  awk -v m=$MAXCOL -v f="$2" 'length>m {print "  wide: "f":"FNR" ("length")"}' "$2"
}
emit "$BEFORE_REF:crates/fresh-editor/src/app/chrome/theme_info.rs" \
     "$OUT/before/chrome_theme_info.rs"
emit "$BEFORE_REF:crates/fresh-editor/src/app/theme_inspect.rs" \
     "$OUT/before/theme_inspect.rs"
emit "$AFTER_REF:crates/fresh-editor/src/view/shell/theme_info.rs" \
     "$OUT/after/shell_theme_info.rs"

# Where each screen opens. Stripping moves every line, so the clip cannot
# hold line numbers: it asks for these anchors and gets told where they are.
# A rebase upstream that moves the code fails here rather than filming the
# wrong function.
echo "anchors:"
anchor() {
  local f="$1" pat="$2"
  local n
  n=$(grep -n -m1 -- "$pat" "$OUT/$f" | cut -d: -f1) \
    || { echo "not found in $f: $pat" >&2; exit 1; }
  [ -n "$n" ] || { echo "not found in $f: $pat" >&2; exit 1; }
  printf '  %-28s %-34s %s\n' "$(basename "$f")" "$pat" "$n"
}
anchor before/chrome_theme_info.rs 'fn collect'
anchor before/chrome_theme_info.rs 'theme_info_guard", PointerPress::Left'
anchor before/theme_inspect.rs     'let height = lines.len()'
anchor before/theme_inspect.rs     'let height = line_count'
anchor before/theme_inspect.rs     'fn compute_popup_rect'
anchor after/shell_theme_info.rs   'pub fn layer'
echo "files ready: $OUT"
