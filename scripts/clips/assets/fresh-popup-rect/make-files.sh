#!/usr/bin/env bash
# Materialise the two files the fresh-popup-rect clip films.
#
# One job — place a popup near a screen edge — spelled twice, four months
# apart. BEFORE is the commit #3028 branched from; AFTER is what master has
# today (the file has not changed since that PR merged, so "after" and "now"
# are the same thing). Both come straight out of git: no excerpting, no
# editing, so what the clip shows is what shipped.
#
#   ./make-files.sh /path/to/dir
set -euo pipefail
OUT="${1:?usage: make-files.sh <dir>}"
REPO="${FRESH_REPO:-$HOME/repos/fresh}"
BEFORE_REF=93741f1e
AFTER_REF=origin/master

rm -rf "$OUT"
mkdir -p "$OUT/before" "$OUT/after"
git -C "$REPO" show "$BEFORE_REF:crates/fresh-editor/src/app/theme_inspect.rs" \
  > "$OUT/before/theme_inspect.rs"
git -C "$REPO" show "$AFTER_REF:crates/fresh-editor/src/view/shell/theme_info.rs" \
  > "$OUT/after/theme_info.rs"

# The line each screen opens on, checked here rather than trusted: a rebase
# upstream moves them, and a clip that silently films the wrong function is
# worse than one that fails to build.
grep -q '^fn compute_popup_rect($' <(sed -n '338p' "$OUT/before/theme_inspect.rs") \
  || { echo "before/theme_inspect.rs:338 is not compute_popup_rect" >&2; exit 1; }
grep -q '^pub fn layer(t: &ThemeInfo) -> Node<UiMsg> {$' <(sed -n '73p' "$OUT/after/theme_info.rs") \
  || { echo "after/theme_info.rs:73 is not layer()" >&2; exit 1; }
echo "files ready: $OUT"
