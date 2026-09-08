#!/usr/bin/env bash
# Build the repo the welcome-screen clip is filmed in.
#
# The page's cards are live: the finder reads `git ls-files`, and the review
# card reads `git status`. Filmed in fresh's own tree they would both be dead —
# a `Cargo.toml` at the root makes the workspace Restricted, and a restricted
# workspace blocks the very `spawnProcess` calls those cards are made of. So
# the clip brings its own repo: no project manifest, so it opens Trusted, and
# a few edits left uncommitted, so the review card has hunks to count.
set -euo pipefail

ROOT="${1:-$HOME/repos/fresh/target/clips/fresh-welcome-scroll-repo}"
rm -rf "$ROOT"
mkdir -p "$ROOT/docs" "$ROOT/notes" "$ROOT/src"

cat > "$ROOT/README.md" <<'EOF'
# Harbor

A small field guide, kept as plain files.

- `docs/` — the guide itself
- `notes/` — working notes, in no particular order
- `src/` — the two scripts that build it
EOF

cat > "$ROOT/docs/getting-started.md" <<'EOF'
# Getting started

Clone the repo, open it in your editor, and read `docs/tides.md` first.
Everything else assumes it.
EOF

cat > "$ROOT/docs/tides.md" <<'EOF'
# Tides

The tide table is regenerated every morning from the harbour feed.
EOF

cat > "$ROOT/docs/moorings.md" <<'EOF'
# Moorings

Twelve berths, numbered from the seaward end.
EOF

cat > "$ROOT/notes/lighthouse.md" <<'EOF'
# Lighthouse

Rebuilt in 1904. The lamp still turns on the original bearing.
EOF

cat > "$ROOT/notes/weather.md" <<'EOF'
# Weather

Onshore in the afternoon, most of the year.
EOF

cat > "$ROOT/src/build_guide.py" <<'EOF'
"""Assemble the guide from the markdown under docs/."""

from pathlib import Path


def chapters(root: Path) -> list[Path]:
    return sorted((root / "docs").glob("*.md"))
EOF

cat > "$ROOT/src/fetch_tides.py" <<'EOF'
"""Pull today's tide table from the harbour feed."""

FEED = "https://example.invalid/tides.json"
EOF

git -C "$ROOT" init -q
git -C "$ROOT" config user.email "clip@example.invalid"
git -C "$ROOT" config user.name "Clip"
git -C "$ROOT" add -A
git -C "$ROOT" commit -qm "Harbor: the guide, the notes and the two scripts"

# Uncommitted work, so the review card counts real hunks rather than nothing.
cat >> "$ROOT/docs/tides.md" <<'EOF'

The feed drops out around slack water; the table falls back to the
previous day's curve when it does.
EOF
cat >> "$ROOT/notes/lighthouse.md" <<'EOF'

The keeper's cottage is a workshop now.
EOF
cat > "$ROOT/notes/berths.md" <<'EOF'
# Berths

Berth 7 is the only one that takes a keel over two metres.
EOF

echo "$ROOT"
