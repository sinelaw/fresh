#!/usr/bin/env bash
# Build the repo the fresh 0.5.2 reel is filmed in.
#
# Every scene of the reel opens this one small project, so the reel reads as
# one editor visited a dozen times rather than a dozen strangers. It has no
# project manifest, so it opens Trusted, and it is its own git repo, so the
# workspace is named for it rather than for whatever tree it was built inside.
set -euo pipefail

ROOT="${1:-$HOME/repos/fresh/target/clips/fresh-0.5.2-repo}"
rm -rf "$ROOT"
mkdir -p "$ROOT/src" "$ROOT/docs"

cat > "$ROOT/README.md" <<'EOF'
# Harbor

Tide tables and mooring notes for a small harbour, kept as plain files.
EOF

# The theme scene. Enough kinds of token -- keywords, types, strings, numbers,
# comments, a macro -- that a theme change repaints every colour on screen.
cat > "$ROOT/src/tides.rs" <<'EOF'
use std::collections::BTreeMap;

/// One high or low water, read off the harbour feed.
#[derive(Debug, Clone, PartialEq)]
pub struct Tide {
    pub at: u32,          // minutes past midnight
    pub height_cm: i32,
    pub kind: Kind,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Kind {
    High,
    Low,
}

const MAX_TIDES: usize = 4;

pub fn parse(line: &str) -> Option<Tide> {
    let (time, rest) = line.split_once(' ')?;
    let (h, m) = time.split_once(':')?;
    let at = h.parse::<u32>().ok()? * 60 + m.parse::<u32>().ok()?;
    let height_cm = rest.trim_end_matches("cm").parse().ok()?;
    let kind = if height_cm > 250 { Kind::High } else { Kind::Low };
    Some(Tide { at, height_cm, kind })
}

pub fn table(feed: &str) -> BTreeMap<u32, Tide> {
    let tides: BTreeMap<_, _> = feed
        .lines()
        .filter_map(parse)
        .map(|t| (t.at, t))
        .collect();
    assert!(tides.len() <= MAX_TIDES, "too many tides: {}", tides.len());
    tides
}
EOF

# The search scene: one word, three casings.
cat > "$ROOT/docs/moorings.md" <<'EOF'
# Moorings

## North quay

- Berths 1-6, visitors welcome
- TODO: repaint the berth numbers
- Water and power at every berth

## South quay

- Berths 7-12, residents only
- Todo: fix the lamp on berth 9
- Fuel pontoon at the end

## Anchorage

- Good holding in sand, 4-6m
- todo: chart the new wreck buoy
- Keep clear of the ferry lane
EOF

# The CJK scene: a tab after double-width text.
printf '# 潮汐表\n\n你好\tworld\n東京\ttides\n港口\tberths\n' > "$ROOT/docs/cjk.md"

git -C "$ROOT" init -q
git -C "$ROOT" config user.email "clip@example.invalid"
git -C "$ROOT" config user.name "Clip"
git -C "$ROOT" add -A
git -C "$ROOT" commit -qm "Harbor: tides, moorings and notes"
