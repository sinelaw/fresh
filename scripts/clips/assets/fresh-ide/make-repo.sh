#!/usr/bin/env bash
# Materialise the demo project the fresh-ide clip opens.
#
# A real cargo project rather than a lone file: the clip shows a coding agent
# running in fresh's embedded terminal beside the buffer, and an agent pointed
# at a directory with no manifest and no git history has nothing to say about
# it. It is also a git repo, because the Orchestrator's unit of work is a
# worktree.
#
# Lines are kept under MAXCOL columns and the script checks it. The clip splits
# the view, so the editor gets about half of its 152 cells; a longer line is a
# line the viewer sees cut off.
#
#   ./make-repo.sh /path/to/repo
set -euo pipefail
REPO="${1:?usage: make-repo.sh <dir>}"
MAXCOL=68
rm -rf "$REPO"
mkdir -p "$REPO/src"

cat > "$REPO/Cargo.toml" <<'EOF'
[package]
name = "ratelimit"
version = "0.2.0"
edition = "2021"

[dependencies]
EOF

cat > "$REPO/src/main.rs" <<'EOF'
//! A token-bucket rate limiter, one bucket per client key.

use std::collections::HashMap;
use std::time::{Duration, Instant};

mod bucket;

use bucket::Bucket;

/// Refill rate and burst ceiling, shared by every bucket.
#[derive(Debug, Clone, Copy)]
pub struct Limits {
    pub per_second: f64,
    pub burst: f64,
}

impl Default for Limits {
    fn default() -> Self {
        Self { per_second: 10.0, burst: 20.0 }
    }
}

/// Tracks one bucket per key, refilling lazily on read.
pub struct Limiter {
    limits: Limits,
    buckets: HashMap<String, Bucket>,
}

impl Limiter {
    pub fn new(limits: Limits) -> Self {
        Self { limits, buckets: HashMap::new() }
    }

    /// Take one token for `key`. False when the bucket is empty.
    pub fn allow(&mut self, key: &str) -> bool {
        let now = Instant::now();
        let limits = self.limits;
        self.buckets
            .entry(key.to_string())
            .or_insert_with(|| Bucket::full(limits.burst, now))
            .take(limits, now)
    }

    /// Drop buckets untouched for `idle`, so keys cannot leak.
    pub fn sweep(&mut self, idle: Duration) {
        let now = Instant::now();
        self.buckets.retain(|_, b| now - b.last() < idle);
    }
}

fn main() {
    let mut limiter = Limiter::new(Limits::default());
    let mut allowed = 0;
    for _ in 0..64 {
        if limiter.allow("demo-client") {
            allowed += 1;
        }
    }
    println!("allowed {allowed} of 64");
    limiter.sweep(Duration::from_secs(300));
}
EOF

cat > "$REPO/src/bucket.rs" <<'EOF'
use std::time::Instant;

use crate::Limits;

/// One client's tokens, refilled from the clock, not a timer.
#[derive(Debug)]
pub struct Bucket {
    tokens: f64,
    last: Instant,
}

impl Bucket {
    pub fn full(burst: f64, now: Instant) -> Self {
        Self { tokens: burst, last: now }
    }

    pub fn last(&self) -> Instant {
        self.last
    }

    /// Refill for elapsed time, then spend a token if any.
    pub fn take(&mut self, limits: Limits, now: Instant) -> bool {
        let elapsed = now.duration_since(self.last).as_secs_f64();
        self.tokens = (self.tokens + elapsed * limits.per_second)
            .min(limits.burst);
        self.last = now;
        if self.tokens >= 1.0 {
            self.tokens -= 1.0;
            true
        } else {
            false
        }
    }
}
EOF

cat > "$REPO/README.md" <<'EOF'
# ratelimit

A token-bucket rate limiter. One bucket per client key, refilled
from the clock instead of a timer, swept when a key goes idle.
EOF

# Every line has to fit the split editor pane, so this is checked rather
# than trusted -- a line over the limit is one the clip shows cut off.
if long=$(grep -rn ".\{$((MAXCOL + 1)),\}" "$REPO" 2>/dev/null); then
  echo "make-repo.sh: lines over $MAXCOL columns:" >&2
  echo "$long" >&2
  exit 1
fi

git -C "$REPO" init -q
git -C "$REPO" add -A
git -C "$REPO" -c user.name=fresh -c user.email=fresh@example.com \
  commit -qm "Token bucket rate limiter"
echo "$REPO"
