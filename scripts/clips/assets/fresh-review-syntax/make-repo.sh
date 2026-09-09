#!/usr/bin/env bash
# Materialise the demo repo the fresh-review-syntax clip reviews.
#
# Review Diff shows the working tree against HEAD, so the repo is built in two
# passes: commit the "before" files, then overwrite them with the "after" ones.
# Rebuild it from scratch any time; nothing here is precious.
#
# Every line is kept to MAXCOL columns, and the script checks it: the clip
# captures 64 cells, the diff gutter takes twelve of them and the fold column
# and scrollbar two more, so a longer line is a line the viewer sees cut off.
#
#   ./make-repo.sh /path/to/repo
set -euo pipefail
REPO="${1:?usage: make-repo.sh <dir>}"
MAXCOL=49
rm -rf "$REPO"
mkdir -p "$REPO/src" "$REPO/web" "$REPO/scripts"

# ---------------------------------------------------------------- before ----
cat > "$REPO/src/limiter.rs" <<'EOF'
use std::collections::HashMap;
use std::time::{Duration, Instant};

/// One token bucket per client key.
#[derive(Debug, Clone)]
pub struct Bucket {
    tokens: f64,
    last: Instant,
}

impl Bucket {
    fn new(tokens: f64, last: Instant) -> Self {
        Self { tokens, last }
    }
}

pub struct RateLimiter {
    buckets: HashMap<String, Bucket>,
    rate: f64,
    burst: f64,
}

impl RateLimiter {
    /// Take a token, if one is there to take.
    pub fn allow(&mut self, k: &str) -> bool {
        let b = self.fill(k);
        if b.tokens >= 1.0 {
            b.tokens -= 1.0;
            return true;
        }
        false
    }

    fn fill(&mut self, k: &str) -> &mut Bucket {
        let burst = self.burst;
        let rate = self.rate;
        let at = Instant::now();
        let b = self.buckets.entry(k.to_owned())
            .or_insert(Bucket::new(burst, at));
        let dt = at - b.last;
        b.tokens += dt.as_secs_f64() * rate;
        b.tokens = b.tokens.min(burst);
        b.last = at;
        b
    }
}
EOF

cat > "$REPO/web/dashboard.ts" <<'EOF'
export interface Sample {
  at: number;
  allowed: number;
  denied: number;
}

const WINDOW_MS = 60_000;

const sum = (rows: Sample[], k: keyof Sample) =>
  rows.reduce((n, s) => n + (s[k] ?? 0), 0);

export function summary(rows: Sample[]): string {
  const cut = Date.now() - WINDOW_MS;
  const live = rows.filter((s) => s.at >= cut);
  const ok = sum(live, "allowed");
  const no = sum(live, "denied");
  return `${ok} allowed, ${no} denied`;
}
EOF

cat > "$REPO/scripts/replay.py" <<'EOF'
"""Replay a request log through the limiter."""

import json
import sys
from dataclasses import dataclass


@dataclass
class Request:
    key: str
    at: float


def load(path: str) -> list[Request]:
    with open(path) as fh:
        rows = json.load(fh)
    return [Request(**r) for r in rows]


def main() -> int:
    for req in load(sys.argv[1]):
        print(f"{req.at:>10.3f} {req.key}")
    return 0
EOF

git -C "$REPO" init -q
git -C "$REPO" config user.email "demo@example.com"
git -C "$REPO" config user.name "Demo"
git -C "$REPO" add -A
git -C "$REPO" commit -qm "Token-bucket rate limiter"

# ----------------------------------------------------------------- after ----
cat > "$REPO/src/limiter.rs" <<'EOF'
use std::collections::HashMap;
use std::time::{Duration, Instant};

/// One token bucket per client key.
#[derive(Debug, Clone)]
pub struct Bucket {
    tokens: f64,
    last: Instant,
}

impl Bucket {
    fn new(tokens: f64, last: Instant) -> Self {
        Self { tokens, last }
    }
}

pub struct RateLimiter {
    buckets: HashMap<String, Bucket>,
    rate: f64,
    burst: f64,
}

/// What a caller is told when the bucket is dry.
#[derive(Debug, PartialEq)]
pub enum Verdict {
    Allow,
    Deny { retry_after: Duration },
}

impl RateLimiter {
    /// Take a token, or say when one turns up.
    pub fn allow(&mut self, k: &str) -> Verdict {
        let rate = self.rate;
        let b = self.fill(k);
        if b.tokens >= 1.0 {
            b.tokens -= 1.0;
            return Verdict::Allow;
        }
        let short_by = 1.0 - b.tokens;
        Verdict::Deny {
            retry_after: Duration::from_secs_f64(
                short_by / rate,
            ),
        }
    }

    fn fill(&mut self, k: &str) -> &mut Bucket {
        let burst = self.burst;
        let rate = self.rate;
        let at = Instant::now();
        let b = self.buckets.entry(k.to_owned())
            .or_insert(Bucket::new(burst, at));
        let dt = at - b.last;
        b.tokens += dt.as_secs_f64() * rate;
        b.tokens = b.tokens.min(burst);
        b.last = at;
        b
    }

    /// Drop buckets left full and idle.
    pub fn sweep(&mut self, idle: Duration) {
        let burst = self.burst;
        let now = Instant::now();
        self.buckets.retain(|_, b| {
            let idled = now - b.last;
            b.tokens < burst || idled < idle
        });
    }
}
EOF

cat > "$REPO/web/dashboard.ts" <<'EOF'
export interface Sample {
  at: number;
  allowed: number;
  denied: number;
  retryMs?: number;
}

const WINDOW_MS = 60_000;

const sum = (rows: Sample[], k: keyof Sample) =>
  rows.reduce((n, s) => n + (s[k] ?? 0), 0);

const backoff = (s: Sample) => s.retryMs ?? 0;

export function summary(rows: Sample[]): string {
  const cut = Date.now() - WINDOW_MS;
  const live = rows.filter((s) => s.at >= cut);
  const ok = sum(live, "allowed");
  const no = sum(live, "denied");
  if (no === 0) return `${ok} allowed, 0 denied`;
  const ms = Math.max(0, ...live.map(backoff));
  return `${ok} ok, ${no} denied, wait ${ms}ms`;
}
EOF

cat > "$REPO/scripts/replay.py" <<'EOF'
"""Replay a request log through the limiter.

Rows are read as JSON so one log replays against
a capture taken from production, where the clock
is the only thing that differs.
"""

import json
import sys
from dataclasses import dataclass


@dataclass
class Request:
    key: str
    at: float
    weight: int = 1


def load(path: str) -> list[Request]:
    with open(path) as fh:
        rows = json.load(fh)
    return [Request(**r) for r in rows]


def main() -> int:
    for req in load(sys.argv[1]):
        w = req.weight
        print(f"{req.at:>10.3f} {req.key} x{w}")
    return 0
EOF

over=$(awk -v m="$MAXCOL" 'length>m {print FILENAME":"FNR" is "length" columns"}' \
  "$REPO/src/limiter.rs" "$REPO/web/dashboard.ts" "$REPO/scripts/replay.py")
if [[ -n "$over" ]]; then
  echo "lines over $MAXCOL columns:" >&2; echo "$over" >&2; exit 1
fi
echo "repo ready: $REPO"
