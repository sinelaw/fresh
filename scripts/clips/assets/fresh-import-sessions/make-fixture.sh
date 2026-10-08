#!/usr/bin/env bash
# Build the developer machine the Import sessions clip scans.
#
#   scripts/clips/assets/fresh-import-sessions/make-fixture.sh
#
# A home directory, $DEMO_HOME (default /home/dev), holding what a week of
# agent work leaves behind:
#
#   ~/src/payments-api                          main              one session
#   ~/src/payments-api.worktrees/idempotency    feat/idempotency  a worktree, one session
#   ~/src/web-dashboard                         chore/date-fns    one session, and a live
#                                                                 one in tmux (start-live.sh)
#   ~/src/dotfiles                              main              one session
#   ~/scratch/csv-dedupe                        (no repository)   one session
#
# The names are short on purpose. The dialog is a fixed share of the
# terminal's width, so the clip films a narrow terminal to get large type,
# and a session's title is its directory's basename: a long one is cut to
# "payments-api…" in the list the clip is about.
#
# The transcripts are real: every one is written by the claude CLI doing the
# task its prompt asks for, against the code below, with $HOME pointed at
# $DEMO_HOME so they land in $DEMO_HOME/.claude/projects exactly where Fresh's
# Claude Code scanner looks. Nothing is faked into the store by hand -- a
# hand-written transcript would show whatever its author thought Claude Code
# writes, and the scanner would be filmed reading a guess.
#
# Built once. The claude runs are the slow, metered part, and the code is
# what they left it as -- rebuilding the repos under kept transcripts would
# film sessions describing edits that are no longer there. So a complete
# fixture is left alone, and an incomplete one (or FRESH_CLIP_RERUN=1) is
# rebuilt whole. CLAUDE_MODEL picks the model (default: sonnet).
set -euo pipefail
DEMO_HOME="${DEMO_HOME:-/home/dev}"
MODEL="${CLAUDE_MODEL:-sonnet}"

command -v claude >/dev/null || { echo "make-fixture: the claude CLI is not on PATH" >&2; exit 1; }
mkdir -p "$DEMO_HOME" || { echo "make-fixture: cannot create $DEMO_HOME; set DEMO_HOME" >&2; exit 1; }

SRC="$DEMO_HOME/src"
MANIFEST="$DEMO_HOME/.clip-sessions"

SESSIONS=5
if [ "${FRESH_CLIP_RERUN:-0}" != 1 ] && [ -d "$DEMO_HOME/.clip-pristine" ] \
   && [ "$(wc -l < "$MANIFEST" 2>/dev/null || echo 0)" -ge "$SESSIONS" ]; then
  echo "fixture already built: $DEMO_HOME (FRESH_CLIP_RERUN=1 rebuilds it)"
  exit 0
fi

# Rebuilt from nothing: a worktree whose directory went away under git makes
# the next `worktree add` refuse, and a half-made store is a different machine.
rm -rf "$SRC" "$DEMO_HOME/scratch" "$DEMO_HOME/.claude" "$DEMO_HOME/.claude.json" \
  "$MANIFEST" "$DEMO_HOME/.clip-pristine"
mkdir -p "$SRC" "$DEMO_HOME/scratch"

export GIT_CONFIG_NOSYSTEM=1
export GIT_AUTHOR_NAME="Dana Reyes" GIT_AUTHOR_EMAIL="dana@acme.dev"
export GIT_COMMITTER_NAME="$GIT_AUTHOR_NAME" GIT_COMMITTER_EMAIL="$GIT_AUTHOR_EMAIL"

# commit DIR DAYS_AGO MESSAGE: dated history, so `git log` in a session reads
# like a project that has been worked on rather than one made a second ago.
commit() {
  local d="$1" ago="$2" msg="$3" when
  when="$(date -d "-$ago days" -R)"
  git -C "$d" add -A
  GIT_AUTHOR_DATE="$when" GIT_COMMITTER_DATE="$when" git -C "$d" commit -q -m "$msg"
}

new_repo() {
  local d="$1" remote="$2"
  mkdir -p "$d"
  git -C "$d" init -q -b main
  git -C "$d" config user.name "$GIT_AUTHOR_NAME"
  git -C "$d" config user.email "$GIT_AUTHOR_EMAIL"
  git -C "$d" remote add origin "$remote"
}

# ── payments-api: a small charges service with a flaky test ───────────

P="$SRC/payments-api"
new_repo "$P" "git@github.com:acme/payments-api.git"
mkdir -p "$P/app" "$P/tests"
cat > "$P/README.md" <<'EOF'
# payments-api

Charges and webhook delivery for the Acme checkout.

    python3 -m unittest discover -s tests
EOF
cat > "$P/app/__init__.py" <<'EOF'
EOF
cat > "$P/app/charges.py" <<'EOF'
"""Charges: create and look up card charges."""

import itertools
from dataclasses import dataclass, field
from decimal import Decimal

_ids = itertools.count(1)
_charges = {}

SUPPORTED = {"usd", "eur", "gbp", "jpy"}


@dataclass
class Charge:
    id: str
    customer_id: str
    amount: Decimal
    currency: str
    status: str = "pending"
    metadata: dict = field(default_factory=dict)


def create_charge(customer_id, amount, currency, metadata=None):
    currency = currency.lower()
    if currency not in SUPPORTED:
        raise ValueError(f"unsupported currency {currency!r}")
    # Amounts arrive as floats from the JSON layer.
    amount = Decimal(amount).quantize(Decimal("0.01"))
    if amount <= 0:
        raise ValueError("amount must be positive")
    charge = Charge(f"ch_{next(_ids):06d}", customer_id, amount, currency,
                    metadata=metadata or {})
    _charges[charge.id] = charge
    return charge


def get_charge(charge_id):
    return _charges.get(charge_id)


def post_charges(request):
    """POST /charges. `request` is a dict with `headers` and `json`."""
    body = request["json"]
    try:
        charge = create_charge(body["customer_id"], body["amount"],
                               body["currency"], body.get("metadata"))
    except (KeyError, ValueError) as e:
        return 400, {"error": str(e)}
    return 201, {"id": charge.id, "status": charge.status,
                 "amount": str(charge.amount), "currency": charge.currency}
EOF
cat > "$P/app/webhooks.py" <<'EOF'
"""Webhook delivery with exponential backoff."""

import random
import time

MAX_ATTEMPTS = 3
BASE_DELAY = 0.05


class DeliveryFailed(Exception):
    pass


def backoff(attempt):
    delay = BASE_DELAY * (2 ** attempt)
    return delay + random.uniform(0, delay)


def deliver(event, send):
    """Send `event`, retrying failures. Returns the attempt count."""
    for attempt in range(MAX_ATTEMPTS):
        try:
            send(event)
            return attempt + 1
        except ConnectionError:
            if attempt == MAX_ATTEMPTS - 1:
                raise DeliveryFailed(event["id"])
            time.sleep(backoff(attempt))
EOF
cat > "$P/tests/test_charges.py" <<'EOF'
import unittest

from app.charges import create_charge, post_charges


class ChargesTest(unittest.TestCase):
    def test_create(self):
        c = create_charge("cus_1", 12.5, "USD")
        self.assertEqual(c.currency, "usd")
        self.assertEqual(str(c.amount), "12.50")

    def test_rejects_unknown_currency(self):
        status, body = post_charges({"headers": {}, "json": {
            "customer_id": "cus_1", "amount": 5, "currency": "xyz"}})
        self.assertEqual(status, 400)


if __name__ == "__main__":
    unittest.main()
EOF
cat > "$P/tests/test_webhooks.py" <<'EOF'
import time
import unittest

from app.webhooks import deliver


class WebhookRetryTest(unittest.TestCase):
    def test_retry_backs_off(self):
        calls = []

        def flaky_send(event):
            calls.append(time.monotonic())
            if len(calls) < 4:
                raise ConnectionError("receiver down")

        started = time.monotonic()
        attempts = deliver({"id": "evt_1"}, flaky_send)
        self.assertEqual(attempts, 4)
        # Three backoffs: 0.05 + 0.1 + 0.2 = 0.35s, plus slack for jitter.
        self.assertLess(time.monotonic() - started, 0.6)


if __name__ == "__main__":
    unittest.main()
EOF
commit "$P" 9 "Charges endpoint and webhook delivery"
sed -i 's/MAX_ATTEMPTS = 3/MAX_ATTEMPTS = 5/' "$P/app/webhooks.py"
printf '\nWebhooks are retried up to five times with jittered exponential backoff.\n' >> "$P/README.md"
commit "$P" 4 "Retry webhooks up to five times"

# The idempotency work happens in its own worktree, the way an agent's
# branch usually does.
WT="$SRC/payments-api.worktrees/idempotency"
mkdir -p "$(dirname "$WT")"
git -C "$P" worktree add -q -b feat/idempotency "$WT"

# ── web-dashboard: a TypeScript app still on moment.js ────────────────

W="$SRC/web-dashboard"
new_repo "$W" "git@github.com:acme/web-dashboard.git"
mkdir -p "$W/src/utils" "$W/src/components"
cat > "$W/package.json" <<'EOF'
{
  "name": "web-dashboard",
  "private": true,
  "version": "0.14.2",
  "scripts": { "dev": "vite", "build": "tsc && vite build", "test": "vitest" },
  "dependencies": {
    "moment": "^2.29.4",
    "react": "^18.3.1",
    "react-dom": "^18.3.1",
    "recharts": "^2.12.7"
  },
  "devDependencies": { "typescript": "^5.5.4", "vite": "^5.4.2", "vitest": "^2.0.5" }
}
EOF
cat > "$W/src/utils/dates.ts" <<'EOF'
import moment from "moment";

/** "3 hours ago", "in 2 days". */
export function fromNow(iso: string): string {
  return moment(iso).fromNow();
}

/** Monday 00:00 of the week `iso` falls in, as an ISO string. */
export function startOfWeek(iso: string): string {
  return moment(iso).startOf("isoWeek").toISOString();
}

/** Axis label for the revenue chart: "Mar 4". */
export function axisLabel(iso: string): string {
  return moment(iso).format("MMM D");
}

/** Every day from `from` to `to`, inclusive, as YYYY-MM-DD. */
export function daysBetween(from: string, to: string): string[] {
  const out: string[] = [];
  const d = moment(from).startOf("day");
  const end = moment(to).startOf("day");
  while (d.isSameOrBefore(end)) {
    out.push(d.format("YYYY-MM-DD"));
    d.add(1, "day");
  }
  return out;
}
EOF
cat > "$W/src/components/RevenueChart.tsx" <<'EOF'
import { LineChart, Line, XAxis, YAxis, Tooltip } from "recharts";
import { axisLabel, daysBetween } from "../utils/dates";

export interface Point { day: string; cents: number }

export function RevenueChart({ points, from, to }: { points: Point[]; from: string; to: string }) {
  const byDay = new Map(points.map((p) => [p.day, p.cents]));
  const data = daysBetween(from, to).map((day) => ({ day, dollars: (byDay.get(day) ?? 0) / 100 }));
  return (
    <LineChart width={720} height={240} data={data}>
      <XAxis dataKey="day" tickFormatter={axisLabel} />
      <YAxis />
      <Tooltip />
      <Line type="monotone" dataKey="dollars" stroke="#7c3aed" dot={false} />
    </LineChart>
  );
}
EOF
commit "$W" 12 "Revenue chart"
git -C "$W" checkout -q -b chore/date-fns

# ── dotfiles: a slow shell ─────────────────────────────────────────────

D="$SRC/dotfiles"
new_repo "$D" "git@github.com:dreyes/dotfiles.git"
cat > "$D/.zshrc" <<'EOF'
export PATH="$HOME/.local/bin:$PATH"

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && . "$NVM_DIR/bash_completion"

# completions
autoload -Uz compinit
compinit

# homebrew
export PATH="$(brew --prefix)/bin:$PATH"
export PATH="$(brew --prefix python)/libexec/bin:$PATH"

# pyenv
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# prompt
autoload -Uz vcs_info
precmd() { vcs_info; RPROMPT="$(kubectl config current-context 2>/dev/null)" }
PROMPT='%F{cyan}%~%f ${vcs_info_msg_0_} %# '

alias gs='git status -sb'
alias k=kubectl
EOF
cat > "$D/.gitconfig" <<'EOF'
[user]
	name = Dana Reyes
	email = dana@acme.dev
[pull]
	rebase = true
EOF
commit "$D" 30 "zsh, git"

# ── scratch/csv-dedupe: a throwaway directory, no repository ───────────

C="$DEMO_HOME/scratch/csv-dedupe"
mkdir -p "$C"
cat > "$C/customers.csv" <<'EOF'
customer_id,name,email,signup_date,plan
1001,Ana Lima,ana.lima@example.com,2024-02-11,pro
1002,Ben Okafor,ben@okafor.io,2024-03-02,free
1003,ana lima,Ana.Lima@Example.com ,2024-06-19,team
1004,Chloe Martin,chloe.m@example.org,2024-04-27,free
1005,Ben Okafor,BEN@OKAFOR.IO,2023-12-30,pro
1006,Dev Patel,dev@patel.dev,2024-05-05,team
1007,Chloe Martin, chloe.m@example.org,2024-07-14,pro
1008,Eli Novak,eli.novak@example.com,2024-01-08,free
EOF

# ── the sessions ───────────────────────────────────────────────────────

# claude, as a developer at this machine would run it: from the project
# directory, with HOME at the demo home. Run from inside another Claude Code
# session (this script often is), the parent's session id and entrypoint are
# in the environment and the child would take them over, so drop them.
cc() {
  local dir="$1"; shift
  (cd "$dir" && env -u CLAUDECODE -u CLAUDE_CODE_SESSION_ID -u CLAUDE_CODE_ENTRYPOINT \
      -u CLAUDE_CODE_REMOTE_SESSION_ID -u CLAUDE_CODE_CHILD_SESSION \
      HOME="$DEMO_HOME" claude --model "$MODEL" --permission-mode acceptEdits \
      --allowedTools "Bash(python3:*)" "Bash(git:*)" "Bash(ls:*)" "Bash(cat:*)" \
      "$@" </dev/null >/dev/null)
}

# session NAME DIR PROMPT [FOLLOW-UP]: one transcript per NAME. The id is
# pinned up front so the manifest can name it.
session() {
  local name="$1" dir="$2" prompt="$3" follow="${4:-}" id
  id="$(python3 -c 'import uuid; print(uuid.uuid4())')"
  echo "  $name: running claude in ${dir#$DEMO_HOME/} ..."
  cc "$dir" --session-id "$id" -p "$prompt"
  [ -n "$follow" ] && cc "$dir" --resume "$id" -p "$follow"
  echo "$name $id $dir" >> "$MANIFEST"
}

echo "sessions in $DEMO_HOME/.claude/projects:"
session flaky-test "$P" \
  "tests/test_webhooks.py::test_retry_backs_off fails about one run in five on CI. Find out why it is flaky and fix it without making the suite slower. Run the tests with: python3 -m unittest discover -s tests" \
  "Run that test file ten times in a row to show it is stable now."
session idempotency "$WT" \
  "Add Idempotency-Key support to POST /charges in app/charges.py: the same key with the same body returns the original charge with a 200; the same key with a different body is a 422. Keep keys in memory for now. Add tests and run them with: python3 -m unittest discover -s tests"
session date-fns "$W" \
  "We are dropping moment.js. Port src/utils/dates.ts to date-fns, keeping every exported function's signature and output identical, and update package.json. There is no node_modules here, so do not try to install or run anything." \
  "Double-check startOfWeek: moment's isoWeek starts on Monday. Does your version match?"
session zsh-startup "$D" \
  "My zsh takes about two seconds to start. Read .zshrc and tell me what is slow and what you would change, ranked by how much time it saves. Do not edit anything yet."
session dedupe "$C" \
  "Write dedupe.py: collapse customers.csv to one row per email address, comparing emails case- and whitespace-insensitively and keeping the row with the latest signup_date. Write the result to customers.deduped.csv and run it."

# ── what an interactive claude needs to start without asking ───────────

# start-live.sh runs claude interactively in tmux. A home it has never seen
# opens on the first-run theme picker and a trust prompt per directory, so
# mark both as done.
python3 - "$DEMO_HOME" "$MANIFEST" <<'EOF'
import json, os, sys
home, manifest = sys.argv[1], sys.argv[2]
path = os.path.join(home, ".claude.json")
cfg = json.load(open(path)) if os.path.exists(path) else {}
cfg.update({"hasCompletedOnboarding": True, "theme": "dark"})
projects = cfg.setdefault("projects", {})
for line in open(manifest):
    _, _, d = line.rstrip("\n").split(" ", 2)
    projects.setdefault(d, {}).update(
        {"hasTrustDialogAccepted": True, "hasCompletedProjectOnboarding": True})
json.dump(cfg, open(path, "w"), indent=2)
EOF

# The store as the sessions left it. Every take starts from this copy
# (start-live.sh restores it): the live claude writes a transcript of its own
# and an imported session appends to the one it resumes, so without it the
# second take would scan a different machine from the first.
rm -rf "$DEMO_HOME/.clip-pristine"
cp -a "$DEMO_HOME/.claude/projects" "$DEMO_HOME/.clip-pristine"

echo "fixture ready: $DEMO_HOME"
