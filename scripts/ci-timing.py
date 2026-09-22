#!/usr/bin/env python3
"""
Where does a CI run's time actually go?

Answers "what should we fix to make CI faster" with numbers instead of
guesses, by reading green runs of .github/workflows/ci.yml off the GitHub
API and the nextest output buried in their logs.

Three levels, because the answer lives at a different one each time:

  jobs     Wall time per job, and which job is the critical path. The run
           is only as fast as its slowest job; everything else is noise.
  suites   Per test binary: how many tests, how much time, mean and max.
           Separates integration binaries from lib unit tests, which
           usually differ by an order of magnitude.
  drill    Inside the hottest binary, by module, then by file. A 6000s
           test binary is never uniformly slow -- it is a handful of
           files, or one pathological test.

Every duration here is WALL CLOCK. nextest reports each test's elapsed
time, so a test that sleeps or blocks on a socket costs exactly as much as
one that pegs a core -- and several of the slowest tests in this repo are
the former. Summing per-test wall time gives test-seconds of occupancy,
NOT CPU time; do not read the totals as CPU load.

That distinction is the whole point of the concurrency line in the report:
sum(per-test wall) / run wall = the parallel slots nextest actually kept
busy. At `-j=N` the run cannot finish faster than either sum/N or the
single slowest test, whichever is larger, so the report prints both. When
sum/N dominates, shard or delete tests; when one test dominates, only that
test matters.

Usage:
    scripts/ci-timing.py                      # newest green run on the default branch
    scripts/ci-timing.py --runs 3             # average the job table over 3 runs
    scripts/ci-timing.py --run 35352416129    # one specific run
    scripts/ci-timing.py --os windows-latest  # drill into a different platform
    scripts/ci-timing.py --log saved.log      # parse a log already on disk, no network
    scripts/ci-timing.py --json out.json      # machine-readable, for trend tracking

Needs the `gh` CLI, authenticated (`gh auth login`). Logs are a few MB per
job; --log lets you re-analyze without re-downloading.
"""

import argparse
import collections
import json
import re
import subprocess
import sys
from datetime import datetime

WORKFLOW = "ci.yml"

# nextest, ANSI stripped, one line per test:
#   "2026-09-18T13:51:27Z    PASS [   0.007s] (   1/9918) fresh-core action::tests::name"
# The bracketed time is that test's elapsed wall clock. The counter is
# optional: it is absent in non-interactive runs of older nextest.
TEST_RE = re.compile(
    r"^(?:\S+Z\s+)?\s*(?:PASS|SLOW|TRY \d+ PASS)\s+\[\s*([\d.]+)s\]\s*"
    r"(?:\(\s*\d+/\d+\)\s*)?(\S+)\s+(\S+)"
)
# "Summary [1727.541s] 9918 tests run: 9918 passed (5 slow), 163 skipped"
SUMMARY_RE = re.compile(
    r"Summary\s+\[\s*([\d.]+)s\]\s+(\d+) tests run: (\d+) passed"
    r"(?:\s*\((\d+) slow\))?(?:, (\d+) skipped)?"
)
# "Starting 9918 tests across 41 binaries (163 tests skipped)"
STARTING_RE = re.compile(r"Starting (\d+) tests across (\d+) binaries")
# "Finished `test` profile [unoptimized] target(s) in 1m 55s"
FINISHED_RE = re.compile(r"Finished `\w+` profile .* in (?:(\d+)m )?([\d.]+)s")
ANSI_RE = re.compile(r"\x1b\[[0-9;]*m")

# nextest's default; the CI workflow passes -j=4 explicitly.
DEFAULT_JOBS = 4


def sh(*args: str) -> str:
    """Run a command, return stdout, die with its stderr on failure."""
    p = subprocess.run(args, capture_output=True, text=True)
    if p.returncode != 0:
        sys.exit(f"{args[0]} failed: {' '.join(args)}\n{p.stderr.strip()}")
    return p.stdout


def gh_json(*args: str):
    return json.loads(sh("gh", *args))


def repo_slug(explicit: str | None) -> str:
    if explicit:
        return explicit
    return gh_json("repo", "view", "--json", "nameWithOwner")["nameWithOwner"]


def green_runs(repo: str, branch: str | None, limit: int) -> list[dict]:
    args = ["run", "list", "-R", repo, "--workflow", WORKFLOW,
            "--status", "success", "--limit", str(limit),
            "--json", "databaseId,headBranch,headSha,createdAt,updatedAt"]
    if branch:
        args += ["--branch", branch]
    runs = gh_json(*args)
    if not runs:
        where = f" on branch {branch}" if branch else ""
        sys.exit(f"no green {WORKFLOW} runs found{where}")
    return runs


def jobs_for(repo: str, run_id: int) -> list[dict]:
    """Every job of a run, paged by hand.

    `gh api --paginate` emits one JSON object per page for this endpoint --
    concatenated, so json.loads chokes the moment a run has over 100 jobs.
    """
    jobs: list[dict] = []
    page = 1
    while True:
        got = gh_json("api", f"repos/{repo}/actions/runs/{run_id}"
                             f"/jobs?per_page=100&page={page}")
        batch = got.get("jobs", [])
        jobs += batch
        if len(jobs) >= got.get("total_count", 0) or not batch:
            return jobs
        page += 1


def job_log(repo: str, job_id: int) -> str:
    return sh("gh", "api", f"repos/{repo}/actions/jobs/{job_id}/logs")


def ts(s: str | None) -> datetime | None:
    if not s:
        return None
    return datetime.strptime(s, "%Y-%m-%dT%H:%M:%SZ")


def secs(a: str | None, b: str | None) -> float:
    """Elapsed seconds between two API timestamps, 0 if either is missing."""
    x, y = ts(a), ts(b)
    return (y - x).total_seconds() if x and y else 0.0


def dur(s: float) -> str:
    """Seconds as the compact form the report uses: 2m 05s, or 47s."""
    if s < 60:
        return f"{s:.0f}s"
    return f"{int(s // 60)}m {int(s % 60):02d}s"


def step_seconds(job: dict, prefix: str) -> float:
    """Wall time of the steps that ran and whose name starts with `prefix`.

    Matched loosely on purpose: the Linux step is spelled "Run tests (Linux
    — headless with Xvfb + lavapipe)", and only one of the two ever runs.
    """
    total = 0.0
    for st in job.get("steps") or []:
        name = st.get("name") or ""
        if name.startswith(prefix) and st.get("conclusion") != "skipped":
            total += secs(st.get("started_at"), st.get("completed_at"))
    return total


class Suite:
    """One test binary's wall-clock profile."""

    def __init__(self, name: str):
        self.name = name
        self.durations: list[float] = []

    def add(self, d: float):
        self.durations.append(d)

    @property
    def total(self) -> float:
        return sum(self.durations)

    @property
    def count(self) -> int:
        return len(self.durations)

    @property
    def mean(self) -> float:
        return self.total / self.count if self.count else 0.0

    @property
    def slowest(self) -> float:
        return max(self.durations, default=0.0)


def classify(suite: str) -> str:
    """Integration binary, or unit tests compiled into a lib/bin target?

    nextest names a lib's own tests by the crate alone ("fresh-core"), a
    binary's by "crate::bin/name", and an integration test by its file
    ("fresh-editor::all_tests"). Only the last is a tests/ directory.
    """
    if "::" not in suite:
        return "unit (lib)"
    if suite.split("::", 1)[1].startswith("bin/"):
        return "unit (bin)"
    return "integration"


def parse_log(text: str) -> dict:
    """Pull every per-test wall time, plus nextest's own totals, from a log."""
    suites: dict[str, Suite] = {}
    tests: list[tuple[float, str, str]] = []
    summary = {}
    build_s = None
    for raw in text.splitlines():
        line = ANSI_RE.sub("", raw).rstrip("\r")
        m = TEST_RE.match(line)
        if m:
            d, suite, name = float(m.group(1)), m.group(2), m.group(3)
            suites.setdefault(suite, Suite(suite)).add(d)
            tests.append((d, suite, name))
            continue
        m = SUMMARY_RE.search(line)
        if m:
            # Update, don't replace: "Starting N tests across M binaries" is
            # printed before the run and "Summary [...]" after it, so assigning
            # a fresh dict here would drop the binary count every time.
            summary.update({
                "wall_s": float(m.group(1)),
                "tests": int(m.group(2)),
                "passed": int(m.group(3)),
                "slow": int(m.group(4) or 0),
                "skipped": int(m.group(5) or 0),
            })
            continue
        m = STARTING_RE.search(line)
        if m:
            summary.setdefault("binaries", int(m.group(2)))
            continue
        m = FINISHED_RE.search(line)
        if m and build_s is None:
            build_s = int(m.group(1) or 0) * 60 + float(m.group(2))
    return {
        "suites": suites,
        "tests": tests,
        "summary": summary,
        "build_s": build_s,
    }


def group_by(tests, suite_name: str, depth: int):
    """Bucket one suite's tests by the first `depth` segments of their path."""
    out: dict[str, Suite] = {}
    for d, suite, name in tests:
        if suite != suite_name:
            continue
        parts = name.split("::")
        key = "::".join(parts[:depth]) if len(parts) > depth else (
            parts[0] if parts else "(root)")
        out.setdefault(key, Suite(key)).add(d)
    return out


def group_under(tests, suite_name: str, prefix: str):
    """Bucket the tests under one module by their next path segment."""
    out: dict[str, Suite] = {}
    for d, suite, name in tests:
        if suite != suite_name:
            continue
        parts = name.split("::")
        if not parts or parts[0] != prefix:
            continue
        key = parts[1] if len(parts) > 1 else "(root)"
        out.setdefault(key, Suite(key)).add(d)
    return out


def table(rows: list[Suite], total: float, label: str, top: int):
    print(f"\n{label:<48}{'tests':>7}{'wall_s':>10}{'share':>8}"
          f"{'mean_ms':>9}{'max_s':>8}")
    print("-" * 90)
    ordered = sorted(rows, key=lambda s: s.total, reverse=True)
    for s in ordered[:top]:
        share = 100 * s.total / total if total else 0
        print(f"{s.name[:47]:<48}{s.count:>7}{s.total:>10.1f}{share:>7.1f}%"
              f"{1000 * s.mean:>9.0f}{s.slowest:>8.1f}")
    rest = ordered[top:]
    if rest:
        t = sum(s.total for s in rest)
        n = sum(s.count for s in rest)
        share = 100 * t / total if total else 0
        print(f"{f'... {len(rest)} more':<48}{n:>7}{t:>10.1f}{share:>7.1f}%")


def report_jobs(repo: str, runs: list[dict], top: int) -> dict:
    """Job wall times, averaged over the sampled runs."""
    agg: dict[str, list[float]] = collections.defaultdict(list)
    run_wall: list[float] = []
    per_run_jobs = {}
    for r in runs:
        js = jobs_for(repo, r["databaseId"])
        per_run_jobs[r["databaseId"]] = js
        for j in js:
            agg[j["name"]].append(secs(j.get("started_at"), j.get("completed_at")))
        if js:
            starts = [ts(j.get("started_at")) for j in js if j.get("started_at")]
            ends = [ts(j.get("completed_at")) for j in js if j.get("completed_at")]
            if starts and ends:
                run_wall.append((max(ends) - min(starts)).total_seconds())
    means = {k: sum(v) / len(v) for k, v in agg.items()}
    machine = sum(means.values())
    # Zero when every sampled job is missing `completed_at` -- which `--run`
    # reaches, since it bypasses the `--status success` filter and so can be
    # pointed at a run that is still going or was cancelled.
    share = (lambda m: 100 * m / machine) if machine else (lambda _m: 0.0)
    print(f"\n{'job':<40}{'wall':>12}{'share of machine-time':>24}")
    print("-" * 78)
    for name, m in sorted(means.items(), key=lambda kv: kv[1], reverse=True)[:top]:
        print(f"{name[:39]:<40}{dur(m):>12}{share(m):>23.1f}%")
    wall = sum(run_wall) / len(run_wall) if run_wall else 0
    print(f"\n  run wall clock      {dur(wall):>10}   (critical path: "
          f"{max(means, key=means.get) if means else 'n/a'})")
    print(f"  machine-time        {dur(machine):>10}   (all jobs added up)")
    return {"job_wall_s": means, "run_wall_s": wall, "machine_s": machine,
            "jobs_by_run": per_run_jobs}


def report_tests(parsed: dict, jobs: int, top: int, label: str) -> dict:
    suites = parsed["suites"]
    tests = parsed["tests"]
    summary = parsed["summary"]
    if not tests:
        print(f"\n!! no nextest results in the {label} log "
              "(did the job change, or was it a cache-only run?)")
        return {}
    occupancy = sum(s.total for s in suites.values())
    wall = summary.get("wall_s") or 0.0

    print(f"\n{'=' * 78}\n  TEST BREAKDOWN -- {label}\n{'=' * 78}")
    if summary:
        print(f"  {summary.get('tests', len(tests))} tests across "
              f"{summary.get('binaries', len(suites))} binaries, "
              f"{summary.get('skipped', 0)} skipped, "
              f"{summary.get('slow', 0)} flagged slow")
    if parsed["build_s"]:
        print(f"  compile  {dur(parsed['build_s'])}")
    print(f"  execute  {dur(wall)} wall clock")
    print(f"  occupancy {occupancy:.0f} test-seconds "
          f"(per-test wall clock, summed -- not CPU time)")

    if wall:
        print(f"\n  concurrency   {occupancy / wall:.2f} slots kept busy "
              f"(nextest -j={jobs})")
        slowest = max((d for d, _, _ in tests), default=0.0)
        floor = max(occupancy / jobs, slowest)
        gate = "one test" if slowest >= occupancy / jobs else "total volume"
        print(f"  floor at -j={jobs}  {dur(floor)}  "
              f"(max of sum/{jobs}={dur(occupancy / jobs)} and "
              f"slowest test={dur(slowest)}) -- gated by {gate}")

    table(list(suites.values()), occupancy, "suite (test binary)", top)

    kinds: dict[str, Suite] = {}
    for d, suite, _ in tests:
        k = classify(suite)
        kinds.setdefault(k, Suite(k)).add(d)
    table(list(kinds.values()), occupancy, "test type", 10)

    out = {
        "occupancy_s": occupancy,
        "execute_wall_s": wall,
        "build_s": parsed["build_s"],
        "summary": summary,
        "suites": {s.name: {"tests": s.count, "wall_s": round(s.total, 1),
                            "mean_ms": round(1000 * s.mean),
                            "max_s": round(s.slowest, 2)}
                   for s in suites.values()},
    }

    hottest = max(suites.values(), key=lambda s: s.total)
    if hottest.total > 0.5 * occupancy and "::" in hottest.name:
        mods = group_by(tests, hottest.name, 1)
        table(list(mods.values()), hottest.total,
              f"module in {hottest.name}", top)
        out["hottest_suite"] = hottest.name
        out["modules"] = {s.name: {"tests": s.count, "wall_s": round(s.total, 1)}
                          for s in mods.values()}
        if mods:
            hot_mod = max(mods.values(), key=lambda s: s.total)
            if hot_mod.count > 10:
                files = group_under(tests, hottest.name, hot_mod.name)
                table(list(files.values()), hot_mod.total,
                      f"file in {hottest.name} {hot_mod.name}::", top)
                out["hottest_module"] = hot_mod.name
                out["files"] = {s.name: {"tests": s.count,
                                         "wall_s": round(s.total, 1)}
                                for s in files.values()}

    print(f"\n  slowest {top} individual tests (wall clock)")
    print("-" * 90)
    for d, suite, name in sorted(tests, reverse=True)[:top]:
        print(f"  {d:>8.2f}s  {suite} {name[:60]}")
    out["slowest_tests"] = [{"wall_s": d, "suite": s, "test": n}
                            for d, s, n in sorted(tests, reverse=True)[:top]]
    return out


def positive_int(value: str) -> int:
    """`--jobs` is a divisor in the concurrency maths, so zero is not a number
    of jobs we can be given -- reject it here rather than divide by it."""
    n = int(value)
    if n < 1:
        raise argparse.ArgumentTypeError(f"must be at least 1, got {n}")
    return n


def main():
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--repo", help="owner/name (default: this checkout's)")
    ap.add_argument("--branch", help="only runs on this branch "
                                     "(default: whatever gh returns)")
    ap.add_argument("--run", type=int, action="append", metavar="ID",
                    help="analyze these run ids instead of the newest green")
    ap.add_argument("--runs", type=int, default=1, metavar="N",
                    help="sample the N newest green runs (default 1)")
    ap.add_argument("--os", default="ubuntu-latest",
                    help="which test job's log to drill into "
                         "(default ubuntu-latest; 'all' for every platform)")
    ap.add_argument("--log", action="append", metavar="FILE",
                    help="parse a saved log instead of downloading; "
                         "skips the API entirely")
    ap.add_argument("--jobs", type=positive_int, default=DEFAULT_JOBS,
                    metavar="N",
                    help=f"nextest -j the run used (default {DEFAULT_JOBS}), "
                         "for the concurrency maths")
    ap.add_argument("--top", type=int, default=20, help="rows per table")
    ap.add_argument("--json", metavar="FILE",
                    help="also write the numbers here, for trend tracking")
    args = ap.parse_args()

    out: dict = {}

    if args.log:
        for path in args.log:
            with open(path, errors="replace") as fh:
                parsed = parse_log(fh.read())
            out[path] = report_tests(parsed, args.jobs, args.top, path)
    else:
        repo = repo_slug(args.repo)
        if args.run:
            runs = [{"databaseId": r} for r in args.run]
        else:
            runs = green_runs(repo, args.branch, args.runs)
        ids = ", ".join(str(r["databaseId"]) for r in runs)
        print(f"{repo}: {WORKFLOW} runs {ids}")

        jobinfo = report_jobs(repo, runs, args.top)
        out["jobs"] = {k: round(v, 1) for k, v in jobinfo["job_wall_s"].items()}
        out["run_wall_s"] = round(jobinfo["run_wall_s"], 1)
        out["machine_s"] = round(jobinfo["machine_s"], 1)

        # Logs come from the newest sampled run only: parsing three platforms
        # across N runs is a lot of megabytes for a number that barely moves.
        newest = runs[0]["databaseId"]
        test_jobs = [j for j in jobinfo["jobs_by_run"][newest]
                     if j["name"].startswith("test ")]
        if args.os != "all":
            test_jobs = [j for j in test_jobs if j["name"].endswith(args.os)]
            if not test_jobs:
                sys.exit(f"no 'test {args.os}' job in run {newest}; "
                         "try --os all")
        out["tests"] = {}
        for j in test_jobs:
            log = job_log(repo, j["id"])
            parsed = parse_log(log)
            res = report_tests(parsed, args.jobs, args.top, j["name"])
            # The job also pays for checkout, toolchain and cache restore;
            # that gap is invisible in nextest's own numbers.
            run_step = step_seconds(j, "Run tests")
            job_wall = secs(j.get("started_at"), j.get("completed_at"))
            if run_step and res:
                res["run_step_s"] = run_step
                res["job_wall_s"] = job_wall
                print(f"\n  job wall {dur(job_wall)}: "
                      f"setup {dur(job_wall - run_step)} + "
                      f"test step {dur(run_step)}")
            out["tests"][j["name"]] = res

    if args.json:
        with open(args.json, "w") as fh:
            json.dump(out, fh, indent=2, default=str)
        print(f"\nwrote {args.json}")


if __name__ == "__main__":
    main()
