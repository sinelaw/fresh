/** The shared header-read pipeline and the env-override rule. */
import { envDirs, readHeaders } from "../lib/agent_scanner.ts";

let failures = 0;

function eq(actual: unknown, expected: unknown, name: string): void {
  const a = JSON.stringify(actual);
  const e = JSON.stringify(expected);
  if (a !== e) {
    console.log(`FAIL ${name}\n  got      ${a}\n  expected ${e}`);
    failures++;
  } else {
    console.log(`ok   ${name}`);
  }
}

/** A machine that answers reads from a table and records what it was asked.
 *  `short` makes it answer only the first `n` requests. */
function fakeMachine(
  files: Record<string, string | { error: string }>,
  env: Record<string, string> = {},
  short: number | null = null,
): FreshMachine & { asked: string[][] } {
  const asked: string[][] = [];
  return {
    id: 0,
    platform: "linux",
    home: "/home/u",
    label: "",
    asked,
    walkTree: () => Promise.resolve({ entries: [], truncated: false }),
    readFilePrefixes(requests) {
      asked.push(requests.map((r) => r.path));
      const answered = short === null ? requests : requests.slice(0, short);
      return Promise.resolve(
        answered.map((r) => {
          const f = files[r.path];
          if (f === undefined) return { path: r.path, error: "no such file" };
          if (typeof f === "string") return { path: r.path, text: f.slice(0, r.maxBytes) };
          return { path: r.path, error: f.error };
        }),
      );
    },
    run: () => Promise.resolve({ code: 1, stdout: "", stderr: "" }),
    env: (names) =>
      Promise.resolve(Object.fromEntries(names.filter((n) => n in env).map((n) => [n, env[n]]))),
    close: () => Promise.resolve(true),
  };
}

function entry(path: string, mtime: number): WalkTreeEntry {
  return { path, rel: path.slice(1), kind: "file", mtime, size: 0 };
}

// ── readHeaders ───────────────────────────────────────────────────

{
  const m = fakeMachine({ "/a": "A", "/b": "B", "/c": "C", "/d": "D" });
  const problems: string[] = [];
  const out = await readHeaders(
    m,
    [entry("/a", 10), entry("/b", 40), entry("/c", 30), entry("/d", 20)],
    { maxBytes: 8, limit: 2, problems },
  );
  eq(m.asked, [["/b", "/c"]], "the cap bounds the read itself, newest first");
  eq(out.map((h) => [h.entry.path, h.text]), [["/b", "B"], ["/c", "C"]], "the newest survive");
  eq(problems, ["reporting the newest 2 of 4"], "what was left out is said");
}

{
  const m = fakeMachine({ "/a": "A", "/b": { error: "permission denied" } });
  const problems: string[] = [];
  const out = await readHeaders(m, [entry("/a", 2), entry("/b", 1)], { maxBytes: 8, problems });
  eq(out.map((h) => h.text), ["A", null], "an unreadable file is a null text, not a missing row");
  eq(problems, ["/b: permission denied"], "and its reason is a problem line");
}

{
  // The batch comes back short.
  const m = fakeMachine({ "/a": "A", "/b": "B", "/c": "C" }, {}, 1);
  const problems: string[] = [];
  const out = await readHeaders(m, [entry("/a", 3), entry("/b", 2), entry("/c", 1)], {
    maxBytes: 8,
    problems,
  });
  eq(out.map((h) => [h.entry.path, h.text]), [["/a", "A"], ["/b", null], ["/c", null]],
    "a short batch is paired by path, so nothing lands on the wrong file");
  eq(problems, ["2 of 3 headers not read — the read was cut short"], "said once, not per file");
}

{
  const m = fakeMachine({});
  const problems: string[] = [];
  const out = await readHeaders(m, [], { maxBytes: 8, limit: 5, problems });
  eq(out, [], "nothing to read is nothing read");
  eq(m.asked, [], "and no round trip");
}

// ── envDirs ───────────────────────────────────────────────────────

{
  const m = fakeMachine({}, { A: "/abs", B: "rel/ative", C: "   ", D: "/other" });
  const problems: string[] = [];
  const dirs = await envDirs(m, ["A", "B", "C", "MISSING"], problems);
  eq(dirs, { A: "/abs" }, "only set, absolute values come back");
  eq(problems, ["B is set to a relative path (rel/ative); ignoring it"],
    "a relative value is rejected out loud; blank and unset are not");
}

{
  const m = fakeMachine({}, { A: "/abs" });
  const problems: string[] = [];
  eq(await envDirs(m, [], problems), {}, "no names asks nothing");
}

console.log(failures === 0 ? "\nAll agent_scanner pipeline tests passed." : `\n${failures} failed.`);
if (failures > 0) process.exit(1);
