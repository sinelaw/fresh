/** The catalog's path rules. A rule that stops matching reports "not installed". */
import {
  AGENT_STORES,
  fileRuleAccepts,
  hasExtension,
  sessionIdFor,
} from "../lib/agent_catalog.ts";

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

// ── file rules ────────────────────────────────────────────────────

eq(fileRuleAccepts({ kind: "any" }, "a/b.jsonl"), true, "any accepts anything");
eq(fileRuleAccepts({ kind: "base-name", value: "summary.json" }, "s/summary.json"), true,
   "base-name matches the file itself");
eq(fileRuleAccepts({ kind: "base-name", value: "summary.json" }, "summary.json/x.json"), false,
   "base-name does not match a directory of that name");
eq(fileRuleAccepts({ kind: "base-name-prefix", value: "session_" }, "d/session_1.json"), true,
   "base-name-prefix matches");
eq(fileRuleAccepts({ kind: "base-name-prefix", value: "session_" }, "session_1/other.json"), false,
   "base-name-prefix looks at the file, not its parent");

eq(fileRuleAccepts({ kind: "path-contains", value: "sessions" }, "a/sessions/b.jsonl"), true,
   "path-contains matches a whole component");
eq(fileRuleAccepts({ kind: "path-contains", value: "sessions" }, "a/sessions-old/b.jsonl"), false,
   "path-contains does not match a longer component");

// Kimi: state.json under session_*, not the sibling wire logs.
eq(fileRuleAccepts({ kind: "kimi-state" }, "session_a/state.json"), true, "kimi state accepted");
eq(fileRuleAccepts({ kind: "kimi-state" }, "session_a/agents/x/wire.jsonl"), false,
   "kimi ignores the sibling wire log");
eq(fileRuleAccepts({ kind: "kimi-state" }, "other/state.json"), false,
   "kimi requires the session_ parent");

// ── ids ───────────────────────────────────────────────────────────

eq(sessionIdFor("file-stem", "a/b/abc123.jsonl"), "abc123", "file-stem drops the extension");
eq(sessionIdFor("parent-dir-name", "a/sess-1/metadata.json"), "sess-1",
   "parent-dir-name uses the directory");
eq(sessionIdFor("file-stem", "a/.session"), ".session",
   "a dotfile is all name, with no extension to strip");
eq(sessionIdFor("parent-dir-name", "metadata.json"), "metadata.json",
   "parent-dir-name with no parent falls back to the file");

// ── extensions ────────────────────────────────────────────────────

eq(hasExtension([".jsonl"], "a/b.JSONL"), true, "extensions match case-insensitively");
eq(hasExtension([".json", ".jsonl"], "a/b.txt"), false, "a non-matching extension is rejected");
eq(hasExtension([], "anything"), true, "no extensions means no filter");

// ── the table itself ──────────────────────────────────────────────

eq(new Set(AGENT_STORES.map((s) => s.id)).size, AGENT_STORES.length,
   "every store id is unique, so no two scanners collide");
eq(AGENT_STORES.every((s) => s.baseSegments.length > 0 || s.baseEnv.length > 0), true,
   "every store can locate itself from $HOME or a variable");
eq(AGENT_STORES.every((s) => s.maxDepth > 0), true,
   "every store bounds its walk, so a mis-set variable cannot crawl the disk");

process.exit(failures === 0 ? 0 : 1);
