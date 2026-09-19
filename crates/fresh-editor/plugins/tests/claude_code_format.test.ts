/** Claude Code's bucket encoding, and reading past the opening record. */
import { encodeProjectDir } from "../lib/claude_code_format.ts";
import { jsonRecords, pickStringFrom } from "../lib/agent_scanner.ts";

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

// Checked against a real ~/.claude/projects bucket.
eq(encodeProjectDir("/home/user/fresh"), "-home-user-fresh", "matches a real bucket name");

eq(encodeProjectDir("/home/user/.claude"), "-home-user--claude", "a dot becomes its own dash");
eq(encodeProjectDir("/a//b"), "-a--b", "a doubled slash becomes two dashes");

eq(encodeProjectDir("/a/b/"), encodeProjectDir("/a/b"), "a trailing slash is ignored");
eq(encodeProjectDir("/"), "-", "the root keeps its one slash");

eq(encodeProjectDir("C:\\Users\\x"), "C--Users-x", "a windows path encodes");

// The transform is lossy, so the cwd is read from the transcript, not decoded.
eq(
  encodeProjectDir("/home/a-b") === encodeProjectDir("/home/a/b"),
  true,
  "different paths can share a bucket, so the name cannot be decoded back",
);

// A real opening record: sessionId present, cwd absent.
const REAL_HEAD = [
  '{"type":"queue-operation","operation":"enqueue","timestamp":"2026-09-16T05:54:01.939Z","sessionId":"3cc1b4b4-ea2d-5dac-9a45-aa08481b6b8f","content":"create a sub-crate"}',
  '{"type":"user","sessionId":"3cc1b4b4-ea2d-5dac-9a45-aa08481b6b8f","cwd":"/home/user/fresh","gitBranch":"claude/serene-heisenberg-u4cdl7","version":"2.1.273"}',
].join("\n");

const records = jsonRecords(REAL_HEAD);
eq(records.length, 2, "both records parse");
eq(pickStringFrom(records, ["cwd"]), "/home/user/fresh", "the cwd is found in a later record");
eq(pickStringFrom(records, ["gitBranch"]), "claude/serene-heisenberg-u4cdl7", "so is the branch");
eq(
  pickStringFrom([records[0]], ["cwd"]),
  undefined,
  "and the opening record alone genuinely has no cwd — the read-forward is load-bearing",
);

eq(
  jsonRecords('{"cwd":"/a"}\n{"cwd":"/b"' ).length,
  1,
  "a partial trailing record is skipped, not fatal",
);

process.exit(failures === 0 ? 0 : 1);
