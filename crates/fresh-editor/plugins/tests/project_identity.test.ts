/** The project-identity ladder's parsers. Each fails quietly, so they are pinned here. */
import {
  ancestorsOf,
  canonicalRepoUrl,
  commonDirOf,
  gitConfigRemoteUrl,
  gitDirPointer,
  headBranch,
  parentDir,
} from "../lib/project_identity.ts";

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

// ── parentDir / ancestorsOf ───────────────────────────────────────

eq(parentDir("/home/user/proj", "/"), "/home/user", "parent of a nested path");
eq(parentDir("/home", "/"), "/", "parent of a top-level path is the root");
eq(parentDir("/", "/"), null, "the root has no parent");
eq(parentDir("relative", "/"), null, "a bare name has no parent");
eq(parentDir("C:\\src\\proj", "\\"), "C:\\src", "windows nested path");
eq(parentDir("C:\\src", "\\"), "C:\\", "windows drive root");
eq(parentDir("\\\\server\\share", "\\"), null, "a UNC share is a root");

eq(
  ancestorsOf("/home/user/proj/src", "/"),
  ["/home/user/proj/src", "/home/user/proj", "/home/user", "/home", "/"],
  "ancestors run to the root, nearest first",
);
eq(ancestorsOf("/a/b/c", "/", 2), ["/a/b/c", "/a/b"], "the depth cap holds");
eq(ancestorsOf("/home/user/", "/"), ["/home/user", "/home", "/"], "a trailing slash is dropped");

// ── gitConfigRemoteUrl ────────────────────────────────────────────

const CONFIG = `[core]
\trepositoryformatversion = 0
\tbare = false
[remote "origin"]
\turl = https://github.com/sinelaw/fresh
\tfetch = +refs/heads/*:refs/remotes/origin/*
[branch "main"]
\tremote = origin
`;
eq(gitConfigRemoteUrl(CONFIG), "https://github.com/sinelaw/fresh", "origin's url");

eq(
  gitConfigRemoteUrl(`[remote "upstream"]\n\turl = git@example.com:a/b.git\n`),
  "git@example.com:a/b.git",
  "the only remote is used when it is not called origin",
);

eq(
  gitConfigRemoteUrl(
    `[remote "fork"]\n\turl = https://h/x/y\n[remote "origin"]\n\turl = https://h/o/p\n`,
  ),
  "https://h/o/p",
  "origin wins over a remote declared earlier",
);

// A rewrite stanza carries `url` outside any remote.
eq(
  gitConfigRemoteUrl(`[url "git@github.com:"]\n\tinsteadOf = https://github.com/\n`),
  null,
  "a url rewrite stanza is not a remote",
);

eq(gitConfigRemoteUrl(`[core]\n\tbare = false\n`), null, "a config with no remote");

// ── canonicalRepoUrl ──────────────────────────────────────────────

const SAME = "github.com/sinelaw/fresh";
for (
  const url of [
    "https://github.com/sinelaw/fresh.git",
    "https://github.com/sinelaw/fresh",
    "http://github.com/sinelaw/fresh/",
    "git@github.com:sinelaw/fresh.git",
    "ssh://git@github.com:22/sinelaw/fresh",
    "git://github.com/sinelaw/fresh.git",
    "https://token:x-oauth-basic@github.com/sinelaw/fresh.git",
    "https://GitHub.com/SineLaw/Fresh.git",
  ]
) {
  eq(canonicalRepoUrl(url)?.id, SAME, `canonical: ${url}`);
}
eq(canonicalRepoUrl("git@github.com:sinelaw/fresh.git")?.label, "sinelaw/fresh", "hosted label is owner/name");

eq(canonicalRepoUrl("/srv/git/thing.git")?.id, "srv/git/thing", "a bare path remote");
eq(canonicalRepoUrl("file:///srv/git/thing.git")?.id, "srv/git/thing", "file:// matches the bare path");
eq(canonicalRepoUrl("/srv/git/thing.git")?.label, "thing", "a path remote's label is its own name");
// Case is kept without a host: on Linux these are two directories.
eq(canonicalRepoUrl("/srv/git/Thing.git")?.id, "srv/git/Thing", "a path remote keeps its case");

eq(canonicalRepoUrl("../sibling.git"), null, "a relative remote identifies nothing");
// A single-letter authority is a drive, not a host, and the drive stays in the id.
eq(canonicalRepoUrl("C:\\src\\repo.git")?.id, "C:/src/repo", "a windows path remote keeps its drive");
eq(canonicalRepoUrl(""), null, "an empty url");

// ── gitDirPointer / commonDirOf ───────────────────────────────────

eq(
  gitDirPointer("gitdir: /home/user/fresh/.git/worktrees/agent-a61f75\n"),
  "/home/user/fresh/.git/worktrees/agent-a61f75",
  "the pointer a linked worktree leaves",
);
eq(gitDirPointer("[core]\n"), null, "an ordinary file is not a pointer");

eq(
  commonDirOf("/home/user/fresh/.git/worktrees/agent-a61f75", "/"),
  "/home/user/fresh/.git",
  "the shared dir is two levels above a worktree's own",
);
eq(commonDirOf("/home/user/fresh/.git", "/"), "/home/user/fresh/.git", "a plain gitdir is used as-is");

// ── headBranch ────────────────────────────────────────────────────

eq(headBranch("ref: refs/heads/main\n"), "main", "a branch");
eq(headBranch("ref: refs/heads/claude/serene-heisenberg-u4cdl7\n"), "claude/serene-heisenberg-u4cdl7", "a slashed branch");
eq(headBranch("4a1c9b2f3e5d6a7b8c9d0e1f2a3b4c5d6e7f8a9b\n"), "4a1c9b2", "a detached head reports its commit");
eq(headBranch("ref: refs/tags/v1\n"), undefined, "HEAD pointing outside refs/heads");
eq(headBranch(""), undefined, "an empty HEAD");

console.log(failures === 0 ? "\nAll project_identity tests passed." : `\n${failures} failed.`);
if (failures > 0) process.exit(1);
