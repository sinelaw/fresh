/** Live Grep backend command lines and how each one leaves out
 *  `file_explorer.custom_ignore_patterns` (`lib/grep_args.ts`). */
import {
  ackArgs,
  agArgs,
  gitExcludePathspecs,
  gitGrepArgs,
  grepArgs,
  nameGlobRegexSource,
  nameRule,
  parseIgnoreLines,
  relativeUnder,
  rgArgs,
} from "../lib/grep_args.ts";

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

/** The arguments after `head`, up to the `--` that ends the options. */
function between(args: string[], head: number): string[] {
  return args.slice(head, args.indexOf("--"));
}

const base = { maxResults: 10 };

// ── Parsing ─────────────────────────────────────────────────────

eq(
  parseIgnoreLines(["", "   ", "# comment", "*.log  ", 7, null]).map((r) => r.line),
  ["*.log"],
  "blank lines, comments, trailing blanks and non-strings are dropped",
);
eq(
  parseIgnoreLines(["!keep.log"]).map((r) => [r.negated, r.glob]),
  [[true, "keep.log"]],
  "a leading ! negates",
);
eq(
  parseIgnoreLines(["\\!bang", "\\#hash"]).map((r) => [r.negated, r.glob]),
  [[false, "!bang"], [false, "#hash"]],
  "an escaped ! or # is part of the pattern",
);
eq(
  parseIgnoreLines(["build/", "/target", "src/gen", "*.log", "**/cache"]).map((r) => [
    r.glob,
    r.anchored,
    r.dirOnly,
  ]),
  [
    ["build", false, true],
    ["target", true, false],
    ["src/gen", true, false],
    ["*.log", false, false],
    ["**/cache", true, false],
  ],
  "a leading or middle / roots a pattern; a trailing / limits it to directories",
);

eq(relativeUnder("/p", "/p"), "", "the root is itself");
eq(relativeUnder("/p/", "/p/a/b"), "a/b", "a directory below the root");
eq(relativeUnder("/p", "/pq/a"), null, "a sibling sharing a prefix is outside");
eq(relativeUnder("C:\\p", "C:\\p\\a"), "a", "Windows separators");

// ── rg ──────────────────────────────────────────────────────────

const rgHead = 8; // fixed flags up to and including `-g !.git`
eq(rgArgs("q", base).slice(-2), ["--", "q"], "rg: the query follows --");
eq(rgArgs("q", base).slice(rgHead - 2, rgHead), ["-g", "!.git"], "rg: .git is always left out");
eq(
  between(rgArgs("q", base), rgHead),
  [],
  "rg: no excludes of its own beyond .git (it honours .gitignore)",
);
eq(
  between(rgArgs("q", { ...base, ignorePatterns: ["build/", "/target", "*.log", "!keep.log", "# c"] }), rgHead),
  ["-g", "!build/", "-g", "!/target", "-g", "!*.log"],
  "rg: each line passes through as an exclude glob; a negation is left out (it would whitelist)",
);
eq(
  between(rgArgs("q", { ...base, includeIgnored: true }), rgHead),
  ["--no-ignore", "--hidden"],
  "rg: the Ignored scope searches ignored and hidden files",
);

// ── ag ──────────────────────────────────────────────────────────

eq(
  between(agArgs("q", { ...base, ignorePatterns: ["build/", "/target", "\\#x", "!keep"] }), 5),
  ["--ignore", ".git", "--ignore", "build/", "--ignore", "/target", "--ignore", "#x"],
  "ag: .git, then each line as --ignore",
);
eq(
  between(agArgs("q", { ...base, includeIgnored: true }), 7),
  ["--skip-vcs-ignores", "--hidden"],
  "ag: the Ignored scope searches ignored and hidden files",
);

// ── git grep ────────────────────────────────────────────────────

const rules = (...lines: string[]) => parseIgnoreLines(lines);
eq(
  gitExcludePathspecs(rules("*.log", "build/", "/target", "src/gen", "**/cache", "!keep"), ""),
  [
    ":(exclude,glob)**/*.log",
    ":(exclude,glob)**/*.log/**",
    ":(exclude,glob)**/build/**",
    ":(exclude,glob)target",
    ":(exclude,glob)target/**",
    ":(exclude,glob)src/gen",
    ":(exclude,glob)src/gen/**",
    ":(exclude,glob)**/cache",
    ":(exclude,glob)**/cache/**",
  ],
  "git: any-depth patterns get **/, directories get /**, negations are left out",
);
eq(
  gitExcludePathspecs(rules("src/gen", "lib/x", "*/a", "*.log"), "src"),
  [":(exclude,glob)gen", ":(exclude,glob)gen/**", ":(exclude,glob)**/*.log", ":(exclude,glob)**/*.log/**"],
  "git: in a subdirectory, rooted patterns are rebased or left out",
);
eq(
  gitExcludePathspecs(rules("/src", "src/a/b"), "src/a"),
  [":(exclude,glob)**", ":(exclude,glob)b", ":(exclude,glob)b/**"],
  "git: a pattern covering the directory searched excludes everything",
);
eq(
  gitExcludePathspecs(rules("/target", "*.log"), null),
  [":(exclude,glob)**/*.log", ":(exclude,glob)**/*.log/**"],
  "git: outside the root only any-depth patterns apply",
);
eq(gitGrepArgs("q", base, "").slice(-2), ["-e", "q"], "git: no pathspecs without patterns");
eq(
  gitGrepArgs("q", { ...base, ignorePatterns: ["build/"] }, "").slice(-4),
  ["-e", "q", "--", ":(exclude,glob)**/build/**"],
  "git: pathspecs follow the query after --",
);

// ── grep / ack ──────────────────────────────────────────────────

eq(
  rules("**/cache", "build/**", "/target", "src/gen", "!keep").map(nameRule),
  [{ glob: "cache", dirOnly: false }, { glob: "build", dirOnly: true }, { glob: "target", dirOnly: false }, null, null],
  "name rules: one component or none",
);
eq(new RegExp(nameGlobRegexSource("*.min.js")).test("a.min.js"), true, "glob regex: * and literal dots");
eq(new RegExp(nameGlobRegexSource("*.min.js")).test("aXminXjs"), false, "glob regex: dots are literal");
eq(new RegExp(nameGlobRegexSource("[!a]?.c")).test("bx.c"), true, "glob regex: [!...] and ?");
eq(new RegExp(nameGlobRegexSource("[!a]?.c")).test("ax.c"), false, "glob regex: [!...] excludes");

eq(
  between(grepArgs("q", base), 0),
  ["-rn", "-I", "--exclude-dir=.git", "--exclude-dir=node_modules", "--exclude-dir=target", "-i", "-E"],
  "grep: stands in for .gitignore with node_modules and target",
);
eq(
  between(grepArgs("q", { ...base, includeIgnored: true }), 0),
  ["-rn", "-I", "--exclude-dir=.git", "-i", "-E"],
  "grep: the Ignored scope drops the stand-ins but keeps .git",
);
eq(
  between(grepArgs("q", { ...base, ignorePatterns: ["*.log", "build/", "src/gen", ".*"] }), 7),
  ["--exclude=*.log", "--exclude-dir=*.log", "--exclude-dir=build", "--exclude=.*"],
  "grep: name globs only; a glob matching . never becomes --exclude-dir",
);
eq(grepArgs("q", base).slice(-3), ["--", "q", "."], "grep: searches .");

eq(
  between(ackArgs("q", { ...base, ignorePatterns: ["*.log", "vendor", "build/", "gen*/", "src/x"] }), 3),
  [
    "--ignore-file=match:/^.*\\.log$/",
    "--ignore-file=is:vendor",
    "--ignore-dir=vendor",
    "--ignore-dir=build",
  ],
  "ack: files by name or regex, plain directory names only",
);

if (failures > 0) {
  console.log(`${failures} failure(s)`);
  process.exit(1);
}
