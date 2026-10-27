/**
 * Command lines for Live Grep's built-in search backends.
 *
 * Kept free of the editor so the argument lists can be unit-tested: each
 * builder takes the query and the search options and returns the argv to
 * spawn. Most of the work is translating `file_explorer.custom_ignore_patterns`
 * — `.gitignore` lines relative to the project root, the same lines the file
 * explorer and Quick Open match (`CustomIgnore` in
 * src/view/file_tree/ignore.rs) — into the closest exclude each tool has:
 *
 * - rg: `-g !<line>`. rg globs follow `.gitignore` rules and are rooted at
 *   the directory searched, so a line passes through unchanged.
 * - ag: `--ignore <line>`, the matcher ag uses for its own ignore files.
 * - git grep: `:(exclude,glob)` pathspecs (see `gitExcludePathspecs`).
 * - grep, ack: patterns on an entry's own name only (see `nameRule`); neither
 *   can match a path below the root.
 *
 * Negated lines (`!keep.log`) are left out everywhere: none of these tools can
 * re-include a path through an exclude flag. rg's `-g keep.log` is not a
 * re-include — any glob without `!` turns the search into a whitelist of
 * the files it matches. So a file a negation brings back stays out of Live
 * Grep, though the file explorer shows it.
 *
 * `.git` is excluded by every builder: it is VCS metadata, never a hit the
 * user wants, even with the "Ignored & hidden" scope on. Unless that scope is
 * on, so are the rest of `IGNORED_DIRS` — the directories Quick Open and
 * project search never enter. `.gitignore` usually covers them already, but
 * not outside a git repo (rg and ag only read `.gitignore` inside one) or
 * when it doesn't list them.
 */

/** `IGNORED_DIRS` in src/services/project_files.rs, less `.git`. */
export const IGNORED_DIRS: readonly string[] = [
  "node_modules",
  "target",
  "__pycache__",
  ".hg",
  ".svn",
  ".DS_Store",
];

/** The search options the builders read (a subset of Live Grep's `SearchOpts`). */
export interface GrepArgOpts {
  maxResults: number;
  includeIgnored?: boolean;
  wholeWord?: boolean;
  regex?: boolean;
  caseSensitive?: boolean;
  /** `.gitignore` lines, relative to the directory searched. */
  ignorePatterns?: readonly string[];
  /** Search every file as text, binary-looking ones too (rg, grep). */
  text?: boolean;
  /** Search only files whose name matches this glob (rg, grep). */
  include?: string;
  /** What to search instead of the working directory (rg, grep). */
  paths?: readonly string[];
}

/** One `.gitignore` line, taken apart the way `.gitignore` reads it. */
export interface IgnoreRule {
  /** The line with trailing blanks trimmed and a leading `!` removed, but
   *  otherwise as written (escapes, a leading or trailing `/`). */
  line: string;
  /** The line started with `!`: it re-includes what it matches. */
  negated: boolean;
  /** The pattern itself: escapes of a leading `!`/`#` resolved and a leading
   *  and trailing `/` removed. */
  glob: string;
  /** It matches from the root rather than at any depth: it had a leading `/`
   *  or a `/` in the middle. A leading `**` still matches at any depth. */
  anchored: boolean;
  /** A trailing `/`: it matches directories only. */
  dirOnly: boolean;
}

/** Parse `.gitignore` lines, dropping blank lines and `#` comments. Anything
 *  that is not a string is skipped, so a malformed config can't break search. */
export function parseIgnoreLines(lines: readonly unknown[]): IgnoreRule[] {
  const rules: IgnoreRule[] = [];
  for (const raw of lines) {
    if (typeof raw !== "string") continue;
    // Trailing blanks are dropped unless the last one is escaped (`\ `).
    let line = raw.endsWith("\\ ") ? raw : raw.trimEnd();
    if (line === "" || line.startsWith("#")) continue;
    let negated = false;
    if (line.startsWith("!")) {
      negated = true;
      line = line.slice(1);
    }
    let glob = line.startsWith("\\!") || line.startsWith("\\#") ? line.slice(1) : line;
    let dirOnly = false;
    if (glob.endsWith("/")) {
      dirOnly = true;
      glob = glob.slice(0, -1);
    }
    let anchored = glob.includes("/");
    if (glob.startsWith("/")) {
      anchored = true;
      glob = glob.slice(1);
    }
    if (glob === "") continue;
    rules.push({ line, negated, glob, anchored, dirOnly });
  }
  return rules;
}

/** Where `dir` sits below `root`, as a `/`-separated relative path: `""` for
 *  `root` itself, null when `dir` is not inside it. */
export function relativeUnder(root: string, dir: string): string | null {
  // Windows drive letters compare without case (`C:` is `c:`).
  const norm = (p: string) =>
    p
      .replace(/\\/g, "/")
      .replace(/\/+$/, "")
      .replace(/^[A-Za-z]:/, (d) => d.toLowerCase());
  const r = norm(root);
  const d = norm(dir);
  if (d === r) return "";
  return d.startsWith(r + "/") ? d.slice(r.length + 1) : null;
}

// ── rg ──────────────────────────────────────────────────────────

export function rgArgs(query: string, o: GrepArgOpts): string[] {
  const args = [
    "--line-number",
    "--column",
    "--no-heading",
    "--color=never",
    o.caseSensitive ? "--case-sensitive" : "--ignore-case",
    `--max-count=${o.maxResults}`,
    "-g", "!.git",
  ];
  if (o.regex === false) args.push("--fixed-strings");
  if (o.wholeWord) args.push("--word-regexp");
  // Search ignored *and* hidden files (dotfiles). `.git` stays excluded via
  // the glob above.
  if (o.includeIgnored) args.push("--no-ignore", "--hidden");
  else for (const dir of IGNORED_DIRS) args.push("-g", `!${dir}`);
  for (const rule of parseIgnoreLines(o.ignorePatterns ?? [])) {
    if (!rule.negated) args.push("-g", `!${rule.line}`);
  }
  if (o.text) args.push("--text");
  if (o.include) args.push("-g", o.include);
  args.push("--", query, ...(o.paths ?? []));
  return args;
}

// ── ag ──────────────────────────────────────────────────────────

export function agArgs(query: string, o: GrepArgOpts): string[] {
  const args = [
    "--column",
    "--numbers",
    "--nogroup",
    "--nocolor",
    o.caseSensitive ? "--case-sensitive" : "--ignore-case",
    "--ignore", ".git",
  ];
  if (o.regex === false) args.push("--literal");
  if (o.wholeWord) args.push("--word-regexp");
  if (o.includeIgnored) args.push("--skip-vcs-ignores", "--hidden");
  else for (const dir of IGNORED_DIRS) args.push("--ignore", dir);
  for (const rule of parseIgnoreLines(o.ignorePatterns ?? [])) {
    if (rule.negated) continue;
    // ag has no `\!`/`\#` escape; the line otherwise goes through as written,
    // and ag reads a leading `/` (rooted) and trailing `/` (directory) itself.
    const escaped = rule.line.startsWith("\\!") || rule.line.startsWith("\\#");
    args.push("--ignore", escaped ? rule.line.slice(1) : rule.line);
  }
  args.push("--", query);
  return args;
}

// ── git grep ────────────────────────────────────────────────────

/**
 * `:(exclude,glob)` pathspecs for `rules`, for a `git grep` run in a
 * directory `prefix` below the project root the rules are relative to (`""`
 * when it runs in the root, null when it runs outside it).
 *
 * `glob` magic gives wildmatch the `.gitignore` meaning of `*` (one path
 * component) and `**` (any number). A pattern without a `/` matches at any
 * depth, so it gets a `**` prefix. A pathspec with a wildcard only matches the
 * path itself, not what is under it, so each pattern also excludes
 * `<pattern>/**` — the contents of a directory it names. A directory-only
 * pattern excludes just that.
 *
 * A rooted pattern is rebased onto `prefix` when its leading components spell
 * it out literally; one that points outside `prefix`, or starts with a
 * wildcard and so can't be rebased, is left out. When the run is outside the
 * root only the any-depth patterns apply.
 */
export function gitExcludePathspecs(rules: readonly IgnoreRule[], prefix: string | null): string[] {
  const specs: string[] = [];
  for (const rule of rules) {
    if (rule.negated) continue;
    let glob = rule.glob;
    if (!rule.anchored) {
      glob = `**/${glob}`;
    } else if (!glob.startsWith("**/") && prefix !== "") {
      if (prefix === null) continue;
      if (prefix === glob || prefix.startsWith(glob + "/")) {
        // The pattern ignores the directory the search runs in.
        specs.push(":(exclude,glob)**");
        continue;
      }
      if (!glob.startsWith(prefix + "/")) continue;
      glob = glob.slice(prefix.length + 1);
    }
    if (!rule.dirOnly) specs.push(`:(exclude,glob)${glob}`);
    specs.push(`:(exclude,glob)${glob}/**`);
  }
  return specs;
}

export function gitGrepArgs(query: string, o: GrepArgOpts, prefix: string | null): string[] {
  // `core.quotePath=false` prints non-ASCII names as they are rather than as
  // quoted octal escapes, which would not open. `--untracked` searches new
  // files too, as the other tools and Quick Open do; `.gitignore` still
  // applies unless the "Ignored & hidden" scope is on.
  const args = ["-c", "core.quotePath=false", "grep", "-n", "--column", "-I", "--untracked"];
  if (!o.caseSensitive) args.push("-i");
  // Default git-grep is basic regex; use extended when regex is on, or
  // fixed-strings when it's off so the query is matched literally.
  args.push(o.regex === false ? "-F" : "-E");
  if (o.wholeWord) args.push("-w");
  // Stop honouring the standard ignore files so `.gitignore`d content is
  // searched too.
  if (o.includeIgnored) args.push("--no-exclude-standard");
  args.push("-e", query);
  // Exclude-only pathspecs search what no pathspec would (git 2.13+).
  const specs = gitExcludePathspecs(parseIgnoreLines(o.ignorePatterns ?? []), prefix);
  if (!o.includeIgnored) {
    for (const dir of IGNORED_DIRS) specs.push(`:(exclude,glob)**/${dir}`, `:(exclude,glob)**/${dir}/**`);
  }
  if (specs.length > 0) args.push("--", ...specs);
  return args;
}

// ── grep / ack: name patterns ───────────────────────────────────

/** A rule reduced to a glob on an entry's own name, for tools that never see
 *  the path. */
export interface NameRule {
  glob: string;
  dirOnly: boolean;
}

/**
 * `rule` as a pattern on a file or directory name, or null when it has none.
 * A leading `**` is dropped (it already means "at any depth"), and a trailing
 * `/**` makes it a directory pattern. What is left must be one path
 * component: `src/gen` can't be expressed, so it is left out. A rooted single
 * component (`/build`, `build/**`) is applied at every depth, which also
 * leaves out nested directories of that name — the nearest these tools come.
 */
export function nameRule(rule: IgnoreRule): NameRule | null {
  if (rule.negated) return null;
  let glob = rule.glob;
  let dirOnly = rule.dirOnly;
  while (glob.startsWith("**/")) glob = glob.slice(3);
  if (glob.endsWith("/**")) {
    glob = glob.slice(0, -3);
    dirOnly = true;
  }
  if (glob === "" || glob === "**" || glob.includes("/")) return null;
  return { glob, dirOnly };
}

/** Anchored regex source for a name glob (`*`, `?`, `[...]`, `\` escapes).
 *  Valid as both a JS and a Perl regex. */
export function nameGlobRegexSource(glob: string): string {
  const escape = (c: string) => c.replace(/[.*+?^${}()|[\]\\\/#\s-]/g, "\\$&");
  let re = "";
  for (let i = 0; i < glob.length; i++) {
    const c = glob[i];
    if (c === "*") re += ".*";
    else if (c === "?") re += ".";
    else if (c === "\\" && i + 1 < glob.length) re += escape(glob[++i]);
    else if (c === "[") {
      let start = i + 1;
      if (glob[start] === "!" || glob[start] === "^") start++;
      if (glob[start] === "]") start++;
      const end = glob.indexOf("]", start);
      if (end < 0) {
        re += "\\[";
        continue;
      }
      let body = glob.slice(i + 1, end);
      if (body.startsWith("!")) body = "^" + body.slice(1);
      re += `[${body}]`;
      i = end;
    } else re += escape(c);
  }
  return `^${re}$`;
}

const hasWildcard = (glob: string) => /[*?[\\]/.test(glob);

export function grepArgs(query: string, o: GrepArgOpts): string[] {
  const args = ["-rn", o.text ? "-a" : "-I", "--exclude-dir=.git"];
  if (!o.includeIgnored) {
    // Plain grep can't read `.gitignore`, so these build/vendor trees would
    // otherwise bury real hits and make this last-resort fallback crawl.
    for (const dir of IGNORED_DIRS) args.push(`--exclude-dir=${dir}`);
  }
  if (!o.caseSensitive) args.push("-i");
  args.push(o.regex === false ? "-F" : "-E");
  if (o.wholeWord) args.push("-w");
  for (const rule of parseIgnoreLines(o.ignorePatterns ?? [])) {
    const name = nameRule(rule);
    if (!name) continue;
    if (!name.dirOnly) args.push(`--exclude=${name.glob}`);
    // grep also applies `--exclude-dir` to the `.` it is started on, so a
    // glob that matches `.` (`.*`, `*`) would skip the whole tree. Those
    // only leave out files.
    if (!new RegExp(nameGlobRegexSource(name.glob)).test(".")) {
      args.push(`--exclude-dir=${name.glob}`);
    }
  }
  if (o.include) args.push(`--include=${o.include}`);
  args.push("--", query, ...(o.paths ?? ["."]));
  return args;
}

export function ackArgs(query: string, o: GrepArgOpts): string[] {
  const args = ["--nocolor", "--column"];
  if (!o.includeIgnored) for (const dir of IGNORED_DIRS) args.push(`--ignore-dir=${dir}`);
  if (!o.caseSensitive) args.push("--ignore-case");
  if (o.regex === false) args.push("--literal");
  if (o.wholeWord) args.push("--word-regexp");
  for (const rule of parseIgnoreLines(o.ignorePatterns ?? [])) {
    const name = nameRule(rule);
    if (!name) continue;
    if (!name.dirOnly) {
      args.push(
        hasWildcard(name.glob)
          ? `--ignore-file=match:/${nameGlobRegexSource(name.glob)}/`
          : `--ignore-file=is:${name.glob}`,
      );
    }
    // `--ignore-dir` takes a plain directory name; a wildcard has no form
    // every ack accepts, so ack still descends into directories it matches.
    if (!hasWildcard(name.glob)) args.push(`--ignore-dir=${name.glob}`);
  }
  args.push("--", query);
  return args;
}
