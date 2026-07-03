#!/usr/bin/env bash
# Build monorepo + single-repo test fixtures for Fresh PR #2369 UX testing.
set -euo pipefail

ROOT="/tmp/claude-0/-home-user-fresh/35eceb0f-10c2-5295-a0dd-ca23e7355799/scratchpad/fixtures"
rm -rf "$ROOT"
mkdir -p "$ROOT"

export GIT_AUTHOR_NAME="Alice Dev"      GIT_AUTHOR_EMAIL="alice@example.com"
export GIT_COMMITTER_NAME="Alice Dev"   GIT_COMMITTER_EMAIL="alice@example.com"

mkrepo() { # $1 = dir
  mkdir -p "$1"; ( cd "$1"; git init -q -b main; )
}
commit() { ( cd "$1"; git add -A; git commit -q -m "$2"; ) }

########################################################################
# 1. MONOREPO workspace: root is NOT a git repo, has nested sub-repos.
########################################################################
MONO="$ROOT/monorepo"
mkdir -p "$MONO"

# --- project-a : depth 1, various statuses ---
mkrepo "$MONO/project-a"
printf 'clean file A\ncontent line 2\ncontent line 3\n' > "$MONO/project-a/clean_a.txt"
printf 'original line1\noriginal line2\noriginal line3\n' > "$MONO/project-a/modified_a.txt"
printf 'to be deleted\n' > "$MONO/project-a/deleted_a.txt"
commit "$MONO/project-a" "initial project-a"
# now dirty it up:
printf 'original line1\nCHANGED line2\noriginal line3\n' > "$MONO/project-a/modified_a.txt"   # modified (unstaged)
rm "$MONO/project-a/deleted_a.txt"                                                              # deleted
printf 'brand new untracked\n' > "$MONO/project-a/untracked_a.txt"                              # untracked
printf 'staged new\n' > "$MONO/project-a/staged_a.txt"; ( cd "$MONO/project-a"; git add staged_a.txt; ) # staged add

# --- project-a-extra : sibling with SHARED PREFIX (depth 1) ---
mkrepo "$MONO/project-a-extra"
printf 'extra clean\n' > "$MONO/project-a-extra/clean_e.txt"
printf 'e original\n' > "$MONO/project-a-extra/mod_e.txt"
commit "$MONO/project-a-extra" "initial extra"
printf 'e CHANGED\n' > "$MONO/project-a-extra/mod_e.txt"   # modified

# --- group/project-b : depth 2 ---
mkrepo "$MONO/group/project-b"
printf 'B file with several lines\nline2\nline3\nline4\n' > "$MONO/group/project-b/main_b.txt"
commit "$MONO/group/project-b" "initial project-b"
printf 'B file with several lines\nline2 EDITED\nline3\nline4\n' > "$MONO/group/project-b/main_b.txt"  # modified

# --- a/b/project-c : depth 3 (at the limit) ---
mkrepo "$MONO/a/b/project-c"
printf 'C content\n' > "$MONO/a/b/project-c/file_c.txt"
commit "$MONO/a/b/project-c" "initial project-c"
printf 'C content changed\n' > "$MONO/a/b/project-c/file_c.txt"  # modified

# --- deep/x/y/project-d : depth 4 (BEYOND limit, should NOT be discovered) ---
mkrepo "$MONO/deep/x/y/project-d"
printf 'D content\n' > "$MONO/deep/x/y/project-d/file_d.txt"
commit "$MONO/deep/x/y/project-d" "initial project-d"
printf 'D changed\n' > "$MONO/deep/x/y/project-d/file_d.txt"  # modified (should NOT show)

# --- node_modules/pkg : has .git, MUST be skipped ---
mkrepo "$MONO/node_modules/pkg"
printf 'should be ignored\n' > "$MONO/node_modules/pkg/index.js"
commit "$MONO/node_modules/pkg" "pkg"
printf 'changed but ignored\n' > "$MONO/node_modules/pkg/index.js"

# --- symlinked .git repo (monorepo layout where .git is a symlink) ---
# Real git dir stored elsewhere; sub-project points at it via a .git symlink.
mkdir -p "$ROOT/_gitdirs"
REALGIT="$ROOT/_gitdirs/proj-sym.git"
mkrepo "$MONO/project-sym"
# Move the real .git aside and replace with a symlink to simulate the monorepo case.
mv "$MONO/project-sym/.git" "$REALGIT"
ln -s "$REALGIT" "$MONO/project-sym/.git"
printf 'sym clean\n' > "$MONO/project-sym/file_s.txt"
printf 's original\n' > "$MONO/project-sym/mod_s.txt"
commit "$MONO/project-sym" "initial sym"
printf 's CHANGED\n' > "$MONO/project-sym/mod_s.txt"   # modified

# --- a merge-conflict repo for merge_conflict plugin testing ---
mkrepo "$MONO/project-conflict"
printf 'line1\nshared\nline3\n' > "$MONO/project-conflict/conf.txt"
commit "$MONO/project-conflict" "base"
( cd "$MONO/project-conflict"
  git checkout -q -b feature
  printf 'line1\nFEATURE change\nline3\n' > conf.txt; git commit -q -am "feature"
  git checkout -q main
  printf 'line1\nMAIN change\nline3\n' > conf.txt; git commit -q -am "main"
  git merge feature -q >/dev/null 2>&1 || true   # produce conflict markers
)

########################################################################
# 2. SINGLE-REPO workspace (regression): root IS a git repo.
########################################################################
SINGLE="$ROOT/singlerepo"
mkrepo "$SINGLE"
mkdir -p "$SINGLE/src"
printf 'fn main() {\n    println!("hi");\n}\n' > "$SINGLE/src/main.rs"
printf 'mod util;\n' > "$SINGLE/src/lib.rs"
printf '# Single Repo\nplain readme\n' > "$SINGLE/README.md"
commit "$SINGLE" "initial single"
printf 'fn main() {\n    println!("MODIFIED");\n}\n' > "$SINGLE/src/main.rs"  # modified
printf 'new file\n' > "$SINGLE/src/new.rs"                                    # untracked

echo "=== FIXTURE TREE (depth-limited) ==="
find "$ROOT/monorepo" -maxdepth 4 \( -name '.git' -prune -o -print \) | sort | sed "s#$ROOT/##"
echo
echo "=== monorepo root is a git repo? ==="
( cd "$MONO"; git rev-parse --show-toplevel 2>&1 | head -1 || echo "NOT A GIT REPO (expected)"; )
echo "=== project-sym .git is symlink? ==="
ls -l "$MONO/project-sym/.git" | sed "s#$ROOT/##"
echo "DONE"
