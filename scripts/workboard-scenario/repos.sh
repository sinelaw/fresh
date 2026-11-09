#!/bin/bash
# Build the demo repos under $1: three git repos (each with a bare "origin"
# so ahead/behind can be measured) and one plain, non-git folder.
set -e
R=$1; mkdir -p "$R/remotes"
g() { git -c user.email=demo@example.com -c user.name=Demo "$@"; }
for r in api web docs; do
  mkdir -p "$R/$r"; cd "$R/$r"; git init -q -b main
  printf '# %s\n\nDemo repository.\n' "$r" > README.md
  mkdir -p src; printf 'pub fn %s() {}\n' "$r" > src/lib.rs
  g add . && g commit -qm "Initial $r"
  git init -q --bare "$R/remotes/$r.git"; git remote add origin "$R/remotes/$r.git"; git push -q -u origin main
done
mkdir -p "$R/notes"; printf 'todo: sort out the agents\n' > "$R/notes/todo.txt"
