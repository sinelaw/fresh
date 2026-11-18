# Runs inside the copied project: a slice of Fresh's own source, so the
# Scripting page's commands run against the files its outputs name.
set -e
REPO="$(cd "$(dirname "$0")/../../../.." && pwd)"
for f in crates/fresh-editor/src/server/command_access.rs \
         crates/fresh-editor/tests/terminal_restore_script_token.rs \
         crates/fresh-editor/src/app/terminal.rs \
         crates/fresh-editor/src/main.rs; do
  mkdir -p "$(dirname "$f")"; cp "$REPO/$f" "$f"
done
# The page's "jump to the line" example says src/main.rs.
ln -s crates/fresh-editor/src src
cp "$(dirname "$0")/../../shots/review.ts" review.ts
cp "$(dirname "$0")/../../shots/fresh-tour.json" .fresh-tour.json
git init -q -b main
git config user.name "Docs"; git config user.email "docs@example.invalid"
git remote add origin https://github.com/sinelaw/fresh.git
git add -A; git commit -q -m "Fresh source excerpt"
# A prompt that reads as "a shell inside the editor", and the init script.
printf 'PS1="fresh$ "\n' > "$DOCSHOT_HOME/.bashrc"
cp "$DOCSHOT_SHOTS/scripting-init.ts" "$DOCSHOT_CONFIG_DIR/init.ts"
