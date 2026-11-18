# Runs inside the copied project.
set -e
git init -q -b main
git config user.name "Docs"
git config user.email "docs@example.invalid"
git remote add origin git@github.com:acme/inventory-api.git
# Commit items.js without its input check, then put the check back, so the
# gutter and the dock have an uncommitted change to show.
cp src/routes/items.js /tmp/.docshot-items.$$
sed -i '26,28d' src/routes/items.js
git add -A
git commit -q -m "inventory-api 1.4.0"
mv /tmp/.docshot-items.$$ src/routes/items.js
# init.ts registers "Docs: Build Workspaces" (see shots/orchestrator.py).
# shoot.py has made the config dir already; find it beside or above us.
for d in ../home/.config/fresh ../../.config/fresh; do
  if [ -d "$d" ]; then cp "$(dirname "$0")/../../shots/orchestrator-init.ts" "$d/init.ts"; break; fi
done
