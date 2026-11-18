# Runs inside the copied project: commit it so the tours point at a clean tree.
set -e
git init -q -b main
git config user.name "Docs"
git config user.email "docs@example.invalid"
git add -A
git commit -q -m "Initial invoicing service"
