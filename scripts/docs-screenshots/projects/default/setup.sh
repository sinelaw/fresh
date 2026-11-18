# Runs inside the copied project: give it a history and a few local edits,
# so git-aware UI (gutter, explorer status, Review Diff, Git Log) has
# something to show.
set -e
git init -q -b main
git config user.name "Dana Reyes"
git config user.email "dana@example.invalid"

commit() {  # commit <date> <message> <paths...>
    local when="$1" msg="$2"; shift 2
    git add -- "$@"
    GIT_AUTHOR_DATE="$when" GIT_COMMITTER_DATE="$when" git commit -q -m "$msg"
}

# items.js starts without input validation; the working copy adds it back.
cp web/routes/items.js /tmp/.items.js.$$
python3 - <<'PY'
from pathlib import Path
p = Path("web/routes/items.js")
s = p.read_text()
s = s.replace('''  if (!warehouse || !Number.isInteger(delta)) {
    return res.status(400).json({ error: "warehouse and integer delta required" });
  }
''', "")
p.write_text(s)
PY

commit "2026-09-02T10:14:00" "Initial invoicing service" \
    README.md pyproject.toml .gitignore invoicing/__init__.py invoicing/models.py
commit "2026-09-04T16:40:00" "Store invoices as JSON files" invoicing/store.py
commit "2026-09-09T09:05:00" "Add a command-line interface" invoicing/cli.py
commit "2026-09-15T14:22:00" "Serve items and invoices over HTTP" web
commit "2026-09-21T11:30:00" "Render invoices as plain text" invoicing/render.py
commit "2026-10-01T17:48:00" "Report outstanding invoices" invoicing/report.py
commit "2026-10-06T08:57:00" "Add tests for models, invoices and reports" tests
commit "2026-10-14T13:12:00" "Split staging and production config" config docs

mv /tmp/.items.js.$$ web/routes/items.js

# Uncommitted: a modified model, a new untracked file, ignored build output.
python3 - <<'PY'
from pathlib import Path
p = Path("invoicing/models.py")
s = p.read_text()
s = s.replace('    currency: str = "EUR"\n', '    currency: str = "EUR"\n    vat_number: str | None = None\n')
s = s.replace('    def tax(self, rate: Decimal = Decimal("0.20")) -> Decimal:', '    def tax(self, rate: Decimal = Decimal("0.21")) -> Decimal:')
p.write_text(s)
PY
mkdir -p web/db
printf 'export const pool = { query: async () => ({ rows: [] }) };\n' > web/db/pool.js
mkdir -p build/lib node_modules/express
printf 'compiled\n' > build/lib/invoicing.txt
printf '{}\n' > node_modules/express/package.json
