"""Orchestrator, Workboard and Scripting shots.

Workspaces are staged by `orchestrator-init.ts` (copied in as the user's
init.ts by projects/inventory-api/setup.sh), with fake agents: the scripts
below, put on PATH, stand in for coding agents. Each prints what a real
agent's terminal would and then does the one thing the Orchestrator reads
its state from — asks a question, keeps printing, or edits a file and goes
quiet.
"""
import os
import pathlib
import sys
import tempfile

sys.path.insert(0, str(pathlib.Path(__file__).parent))
from _common import palette  # noqa: E402

REPO = pathlib.Path(__file__).resolve().parents[3]
AGENTS = pathlib.Path(tempfile.gettempdir()) / "docshot-fake-agents"

FAKE_AGENTS = {
    # Edits a file, then asks: "Needs you".
    "migrate": """
sleep 4; echo "Reading migrations/ ..."; sleep 1
echo "-- 0042_low_stock_threshold.sql" >> src/db/pool.js
echo "Found 3 pending migrations."
echo "Apply 3 pending migrations to staging? (y/n)"
sleep 100000
""",
    # Keeps printing: "Working".
    "bench": """
sleep 2; while true; do echo "GET /items  p95 $((40 + RANDOM % 20))ms"; sleep 0.5; done
""",
    # Stand-ins for Claude Code and Codex: they keep printing, so they read as
    # working. Also what makes the New Workspace dialog offer them.
    "claude": """
sleep 2; while true; do echo "Reading src/queue.rs ($RANDOM)"; sleep 0.6; done
""",
    "codex": """
sleep 2; while true; do echo "running cargo test drain ... attempt $RANDOM"; sleep 0.7; done
""",
    # Works, changes a file, says it is done and goes quiet: "Ready to review".
    "run-tests": """
sleep 2; for t in items stock adjust; do echo "test $t ... ok"; sleep 0.7; done
echo "// reviewed" >> src/routes/items.js
echo "Done. All 42 tests pass."
sleep 100000
""",
}


def _install_agents():
    AGENTS.mkdir(exist_ok=True)
    for name, body in FAKE_AGENTS.items():
        f = AGENTS / name
        f.write_text("#!/bin/bash\n" + body.lstrip())
        f.chmod(0o755)
    path = os.environ.get("PATH", "")
    for d in (str(AGENTS), str(REPO / "target/release")):
        if d not in path.split(":"):
            path = d + ":" + path
    os.environ["PATH"] = path


_install_agents()

# live_sessions would list agent CLIs running on the machine taking the shots.
INV = {"project": "inventory-api", "dir": "inventory-api",
       "config": {"auto_attach": False, "plugins": {"live_sessions": {"settings": {"enabled": False}}}}}

# Wait for the command to be listed: init.ts registers it a moment after start.
build = [("key", "ctrl+p"), ("text", "Docs: Build Workspaces"), ("wait", "Stage the Orchestrator"),
         ("settle",), ("key", "Return"), ("wait", "adjust-validation"), ("sleep", 10), ("settle",)]

WIDE = {"cols": 140, "rows": 36}

FRESH = {"project": "fresh", "dir": "fresh",
         "config": {**INV["config"], "editor": {"line_wrap": False}}}

# The Scripting page's one-liners, verbatim.
DESCRIBE = "echo 'return editor.describeWorkspace().panes.map(p => p.name)' | fresh --cmd script run"
JUMP = ("echo 'const p = editor.describeWorkspace().panes[0]; "
        "return editor.openFileInSplit(p.splitId, \"src/main.rs\", 3502)' | fresh --cmd script run")
TOUR = "echo 'return editor.getPluginApi(\"code-tour\").openTour(\".fresh-tour.json\")' | fresh --cmd script run"


def sh(line):
    """Type a command into the focused terminal and run it."""
    return [("text", line + "\r")]


script_shell = [("key", "ctrl+p"), ("text", "Docs: Script Shell"), ("wait", "may script the editor"),
                ("settle",), ("key", "Return"), ("wait", "fresh$"), ("settle",)]

SHOTS = [
    {
        "id": "orchestrator-dock", **INV, **WIDE, "args": ["src/routes/items.js"],
        "steps": [("wait", "itemsRouter")] + build + [("shot", "orchestrator-dock")],
    },
    {
        "id": "orchestrator-new-workspace", **INV, **WIDE, "args": ["src/routes/items.js"],
        "steps": [("wait", "itemsRouter")] + build + palette("Orchestrator: New Workspace") + [
            ("sleep", 2), ("settle",), ("click_text", "Save as project"), ("settle",),
            ("click_text", "[   Add   ]"), ("settle",), ("click_text", "custom…"), ("settle",), ("click_text", "claude   "), ("settle",),
            ("click_text", "Describe the task"), ("settle",),
            ("text", "Add a GET /items/export.csv endpoint that streams every item with its stock "
                     "per warehouse. Include a header row and add tests."),
            ("settle",), ("shot", "orchestrator-new-workspace")],
    },
    {
        "id": "workboard", **INV, "cols": 150, "rows": 22, "args": ["src/routes/items.js"],
        "steps": [("wait", "itemsRouter")] + build + palette("Workboard: Open") + [
            ("wait", "Inbox"), ("wait", "fix/adjust-validation · 1 changed", 120), ("settle",),
            ("shot", "workboard-inbox"), ("key", "4"), ("settle",), ("shot", "workboard-board")],
    },
    {
        "id": "scripting", **FRESH, "cols": 150, "rows": 40, "args": ["crates/fresh-editor/src/server/command_access.rs"],
        "steps": [("wait", "Capability")] + script_shell
        + sh("fresh --cmd script run review.ts") + [("wait", '["command_access.rs"'), ("settle",),
                                                     ("shot", "scripting-layout-after")]
        + sh("clear") + sh(DESCRIBE) + [("wait", '"terminal"]'), ("settle",)]
        + sh(JUMP) + [("wait", "\ntrue"), ("settle",), ("shot", "scripting-cmd-run")]
        # openFileInSplit moved focus to the editor pane: click back into the terminal.
        + [("click_text", "fresh$", 2), ("settle",)] + sh("clear") + sh(TOUR) + [("wait", "Step 1 of 3"), ("settle",), ("key", "n"), ("settle",),
                                    ("shot", "scripting-tour-handoff")],
    },
    {
        "id": "scripting-run-agent-dialog", **FRESH, "cols": 140, "rows": 46, "args": ["crates/fresh-editor/src/main.rs"],
        "steps": [("wait", "use "), ("settle",)] + palette("Orchestrator: New Workspace") + [
            ("sleep", 3), ("settle",), ("click_text", "Save as project"), ("settle",),
            ("click_text", "[   Add   ]"), ("settle",), ("click_text", "Choose an agent"), ("settle",),
            ("click_text", "claude   "), ("settle",), ("click_text", "Describe the task"), ("settle",),
            ("text", "Fix the flaky drain() test"), ("settle",), ("click_text", "Details"), ("settle",),
            ("click_text", "fresh-1", 1), ("key", "End", *["BackSpace"] * 8), ("text", "fix/flaky-drain"),
            ("settle",), ("shot", "scripting-run-agent-dialog")],
    },
    {
        "id": "scripting-orchestrator-dock", **FRESH, **WIDE, "args": ["crates/fresh-editor/src/server/command_access.rs"],
        "steps": [("wait", "Capability"), ("key", "ctrl+p"), ("text", "Docs: Agent Workspaces"),
                  ("wait", "Stage the Scripting"), ("settle",), ("key", "Return"),
                  ("wait", "flaky-repro"), ("sleep", 8), ("settle",), ("shot", "scripting-orchestrator-dock")],
    },
]
