"""Core editor screens: palette, explorer, editing, terminal, git, themes, settings."""
import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).parent))
from _common import palette, settings_jump  # noqa: E402

EXPLORER = {"file_explorer": {"show_gitignored": True}}


def explorer_open():
    """Open the explorer on models.py and unfold web/ and web/db/."""
    return [("wait", "class Client"), ("key", "ctrl+e"), ("wait", "File Explorer"),
            ("click_text", "  web "), ("settle",), ("click_text", "  db "), ("settle",),
            ("click_text", "models.py", 1), ("settle",)]


SHOTS = [
    {"id": "palette-commands", "args": ["web/routes/items.js"], "steps": [
        ("wait", "itemsRouter"), ("key", "ctrl+p"), ("settle",), ("text", "tog line"),
        ("wait", "Toggle Line Numbers"), ("shot", "palette-commands")]},
    {"id": "palette-file-finder", "args": ["web/routes/items.js"], "steps": [
        ("wait", "itemsRouter"), ("key", "ctrl+p"), ("settle",), ("key", "BackSpace"), ("text", "test inv"),
        ("settle",), ("shot", "palette-file-finder")]},
    {"id": "explorer-git-status", "args": ["invoicing/models.py"], "config": EXPLORER,
     "steps": explorer_open() + [("shot", "explorer-git-status")]},
    {"id": "explorer-context-menu", "args": ["invoicing/models.py"], "config": EXPLORER,
     "steps": explorer_open() + [("click", 14, 11, 3), ("wait", "Copy Relative Path"),
                                 ("shot", "explorer-context-menu")]},
    {"id": "editing-multiple-cursors", "args": ["invoicing/render.py"], "steps": [
        ("wait", "render_text"), ("key", "ctrl+f"), ("settle",), ("text", "client.currency"), ("settle",),
        ("key", "Return"), ("settle",), ("key", "Escape"), ("settle",),
        *[("key", "ctrl+d") for _ in range(4)], ("wait", "4 cursors"), ("shot", "editing-multiple-cursors")]},
    {"id": "compare-side-by-side", "args": ["--diff", "config/staging.toml", "config/production.toml"], "steps": [
        ("wait", "staging"), ("sleep", 8), ("shot", "compare-side-by-side")], "rows": 22},
    {"id": "terminal-split", "args": ["invoicing/models.py"], "steps": [("wait", "class Client")]
        + palette("open terminal to the right") + [
        ("wait", "#"), ("text", "PS1='\\[\\e[32m\\]dev\\[\\e[0m\\]:\\[\\e[34m\\]\\W\\[\\e[0m\\]$ '; clear\r"), ("settle",),
        ("text", "git status -s\r"), ("settle",),
        ("text", "git --no-pager diff -U1 invoicing/models.py\r"), ("settle",),
        ("text", "python3 -m py_compile invoicing/models.py && echo compiles\r"), ("sleep", 1),
        ("shot", "terminal-split")], "cols": 140},
    {"id": "search-live-grep", "args": ["invoicing/cli.py"], "steps": [("wait", "argparse")] + palette("live grep")
        + [("text", "open_only"), ("wait", "test_report.py:28"), ("key", "Down"), ("sleep", 2), ("settle",), ("shot", "search-live-grep")], "cols": 150, "rows": 36},
    {"id": "git-review-diff", "args": ["invoicing/models.py"], "steps": [("wait", "class Client")] + palette("review diff")
        + [("wait", "UNSTAGED"), ("key", "2"), ("settle",), ("key", "F"), ("settle",), ("shot", "git-review-diff")]},
    {"id": "git-log", "args": ["invoicing/models.py"], "steps": [("wait", "class Client")] + palette("git log")
        + [("wait", "Initial invoicing"), ("key", "Down", "Down"), ("settle",), ("shot", "git-log")]},
    {"id": "welcome-screen", "args": [], "rows": 42, "steps": [
        ("sleep", 3), ("settle",), ("click_text", "Welcome"), ("wait", "Show this screen on startup"), ("sleep", 6), ("shot", "welcome-screen")]},
    {"id": "themes-select-theme", "args": ["invoicing/models.py"], "steps": [("wait", "class Client")] + palette("select theme")
        + [("wait", "(current)"), ("shot", "themes-select-theme")]},
    {"id": "themes-editor", "args": ["invoicing/models.py"], "rows": 44, "steps": [("wait", "class Client")] + palette("edit theme")
        + [("key", "Return"), ("wait", "Theme Editor: dark"), ("settle",),
           ("key", "Return"), ("settle",),                    # fold Editor
           *[("key", "Down") for _ in range(6)], ("settle",),  # to Syntax Highlighting
           *[("key", "Down") for _ in range(5)], ("settle",),  # to keyword
           ("shot", "themes-editor")]},
    # The two full-dialog settings shots run wider than SETTINGS_COLS, so the
    # picker's key hints and the Cursor Style dropdown are not clipped.
    {"id": "settings-status-bar", "args": ["invoicing/models.py"], "cols": 132, "rows": 34,
     "steps": [("wait", "class Client")] + settings_jump("left") + [("crop_frame", "Settings ["), ("key", "Return"), ("wait", "Git: branch"), ("settle",),
        ("key", "Down", "Down", "Down", "Down"), ("settle",), ("key", "shift+Right"), ("settle",), ("shot", "settings-status-bar")]},
    {"id": "settings-editor", "args": ["invoicing/models.py"], "cols": 132, "rows": 34,
     "steps": [("wait", "class Client")] + settings_jump("cursor style") + [("crop_frame", "Settings ["), ("shot", "settings-editor")]},
]
