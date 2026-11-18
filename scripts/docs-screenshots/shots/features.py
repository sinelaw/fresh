"""Feature-page shots: LSP, Markdown, code tours, packages."""
import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).parent))
from _common import SETTINGS_COLS, palette, settings_jump  # noqa: E402

TOUR_ROWS = 40

PYRIGHT = {"lsp": {"python": [{"command": "pyright-langserver", "args": ["--stdio"],
                                "enabled": True, "auto_start": True}]}}

SHOTS = [
    # markdown.md: the Renderers setting, and one renderer opened for editing.
    {"id": "markdown-preview-settings", "project": "markdown", "args": ["preview.md"], "cols": SETTINGS_COLS,
     "steps": [("wait", "Release notes")] + settings_jump("renderers")
     + [("crop_frame", "Settings ["), ("shot", "markdown-preview-settings"),
        ("key", "Down", "Down", "Down"), ("key", "Return"), ("wait", "Edit Value"),
        ("key", "Down"), ("key", "Return"), ("settle",),
        ("crop_frame", "Edit Value"), ("shot", "markdown-preview-settings-entry")]},
    {"id": "lsp-hover-diagnostic", "rows": 28, "project": "lsp", "args": ["invoicing/__main__.py"], "config": PYRIGHT,
     "steps": [("wait", "import argparse"), ("wait", "ready", 60), ("wait", "E:1", 60),
               ("key", "ctrl+g"), ("settle",), ("text", "35"), ("settle",), ("key", "Return"), ("settle",),
               ("key", "End", "Left", "Left", "Left", "Left", "Left"), ("settle",)]
     + palette("Show Hover Info") + [("sleep", 1), ("settle",), ("dump",), ("shot", "lsp-hover-diagnostic")]},
    {"id": "lsp-status-popup", "project": "lsp", "args": ["invoicing/models.py"], "config": PYRIGHT,
     "steps": [("wait", "class Client"), ("wait", "ready", 60), ("settle",)]
     + palette("Show LSP Status") + [("sleep", 1), ("settle",), ("dump",), ("shot", "lsp-status-popup")]},
    # code-tours.md: the picker, a tour on step 3, and the panel on its own.
    {"id": "code-tours", "project": "tours", "args": ["invoicing/models.py"], "rows": TOUR_ROWS,
     "steps": [("wait", "class Client")]
     + palette("Tour: Open Workspace Tour") + [("text", ".tours/"), ("settle",), ("shot", "code-tour-picker"),
                                               ("text", "money"), ("settle",), ("key", "Return"), ("wait", "Step 1 of 5"),
                                               ("key", "n"), ("key", "n"), ("wait", "Step 3 of 5"),
                                               ("shot", "code-tour-panel"),
                                               # the dock alone: its tab bar down to the status bar
                                               ("crop", (0, 26, 120, TOUR_ROWS - 26)), ("shot", "code-tour-steps")]},
    # plugins/index.md: the package browser on a theme (needs network to the GitHub registry).
    {"id": "packages-browser", "args": ["invoicing/models.py"],
     "steps": [("wait", "class Client")] + palette("Packages")
     + [("wait", "Registry synced", 90), ("wait", "catppuccin"), ("key", "Down", "Down", "Down"),
        ("wait", "Catppuccin theme"), ("shot", "packages-browser")]},
]
