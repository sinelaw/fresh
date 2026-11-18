"""One shot per built-in theme, for the gallery at the top of features/themes.md.

Every shot is the same scene -- the explorer and models.py with an
uncommitted change -- so the gallery compares only the colours.
"""
import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).parent))
from _common import palette  # noqa: E402

THEMES = [
    "dark", "light", "high-contrast", "quiet", "nord", "dracula",
    "tokyo-night", "gruvbox", "solarized-dark", "flat", "nostalgia", "terminal",
]


def theme_shot(theme):
    return {
        "id": f"theme-{theme}",
        "cols": 92,
        "rows": 24,
        "config": {"theme": theme},
        "args": ["invoicing/models.py"],
        "steps": [("wait", "class Client")] + palette("toggle file explorer") + [
            ("key", "ctrl+Home"), ("settle",),
            ("shot", f"themes/{theme}"),
        ],
    }


SHOTS = [theme_shot(t) for t in THEMES]
