"""Step helpers shared by the shot files. Import with:

    import sys, pathlib; sys.path.insert(0, str(pathlib.Path(__file__).parent))
    from _common import *
"""


def palette(command, wait=None):
    """Run a command-palette command by name."""
    steps = [("key", "ctrl+p"), ("text", command), ("settle",), ("key", "Return"), ("settle",)]
    if wait:
        steps.append(("wait", wait))
    return steps


def open_settings():
    return palette("open settings", wait="Settings [")


def settings_jump(query):
    """Open Settings, search for `query`, and jump to the first result."""
    return open_settings() + [("key", "slash"), ("text", query), ("settle",), ("key", "Return"), ("settle",)]


# Settings shots use a smaller terminal than the default, so the text is still
# legible once the image is scaled down to the docs' content column.
SETTINGS_COLS = 100
SETTINGS_ROWS = 30
