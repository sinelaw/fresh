"""Settings UI shots: the screens the docs point to instead of raw config.json."""
import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).parent))
from _common import SETTINGS_COLS, SETTINGS_ROWS, open_settings, settings_jump  # noqa: E402

FILE = ["invoicing/models.py"]
READY = ("wait", "class Client")


def jump(id, query, extra=()):
    return {"id": id, "args": FILE, "cols": SETTINGS_COLS, "rows": SETTINGS_ROWS,
            "steps": [READY] + settings_jump(query) + [("crop_frame", "Settings [")] + list(extra) + [("shot", id)]}


SHOTS = [
    jump("settings-search-case-sensitive", "case sensitive"),
    jump("settings-locale", "locale"),
    jump("settings-check-for-updates", "check for updates"),
    jump("settings-dashboard-auto-open", "auto open"),
    jump("settings-keybinding-map", "active keybinding map", [("key", "Return"), ("settle",)]),
    jump("settings-keyboard-flags", "disambiguate"),
    jump("settings-packages-sources", "sources"),
    jump("settings-env-detectors", "detectors"),
    jump("settings-tab-size", "tab size"),
    jump("settings-default-language", "default language"),
    jump("settings-lsp-enabled", "lsp enabled"),
    jump("settings-indentation-guide", "indentation guide"),
    # Languages > python, opened in its entry dialog.
    jump("settings-language-entry", "python", [("key", "Return"), ("settle",), ("wait", "Edit Value"), ("crop_frame", "Edit Value")]),
    # Languages > yaml, scrolled to its Indentation Guide override.
    jump("settings-language-indentation-guide", "yaml", [
        ("key", "Return"), ("wait", "Edit Value"), ("key", *["Down"] * 14), ("settle",),
        ("crop_frame", "Edit Value")]),
    # Languages > python, scrolled to its Tab Size override.
    jump("settings-language-tab-size", "python", [
        ("key", "Return"), ("wait", "Edit Value"), ("key", *["Down"] * 20), ("settle",),
        ("crop_frame", "Edit Value")]),
    # Language Servers > python, then the server inside it.
    {"id": "settings-lsp-entry", "args": FILE, "cols": SETTINGS_COLS, "rows": SETTINGS_ROWS,
     "steps": [READY] + open_settings() + [
         ("key", "slash"), ("text", "python"), ("settle",), ("key", "Down"), ("key", "Return"), ("settle",),
         ("crop_frame", "Settings ["), ("key", "Return"), ("wait", "Edit Value"), ("key", "Return"), ("settle",),
         ("crop_frame", "Edit Item"), ("shot", "settings-lsp-entry")]},
    # The layer button, switched to Project.
    {"id": "settings-layer-project", "args": FILE, "cols": SETTINGS_COLS, "rows": SETTINGS_ROWS,
     "steps": [READY] + open_settings() + [("crop_frame", "Settings ["), ("click_text", "[ User ]"),
                                           ("settle",), ("shot", "settings-layer-project")]},
]
