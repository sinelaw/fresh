#!/usr/bin/env python3
"""The "failed validation" notice on a GitHub release.

    validation-notice.py add   <tag> <run-url>
    validation-notice.py strip <tag>
"""

import subprocess
import sys

MARKER = "> [!CAUTION]"
SENTINEL = "failed its own validation"


def gh(*args: str) -> str:
    return subprocess.run(("gh", *args), check=True, capture_output=True, text=True).stdout


def body_of(tag: str) -> str:
    return gh("release", "view", tag, "--json", "body", "-q", ".body")


def without_notice(body: str) -> str | None:
    """`body` minus a leading notice, or None if it carries none."""
    head, blank, rest = body.partition("\n\n")
    return rest if blank and head.startswith(MARKER) and SENTINEL in head else None


def edit(tag: str, suffix: str, body: str) -> None:
    gh("release", "edit", tag,
       "--title", f"fresh-editor {tag.lstrip('v')}{suffix}", "--notes", body)


def add(tag: str, run_url: str) -> None:
    notice = (
        f"{MARKER}\n"
        f"> **This release {SENTINEL} and must not be installed.**\n"
        "> It remains a pre-release: `fresh` will not offer it, and the latest\n"
        f"> release is unchanged. See [the run that rejected it]({run_url}).\n\n"
    )
    body = body_of(tag)
    edit(tag, " — FAILED VALIDATION, do not install", notice + (without_notice(body) or body))


def strip(tag: str) -> None:
    rest = without_notice(body_of(tag))
    if rest is not None:
        edit(tag, "", rest)
        print("took back the failed-validation notice an earlier attempt left")


if __name__ == "__main__":
    match sys.argv[1:]:
        case ["add", tag, run_url]:
            add(tag, run_url)
        case ["strip", tag]:
            strip(tag)
        case _:
            sys.exit(__doc__)
