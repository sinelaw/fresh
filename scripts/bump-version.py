#!/usr/bin/env python3

import argparse
import json
import re
import shutil
import subprocess
import sys
from pathlib import Path
from typing import List, Literal, Optional

# ANSI color codes
RED = "\033[0;31m"
GREEN = "\033[0;32m"
YELLOW = "\033[1;33m"
BLUE = "\033[0;34m"
NC = "\033[0m"

BumpType = Literal["patch", "minor", "major"]

MAIN_BRANCH = "master"

# A [workspace.dependencies] entry for a local crate: has both a version and a path.
# Discovered rather than hardcoded so new workspace crates are never left behind.
LOCAL_DEP_RE = re.compile(
    r'^(?P<crate>[A-Za-z0-9_.-]+) = \{(?=[^}]*\bpath = ")(?P<body>[^}]*)\}',
    re.MULTILINE,
)
VERSION_FIELD_RE = re.compile(r'(\bversion = ")(?P<ver>[^"]*)(")')

def print_usage():
    """Prints the usage instructions."""
    print("Usage: ./scripts/bump-version.py [patch|minor|major]")
    print("")
    print("Examples:")
    print("  ./scripts/bump-version.py          # Bump patch version (default): 0.1.0 -> 0.1.1")
    print("  ./scripts/bump-version.py patch    # Bump patch version: 0.1.0 -> 0.1.1")
    print("  ./scripts/bump-version.py minor    # Bump minor version: 0.1.0 -> 0.2.0")
    print("  ./scripts/bump-version.py major    # Bump major version: 0.1.0 -> 1.0.0")
    print("")
    print("The script will:")
    print("  1. Read current version from Cargo.toml")
    print("  2. Calculate the new version")
    print("  3. Ask for confirmation")
    print("  4. Update Cargo.toml (workspace.package and workspace.dependencies)")
    print("  5. Update Cargo.lock")
    print("  6. Regenerate debian/changelog to match the new version")
    print("  7. Optionally generate release notes")
    print("  8. Commit to a new branch, open a PR, and merge it, skipping required checks")
    print("  9. Tag the merged commit and push the tag")
    print("")
    print("Options:")
    print("  --skip-bump  Skip the bump and PR; tag current master and push the tag")
    print("")
    print("GitHub Actions will then automatically:")
    print("  - Build binaries for all platforms")
    print("  - Create GitHub Release")
    print("  - Publish to crates.io")
    print("  - Publish to npm")
    print("  - Update Homebrew tap")
    print("  - Update AUR package")

def run_command(command: List[str], capture_output: bool = False, check: bool = True) -> subprocess.CompletedProcess:
    """Runs a shell command."""
    return subprocess.run(command, capture_output=capture_output, text=True, check=check)

def get_current_version(cargo_toml_path: Path) -> str:
    """Gets the current version from Cargo.toml."""
    content = cargo_toml_path.read_text()
    match = re.search(r'^version = "(.*)"', content, re.MULTILINE)
    if not match:
        raise ValueError("Could not find version in Cargo.toml")
    return match.group(1)

def calculate_new_version(current_version: str, bump_type: BumpType) -> str:
    """Calculates the new version."""
    major, minor, patch = map(int, current_version.split("-")[0].split("."))
    if bump_type == "patch":
        patch += 1
    elif bump_type == "minor":
        minor += 1
        patch = 0
    elif bump_type == "major":
        major += 1
        minor = 0
        patch = 0
    return f"{major}.{minor}.{patch}"

def update_cargo_toml(cargo_toml_path: Path, current_version: str, new_version: str) -> None:
    """Updates the version in Cargo.toml (both workspace.package and workspace.dependencies)."""
    content = cargo_toml_path.read_text()

    # Update [workspace.package] version
    pattern = rf'^version = "{re.escape(current_version)}"'
    new_content = re.sub(
        pattern,
        f'version = "{new_version}"',
        content,
        count=1,
        flags=re.MULTILINE,
    )

    # Update every local (path) crate in [workspace.dependencies], whatever version
    # it currently pins — a crate that already drifted must be pulled back in line.
    updated: List[str] = []
    missing_version: List[str] = []

    def bump_local_dep(match: re.Match) -> str:
        crate = match.group("crate")
        body = match.group("body")
        if not VERSION_FIELD_RE.search(body):
            missing_version.append(crate)
            return match.group(0)
        old_ver = VERSION_FIELD_RE.search(body).group("ver")
        if old_ver != new_version:
            updated.append(f"{crate}: {old_ver} -> {new_version}")
        body = VERSION_FIELD_RE.sub(rf'\g<1>{new_version}\g<3>', body, count=1)
        return f"{crate} = {{{body}}}"

    new_content = LOCAL_DEP_RE.sub(bump_local_dep, new_content)

    cargo_toml_path.write_text(new_content)

    for line in updated:
        print(f"  {line}")
    for crate in missing_version:
        print(f"{YELLOW}Warning:{NC} local dependency {crate} has a path but no version; not bumped")

def update_cargo_lock() -> None:
    """Updates Cargo.lock by running cargo build."""
    try:
        run_command(["cargo", "build", "--quiet"])
    except subprocess.CalledProcessError as e:
        print(f"{YELLOW}Warning:{NC} cargo build had some output (this might be normal)")
        print(e.stderr)

def update_debian_changelog(repo_root: Path) -> None:
    """Regenerates debian/changelog so its strict-format top entry matches the
    new Cargo.toml version. Without this, the committed debian/changelog drifts
    until CI's scripts/update-debian-changelog.sh runs it before dpkg-buildpackage."""
    script = repo_root / "scripts" / "update-debian-changelog.sh"
    if not script.exists():
        print(f"{YELLOW}Warning:{NC} {script} not found; skipping debian/changelog regen")
        return
    run_command([str(script)])

def get_previous_tag() -> Optional[str]:
    """Gets the previous git tag."""
    try:
        result = run_command(["git", "describe", "--tags", "--abbrev=0"], capture_output=True)
        return result.stdout.strip()
    except subprocess.CalledProcessError:
        try:
            # If no tags, get the initial commit
            result = run_command(["git", "rev-list", "--max-parents=0", "HEAD"], capture_output=True)
            return result.stdout.strip()
        except subprocess.CalledProcessError:
            return None

def tag_exists_at(tag_name: str, commit: str) -> bool:
    """Check if a tag exists and points to the given commit."""
    tag_result = run_command(["git", "rev-parse", f"{tag_name}^{{commit}}"], capture_output=True, check=False)
    if tag_result.returncode != 0:
        return False  # Tag doesn't exist
    return tag_result.stdout.strip() == commit

def check_release_preconditions() -> None:
    """Exits unless gh works and the checkout is a clean, up-to-date master."""
    errors: List[str] = []
    if shutil.which("gh") is None:
        errors.append("'gh' command not found; it is needed to open the PR")
    elif run_command(["gh", "auth", "status"], capture_output=True, check=False).returncode != 0:
        errors.append("gh is not logged in; run 'gh auth login'")

    branch = run_command(["git", "rev-parse", "--abbrev-ref", "HEAD"], capture_output=True).stdout.strip()
    if branch != MAIN_BRANCH:
        errors.append(f"must run from {MAIN_BRANCH}, not {branch}")

    if run_command(["git", "status", "--porcelain", "--untracked-files=no"], capture_output=True).stdout.strip():
        errors.append("working tree has uncommitted changes to tracked files")

    run_command(["git", "fetch", "--quiet", "origin", MAIN_BRANCH])
    head = run_command(["git", "rev-parse", "HEAD"], capture_output=True).stdout.strip()
    remote = run_command(["git", "rev-parse", f"origin/{MAIN_BRANCH}"], capture_output=True).stdout.strip()
    if head != remote:
        errors.append(f"HEAD differs from origin/{MAIN_BRANCH}; pull or reset first")

    if errors:
        for error in errors:
            print(f"{RED}Error: {error}{NC}")
        sys.exit(1)

def get_merge_commit(pr_url: str) -> str:
    """Returns the commit a merged PR landed as on the base branch."""
    result = run_command(["gh", "pr", "view", pr_url, "--json", "state,mergeCommit"], capture_output=True)
    pr = json.loads(result.stdout)
    if pr["state"] != "MERGED":
        print(f"{RED}Error: {pr_url} is {pr['state']}, not merged. Nothing was tagged.{NC}")
        print(f"Once it is merged, run {YELLOW}./scripts/bump-version.py --skip-bump{NC} from an up-to-date {MAIN_BRANCH}.")
        sys.exit(1)
    return pr["mergeCommit"]["oid"]

def main() -> None:
    """Main function."""
    parser = argparse.ArgumentParser(description="Version bump script for the editor project.")
    parser.add_argument(
        "bump_type",
        nargs="?",
        default="patch",
        choices=["patch", "minor", "major"],
        help="The type of version bump.",
    )
    parser.add_argument(
        "--skip-bump",
        action="store_true",
        help="Skip version bump and PR, just tag current master and push the tag.",
    )
    args = parser.parse_args()
    bump_type: BumpType = args.bump_type
    skip_bump: bool = args.skip_bump

    cargo_toml_path = Path("Cargo.toml")
    if not cargo_toml_path.exists():
        print(f"{RED}Error: Cargo.toml not found{NC}")
        print("Please run this script from the project root directory")
        sys.exit(1)

    try:
        current_version = get_current_version(cargo_toml_path)
    except ValueError as e:
        print(f"{RED}Error: {e}{NC}")
        sys.exit(1)

    check_release_preconditions()

    if skip_bump:
        new_version = current_version
        print(f"{BLUE}Tag and Push (skipping bump){NC}")
        print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
        print(f"Current version: {GREEN}{current_version}{NC}")
        print("")

        reply = input(f"Tag current {MAIN_BRANCH} as v{new_version} and push the tag? (y/N) ").lower()
        if reply != "y":
            print("Aborted.")
            sys.exit(0)
    else:
        new_version = calculate_new_version(current_version, bump_type)

        print(f"{BLUE}Version Bump ({bump_type}){NC}")
        print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
        print(f"Current version: {YELLOW}{current_version}{NC}")
        print(f"New version:     {GREEN}{new_version}{NC}")
        print("")

        reply = input(
            f"Bump {bump_type} version {current_version} -> {new_version}, open a PR, "
            "merge it skipping checks, then tag and push the tag? (y/N) "
        ).lower()
        if reply != "y":
            print("Aborted.")
            sys.exit(0)

        print("")
        print(f"{BLUE}Step: update Cargo.toml{NC}")
        update_cargo_toml(cargo_toml_path, current_version, new_version)
        print(f"{GREEN}✓{NC} Updated Cargo.toml (workspace.package and workspace.dependencies)")

        print("")
        print(f"{BLUE}Step: update Cargo.lock (cargo build){NC}")
        update_cargo_lock()
        print(f"{GREEN}✓{NC} Updated Cargo.lock")

        print("")
        print(f"{BLUE}Step: regenerate debian/changelog{NC}")
        update_debian_changelog(cargo_toml_path.resolve().parent)
        print(f"{GREEN}✓{NC} Updated debian/changelog")

        print("")
        print(f"{BLUE}Step: summary of changes{NC}")
        print("")
        try:
            diff_result = run_command(["git", "diff", "Cargo.toml", "Cargo.lock", "debian/changelog"], capture_output=True)
            print("Git diff:")
            print(diff_result.stdout)
        except subprocess.CalledProcessError:
            print("Could not get git diff.")


    print("")
    print(f"{GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━{NC}")
    print("")



    print("")
    release_notes_content = ""
    release_notes_path = Path("CHANGELOG.md")
    if release_notes_path.exists():
        release_notes_content = release_notes_path.read_text().strip()
        print(f"{BLUE}Found existing CHANGELOG.md.{NC}")
    else:
        print(f"{YELLOW}Warning: CHANGELOG.md not found. Tag will not include release notes.{NC}")

    try:
        if skip_bump:
            tag_commit = run_command(["git", "rev-parse", "HEAD"], capture_output=True).stdout.strip()
        else:
            bump_branch = f"bump-{new_version}"
            commit_title = f"Bump version to {new_version}"

            print("")
            print(f"{BLUE}Step: commit changes to {bump_branch}{NC}")
            run_command(["git", "switch", "-c", bump_branch])
            run_command(["git", "add", "Cargo.toml", "Cargo.lock", "debian/changelog"])
            run_command(["git", "commit", "-m", commit_title])
            print(f"{GREEN}✓{NC} Committed")

            print("")
            print(f"{BLUE}Step: open pull request{NC}")
            run_command(["git", "push", "-u", "origin", bump_branch])
            pr_result = run_command(
                ["gh", "pr", "create", "--base", MAIN_BRANCH, "--head", bump_branch, "--title", commit_title, "--body", ""],
                capture_output=True,
            )
            pr_url = pr_result.stdout.strip().splitlines()[-1]
            print(f"{GREEN}✓{NC} Opened {pr_url}")

            print("")
            print(f"{BLUE}Step: merge pull request (skipping checks){NC}")
            run_command(["gh", "pr", "merge", pr_url, "--admin", "--squash"])
            # Squashing makes a new commit; tagging the branch commit would leave the tag off master.
            tag_commit = get_merge_commit(pr_url)
            print(f"{GREEN}✓{NC} Merged as {tag_commit[:12]}")

            print("")
            print(f"{BLUE}Step: update local {MAIN_BRANCH}{NC}")
            run_command(["git", "switch", MAIN_BRANCH])
            run_command(["git", "pull", "--ff-only", "origin", MAIN_BRANCH])
            run_command(["git", "branch", "-D", bump_branch])
            print(f"{GREEN}✓{NC} Updated")

        print("")
        tag_name = f"v{new_version}"
        if tag_exists_at(tag_name, tag_commit):
            print(f"{BLUE}Step: create tag {tag_name}{NC} — already exists at {tag_commit[:12]}")
            print(f"{GREEN}✓{NC} Skipped (already tagged)")
        else:
            print(f"{BLUE}Step: create tag {tag_name}{NC}")
            if release_notes_content:
                run_command(["git", "tag", "-a", tag_name, tag_commit, "-F", "CHANGELOG.md"])
            else:
                run_command(["git", "tag", tag_name, tag_commit])
            print(f"{GREEN}✓{NC} Tagged")

        print("")
        print(f"{BLUE}Step: push tag to origin{NC}")
        run_command(["git", "push", "origin", tag_name])
        print(f"{GREEN}✓{NC} Pushed")

        print("")
        print(f"{GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━{NC}")
        print(f"{GREEN}✓ Version {new_version} tagged and pushed!{NC}")
        print("")
        print("GitHub Actions will now automatically:")
        print(f"  - Build binaries for all platforms")
        print(f"  - Create GitHub Release")
        print(f"  - Publish to crates.io")
        print(f"  - Publish to npm (@fresh-editor/fresh-editor)")
        print(f"  - Update Homebrew tap (sinelaw/fresh)")
        print(f"  - Update AUR package (fresh-editor)")
        print("")
        print(f"Monitor progress at: {BLUE}https://github.com/sinelaw/fresh/actions{NC}")

    except subprocess.CalledProcessError as e:
        print(f"{RED}An error occurred during git operations: {e}{NC}")
        print(e.stderr)
        sys.exit(1)
    except FileNotFoundError as e:
        print(f"{RED}Error: '{e.filename}' command not found. Is it installed and in your PATH?{NC}")
        sys.exit(1)

if __name__ == "__main__":
    main()
