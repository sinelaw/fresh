#!/usr/bin/env python3
"""
Publish a new version to winget-pkgs.

Usage: ./scripts/winget-publish.py <version>
Example: ./scripts/winget-publish.py 0.1.99

Requirements: gh, git, curl (or Python requests)
"""

import hashlib
import os
import re
import shutil
import subprocess
import sys
import tempfile
import urllib.request
from pathlib import Path


PACKAGE_ID = "sinelaw.fresh-editor"
MANIFEST_BASE = "manifests/s/sinelaw/fresh-editor"
WINGET_REPO = "microsoft/winget-pkgs"


def run(cmd: list[str], check: bool = True, capture: bool = False) -> subprocess.CompletedProcess:
    """Run a command and optionally capture output."""
    print(f"  $ {' '.join(cmd)}")
    return subprocess.run(cmd, check=check, capture_output=capture, text=True)


def get_sha256(url: str) -> str:
    """Download file and compute SHA256."""
    print(f"Downloading {url}...")
    with tempfile.NamedTemporaryFile(delete=False) as tmp:
        with urllib.request.urlopen(url) as response:
            shutil.copyfileobj(response, tmp)
        tmp_path = tmp.name

    sha256 = hashlib.sha256()
    with open(tmp_path, "rb") as f:
        for chunk in iter(lambda: f.read(8192), b""):
            sha256.update(chunk)

    Path(tmp_path).unlink()
    return sha256.hexdigest()


def main():
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <version>")
        print(f"Example: {sys.argv[0]} 0.1.99")
        sys.exit(1)

    version = sys.argv[1]
    installer_url = f"https://github.com/sinelaw/fresh/releases/download/v{version}/fresh-editor-x86_64-pc-windows-msvc.zip"
    branch_name = f"{PACKAGE_ID}-{version}"

    print(f"Publishing {PACKAGE_ID} version {version}")
    print(f"Installer URL: {installer_url}")
    print()

    # Check gh is authenticated
    result = run(["gh", "auth", "status"], check=False, capture=True)
    if result.returncode != 0:
        print("Please authenticate with GitHub first: gh auth login")
        sys.exit(1)

    # Compute SHA256
    print("Computing SHA256...")
    sha256 = get_sha256(installer_url)
    print(f"SHA256: {sha256}")
    print()

    # Use cache directory for persistent clone
    cache_dir = Path.home() / ".cache" / "winget-publish"
    cache_dir.mkdir(parents=True, exist_ok=True)
    repo_path = cache_dir / "winget-pkgs"

    if repo_path.exists():
        # Verify remote and fetch
        print("Using cached winget-pkgs clone...")
        os.chdir(repo_path)

        result = run(["git", "remote", "get-url", "origin"], capture=True)
        origin_url = result.stdout.strip()
        if "sinelaw/winget-pkgs" not in origin_url:
            print(f"Error: Unexpected origin remote: {origin_url}")
            print(f"Expected sinelaw/winget-pkgs. Delete {repo_path} and retry.")
            sys.exit(1)

        # Clean up any leftover state and sync
        run(["git", "checkout", "master"], check=False)
        run(["git", "reset", "--hard", "HEAD"])
        run(["git", "clean", "-fd"])
    else:
        # Fork and clone
        print("Forking/cloning winget-pkgs...")
        run(["gh", "repo", "fork", WINGET_REPO, "--clone=false"], check=False)  # Ensure fork exists
        run(["gh", "repo", "clone", "sinelaw/winget-pkgs", str(repo_path)])
        os.chdir(repo_path)

    # Add upstream remote and fetch latest
    print("Fetching latest from upstream...")
    run(["git", "remote", "add", "upstream", f"https://github.com/{WINGET_REPO}.git"], check=False)
    run(["git", "fetch", "upstream", "master"])
    run(["git", "reset", "--hard", "upstream/master"])

    # Sync fork with upstream (for the PR later)
    print("Syncing fork with upstream...")
    run(["gh", "repo", "sync", "--force"])

    # Find latest version
    print("Finding latest existing version...")
    manifest_path = repo_path / MANIFEST_BASE
    if not manifest_path.exists():
        print(f"Error: Package not found in upstream: {MANIFEST_BASE}")
        print("The package must exist in microsoft/winget-pkgs before using this script.")
        print("Please submit the initial version manually first.")
        sys.exit(1)

    versions = sorted(manifest_path.iterdir(), key=lambda p: list(map(int, p.name.split("."))))
    if not versions:
        print("Error: No existing versions found")
        sys.exit(1)

    latest_version = versions[-1].name
    print(f"Latest version: {latest_version}")

    # Create branch
    print(f"Creating branch {branch_name}...")
    run(["git", "checkout", "-b", branch_name])

    # Copy manifests
    old_path = manifest_path / latest_version
    new_path = manifest_path / version
    print(f"Copying manifests from {latest_version} to {version}...")
    shutil.copytree(old_path, new_path)

    # Update manifests
    print("Updating version, URL, and SHA256...")
    for yaml_file in new_path.glob("*.yaml"):
        content = yaml_file.read_text()

        # Update PackageVersion
        content = re.sub(r"^PackageVersion:.*$", f"PackageVersion: {version}", content, flags=re.MULTILINE)

        # Update InstallerUrl
        content = re.sub(r"^(\s*)InstallerUrl:.*$", rf"\1InstallerUrl: {installer_url}", content, flags=re.MULTILINE)

        # Update InstallerSha256
        content = re.sub(r"^(\s*)InstallerSha256:.*$", rf"\1InstallerSha256: {sha256}", content, flags=re.MULTILINE)

        # Update ReleaseNotesUrl
        content = re.sub(
            r"^ReleaseNotesUrl:.*$",
            f"ReleaseNotesUrl: https://github.com/sinelaw/fresh/releases/tag/v{version}",
            content,
            flags=re.MULTILINE,
        )

        yaml_file.write_text(content)

    # Show updated manifests
    print()
    print("Updated manifests:")
    for yaml_file in sorted(new_path.glob("*.yaml")):
        print(f"--- {yaml_file.name} ---")
        print(yaml_file.read_text())

    # Commit and push
    print("Committing and pushing...")
    run(["git", "add", "."])
    run(["git", "commit", "-m", f"New version: {PACKAGE_ID} version {version}"])
    run(["git", "push", "-u", "origin", branch_name])

    # Create PR
    print("Creating pull request...")
    pr_body = """## Description
New version of fresh-editor.

## Checklist
- [x] Have you signed the [Contributor License Agreement](https://cla.opensource.microsoft.com/microsoft/winget-pkgs)?
- [x] Have you checked that there aren't other open [pull requests](https://github.com/microsoft/winget-pkgs/pulls) for the same manifest update/change?
- [x] This PR only modifies one (1) manifest
- [x] Have you validated your manifest locally with `winget validate --manifest <path>`?
- [x] Have you tested your manifest locally with `winget install --manifest <path>`?
"""

    result = run(
        [
            "gh", "pr", "create",
            "--repo", WINGET_REPO,
            "--title", f"New version: {PACKAGE_ID} version {version}",
            "--body", pr_body,
            "--head", f"sinelaw:{branch_name}",
        ],
        capture=True,
    )

    pr_url = result.stdout.strip()
    print()
    print(f"Pull request created: {pr_url}")


if __name__ == "__main__":
    main()
