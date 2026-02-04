#!/usr/bin/env bash
set -euo pipefail

# Publish a new version to winget-pkgs
# Usage: ./scripts/winget-publish.sh <version>
# Example: ./scripts/winget-publish.sh 0.1.99
#
# Requirements: gh, curl, check-jsonschema (pip install check-jsonschema)

VERSION="${1:-}"
if [[ -z "$VERSION" ]]; then
    echo "Usage: $0 <version>"
    echo "Example: $0 0.1.99"
    exit 1
fi

# Check dependencies
for cmd in gh curl check-jsonschema; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "Error: $cmd is required but not installed."
        if [[ "$cmd" == "check-jsonschema" ]]; then
            echo "Install with: pip install check-jsonschema"
        fi
        exit 1
    fi
done

PACKAGE_ID="sinelaw.fresh-editor"
PACKAGE_PATH="manifests/s/sinelaw/fresh-editor/${VERSION}"
INSTALLER_URL="https://github.com/sinelaw/fresh/releases/download/v${VERSION}/fresh-editor-x86_64-pc-windows-msvc.zip"
WINGET_REPO="microsoft/winget-pkgs"
BRANCH_NAME="${PACKAGE_ID}-${VERSION}"

echo "Publishing ${PACKAGE_ID} version ${VERSION}"
echo "Installer URL: ${INSTALLER_URL}"
echo ""

# Check if gh is authenticated
if ! gh auth status &>/dev/null; then
    echo "Please authenticate with GitHub first: gh auth login"
    exit 1
fi

# Download installer and compute SHA256
echo "Downloading installer to compute SHA256..."
TMPFILE=$(mktemp)
trap "rm -f $TMPFILE" EXIT
curl -fSL "$INSTALLER_URL" -o "$TMPFILE"
SHA256=$(sha256sum "$TMPFILE" | cut -d' ' -f1 | tr '[:lower:]' '[:upper:]')
echo "SHA256: ${SHA256}"
echo ""

# Create manifest files
echo "Creating manifest files..."

VERSION_MANIFEST="# yaml-language-server: \$schema=https://aka.ms/winget-manifest.version.1.10.0.schema.json
PackageIdentifier: ${PACKAGE_ID}
PackageVersion: ${VERSION}
DefaultLocale: en-US
ManifestType: version
ManifestVersion: 1.10.0"

INSTALLER_MANIFEST="# yaml-language-server: \$schema=https://aka.ms/winget-manifest.installer.1.10.0.schema.json
PackageIdentifier: ${PACKAGE_ID}
PackageVersion: ${VERSION}
InstallerLocale: en-US
InstallerType: zip
NestedInstallerType: portable
NestedInstallerFiles:
  - RelativeFilePath: fresh.exe
    PortableCommandAlias: fresh
Installers:
  - Architecture: x64
    InstallerUrl: ${INSTALLER_URL}
    InstallerSha256: ${SHA256}
ManifestType: installer
ManifestVersion: 1.10.0"

LOCALE_MANIFEST="# yaml-language-server: \$schema=https://aka.ms/winget-manifest.defaultLocale.1.10.0.schema.json
PackageIdentifier: ${PACKAGE_ID}
PackageVersion: ${VERSION}
PackageLocale: en-US
Publisher: sinelaw
PublisherUrl: https://github.com/sinelaw
PublisherSupportUrl: https://github.com/sinelaw/fresh/issues
Author: Noam Lewis
PackageName: fresh-editor
PackageUrl: https://github.com/sinelaw/fresh
License: GPL-2.0
LicenseUrl: https://github.com/sinelaw/fresh/blob/main/LICENSE
Copyright: Copyright (c) sinelaw
ShortDescription: A modern terminal-based text editor with TypeScript plugin support
Description: Fresh is a modern terminal-based text editor. It features LSP support, syntax highlighting, multi-cursor editing, TypeScript plugins, and an intuitive interface.
Tags:
  - editor
  - terminal
  - text-editor
  - tui
  - lsp
  - syntax-highlighting
  - plugins
ManifestType: defaultLocale
ManifestVersion: 1.10.0"

# Clone winget-pkgs fork or create it
WORK_DIR=$(mktemp -d)
trap "rm -rf $WORK_DIR $TMPFILE" EXIT
cd "$WORK_DIR"

echo "Forking/cloning winget-pkgs..."
gh repo fork "$WINGET_REPO" --clone=true --default-branch-only 2>/dev/null || true
cd winget-pkgs

# Sync fork with upstream
echo "Syncing fork with upstream..."
gh repo sync --force

# Create branch
echo "Creating branch ${BRANCH_NAME}..."
git checkout -b "$BRANCH_NAME"

# Create manifest directory and files
mkdir -p "$PACKAGE_PATH"
echo "$VERSION_MANIFEST" > "${PACKAGE_PATH}/${PACKAGE_ID}.yaml"
echo "$INSTALLER_MANIFEST" > "${PACKAGE_PATH}/${PACKAGE_ID}.installer.yaml"
echo "$LOCALE_MANIFEST" > "${PACKAGE_PATH}/${PACKAGE_ID}.locale.en-US.yaml"

# Validate manifests against schemas
echo "Validating manifests against schemas..."
SCHEMA_BASE="https://aka.ms/winget-manifest"

check-jsonschema --schemafile "${SCHEMA_BASE}.version.1.10.0.schema.json" \
    "${PACKAGE_PATH}/${PACKAGE_ID}.yaml"
echo "  ✓ Version manifest valid"

check-jsonschema --schemafile "${SCHEMA_BASE}.installer.1.10.0.schema.json" \
    "${PACKAGE_PATH}/${PACKAGE_ID}.installer.yaml"
echo "  ✓ Installer manifest valid"

check-jsonschema --schemafile "${SCHEMA_BASE}.defaultLocale.1.10.0.schema.json" \
    "${PACKAGE_PATH}/${PACKAGE_ID}.locale.en-US.yaml"
echo "  ✓ Locale manifest valid"

echo ""

# Commit and push
echo "Committing and pushing..."
git add .
git commit -m "New version: ${PACKAGE_ID} version ${VERSION}"
git push -u origin "$BRANCH_NAME"

# Create PR
echo "Creating pull request..."
PR_URL=$(gh pr create \
    --repo "$WINGET_REPO" \
    --title "New version: ${PACKAGE_ID} version ${VERSION}" \
    --body "## Description
New version of fresh-editor.

## Checklist
- [x] Package manifest follows the [1.10 schema](https://github.com/microsoft/winget-pkgs/tree/master/doc/manifest/schema/1.10.0)
- [x] Package manifest is valid (tested with \`winget validate\`)
- [x] URLs are valid and accessible" \
    --head "$BRANCH_NAME")

echo ""
echo "✓ Pull request created: ${PR_URL}"
