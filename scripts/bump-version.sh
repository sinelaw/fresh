#!/bin/bash

# Version bump script for the editor project
# This script updates the version in Cargo.toml and Cargo.lock
# but does NOT commit, tag, or push - that's left for you to do manually

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Print usage
usage() {
    echo "Usage: $0 <new-version>"
    echo ""
    echo "Examples:"
    echo "  $0 0.2.0"
    echo "  $0 1.0.0"
    echo "  $0 0.1.1-beta.1"
    echo ""
    echo "The script will:"
    echo "  1. Update version in Cargo.toml"
    echo "  2. Update Cargo.lock by running 'cargo build'"
    echo "  3. Show you what changed"
    echo ""
    echo "After running this script, you should:"
    echo "  1. Review the changes"
    echo "  2. Commit: git add Cargo.toml Cargo.lock && git commit -m 'Bump version to X.Y.Z'"
    echo "  3. Tag: git tag vX.Y.Z"
    echo "  4. Push: git push origin main && git push origin vX.Y.Z"
    exit 1
}

# Check arguments
if [ $# -ne 1 ]; then
    usage
fi

NEW_VERSION="$1"

# Validate version format (basic check)
if ! echo "$NEW_VERSION" | grep -qE '^[0-9]+\.[0-9]+\.[0-9]+(-[a-zA-Z0-9.]+)?$'; then
    echo -e "${RED}Error: Invalid version format${NC}"
    echo "Version must be in format: MAJOR.MINOR.PATCH (e.g., 0.2.0)"
    echo "Or with pre-release: MAJOR.MINOR.PATCH-PRERELEASE (e.g., 0.2.0-beta.1)"
    exit 1
fi

# Check if we're in the project root
if [ ! -f "Cargo.toml" ]; then
    echo -e "${RED}Error: Cargo.toml not found${NC}"
    echo "Please run this script from the project root directory"
    exit 1
fi

# Get current version
CURRENT_VERSION=$(grep '^version = ' Cargo.toml | head -1 | sed 's/version = "\(.*\)"/\1/')

echo -e "${BLUE}Version Bump${NC}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo -e "Current version: ${YELLOW}$CURRENT_VERSION${NC}"
echo -e "New version:     ${GREEN}$NEW_VERSION${NC}"
echo ""

# Ask for confirmation
read -p "Continue with version bump? (y/N) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Aborted."
    exit 0
fi

echo ""
echo -e "${BLUE}Step 1:${NC} Updating Cargo.toml..."

# Update version in Cargo.toml
sed -i.bak "0,/^version = \".*\"/{s/^version = \".*\"/version = \"$NEW_VERSION\"/}" Cargo.toml
rm Cargo.toml.bak

echo -e "${GREEN}✓${NC} Updated Cargo.toml"

echo ""
echo -e "${BLUE}Step 2:${NC} Updating Cargo.lock (running cargo build)..."

# Update Cargo.lock by running cargo build
if cargo build --quiet 2>&1 | head -20; then
    echo -e "${GREEN}✓${NC} Updated Cargo.lock"
else
    echo -e "${YELLOW}Warning:${NC} cargo build had some output (this might be normal)"
fi

echo ""
echo -e "${BLUE}Step 3:${NC} Summary of changes..."
echo ""

# Show the diff
if command -v git &> /dev/null && git rev-parse --git-dir > /dev/null 2>&1; then
    echo "Git diff:"
    git diff Cargo.toml Cargo.lock
else
    echo "Changes made to:"
    echo "  - Cargo.toml (version: $CURRENT_VERSION -> $NEW_VERSION)"
    echo "  - Cargo.lock (updated)"
fi

echo ""
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${GREEN}✓ Version bump complete!${NC}"
echo ""
echo "Next steps:"
echo -e "  1. Review changes: ${YELLOW}git diff${NC}"
echo -e "  2. Commit changes: ${YELLOW}git add Cargo.toml Cargo.lock && git commit -m 'Bump version to $NEW_VERSION'${NC}"
echo -e "  3. Create tag:     ${YELLOW}git tag v$NEW_VERSION${NC}"
echo -e "  4. Push:           ${YELLOW}git push origin main && git push origin v$NEW_VERSION${NC}"
echo ""
echo "The GitHub Actions workflow will automatically create a release when you push the tag."
