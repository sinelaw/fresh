#!/bin/bash
set -euo pipefail

# Release script for fresh-editor
# Usage: ./scripts/release.sh [--dry-run]

DRY_RUN=false
if [[ "${1:-}" == "--dry-run" ]]; then
    DRY_RUN=true
    echo "=== DRY RUN MODE ==="
fi

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

info() { echo -e "${GREEN}[INFO]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; exit 1; }

# Ensure we're in the repo root
cd "$(dirname "$0")/.."

# Check for uncommitted changes
if [[ -n $(git status --porcelain) ]]; then
    error "Uncommitted changes detected. Please commit or stash them first."
fi

# Get current version from Cargo.toml
CURRENT_VERSION=$(grep '^version = ' Cargo.toml | head -1 | sed 's/version = "\(.*\)"/\1/')
info "Current version: $CURRENT_VERSION"

# Parse version components
IFS='.' read -r MAJOR MINOR PATCH <<< "$CURRENT_VERSION"

# Bump minor version, reset patch
NEW_MINOR=$((MINOR + 1))
NEW_VERSION="$MAJOR.$NEW_MINOR.0"
info "New version: $NEW_VERSION"

if $DRY_RUN; then
    info "[DRY RUN] Would update Cargo.toml version to $NEW_VERSION"
else
    # Update version in Cargo.toml
    sed -i "s/^version = \"$CURRENT_VERSION\"/version = \"$NEW_VERSION\"/" Cargo.toml
    info "Updated Cargo.toml"
fi

# Regenerate Cargo.lock
info "Updating Cargo.lock..."
if $DRY_RUN; then
    info "[DRY RUN] Would run: cargo check"
else
    cargo check --quiet
fi

# Run tests
info "Running tests..."
if $DRY_RUN; then
    info "[DRY RUN] Would run: cargo test"
else
    cargo test --quiet
fi

# Dry run cargo publish
info "Running cargo publish --dry-run..."
cargo publish --dry-run

if $DRY_RUN; then
    info "[DRY RUN] Would create git commit and tag v$NEW_VERSION"
    info "[DRY RUN] Would push to origin"
    info "[DRY RUN] Would run: cargo publish"

    # Revert Cargo.toml change in dry run
    git checkout Cargo.toml 2>/dev/null || true

    echo ""
    info "=== DRY RUN COMPLETE ==="
    info "Run without --dry-run to perform the actual release"
    exit 0
fi

# Commit version bump
info "Committing version bump..."
git add Cargo.toml Cargo.lock 2>/dev/null || git add Cargo.toml
git commit -m "Release v$NEW_VERSION"

# Create and push tag
TAG="v$NEW_VERSION"
info "Creating tag $TAG..."
git tag "$TAG"

# Push commit and tag
info "Pushing to origin..."
git push origin HEAD
git push origin "$TAG"

# Publish to crates.io
info "Publishing to crates.io..."
cargo publish

echo ""
info "=== RELEASE COMPLETE ==="
info "Released version $NEW_VERSION"
info "  - Tag: $TAG"
info "  - crates.io: https://crates.io/crates/fresh-editor"
info "  - GitHub release will be created by cargo-dist CI"
