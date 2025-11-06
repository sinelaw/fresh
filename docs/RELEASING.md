# Release Process

This document describes how to create a new release of the editor.

## Automatic Binary Releases

The project is configured with GitHub Actions to automatically build and release binaries for multiple platforms when you create a version tag.

### Supported Platforms

The release workflow builds binaries for:

- **Linux x86_64**
  - GNU libc version: `editor-linux-x86_64.tar.gz`
  - musl version (static, more portable): `editor-linux-x86_64-musl.tar.gz`
- **macOS**
  - Intel (x86_64): `editor-macos-x86_64.tar.gz`
  - Apple Silicon (ARM64): `editor-macos-aarch64.tar.gz`
- **Windows x86_64**: `editor-windows-x86_64.zip`

### Creating a Release

#### Using the Version Bump Script (Recommended)

The easiest way to prepare a release is using the provided script:

```bash
# Prepare the version bump
./scripts/bump-version.sh 0.2.0

# Review the changes
git diff

# Commit, tag, and push
git add Cargo.toml Cargo.lock
git commit -m "Bump version to 0.2.0"
git tag v0.2.0
git push origin main
git push origin v0.2.0
```

The script will:
- Validate the version format
- Update `Cargo.toml` with the new version
- Run `cargo build` to update `Cargo.lock`
- Show you a diff of the changes
- Provide the exact commands to commit and push

#### Manual Process

If you prefer to do it manually:

1. **Update the version** in `Cargo.toml`:
   ```toml
   [package]
   version = "0.2.0"  # Update this
   ```

2. **Update the version** in `Cargo.lock`:
   ```bash
   cargo build
   ```

3. **Commit the version changes**:
   ```bash
   git add Cargo.toml Cargo.lock
   git commit -m "Bump version to 0.2.0"
   ```

4. **Create and push a version tag**:
   ```bash
   git tag v0.2.0
   git push origin main
   git push origin v0.2.0
   ```

#### After Creating the Tag

Once you've pushed the version tag (either method):

1. **Wait for GitHub Actions** to build the binaries (usually takes 5-10 minutes)
2. **Check the release** at `https://github.com/YOUR_USERNAME/YOUR_REPO/releases`

The GitHub Actions workflow will:
- **Run all tests** on Linux, macOS, and Windows (fails if any test fails)
- **Verify** the Cargo.toml version matches the git tag (fails if mismatch)
- Build binaries for all supported platforms
- Create a new GitHub Release
- Upload all binary archives to the release
- Generate SHA256 checksums for verification

**Important:**
- The workflow runs the complete test suite before creating any release. If any test fails on any platform, the release will be aborted.
- The workflow includes automatic version validation. If the version in `Cargo.toml` doesn't match the git tag, the release will fail with a clear error message.
- These checks ensure releases are always properly tested and versioned.

### Manual Testing Before Release

Before creating a release tag, it's recommended to test the build process locally:

```bash
# Test release build
cargo build --release

# Run tests
cargo test --release

# Test the binary
./target/release/editor
```

### Version Numbering

This project follows [Semantic Versioning](https://semver.org/):

- **MAJOR** version (1.0.0): Incompatible API changes
- **MINOR** version (0.1.0): New functionality, backwards compatible
- **PATCH** version (0.0.1): Backwards compatible bug fixes

### Troubleshooting

**Release workflow fails:**
- Check the Actions tab in GitHub for error logs
- **Test failures:** The workflow runs all tests first. If any test fails, fix the issue, commit, delete the tag, and recreate it
- **Version mismatch error:** If you see "Version mismatch!", update `Cargo.toml` to match the tag version, commit, delete the tag, and recreate it
- Verify the tag format is `vX.Y.Z` (e.g., `v0.1.0`)

**Test failures before release:**
```bash
# Run tests locally first
cargo test --all-features --all-targets
cargo test --doc

# If tests pass locally but fail in CI, check for platform-specific issues
```

**Need to delete a failed release:**
```bash
# Delete the tag locally
git tag -d v0.2.0

# Delete the tag on GitHub
git push origin :refs/tags/v0.2.0

# Delete the release on GitHub (via web UI)
```

Then fix the issue and create the tag again.

### Pre-release Versions

To create a pre-release (won't be marked as "latest"):

1. Use a pre-release version tag: `v0.2.0-beta.1`
2. The workflow will create a draft release
3. Edit the release on GitHub and check "This is a pre-release"
