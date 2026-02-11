/// <reference path="./lib/fresh.d.ts" />

/**
 * Fresh Package Manager Plugin
 *
 * A decentralized, git-based package manager for Fresh plugins and themes.
 * Inspired by Emacs straight.el and Neovim lazy.nvim.
 *
 * Features:
 * - Install plugins/themes from any git repository
 * - Update packages via git pull
 * - Optional curated registry (also a git repo)
 * - Version pinning with tags, branches, or commits
 * - Lockfile for reproducibility
 *
 * TODO: Plugin UI Component Library
 * ---------------------------------
 * The UI code in this plugin manually constructs buttons, lists, split views,
 * and focus management using raw text property entries. This is verbose and
 * error-prone. We need a shared UI component library that plugins can use to
 * build interfaces in virtual buffers:
 *
 * - Buttons, lists, scroll bars, tabs, split views, text inputs, etc.
 * - Automatic keyboard navigation and focus management
 * - Theme-aware styling
 *
 * The editor's settings UI already implements similar components - these could
 * be unified into a shared framework. See PLUGIN_MARKETPLACE_DESIGN.md for details.
 */

import { Finder } from "./lib/finder.ts";

const editor = getEditor();

// =============================================================================
// Configuration
// =============================================================================

const CONFIG_DIR = editor.getConfigDir();
const PACKAGES_DIR = editor.pathJoin(CONFIG_DIR, "plugins", "packages");
const THEMES_PACKAGES_DIR = editor.pathJoin(CONFIG_DIR, "themes", "packages");
const LANGUAGES_PACKAGES_DIR = editor.pathJoin(CONFIG_DIR, "languages", "packages");
const BUNDLES_PACKAGES_DIR = editor.pathJoin(CONFIG_DIR, "bundles", "packages");
const INDEX_DIR = editor.pathJoin(PACKAGES_DIR, ".index");
const CACHE_DIR = editor.pathJoin(PACKAGES_DIR, ".cache");
const LOCKFILE_PATH = editor.pathJoin(CONFIG_DIR, "fresh.lock");

// Default registry source
const DEFAULT_REGISTRY = "https://github.com/sinelaw/fresh-plugins-registry";

// =============================================================================
// Types
// =============================================================================

// TODO: Generate PackageManifest from the JSON schema (or vice versa) to ensure
// pkg.ts types stay in sync with package.schema.json. Consider using json-schema-to-typescript
// or ts-json-schema-generator to automate this.
// Related files:
//   - docs/internal/package-index-template/schemas/package.schema.json
//   - crates/fresh-editor/plugins/schemas/package.schema.json

// Bundle language definition (used in fresh.languages[])
interface BundleLanguage {
  /** Language identifier (e.g., 'elixir', 'heex') */
  id: string;
  /** Grammar configuration */
  grammar?: {
    file: string;
    extensions?: string[];
    firstLine?: string;
  };
  /** Language configuration */
  language?: {
    commentPrefix?: string;
    blockCommentStart?: string;
    blockCommentEnd?: string;
    useTabs?: boolean;
    tabSize?: number;
    autoIndent?: boolean;
    showWhitespaceTabs?: boolean;
    formatter?: {
      command: string;
      args?: string[];
    };
  };
  /** LSP server configuration */
  lsp?: {
    command: string;
    args?: string[];
    autoStart?: boolean;
    initializationOptions?: Record<string, unknown>;
  };
}

// Bundle plugin definition (used in fresh.plugins[])
interface BundlePlugin {
  /** Plugin entry point file relative to package */
  entry: string;
}

// Bundle theme definition (used in fresh.themes[])
interface BundleTheme {
  /** Theme JSON file path relative to package */
  file: string;
  /** Display name for the theme */
  name: string;
  /** Theme variant (dark or light) */
  variant?: "dark" | "light";
}

interface PackageManifest {
  name: string;
  version: string;
  description: string;
  type: "plugin" | "theme" | "theme-pack" | "language" | "bundle";
  author?: string;
  license?: string;
  repository?: string;
  fresh?: {
    min_version?: string;
    entry?: string;
    themes?: Array<{
      file: string;
      name: string;
      variant?: "dark" | "light";
    }>;
    config_schema?: Record<string, unknown>;

    // Language pack fields
    grammar?: {
      /** Path to grammar file relative to package */
      file: string;
      /** File extensions (e.g., ["rs", "rust"]) */
      extensions?: string[];
      /** Shebang pattern for detection */
      firstLine?: string;
    };
    language?: {
      commentPrefix?: string;
      blockCommentStart?: string;
      blockCommentEnd?: string;
      useTabs?: boolean;
      tabSize?: number;
      autoIndent?: boolean;
      showWhitespaceTabs?: boolean;
      formatter?: {
        command: string;
        args?: string[];
      };
    };
    lsp?: {
      command: string;
      args?: string[];
      autoStart?: boolean;
      initializationOptions?: Record<string, unknown>;
    };

    // Bundle fields
    /** Languages included in this bundle */
    languages?: BundleLanguage[];
    /** Plugins included in this bundle */
    plugins?: BundlePlugin[];
  };
  keywords?: string[];
}

interface RegistryEntry {
  description: string;
  repository: string;
  author?: string;
  license?: string;
  keywords?: string[];
  stars?: number;
  downloads?: number;
  latest_version?: string;
  fresh_min_version?: string;
  variants?: string[];
}

interface RegistryData {
  schema_version: number;
  updated: string;
  packages: Record<string, RegistryEntry>;
}

interface InstalledPackage {
  name: string;
  path: string;
  type: "plugin" | "theme" | "language" | "bundle";
  source: string;
  version: string;
  commit?: string;
  manifest?: PackageManifest;
  /** Original local path if installed from a local directory */
  localSource?: string;
}

interface LockfileEntry {
  source: string;
  commit: string;
  version: string;
  integrity?: string;
}

interface Lockfile {
  lockfile_version: number;
  generated: string;
  packages: Record<string, LockfileEntry>;
}

// =============================================================================
// Types for URL parsing
// =============================================================================

interface ParsedPackageUrl {
  /** The base git repository URL or local path (without fragment) */
  repoUrl: string;
  /** Optional path within the repository/directory (from fragment) */
  subpath: string | null;
  /** Extracted package name */
  name: string;
  /** Whether this is a local file path (not a remote URL) */
  isLocal: boolean;
}

// =============================================================================
// Utility Functions
// =============================================================================

/**
 * Ensure a directory exists
 */
async function ensureDir(path: string): Promise<boolean> {
  if (editor.fileExists(path)) {
    return true;
  }
  const result = await editor.spawnProcess("mkdir", ["-p", path]);
  return result.exit_code === 0;
}

/**
 * Hash a string (simple djb2 hash for source identification)
 */
function hashString(str: string): string {
  let hash = 5381;
  for (let i = 0; i < str.length; i++) {
    hash = ((hash << 5) + hash) + str.charCodeAt(i);
  }
  return Math.abs(hash).toString(16).slice(0, 8);
}

/**
 * Run a git command without prompting for credentials.
 * Uses git config options to prevent interactive prompts (cross-platform).
 */
async function gitCommand(args: string[]): Promise<{ exit_code: number; stdout: string; stderr: string }> {
  // Use git config options to disable credential prompts (works on Windows and Unix)
  // -c credential.helper= disables credential helper
  // -c core.askPass= disables askpass program
  const gitArgs = [
    "-c", "credential.helper=",
    "-c", "core.askPass=",
    ...args
  ];
  const result = await editor.spawnProcess("git", gitArgs);
  return result;
}

/**
 * Check if a string is a local file path (not a URL).
 */
function isLocalPath(str: string): boolean {
  // Absolute paths start with /
  if (str.startsWith("/")) return true;
  // Windows absolute paths (C:\, D:\, etc.)
  if (/^[A-Za-z]:[\\\/]/.test(str)) return true;
  // Relative paths starting with . or ..
  if (str.startsWith("./") || str.startsWith("../")) return true;
  // Home directory expansion
  if (str.startsWith("~/")) return true;
  // Not a URL scheme (http://, https://, git://, ssh://, file://)
  if (!/^[a-zA-Z][a-zA-Z0-9+.-]*:\/\//.test(str)) {
    // If it doesn't look like a URL and doesn't contain @, it's probably a path
    // (git@github.com:user/repo is a git URL)
    if (!str.includes("@") || str.startsWith("/")) {
      return true;
    }
  }
  return false;
}

/**
 * Parse a package URL that may contain a subpath fragment.
 *
 * Supported formats:
 * - `https://github.com/user/repo` - standard repo
 * - `https://github.com/user/repo#path/to/plugin` - monorepo with subpath
 * - `https://github.com/user/repo.git#packages/my-plugin` - with .git suffix
 * - `/path/to/local/repo#subdir` - local path with subpath
 * - `/path/to/local/package` - direct local package path
 *
 * The fragment (after #) specifies a subdirectory within the repo.
 */
function parsePackageUrl(url: string): ParsedPackageUrl {
  // Split on # to get subpath
  const hashIndex = url.indexOf("#");
  let repoUrl: string;
  let subpath: string | null = null;

  if (hashIndex !== -1) {
    repoUrl = url.slice(0, hashIndex);
    subpath = url.slice(hashIndex + 1);
    // Clean up subpath - remove leading/trailing slashes
    subpath = subpath.replace(/^\/+|\/+$/g, "");
    if (subpath === "") {
      subpath = null;
    }
  } else {
    repoUrl = url;
  }

  // Determine if this is a local path
  const isLocal = isLocalPath(repoUrl);

  // Extract package name
  let name: string;
  if (subpath) {
    // For monorepo/directory, use the last component of the subpath
    const parts = subpath.split("/");
    name = parts[parts.length - 1].replace(/^fresh-/, "");
  } else {
    // For regular repo/path, use the last component
    const match = repoUrl.match(/\/([^\/]+?)(\.git)?$/);
    name = match ? match[1].replace(/^fresh-/, "") : "unknown";
  }

  return { repoUrl, subpath, name, isLocal };
}

/**
 * Extract package name from git URL (legacy helper)
 */
function extractPackageName(url: string): string {
  return parsePackageUrl(url).name;
}

/**
 * Get registry sources from config
 */
function getRegistrySources(): string[] {
  const config = editor.getConfig() as Record<string, unknown>;
  const packages = config?.packages as Record<string, unknown> | undefined;
  const sources = packages?.sources as string[] | undefined;
  return sources && sources.length > 0 ? sources : [DEFAULT_REGISTRY];
}

/**
 * Read and parse a JSON file
 */
function readJsonFile<T>(path: string): T | null {
  try {
    const content = editor.readFile(path);
    if (content) {
      return JSON.parse(content) as T;
    }
  } catch (e) {
    editor.debug(`[pkg] Failed to read JSON file ${path}: ${e}`);
  }
  return null;
}

/**
 * Write a JSON file
 */
async function writeJsonFile(path: string, data: unknown): Promise<boolean> {
  try {
    const content = JSON.stringify(data, null, 2);
    return editor.writeFile(path, content);
  } catch (e) {
    editor.debug(`[pkg] Failed to write JSON file ${path}: ${e}`);
    return false;
  }
}

// =============================================================================
// Registry Operations
// =============================================================================

/**
 * Sync registry sources
 */
async function syncRegistry(): Promise<void> {
  editor.setStatus("Syncing package registry...");

  await ensureDir(INDEX_DIR);

  const sources = getRegistrySources();
  let synced = 0;
  const errors: string[] = [];

  for (const source of sources) {
    const indexPath = editor.pathJoin(INDEX_DIR, hashString(source));

    if (editor.fileExists(indexPath)) {
      // Update existing
      editor.setStatus(`Updating registry: ${source}...`);
      const result = await gitCommand(["-C", `${indexPath}`, "pull", "--ff-only"]);
      if (result.exit_code === 0) {
        synced++;
      } else {
        const errorMsg = result.stderr.includes("Could not resolve host")
          ? "Network error"
          : result.stderr.includes("Authentication") || result.stderr.includes("403")
          ? "Authentication failed (check if repo is public)"
          : result.stderr.split("\n")[0] || "Unknown error";
        errors.push(`${source}: ${errorMsg}`);
        editor.warn(`[pkg] Failed to update registry ${source}: ${result.stderr}`);
      }
    } else {
      // Clone new
      editor.setStatus(`Cloning registry: ${source}...`);
      const result = await gitCommand(["clone", "--depth", "1", `${source}`, `${indexPath}`]);
      if (result.exit_code === 0) {
        synced++;
      } else {
        const errorMsg = result.stderr.includes("Could not resolve host")
          ? "Network error"
          : result.stderr.includes("not found") || result.stderr.includes("404")
          ? "Repository not found"
          : result.stderr.includes("Authentication") || result.stderr.includes("403")
          ? "Authentication failed (check if repo is public)"
          : result.stderr.split("\n")[0] || "Unknown error";
        errors.push(`${source}: ${errorMsg}`);
        editor.warn(`[pkg] Failed to clone registry ${source}: ${result.stderr}`);
      }
    }
  }

  // Cache registry data locally for faster startup next time
  if (synced > 0) {
    await cacheRegistry();
  }

  if (errors.length > 0) {
    editor.setStatus(`Registry: ${synced}/${sources.length} synced. Errors: ${errors.join("; ")}`);
  } else {
    editor.setStatus(`Registry synced (${synced}/${sources.length} sources)`);
  }
}

/**
 * Load merged registry data from git index or cache
 */
function loadRegistry(type: "plugins" | "themes" | "languages"): RegistryData {
  editor.debug(`[pkg] loadRegistry called for ${type}`);
  const sources = getRegistrySources();
  editor.debug(`[pkg] sources: ${JSON.stringify(sources)}`);
  const merged: RegistryData = {
    schema_version: 1,
    updated: new Date().toISOString(),
    packages: {}
  };

  for (const source of sources) {
    // Try git index first
    const indexPath = editor.pathJoin(INDEX_DIR, hashString(source), `${type}.json`);
    editor.debug(`[pkg] checking index path: ${indexPath}`);
    let data = readJsonFile<RegistryData>(indexPath);

    // Fall back to cache if index not available
    if (!data?.packages) {
      const cachePath = editor.pathJoin(CACHE_DIR, `${hashString(source)}_${type}.json`);
      data = readJsonFile<RegistryData>(cachePath);
      if (data?.packages) {
        editor.debug(`[pkg] using cached data for ${type}`);
      }
    }

    editor.debug(`[pkg] data loaded: ${data ? 'yes' : 'no'}, packages: ${data?.packages ? Object.keys(data.packages).length : 0}`);
    if (data?.packages) {
      Object.assign(merged.packages, data.packages);
    }
  }

  editor.debug(`[pkg] total merged packages: ${Object.keys(merged.packages).length}`);
  return merged;
}

/**
 * Cache registry data locally for offline/fast access
 */
async function cacheRegistry(): Promise<void> {
  await ensureDir(CACHE_DIR);
  const sources = getRegistrySources();

  for (const source of sources) {
    const sourceHash = hashString(source);
    for (const type of ["plugins", "themes", "languages"] as const) {
      const indexPath = editor.pathJoin(INDEX_DIR, sourceHash, `${type}.json`);
      const cachePath = editor.pathJoin(CACHE_DIR, `${sourceHash}_${type}.json`);

      const data = readJsonFile<RegistryData>(indexPath);
      if (data?.packages && Object.keys(data.packages).length > 0) {
        await writeJsonFile(cachePath, data);
      }
    }
  }
}

/**
 * Check if registry data is available (from index or cache)
 */
function isRegistrySynced(): boolean {
  const sources = getRegistrySources();
  for (const source of sources) {
    // Check git index
    const indexPath = editor.pathJoin(INDEX_DIR, hashString(source));
    if (editor.fileExists(indexPath)) {
      return true;
    }
    // Check cache
    const cachePath = editor.pathJoin(CACHE_DIR, `${hashString(source)}_plugins.json`);
    if (editor.fileExists(cachePath)) {
      return true;
    }
  }
  return false;
}

// =============================================================================
// Package Operations
// =============================================================================

/**
 * Get list of installed packages
 */
function getInstalledPackages(type: "plugin" | "theme" | "language" | "bundle"): InstalledPackage[] {
  const packagesDir = type === "plugin" ? PACKAGES_DIR
                    : type === "theme" ? THEMES_PACKAGES_DIR
                    : type === "bundle" ? BUNDLES_PACKAGES_DIR
                    : LANGUAGES_PACKAGES_DIR;
  const packages: InstalledPackage[] = [];

  if (!editor.fileExists(packagesDir)) {
    return packages;
  }

  try {
    const entries = editor.readDir(packagesDir);
    for (const entry of entries) {
      if (entry.is_dir && !entry.name.startsWith(".")) {
        const pkgPath = editor.pathJoin(packagesDir, entry.name);
        const manifestPath = editor.pathJoin(pkgPath, "package.json");
        const manifest = readJsonFile<PackageManifest>(manifestPath);

        // Try to get git remote
        const gitConfigPath = editor.pathJoin(pkgPath, ".git", "config");
        let source = "";
        let localSource: string | undefined;
        if (editor.fileExists(gitConfigPath)) {
          const gitConfig = editor.readFile(gitConfigPath);
          if (gitConfig) {
            const match = gitConfig.match(/url\s*=\s*(.+)/);
            if (match) {
              source = match[1].trim();
            }
          }
        }

        // Check for .fresh-source.json (local path or monorepo installs)
        if (!source) {
          const freshSourcePath = editor.pathJoin(pkgPath, ".fresh-source.json");
          const freshSource = readJsonFile<{ local_path?: string; original_url?: string }>(freshSourcePath);
          if (freshSource?.local_path) {
            localSource = freshSource.local_path;
            source = freshSource.original_url || freshSource.local_path;
          }
        }

        packages.push({
          name: entry.name,
          path: pkgPath,
          type,
          source,
          version: manifest?.version || "unknown",
          manifest,
          localSource,
        });
      }
    }
  } catch (e) {
    editor.debug(`[pkg] Failed to list packages: ${e}`);
  }

  return packages;
}

/**
 * Validation result for a package
 */
interface ValidationResult {
  valid: boolean;
  error?: string;
  manifest?: PackageManifest;
  entryPath?: string;
}

/**
 * Validate a package directory has correct structure
 *
 * Checks:
 * 1. package.json exists
 * 2. package.json has required fields (name, type)
 * 3. Entry file exists (for plugins)
 */
function validatePackage(packageDir: string, packageName: string): ValidationResult {
  const manifestPath = editor.pathJoin(packageDir, "package.json");

  // Check package.json exists
  if (!editor.fileExists(manifestPath)) {
    return {
      valid: false,
      error: `Missing package.json - expected at ${manifestPath}`
    };
  }

  // Read and validate manifest
  const manifest = readJsonFile<PackageManifest>(manifestPath);
  if (!manifest) {
    return {
      valid: false,
      error: "Invalid package.json - could not parse JSON"
    };
  }

  // Validate required fields
  if (!manifest.name) {
    return {
      valid: false,
      error: "Invalid package.json - missing 'name' field"
    };
  }

  if (!manifest.type) {
    return {
      valid: false,
      error: "Invalid package.json - missing 'type' field (should be 'plugin', 'theme', 'language', or 'bundle')"
    };
  }

  if (manifest.type !== "plugin" && manifest.type !== "theme" && manifest.type !== "language" && manifest.type !== "bundle") {
    return {
      valid: false,
      error: `Invalid package.json - 'type' must be 'plugin', 'theme', 'language', or 'bundle', got '${manifest.type}'`
    };
  }

  // For plugins, validate entry file exists
  if (manifest.type === "plugin") {
    const entryFile = manifest.fresh?.entry || `${manifest.name}.ts`;
    const entryPath = editor.pathJoin(packageDir, entryFile);

    if (!editor.fileExists(entryPath)) {
      // Try .js as fallback
      const jsEntryPath = entryPath.replace(/\.ts$/, ".js");
      if (editor.fileExists(jsEntryPath)) {
        return { valid: true, manifest, entryPath: jsEntryPath };
      }

      return {
        valid: false,
        error: `Missing entry file '${entryFile}' - check fresh.entry in package.json`
      };
    }

    return { valid: true, manifest, entryPath };
  }

  // For language packs, validate at least one component is defined
  if (manifest.type === "language") {
    if (!manifest.fresh?.grammar && !manifest.fresh?.language && !manifest.fresh?.lsp) {
      return {
        valid: false,
        error: "Language package must define at least one of: grammar, language, or lsp"
      };
    }

    // Validate grammar file exists if specified
    if (manifest.fresh?.grammar?.file) {
      const grammarPath = editor.pathJoin(packageDir, manifest.fresh.grammar.file);
      if (!editor.fileExists(grammarPath)) {
        return {
          valid: false,
          error: `Grammar file not found: ${manifest.fresh.grammar.file}`
        };
      }
    }

    return { valid: true, manifest };
  }

  // For bundles, validate at least one language, plugin, or theme is defined
  if (manifest.type === "bundle") {
    const hasLanguages = manifest.fresh?.languages && manifest.fresh.languages.length > 0;
    const hasPlugins = manifest.fresh?.plugins && manifest.fresh.plugins.length > 0;
    const hasThemes = manifest.fresh?.themes && manifest.fresh.themes.length > 0;

    if (!hasLanguages && !hasPlugins && !hasThemes) {
      return {
        valid: false,
        error: "Bundle package must define at least one language, plugin, or theme"
      };
    }

    // Validate each language entry
    if (manifest.fresh?.languages) {
      for (const lang of manifest.fresh.languages) {
        if (!lang.id) {
          return {
            valid: false,
            error: "Bundle language entry missing required 'id' field"
          };
        }
        // Validate grammar file exists if specified
        if (lang.grammar?.file) {
          const grammarPath = editor.pathJoin(packageDir, lang.grammar.file);
          if (!editor.fileExists(grammarPath)) {
            return {
              valid: false,
              error: `Grammar file not found for language '${lang.id}': ${lang.grammar.file}`
            };
          }
        }
      }
    }

    // Validate each plugin entry
    if (manifest.fresh?.plugins) {
      for (const plugin of manifest.fresh.plugins) {
        if (!plugin.entry) {
          return {
            valid: false,
            error: "Bundle plugin entry missing required 'entry' field"
          };
        }
        const entryPath = editor.pathJoin(packageDir, plugin.entry);
        if (!editor.fileExists(entryPath)) {
          // Try .js as fallback
          const jsEntryPath = entryPath.replace(/\.ts$/, ".js");
          if (!editor.fileExists(jsEntryPath)) {
            return {
              valid: false,
              error: `Plugin entry file not found: ${plugin.entry}`
            };
          }
        }
      }
    }

    return { valid: true, manifest };
  }

  // Themes don't need entry file validation
  return { valid: true, manifest };
}

/**
 * Install a package from git URL or local path.
 *
 * Supports:
 * - `https://github.com/user/repo` - standard git repo
 * - `https://github.com/user/repo#packages/my-plugin` - monorepo with subpath
 * - `/path/to/local/repo#subdir` - local path with subpath
 * - `/path/to/local/package` - direct local package path
 *
 * For subpath packages, clones/copies to temp directory and copies the subdirectory.
 */
async function installPackage(
  url: string,
  name?: string,
  _type?: "plugin" | "theme" | "language" | "bundle",  // Ignored - type is auto-detected from manifest
  version?: string
): Promise<boolean> {
  const parsed = parsePackageUrl(url);
  const packageName = name || parsed.name;

  editor.setStatus(`Installing ${packageName}...`);

  if (parsed.isLocal) {
    // Local path installation: copy directly
    return await installFromLocalPath(parsed, packageName);
  } else if (parsed.subpath) {
    // Remote monorepo installation: clone to temp, copy subdirectory
    return await installFromMonorepo(parsed, packageName, version);
  } else {
    // Standard git installation: clone directly
    return await installFromRepo(parsed.repoUrl, packageName, version);
  }
}

/**
 * Install from a standard git repository (no subpath)
 * Clones to temp first to detect type, then moves to correct location.
 */
async function installFromRepo(
  repoUrl: string,
  packageName: string,
  version?: string
): Promise<boolean> {
  // Clone to temp directory first to detect package type
  const tempDir = `/tmp/fresh-pkg-clone-${hashString(repoUrl)}-${Date.now()}`;

  const cloneArgs = ["clone"];
  if (!version || version === "latest") {
    cloneArgs.push("--depth", "1");
  }
  cloneArgs.push(`${repoUrl}`, `${tempDir}`);

  const result = await gitCommand(cloneArgs);

  if (result.exit_code !== 0) {
    const errorMsg = result.stderr.includes("not found") || result.stderr.includes("404")
      ? "Repository not found"
      : result.stderr.includes("Authentication") || result.stderr.includes("403")
      ? "Access denied (repository may be private)"
      : result.stderr.split("\n")[0] || "Clone failed";
    editor.setStatus(`Failed to install ${packageName}: ${errorMsg}`);
    return false;
  }

  // Checkout specific version if requested
  if (version && version !== "latest") {
    const checkoutResult = await checkoutVersion(tempDir, version);
    if (!checkoutResult) {
      editor.setStatus(`Installed ${packageName} but failed to checkout version ${version}`);
    }
  }

  // Validate package structure
  const validation = validatePackage(tempDir, packageName);
  if (!validation.valid) {
    editor.warn(`[pkg] Invalid package '${packageName}': ${validation.error}`);
    editor.setStatus(`Failed to install ${packageName}: ${validation.error}`);
    // Clean up
    await editor.spawnProcess("rm", ["-rf", tempDir]);
    return false;
  }

  const manifest = validation.manifest;

  // Use manifest name as the authoritative package name
  if (manifest?.name) packageName = manifest.name;

  // Determine correct target directory based on actual package type
  const actualType = manifest?.type || "plugin";
  const correctPackagesDir = actualType === "plugin" ? PACKAGES_DIR
                           : actualType === "theme" ? THEMES_PACKAGES_DIR
                           : actualType === "bundle" ? BUNDLES_PACKAGES_DIR
                           : LANGUAGES_PACKAGES_DIR;
  const correctTargetDir = editor.pathJoin(correctPackagesDir, packageName);

  // Check if already installed in correct location
  if (editor.fileExists(correctTargetDir)) {
    editor.setStatus(`Package '${packageName}' is already installed`);
    await editor.spawnProcess("rm", ["-rf", tempDir]);
    return false;
  }

  // Ensure correct directory exists and move from temp
  await ensureDir(correctPackagesDir);
  const moveResult = await editor.spawnProcess("mv", [tempDir, correctTargetDir]);
  if (moveResult.exit_code !== 0) {
    editor.setStatus(`Failed to install ${packageName}: ${moveResult.stderr}`);
    await editor.spawnProcess("rm", ["-rf", tempDir]);
    return false;
  }

  // Dynamically load plugins, reload themes, load language packs, or load bundles
  if (manifest?.type === "plugin" && validation.entryPath) {
    // Update entry path to new location
    const newEntryPath = validation.entryPath.replace(tempDir, correctTargetDir);
    await editor.loadPlugin(newEntryPath);
    editor.setStatus(`Installed and activated ${packageName}${manifest ? ` v${manifest.version}` : ""}`);
  } else if (manifest?.type === "theme") {
    editor.reloadThemes();
    editor.setStatus(`Installed theme ${packageName}${manifest ? ` v${manifest.version}` : ""}`);
  } else if (manifest?.type === "language") {
    await loadLanguagePack(correctTargetDir, manifest);
    editor.setStatus(`Installed language pack ${packageName}${manifest ? ` v${manifest.version}` : ""}`);
  } else if (manifest?.type === "bundle") {
    await loadBundle(correctTargetDir, manifest);
    editor.setStatus(`Installed bundle ${packageName}${manifest ? ` v${manifest.version}` : ""}`);
  } else {
    editor.setStatus(`Installed ${packageName}${manifest ? ` v${manifest.version}` : ""}`);
  }
  return true;
}

/**
 * Install from a local file path.
 *
 * Strategy:
 * - If subpath is specified: copy that subdirectory
 * - Otherwise: copy the entire directory
 * - Store the source path for reference
 * - Auto-detect package type from manifest and install to correct directory
 */
async function installFromLocalPath(
  parsed: ParsedPackageUrl,
  packageName: string
): Promise<boolean> {
  // Resolve the full source path
  let sourcePath = parsed.repoUrl;

  // Handle home directory expansion
  if (sourcePath.startsWith("~/")) {
    const home = editor.getEnv("HOME") || editor.getEnv("USERPROFILE") || "";
    sourcePath = editor.pathJoin(home, sourcePath.slice(2));
  }

  // If there's a subpath, append it
  if (parsed.subpath) {
    sourcePath = editor.pathJoin(sourcePath, parsed.subpath);
  }

  // Check if source exists
  if (!editor.fileExists(sourcePath)) {
    editor.setStatus(`Local path not found: ${sourcePath}`);
    return false;
  }

  // Check if it's a directory (by checking for package.json)
  const manifestPath = editor.pathJoin(sourcePath, "package.json");
  if (!editor.fileExists(manifestPath)) {
    editor.setStatus(`Not a valid package (no package.json): ${sourcePath}`);
    return false;
  }

  // Read manifest FIRST to determine actual package type and name
  const manifest = readJsonFile<PackageManifest>(manifestPath);
  if (!manifest) {
    editor.setStatus(`Invalid package.json in ${sourcePath}`);
    return false;
  }

  // Use manifest name as the authoritative package name
  packageName = manifest.name;

  // Determine correct target directory based on actual package type
  const actualType = manifest.type || "plugin";
  const correctPackagesDir = actualType === "plugin" ? PACKAGES_DIR
                           : actualType === "theme" ? THEMES_PACKAGES_DIR
                           : actualType === "bundle" ? BUNDLES_PACKAGES_DIR
                           : LANGUAGES_PACKAGES_DIR;
  const correctTargetDir = editor.pathJoin(correctPackagesDir, packageName);

  // Check if already installed in correct location
  if (editor.fileExists(correctTargetDir)) {
    editor.setStatus(`Package '${packageName}' is already installed`);
    return false;
  }

  // Ensure correct directory exists
  await ensureDir(correctPackagesDir);

  // Copy the directory to correct target
  editor.setStatus(`Copying from ${sourcePath}...`);
  const copyResult = await editor.spawnProcess("cp", ["-r", sourcePath, correctTargetDir]);
  if (copyResult.exit_code !== 0) {
    editor.setStatus(`Failed to copy package: ${copyResult.stderr}`);
    return false;
  }

  // Validate package structure
  const validation = validatePackage(correctTargetDir, packageName);
  if (!validation.valid) {
    editor.warn(`[pkg] Invalid package '${packageName}': ${validation.error}`);
    editor.setStatus(`Failed to install ${packageName}: ${validation.error}`);
    // Clean up the invalid package
    await editor.spawnProcess("rm", ["-rf", correctTargetDir]);
    return false;
  }

  // Store the source path for reference
  const sourceInfo = {
    local_path: sourcePath,
    original_url: parsed.subpath ? `${parsed.repoUrl}#${parsed.subpath}` : parsed.repoUrl,
    installed_at: new Date().toISOString()
  };
  await writeJsonFile(editor.pathJoin(correctTargetDir, ".fresh-source.json"), sourceInfo);

  // Dynamically load plugins, reload themes, load language packs, or load bundles
  if (manifest.type === "plugin" && validation.entryPath) {
    await editor.loadPlugin(validation.entryPath);
    editor.setStatus(`Installed and activated ${packageName} v${manifest.version || "unknown"}`);
  } else if (manifest.type === "theme") {
    editor.reloadThemes();
    editor.setStatus(`Installed theme ${packageName} v${manifest.version || "unknown"}`);
  } else if (manifest.type === "language") {
    await loadLanguagePack(correctTargetDir, manifest);
    editor.setStatus(`Installed language pack ${packageName} v${manifest.version || "unknown"}`);
  } else if (manifest.type === "bundle") {
    await loadBundle(correctTargetDir, manifest);
    editor.setStatus(`Installed bundle ${packageName} v${manifest.version || "unknown"}`);
  } else {
    editor.setStatus(`Installed ${packageName} v${manifest.version || "unknown"}`);
  }
  return true;
}

/**
 * Install from a monorepo (URL with subpath fragment)
 *
 * Strategy:
 * 1. Clone the repo to a temp directory
 * 2. Detect package type from manifest
 * 3. Copy the subdirectory to the correct target location
 * 4. Store the original URL for reference
 */
async function installFromMonorepo(
  parsed: ParsedPackageUrl,
  packageName: string,
  version?: string
): Promise<boolean> {
  const tempDir = `/tmp/fresh-pkg-${hashString(parsed.repoUrl)}-${Date.now()}`;

  try {
    // Clone the full repo to temp
    editor.setStatus(`Cloning ${parsed.repoUrl}...`);
    const cloneArgs = ["clone"];
    if (!version || version === "latest") {
      cloneArgs.push("--depth", "1");
    }
    cloneArgs.push(`${parsed.repoUrl}`, `${tempDir}`);

    const cloneResult = await gitCommand(cloneArgs);
    if (cloneResult.exit_code !== 0) {
      const errorMsg = cloneResult.stderr.includes("not found") || cloneResult.stderr.includes("404")
        ? "Repository not found"
        : cloneResult.stderr.includes("Authentication") || cloneResult.stderr.includes("403")
        ? "Access denied (repository may be private)"
        : cloneResult.stderr.split("\n")[0] || "Clone failed";
      editor.setStatus(`Failed to clone repository: ${errorMsg}`);
      return false;
    }

    // Checkout specific version if requested
    if (version && version !== "latest") {
      await checkoutVersion(tempDir, version);
    }

    // Verify subpath exists
    const subpathDir = editor.pathJoin(tempDir, parsed.subpath!);
    if (!editor.fileExists(subpathDir)) {
      editor.setStatus(`Subpath '${parsed.subpath}' not found in repository`);
      await editor.spawnProcess("rm", ["-rf", tempDir]);
      return false;
    }

    // Validate package structure (validates against subpath dir)
    const validation = validatePackage(subpathDir, packageName);
    if (!validation.valid) {
      editor.warn(`[pkg] Invalid package '${packageName}': ${validation.error}`);
      editor.setStatus(`Failed to install ${packageName}: ${validation.error}`);
      await editor.spawnProcess("rm", ["-rf", tempDir]);
      return false;
    }

    const manifest = validation.manifest;

    // Use manifest name as the authoritative package name
    if (manifest?.name) packageName = manifest.name;

    // Determine correct target directory based on actual package type
    const actualType = manifest?.type || "plugin";
    const correctPackagesDir = actualType === "plugin" ? PACKAGES_DIR
                             : actualType === "theme" ? THEMES_PACKAGES_DIR
                             : actualType === "bundle" ? BUNDLES_PACKAGES_DIR
                             : LANGUAGES_PACKAGES_DIR;
    const correctTargetDir = editor.pathJoin(correctPackagesDir, packageName);

    // Check if already installed
    if (editor.fileExists(correctTargetDir)) {
      editor.setStatus(`Package '${packageName}' is already installed`);
      await editor.spawnProcess("rm", ["-rf", tempDir]);
      return false;
    }

    // Ensure correct directory exists
    await ensureDir(correctPackagesDir);

    // Copy subdirectory to correct target
    editor.setStatus(`Installing ${packageName} from ${parsed.subpath}...`);
    const copyResult = await editor.spawnProcess("cp", ["-r", subpathDir, correctTargetDir]);
    if (copyResult.exit_code !== 0) {
      editor.setStatus(`Failed to copy package: ${copyResult.stderr}`);
      await editor.spawnProcess("rm", ["-rf", tempDir]);
      return false;
    }

    // Store the original monorepo URL in a .fresh-source file
    const sourceInfo = {
      repository: parsed.repoUrl,
      subpath: parsed.subpath,
      installed_from: `${parsed.repoUrl}#${parsed.subpath}`,
      installed_at: new Date().toISOString()
    };
    await writeJsonFile(editor.pathJoin(correctTargetDir, ".fresh-source.json"), sourceInfo);

    // Dynamically load plugins, reload themes, load language packs, or load bundles
    if (manifest?.type === "plugin" && validation.entryPath) {
      // Update entry path to new location
      const newEntryPath = validation.entryPath.replace(subpathDir, correctTargetDir);
      await editor.loadPlugin(newEntryPath);
      editor.setStatus(`Installed and activated ${packageName}${manifest ? ` v${manifest.version}` : ""}`);
    } else if (manifest?.type === "theme") {
      editor.reloadThemes();
      editor.setStatus(`Installed theme ${packageName}${manifest ? ` v${manifest.version}` : ""}`);
    } else if (manifest?.type === "language") {
      await loadLanguagePack(correctTargetDir, manifest);
      editor.setStatus(`Installed language pack ${packageName}${manifest ? ` v${manifest.version}` : ""}`);
    } else if (manifest?.type === "bundle") {
      await loadBundle(correctTargetDir, manifest);
      editor.setStatus(`Installed bundle ${packageName}${manifest ? ` v${manifest.version}` : ""}`);
    } else {
      editor.setStatus(`Installed ${packageName}${manifest ? ` v${manifest.version}` : ""}`);
    }
    return true;
  } finally {
    // Cleanup temp directory
    await editor.spawnProcess("rm", ["-rf", tempDir]);
  }
}

/**
 * Load a language pack (register grammar, language config, and LSP server)
 */
async function loadLanguagePack(packageDir: string, manifest: PackageManifest): Promise<void> {
  const langId = manifest.name;

  // Register grammar if present
  if (manifest.fresh?.grammar) {
    const grammarPath = editor.pathJoin(packageDir, manifest.fresh.grammar.file);
    const extensions = manifest.fresh.grammar.extensions || [];
    editor.registerGrammar(langId, grammarPath, extensions);
  }

  // Register language config if present
  if (manifest.fresh?.language) {
    const lang = manifest.fresh.language;
    editor.registerLanguageConfig(langId, {
      commentPrefix: lang.commentPrefix ?? null,
      blockCommentStart: lang.blockCommentStart ?? null,
      blockCommentEnd: lang.blockCommentEnd ?? null,
      useTabs: lang.useTabs ?? null,
      tabSize: lang.tabSize ?? null,
      autoIndent: lang.autoIndent ?? null,
      showWhitespaceTabs: lang.showWhitespaceTabs ?? null,
      formatter: lang.formatter ? {
        command: lang.formatter.command,
        args: lang.formatter.args ?? [],
      } : null,
    });
  }

  // Register LSP server if present
  if (manifest.fresh?.lsp) {
    const lsp = manifest.fresh.lsp;
    editor.registerLspServer(langId, {
      command: lsp.command,
      args: lsp.args ?? [],
      autoStart: lsp.autoStart ?? null,
      initializationOptions: lsp.initializationOptions ?? null,
    });
  }

  // Apply changes
  editor.reloadGrammars();
}

/**
 * Load a bundle package (register all languages and load all plugins)
 */
async function loadBundle(packageDir: string, manifest: PackageManifest): Promise<void> {
  const bundleName = manifest.name;
  editor.debug(`[pkg] Loading bundle: ${bundleName}`);

  // Load all languages from the bundle
  if (manifest.fresh?.languages) {
    for (const lang of manifest.fresh.languages) {
      const langId = lang.id;
      editor.debug(`[pkg] Loading bundle language: ${langId}`);

      // Register grammar if present
      if (lang.grammar) {
        const grammarPath = editor.pathJoin(packageDir, lang.grammar.file);
        const extensions = lang.grammar.extensions || [];
        editor.registerGrammar(langId, grammarPath, extensions);
      }

      // Register language config if present
      if (lang.language) {
        const langConfig = lang.language;
        editor.registerLanguageConfig(langId, {
          commentPrefix: langConfig.commentPrefix ?? null,
          blockCommentStart: langConfig.blockCommentStart ?? null,
          blockCommentEnd: langConfig.blockCommentEnd ?? null,
          useTabs: langConfig.useTabs ?? null,
          tabSize: langConfig.tabSize ?? null,
          autoIndent: langConfig.autoIndent ?? null,
          showWhitespaceTabs: langConfig.showWhitespaceTabs ?? null,
          formatter: langConfig.formatter ? {
            command: langConfig.formatter.command,
            args: langConfig.formatter.args ?? [],
          } : null,
        });
      }

      // Register LSP server if present
      if (lang.lsp) {
        const lsp = lang.lsp;
        editor.registerLspServer(langId, {
          command: lsp.command,
          args: lsp.args ?? [],
          autoStart: lsp.autoStart ?? null,
          initializationOptions: lsp.initializationOptions ?? null,
        });
      }
    }
  }

  // Load all plugins from the bundle
  if (manifest.fresh?.plugins) {
    for (const plugin of manifest.fresh.plugins) {
      let entryPath = editor.pathJoin(packageDir, plugin.entry);

      // Try .js fallback if .ts doesn't exist
      if (!editor.fileExists(entryPath) && entryPath.endsWith(".ts")) {
        const jsPath = entryPath.replace(/\.ts$/, ".js");
        if (editor.fileExists(jsPath)) {
          entryPath = jsPath;
        }
      }

      if (editor.fileExists(entryPath)) {
        editor.debug(`[pkg] Loading bundle plugin: ${plugin.entry}`);
        await editor.loadPlugin(entryPath);
      } else {
        editor.warn(`[pkg] Bundle plugin not found: ${plugin.entry}`);
      }
    }
  }

  // Reload themes if bundle contains any (uses same format as theme-packs)
  if (manifest.fresh?.themes && manifest.fresh.themes.length > 0) {
    editor.debug(`[pkg] Bundle contains ${manifest.fresh.themes.length} theme(s), reloading themes`);
    editor.reloadThemes();
  }

  // Apply grammar changes
  editor.reloadGrammars();
  editor.debug(`[pkg] Bundle loaded: ${bundleName}`);
}

/**
 * Checkout a specific version in a package directory
 */
async function checkoutVersion(pkgPath: string, version: string): Promise<boolean> {
  let target: string;

  if (version === "latest") {
    // Get latest tag
    const tagsResult = await gitCommand(["-C", `${pkgPath}`, "tag", "--sort=-v:refname"]);
    const tags = tagsResult.stdout.split("\n").filter(t => t.trim());
    target = tags[0] || "HEAD";
  } else if (version.startsWith("^") || version.startsWith("~")) {
    // Semver matching - find best matching tag
    target = await findMatchingSemver(pkgPath, version);
  } else if (version.match(/^[0-9a-f]{7,40}$/)) {
    // Commit hash
    target = version;
  } else {
    // Exact version or branch
    target = version.startsWith("v") ? version : `v${version}`;
  }

  // Fetch if needed
  await gitCommand(["-C", `${pkgPath}`, "fetch", "--tags"]);

  // Checkout
  const result = await gitCommand(["-C", `${pkgPath}`, "checkout", target]);
  return result.exit_code === 0;
}

/**
 * Find best semver matching version
 */
async function findMatchingSemver(pkgPath: string, spec: string): Promise<string> {
  const tagsResult = await gitCommand(["-C", `${pkgPath}`, "tag", "--sort=-v:refname"]);
  const tags = tagsResult.stdout.split("\n").filter(t => t.trim());

  // Simple semver matching (^ means compatible, ~ means patch only)
  const prefix = spec.startsWith("^") ? "^" : "~";
  const baseVersion = spec.slice(1);
  const [major, minor] = baseVersion.split(".").map(n => parseInt(n, 10));

  for (const tag of tags) {
    const version = tag.replace(/^v/, "");
    const [tagMajor, tagMinor] = version.split(".").map(n => parseInt(n, 10));

    if (prefix === "^") {
      // Compatible: same major
      if (tagMajor === major && !isNaN(tagMinor)) {
        return tag;
      }
    } else {
      // Patch: same major.minor
      if (tagMajor === major && tagMinor === minor) {
        return tag;
      }
    }
  }

  // Fallback to latest
  return tags[0] || "HEAD";
}

/**
 * Update a package
 */
async function updatePackage(pkg: InstalledPackage): Promise<boolean> {
  editor.setStatus(`Updating ${pkg.name}...`);

  const result = await gitCommand(["-C", `${pkg.path}`, "pull", "--ff-only"]);

  if (result.exit_code === 0) {
    if (result.stdout.includes("Already up to date")) {
      editor.setStatus(`${pkg.name} is already up to date`);
    } else {
      // Reload the plugin to apply changes
      // Use listPlugins to find the correct runtime plugin name
      if (pkg.type === "plugin") {
        const loadedPlugins = await editor.listPlugins();
        const plugin = loadedPlugins.find((p: { path: string }) => p.path.startsWith(pkg.path));
        if (plugin) {
          await editor.reloadPlugin(plugin.name);
        }
      } else if (pkg.type === "theme") {
        editor.reloadThemes();
      }
      editor.setStatus(`Updated and reloaded ${pkg.name}`);
    }
    return true;
  } else {
    const errorMsg = result.stderr.includes("Could not resolve host")
      ? "Network error"
      : result.stderr.includes("Authentication") || result.stderr.includes("403")
      ? "Authentication failed"
      : result.stderr.split("\n")[0] || "Update failed";
    editor.setStatus(`Failed to update ${pkg.name}: ${errorMsg}`);
    return false;
  }
}

/**
 * Reinstall a package from its original local path.
 * Removes the installed copy and re-copies from the source directory.
 */
async function reinstallPackage(pkg: InstalledPackage): Promise<boolean> {
  if (!pkg.localSource) {
    editor.setStatus(`Cannot reinstall ${pkg.name}: no local source path`);
    return false;
  }

  const sourcePath = pkg.localSource;

  if (!editor.fileExists(sourcePath)) {
    editor.setStatus(`Source path no longer exists: ${sourcePath}`);
    return false;
  }

  editor.setStatus(`Reinstalling ${pkg.name} from ${sourcePath}...`);

  // Unload plugin first if applicable
  if (pkg.type === "plugin") {
    const loadedPlugins = await editor.listPlugins();
    const plugin = loadedPlugins.find((p: { path: string }) => p.path.startsWith(pkg.path));
    if (plugin) {
      await editor.unloadPlugin(plugin.name).catch(() => {});
    }
  }

  // Remove old copy
  const rmResult = await editor.spawnProcess("rm", ["-rf", pkg.path]);
  if (rmResult.exit_code !== 0) {
    editor.setStatus(`Failed to remove old copy: ${rmResult.stderr}`);
    return false;
  }

  // Re-copy from source
  const copyResult = await editor.spawnProcess("cp", ["-r", sourcePath, pkg.path]);
  if (copyResult.exit_code !== 0) {
    editor.setStatus(`Failed to copy from source: ${copyResult.stderr}`);
    return false;
  }

  // Re-write the .fresh-source.json marker
  const sourceInfo = {
    local_path: sourcePath,
    original_url: pkg.source,
    installed_at: new Date().toISOString()
  };
  await writeJsonFile(editor.pathJoin(pkg.path, ".fresh-source.json"), sourceInfo);

  // Re-read manifest for validation and reload
  const validation = validatePackage(pkg.path, pkg.name);
  if (!validation.valid) {
    editor.setStatus(`Reinstalled ${pkg.name} but package is invalid: ${validation.error}`);
    return false;
  }

  const manifest = validation.manifest;

  // Reload
  if (manifest?.type === "plugin" && validation.entryPath) {
    await editor.loadPlugin(validation.entryPath);
    editor.setStatus(`Reinstalled and activated ${pkg.name}`);
  } else if (manifest?.type === "theme") {
    editor.reloadThemes();
    editor.setStatus(`Reinstalled theme ${pkg.name}`);
  } else if (manifest?.type === "language") {
    await loadLanguagePack(pkg.path, manifest);
    editor.setStatus(`Reinstalled language pack ${pkg.name}`);
  } else if (manifest?.type === "bundle") {
    await loadBundle(pkg.path, manifest);
    editor.setStatus(`Reinstalled bundle ${pkg.name}`);
  } else {
    editor.setStatus(`Reinstalled ${pkg.name}`);
  }

  return true;
}

/**
 * Remove a package
 */
async function removePackage(pkg: InstalledPackage): Promise<boolean> {
  editor.setStatus(`Removing ${pkg.name}...`);

  // Unload the plugin first (ignore errors - plugin might not be loaded)
  // Use listPlugins to find the correct runtime plugin name by matching path
  if (pkg.type === "plugin") {
    const loadedPlugins = await editor.listPlugins();
    const plugin = loadedPlugins.find((p: { path: string }) => p.path.startsWith(pkg.path));
    if (plugin) {
      await editor.unloadPlugin(plugin.name).catch(() => {});
    }
  }

  // Use trash if available, otherwise rm -rf
  let result = await editor.spawnProcess("trash", [pkg.path]);
  if (result.exit_code !== 0) {
    result = await editor.spawnProcess("rm", ["-rf", pkg.path]);
  }

  if (result.exit_code === 0) {
    // Reload themes if we removed a theme so Select Theme list is updated
    if (pkg.type === "theme") {
      editor.reloadThemes();
    }
    editor.setStatus(`Removed ${pkg.name}`);
    return true;
  } else {
    editor.setStatus(`Failed to remove ${pkg.name}: ${result.stderr}`);
    return false;
  }
}

/**
 * Update all packages
 */
async function updateAllPackages(): Promise<void> {
  const plugins = getInstalledPackages("plugin");
  const themes = getInstalledPackages("theme");
  const all = [...plugins, ...themes];

  if (all.length === 0) {
    editor.setStatus("No packages installed");
    return;
  }

  let updated = 0;
  let failed = 0;

  for (const pkg of all) {
    editor.setStatus(`Updating ${pkg.name} (${updated + failed + 1}/${all.length})...`);

    if (pkg.localSource) {
      // Local packages: reinstall from source path
      const ok = await reinstallPackage(pkg);
      if (ok) {
        updated++;
      } else {
        failed++;
      }
    } else {
      const result = await gitCommand(["-C", `${pkg.path}`, "pull", "--ff-only"]);
      if (result.exit_code === 0) {
        if (!result.stdout.includes("Already up to date")) {
          updated++;
        }
      } else {
        failed++;
      }
    }
  }

  editor.setStatus(`Update complete: ${updated} updated, ${all.length - updated - failed} unchanged, ${failed} failed`);
}

// =============================================================================
// Lockfile Operations
// =============================================================================

/**
 * Generate lockfile from current state
 */
async function generateLockfile(): Promise<void> {
  editor.setStatus("Generating lockfile...");

  const plugins = getInstalledPackages("plugin");
  const themes = getInstalledPackages("theme");
  const all = [...plugins, ...themes];

  const lockfile: Lockfile = {
    lockfile_version: 1,
    generated: new Date().toISOString(),
    packages: {}
  };

  for (const pkg of all) {
    // Get current commit
    const commitResult = await gitCommand(["-C", `${pkg.path}`, "rev-parse", "HEAD"]);
    const commit = commitResult.stdout.trim();

    lockfile.packages[pkg.name] = {
      source: pkg.source,
      commit,
      version: pkg.version
    };
  }

  if (await writeJsonFile(LOCKFILE_PATH, lockfile)) {
    editor.setStatus(`Lockfile generated with ${all.length} packages`);
  } else {
    editor.setStatus("Failed to write lockfile");
  }
}

/**
 * Install packages from lockfile
 */
async function installFromLockfile(): Promise<void> {
  const lockfile = readJsonFile<Lockfile>(LOCKFILE_PATH);
  if (!lockfile) {
    editor.setStatus("No lockfile found");
    return;
  }

  editor.setStatus("Installing from lockfile...");

  let installed = 0;
  let failed = 0;

  for (const [name, entry] of Object.entries(lockfile.packages)) {
    editor.setStatus(`Installing ${name} (${installed + failed + 1}/${Object.keys(lockfile.packages).length})...`);

    // Check if already installed
    const pluginPath = editor.pathJoin(PACKAGES_DIR, name);
    const themePath = editor.pathJoin(THEMES_PACKAGES_DIR, name);

    if (editor.fileExists(pluginPath) || editor.fileExists(themePath)) {
      // Already installed, just checkout the commit
      const path = editor.fileExists(pluginPath) ? pluginPath : themePath;
      await gitCommand(["-C", `${path}`, "fetch"]);
      const result = await gitCommand(["-C", `${path}`, "checkout", entry.commit]);
      if (result.exit_code === 0) {
        installed++;
      } else {
        failed++;
      }
    } else {
      // Need to clone
      await ensureDir(PACKAGES_DIR);
      const result = await gitCommand(["clone", `${entry.source}`, `${pluginPath}`]);

      if (result.exit_code === 0) {
        await gitCommand(["-C", `${pluginPath}`, "checkout", entry.commit]);
        installed++;
      } else {
        failed++;
      }
    }
  }

  editor.setStatus(`Lockfile install complete: ${installed} installed, ${failed} failed`);
}

// =============================================================================
// Package Manager UI (VSCode-style virtual buffer)
// =============================================================================

// UI State
interface PackageListItem {
  type: "installed" | "available";
  name: string;
  description: string;
  version: string;
  installed: boolean;
  updateAvailable: boolean;
  latestVersion?: string;
  author?: string;
  license?: string;
  repository?: string;
  stars?: number;
  downloads?: number;
  keywords?: string[];
  packageType: "plugin" | "theme" | "language" | "bundle";
  // For installed packages
  installedPackage?: InstalledPackage;
  // For available packages
  registryEntry?: RegistryEntry;
}

// Focus target types for Tab navigation
type FocusTarget =
  | { type: "filter"; index: number }  // 0=All, 1=Installed, 2=Plugins, 3=Themes, 4=Languages, 5=Bundles
  | { type: "sync" }
  | { type: "search" }
  | { type: "list" }  // Package list (use arrows to navigate)
  | { type: "action"; index: number };  // Action buttons for selected package

interface PkgManagerState {
  isOpen: boolean;
  bufferId: number | null;
  splitId: number | null;
  sourceBufferId: number | null;
  filter: "all" | "installed" | "plugins" | "themes" | "languages" | "bundles";
  searchQuery: string;
  items: PackageListItem[];
  selectedIndex: number;
  focus: FocusTarget;  // What element has Tab focus
  isLoading: boolean;
}

const pkgState: PkgManagerState = {
  isOpen: false,
  bufferId: null,
  splitId: null,
  sourceBufferId: null,
  filter: "all",
  searchQuery: "",
  items: [],
  selectedIndex: 0,
  focus: { type: "list" },
  isLoading: false,
};

// Theme-aware color configuration
// Maps UI elements to theme keys with RGB fallbacks
interface ThemeColor {
  fg?: { theme?: string; rgb: [number, number, number] };
  bg?: { theme?: string; rgb: [number, number, number] };
}

const pkgTheme: Record<string, ThemeColor> = {
  // Headers and titles
  header: { fg: { theme: "syntax.keyword", rgb: [100, 180, 255] } },
  sectionTitle: { fg: { theme: "syntax.function", rgb: [180, 140, 80] } },

  // Package items
  installed: { fg: { theme: "syntax.string", rgb: [100, 200, 120] } },
  available: { fg: { theme: "editor.fg", rgb: [200, 200, 210] } },
  selected: {
    fg: { theme: "ui.menu_active_fg", rgb: [255, 255, 255] },
    bg: { theme: "ui.menu_active_bg", rgb: [50, 80, 120] }
  },

  // Descriptions and details
  description: { fg: { theme: "syntax.comment", rgb: [140, 140, 150] } },
  infoRow: { fg: { theme: "editor.fg", rgb: [180, 180, 190] } },
  infoLabel: { fg: { theme: "syntax.comment", rgb: [120, 120, 130] } },
  infoValue: { fg: { theme: "editor.fg", rgb: [200, 200, 210] } },

  // UI elements
  separator: { fg: { rgb: [60, 60, 65] } },
  divider: { fg: { rgb: [50, 50, 55] } },
  help: { fg: { theme: "syntax.comment", rgb: [100, 100, 110] } },
  emptyState: { fg: { theme: "syntax.comment", rgb: [120, 120, 130] } },

  // Filter buttons
  filterActive: {
    fg: { rgb: [255, 255, 255] },
    bg: { theme: "syntax.keyword", rgb: [60, 100, 160] }
  },
  filterInactive: {
    fg: { rgb: [160, 160, 170] },
  },
  filterFocused: {
    fg: { rgb: [255, 255, 255] },
    bg: { rgb: [80, 80, 90] }
  },

  // Action buttons
  button: {
    fg: { rgb: [180, 180, 190] },
  },
  buttonFocused: {
    fg: { rgb: [255, 255, 255] },
    bg: { theme: "syntax.keyword", rgb: [60, 110, 180] }
  },

  // Search box - distinct input field appearance
  searchBox: {
    fg: { rgb: [200, 200, 210] },
    bg: { rgb: [40, 42, 48] }
  },
  searchBoxFocused: {
    fg: { rgb: [255, 255, 255] },
    bg: { theme: "syntax.keyword", rgb: [60, 110, 180] }
  },

  // Status indicators
  statusOk: { fg: { rgb: [100, 200, 120] } },
  statusUpdate: { fg: { rgb: [220, 180, 80] } },
};

// Define pkg-manager mode with arrow key navigation
editor.defineMode(
  "pkg-manager",
  "normal",
  [
    ["Up", "pkg_nav_up"],
    ["Down", "pkg_nav_down"],
    ["Return", "pkg_activate"],
    ["Tab", "pkg_next_button"],
    ["S-Tab", "pkg_prev_button"],
    ["Escape", "pkg_back_or_close"],
    ["/", "pkg_search"],
  ],
  true // read-only
);

// Define pkg-detail mode for package details view
editor.defineMode(
  "pkg-detail",
  "normal",
  [
    ["Up", "pkg_scroll_up"],
    ["Down", "pkg_scroll_down"],
    ["Return", "pkg_activate"],
    ["Tab", "pkg_next_button"],
    ["S-Tab", "pkg_prev_button"],
    ["Escape", "pkg_back_or_close"],
  ],
  true // read-only
);

/**
 * Build package list from installed and registry data
 */
function buildPackageList(): PackageListItem[] {
  const items: PackageListItem[] = [];

  // Get installed packages
  const installedPlugins = getInstalledPackages("plugin");
  const installedThemes = getInstalledPackages("theme");
  const installedLanguages = getInstalledPackages("language");
  const installedBundles = getInstalledPackages("bundle");
  const installedMap = new Map<string, InstalledPackage>();

  for (const pkg of [...installedPlugins, ...installedThemes, ...installedLanguages, ...installedBundles]) {
    installedMap.set(pkg.name, pkg);
    items.push({
      type: "installed",
      name: pkg.name,
      description: pkg.manifest?.description || "No description",
      version: pkg.version,
      installed: true,
      updateAvailable: false, // TODO: Check for updates
      author: pkg.manifest?.author,
      license: pkg.manifest?.license,
      repository: pkg.source,
      packageType: pkg.type,
      installedPackage: pkg,
    });
  }

  // Get available packages from registry
  if (isRegistrySynced()) {
    const pluginRegistry = loadRegistry("plugins");
    const themeRegistry = loadRegistry("themes");

    for (const [name, entry] of Object.entries(pluginRegistry.packages)) {
      if (!installedMap.has(name)) {
        items.push({
          type: "available",
          name,
          description: entry.description || "No description",
          version: entry.latest_version || "latest",
          installed: false,
          updateAvailable: false,
          latestVersion: entry.latest_version,
          author: entry.author,
          license: entry.license,
          repository: entry.repository,
          stars: entry.stars,
          downloads: entry.downloads,
          keywords: entry.keywords,
          packageType: "plugin",
          registryEntry: entry,
        });
      }
    }

    for (const [name, entry] of Object.entries(themeRegistry.packages)) {
      if (!installedMap.has(name)) {
        items.push({
          type: "available",
          name,
          description: entry.description || "No description",
          version: entry.latest_version || "latest",
          installed: false,
          updateAvailable: false,
          latestVersion: entry.latest_version,
          author: entry.author,
          license: entry.license,
          repository: entry.repository,
          stars: entry.stars,
          downloads: entry.downloads,
          keywords: entry.keywords,
          packageType: "theme",
          registryEntry: entry,
        });
      }
    }

    // Add language packages from registry
    const languageRegistry = loadRegistry("languages");
    for (const [name, entry] of Object.entries(languageRegistry.packages)) {
      if (!installedMap.has(name)) {
        items.push({
          type: "available",
          name,
          description: entry.description || "No description",
          version: entry.latest_version || "latest",
          installed: false,
          updateAvailable: false,
          latestVersion: entry.latest_version,
          author: entry.author,
          license: entry.license,
          repository: entry.repository,
          stars: entry.stars,
          downloads: entry.downloads,
          keywords: entry.keywords,
          packageType: "language",
          registryEntry: entry,
        });
      }
    }
  }

  return items;
}

/**
 * Filter items based on current filter and search query
 */
function getFilteredItems(): PackageListItem[] {
  let items = pkgState.items;

  // Apply filter
  switch (pkgState.filter) {
    case "installed":
      items = items.filter(i => i.installed);
      break;
    case "plugins":
      items = items.filter(i => i.packageType === "plugin");
      break;
    case "themes":
      items = items.filter(i => i.packageType === "theme");
      break;
    case "languages":
      items = items.filter(i => i.packageType === "language");
      break;
    case "bundles":
      items = items.filter(i => i.packageType === "bundle");
      break;
  }

  // Apply search (case insensitive)
  if (pkgState.searchQuery) {
    const query = pkgState.searchQuery.toLowerCase();
    items = items.filter(i =>
      i.name.toLowerCase().includes(query) ||
      (i.description && i.description.toLowerCase().includes(query)) ||
      (i.keywords && i.keywords.some(k => k.toLowerCase().includes(query)))
    );
  }

  // Sort: installed first, then by name
  items.sort((a, b) => {
    if (a.installed !== b.installed) {
      return a.installed ? -1 : 1;
    }
    return a.name.localeCompare(b.name);
  });

  return items;
}

/**
 * Format number with K/M suffix
 */
function formatNumber(n: number | undefined): string {
  if (n === undefined) return "";
  if (n >= 1000000) return (n / 1000000).toFixed(1) + "M";
  if (n >= 1000) return (n / 1000).toFixed(1) + "k";
  return n.toString();
}

// Layout constants
const LIST_WIDTH = 36;  // Width of left panel (package list)
const TOTAL_WIDTH = 88; // Total width of UI
const DETAIL_WIDTH = TOTAL_WIDTH - LIST_WIDTH - 3; // Right panel width (minus divider)

/**
 * Helper to check if a button is focused
 */
function isButtonFocused(type: FocusTarget["type"], index?: number): boolean {
  if (pkgState.focus.type !== type) return false;
  if (index !== undefined && "index" in pkgState.focus) {
    return pkgState.focus.index === index;
  }
  return true;
}

/**
 * Get action buttons for the selected package
 */
function getActionButtons(): string[] {
  const items = getFilteredItems();
  if (items.length === 0 || pkgState.selectedIndex >= items.length) return [];
  const item = items[pkgState.selectedIndex];

  if (item.installed) {
    if (item.installedPackage?.localSource) {
      return ["Reinstall", "Uninstall"];
    }
    return item.updateAvailable ? ["Update", "Uninstall"] : ["Uninstall"];
  } else {
    return ["Install"];
  }
}

/**
 * Word-wrap text to fit within a given width
 */
function wrapText(text: string, maxWidth: number): string[] {
  const words = text.split(/\s+/);
  const lines: string[] = [];
  let currentLine = "";

  for (const word of words) {
    if (currentLine.length + word.length + 1 <= maxWidth) {
      currentLine += (currentLine ? " " : "") + word;
    } else {
      if (currentLine) lines.push(currentLine);
      currentLine = word.length > maxWidth ? word.slice(0, maxWidth - 1) + "…" : word;
    }
  }
  if (currentLine) lines.push(currentLine);
  return lines.length > 0 ? lines : [""];
}

/**
 * Build virtual buffer entries for the package manager (split-view layout)
 */
function buildListViewEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];
  const items = getFilteredItems();
  const selectedItem = items.length > 0 && pkgState.selectedIndex < items.length
    ? items[pkgState.selectedIndex] : null;
  const installedItems = items.filter(i => i.installed);
  const availableItems = items.filter(i => !i.installed);

  // === HEADER ===
  entries.push({
    text: " Packages\n",
    properties: { type: "header" },
  });

  // Empty line after header
  entries.push({ text: "\n", properties: { type: "blank" } });

  // === SEARCH BAR (input-style) ===
  const searchFocused = isButtonFocused("search");
  const searchInputWidth = 30;
  const searchText = pkgState.searchQuery || "";
  const searchDisplay = searchText.length > searchInputWidth - 1
    ? searchText.slice(0, searchInputWidth - 2) + "…"
    : searchText.padEnd(searchInputWidth);

  entries.push({ text: " Search: ", properties: { type: "search-label" } });
  entries.push({
    text: searchFocused ? `[${searchDisplay}]` : ` ${searchDisplay} `,
    properties: { type: "search-input", focused: searchFocused },
  });
  entries.push({ text: "\n", properties: { type: "newline" } });

  // === FILTER BAR with focusable buttons ===
  const filters: Array<{ id: string; label: string }> = [
    { id: "all", label: "All" },
    { id: "installed", label: "Installed" },
    { id: "plugins", label: "Plugins" },
    { id: "themes", label: "Themes" },
    { id: "languages", label: "Languages" },
    { id: "bundles", label: "Bundles" },
  ];

  // Build filter buttons with position tracking
  let filterBarParts: Array<{ text: string; type: string; focused?: boolean; active?: boolean }> = [];
  filterBarParts.push({ text: " ", type: "spacer" });

  for (let i = 0; i < filters.length; i++) {
    const f = filters[i];
    const isActive = pkgState.filter === f.id;
    const isFocused = isButtonFocused("filter", i);
    // Always reserve space for brackets - show [ ] when focused, spaces when not
    const leftBracket = isFocused ? "[" : " ";
    const rightBracket = isFocused ? "]" : " ";
    filterBarParts.push({
      text: `${leftBracket} ${f.label} ${rightBracket}`,
      type: "filter-btn",
      focused: isFocused,
      active: isActive,
    });
  }

  filterBarParts.push({ text: "  ", type: "spacer" });

  // Sync button - always reserve space for brackets
  const syncFocused = isButtonFocused("sync");
  const syncLeft = syncFocused ? "[" : " ";
  const syncRight = syncFocused ? "]" : " ";
  filterBarParts.push({ text: `${syncLeft} Sync ${syncRight}`, type: "sync-btn", focused: syncFocused });

  // Emit each filter bar part as separate entry for individual styling
  for (const part of filterBarParts) {
    entries.push({
      text: part.text,
      properties: {
        type: part.type,
        focused: part.focused,
        active: part.active,
      },
    });
  }
  entries.push({ text: "\n", properties: { type: "newline" } });

  // === TOP SEPARATOR ===
  entries.push({
    text: " " + "─".repeat(TOTAL_WIDTH - 2) + "\n",
    properties: { type: "separator" },
  });

  // === SPLIT VIEW: Package list on left, Details on right ===

  // Build left panel lines (package list)
  const leftLines: Array<{ text: string; type: string; selected?: boolean; installed?: boolean }> = [];

  // Installed section
  if (installedItems.length > 0) {
    leftLines.push({ text: `INSTALLED (${installedItems.length})`, type: "section-title" });

    let idx = 0;
    for (const item of installedItems) {
      const isSelected = idx === pkgState.selectedIndex;
      const listFocused = pkgState.focus.type === "list";
      const prefix = isSelected && listFocused ? "▸" : " ";
      const status = item.updateAvailable ? "↑" : "✓";
      const ver = item.version.length > 7 ? item.version.slice(0, 6) + "…" : item.version;
      const name = item.name.length > 18 ? item.name.slice(0, 17) + "…" : item.name;
      const line = `${prefix} ${name.padEnd(18)} ${ver.padEnd(7)} ${status}`;
      leftLines.push({ text: line, type: "package-row", selected: isSelected, installed: true });
      idx++;
    }
  }

  // Available section
  if (availableItems.length > 0) {
    if (leftLines.length > 0) leftLines.push({ text: "", type: "blank" });
    leftLines.push({ text: `AVAILABLE (${availableItems.length})`, type: "section-title" });

    let idx = installedItems.length;
    for (const item of availableItems) {
      const isSelected = idx === pkgState.selectedIndex;
      const listFocused = pkgState.focus.type === "list";
      const prefix = isSelected && listFocused ? "▸" : " ";
      const typeTag = item.packageType === "theme" ? "T" : item.packageType === "language" ? "L" : item.packageType === "bundle" ? "B" : "P";
      const name = item.name.length > 22 ? item.name.slice(0, 21) + "…" : item.name;
      const line = `${prefix} ${name.padEnd(22)} [${typeTag}]`;
      leftLines.push({ text: line, type: "package-row", selected: isSelected, installed: false });
      idx++;
    }
  }

  // Empty state for left panel
  if (items.length === 0) {
    if (pkgState.isLoading) {
      leftLines.push({ text: "Loading...", type: "empty-state" });
    } else if (!isRegistrySynced()) {
      leftLines.push({ text: "Registry not synced", type: "empty-state" });
      leftLines.push({ text: "Tab to Sync button", type: "empty-state" });
    } else {
      leftLines.push({ text: "No packages found", type: "empty-state" });
    }
  }

  // Build right panel lines (details for selected package)
  const rightLines: Array<{ text: string; type: string; focused?: boolean; btnIndex?: number }> = [];

  if (selectedItem) {
    // Package name
    rightLines.push({ text: selectedItem.name, type: "detail-title" });
    rightLines.push({ text: "─".repeat(Math.min(selectedItem.name.length + 2, DETAIL_WIDTH - 2)), type: "detail-sep" });

    // Version / Author / License on one line
    let metaLine = `v${selectedItem.version}`;
    if (selectedItem.author) metaLine += ` • ${selectedItem.author}`;
    if (selectedItem.license) metaLine += ` • ${selectedItem.license}`;
    if (metaLine.length > DETAIL_WIDTH - 2) metaLine = metaLine.slice(0, DETAIL_WIDTH - 5) + "...";
    rightLines.push({ text: metaLine, type: "detail-meta" });

    rightLines.push({ text: "", type: "blank" });

    // Description (wrapped)
    const descText = selectedItem.description || "No description available";
    const descLines = wrapText(descText, DETAIL_WIDTH - 2);
    for (const line of descLines) {
      rightLines.push({ text: line, type: "detail-desc" });
    }

    rightLines.push({ text: "", type: "blank" });

    // Keywords
    if (selectedItem.keywords && selectedItem.keywords.length > 0) {
      const kwText = selectedItem.keywords.slice(0, 4).join(", ");
      rightLines.push({ text: `Tags: ${kwText}`, type: "detail-tags" });
      rightLines.push({ text: "", type: "blank" });
    }

    // Repository URL
    if (selectedItem.repository) {
      // Shorten URL for display (remove protocol, truncate if needed)
      let displayUrl = selectedItem.repository
        .replace(/^https?:\/\//, "")
        .replace(/\.git$/, "");
      if (displayUrl.length > DETAIL_WIDTH - 2) {
        displayUrl = displayUrl.slice(0, DETAIL_WIDTH - 5) + "...";
      }
      rightLines.push({ text: displayUrl, type: "detail-url" });
      rightLines.push({ text: "", type: "blank" });
    }

    // Action buttons - always reserve space for brackets
    const actions = getActionButtons();
    for (let i = 0; i < actions.length; i++) {
      const focused = isButtonFocused("action", i);
      const leftBracket = focused ? "[" : " ";
      const rightBracket = focused ? "]" : " ";
      const btnText = `${leftBracket} ${actions[i]} ${rightBracket}`;
      rightLines.push({ text: btnText, type: "action-btn", focused, btnIndex: i });
    }
  } else {
    rightLines.push({ text: "Select a package", type: "empty-state" });
    rightLines.push({ text: "to view details", type: "empty-state" });
  }

  // Merge left and right panels into rows
  const maxRows = Math.max(leftLines.length, rightLines.length, 8);
  for (let i = 0; i < maxRows; i++) {
    const leftItem = leftLines[i];
    const rightItem = rightLines[i];

    // Left side (padded to fixed width)
    const leftText = leftItem ? (" " + leftItem.text) : "";
    entries.push({
      text: leftText.padEnd(LIST_WIDTH),
      properties: {
        type: leftItem?.type || "blank",
        selected: leftItem?.selected,
        installed: leftItem?.installed,
      },
    });

    // Divider
    entries.push({ text: "│", properties: { type: "divider" } });

    // Right side
    const rightText = rightItem ? (" " + rightItem.text) : "";
    entries.push({
      text: rightText,
      properties: {
        type: rightItem?.type || "blank",
        focused: rightItem?.focused,
        btnIndex: rightItem?.btnIndex,
      },
    });

    entries.push({ text: "\n", properties: { type: "newline" } });
  }

  // === BOTTOM SEPARATOR ===
  entries.push({
    text: " " + "─".repeat(TOTAL_WIDTH - 2) + "\n",
    properties: { type: "separator" },
  });

  // === HELP LINE ===
  let helpText = " ↑↓ Navigate  Tab Next  / Search  Enter ";
  if (pkgState.focus.type === "action") {
    helpText += "Activate";
  } else if (pkgState.focus.type === "filter") {
    helpText += "Filter";
  } else if (pkgState.focus.type === "sync") {
    helpText += "Sync";
  } else if (pkgState.focus.type === "search") {
    helpText += "Search";
  } else {
    helpText += "Select";
  }
  helpText += "  Esc Close\n";

  entries.push({
    text: helpText,
    properties: { type: "help" },
  });

  return entries;
}

/**
 * Calculate UTF-8 byte length of a string.
 * Needed because string.length returns character count, not byte count.
 * Unicode chars like ▸ and ─ are 1 char but 3 bytes in UTF-8.
 */
function utf8ByteLength(str: string): number {
  let bytes = 0;
  for (let i = 0; i < str.length; i++) {
    const code = str.charCodeAt(i);
    if (code < 0x80) {
      bytes += 1;
    } else if (code < 0x800) {
      bytes += 2;
    } else if (code >= 0xD800 && code <= 0xDBFF) {
      // Surrogate pair = 4 bytes, skip low surrogate
      bytes += 4;
      i++;
    } else {
      bytes += 3;
    }
  }
  return bytes;
}

/**
 * Apply theme-aware highlighting to the package manager view
 */
function applyPkgManagerHighlighting(): void {
  if (pkgState.bufferId === null) return;

  // Clear existing overlays
  editor.clearNamespace(pkgState.bufferId, "pkg");

  const entries = buildListViewEntries();
  let byteOffset = 0;

  for (const entry of entries) {
    const props = entry.properties as Record<string, unknown>;
    const len = utf8ByteLength(entry.text);

    // Determine theme colors based on entry type
    let themeStyle: ThemeColor | null = null;

    switch (props.type) {
      case "header":
        themeStyle = pkgTheme.header;
        break;

      case "section-title":
        themeStyle = pkgTheme.sectionTitle;
        break;

      case "filter-btn":
        if (props.focused && props.active) {
          // Both focused and active - use focused style
          themeStyle = pkgTheme.buttonFocused;
        } else if (props.focused) {
          // Only focused (not the active filter)
          themeStyle = pkgTheme.filterFocused;
        } else if (props.active) {
          // Active filter but not focused
          themeStyle = pkgTheme.filterActive;
        } else {
          themeStyle = pkgTheme.filterInactive;
        }
        break;

      case "sync-btn":
        themeStyle = props.focused ? pkgTheme.buttonFocused : pkgTheme.button;
        break;

      case "search-label":
        themeStyle = pkgTheme.infoLabel;
        break;

      case "search-input":
        // Search input field styling - distinct background
        themeStyle = props.focused ? pkgTheme.searchBoxFocused : pkgTheme.searchBox;
        break;

      case "package-row":
        if (props.selected) {
          themeStyle = pkgTheme.selected;
        } else if (props.installed) {
          themeStyle = pkgTheme.installed;
        } else {
          themeStyle = pkgTheme.available;
        }
        break;

      case "detail-title":
        themeStyle = pkgTheme.header;
        break;

      case "detail-sep":
      case "separator":
        themeStyle = pkgTheme.separator;
        break;

      case "divider":
        themeStyle = pkgTheme.divider;
        break;

      case "detail-meta":
      case "detail-tags":
      case "detail-url":
        themeStyle = pkgTheme.infoLabel;
        break;

      case "detail-desc":
        themeStyle = pkgTheme.description;
        break;

      case "action-btn":
        themeStyle = props.focused ? pkgTheme.buttonFocused : pkgTheme.button;
        break;

      case "help":
        themeStyle = pkgTheme.help;
        break;

      case "empty-state":
        themeStyle = pkgTheme.emptyState;
        break;
    }

    if (themeStyle) {
      const fg = themeStyle.fg;
      const bg = themeStyle.bg;

      // Build overlay options - prefer theme keys, fallback to RGB
      const options: Record<string, unknown> = {};

      if (fg?.theme) {
        options.fg = fg.theme;
      } else if (fg?.rgb) {
        options.fg = fg.rgb;
      }

      if (bg?.theme) {
        options.bg = bg.theme;
      } else if (bg?.rgb) {
        options.bg = bg.rgb;
      }

      if (Object.keys(options).length > 0) {
        editor.addOverlay(
          pkgState.bufferId,
          "pkg",
          byteOffset,
          byteOffset + len,
          options
        );
      }
    }

    byteOffset += len;
  }
}

/**
 * Update the package manager view
 */
function updatePkgManagerView(): void {
  if (pkgState.bufferId === null) return;

  const entries = buildListViewEntries();
  editor.setVirtualBufferContent(pkgState.bufferId, entries);
  applyPkgManagerHighlighting();
}

/**
 * Open the package manager
 */
async function openPackageManager(): Promise<void> {
  if (pkgState.isOpen) {
    // Already open, just focus it
    if (pkgState.bufferId !== null) {
      editor.showBuffer(pkgState.bufferId);
    }
    return;
  }

  // Store current buffer
  pkgState.sourceBufferId = editor.getActiveBufferId();
  pkgState.splitId = editor.getActiveSplitId();

  // Reset state
  pkgState.filter = "all";
  pkgState.searchQuery = "";
  pkgState.selectedIndex = 0;
  pkgState.focus = { type: "list" };

  // Build package list immediately with installed packages and cached registry
  // This allows viewing/managing installed packages without waiting for network
  pkgState.items = buildPackageList();
  pkgState.isLoading = false;

  // Build initial entries
  const entries = buildListViewEntries();

  // Create virtual buffer
  const result = await editor.createVirtualBufferInExistingSplit({
    name: "*Packages*",
    mode: "pkg-manager",
    readOnly: true,
    editingDisabled: true,
    showCursors: false,
    entries: entries,
    splitId: pkgState.splitId!,
    showLineNumbers: false,
  });

  pkgState.bufferId = result.bufferId;
  pkgState.isOpen = true;

  // Apply initial highlighting
  applyPkgManagerHighlighting();

  // Sync registry in background and update view when done
  // User can still interact with installed packages during sync
  syncRegistry().then(() => {
    if (pkgState.isOpen) {
      pkgState.items = buildPackageList();
      updatePkgManagerView();
    }
  });
}

/**
 * Close the package manager
 */
function closePackageManager(): void {
  if (!pkgState.isOpen) return;

  // Close the buffer
  if (pkgState.bufferId !== null) {
    editor.closeBuffer(pkgState.bufferId);
  }

  // Restore previous buffer if possible
  if (pkgState.sourceBufferId !== null && pkgState.splitId !== null) {
    editor.showBuffer(pkgState.sourceBufferId);
  }

  // Reset state
  pkgState.isOpen = false;
  pkgState.bufferId = null;
  pkgState.splitId = null;
  pkgState.sourceBufferId = null;
}

/**
 * Get all focusable elements in order for Tab navigation
 */
function getFocusOrder(): FocusTarget[] {
  const order: FocusTarget[] = [
    { type: "search" },
    { type: "filter", index: 0 },  // All
    { type: "filter", index: 1 },  // Installed
    { type: "filter", index: 2 },  // Plugins
    { type: "filter", index: 3 },  // Themes
    { type: "filter", index: 4 },  // Languages
    { type: "filter", index: 5 },  // Bundles
    { type: "sync" },
    { type: "list" },
  ];

  // Add action buttons for selected package
  const actions = getActionButtons();
  for (let i = 0; i < actions.length; i++) {
    order.push({ type: "action", index: i });
  }

  return order;
}

/**
 * Find current focus index in the focus order
 */
function getCurrentFocusIndex(): number {
  const order = getFocusOrder();
  for (let i = 0; i < order.length; i++) {
    const target = order[i];
    if (target.type === pkgState.focus.type) {
      if ("index" in target && "index" in pkgState.focus) {
        if (target.index === pkgState.focus.index) return i;
      } else if (!("index" in target) && !("index" in pkgState.focus)) {
        return i;
      }
    }
  }
  return 6; // Default to list
}

// Navigation commands
globalThis.pkg_nav_up = function(): void {
  if (!pkgState.isOpen) return;

  const items = getFilteredItems();
  if (items.length === 0) return;

  // Always focus list and navigate (auto-focus behavior)
  pkgState.selectedIndex = Math.max(0, pkgState.selectedIndex - 1);
  pkgState.focus = { type: "list" };
  updatePkgManagerView();
};

globalThis.pkg_nav_down = function(): void {
  if (!pkgState.isOpen) return;

  const items = getFilteredItems();
  if (items.length === 0) return;

  // Always focus list and navigate (auto-focus behavior)
  pkgState.selectedIndex = Math.min(items.length - 1, pkgState.selectedIndex + 1);
  pkgState.focus = { type: "list" };
  updatePkgManagerView();
};

globalThis.pkg_next_button = function(): void {
  if (!pkgState.isOpen) return;

  const order = getFocusOrder();
  const currentIdx = getCurrentFocusIndex();
  const nextIdx = (currentIdx + 1) % order.length;
  pkgState.focus = order[nextIdx];
  updatePkgManagerView();
};

globalThis.pkg_prev_button = function(): void {
  if (!pkgState.isOpen) return;

  const order = getFocusOrder();
  const currentIdx = getCurrentFocusIndex();
  const prevIdx = (currentIdx - 1 + order.length) % order.length;
  pkgState.focus = order[prevIdx];
  updatePkgManagerView();
};

globalThis.pkg_activate = async function(): Promise<void> {
  if (!pkgState.isOpen) return;

  const focus = pkgState.focus;

  // Handle filter button activation
  if (focus.type === "filter") {
    const filters = ["all", "installed", "plugins", "themes", "languages", "bundles"] as const;
    pkgState.filter = filters[focus.index];
    pkgState.selectedIndex = 0;
    pkgState.items = buildPackageList();
    updatePkgManagerView();
    return;
  }

  // Handle sync button
  if (focus.type === "sync") {
    await syncRegistry();
    pkgState.items = buildPackageList();
    updatePkgManagerView();
    return;
  }

  // Handle search button - open search prompt with current query
  if (focus.type === "search") {
    globalThis.pkg_search();
    return;
  }

  // Handle list selection - move focus to action buttons
  if (focus.type === "list") {
    const items = getFilteredItems();
    if (items.length === 0) {
      if (!isRegistrySynced()) {
        await syncRegistry();
        pkgState.items = buildPackageList();
        updatePkgManagerView();
      }
      return;
    }
    // Move focus to action button
    pkgState.focus = { type: "action", index: 0 };
    updatePkgManagerView();
    return;
  }

  // Handle action button activation
  if (focus.type === "action") {
    const items = getFilteredItems();
    if (items.length === 0 || pkgState.selectedIndex >= items.length) return;

    const item = items[pkgState.selectedIndex];
    const actions = getActionButtons();
    const actionName = actions[focus.index];

    if (actionName === "Reinstall" && item.installedPackage) {
      await reinstallPackage(item.installedPackage);
      pkgState.items = buildPackageList();
      updatePkgManagerView();
    } else if (actionName === "Update" && item.installedPackage) {
      await updatePackage(item.installedPackage);
      pkgState.items = buildPackageList();
      updatePkgManagerView();
    } else if (actionName === "Uninstall" && item.installedPackage) {
      await removePackage(item.installedPackage);
      pkgState.items = buildPackageList();
      const newItems = getFilteredItems();
      pkgState.selectedIndex = Math.min(pkgState.selectedIndex, Math.max(0, newItems.length - 1));
      pkgState.focus = { type: "list" };
      updatePkgManagerView();
    } else if (actionName === "Install" && item.registryEntry) {
      await installPackage(item.registryEntry.repository, item.name, item.packageType);
      pkgState.items = buildPackageList();
      updatePkgManagerView();
    }
  }
};

globalThis.pkg_back_or_close = function(): void {
  if (!pkgState.isOpen) return;

  // If focus is on action buttons, go back to list
  if (pkgState.focus.type === "action") {
    pkgState.focus = { type: "list" };
    updatePkgManagerView();
    return;
  }

  // Otherwise close
  closePackageManager();
};

globalThis.pkg_scroll_up = function(): void {
  // Just move cursor up in detail view
  editor.executeAction("move_up");
};

globalThis.pkg_scroll_down = function(): void {
  // Just move cursor down in detail view
  editor.executeAction("move_down");
};

globalThis.pkg_search = function(): void {
  if (!pkgState.isOpen) return;

  // Pre-fill with current search query so typing replaces it
  if (pkgState.searchQuery) {
    editor.startPromptWithInitial("Search packages: ", "pkg-search", pkgState.searchQuery);
  } else {
    editor.startPrompt("Search packages: ", "pkg-search");
  }
};

globalThis.onPkgSearchConfirmed = function(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): boolean {
  if (args.prompt_type !== "pkg-search") return true;

  pkgState.searchQuery = args.input.trim();
  pkgState.selectedIndex = 0;
  pkgState.focus = { type: "list" };
  updatePkgManagerView();

  return true;
};

editor.on("prompt_confirmed", "onPkgSearchConfirmed");

// Legacy Finder-based UI (kept for backwards compatibility)
const registryFinder = new Finder<[string, RegistryEntry]>(editor, {
  id: "pkg-registry",
  format: ([name, entry]) => ({
    label: name,
    description: entry.description,
    metadata: { name, entry }
  }),
  preview: false,
  maxResults: 100,
  onSelect: async ([name, entry]) => {
    await installPackage(entry.repository, name, "plugin");
  }
});

// =============================================================================
// Commands
// =============================================================================

/**
 * Browse and install plugins from registry
 */
globalThis.pkg_install_plugin = async function(): Promise<void> {
  editor.debug("[pkg] pkg_install_plugin called");
  try {
    // Always sync registry to ensure latest plugins are available
    await syncRegistry();

    const registry = loadRegistry("plugins");
    editor.debug(`[pkg] loaded registry with ${Object.keys(registry.packages).length} packages`);
    const entries = Object.entries(registry.packages);
    editor.debug(`[pkg] entries.length = ${entries.length}`);

    if (entries.length === 0) {
      editor.debug("[pkg] No plugins found, setting status");
      editor.setStatus("No plugins in registry (registry may be empty)");
      editor.debug("[pkg] setStatus called");
      return;
    }
    editor.debug("[pkg] About to show finder");

    registryFinder.prompt({
      title: "Install Plugin:",
      source: {
        mode: "filter",
        load: async () => entries
      }
    });
  } catch (e) {
    editor.debug(`[pkg] Error in pkg_install_plugin: ${e}`);
    editor.setStatus(`Error: ${e}`);
  }
};

/**
 * Browse and install themes from registry
 */
globalThis.pkg_install_theme = async function(): Promise<void> {
  editor.debug("[pkg] pkg_install_theme called");
  try {
    // Always sync registry to ensure latest themes are available
    await syncRegistry();

    const registry = loadRegistry("themes");
    editor.debug(`[pkg] loaded registry with ${Object.keys(registry.packages).length} themes`);
    const entries = Object.entries(registry.packages);

    if (entries.length === 0) {
      editor.setStatus("No themes in registry (registry may be empty)");
      return;
    }

    registryFinder.prompt({
      title: "Install Theme:",
      source: {
        mode: "filter",
        load: async () => entries
      }
    });
  } catch (e) {
    editor.debug(`[pkg] Error in pkg_install_theme: ${e}`);
    editor.setStatus(`Error: ${e}`);
  }
};

/**
 * Install from git URL or local path
 */
globalThis.pkg_install_url = function(): void {
  editor.startPrompt("Git URL or local path:", "pkg-install-url");
};

globalThis.onPkgInstallUrlConfirmed = async function(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): Promise<boolean> {
  if (args.prompt_type !== "pkg-install-url") return true;

  const url = args.input.trim();
  if (url) {
    await installPackage(url);
  } else {
    editor.setStatus("No URL or path provided");
  }

  return true;
};

editor.on("prompt_confirmed", "onPkgInstallUrlConfirmed");

/**
 * Open the package manager UI
 */
globalThis.pkg_list = async function(): Promise<void> {
  await openPackageManager();
};

/**
 * Update all packages
 */
globalThis.pkg_update_all = async function(): Promise<void> {
  await updateAllPackages();
};

/**
 * Update a specific package
 */
globalThis.pkg_update = function(): void {
  const plugins = getInstalledPackages("plugin");
  const themes = getInstalledPackages("theme");
  const all = [...plugins, ...themes];

  if (all.length === 0) {
    editor.setStatus("No packages installed");
    return;
  }

  const finder = new Finder<InstalledPackage>(editor, {
    id: "pkg-update",
    format: (pkg) => ({
      label: pkg.name,
      description: `${pkg.type} | ${pkg.version}${pkg.localSource ? " (local)" : ""}`,
      metadata: pkg
    }),
    preview: false,
    onSelect: async (pkg) => {
      if (pkg.localSource) {
        await reinstallPackage(pkg);
      } else {
        await updatePackage(pkg);
      }
    }
  });

  finder.prompt({
    title: "Update Package:",
    source: {
      mode: "filter",
      load: async () => all
    }
  });
};

/**
 * Remove a package
 */
globalThis.pkg_remove = function(): void {
  const plugins = getInstalledPackages("plugin");
  const themes = getInstalledPackages("theme");
  const all = [...plugins, ...themes];

  if (all.length === 0) {
    editor.setStatus("No packages installed");
    return;
  }

  const finder = new Finder<InstalledPackage>(editor, {
    id: "pkg-remove",
    format: (pkg) => ({
      label: pkg.name,
      description: `${pkg.type} | ${pkg.version}`,
      metadata: pkg
    }),
    preview: false,
    onSelect: async (pkg) => {
      await removePackage(pkg);
    }
  });

  finder.prompt({
    title: "Remove Package:",
    source: {
      mode: "filter",
      load: async () => all
    }
  });
};

/**
 * Sync registry
 */
globalThis.pkg_sync = async function(): Promise<void> {
  await syncRegistry();
};

/**
 * Show outdated packages
 */
globalThis.pkg_outdated = async function(): Promise<void> {
  const plugins = getInstalledPackages("plugin");
  const themes = getInstalledPackages("theme");
  const all = [...plugins, ...themes];

  if (all.length === 0) {
    editor.setStatus("No packages installed");
    return;
  }

  editor.setStatus("Checking for updates...");

  const outdated: Array<{ pkg: InstalledPackage; behind: number }> = [];

  for (const pkg of all) {
    // Fetch latest
    await gitCommand(["-C", `${pkg.path}`, "fetch"]);

    // Check how many commits behind
    const result = await gitCommand([
      "-C", `${pkg.path}`, "rev-list", "--count", "HEAD..origin/HEAD"
    ]);

    const behind = parseInt(result.stdout.trim(), 10);
    if (behind > 0) {
      outdated.push({ pkg, behind });
    }
  }

  if (outdated.length === 0) {
    editor.setStatus("All packages are up to date");
    return;
  }

  const finder = new Finder<{ pkg: InstalledPackage; behind: number }>(editor, {
    id: "pkg-outdated",
    format: (item) => ({
      label: item.pkg.name,
      description: `${item.behind} commits behind`,
      metadata: item
    }),
    preview: false,
    onSelect: async (item) => {
      await updatePackage(item.pkg);
    }
  });

  finder.prompt({
    title: `Outdated Packages (${outdated.length}):`,
    source: {
      mode: "filter",
      load: async () => outdated
    }
  });
};

/**
 * Generate lockfile
 */
globalThis.pkg_lock = async function(): Promise<void> {
  await generateLockfile();
};

/**
 * Install from lockfile
 */
globalThis.pkg_install_lock = async function(): Promise<void> {
  await installFromLockfile();
};

// =============================================================================
// Command Registration
// =============================================================================

// Main entry point - opens the package manager UI
editor.registerCommand("%cmd.list", "%cmd.list_desc", "pkg_list", null);

// Install from URL - for packages not in registry
editor.registerCommand("%cmd.install_url", "%cmd.install_url_desc", "pkg_install_url", null);

// Note: Other commands (install_plugin, install_theme, update, remove, sync, etc.)
// are available via the package manager UI and don't need global command palette entries.

// =============================================================================
// Startup: Load installed language packs and bundles
// =============================================================================

(async function loadInstalledPackages() {
  // Load language packs
  const languages = getInstalledPackages("language");
  for (const pkg of languages) {
    if (pkg.manifest) {
      editor.debug(`[pkg] Loading language pack: ${pkg.name}`);
      await loadLanguagePack(pkg.path, pkg.manifest);
    }
  }
  if (languages.length > 0) {
    editor.debug(`[pkg] Loaded ${languages.length} language pack(s)`);
  }

  // Load bundles
  const bundles = getInstalledPackages("bundle");
  for (const pkg of bundles) {
    if (pkg.manifest) {
      editor.debug(`[pkg] Loading bundle: ${pkg.name}`);
      await loadBundle(pkg.path, pkg.manifest);
    }
  }
  if (bundles.length > 0) {
    editor.debug(`[pkg] Loaded ${bundles.length} bundle(s)`);
  }
})();

editor.debug("Package Manager plugin loaded");
