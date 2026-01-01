/// <reference path="../types/fresh.d.ts" />

/**
 * Git Find File Plugin
 *
 * Provides interactive file finding functionality with fuzzy search
 * for git-tracked files. Uses the prompt API for interactive selection.
 */

// State management
let allFiles: string[] = [];
let filteredFiles: string[] = [];
let isLoading = false;

// Simple fuzzy filter function
function fuzzyMatch(str: string, pattern: string): boolean {
  if (pattern === "") {
    return true;
  }

  str = str.toLowerCase();
  pattern = pattern.toLowerCase();

  let strIdx = 0;
  let patIdx = 0;

  while (strIdx < str.length && patIdx < pattern.length) {
    if (str[strIdx] === pattern[patIdx]) {
      patIdx++;
    }
    strIdx++;
  }

  return patIdx >= pattern.length;
}

// Score a fuzzy match (higher is better)
function fuzzyScore(str: string, pattern: string): number {
  if (pattern === "") return 0;

  str = str.toLowerCase();
  pattern = pattern.toLowerCase();

  let score = 0;
  let strIdx = 0;
  let patIdx = 0;
  let consecutiveMatches = 0;
  let lastMatchIdx = -1;

  while (strIdx < str.length && patIdx < pattern.length) {
    if (str[strIdx] === pattern[patIdx]) {
      // Bonus for consecutive matches
      if (lastMatchIdx === strIdx - 1) {
        consecutiveMatches++;
        score += consecutiveMatches * 10;
      } else {
        consecutiveMatches = 1;
        score += 1;
      }

      // Bonus for matching at start of path segments
      if (strIdx === 0 || str[strIdx - 1] === "/" || str[strIdx - 1] === "_" || str[strIdx - 1] === "-") {
        score += 15;
      }

      // Bonus for matching filename (after last /)
      const lastSlash = str.lastIndexOf("/");
      if (strIdx > lastSlash) {
        score += 5;
      }

      lastMatchIdx = strIdx;
      patIdx++;
    }
    strIdx++;
  }

  // Penalty for longer paths
  score -= str.length * 0.1;

  return patIdx >= pattern.length ? score : -1;
}

// Filter and sort files by query using fuzzy matching
function filterFiles(files: string[], query: string): string[] {
  if (query === "" || query.trim() === "") {
    // Return first 100 files for empty query
    return files.slice(0, 100);
  }

  const scored: Array<{ file: string; score: number }> = [];

  for (const file of files) {
    const score = fuzzyScore(file, query);
    if (score > 0) {
      scored.push({ file, score });
    }

    // Stop early if we have enough high-quality matches
    if (scored.length >= 500) {
      break;
    }
  }

  // Sort by score descending
  scored.sort((a, b) => b.score - a.score);

  // Return top 100 results
  return scored.slice(0, 100).map((s) => s.file);
}

// Load git-tracked files asynchronously
async function loadGitFiles(): Promise<void> {
  if (isLoading) {
    return;
  }

  isLoading = true;
  editor.setStatus(editor.t("status.loading"));

  try {
    const result = await editor.spawnProcess("git", ["ls-files"]);

    if (result.exit_code === 0) {
      allFiles = result.stdout.split("\n").filter((line) => line.trim() !== "");

      editor.debug(`Loaded ${allFiles.length} git-tracked files`);
      editor.setStatus(editor.t("status.indexed", { count: String(allFiles.length) }));
    } else {
      editor.debug(`Failed to load git files: ${result.stderr}`);
      editor.setStatus(editor.t("status.error_loading", { error: result.stderr }));
      allFiles = [];
    }
  } catch (e) {
    editor.debug(`Exception loading git files: ${e}`);
    editor.setStatus(editor.t("status.failed_load"));
    allFiles = [];
  } finally {
    isLoading = false;
  }
}

// Convert filtered files to prompt suggestions
function filesToSuggestions(files: string[]): PromptSuggestion[] {
  return files.map((file) => {
    return {
      text: file,
      description: undefined,
      value: file,
      disabled: false,
    };
  });
}

// Global function to start file finder
globalThis.start_git_find_file = async function (): Promise<void> {
  // Load files if not already loaded
  if (allFiles.length === 0 && !isLoading) {
    await loadGitFiles();
  }

  if (allFiles.length === 0) {
    editor.setStatus(editor.t("status.no_files"));
    return;
  }

  // Clear previous results
  filteredFiles = [];

  // Start the prompt
  editor.startPrompt(editor.t("prompt.find_file"), "git-find-file");

  // Show initial suggestions (first 100 files)
  const initial = filterFiles(allFiles, "");
  filteredFiles = initial;
  editor.setPromptSuggestions(filesToSuggestions(initial));
  editor.setStatus(editor.t("status.files_available", { count: String(allFiles.length) }));
};

// React to prompt input changes
globalThis.onGitFindFilePromptChanged = function (args: { prompt_type: string; input: string }): boolean {
  if (args.prompt_type !== "git-find-file") {
    return true; // Not our prompt
  }

  const query = args.input;

  // Filter files based on query
  const matches = filterFiles(allFiles, query);
  filteredFiles = matches;

  // Update suggestions
  editor.setPromptSuggestions(filesToSuggestions(matches));

  // Update status
  if (matches.length > 0) {
    if (query.trim() === "") {
      editor.setStatus(editor.t("status.showing_first", { shown: String(matches.length), total: String(allFiles.length) }));
    } else {
      editor.setStatus(editor.t("status.found_matching", { count: String(matches.length), query }));
    }
  } else {
    editor.setStatus(editor.t("status.no_matching", { query }));
  }

  return true;
};

// Handle prompt confirmation (user pressed Enter)
globalThis.onGitFindFilePromptConfirmed = function (args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): boolean {
  if (args.prompt_type !== "git-find-file") {
    return true; // Not our prompt
  }

  editor.debug(`git-find-file confirmed: selected_index=${args.selected_index}, input=${args.input}`);

  // Check if user selected a suggestion
  if (args.selected_index !== null && filteredFiles[args.selected_index]) {
    const selectedFile = filteredFiles[args.selected_index];

    editor.debug(`Opening file: ${selectedFile}`);

    // Open the file at line 1
    editor.openFile(selectedFile, 1, 1);
    editor.setStatus(editor.t("status.opened", { file: selectedFile }));
  } else if (args.input.trim() !== "") {
    // Try to open input directly if it's a valid file path
    const inputFile = args.input.trim();

    // Check if the exact input matches any file
    if (allFiles.includes(inputFile)) {
      editor.openFile(inputFile, 1, 1);
      editor.setStatus(editor.t("status.opened", { file: inputFile }));
    } else {
      editor.setStatus(editor.t("status.file_not_found", { file: inputFile }));
    }
  } else {
    editor.setStatus(editor.t("status.no_selection"));
  }

  return true;
};

// Handle prompt cancellation (user pressed Escape)
globalThis.onGitFindFilePromptCancelled = function (args: { prompt_type: string }): boolean {
  if (args.prompt_type !== "git-find-file") {
    return true; // Not our prompt
  }

  // Clear results
  filteredFiles = [];
  editor.setStatus(editor.t("status.cancelled"));

  return true;
};

// Register event handlers
editor.on("prompt_changed", "onGitFindFilePromptChanged");
editor.on("prompt_confirmed", "onGitFindFilePromptConfirmed");
editor.on("prompt_cancelled", "onGitFindFilePromptCancelled");

// Reload git files command
globalThis.git_reload_files = async function (): Promise<void> {
  allFiles = [];
  await loadGitFiles();
};

// Show file count command
globalThis.git_file_count = function (): void {
  if (allFiles.length === 0) {
    editor.setStatus(editor.t("status.no_files_loaded"));
  } else {
    editor.setStatus(editor.t("status.indexed", { count: String(allFiles.length) }));
  }
};

// Register commands
editor.registerCommand("%cmd.find", "%cmd.find_desc", "start_git_find_file", "normal");

editor.registerCommand(
  "%cmd.reload",
  "%cmd.reload_desc",
  "git_reload_files",
  "normal"
);

editor.registerCommand("%cmd.count", "%cmd.count_desc", "git_file_count", "normal");

// Note: We don't load git files on plugin init because spawning processes requires async context
// Files will be loaded lazily on first use

editor.debug("Git Find File plugin loaded successfully (TypeScript)");
editor.debug("Usage: Call start_git_find_file() or use command palette 'Git Find File'");
editor.setStatus(editor.t("status.ready"));
