/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * Path Completion Plugin
 *
 * Provides path autocompletion for file prompts (Open File, Save File As).
 * Shows directory contents and filters based on user input.
 */

// Parse the input to extract directory path and search pattern
function parsePath(input: string): { dir: string; pattern: string; isAbsolute: boolean } {
  if (input === "") {
    return { dir: ".", pattern: "", isAbsolute: false };
  }

  const isAbsolute = input.startsWith("/");

  // Find the last path separator
  const lastSlash = input.lastIndexOf("/");

  if (lastSlash === -1) {
    // No slash, searching in current directory
    return { dir: ".", pattern: input, isAbsolute: false };
  }

  if (lastSlash === 0) {
    // Root directory
    return { dir: "/", pattern: input.slice(1), isAbsolute: true };
  }

  // Has directory component
  const dir = input.slice(0, lastSlash);
  const pattern = input.slice(lastSlash + 1);

  return { dir: dir || "/", pattern, isAbsolute };
}

// Filter and sort entries based on pattern
function filterEntries(entries: DirEntry[], pattern: string): DirEntry[] {
  const patternLower = pattern.toLowerCase();

  // Filter entries that match the pattern
  const filtered = entries.filter((entry) => {
    const nameLower = entry.name.toLowerCase();
    // Match if pattern is prefix of name (case-insensitive)
    return nameLower.startsWith(patternLower);
  });

  // Sort: directories first, then alphabetically
  filtered.sort((a, b) => {
    // Directories come first
    if (a.is_dir && !b.is_dir) return -1;
    if (!a.is_dir && b.is_dir) return 1;
    // Alphabetical within same type
    return a.name.localeCompare(b.name);
  });

  return filtered;
}

// Convert directory entries to suggestions
function entriesToSuggestions(entries: DirEntry[], basePath: string): PromptSuggestion[] {
  return entries.map((entry) => {
    // Build full path
    let fullPath: string;
    if (basePath === ".") {
      fullPath = entry.name;
    } else if (basePath === "/") {
      fullPath = "/" + entry.name;
    } else {
      fullPath = basePath + "/" + entry.name;
    }

    // Add trailing slash for directories
    const displayName = entry.is_dir ? entry.name + "/" : entry.name;
    const value = entry.is_dir ? fullPath + "/" : fullPath;

    return {
      text: displayName,
      description: entry.is_dir ? editor.t("suggestion.directory") : undefined,
      value: value,
      disabled: false,
    };
  });
}

function missingFileSuggestion(
  input: string,
  pattern: string,
): PromptSuggestion | null {
  if (pattern === "" || input === "") {
    return null;
  }

  let absolutePath = input;
  if (!editor.pathIsAbsolute(absolutePath)) {
    let cwd: string;
    try {
      cwd = editor.getCwd();
    } catch {
      return null;
    }
    absolutePath = editor.pathJoin(cwd, absolutePath);
  }

  if (editor.fileExists(absolutePath)) {
    return null;
  }

  return {
    text: editor.t("suggestion.new_file", { filename: input }),
    description: editor.t("suggestion.new_file_desc"),
    value: input,
  };
}

// Generate path completions for the given input
function generateCompletions(input: string): PromptSuggestion[] {
  const { dir, pattern } = parsePath(input);

  // Read the directory
  const entries = editor.readDir(dir);
  const newFileSuggestion = missingFileSuggestion(input, pattern);

  if (!entries) {
    // Directory doesn't exist or can't be read
    return newFileSuggestion ? [newFileSuggestion] : [];
  }

  // Filter hidden files (starting with .) unless pattern starts with .
  const showHidden = pattern.startsWith(".");
  const visibleEntries = entries.filter((e) => showHidden || !e.name.startsWith("."));

  // Filter by pattern
  const filtered = filterEntries(visibleEntries, pattern);

  // Limit results
  const limited = filtered.slice(0, 100);

  // Convert to suggestions
  const suggestions = entriesToSuggestions(limited, dir);
  if (newFileSuggestion) {
    suggestions.push(newFileSuggestion);
  }
  return suggestions;
}

// Handle prompt changes for file prompts
globalThis.onPathCompletePromptChanged = function (args: { prompt_type: string; input: string }): boolean {
  if (args.prompt_type !== "open-file" && args.prompt_type !== "save-file-as") {
    return true; // Not our prompt
  }

  const suggestions = generateCompletions(args.input);
  editor.setPromptSuggestions(suggestions);

  return true;
};

// Register event handler
editor.on("prompt_changed", "onPathCompletePromptChanged");

editor.setStatus(editor.t("status.loaded"));
