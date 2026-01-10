/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * Multi-File Search & Replace Plugin
 *
 * Provides project-wide search and replace functionality using git grep.
 * Shows results in a virtual buffer split with preview and confirmation.
 */

// Result item structure
interface SearchResult {
  file: string;
  line: number;
  column: number;
  content: string;
  selected: boolean; // Whether this result will be replaced
}

// Plugin state
let panelOpen = false;
let resultsBufferId: number | null = null;
let sourceSplitId: number | null = null;
let resultsSplitId: number | null = null;
let searchResults: SearchResult[] = [];
let searchPattern: string = "";
let replaceText: string = "";
let searchRegex: boolean = false;

// Maximum results to display
const MAX_RESULTS = 200;

// Define the search-replace mode with keybindings
// Inherits from "normal" for cursor navigation (Up/Down)
// Simplified keybindings following UX best practices:
// - Enter: Execute replace (primary action)
// - Space: Toggle selection
// - Escape: Close panel
editor.defineMode(
  "search-replace-list",
  "normal", // Inherit from normal for cursor movement
  [
    ["Return", "search_replace_execute"],
    ["space", "search_replace_toggle_item"],
    ["Escape", "search_replace_close"],
  ],
  true // read-only
);

// Get relative path for display
function getRelativePath(filePath: string): string {
  const cwd = editor.getCwd();
  if (filePath.startsWith(cwd)) {
    return filePath.slice(cwd.length + 1);
  }
  return filePath;
}

// Parse git grep output
function parseGitGrepLine(line: string): SearchResult | null {
  const match = line.match(/^([^:]+):(\d+):(\d+):(.*)$/);
  if (match) {
    return {
      file: match[1],
      line: parseInt(match[2], 10),
      column: parseInt(match[3], 10),
      content: match[4],
      selected: true, // Selected by default
    };
  }
  return null;
}

// Format a result for display
function formatResult(item: SearchResult, index: number): string {
  const checkbox = item.selected ? "[x]" : "[ ]";
  const displayPath = getRelativePath(item.file);
  const location = `${displayPath}:${item.line}`;

  // Truncate for display
  const maxLocationLen = 40;
  const truncatedLocation = location.length > maxLocationLen
    ? "..." + location.slice(-(maxLocationLen - 3))
    : location.padEnd(maxLocationLen);

  const trimmedContent = item.content.trim();
  const maxContentLen = 50;
  const displayContent = trimmedContent.length > maxContentLen
    ? trimmedContent.slice(0, maxContentLen - 3) + "..."
    : trimmedContent;

  return `${checkbox} ${truncatedLocation}  ${displayContent}\n`;
}

// Build panel entries
function buildPanelEntries(): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  // Header
  const selectedCount = searchResults.filter(r => r.selected).length;
  entries.push({
    text: `═══ ${editor.t("panel.header")} ═══\n`,
    properties: { type: "header" },
  });
  entries.push({
    text: `${editor.t("panel.search_label")} "${searchPattern}"${searchRegex ? " " + editor.t("panel.regex") : ""}\n`,
    properties: { type: "info" },
  });
  entries.push({
    text: `${editor.t("panel.replace_label")} "${replaceText}"\n`,
    properties: { type: "info" },
  });
  entries.push({
    text: `\n`,
    properties: { type: "spacer" },
  });

  if (searchResults.length === 0) {
    entries.push({
      text: "  " + editor.t("panel.no_matches") + "\n",
      properties: { type: "empty" },
    });
  } else {
    // Results header
    const limitNote = searchResults.length >= MAX_RESULTS ? " " + editor.t("panel.limited", { max: String(MAX_RESULTS) }) : "";
    entries.push({
      text: `${editor.t("panel.results", { count: String(searchResults.length) })}${limitNote} ${editor.t("panel.selected", { selected: String(selectedCount) })}\n`,
      properties: { type: "count" },
    });
    entries.push({
      text: `\n`,
      properties: { type: "spacer" },
    });

    // Add each result
    for (let i = 0; i < searchResults.length; i++) {
      const result = searchResults[i];
      entries.push({
        text: formatResult(result, i),
        properties: {
          type: "result",
          index: i,
          location: {
            file: result.file,
            line: result.line,
            column: result.column,
          },
        },
      });
    }
  }

  // Footer
  entries.push({
    text: `───────────────────────────────────────────────────────────────────────────────\n`,
    properties: { type: "separator" },
  });
  entries.push({
    text: editor.t("panel.help") + "\n",
    properties: { type: "help" },
  });

  return entries;
}

// Update panel content
function updatePanelContent(): void {
  if (resultsBufferId !== null) {
    const entries = buildPanelEntries();
    editor.setVirtualBufferContent(resultsBufferId, entries);
  }
}

// Perform the search
async function performSearch(pattern: string, replace: string, isRegex: boolean): Promise<void> {
  searchPattern = pattern;
  replaceText = replace;
  searchRegex = isRegex;

  // Build git grep args
  const args = ["grep", "-n", "--column", "-I"];
  if (isRegex) {
    args.push("-E"); // Extended regex
  } else {
    args.push("-F"); // Fixed string
  }
  args.push("--", pattern);

  try {
    const cwd = editor.getCwd();
    const result = await editor.spawnProcess("git", args, cwd);

    searchResults = [];

    if (result.exit_code === 0) {
      for (const line of result.stdout.split("\n")) {
        if (!line.trim()) continue;
        const match = parseGitGrepLine(line);
        if (match) {
          searchResults.push(match);
          if (searchResults.length >= MAX_RESULTS) break;
        }
      }
    }

    if (searchResults.length === 0) {
      editor.setStatus(editor.t("status.no_matches", { pattern }));
    } else {
      editor.setStatus(editor.t("status.found_matches", { count: String(searchResults.length) }));
    }
  } catch (e) {
    editor.setStatus(editor.t("status.search_error", { error: String(e) }));
    searchResults = [];
  }
}

// Show the search results panel
async function showResultsPanel(): Promise<void> {
  if (panelOpen && resultsBufferId !== null) {
    updatePanelContent();
    return;
  }

  sourceSplitId = editor.getActiveSplitId();
  const entries = buildPanelEntries();

  try {
    const result = await editor.createVirtualBufferInSplit({
      name: "*Search/Replace*",
      mode: "search-replace-list",
      read_only: true,
      entries: entries,
      ratio: 0.6, // 60/40 split
      panel_id: "search-replace-panel",
      show_line_numbers: false,
      show_cursors: true,
    });
    resultsBufferId = result.buffer_id;
    resultsSplitId = result.split_id ?? editor.getActiveSplitId();

    panelOpen = true;
    editor.debug(`Search/Replace panel opened with buffer ID ${resultsBufferId}`);
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error);
    editor.setStatus(editor.t("status.failed_open_panel"));
    editor.debug(`ERROR: createVirtualBufferInSplit failed: ${errorMessage}`);
  }
}

// Execute replacements
async function executeReplacements(): Promise<void> {
  const selectedResults = searchResults.filter(r => r.selected);

  if (selectedResults.length === 0) {
    editor.setStatus(editor.t("status.no_selected"));
    return;
  }

  // Group by file
  const fileGroups: Map<string, SearchResult[]> = new Map();
  for (const result of selectedResults) {
    if (!fileGroups.has(result.file)) {
      fileGroups.set(result.file, []);
    }
    fileGroups.get(result.file)!.push(result);
  }

  let filesModified = 0;
  let replacementsCount = 0;
  const errors: string[] = [];

  for (const [filePath, results] of fileGroups) {
    try {
      // Read file
      const content = await editor.readFile(filePath);
      const lines = content.split("\n");

      // Sort results by line (descending) to avoid offset issues
      const sortedResults = [...results].sort((a, b) => {
        if (a.line !== b.line) return b.line - a.line;
        return b.column - a.column;
      });

      // Apply replacements
      for (const result of sortedResults) {
        const lineIndex = result.line - 1;
        if (lineIndex >= 0 && lineIndex < lines.length) {
          let line = lines[lineIndex];

          if (searchRegex) {
            // Regex replacement
            const regex = new RegExp(searchPattern, "g");
            lines[lineIndex] = line.replace(regex, replaceText);
          } else {
            // Simple string replacement (all occurrences in line)
            lines[lineIndex] = line.split(searchPattern).join(replaceText);
          }
          replacementsCount++;
        }
      }

      // Write back
      const newContent = lines.join("\n");
      await editor.writeFile(filePath, newContent);
      filesModified++;

    } catch (e) {
      const errorMessage = e instanceof Error ? e.message : String(e);
      errors.push(`${filePath}: ${errorMessage}`);
    }
  }

  // Report results
  if (errors.length > 0) {
    editor.setStatus(editor.t("status.replaced_with_errors", { files: String(filesModified), errors: String(errors.length) }));
    editor.debug(`Replacement errors: ${errors.join(", ")}`);
  } else {
    editor.setStatus(editor.t("status.replaced", { count: String(replacementsCount), files: String(filesModified) }));
  }

  // Close panel after replacement
  globalThis.search_replace_close();
}

// Start search/replace workflow
globalThis.start_search_replace = function(): void {
  searchResults = [];
  searchPattern = "";
  replaceText = "";

  editor.startPrompt(editor.t("prompt.search"), "search-replace-search");
  editor.setStatus(editor.t("status.enter_pattern"));
};

// Handle search prompt confirmation
globalThis.onSearchReplaceSearchConfirmed = function(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): boolean {
  if (args.prompt_type !== "search-replace-search") {
    return true;
  }

  const pattern = args.input.trim();
  if (!pattern) {
    editor.setStatus(editor.t("status.cancelled_empty"));
    return true;
  }

  searchPattern = pattern;

  // Ask for replacement text
  editor.startPrompt(editor.t("prompt.replace"), "search-replace-replace");
  return true;
};

// Handle replace prompt confirmation
globalThis.onSearchReplaceReplaceConfirmed = async function(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): Promise<boolean> {
  if (args.prompt_type !== "search-replace-replace") {
    return true;
  }

  replaceText = args.input; // Can be empty for deletion

  // Perform search and show results
  await performSearch(searchPattern, replaceText, false);
  await showResultsPanel();

  return true;
};

// Handle prompt cancellation
globalThis.onSearchReplacePromptCancelled = function(args: {
  prompt_type: string;
}): boolean {
  if (args.prompt_type !== "search-replace-search" &&
      args.prompt_type !== "search-replace-replace") {
    return true;
  }

  editor.setStatus(editor.t("status.cancelled"));
  return true;
};

// Toggle selection of current item
globalThis.search_replace_toggle_item = function(): void {
  if (resultsBufferId === null || searchResults.length === 0) return;

  const props = editor.getTextPropertiesAtCursor(resultsBufferId);
  if (props.length > 0 && typeof props[0].index === "number") {
    const index = props[0].index as number;
    if (index >= 0 && index < searchResults.length) {
      searchResults[index].selected = !searchResults[index].selected;
      updatePanelContent();
      const selected = searchResults.filter(r => r.selected).length;
      editor.setStatus(editor.t("status.selected_count", { selected: String(selected), total: String(searchResults.length) }));
    }
  }
};

// Select all items
globalThis.search_replace_select_all = function(): void {
  for (const result of searchResults) {
    result.selected = true;
  }
  updatePanelContent();
  editor.setStatus(editor.t("status.selected_count", { selected: String(searchResults.length), total: String(searchResults.length) }));
};

// Select no items
globalThis.search_replace_select_none = function(): void {
  for (const result of searchResults) {
    result.selected = false;
  }
  updatePanelContent();
  editor.setStatus(editor.t("status.selected_count", { selected: "0", total: String(searchResults.length) }));
};

// Execute replacement
globalThis.search_replace_execute = function(): void {
  const selected = searchResults.filter(r => r.selected).length;
  if (selected === 0) {
    editor.setStatus(editor.t("status.no_items_selected"));
    return;
  }

  editor.setStatus(editor.t("status.replacing", { count: String(selected) }));
  executeReplacements();
};

// Preview current item (jump to location)
globalThis.search_replace_preview = function(): void {
  if (sourceSplitId === null || resultsBufferId === null) return;

  const props = editor.getTextPropertiesAtCursor(resultsBufferId);
  if (props.length > 0) {
    const location = props[0].location as { file: string; line: number; column: number } | undefined;
    if (location) {
      editor.openFileInSplit(sourceSplitId, location.file, location.line, location.column);
      editor.setStatus(editor.t("status.preview", { file: getRelativePath(location.file), line: String(location.line) }));
    }
  }
};

// Close the panel
globalThis.search_replace_close = function(): void {
  if (!panelOpen) return;

  if (resultsBufferId !== null) {
    editor.closeBuffer(resultsBufferId);
  }

  if (resultsSplitId !== null && resultsSplitId !== sourceSplitId) {
    editor.closeSplit(resultsSplitId);
  }

  panelOpen = false;
  resultsBufferId = null;
  sourceSplitId = null;
  resultsSplitId = null;
  searchResults = [];
  editor.setStatus(editor.t("status.closed"));
};

// Register event handlers
editor.on("prompt_confirmed", "onSearchReplaceSearchConfirmed");
editor.on("prompt_confirmed", "onSearchReplaceReplaceConfirmed");
editor.on("prompt_cancelled", "onSearchReplacePromptCancelled");

// Register command
editor.registerCommand(
  "%cmd.search_replace",
  "%cmd.search_replace_desc",
  "start_search_replace",
  "normal"
);

// Plugin initialization
editor.debug("Search & Replace plugin loaded");
editor.setStatus(editor.t("status.ready"));
