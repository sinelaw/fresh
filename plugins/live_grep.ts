/// <reference path="./lib/fresh.d.ts" />

/**
 * Live Grep Plugin
 *
 * Project-wide search with ripgrep and live preview.
 * - Type to search across all files
 * - Navigate results with Up/Down to see preview
 * - Press Enter to open file at location
 */

interface GrepMatch {
  file: string;
  line: number;
  column: number;
  content: string;
}

// State management
let grepResults: GrepMatch[] = [];
let previewBufferId: number | null = null;
let previewSplitId: number | null = null;
let originalSplitId: number | null = null;
let lastQuery: string = "";
let previewCreated: boolean = false;
let currentSearch: ProcessHandle | null = null;
let searchVersion = 0;  // Incremented on each input change for debouncing

const DEBOUNCE_MS = 150;  // Wait 150ms after last keystroke before searching

// Parse ripgrep output line
// Format: file:line:column:content
function parseRipgrepLine(line: string): GrepMatch | null {
  const match = line.match(/^([^:]+):(\d+):(\d+):(.*)$/);
  if (match) {
    return {
      file: match[1],
      line: parseInt(match[2], 10),
      column: parseInt(match[3], 10),
      content: match[4],
    };
  }
  return null;
}

// Parse ripgrep output into suggestions
function parseRipgrepOutput(stdout: string): {
  results: GrepMatch[];
  suggestions: PromptSuggestion[];
} {
  const results: GrepMatch[] = [];
  const suggestions: PromptSuggestion[] = [];

  for (const line of stdout.split("\n")) {
    if (!line.trim()) continue;
    const match = parseRipgrepLine(line);
    if (match) {
      results.push(match);

      // Truncate long content for display
      const displayContent =
        match.content.length > 60
          ? match.content.substring(0, 57) + "..."
          : match.content;

      suggestions.push({
        text: `${match.file}:${match.line}`,
        description: displayContent.trim(),
        value: `${results.length - 1}`, // Store index as value
        disabled: false,
      });

      // Limit to 100 results for performance
      if (results.length >= 100) {
        break;
      }
    }
  }

  return { results, suggestions };
}

// Create or update preview buffer with file content
async function updatePreview(match: GrepMatch): Promise<void> {
  try {
    // Read the file content
    const content = await editor.readFile(match.file);
    const lines = content.split("\n");

    // Calculate context window (5 lines before and after)
    const contextBefore = 5;
    const contextAfter = 5;
    const startLine = Math.max(0, match.line - 1 - contextBefore);
    const endLine = Math.min(lines.length, match.line + contextAfter);

    // Build preview entries with highlighting
    const entries: TextPropertyEntry[] = [];

    // Header
    entries.push({
      text: `  ${match.file}:${match.line}:${match.column}\n`,
      properties: { type: "header" },
    });
    entries.push({
      text: "─".repeat(60) + "\n",
      properties: { type: "separator" },
    });

    // Content lines with line numbers
    for (let i = startLine; i < endLine; i++) {
      const lineNum = i + 1;
      const lineContent = lines[i] || "";
      const isMatchLine = lineNum === match.line;
      const prefix = isMatchLine ? "> " : "  ";
      const lineNumStr = String(lineNum).padStart(4, " ");

      entries.push({
        text: `${prefix}${lineNumStr} │ ${lineContent}\n`,
        properties: {
          type: isMatchLine ? "match" : "context",
          line: lineNum,
        },
      });
    }

    // Create or update the preview buffer
    if (previewBufferId === null) {
      // Define mode for preview buffer
      editor.defineMode("live-grep-preview", "special", [["q", "close_buffer"]], true);

      // Create preview in a split on the right
      const result = await editor.createVirtualBufferInSplit({
        name: "*Preview*",
        mode: "live-grep-preview",
        read_only: true,
        entries,
        ratio: 0.5,
        direction: "vertical",
        panel_id: "live-grep-preview",
        show_line_numbers: false,
        editing_disabled: true,
      });

      // Extract buffer and split IDs from result
      previewBufferId = result.buffer_id;
      previewSplitId = result.split_id ?? null;

      // Return focus to original split so prompt stays active
      if (originalSplitId !== null) {
        editor.focusSplit(originalSplitId);
      }
    } else {
      // Update existing buffer content
      editor.setVirtualBufferContent(previewBufferId, entries);
    }
  } catch (e) {
    editor.debug(`Failed to update preview: ${e}`);
  }
}

// Close preview buffer and its split
function closePreview(): void {
  // Close the buffer first
  if (previewBufferId !== null) {
    editor.closeBuffer(previewBufferId);
    previewBufferId = null;
  }
  // Then close the split
  if (previewSplitId !== null) {
    editor.closeSplit(previewSplitId);
    previewSplitId = null;
  }
}

// Run ripgrep search with debouncing
async function runSearch(query: string): Promise<void> {
  // Increment version to invalidate any pending debounced search
  const thisVersion = ++searchVersion;
  editor.debug(`[live_grep] runSearch called: query="${query}", version=${thisVersion}`);

  // Kill any existing search immediately (don't wait) to stop wasting CPU
  // We'll await the kill promise later before spawning a new search
  let killPromise: Promise<boolean> | null = null;
  if (currentSearch) {
    editor.debug(`[live_grep] killing existing search immediately`);
    killPromise = currentSearch.kill();
    currentSearch = null;
  }

  if (!query || query.trim().length < 2) {
    // Wait for kill to complete before returning
    if (killPromise) {
      await killPromise;
    }
    editor.debug(`[live_grep] query too short, clearing`);
    editor.setPromptSuggestions([]);
    grepResults = [];
    return;
  }

  // Debounce: wait a bit to see if user is still typing
  editor.debug(`[live_grep] debouncing for ${DEBOUNCE_MS}ms...`);
  await editor.delay(DEBOUNCE_MS);

  // If version changed during delay, a newer search was triggered - abort this one
  if (searchVersion !== thisVersion) {
    editor.debug(`[live_grep] version mismatch after debounce (${thisVersion} vs ${searchVersion}), aborting`);
    return;
  }

  // Avoid duplicate searches
  if (query === lastQuery) {
    editor.debug(`[live_grep] duplicate query, skipping`);
    return;
  }
  lastQuery = query;

  // Wait for kill to complete before spawning new process
  if (killPromise) {
    editor.debug(`[live_grep] waiting for previous search to terminate`);
    await killPromise;
    editor.debug(`[live_grep] previous search terminated`);
  }

  try {
    const cwd = editor.getCwd();
    editor.debug(`[live_grep] spawning rg for query="${query}" in cwd="${cwd}"`);
    const searchStartTime = Date.now();
    const search = editor.spawnProcess("rg", [
      "--line-number",
      "--column",
      "--no-heading",
      "--color=never",
      "--smart-case",
      "--max-count=100",
      "-g", "!.git",
      "-g", "!node_modules",
      "-g", "!target",
      "-g", "!*.lock",
      "--",
      query,
    ], cwd);

    currentSearch = search;
    editor.debug(`[live_grep] awaiting search result...`);
    const result = await search;
    const searchDuration = Date.now() - searchStartTime;
    editor.debug(`[live_grep] rg completed in ${searchDuration}ms, exit_code=${result.exit_code}, stdout_len=${result.stdout.length}`);

    // Check if this search was cancelled (a new search started)
    if (currentSearch !== search) {
      editor.debug(`[live_grep] search was superseded, discarding results`);
      return; // Discard stale results
    }
    currentSearch = null;

    if (result.exit_code === 0) {
      const parseStart = Date.now();
      const { results, suggestions } = parseRipgrepOutput(result.stdout);
      editor.debug(`[live_grep] parsed ${results.length} results in ${Date.now() - parseStart}ms`);
      grepResults = results;
      editor.setPromptSuggestions(suggestions);

      if (results.length > 0) {
        editor.setStatus(`Found ${results.length} matches`);
        // Show preview of first result
        await updatePreview(results[0]);
      } else {
        editor.setStatus("No matches found");
      }
    } else if (result.exit_code === 1) {
      // No matches
      editor.debug(`[live_grep] no matches (exit_code=1)`);
      grepResults = [];
      editor.setPromptSuggestions([]);
      editor.setStatus("No matches found");
    } else if (result.exit_code === -1) {
      // Process was killed, ignore
      editor.debug(`[live_grep] process was killed`);
    } else {
      editor.debug(`[live_grep] search error: ${result.stderr}`);
      editor.setStatus(`Search error: ${result.stderr}`);
    }
  } catch (e) {
    // Ignore errors from killed processes
    const errorMsg = String(e);
    editor.debug(`[live_grep] caught error: ${errorMsg}`);
    if (!errorMsg.includes("killed") && !errorMsg.includes("not found")) {
      editor.setStatus(`Search error: ${e}`);
    }
  }
}

// Start live grep
globalThis.start_live_grep = function (): void {
  // Clear previous state
  grepResults = [];
  lastQuery = "";
  previewBufferId = null;

  // Remember original split to keep focus
  originalSplitId = editor.getActiveSplitId();

  // Start the prompt
  editor.startPrompt("Live grep: ", "live-grep");
  editor.setStatus("Type to search (min 2 chars)...");
};

// Handle prompt input changes
globalThis.onLiveGrepPromptChanged = function (args: {
  prompt_type: string;
  input: string;
}): boolean {
  if (args.prompt_type !== "live-grep") {
    return true;
  }

  editor.debug(`[live_grep] onPromptChanged: input="${args.input}"`);

  // runSearch handles debouncing internally
  runSearch(args.input);

  return true;
};

// Handle selection changes - update preview
globalThis.onLiveGrepSelectionChanged = function (args: {
  prompt_type: string;
  selected_index: number;
}): boolean {
  if (args.prompt_type !== "live-grep") {
    return true;
  }

  const match = grepResults[args.selected_index];
  if (match) {
    updatePreview(match);
  }

  return true;
};

// Handle prompt confirmation - open file
globalThis.onLiveGrepPromptConfirmed = function (args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): boolean {
  if (args.prompt_type !== "live-grep") {
    return true;
  }

  // Kill any running search
  if (currentSearch) {
    currentSearch.kill();
    currentSearch = null;
  }

  // Close preview first
  closePreview();

  // Open selected file
  if (args.selected_index !== null && grepResults[args.selected_index]) {
    const selected = grepResults[args.selected_index];
    editor.openFile(selected.file, selected.line, selected.column);
    editor.setStatus(`Opened ${selected.file}:${selected.line}`);
  } else {
    editor.setStatus("No file selected");
  }

  // Clear state
  grepResults = [];
  originalSplitId = null;
  previewSplitId = null;

  return true;
};

// Handle prompt cancellation
globalThis.onLiveGrepPromptCancelled = function (args: {
  prompt_type: string;
}): boolean {
  if (args.prompt_type !== "live-grep") {
    return true;
  }

  // Kill any running search
  if (currentSearch) {
    currentSearch.kill();
    currentSearch = null;
  }

  // Close preview and cleanup
  closePreview();
  grepResults = [];
  originalSplitId = null;
  previewSplitId = null;
  editor.setStatus("Live grep cancelled");

  return true;
};

// Register event handlers
editor.on("prompt_changed", "onLiveGrepPromptChanged");
editor.on("prompt_selection_changed", "onLiveGrepSelectionChanged");
editor.on("prompt_confirmed", "onLiveGrepPromptConfirmed");
editor.on("prompt_cancelled", "onLiveGrepPromptCancelled");

// Register command
editor.registerCommand(
  "Live Grep (Find in Files)",
  "Search for text across project with live preview",
  "start_live_grep",
  "normal"
);

editor.debug("Live Grep plugin loaded");
editor.setStatus("Live Grep ready - use command palette or bind 'start_live_grep'");
