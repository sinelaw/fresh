/// <reference path="./lib/fresh.d.ts" />

/**
 * Code Tour Plugin
 *
 * A JSON-driven walkthrough system that guides users through a codebase
 * using visual overlays and explanatory text.
 *
 * Usage:
 * 1. Create a .fresh-tour.json file in your project root
 * 2. Use "Tour: Load Definition..." command to start a tour
 * 3. Navigate with Space/Right (next), Backspace/Left (prev), Tab (resume), Esc (exit)
 */

const editor = getEditor();

// ============================================================================
// Types
// ============================================================================

interface OverlayConfig {
  type: "block" | "line";
  focus_mode: boolean;
}

interface TourStep {
  step_id: number;
  title: string;
  file_path: string;
  lines: [number, number]; // 1-indexed, inclusive
  explanation: string;
  overlay_config?: OverlayConfig;
}

interface TourManifest {
  $schema?: string;
  title: string;
  description: string;
  schema_version: "1.0";
  commit_hash?: string;
  steps: TourStep[];
}

type TourState =
  | { kind: "idle" }
  | { kind: "active"; currentStep: number; isPaused: boolean };

interface TourManager {
  state: TourState;
  manifest: TourManifest | null;
  dockBufferId: number | null;
  dockSplitId: number | null;
  contentBufferId: number | null;
  contentSplitId: number | null;
  overlayNamespace: string;
  lastKnownTopByte: number;
  lastKnownBufferId: number;
}

// ============================================================================
// State
// ============================================================================

const TOUR_NAMESPACE = "code-tour";

const tourManager: TourManager = {
  state: { kind: "idle" },
  manifest: null,
  dockBufferId: null,
  dockSplitId: null,
  contentBufferId: null,
  contentSplitId: null,
  overlayNamespace: TOUR_NAMESPACE,
  lastKnownTopByte: 0,
  lastKnownBufferId: 0,
};

// ============================================================================
// Tour Status Updates
// ============================================================================

const TOUR_POPUP_ID = "code-tour-step";

function showStepPopup(
  step: TourStep,
  stepIndex: number,
  totalSteps: number,
  fileError?: string
): void {
  const manifest = tourManager.manifest;
  if (!manifest) return;

  const stepInfo = `Step ${stepIndex + 1}/${totalSteps}: ${step.title}`;

  // Build message with explanation
  let message = step.explanation;
  if (fileError) {
    message = `ERROR: ${fileError}\n\n${step.explanation}`;
  }

  // Build actions based on position
  const actions: Array<{ id: string; label: string }> = [];

  if (stepIndex > 0) {
    actions.push({ id: "prev", label: "← Previous" });
  }
  if (stepIndex < totalSteps - 1) {
    actions.push({ id: "next", label: "Next →" });
  }
  actions.push({ id: "exit", label: "Exit Tour" });

  editor.showActionPopup({
    id: TOUR_POPUP_ID,
    title: stepInfo,
    message: message,
    actions: actions,
  });
}

// Handle popup button clicks
interface ActionPopupResultData {
  popup_id: string;
  action_id: string;
}

globalThis.tour_on_action_popup_result = function (data: ActionPopupResultData): void {
  if (data.popup_id !== TOUR_POPUP_ID) return;

  switch (data.action_id) {
    case "next":
      nextStep();
      break;
    case "prev":
      prevStep();
      break;
    case "exit":
      exitTour();
      break;
  }
};

// ============================================================================
// Overlay Rendering
// ============================================================================

async function clearTourOverlays(): Promise<void> {
  // Clear overlays from all open buffers
  const buffers = editor.listBuffers();
  for (const buf of buffers) {
    editor.clearNamespace(buf.id, TOUR_NAMESPACE);
  }
}

async function renderStepOverlays(step: TourStep): Promise<void> {
  const bufferId = editor.findBufferByPath(step.file_path);
  if (!bufferId) return;

  // Clear previous overlays
  await clearTourOverlays();

  // Get line positions (convert from 1-indexed to 0-indexed)
  const startLine = step.lines[0] - 1;
  const endLine = step.lines[1] - 1;

  const startPos = await editor.getLineStartPosition(startLine);
  const endPos = await editor.getLineEndPosition(endLine);

  if (startPos === null || endPos === null) {
    editor.warn(`Tour: Could not get line positions for lines ${step.lines[0]}-${step.lines[1]}`);
    return;
  }

  // Add highlight overlay for active lines
  editor.addOverlay(bufferId, TOUR_NAMESPACE, startPos, endPos, {
    bg: [42, 74, 106], // Highlighted background color
    extendToLineEnd: true,
  });

  // If focus mode is enabled, we could dim surrounding lines
  // For now, just the highlight is sufficient
}

// ============================================================================
// Navigation
// ============================================================================

async function navigateToStep(stepIndex: number): Promise<void> {
  if (!tourManager.manifest) return;

  const step = tourManager.manifest.steps[stepIndex];
  if (!step) return;

  // Check if file exists (fileExists is sync, not async)
  const fileExists = editor.fileExists(step.file_path);

  if (!fileExists) {
    // Show error in popup but allow navigation to continue
    showStepPopup(
      step,
      stepIndex,
      tourManager.manifest.steps.length,
      "File not found"
    );
    return;
  }

  // Open the file at the starting line
  editor.openFile(step.file_path, step.lines[0], 1);

  // Wait a bit for the file to open
  await editor.delay(50);

  // Get the buffer ID after opening
  const bufferId = editor.findBufferByPath(step.file_path);
  if (bufferId) {
    // Center the view on the middle of the highlighted region
    const middleLine = Math.floor((step.lines[0] + step.lines[1]) / 2) - 1;
    const splitId = editor.getActiveSplitId();
    editor.scrollToLineCenter(splitId, bufferId, middleLine);

    // Render overlays
    await renderStepOverlays(step);

    // Track for detour detection
    tourManager.lastKnownBufferId = bufferId;
    const viewport = editor.getViewport();
    if (viewport) {
      tourManager.lastKnownTopByte = viewport.topByte;
    }
  }

  // Show explanation popup
  showStepPopup(step, stepIndex, tourManager.manifest.steps.length);
}

// ============================================================================
// Tour Lifecycle
// ============================================================================

async function loadTour(manifestPath: string): Promise<void> {
  try {
    // Read and parse manifest
    const content = editor.readFile(manifestPath);
    if (!content) {
      editor.error("Failed to read tour file: " + manifestPath);
      return;
    }
    const manifest: TourManifest = JSON.parse(content);

    // Validate schema version
    if (manifest.schema_version !== "1.0") {
      editor.error(`Unsupported tour schema version: ${manifest.schema_version}`);
      return;
    }

    // Validate steps
    if (!manifest.steps || manifest.steps.length === 0) {
      editor.error("Tour has no steps");
      return;
    }

    // Check commit hash if specified
    if (manifest.commit_hash) {
      const result = await editor.spawnProcess("git", [
        "rev-parse",
        "--short",
        "HEAD",
      ]);
      if (result.exit_code === 0) {
        const currentCommit = result.stdout.trim();
        if (!currentCommit.startsWith(manifest.commit_hash) &&
            !manifest.commit_hash.startsWith(currentCommit)) {
          editor.warn(
            `Tour was created for commit ${manifest.commit_hash}, current: ${currentCommit}`
          );
        }
      }
    }

    // Initialize tour
    tourManager.manifest = manifest;
    tourManager.state = { kind: "active", currentStep: 0, isPaused: false };

    // Navigate to first step
    await navigateToStep(0);

    // Set tour context for keybindings
    editor.setContext("tour-active", true);
  } catch (e) {
    editor.error(`Failed to load tour: ${e}`);
  }
}

function exitTour(): void {
  if (tourManager.state.kind !== "active") return;

  // Clear overlays
  clearTourOverlays();

  // Reset state
  tourManager.state = { kind: "idle" };
  tourManager.manifest = null;

  // Clear context
  editor.setContext("tour-active", false);

  editor.setStatus("Tour ended");
}

async function nextStep(): Promise<void> {
  if (tourManager.state.kind !== "active" || !tourManager.manifest) return;

  const newIndex = tourManager.state.currentStep + 1;
  if (newIndex >= tourManager.manifest.steps.length) {
    editor.setStatus("Tour: Already at last step");
    return;
  }

  tourManager.state = { ...tourManager.state, currentStep: newIndex, isPaused: false };
  await navigateToStep(newIndex);
}

async function prevStep(): Promise<void> {
  if (tourManager.state.kind !== "active" || !tourManager.manifest) return;

  const newIndex = tourManager.state.currentStep - 1;
  if (newIndex < 0) {
    editor.setStatus("Tour: Already at first step");
    return;
  }

  tourManager.state = { ...tourManager.state, currentStep: newIndex, isPaused: false };
  await navigateToStep(newIndex);
}

// ============================================================================
// Command Handlers
// ============================================================================

globalThis.tour_load = async function (): Promise<void> {
  // Prompt for tour file
  const result = await editor.prompt("Enter tour file path:", ".fresh-tour.json");

  if (result) {
    await loadTour(result);
  }
};

globalThis.tour_next = async function (): Promise<void> {
  await nextStep();
};

globalThis.tour_prev = async function (): Promise<void> {
  await prevStep();
};

globalThis.tour_exit = function (): void {
  exitTour();
};

// ============================================================================
// Registration
// ============================================================================

// Register commands
editor.registerCommand(
  "Tour: Load Definition...",
  "Load a .fresh-tour.json file to start a guided code tour",
  "tour_load",
  null
);

editor.registerCommand(
  "Tour: Next Step",
  "Go to the next step in the tour",
  "tour_next",
  "tour-active"
);

editor.registerCommand(
  "Tour: Previous Step",
  "Go to the previous step in the tour",
  "tour_prev",
  "tour-active"
);

editor.registerCommand(
  "Tour: Exit",
  "Exit the current tour",
  "tour_exit",
  "tour-active"
);

// Subscribe to action popup results for navigation buttons
editor.on("action_popup_result", "tour_on_action_popup_result");

editor.debug("Code Tour plugin loaded");
