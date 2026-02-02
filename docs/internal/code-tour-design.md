# Code Tour Feature Design

**Status**: Design Phase
**Author**: Claude
**Date**: 2026-02-02

## 1. Executive Summary

Code Tour is a JSON-driven walkthrough system that guides users through a codebase using visual overlays and explanatory text. This document analyzes whether the feature can be implemented as a plugin and identifies the required plugin API additions.

**Conclusion**: Code Tour CAN be built as a plugin with the addition of 4 new plugin API methods.

## 2. UX Philosophy (NN/g Heuristics)

| Heuristic | Implementation |
|-----------|----------------|
| **Visibility** | Persistent status indicator "Tour Mode: Step X/Y" in status bar |
| **User Control** | User can scroll away (pause) and resume with Tab key |
| **Recognition over Recall** | All commands in Command Palette, no obscure `:commands` |
| **Focus (Figure/Ground)** | Active lines highlighted, context lines dimmed |

## 3. Data Structure: Tour Manifest Schema

```typescript
interface TourStep {
  step_id: number;
  title: string;
  file_path: string;         // Relative to project root
  lines: [number, number];   // Start and End line (1-indexed)
  explanation: string;       // Markdown supported text
  overlay_config?: {
    type: 'block' | 'line';
    focus_mode: boolean;     // If true, dim non-active lines
  };
}

interface TourManifest {
  title: string;
  description: string;
  schema_version: "1.0";
  commit_hash?: string;      // Optional: verify source matches expected state
  steps: TourStep[];
}
```

**Example `.fresh-tour.json`**:
```json
{
  "title": "Fresh Plugin System Tour",
  "description": "Learn how plugins work in Fresh",
  "schema_version": "1.0",
  "commit_hash": "ee3bda2",
  "steps": [
    {
      "step_id": 1,
      "title": "Plugin Entry Point",
      "file_path": "crates/fresh-plugin-runtime/src/backend/quickjs_backend.rs",
      "lines": [1, 50],
      "explanation": "This is where plugins are loaded and executed...",
      "overlay_config": {
        "type": "block",
        "focus_mode": true
      }
    }
  ]
}
```

## 4. Plugin API Analysis

### 4.1 Existing APIs That Support Code Tour

| Capability | Plugin API | Code Tour Use |
|------------|-----------|---------------|
| Line highlighting | `addOverlay(bufferId, namespace, start, end, options)` | Highlight active tour step lines |
| Virtual text | `addVirtualText()`, `addVirtualLine()` | Step annotations/explanations |
| File navigation | `openFile(path, line, column)` | Navigate to step file/position |
| Command palette | `registerCommand(name, desc, handler, context)` | Tour: Next, Previous, Exit |
| Scroll detection | `ViewportChanged` hook | Detect user scroll (detour detection) |
| Cursor positioning | `setBufferCursor(bufferId, position)` | Position cursor at step |
| Scroll control | `setSplitScroll(splitId, topByte)` | Scroll to step location |
| Line byte offset | `getLineStartPosition(line)` async | Convert line numbers to byte offsets |
| File reading | `readFile(path)` | Load `.fresh-tour.json` manifest |
| File existence | `fileExists(path)` | Check if step files exist |
| Status messages | `setStatus(message)` | Show "Tour Mode" indicator |
| Custom keybindings | `defineMode(name, parent, bindings)` | Tour navigation keys |
| Prompts | `prompt(label, initial, suggestions)` | File picker for loading tours |
| Viewport info | `getViewport()` | Get current viewport dimensions |
| Buffer info | `getBufferInfo(bufferId)` | Get buffer path and length |

### 4.2 API Gaps Identified

The following capabilities are **missing** and need to be added:

#### Gap 1: `scrollToLineCenter(splitId, bufferId, line)`

**Problem**: `setSplitScroll()` requires a raw byte offset. There's no easy way to scroll such that a specific line is centered in the viewport.

**Solution**: Add a new API that:
1. Calculates the byte offset for the target line
2. Computes viewport height
3. Scrolls so the line appears in the center

```typescript
// Proposed API
editor.scrollToLineCenter(splitId: number, bufferId: number, line: number): boolean
```

#### Gap 2: `getLineEndPosition(bufferId, line)`

**Problem**: Only `getLineStartPosition()` exists. To highlight a line range, we need both start AND end byte offsets.

**Solution**: Add async method similar to `getLineStartPosition()`:

```typescript
// Proposed API
editor.getLineEndPosition(bufferId: number, line: number): Promise<number | null>
```

#### Gap 3: `getBufferLineCount(bufferId)`

**Problem**: To validate that step line ranges are valid, we need to know the total line count.

**Solution**: Add to EditorStateSnapshot and expose via API:

```typescript
// Proposed API
editor.getBufferLineCount(bufferId: number): number | null
```

#### Gap 4: Full-Line Background Overlay with "Extend to Line End"

**Problem**: Current overlays require exact byte ranges. For focus mode dimming, we need overlays that extend to the visual end of each line, regardless of actual content length.

**Existing Support**: The `Overlay` struct already has `extend_to_line_end: bool` field, but it's not exposed to the plugin API.

**Solution**: Expose `extendToLineEnd` option in `addOverlay()`:

```typescript
// Proposed API addition to OverlayOptions
interface OverlayOptions {
  // ... existing fields ...
  extendToLineEnd?: boolean;  // NEW: Extend background to visual line end
}
```

### 4.3 Features NOT Needed

| Feature | Reason Not Needed |
|---------|-------------------|
| Code Folding API | Fresh doesn't have folding implemented |
| Persistent Dock Panel | Virtual buffer with panel_id achieves similar result |

## 5. TourManager State Machine Design

```
                    ┌──────────────────────────────────────────────┐
                    │                                              │
                    ▼                                              │
              ┌─────────┐     load_tour()      ┌─────────────┐    │
              │  IDLE   │ ──────────────────▶  │   ACTIVE    │    │
              └─────────┘                      └─────────────┘    │
                    ▲                                │             │
                    │                                │             │
                    │          exit_tour()           │             │
                    └────────────────────────────────┘             │
                                                     │             │
                              user_scrolls_away()    │             │
                                     │               │             │
                                     ▼               │             │
                              ┌─────────────┐        │             │
                              │   PAUSED    │        │             │
                              │ (Detached)  │        │             │
                              └─────────────┘        │             │
                                     │               │             │
                         resume_location()           │             │
                                     │               │             │
                                     └───────────────┘             │
                                                                   │
                               next_step() / prev_step()           │
                                     │                             │
                                     └─────────────────────────────┘
```

### 5.1 State Definitions

```typescript
type TourState =
  | { kind: 'idle' }
  | { kind: 'active', currentStep: number, isPaused: boolean }

interface TourManager {
  state: TourState;
  manifest: TourManifest | null;
  dockBufferId: number | null;      // Virtual buffer for Tour Dock
  dockSplitId: number | null;       // Split containing the dock
  overlayNamespace: string;          // "code-tour" for cleanup

  // Track last known viewport for detour detection
  lastKnownTopByte: number;
  lastKnownBufferId: number;
}
```

### 5.2 State Transitions

| Current State | Event | Next State | Actions |
|--------------|-------|------------|---------|
| IDLE | `loadTour(manifest)` | ACTIVE(step=0) | Parse JSON, create dock, go to step 0 |
| ACTIVE | `nextStep()` | ACTIVE(step+1) | Clear overlays, navigate, highlight |
| ACTIVE | `prevStep()` | ACTIVE(step-1) | Clear overlays, navigate, highlight |
| ACTIVE | `viewport_changed` (user scroll) | ACTIVE(paused=true) | Dim dock, show "Paused" |
| ACTIVE(paused) | `resumeLocation()` | ACTIVE(paused=false) | Scroll back to step location |
| ACTIVE | `exitTour()` | IDLE | Clear overlays, close dock |
| ACTIVE | file missing | ACTIVE | Show "Broken Link" in dock, allow skip |

## 6. Implementation Architecture

### 6.1 Plugin File Structure

```
plugins/
└── code-tour/
    ├── index.ts           # Main plugin entry point
    ├── tour-manager.ts    # TourManager state machine
    ├── tour-renderer.ts   # Overlay and virtual text rendering
    ├── tour-dock.ts       # Tour Dock UI (virtual buffer)
    └── types.ts           # TypeScript types for manifest
```

### 6.2 Command Palette Integration

```typescript
// In index.ts
editor.registerCommand(
  "tour:load",
  "Tour: Load Definition...",
  "handleLoadTour",
  "normal"  // Available in normal editing context
);

editor.registerCommand(
  "tour:next",
  "Tour: Next Step",
  "handleNextStep",
  "tour-mode"  // Only when tour is active
);

editor.registerCommand(
  "tour:prev",
  "Tour: Previous Step",
  "handlePrevStep",
  "tour-mode"
);

editor.registerCommand(
  "tour:resume",
  "Tour: Resume Location",
  "handleResumeLocation",
  "tour-mode"
);

editor.registerCommand(
  "tour:exit",
  "Tour: Exit",
  "handleExitTour",
  "tour-mode"
);
```

### 6.3 Keybinding Mode

```typescript
editor.defineMode(
  "tour-mode",
  "normal",  // Parent mode
  [
    ["<Space>", "tour:next"],
    ["<Right>", "tour:next"],
    ["<Backspace>", "tour:prev"],
    ["<Left>", "tour:prev"],
    ["<Tab>", "tour:resume"],
    ["<Escape>", "tour:exit"],
  ],
  true  // read_only
);
```

### 6.4 Overlay Rendering Strategy

```typescript
async function renderStepOverlays(step: TourStep) {
  const bufferId = await editor.findBufferByPath(step.file_path);
  if (!bufferId) return;

  // Get line positions
  const startPos = await editor.getLineStartPosition(step.lines[0] - 1);
  const endPos = await editor.getLineEndPosition(step.lines[1] - 1);

  if (startPos === null || endPos === null) {
    // Handle version mismatch - clamp to file end
    return renderClampedOverlay(bufferId, step);
  }

  // Clear previous overlays
  editor.clearNamespace(bufferId, TOUR_NAMESPACE);

  // Add highlight overlay for active lines
  editor.addOverlay(bufferId, TOUR_NAMESPACE, startPos, endPos, {
    bg: "tour.active_line_bg",  // Theme key
    extendToLineEnd: true,
    priority: 100,
  });

  // If focus mode, dim surrounding context
  if (step.overlay_config?.focus_mode) {
    await renderDimmedContext(bufferId, startPos, endPos);
  }
}
```

### 6.5 Tour Dock Implementation

The Tour Dock is a virtual buffer that displays:
- Current step title and progress ("Step 2 of 5")
- Explanation text (with markdown rendering if supported)
- Navigation hints

```typescript
async function updateTourDock(step: TourStep, stepIndex: number, totalSteps: number) {
  const entries: TextPropertyEntry[] = [
    {
      text: `Step ${stepIndex + 1} of ${totalSteps}`,
      properties: { style: "bold", fg: [100, 200, 255] }
    },
    { text: `\n\n${step.title}\n`, properties: { style: "bold" } },
    { text: `\n${step.explanation}\n`, properties: {} },
    { text: `\n─────────────────────────\n`, properties: { fg: [80, 80, 80] } },
    { text: `Space/→ Next  ←/Backspace Prev  Tab Resume  Esc Exit`, properties: { fg: [120, 120, 120] } },
  ];

  if (tourManager.dockBufferId) {
    editor.setVirtualBufferContent(tourManager.dockBufferId, entries);
  }
}
```

### 6.6 Detour Detection

```typescript
// Subscribe to viewport changes
editor.on("viewport_changed", "handleViewportChanged");

function handleViewportChanged(event: ViewportChangedEvent) {
  if (tourManager.state.kind !== 'active') return;

  // Check if this is the buffer we're touring
  if (event.buffer_id !== tourManager.lastKnownBufferId) return;

  // If scroll position changed significantly, user has "wandered"
  const scrollDelta = Math.abs(event.top_byte - tourManager.lastKnownTopByte);
  const threshold = event.height * 50;  // ~50 bytes per line estimate

  if (scrollDelta > threshold && !tourManager.state.isPaused) {
    tourManager.state = { ...tourManager.state, isPaused: true };
    showPausedIndicator();
  }
}
```

## 7. Required Plugin API Changes

### 7.1 New PluginCommand Variants

Add to `fresh-core/src/api.rs`:

```rust
/// Scroll a split to center a specific line in the viewport
ScrollToLineCenter {
    split_id: SplitId,
    buffer_id: BufferId,
    line: usize,  // 0-indexed
},

/// Get the byte offset of the end of a line (async)
GetLineEndPosition {
    buffer_id: BufferId,
    line: usize,  // 0-indexed
    request_id: u64,
},

/// Get the total line count of a buffer
GetBufferLineCount {
    buffer_id: BufferId,
    request_id: u64,
},
```

### 7.2 New PluginResponse Variants

```rust
/// Response to GetLineEndPosition
LineEndPosition {
    request_id: u64,
    position: Option<usize>,
},

/// Response to GetBufferLineCount
BufferLineCount {
    request_id: u64,
    count: Option<usize>,
},
```

### 7.3 OverlayOptions Enhancement

Expose `extend_to_line_end` in the plugin API:

```rust
// In fresh-core/src/api.rs, OverlayOptions struct
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
pub struct OverlayOptions {
    // ... existing fields ...

    /// Extend the overlay background to the visual end of the line
    /// Useful for full-line highlighting effects
    #[serde(default)]
    pub extend_to_line_end: bool,
}
```

### 7.4 Implementation in Plugin Runtime

Add to `fresh-plugin-runtime/src/backend/quickjs_backend.rs`:

```rust
/// Scroll to center a line in the viewport
pub fn scroll_to_line_center(&self, split_id: u32, buffer_id: u32, line: u32) -> bool {
    self.command_sender
        .send(PluginCommand::ScrollToLineCenter {
            split_id: SplitId(split_id as usize),
            buffer_id: BufferId(buffer_id as usize),
            line: line as usize,
        })
        .is_ok()
}

/// Get the byte offset of the end of a line (0-indexed)
#[plugin_api(async_promise, js_name = "getLineEndPosition", ts_return = "number | null")]
#[qjs(rename = "_getLineEndPositionStart")]
pub fn get_line_end_position_start(&self, buffer_id: u32, line: u32) -> u64 {
    // Implementation similar to getLineStartPosition
}

/// Get total line count of a buffer
#[plugin_api(async_promise, js_name = "getBufferLineCount", ts_return = "number | null")]
#[qjs(rename = "_getBufferLineCountStart")]
pub fn get_buffer_line_count_start(&self, buffer_id: u32) -> u64 {
    // Implementation
}
```

### 7.5 Handler in Editor

Add to `fresh-editor/src/app/plugin_commands.rs`:

```rust
pub(super) fn handle_scroll_to_line_center(
    &mut self,
    split_id: SplitId,
    buffer_id: BufferId,
    line: usize,
) {
    if let Some(split_state) = self.split_states.get_mut(&split_id) {
        if let Some(buffer_state) = self.buffers.get_mut(&buffer_id) {
            // Calculate byte position for line start
            let line_start = buffer_state.state.buffer.line_start_byte(line);

            // Get viewport height
            let viewport_height = split_state.viewport.height as usize;

            // Calculate offset to center the line
            let lines_above = viewport_height / 2;
            let target_line = line.saturating_sub(lines_above);
            let target_byte = buffer_state.state.buffer.line_start_byte(target_line);

            split_state.viewport.scroll_to(&mut buffer_state.state.buffer, target_line);
        }
    }
}
```

## 8. Edge Case Handling

### 8.1 Missing File

```typescript
async function navigateToStep(step: TourStep) {
  const exists = await editor.fileExists(step.file_path);

  if (!exists) {
    // Show broken link indicator in dock
    updateTourDock({
      ...step,
      title: `[File Not Found] ${step.title}`,
      explanation: `The file "${step.file_path}" could not be found.\n\nPress Space to skip to the next step.`
    }, currentStep, totalSteps);

    // Add warning icon to gutter
    editor.setLineIndicator(dockBufferId, 0, "tour-warning", "⚠", [255, 200, 0], 100);
    return;
  }

  // Proceed with normal navigation
  await editor.openFile(step.file_path, step.lines[0], 1);
}
```

### 8.2 Version Mismatch (File Shorter Than Expected)

```typescript
async function renderStepOverlays(step: TourStep) {
  const lineCount = await editor.getBufferLineCount(bufferId);

  if (lineCount !== null && step.lines[1] > lineCount) {
    // Clamp to actual file length
    const clampedEnd = lineCount;

    // Show warning in dock
    editor.setStatus(`Warning: File has ${lineCount} lines, tour expects ${step.lines[1]}`);

    // Highlight what we can
    const startPos = await editor.getLineStartPosition(step.lines[0] - 1);
    const endPos = await editor.getLineEndPosition(clampedEnd - 1);

    if (startPos !== null && endPos !== null) {
      editor.addOverlay(bufferId, TOUR_NAMESPACE, startPos, endPos, {
        bg: [80, 60, 0],  // Amber warning color
        extendToLineEnd: true,
      });
    }
    return;
  }

  // Normal rendering
  // ...
}
```

### 8.3 Commit Hash Verification

```typescript
async function loadTour(manifestPath: string) {
  const content = await editor.readFile(manifestPath);
  const manifest: TourManifest = JSON.parse(content);

  if (manifest.commit_hash) {
    // Verify current commit matches
    const result = await editor.spawnProcess("git", ["rev-parse", "--short", "HEAD"]);
    const currentCommit = result.stdout.trim();

    if (currentCommit !== manifest.commit_hash) {
      const proceed = await editor.prompt(
        `Tour was created for commit ${manifest.commit_hash}, but current commit is ${currentCommit}. Continue anyway?`,
        "",
        [
          { text: "Yes, continue", value: "yes" },
          { text: "No, cancel", value: "no" }
        ]
      );

      if (proceed !== "yes") return;
    }
  }

  // Continue loading tour
  initializeTour(manifest);
}
```

## 9. Theme Integration

Add theme keys for Code Tour in theme schema:

```json
{
  "tour": {
    "active_line_bg": "#2a4a6a",
    "dimmed_line_fg": "#606060",
    "dimmed_line_bg": "#1a1a1a",
    "dock_header_fg": "#64b5f6",
    "dock_hint_fg": "#808080",
    "warning_fg": "#ffcc00",
    "error_fg": "#ff6666"
  }
}
```

## 10. Implementation Phases

### Phase 1: API Additions (This PR)
- [ ] Add `scrollToLineCenter` command
- [ ] Add `getLineEndPosition` async API
- [ ] Add `getBufferLineCount` async API
- [ ] Expose `extendToLineEnd` in overlay options
- [ ] Add theme keys for tour colors
- [ ] Regenerate TypeScript definitions

### Phase 2: Core Plugin
- [ ] Create `plugins/code-tour/` directory
- [ ] Implement TourManager state machine
- [ ] Implement tour loading and validation
- [ ] Implement step navigation
- [ ] Implement Tour Dock virtual buffer

### Phase 3: Visual Polish
- [ ] Implement focus mode dimming
- [ ] Implement detour detection
- [ ] Implement resume functionality
- [ ] Add keyboard hints overlay

### Phase 4: Testing & Documentation
- [ ] Create sample `.fresh-tour.json` for Fresh codebase
- [ ] Write user documentation
- [ ] Add to plugin marketplace

## 11. Summary

Code Tour **can be implemented as a plugin** with the following API additions:

| API | Purpose | Complexity |
|-----|---------|------------|
| `scrollToLineCenter()` | Center viewport on line | Low |
| `getLineEndPosition()` | Get line end byte offset | Low |
| `getBufferLineCount()` | Validate line ranges | Low |
| `extendToLineEnd` overlay option | Full-line highlighting | Very Low |

Total estimated effort: **~2-3 days** for API additions, **~3-5 days** for plugin implementation.

The existing plugin API already provides ~85% of the required functionality. The gaps identified are straightforward to implement and follow established patterns in the codebase.
