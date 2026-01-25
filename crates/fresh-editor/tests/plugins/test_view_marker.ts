/// <reference path="../../plugins/lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Test plugin for virtual lines (Emacs-like persistent state model).
 *
 * This plugin demonstrates the virtual lines API by injecting header lines
 * above content. Virtual lines are added to persistent state and rendered
 * synchronously from memory - no view transform hooks needed.
 */

let activeBufferId: number | null = null;
const padLinesByBuffer = new Map<number, number>();
const interleavedModeByBuffer = new Map<number, boolean>();

const TEST_NAMESPACE = "test-view-marker";

// Define a simple mode for testing
editor.defineMode(
  "test-view-marker",
  "normal",  // parent mode
  [
    ["q", "test_view_marker_close"],
  ],
  true // read-only
);

/**
 * Add virtual header lines for a buffer
 */
function addTestHeaders(bufferId: number): void {
  const padLines = padLinesByBuffer.get(bufferId) ?? 0;
  const interleavedMode = interleavedModeByBuffer.get(bufferId) ?? false;

  // Clear existing headers first
  editor.clearVirtualTextNamespace(bufferId, TEST_NAMESPACE);

  if (interleavedMode) {
    // Interleaved mode: add a header above each source line
    const content = "Line 1\nLine 2\nLine 3\n";
    let byteOffset = 0;
    let lineNum = 1;

    for (const line of content.split('\n')) {
      if (line.length === 0 && byteOffset >= content.length - 1) break;

      // Add header above this line
      editor.addVirtualLine(
        bufferId,
        byteOffset,
        `── Header before line ${lineNum} ──`,
        200, 200, 100,  // Yellow-ish fg
        0, 0, 0,        // Black background (u8 values required)
        true,           // above
        TEST_NAMESPACE,
        0
      );

      byteOffset += line.length + 1; // +1 for newline
      lineNum++;
    }

    // Also add initial header at byte 0
    editor.addVirtualLine(
      bufferId,
      0,
      "== INTERLEAVED HEADER ==",
      255, 255, 0,  // Yellow fg
      0, 0, 0,      // Black background (u8 values required)
      true,         // above
      TEST_NAMESPACE,
      -1  // lower priority to appear first
    );
  } else {
    // Simple mode: just one header at byte 0
    editor.addVirtualLine(
      bufferId,
      0,
      "== HEADER AT BYTE 0 ==",
      255, 255, 0,  // Yellow fg
      0, 0, 0,      // Black background (u8 values required)
      true,         // above
      TEST_NAMESPACE,
      0
    );

    // Optionally add many pad lines (for scroll stress tests)
    for (let i = 0; i < padLines; i++) {
      editor.addVirtualLine(
        bufferId,
        0,
        `Virtual pad ${i + 1}`,
        180, 180, 180,  // Light gray fg
        0, 0, 0,        // Black background (u8 values required)
        true,           // above
        TEST_NAMESPACE,
        i + 1  // increasing priority so they appear in order after header
      );
    }
  }

  editor.debug(`[test_view_marker] added ${interleavedMode ? 'interleaved' : 'simple'} headers with ${padLines} pad lines`);
}

async function open_test_view_marker(padLines: number, name: string): Promise<void> {
  const splitId = editor.getActiveSplitId();

  editor.debug(
    `[test_view_marker] opening view marker in split ${splitId} with ${padLines} pad lines`
  );

  // Create virtual buffer with simple hardcoded content
  const entries: TextPropertyEntry[] = [
    { text: "Line 1\n", properties: { type: "content", line: 1 } },
    { text: "Line 2\n", properties: { type: "content", line: 2 } },
    { text: "Line 3\n", properties: { type: "content", line: 3 } },
  ];

  const result = await editor.createVirtualBufferInExistingSplit({
    name,
    mode: "test-view-marker",
    readOnly: true,
    entries,
    splitId: splitId,
    showLineNumbers: true,
    showCursors: true,
    editingDisabled: true,
  });
  const bufferId = result.bufferId;

  if (bufferId != null) {
    activeBufferId = bufferId;
    padLinesByBuffer.set(bufferId, padLines);
    interleavedModeByBuffer.set(bufferId, false);

    // Add virtual header lines
    addTestHeaders(bufferId);

    editor.debug(
      `[test_view_marker] buffer created with id ${bufferId}, padLines=${padLines}`
    );
    editor.setStatus("Test view marker active - press q to close");
  } else {
    editor.debug(`[test_view_marker] failed to create buffer`);
    editor.setStatus("Failed to create test view marker buffer");
  }
}

async function open_test_view_marker_interleaved(name: string): Promise<void> {
  const splitId = editor.getActiveSplitId();

  editor.debug(`[test_view_marker] opening interleaved view marker in split ${splitId}`);

  // Create virtual buffer with simple hardcoded content
  const entries: TextPropertyEntry[] = [
    { text: "Line 1\n", properties: { type: "content", line: 1 } },
    { text: "Line 2\n", properties: { type: "content", line: 2 } },
    { text: "Line 3\n", properties: { type: "content", line: 3 } },
  ];

  const bufferId = await editor.createVirtualBufferInExistingSplit({
    name,
    mode: "test-view-marker",
    readOnly: true,
    entries,
    splitId: splitId,
    showLineNumbers: true,
    showCursors: true,
    editingDisabled: true,
  });

  if (bufferId !== null) {
    activeBufferId = bufferId;
    padLinesByBuffer.set(bufferId, 0);
    interleavedModeByBuffer.set(bufferId, true);

    // Add virtual header lines in interleaved mode
    addTestHeaders(bufferId);

    editor.debug(`[test_view_marker] interleaved buffer created with id ${bufferId}`);
    editor.setStatus("Test view marker (interleaved) active - press q to close");
  } else {
    editor.debug(`[test_view_marker] failed to create buffer`);
    editor.setStatus("Failed to create test view marker buffer");
  }
}

globalThis.show_test_view_marker = async function(): Promise<void> {
  await open_test_view_marker(0, "*test-view-marker*");
};

globalThis.show_test_view_marker_many_virtual_lines = async function(): Promise<void> {
  await open_test_view_marker(120, "*test-view-marker-many*");
};

globalThis.show_test_view_marker_interleaved = async function(): Promise<void> {
  await open_test_view_marker_interleaved("*test-view-marker-interleaved*");
};

/**
 * Close the test view marker
 */
globalThis.test_view_marker_close = function(): void {
  if (activeBufferId !== null) {
    // Clear virtual lines before closing
    editor.clearVirtualTextNamespace(activeBufferId, TEST_NAMESPACE);

    editor.closeBuffer(activeBufferId);
    padLinesByBuffer.delete(activeBufferId);
    interleavedModeByBuffer.delete(activeBufferId);
    activeBufferId = null;
    editor.setStatus("Test view marker closed");
  }
};

// Register command
editor.registerCommand(
  "Test View Marker",
  "Test virtual lines with header at byte 0",
  "show_test_view_marker",
  null
);

editor.registerCommand(
  "Test View Marker (Many Virtual Lines)",
  "Test virtual lines with many header lines",
  "show_test_view_marker_many_virtual_lines",
  null
);

editor.registerCommand(
  "Test View Marker (Interleaved)",
  "Test virtual lines with headers between each source line",
  "show_test_view_marker_interleaved",
  null
);

editor.setStatus("Test View Marker plugin loaded (virtual lines)");
