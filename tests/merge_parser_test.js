/**
 * Unit tests for merge conflict parser regex
 * Run with: node tests/merge_parser_test.js
 */

// The exact regex from the plugin
const conflictRegex = /^<<<<<<<[^\n]*\n([\s\S]*?)(?:^\|\|\|\|\|\|\|[^\n]*\n([\s\S]*?))?^=======\n([\s\S]*?)^>>>>>>>[^\n]*$/gm;

function parseConflicts(content) {
  const conflicts = [];

  // Reset regex state
  conflictRegex.lastIndex = 0;

  let match;
  while ((match = conflictRegex.exec(content)) !== null) {
    conflicts.push({
      startOffset: match.index,
      endOffset: match.index + match[0].length,
      ours: match[1] || "",
      base: match[2] || "",
      theirs: match[3] || "",
    });
  }

  return conflicts;
}

// Test cases
const tests = [
  {
    name: "Simple 2-way conflict",
    content: `<<<<<<< HEAD
ours
=======
theirs
>>>>>>> branch
`,
    expected: { count: 1, hasBase: false },
  },
  {
    name: "Diff3-style conflict with base section",
    content: `}

static int showdf(char *mntdir, struct obd_statfs *stat,
<<<<<<< HEAD
                  char *uuid, enum mntdf_flags flags,
                  char *type, int index, int rc)
||||||| parent of a3f05d81f6 (LU-18243 lfs: Add --output and --no-header options to lfs df command)
                  const char *uuid, enum mntdf_flags flags,
                  char *type, int index, int rc)
=======
                  const char *uuid, enum mntdf_flags flags,
                  char *type, int index, int rc, enum showdf_fields fields,
                  enum showdf_fields *field_order, int field_count)
>>>>>>> a3f05d81f6 (LU-18243 lfs: Add --output and --no-header options to lfs df command)
{
        int base = flags & MNTDF_DECIMAL ? 1000 : 1024;
        char *suffix = flags & MNTDF_DECIMAL ? "kMGTPEZY" : "KMGTPEZY";
        int shift = flags & MNTDF_COOKED ? 0 : 10;
`,
    expected: { count: 1, hasBase: true },
  },
  {
    name: "Multiple conflicts",
    content: `<<<<<<< HEAD
first ours
=======
first theirs
>>>>>>> branch

middle text

<<<<<<< HEAD
second ours
=======
second theirs
>>>>>>> branch
`,
    expected: { count: 2, hasBase: false },
  },
  {
    name: "Conflict with multiline content",
    content: `<<<<<<< HEAD
line 1
line 2
line 3
=======
other line 1
other line 2
>>>>>>> branch
`,
    expected: { count: 1, hasBase: false },
  },
];

// Run tests
let passed = 0;
let failed = 0;

for (const test of tests) {
  const result = parseConflicts(test.content);

  let success = true;
  let error = "";

  if (result.length !== test.expected.count) {
    success = false;
    error = `Expected ${test.expected.count} conflicts, got ${result.length}`;
  } else if (test.expected.hasBase !== undefined && result.length > 0) {
    const hasBase = result[0].base.trim().length > 0;
    if (hasBase !== test.expected.hasBase) {
      success = false;
      error = `Expected hasBase=${test.expected.hasBase}, got hasBase=${hasBase}`;
    }
  }

  if (success) {
    console.log(`✓ ${test.name}`);
    passed++;
  } else {
    console.log(`✗ ${test.name}: ${error}`);
    console.log("  Content preview:", test.content.substring(0, 100).replace(/\n/g, "\\n") + "...");
    if (result.length > 0) {
      console.log("  Parsed ours:", JSON.stringify(result[0].ours.substring(0, 50)));
      console.log("  Parsed base:", JSON.stringify(result[0].base.substring(0, 50)));
      console.log("  Parsed theirs:", JSON.stringify(result[0].theirs.substring(0, 50)));
    }
    failed++;
  }
}

console.log(`\n${passed} passed, ${failed} failed`);

if (failed > 0) {
  process.exit(1);
}
