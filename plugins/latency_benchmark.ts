/**
 * Latency Benchmark Plugin
 *
 * Measures round-trip latency for plugin-to-editor communication.
 * Tests various operation types to identify bottlenecks.
 */

interface LatencyResult {
  operation: string;
  samples: number[];
  min: number;
  max: number;
  avg: number;
  p50: number;
  p95: number;
  p99: number;
}

interface BenchmarkResults {
  timestamp: string;
  results: LatencyResult[];
  summary: string;
}

// Store benchmark results
let benchmarkResults: BenchmarkResults | null = null;

// Calculate percentile from sorted array
function percentile(sortedArr: number[], p: number): number {
  if (sortedArr.length === 0) return 0;
  const index = Math.ceil((p / 100) * sortedArr.length) - 1;
  return sortedArr[Math.max(0, index)];
}

// Measure a single operation
async function measureOperation(
  name: string,
  iterations: number,
  operation: () => boolean | number | string | null | void
): Promise<LatencyResult> {
  const samples: number[] = [];

  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    operation();
    const end = performance.now();
    samples.push(end - start);
  }

  // Sort for percentile calculation
  const sorted = [...samples].sort((a, b) => a - b);

  return {
    operation: name,
    samples,
    min: sorted[0],
    max: sorted[sorted.length - 1],
    avg: samples.reduce((a, b) => a + b, 0) / samples.length,
    p50: percentile(sorted, 50),
    p95: percentile(sorted, 95),
    p99: percentile(sorted, 99),
  };
}

// Measure async operation
async function measureAsyncOperation(
  name: string,
  iterations: number,
  operation: () => Promise<unknown>
): Promise<LatencyResult> {
  const samples: number[] = [];

  for (let i = 0; i < iterations; i++) {
    const start = performance.now();
    await operation();
    const end = performance.now();
    samples.push(end - start);
  }

  // Sort for percentile calculation
  const sorted = [...samples].sort((a, b) => a - b);

  return {
    operation: name,
    samples,
    min: sorted[0],
    max: sorted[sorted.length - 1],
    avg: samples.reduce((a, b) => a + b, 0) / samples.length,
    p50: percentile(sorted, 50),
    p95: percentile(sorted, 95),
    p99: percentile(sorted, 99),
  };
}

// Main benchmark function
async function runBenchmark(): Promise<BenchmarkResults> {
  const results: LatencyResult[] = [];
  const iterations = 100;

  editor.setStatus("Running latency benchmark...");

  // Create test file for readFile benchmark
  try {
    await editor.writeFile("/tmp/.fresh_benchmark_test", "test content");
  } catch (e) {
    editor.debug(`Failed to create test file: ${e}`);
  }

  // Start Rust-side metrics collection
  (editor as any).metricsStart();

  // Get current buffer for tests
  const bufferId = editor.getActiveBufferId();

  // 1. State query operations (read from snapshot)
  results.push(await measureOperation(
    "getActiveBufferId",
    iterations,
    () => editor.getActiveBufferId()
  ));

  results.push(await measureOperation(
    "getCursorPosition",
    iterations,
    () => editor.getCursorPosition()
  ));

  results.push(await measureOperation(
    "getBufferLength",
    iterations,
    () => editor.getBufferLength(bufferId)
  ));

  results.push(await measureOperation(
    "isBufferModified",
    iterations,
    () => editor.isBufferModified(bufferId)
  ));

  results.push(await measureOperation(
    "getBufferPath",
    iterations,
    () => editor.getBufferPath(bufferId)
  ));

  results.push(await measureOperation(
    "getActiveSplitId",
    iterations,
    () => editor.getActiveSplitId()
  ));

  results.push(await measureOperation(
    "getPrimaryCursor",
    iterations,
    () => editor.getPrimaryCursor()
  ));

  results.push(await measureOperation(
    "getViewport",
    iterations,
    () => editor.getViewport()
  ));

  results.push(await measureOperation(
    "listBuffers",
    iterations,
    () => editor.listBuffers()
  ));

  // 2. Buffer text operations
  results.push(await measureOperation(
    "getBufferText (100 bytes)",
    iterations,
    () => editor.getBufferText(bufferId, 0, 100)
  ));

  results.push(await measureOperation(
    "getBufferText (1000 bytes)",
    iterations,
    () => editor.getBufferText(bufferId, 0, 1000)
  ));

  // 3. Command send operations (fire-and-forget to editor)
  results.push(await measureOperation(
    "setStatus",
    iterations,
    () => editor.setStatus("benchmark")
  ));

  // Overlay operations
  results.push(await measureOperation(
    "addOverlay",
    iterations,
    () => {
      const id = `bench_${Math.random()}`;
      return editor.addOverlay(bufferId, id, 0, 10, 255, 0, 0, false);
    }
  ));

  // Clean up overlays
  editor.removeOverlaysByPrefix(bufferId, "bench_");

  // 4. File system operations
  results.push(await measureOperation(
    "fileExists",
    iterations,
    () => editor.fileExists("/tmp/nonexistent")
  ));

  results.push(await measureOperation(
    "fileStat",
    iterations,
    () => editor.fileStat("/tmp")
  ));

  results.push(await measureOperation(
    "getCwd",
    iterations,
    () => editor.getCwd()
  ));

  results.push(await measureOperation(
    "getEnv",
    iterations,
    () => editor.getEnv("PATH")
  ));

  // 5. Path operations
  results.push(await measureOperation(
    "pathJoin",
    iterations,
    () => editor.pathJoin("/home", "user", "file.txt")
  ));

  results.push(await measureOperation(
    "pathBasename",
    iterations,
    () => editor.pathBasename("/home/user/file.txt")
  ));

  // 6. Async operations (actual round-trip)
  editor.setStatus("Benchmark: testing spawnProcess...");
  results.push(await measureAsyncOperation(
    "spawnProcess (echo)",
    10,
    () => editor.spawnProcess("echo", ["test"])
  ));

  editor.setStatus("Benchmark: testing readFile...");
  results.push(await measureAsyncOperation(
    "readFile (small)",
    10,
    () => editor.readFile("/tmp/.fresh_benchmark_test")
  ));

  // 7. Virtual buffer creation (full round-trip with response)
  editor.setStatus("Benchmark: testing createVirtualBufferInSplit...");
  results.push(await measureAsyncOperation(
    "createVirtualBufferInSplit",
    5,
    async () => {
      const id = await editor.createVirtualBufferInSplit({
        name: "*Benchmark*",
        mode: "special",
        read_only: true,
        entries: [{ text: "test\n", properties: {} }],
        ratio: 0.8,
        panel_id: "bench_panel",
        show_line_numbers: false,
        show_cursors: false,
      });
      // Clean up
      editor.closeBuffer(id);
    }
  ));

  // Stop Rust-side metrics collection and get report
  const rustMetricsReport = (editor as any).metricsStop();

  // Generate summary
  const summary = generateSummary(results, rustMetricsReport);

  const benchResults: BenchmarkResults = {
    timestamp: new Date().toISOString(),
    results,
    summary,
  };

  benchmarkResults = benchResults;

  editor.setStatus("Benchmark complete - use benchmark_show to view results");

  return benchResults;
}

function generateSummary(results: LatencyResult[], rustMetricsReport?: string): string {
  const lines: string[] = [
    "=== Plugin Latency Benchmark Results ===",
    "",
  ];

  // Group by operation type
  const syncOps = results.filter(r => !r.operation.includes("spawn") && !r.operation.includes("read") && !r.operation.includes("create"));
  const asyncOps = results.filter(r => r.operation.includes("spawn") || r.operation.includes("read") || r.operation.includes("create"));

  lines.push("SYNCHRONOUS OPERATIONS (Plugin -> Snapshot):");
  lines.push("-".repeat(80));
  lines.push(formatHeader());

  for (const r of syncOps) {
    lines.push(formatResult(r));
  }

  lines.push("");
  lines.push("ASYNCHRONOUS OPERATIONS (Full Round-Trip):");
  lines.push("-".repeat(80));
  lines.push(formatHeader());

  for (const r of asyncOps) {
    lines.push(formatResult(r));
  }

  // Analysis
  lines.push("");
  lines.push("=== Analysis ===");

  const avgSync = syncOps.reduce((sum, r) => sum + r.avg, 0) / syncOps.length;
  const avgAsync = asyncOps.reduce((sum, r) => sum + r.avg, 0) / asyncOps.length;

  lines.push(`Average sync operation: ${avgSync.toFixed(3)}ms`);
  lines.push(`Average async operation: ${avgAsync.toFixed(3)}ms`);

  // Find slowest operations
  const sorted = [...results].sort((a, b) => b.p95 - a.p95);
  lines.push("");
  lines.push("Slowest operations (p95):");
  for (let i = 0; i < Math.min(5, sorted.length); i++) {
    lines.push(`  ${i + 1}. ${sorted[i].operation}: ${sorted[i].p95.toFixed(3)}ms`);
  }

  // Identify bottlenecks
  lines.push("");
  lines.push("=== Bottleneck Analysis ===");

  const stateQueryOps = results.filter(r =>
    r.operation.includes("get") ||
    r.operation.includes("list") ||
    r.operation.includes("is")
  );
  const avgStateQuery = stateQueryOps.reduce((sum, r) => sum + r.avg, 0) / stateQueryOps.length;

  if (avgStateQuery > 0.1) {
    lines.push(`[WARN] State queries averaging ${avgStateQuery.toFixed(3)}ms - RwLock contention possible`);
  } else {
    lines.push(`[OK] State queries fast: ${avgStateQuery.toFixed(3)}ms avg`);
  }

  const createOp = results.find(r => r.operation.includes("createVirtualBuffer"));
  if (createOp && createOp.avg > 50) {
    lines.push(`[WARN] Virtual buffer creation slow: ${createOp.avg.toFixed(1)}ms avg`);
    lines.push("       Consider: caching, lazy initialization");
  } else if (createOp) {
    lines.push(`[OK] Virtual buffer creation: ${createOp.avg.toFixed(1)}ms avg`);
  }

  const spawnOp = results.find(r => r.operation.includes("spawn"));
  if (spawnOp) {
    lines.push(`[INFO] Process spawn: ${spawnOp.avg.toFixed(1)}ms avg (OS dependent)`);
  }

  // Add Rust-side metrics if available
  if (rustMetricsReport) {
    lines.push("");
    lines.push("");
    lines.push("=== Rust-Side Metrics (Command Processing) ===");
    lines.push("");
    lines.push(rustMetricsReport);
  }

  return lines.join("\n");
}

function formatHeader(): string {
  return `${"Operation".padEnd(35)} ${"Min".padStart(8)} ${"Avg".padStart(8)} ${"P50".padStart(8)} ${"P95".padStart(8)} ${"P99".padStart(8)} ${"Max".padStart(8)}`;
}

function formatResult(r: LatencyResult): string {
  return `${r.operation.padEnd(35)} ${r.min.toFixed(3).padStart(8)} ${r.avg.toFixed(3).padStart(8)} ${r.p50.toFixed(3).padStart(8)} ${r.p95.toFixed(3).padStart(8)} ${r.p99.toFixed(3).padStart(8)} ${r.max.toFixed(3).padStart(8)}`;
}

// Show results in a virtual buffer
async function showResults(): Promise<void> {
  if (!benchmarkResults) {
    editor.setStatus("No benchmark results - run benchmark first");
    return;
  }

  const lines = benchmarkResults.summary.split("\n");
  const entries = lines.map(line => ({
    text: line + "\n",
    properties: {},
  }));

  await editor.createVirtualBufferInSplit({
    name: "*Latency Benchmark*",
    mode: "special",
    read_only: true,
    entries,
    ratio: 0.5,
    panel_id: "latency_benchmark",
    show_line_numbers: false,
    show_cursors: false,
  });
}

// Export results as JSON
function exportResults(): string {
  if (!benchmarkResults) {
    return "{}";
  }
  return JSON.stringify(benchmarkResults, null, 2);
}

// Register actions
(globalThis as any).runLatencyBenchmark = runBenchmark;
(globalThis as any).showLatencyResults = showResults;
(globalThis as any).exportLatencyResults = exportResults;

// Register commands
editor.registerCommand(
  "benchmark_run",
  "Run plugin latency benchmark",
  "runLatencyBenchmark",
  ""
);

editor.registerCommand(
  "benchmark_show",
  "Show latency benchmark results",
  "showLatencyResults",
  ""
);

editor.setStatus("Latency benchmark plugin loaded - use :benchmark_run to start");
