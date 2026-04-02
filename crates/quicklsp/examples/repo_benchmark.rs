//! QuickLSP Unified Workspace Benchmark
//!
//! Tests the single Workspace engine against real open-source code.
//!
//! Usage: cargo run -p quicklsp --example repo_benchmark --release

use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use quicklsp::workspace::Workspace;

const SOURCE_EXTENSIONS: &[&str] = &[
    "rs", "py", "js", "ts", "tsx", "go", "c", "h", "cpp", "cc", "hpp", "java", "cs", "rb", "jsx",
    "mjs",
];

fn discover_source_files(root: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    discover_recursive(root, &mut files, 0);
    files.sort();
    files
}

fn discover_recursive(dir: &Path, files: &mut Vec<PathBuf>, depth: usize) {
    if depth > 20 {
        return;
    }
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
        if name.starts_with('.')
            || matches!(
                name,
                "node_modules"
                    | "target"
                    | "vendor"
                    | "build"
                    | "__pycache__"
                    | "dist"
                    | "third_party"
                    | "testdata"
            )
        {
            continue;
        }
        if path.is_dir() {
            discover_recursive(&path, files, depth + 1);
        } else if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
            if SOURCE_EXTENSIONS.contains(&ext) {
                files.push(path);
            }
        }
    }
}

fn read_file_lossy(path: &Path) -> Option<String> {
    let mut buf = Vec::new();
    fs::File::open(path).ok()?.read_to_end(&mut buf).ok()?;
    Some(String::from_utf8_lossy(&buf).into_owned())
}

struct Stats {
    samples: Vec<Duration>,
}
impl Stats {
    fn new() -> Self {
        Self {
            samples: Vec::new(),
        }
    }
    fn record(&mut self, d: Duration) {
        self.samples.push(d);
    }
    fn count(&self) -> usize {
        self.samples.len()
    }
    fn total(&self) -> Duration {
        self.samples.iter().sum()
    }
    fn mean(&self) -> Duration {
        if self.samples.is_empty() {
            Duration::ZERO
        } else {
            self.total() / self.samples.len() as u32
        }
    }
    fn median(&self) -> Duration {
        if self.samples.is_empty() {
            return Duration::ZERO;
        }
        let mut s = self.samples.clone();
        s.sort();
        s[s.len() / 2]
    }
    fn p99(&self) -> Duration {
        if self.samples.is_empty() {
            return Duration::ZERO;
        }
        let mut s = self.samples.clone();
        s.sort();
        s[(s.len() as f64 * 0.99) as usize]
    }
    fn max(&self) -> Duration {
        self.samples.iter().max().copied().unwrap_or(Duration::ZERO)
    }
    fn under(&self, t: Duration) -> (usize, f64) {
        let c = self.samples.iter().filter(|d| **d <= t).count();
        let pct = if self.samples.is_empty() {
            0.0
        } else {
            c as f64 / self.samples.len() as f64 * 100.0
        };
        (c, pct)
    }
}

fn fmt_dur(d: Duration) -> String {
    let us = d.as_micros();
    if us < 1000 {
        format!("{us} µs")
    } else if us < 1_000_000 {
        format!("{:.2} ms", us as f64 / 1000.0)
    } else {
        format!("{:.2} s", us as f64 / 1_000_000.0)
    }
}
fn fmt_bytes(b: usize) -> String {
    if b < 1024 {
        format!("{b} B")
    } else if b < 1024 * 1024 {
        format!("{:.1} KB", b as f64 / 1024.0)
    } else {
        format!("{:.1} MB", b as f64 / (1024.0 * 1024.0))
    }
}

fn main() {
    let repos_dir =
        std::env::var("QUICKLSP_REPOS_DIR").unwrap_or_else(|_| "/tmp/quicklsp-bench-repos".into());
    let repos_path = Path::new(&repos_dir);

    println!("╔══════════════════════════════════════════════════════════════╗");
    println!("║    QuickLSP Unified Workspace Benchmark                     ║");
    println!("╚══════════════════════════════════════════════════════════════╝\n");

    let all_files = discover_source_files(repos_path);
    if all_files.is_empty() {
        println!("No source files found in {repos_dir} — skipping benchmark.");
        println!("Clone repos first: ./crates/quicklsp/scripts/bench_repos.sh");
        return;
    }

    let total_bytes: u64 = all_files
        .iter()
        .filter_map(|f| fs::metadata(f).ok())
        .map(|m| m.len())
        .sum();

    println!("── Corpus ─────────────────────────────────────────────────────");
    println!("  Root:   {repos_dir}");
    println!("  Files:  {}", all_files.len());
    println!("  Size:   {}\n", fmt_bytes(total_bytes as usize));

    // 1. Indexing
    println!("── [1/4] Workspace Indexing ────────────────────────────────────");
    let ws = Workspace::new();
    let mut index_stats = Stats::new();
    let mut total_src = 0usize;
    let t0 = Instant::now();
    for file in &all_files {
        if let Some(source) = read_file_lossy(file) {
            total_src += source.len();
            let t = Instant::now();
            ws.index_file(file.clone(), source);
            index_stats.record(t.elapsed());
        }
    }
    let total_time = t0.elapsed();
    let (u1, p1) = index_stats.under(Duration::from_millis(1));
    println!("  Files:       {}", index_stats.count());
    println!("  Definitions: {}", ws.definition_count());
    println!("  Unique syms: {}", ws.unique_symbol_count());
    println!("  Source mem:  {}", fmt_bytes(total_src));
    println!("  Total time:  {}", fmt_dur(total_time));
    println!("  Mean/file:   {}", fmt_dur(index_stats.mean()));
    println!("  Median:      {}", fmt_dur(index_stats.median()));
    println!("  P99:         {}", fmt_dur(index_stats.p99()));
    println!("  Max:         {}", fmt_dur(index_stats.max()));
    println!("  <1ms: {u1}/{} ({p1:.1}%)\n", index_stats.count());

    // Collect symbols for queries
    let mut all_names: Vec<String> = Vec::new();
    for f in &all_files {
        for s in ws.file_symbols(f) {
            if !all_names.contains(&s.name) {
                all_names.push(s.name);
            }
            if all_names.len() >= 1000 {
                break;
            }
        }
        if all_names.len() >= 1000 {
            break;
        }
    }

    // 2. Definition lookup
    println!("── [2/4] Definition Lookup ─────────────────────────────────────");
    let mut ds = Stats::new();
    let n = all_names.len().min(500);
    let mut total_defs = 0;
    for name in all_names.iter().take(n) {
        let t = Instant::now();
        total_defs += ws.find_definitions(name).len();
        ds.record(t.elapsed());
    }
    let (du, dp) = ds.under(Duration::from_micros(10));
    println!("  Queries:     {n}");
    println!("  Defs found:  {total_defs}");
    println!("  Mean:        {}", fmt_dur(ds.mean()));
    println!("  Median:      {}", fmt_dur(ds.median()));
    println!("  P99:         {}", fmt_dur(ds.p99()));
    println!("  <10µs: {du}/{} ({dp:.1}%)\n", ds.count());

    // 3. Reference search
    println!("── [3/4] Reference Search ──────────────────────────────────────");
    let mut rs = Stats::new();
    let mut total_refs = 0;
    let rn = all_names.iter().take(50).collect::<Vec<_>>();
    for name in &rn {
        let t = Instant::now();
        total_refs += ws.find_references(name).len();
        rs.record(t.elapsed());
    }
    let (ru, rp) = rs.under(Duration::from_millis(50));
    println!("  Queries:     {}", rn.len());
    println!("  Refs found:  {total_refs}");
    println!("  Mean:        {}", fmt_dur(rs.mean()));
    println!("  Median:      {}", fmt_dur(rs.median()));
    println!("  P99:         {}", fmt_dur(rs.p99()));
    println!("  Max:         {}", fmt_dur(rs.max()));
    println!("  <50ms: {ru}/{} ({rp:.1}%)\n", rs.count());

    // 4. Fuzzy search
    println!("── [4/4] Fuzzy Workspace Symbol Search ─────────────────────────");
    let mut fs_stats = Stats::new();
    let mut correct = 0;
    let mut typo_count = 0;
    for name in all_names.iter().take(200) {
        if name.len() < 4 {
            continue;
        }
        let chars: Vec<char> = name.chars().collect();
        let mut t = chars.clone();
        t.swap(1, 2);
        let typo: String = t.into_iter().collect();
        let st = Instant::now();
        let results = ws.search_symbols(&typo);
        fs_stats.record(st.elapsed());
        typo_count += 1;
        if results.iter().any(|r| r.symbol.name == *name) {
            correct += 1;
        }
    }
    let acc = if typo_count > 0 {
        correct as f64 / typo_count as f64 * 100.0
    } else {
        0.0
    };
    let (fu, fp) = fs_stats.under(Duration::from_millis(1));
    println!("  Queries:     {typo_count}");
    println!("  Correct:     {correct}/{typo_count} ({acc:.1}%)");
    println!("  Mean:        {}", fmt_dur(fs_stats.mean()));
    println!("  Median:      {}", fmt_dur(fs_stats.median()));
    println!("  P99:         {}", fmt_dur(fs_stats.p99()));
    println!("  <1ms: {fu}/{} ({fp:.1}%)\n", fs_stats.count());

    println!("Done.");
}
