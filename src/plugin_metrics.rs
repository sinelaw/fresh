//! Plugin Latency Metrics Collection
//!
//! This module collects timing metrics for plugin-editor communication
//! to help identify latency bottlenecks.

use std::collections::HashMap;
use std::sync::Mutex;
use std::time::{Duration, Instant};

/// Global metrics collector
static METRICS: Mutex<Option<PluginMetrics>> = Mutex::new(None);

/// Metrics for plugin operations
#[derive(Debug, Default)]
pub struct PluginMetrics {
    /// Command processing times by command type
    pub command_times: HashMap<String, Vec<Duration>>,
    /// Hook execution times by hook name
    pub hook_times: HashMap<String, Vec<Duration>>,
    /// State snapshot update times
    pub snapshot_times: Vec<Duration>,
    /// Last time metrics were reset
    pub started_at: Option<Instant>,
}

impl PluginMetrics {
    /// Start collecting metrics
    pub fn start() {
        let mut guard = METRICS.lock().unwrap();
        *guard = Some(PluginMetrics {
            command_times: HashMap::new(),
            hook_times: HashMap::new(),
            snapshot_times: Vec::new(),
            started_at: Some(Instant::now()),
        });
        tracing::info!("Plugin metrics collection started");
    }

    /// Stop collecting metrics and return results
    pub fn stop() -> Option<PluginMetrics> {
        let mut guard = METRICS.lock().unwrap();
        guard.take()
    }

    /// Check if metrics collection is active
    pub fn is_active() -> bool {
        METRICS.lock().unwrap().is_some()
    }

    /// Record a command processing time
    pub fn record_command(command_type: &str, duration: Duration) {
        if let Ok(mut guard) = METRICS.lock() {
            if let Some(ref mut metrics) = *guard {
                metrics
                    .command_times
                    .entry(command_type.to_string())
                    .or_default()
                    .push(duration);
            }
        }
    }

    /// Record a hook execution time
    pub fn record_hook(hook_name: &str, duration: Duration) {
        if let Ok(mut guard) = METRICS.lock() {
            if let Some(ref mut metrics) = *guard {
                metrics
                    .hook_times
                    .entry(hook_name.to_string())
                    .or_default()
                    .push(duration);
            }
        }
    }

    /// Record a state snapshot update time
    pub fn record_snapshot(duration: Duration) {
        if let Ok(mut guard) = METRICS.lock() {
            if let Some(ref mut metrics) = *guard {
                metrics.snapshot_times.push(duration);
            }
        }
    }

    /// Generate a summary report
    pub fn generate_report(metrics: &PluginMetrics) -> String {
        let mut lines = vec![
            "=== Plugin Metrics Report ===".to_string(),
            String::new(),
        ];

        if let Some(started) = metrics.started_at {
            let duration = started.elapsed();
            lines.push(format!("Collection duration: {:.2}s", duration.as_secs_f64()));
            lines.push(String::new());
        }

        // Command processing times
        lines.push("COMMAND PROCESSING TIMES:".to_string());
        lines.push("-".repeat(80));
        lines.push(format!(
            "{:<40} {:>8} {:>10} {:>10} {:>10}",
            "Command Type", "Count", "Avg (us)", "P95 (us)", "Max (us)"
        ));

        let mut sorted_commands: Vec<_> = metrics.command_times.iter().collect();
        sorted_commands.sort_by_key(|(k, _)| k.as_str());

        for (cmd_type, times) in sorted_commands {
            if times.is_empty() {
                continue;
            }
            let stats = calculate_stats(times);
            lines.push(format!(
                "{:<40} {:>8} {:>10.1} {:>10.1} {:>10.1}",
                cmd_type,
                times.len(),
                stats.avg.as_micros() as f64,
                stats.p95.as_micros() as f64,
                stats.max.as_micros() as f64
            ));
        }

        // Hook execution times
        if !metrics.hook_times.is_empty() {
            lines.push(String::new());
            lines.push("HOOK EXECUTION TIMES:".to_string());
            lines.push("-".repeat(80));
            lines.push(format!(
                "{:<40} {:>8} {:>10} {:>10} {:>10}",
                "Hook Name", "Count", "Avg (us)", "P95 (us)", "Max (us)"
            ));

            let mut sorted_hooks: Vec<_> = metrics.hook_times.iter().collect();
            sorted_hooks.sort_by_key(|(k, _)| k.as_str());

            for (hook_name, times) in sorted_hooks {
                if times.is_empty() {
                    continue;
                }
                let stats = calculate_stats(times);
                lines.push(format!(
                    "{:<40} {:>8} {:>10.1} {:>10.1} {:>10.1}",
                    hook_name,
                    times.len(),
                    stats.avg.as_micros() as f64,
                    stats.p95.as_micros() as f64,
                    stats.max.as_micros() as f64
                ));
            }
        }

        // Snapshot update times
        if !metrics.snapshot_times.is_empty() {
            lines.push(String::new());
            lines.push("STATE SNAPSHOT UPDATES:".to_string());
            lines.push("-".repeat(80));
            let stats = calculate_stats(&metrics.snapshot_times);
            lines.push(format!("Count: {}", metrics.snapshot_times.len()));
            lines.push(format!("Average: {:.1} us", stats.avg.as_micros() as f64));
            lines.push(format!("P95: {:.1} us", stats.p95.as_micros() as f64));
            lines.push(format!("Max: {:.1} us", stats.max.as_micros() as f64));
        }

        // Summary
        lines.push(String::new());
        lines.push("=== Summary ===".to_string());

        let total_commands: usize = metrics.command_times.values().map(|v| v.len()).sum();
        let total_hooks: usize = metrics.hook_times.values().map(|v| v.len()).sum();

        lines.push(format!("Total commands processed: {}", total_commands));
        lines.push(format!("Total hooks executed: {}", total_hooks));
        lines.push(format!(
            "Total snapshot updates: {}",
            metrics.snapshot_times.len()
        ));

        lines.join("\n")
    }
}

struct Stats {
    avg: Duration,
    p95: Duration,
    max: Duration,
}

fn calculate_stats(times: &[Duration]) -> Stats {
    if times.is_empty() {
        return Stats {
            avg: Duration::ZERO,
            p95: Duration::ZERO,
            max: Duration::ZERO,
        };
    }

    let sum: Duration = times.iter().sum();
    let avg = sum / times.len() as u32;

    let mut sorted: Vec<_> = times.iter().copied().collect();
    sorted.sort();

    let p95_idx = ((times.len() as f64 * 0.95) as usize).saturating_sub(1);
    let p95 = sorted[p95_idx.min(sorted.len() - 1)];
    let max = *sorted.last().unwrap();

    Stats { avg, p95, max }
}

/// Guard that records timing when dropped
pub struct TimingGuard {
    start: Instant,
    record_fn: Box<dyn FnOnce(Duration) + Send>,
}

impl TimingGuard {
    pub fn for_command(command_type: String) -> Self {
        Self {
            start: Instant::now(),
            record_fn: Box::new(move |d| PluginMetrics::record_command(&command_type, d)),
        }
    }

    pub fn for_hook(hook_name: String) -> Self {
        Self {
            start: Instant::now(),
            record_fn: Box::new(move |d| PluginMetrics::record_hook(&hook_name, d)),
        }
    }

    pub fn for_snapshot() -> Self {
        Self {
            start: Instant::now(),
            record_fn: Box::new(PluginMetrics::record_snapshot),
        }
    }
}

impl Drop for TimingGuard {
    fn drop(&mut self) {
        let duration = self.start.elapsed();
        // We need to swap out the closure to call it
        let record_fn = std::mem::replace(&mut self.record_fn, Box::new(|_| {}));
        record_fn(duration);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_metrics_collection() {
        PluginMetrics::start();

        PluginMetrics::record_command("InsertText", Duration::from_micros(100));
        PluginMetrics::record_command("InsertText", Duration::from_micros(150));
        PluginMetrics::record_command("SetStatus", Duration::from_micros(50));

        let metrics = PluginMetrics::stop().unwrap();

        assert_eq!(metrics.command_times["InsertText"].len(), 2);
        assert_eq!(metrics.command_times["SetStatus"].len(), 1);
    }

    #[test]
    fn test_stats_calculation() {
        let times = vec![
            Duration::from_micros(100),
            Duration::from_micros(200),
            Duration::from_micros(300),
            Duration::from_micros(400),
            Duration::from_micros(500),
        ];

        let stats = calculate_stats(&times);
        assert_eq!(stats.avg, Duration::from_micros(300));
        assert_eq!(stats.max, Duration::from_micros(500));
    }
}
