/// Process resource limiting infrastructure
///
/// Provides cross-platform support for limiting memory and CPU usage of spawned processes.
/// On Linux, uses setrlimit and cgroups. On other platforms, this is a TODO.

use serde::{Deserialize, Serialize};
use std::io;

/// Configuration for process resource limits
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ProcessLimits {
    /// Maximum memory usage in megabytes (None = no limit)
    #[serde(default)]
    pub max_memory_mb: Option<u64>,

    /// Maximum CPU usage as percentage of total CPU (None = no limit)
    /// For multi-core systems, 100% = 1 core, 200% = 2 cores, etc.
    #[serde(default)]
    pub max_cpu_percent: Option<u32>,

    /// Enable resource limiting (can be disabled per-platform)
    #[serde(default = "default_true")]
    pub enabled: bool,
}

fn default_true() -> bool {
    true
}

impl Default for ProcessLimits {
    fn default() -> Self {
        Self {
            max_memory_mb: Self::default_memory_limit_mb(),
            max_cpu_percent: Some(90), // 90% of total CPU
            enabled: cfg!(target_os = "linux"), // Only enabled on Linux by default
        }
    }
}

impl ProcessLimits {
    /// Get the default memory limit (50% of total system memory)
    pub fn default_memory_limit_mb() -> Option<u64> {
        SystemResources::total_memory_mb()
            .map(|total| total / 2) // 50% of total memory
            .ok()
    }

    /// Get the default CPU limit (90% of total CPU)
    pub fn default_cpu_limit_percent() -> u32 {
        90
    }

    /// Create a new ProcessLimits with no restrictions
    pub fn unlimited() -> Self {
        Self {
            max_memory_mb: None,
            max_cpu_percent: None,
            enabled: false,
        }
    }

    /// Apply these limits to a tokio Command before spawning
    ///
    /// This configures the command to have resource limits applied when the process starts.
    /// On Linux, uses setrlimit. On other platforms, this is currently a no-op (TODO).
    pub fn apply_to_command(&self, cmd: &mut tokio::process::Command) -> io::Result<()> {
        if !self.enabled {
            return Ok(());
        }

        #[cfg(target_os = "linux")]
        {
            self.apply_linux_limits(cmd)
        }

        #[cfg(not(target_os = "linux"))]
        {
            // TODO: Implement for macOS using setrlimit
            // TODO: Implement for Windows using Job Objects
            tracing::warn!("Process resource limits are not yet implemented for this platform");
            Ok(())
        }
    }

    #[cfg(target_os = "linux")]
    fn apply_linux_limits(&self, cmd: &mut tokio::process::Command) -> io::Result<()> {
        use std::os::unix::process::CommandExt;

        let max_memory_bytes = self.max_memory_mb.map(|mb| mb * 1024 * 1024);
        let max_cpu_percent = self.max_cpu_percent;

        unsafe {
            cmd.pre_exec(move || {
                // Apply memory limit using RLIMIT_AS (virtual memory)
                if let Some(mem_limit) = max_memory_bytes {
                    match apply_memory_limit(mem_limit) {
                        Ok(()) => tracing::debug!("Applied memory limit: {} MB", mem_limit / 1024 / 1024),
                        Err(e) => tracing::warn!("Failed to apply memory limit: {}", e),
                    }
                }

                // Apply CPU limit using cgroups or CPU time rlimit
                if let Some(cpu_percent) = max_cpu_percent {
                    match apply_cpu_limit(cpu_percent) {
                        Ok(()) => tracing::debug!("Applied CPU limit: {}%", cpu_percent),
                        Err(e) => tracing::warn!("Failed to apply CPU limit: {}", e),
                    }
                }

                Ok(())
            });
        }

        Ok(())
    }
}

/// System resource information utilities
pub struct SystemResources;

impl SystemResources {
    /// Get total system memory in megabytes
    pub fn total_memory_mb() -> io::Result<u64> {
        #[cfg(target_os = "linux")]
        {
            Self::linux_total_memory_mb()
        }

        #[cfg(not(target_os = "linux"))]
        {
            // TODO: Implement for other platforms
            Err(io::Error::new(
                io::ErrorKind::Unsupported,
                "Memory detection not implemented for this platform"
            ))
        }
    }

    #[cfg(target_os = "linux")]
    fn linux_total_memory_mb() -> io::Result<u64> {
        // Read from /proc/meminfo
        let meminfo = std::fs::read_to_string("/proc/meminfo")?;

        for line in meminfo.lines() {
            if line.starts_with("MemTotal:") {
                // Format: "MemTotal:       16384000 kB"
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 2 {
                    if let Ok(kb) = parts[1].parse::<u64>() {
                        return Ok(kb / 1024); // Convert KB to MB
                    }
                }
            }
        }

        Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Could not parse MemTotal from /proc/meminfo"
        ))
    }

    /// Get total number of CPU cores
    pub fn cpu_count() -> io::Result<usize> {
        #[cfg(target_os = "linux")]
        {
            Ok(num_cpus())
        }

        #[cfg(not(target_os = "linux"))]
        {
            // TODO: Implement for other platforms
            Err(io::Error::new(
                io::ErrorKind::Unsupported,
                "CPU detection not implemented for this platform"
            ))
        }
    }
}

/// Linux-specific resource limit application
#[cfg(target_os = "linux")]
fn apply_memory_limit(bytes: u64) -> io::Result<()> {
    use nix::sys::resource::{Resource, setrlimit};

    // Set RLIMIT_AS (address space / virtual memory limit)
    // This is more effective than RLIMIT_DATA for modern applications
    setrlimit(Resource::RLIMIT_AS, bytes, bytes)
        .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("setrlimit failed: {}", e)))
}

/// Linux-specific CPU limit application
#[cfg(target_os = "linux")]
fn apply_cpu_limit(cpu_percent: u32) -> io::Result<()> {
    // CPU percentage limiting via cgroups is complex and requires root or systemd user units.
    // For now, we'll use RLIMIT_CPU as a softer limit (CPU time in seconds).
    // This is not a perfect solution but provides some protection.

    // TODO: Implement proper CPU throttling using cgroups v2
    // This would require:
    // 1. Creating a cgroup for the process
    // 2. Setting cpu.max to limit CPU bandwidth
    // 3. Moving the process into the cgroup

    // For now, we'll set a generous CPU time limit as a safety measure
    // This limits total CPU time, not percentage usage
    use nix::sys::resource::{Resource, setrlimit};

    // Set a very high CPU time limit (24 hours) as a safety measure
    // This prevents runaway processes but doesn't throttle CPU percentage
    let cpu_time_seconds: u64 = 24 * 60 * 60; // 24 hours

    setrlimit(Resource::RLIMIT_CPU, cpu_time_seconds, cpu_time_seconds)
        .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("setrlimit CPU failed: {}", e)))?;

    tracing::debug!(
        "Set RLIMIT_CPU to {} seconds (note: CPU percentage throttling requires cgroups)",
        cpu_time_seconds
    );

    Ok(())
}

/// Get the number of CPU cores (Linux)
#[cfg(target_os = "linux")]
fn num_cpus() -> usize {
    // Read from /proc/cpuinfo or use std::thread::available_parallelism
    std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_process_limits_default() {
        let limits = ProcessLimits::default();

        #[cfg(target_os = "linux")]
        {
            assert!(limits.enabled);
            assert!(limits.max_memory_mb.is_some());
            assert_eq!(limits.max_cpu_percent, Some(90));
        }

        #[cfg(not(target_os = "linux"))]
        {
            assert!(!limits.enabled);
        }
    }

    #[test]
    fn test_process_limits_unlimited() {
        let limits = ProcessLimits::unlimited();
        assert!(!limits.enabled);
        assert_eq!(limits.max_memory_mb, None);
        assert_eq!(limits.max_cpu_percent, None);
    }

    #[test]
    fn test_process_limits_serialization() {
        let limits = ProcessLimits {
            max_memory_mb: Some(1024),
            max_cpu_percent: Some(80),
            enabled: true,
        };

        let json = serde_json::to_string(&limits).unwrap();
        let deserialized: ProcessLimits = serde_json::from_str(&json).unwrap();

        assert_eq!(limits, deserialized);
    }

    #[test]
    #[cfg(target_os = "linux")]
    fn test_system_resources_memory() {
        let mem_mb = SystemResources::total_memory_mb();
        assert!(mem_mb.is_ok());

        if let Ok(mem) = mem_mb {
            assert!(mem > 0);
            println!("Total system memory: {} MB", mem);
        }
    }

    #[test]
    #[cfg(target_os = "linux")]
    fn test_system_resources_cpu() {
        let cpu_count = SystemResources::cpu_count();
        assert!(cpu_count.is_ok());

        if let Ok(count) = cpu_count {
            assert!(count > 0);
            println!("Total CPU cores: {}", count);
        }
    }

    #[test]
    fn test_process_limits_apply_to_command_disabled() {
        let limits = ProcessLimits::unlimited();
        let mut cmd = tokio::process::Command::new("echo");

        // Should succeed without applying any limits
        let result = limits.apply_to_command(&mut cmd);
        assert!(result.is_ok());
    }

    #[test]
    #[cfg(target_os = "linux")]
    fn test_process_limits_apply_to_command_with_limits() {
        let limits = ProcessLimits {
            max_memory_mb: Some(512),
            max_cpu_percent: Some(50),
            enabled: true,
        };

        let mut cmd = tokio::process::Command::new("echo");

        // Should succeed in setting up the pre_exec hook
        let result = limits.apply_to_command(&mut cmd);
        assert!(result.is_ok());
    }

    #[test]
    #[cfg(target_os = "linux")]
    fn test_process_limits_default_memory_calculation() {
        let default_memory = ProcessLimits::default_memory_limit_mb();

        // Should be able to determine system memory
        assert!(default_memory.is_some());

        if let Some(mem_mb) = default_memory {
            // Should be reasonable (at least 1MB, less than 1TB)
            assert!(mem_mb > 0);
            assert!(mem_mb < 1_000_000);

            // Should be approximately 50% of system memory
            let total_memory = SystemResources::total_memory_mb().unwrap();
            let expected = total_memory / 2;

            // Allow for some rounding differences
            assert!((mem_mb as i64 - expected as i64).abs() < 10);
        }
    }

    #[test]
    fn test_process_limits_json_with_null_memory() {
        // Test that null memory value deserializes correctly and uses default
        let json = r#"{
            "max_memory_mb": null,
            "max_cpu_percent": 90,
            "enabled": true
        }"#;

        let limits: ProcessLimits = serde_json::from_str(json).unwrap();
        assert_eq!(limits.max_memory_mb, None);
        assert_eq!(limits.max_cpu_percent, Some(90));
        assert!(limits.enabled);
    }

    #[tokio::test]
    #[cfg(target_os = "linux")]
    async fn test_spawn_process_with_limits() {
        // Test that we can successfully spawn a process with limits applied
        let limits = ProcessLimits {
            max_memory_mb: Some(100),
            max_cpu_percent: Some(50),
            enabled: true,
        };

        let mut cmd = tokio::process::Command::new("echo");
        cmd.arg("test");

        // Apply limits
        limits.apply_to_command(&mut cmd).unwrap();

        // Spawn and wait for the process
        let output = cmd.output().await;

        // Process should succeed despite limits (echo is very lightweight)
        assert!(output.is_ok());
        let output = output.unwrap();
        assert!(output.status.success());
        assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "test");
    }
}
