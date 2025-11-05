/// Process resource limiting infrastructure
///
/// Provides cross-platform support for limiting memory and CPU usage of spawned processes.
/// On Linux, uses user-delegated cgroups v2 if available, otherwise falls back to setrlimit.

use serde::{Deserialize, Serialize};
use std::io;
use std::fs;
use std::path::PathBuf;

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
    /// On Linux, tries user-delegated cgroups v2, otherwise falls back to setrlimit.
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

        // Try to set up a user-delegated cgroup
        let cgroup_path = self.try_setup_user_cgroup();

        if let Some(ref cgroup) = cgroup_path {
            tracing::info!(
                "Using cgroups for resource limits (memory: {:?} MB, CPU: {:?}%, path: {:?})",
                self.max_memory_mb, self.max_cpu_percent, cgroup
            );
        } else {
            tracing::info!(
                "Using setrlimit fallback for resource limits (memory: {:?} MB, CPU throttling not available)",
                self.max_memory_mb
            );
        }

        unsafe {
            cmd.pre_exec(move || {
                // If we have a cgroup set up, move this process into it
                if let Some(ref cgroup) = cgroup_path {
                    if let Err(e) = move_to_cgroup(cgroup) {
                        tracing::warn!("Failed to move process to cgroup: {}, falling back to setrlimit", e);
                    } else {
                        // Successfully moved to cgroup, limits are already set
                        return Ok(());
                    }
                }

                // Fall back to setrlimit
                if let Some(mem_limit) = max_memory_bytes {
                    if let Err(e) = apply_memory_limit_setrlimit(mem_limit) {
                        tracing::warn!("Failed to apply memory limit via setrlimit: {}", e);
                    }
                }

                // Note: CPU percentage limiting not possible with setrlimit
                if max_cpu_percent.is_some() {
                    tracing::debug!("CPU throttling not available with setrlimit (only total CPU time limits work)");
                }

                Ok(())
            });
        }

        Ok(())
    }

    /// Try to set up a cgroup in user-delegated space
    /// Returns the cgroup path if successful, None otherwise
    #[cfg(target_os = "linux")]
    fn try_setup_user_cgroup(&self) -> Option<PathBuf> {
        let cgroup_root = PathBuf::from("/sys/fs/cgroup");
        if !cgroup_root.exists() {
            tracing::debug!("cgroups v2 not available at /sys/fs/cgroup");
            return None;
        }

        let uid = get_uid();
        let pid = std::process::id();

        // Try common locations for user-delegated cgroups
        // On systemd systems: /sys/fs/cgroup/user.slice/user-{uid}.slice/user@{uid}.service/
        // On some systems: /sys/fs/cgroup/user.slice/user-{uid}.slice/
        // Direct: /sys/fs/cgroup/user-{uid}/
        let locations = vec![
            cgroup_root.join(format!("user.slice/user-{}.slice/user@{}.service/app.slice", uid, uid)),
            cgroup_root.join(format!("user.slice/user-{}.slice/user@{}.service", uid, uid)),
            cgroup_root.join(format!("user.slice/user-{}.slice", uid)),
            cgroup_root.join(format!("user-{}", uid)),
        ];

        for parent in locations {
            if !parent.exists() {
                continue;
            }

            // Check if we can write to this location by checking cgroup.procs
            let test_file = parent.join("cgroup.procs");
            if !is_writable(&test_file) {
                tracing::debug!("Parent cgroup not writable: {:?}", parent);
                continue;
            }

            // Create a child cgroup for our process
            let cgroup_name = format!("editor-lsp-{}", pid);
            let cgroup_path = parent.join(&cgroup_name);

            if let Ok(()) = fs::create_dir(&cgroup_path) {
                // Try to set the limits
                if self.apply_cgroup_limits(&cgroup_path).is_ok() {
                    tracing::debug!("Created cgroup at {:?}", cgroup_path);
                    return Some(cgroup_path);
                } else {
                    // Clean up failed attempt
                    let _ = fs::remove_dir(&cgroup_path);
                    tracing::debug!("Failed to set limits on cgroup: {:?}", cgroup_path);
                }
            }
        }

        tracing::debug!("Could not find writable user-delegated cgroup, will use setrlimit");
        None
    }

    /// Apply resource limits to an existing cgroup
    #[cfg(target_os = "linux")]
    fn apply_cgroup_limits(&self, cgroup_path: &PathBuf) -> io::Result<()> {
        // Set memory limit
        if let Some(memory_mb) = self.max_memory_mb {
            let memory_bytes = memory_mb * 1024 * 1024;
            let memory_max_file = cgroup_path.join("memory.max");
            fs::write(&memory_max_file, format!("{}", memory_bytes))?;
            tracing::debug!("Set cgroup memory.max to {} bytes ({} MB)", memory_bytes, memory_mb);
        }

        // Set CPU limit
        if let Some(cpu_percent) = self.max_cpu_percent {
            // cpu.max format: "$MAX $PERIOD" where MAX/PERIOD = desired quota
            // Standard period is 100ms (100000 microseconds)
            let period_us = 100_000;
            let max_us = (period_us * cpu_percent as u64) / 100;
            let cpu_max_file = cgroup_path.join("cpu.max");
            fs::write(&cpu_max_file, format!("{} {}", max_us, period_us))?;
            tracing::debug!("Set cgroup cpu.max to {} {} ({}%)", max_us, period_us, cpu_percent);
        }

        Ok(())
    }
}

/// Check if a file is writable
#[cfg(target_os = "linux")]
fn is_writable(path: &PathBuf) -> bool {
    use std::os::unix::fs::PermissionsExt;

    if let Ok(metadata) = fs::metadata(path) {
        let permissions = metadata.permissions();
        // Check if user has write permission
        permissions.mode() & 0o200 != 0
    } else {
        false
    }
}

/// Move the current process into a cgroup
#[cfg(target_os = "linux")]
fn move_to_cgroup(cgroup_path: &PathBuf) -> io::Result<()> {
    let procs_file = cgroup_path.join("cgroup.procs");
    let pid = std::process::id();
    fs::write(&procs_file, format!("{}", pid))?;
    Ok(())
}

/// Get the current user's UID
#[cfg(target_os = "linux")]
fn get_uid() -> u32 {
    unsafe { libc::getuid() }
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

/// Apply memory limit via setrlimit (fallback method)
#[cfg(target_os = "linux")]
fn apply_memory_limit_setrlimit(bytes: u64) -> io::Result<()> {
    use nix::sys::resource::{Resource, setrlimit};

    // Set RLIMIT_AS (address space / virtual memory limit)
    setrlimit(Resource::RLIMIT_AS, bytes, bytes)
        .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("setrlimit AS failed: {}", e)))
}

/// Get the number of CPU cores (Linux)
#[cfg(target_os = "linux")]
fn num_cpus() -> usize {
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

        // Apply limits (will try cgroups or fall back to setrlimit)
        limits.apply_to_command(&mut cmd).unwrap();

        // Spawn and wait for the process
        let output = cmd.output().await;

        // Process should succeed despite limits (echo is very lightweight)
        assert!(output.is_ok());
        let output = output.unwrap();
        assert!(output.status.success());
        assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "test");
    }

    #[test]
    #[cfg(target_os = "linux")]
    fn test_user_cgroup_detection() {
        // Check if we can find user-delegated cgroups
        let limits = ProcessLimits {
            max_memory_mb: Some(100),
            max_cpu_percent: Some(50),
            enabled: true,
        };

        let cgroup = limits.try_setup_user_cgroup();
        match cgroup {
            Some(path) => {
                println!("✓ Found writable user cgroup at: {:?}", path);
                // Clean up
                let _ = std::fs::remove_dir(&path);
            }
            None => {
                println!("✗ No writable user cgroup found, will use setrlimit fallback");
            }
        }
    }
}
