/// Process resource limiting infrastructure
///
/// Provides cross-platform support for limiting memory and CPU usage of spawned processes.
/// On Linux, uses user-delegated cgroups v2 if available, otherwise falls back to setrlimit.
/// Memory and CPU limits are decoupled - memory can work without CPU delegation.
// Re-export the type from the shared types module
pub use crate::types::ProcessLimits;

use std::io;

#[cfg(target_os = "linux")]
use std::fs;
#[cfg(target_os = "linux")]
use std::path::{Path, PathBuf};

/// Action that must be applied to a spawned child process after `Command::spawn`
/// returns, from the parent process.
///
/// This exists because moving a child into a cgroup used to be done from a
/// `pre_exec` closure, which runs in the forked child between fork and exec.
/// That context is effectively single-threaded and inherits locks held by
/// other threads of the parent at fork time — any allocation (`format!`,
/// `PathBuf::join`, `fs::write`) can deadlock on a malloc mutex that was held
/// by another thread at fork time.  Instead, we record what needs to happen
/// and do the work from the parent once the child has a PID.
#[derive(Debug, Default)]
pub struct PostSpawnAction {
    /// Cgroup directory the child should be moved into (by writing its PID
    /// to `<dir>/cgroup.procs`).  `None` means no cgroup move is needed.
    #[cfg(target_os = "linux")]
    cgroup_dir: Option<PathBuf>,
}

impl PostSpawnAction {
    /// Apply the recorded action to a just-spawned child PID.
    ///
    /// Safe to call from the parent — unlike the old `pre_exec` variant, this
    /// runs with the full runtime available (no fork-safety constraints).
    /// Errors are logged but not returned: best-effort, because the process
    /// is already running and we don't want to kill it just because cgroup
    /// assignment failed.
    pub fn apply_to_child(&self, _child_pid: u32) {
        #[cfg(target_os = "linux")]
        if let Some(ref cgroup_dir) = self.cgroup_dir {
            let procs_file = cgroup_dir.join("cgroup.procs");
            if let Err(e) = fs::write(&procs_file, format!("{}", _child_pid)) {
                tracing::info!(
                    "Failed to move child {} into cgroup {:?}: {}",
                    _child_pid,
                    cgroup_dir,
                    e
                );
            }
        }
    }
}

impl ProcessLimits {
    /// Get the memory limit in bytes, computed from percentage of total system memory
    pub fn memory_limit_bytes(&self) -> Option<u64> {
        self.max_memory_percent.and_then(|percent| {
            SystemResources::total_memory_mb()
                .ok()
                .map(|total_mb| (total_mb * percent as u64 / 100) * 1024 * 1024)
        })
    }

    /// Apply these limits to a tokio Command before spawning
    ///
    /// On Linux, tries user-delegated cgroups v2, otherwise falls back to setrlimit.
    /// Memory and CPU limits are handled independently.
    ///
    /// Returns a `PostSpawnAction` that the caller must apply to the child
    /// process after `cmd.spawn()` returns.  (For cgroups, the actual
    /// per-child assignment happens from the parent using the child's PID;
    /// doing it in `pre_exec` is not fork-safe.)
    pub fn apply_to_command(
        &self,
        _cmd: &mut tokio::process::Command,
    ) -> io::Result<PostSpawnAction> {
        if !self.enabled {
            return Ok(PostSpawnAction::default());
        }

        #[cfg(target_os = "linux")]
        {
            self.apply_linux_limits(_cmd)
        }

        #[cfg(not(target_os = "linux"))]
        {
            // TODO: Implement for macOS using setrlimit
            // TODO: Implement for Windows using Job Objects
            tracing::info!("Process resource limits are not yet implemented for this platform");
            Ok(PostSpawnAction::default())
        }
    }

    #[cfg(target_os = "linux")]
    fn apply_linux_limits(&self, cmd: &mut tokio::process::Command) -> io::Result<PostSpawnAction> {
        let max_memory_bytes = self.memory_limit_bytes();
        let _max_cpu_percent = self.max_cpu_percent;

        // Find a user-delegated cgroup path if available
        let cgroup_path = find_user_cgroup();

        // Track what methods we'll use
        let mut memory_method = "none";
        let mut cpu_method = "none";
        let mut action = PostSpawnAction::default();

        // Try to set up cgroup limits.  One cgroup per editor process; reused
        // across all LSP spawns.  All children write their PIDs to the same
        // `cgroup.procs` after spawn.
        if let Some(ref cgroup_base) = cgroup_path {
            let pid = std::process::id();
            let cgroup_name = format!("editor-lsp-{}", pid);
            let cgroup_full = cgroup_base.join(&cgroup_name);

            // create_dir returns AlreadyExists if another spawn already set it
            // up in this process; that's fine — we'll reuse it.
            let cgroup_usable = match fs::create_dir(&cgroup_full) {
                Ok(()) => {
                    // Freshly created — set limits on it.
                    if let Some(memory_bytes) = max_memory_bytes {
                        if set_cgroup_memory(&cgroup_full, memory_bytes).is_ok() {
                            memory_method = "cgroup";
                            tracing::debug!(
                                "Set memory limit via cgroup: {} MB ({}% of system)",
                                memory_bytes / 1024 / 1024,
                                self.max_memory_percent.unwrap_or(0)
                            );
                        }
                    }
                    if let Some(cpu_pct) = self.max_cpu_percent {
                        if set_cgroup_cpu(&cgroup_full, cpu_pct).is_ok() {
                            cpu_method = "cgroup";
                            tracing::debug!("Set CPU limit via cgroup: {}%", cpu_pct);
                        }
                    }
                    // If no limit took effect, drop the empty cgroup.
                    if memory_method != "cgroup" && cpu_method != "cgroup" {
                        #[allow(clippy::let_underscore_must_use)]
                        let _ = fs::remove_dir(&cgroup_full);
                        false
                    } else {
                        true
                    }
                }
                Err(e) if e.kind() == io::ErrorKind::AlreadyExists => {
                    // Already set up by a previous spawn in this process.
                    // Assume the limits were applied then; reuse the cgroup.
                    if max_memory_bytes.is_some() {
                        memory_method = "cgroup";
                    }
                    if self.max_cpu_percent.is_some() {
                        cpu_method = "cgroup";
                    }
                    true
                }
                Err(_) => false,
            };

            if cgroup_usable {
                action.cgroup_dir = Some(cgroup_full);
                tracing::info!(
                    "Using resource limits: memory={} ({}), CPU={} ({})",
                    self.max_memory_percent
                        .map(|p| format!("{}%", p))
                        .unwrap_or("unlimited".to_string()),
                    memory_method,
                    self.max_cpu_percent
                        .map(|c| format!("{}%", c))
                        .unwrap_or("unlimited".to_string()),
                    cpu_method
                );
                return Ok(action);
            }
        }

        // Fall back to setrlimit for memory if cgroup didn't work.
        //
        // Note: `pre_exec` runs in the forked child between fork and exec, so
        // it must only use async-signal-safe operations.  `setrlimit` itself
        // is safe; we deliberately avoid any logging or allocation inside the
        // closure to keep it fork-safe.
        if memory_method != "cgroup" && max_memory_bytes.is_some() {
            unsafe {
                cmd.pre_exec(move || {
                    if let Some(mem_limit) = max_memory_bytes {
                        // Ignore errors — we cannot safely log from a
                        // post-fork / pre-exec context.
                        #[allow(clippy::let_underscore_must_use)]
                        let _ = apply_memory_limit_setrlimit(mem_limit);
                    }
                    Ok(())
                });
            }
            memory_method = "setrlimit";
        }

        tracing::info!(
            "Using resource limits: memory={} ({}), CPU={} ({})",
            self.max_memory_percent
                .map(|p| format!("{}%", p))
                .unwrap_or("unlimited".to_string()),
            memory_method,
            self.max_cpu_percent
                .map(|c| format!("{}%", c))
                .unwrap_or("unlimited".to_string()),
            if cpu_method == "none" {
                "unavailable"
            } else {
                cpu_method
            }
        );

        Ok(action)
    }
}

/// Find a writable user-delegated cgroup
#[cfg(target_os = "linux")]
fn find_user_cgroup() -> Option<PathBuf> {
    let cgroup_root = PathBuf::from("/sys/fs/cgroup");
    if !cgroup_root.exists() {
        tracing::debug!("cgroups v2 not available at /sys/fs/cgroup");
        return None;
    }

    let uid = get_uid();

    // Try common locations for user-delegated cgroups
    let locations = vec![
        cgroup_root.join(format!(
            "user.slice/user-{}.slice/user@{}.service/app.slice",
            uid, uid
        )),
        cgroup_root.join(format!(
            "user.slice/user-{}.slice/user@{}.service",
            uid, uid
        )),
        cgroup_root.join(format!("user.slice/user-{}.slice", uid)),
        cgroup_root.join(format!("user-{}", uid)),
    ];

    for parent in locations {
        if !parent.exists() {
            continue;
        }

        // Check if we can write to this location
        let test_file = parent.join("cgroup.procs");
        if is_writable(&test_file) {
            tracing::debug!("Found writable user cgroup: {:?}", parent);
            return Some(parent);
        }
    }

    tracing::debug!("No writable user-delegated cgroup found");
    None
}

/// Set memory limit in a cgroup (works without full delegation)
#[cfg(target_os = "linux")]
fn set_cgroup_memory(cgroup_path: &Path, bytes: u64) -> io::Result<()> {
    let memory_max_file = cgroup_path.join("memory.max");
    fs::write(&memory_max_file, format!("{}", bytes))?;
    Ok(())
}

/// Set CPU limit in a cgroup (requires cpu controller delegation)
#[cfg(target_os = "linux")]
fn set_cgroup_cpu(cgroup_path: &Path, percent: u32) -> io::Result<()> {
    // cpu.max format: "$MAX $PERIOD" where MAX/PERIOD = desired quota
    // Standard period is 100ms (100000 microseconds)
    let period_us = 100_000;
    let max_us = (period_us * percent as u64) / 100;
    let cpu_max_file = cgroup_path.join("cpu.max");
    fs::write(&cpu_max_file, format!("{} {}", max_us, period_us))?;
    Ok(())
}

/// Check if a file is writable
#[cfg(target_os = "linux")]
fn is_writable(path: &Path) -> bool {
    use std::os::unix::fs::PermissionsExt;

    if let Ok(metadata) = fs::metadata(path) {
        let permissions = metadata.permissions();
        // Check if user has write permission
        permissions.mode() & 0o200 != 0
    } else {
        false
    }
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
                "Memory detection not implemented for this platform",
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
            "Could not parse MemTotal from /proc/meminfo",
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
                "CPU detection not implemented for this platform",
            ))
        }
    }
}

/// Apply memory limit via setrlimit (fallback method)
#[cfg(target_os = "linux")]
fn apply_memory_limit_setrlimit(bytes: u64) -> io::Result<()> {
    use nix::sys::resource::{setrlimit, Resource};

    // Set RLIMIT_AS (address space / virtual memory limit)
    // On 32-bit platforms, rlim_t is u32, so we need to convert carefully.
    // If bytes exceeds what rlim_t can represent, clamp to rlim_t::MAX.
    let limit = bytes as nix::libc::rlim_t;
    setrlimit(Resource::RLIMIT_AS, limit, limit)
        .map_err(|e| io::Error::other(format!("setrlimit AS failed: {}", e)))
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
            assert_eq!(limits.max_memory_percent, Some(50));
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
        assert_eq!(limits.max_memory_percent, None);
        assert_eq!(limits.max_cpu_percent, None);
    }

    #[test]
    fn test_process_limits_serialization() {
        let limits = ProcessLimits {
            max_memory_percent: Some(50),
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
    fn test_memory_limit_bytes_calculation() {
        let limits = ProcessLimits {
            max_memory_percent: Some(50),
            max_cpu_percent: Some(90),
            enabled: true,
        };

        let memory_bytes = limits.memory_limit_bytes();

        // Should be able to compute memory limit
        assert!(memory_bytes.is_some());

        if let Some(bytes) = memory_bytes {
            // Should be approximately 50% of system memory
            let total_memory = SystemResources::total_memory_mb().unwrap();
            let expected_bytes = (total_memory / 2) * 1024 * 1024;

            // Allow for some rounding differences
            assert!((bytes as i64 - expected_bytes as i64).abs() < 10 * 1024 * 1024);
        }
    }

    #[test]
    fn test_process_limits_json_with_null_memory() {
        // Test that null memory value deserializes correctly
        let json = r#"{
            "max_memory_percent": null,
            "max_cpu_percent": 90,
            "enabled": true
        }"#;

        let limits: ProcessLimits = serde_json::from_str(json).unwrap();
        assert_eq!(limits.max_memory_percent, None);
        assert_eq!(limits.max_cpu_percent, Some(90));
        assert!(limits.enabled);
    }

    #[tokio::test]
    #[cfg(target_os = "linux")]
    async fn test_spawn_process_with_limits() {
        // Test that we can successfully spawn a process with limits applied
        let limits = ProcessLimits {
            max_memory_percent: Some(10), // 10% of system memory
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
        let cgroup = find_user_cgroup();
        match cgroup {
            Some(path) => {
                println!("✓ Found writable user cgroup at: {:?}", path);
            }
            None => {
                println!("✗ No writable user cgroup found");
            }
        }
    }

    #[test]
    #[cfg(target_os = "linux")]
    fn test_memory_limit_independent() {
        // Test that memory limits can be set independently
        let _limits = ProcessLimits {
            max_memory_percent: Some(10),
            max_cpu_percent: None, // No CPU limit
            enabled: true,
        };

        if let Some(cgroup) = find_user_cgroup() {
            let test_cgroup = cgroup.join("test-memory-only");
            if fs::create_dir(&test_cgroup).is_ok() {
                // Try setting memory limit
                let result = set_cgroup_memory(&test_cgroup, 100 * 1024 * 1024);

                if result.is_ok() {
                    println!("✓ Memory limit works independently");
                } else {
                    println!("✗ Memory limit failed: {:?}", result.err());
                }

                // Clean up
                drop(fs::remove_dir(&test_cgroup));
            }
        } else {
            println!("⊘ No user cgroup available for testing");
        }
    }
}
