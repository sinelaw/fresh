# Process Resource Limits

## Overview

This editor automatically applies resource limits to LSP servers (like `rust-analyzer`, `typescript-language-server`, etc.) to prevent them from consuming excessive system resources and making your development environment unresponsive.

## Why This Matters

Language servers can sometimes:
- Consume 100% CPU while indexing large projects, making your system sluggish
- Leak memory or use excessive amounts, causing system slowdown or OOM kills
- Monopolize resources when multiple language servers run concurrently

Resource limits prevent these issues by automatically constraining LSP servers to reasonable CPU and memory usage.

## What Works Out of the Box

**Memory limiting** works immediately on all modern Linux systems with cgroups v2:

```
Using resource limits: memory=6815 MB (cgroup), CPU=90% (unavailable)
```

The editor will limit each LSP server to **50% of system memory** by default. This happens automatically—no configuration required.

## CPU Throttling (Optional Setup)

**CPU throttling** (limiting to a percentage like 90%) requires enabling cgroup delegation. This is optional but recommended if your LSP servers frequently use 100% CPU.

### When to Enable

Enable CPU delegation if:
- Your LSP server frequently uses 100% CPU and makes the system sluggish
- You want to prevent any single process from monopolizing CPU resources
- You're running multiple resource-intensive language servers simultaneously
- You work on low-powered systems (laptops, older hardware)

### Setup Instructions

#### On systemd systems (Ubuntu 18.04+, Debian 10+, Fedora 31+, Arch Linux)

```bash
# Create systemd drop-in configuration
sudo mkdir -p /etc/systemd/system/user@.service.d/
sudo tee /etc/systemd/system/user@.service.d/delegate.conf <<EOF
[Service]
Delegate=cpu cpuset io memory pids
EOF

# Apply changes
sudo systemctl daemon-reload

# Log out and back in for changes to take effect

# Verify delegation is working:
cat /sys/fs/cgroup/user.slice/user-$(id -u).slice/user@$(id -u).service/cgroup.controllers
# Should show: cpu memory io pids
```

#### On non-systemd systems with cgroups v2

```bash
# Enable cpu controller delegation for your user
echo "+cpu" | sudo tee /sys/fs/cgroup/user.slice/user-$(id -u).slice/cgroup.subtree_control

# Verify:
cat /sys/fs/cgroup/user.slice/user-$(id -u).slice/cgroup.controllers
# Should include: cpu
```

#### Verify It's Working

After setup, check the editor logs (`/tmp/editor.log`). You should see:

```
Using resource limits: memory=6815 MB (cgroup), CPU=90% (cgroup)
```

If CPU still shows as `unavailable`, delegation may not have taken effect. Try:
1. Log out and back in (not just closing the terminal)
2. Check that your user session is running under systemd: `systemctl --user status`
3. Verify controllers are listed in the cgroup.controllers file

## Configuration

Limits are configurable per LSP server in `~/.config/editor/config.json`:

```json
{
  "lsp": {
    "rust": {
      "command": "rust-analyzer",
      "enabled": true,
      "process_limits": {
        "max_memory_mb": null,     // null = 50% of system memory
        "max_cpu_percent": 90,     // 90% of total CPU (100% = 1 core)
        "enabled": true            // true on Linux, false elsewhere
      }
    },
    "typescript": {
      "command": "typescript-language-server",
      "args": ["--stdio"],
      "enabled": true,
      "process_limits": {
        "max_memory_mb": 2048,     // Explicit 2GB limit
        "max_cpu_percent": 50,     // Limit to 50% of one core
        "enabled": true
      }
    }
  }
}
```

### Configuration Options

- **max_memory_mb**: Memory limit in megabytes
  - `null` (default): 50% of total system memory
  - Number: Explicit limit in MB (e.g., `2048` for 2GB)
  - Note: On systems with >16GB RAM, you may want to set explicit limits

- **max_cpu_percent**: CPU usage limit as percentage
  - `90` (default): 90% of total CPU
  - For multi-core systems: `100` = 1 core, `200` = 2 cores, etc.
  - Example: On a 4-core system, `200` means up to 2 cores

- **enabled**: Whether to apply limits
  - `true` (default on Linux): Apply limits
  - `false`: No limits (useful for debugging or benchmarking)

## How It Works

The editor uses **cgroups v2** for resource limiting when available, with automatic fallbacks for compatibility.

### Memory Limiting

**Works without delegation** (available immediately on modern Linux):

1. **cgroups v2** (primary method):
   - Uses `memory.max` controller
   - Hard limit—process cannot exceed this
   - Enforced by the kernel
   - Works in user-delegated cgroups without special permissions

2. **setrlimit** (fallback):
   - Uses `RLIMIT_AS` (address space limit)
   - Less precise than cgroups but widely compatible
   - Used when cgroups unavailable or not writable

### CPU Throttling

**Requires delegation** (see setup above):

1. **cgroups v2** (only method):
   - Uses `cpu.max` controller for percentage-based throttling
   - Smoothly limits CPU usage over time windows
   - Prevents monopolization without hard-stopping processes

2. **No fallback**:
   - `setrlimit(RLIMIT_CPU)` only limits total CPU time, not percentage
   - Cannot throttle with setrlimit alone
   - Shows as "unavailable" without delegation

### Status Messages

The editor logs show which methods are active:

| Message | Meaning |
|---------|---------|
| `memory=6815 MB (cgroup)` | Memory limit via cgroups v2 ✓ |
| `memory=6815 MB (setrlimit)` | Memory limit via setrlimit fallback |
| `memory=unlimited (none)` | No memory limiting available |
| `CPU=90% (cgroup)` | CPU throttling via cgroups v2 ✓ |
| `CPU=90% (unavailable)` | CPU throttling not available (needs delegation) |

Check `/tmp/editor.log` after starting the editor to see what's active.

## Platform Support

### Current Status

| Platform | Memory Limiting | CPU Throttling | Method |
|----------|----------------|----------------|--------|
| **Linux (cgroups v2)** | ✅ Works | ✅ With delegation | cgroups + setrlimit fallback |
| **Linux (cgroups v1)** | ⚠️ Fallback only | ❌ Not supported | setrlimit only |
| **macOS** | 🔄 TODO | 🔄 TODO | Will use setrlimit |
| **Windows** | 🔄 TODO | 🔄 TODO | Will use Job Objects |

### Checking Your System

**Check if you have cgroups v2:**
```bash
# Should show "cgroup2"
mount | grep cgroup

# Should exist and contain files
ls /sys/fs/cgroup/cgroup.controllers
```

**Check which controllers you can use:**
```bash
# Show available controllers for your user
cat /sys/fs/cgroup/user.slice/user-$(id -u).slice/user@$(id -u).service/cgroup.controllers

# If empty or missing 'cpu', you need to enable delegation (see Setup above)
```

## Troubleshooting

### Memory Limits Not Working

**Symptoms:** Logs show `memory=unlimited (none)`

**Solutions:**
1. Check cgroups v2 is mounted: `mount | grep cgroup2`
2. Verify user cgroups exist: `ls /sys/fs/cgroup/user.slice/`
3. Check permissions: `ls -la /sys/fs/cgroup/user.slice/user-$(id -u).slice/`

If cgroups unavailable, setrlimit fallback should still work (check logs).

### CPU Throttling Shows "unavailable"

**Symptoms:** Logs show `CPU=90% (unavailable)`

**This is normal** if you haven't enabled delegation. CPU throttling is optional.

**To enable:**
1. Follow setup instructions above
2. **Log out and back in** (critical step)
3. Verify delegation: `cat /sys/fs/cgroup/user.slice/user-$(id -u).slice/user@$(id -u).service/cgroup.controllers | grep cpu`
4. If still not working, check your init system supports user sessions: `systemctl --user status`

### LSP Server Killed by OOM

**Symptoms:** LSP server dies with "Out of memory" error

**Solutions:**
1. Increase memory limit for that LSP server in config
2. Check available system memory: `free -h`
3. Consider closing other applications or increasing system RAM
4. For rust-analyzer specifically, try smaller project or exclude large dependencies

### High CPU Usage Despite Limits

**Symptoms:** LSP server still uses 100% CPU

**Check:**
1. Verify CPU throttling is active in logs (should show "cgroup", not "unavailable")
2. Check `max_cpu_percent` in config—90% is still quite high
3. Lower the limit (e.g., `50%`) if needed
4. Verify process is in cgroup: `cat /proc/$(pgrep rust-analyzer)/cgroup`

## Technical Details

### Cgroup Hierarchy

The editor creates cgroups in this hierarchy:
```
/sys/fs/cgroup/
└── user.slice/
    └── user-1000.slice/
        └── user@1000.service/
            └── editor-lsp-12345/    ← Created per LSP server
                ├── cgroup.procs     ← Process moved here
                ├── memory.max       ← Memory limit written here
                └── cpu.max          ← CPU limit written here (if delegated)
```

### Why Memory Works Without Delegation

The `memory` controller is simpler than `cpu`—it just tracks allocations and kills processes that exceed limits. It doesn't affect scheduling or other processes.

Modern systemd typically makes `memory` available in user slices by default because it's considered safe for unprivileged users.

### Why CPU Requires Delegation

The `cpu` controller affects kernel scheduling decisions and can impact system-wide performance. For security, it requires explicit delegation by root to prevent unprivileged users from disrupting the system.

## References

| Resource | Description | Link |
| :--- | :--- | :--- |
| **Arch Wiki: cgroups** | Comprehensive guide to cgroups v2 and delegation | [wiki.archlinux.org](https://wiki.archlinux.org/title/Cgroups) |
| **Ubuntu Security Docs** | Explains cgroups and security benefits on Ubuntu | [Ubuntu cgroups](https://documentation.ubuntu.com/security/docs/security-features/privilege-restriction/cgroups/) |
| **systemd.resource-control** | Documentation for CPU and memory quota properties | [systemd.resource-control](https://www.freedesktop.org/software/systemd/man/systemd.resource-control.html) |
| **Kernel cgroups docs** | Authoritative documentation on cgroup v2 controllers | [kernel.org](https://www.kernel.org/doc/html/latest/admin-guide/cgroup-v2.html) |

## Examples

### Conservative Limits (Laptops, Shared Systems)

```json
{
  "lsp": {
    "rust": {
      "command": "rust-analyzer",
      "enabled": true,
      "process_limits": {
        "max_memory_mb": 1024,    // 1GB
        "max_cpu_percent": 50,    // 50% of one core
        "enabled": true
      }
    }
  }
}
```

### Aggressive Limits (High-Performance Workstations)

```json
{
  "lsp": {
    "rust": {
      "command": "rust-analyzer",
      "enabled": true,
      "process_limits": {
        "max_memory_mb": 8192,    // 8GB
        "max_cpu_percent": 300,   // Up to 3 cores
        "enabled": true
      }
    }
  }
}
```

### Disable Limits (Debugging, Benchmarking)

```json
{
  "lsp": {
    "rust": {
      "command": "rust-analyzer",
      "enabled": true,
      "process_limits": {
        "enabled": false          // No limits
      }
    }
  }
}
```
