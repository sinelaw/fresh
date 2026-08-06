//! Raising privilege for the one step that needs it.
//!
//! One mechanism: `sudo`. Probing for `doas` when it happens to be installed
//! would mean the same provenance class updated itself through a different
//! authorization mechanism depending on the machine — different config file,
//! different audit trail — which is precisely what the receipt exists to rule
//! out. Two identical receipts must update the same way, so a machine without
//! `sudo` gets a plain "command not found" naming exactly what is missing,
//! rather than something that quietly worked differently.
//!
//! Running as root is **not** an exception to that rule, because it is not
//! another mechanism. `sudo dpkg -i` and `dpkg -i` as root are the same
//! privileged operation with and without a no-op escalation step; the command
//! that actually runs is identical either way. Treating a missing `sudo` as
//! fatal when we are already root failed in exactly the place it is most
//! common — a minimal container running as root, which is also how the
//! packaging tests run.

/// The argv to actually execute, with the escalation step prepended when the
/// install needs root and we do not already have it.
pub fn elevated(cmd: &[String], needs_privilege: bool) -> Vec<String> {
    if !needs_privilege || already_root() {
        return cmd.to_vec();
    }
    let mut argv = Vec::with_capacity(cmd.len() + 1);
    argv.push("sudo".to_string());
    argv.extend_from_slice(cmd);
    argv
}

/// Whether this process already has the privilege the install needs.
#[cfg(unix)]
pub fn already_root() -> bool {
    // Safety: `geteuid` takes no arguments, touches no memory, and cannot
    // fail — POSIX specifies it as always succeeding.
    unsafe { libc::geteuid() == 0 }
}

/// Windows has no equivalent that helps here. Elevation cannot be obtained in
/// place at all — `runas` starts a *new* console — so the caller prints the
/// command rather than running it, and this always answers "not already".
#[cfg(not(unix))]
pub fn already_root() -> bool {
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    fn argv(parts: &[&str]) -> Vec<String> {
        parts.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn an_unprivileged_command_is_never_wrapped() {
        let cmd = argv(&["brew", "upgrade", "--formula", "fresh"]);
        assert_eq!(elevated(&cmd, false), cmd);
    }

    #[test]
    fn the_escalation_step_is_a_prefix_not_a_different_command() {
        let cmd = argv(&["apt-get", "install", "-y", "/tmp/x.deb"]);
        let raised = elevated(&cmd, true);
        // Whatever we decided about privilege, the command itself is the same
        // one — that is what keeps "one provenance, one mechanism" true when
        // the same receipt is updated as root and as a user.
        assert_eq!(&raised[raised.len() - cmd.len()..], cmd.as_slice());
        if already_root() {
            assert_eq!(raised, cmd, "root does not need an escalation step");
        } else {
            assert_eq!(raised.first().map(String::as_str), Some("sudo"));
        }
    }

    /// The case that made a root container fail with `failed to run sudo`.
    #[cfg(unix)]
    #[test]
    fn root_runs_the_command_directly() {
        if !already_root() {
            // Not root here; the branch above covers the other side.
            return;
        }
        let cmd = argv(&["dnf", "install", "-y", "/tmp/x.rpm"]);
        assert_eq!(elevated(&cmd, true), cmd);
    }
}
