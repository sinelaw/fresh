//! A shared fixture for the tests that check an activated environment reaches
//! children: `--inherit-env` capture mode pinned on, a venv with its activate
//! script, and a mock shell installed as `$SHELL` until dropped.
//!
//! The activate script sets VIRTUAL_ENV, puts the venv's bin first on PATH,
//! and removes FROM_PARENT_ENV, a variable the fixture sets in this process:
//! the child must not inherit it. FROM_PARENT_ENV is the regression guard
//! for the capture shell replacing fresh's inherited env with its own
//! reconstruction: an unset can only propagate if the delta was captured
//! against the environment the child actually inherits. The mock shell
//! exports a canary of its own, FROM_CAPTURE_SHELL, when it starts up: it
//! diffs out of the delta (baseline and activated dumps both contain it),
//! so if it reaches a child the delta was captured against the wrong
//! baseline.

use super::EnvProvider;
use std::sync::Arc;

pub(crate) struct ActivatedVenv {
    dir: tempfile::TempDir,
    pub(crate) provider: Arc<EnvProvider>,
    /// The values of the variables the fixture overrides, restored on drop.
    real_shell: Option<String>,
    real_parent_env: Option<String>,
    real_inherit_env: bool,
}

impl ActivatedVenv {
    pub(crate) fn new() -> Self {
        use std::os::unix::fs::PermissionsExt;

        let dir = tempfile::tempdir().unwrap();
        let venv = dir.path().join(".venv");
        std::fs::create_dir_all(venv.join("bin")).unwrap();
        std::fs::write(
            venv.join("bin/activate"),
            format!(
                "VIRTUAL_ENV='{}'\nexport VIRTUAL_ENV\nPATH=\"$VIRTUAL_ENV/bin:$PATH\"\nexport PATH\nunset FROM_PARENT_ENV\n",
                venv.display()
            ),
        )
        .unwrap();

        // The provider runs the activate script inside this shell
        // ("$SHELL -c <script>" when inheriting, "$SHELL -lc <script>" when
        // rebuilding from rc files) to find the venv's changes. It behaves
        // like a plain shell: eat the flags with `shift`, then run the
        // script text it was handed, after exporting a canary.
        let mock = dir.path().join("mock-shell");
        std::fs::write(
            &mock,
            concat!(
                "#!/bin/sh\n",
                "export FROM_CAPTURE_SHELL=1\n",
                "shift\n",
                "exec /bin/sh -c \"$1\"\n",
            ),
        )
        .unwrap();
        std::fs::set_permissions(&mock, std::fs::Permissions::from_mode(0o755)).unwrap();

        let real_shell = std::env::var("SHELL").ok();
        std::env::set_var("SHELL", &mock);
        let real_parent_env = std::env::var("FROM_PARENT_ENV").ok();
        std::env::set_var("FROM_PARENT_ENV", "fixture");
        let real_inherit_env = super::inherit_env_is_set();
        super::set_inherit_env(true);

        let provider = Arc::new(EnvProvider::inactive());
        provider.set(
            ". .venv/bin/activate".into(),
            Some(dir.path().to_path_buf()),
        );

        Self {
            dir,
            provider,
            real_shell,
            real_parent_env,
            real_inherit_env,
        }
    }

    fn venv(&self) -> std::path::PathBuf {
        self.dir.path().join(".venv")
    }

    /// The child's env split into a map, from `env` output.
    pub(crate) fn parse_env(out: &str) -> std::collections::HashMap<String, String> {
        out.lines()
            .filter_map(|line| line.split_once('='))
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect()
    }

    /// The checks every env path must pass.
    pub(crate) fn assert_env(&self, site: &str, env: &std::collections::HashMap<String, String>) {
        let venv = self.venv();
        assert!(
            env.get("VIRTUAL_ENV").map(String::as_str) == venv.to_str(),
            "{site}: VIRTUAL_ENV wrong or missing: {env:?}"
        );
        let path = env.get("PATH").map(String::as_str).unwrap_or_default();
        assert!(
            path.starts_with(&format!("{}:", venv.join("bin").display())),
            "{site}: venv bin must lead PATH, got: {path}"
        );
        assert!(
            !env.contains_key("FROM_PARENT_ENV"),
            "{site}: the var the activate script removed is present: {env:?}"
        );
        assert!(
            !env.contains_key("FROM_CAPTURE_SHELL"),
            "{site}: the capture shell's env reached the child env: {env:?}"
        );
    }
}

impl Drop for ActivatedVenv {
    fn drop(&mut self) {
        match self.real_shell.take() {
            Some(s) => std::env::set_var("SHELL", s),
            None => std::env::remove_var("SHELL"),
        }
        match self.real_parent_env.take() {
            Some(s) => std::env::set_var("FROM_PARENT_ENV", s),
            None => std::env::remove_var("FROM_PARENT_ENV"),
        }
        super::set_inherit_env(self.real_inherit_env);
    }
}
