//! Agent-submitted script evaluation.
//!
//! Not input handling at all — this is the editor half of the server's
//! script channel (`server::command_access`), housed in its own module so
//! the input pipeline split (`key_router` / `action_dispatch`) doesn't have
//! to carry it.

use super::*;

impl Editor {
    /// Evaluate an agent-submitted script (already wrapped by
    /// `server::command_access`) as an ephemeral plugin.
    ///
    /// This is the whole of the script channel's runtime cost: the source goes
    /// through the *existing* load-from-source path — the one `init.ts` and
    /// "Load Plugin from Buffer" use — so it gets TypeScript transpilation, its
    /// own context, and the full `editor` API without a line of new runtime
    /// code. The wrapper answers the caller through `editor.completeCommand`,
    /// which is an ordinary plugin API method, so the result travels the same
    /// route a plugin command's return value already took.
    ///
    /// `Ok(())` means the script was *submitted*. Success is reported later by
    /// the script itself; only a failure to compile or to reach the runtime is
    /// settled here, since in that case nothing will ever call
    /// `completeCommand` and the caller would wait for an answer that cannot
    /// come.
    pub fn eval_agent_script(
        &mut self,
        wrapped: &str,
        request_id: u64,
        as_name: Option<&str>,
    ) -> Result<(), String> {
        #[cfg(feature = "plugins")]
        {
            // A name per request by default: two scripts in flight must not
            // share a context, or the second would see (and could clobber) the
            // first's globals.
            //
            // `--as` overrides that on purpose. A script is never unloaded on
            // its own — unloading runs the cleanup path, which tears down the
            // terminals and workspaces a script is usually run to create — so
            // a persistent watcher submitted twice would otherwise be running
            // twice. Unloading the previous copy *of that name* first is what
            // makes install idempotent, and it is safe precisely because the
            // caller named it: it is replacing its own earlier submission.
            let name = match as_name {
                Some(n) => {
                    let _ = self.plugin_manager.read().unwrap().unload_plugin(n);
                    n.to_string()
                }
                None => format!("agent-script-{}", request_id),
            };
            let rx = self
                .plugin_manager
                .read()
                .unwrap()
                .load_plugin_from_source_request(wrapped, &name, true)
                .ok_or_else(|| "plugin runtime unavailable".to_string())?;
            // Wait for the load off the editor thread: the script's own host
            // calls only complete on later editor ticks, which this thread must
            // stay free to service.
            std::thread::Builder::new()
                .name("agent-script-load".to_string())
                .spawn(move || {
                    let failure = match rx.recv() {
                        // The script is running; it answers for itself.
                        Ok(Ok(())) => return,
                        Ok(Err(e)) => format!("{e}"),
                        Err(e) => format!("plugin thread closed: {e}"),
                    };
                    crate::server::command_access::complete(request_id, false, None, Some(failure));
                })
                .map_err(|e| format!("could not start script loader: {e}"))?;
            Ok(())
        }
        #[cfg(not(feature = "plugins"))]
        {
            let _ = (wrapped, request_id);
            Err("scripts not available (compiled without plugin support)".to_string())
        }
    }
}
