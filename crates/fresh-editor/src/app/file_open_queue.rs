//! Pending-file-open queue and --wait tracking on `Editor`.
//!
//! CLI file arguments are queued and processed after the TUI starts so
//! they go through the same code path as interactive opens (with proper
//! encoding-prompt handling). Wait tracking lets the CLI block until
//! a popup-based wait is dismissed.

use std::path::PathBuf;

use rust_i18n::t;

use super::Editor;

impl Editor {
    /// Queue a file to be opened after the TUI starts.
    ///
    /// This is used for CLI file arguments to ensure they go through the same
    /// code path as interactive file opens, providing consistent error handling
    /// (e.g., encoding confirmation prompts are shown in the UI instead of crashing).
    /// Schedule hot exit recovery to run after the next batch of pending file opens.
    pub fn schedule_hot_exit_recovery(&mut self) {
        if self.config.editor.hot_exit {
            self.pending_hot_exit_recovery = true;
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn queue_file_open(
        &mut self,
        path: PathBuf,
        line: Option<usize>,
        column: Option<usize>,
        end_line: Option<usize>,
        end_column: Option<usize>,
        message: Option<String>,
        wait_id: Option<u64>,
    ) {
        self.pending_file_opens.push(super::PendingFileOpen {
            path,
            line,
            column,
            end_line,
            end_column,
            message,
            wait_id,
        });
    }

    /// Process pending file opens (called from the event loop).
    ///
    /// Opens files that were queued during startup, using the same error handling
    /// as interactive file opens. Returns true if any files were processed.
    pub fn process_pending_file_opens(&mut self) -> bool {
        if self.pending_file_opens.is_empty() {
            return false;
        }

        // Take all pending files to process
        let pending = std::mem::take(&mut self.pending_file_opens);
        let mut processed_any = false;

        for pending_file in pending {
            tracing::info!(
                "[SYNTAX DEBUG] Processing pending file open: {:?}",
                pending_file.path
            );

            match self.open_file(&pending_file.path) {
                Ok(_) => {
                    // Navigate to line/column or select range if specified
                    if let (Some(line), Some(end_line)) = (pending_file.line, pending_file.end_line)
                    {
                        self.select_range(
                            line,
                            pending_file.column,
                            end_line,
                            pending_file.end_column,
                        );
                    } else if let Some(line) = pending_file.line {
                        self.goto_line_col(line, pending_file.column);
                    }
                    // Show hover message popup if specified
                    let has_popup = pending_file.message.is_some();
                    if let Some(ref msg) = pending_file.message {
                        self.show_file_message_popup(msg);
                    }
                    // Track wait ID for --wait support
                    if let Some(wait_id) = pending_file.wait_id {
                        let buffer_id = self.active_buffer();
                        self.wait_tracking.insert(buffer_id, (wait_id, has_popup));
                    }
                    processed_any = true;
                }
                Err(e) => {
                    // Check if this is a large file encoding confirmation error
                    // Show prompt instead of crashing
                    if let Some(confirmation) =
                        e.downcast_ref::<crate::model::buffer::LargeFileEncodingConfirmation>()
                    {
                        self.start_large_file_encoding_confirmation(confirmation);
                    } else {
                        // For other errors, show status message (consistent with file browser)
                        self.set_status_message(
                            t!("file.error_opening", error = e.to_string()).to_string(),
                        );
                    }
                    processed_any = true;
                }
            }
        }

        // Apply hot exit recovery if flagged (one-shot after CLI files are opened)
        if processed_any && self.pending_hot_exit_recovery {
            self.pending_hot_exit_recovery = false;
            match self.apply_hot_exit_recovery() {
                Ok(count) if count > 0 => {
                    tracing::info!("Hot exit: restored unsaved changes for {} buffer(s)", count);
                }
                Ok(_) => {}
                Err(e) => {
                    tracing::warn!("Failed to apply hot exit recovery: {}", e);
                }
            }
        }

        processed_any
    }

    /// Take and return completed wait IDs (for --wait support).
    pub fn take_completed_waits(&mut self) -> Vec<u64> {
        std::mem::take(&mut self.completed_waits)
    }

    /// Remove wait tracking for a given wait_id (e.g., when waiting client disconnects).
    pub fn remove_wait_tracking(&mut self, wait_id: u64) {
        self.wait_tracking.retain(|_, (wid, _)| *wid != wait_id);
    }
}
