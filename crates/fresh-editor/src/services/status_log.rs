//! Status message log layer for tracing
//!
//! This module provides a custom tracing layer that captures status messages
//! (those with target "status") to a separate file. This allows users to view
//! the full history of status messages by clicking on the status bar.

use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use tracing_subscriber::layer::Context;
use tracing_subscriber::registry::LookupSpan;
use tracing_subscriber::Layer;

/// A tracing layer that writes status messages to a file
pub struct StatusLogLayer {
    file: Arc<Mutex<File>>,
}

/// Handle returned from setup, containing the log path
pub struct StatusLogHandle {
    /// Path to the status log file
    pub path: PathBuf,
}

/// Create a status log layer and handle
///
/// Returns the layer (to add to tracing subscriber) and a handle (to pass to editor)
pub fn create() -> std::io::Result<(StatusLogLayer, StatusLogHandle)> {
    create_with_path(super::log_dirs::status_log_path())
}

/// Create a status log layer with a specific path (for testing)
pub fn create_with_path(path: PathBuf) -> std::io::Result<(StatusLogLayer, StatusLogHandle)> {
    let file = File::create(&path)?;

    let layer = StatusLogLayer {
        file: Arc::new(Mutex::new(file)),
    };

    let handle = StatusLogHandle { path };

    Ok((layer, handle))
}

impl<S> Layer<S> for StatusLogLayer
where
    S: tracing::Subscriber + for<'a> LookupSpan<'a>,
{
    fn on_event(&self, event: &tracing::Event<'_>, _ctx: Context<'_, S>) {
        // Only capture events with target "status"
        let target = event.metadata().target();
        if target != "status" {
            return;
        }

        // Format the event
        let mut visitor = StringVisitor::default();
        event.record(&mut visitor);

        // Build the log line with timestamp
        let timestamp = chrono::Local::now().format("%Y-%m-%d %H:%M:%S");
        let line = format!("{} {}\n", timestamp, visitor.0);

        // Write to file
        if let Ok(mut file) = self.file.lock() {
            let _ = file.write_all(line.as_bytes());
            let _ = file.flush();
        }
    }
}

/// Simple visitor to extract message from event
#[derive(Default)]
struct StringVisitor(String);

impl tracing::field::Visit for StringVisitor {
    fn record_debug(&mut self, field: &tracing::field::Field, value: &dyn std::fmt::Debug) {
        if field.name() == "message" {
            self.0 = format!("{:?}", value);
        }
    }

    fn record_str(&mut self, field: &tracing::field::Field, value: &str) {
        if field.name() == "message" {
            self.0 = value.to_string();
        }
    }
}
