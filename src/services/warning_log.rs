//! Warning log layer for tracing
//!
//! This module provides a custom tracing layer that captures WARN and ERROR
//! level logs to a separate file and notifies the editor when warnings occur.

use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use tracing::Level;
use tracing_subscriber::layer::Context;
use tracing_subscriber::registry::LookupSpan;
use tracing_subscriber::Layer;

/// A tracing layer that writes WARN+ logs to a file and notifies via channel
pub struct WarningLogLayer {
    file: Arc<Mutex<File>>,
    sender: mpsc::Sender<()>,
}

/// Handle returned from setup, containing the receiver and log path
pub struct WarningLogHandle {
    /// Receiver that gets notified when warnings are logged
    pub receiver: mpsc::Receiver<()>,
    /// Path to the warning log file
    pub path: PathBuf,
}

/// Create a warning log layer and handle
///
/// Returns the layer (to add to tracing subscriber) and a handle (to pass to editor)
pub fn create() -> std::io::Result<(WarningLogLayer, WarningLogHandle)> {
    create_with_path(
        std::env::temp_dir().join(format!("fresh-warnings-{}.log", std::process::id())),
    )
}

/// Create a warning log layer with a specific path (for testing)
pub fn create_with_path(path: PathBuf) -> std::io::Result<(WarningLogLayer, WarningLogHandle)> {
    let file = File::create(&path)?;

    let (sender, receiver) = mpsc::channel();

    let layer = WarningLogLayer {
        file: Arc::new(Mutex::new(file)),
        sender,
    };

    let handle = WarningLogHandle { receiver, path };

    Ok((layer, handle))
}

impl<S> Layer<S> for WarningLogLayer
where
    S: tracing::Subscriber + for<'a> LookupSpan<'a>,
{
    fn on_event(&self, event: &tracing::Event<'_>, _ctx: Context<'_, S>) {
        // Only capture WARN and ERROR
        let level = *event.metadata().level();
        if level > Level::WARN {
            return;
        }

        // Format the event
        let mut visitor = StringVisitor::default();
        event.record(&mut visitor);

        let line = format!(
            "{} {} {}: {}\n",
            chrono::Local::now().format("%Y-%m-%d %H:%M:%S%.3f"),
            level,
            event.metadata().target(),
            visitor.0
        );

        // Write to file
        if let Ok(mut file) = self.file.lock() {
            let _ = file.write_all(line.as_bytes());
            let _ = file.flush();
        }

        // Notify that a warning was logged (non-blocking)
        let _ = self.sender.send(());
    }
}

/// Simple visitor to extract message from event
#[derive(Default)]
struct StringVisitor(String);

impl tracing::field::Visit for StringVisitor {
    fn record_debug(&mut self, field: &tracing::field::Field, value: &dyn std::fmt::Debug) {
        if field.name() == "message" {
            self.0 = format!("{:?}", value);
        } else if !self.0.is_empty() {
            self.0.push_str(&format!(" {}={:?}", field.name(), value));
        } else {
            self.0 = format!("{}={:?}", field.name(), value);
        }
    }

    fn record_str(&mut self, field: &tracing::field::Field, value: &str) {
        if field.name() == "message" {
            self.0 = value.to_string();
        } else if !self.0.is_empty() {
            self.0.push_str(&format!(" {}={}", field.name(), value));
        } else {
            self.0 = format!("{}={}", field.name(), value);
        }
    }
}
