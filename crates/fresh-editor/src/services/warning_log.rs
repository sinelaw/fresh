//! Warning log layer for tracing
//!
//! This module provides a custom tracing layer that captures WARN and ERROR
//! level logs to a separate file and notifies the editor when warnings occur.
//! Duplicate messages are suppressed to avoid log spam.

use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use tracing::Level;
use tracing_subscriber::layer::Context;
use tracing_subscriber::registry::LookupSpan;
use tracing_subscriber::Layer;

/// Deduplication state for warning messages
struct DeduplicationState {
    /// Map from message hash to (last_seen_time, count)
    recent_messages: HashMap<u64, (Instant, usize)>,
    /// Time window for deduplication (messages within this window are deduplicated)
    window: Duration,
    /// Maximum number of unique messages to track
    max_entries: usize,
}

impl DeduplicationState {
    fn new() -> Self {
        Self {
            recent_messages: HashMap::new(),
            window: Duration::from_secs(5), // Deduplicate within 5 seconds
            max_entries: 100,               // Track up to 100 unique messages
        }
    }

    /// Check if a message should be logged or suppressed
    /// Returns (should_log, suppressed_count) where suppressed_count is > 0 when
    /// we're logging after having suppressed duplicates
    fn check_message(&mut self, message: &str) -> (bool, usize) {
        let hash = self.hash_message(message);
        let now = Instant::now();

        // Clean up old entries periodically
        if self.recent_messages.len() > self.max_entries {
            self.recent_messages
                .retain(|_, (time, _)| now.duration_since(*time) < self.window * 2);
        }

        if let Some((last_seen, count)) = self.recent_messages.get_mut(&hash) {
            if now.duration_since(*last_seen) < self.window {
                // Duplicate within window - suppress
                *count += 1;
                *last_seen = now;
                (false, 0)
            } else {
                // Same message but outside window - log it and report suppressed count
                let suppressed = *count;
                *count = 1;
                *last_seen = now;
                (true, suppressed.saturating_sub(1))
            }
        } else {
            // New message - log it
            self.recent_messages.insert(hash, (now, 1));
            (true, 0)
        }
    }

    fn hash_message(&self, message: &str) -> u64 {
        use std::hash::{Hash, Hasher};
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        message.hash(&mut hasher);
        hasher.finish()
    }
}

/// A tracing layer that writes WARN+ logs to a file and notifies via channel
pub struct WarningLogLayer {
    file: Arc<Mutex<File>>,
    sender: mpsc::Sender<()>,
    dedup: Arc<Mutex<DeduplicationState>>,
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
    create_with_path(super::log_dirs::warnings_log_path())
}

/// Create a warning log layer with a specific path (for testing)
pub fn create_with_path(path: PathBuf) -> std::io::Result<(WarningLogLayer, WarningLogHandle)> {
    let file = File::create(&path)?;

    let (sender, receiver) = mpsc::channel();

    let layer = WarningLogLayer {
        file: Arc::new(Mutex::new(file)),
        sender,
        dedup: Arc::new(Mutex::new(DeduplicationState::new())),
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

        // Check for duplicates
        let (should_log, suppressed_count) = if let Ok(mut dedup) = self.dedup.lock() {
            dedup.check_message(&visitor.0)
        } else {
            (true, 0) // If lock fails, log anyway
        };

        if !should_log {
            return; // Suppress duplicate
        }

        // Build the log line
        let timestamp = chrono::Local::now().format("%Y-%m-%d %H:%M:%S%.3f");
        let target = event.metadata().target();

        let line = if suppressed_count > 0 {
            format!(
                "{} {} {}: {} (suppressed {} similar messages)\n",
                timestamp, level, target, visitor.0, suppressed_count
            )
        } else {
            format!("{} {} {}: {}\n", timestamp, level, target, visitor.0)
        };

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
