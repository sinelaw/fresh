//! Tracing subscriber setup
//!
//! This module provides shared tracing configuration used by both
//! the main application and tests.

use std::fs::File;
use std::path::Path;
use std::sync::Arc;
use tracing_subscriber::prelude::*;
use tracing_subscriber::{fmt, EnvFilter};

use super::warning_log::{WarningLogHandle, WarningLogLayer};

/// Initialize the global tracing subscriber with file logging and warning capture.
///
/// This sets up:
/// - File-based logging with the given log file
/// - Environment-based filtering (RUST_LOG) with DEBUG default
/// - Warning log layer that captures WARN+ to a separate file
///
/// Returns the warning log handle if successful, None if setup failed.
pub fn init_global(log_file_path: &Path) -> Option<WarningLogHandle> {
    let (warning_layer, warning_handle) = super::warning_log::create().ok()?;
    let log_file = File::create(log_file_path).ok()?;

    let subscriber = build_subscriber(log_file, Some(warning_layer));
    subscriber.init();

    Some(warning_handle)
}

/// Build a subscriber with file logging and optional warning layer.
///
/// This is the core subscriber configuration shared between production and tests.
pub fn build_subscriber(
    log_file: File,
    warning_layer: Option<WarningLogLayer>,
) -> impl tracing::Subscriber + Send + Sync {
    let env_filter = EnvFilter::from_default_env()
        .add_directive(tracing::Level::DEBUG.into())
        // Suppress noisy SWC debug logs
        .add_directive("swc_ecma_transforms_base=info".parse().unwrap())
        .add_directive("swc_common=info".parse().unwrap());

    let fmt_layer = fmt::layer().with_writer(Arc::new(log_file));

    tracing_subscriber::registry()
        .with(fmt_layer)
        .with(env_filter)
        .with(warning_layer)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;
    use tempfile::{NamedTempFile, TempPath};

    struct TestSubscriber {
        subscriber: Box<dyn tracing::Subscriber + Send + Sync>,
        warning_handle: WarningLogHandle,
        // Keep tempfiles alive so they don't get deleted
        _log_file: NamedTempFile,
        _warning_log_path: TempPath,
    }

    fn create_test_subscriber() -> TestSubscriber {
        let log_file = NamedTempFile::new().unwrap();
        let warning_log_file = NamedTempFile::new().unwrap();
        let warning_log_path = warning_log_file.into_temp_path();

        let (warning_layer, warning_handle) =
            super::super::warning_log::create_with_path(warning_log_path.to_path_buf()).unwrap();

        let subscriber = build_subscriber(log_file.reopen().unwrap(), Some(warning_layer));

        TestSubscriber {
            subscriber: Box::new(subscriber),
            warning_handle,
            _log_file: log_file,
            _warning_log_path: warning_log_path,
        }
    }

    #[test]
    fn test_warning_log_captures_warn_level() {
        let test = create_test_subscriber();
        let path = test.warning_handle.path.clone();

        tracing::subscriber::with_default(test.subscriber, || {
            tracing::warn!("Test warning message");
        });

        let result = test
            .warning_handle
            .receiver
            .recv_timeout(Duration::from_secs(1));
        assert!(result.is_ok(), "Should receive notification for WARN");

        let contents = std::fs::read_to_string(&path).expect("Failed to read log");
        assert!(contents.contains("WARN"), "Log should contain WARN level");
        assert!(
            contents.contains("Test warning message"),
            "Log should contain message"
        );
    }

    #[test]
    fn test_warning_log_captures_error_level() {
        let test = create_test_subscriber();
        let path = test.warning_handle.path.clone();

        tracing::subscriber::with_default(test.subscriber, || {
            tracing::error!("Test error message");
        });

        let result = test
            .warning_handle
            .receiver
            .recv_timeout(Duration::from_secs(1));
        assert!(result.is_ok(), "Should receive notification for ERROR");

        let contents = std::fs::read_to_string(&path).expect("Failed to read log");
        assert!(contents.contains("ERROR"), "Log should contain ERROR level");
        assert!(
            contents.contains("Test error message"),
            "Log should contain message"
        );
    }

    #[test]
    fn test_warning_log_ignores_info_level() {
        let test = create_test_subscriber();
        let path = test.warning_handle.path.clone();

        tracing::subscriber::with_default(test.subscriber, || {
            tracing::info!("Test info message");
        });

        let result = test
            .warning_handle
            .receiver
            .recv_timeout(Duration::from_millis(100));
        assert!(result.is_err(), "Should NOT receive notification for INFO");

        let contents = std::fs::read_to_string(&path).unwrap_or_default();
        assert!(
            !contents.contains("Test info message"),
            "Log should NOT contain INFO message"
        );
    }

    #[test]
    fn test_warning_log_ignores_debug_level() {
        let test = create_test_subscriber();

        tracing::subscriber::with_default(test.subscriber, || {
            tracing::debug!("Test debug message");
        });

        let result = test
            .warning_handle
            .receiver
            .recv_timeout(Duration::from_millis(100));
        assert!(result.is_err(), "Should NOT receive notification for DEBUG");
    }

    #[test]
    fn test_warning_log_multiple_warnings() {
        let test = create_test_subscriber();
        let path = test.warning_handle.path.clone();

        tracing::subscriber::with_default(test.subscriber, || {
            tracing::warn!("First warning");
            tracing::error!("An error");
            tracing::warn!("Second warning");
        });

        for i in 0..3 {
            let result = test
                .warning_handle
                .receiver
                .recv_timeout(Duration::from_secs(1));
            assert!(result.is_ok(), "Should receive notification {}", i + 1);
        }

        let contents = std::fs::read_to_string(&path).expect("Failed to read log");
        assert!(
            contents.contains("First warning"),
            "Log should contain first warning"
        );
        assert!(contents.contains("An error"), "Log should contain error");
        assert!(
            contents.contains("Second warning"),
            "Log should contain second warning"
        );
    }
}
