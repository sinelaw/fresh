use super::time_source::TimeSource;
use serde::Serialize;
use std::collections::hash_map::RandomState;
use std::env::consts::{ARCH, OS};
use std::fs;
use std::hash::{BuildHasher, Hasher};
use std::io::Write;
use std::path::PathBuf;
use std::thread;
use std::time::Duration;

const TELEMETRY_URL: &str = "https://t.getfresh.dev";
const STAMP_FILE_NAME: &str = "telemetry_stamp";

#[derive(Serialize, Default)]
struct Event {
    #[serde(skip_serializing_if = "Option::is_none")]
    version: Option<&'static str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    os: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    command: Option<&'static str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    value: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    uid: Option<String>,
}

/// Stamp file data containing unique ID and last check date
struct StampData {
    unique_id: String,
    last_date: String,
}

/// Get the path to the telemetry stamp file
fn stamp_file_path(data_dir: &std::path::Path) -> PathBuf {
    data_dir.join("fresh").join(STAMP_FILE_NAME)
}

/// Generate a random 64-bit hex string using std's RandomState
fn generate_unique_id() -> String {
    let state = RandomState::new();
    let mut hasher = state.build_hasher();
    // Add extra entropy from time and process ID
    hasher.write_u128(
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0),
    );
    hasher.write_u32(std::process::id());
    format!("{:016x}", hasher.finish())
}

/// Read stamp file data (unique_id and last_date)
/// Returns None if file doesn't exist, can't be read, or is malformed.
fn read_stamp_file(data_dir: &std::path::Path) -> Option<StampData> {
    let path = stamp_file_path(data_dir);
    let content = fs::read_to_string(&path).ok()?;
    let mut lines = content.lines();
    let unique_id = lines.next().filter(|s| !s.is_empty())?.to_string();
    let last_date = lines.next().filter(|s| !s.is_empty())?.to_string();
    // Validate date format (YYYY-MM-DD)
    if last_date.len() != 10 || last_date.chars().filter(|&c| c == '-').count() != 2 {
        return None;
    }
    Some(StampData {
        unique_id,
        last_date,
    })
}

/// Write stamp file with unique_id and the given date
fn write_stamp_file(data_dir: &std::path::Path, unique_id: &str, today: &str) -> bool {
    let path = stamp_file_path(data_dir);

    // Create parent directory if needed
    if let Some(parent) = path.parent() {
        if let Err(e) = fs::create_dir_all(parent) {
            tracing::debug!("Failed to create telemetry stamp directory: {}", e);
            return false;
        }
    }

    let content = format!("{}\n{}\n", unique_id, today);
    match fs::File::create(&path).and_then(|mut f| f.write_all(content.as_bytes())) {
        Ok(()) => true,
        Err(e) => {
            tracing::debug!("Failed to write telemetry stamp file: {}", e);
            false
        }
    }
}

/// Check if we should run the daily check (telemetry + update).
/// Returns Some(unique_id) if we should proceed, None if already done today.
pub fn should_run_daily_check(
    time_source: &dyn TimeSource,
    data_dir: &std::path::Path,
) -> Option<String> {
    let today = time_source.today_date_string();

    match read_stamp_file(data_dir) {
        Some(data) if data.last_date == today => {
            // Already checked today, skip
            tracing::debug!("Daily check already done today, skipping");
            None
        }
        Some(data) => {
            // Have ID but date is old, update stamp and proceed
            write_stamp_file(data_dir, &data.unique_id, &today);
            Some(data.unique_id)
        }
        None => {
            // No stamp file, generate new ID
            let unique_id = generate_unique_id();
            write_stamp_file(data_dir, &unique_id, &today);
            Some(unique_id)
        }
    }
}

/// Track app open with unique ID
pub fn track_open(unique_id: &str) {
    let event = Event {
        version: Some(env!("CARGO_PKG_VERSION")),
        os: Some(format!("{}-{}", OS, ARCH)),
        command: Some("fresh"),
        value: std::env::var("TERM").ok(),
        uid: Some(unique_id.to_string()),
    };
    send(event);
}

fn send(event: Event) {
    // Serialize to JSON string to own the data for the thread
    let Ok(body) = serde_json::to_string(&event) else {
        return;
    };

    thread::spawn(move || {
        let _ = ureq::post(TELEMETRY_URL)
            .set("Content-Type", "application/json")
            .timeout(Duration::from_secs(5))
            .send_bytes(body.as_bytes());
    });
}
