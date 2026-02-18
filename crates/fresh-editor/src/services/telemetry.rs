use super::time_source::TimeSource;
use chrono::NaiveDate;
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
    last_date: NaiveDate,
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

/// Format stamp data into file content.
fn format_stamp_content(unique_id: &str, date: NaiveDate) -> String {
    format!("{}\n{}\n", unique_id, date.format("%Y-%m-%d"))
}

/// Parse stamp file content into StampData.
/// Expects format: unique_id on first line, YYYY-MM-DD date on second line.
fn parse_stamp_content(content: &str) -> Option<StampData> {
    let mut lines = content.lines();
    let unique_id = lines.next().filter(|s| !s.is_empty())?.to_string();
    let date_str = lines.next().filter(|s| !s.is_empty())?;
    let last_date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d").ok()?;
    Some(StampData {
        unique_id,
        last_date,
    })
}

/// Read stamp file data (unique_id and last_date)
/// Returns None if file doesn't exist, can't be read, or is malformed.
fn read_stamp_file(data_dir: &std::path::Path) -> Option<StampData> {
    let path = stamp_file_path(data_dir);
    let content = fs::read_to_string(&path).ok()?;
    parse_stamp_content(&content)
}

/// Write stamp file with unique_id and the given date
fn write_stamp_file(data_dir: &std::path::Path, unique_id: &str, today: NaiveDate) -> bool {
    let path = stamp_file_path(data_dir);

    // Create parent directory if needed
    if let Some(parent) = path.parent() {
        if let Err(e) = fs::create_dir_all(parent) {
            tracing::debug!("Failed to create telemetry stamp directory: {}", e);
            return false;
        }
    }

    let content = format_stamp_content(unique_id, today);
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
    let today = time_source.today_date();

    match read_stamp_file(data_dir) {
        Some(data) if data.last_date == today => {
            // Already checked today, skip
            tracing::debug!("Daily check already done today, skipping");
            None
        }
        Some(data) => {
            // Have ID but date is old, update stamp and proceed
            write_stamp_file(data_dir, &data.unique_id, today);
            Some(data.unique_id)
        }
        None => {
            // No stamp file, generate new ID
            let unique_id = generate_unique_id();
            write_stamp_file(data_dir, &unique_id, today);
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

    // Best-effort fire-and-forget telemetry -- if the request fails, we silently ignore it.
    #[allow(clippy::let_underscore_must_use)]
    let _ = thread::spawn(move || {
        let agent = ureq::Agent::config_builder()
            .timeout_global(Some(Duration::from_secs(5)))
            .build()
            .new_agent();
        #[allow(clippy::let_underscore_must_use)]
        let _ = agent
            .post(TELEMETRY_URL)
            .header("Content-Type", "application/json")
            .send(body.as_bytes());
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::services::time_source::TestTimeSource;
    use std::time::Duration;

    #[test]
    fn parse_stamp_content_valid() {
        for content in ["abc123\n2025-01-15\n", "abc123\n2025-01-15"] {
            let result = parse_stamp_content(content).unwrap();
            assert_eq!(result.unique_id, "abc123");
            assert_eq!(
                result.last_date,
                NaiveDate::from_ymd_opt(2025, 1, 15).unwrap()
            );
        }
    }

    #[test]
    fn parse_stamp_content_invalid() {
        for content in [
            "",                     // empty
            "abc123\n",             // missing date
            "abc123",               // missing date
            "\n2025-01-15\n",       // empty unique_id
            "abc123\nnot-a-date\n", // invalid date
            "abc123\n2025-13-15\n", // invalid month
            "abc123\n2025-02-30\n", // invalid day
        ] {
            assert!(
                parse_stamp_content(content).is_none(),
                "expected None for {:?}",
                content
            );
        }
    }

    #[test]
    fn should_run_daily_check_debounces_by_day() {
        let time_source = TestTimeSource::new();
        let temp_dir = tempfile::tempdir().unwrap();

        // First call: creates stamp file, returns unique_id
        let id1 = should_run_daily_check(&time_source, temp_dir.path());
        assert!(id1.is_some(), "first call should return Some");

        // Same-day call: returns None (already checked today)
        let id2 = should_run_daily_check(&time_source, temp_dir.path());
        assert!(id2.is_none(), "same-day call should return None");

        // Advance time by 1 day
        time_source.advance(Duration::from_secs(86400));

        // Next-day call: returns same unique_id
        let id3 = should_run_daily_check(&time_source, temp_dir.path());
        assert!(id3.is_some(), "next-day call should return Some");
        assert_eq!(id1, id3, "unique_id should persist across days");
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    fn valid_unique_id() -> impl Strategy<Value = String> {
        "[a-zA-Z0-9]{1,32}".prop_map(|s| s.to_string())
    }

    fn valid_date() -> impl Strategy<Value = NaiveDate> {
        (1970i32..2100, 1u32..=12, 1u32..=28)
            .prop_map(|(y, m, d)| NaiveDate::from_ymd_opt(y, m, d).unwrap())
    }

    proptest! {
        #[test]
        fn roundtrip_format_parse(id in valid_unique_id(), date in valid_date()) {
            let content = format_stamp_content(&id, date);
            let parsed = parse_stamp_content(&content).unwrap();
            prop_assert_eq!(parsed.unique_id, id);
            prop_assert_eq!(parsed.last_date, date);
        }
    }
}
