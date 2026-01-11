use serde::Serialize;
use std::env::consts::{ARCH, OS};
use std::thread;
use std::time::Duration;

const TELEMETRY_URL: &str = "https://t.getfresh.dev";

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
}

/// Track app open
pub fn track_open() {
    let event = Event {
        version: Some(env!("CARGO_PKG_VERSION")),
        os: Some(format!("{}-{}", OS, ARCH)),
        command: Some("fresh"),
        value: std::env::var("TERM").ok(),
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
