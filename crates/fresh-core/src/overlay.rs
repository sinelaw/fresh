use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicU64, Ordering};

/// Opaque handle for an overlay
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, ts_rs::TS)]
#[ts(export)]
pub struct OverlayHandle(pub String);

impl OverlayHandle {
    pub fn new() -> Self {
        static NEXT_HANDLE: AtomicU64 = AtomicU64::new(1);
        Self(format!(
            "ovl_{}",
            NEXT_HANDLE.fetch_add(1, Ordering::Relaxed)
        ))
    }

    pub fn from_string(s: String) -> Self {
        Self(s)
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Default for OverlayHandle {
    fn default() -> Self {
        Self::new()
    }
}

/// Namespace for grouping overlays
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, ts_rs::TS)]
#[ts(export)]
pub struct OverlayNamespace(pub String);

impl OverlayNamespace {
    pub fn new() -> Self {
        static NEXT_NAMESPACE: AtomicU64 = AtomicU64::new(1);
        Self(format!(
            "ns_{}",
            NEXT_NAMESPACE.fetch_add(1, Ordering::Relaxed)
        ))
    }

    pub fn from_string(s: String) -> Self {
        Self(s)
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Default for OverlayNamespace {
    fn default() -> Self {
        Self::new()
    }
}
