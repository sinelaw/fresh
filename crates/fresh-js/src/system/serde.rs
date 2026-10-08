//! Conversion between JS values and serde types, through JSON.
//!
//! rquickjs-serde walks values directly; going through `JSON.stringify` /
//! `JSON.parse` gives the same results for the plain data the plugin API
//! exchanges (objects, arrays, strings, numbers, booleans, null; `None` and
//! `()` become `null`), at the cost of a string round-trip.

use super::{Ctx, Value};
use std::fmt;

/// A conversion failure.
#[derive(Debug)]
pub struct Error(String);

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for Error {}

impl From<super::Error> for Error {
    fn from(e: super::Error) -> Self {
        Error(e.to_string())
    }
}

/// Convert a Rust value into a JS value.
pub fn to_value<T: serde::Serialize>(ctx: Ctx<'_>, value: T) -> Result<Value<'_>, Error> {
    let json = serde_json::to_string(&value).map_err(|e| Error(e.to_string()))?;
    ctx.json_parse(json).map_err(Error::from)
}

/// Convert a JS value into a Rust value.
pub fn from_value<T: serde::de::DeserializeOwned>(value: Value<'_>) -> Result<T, Error> {
    let ctx = value.ctx().clone();
    let json = match ctx.json_stringify(&value) {
        Ok(Some(json)) => json,
        // undefined, functions and symbols have no JSON form; serde sees unit.
        Ok(None) => "null".to_string(),
        Err(e) => {
            // Clear the exception JSON.stringify raised (e.g. a BigInt or a
            // cycle) so it does not leak into the caller's next check.
            let _ = ctx.catch();
            return Err(Error::from(e));
        }
    };
    serde_json::from_str(&json).map_err(|e| Error(e.to_string()))
}
