//! Errors as the protocol reports them: a stable, machine-readable code and
//! a message for people.
//!
//! An operation fails with an [`ApiError`] (made by [`err`] or the [`fail!`]
//! macro) when it knows why; an I/O error from the filesystem is coded by its
//! kind. `/api` answers `{"error": {"code", "message"}}`; clients act on the
//! code and show the message.

use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Code {
    /// A missing or mistyped argument, or one out of range.
    BadRequest,
    UnknownOp,
    /// The client has no live event stream (it never said `hello`, or its
    /// stream ended).
    UnknownClient,
    /// The workspace is restricted: no processes, terminals or language
    /// servers until it is trusted.
    Untrusted,
    /// No such file, folder, document or terminal.
    NotFound,
    /// The target name is taken (by a file, or by an open document).
    Exists,
    /// The file changed on disk since it was loaded or last saved; saving
    /// again with `force` overwrites it.
    ChangedOnDisk,
    /// The file is in use in a way that forbids the operation: a huge
    /// file read lazily from disk cannot be moved or deleted while open.
    InUse,
    /// An edit's base version is older than the kept log: reload the
    /// document.
    Resync,
    Binary,
    TooLarge,
    NoLanguageServer,
    /// The language server answered with an error, or did not answer.
    LanguageServer,
    PermissionDenied,
    /// Any other I/O error.
    Io,
    /// Anything else.
    Failed,
}

impl Code {
    pub fn as_str(self) -> &'static str {
        match self {
            Code::BadRequest => "bad_request",
            Code::UnknownOp => "unknown_op",
            Code::UnknownClient => "unknown_client",
            Code::Untrusted => "untrusted",
            Code::NotFound => "not_found",
            Code::Exists => "exists",
            Code::ChangedOnDisk => "changed_on_disk",
            Code::InUse => "in_use",
            Code::Resync => "resync",
            Code::Binary => "binary",
            Code::TooLarge => "too_large",
            Code::NoLanguageServer => "no_language_server",
            Code::LanguageServer => "language_server",
            Code::PermissionDenied => "permission_denied",
            Code::Io => "io",
            Code::Failed => "failed",
        }
    }
}

#[derive(Debug)]
pub struct ApiError {
    pub code: Code,
    pub message: String,
}

impl fmt::Display for ApiError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for ApiError {}

pub fn err(code: Code, message: impl Into<String>) -> anyhow::Error {
    ApiError {
        code,
        message: message.into(),
    }
    .into()
}

/// `fail!(Code, "format", args…)`: return an [`ApiError`].
macro_rules! fail {
    ($code:ident, $($t:tt)*) => {
        return Err($crate::error::err($crate::error::Code::$code, format!($($t)*)))
    };
}

/// The code of any error: the one it was raised with, or an I/O error's
/// kind, or `failed`.
pub fn code_of(e: &anyhow::Error) -> Code {
    for c in e.chain() {
        if let Some(a) = c.downcast_ref::<ApiError>() {
            return a.code;
        }
        if let Some(io) = c.downcast_ref::<std::io::Error>() {
            return match io.kind() {
                std::io::ErrorKind::NotFound => Code::NotFound,
                std::io::ErrorKind::AlreadyExists => Code::Exists,
                std::io::ErrorKind::PermissionDenied => Code::PermissionDenied,
                _ => Code::Io,
            };
        }
    }
    Code::Failed
}

/// The protocol's error object.
pub fn to_json(e: &anyhow::Error) -> serde_json::Value {
    serde_json::json!({ "code": code_of(e).as_str(), "message": e.to_string() })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn codes_come_from_the_error_or_its_io_kind() {
        let e = err(Code::ChangedOnDisk, "x changed");
        assert_eq!(to_json(&e)["code"], "changed_on_disk");
        assert_eq!(to_json(&e)["message"], "x changed");
        let io: anyhow::Error = std::io::Error::from(std::io::ErrorKind::NotFound).into();
        assert_eq!(code_of(&io), Code::NotFound);
        let wrapped = io.context("reading a");
        assert_eq!(code_of(&wrapped), Code::NotFound);
        assert_eq!(code_of(&anyhow::anyhow!("other")), Code::Failed);
    }
}
