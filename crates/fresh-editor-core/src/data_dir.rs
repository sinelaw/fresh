//! Fresh's per-user data directory.
//!
//! Shared by the input-history store (`fresh-editor`) and the in-place-write
//! recovery temp files (`model::buffer::save`), which is why it sits down here
//! rather than next to either caller.

/// The `fresh` data directory (`$XDG_DATA_HOME/fresh`, or the platform equivalent).
pub fn get_data_dir() -> std::io::Result<std::path::PathBuf> {
    let data_dir = dirs::data_dir().ok_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "Could not determine data directory",
        )
    })?;
    Ok(data_dir.join("fresh"))
}
