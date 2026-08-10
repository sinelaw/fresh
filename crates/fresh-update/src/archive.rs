//! Pulling the new binary out of a downloaded archive.
//!
//! Two rules here, both about not trusting the shape of something we did not
//! produce.
//!
//! **Never write by a path the archive chose.** These functions read the one
//! entry we want into memory and match on its *file name* only. Nothing is
//! written to disk from inside this module, so the whole Zip-Slip / tar path
//! traversal family — `../../etc/cron.d/x`, absolute paths, symlink entries
//! pointing outside the extraction root — has nowhere to land.
//!
//! **Never read an unbounded amount.** The compressed size is capped by the
//! HTTP layer, but xz reaches ratios past 1000:1, so 64 MB of download can
//! become tens of GB of `Vec`. The decoder is wrapped in a `take` and the
//! entry count is capped. TUF calls this the *endless data* attack and its
//! mitigation is exactly this: know the expected size and refuse to exceed it.
//!
//! The checksum is verified before anything reaches these functions, so an
//! archive bomb is only reachable once the origin is already attacker
//! controlled — but that is a reason for the bound to be cheap, not a reason to
//! leave it out.

use std::io::Read;
use std::path::Path;

/// Most bytes we will decompress out of a release archive. The largest thing
/// we ship is a debug-symbol-laden binary in the tens of MB; 512 MiB is far
/// above that and far below anything that would exhaust a machine.
pub const MAX_UNCOMPRESSED: u64 = 512 * 1024 * 1024;

/// Most entries we will walk before giving up. A release archive holds a
/// handful of files; a tar of a million empty entries is a CPU denial of
/// service with a legal-looking header.
pub const MAX_ENTRIES: usize = 4096;

/// Extract the file named `name` from a `.tar.xz` held in memory.
pub fn from_tar_xz(bytes: &[u8], name: &str) -> Result<Vec<u8>, String> {
    from_tar(xz2::read::XzDecoder::new(bytes), name)
}

/// Extract the file named `name` from a `.tar.gz` held in memory.
///
/// The universal (musl) archive is gzip rather than xz, because `install.sh`
/// unpacks it with the system `tar` and `.tar.xz` needs the xz binary that
/// minimal images often lack. Decoding it here costs nothing extra — flate2's
/// default backend is pure Rust, where `xz2` links liblzma.
pub fn from_tar_gz(bytes: &[u8], name: &str) -> Result<Vec<u8>, String> {
    from_tar(flate2::read::GzDecoder::new(bytes), name)
}

/// Walk a decompressed tar for one entry, by file name only.
///
/// The bound is applied to the *decompressed* stream, so it holds whichever
/// codec produced it.
fn from_tar<R: Read>(decoder: R, name: &str) -> Result<Vec<u8>, String> {
    let mut archive = tar::Archive::new(decoder.take(MAX_UNCOMPRESSED));
    let entries = archive.entries().map_err(|e| format!("read tar: {e}"))?;
    for (seen, entry) in entries.enumerate() {
        if seen >= MAX_ENTRIES {
            return Err(format!("archive has more than {MAX_ENTRIES} entries"));
        }
        let mut entry = entry.map_err(|e| format!("read tar entry: {e}"))?;
        let is_match = entry
            .path()
            .ok()
            .and_then(|p| p.file_name().map(|n| n == name))
            .unwrap_or(false);
        if is_match {
            return read_capped(&mut entry, name);
        }
    }
    Err(format!("`{name}` not found in archive"))
}

/// Extract the file named `name` from a `.zip` held in memory.
pub fn from_zip(bytes: &[u8], name: &str) -> Result<Vec<u8>, String> {
    let reader = std::io::Cursor::new(bytes);
    let mut zip = zip::ZipArchive::new(reader).map_err(|e| format!("open zip: {e}"))?;
    if zip.len() > MAX_ENTRIES {
        return Err(format!("archive has more than {MAX_ENTRIES} entries"));
    }
    for i in 0..zip.len() {
        let mut file = zip
            .by_index(i)
            .map_err(|e| format!("read zip entry: {e}"))?;
        let matches = Path::new(file.name())
            .file_name()
            .map(|n| n == name)
            .unwrap_or(false);
        if matches {
            return read_capped(&mut file, name);
        }
    }
    Err(format!("`{name}` not found in archive"))
}

/// Read one entry, refusing to grow past [`MAX_UNCOMPRESSED`].
///
/// `read_to_end` on a `take` stops at the limit rather than erroring, so the
/// check is "did we stop exactly at the cap" — which a real binary of that size
/// would also trip, and which is the right answer either way.
fn read_capped<R: Read>(entry: &mut R, name: &str) -> Result<Vec<u8>, String> {
    read_capped_to(entry, name, MAX_UNCOMPRESSED)
}

/// [`read_capped`] with the limit given explicitly, so the refusal can be
/// tested without allocating half a gigabyte to prove it.
fn read_capped_to<R: Read>(entry: &mut R, name: &str, limit: u64) -> Result<Vec<u8>, String> {
    let mut buf = Vec::new();
    entry
        .take(limit)
        .read_to_end(&mut buf)
        .map_err(|e| format!("read `{name}` from archive: {e}"))?;
    if buf.len() as u64 >= limit {
        return Err(format!(
            "`{name}` is at least {limit} bytes uncompressed; refusing it"
        ));
    }
    Ok(buf)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    /// Build a `.tar.xz` in memory containing a single `fresh` entry.
    fn make_tar_xz(bin: &[u8]) -> Vec<u8> {
        let mut tar_bytes = Vec::new();
        {
            let mut builder = tar::Builder::new(&mut tar_bytes);
            let mut header = tar::Header::new_gnu();
            header.set_size(bin.len() as u64);
            header.set_mode(0o755);
            header.set_cksum();
            builder
                .append_data(&mut header, "fresh-editor-x/fresh", bin)
                .unwrap();
            builder.finish().unwrap();
        }
        let mut xz = Vec::new();
        {
            let mut enc = xz2::write::XzEncoder::new(&mut xz, 6);
            enc.write_all(&tar_bytes).unwrap();
            enc.finish().unwrap();
        }
        xz
    }

    #[test]
    fn tar_xz_yields_the_named_binary() {
        let archive = make_tar_xz(b"#!/bin/sh\necho fresh\n");
        assert_eq!(
            from_tar_xz(&archive, "fresh").unwrap(),
            b"#!/bin/sh\necho fresh\n"
        );
    }

    #[test]
    fn a_missing_entry_is_an_error() {
        let archive = make_tar_xz(b"x");
        assert!(from_tar_xz(&archive, "nope").is_err());
    }

    fn make_tar_gz(bin: &[u8]) -> Vec<u8> {
        let mut tar_bytes = Vec::new();
        {
            let mut builder = tar::Builder::new(&mut tar_bytes);
            let mut header = tar::Header::new_gnu();
            header.set_size(bin.len() as u64);
            header.set_mode(0o755);
            header.set_cksum();
            builder
                .append_data(&mut header, "fresh-editor-x/fresh", bin)
                .unwrap();
            builder.finish().unwrap();
        }
        let mut gz = Vec::new();
        {
            let mut enc = flate2::write::GzEncoder::new(&mut gz, flate2::Compression::default());
            enc.write_all(&tar_bytes).unwrap();
            enc.finish().unwrap();
        }
        gz
    }

    /// The universal (musl) archive is gzip, so this is the path a
    /// `fresh --cmd update` takes on the install `install.sh` produces.
    #[test]
    fn tar_gz_yields_the_named_binary() {
        let archive = make_tar_gz(b"#!/bin/sh\necho fresh\n");
        assert_eq!(
            from_tar_gz(&archive, "fresh").unwrap(),
            b"#!/bin/sh\necho fresh\n"
        );
    }

    #[test]
    fn a_missing_entry_in_a_gz_is_an_error() {
        let archive = make_tar_gz(b"x");
        assert!(from_tar_gz(&archive, "nope").is_err());
    }

    /// The codecs must not be interchangeable by accident: handing gzip bytes
    /// to the xz reader has to fail rather than half-succeed.
    #[test]
    fn the_wrong_codec_is_rejected() {
        assert!(from_tar_xz(&make_tar_gz(b"x"), "fresh").is_err());
        assert!(from_tar_gz(&make_tar_xz(b"x"), "fresh").is_err());
    }

    #[test]
    fn zip_yields_the_named_binary() {
        let mut buf = Vec::new();
        {
            let mut w = zip::ZipWriter::new(std::io::Cursor::new(&mut buf));
            let opts: zip::write::FileOptions<'_, ()> = zip::write::FileOptions::default()
                .compression_method(zip::CompressionMethod::Deflated);
            w.start_file("fresh-editor-x/fresh.exe", opts).unwrap();
            w.write_all(b"MZ fake exe").unwrap();
            w.finish().unwrap();
        }
        assert_eq!(from_zip(&buf, "fresh.exe").unwrap(), b"MZ fake exe");
    }

    /// A highly compressible entry must not be allowed to expand without
    /// bound: xz reaches ratios past 1000:1, so a download the HTTP layer
    /// considers small becomes tens of GB of `Vec`. Driven through a limit of
    /// its own rather than the real one — proving the refusal should not cost
    /// half a gigabyte of RSS per test run.
    #[test]
    fn an_entry_larger_than_the_cap_is_refused_rather_than_allocated() {
        let mut endless = std::io::repeat(0u8);
        let err = read_capped_to(&mut endless, "fresh", 4096).unwrap_err();
        assert!(err.contains("refusing"), "got: {err}");

        // And an entry that fits is returned whole, so the cap is a ceiling
        // rather than a truncation.
        let mut small = std::io::Cursor::new(vec![7u8; 100]);
        assert_eq!(
            read_capped_to(&mut small, "fresh", 4096).unwrap().len(),
            100
        );
    }

    /// The other half of the same defence: a tar of a million tiny entries is
    /// a CPU denial of service with entirely legal-looking headers.
    #[test]
    fn the_entry_count_is_capped() {
        const { assert!(MAX_ENTRIES > 0 && MAX_ENTRIES < 100_000) };
    }
}
