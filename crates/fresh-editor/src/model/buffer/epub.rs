use epub::doc::{EpubDoc, NavPoint};
use html2text::from_read;
use std::collections::HashMap;
use std::io::Cursor;
use std::path::{Path, PathBuf};

/// Check if a path points to an EPUB file (case-insensitive extension check)
pub fn is_epub_path(path: &Path) -> bool {
    path.extension()
        .map(|ext| ext.to_string_lossy().to_ascii_lowercase() == "epub")
        .unwrap_or(false)
}

/// Helper to recursively collect TOC titles mapped to their resource paths
fn collect_toc_titles(nav_points: &[NavPoint], map: &mut HashMap<PathBuf, String>) {
    for nav in nav_points {
        map.insert(nav.content.clone(), nav.label.clone());
        collect_toc_titles(&nav.children, map);
    }
}

/// Extract plain text from EPUB file bytes
pub fn extract_epub_text(bytes: &[u8]) -> anyhow::Result<String> {
    let cursor = Cursor::new(bytes);
    let mut doc =
        EpubDoc::from_reader(cursor).map_err(|e| anyhow::anyhow!("Failed to parse EPUB: {}", e))?;

    let mut full_text = String::new();

    // 1. Extract metadata for the header
    if let Some(title) = doc.mdata("title") {
        full_text.push_str(&format!("# {}\n\n", title.value));
    }
    if let Some(author) = doc.mdata("creator") {
        full_text.push_str(&format!("Author: {}\n", author.value));
    }
    if let Some(language) = doc.mdata("language") {
        full_text.push_str(&format!("Language: {}\n", language.value));
    }
    full_text.push_str("\n========================================\n\n");

    // Build a map of internal paths to TOC labels
    let mut toc_map = HashMap::new();
    collect_toc_titles(&doc.toc, &mut toc_map);

    // 2. Iterate over spine (reading order) and extract text
    let spine_len = doc.spine.len();
    for i in 0..spine_len {
        if !doc.set_current_chapter(i) {
            anyhow::bail!("Failed to navigate EPUB spine to chapter {}", i);
        }

        let current_id = doc.get_current_id().unwrap_or_else(|| "".to_string());

        let title = if let Some(resource) = doc.resources.get(&current_id) {
            let path = &resource.path;
            let path_str = path.to_string_lossy();
            // Some EPUB paths have anchors (e.g. "chapter1.xhtml#section1").
            // Let's also check the path without the anchor fragment.
            let path_without_anchor = if let Some(idx) = path_str.find('#') {
                PathBuf::from(&path_str[..idx])
            } else {
                path.clone()
            };
            toc_map
                .get(path)
                .or_else(|| toc_map.get(&path_without_anchor))
                .cloned()
                .unwrap_or_else(|| format!("Chapter {}", i + 1))
        } else {
            format!("Chapter {}", i + 1)
        };

        if let Some(html_content) = doc.get_current_str() {
            // Convert HTML to human-readable plain text
            let text = from_read(html_content.0.as_bytes(), 100)
                .unwrap_or_else(|_| "[Error converting HTML content]".to_string());

            // If the HTML already starts with the title (often in an h1), avoid doubling it
            let first_line = text.lines().next().unwrap_or("").trim();
            let is_duplicate = if first_line.starts_with("# ") {
                let h1_title = first_line.trim_start_matches('#').trim();
                h1_title.to_lowercase() == title.to_lowercase()
            } else {
                first_line.to_lowercase() == title.to_lowercase()
            };

            if !is_duplicate {
                full_text.push_str(&format!("## {}\n\n", title));
            }

            full_text.push_str(&text);
            full_text.push_str("\n\n");
        } else {
            full_text.push_str(&format!("## {}\n\n", title));
        }

        if i + 1 < spine_len {
            full_text.push_str("\n----------------------------------------\n\n");
        }
    }

    Ok(full_text)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use zip::write::SimpleFileOptions;
    use zip::ZipWriter;

    #[test]
    fn test_is_epub_path() {
        assert!(is_epub_path(Path::new("book.epub")));
        assert!(is_epub_path(Path::new("project/docs/guide.EPUB")));
        assert!(!is_epub_path(Path::new("document.txt")));
        assert!(!is_epub_path(Path::new("epub")));
    }

    #[test]
    fn test_extract_epub_text() {
        let mut buf = Vec::new();
        {
            let mut zip = ZipWriter::new(std::io::Cursor::new(&mut buf));
            let options =
                SimpleFileOptions::default().compression_method(zip::CompressionMethod::Stored);

            zip.start_file("mimetype", options).unwrap();
            zip.write_all(b"application/epub+zip").unwrap();

            zip.start_file("META-INF/container.xml", options).unwrap();
            zip.write_all(
                br#"<?xml version="1.0"?>
<container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
  <rootfiles>
    <rootfile full-path="OEBPS/content.opf" media-type="application/oebps-package+xml"/>
  </rootfiles>
</container>"#,
            )
            .unwrap();

            zip.start_file("OEBPS/content.opf", options).unwrap();
            zip.write_all(
                br#"<?xml version="1.0" encoding="utf-8"?>
<package xmlns="http://www.idpf.org/2007/opf" unique-identifier="BookId" version="2.0">
  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/">
    <dc:title>Test Book</dc:title>
    <dc:creator>Test Author</dc:creator>
    <dc:language>en</dc:language>
  </metadata>
  <manifest>
    <item id="ncx" href="toc.ncx" media-type="application/x-dtbncx+xml"/>
    <item id="chapter1" href="chapter1.xhtml" media-type="application/xhtml+xml"/>
  </manifest>
  <spine toc="ncx">
    <itemref idref="chapter1"/>
  </spine>
</package>"#,
            )
            .unwrap();

            zip.start_file("OEBPS/toc.ncx", options).unwrap();
            zip.write_all(
                br#"<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE ncx PUBLIC "-//NISO//DTD ncx 2005-1//EN" "http://www.daisy.org/z3986/2005/ncx-2005-1.dtd">
<ncx xmlns="http://www.daisy.org/z3986/2005/ncx/" version="2005-1">
  <navMap>
    <navPoint id="navpoint-1" playOrder="1">
      <navLabel><text>First Chapter</text></navLabel>
      <content src="chapter1.xhtml"/>
    </navPoint>
  </navMap>
</ncx>"#,
            )
            .unwrap();

            zip.start_file("OEBPS/chapter1.xhtml", options).unwrap();
            zip.write_all(
                br#"<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head><title>Chapter 1</title></head>
<body>
  <h1>Chapter One</h1>
  <p>Hello, world! This is a test.</p>
</body>
</html>"#,
            )
            .unwrap();

            zip.finish().unwrap();
        }

        let result = extract_epub_text(&buf).unwrap();
        assert!(result.contains("# Test Book"));
        assert!(result.contains("Author: Test Author"));
        assert!(result.contains("Language: en"));
        assert!(result.contains("## First Chapter"));
        assert!(result.contains("Chapter One"));
        assert!(result.contains("Hello, world! This is a test."));
    }
}
