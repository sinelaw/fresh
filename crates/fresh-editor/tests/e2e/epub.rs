use crate::common::harness::EditorTestHarness;
use tempfile::TempDir;
use zip::write::SimpleFileOptions;
use zip::ZipWriter;
use std::io::Write;
use crossterm::event::{KeyCode, KeyModifiers};

fn create_mock_epub(temp_dir: &TempDir) -> std::path::PathBuf {
    let epub_path = temp_dir.path().join("test_book.epub");
    let file = std::fs::File::create(&epub_path).unwrap();
    let mut zip = ZipWriter::new(file);
    let options = SimpleFileOptions::default()
        .compression_method(zip::CompressionMethod::Stored);

    zip.start_file("mimetype", options).unwrap();
    zip.write_all(b"application/epub+zip").unwrap();

    zip.start_file("META-INF/container.xml", options).unwrap();
    zip.write_all(br#"<?xml version="1.0"?>
<container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
  <rootfiles>
    <rootfile full-path="OEBPS/content.opf" media-type="application/oebps-package+xml"/>
  </rootfiles>
</container>"#).unwrap();

    zip.start_file("OEBPS/content.opf", options).unwrap();
    zip.write_all(br#"<?xml version="1.0" encoding="utf-8"?>
<package xmlns="http://www.idpf.org/2007/opf" unique-identifier="BookId" version="2.0">
  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/">
    <dc:title>Test EPUB Title</dc:title>
    <dc:creator>Test EPUB Author</dc:creator>
    <dc:language>en</dc:language>
  </metadata>
  <manifest>
    <item id="ncx" href="toc.ncx" media-type="application/x-dtbncx+xml"/>
    <item id="chapter1" href="chapter1.xhtml" media-type="application/xhtml+xml"/>
  </manifest>
  <spine toc="ncx">
    <itemref idref="chapter1"/>
  </spine>
</package>"#).unwrap();

    zip.start_file("OEBPS/toc.ncx", options).unwrap();
    zip.write_all(br#"<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE ncx PUBLIC "-//NISO//DTD ncx 2005-1//EN" "http://www.daisy.org/z3986/2005/ncx-2005-1.dtd">
<ncx xmlns="http://www.daisy.org/z3986/2005/ncx/" version="2005-1">
  <navMap>
    <navPoint id="navpoint-1" playOrder="1">
      <navLabel><text>First Chapter Title</text></navLabel>
      <content src="chapter1.xhtml"/>
    </navPoint>
  </navMap>
</ncx>"#).unwrap();

    zip.start_file("OEBPS/chapter1.xhtml", options).unwrap();
    zip.write_all(br#"<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head><title>Chapter 1</title></head>
<body>
  <h1>Chapter One Content</h1>
  <p>This is paragraph text in the EPUB chapter.</p>
</body>
</html>"#).unwrap();

    zip.finish().unwrap();
    epub_path
}

#[test]
fn test_epub_file_opens_readonly_markdown() {
    let temp_dir = TempDir::new().unwrap();
    let epub_path = create_mock_epub(&temp_dir);

    let mut harness = EditorTestHarness::new(120, 24).unwrap();
    harness.open_file(&epub_path).unwrap();
    harness.render().unwrap();

    // Verify the file is loaded and editing is disabled
    assert!(
        harness.editor().active_window().is_editing_disabled(),
        "EPUB file should have editing disabled"
    );

    // Verify the metadata title, creator, language and body are on screen
    harness.assert_screen_contains("Test EPUB Title");
    harness.assert_screen_contains("Test EPUB Author");
    harness.assert_screen_contains("First Chapter Title");
    harness.assert_screen_contains("Chapter One Content");
    harness.assert_screen_contains("This is paragraph text in the EPUB chapter.");

    // Attempt to type text - should be blocked/ignored
    let initial_len = harness.buffer_len();
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::NONE)
        .unwrap();

    // Buffer length should not change
    assert_eq!(
        harness.buffer_len(),
        initial_len,
        "Typing should be blocked/ignored in EPUB files"
    );
}
