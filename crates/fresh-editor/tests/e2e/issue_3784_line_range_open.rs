//! `fresh file:10-25` selects lines 10 to 25 inclusive (#3784).

use crate::common::harness::EditorTestHarness;

fn open_with_range(
    content: &str,
    line: usize,
    column: Option<usize>,
    end_line: usize,
    end_column: Option<usize>,
) -> anyhow::Result<(tempfile::TempDir, EditorTestHarness)> {
    let temp = tempfile::TempDir::new()?;
    let file = temp.path().join("lines.txt");
    std::fs::write(&file, content)?;
    let mut harness = EditorTestHarness::with_temp_project(80, 24)?;
    harness.editor_mut().queue_file_open(
        file,
        Some(line),
        column,
        Some(end_line),
        end_column,
        None,
        None,
    );
    harness.editor_mut().process_pending_file_opens();
    harness.render()?;
    Ok((temp, harness))
}

fn numbered_lines(count: usize) -> String {
    (1..=count).map(|n| format!("line {n}\n")).collect()
}

#[test]
fn line_range_includes_the_last_line() -> anyhow::Result<()> {
    let (_temp, mut harness) = open_with_range(&numbered_lines(30), 10, None, 25, None)?;
    let expected: Vec<String> = (10..=25).map(|n| format!("line {n}")).collect();
    assert_eq!(harness.get_selected_text(), expected.join("\n"));
    Ok(())
}

#[test]
fn line_range_to_last_line_without_newline() -> anyhow::Result<()> {
    let (_temp, mut harness) = open_with_range("a\r\nb\r\nc", 2, None, 3, None)?;
    assert_eq!(harness.get_selected_text(), "b\r\nc");
    Ok(())
}

#[test]
fn line_range_with_crlf_stops_before_the_line_break() -> anyhow::Result<()> {
    let (_temp, mut harness) = open_with_range("a\r\nb\r\nc\r\n", 1, None, 2, None)?;
    assert_eq!(harness.get_selected_text(), "a\r\nb");
    Ok(())
}

#[test]
fn explicit_end_column_is_kept() -> anyhow::Result<()> {
    let (_temp, mut harness) = open_with_range(&numbered_lines(30), 10, Some(3), 12, Some(1))?;
    assert_eq!(harness.get_selected_text(), "ne 10\nline 11\n");
    Ok(())
}
