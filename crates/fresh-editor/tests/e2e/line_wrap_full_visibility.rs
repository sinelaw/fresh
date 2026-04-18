//! End-to-end sweep: with `line_wrap = true`, every character of every
//! logical line in the buffer must be rendered somewhere in the viewport,
//! at every terminal width, and both with and without the file-explorer
//! sidebar open.  Word-breaking behavior is allowed (wraps may leave
//! trailing whitespace on a visual row rather than splitting a word),
//! but no printable character from the source may go missing from the
//! render — that's the only criterion.
//!
//! The test is written property-style: a curated fixture covers many
//! "interesting" line shapes (words, trailing punctuation, matched and
//! nested parens/brackets, series of punctuation, series of parens, code-
//! like content, uniformly long tokens) and is then rendered at every
//! sampled width with the sidebar both closed and open.  At each
//! combination we read only the rendered screen (no editor internals) and
//! assert that every non-whitespace character of the fixture appears in
//! order across the visible content rows.
//!
//! Terminal height is fixed large enough that the narrowest sampled width
//! + sidebar can still fit every wrapped row — the invariant is strictly
//! about what's visible, not about scrolling.
//!
//! All widths share a single fixture written once to a tempfile; each
//! `(width, sidebar)` spins up its own harness so state doesn't leak.

use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

fn config_with_wrap() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config
}

/// A deliberately diverse set of single-line buffer contents.  Covers
/// words, many kinds of trailing punctuation, parens/brackets/braces in
/// matched and nested forms, series of each, code-like payloads, and
/// uniformly long tokens that force char-boundary wraps.
fn interesting_lines() -> Vec<String> {
    let mut lines: Vec<String> = Vec::new();

    // Plain words, short and medium.
    lines.push("alpha".into());
    lines.push("hello world".into());
    lines.push("two three four five six seven eight nine".into());

    // Trailing single punctuation.
    for ending in [
        '.', '!', '?', ',', ';', ':', '-', '~', '*', '+', '=', '/', '\\', '|',
    ] {
        lines.push(format!("line ending with a {ending}{ending}"));
    }

    // Trailing punctuation runs (2, 3, 5 repeats of each).
    for rep in [2usize, 3, 5] {
        for ch in ['.', '!', '?', ',', '-', '*', '='] {
            let run: String = std::iter::repeat(ch).take(rep).collect();
            lines.push(format!("run of {ch}s{run}"));
        }
    }

    // Parens / brackets / braces — various positions.
    lines.push("a line with (inline parens) in the middle".into());
    lines.push("closing paren at the very end)".into());
    lines.push("closing bracket at the very end]".into());
    lines.push("closing brace at the very end}".into());
    lines.push("(entirely wrapped in parens)".into());
    lines.push("((doubly nested))".into());
    lines.push("(((triple nested)))".into());
    lines.push("((((four deep))))".into());
    lines.push("{[((mixed brackets))]}".into());
    lines.push("{[({ and back )]}".into());

    // Series of parens and bracket pairs.
    lines.push("() () () () () () () ()".into());
    lines.push("(a) (b) (c) (d) (e) (f) (g)".into());
    lines.push("(1)(2)(3)(4)(5)(6)(7)(8)(9)".into());
    lines.push("[a][b][c][d][e][f][g][h][i]".into());
    lines.push("{a}{b}{c}{d}{e}{f}{g}{h}{i}".into());
    lines.push("()[]{}()[]{}()[]{}()[]{}".into());

    // Mixed content with punctuation, parens, and words interleaved.
    lines.push("word, word. word! (word) word? word;".into());
    lines.push("first (second), third; fourth: fifth.".into());
    lines.push("one (two, three); four (five: six).".into());
    lines.push("x=1; y=(a+b)*c; z={a:1, b:2}; done.".into());

    // Code-like snippets.
    lines.push("fn sum(x: i32, y: i32) -> i32 { (x + y) * 2 }".into());
    lines.push("let v = [(1, 2), (3, 4), (5, 6), (7, 8)];".into());
    lines.push("if (a && b) || (!c) { return Some((1, 2)); }".into());

    // Moderately long wrapping stressors (bounded to keep viewport
    // requirements modest across the width sweep).
    lines.push("a medium length line with (parens), commas, and a period at the end.".into());
    lines.push("another medium line; semicolons, colons: and some (groups) inside it.".into());

    // Long single tokens (force char-boundary wraps).
    lines.push("supercalifragilisticexpialidocious".into());
    lines.push("pneumonoultramicroscopicsilicovolcano".into());
    lines.push("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".into());
    lines.push("abcdefghijklmnopqrstuvwxyz0123456789".into());

    // Indented lines — exercise the hanging-indent wrap path.  Mirrors
    // the Kotlin-style code that originally surfaced the "too late"
    // wrap bug where characters straddling the wrap boundary get lost.
    for indent in [4usize, 8, 12, 16, 20] {
        let pad: String = std::iter::repeat(' ').take(indent).collect();
        lines.push(format!(
            "{pad}dialog.getButton(DialogInterface.BUTTON_NEUTRAL).setOnClickListener {{"
        ));
        lines.push(format!(
            "{pad}val folderChooserDialog = childFragmentManager.findFragmentByTag(\"FOLDER_CHOOSER\") as FolderChooserDialog?"
        ));
        lines.push(format!(
            "{pad}binding.recyclerView.layoutManager = LinearLayoutManager(requireContext())"
        ));
        lines.push(format!(
            "{pad}return MaterialAlertDialogBuilder(requireContext()).setTitle(titleRes).setView(binding.root)"
        ));
        lines.push(format!(
            "{pad}someObject.doSomething(with: a, and: b).thenChain(other).forEach {{ item -> item.process() }}"
        ));
        lines.push(format!(
            "{pad}aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        ));
        lines.push(format!(
            "{pad}token1 token2 token3 token4 token5 token6 token7 token8 token9 token10 token11"
        ));
    }

    // Edge-case short lines.
    lines.push("x".into());
    lines.push("a.".into());
    lines.push("a!".into());
    lines.push(")".into());
    lines.push("()".into());
    lines.push("...".into());
    lines.push("!!!".into());

    // Final marker — cheap visible anchor for diagnostics.
    lines.push("== END OF FIXTURE ==".into());

    lines
}

/// Check that every fixture line that is fully rendered in the viewport
/// has all its non-whitespace characters present in the corresponding
/// rendered rows, in order.  Rows are segmented by gutter line number:
/// a row with a number in its gutter starts a new source line; rows
/// with an empty gutter are wrap continuations of the previous one.
///
/// Whitespace is ignored on both sides because the renderer injects
/// hanging-indent spaces and may wrap across spaces; only printable
/// characters must be preserved across a wrap.
///
/// The check bails out at the FIRST fixture line whose rendered non-ws
/// payload disagrees with the source non-ws payload, reporting exactly
/// which line and what the divergence looks like.
fn verify_all_chars_rendered(
    harness: &EditorTestHarness,
    expected_lines: &[String],
    label: &str,
) -> Result<(), String> {
    let (first, last) = harness.content_area_rows();

    // Walk rows, grouping continuation rows with their first-visual-row.
    // Each entry: (line_number_1_indexed, concatenated_body_after_last_pipe).
    let mut groups: Vec<(usize, String)> = Vec::new();
    let mut last_group_had_number = false;
    for r in first..=last {
        let row = harness.get_screen_row(r);
        let Some(idx) = row.rfind('│') else {
            continue;
        };
        let gutter = &row[..idx];
        let body = &row[idx + '│'.len_utf8()..];

        // Digit run at the tail of the gutter (after skipping trailing
        // whitespace).  Read right-to-left, then reverse to restore order.
        let digits_rev: String = gutter
            .chars()
            .rev()
            .skip_while(|c| c.is_whitespace())
            .take_while(|c| c.is_ascii_digit())
            .collect();
        let ln_opt: Option<usize> = if digits_rev.is_empty() {
            None
        } else {
            digits_rev.chars().rev().collect::<String>().parse().ok()
        };

        if let Some(ln) = ln_opt {
            groups.push((ln, body.to_string()));
            last_group_had_number = true;
        } else if last_group_had_number {
            if let Some(entry) = groups.last_mut() {
                entry.1.push_str(body);
            }
        }
    }

    // Skip the final group if its source line is not fully visible — the
    // last wrap segment may be below the viewport.  We know a line is
    // complete when either another line follows it or the fixture has
    // that line as its last entry; we approximate by dropping the final
    // in-viewport group whenever it's not the last fixture line.
    let last_visible_ln_opt = groups.last().map(|g| g.0);

    for (ln, body) in &groups {
        let src_idx = ln.saturating_sub(1);
        let Some(src) = expected_lines.get(src_idx) else {
            continue;
        };

        let is_last_in_viewport = last_visible_ln_opt == Some(*ln);
        let is_last_in_fixture = src_idx + 1 == expected_lines.len();
        if is_last_in_viewport && !is_last_in_fixture {
            continue;
        }

        let src_nonws: String = src.chars().filter(|c| !c.is_whitespace()).collect();
        let rendered_nonws: String = body.chars().filter(|c| c.is_ascii_graphic()).collect();

        if src_nonws != rendered_nonws {
            let content: String = (first..=last)
                .map(|r| harness.get_screen_row(r))
                .collect::<Vec<_>>()
                .join("\n");
            let full_screen = harness.screen_to_string();
            return Err(format!(
                "{label}: fixture line {ln} (vec index {src_idx}) rendered body is \
                 missing or reorders characters from the source.\n\
                 source (non-ws, {slen} chars): {src_nonws:?}\n\
                 rendered (non-ws, {rlen} chars): {rendered_nonws:?}\n\
                 source line: {src:?}\n\
                 rendered body (all segments concatenated): {body:?}\n\
                 --- Rendered content area ({first}..={last}) ---\n{content}\n\
                 --- Full rendered screen ---\n{full_screen}",
                slen = src_nonws.len(),
                rlen = rendered_nonws.len(),
            ));
        }
    }

    Ok(())
}

/// One `(width, sidebar_open)` trial.  Returns Err diagnostic on
/// visibility failure, Ok on success.
fn run_trial(
    width: u16,
    height: u16,
    sidebar_open: bool,
    fixture_path: &std::path::Path,
    lines: &[String],
) -> Result<(), String> {
    let mut harness = EditorTestHarness::with_config(width, height, config_with_wrap())
        .map_err(|e| format!("w={width} sidebar={sidebar_open}: harness init: {e}"))?;
    harness
        .open_file(fixture_path)
        .map_err(|e| format!("w={width} sidebar={sidebar_open}: open_file: {e}"))?;
    harness
        .render()
        .map_err(|e| format!("w={width} sidebar={sidebar_open}: render: {e}"))?;

    // Jump to the very top so the first visual row is the fixture's
    // first logical line.  The fixture is short enough to fit in the
    // viewport at `height`, so nothing should scroll off.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .map_err(|e| format!("w={width} sidebar={sidebar_open}: Ctrl+Home: {e}"))?;
    harness
        .render()
        .map_err(|e| format!("w={width} sidebar={sidebar_open}: post-home render: {e}"))?;

    if sidebar_open {
        harness
            .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
            .map_err(|e| format!("w={width} sidebar={sidebar_open}: Ctrl+E: {e}"))?;
        // Let any async side-effects (explorer directory scan) settle,
        // then re-render.  The sidebar column itself is allocated
        // synchronously on toggle, so the visibility check doesn't need
        // to wait for the tree to populate — we only care about how the
        // content area re-wraps when its width shrinks.
        harness
            .process_async_and_render()
            .map_err(|e| format!("w={width} sidebar={sidebar_open}: post-toggle render: {e}"))?;
    }

    let label = format!("width={width}, sidebar={sidebar_open}");
    verify_all_chars_rendered(&harness, lines, &label)?;
    Ok(())
}

/// Small sanity-check variant that runs a single representative trial and
/// always dumps the rendered screen (via `println!` under `--no-capture`).
/// Use this when debugging: it's fast, it prints context even on success,
/// and its failure message mirrors the sweep's.
#[test]
fn test_line_wrap_visibility_single_width_debug_dump() {
    init_tracing_from_env();
    fresh::services::signal_handler::install_signal_handlers();

    let lines = interesting_lines();
    let fixture = TestFixture::new("line_wrap_visibility_dbg.txt", &lines.join("\n"))
        .expect("create fixture");

    let width: u16 = 60;
    let height: u16 = 120;

    for &sidebar_open in &[false, true] {
        let mut harness =
            EditorTestHarness::with_config(width, height, config_with_wrap()).expect("harness");
        harness.open_file(&fixture.path).expect("open_file");
        harness.render().expect("render");
        harness
            .send_key(KeyCode::Home, KeyModifiers::CONTROL)
            .expect("Ctrl+Home");
        harness.render().expect("post-home render");
        if sidebar_open {
            harness
                .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
                .expect("Ctrl+E");
            harness
                .process_async_and_render()
                .expect("post-toggle render");
        }
        let full = harness.screen_to_string();
        println!(
            "=== DEBUG DUMP width={width} height={height} sidebar={sidebar_open} ===\n{full}\n=== END DUMP ===\n",
        );
        let res = verify_all_chars_rendered(
            &harness,
            &lines,
            &format!("debug width={width} sidebar={sidebar_open}"),
        );
        if let Err(msg) = res {
            panic!("{msg}");
        }
    }
}

#[test]
fn test_line_wrap_all_chars_visible_across_widths_and_sidebar() {
    init_tracing_from_env();
    fresh::services::signal_handler::install_signal_handlers();

    let lines = interesting_lines();
    let fixture =
        TestFixture::new("line_wrap_visibility.txt", &lines.join("\n")).expect("create fixture");

    // Sweep every integer width in the range, so a bug whose period
    // aligns with any particular stride can't hide.  Minimum is chosen
    // so the content column is wide enough to render a few characters
    // even with the sidebar open (sidebar consumes ~20% of terminal
    // width).
    let widths: Vec<u16> = (40u16..=100).collect();
    // Height generous enough that every fixture line fits its wrapped
    // rows inside the content area at the narrowest sampled width with
    // the sidebar open.  The test checks visibility of wrapped content,
    // not scrolling, so nothing must fall off the bottom.
    let height: u16 = 200;

    let total = widths.len() * 2;
    let mut done = 0usize;
    let started = std::time::Instant::now();

    for &width in &widths {
        for &sidebar_open in &[false, true] {
            done += 1;
            tracing::info!(
                "line-wrap visibility sweep: {}/{} (width={width}, sidebar={sidebar_open}, elapsed={:?})",
                done,
                total,
                started.elapsed(),
            );
            if let Err(msg) = run_trial(width, height, sidebar_open, &fixture.path, &lines) {
                panic!(
                    "Line-wrap regression at trial {done}/{total} \
                     (elapsed={:?}):\n\n{msg}",
                    started.elapsed()
                );
            }
        }
    }

    tracing::info!(
        "line-wrap visibility sweep: done in {:?}, {} trials all passed",
        started.elapsed(),
        total,
    );
}
