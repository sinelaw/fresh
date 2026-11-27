//! The `hl_*` calls through their C ABI, as the web client makes them.

use super::*;

const TINY: &str = "\
%YAML 1.2
---
name: Tiny
file_extensions: [tiny]
scope: source.tiny
contexts:
  main:
    - match: '\\bkw\\b'
      scope: keyword.control.tiny
";

fn defaults() -> Vec<u8> {
    fresh_highlight::dump_syntax_set(&SyntaxSet::load_defaults_newlines()).unwrap()
}

/// A word, as the list encodings write it.
fn encoded_word(word: u32) -> Vec<u8> {
    word.to_le_bytes().to_vec()
}

/// A text, as the list encodings write it: `[len u32][utf8]`.
fn encoded_text(text: &str) -> Vec<u8> {
    [encoded_word(word(text.len())), text.as_bytes().to_vec()].concat()
}

/// `[len u32][bytes]` at `ptr`.
///
/// # Safety
/// `ptr` points to such a buffer.
unsafe fn framed_bytes(ptr: *const u8) -> Vec<u8> {
    // SAFETY: the caller's contract.
    unsafe {
        let len = u32::from_le_bytes(*ptr.cast::<[u8; 4]>()) as usize;
        std::slice::from_raw_parts(ptr.add(4), len).to_vec()
    }
}

fn error() -> String {
    // SAFETY: hl_error answers a framed buffer.
    String::from_utf8(unsafe { framed_bytes(hl_error()) }).unwrap()
}

/// The words of an answer: `[n]` then `n` words.
///
/// # Safety
/// `ptr` is an answer's.
unsafe fn answer_words(ptr: *const u32) -> Vec<u32> {
    // SAFETY: the caller's contract.
    unsafe { std::slice::from_raw_parts(ptr.add(1), *ptr as usize).to_vec() }
}

fn init(packdump: &[u8]) {
    // SAFETY: the pointer and length are the slice's.
    assert_eq!(
        unsafe { hl_init(packdump.as_ptr(), word(packdump.len())) },
        OK,
        "{}",
        error()
    );
}

fn build(grammars: &[(&str, &str)]) -> *const u8 {
    let packdump = defaults();
    let sources: Vec<u8> = std::iter::once(encoded_word(word(grammars.len())))
        .chain(
            grammars
                .iter()
                .flat_map(|(name, source)| [encoded_text(name), encoded_text(source)]),
        )
        .flatten()
        .collect();
    // SAFETY: the pointers and lengths are the slices'.
    unsafe {
        hl_build(
            packdump.as_ptr(),
            word(packdump.len()),
            sources.as_ptr(),
            word(sources.len()),
        )
    }
}

/// Work until done, then take document `doc`'s lines as `(line, flags,
/// runs as (start, len, category, background))`.
fn take(doc: u32) -> Vec<(u32, u32, Vec<[u32; 4]>)> {
    while hl_work(1 << 16) == WORK_MORE {}
    let ptr = hl_take(doc);
    assert!(!ptr.is_null(), "{}", error());
    // SAFETY: hl_take answers `[total_words, …]`.
    let words = unsafe { std::slice::from_raw_parts(ptr, *ptr as usize) };
    let mut at = 2;
    (0..words[1])
        .map(|_| {
            let (line, flags, n) = (words[at], words[at + 1], words[at + 2] as usize);
            let runs = words[at + 3..at + 3 + 4 * n]
                .chunks_exact(4)
                .map(|run| [run[0], run[1], run[2], run[3]])
                .collect();
            at += 3 + 4 * n;
            (line, flags, runs)
        })
        .collect()
}

fn open(doc: u32, syntax: &str, text: &str) {
    // SAFETY: the pointers and lengths are the strings'.
    let opened = unsafe {
        hl_open(
            doc,
            syntax.as_ptr(),
            word(syntax.len()),
            text.as_ptr(),
            word(text.len()),
        )
    };
    assert_eq!(opened, OK, "{}", error());
}

#[test]
fn a_document_in_a_grammar_hl_build_added_has_its_categories() {
    let built = build(&[("tiny", TINY)]);
    assert!(!built.is_null(), "{}", error());
    // SAFETY: hl_build answers a framed buffer.
    init(&unsafe { framed_bytes(built) });
    open(1, "Tiny", "x kw y");
    let keyword = u32::from(Category::Keyword.index());
    assert_eq!(
        take(1),
        [(0, LINE_EXACT | LINE_ROLES_KNOWN, vec![[2, 2, keyword, 0]])]
    );
}

#[test]
fn hl_build_names_every_grammar_that_fails_to_parse() {
    assert!(build(&[("tiny", TINY), ("broken", "contexts: [x")]).is_null());
    let message = error();
    assert!(message.contains("\"broken\""), "{message}");
    assert!(!message.contains("\"tiny\""), "{message}");
}

#[test]
fn hl_init_again_closes_every_document() {
    init(&defaults());
    open(1, "Rust", "fn main() {}");
    init(&defaults());
    assert!(hl_take(1).is_null());
    assert!(error().contains("no document 1"));
    // The id is free again.
    open(1, "Rust", "fn main() {}");
}

#[test]
fn hl_take_flags_a_lines_roles() {
    init(&defaults());
    open(1, "Markdown", "| K |\n|---|\n| a |\n\n```\nx\n```\n");
    let flags: Vec<u32> = take(1).into_iter().map(|(_, flags, _)| flags).collect();
    let known = LINE_EXACT | LINE_ROLES_KNOWN;
    assert_eq!(
        flags,
        [
            known | LINE_TABLE_HEADER,
            known | LINE_TABLE_DELIMITER,
            known | LINE_TABLE_ROW | LINE_TABLE_FIRST_ROW | LINE_TABLE_LAST,
            known,
            known | LINE_REGION_OPEN,
            known | LINE_REGION_BODY,
            known | LINE_REGION_CLOSE,
            known,
        ]
    );
}

#[test]
fn hl_regions_answers_what_became_of_each_region() {
    init(&defaults());
    let text = "-x = 1\n+y = \"s\"\n  note\n";
    // SAFETY: the pointer and length are the string's.
    assert_eq!(
        unsafe { hl_open_declared(1, text.as_ptr(), word(text.len())) },
        OK
    );
    // (start, end, prefix, streams, language)
    let regions: [(u32, u32, u32, &[u32], &str); 4] = [
        (0, 7, 1, &[0], "py"),
        (7, 16, 1, &[1], "src/lib.py"),
        (3, 9, 1, &[], "py"),
        (16, 23, 0, &[], "no-such-language"),
    ];
    let encoded: Vec<u8> = std::iter::once(encoded_word(word(regions.len())))
        .chain(
            regions
                .iter()
                .flat_map(|(start, end, prefix, streams, language)| {
                    [
                        encoded_word(*start),
                        encoded_word(*end),
                        encoded_word(*prefix),
                        encoded_word(word(streams.len())),
                        streams.iter().flat_map(|s| encoded_word(*s)).collect(),
                        encoded_text(language),
                    ]
                }),
        )
        .flatten()
        .collect();
    // SAFETY: the pointer and length are the vector's; the answer is read
    // before the next call.
    let statuses = unsafe {
        let answer = hl_regions(1, encoded.as_ptr(), word(encoded.len()));
        assert!(!answer.is_null(), "{}", error());
        answer_words(answer)
    };
    assert_eq!(
        statuses,
        [
            REGION_KEPT,
            REGION_KEPT,
            REGION_OVERLAPS,
            REGION_UNKNOWN_LANGUAGE
        ]
    );
    let lines = take(1);
    let number = u32::from(Category::Number.index());
    let string = u32::from(Category::String.index());
    assert!(lines[0].2.iter().any(|run| run[2] == number && run[0] == 5));
    assert!(lines[1].2.iter().any(|run| run[2] == string));
    assert!(lines[2].2.is_empty());

    // A malformed list is refused, and the regions stay.
    // SAFETY: as above.
    assert!(unsafe { hl_regions(1, encoded.as_ptr(), word(encoded.len() - 1)) }.is_null());
    assert!(error().contains("the region list"), "{}", error());
}
