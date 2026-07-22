//! Behavior tests for [`InputParser`].
//!
//! These assert only on the events produced by the public [`InputParser::parse`]
//! API — never on internal parser state — so they stay valid across parser
//! refactors.

use super::*;

/// Collect only the `Key` events from a parse, for terse assertions.
fn keys(events: &[Event]) -> Vec<(KeyCode, KeyModifiers)> {
    events
        .iter()
        .filter_map(|e| match e {
            Event::Key(ke) => Some((ke.code, ke.modifiers)),
            _ => None,
        })
        .collect()
}

/// True if any event is a `Key(Char(_))` — used to prove mouse bytes never
/// leak into the child as literal characters.
fn has_char_key(events: &[Event]) -> bool {
    events
        .iter()
        .any(|e| matches!(e, Event::Key(ke) if matches!(ke.code, KeyCode::Char(_))))
}

// ---- Basic characters and control keys ----

#[test]
fn simple_characters() {
    let mut p = InputParser::new();
    let ev = p.parse(b"abc");
    assert_eq!(
        keys(&ev),
        vec![
            (KeyCode::Char('a'), KeyModifiers::empty()),
            (KeyCode::Char('b'), KeyModifiers::empty()),
            (KeyCode::Char('c'), KeyModifiers::empty()),
        ]
    );
}

#[test]
fn control_characters_have_ctrl_modifier() {
    let mut p = InputParser::new();
    let ev = p.parse(&[0x03]); // Ctrl+C
    assert_eq!(keys(&ev), vec![(KeyCode::Char('c'), KeyModifiers::CONTROL)]);
}

#[test]
fn enter_key_cr_and_lf() {
    let mut p = InputParser::new();
    assert_eq!(
        keys(&p.parse(&[0x0D])),
        vec![(KeyCode::Enter, KeyModifiers::empty())]
    );
    assert_eq!(
        keys(&p.parse(&[0x0A])),
        vec![(KeyCode::Enter, KeyModifiers::empty())]
    );
}

#[test]
fn tab_key() {
    let mut p = InputParser::new();
    assert_eq!(
        keys(&p.parse(&[0x09])),
        vec![(KeyCode::Tab, KeyModifiers::empty())]
    );
}

// ---- Escape sequences ----

#[test]
fn esc_buffers_until_complete() {
    let mut p = InputParser::new();
    assert!(p.parse(&[0x1b]).is_empty());
    assert!(p.parse(b"[").is_empty());
    assert_eq!(
        keys(&p.parse(b"A")),
        vec![(KeyCode::Up, KeyModifiers::empty())]
    );
}

#[test]
fn csi_arrow_keys() {
    let mut p = InputParser::new();
    assert_eq!(
        keys(&p.parse(b"\x1b[A")),
        vec![(KeyCode::Up, KeyModifiers::empty())]
    );
}

#[test]
fn ss3_function_keys() {
    let mut p = InputParser::new();
    assert_eq!(
        keys(&p.parse(b"\x1bOP")),
        vec![(KeyCode::F(1), KeyModifiers::empty())]
    );
}

#[test]
fn alt_key_via_esc_prefix() {
    let mut p = InputParser::new();
    assert_eq!(
        keys(&p.parse(b"\x1ba")),
        vec![(KeyCode::Char('a'), KeyModifiers::ALT)]
    );
}

#[test]
fn csi_modifiers_parsed() {
    let mut p = InputParser::new();
    // Shift+Up: ESC [ 1 ; 2 A
    assert_eq!(
        keys(&p.parse(b"\x1b[1;2A")),
        vec![(KeyCode::Up, KeyModifiers::SHIFT)]
    );
}

#[test]
fn modified_f1_f4_csi_form() {
    // Modified F1–F4 use xterm's SS3-derived CSI form `CSI 1 ; <mod> {P,Q,R,S}`.
    // (#699: Shift+F3 was dropped, so "Find Previous" never fired.) Sequences
    // below were captured from tmux via `cat -v`.
    let mut p = InputParser::new();
    // Shift+F3
    assert_eq!(
        keys(&p.parse(b"\x1b[1;2R")),
        vec![(KeyCode::F(3), KeyModifiers::SHIFT)]
    );
    // Ctrl+F3
    assert_eq!(
        keys(&p.parse(b"\x1b[1;5R")),
        vec![(KeyCode::F(3), KeyModifiers::CONTROL)]
    );
    // Alt+F3
    assert_eq!(
        keys(&p.parse(b"\x1b[1;3R")),
        vec![(KeyCode::F(3), KeyModifiers::ALT)]
    );
    // Ctrl+Shift+F3
    assert_eq!(
        keys(&p.parse(b"\x1b[1;6R")),
        vec![(KeyCode::F(3), KeyModifiers::SHIFT | KeyModifiers::CONTROL)]
    );
    // Shift+F1 / F2 / F4 (P/Q/S siblings)
    assert_eq!(
        keys(&p.parse(b"\x1b[1;2P")),
        vec![(KeyCode::F(1), KeyModifiers::SHIFT)]
    );
    assert_eq!(
        keys(&p.parse(b"\x1b[1;2Q")),
        vec![(KeyCode::F(2), KeyModifiers::SHIFT)]
    );
    assert_eq!(
        keys(&p.parse(b"\x1b[1;2S")),
        vec![(KeyCode::F(4), KeyModifiers::SHIFT)]
    );
}

#[test]
fn unmodified_f1_f4_still_ss3() {
    // Regression guard: unmodified F1–F4 must keep decoding via SS3
    // (`ESC O P/Q/R/S`) and not be affected by the new CSI arms.
    let mut p = InputParser::new();
    assert_eq!(
        keys(&p.parse(b"\x1bOP")),
        vec![(KeyCode::F(1), KeyModifiers::empty())]
    );
    assert_eq!(
        keys(&p.parse(b"\x1bOR")),
        vec![(KeyCode::F(3), KeyModifiers::empty())]
    );
}

#[test]
fn bare_csi_r_is_not_f3() {
    // A bare `CSI R` (and a `1;1R` / non-`1` first field) is *not* a modified
    // F3 — it must not be misdecoded as a keypress. This is the Cursor
    // Position Report shape; guarding on `1;<mod≥2>` keeps it out.
    let mut p = InputParser::new();
    assert!(keys(&p.parse(b"\x1b[R")).is_empty());
    assert!(keys(&p.parse(b"\x1b[1;1R")).is_empty());
    // A realistic CPR body `CSI <row>;<col> R` (row != 1) also stays out.
    assert!(keys(&p.parse(b"\x1b[24;80R")).is_empty());
}

#[test]
fn shift_tab_csi_z() {
    let mut p = InputParser::new();
    assert_eq!(
        keys(&p.parse(b"\x1b[Z")),
        vec![(KeyCode::BackTab, KeyModifiers::SHIFT)]
    );
}

#[test]
fn focus_events() {
    let mut p = InputParser::new();
    assert!(matches!(p.parse(b"\x1b[I")[0], Event::FocusGained));
    assert!(matches!(p.parse(b"\x1b[O")[0], Event::FocusLost));
}

#[test]
fn tilde_sequences_function_and_editing_keys() {
    let mut p = InputParser::new();
    assert_eq!(
        keys(&p.parse(b"\x1b[3~")),
        vec![(KeyCode::Delete, KeyModifiers::empty())]
    );
    assert_eq!(
        keys(&p.parse(b"\x1b[5~")),
        vec![(KeyCode::PageUp, KeyModifiers::empty())]
    );
    assert_eq!(
        keys(&p.parse(b"\x1b[15~")),
        vec![(KeyCode::F(5), KeyModifiers::empty())]
    );
    assert_eq!(
        keys(&p.parse(b"\x1b[24~")),
        vec![(KeyCode::F(12), KeyModifiers::empty())]
    );
}

#[test]
fn csi_u_kitty_protocol() {
    let mut p = InputParser::new();
    // 'a' with no modifiers: CSI 97 u
    assert_eq!(
        keys(&p.parse(b"\x1b[97u")),
        vec![(KeyCode::Char('a'), KeyModifiers::empty())]
    );
    // Ctrl+Enter: CSI 13 ; 5 u
    assert_eq!(
        keys(&p.parse(b"\x1b[13;5u")),
        vec![(KeyCode::Enter, KeyModifiers::CONTROL)]
    );
}

#[test]
fn csi_u_alternate_keys_subparameters() {
    // kitty REPORT_ALTERNATE_KEYS encodes the key as `unicode:shifted:base`.
    // We must key off the primary (base) codepoint, not choke on the ':'.
    let mut p = InputParser::new();
    // Shift+'a' reported as base 97, shifted 65: CSI 97:65 ; 2 u
    assert_eq!(
        keys(&p.parse(b"\x1b[97:65;2u")),
        vec![(KeyCode::Char('a'), KeyModifiers::SHIFT)]
    );
    // A modifier field may carry an event-type sub-param: CSI 13 ; 5:1 u
    assert_eq!(
        keys(&p.parse(b"\x1b[13;5:1u")),
        vec![(KeyCode::Enter, KeyModifiers::CONTROL)]
    );
}

#[test]
fn modify_other_keys_mode_2() {
    let mut p = InputParser::new();
    // CSI 27 ; 5 ; 97 ~  => Ctrl + 'a'
    assert_eq!(
        keys(&p.parse(b"\x1b[27;5;97~")),
        vec![(KeyCode::Char('a'), KeyModifiers::CONTROL)]
    );
}

#[test]
fn mixed_input_preserves_order() {
    let mut p = InputParser::new();
    let ev = p.parse(b"a\x1b[Ab");
    assert_eq!(
        keys(&ev),
        vec![
            (KeyCode::Char('a'), KeyModifiers::empty()),
            (KeyCode::Up, KeyModifiers::empty()),
            (KeyCode::Char('b'), KeyModifiers::empty()),
        ]
    );
}

// ---- Standalone-ESC disambiguation (issue #1089 regressions) ----

#[test]
fn esc_then_esc_emits_standalone_esc() {
    let mut p = InputParser::new();
    assert!(p.parse(&[0x1b]).is_empty());
    let ev = p.parse(&[0x1b]);
    assert_eq!(keys(&ev), vec![(KeyCode::Esc, KeyModifiers::empty())]);
    // The second ESC stayed buffered: a following CSI now completes cleanly.
    assert_eq!(
        keys(&p.parse(b"[A")),
        vec![(KeyCode::Up, KeyModifiers::empty())]
    );
}

#[test]
fn esc_then_printable_emits_alt_key() {
    let mut p = InputParser::new();
    assert!(p.parse(&[0x1b]).is_empty());
    assert_eq!(
        keys(&p.parse(b"a")),
        vec![(KeyCode::Char('a'), KeyModifiers::ALT)]
    );
}

#[test]
fn esc_waits_for_next_byte() {
    let mut p = InputParser::new();
    assert!(p.parse(&[0x1b]).is_empty());
    // Empty input is a no-op and must not flush the buffered ESC.
    assert!(p.parse(b"").is_empty());
    assert_eq!(
        keys(&p.parse(b"[A")),
        vec![(KeyCode::Up, KeyModifiers::empty())]
    );
}

#[test]
fn flush_emits_standalone_esc() {
    let mut p = InputParser::new();
    assert!(p.parse(&[0x1b]).is_empty());
    assert!(p.escape_pending());
    assert_eq!(
        keys(&p.flush()),
        vec![(KeyCode::Esc, KeyModifiers::empty())]
    );
    // Flushed once: the ESC is gone, and a following key stands alone.
    assert!(!p.escape_pending());
    assert!(p.flush().is_empty());
    assert_eq!(
        keys(&p.parse(b"a")),
        vec![(KeyCode::Char('a'), KeyModifiers::empty())]
    );
}

#[test]
fn flush_never_breaks_up_a_partial_sequence() {
    // Mid-CSI and mid-UTF-8 bytes must keep waiting, never surface as keys.
    for partial in [&b"\x1b[<35;67"[..], &b"\x1b[1;5"[..], &[0xe2, 0x82][..]] {
        let mut p = InputParser::new();
        p.parse(partial);
        assert!(!p.escape_pending(), "{partial:02x?} is not a lone ESC");
        assert!(p.flush().is_empty(), "flush emitted from {partial:02x?}");
    }
}

#[test]
fn esc_then_mouse_same_chunk() {
    let mut p = InputParser::new();
    let ev = p.parse(b"\x1b\x1b[<35;67;18M");
    assert_eq!(ev.len(), 2, "expected Esc + mouse, got {:?}", ev);
    assert!(matches!(ev[0], Event::Key(ke) if ke.code == KeyCode::Esc));
    match &ev[1] {
        Event::Mouse(me) => {
            assert!(matches!(me.kind, MouseEventKind::Moved));
            assert_eq!((me.column, me.row), (66, 17));
        }
        other => panic!("expected mouse, got {:?}", other),
    }
}

#[test]
fn esc_then_mouse_separate_chunks() {
    let mut p = InputParser::new();
    assert!(p.parse(&[0x1b]).is_empty());
    let ev = p.parse(b"\x1b[<35;67;18M");
    assert!(matches!(ev[0], Event::Key(ke) if ke.code == KeyCode::Esc));
    assert!(matches!(ev[1], Event::Mouse(_)));
}

#[test]
fn esc_then_csi_arrow_separate_chunks() {
    let mut p = InputParser::new();
    assert!(p.parse(&[0x1b]).is_empty());
    let ev = p.parse(b"\x1b[A");
    assert_eq!(
        keys(&ev),
        vec![
            (KeyCode::Esc, KeyModifiers::empty()),
            (KeyCode::Up, KeyModifiers::empty()),
        ]
    );
}

#[test]
fn esc_then_mouse_click() {
    let mut p = InputParser::new();
    let ev = p.parse(b"\x1b\x1b[<0;10;5M");
    assert!(matches!(ev[0], Event::Key(ke) if ke.code == KeyCode::Esc));
    assert!(
        matches!(ev[1], Event::Mouse(me) if matches!(me.kind, MouseEventKind::Down(MouseButton::Left)))
    );
}

// ---- SGR mouse ----

#[test]
fn sgr_mouse_is_zero_indexed() {
    let mut p = InputParser::new();
    let ev = p.parse(b"\x1b[<0;10;5M");
    match &ev[0] {
        Event::Mouse(me) => assert_eq!((me.column, me.row), (9, 4)),
        other => panic!("expected mouse, got {:?}", other),
    }
}

#[test]
fn sgr_mouse_motion_without_button() {
    let mut p = InputParser::new();
    let ev = p.parse(b"\x1b[<35;10;5M");
    assert!(matches!(ev[0], Event::Mouse(me) if matches!(me.kind, MouseEventKind::Moved)));
}

#[test]
fn sgr_mouse_split_across_batches() {
    let mut p = InputParser::new();
    assert!(p.parse(&[0x1b]).is_empty());
    let ev = p.parse(b"[<35;42;5M");
    assert!(matches!(ev[0], Event::Mouse(_)));
}

#[test]
fn partial_csi_mouse_not_flushed() {
    let mut p = InputParser::new();
    assert!(p.parse(b"\x1b[<35;").is_empty());
    let ev = p.parse(b"42;5M");
    match &ev[0] {
        Event::Mouse(me) => assert_eq!((me.column, me.row), (41, 4)),
        other => panic!("expected mouse, got {:?}", other),
    }
}

// ---- X10 mouse ----

#[test]
fn x10_mouse_press() {
    let mut p = InputParser::new();
    // ESC [ M  btn=0(+32=' ')  x=41(+32=')')  y=21(+32='5')  => col 9, row 21? decode:
    // cx = 0x29 - 32 = 9, cy = 0x35 - 32 = 21
    let ev = p.parse(b"\x1b[M \x29\x35");
    match &ev[0] {
        Event::Mouse(me) => {
            assert!(matches!(me.kind, MouseEventKind::Down(MouseButton::Left)));
            assert_eq!((me.column, me.row), (9, 21));
        }
        other => panic!("expected x10 mouse, got {:?}", other),
    }
    assert!(!has_char_key(&ev));
}

#[test]
fn x10_mouse_split_across_batches_no_leak() {
    let mut p = InputParser::new();
    // Split the 6-byte X10 report at every boundary; none should leak.
    let seq = b"\x1b[M\x43\x48\x34"; // btn 'C', x 'H', y '4'
    for cut in 1..seq.len() {
        let mut p = InputParser::new();
        let mut ev = p.parse(&seq[..cut]);
        ev.extend(p.parse(&seq[cut..]));
        assert!(!has_char_key(&ev), "cut {cut} leaked: {:?}", ev);
        assert!(
            ev.iter().any(|e| matches!(e, Event::Mouse(_))),
            "cut {cut} produced no mouse event: {:?}",
            ev
        );
    }
    let _ = &mut p;
}

// ---- Issue #2745: mouse sequences must never leak as literal input ----

#[test]
fn truncated_x10_then_wellformed_sgr_does_not_leak() {
    // The heart of #2745: a truncated X10 report (`ESC[M` + two bytes) followed
    // by a *complete, well-formed* SGR report. The old parser swallowed the
    // SGR's ESC as the X10's third coordinate, desynced, and dumped the SGR
    // remainder as literal characters. The state machine must instead abandon
    // the truncated X10 and parse the SGR cleanly.
    let mut p = InputParser::new();
    let mut ev = p.parse(b"\x1b[Mxy"); // truncated X10 (only 2 coord bytes)
    ev.extend(p.parse(b"\x1b[<35;41;20M")); // clean SGR mouse
    assert!(!has_char_key(&ev), "leaked literal chars: {:?}", ev);
    // Exactly one clean mouse event survives (the SGR one); the truncated X10
    // is dropped.
    let mice: Vec<_> = ev.iter().filter(|e| matches!(e, Event::Mouse(_))).collect();
    assert_eq!(mice.len(), 1, "expected 1 mouse event, got {:?}", ev);
}

#[test]
fn esc_arriving_mid_x10_resyncs() {
    // While collecting X10 coordinates, an ESC (0x1b < 0x20) can't be a valid
    // coordinate — it must resync a fresh escape sequence, not be eaten.
    let mut p = InputParser::new();
    let ev = p.parse(b"\x1b[M\x40\x1b[A"); // one coord byte, then ESC + Up-arrow
    assert!(!has_char_key(&ev), "leaked: {:?}", ev);
    assert_eq!(keys(&ev), vec![(KeyCode::Up, KeyModifiers::empty())]);
}

/// True if any event is a Char key for a *structural* byte of a mouse/CSI
/// sequence (`[ ] < ; M m` or an ASCII digit). These are what leaked in #2745
/// when a sequence desynced. A stray high-Unicode data byte left over from an
/// out-of-spec, over-supplied coordinate run is a different (unpreventable,
/// harmless) matter and is not counted here.
fn has_structural_char_leak(events: &[Event]) -> bool {
    events.iter().any(|e| match e {
        Event::Key(ke) => matches!(
            ke.code,
            KeyCode::Char('[' | ']' | '<' | ';' | 'M' | 'm' | '0'..='9')
        ),
        _ => false,
    })
}

#[test]
fn out_of_spec_mouse_fragments_never_leak_structural_bytes() {
    // Mirrors the repro's out-of-spec injections. The parser must never surface
    // the structural bytes of a mouse sequence (`[`, `<`, `M`, `;`, digits) as
    // literal characters — that is the #2745 leak.
    let mut p = InputParser::new();
    let mut ev = Vec::new();
    ev.extend(p.parse(b"\x1b[M\x43\xc2\xa0\xc2\xa1")); // UTF-8-ish coords
    ev.extend(p.parse(b"\x1b[M\x80\xfa\xfa")); // unknown/high button
    ev.extend(p.parse(b"\x1b[<35;41M")); // malformed SGR (2 fields)
    ev.extend(p.parse(b"\x1b[<99999999999;1;1M")); // overflowing SGR field
    assert!(
        !has_structural_char_leak(&ev),
        "leaked structural chars: {:?}",
        ev
    );
}

#[test]
fn malformed_csi_is_dropped_not_printed() {
    let mut p = InputParser::new();
    // A CSI interrupted by a C0 control (here Ctrl+C, 0x03): the partial CSI
    // must be dropped, and the Ctrl+C must still register.
    let ev = p.parse(b"\x1b[<35;\x03");
    assert_eq!(keys(&ev), vec![(KeyCode::Char('c'), KeyModifiers::CONTROL)]);
}

#[test]
fn unknown_csi_final_is_dropped() {
    let mut p = InputParser::new();
    // Unknown final byte 'q' with params: drop silently, don't print.
    let ev = p.parse(b"\x1b[42q");
    assert!(ev.is_empty(), "expected drop, got {:?}", ev);
}

// ---- Bracketed paste ----

#[test]
fn bracketed_paste_simple() {
    let mut p = InputParser::new();
    let ev = p.parse(b"\x1b[200~Hello, world!\x1b[201~");
    assert!(matches!(&ev[0], Event::Paste(t) if t == "Hello, world!"));
}

#[test]
fn bracketed_paste_with_newlines() {
    let mut p = InputParser::new();
    let ev = p.parse(b"\x1b[200~line1\nline2\nline3\x1b[201~");
    assert!(matches!(&ev[0], Event::Paste(t) if t == "line1\nline2\nline3"));
}

#[test]
fn bracketed_paste_split_across_chunks() {
    let mut p = InputParser::new();
    assert!(p.parse(b"\x1b[200~Hello").is_empty());
    assert!(p.parse(b", world!").is_empty());
    let ev = p.parse(b"\x1b[201~");
    assert!(matches!(&ev[0], Event::Paste(t) if t == "Hello, world!"));
}

#[test]
fn bracketed_paste_followed_by_keypress() {
    let mut p = InputParser::new();
    let ev = p.parse(b"\x1b[200~pasted\x1b[201~a");
    assert!(matches!(&ev[0], Event::Paste(t) if t == "pasted"));
    assert!(matches!(ev[1], Event::Key(ke) if ke.code == KeyCode::Char('a')));
}

#[test]
fn bracketed_paste_empty() {
    let mut p = InputParser::new();
    let ev = p.parse(b"\x1b[200~\x1b[201~");
    assert!(matches!(&ev[0], Event::Paste(t) if t.is_empty()));
}

#[test]
fn bracketed_paste_with_escape_sequences_inside() {
    let mut p = InputParser::new();
    let ev = p.parse(b"\x1b[200~\x1b[31mred text\x1b[0m\x1b[201~");
    assert!(matches!(&ev[0], Event::Paste(t) if t == "\x1b[31mred text\x1b[0m"));
}

#[test]
fn keypress_then_bracketed_paste() {
    let mut p = InputParser::new();
    let ev = p.parse(b"x\x1b[200~pasted\x1b[201~");
    assert!(matches!(ev[0], Event::Key(ke) if ke.code == KeyCode::Char('x')));
    assert!(matches!(&ev[1], Event::Paste(t) if t == "pasted"));
}

// ---- UTF-8 ----

#[test]
fn utf8_two_byte_char() {
    let mut p = InputParser::new();
    let ev = p.parse(&[0xC3, 0xA9]); // é
    assert_eq!(keys(&ev), vec![(KeyCode::Char('é'), KeyModifiers::empty())]);
}

#[test]
fn utf8_three_byte_char() {
    let mut p = InputParser::new();
    let ev = p.parse(&[0xE4, 0xB8, 0xAD]); // 中
    assert_eq!(
        keys(&ev),
        vec![(KeyCode::Char('中'), KeyModifiers::empty())]
    );
}

#[test]
fn utf8_four_byte_emoji() {
    let mut p = InputParser::new();
    let ev = p.parse(&[0xF0, 0x9F, 0x98, 0x80]); // 😀
    assert_eq!(
        keys(&ev),
        vec![(KeyCode::Char('😀'), KeyModifiers::empty())]
    );
}

#[test]
fn utf8_incomplete_returns_no_events() {
    let mut p = InputParser::new();
    assert!(p.parse(&[0xE4]).is_empty());
}

#[test]
fn utf8_split_across_batches() {
    let mut p = InputParser::new();
    assert!(p.parse(&[0xE4]).is_empty());
    let ev = p.parse(&[0xB8, 0xAD]);
    assert_eq!(
        keys(&ev),
        vec![(KeyCode::Char('中'), KeyModifiers::empty())]
    );
}

#[test]
fn utf8_invalid_continuation_recovers_gracefully() {
    let mut p = InputParser::new();
    let ev = p.parse(&[0xE4, 0x00, 0xAD]);
    // Graceful recovery: no panic, some events produced.
    assert!(!ev.is_empty());
}

#[test]
fn overlong_encoding_start_bytes_rejected() {
    let mut p = InputParser::new();
    // 0xC0/0xC1 must not be treated as UTF-8 starts (RFC 3629). No panic.
    let _ = p.parse(&[0xC0, 0x80]);
    let _ = p.parse(&[0xC1, 0xA0]);
}

// ---- Exhaustive split-invariance for the repro's sequences ----

#[test]
fn every_split_of_repro_sequences_never_leaks_chars() {
    // For each well-formed mouse sequence, splitting it at every byte boundary
    // must yield the same single mouse event and never a literal Char key.
    let seqs: &[&[u8]] = &[
        b"\x1b[<35;67;18M",    // SGR motion
        b"\x1b[<0;10;5M",      // SGR press
        b"\x1b[<0;10;5m",      // SGR release
        b"\x1b[M\x43\x48\x34", // X10
    ];
    for seq in seqs {
        for cut in 0..=seq.len() {
            let mut p = InputParser::new();
            let mut ev = p.parse(&seq[..cut]);
            ev.extend(p.parse(&seq[cut..]));
            assert!(
                !has_char_key(&ev),
                "seq {:?} cut {cut} leaked: {:?}",
                seq,
                ev
            );
            let mice = ev.iter().filter(|e| matches!(e, Event::Mouse(_))).count();
            assert_eq!(
                mice, 1,
                "seq {:?} cut {cut} gave {mice} mouse events: {:?}",
                seq, ev
            );
        }
    }
}
