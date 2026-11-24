//! Operational transform over a linear byte sequence.
//!
//! Every document has one totally ordered edit log, sequenced by the server.
//! A client edits its own copy optimistically and sends its edits tagged with
//! the log version they were based on; the server transforms them past every
//! entry sequenced since, so the result is the same whatever the interleaving.
//! The client runs the identical rules (`web/js/ot.js`) on its side.
//!
//! Positions are byte offsets. An edit is a sequence of primitives, each
//! relative to the document after the ones before it.
//!
//! Ties are broken by order: of two concurrent inserts at one position, the
//! one sequenced first ends up first.

use serde::{Deserialize, Serialize};

/// One primitive edit.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "t", rename_all = "lowercase")]
pub enum Prim {
    /// Insert `text` at `pos`.
    Ins { pos: usize, text: String },
    /// Delete `len` bytes at `pos`.
    Del { pos: usize, len: usize },
}

impl Prim {
    fn is_noop(&self) -> bool {
        match self {
            Prim::Ins { text, .. } => text.is_empty(),
            Prim::Del { len, .. } => *len == 0,
        }
    }
}

/// Transform `a` so it applies after `b`, both having been made against the
/// same document. `a_first` says whether `a` is ordered before `b` (decides
/// ties between inserts at one position).
///
/// Returns a list because a delete that `b` inserted into the middle of is
/// split in two.
pub fn transform(a: &Prim, b: &Prim, a_first: bool) -> Vec<Prim> {
    use Prim::*;
    let out = match (a, b) {
        (Ins { pos: pa, text: ta }, Ins { pos: pb, text: tb }) => {
            if *pa < *pb || (*pa == *pb && a_first) {
                vec![a.clone()]
            } else {
                vec![Ins { pos: pa + tb.len(), text: ta.clone() }]
            }
        }
        (Ins { pos: pa, text: ta }, Del { pos: pb, len: lb }) => {
            let p = if *pa <= *pb {
                *pa
            } else if *pa >= pb + lb {
                pa - lb
            } else {
                // Inserted inside text `b` deleted: the insert survives, at
                // the deletion point.
                *pb
            };
            vec![Ins { pos: p, text: ta.clone() }]
        }
        (Del { pos: pa, len: la }, Ins { pos: pb, text: tb }) => {
            if *pb <= *pa {
                vec![Del { pos: pa + tb.len(), len: *la }]
            } else if *pb >= pa + la {
                vec![a.clone()]
            } else {
                // `b` inserted inside the range `a` deletes: delete around it.
                let before = pb - pa;
                vec![
                    Del { pos: *pa, len: before },
                    Del { pos: pa + tb.len(), len: la - before },
                ]
            }
        }
        (Del { pos: pa, len: la }, Del { pos: pb, len: lb }) => {
            let (a0, a1, b0, b1) = (*pa, pa + la, *pb, pb + lb);
            if a1 <= b0 {
                vec![a.clone()]
            } else if a0 >= b1 {
                vec![Del { pos: a0 - lb, len: *la }]
            } else {
                let overlap = a1.min(b1) - a0.max(b0);
                vec![Del { pos: a0.min(b0), len: la - overlap }]
            }
        }
    };
    out.into_iter().filter(|p| !p.is_noop()).collect()
}

/// Transform the sequence `a` against the sequence `b` (both made against the
/// same document). Returns `(a', b')`: `a'` applies after `b`, `b'` after `a`.
/// `a_first` orders all of `a` before all of `b`.
pub fn transform_seqs(a: &[Prim], b: &[Prim], a_first: bool) -> (Vec<Prim>, Vec<Prim>) {
    if a.is_empty() || b.is_empty() {
        return (a.to_vec(), b.to_vec());
    }
    if a.len() == 1 && b.len() == 1 {
        return (
            transform(&a[0], &b[0], a_first),
            transform(&b[0], &a[0], !a_first),
        );
    }
    if a.len() > 1 {
        // a = a1 then the rest: a1 goes past b, then the rest goes past what
        // b became after a1.
        let (a1, b1) = transform_seqs(&a[..1], b, a_first);
        let (rest, b2) = transform_seqs(&a[1..], &b1, a_first);
        return ([a1, rest].concat(), b2);
    }
    let (a1, b1) = transform_seqs(a, &b[..1], a_first);
    let (a2, rest) = transform_seqs(&a1, &b[1..], a_first);
    (a2, [b1, rest].concat())
}

/// Apply a primitive to a byte string.
pub fn apply(doc: &mut Vec<u8>, p: &Prim) {
    match p {
        Prim::Ins { pos, text } => {
            let at = (*pos).min(doc.len());
            doc.splice(at..at, text.bytes());
        }
        Prim::Del { pos, len } => {
            let s = (*pos).min(doc.len());
            let e = (pos + len).min(doc.len());
            doc.drain(s..e);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ins(pos: usize, t: &str) -> Prim {
        Prim::Ins { pos, text: t.into() }
    }
    fn del(pos: usize, len: usize) -> Prim {
        Prim::Del { pos, len }
    }
    fn run(doc: &str, ops: &[Prim]) -> String {
        let mut d = doc.as_bytes().to_vec();
        for o in ops {
            apply(&mut d, o);
        }
        String::from_utf8(d).unwrap()
    }

    /// Convergence: applying a then b' equals applying b then a'.
    fn converge(doc: &str, a: &[Prim], b: &[Prim]) -> String {
        let (a2, b2) = transform_seqs(a, b, false);
        let mut ab = a.to_vec();
        ab.extend(b2);
        let mut ba = b.to_vec();
        ba.extend(a2);
        let x = run(doc, &ab);
        let y = run(doc, &ba);
        assert_eq!(x, y, "a={a:?} b={b:?}");
        x
    }

    #[test]
    fn concurrent_inserts_order_by_sequence() {
        // b sequenced first, so its text comes first at a tie.
        assert_eq!(converge("ab", &[ins(1, "X")], &[ins(1, "Y")]), "aYXb");
    }

    #[test]
    fn insert_inside_a_deletion_survives() {
        assert_eq!(converge("abcdef", &[del(1, 4)], &[ins(3, "X")]), "aXf");
    }

    #[test]
    fn overlapping_deletes() {
        assert_eq!(converge("abcdef", &[del(1, 3)], &[del(2, 3)]), "af");
    }

    #[test]
    fn sequences_converge_exhaustively() {
        let doc = "0123456789";
        let mut prims = Vec::new();
        for p in 0..=10 {
            prims.push(ins(p, "X"));
            prims.push(ins(p, "YY"));
        }
        for p in 0..10 {
            for l in 1..=(10 - p).min(4) {
                prims.push(del(p, l));
            }
        }
        // Pairs of single primitives, and a two-step sequence against one.
        for a in &prims {
            for b in &prims {
                converge(doc, &[a.clone()], &[b.clone()]);
            }
        }
        for a in prims.iter().step_by(3) {
            for a2 in [ins(0, "Q"), del(0, 1), ins(5, "R")] {
                for b in prims.iter().step_by(2) {
                    converge(doc, &[a.clone(), a2.clone()], &[b.clone()]);
                    converge(doc, &[b.clone()], &[a.clone(), a2.clone()]);
                }
            }
        }
    }
}
