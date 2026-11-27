//! The web client's marker lists: positions that follow its buffers' edits,
//! by the rules of `js/ot.js` `shiftPos(p, op, right)`, over
//! [`fresh_markers::marker::MarkerList`].
//!
//! A marker is a point or a span, each end with its own gravity: text
//! inserted exactly at a right-gravity end goes before it (the end moves),
//! at a left-gravity end after it (the end stays). A deletion moves a
//! position inside it to where it starts, whatever the gravity, and an end
//! never precedes its start. Every position is a byte offset.

use fresh_markers::marker::{MarkerId, MarkerList};

/// A marker where it is now.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Placed {
    pub id: u32,
    pub start: u32,
    pub end: u32,
}

/// One buffer's markers.
#[derive(Default)]
pub struct Markers {
    list: MarkerList,
}

fn offset(n: usize) -> Result<u32, String> {
    u32::try_from(n).map_err(|_| format!("position {n} is past 4 GiB"))
}

fn id_of(id: MarkerId) -> Result<u32, String> {
    u32::try_from(id.0).map_err(|_| "this list has made 2^32 markers".to_string())
}

impl Markers {
    /// A point at `pos`.
    pub fn point(&mut self, pos: u32, right: bool) -> Result<u32, String> {
        let pos = pos as usize;
        id_of(if right {
            self.list.create(pos)
        } else {
            self.list.create_left_gravity(pos)
        })
    }

    /// A span over `start..end`.
    pub fn span(
        &mut self,
        start: u32,
        end: u32,
        start_right: bool,
        end_right: bool,
    ) -> Result<u32, String> {
        if end < start {
            return Err(format!("span {start}..{end} ends before it starts"));
        }
        id_of(
            self.list
                .create_range(start as usize, end as usize, start_right, end_right),
        )
    }

    pub fn delete(&mut self, id: u32) -> Result<(), String> {
        let id = MarkerId(u64::from(id));
        self.list
            .get_position(id)
            .ok_or_else(|| format!("no marker {}", id.0))?;
        self.list.delete(id);
        Ok(())
    }

    /// `removed` bytes at `at` deleted, then `inserted` bytes inserted there.
    pub fn edit(&mut self, at: u32, removed: u32, inserted: u32) {
        self.list.adjust_for_delete(at as usize, removed as usize);
        self.list.adjust_for_insert(at as usize, inserted as usize);
    }

    pub fn get(&self, id: u32) -> Result<Placed, String> {
        let (start, end) = self
            .list
            .get_range(MarkerId(u64::from(id)))
            .ok_or_else(|| format!("no marker {id}"))?;
        Ok(Placed {
            id,
            start: offset(start)?,
            end: offset(end)?,
        })
    }

    /// The markers touching `start..end`, by start, then by creation: those
    /// starting before `end` and ending after `start`, an empty one counting
    /// as covering the byte at its position.
    pub fn query(&self, start: u32, end: u32) -> Result<Vec<Placed>, String> {
        if end < start {
            return Err(format!("query {start}..{end} ends before it starts"));
        }
        // The tree's query is closed at both ends: a superset to narrow. It
        // comes in key order, which is (start, id) here (no marker has an
        // `order`) except where a deletion clamped markers together, so the
        // stable sort below has little to do.
        let candidates = self
            .list
            .query_range(start as usize, end as usize)
            .into_iter()
            .map(|(id, s, e)| {
                Ok(Placed {
                    id: id_of(id)?,
                    start: offset(s)?,
                    end: offset(e)?,
                })
            })
            .collect::<Result<Vec<_>, String>>()?;
        let mut found: Vec<Placed> = candidates
            .into_iter()
            .filter(|m| m.start < end && m.end.max(m.start + 1) > start)
            .collect();
        found.sort_by_key(|m| (m.start, m.id));
        Ok(found)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    /// A position through one edit by `js/ot.js` `shiftPos`: `ins` bytes
    /// inserted at `at`, or `del` deleted there.
    fn shift_pos(p: u32, at: u32, del: u32, ins: u32, right: bool) -> u32 {
        if ins > 0 {
            if p > at || (p == at && right) {
                p + ins
            } else {
                p
            }
        } else if p >= at + del {
            p - del
        } else if p > at {
            at
        } else {
            p
        }
    }

    /// A marker as the client kept them before the tree: positions shifted
    /// one by one through every edit.
    #[derive(Debug, Clone)]
    struct Plain {
        id: u32,
        start: u32,
        end: u32,
        start_right: bool,
        end_right: bool,
    }

    impl Plain {
        fn shift(&mut self, at: u32, del: u32, ins: u32) {
            self.start = shift_pos(self.start, at, del, ins, self.start_right);
            self.end = shift_pos(self.end, at, del, ins, self.end_right).max(self.start);
        }
        fn placed(&self) -> Placed {
            Placed {
                id: self.id,
                start: self.start,
                end: self.end,
            }
        }
    }

    /// The kinds of marker the client makes (js/decor.js): a range (start
    /// right, end left), points of either gravity, and spans of any.
    #[derive(Debug, Clone)]
    enum Op {
        Range {
            start: u32,
            len: u32,
        },
        Point {
            pos: u32,
            right: bool,
        },
        Span {
            start: u32,
            len: u32,
            start_right: bool,
            end_right: bool,
        },
        Delete {
            pick: usize,
        },
        Insert {
            at: u32,
            len: u32,
        },
        Remove {
            at: u32,
            len: u32,
        },
        Query {
            start: u32,
            len: u32,
        },
    }

    fn op() -> impl Strategy<Value = Op> {
        prop_oneof![
            (0..300u32, 0..40u32).prop_map(|(start, len)| Op::Range { start, len }),
            (0..300u32, any::<bool>()).prop_map(|(pos, right)| Op::Point { pos, right }),
            (0..300u32, 0..40u32, any::<bool>(), any::<bool>()).prop_map(
                |(start, len, start_right, end_right)| Op::Span {
                    start,
                    len,
                    start_right,
                    end_right
                }
            ),
            any::<usize>().prop_map(|pick| Op::Delete { pick }),
            (0..300u32, 1..20u32).prop_map(|(at, len)| Op::Insert { at, len }),
            (0..300u32, 1..20u32).prop_map(|(at, len)| Op::Remove { at, len }),
            (0..300u32, 0..60u32).prop_map(|(start, len)| Op::Query { start, len }),
        ]
    }

    proptest! {
        /// Random edits move the markers exactly as shifting a plain list
        /// by the client's rules does, and a range query finds exactly what
        /// a scan of that list finds, in the same order.
        #[test]
        fn markers_follow_edits_like_a_plain_list(ops in prop::collection::vec(op(), 1..200)) {
            fn add(markers: &mut Markers, plain: &mut Vec<Plain>, start: u32, end: u32, start_right: bool, end_right: bool) {
                let id = if start == end && start_right == end_right {
                    markers.point(start, start_right)
                } else {
                    markers.span(start, end, start_right, end_right)
                };
                plain.push(Plain { id: id.unwrap(), start, end, start_right, end_right });
            }
            let mut markers = Markers::default();
            let mut plain: Vec<Plain> = Vec::new();
            for op in ops {
                match op {
                    Op::Range { start, len } => add(&mut markers, &mut plain, start, start + len, true, false),
                    Op::Point { pos, right } => add(&mut markers, &mut plain, pos, pos, right, right),
                    Op::Span { start, len, start_right, end_right } => add(&mut markers, &mut plain, start, start + len, start_right, end_right),
                    Op::Delete { pick } => {
                        if !plain.is_empty() {
                            let gone = plain.swap_remove(pick % plain.len());
                            markers.delete(gone.id).unwrap();
                            prop_assert!(markers.get(gone.id).is_err());
                        }
                    }
                    Op::Insert { at, len } => {
                        markers.edit(at, 0, len);
                        for m in &mut plain { m.shift(at, 0, len); }
                    }
                    Op::Remove { at, len } => {
                        markers.edit(at, len, 0);
                        for m in &mut plain { m.shift(at, len, 0); }
                    }
                    Op::Query { start, len } => {
                        let end = start + len;
                        let mut scan: Vec<Placed> = plain
                            .iter()
                            .map(Plain::placed)
                            .filter(|m| m.start < end && m.end.max(m.start + 1) > start)
                            .collect();
                        scan.sort_unstable_by_key(|m| (m.start, m.id));
                        prop_assert_eq!(markers.query(start, end).unwrap(), scan);
                    }
                }
                for m in &plain {
                    prop_assert_eq!(markers.get(m.id).unwrap(), m.placed());
                }
            }
        }
    }

    #[test]
    fn a_range_keeps_text_typed_at_its_edges_out() {
        let mut markers = Markers::default();
        let range = markers.span(10, 20, true, false).unwrap();
        markers.edit(10, 0, 3);
        markers.edit(23, 0, 3);
        assert_eq!(
            markers.get(range).unwrap(),
            Placed {
                id: range,
                start: 13,
                end: 23
            }
        );
    }

    #[test]
    fn a_deletion_clamps_an_end_inside_it_to_where_it_starts() {
        let mut markers = Markers::default();
        let range = markers.span(5, 12, true, false).unwrap();
        markers.edit(10, 5, 0);
        assert_eq!(
            markers.get(range).unwrap(),
            Placed {
                id: range,
                start: 5,
                end: 10
            }
        );
    }

    #[test]
    fn bad_arguments_are_errors() {
        let mut markers = Markers::default();
        assert!(markers.span(5, 4, true, false).is_err());
        assert!(markers.query(5, 4).is_err());
        assert!(markers.delete(7).is_err());
        assert!(markers.get(7).is_err());
    }
}
