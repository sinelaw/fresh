//! Line structure: the role a line plays in the blocks its host grammar
//! recognizes — an embedded-language region ([`crate::embedding`]: a
//! Markdown fence, a Vue `<script>`/`<style>` block, a diff's file hunks)
//! and a table — as the editor's native engine classifies them for the
//! `lines_changed` hook (`region`, `table`).
//!
//! Both are read off the host's scope stack before and after each line, as
//! the parse already has it: a region is open while its region scope is on
//! the stack, a table's body while its body scope is ([`TABLE_SPECS`]). A
//! line's region role is a function of its own two edges; its table role
//! also needs its neighbours': the header is the line above the one that
//! opens the body (the delimiter row), the first data row the one below
//! it, and the table's last line the one above the line that closes the
//! body — or the document's last line, when the body is still open there.

use syntect::parsing::Scope;

/// A line's role in an embedded-language region.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegionRole {
    /// The region's opening delimiter (Markdown: ```` ```rust ````).
    Open,
    /// A content line strictly inside the region.
    Body,
    /// The region's closing delimiter.
    Close,
}

/// A line's kind in a table.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TableRole {
    /// The column-name row, above the delimiter row.
    Header,
    /// The `|---|---|` row separating the header from the body.
    Delimiter,
    /// A data row.
    Row,
}

/// Where a line sits in its table.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TableLine {
    pub role: TableRole,
    /// The first data row: the one directly below the delimiter row. Only
    /// ever true for [`TableRole::Row`].
    pub first_row: bool,
    /// The table's final line.
    pub last: bool,
}

/// What the parse says of a line's structure.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Roles {
    /// Not known: the line's runs are provisional, or the lines the answer
    /// depends on (the line itself and, for a table, the line after it)
    /// are not all parsed exactly yet.
    Unknown,
    /// The line's roles, `None` where it has none.
    Known {
        region: Option<RegionRole>,
        table: Option<TableLine>,
    },
}

/// How one host syntax scopes a table: a block whose rows the grammar
/// recognizes but which embeds no second language.
struct TableSpecDef {
    host_syntax: &'static str,
    /// Scope (prefix) the host keeps on the stack from the end of the
    /// delimiter row through the table's last row.
    body_scope: &'static str,
}

/// Markdown is the only host that scopes tables: the bundled grammar pushes
/// `meta.table.markdown` on the delimiter row and pops it after the table's
/// last row.
const TABLE_SPECS: &[TableSpecDef] = &[TableSpecDef {
    host_syntax: "Markdown",
    body_scope: "meta.table",
}];

/// The table body scope of the syntax named `host_syntax`, if it scopes
/// tables.
pub fn table_body_scope(host_syntax: &str) -> Option<Scope> {
    TABLE_SPECS
        .iter()
        .find(|spec| spec.host_syntax == host_syntax)
        .map(|spec| {
            Scope::new(spec.body_scope).expect("the table specs' body scopes are valid scopes")
        })
}

/// Whether a block was open at a line's start and at its end.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Edge {
    pub before: bool,
    pub after: bool,
}

impl Edge {
    /// The block opens on the line.
    fn opens(self) -> bool {
        !self.before && self.after
    }

    /// The block closes on the line.
    fn closes(self) -> bool {
        self.before && !self.after
    }
}

/// The structure state around one line: whether an embedded region and a
/// table body were open before it and after it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Transitions {
    pub region: Edge,
    pub table: Edge,
}

/// A line's region role, from its own edges.
pub(crate) fn region_role(edge: Edge) -> Option<RegionRole> {
    match (edge.before, edge.after) {
        (false, true) => Some(RegionRole::Open),
        (true, true) => Some(RegionRole::Body),
        (true, false) => Some(RegionRole::Close),
        (false, false) => None,
    }
}

/// A line's table role, from its table edge (`this`), the previous line's
/// (`None` for the document's first line) and the next line's (`None` for
/// the document's last).
pub(crate) fn table_line(prev: Option<Edge>, this: Edge, next: Option<Edge>) -> Option<TableLine> {
    // The table's last line is the one above the line that closes its
    // body, or the document's last line while the body is open.
    let last = next.is_none_or(Edge::closes);
    match (this.before, this.after) {
        (false, true) => Some(TableLine {
            role: TableRole::Delimiter,
            first_row: false,
            last,
        }),
        (true, true) => Some(TableLine {
            role: TableRole::Row,
            first_row: prev.is_some_and(Edge::opens),
            last,
        }),
        // The body opens on the delimiter row, so the line above it is the
        // header.
        _ => next.filter(|next| next.opens()).map(|_| TableLine {
            role: TableRole::Header,
            first_row: false,
            last: false,
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn edge(before: bool, after: bool) -> Edge {
        Edge { before, after }
    }

    #[test]
    fn region_roles_follow_the_edges() {
        assert_eq!(region_role(edge(false, true)), Some(RegionRole::Open));
        assert_eq!(region_role(edge(true, true)), Some(RegionRole::Body));
        assert_eq!(region_role(edge(true, false)), Some(RegionRole::Close));
        assert_eq!(region_role(edge(false, false)), None);
    }

    #[test]
    fn a_tables_lines_are_classified_from_their_neighbours() {
        let outside = edge(false, false);
        let delimiter = edge(false, true);
        let row = edge(true, true);
        let closing = edge(true, false);
        let line = |role, first_row, last| {
            Some(TableLine {
                role,
                first_row,
                last,
            })
        };
        assert_eq!(
            table_line(Some(outside), outside, Some(delimiter)),
            line(TableRole::Header, false, false)
        );
        assert_eq!(
            table_line(Some(outside), delimiter, Some(row)),
            line(TableRole::Delimiter, false, false)
        );
        assert_eq!(
            table_line(Some(delimiter), row, Some(row)),
            line(TableRole::Row, true, false)
        );
        assert_eq!(
            table_line(Some(row), row, Some(closing)),
            line(TableRole::Row, false, true)
        );
        assert_eq!(
            table_line(Some(row), row, None),
            line(TableRole::Row, false, true),
            "the document's end closes the table"
        );
        assert_eq!(table_line(Some(row), closing, Some(outside)), None);
        assert_eq!(table_line(None, outside, None), None);
    }
}
