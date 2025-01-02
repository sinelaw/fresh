use std::slice::{RSplit, Split};

use crate::LoadedLine;

pub fn lines_iter<'a>(data: &'a [char]) -> ChunkLinesIter {
    ChunkLinesIter {
        forward: data.split(Box::new(|c: &char| *c == '\n')),
        back: data.rsplit(Box::new(|c: &char| *c == '\n')),
    }
}

pub struct ChunkLinesIter<'a> {
    forward: Split<'a, char, Box<dyn FnMut(&char) -> bool>>,
    back: RSplit<'a, char, Box<dyn FnMut(&char) -> bool>>,
}

impl<'a> Iterator for ChunkLinesIter<'a> {
    type Item = LoadedLine;

    fn next(&mut self) -> Option<Self::Item> {
        let line = self.forward.next();
        return line.map(LoadedLine::new);
    }
}

impl<'a> DoubleEndedIterator for ChunkLinesIter<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let line = self.back.next();
        return line.map(LoadedLine::new);
    }
}

#[cfg(test)]
mod tests {

    use super::lines_iter;

    #[test]
    fn test_chunk_lines_iter() {
        let data: Vec<char> = "line1\nline2\nline3\n".chars().collect();
        let mut iter = lines_iter(&data);

        assert_eq!(
            iter.next().unwrap().chars_iter().collect::<String>(),
            "line1"
        );
        assert_eq!(
            iter.next().unwrap().chars_iter().collect::<String>(),
            "line2"
        );
        assert_eq!(
            iter.next().unwrap().chars_iter().collect::<String>(),
            "line3"
        );

        assert_eq!(iter.next().unwrap().chars_iter().collect::<String>(), "");
        assert!(iter.next().is_none());
    }

    #[test]
    fn test_chunk_lines_iter_empty() {
        let data: Vec<char> = "".chars().collect();
        let mut iter = lines_iter(&data);

        assert_eq!(iter.next().unwrap().chars_iter().collect::<String>(), "");
        assert!(iter.next().is_none());
    }

    #[test]
    fn test_chunk_lines_iter_no_newline() {
        let data: Vec<char> = "line1".chars().collect();
        let mut iter = lines_iter(&data);

        assert_eq!(
            iter.next().unwrap().chars_iter().collect::<String>(),
            "line1"
        );
        assert!(iter.next().is_none());
    }

    #[test]
    fn test_chunk_lines_iter_double_ended() {
        let data: Vec<char> = "line1\nline2\nline3\n".chars().collect();
        let mut iter = lines_iter(&data);

        assert_eq!(
            iter.next_back().unwrap().chars_iter().collect::<String>(),
            ""
        );
        assert_eq!(
            iter.next_back().unwrap().chars_iter().collect::<String>(),
            "line3"
        );
        assert_eq!(
            iter.next_back().unwrap().chars_iter().collect::<String>(),
            "line2"
        );
        assert_eq!(
            iter.next_back().unwrap().chars_iter().collect::<String>(),
            "line1"
        );
        assert!(iter.next_back().is_none());
    }
}
