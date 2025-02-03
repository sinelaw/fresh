use std::ops::Range;
use std::sync::Arc;

const CHUNK_SIZE: usize = 1024;

enum ChunkTree<'a> {
    Leaf {
        data: &'a [u8],
    },
    Internal {
        left: Arc<ChunkTree<'a>>,
        mid: Arc<ChunkTree<'a>>,
        right: Arc<ChunkTree<'a>>,
        size: usize,
    },
}

impl<'a> ChunkTree<'a> {
    fn new() -> Arc<ChunkTree<'a>> {
        Self::from_slice(&[])
    }

    fn from_slice(data: &[u8]) -> Arc<ChunkTree> {
        if data.len() <= CHUNK_SIZE {
            return Arc::new(ChunkTree::Leaf { data });
        }

        let mid_index = data.len() / 2;
        let left = Self::from_slice(&data[..mid_index]);
        let right = Self::from_slice(&data[mid_index..]);
        let size = data.len();

        Arc::new(ChunkTree::Internal {
            left,
            mid: Arc::new(ChunkTree::Leaf { data: &[] }),
            right,
            size,
        })
    }

    fn len(&self) -> usize {
        match self {
            ChunkTree::Leaf { data } => data.len(),
            ChunkTree::Internal { size, .. } => *size,
        }
    }

    fn is_empty(&self) -> bool {
        match self {
            ChunkTree::Leaf { data } => data.is_empty(),
            ChunkTree::Internal { size, .. } => *size == 0,
        }
    }

    fn insert(&'a self, index: usize, data: &'a [u8]) -> Arc<ChunkTree> {
        match self {
            ChunkTree::Leaf { data: leaf_data } => {
                let left = Self::from_slice(&leaf_data[..index]);
                let mid = Self::from_slice(data);
                let right = Self::from_slice(&leaf_data[index..]);

                Arc::new(ChunkTree::Internal {
                    left,
                    mid,
                    right,
                    size: leaf_data.len() + data.len(),
                })
            }
            ChunkTree::Internal {
                left,
                mid,
                right,
                size: _,
            } => {
                let left_size = left.len();
                if index <= left_size {
                    let new_left = left.insert(index, data);
                    let size = new_left.len() + mid.len() + right.len();
                    Arc::new(ChunkTree::Internal {
                        left: new_left,
                        mid: mid.clone(),
                        right: right.clone(),
                        size,
                    })
                } else if index <= left_size + mid.len() {
                    let new_mid = mid.insert(index - left_size, data);
                    let size = left_size + new_mid.len() + right.len();
                    Arc::new(ChunkTree::Internal {
                        left: left.clone(),
                        mid: new_mid,
                        right: right.clone(),
                        size,
                    })
                } else {
                    let new_right = right.insert(index - left_size - mid.len(), data);
                    let size = left_size + mid.len() + new_right.len();
                    Arc::new(ChunkTree::Internal {
                        left: left.clone(),
                        mid: mid.clone(),
                        right: new_right,
                        size,
                    })
                }
            }
        }
    }

    fn range_shift_left(range: &Range<usize>, amount: usize) -> Range<usize> {
        let start = if amount < range.start {
            range.start - amount
        } else {
            0
        };
        let end = if amount < range.end {
            range.end - amount
        } else {
            0
        };
        start..end
    }

    fn remove(&'a self, range: Range<usize>) -> Arc<ChunkTree> {
        match self {
            ChunkTree::Leaf { data } => Arc::new(ChunkTree::Internal {
                left: Self::from_slice(&data[..range.start]),
                mid: Self::from_slice(&[]),
                right: Self::from_slice(&data[range.end..]),
                size: data.len() - range.len(),
            }),
            ChunkTree::Internal {
                left,
                mid,
                right,
                size,
            } => {
                if range.start > *size {
                    return Arc::new(ChunkTree::Internal {
                        left: left.clone(),
                        mid: mid.clone(),
                        right: right.clone(),
                        size: *size,
                    });
                }

                let new_left = left.remove(range.start..range.end);
                let new_mid = mid.remove(Self::range_shift_left(&range, left.len()));
                let new_right =
                    right.remove(Self::range_shift_left(&range, left.len() + mid.len()));

                let new_size = new_left.len() + new_mid.len() + new_right.len();

                Arc::new(ChunkTree::Internal {
                    left: new_left,
                    mid: new_mid,
                    right: new_right,
                    size: new_size,
                })
            }
        }
    }

    fn collect_bytes(&self) -> Vec<u8> {
        let mut v = vec![];
        self.collect_bytes_into(&mut v);
        v
    }

    fn collect_bytes_into(&self, output: &mut Vec<u8>) {
        match self {
            ChunkTree::Leaf { data } => output.extend_from_slice(data),
            ChunkTree::Internal {
                left,
                mid,
                right,
                size: _,
            } => {
                left.collect_bytes(output);
                mid.collect_bytes(output);
                right.collect_bytes(output);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_tree() {
        let tree = ChunkTree::new();
        assert!(tree.is_empty());
        assert_eq!(tree.len(), 0);
        assert_eq!(tree.collect_bytes(), vec![]);
    }

    #[test]
    fn test_from_slice() {
        let data = b"Hello World!";
        let tree = ChunkTree::from_slice(data);
        assert!(!tree.is_empty());
        assert_eq!(tree.len(), data.len());
        assert_eq!(tree.collect_bytes(), data);
    }

    #[test]
    fn test_insert_middle() {
        let tree = ChunkTree::from_slice(b"Hello World!");
        let tree = tree.insert(5, b" beautiful");
        assert_eq!(tree.collect_bytes(), b"Hello beautiful World!");
    }

    #[test]
    fn test_insert_start() {
        let tree = ChunkTree::from_slice(b"World!");
        let tree = tree.insert(0, b"Hello ");
        assert_eq!(tree.collect_bytes(), b"Hello World!");
    }

    #[test]
    fn test_insert_end() {
        let tree = ChunkTree::from_slice(b"Hello");
        let tree = tree.insert(5, b" World!");
        assert_eq!(tree.collect_bytes(), b"Hello World!");
    }

    #[test]
    fn test_remove_middle() {
        let tree = ChunkTree::from_slice(b"Hello beautiful World!");
        let tree = tree.remove(5..15);
        assert_eq!(tree.collect_bytes(), b"Hello World!");
    }

    #[test]
    fn test_remove_start() {
        let tree = ChunkTree::from_slice(b"Hello World!");
        let tree = tree.remove(0..6);
        assert_eq!(tree.collect_bytes(), b"World!");
    }

    #[test]
    fn test_remove_end() {
        let tree = ChunkTree::from_slice(b"Hello World!");
        let tree = tree.remove(5..12);
        assert_eq!(tree.collect_bytes(), b"Hello");
    }

    #[test]
    #[should_panic]
    fn test_insert_out_of_bounds() {
        let tree = ChunkTree::from_slice(b"Hello");
        tree.insert(6, b" World!");
    }

    #[test]
    #[should_panic]
    fn test_remove_invalid_range() {
        let tree = ChunkTree::from_slice(b"Hello");
        tree.remove(3..6);
    }
}
