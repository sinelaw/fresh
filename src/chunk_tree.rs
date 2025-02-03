use std::ops::Range;
use std::sync::Arc;

enum ChunkTree<'a, const N: usize> {
    Leaf {
        data: &'a [u8],
    },
    Internal {
        left: Arc<ChunkTree<'a, N>>,
        mid: Arc<ChunkTree<'a, N>>,
        right: Arc<ChunkTree<'a, N>>,
        size: usize,
    },
}

impl<'a, const N: usize> ChunkTree<'a, N> {
    fn new() -> Arc<ChunkTree<'a, N>> {
        assert!(N > 0);
        Self::from_slice(&[])
    }

    fn from_slice(data: &[u8]) -> Arc<ChunkTree<N>> {
        if data.len() <= N {
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

    fn insert(&'a self, index: usize, data: &'a [u8]) -> Arc<ChunkTree<N>> {
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
                size,
            } => {
                let left_size = left.len();
                if index <= left_size {
                    let new_left = left.insert(index, data);
                    let new_size = new_left.len() + mid.len() + right.len();
                    Arc::new(ChunkTree::Internal {
                        left: new_left,
                        mid: mid.clone(),
                        right: right.clone(),
                        size: new_size,
                    })
                } else if index <= left_size + mid.len() {
                    let new_mid = mid.insert(index - left_size, data);
                    let new_size = left_size + new_mid.len() + right.len();
                    Arc::new(ChunkTree::Internal {
                        left: left.clone(),
                        mid: new_mid,
                        right: right.clone(),
                        size: new_size,
                    })
                } else if index <= left_size + mid.len() + right.len() {
                    let new_right = right.insert(index - left_size - mid.len(), data);
                    let new_size = left_size + mid.len() + new_right.len();
                    Arc::new(ChunkTree::Internal {
                        left: left.clone(),
                        mid: mid.clone(),
                        right: new_right,
                        size: new_size,
                    })
                } else {
                    panic!("index out of range: {}, expected <= {}", index, size);
                }
            }
        }
    }

    fn range_shift_left(range: &Range<usize>, amount: usize) -> Range<usize> {
        (range.start.saturating_sub(amount))..(range.end.saturating_sub(amount))
    }

    fn range_cap(range: &Range<usize>, max: usize) -> Range<usize> {
        (std::cmp::min(range.start, max))..(std::cmp::min(range.end, max))
    }

    fn remove(&'a self, range: Range<usize>) -> Arc<ChunkTree<N>> {
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
                if range.start > self.len() || range.end > self.len() {
                    panic!(
                        "invalid range: {:?}, expected to be bound by 0..{}",
                        range,
                        self.len()
                    );
                }
                if range.start > *size {
                    return Arc::new(ChunkTree::Internal {
                        left: left.clone(),
                        mid: mid.clone(),
                        right: right.clone(),
                        size: *size,
                    });
                }

                let new_left = left.remove(Self::range_cap(&range, left.len()));
                let new_mid = mid.remove(Self::range_cap(
                    &Self::range_shift_left(&range, left.len()),
                    mid.len(),
                ));
                let new_right = right.remove(Self::range_cap(
                    &Self::range_shift_left(&range, left.len() + mid.len()),
                    right.len(),
                ));

                let new_size = new_left.len() + new_mid.len() + new_right.len();

                assert!(*size >= new_size);
                assert_eq!(size - Self::range_cap(&range, *size).len(), new_size);

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
                left.collect_bytes_into(output);
                mid.collect_bytes_into(output);
                right.collect_bytes_into(output);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_tree() {
        let tree = ChunkTree::<2>::new();
        assert!(tree.is_empty());
        assert_eq!(tree.len(), 0);
        assert_eq!(tree.collect_bytes(), vec![]);
    }

    #[test]
    fn test_from_slice() {
        let data = b"Hello World!";
        let tree = ChunkTree::<2>::from_slice(data);
        assert!(!tree.is_empty());
        assert_eq!(tree.len(), data.len());
        assert_eq!(tree.collect_bytes(), b"Hello World!");
    }

    #[test]
    fn test_insert_middle() {
        let tree = ChunkTree::<2>::from_slice(b"Hello World!");
        let tree = tree.insert(5, b" beautiful");
        assert_eq!(tree.collect_bytes(), b"Hello beautiful World!");
    }

    #[test]
    fn test_insert_start() {
        let tree = ChunkTree::<2>::from_slice(b"World!");
        let tree = tree.insert(0, b"Hello ");
        assert_eq!(tree.collect_bytes(), b"Hello World!");
    }

    #[test]
    fn test_insert_end() {
        let tree = ChunkTree::<2>::from_slice(b"Hello");
        let tree = tree.insert(5, b" World!");
        assert_eq!(tree.collect_bytes(), b"Hello World!");
    }

    #[test]
    fn test_remove_middle() {
        let tree = ChunkTree::<2>::from_slice(b"Hello beautiful World!");
        let tree = tree.remove(5..15);
        assert_eq!(tree.collect_bytes(), b"Hello World!");
    }

    #[test]
    fn test_remove_start() {
        let tree = ChunkTree::<2>::from_slice(b"Hello World!");
        let tree = tree.remove(0..6);
        assert_eq!(tree.collect_bytes(), b"World!");
    }

    #[test]
    fn test_remove_end() {
        let tree = ChunkTree::<2>::from_slice(b"Hello World!");
        let tree = tree.remove(5..12);
        assert_eq!(tree.collect_bytes(), b"Hello");
    }

    #[test]
    #[should_panic]
    fn test_insert_out_of_bounds() {
        let tree = ChunkTree::<2>::from_slice(b"Hello");
        tree.insert(6, b" World!");
    }

    #[test]
    #[should_panic]
    fn test_remove_invalid_range() {
        let tree = ChunkTree::<2>::from_slice(b"Hello");
        tree.remove(3..6);
    }
}
