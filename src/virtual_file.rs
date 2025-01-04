use std::{io::Seek, os::unix::fs::FileExt, sync::Arc};

use crate::{lines::LoadedLine, memstore::Memstore};

pub struct VirtualFile<'a> {
    file: Arc<std::fs::File>,
    memstore: Memstore<'a>,
}

impl<'a> VirtualFile<'a> {
    pub fn new(chunk_size: u64, file: std::fs::File) -> VirtualFile<'a> {
        let file = Arc::new(file);
        let load_fn = {
            let file = Arc::clone(&file);
            move |x: u64| -> Option<Vec<u8>> {
                let mut buf = vec![0; chunk_size as usize];
                file.read_at(&mut buf, x).expect("failed reading from file");
                return Some(buf);
            }
        };
        let store_fn = {
            let file = Arc::clone(&file);
            move |x: u64, buf: &[u8]| {
                file.write_at(&buf, x).expect("failed writing to file");
            }
        };
        VirtualFile {
            file,
            memstore: Memstore::new(chunk_size, load_fn, store_fn),
        }
    }

    pub fn get_mut(&self, line_index: usize) -> &mut LoadedLine {
        todo!()
    }

    pub fn len(&self) -> u16 {
        todo!()
    }

    pub fn remove(&self, y: usize) -> LoadedLine {
        todo!()
    }

    pub fn insert(&self, y: usize, new_line: LoadedLine) {
        todo!()
    }

    pub fn get(&self, y: usize) -> &LoadedLine {
        todo!()
    }

    pub fn iter_at<'b, I>(&self, offset: usize) -> I
    where
        I: Iterator<Item = &'b LoadedLine>,
    {
    }
}
