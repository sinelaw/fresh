use std::collections::HashMap;
use std::path::{Path, PathBuf};

pub(super) fn insert_with_aliases<T, FInsert>(
    direct: &mut HashMap<PathBuf, T>,
    path: &Path,
    value: &T,
    symlink_mappings: &HashMap<PathBuf, PathBuf>,
    mut insert: FInsert,
) where
    T: Clone,
    FInsert: FnMut(&mut HashMap<PathBuf, T>, PathBuf, T),
{
    insert(direct, path.to_path_buf(), value.clone());

    for (symlink_path, canonical_target) in symlink_mappings {
        if let Ok(suffix) = path.strip_prefix(canonical_target) {
            insert(direct, symlink_path.join(suffix), value.clone());
        }
    }
}

pub(super) fn build_bubbled_cache<T, FInsert, FAncestorValue>(
    direct: &HashMap<PathBuf, T>,
    root: &Path,
    mut insert: FInsert,
    mut ancestor_value: FAncestorValue,
) -> HashMap<PathBuf, T>
where
    T: Clone,
    FInsert: FnMut(&mut HashMap<PathBuf, T>, PathBuf, T),
    FAncestorValue: FnMut(&Path, &T) -> T,
{
    let mut bubbled = HashMap::new();

    for (path, value) in direct {
        for ancestor in path.ancestors() {
            if !ancestor.starts_with(root) {
                break;
            }
            insert(
                &mut bubbled,
                ancestor.to_path_buf(),
                ancestor_value(ancestor, value),
            );
        }
    }

    bubbled
}
