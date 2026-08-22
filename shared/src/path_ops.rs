//! Path operations (port of `shared/PathOps.scala`).
//!
//! The `RichPath` helpers (`folderSize`, `recursivelyDelete`) are always available; the
//! `Log`-based `PathDelete` helpers (`deleteDirectory`, `deleteSingleFile`) are gated on the
//! `tokio` feature (which provides the `Log` facade).

use std::fs;
use std::io;
use std::path::Path;
#[cfg(feature = "tokio")]
use std::path::PathBuf;

#[cfg(feature = "tokio")]
use crate::log::{Log, LogSource};

/// Sum of all file sizes under `path` (port of `RichPath.folderSize`).
pub fn folder_size(path: &Path) -> io::Result<u64> {
    let mut total = 0u64;
    accumulate(path, &mut total)?;
    Ok(total)
}

fn accumulate(path: &Path, total: &mut u64) -> io::Result<()> {
    let meta = fs::metadata(path)?;
    if meta.is_file() {
        *total += meta.len();
    } else if meta.is_dir() {
        for entry in fs::read_dir(path)? {
            accumulate(&entry?.path(), total)?;
        }
    }
    Ok(())
}

/// Recursively delete a directory tree (port of `RichPath.recursivelyDelete`).
pub fn recursively_delete(path: &Path) -> io::Result<()> {
    fs::remove_dir_all(path)
}

#[cfg(feature = "tokio")]
/// Delete a directory tree, logging each removed path (port of `PathDelete.deleteDirectory`).
pub fn delete_directory(path: &Path, log: &dyn Log, source: LogSource) {
    let mut files = match collect_tree(path) {
        Ok(files) => files,
        Err(e) if e.kind() == io::ErrorKind::NotFound => {
            log.warn(
                source,
                &format!("Can't delete file or directory {}: No such file", path.display()),
            );
            return;
        }
        Err(e) => {
            log.error(
                source,
                &format!("Can't delete file or directory {}: {}", path.display(), e),
            );
            return;
        }
    };
    files.sort();
    files.reverse();
    for file in &files {
        let deleted = if file.is_dir() {
            fs::remove_dir(file).is_ok()
        } else {
            fs::remove_file(file).is_ok()
        };
        if deleted {
            log.debug(source, &format!("Deleted file {}", file.display()));
        } else {
            log.warn(source, &format!("Can't delete file {}", file.display()));
        }
    }
}

#[cfg(feature = "tokio")]
/// Delete a single file, logging the outcome (port of `PathDelete.deleteSingleFile`).
pub fn delete_single_file(path: &Path, log: &dyn Log, source: LogSource) {
    if !path.exists() {
        log.warn(
            source,
            &format!("Can't delete file {}. File not found.", path.display()),
        );
        return;
    }
    match fs::remove_file(path) {
        Ok(()) => log.debug(source, &format!("Deleted file {}", path.display())),
        Err(_) => log.warn(source, &format!("Can't delete file {}.", path.display())),
    }
}

#[cfg(feature = "tokio")]
fn collect_tree(path: &Path) -> io::Result<Vec<PathBuf>> {
    let mut out = Vec::new();
    collect_recursive(path, &mut out)?;
    Ok(out)
}

#[cfg(feature = "tokio")]
fn collect_recursive(path: &Path, out: &mut Vec<PathBuf>) -> io::Result<()> {
    out.push(path.to_path_buf());
    if path.is_dir() {
        for entry in fs::read_dir(path)? {
            collect_recursive(&entry?.path(), out)?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn temp_dir(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("rchain_{name}_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        dir
    }

    #[test]
    fn folder_size_sums_file_sizes_recursively() {
        let dir = temp_dir("folder_size");
        fs::create_dir_all(dir.join("sub")).unwrap();
        fs::write(dir.join("a.txt"), b"hello").unwrap(); // 5 bytes
        fs::write(dir.join("sub/b.txt"), b"world!").unwrap(); // 6 bytes

        assert_eq!(folder_size(&dir).unwrap(), 11);

        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn folder_size_of_a_file_is_its_length() {
        let dir = temp_dir("folder_size_file");
        fs::create_dir_all(&dir).unwrap();
        let file = dir.join("f.txt");
        fs::write(&file, b"abcdef").unwrap();

        assert_eq!(folder_size(&file).unwrap(), 6);

        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn recursively_delete_removes_the_tree() {
        let dir = temp_dir("recursive_delete");
        fs::create_dir_all(dir.join("sub")).unwrap();
        fs::write(dir.join("sub/f.txt"), b"x").unwrap();

        recursively_delete(&dir).unwrap();

        assert!(!dir.exists());
    }

    #[cfg(feature = "tokio")]
    #[test]
    fn delete_directory_removes_the_tree() {
        use crate::log::{LogSource, NopLog};

        let dir = temp_dir("delete_directory");
        fs::create_dir_all(dir.join("sub")).unwrap();
        fs::write(dir.join("sub/f.txt"), b"x").unwrap();

        delete_directory(&dir, &NopLog, LogSource::new("test"));

        assert!(!dir.exists());
    }

    #[cfg(feature = "tokio")]
    #[test]
    fn delete_directory_missing_is_a_noop() {
        use crate::log::{LogSource, NopLog};

        let missing = temp_dir("delete_directory_missing");
        delete_directory(&missing, &NopLog, LogSource::new("test"));
    }

    #[cfg(feature = "tokio")]
    #[test]
    fn delete_single_file_removes_file() {
        use crate::log::{LogSource, NopLog};

        let dir = temp_dir("delete_single_file");
        fs::create_dir_all(&dir).unwrap();
        let file = dir.join("f.txt");
        fs::write(&file, b"x").unwrap();

        delete_single_file(&file, &NopLog, LogSource::new("test"));
        assert!(!file.exists());

        // Deleting an already-missing file is a logged no-op.
        delete_single_file(&file, &NopLog, LogSource::new("test"));

        fs::remove_dir_all(&dir).unwrap();
    }
}
