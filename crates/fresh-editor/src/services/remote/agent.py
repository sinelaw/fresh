#!/usr/bin/env python3
"""Fresh Remote Agent - bootstrapped via SSH stdin"""
import sys
import os
import json
import base64
import stat
import shutil
import subprocess
import re
import threading
import select
from concurrent.futures import ThreadPoolExecutor

CHUNK = 65536
VERSION = 1

# Active background processes: id -> Popen
procs = {}
# Request IDs marked for cancellation
cancelled = set()
# Lock for thread-safe access to procs/cancelled
lock = threading.Lock()
# Lock for serializing stdout writes (prevents interleaved JSON lines)
write_lock = threading.Lock()


def send(id, **kw):
    """Send a JSON message to stdout (thread-safe)."""
    msg = {"id": id, **kw}
    line = json.dumps(msg, separators=(",", ":")) + "\n"
    with write_lock:
        sys.stdout.write(line)
        sys.stdout.flush()


def b64(data):
    """Encode bytes to base64 string."""
    return base64.b64encode(data).decode("ascii")


def unb64(s):
    """Decode base64 string to bytes."""
    return base64.b64decode(s)


def validate_path(p):
    """Validate and canonicalize a path."""
    if not p:
        raise ValueError("empty path")
    expanded = os.path.expanduser(p)
    if not os.path.isabs(expanded):
        expanded = os.path.abspath(expanded)
    return os.path.realpath(expanded)


# === File Operations ===


def cmd_read(id, p):
    """Read file contents, streaming in chunks for large files."""
    path = validate_path(p["path"])
    off = p.get("off", 0)
    length = p.get("len")

    with open(path, "rb") as f:
        if off:
            f.seek(off)
        total = 0
        while True:
            to_read = min(CHUNK, length - total) if length else CHUNK
            chunk = f.read(to_read)
            if not chunk:
                break
            total += len(chunk)
            send(id, d={"data": b64(chunk)})
            if length and total >= length:
                break
    send(id, r={"size": total})


def cmd_write(id, p):
    """Write file contents atomically."""
    path = validate_path(p["path"])
    data = unb64(p["data"])

    # Atomic write: write to temp, then rename
    tmp = f"{path}.fresh-{os.getpid()}"
    try:
        # Preserve permissions if file exists
        mode = None
        if os.path.exists(path):
            mode = os.stat(path).st_mode

        with open(tmp, "wb") as f:
            f.write(data)
            f.flush()
            os.fsync(f.fileno())

        if mode is not None:
            os.chmod(tmp, mode)

        os.replace(tmp, path)  # replace() works cross-platform, rename() fails on Windows if dest exists
    finally:
        if os.path.exists(tmp):
            try:
                os.unlink(tmp)
            except OSError:
                pass

    send(id, r={"size": len(data)})


def cmd_sudo_write(id, p):
    """Write file contents using sudo (for root-owned files).

    Uses sudo tee to write the file. Preserves original permissions and ownership.
    """
    path = validate_path(p["path"])
    data = unb64(p["data"])

    # Get original metadata to preserve permissions
    mode = p.get("mode")
    uid = p.get("uid")
    gid = p.get("gid")

    # Use sudo tee to write the file
    proc = subprocess.Popen(
        ["sudo", "tee", path],
        stdin=subprocess.PIPE,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
    )
    _, stderr = proc.communicate(data)

    if proc.returncode != 0:
        raise RuntimeError(f"sudo tee failed: {stderr.decode().strip()}")

    # Restore permissions and ownership if provided
    if mode is not None:
        subprocess.run(["sudo", "chmod", f"{mode:o}", path], check=True,
                       capture_output=True)
    if uid is not None and gid is not None:
        subprocess.run(["sudo", "chown", f"{uid}:{gid}", path], check=True,
                       capture_output=True)

    send(id, r={"size": len(data)})


def cmd_stat(id, p):
    """Get file/directory metadata."""
    path = validate_path(p["path"])
    follow = p.get("link", True)

    st = os.stat(path, follow_symlinks=follow)
    is_link = stat.S_ISLNK(os.lstat(path).st_mode) if follow else False

    send(
        id,
        r={
            "size": st.st_size,
            "mtime": int(st.st_mtime),
            "mode": st.st_mode,
            "uid": st.st_uid,
            "gid": st.st_gid,
            "dir": stat.S_ISDIR(st.st_mode),
            "file": stat.S_ISREG(st.st_mode),
            "link": is_link,
        },
    )


def cmd_ls(id, p):
    """List directory contents with metadata."""
    path = validate_path(p["path"])
    entries = []

    for entry in os.scandir(path):
        try:
            st = entry.stat(follow_symlinks=False)
            is_link = entry.is_symlink()

            # For symlinks, check target type
            target_is_dir = False
            if is_link:
                try:
                    target_is_dir = entry.is_dir(follow_symlinks=True)
                except OSError:
                    pass

            entries.append(
                {
                    "name": entry.name,
                    "path": os.path.join(path, entry.name),
                    "dir": entry.is_dir(follow_symlinks=True),
                    "file": entry.is_file(follow_symlinks=True),
                    "link": is_link,
                    "link_dir": target_is_dir,
                    "size": st.st_size,
                    "mtime": int(st.st_mtime),
                    "mode": st.st_mode,
                }
            )
        except OSError:
            # Skip entries we can't stat
            pass

    send(id, r={"entries": entries})


def cmd_rm(id, p):
    """Remove a file."""
    os.unlink(validate_path(p["path"]))
    send(id, r={})


def cmd_rmdir(id, p):
    """Remove an empty directory."""
    os.rmdir(validate_path(p["path"]))
    send(id, r={})


def cmd_mkdir(id, p):
    """Create a directory."""
    path = validate_path(p["path"])
    if p.get("parents"):
        os.makedirs(path, exist_ok=True)
    else:
        os.mkdir(path)
    send(id, r={})


def cmd_mv(id, p):
    """Move/rename a file or directory.

    Uses shutil.move() to handle cross-device moves (e.g., /tmp to /etc).
    """
    shutil.move(validate_path(p["from"]), validate_path(p["to"]))
    send(id, r={})


def cmd_cp(id, p):
    """Copy a file."""
    dst = validate_path(p["to"])
    shutil.copy2(validate_path(p["from"]), dst)
    send(id, r={"size": os.path.getsize(dst)})


def cmd_realpath(id, p):
    """Get canonical absolute path."""
    send(id, r={"path": validate_path(p["path"])})


def cmd_chmod(id, p):
    """Change file permissions."""
    os.chmod(validate_path(p["path"]), p["mode"])
    send(id, r={})


def cmd_append(id, p):
    """Append data to a file (creates if doesn't exist)."""
    path = validate_path(p["path"])
    data = unb64(p["data"])
    with open(path, "ab") as f:
        f.write(data)
        f.flush()
        os.fsync(f.fileno())
    send(id, r={"size": len(data)})


def cmd_truncate(id, p):
    """Truncate or extend a file to a specified length."""
    path = validate_path(p["path"])
    os.truncate(path, p["len"])
    send(id, r={})


def cmd_patch(id, p):
    """Apply a patch recipe to create a new file from original + edits.

    Recipe format:
    - {"copy": {"off": offset, "len": length}} - copy from original file
    - {"insert": {"data": base64_data}} - insert new content

    This allows saving edits without transferring unchanged portions of the file.
    """
    src = validate_path(p["src"])  # Original file to read from
    dst = validate_path(p.get("dst", src))  # Destination (defaults to same file)
    ops = p["ops"]

    # Get original file's metadata to preserve permissions
    mode = None
    if os.path.exists(dst):
        mode = os.stat(dst).st_mode

    tmp = f"{dst}.fresh-{os.getpid()}"
    try:
        with open(src, "rb") as orig, open(tmp, "wb") as out:
            for op in ops:
                if "copy" in op:
                    off = op["copy"]["off"]
                    length = op["copy"]["len"]
                    orig.seek(off)
                    data = orig.read(length)
                    # Validate we read the expected number of bytes
                    if len(data) != length:
                        raise IOError(
                            f"cmd_patch copy: expected {length} bytes at offset {off}, "
                            f"got {len(data)} (src: {src})"
                        )
                    out.write(data)
                elif "insert" in op:
                    out.write(unb64(op["insert"]["data"]))

            out.flush()
            os.fsync(out.fileno())

        if mode is not None:
            os.chmod(tmp, mode)

        os.replace(tmp, dst)
    finally:
        if os.path.exists(tmp):
            try:
                os.unlink(tmp)
            except OSError:
                pass

    send(id, r={})


def cmd_count_lf(id, p):
    """Count newline (0x0A) bytes in a file range without returning the data."""
    path = validate_path(p["path"])
    off = p.get("off", 0)
    length = p["len"]

    count = 0
    with open(path, "rb") as f:
        if off:
            f.seek(off)
        remaining = length
        while remaining > 0:
            to_read = min(CHUNK, remaining)
            chunk = f.read(to_read)
            if not chunk:
                break
            count += chunk.count(b"\n")
            remaining -= len(chunk)
    send(id, r={"count": count})


def cmd_exists(id, p):
    """Check if path exists."""
    try:
        path = validate_path(p["path"])
        send(id, r={"exists": os.path.exists(path)})
    except (ValueError, OSError):
        send(id, r={"exists": False})


def cmd_info(id, p):
    """Get system info (home directory, cwd, temp directory, etc.)."""
    import tempfile
    send(id, r={
        "home": os.path.expanduser("~"),
        "cwd": os.getcwd(),
        "temp_dir": tempfile.gettempdir(),
    })


def cmd_search_file(id, p):
    """Search for a pattern in a file, returning matches with line/column info."""
    path = validate_path(p["path"])
    pattern = p["pattern"]
    fixed_string = p.get("fixed_string", False)
    case_sensitive = p.get("case_sensitive", True)
    whole_word = p.get("whole_word", False)
    max_matches = p.get("max_matches", 100)
    offset = p.get("offset", 0)
    running_line = p.get("running_line", 1)
    end_offset = p.get("end_offset")  # None = search to EOF

    file_size = os.path.getsize(path)
    effective_end = min(end_offset, file_size) if end_offset is not None else file_size

    # Binary detection: check first 8192 bytes for null byte (only for full-file searches)
    if offset == 0 and end_offset is None:
        with open(path, "rb") as f:
            header = f.read(8192)
            if b"\x00" in header:
                send(id, r={"matches": [], "next_offset": file_size,
                             "running_line": running_line, "done": True})
                return

    # Build regex pattern (bytes)
    pat = pattern
    if fixed_string:
        pat = re.escape(pat)
    if whole_word:
        pat = r"\b" + pat + r"\b"
    pat_bytes = pat.encode("utf-8")

    flags = 0
    if not case_sensitive:
        flags |= re.IGNORECASE
    regex = re.compile(pat_bytes, flags)

    # Read one chunk
    CHUNK_SIZE = 1048576  # 1MB
    overlap = max(len(pattern), 256)
    read_start = max(0, offset - overlap)
    read_end = min(read_start + CHUNK_SIZE, effective_end)

    with open(path, "rb") as f:
        f.seek(read_start)
        chunk = f.read(read_end - read_start)

    overlap_len = offset - read_start

    # Count newlines in the overlap region to adjust running_line
    line_at = running_line - chunk[:overlap_len].count(b"\n")

    # Scan for matches
    matches = []
    last_pos = 0
    for m in regex.finditer(chunk):
        if m.end() <= overlap_len:
            continue
        if len(matches) >= max_matches:
            break

        # Count newlines from last_pos to match start to track line number
        line_at += chunk[last_pos:m.start()].count(b"\n")
        last_pos = m.start()

        # Find line boundaries for context
        line_start = chunk.rfind(b"\n", 0, m.start())
        line_start = line_start + 1 if line_start != -1 else 0
        line_end = chunk.find(b"\n", m.end())
        if line_end == -1:
            line_end = len(chunk)

        context = chunk[line_start:line_end].decode("utf-8", errors="replace")
        col = m.start() - line_start + 1
        byte_offset = read_start + m.start()

        matches.append({
            "byte_offset": byte_offset,
            "length": m.end() - m.start(),
            "line": line_at,
            "column": col,
            "context": context,
        })

    # Update running_line by counting newlines in new data
    new_running_line = running_line + chunk[overlap_len:].count(b"\n")

    send(id, r={
        "matches": matches,
        "next_offset": read_end,
        "running_line": new_running_line,
        "done": read_end >= effective_end,
    })


# === Process Operations ===


def cmd_exec(id, p):
    """Execute a process with streaming output."""
    cwd = validate_path(p["cwd"]) if p.get("cwd") else None
    cmd = p["cmd"]
    args = p.get("args", [])

    try:
        proc = subprocess.Popen(
            [cmd] + args,
            cwd=cwd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
    except FileNotFoundError:
        send(id, e=f"command not found: {cmd}")
        return
    except PermissionError:
        send(id, e=f"permission denied: {cmd}")
        return

    with lock:
        procs[id] = proc

    def stream_output():
        """Stream process output in a background thread."""
        try:
            while proc.poll() is None:
                # Check for cancellation
                if id in cancelled:
                    proc.terminate()
                    try:
                        proc.wait(timeout=2)
                    except subprocess.TimeoutExpired:
                        proc.kill()
                    send(id, e="cancelled")
                    return

                # Non-blocking read from stdout and stderr
                readable, _, _ = select.select(
                    [proc.stdout, proc.stderr], [], [], 0.05
                )
                for fd in readable:
                    data = fd.read(4096)
                    if data:
                        key = "out" if fd == proc.stdout else "err"
                        send(id, d={key: b64(data)})

            # Drain any remaining output
            out, err = proc.communicate(timeout=5)
            if out:
                send(id, d={"out": b64(out)})
            if err:
                send(id, d={"err": b64(err)})

            send(id, r={"code": proc.returncode})
        except Exception as e:
            send(id, e=str(e))
        finally:
            with lock:
                procs.pop(id, None)
                cancelled.discard(id)

    threading.Thread(target=stream_output, daemon=True).start()


def cmd_kill(id, p):
    """Kill a background process."""
    target_id = p["id"]
    with lock:
        proc = procs.get(target_id)

    if proc:
        proc.terminate()
        try:
            proc.wait(timeout=2)
        except subprocess.TimeoutExpired:
            proc.kill()
        send(id, r={})
    else:
        send(id, e="process not found")


def cmd_cancel(id, p):
    """Cancel an in-flight request."""
    target_id = p["id"]
    cancelled.add(target_id)

    with lock:
        proc = procs.get(target_id)
    if proc:
        proc.terminate()

    send(id, r={})


# === Method dispatch ===

METHODS = {
    "read": cmd_read,
    "write": cmd_write,
    "sudo_write": cmd_sudo_write,
    "stat": cmd_stat,
    "ls": cmd_ls,
    "rm": cmd_rm,
    "rmdir": cmd_rmdir,
    "mkdir": cmd_mkdir,
    "mv": cmd_mv,
    "cp": cmd_cp,
    "realpath": cmd_realpath,
    "chmod": cmd_chmod,
    "append": cmd_append,
    "truncate": cmd_truncate,
    "patch": cmd_patch,
    "count_lf": cmd_count_lf,
    "exists": cmd_exists,
    "info": cmd_info,
    "search_file": cmd_search_file,
    "exec": cmd_exec,
    "kill": cmd_kill,
    "cancel": cmd_cancel,
}


def handle_request(line):
    """Parse and handle a single request."""
    try:
        req = json.loads(line)
    except json.JSONDecodeError as e:
        send(0, e=f"parse error: {e}")
        return

    id = req.get("id", 0)
    method = req.get("m")
    params = req.get("p", {})

    if method not in METHODS:
        send(id, e=f"unknown method: {method}")
        return

    try:
        METHODS[method](id, params)
    except PermissionError as e:
        send(id, e=f"permission denied: {e}")
    except FileNotFoundError as e:
        send(id, e=f"not found: {e}")
    except IsADirectoryError as e:
        send(id, e=f"is a directory: {e}")
    except NotADirectoryError as e:
        send(id, e=f"not a directory: {e}")
    except OSError as e:
        send(id, e=f"os error: {e}")
    except Exception as e:
        send(id, e=str(e))


def main():
    """Main entry point."""
    # Send ready message
    send(0, ok=True, v=VERSION)

    # Thread pool for concurrent request handling.
    # File I/O requests (read, count_lf, stat, etc.) can execute in parallel,
    # which is important when the editor pipelines many count_lf requests
    # during line-feed scanning of large files.
    pool = ThreadPoolExecutor(max_workers=64)

    # Process requests from stdin
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        pool.submit(handle_request, line)

    pool.shutdown(wait=True)


if __name__ == "__main__":
    main()
