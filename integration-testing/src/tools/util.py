from contextlib import contextmanager
import tempfile
import os


@contextmanager
def log_box(log_function, title="", char="*", length=150):
    full_title = f" {title} " if title else ""
    title_stars_len = int((length - len(full_title)) / 2)
    title_stars = char * title_stars_len
    log_function(title_stars + full_title + title_stars)
    try:
        yield
    finally:
        log_function(char * length)


def make_tempfile(prefix, content):
    fd, path = tempfile.mkstemp(dir="/tmp", prefix=prefix)

    with os.fdopen(fd, 'w') as tmp:
        tmp.write(content)


def make_tempdir(prefix):
    return tempfile.mkdtemp(dir="/tmp", prefix=prefix)
