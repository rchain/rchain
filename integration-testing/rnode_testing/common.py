import os
import random
import string
import tempfile
import contextlib

from typing import Callable, Iterator



def random_string(length: int) -> str:
    return ''.join(random.choice(string.ascii_letters) for m in range(length))


@contextlib.contextmanager
def log_box(log_function: Callable, title: str = "", char: str = "*", length: int = 150) -> Iterator[None]:
    full_title = " {} ".format(title) if title else ""
    title_stars_len = int((length - len(full_title)) / 2)
    title_stars = char * title_stars_len
    log_function(title_stars + full_title + title_stars)
    try:
        yield
    finally:
        log_function(char * length)


def make_tempfile(prefix: str, content: str) -> str:
    fd, path = tempfile.mkstemp(dir="/tmp", prefix=prefix)

    with os.fdopen(fd, 'w') as tmp:
        tmp.write(content)

    return path


def make_tempdir(prefix: str) -> str:
    return tempfile.mkdtemp(dir="/tmp", prefix=prefix)
