import os
import random
import string
import tempfile


def random_string(length: int) -> str:
    return ''.join(random.choice(string.ascii_letters) for m in range(length))


def make_tempfile(prefix: str, content: str) -> str:
    fd, path = tempfile.mkstemp(dir="/tmp", prefix=prefix)

    with os.fdopen(fd, 'w') as tmp:
        tmp.write(content)

    return path


def make_tempdir(prefix: str) -> str:
    return tempfile.mkdtemp(dir="/tmp", prefix=prefix)
