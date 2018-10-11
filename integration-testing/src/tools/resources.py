import os
import inspect
from pathlib import Path


def file_path(path):
    frame = inspect.stack()[1]
    caller_module = inspect.getmodule(frame[0])
    base_dir = os.path.realpath(__file__) + f'/../../'
    relative_to_base = caller_module.__file__[len(base_dir):]
    return os.path.realpath(f"resources/{relative_to_base}/{path}")


def file_content(path):
    return Path(file_path(path)).read_text()
