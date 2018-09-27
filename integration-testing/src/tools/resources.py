import os
import inspect


def file_path(path):
    frame = inspect.stack()[1]
    caller_module = inspect.getmodule(frame[0])
    base_dir = os.path.realpath(__file__) + f'/../../'
    relative_to_base = caller_module.__file__[len(base_dir):]
    return os.path.realpath(f"resources/{relative_to_base}/{path}")
