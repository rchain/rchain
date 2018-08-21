import os

def file_path(path, group=""):
    return os.path.dirname(os.path.realpath(__file__)) + f'/../../resources/{group}/{path}'

def read(path, group=""):
    abs_path = file_path(path, group)
    with open(abs_path) as f:
        return f.read()
