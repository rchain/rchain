import os

def file_path(path, group=""):
    return os.path.dirname(os.path.realpath(__file__)) + f'/../../resources/{group}/{path}'

