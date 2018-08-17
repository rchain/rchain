import os

def file_path(file_name, group=""):
    return os.path.dirname(os.path.realpath(__file__)) + f'/../../resources/{group}/{file_name}'