from contextlib import contextmanager

@contextmanager
def log_box(log_function, length=150):
    log_function("*" * length)
    yield
    log_function("*" * length)