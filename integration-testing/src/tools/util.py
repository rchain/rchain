from contextlib import contextmanager

@contextmanager
def log_box(log_function, title="", char = "*", length=150):
    full_title = f" {title} " if title else ""
    title_stars_len = int((length - len(full_title)) / 2)
    title_stars = char * title_stars_len
    log_function(title_stars + full_title + title_stars)
    yield
    log_function(char * length)

