import time
from functools import wraps
import logging
from rnode_testing.util import log_box
from typing import Dict, List

PROF_DATA: Dict[str, List] = {}


def profile(fn):
    @wraps(fn)
    def with_profiling(*args, **kwargs):
        start_time = time.time()

        ret = fn(*args, **kwargs)

        elapsed_time = time.time() - start_time

        if fn.__name__ not in PROF_DATA:
            PROF_DATA[fn.__name__] = [0, []]
        PROF_DATA[fn.__name__][0] += 1
        PROF_DATA[fn.__name__][1].append(elapsed_time)

        return ret

    return with_profiling


def log_prof_data():
    with log_box(logging.info, "Profiling information:"):
        for fname, (count, calls) in PROF_DATA.items():
            max_time = max(calls)
            avg_time = sum(calls) / len(calls)
            logging.info("Function %s called %d times. Execution time max: %.3fs, average: %.3fs", fname, count, max_time, avg_time)
