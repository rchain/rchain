import logging
import re
import pytest
import time
from tools.util import log_box

def wait_for(condition, timeout, error_message):
    """
    Waits for a condition to be fulfilled. It retries until the timeout expires.

    :param condition: the condition. Has to be a function 'Unit -> Boolean'
    :param timeout: the total time to wait
    :return: true  if the condition was met in the given timeout
    """

    __tracebackhide__ = True

    with log_box(logging.info, f"Waiting maximum timeout={timeout}. Patience please!", "."):
        logging.info(f"Wait condition `{condition.__doc__}`")
        elapsed = 0
        current_ex = None
        while elapsed < timeout:
            start_time = time.time()

            try:
                value = condition()

                logging.info(f"Condition satisfied after {elapsed}s. Returning {value}")
                return value

            except Exception as ex:
                condition_evaluation_duration = time.time() - start_time
                elapsed = int(elapsed + condition_evaluation_duration)
                time_left = timeout - elapsed

                iteration_duration = int(max(1, int(0.15 * time_left))) # iteration duration is 15% of remaining timeout

                if str(ex) == current_ex:
                    details = "same as above"
                else:
                    details = str(ex)
                    current_ex = str(ex)

                logging.info(f"Condition not fulfilled yet ({details}). Time left: {time_left}s. Sleeping {iteration_duration}s...")

                time.sleep(iteration_duration)
                elapsed = elapsed + iteration_duration

        logging.warning(f"Giving up after {elapsed}s.")
        pytest.fail(error_message)

# Predicates
# For each predicate please provide a nicely formatted __doc__ because it is used in wait_for to display a nice message
# Warning: The __doc__ has to be explicitly assigned as seen below if it's a formatted string, otherwise it will be None.

def node_logs(node):
    def go(): return node.logs()
    go.__doc__ = f"node_logs({node.name})"
    return go

def show_blocks(node):
    def go():
        exit_code, output = node.show_blocks()

        if exit_code != 0: raise Exception("Show-blocks failed")

        return output

    go.__doc__ = f"show_blocks({node.name})"
    return go

def string_contains(string_factory, regex_str, flags = 0):
    rx = re.compile(regex_str, flags)

    def go():
        s = string_factory()

        m = rx.search(s)
        if m:
            return m
        else:
            raise Exception(f"{string_factory.__doc__} doesn't contain regex '{regex_str}'")

    go.__doc__ = f"{string_factory.__doc__} contains regex '{regex_str}'"
    return go

def has_peers(bootstrap_node, expected_peers):
    rx = re.compile("^peers (\d+).0\s*$", re.MULTILINE | re.DOTALL)

    def go():
        exit_code, output = bootstrap_node.get_metrics()

        m = rx.search(output)

        peers = int(m[1]) if m else 0

        if peers < expected_peers:
            raise Exception(f"Expected peers: {expected_peers}. Actual peers: {peers}")

    go.__doc__ = f"Node {bootstrap_node.name} is connected to {expected_peers} peers."

    return go

def node_started(node):
    return string_contains( node_logs(node),
                            "coop.rchain.node.NodeRuntime - Listening for traffic on rnode")