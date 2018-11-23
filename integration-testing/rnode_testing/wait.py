import logging
import re
import pytest
import time
from rnode_testing.util import log_box


def wait_for(condition, timeout, error_message):
    """
    Waits for a condition to be fulfilled. It retries until the timeout expires.

    :param condition: the condition. Has to be a function 'Unit -> Boolean'
    :param timeout: the total time to wait
    :return: true  if the condition was met in the given timeout
    """

    logging.info("Waiting maximum timeout={}. Patience please!".format(timeout))
    logging.info("Wait condition is: `{}`".format(condition.__doc__))
    elapsed = 0
    current_ex = None
    while elapsed < timeout:
        start_time = time.time()

        try:
            value = condition()

            logging.info("Condition satisfied after {elapsed}s. Returning {value}".format(elapsed=elapsed, value=value))
            return value

        except Exception as ex:
            condition_evaluation_duration = time.time() - start_time
            elapsed = int(elapsed + condition_evaluation_duration)
            time_left = timeout - elapsed

            # iteration duration is 15% of remaining timeout
            # but no more than 10s and no less than 1s
            iteration_duration = int(min(10, max(1, int(0.15 * time_left))))

            if str(ex) == current_ex:
                details = "same as above"
            else:
                details = str(ex)
                current_ex = str(ex)

            logging.info("Condition not satisfied yet ({details}). Time left: {time_left}s. Sleeping {iteration_duration}s...".format(
                details=details, time_left=time_left, iteration_duration=iteration_duration)
            )

            time.sleep(iteration_duration)
            elapsed = elapsed + iteration_duration

    logging.warning("Giving up after {}s.".format(elapsed))
    pytest.fail(error_message)


# Predicates
# For each predicate please provide a nicely formatted __doc__ because it is used in wait_for to display a nice message
# Warning: The __doc__ has to be explicitly assigned as seen below if it's a formatted string, otherwise it will be None.


def node_logs(node):
    def go(): return node.logs()
    go.__doc__ = "node_logs({})".format(node.name)
    return go


def show_blocks(node):
    def go():
        exit_code, output = node.show_blocks()

        if exit_code != 0:
            raise Exception("Show-blocks failed")

        return output

    go.__doc__ = "show_blocks({})".format(node.name)
    return go


def string_contains(string_factory, regex_str, flags=0):
    rx = re.compile(regex_str, flags)

    def go():
        s = string_factory()

        m = rx.search(s)
        if m:
            return m
        else:
            raise Exception("{string_factory} doesn't contain regex '{regex_str}'".format(
                string_factory=string_factory.__doc__, regex_str=regex_str)
            )

    go.__doc__ = "{string_factory} contains regex '{regex_str}'".format(string_factory=string_factory.__doc__, regex_str=regex_str)
    return go


def has_peers(bootstrap_node, expected_peers):
    rx = re.compile(r"^peers (\d+).0\s*$", re.MULTILINE | re.DOTALL)

    def go():
        exit_code, output = bootstrap_node.get_metrics()

        m = rx.search(output)

        peers = int(m[1]) if m else 0

        if peers < expected_peers:
            raise Exception("Expected peers: {expected_peers}. Actual peers: {peers}".format(expected_peers=expected_peers, peers=peers))

    go.__doc__ = "Node {name} is connected to {expected_peers} peers.".format(name=bootstrap_node.name, expected_peers=expected_peers)

    return go


def node_started(node):
    return string_contains(node_logs(node),
                           "coop.rchain.node.NodeRuntime - Listening for traffic on rnode")


def sent_unapproved_block():
    return string_contains(
        node_logs(node),
        "Sent UnapprovedBlock",
    )


def approved_block_received_handler_state(bootstrap_node):
    return string_contains(
        node_logs(bootstrap_node),
        "Making a transition to ApprovedBlockRecievedHandler state.",
    )


def approved_block_received(peer):
    return string_contains(
        node_logs(peer),
        "Valid ApprovedBlock received!",
    )
