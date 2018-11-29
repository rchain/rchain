import re
import sys
import time
import logging

import pytest
import psutil
import docker
import typing_extensions


class WaitPredicate(typing_extensions.Protocol):
    def __str__(self) -> str:
        ...

    def is_satisfied(self, node: 'Node') -> bool:
        ...


class LogsContainMessage:
    def __init__(self, message: str) -> None:
        self.message = message

    def __str__(self):
        return '{}({})'.format(self.__class__.__name__, repr(self.message))

    def is_satisfied(self, node: 'Node') -> bool:
        return self.message in node.logs()


class NodeStarted(LogsContainMessage):
    def __init__(self):
        super().__init__('coop.rchain.node.NodeRuntime - Listening for traffic on rnode')


class ApprovedBlockReceivedHandlerStateEntered(LogsContainMessage):
    def __init__(self):
        super().__init__('Making a transition to ApprovedBlockRecievedHandler state.')


def wait_for_node(node: 'Node', predicate: WaitPredicate, timeout: int, timeout_message: str) -> None:
    # It is easy to precisely measure the time spent in user mode only on
    # Linux for a dockerized process; on other platforms we fall back on
    # measuring wall-clock time as an approximation.
    if sys.platform != 'linux':
        def shim():
            return predicate.is_satisfied(node)
        shim.__doc__ = str(predicate)
        return wait_for(shim, timeout, timeout_message)

    container_data = node.docker_client.api.inspect_container(node.name)
    container_pid = container_data['State']['Pid']
    process = psutil.Process(container_pid)

    while True:
        if predicate.is_satisfied(node):
            return

        node_user_time_consumed = process.cpu_times().user
        if node_user_time_consumed >= timeout:
            pytest.fail(timeout_message)

        time.sleep(1)


def wait_for(wait_condition, timeout, error_message):
    """
    Waits for a wait_condition to be satisfied. It retries until the timeout expires.

    :param wait_condition: the wait_condition. Has to be a function 'Unit -> Boolean'
    :param timeout: the total time to wait
    :return: true  if the wait_condition was met in the given timeout
    """

    logging.info("Waiting on: {}".format(repr(wait_condition.__doc__)))
    elapsed = 0
    current_ex = None
    while elapsed < timeout:
        start_time = time.time()

        is_satisfied = wait_condition()
        if is_satisfied:
            return

        condition_evaluation_duration = time.time() - start_time
        elapsed = int(elapsed + condition_evaluation_duration)
        time_left = timeout - elapsed

        # iteration duration is 15% of remaining timeout
        # but no more than 10s and no less than 1s
        iteration_duration = int(min(10, max(1, int(0.15 * time_left))))

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


def get_block(node, block_hash):
    def go():
        block_contents = node.get_block(block_hash)
        return block_contents

    go.__doc__ = 'get_block({})'.format(repr(block_hash))
    return go


def show_blocks(node):
    def go():
        exit_code, _ = node.show_blocks()

        if exit_code != 0:
            return False

        return True

    go.__doc__ = "show_blocks({})".format(node.name)
    return go


def string_contains(string_factory, regex_str, flags=0):
    rx = re.compile(regex_str, flags)

    def go():
        s = string_factory()

        m = rx.search(s)
        if m:
            return True
        else:
            return False

    go.__doc__ = "{string_factory} contains regex '{regex_str}'".format(string_factory=string_factory.__doc__, regex_str=regex_str)
    return go


def has_peers(bootstrap_node, expected_peers):
    rx = re.compile(r"^peers (\d+).0\s*$", re.MULTILINE | re.DOTALL)

    def go():
        exit_code, output = bootstrap_node.get_metrics()

        m = rx.search(output)

        peers = int(m[1]) if m else 0

        if peers < expected_peers:
            return False

        return True

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


def wait_for_node_started(node: 'Node', startup_timeout: int):
    wait_for_node(
        node,
        NodeStarted(),
        startup_timeout,
        'Node {} did not start within {}s'.format(node.name, startup_timeout),
    )


def wait_for_approved_block_received_handler_state(node: 'Node', timeout: int):
    wait_for_node(
        node,
        ApprovedBlockReceivedHandlerStateEntered(),
        timeout,
        "Node {} did not enter ApprovedBlockRecievedHandler state".format(node.name),
    )


def wait_for_approved_block_received(network, timeout: int):
    for peer in network.peers:
        wait_for(
            approved_block_received(peer),
            timeout,
            "Peer {} did not receive the approved block",
        )


def wait_for_started_network(node_startup_timeout, network):
    for peer in network.peers:
        wait_for(node_started(peer), node_startup_timeout, "Peer {} did not start correctly.".format(peer.name))


def wait_for_converged_network(timeout, network, peer_connections):
    wait_for(has_peers(network.bootstrap, len(network.peers)),
             timeout,
             "The network did NOT converge. Check container logs for issues. One or more containers might have failed to start or connect.")

    for node in network.peers:
        wait_for(has_peers(node, peer_connections),
                 timeout,
                 "The network did NOT converge. Check container logs for issues. One or more containers might have failed to start or connect.")
