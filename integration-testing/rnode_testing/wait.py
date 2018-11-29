import re
import sys
import time
import logging

import pytest
import psutil
import typing_extensions


class UnexpectedMetricsOutputFormatError(Exception):
    pass



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


class ApprovedBlockReceived(LogsContainMessage):
    def __init__(self):
        super().__init__('Valid ApprovedBlock received!')


class HasAtLeastPeers:
    def __init__(self, minimum_peers_number):
        self.minimum_peers_number = minimum_peers_number
        self.metric_regex = re.compile(r"^peers (\d+).0\s*$", re.MULTILINE | re.DOTALL)

    def __str__(self):
        return '{}({})'.format(self.__class__.__name__, repr(self.minimum_peers_number))

    def is_satisfied(self, node):
        output = node.get_metrics_strict()
        match = self.metric_regex.search(output)
        if match is None:
            raise UnexpectedMetricsOutputFormatError(output)
        peers = int(match[1])
        return peers >= self.minimum_peers_number


class BlockContainsString:
    def __init__(self, block_hash, expected_string):
        self.block_hash = block_hash
        self.expected_string = expected_string

    def __str__(self):
        return '{}({})'.format(self.__class__.__name__, repr(self.block_hash, self.expected_string))

    def is_satisfied(self, node):
        block = node.get_block(self.block_hash)
        return self.expected_string in block


class BlocksCountAtLeast:
    def __init__(self, blocks_count, max_retrieved_blocks):
        self.blocks_count = blocks_count
        self.max_retrieved_blocks = max_retrieved_blocks

    def __str__(self):
        return '{}({})'.format(self.__class__.__name__, repr(self.blocks_count, self.max_retrieved_blocks))

    def is_satisfied(self, node):
        actual_blocks_count = node.get_blocks_count(self.max_retrieved_blocks)
        return actual_blocks_count >= expected_blocks_count


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

    logging.warning("Giving up on {} after {}s".format(repr(wait_condition.__doc__), elapsed))
    pytest.fail(error_message)


def wait_for_block_contains(node, block_hash, expected_string, timeout):
    predicate = BlockContainsString(block_hash, expected_string)
    wait_for_node(
        node,
        predicate,
        timeout,
        'Node {} failed to satisfy {}'.format(node, predicate),
    )


def wait_for_blocks_count_at_least(node, expected_blocks_count, max_retrieved_blocks, timeout):
    predicate = BlocksCountAtLeast(expected_blocks_count, max_retrieved_blocks)
    wait_for_node(
        node,
        predicate,
        timeout,
        'Node {} failed to satisfy {}'.format(node, predicate),
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


def wait_for_approved_block_received(network: 'Network', timeout: int):
    predicate = ApprovedBlockReceived()
    for peer in network.peers:
        wait_for_node(
            peer,
            predicate,
            timeout,
            "Peer {} failed to satisfy {}".format(peer.name, predicate),
        )


def wait_for_started_network(node_startup_timeout: int, network: 'Network'):
    for peer in network.peers:
        wait_for_node_started(peer, node_start_timeout)


def wait_for_converged_network(timeout: int, network: 'Network', peer_connections: int):
    bootstrap_predicate = HasAtLeastPeers(len(network.peers))
    wait_for_node(
        network.bootstrap,
        bootstrap_predicate,
        timeout,
        "Node {} failed to satisfy {}".format(network.bootstrap, bootstrap_predicate),
    )

    peer_predicate = HasAtLeastPeers(peer_connections)
    for node in network.peers:
        wait_for_node(
            node,
            peer_predicate,
            timeout,
            "Node {} failed to satisfy {}".format(node.name, peer_predicate),
        )
