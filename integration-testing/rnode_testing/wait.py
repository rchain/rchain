import re
import sys
import time
import logging

import pytest
import psutil
import typing_extensions


from typing import TYPE_CHECKING
if TYPE_CHECKING:
    from rnode_testing.rnode import Node
    from rnode_testing.network import Network


class PredicateProtocol(typing_extensions.Protocol):
    def __str__(self) -> str:
        ...

    def is_satisfied(self) -> bool:
        ...


class NodePredicateProtocol(typing_extensions.Protocol):
    def __str__(self) -> str:
        ...

    def is_satisfied(self, node: 'Node') -> bool:
        ...


class LogsContainMessage:
    def __init__(self, message: str) -> None:
        self.message = message

    def __str__(self) -> str:
        return '{}({})'.format(self.__class__.__name__, repr(self.message))

    def is_satisfied(self, node: 'Node') -> bool:
        return self.message in node.logs()


class NodeStarted(LogsContainMessage):
    def __init__(self) -> None:
        super().__init__('coop.rchain.node.NodeRuntime - Listening for traffic on rnode')


class ApprovedBlockReceivedHandlerStateEntered(LogsContainMessage):
    def __init__(self) -> None:
        super().__init__('Making a transition to ApprovedBlockRecievedHandler state.')


class ApprovedBlockReceived(LogsContainMessage):
    def __init__(self) -> None:
        super().__init__('Valid ApprovedBlock received!')


class HasAtLeastPeers:
    def __init__(self, minimum_peers_number) -> None:
        self.minimum_peers_number = minimum_peers_number
        self.metric_regex = re.compile(r"^peers (\d+).0\s*$", re.MULTILINE | re.DOTALL)

    def __str__(self) -> str:
        return '{}({})'.format(self.__class__.__name__, repr(self.minimum_peers_number))

    def is_satisfied(self, node) -> bool:
        output = node.get_metrics_strict()
        match = self.metric_regex.search(output)
        if match is None:
            return False
        peers = int(match[1])
        return peers >= self.minimum_peers_number


class BlockContainsString:
    def __init__(self, block_hash, expected_string) -> None:
        self.block_hash = block_hash
        self.expected_string = expected_string

    def __str__(self) -> str:
        args = ', '.join(repr(a) for a in (self.block_hash, self.expected_string))
        return '{}({})'.format(self.__class__.__name__, args)

    def is_satisfied(self, node: 'Node') -> bool:
        block = node.get_block(self.block_hash)
        return self.expected_string in block


class BlocksCountAtLeast:
    def __init__(self, blocks_count, max_retrieved_blocks) -> None:
        self.blocks_count = blocks_count
        self.max_retrieved_blocks = max_retrieved_blocks

    def __str__(self) -> str:
        args = ', '.join(repr(a) for a in (self.blocks_count, self.max_retrieved_blocks))
        return '{}({})'.format(self.__class__.__name__, args)

    def is_satisfied(self, node: 'Node') -> bool:
        actual_blocks_count = node.get_blocks_count(self.max_retrieved_blocks)
        return actual_blocks_count >= self.blocks_count


class FixedNodeShim:
    def __init__(self, node: 'Node', predicate: NodePredicateProtocol) -> None:
        self.node = node
        self.predicate = predicate

    def __str__(self) -> str:
        args = ', '.join(repr(a) for a in (self.node, self.predicate))
        return '{}({})'.format(self.__class__.__name__, args)

    def is_satisfied(self) -> bool:
        return self.predicate.is_satisfied(self.node)


def wait_on_node_using_user_time(node: 'Node', predicate: NodePredicateProtocol, timeout: int, timeout_message: str) -> None:
    # It is easy to precisely measure the time spent in user mode only on
    # Linux for a dockerized process; on other platforms we fall back on
    # measuring wall-clock time as an approximation.
    if sys.platform != 'linux':
        shim = FixedNodeShim(node, predicate)
        return wait_on_using_wall_clock_time(shim, timeout, timeout_message)

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


def wait_on_using_wall_clock_time(predicate: PredicateProtocol, timeout: int, error_message: str) -> None:
    logging.info("Waiting on: {}".format(predicate))
    elapsed = 0
    while elapsed < timeout:
        start_time = time.time()

        is_satisfied = predicate.is_satisfied()
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

    logging.warning("Giving up on {} after {}s".format(predicate, elapsed))
    pytest.fail(error_message)


def wait_for_block_contains(node, block_hash, expected_string, timeout):
    predicate = BlockContainsString(block_hash, expected_string)
    wait_on_node_using_user_time(
        node,
        predicate,
        timeout,
        'Node {} failed to satisfy {}'.format(node, predicate),
    )


def wait_for_blocks_count_at_least(node, expected_blocks_count, max_retrieved_blocks, timeout):
    predicate = BlocksCountAtLeast(expected_blocks_count, max_retrieved_blocks)
    wait_on_node_using_user_time(
        node,
        predicate,
        timeout,
        'Node {} failed to satisfy {}'.format(node, predicate),
    )


def wait_for_node_started(node: 'Node', startup_timeout: int):
    wait_on_node_using_user_time(
        node,
        NodeStarted(),
        startup_timeout,
        'Node {} did not start within {}s'.format(node.name, startup_timeout),
    )


def wait_for_approved_block_received_handler_state(node: 'Node', timeout: int):
    wait_on_node_using_user_time(
        node,
        ApprovedBlockReceivedHandlerStateEntered(),
        timeout,
        "Node {} did not enter ApprovedBlockRecievedHandler state".format(node.name),
    )


def wait_for_approved_block_received(network: 'Network', timeout: int):
    predicate = ApprovedBlockReceived()
    for peer in network.peers:
        wait_on_node_using_user_time(
            peer,
            predicate,
            timeout,
            "Peer {} failed to satisfy {}".format(peer.name, predicate),
        )


def wait_for_started_network(node_startup_timeout: int, network: 'Network'):
    for peer in network.peers:
        wait_for_node_started(peer, node_startup_timeout)


def wait_for_converged_network(timeout: int, network: 'Network', peer_connections: int):
    bootstrap_predicate = HasAtLeastPeers(len(network.peers))
    wait_on_node_using_user_time(
        network.bootstrap,
        bootstrap_predicate,
        timeout,
        "Node {} failed to satisfy {}".format(network.bootstrap, bootstrap_predicate),
    )

    peer_predicate = HasAtLeastPeers(peer_connections)
    for node in network.peers:
        wait_on_node_using_user_time(
            node,
            peer_predicate,
            timeout,
            "Node {} failed to satisfy {}".format(node.name, peer_predicate),
        )
