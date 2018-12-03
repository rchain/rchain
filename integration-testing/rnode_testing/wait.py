import re
import time
import logging

import pytest
import typing_extensions

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from rnode_testing.common import Network
    from rnode_testing.rnode import Node


class PredicateProtocol(typing_extensions.Protocol):
    def __str__(self) -> str:
        ...

    def is_satisfied(self) -> bool:
        ...


class LogsContainMessage:
    def __init__(self, node: 'Node', message: str) -> None:
        self.node = node
        self.message = message

    def __str__(self) -> str:
        args = ', '.join(repr(a) for a in (self.node.name, self.message))
        return '<{}({})>'.format(self.__class__.__name__, args)

    def is_satisfied(self) -> bool:
        return self.message in self.node.logs()


class NodeStarted(LogsContainMessage):
    def __init__(self, node: 'Node') -> None:
        super().__init__(node, 'coop.rchain.node.NodeRuntime - Listening for traffic on rnode')


class ApprovedBlockReceivedHandlerStateEntered(LogsContainMessage):
    def __init__(self, node: 'Node') -> None:
        super().__init__(node, 'Making a transition to ApprovedBlockRecievedHandler state.')


class ApprovedBlockReceived(LogsContainMessage):
    def __init__(self, node: 'Node') -> None:
        super().__init__(node, 'Valid ApprovedBlock received!')


class HasAtLeastPeers:
    def __init__(self, node: 'Node', minimum_peers_number: int) -> None:
        self.node = node
        self.minimum_peers_number = minimum_peers_number
        self.metric_regex = re.compile(r"^peers (\d+).0\s*$", re.MULTILINE | re.DOTALL)

    def __str__(self) -> str:
        args = ', '.join(repr(a) for a in (self.node.name, self.minimum_peers_number))
        return '<{}({})>'.format(self.__class__.__name__, args)

    def is_satisfied(self) -> bool:
        output = self.node.get_metrics_strict()
        match = self.metric_regex.search(output)
        if match is None:
            return False
        peers = int(match[1])
        return peers >= self.minimum_peers_number


class BlockContainsString:
    def __init__(self, node: 'Node', block_hash: str, expected_string: str) -> None:
        self.node = node
        self.block_hash = block_hash
        self.expected_string = expected_string

    def __str__(self) -> str:
        args = ', '.join(repr(a) for a in (self.node.name, self.block_hash, self.expected_string))
        return '<{}({})>'.format(self.__class__.__name__, args)

    def is_satisfied(self) -> bool:
        block = self.node.get_block(self.block_hash)
        return self.expected_string in block


class BlocksCountAtLeast:
    def __init__(self, node: 'Node', blocks_count: int, max_retrieved_blocks: int) -> None:
        self.node = node
        self.blocks_count = blocks_count
        self.max_retrieved_blocks = max_retrieved_blocks

    def __str__(self) -> str:
        args = ', '.join(repr(a) for a in (self.node.name, self.blocks_count, self.max_retrieved_blocks))
        return '<{}({})>'.format(self.__class__.__name__, args)

    def is_satisfied(self) -> bool:
        actual_blocks_count = self.node.get_blocks_count(self.max_retrieved_blocks)
        return actual_blocks_count >= self.blocks_count


def wait_on_using_wall_clock_time(predicate: PredicateProtocol, timeout: int) -> None:
    logging.info("AWAITING {}".format(predicate))

    elapsed = 0
    while elapsed < timeout:
        start_time = time.time()

        is_satisfied = predicate.is_satisfied()
        if is_satisfied:
            logging.info("SATISFIED {}".format(predicate))
            return

        condition_evaluation_duration = time.time() - start_time
        elapsed = int(elapsed + condition_evaluation_duration)
        time_left = timeout - elapsed

        # iteration duration is 15% of remaining timeout
        # but no more than 10s and no less than 1s
        iteration_duration = int(min(10, max(1, int(0.15 * time_left))))

        time.sleep(iteration_duration)
        elapsed = elapsed + iteration_duration

    pytest.fail('Failed to satisfy {} after {}s'.format(predicate, elapsed))


def wait_for_block_contains(node: 'Node', block_hash: str, expected_string: str, timeout: int):
    predicate = BlockContainsString(node, block_hash, expected_string)
    wait_on_using_wall_clock_time(predicate, timeout)


def wait_for_blocks_count_at_least(node: 'Node', expected_blocks_count: int, max_retrieved_blocks: int, timeout: int):
    predicate = BlocksCountAtLeast(node, expected_blocks_count, max_retrieved_blocks)
    wait_on_using_wall_clock_time(predicate, timeout)


def wait_for_node_started(node: 'Node', startup_timeout: int):
    predicate = NodeStarted(node)
    wait_on_using_wall_clock_time(predicate, startup_timeout)


def wait_for_approved_block_received_handler_state(node: 'Node', timeout: int):
    predicate = ApprovedBlockReceivedHandlerStateEntered(node)
    wait_on_using_wall_clock_time(predicate, timeout)


def wait_for_approved_block_received(network: 'Network', timeout: int):
    for peer in network.peers:
        predicate = ApprovedBlockReceived(peer)
        wait_on_using_wall_clock_time(predicate, timeout)


def wait_for_started_network(node_startup_timeout: int, network: 'Network'):
    for peer in network.peers:
        wait_for_node_started(peer, node_startup_timeout)


def wait_for_converged_network(timeout: int, network: 'Network', peer_connections: int):
    bootstrap_predicate = HasAtLeastPeers(network.bootstrap, len(network.peers))
    wait_on_using_wall_clock_time(bootstrap_predicate, timeout)

    for peer in network.peers:
        peer_predicate = HasAtLeastPeers(peer, peer_connections)
        wait_on_using_wall_clock_time(peer_predicate, timeout)
