import re
import time
import logging
import pytest
import typing_extensions

from .common import (
    Node,
    Network,
    TestingContext,
    GetBlockError,
)


class PredicateProtocol(typing_extensions.Protocol):
    def __str__(self) -> str:
        # pylint: disable=pointless-statement
        ...

    def is_satisfied(self) -> bool:
        # pylint: disable=pointless-statement, no-self-use
        ...


class LogsContainMessage:
    def __init__(self, node: Node, message: str) -> None:
        self.node = node
        self.message = message

    def __str__(self) -> str:
        args = ', '.join(repr(a) for a in (self.node.name, self.message))
        return '<{}({})>'.format(self.__class__.__name__, args)

    def is_satisfied(self) -> bool:
        return self.message in self.node.logs()


class NodeStarted(LogsContainMessage):
    def __init__(self, node: Node) -> None:
        super().__init__(node, 'coop.rchain.node.NodeRuntime - Listening for traffic on rnode')


class ApprovedBlockReceivedHandlerStateEntered(LogsContainMessage):
    def __init__(self, node: Node) -> None:
        super().__init__(node, 'Making a transition to ApprovedBlockRecievedHandler state.')


class ApprovedBlockReceived(LogsContainMessage):
    def __init__(self, node: Node) -> None:
        super().__init__(node, 'Valid ApprovedBlock received!')


class SentUnapprovedBlock(LogsContainMessage):
    def __init__(self, node: Node) -> None:
        super().__init__(node, 'c.r.c.u.c.ApproveBlockProtocol$ApproveBlockProtocolImpl - APPROVAL: Sent UnapprovedBlock')


class HasAtLeastPeers:
    def __init__(self, node: Node, minimum_peers_number: int) -> None:
        self.node = node
        self.minimum_peers_number = minimum_peers_number
        self.metric_regex = re.compile(r"^rchain_comm_rp_connect_peers (\d+).0\s*$", re.MULTILINE | re.DOTALL)

    def __str__(self) -> str:
        args = ', '.join(repr(a) for a in (self.node.name, self.minimum_peers_number))
        return '<{}({})>'.format(self.__class__.__name__, args)

    def is_satisfied(self) -> bool:
        output = self.node.get_metrics()
        match = self.metric_regex.search(output)
        if match is None:
            return False
        peers = int(match[1])
        return peers >= self.minimum_peers_number


class NodeSeesBlock:
    def __init__(self, node: Node, block_hash: str) -> None:
        self.node = node
        self.block_hash = block_hash

    def __str__(self) -> str:
        args = ', '.join(repr(a) for a in (self.node.name, self.block_hash))
        return '<{}({})>'.format(self.__class__.__name__, args)

    def is_satisfied(self) -> bool:
        try:
            self.node.get_block(self.block_hash)
            return True
        except GetBlockError:
            return False

class BlockContainsString:
    def __init__(self, node: Node, block_hash: str, expected_string: str) -> None:
        self.node = node
        self.block_hash = block_hash
        self.expected_string = expected_string

    def __str__(self) -> str:
        args = ', '.join(repr(a) for a in (self.node.name, self.block_hash, self.expected_string))
        return '<{}({})>'.format(self.__class__.__name__, args)

    def is_satisfied(self) -> bool:
        try:
            block = self.node.get_block(self.block_hash)
            return self.expected_string in block
        except GetBlockError:
            return False


class BlocksCountAtLeast:
    def __init__(self, node: Node, blocks_count: int) -> None:
        self.node = node
        self.blocks_count = blocks_count

    def __str__(self) -> str:
        args = ', '.join(repr(a) for a in (self.node.name, self.blocks_count))
        return '<{}({})>'.format(self.__class__.__name__, args)

    def is_satisfied(self) -> bool:
        actual_blocks_count = self.node.get_blocks_count(self.blocks_count)
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

    logging.info("TIMEOUT %s", predicate)
    pytest.fail('Failed to satisfy {} after {}s'.format(predicate, elapsed))

def wait_for_node_sees_block(context: TestingContext, node: Node, block_hash: str) -> None:
    predicate = NodeSeesBlock(node, block_hash)
    wait_on_using_wall_clock_time(predicate, context.node_startup_timeout)


def wait_for_block_contains(context: TestingContext, node: Node, block_hash: str, expected_string: str) -> None:
    predicate = BlockContainsString(node, block_hash, expected_string)
    wait_on_using_wall_clock_time(predicate, context.receive_timeout)


def wait_for_blocks_count_at_least(context: TestingContext, node: Node, expected_blocks_count: int) -> None:
    predicate = BlocksCountAtLeast(node, expected_blocks_count)
    wait_on_using_wall_clock_time(predicate, context.receive_timeout * expected_blocks_count)


def wait_for_node_started(context: TestingContext, node: Node) -> None:
    predicate = NodeStarted(node)
    wait_on_using_wall_clock_time(predicate, context.node_startup_timeout)


def wait_for_approved_block_received_handler_state(context: TestingContext, node: Node) -> None:
    predicate = ApprovedBlockReceivedHandlerStateEntered(node)
    wait_on_using_wall_clock_time(predicate, context.node_startup_timeout)


def wait_for_approved_block_received(context: TestingContext, network: Network) -> None:
    for peer in network.peers:
        predicate = ApprovedBlockReceived(peer)
        wait_on_using_wall_clock_time(predicate, context.node_startup_timeout + context.receive_timeout)


def wait_for_started_network(context: TestingContext, network: Network) -> None:
    for peer in network.peers:
        wait_for_node_started(context, peer)


def wait_for_converged_network(context: TestingContext, network: Network, peer_connections: int) -> None:
    bootstrap_predicate = HasAtLeastPeers(network.bootstrap, len(network.peers))
    wait_on_using_wall_clock_time(bootstrap_predicate, context.network_converge_timeout)

    for peer in network.peers:
        peer_predicate = HasAtLeastPeers(peer, peer_connections)
        wait_on_using_wall_clock_time(peer_predicate, context.network_converge_timeout)


def wait_for_peers_count_at_least(context: TestingContext, node: Node, npeers: int) -> None:
    predicate = HasAtLeastPeers(node, npeers)
    wait_on_using_wall_clock_time(predicate, context.network_converge_timeout)


def wait_for_sent_unapproved_block(context: TestingContext, node: Node) -> None:
    predicate = SentUnapprovedBlock(node)
    wait_on_using_wall_clock_time(predicate, context.network_converge_timeout)
