import logging
from contextlib import contextmanager
from rnode_testing.wait import (
    wait_for,
    has_peers,
    node_started,
    approved_block_received_handler_state,
    approved_block_received,
)
from rnode_testing.rnode import create_peer_nodes



class RChain:
    def __init__(self, network, bootstrap, peers):
        self.network = network
        self.bootstrap = bootstrap
        self.peers = peers
        self.nodes = [bootstrap] + peers


@contextmanager
def start_network(config, docker, bootstrap, validators_data, allowed_peers=None):
    logging.debug("Docker network = {}".format(bootstrap.network))

    peers = create_peer_nodes(
        docker_client=docker,
        bootstrap=bootstrap,
        network=bootstrap.network,
        bonds_file=validators_data.bonds_file,
        key_pairs=validators_data.peers_keys,
        rnode_timeout=config.rnode_timeout,
        allowed_peers=allowed_peers,
    )

    try:
        yield RChain(network=bootstrap.network, bootstrap=bootstrap, peers=peers)
    finally:
        for peer in peers:
            peer.cleanup()


def wait_for_approved_block_received_handler_state(bootstrap_node, node_startup_timeout):
    wait_for(
        approved_block_received_handler_state(bootstrap_node),
        node_startup_timeout,
        "Bootstrap node {} did not enter ApprovedBlockRecievedHandler state".format(bootstrap_node.name),
    )


def wait_for_approved_block_received(network, node_startup_timeout):
    for peer in network.peers:
        wait_for(
            approved_block_received(peer),
            node_startup_timeout,
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


def wait_for_approved_block_received(network, node_startup_timeout):
    for peer in network.peers:
        wait_for(
            approved_block_received(peer),
            node_startup_timeout,
            "Peer {} did not receive the approved block",
        )
