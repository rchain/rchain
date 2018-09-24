from contextlib import contextmanager
from tools.wait import wait_for, string_contains, node_logs, network_converged
import logging
from tools.rnode import create_peer_nodes


class RChain:
    def __init__(self, network, bootstrap, peers):
        self.network = network
        self.bootstrap = bootstrap
        self.peers = peers
        self.nodes = [bootstrap] + peers


@contextmanager
def network(config, docker, bootstrap, docker_network, validators_data, allowed_peers=None):
    logging.debug(f"Docker network = {docker_network}")

    bonds_file, _, peer_keys = validators_data
    peers = create_peer_nodes(docker, bootstrap, docker_network, bonds_file, peer_keys, config.rnode_timeout, allowed_peers)

    yield RChain(network = docker_network, bootstrap = bootstrap, peers = peers)

    for peer in peers:
        peer.cleanup()


@contextmanager
def started_network(config, network):
    for peer in network.peers:
        wait_for( string_contains(node_logs(peer),
                                  "coop.rchain.node.NodeRuntime - Listening for traffic on rnode"),
                  config.node_startup_timeout,
                  f"Peer node {peer.name} didn't start correctly")

    yield network

@contextmanager
def converged_network(config, network, peer_connections):
    wait_for( network_converged( network.bootstrap, len(network.peers)),
              config.network_converge_timeout,
              "The network did NOT converge. Check container logs for issues. One or more containers might have failed to start or connect.")

    for node in network.peers:
        wait_for( network_converged( node, peer_connections),
                  config.network_converge_timeout,
                  "The network did NOT converge. Check container logs for issues. One or more containers might have failed to start or connect.")

    yield network