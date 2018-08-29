import logging
import pytest

from tools.rnode import create_bootstrap_node, create_peer_nodes
from tools.wait import wait_for, string_contains, node_logs, network_converged
from tools.fixture import *

@pytest.fixture(scope="package")
def rchain_network(config, docker, started_bootstrap, docker_network, validators_data):
    logging.debug(f"Docker network = {docker_network}")

    bonds_file, _, peer_keys = validators_data
    peers = create_peer_nodes(docker, started_bootstrap, docker_network, bonds_file, peer_keys, config.rnode_timeout)

    yield RChain(network = docker_network, bootstrap = started_bootstrap, peers = peers)

    for peer in peers:
        peer.cleanup()

@pytest.fixture(scope="package")
def started_rchain_network(config, rchain_network):
    for peer in rchain_network.peers:
        wait_for( string_contains(node_logs(peer),
                                  "coop.rchain.node.NodeRuntime - Listening for traffic on rnode"),
                  config.node_startup_timeout,
                  f"Peer node {peer.name} didn't start correctly")

    yield rchain_network


@pytest.fixture(scope="package")
def converged_network(config, started_rchain_network):
    wait_for( network_converged( started_rchain_network.bootstrap, len(started_rchain_network.peers)),
              config.network_converge_timeout,
              "The network did NOT converge. Check container logs for issues. One or more containers might have failed to start or connect.")

    yield started_rchain_network