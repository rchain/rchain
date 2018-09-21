import logging
import pytest

from tools.rnode import create_bootstrap_node, create_peer_nodes
from tools.wait import wait_for, string_contains, node_logs, network_converged
from fixtures.datastructures import RChain
from fixtures.common import docker, config, validators_data
import tools.random

@pytest.fixture(scope="function")
def stress_docker_network(docker):
    network_name = f"rchain-{tools.random.random_string(5).lower()}"

    docker.networks.create(network_name, driver="bridge")

    yield network_name

    for network in docker.networks.list():
        if network_name == network.name:
            logging.info(f"removing {network.name}")
            network.remove()

@pytest.fixture(scope="function")
def stress_bootstrap(docker, stress_docker_network, config, validators_data):
    bonds_file, bootstrap_keys, _ = validators_data
    node = create_bootstrap_node(docker, stress_docker_network, bonds_file, bootstrap_keys, config.rnode_timeout)

    yield node

    node.cleanup()

@pytest.fixture(scope="function")
def stress_started_bootstrap(config, stress_bootstrap):
    wait_for( string_contains( node_logs(stress_bootstrap),
                               "coop.rchain.node.NodeRuntime - Listening for traffic on rnode"),
              config.node_startup_timeout,
              "Bootstrap node didn't start correctly")
    yield stress_bootstrap

@pytest.fixture(scope="function")
def complete_network(config, docker, stress_started_bootstrap, stress_docker_network, validators_data):
    logging.debug(f"Docker network = {stress_docker_network}")

    bonds_file, _, peer_keys = validators_data
    peers = create_peer_nodes(docker, stress_started_bootstrap, stress_docker_network, bonds_file, peer_keys, config.rnode_timeout)

    yield RChain(network = stress_docker_network, bootstrap = stress_started_bootstrap, peers = peers)

    for peer in peers:
        peer.cleanup()

@pytest.fixture(scope="function")
def started_complete_network(config, complete_network):
    for peer in complete_network.peers:
        wait_for( string_contains(node_logs(peer),
                                  "coop.rchain.node.NodeRuntime - Listening for traffic on rnode"),
                  config.node_startup_timeout,
                  f"Peer node {peer.name} didn't start correctly")

    yield complete_network


@pytest.fixture(scope="function")
def converged_complete_network(config, started_complete_network):
    for node in started_complete_network.nodes:
        wait_for( network_converged( node, len(started_complete_network.peers)),
                  config.network_converge_timeout,
                  "The network did NOT converge. Check container logs for issues. One or more containers might have failed to start or connect.")

    yield started_complete_network