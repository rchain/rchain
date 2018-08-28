import logging
import pytest
import tools.random as random

from tools.rnode import create_bootstrap_node, create_peer_nodes
from tools.wait import wait_for, string_matches, node_logs, network_converged

import collections

def pytest_addoption(parser):
    parser.addoption(
        "--peer-count", action="store", default="2", help="number of peers in the network (excluding bootstrap node)"
    )
    parser.addoption(
        "--start-timeout", action="store", default="0", help="timeout in seconds for starting a node. Defaults to 30 + peer_count * 10"
    )
    parser.addoption(
        "--converge-timeout", action="store", default="0", help="timeout in seconds for network converge. Defaults to 200 + peer_count * 10"
    )



class RChain:
    def __init__(self, network, bootstrap, peers):
        self.network = network
        self.bootstrap = bootstrap
        self.peers = peers
        self.nodes = [bootstrap] + peers

Config = collections.namedtuple( "Config",
                                 [
                                     "peer_count",
                                     "node_startup_timeout",
                                     "network_converge_timeout"
                                 ])

def parse_config(request):
    peer_count = int(request.config.getoption("--peer-count"))
    start_timeout = int(request.config.getoption("--start-timeout"))
    converge_timeout = int(request.config.getoption("--converge-timeout"))
    return Config(peer_count = peer_count,
                  node_startup_timeout = start_timeout if start_timeout > 0 else 30 + peer_count * 10,
                  network_converge_timeout = converge_timeout if converge_timeout > 0 else 200 + peer_count * 2
                  )

@pytest.fixture(scope="session")
def config(request):
    return parse_config(request)


@pytest.fixture(scope="session")
def docker():
    import docker

    docker_client = docker.from_env()

    yield docker_client

    logging.info("Remove unused volumes")
    docker_client.volumes.prune()


@pytest.fixture(scope="package")
def docker_network(docker):
    network_name = f"rchain-{random.random_string(5)}"

    docker.networks.create(network_name, driver="bridge")

    yield network_name

    for network in docker.networks.list():
        if network_name == network.name:
            logging.info(f"removing {network.name}")
            network.remove()


@pytest.fixture(scope="package")
def bootstrap(docker, docker_network):
    node = create_bootstrap_node(docker, docker_network)

    yield node

    node.cleanup()

@pytest.fixture(scope="package")
def started_bootstrap(config, bootstrap):
    wait_for( string_matches( node_logs(bootstrap),
                              "coop.rchain.node.NodeRuntime - Listening for traffic on rnode"),
              config.node_startup_timeout,
        "Bootstrap node didn't start correctly")
    yield bootstrap

@pytest.fixture(scope="package")
def rchain_network(config, docker, started_bootstrap, docker_network):
    logging.debug(f"Docker network = {docker_network}")

    peers = create_peer_nodes(docker, config.peer_count, started_bootstrap, docker_network)

    yield RChain(network = docker_network, bootstrap = started_bootstrap, peers = peers)

    for peer in peers:
        peer.cleanup()

@pytest.fixture(scope="package")
def started_rchain_network(config, rchain_network):
    for peer in rchain_network.peers:
        wait_for( string_matches(node_logs(peer),
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