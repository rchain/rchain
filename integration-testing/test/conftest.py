import logging
import pytest
import tools.random as random

from tools.rnode import create_bootstrap_node, get_rnode_address, create_peer_nodes
from tools.wait import wait_for, contains, container_logs, network_converged

import collections

def pytest_addoption(parser):
    parser.addoption(
        "--peer-count", action="store", default="3", help="number of peers in the network (excluding bootstrap node)"
    )



RChain = collections.namedtuple("RChain", ["network", "bootstrap", "peers"])

Config = collections.namedtuple( "Config",
                                 [
                                     "peer_count",
                                     "node_startup_timeout",
                                     "network_converge_timeout"
                                 ])

def cleanup(*containers):
    for container in containers:
        logging.info("=" * 100)
        logging.info(f"Docker container logs for {container.name}:")
        logging.info("=" * 100)
        logs = container.logs().decode('utf-8').splitlines()
        for log_line in logs:
            logging.info(f"{container.name}: {log_line}")

        logging.info("=" * 100)

    for container in containers:
        logging.info(f"Remove container {container.name}")
        container.remove(force=True, v=True)

def parse_config(request):
    peer_count = int(request.config.getoption("--peer-count"))
    return Config(peer_count = peer_count,
                  node_startup_timeout = 20 + peer_count * 10,
                  network_converge_timeout = 200 + peer_count * 2
                  )

@pytest.fixture(scope="module")
def config(request):
    return parse_config(request)


@pytest.fixture(scope="module")
def docker():
    import docker

    docker_client = docker.from_env()

    yield docker_client

    logging.info("Remove unused volumes")
    docker_client.volumes.prune()


@pytest.fixture(scope="module")
def docker_network(docker):
    network_name = f"rchain-{random.random_string(5)}"

    docker.networks.create(network_name, driver="bridge")

    yield network_name

    for network in docker.networks.list():
        if network_name == network.name:
            logging.info(f"removing {network.name}")
            network.remove()


@pytest.fixture(scope="module")
def bootstrap(docker, docker_network):
    node = create_bootstrap_node(docker, docker_network)

    yield node

    cleanup(node)

@pytest.fixture(scope="module")
def started_bootstrap(config, bootstrap):
    assert wait_for( contains( container_logs(bootstrap),
                               "coop.rchain.node.NodeRuntime - Starting stand-alone node."),
                     config.node_startup_timeout), \
        "Bootstrap node didn't start correctly"
    yield bootstrap

@pytest.fixture(scope="module")
def rchain_network(config, docker, started_bootstrap, docker_network):
    logging.debug(f"Docker network = {docker_network}")

    bootstrap_address  = get_rnode_address(started_bootstrap)

    peers = create_peer_nodes(docker, config.peer_count, bootstrap_address, docker_network)

    yield RChain(network = docker_network, bootstrap = started_bootstrap, peers = peers)

    cleanup(*peers)

@pytest.fixture(scope="module")
def started_rchain_network(config, rchain_network):
    for peer in rchain_network.peers:
        assert wait_for( contains(container_logs(peer),
                                  "kamon.prometheus.PrometheusReporter - Started the embedded HTTP server on http://0.0.0.0:40403"),
                         config.node_startup_timeout), \
            "Prometeus port is not started "

    yield rchain_network


@pytest.fixture(scope="module")
def converged_network(config, started_rchain_network):
    assert wait_for( network_converged(started_rchain_network.bootstrap, len(started_rchain_network.peers)),
                     config.network_converge_timeout), \
        "The network did NOT converge. Check container logs for issues. One or more containers might have failed to start or connect."

    yield started_rchain_network