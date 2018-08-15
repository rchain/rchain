import logging
import pytest
import docker
import tools.random as random

from tools.rnode import create_bootstrap_node, get_rnode_address, create_peer_nodes
from tools.wait import wait_for, contains, container_logs, network_converged

import collections

docker_client = docker.from_env()

RChain = collections.namedtuple("RChain", ["network", "bootstrap", "peers"])

Config = collections.namedtuple( "Config",
                                 [
                                     "peer_count",
                                     "bootstrap_startup_timeout",
                                     "network_converge_timeout"
                                 ])
@pytest.fixture
def config(request):
    peer_count = int(request.config.getoption("--peer-count"))
    return Config(peer_count = peer_count,
                bootstrap_startup_timeout = 60,
                network_converge_timeout = 200
                )


@pytest.fixture
def docker_network():
    network_name = f"rchain-{random.random_string(5)}"

    docker_client.networks.create(network_name, driver="bridge")

    yield network_name

    for network in docker_client.networks.list():
        if network_name == network.name:
            print(f"removing {network.name}")
            network.remove()

@pytest.fixture
def converged_network(config, docker_network):
    logging.debug(f"Docker network = {docker_network}")

    bootstrap = create_bootstrap_node(docker_network)

    assert wait_for( contains( container_logs(bootstrap),
                               "coop.rchain.node.NodeRuntime - Starting stand-alone node."),
                     config.bootstrap_startup_timeout, 2), \
        "Bootstrap node didn't start correctly"

    bootstrap_address  = get_rnode_address(bootstrap)

    peers = create_peer_nodes(config.peer_count, bootstrap_address, docker_network)

    assert wait_for( network_converged(bootstrap, config.peer_count),
                     config.network_converge_timeout, 10), \
        "The network did NOT converge. Check container logs for issues. One or more containers might have failed to start or connect."


    yield RChain(docker_network, bootstrap, peers)

    containers = [bootstrap] + peers

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

    logging.info("Remove unused volumes")
    docker_client.volumes.prune()