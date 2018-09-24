import pytest
from tools.network import network, started_network, converged_network
from tools.rnode import start_bootstrap


@pytest.fixture(scope="package")
def complete_network(config, docker, validators_data):
    with start_bootstrap(docker, config.node_startup_timeout, validators_data) as bootstrap_node:
        with network(config, docker, bootstrap_node, bootstrap_node.network, validators_data) as fixture:
            yield fixture



@pytest.fixture(scope="package")
def started_complete_network(config, complete_network):
    with started_network(config,  complete_network) as fixture:
        yield fixture



@pytest.fixture(scope="package")
def converged_complete_network(config, started_complete_network):
    with converged_network(config, started_complete_network, len(started_complete_network.peers)) as fixture:
        yield fixture


@pytest.fixture(scope="package")
def star_network(config, docker, validators_data):
    with start_bootstrap(docker, config.node_startup_timeout, validators_data) as bootstrap_node:
        with network(config, docker, bootstrap_node, bootstrap_node.network, validators_data, [bootstrap_node.name]) as fixture:
            yield fixture


@pytest.fixture(scope="package")
def started_star_network(config, star_network):
    with started_network(config, star_network) as fixture:
        yield fixture


@pytest.fixture(scope="package")
def converged_star_network(config, started_star_network):
    with converged_network(config, started_star_network, 1) as fixture:
        yield fixture