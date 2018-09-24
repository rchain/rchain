import pytest
from tools.network import network, started_network, converged_network


@pytest.fixture(scope="package")
def complete_network(config, docker, started_bootstrap, docker_network, validators_data):
    with network(config, docker, started_bootstrap, docker_network, validators_data) as fixture:
        yield fixture



@pytest.fixture(scope="package")
def started_complete_network(config, complete_network):
    with started_network(config,  complete_network) as fixture:
        yield fixture



@pytest.fixture(scope="package")
def converged_complete_network(config, started_complete_network):
    with converged_network(config, started_complete_network, len(started_complete_network.peers)) as fixture:
        yield fixture