import pytest
from tools.network import network, started_network, converged_network

@pytest.fixture(scope="package")
def star_network(config, docker, started_bootstrap, docker_network, validators_data):
    with network(config, docker, started_bootstrap, docker_network, validators_data, [started_bootstrap.name]) as fixture:
        yield fixture


@pytest.fixture(scope="package")
def started_star_network(config, star_network):
    with started_network(config, star_network) as fixture:
        yield fixture


@pytest.fixture(scope="package")
def converged_star_network(config, started_star_network):
    with converged_network(config, started_star_network, 1) as fixture:
        yield fixture