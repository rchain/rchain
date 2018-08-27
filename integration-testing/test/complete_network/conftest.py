import pytest
import fixture

@pytest.fixture(scope="session")
def config(request):
    return fixture.parse_config(request)


@pytest.fixture(scope="session")
def docker():
    with fixture.docker() as docker_client:
        yield docker_client



@pytest.fixture(scope="package")
def docker_network(docker):
    with fixture.docker_network(docker) as docker_network:
        yield docker_network


@pytest.fixture(scope="package")
def bootstrap(docker, docker_network):
    with fixture.bootstrap(docker, docker_network) as bootstrap:
        yield bootstrap

@pytest.fixture(scope="package")
def started_bootstrap(config, bootstrap):
    with fixture.started_node(config, bootstrap) as started_bootstrap:
        yield started_bootstrap

@pytest.fixture(scope="package")
def rchain_network(config, docker, started_bootstrap, docker_network):
    with fixture.rchain_network(config, docker, started_bootstrap, docker_network) as rchain_network:
        yield rchain_network

@pytest.fixture(scope="package")
def started_rchain_network(config, rchain_network):
    with fixture.started_rchain_network(config, rchain_network) as started_rchain_network:
        yield started_rchain_network


@pytest.fixture(scope="package")
def converged_network(config, started_rchain_network):
    with fixture.converged_network(config, started_rchain_network) as converged_network:
        yield converged_network