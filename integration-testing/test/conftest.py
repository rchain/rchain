import pytest
import collections
from tools.profiling import log_prof_data
from tools.util import parse_config, docker, validators_data
from tools.rnode import start_bootstrap

System = collections.namedtuple("System", ["config", "docker", "validators_data"])

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
    parser.addoption(
        "--receive-timeout", action="store", default="0", help="timeout in seconds for receiving a message. Defaults to 10 + peer_count * 10"
    )
    parser.addoption(
        "--rnode-timeout", action="store", default="10", help="timeout in seconds for executing an rnode call (Examples: propose, show-logs etc.). Defaults to 10s"
    )
    parser.addoption(
        "--blocks", action="store", default="1", help="The number of deploys per test deploy"
    )



@pytest.fixture(scope="session")
def system(request):
    cfg = parse_config(request)

    with docker() as docker_client, validators_data(cfg) as vd:
        try:
            yield System(cfg, docker_client, vd)
        finally:
            log_prof_data()


@pytest.fixture(scope="module")
def bootstrap_node(system):
    with start_bootstrap(system.docker,
                         system.config.node_startup_timeout,
                         system.config.rnode_timeout,
                         system.validators_data) as node:
        yield node
