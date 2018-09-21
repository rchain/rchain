import pytest
from fixtures.common import *
from tools.rnode import create_bootstrap_node, create_peer_nodes
from tools.wait import wait_for, string_contains, node_logs, network_converged

@pytest.fixture(scope="package")
def bootstrap(docker, docker_network, config, validators_data):
    bonds_file, bootstrap_keys, _ = validators_data
    node = create_bootstrap_node(docker, docker_network, bonds_file, bootstrap_keys, config.rnode_timeout)

    yield node

    node.cleanup()

@pytest.fixture(scope="package")
def started_bootstrap(config, bootstrap):
    wait_for( string_contains( node_logs(bootstrap),
                               "coop.rchain.node.NodeRuntime - Listening for traffic on rnode"),
              config.node_startup_timeout,
              "Bootstrap node didn't start correctly")
    yield bootstrap