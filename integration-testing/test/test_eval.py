import os
import pytest
from tools.rnode import start_bootstrap
import logging


@pytest.fixture(scope="module")
def bootstrap_node(system):
    with start_bootstrap(system.docker,
                         system.config.node_startup_timeout,
                         system.config.rnode_timeout,
                         system.validators_data) as node:
        yield node


def test_eval(bootstrap_node):
    relative_paths = bootstrap_node.shell_out('sh', '-c', 'ls /opt/docker/examples/*.rho').splitlines()
    for relative_path in relative_paths:
        full_path = os.path.join('/opt/docker/examples', relative_path)
        bootstrap_node.eval(full_path)
