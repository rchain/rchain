import os
import pytest


def test_propose(bootstrap_node):
    relative_paths = bootstrap_node.shell_out('sh', '-c', 'ls /opt/docker/examples/*.rho').splitlines()
    for relative_path in relative_paths:
        full_path = os.path.join('/opt/docker/examples', relative_path)
        bootstrap_node.deploy(full_path)
        bootstrap_node.propose()
