import os

import pytest

from rnode_testing.rnode import started_standalone_bootstrap_node
from rnode_testing.wait import (
    wait_for_approved_block_received_handler_state,
)

from typing import TYPE_CHECKING

def test_propose(started_standalone_bootstrap_node):
    wait_for_approved_block_received_handler_state(started_standalone_bootstrap_node, started_standalone_bootstrap_node.timeout)
    relative_paths = started_standalone_bootstrap_node.shell_out('sh', '-c', 'ls /opt/docker/examples/*.rho').splitlines()
    for relative_path in relative_paths:
        full_path = os.path.join('/opt/docker/examples', relative_path)
        started_standalone_bootstrap_node.deploy(full_path)
        started_standalone_bootstrap_node.propose()
