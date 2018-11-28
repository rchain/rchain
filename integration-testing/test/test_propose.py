import os
from rnode_testing.wait import (
    wait_for_approved_block_received_handler_state,
)

from typing import TYPE_CHECKING
if TYPE_CHECKING:
    from conftest import System
    from rnode_testing.rnode import Node


def test_propose(system: "System", bootstrap_node: "Node") -> None:
    wait_for_approved_block_received_handler_state(bootstrap_node, system.config.node_startup_timeout)
    relative_paths = bootstrap_node.shell_out('sh', '-c', 'ls /opt/docker/examples/*.rho').splitlines()
    for relative_path in relative_paths:
        full_path = os.path.join('/opt/docker/examples', relative_path)
        bootstrap_node.deploy(full_path)
        bootstrap_node.propose()
