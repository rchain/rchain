import os

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from rnode_testing.rnode import Node


def test_eval(bootstrap_node: "Node") -> None:
    relative_paths = bootstrap_node.shell_out('sh', '-c', 'ls /opt/docker/examples/*.rho').splitlines()
    for relative_path in relative_paths:
        full_path = os.path.join('/opt/docker/examples', relative_path)
        bootstrap_node.eval(full_path)
