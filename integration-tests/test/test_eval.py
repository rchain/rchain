import os
from random import Random

from .rnode import (
    Node,
)


def test_eval(started_standalone_bootstrap_node: Node, random_generator: Random) -> None:
    relative_paths = started_standalone_bootstrap_node.shell_out('sh', '-c', 'ls /opt/docker/examples/*.rho').splitlines()
    relative_path = random_generator.choice(relative_paths)
    full_path = os.path.join('/opt/docker/examples', relative_path)
    started_standalone_bootstrap_node.eval(full_path)
