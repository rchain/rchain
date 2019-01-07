import os
from random import Random

from docker.client import DockerClient

from .common import (
    CommandLineOptions,
)
from .conftest import testing_context
from .rnode import docker_network_with_started_bootstrap
from .wait import (
    wait_for_approved_block_received_handler_state,
)


def test_propose(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient):
    with testing_context(command_line_options, random_generator, docker_client) as context:
        with docker_network_with_started_bootstrap(context=context) as bootstrap_node:
            wait_for_approved_block_received_handler_state(context, bootstrap_node)
            relative_paths = bootstrap_node.shell_out('sh', '-c', 'ls /opt/docker/examples/*.rho').splitlines()
            for relative_path in relative_paths:
                full_path = os.path.join('/opt/docker/examples', relative_path)
                bootstrap_node.deploy(full_path)
                bootstrap_node.propose()
