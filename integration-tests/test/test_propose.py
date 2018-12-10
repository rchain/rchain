import os

import pytest

from .conftest import testing_context
from .rnode import docker_network_with_started_bootstrap
from .wait import (
    wait_for_approved_block_received_handler_state,
)

from typing import TYPE_CHECKING

def test_propose(command_line_options_fixture, docker_client_fixture):
    with testing_context(command_line_options_fixture, docker_client_fixture) as context:
        with docker_network_with_started_bootstrap(context=context) as bootstrap_node:
            wait_for_approved_block_received_handler_state(bootstrap_node, context.receive_timeout)
            relative_paths = bootstrap_node.shell_out('sh', '-c', 'ls /opt/docker/examples/*.rho').splitlines()
            for relative_path in relative_paths:
                full_path = os.path.join('/opt/docker/examples', relative_path)
                bootstrap_node.deploy(full_path)
                bootstrap_node.propose()
