import os

import conftest

from rnode_testing.rnode import docker_network_with_started_bootstrap


def test_eval(command_line_options_fixture, docker_client_fixture):
    with conftest.testing_context(command_line_options_fixture, docker_client_fixture) as context:
        with docker_network_with_started_bootstrap(context=context) as bootstrap_node:
            relative_paths = bootstrap_node.shell_out('sh', '-c', 'ls /opt/docker/examples/*.rho').splitlines()
            for relative_path in relative_paths:
                full_path = os.path.join('/opt/docker/examples', relative_path)
                bootstrap_node.eval(full_path)
