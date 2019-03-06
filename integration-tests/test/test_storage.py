from random import Random
import shutil
import os
import re
from docker.client import DockerClient

from .common import (
    CommandLineOptions,
    random_string,
)

from .conftest import testing_context
from .rnode import Node, docker_network_with_started_bootstrap
from .wait import wait_for_log_match

def deploy_contract_with_substitution(node: Node, substitute_target: str, expected_string: str, contract_name: str) -> str:
    local_contract_file_path = os.path.join('resources/storage', contract_name)
    shutil.copyfile(local_contract_file_path, f"{node.local_deploy_dir}/{contract_name}")
    container_contract_file_path = '{}/{}'.format(node.remote_deploy_dir, contract_name)
    node.shell_out(
        'sed',
        '-i',
        '-e', 's/@{}@/{}/g'.format(substitute_target, expected_string),
        container_contract_file_path,
    )
    node.deploy(container_contract_file_path)
    block_hash = node.propose()
    return block_hash


def test_data_is_stored_and_served_by_node(command_line_options: CommandLineOptions, docker_client: DockerClient, random_generator: Random) -> None:
    with testing_context(command_line_options, random_generator, docker_client) as context:
        with docker_network_with_started_bootstrap(context=context) as bootstrap_node:
            string_length = 20
            random_data = random_string(context, string_length)

            store_pattern = re.compile('"Store data (?P<data>[a-zA-Z]*) in rho:id:(?P<id_address>[a-zA-Z0-9]*)"')
            read_pattern = re.compile('"Read data (?P<data>[a-zA-Z]*) from (?P<id_address>[a-zA-Z0-9]*)"')

            deploy_contract_with_substitution(bootstrap_node, "store_data", random_data, "store-data.rho")

            wait_for_log_match(context, bootstrap_node, store_pattern)

            id_search = store_pattern.search(bootstrap_node.logs())

            if id_search:
                id_address = id_search.group('id_address')

            deploy_contract_with_substitution(bootstrap_node, "id_address", id_address, "read-data.rho")

            wait_for_log_match(context, bootstrap_node, read_pattern)

            data_search = read_pattern.search(bootstrap_node.logs())

            if data_search:
                read_data = data_search.group('data')

            assert read_data == random_data

