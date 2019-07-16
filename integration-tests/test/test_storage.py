from random import Random
import os
import re
from docker.client import DockerClient
from rchain.crypto import PrivateKey

from .common import (
    CommandLineOptions,
    random_string,
)

from .conftest import testing_context
from .rnode import docker_network_with_started_bootstrap
from .wait import wait_for_log_match

USER_KEY = PrivateKey.from_hex("3596e2e5fd14b24a6d84af04b7f0a8f13e3e68ee2ca91dc4b19550f12e61502c")

def test_data_is_stored_and_served_by_node(command_line_options: CommandLineOptions, docker_client: DockerClient, random_generator: Random) -> None:
    with testing_context(command_line_options, random_generator, docker_client) as context:
        with docker_network_with_started_bootstrap(context=context) as bootstrap_node:
            store_data_contract = os.path.join('resources/storage', "store-data.rho")
            read_data_contract = os.path.join('resources/storage', "read-data.rho")
            string_length = 20
            random_data = random_string(context, string_length)

            store_pattern = re.compile('"Store data (?P<data>[a-zA-Z]*) in rho:id:(?P<id_address>[a-zA-Z0-9]*)"')
            read_pattern = re.compile('"Read data (?P<data>[a-zA-Z]*) from (?P<id_address>[a-zA-Z0-9]*)"')

            bootstrap_node.deploy_contract_with_substitution({'@store_data@': random_data}, store_data_contract, USER_KEY)

            wait_for_log_match(context, bootstrap_node, store_pattern)

            id_match = store_pattern.search(bootstrap_node.logs()) # here always have a match otherwise the test would fail in the above line

            id_address = id_match.group('id_address') # type: ignore

            bootstrap_node.deploy_contract_with_substitution({'@id_address@': id_address}, read_data_contract, USER_KEY)

            wait_for_log_match(context, bootstrap_node, read_pattern)

            data_match = read_pattern.search(bootstrap_node.logs()) # here always have a match otherwise the test would fail in the above line

            read_data = data_match.group('data') # type: ignore

            assert read_data == random_data

