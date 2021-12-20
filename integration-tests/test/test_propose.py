from random import Random
import os
import shutil
from typing import Generator
from contextlib import contextmanager
import pytest
from rchain.client import RClientException
from rchain.crypto import PrivateKey
from docker.client import DockerClient


from .common import (
    CommandLineOptions,
)

from .conftest import (
    testing_context,
)
from .rnode import (
    Node,
    started_bootstrap_with_network,
)
from .common import (
    ParsingError
)
from .wait import (
    wait_for_approved_block_received_handler_state
)

USER_KEY = PrivateKey.from_hex("b2527b00340a83e302beae2a8daf6d654e8e57541acfa261cc1b5635eb16aa15")

FIX_COST_RHO_CONTRACTS = {
    "contract_1.rho": 97,
    "contract_2.rho": 197,
    "contract_3.rho": 329,
    "contract_4.rho": 782,
    "contract_5.rho": 3892,
}

@contextmanager
def start_node(command_line_options: CommandLineOptions, docker_client: DockerClient, random_generator: Random, min_phlo_price: int = 1) -> Generator[Node, None, None]:
    genesis_vault = {
        USER_KEY: 5000000000
    }

    with testing_context(command_line_options, random_generator, docker_client, wallets_dict=genesis_vault) as context, \
            started_bootstrap_with_network(context=context, min_phlo_price=min_phlo_price) as bootstrap:
            wait_for_approved_block_received_handler_state(context, bootstrap)
            yield bootstrap


def test_propose_cost(command_line_options: CommandLineOptions, docker_client: DockerClient, random_generator: Random) -> None:
    with start_node(command_line_options, docker_client, random_generator) as bootstrap:
        rho_contract, contract_cost = random_generator.choice(list(FIX_COST_RHO_CONTRACTS.items()))
        shutil.copyfile(os.path.join('resources/cost', rho_contract), os.path.join(bootstrap.local_deploy_dir, rho_contract))
        container_contract_file_path = os.path.join(bootstrap.remote_deploy_dir, rho_contract)
        bootstrap.deploy(container_contract_file_path, USER_KEY, 100000000, 1)
        block_hash = bootstrap.propose()
        block_info = bootstrap.get_block(block_hash)
        deploys = block_info.deploys
        assert contract_cost == deploys[0].cost


def test_find_block_by_deploy_id(command_line_options: CommandLineOptions, docker_client: DockerClient, random_generator: Random) -> None:
    with start_node(command_line_options, docker_client, random_generator) as bootstrap:
        relative_paths = bootstrap.shell_out('sh', '-c', 'ls /opt/docker/examples/*.rho').splitlines()
        relative_path = random_generator.choice(relative_paths)
        full_path = os.path.join('/opt/docker/examples', relative_path)
        deploy_id = bootstrap.deploy(full_path, USER_KEY, 100000000, 1)
        block_hash = bootstrap.propose()
        block_info = bootstrap.find_deploy(deploy_id)

        assert block_info.blockHash == block_hash


def test_deploy_invalid_contract(command_line_options: CommandLineOptions, docker_client: DockerClient, random_generator: Random) -> None:
    with start_node(command_line_options, docker_client, random_generator) as bootstrap:
        shutil.copyfile('resources/invalid.rho', os.path.join(bootstrap.local_deploy_dir, 'invalid.rho'))
        invalid_contract_path = os.path.join(bootstrap.remote_deploy_dir, 'invalid.rho')


        with pytest.raises(ParsingError):
            bootstrap.deploy(invalid_contract_path, USER_KEY)

        bootstrap.deploy('/opt/docker/examples/hello_world_again.rho', USER_KEY, 100000000, 1)
        block_hash = bootstrap.propose()
        block_info = bootstrap.get_block(block_hash)
        assert len(block_info.deploys) == 1


def test_deploy_phlo_price_too_small(command_line_options: CommandLineOptions, docker_client: DockerClient, random_generator: Random) -> None:
    deploy_phlo_price = 1  # less than min_phlo_price
    min_phlo_price = 10
    with start_node(command_line_options, docker_client, random_generator, min_phlo_price=min_phlo_price) as bootstrap:
        contract = 'contract.rho'
        shutil.copyfile(f'resources/{contract}', os.path.join(bootstrap.local_deploy_dir, contract))
        container_contract_file_path = os.path.join(bootstrap.remote_deploy_dir, contract)

        with pytest.raises(RClientException) as ex:
            bootstrap.deploy(container_contract_file_path, USER_KEY, 100000000, deploy_phlo_price)
            assert ex == f'Phlo price is less than minimum price {min_phlo_price}.'
