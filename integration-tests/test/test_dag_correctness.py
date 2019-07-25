from random import Random
import pytest
from docker.client import DockerClient
from rchain.crypto import PrivateKey

from . import conftest

from .rnode import (
    bootstrap_connected_peer,
    docker_network_with_started_bootstrap,
)
from .wait import (
    wait_for_peers_count_at_least,
    wait_for_blocks_count_at_least,
)
from .common import (
    CommandLineOptions,
)

from .test_multiple_deploys import DeployThread

BOOTSTRAP_NODE_KEYS = PrivateKey.from_hex("ff2ba092524bafdbc85fa0c7eddb2b41c69bc9bf066a4711a8a16f749199e5be")
BONDED_VALIDATOR_KEY_1 = PrivateKey.from_hex("597623f0b50e82008d52644983699724538b4307efbe3d0b992c651ca7f860c8")
BONDED_VALIDATOR_KEY_2 = PrivateKey.from_hex("9a32ff7b7c6e25527e0b4e5bec70596c6094e6529d56bf61cbd1ca26d3e92b10")
BONDED_VALIDATOR_KEY_3 = PrivateKey.from_hex("af47862137d4e772f540029ae73ee01443c61288f3df9307a13d681de6ad2de4")
BONDED_VALIDATOR_KEY_4 = PrivateKey.from_hex("2a6018851984203e0983f0671e94fcf649ec04b614e5924f435081f7d1e3b44b")


@pytest.mark.xfail
def test_fault_tolerance(command_line_options: CommandLineOptions, docker_client: DockerClient) -> None:
    assert False


@pytest.mark.xfail
def test_catch_up_next_round(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    peers_keypairs = [BONDED_VALIDATOR_KEY_1, BONDED_VALIDATOR_KEY_2, BONDED_VALIDATOR_KEY_3, BONDED_VALIDATOR_KEY_4]
    contract_path = '/opt/docker/examples/tut-hello.rho'
    with conftest.testing_context(command_line_options, random_generator, docker_client, bootstrap_key=BOOTSTRAP_NODE_KEYS, peers_keys=peers_keypairs) as context, \
        docker_network_with_started_bootstrap(context=context) as bootstrap_node, \
        bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-1', private_key=BONDED_VALIDATOR_KEY_1) as validator1, \
        bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-2', private_key=BONDED_VALIDATOR_KEY_2) as validator2, \
        bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-3', private_key=BONDED_VALIDATOR_KEY_3) as validator3:
            wait_for_peers_count_at_least(context, validator1, 3)
            wait_for_peers_count_at_least(context, validator2, 3)
            wait_for_peers_count_at_least(context, validator3, 3)

            deploy1 = DeployThread("validator1", validator1, contract_path, 10, BONDED_VALIDATOR_KEY_1)
            deploy1.start()

            deploy2 = DeployThread("validator2", validator2, contract_path, 10, BONDED_VALIDATOR_KEY_2)
            deploy2.start()

            deploy3 = DeployThread("validator3", validator3, contract_path, 10, BONDED_VALIDATOR_KEY_3)
            deploy3.start()


            deploy1.join()
            deploy2.join()
            deploy3.join()

            expected_blocks_count = 31
            wait_for_blocks_count_at_least(
                context,
                validator1,
                expected_blocks_count,
            )
            wait_for_blocks_count_at_least(
                context,
                validator2,
                expected_blocks_count,
            )
            wait_for_blocks_count_at_least(
                context,
                validator3,
                expected_blocks_count,
            )

            with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-4', private_key=BONDED_VALIDATOR_KEY_4) as validator4:

                deploy4 = DeployThread("catch_up", validator1, contract_path, 1, BONDED_VALIDATOR_KEY_1)
                deploy4.start()
                deploy4.join()

                wait_for_blocks_count_at_least(
                    context,
                    validator1,
                    expected_blocks_count + 1,
                )
                wait_for_blocks_count_at_least(
                    context,
                    validator2,
                    expected_blocks_count + 1,
                )
                wait_for_blocks_count_at_least(
                    context,
                    validator3,
                    expected_blocks_count + 1,
                )
                wait_for_blocks_count_at_least(
                    context,
                    validator4,
                    expected_blocks_count + 1,
                )

                validator1_vdag = validator1.get_parsed_mvdag()
                validator2_vdag = validator2.get_parsed_mvdag()
                validator3_vdag = validator3.get_parsed_mvdag()
                validator4_vdag = validator4.get_parsed_mvdag()

                assert validator1_vdag == validator4_vdag
                assert validator2_vdag == validator4_vdag
                assert validator3_vdag == validator4_vdag


@pytest.mark.xfail
def test_catch_up(command_line_options: CommandLineOptions, docker_client: DockerClient) -> None:
    assert False
