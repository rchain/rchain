from random import Random
import pytest
from docker.client import DockerClient

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

BOOTSTRAP_NODE_KEYS = conftest.KeyPair(private_key='ff2ba092524bafdbc85fa0c7eddb2b41c69bc9bf066a4711a8a16f749199e5be', public_key='e2d64cea324a5a890326fc64c302cae1829d34ff4a584ff6a18e5026ad21f31f')
BONDED_VALIDATOR_KEY_1 = conftest.KeyPair(private_key='597623f0b50e82008d52644983699724538b4307efbe3d0b992c651ca7f860c8', public_key='e8efbecd4f7067ec163a16e0ca6cf9b8c832b9cc9d2d1cc0ca1a642ca630aefb')
BONDED_VALIDATOR_KEY_2 = conftest.KeyPair(private_key='9a32ff7b7c6e25527e0b4e5bec70596c6094e6529d56bf61cbd1ca26d3e92b10', public_key='fc99ed51e788bd040d4685721c46aaa580de06190cb77e51e6e3e50d38b1c6a2')
BONDED_VALIDATOR_KEY_3 = conftest.KeyPair(private_key='af47862137d4e772f540029ae73ee01443c61288f3df9307a13d681de6ad2de4', public_key='a063cbb51f5d8b7e5b5eb9be3f8222c61287c84e0fc410e35c71c277ed12b9db')
BONDED_VALIDATOR_KEY_4 = conftest.KeyPair(private_key='2a6018851984203e0983f0671e94fcf649ec04b614e5924f435081f7d1e3b44b', public_key='95c812922f7e2b956fec27d0eadcd4beb6856c775795c2407aff376261972f1e')


@pytest.mark.xfail
def test_fault_tolerance(command_line_options: CommandLineOptions, docker_client: DockerClient) -> None:
    assert False


def test_catch_up_next_round(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    peers_keypairs = [BONDED_VALIDATOR_KEY_1, BONDED_VALIDATOR_KEY_2, BONDED_VALIDATOR_KEY_3, BONDED_VALIDATOR_KEY_4]
    contract_path = '/opt/docker/examples/tut-hello.rho'
    with conftest.testing_context(command_line_options, random_generator, docker_client, bootstrap_keypair=BOOTSTRAP_NODE_KEYS, peers_keypairs=peers_keypairs) as context:
        with docker_network_with_started_bootstrap(context=context) as bootstrap_node:
            with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-1', keypair=BONDED_VALIDATOR_KEY_1) as validator1:
                with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-2', keypair=BONDED_VALIDATOR_KEY_2) as validator2:
                    with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-3', keypair=BONDED_VALIDATOR_KEY_3) as validator3:
                        wait_for_peers_count_at_least(context, validator1, 3)
                        wait_for_peers_count_at_least(context, validator2, 3)
                        wait_for_peers_count_at_least(context, validator3, 3)

                        deploy1 = DeployThread("validator1", validator1, contract_path, 10)
                        deploy1.start()

                        deploy2 = DeployThread("validator2", validator2, contract_path, 10)
                        deploy2.start()

                        deploy3 = DeployThread("validator3", validator3, contract_path, 10)
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

                        with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-4', keypair=BONDED_VALIDATOR_KEY_4) as validator4:

                            deploy4 = DeployThread("catch_up", validator1, contract_path, 1)
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

                            validator1_vdag = validator1.get_vdag()
                            validator2_vdag = validator2.get_vdag()
                            validator3_vdag = validator3.get_vdag()
                            validator4_vdag = validator4.get_vdag()

                            assert validator1_vdag == validator4_vdag
                            assert validator2_vdag == validator4_vdag
                            assert validator3_vdag == validator4_vdag


@pytest.mark.xfail
def test_catch_up(command_line_options: CommandLineOptions, docker_client: DockerClient) -> None:
    assert False
