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

BOOTSTRAP_NODE_KEYS = conftest.KeyPair(private_key='ff2ba092524bafdbc85fa0c7eddb2b41c69bc9bf066a4711a8a16f749199e5be', public_key='0444f16eee91c879a70a2d53e90b329670580395c8639ffef3f39ef74bdd9364279f877cd3d7cca806c815bd6fc568bf2fc0695a9c2cd6ac3d36fc1f4864243efb')
BONDED_VALIDATOR_KEY_1 = conftest.KeyPair(private_key='597623f0b50e82008d52644983699724538b4307efbe3d0b992c651ca7f860c8', public_key='04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519')
BONDED_VALIDATOR_KEY_2 = conftest.KeyPair(private_key='9a32ff7b7c6e25527e0b4e5bec70596c6094e6529d56bf61cbd1ca26d3e92b10', public_key='04ab4c08f1986bb40c57d6aa24a650a4122bd6afb6b77990a1447230fc428cefd1d8d51b75812e549e0e4f2289c8fea6389b1d26ce71a7204782d92ea6c9862a35')
BONDED_VALIDATOR_KEY_3 = conftest.KeyPair(private_key='af47862137d4e772f540029ae73ee01443c61288f3df9307a13d681de6ad2de4', public_key='04debd80cba7bff3f33cc8ca0520740060d3e770f6618d445c02746ea4cc320791b7aca71d9750b2805a8c930a9d568e46d5166f1d7502260e82e5e3a7a535b981')
BONDED_VALIDATOR_KEY_4 = conftest.KeyPair(private_key='2a6018851984203e0983f0671e94fcf649ec04b614e5924f435081f7d1e3b44b', public_key='04b178f0acdaa4e2e46ac3c534d3fe5c63d65dbafa05a73e678b3331f54c4ba91357d3f337b8db6234570469c9d3385c5c9c86a236d3f2a2dafe7579c9f2580942')


@pytest.mark.xfail
def test_fault_tolerance(command_line_options: CommandLineOptions, docker_client: DockerClient) -> None:
    assert False


@pytest.mark.xfail
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

                        deploy1 = DeployThread("validator1", validator1, contract_path, 10, BONDED_VALIDATOR_KEY_1.private_key)
                        deploy1.start()

                        deploy2 = DeployThread("validator2", validator2, contract_path, 10, BONDED_VALIDATOR_KEY_2.private_key)
                        deploy2.start()

                        deploy3 = DeployThread("validator3", validator3, contract_path, 10, BONDED_VALIDATOR_KEY_3.private_key)
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

                            deploy4 = DeployThread("catch_up", validator1, contract_path, 1, BONDED_VALIDATOR_KEY_1.private_key)
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
