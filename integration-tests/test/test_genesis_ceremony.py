import pytest
from docker.client import DockerClient

from .common import (
    KeyPair,
    CommandLineOptions,
)



CEREMONY_MASTER_KEYPAIR = KeyPair(private_key='80366db5fbb8dad7946f27037422715e4176dda41d582224db87b6c3b783d709', public_key='1cd8bf79a2c1bd0afa160f6cdfeb8597257e48135c9bf5e4823f2875a1492c97')
VALIDATORA_KEYPAIR = KeyPair(private_key='120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5', public_key='02ab69930f74b931209df3ce54e3993674ab3e7c98f715608a5e74048b332821')
VALIDATORB_KEYPAIR = KeyPair(private_key='1f52d0bce0a92f5c79f2a88aae6d391ddf853e2eb8e688c5aa68002205f92dad', public_key='043c56051a613623cd024976427c073fe9c198ac2b98315a4baff9d333fbb42e')


@pytest.mark.xfail
def test_successful_genesis_ceremony(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    bootstrap_cli_options = {
        '--required-sigs':  '2',
        '--duration':       '5min',
        '--interval':       '10sec',
    }
    peers_keypairs = [
        VALIDATOR_A_KEYPAIR,
        VALIDATOR_B_KEYPAIR,
    ]
    with conftest.testing_context(command_line_options, random_generator, docker_client, bootstrap_keypair=CEREMONY_MASTER_KEYPAIR, peers_keypairs=peers_keypairs) as context:
        with ready_bootstrap(context=context, cli_options=bootstrap_cli_options) as bootstrap:
            with started_peer(context=context, network=bootstrap.network, bootstrap=bootstrap, name='validator-a', keypair=VALIDATOR_A_KEYPAIR):
                with started_peer(context=context, network=bootstrap.network, bootstrap=bootstrap, name='validator-b', keypair=VALIDATOR_B_KEYPAIR):
                    assert False
