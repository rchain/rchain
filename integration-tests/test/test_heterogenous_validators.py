from random import Random

from docker.client import DockerClient
import pytest

from . import conftest
from .common import (
    KeyPair,
    CommandLineOptions,
)
from .rnode import (
    bootstrap_connected_peer,
    docker_network_with_started_bootstrap,
)
from .wait import (
    wait_for_node_sees_block,
    wait_for_peers_count_at_least,
    wait_for_blocks_count_at_least,
)


BOOTSTRAP_NODE_KEYPAIR = KeyPair(private_key='80366db5fbb8dad7946f27037422715e4176dda41d582224db87b6c3b783d709', public_key='04126107bc353c73e044fb21a5085aeafeecd69895fc05ec5033764a586bf044ddb19da5140a00912d892bfe8e10aa34eb7f9a68308646c3ac8804096ba605c2d2')
BONDED_VALIDATOR_KEYPAIR = KeyPair(private_key='120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5', public_key='0412ce31a3c3cbf9c69c098e593568c476a6bf7efdf9f7579c80e5328af05db7693b077d04fabbed28bb4e2d28aaba4ee50af6eddfab957c9c3c16d629c9d6aac3')
JOINING_VALIDATOR_KEYPAIR = KeyPair(private_key='1f52d0bce0a92f5c79f2a88aae6d391ddf853e2eb8e688c5aa68002205f92dad', public_key='04f42348554ab10387739d6f709ddba0eb9b80792f57ed68a1c9341635c0777590e9dbdd316c57cff51587f2f320e30605e6641e042f030b83aaaa3a3268a00fb0')
READONLY_PEER_KEYPAIR = KeyPair(private_key='2bdedd2e4dd2e7b5f176b7a5bc155f10fafd3fbd9c03fb7556f2ffd22c786f8b', public_key='0409becddaedb5f6063e815073bf958450352ea8d88435769b78792ad759d3587b81500898eb9e8c683a43b030bccbde3293b6478eab44ba8a48c4201ff4866ed8')

@pytest.mark.skip(reason="Skipped because we don't support bonding/unbonding in the new PoS yet")
def test_heterogenous_validators(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    BONDED_VALIDATOR_BLOCKS = JOINING_VALIDATOR_BLOCKS = 10
    with conftest.testing_context(command_line_options, random_generator, docker_client, bootstrap_keypair=BOOTSTRAP_NODE_KEYPAIR, peers_keypairs=[BONDED_VALIDATOR_KEYPAIR]) as context:
        with docker_network_with_started_bootstrap(context=context, cli_flags=set(['--has-faucet'])) as bootstrap_node:
            with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator', keypair=BONDED_VALIDATOR_KEYPAIR) as bonded_validator:
                wait_for_peers_count_at_least(context, bonded_validator, 1)
                contract_path = '/opt/docker/examples/hello_world_again.rho'
                for _ in range(BONDED_VALIDATOR_BLOCKS):
                    bonded_validator.deploy(contract_path, BOOTSTRAP_NODE_KEYPAIR.private_key)
                    bonded_validator.propose()

                with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='joining-validator', keypair=JOINING_VALIDATOR_KEYPAIR) as joining_validator:
                    joining_validator.generate_faucet_bonding_deploys(
                        bond_amount=123,
                        private_key=JOINING_VALIDATOR_KEYPAIR.private_key,
                        public_key=JOINING_VALIDATOR_KEYPAIR.public_key,
                    )
                    wait_for_peers_count_at_least(context, bonded_validator, 2)
                    forward_file = joining_validator.cat_forward_file(public_key=JOINING_VALIDATOR_KEYPAIR.public_key)
                    bond_file = joining_validator.cat_bond_file(public_key=JOINING_VALIDATOR_KEYPAIR.public_key)
                    bonded_validator.deploy_string(forward_file, BONDED_VALIDATOR_KEYPAIR.private_key)
                    bonded_validator.propose()
                    bonded_validator.deploy_string(bond_file, BONDED_VALIDATOR_KEYPAIR.private_key)
                    bonding_block_hash = bonded_validator.propose()
                    wait_for_node_sees_block(context, joining_validator, bonding_block_hash)
                    for _ in range(JOINING_VALIDATOR_BLOCKS):
                        joining_validator.deploy(contract_path, JOINING_VALIDATOR_KEYPAIR.private_key)
                        joining_validator.propose()

                    with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='readonly-peer', keypair=READONLY_PEER_KEYPAIR) as readonly_peer:
                        wait_for_peers_count_at_least(context, bonded_validator, 3)
                        # Force sync with the network
                        joining_validator.deploy(contract_path, JOINING_VALIDATOR_KEYPAIR.private_key)
                        joining_validator.propose()
                        expected_blocks_count = BONDED_VALIDATOR_BLOCKS + JOINING_VALIDATOR_BLOCKS + 2
                        wait_for_blocks_count_at_least(
                            context,
                            readonly_peer,
                            expected_blocks_count,
                        )
