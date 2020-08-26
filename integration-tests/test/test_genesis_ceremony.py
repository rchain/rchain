from random import Random

from rchain.crypto import PrivateKey
from docker.client import DockerClient

from .conftest import (
    testing_context,
)
from .common import (
    CommandLineOptions,
)
from .rnode import (
    started_peer,
    started_bootstrap_with_network,
)
from .wait import (
    wait_for_block_approval,
    wait_for_approved_block_received_handler_state,
    wait_for_sent_approved_block,
)



CEREMONY_MASTER_KEYPAIR = PrivateKey.from_hex("80366db5fbb8dad7946f27037422715e4176dda41d582224db87b6c3b783d709")
VALIDATOR_A_KEYPAIR = PrivateKey.from_hex("120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5")
VALIDATOR_B_KEYPAIR = PrivateKey.from_hex("1f52d0bce0a92f5c79f2a88aae6d391ddf853e2eb8e688c5aa68002205f92dad")
VALIDATOR_C_KEYPAIR = PrivateKey.from_hex("5322dbb1828bdb44cb3275d35672bb3453347ee249a1f32d3c035e5ec3bfad1a")
READONLY_A_KEYPAIR = PrivateKey.from_hex("632a21e0176c4daed1ca78f08f98885f61d2050e0391e31eae59ff1a35ccca7f")


def test_successful_genesis_ceremony(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    """
    https://docs.google.com/document/d/1Z5Of7OVVeMGl2Fw054xrwpRmDmKCC-nAoIxtIIHD-Tc/
    """
    bootstrap_cli_options = {
        '--deploy-timestamp'   : '1',
        '--required-signatures': '2',
        '--approve-duration'   : '1min',
        '--approve-interval'   : '10sec',
    }
    peers_cli_flags = {'--genesis-validator'}
    peers_cli_options = {
        '--deploy-timestamp'   : '1',
        '--required-signatures': '2',
    }
    peers_keypairs = [
        VALIDATOR_A_KEYPAIR,
        VALIDATOR_B_KEYPAIR,
    ]

    wallet_map = {
        CEREMONY_MASTER_KEYPAIR: 10000,
        VALIDATOR_A_KEYPAIR: 10000,
        VALIDATOR_B_KEYPAIR: 10000,
        VALIDATOR_C_KEYPAIR: 10000
    }

    with testing_context(command_line_options, random_generator, docker_client, bootstrap_key=CEREMONY_MASTER_KEYPAIR, peers_keys=peers_keypairs, wallets_dict=wallet_map) as context, \
        started_bootstrap_with_network(context=context, cli_options=bootstrap_cli_options) as ceremony_master, \
        started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='validator-a', private_key=VALIDATOR_A_KEYPAIR, cli_flags=peers_cli_flags, cli_options=peers_cli_options) as validator_a, \
        started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='validator-b', private_key=VALIDATOR_B_KEYPAIR, cli_flags=peers_cli_flags, cli_options=peers_cli_options) as validator_b, \
        started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='readonly-a', private_key=READONLY_A_KEYPAIR) as readonly_a:
            wait_for_block_approval(context, ceremony_master)
            wait_for_approved_block_received_handler_state(context, ceremony_master)
            wait_for_sent_approved_block(context, ceremony_master)
            wait_for_approved_block_received_handler_state(context, validator_a)
            wait_for_approved_block_received_handler_state(context, validator_b)

            assert ceremony_master.get_blocks_count(2) == 1
            assert validator_a.get_blocks_count(2) == 1
            assert validator_b.get_blocks_count(2) == 1

            ceremony_master_blocks = ceremony_master.get_blocks(2)
            assert len(ceremony_master_blocks) == 1
            ceremony_master_genesis_block = ceremony_master_blocks[0]
            assert len(ceremony_master_genesis_block.parentsHashList) == 0

            validator_a_blocks = validator_a.get_blocks(2)
            assert len(validator_a_blocks) == 1
            validator_a_genesis_block = validator_a_blocks[0]
            assert validator_a_genesis_block.blockHash == ceremony_master_genesis_block.blockHash
            assert len(validator_a_genesis_block.parentsHashList) == 0

            validator_b_blocks = validator_b.get_blocks(2)
            assert len(validator_b_blocks) == 1
            validator_b_genesis_block = validator_b_blocks[0]
            assert validator_b_genesis_block.blockHash == ceremony_master_genesis_block.blockHash
            assert len(validator_b_genesis_block.parentsHashList) == 0

            wait_for_approved_block_received_handler_state(context, readonly_a)

