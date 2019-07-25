from random import Random

from docker.client import DockerClient
from rchain.crypto import PrivateKey

from .conftest import (
    testing_context,
    temporary_wallets_file,
)
from .common import (
    CommandLineOptions,
)
from .rnode import (
    started_peer,
    ready_bootstrap,
    extract_validator_stake_from_bonds_validator_str,
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
        '--deploy-timestamp':   '1',
        '--required-sigs':      '2',
        '--duration':           '5min',
        '--interval':           '10sec',
    }
    peers_cli_flags = set(['--genesis-validator'])
    peers_cli_options = {
        '--deploy-timestamp':   '1',
        '--required-sigs':      '2',
    }
    peers_keypairs = [
        VALIDATOR_A_KEYPAIR,
        VALIDATOR_B_KEYPAIR,
    ]

    with testing_context(command_line_options, random_generator, docker_client, bootstrap_key=CEREMONY_MASTER_KEYPAIR, peers_keys=peers_keypairs) as context, \
        temporary_wallets_file(context.random_generator, [context.bootstrap_key] + context.peers_keys) as wallets, \
        ready_bootstrap(context=context, cli_options=bootstrap_cli_options, wallets_file=wallets) as ceremony_master, \
        started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='validator-a', private_key=VALIDATOR_A_KEYPAIR, wallets_file=wallets, cli_flags=peers_cli_flags, cli_options=peers_cli_options) as validator_a, \
        started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='validator-b', private_key=VALIDATOR_B_KEYPAIR, wallets_file=wallets, cli_flags=peers_cli_flags, cli_options=peers_cli_options) as validator_b, \
        started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='readonly-a', private_key=READONLY_A_KEYPAIR) as readonly_a:
            wait_for_block_approval(context, ceremony_master)
            wait_for_approved_block_received_handler_state(context, ceremony_master)
            wait_for_sent_approved_block(context, ceremony_master)
            wait_for_approved_block_received_handler_state(context, validator_a)
            wait_for_approved_block_received_handler_state(context, validator_b)

            assert ceremony_master.get_blocks_count(2) == 1
            assert validator_a.get_blocks_count(2) == 1
            assert validator_b.get_blocks_count(2) == 1

            ceremony_master_blocks = ceremony_master.show_blocks_parsed(2)
            assert len(ceremony_master_blocks) == 1
            ceremony_master_genesis_block = ceremony_master_blocks[0]
            assert ceremony_master_genesis_block['mainParentHash'] == ''

            validator_a_blocks = validator_a.show_blocks_parsed(2)
            assert len(validator_a_blocks) == 1
            validator_a_genesis_block = validator_a_blocks[0]
            assert validator_a_genesis_block['blockHash'] == ceremony_master_genesis_block['blockHash']
            assert validator_a_genesis_block['mainParentHash'] == ''

            validator_b_blocks = validator_b.show_blocks_parsed(2)
            assert len(validator_b_blocks) == 1
            validator_b_genesis_block = validator_b_blocks[0]
            assert validator_b_genesis_block['blockHash'] == ceremony_master_genesis_block['blockHash']
            assert validator_b_genesis_block['mainParentHash'] == ''

            wait_for_approved_block_received_handler_state(context, readonly_a)


def test_validator_catching_up(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    bootstrap_cli_options = {
        '--deploy-timestamp':   '1',
        '--required-sigs':      '2',
        '--duration':           '5min',
        '--interval':           '10sec',
    }
    peers_cli_flags = set(['--genesis-validator'])
    peers_cli_options = {
        '--deploy-timestamp':   '1',
        '--required-sigs':      '2',
    }
    peers_keypairs = [
        VALIDATOR_A_KEYPAIR,
        VALIDATOR_B_KEYPAIR,
        VALIDATOR_C_KEYPAIR
    ]
    with testing_context(command_line_options, random_generator, docker_client, bootstrap_key=CEREMONY_MASTER_KEYPAIR, peers_keys=peers_keypairs) as context, \
        temporary_wallets_file(context.random_generator, [context.bootstrap_key] + context.peers_keys) as wallets, \
        ready_bootstrap(context=context, cli_options=bootstrap_cli_options, wallets_file=wallets) as ceremony_master, \
        started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='validator-a', private_key=VALIDATOR_A_KEYPAIR, wallets_file=wallets, cli_flags=peers_cli_flags, cli_options=peers_cli_options) as validator_a, \
        started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='validator-b', private_key=VALIDATOR_B_KEYPAIR, wallets_file=wallets, cli_flags=peers_cli_flags, cli_options=peers_cli_options) as validator_b:
            wait_for_block_approval(context, ceremony_master)
            wait_for_approved_block_received_handler_state(context, ceremony_master)
            wait_for_sent_approved_block(context, ceremony_master)
            wait_for_approved_block_received_handler_state(context, validator_a)
            wait_for_approved_block_received_handler_state(context, validator_b)

            assert ceremony_master.get_blocks_count(2) == 1
            assert validator_a.get_blocks_count(2) == 1
            assert validator_b.get_blocks_count(2) == 1

            ceremony_master_blocks = ceremony_master.show_blocks_parsed(2)
            assert len(ceremony_master_blocks) == 1
            ceremony_master_genesis_block = ceremony_master_blocks[0]
            assert ceremony_master_genesis_block['mainParentHash'] == ''

            validator_a_blocks = validator_a.show_blocks_parsed(2)
            assert len(validator_a_blocks) == 1
            validator_a_genesis_block = validator_a_blocks[0]
            assert validator_a_genesis_block['blockHash'] == ceremony_master_genesis_block['blockHash']
            assert validator_a_genesis_block['mainParentHash'] == ''

            validator_b_blocks = validator_b.show_blocks_parsed(2)
            assert len(validator_b_blocks) == 1
            validator_b_genesis_block = validator_b_blocks[0]
            assert validator_b_genesis_block['blockHash'] == ceremony_master_genesis_block['blockHash']
            assert validator_b_genesis_block['mainParentHash'] == ''

            with started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='validator-c', private_key=VALIDATOR_C_KEYPAIR) as validator_c:
                wait_for_approved_block_received_handler_state(context, validator_c)
                assert validator_c.get_blocks_count(2) == 1
                validator_c_blocks = validator_c.show_blocks_parsed(2)
                assert len(validator_c_blocks) == 1
                validator_c_genesis_block = validator_c_blocks[0]
                assert validator_c_genesis_block['blockHash'] == ceremony_master_genesis_block['blockHash']
                assert validator_c_genesis_block['mainParentHash'] == ''

                validator_c_genesis_block_info = validator_c.show_block_parsed(validator_c_genesis_block['blockHash'].strip('"'))
                validator_c_bonds_validator_stake = extract_validator_stake_from_bonds_validator_str(validator_c_genesis_block_info['bondsValidatorList'])
                assert VALIDATOR_A_KEYPAIR.get_public_key().to_hex() in validator_c_bonds_validator_stake
                assert VALIDATOR_B_KEYPAIR.get_public_key().to_hex()in validator_c_bonds_validator_stake