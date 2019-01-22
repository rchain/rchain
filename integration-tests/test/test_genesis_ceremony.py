from random import Random

import pytest
from docker.client import DockerClient

from .conftest import (
    testing_context,
    temporary_wallets_file,
)
from .common import (
    KeyPair,
    CommandLineOptions,
    WaitTimeoutError,
)
from .rnode import (
    started_peer,
    ready_bootstrap,
)
from .wait import (
    wait_for_block_approval,
    wait_for_approved_block_received_handler_state,
    wait_for_sent_approved_block,
    wait_for_sent_unapproved_block,
    wait_for_approved_block_received_handler,
)



CEREMONY_MASTER_KEYPAIR = KeyPair(private_key='80366db5fbb8dad7946f27037422715e4176dda41d582224db87b6c3b783d709', public_key='1cd8bf79a2c1bd0afa160f6cdfeb8597257e48135c9bf5e4823f2875a1492c97')
VALIDATOR_A_KEYPAIR = KeyPair(private_key='120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5', public_key='02ab69930f74b931209df3ce54e3993674ab3e7c98f715608a5e74048b332821')
VALIDATOR_B_KEYPAIR = KeyPair(private_key='1f52d0bce0a92f5c79f2a88aae6d391ddf853e2eb8e688c5aa68002205f92dad', public_key='043c56051a613623cd024976427c073fe9c198ac2b98315a4baff9d333fbb42e')
READONLY_A_KEYPAIR = KeyPair(private_key='632a21e0176c4daed1ca78f08f98885f61d2050e0391e31eae59ff1a35ccca7f', public_key='15ab05a878ddce564865e591ff5927613b341e39572065dfeaa0b8b442b6084b')


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
    with testing_context(command_line_options, random_generator, docker_client, bootstrap_keypair=CEREMONY_MASTER_KEYPAIR, peers_keypairs=peers_keypairs) as context:
        with temporary_wallets_file(context.random_generator, [context.bootstrap_keypair] + context.peers_keypairs) as wallets:
            with ready_bootstrap(context=context, cli_options=bootstrap_cli_options, wallets_file=wallets) as ceremony_master:
                with started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='validator-a', keypair=VALIDATOR_A_KEYPAIR, wallets_file=wallets, cli_flags=peers_cli_flags, cli_options=peers_cli_options) as validator_a:
                    with started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='validator-b', keypair=VALIDATOR_B_KEYPAIR, wallets_file=wallets, cli_flags=peers_cli_flags, cli_options=peers_cli_options) as validator_b:
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
                        assert ceremony_master_genesis_block['mainParentHash'] == '""'

                        validator_a_blocks = validator_a.show_blocks_parsed(2)
                        assert len(validator_a_blocks) == 1
                        validator_a_genesis_block = validator_a_blocks[0]
                        assert validator_a_genesis_block['blockHash'] == ceremony_master_genesis_block['blockHash']
                        assert validator_a_genesis_block['mainParentHash'] == '""'

                        validator_b_blocks = validator_a.show_blocks_parsed(2)
                        assert len(validator_b_blocks) == 1
                        validator_b_genesis_block = validator_b_blocks[0]
                        assert validator_b_genesis_block['blockHash'] == ceremony_master_genesis_block['blockHash']
                        assert validator_b_genesis_block['mainParentHash'] == '""'



def test_successful_genesis_ceremony_with_read_only(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
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

    with testing_context(command_line_options, random_generator, docker_client, bootstrap_keypair=CEREMONY_MASTER_KEYPAIR, peers_keypairs=peers_keypairs) as context:
        with temporary_wallets_file(context.random_generator, [context.bootstrap_keypair] + context.peers_keypairs) as wallets:
            with ready_bootstrap(context=context, cli_options=bootstrap_cli_options, wallets_file=wallets) as ceremony_master:
                with started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='validator-a', keypair=VALIDATOR_A_KEYPAIR, wallets_file=wallets, cli_flags=peers_cli_flags, cli_options=peers_cli_options) as validator_a:
                    with started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='validator-b', keypair=VALIDATOR_B_KEYPAIR, wallets_file=wallets, cli_flags=peers_cli_flags, cli_options=peers_cli_options) as validator_b:
                        with started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='readonly-a', keypair=READONLY_A_KEYPAIR) as readonly_a:
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
                            assert ceremony_master_genesis_block['mainParentHash'] == '""'

                            validator_a_blocks = validator_a.show_blocks_parsed(2)
                            assert len(validator_a_blocks) == 1
                            validator_a_genesis_block = validator_a_blocks[0]
                            assert validator_a_genesis_block['blockHash'] == ceremony_master_genesis_block['blockHash']
                            assert validator_a_genesis_block['mainParentHash'] == '""'

                            validator_b_blocks = validator_a.show_blocks_parsed(2)
                            assert len(validator_b_blocks) == 1
                            validator_b_genesis_block = validator_b_blocks[0]
                            assert validator_b_genesis_block['blockHash'] == ceremony_master_genesis_block['blockHash']
                            assert validator_b_genesis_block['mainParentHash'] == '""'

                            wait_for_approved_block_received_handler_state(context, readonly_a)

def test_not_successful_genesis_ceremony(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    bootstrap_cli_options = {
        '--deploy-timestamp':   '1',
        '--required-sigs':      '3',
        '--duration':           '5min',
        '--interval':           '10sec',
    }
    peers_cli_flags = set(['--genesis-validator'])
    peers_cli_options = {
        '--deploy-timestamp':   '1',
        '--required-sigs':      '3',
    }
    peers_keypairs = [
        VALIDATOR_A_KEYPAIR,
        VALIDATOR_B_KEYPAIR,
    ]
    with testing_context(command_line_options, random_generator, docker_client, bootstrap_keypair=CEREMONY_MASTER_KEYPAIR, peers_keypairs=peers_keypairs) as context:
        with temporary_wallets_file(context.random_generator, [context.bootstrap_keypair] + context.peers_keypairs) as wallets:
            with ready_bootstrap(context=context, cli_options=bootstrap_cli_options, wallets_file=wallets) as ceremony_master:
                with started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='validator-a', keypair=VALIDATOR_A_KEYPAIR, wallets_file=wallets, cli_flags=peers_cli_flags, cli_options=peers_cli_options):
                    with started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='validator-b', keypair=VALIDATOR_B_KEYPAIR, wallets_file=wallets, cli_flags=peers_cli_flags, cli_options=peers_cli_options):
                        wait_for_sent_unapproved_block(context, ceremony_master)
                        with pytest.raises(WaitTimeoutError):
                            wait_for_approved_block_received_handler(context, ceremony_master)

@pytest.mark.xfail
def test_validator_catching_up(docker_client_session: DockerClient) -> None:
    assert False
