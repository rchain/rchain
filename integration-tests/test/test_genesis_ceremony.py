from random import Random

from docker.client import DockerClient

from .conftest import (
    testing_context,
    temporary_wallets_file,
)
from .common import (
    KeyPair,
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


CEREMONY_MASTER_KEYPAIR = KeyPair(private_key='80366db5fbb8dad7946f27037422715e4176dda41d582224db87b6c3b783d709', public_key='04126107bc353c73e044fb21a5085aeafeecd69895fc05ec5033764a586bf044ddb19da5140a00912d892bfe8e10aa34eb7f9a68308646c3ac8804096ba605c2d2')
VALIDATOR_A_KEYPAIR = KeyPair(private_key='120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5', public_key='0412ce31a3c3cbf9c69c098e593568c476a6bf7efdf9f7579c80e5328af05db7693b077d04fabbed28bb4e2d28aaba4ee50af6eddfab957c9c3c16d629c9d6aac3')
VALIDATOR_B_KEYPAIR = KeyPair(private_key='1f52d0bce0a92f5c79f2a88aae6d391ddf853e2eb8e688c5aa68002205f92dad', public_key='04f42348554ab10387739d6f709ddba0eb9b80792f57ed68a1c9341635c0777590e9dbdd316c57cff51587f2f320e30605e6641e042f030b83aaaa3a3268a00fb0')
VALIDATOR_C_KEYPAIR = KeyPair(private_key='5322dbb1828bdb44cb3275d35672bb3453347ee249a1f32d3c035e5ec3bfad1a', public_key='04a8a7f3cc3ddcd8ddd511605c69d516b9097f613a433a92556bc4bf357b58530392a80509a56458abacc97275854be6ea44308f4a848c015de6d8ea0e6e89f159')
READONLY_A_KEYPAIR = KeyPair(private_key='632a21e0176c4daed1ca78f08f98885f61d2050e0391e31eae59ff1a35ccca7f', public_key='040d09c2c290d458d666df9be22fe77cc71711bc052656bc089662c803ad61568a647585acda4974ea63dd7f820d1f349a498684e2527941140c5e0386441a2177')


def test_successful_genesis_ceremony(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    """
    https://docs.google.com/document/d/1Z5Of7OVVeMGl2Fw054xrwpRmDmKCC-nAoIxtIIHD-Tc/
    """
    bootstrap_cli_options = {
        '--deploy-timestamp': '1',
        '--required-sigs': '2',
        '--duration': '5min',
        '--interval': '10sec',
    }
    peers_cli_flags = set(['--genesis-validator'])
    peers_cli_options = {
        '--deploy-timestamp': '1',
        '--required-sigs': '2',
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
        '--deploy-timestamp': '1',
        '--required-sigs': '2',
        '--duration': '5min',
        '--interval': '10sec',
    }
    peers_cli_flags = set(['--genesis-validator'])
    peers_cli_options = {
        '--deploy-timestamp': '1',
        '--required-sigs': '2',
    }
    peers_keypairs = [
        VALIDATOR_A_KEYPAIR,
        VALIDATOR_B_KEYPAIR,
        VALIDATOR_C_KEYPAIR
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

                        with started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='validator-c', keypair=VALIDATOR_C_KEYPAIR) as validator_c:
                            wait_for_approved_block_received_handler_state(context, validator_c)
                            assert validator_c.get_blocks_count(2) == 1
                            validator_c_blocks = validator_c.show_blocks_parsed(2)
                            assert len(validator_c_blocks) == 1
                            validator_c_genesis_block = validator_c_blocks[0]
                            assert validator_c_genesis_block['blockHash'] == ceremony_master_genesis_block['blockHash']
                            assert validator_c_genesis_block['mainParentHash'] == ''

                            validator_c_genesis_block_info = validator_c.show_block_parsed(validator_c_genesis_block['blockHash'].strip('"'))
                            validator_c_bonds_validator_stake = extract_validator_stake_from_bonds_validator_str(validator_c_genesis_block_info['bondsValidatorList'])
                            assert VALIDATOR_A_KEYPAIR.public_key in validator_c_bonds_validator_stake
                            assert VALIDATOR_B_KEYPAIR.public_key in validator_c_bonds_validator_stake
