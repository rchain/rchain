import contextlib
import logging
import os
from random import Random
import subprocess
from typing import Generator
from test.conftest import (CommandLineOptions, docker_client_context,
                           testing_context)
from test.rnode import ready_bootstrap_with_network, started_peer, Node
from test.wait import (wait_for_approved_block_received_handler_state,
                       wait_for_sent_approved_block, wait_for_blocks_count_at_least)
from test.test_wallets import transfer_funds

from rchain.crypto import PrivateKey

CEREMONY_MASTER_PRIVATE = PrivateKey.from_hex("80366db5fbb8dad7946f27037422715e4176dda41d582224db87b6c3b783d709")
VALIDATOR_A_PRIVATE = PrivateKey.from_hex("120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5")
VALIDATOR_B_PRIVATE = PrivateKey.from_hex("1f52d0bce0a92f5c79f2a88aae6d391ddf853e2eb8e688c5aa68002205f92dad")


logging.getLogger('peers').setLevel(logging.INFO)
logging.root.setLevel(logging.INFO)


@contextlib.contextmanager
def generate_cmd_opts() -> Generator[CommandLineOptions, None, None]:
    yield CommandLineOptions(60 * 30, 60 * 30, 30, 60 * 30, None, None)


def get_docker_folder(node: Node, target_dir: str, output_file: str):
    logging.info("Retrieve the data file from {} container".format(node.name))
    if os.path.exists(output_file):
        os.remove(output_file)
    bits, _ = node.container.get_archive(target_dir)
    with open(output_file, 'wb') as f:
        for chunk in bits:
            f.write(chunk)

def get_current_commit_hash():
    p = subprocess.run(["git", "rev-parse", "HEAD"], capture_output=True, check=True)
    with open("resources/rnode_data/version", 'wb') as f:
        f.write(p.stdout)

def generate_rnode_data() -> None:
    """
    This function would start a 3 nodes rchain network with one bootstrap node and two validators and start the genesis ceremony.
    After the genesis ceremony is done, these node would try to propose to generate node data.
    After the data generated above, it would copy the data to current directory which can be reused to start an existing network.
    """
    peers_keypairs = [
        VALIDATOR_A_PRIVATE,
        VALIDATOR_B_PRIVATE,
    ]
    random_seed = Random()

    bonding_map = {
        CEREMONY_MASTER_PRIVATE: 100,
        VALIDATOR_A_PRIVATE: 110,
        VALIDATOR_B_PRIVATE: 90
    }

    wallet_map = {
        CEREMONY_MASTER_PRIVATE: 5000000,
        VALIDATOR_A_PRIVATE: 5000000,
        VALIDATOR_B_PRIVATE: 5000000
    }
    logging.info("start the genesis ceremony")

    with generate_cmd_opts() as command_line_options ,\
            docker_client_context() as  docker_cli, \
            testing_context(command_line_options, random_seed, docker_cli, bootstrap_key=CEREMONY_MASTER_PRIVATE, peers_keys=peers_keypairs, validator_bonds_dict=bonding_map, wallets_dict=wallet_map) as context, \
            ready_bootstrap_with_network(context=context) as ceremony_master, \
            started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='validator-a', private_key=VALIDATOR_A_PRIVATE, synchrony_constraint_threshold=0.33) as validator_a, \
            started_peer(context=context, network=ceremony_master.network, bootstrap=ceremony_master, name='validator-b', private_key=VALIDATOR_B_PRIVATE, synchrony_constraint_threshold=0.33) as validator_b:
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
        assert ceremony_master_genesis_block['mainParentHash'] == ''

        validator_a_blocks = validator_a.get_blocks(2)
        assert len(validator_a_blocks) == 1
        validator_a_genesis_block = validator_a_blocks[0]
        assert validator_a_genesis_block['blockHash'] == ceremony_master_genesis_block['blockHash']
        assert validator_a_genesis_block['mainParentHash'] == ''

        validator_b_blocks = validator_b.get_blocks(2)
        assert len(validator_b_blocks) == 1
        validator_b_genesis_block = validator_b_blocks[0]
        assert validator_b_genesis_block['blockHash'] == ceremony_master_genesis_block['blockHash']
        assert validator_b_genesis_block['mainParentHash'] == ''


        logging.info("Genesis ceremony is done, do some deploys and propose now")
        contract_path = '/opt/docker/examples/tut-hello.rho'
        for _ in range(3):
            ceremony_master.deploy(contract_path, VALIDATOR_A_PRIVATE)
            ceremony_master.propose()

            validator_a.deploy(contract_path, VALIDATOR_B_PRIVATE)
            validator_a.propose()

            validator_b.deploy(contract_path, VALIDATOR_B_PRIVATE)
            validator_b.propose()

        transfer_funds(context, ceremony_master, CEREMONY_MASTER_PRIVATE.get_public_key().get_rev_address(), VALIDATOR_A_PRIVATE.get_public_key().get_rev_address(), 100, CEREMONY_MASTER_PRIVATE, 100000, 1)
        transfer_funds(context, ceremony_master, VALIDATOR_A_PRIVATE.get_public_key().get_rev_address(), VALIDATOR_B_PRIVATE.get_public_key().get_rev_address(), 200, VALIDATOR_A_PRIVATE, 100000, 1)
        transfer_funds(context, ceremony_master, VALIDATOR_B_PRIVATE.get_public_key().get_rev_address(), CEREMONY_MASTER_PRIVATE.get_public_key().get_rev_address(), 300, VALIDATOR_B_PRIVATE, 100000, 1)
        # finally master: 1200, validator a: 900, validator b: 1900

        for _ in range(3):
            ceremony_master.deploy(contract_path, VALIDATOR_A_PRIVATE)
            ceremony_master.propose()

            validator_a.deploy(contract_path, VALIDATOR_B_PRIVATE)
            validator_a.propose()

            validator_b.deploy(contract_path, VALIDATOR_B_PRIVATE)
            validator_b.propose()

        wait_for_blocks_count_at_least(context, ceremony_master, 22)
        wait_for_blocks_count_at_least(context, validator_a, 22)
        wait_for_blocks_count_at_least(context, validator_b, 22)

        ceremony_master.container.stop()
        validator_a.container.stop()
        validator_b.container.stop()

        get_docker_folder(ceremony_master, '/var/lib/rnode', 'resources/rnode_data/bootstrap')
        get_docker_folder(validator_a, '/var/lib/rnode', 'resources/rnode_data/validatorA')
        get_docker_folder(validator_b, '/var/lib/rnode', 'resources/rnode_data/validatorB')


if __name__ == '__main__':
    generate_rnode_data()
    get_current_commit_hash()
