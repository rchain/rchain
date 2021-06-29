from random import Random

import pytest
from typing import List, Tuple

from rchain.crypto import PrivateKey
from docker.client import DockerClient
from . import conftest
from .common import (
    CommandLineOptions,
)
from .rnode import (
    bootstrap_connected_peer,
    ready_bootstrap_with_network,
    Node,
    DeployThread
)

from .wait import (
    wait_for_node_sees_block,
    wait_for_peers_count_at_least,
)
from .conftest import TestingContext
from rchain.client import RClientException

BOOTSTRAP_KEY = PrivateKey.from_hex("b2527b00340a83e302beae2a8daf6d654e8e57541acfa261cc1b5635eb16aa15")
BONDED_VALIDATOR_KEY1 = PrivateKey.from_hex("9a801debae8bb97fe54c99389cafa576c60612503348578125b65ab182ff5850")
BONDED_VALIDATOR_KEY2 = PrivateKey.from_hex("ff2ba092524bafdbc85fa0c7eddb2b41c69bc9bf066a4711a8a16f749199e5be")
JOINING_VALIDATOR_KEY = PrivateKey.from_hex("567ea426deaeb8233f134c3a266149fb196d6eea7d28b447dfefff92002cb400")
READONLY_PEER_KEY = PrivateKey.from_hex("3596e2e5fd14b24a6d84af04b7f0a8f13e3e68ee2ca91dc4b19550f12e61502c")


def propose_one_round(context: TestingContext,leader: Node, followers: List[Tuple[Node, PrivateKey]]):
    contract_path = '/opt/docker/examples/hello_world_again.rho'

    leader.deploy(contract_path, BOOTSTRAP_KEY)
    blockInLeader = leader.propose()

    for node, key in followers:
        wait_for_node_sees_block(context, node, blockInLeader)

    followerBlocks = propose_simotanously(followers, contract_path)
    for block in followerBlocks:
        wait_for_node_sees_block(context, leader, block)

def propose_simotanously(nodes: List[Tuple[Node, PrivateKey]], contract_path: str):
    propose_thread = [DeployThread(str(i), node, contract_path, 1, key) for i, (node, key) in enumerate(nodes)]
    [t.start() for t in propose_thread]
    [t.join() for t in propose_thread]

    followerBlocks = []
    [followerBlocks.extend(t.propose_result) for t in propose_thread]
    return followerBlocks

def test_leaderful_epoch(command_line_options: CommandLineOptions, random_generator: Random,
                                   docker_client: DockerClient) -> None:
    genesis_vault = {
        BOOTSTRAP_KEY: 50000000,
        BONDED_VALIDATOR_KEY1: 50000000,
        BONDED_VALIDATOR_KEY2: 50000000,
        JOINING_VALIDATOR_KEY: 50000000,
        READONLY_PEER_KEY: 50000000
    }

    bonded_validator_map = {
        BOOTSTRAP_KEY: 10000000,
        BONDED_VALIDATOR_KEY1: 10000000,
        BONDED_VALIDATOR_KEY2: 10000000

    }

    epoch_length = 3
    quarantine_length = 0
    synchrony_constraint_threshold = 0.99
    with conftest.testing_context(command_line_options, random_generator, docker_client, bootstrap_key=BOOTSTRAP_KEY,  validator_bonds_dict=bonded_validator_map, wallets_dict=genesis_vault) as context, \
            ready_bootstrap_with_network(context=context, synchrony_constraint_threshold=synchrony_constraint_threshold, epoch_length=epoch_length, quarantine_length=quarantine_length) as bootstrap_node, \
            bootstrap_connected_peer(context=context, synchrony_constraint_threshold=synchrony_constraint_threshold, bootstrap=bootstrap_node, name='bonded-validator1', private_key=BONDED_VALIDATOR_KEY1, epoch_length=epoch_length, quarantine_length=quarantine_length) as bonded_validator1, \
            bootstrap_connected_peer(context=context, synchrony_constraint_threshold=synchrony_constraint_threshold, bootstrap=bootstrap_node, name='bonded-validator2', private_key=BONDED_VALIDATOR_KEY2, epoch_length=epoch_length, quarantine_length=quarantine_length) as bonded_validator2:
        wait_for_peers_count_at_least(context, bonded_validator1, 2)
        contract_path = '/opt/docker/examples/hello_world_again.rho'

        followers = [
            (bonded_validator1, BONDED_VALIDATOR_KEY1),
            (bonded_validator2, BONDED_VALIDATOR_KEY2)
        ]
        # propose block 1, 2
        propose_one_round(context,
                          leader=bootstrap_node,
                          followers=followers)

        # propose block 3, 4
        # epoch on leader, block3
        propose_one_round(context,
                          leader=bootstrap_node,
                          followers=followers)
        with bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='joining-validator',
                                      private_key=JOINING_VALIDATOR_KEY) as joining_validator:
            bond_amount = 10000000

            # bond a new node
            # block 5
            bonding_block_hash = bootstrap_node.deploy_contract_with_substitution(
                substitute_dict={"%AMOUNT": "{}".format(bond_amount)},
                rho_file_path="resources/wallets/bond.rho",
                private_key=JOINING_VALIDATOR_KEY,
                phlo_limit=1000000,
                phlo_price=1
            )

            for node, _ in followers:
                wait_for_node_sees_block(context, node, bonding_block_hash)

            wait_for_node_sees_block(context, joining_validator, bonding_block_hash)

            # epoch on both followers and add new active validator
            # block 6
            propose_simotanously(followers, contract_path)

            new_followers = [
                (bonded_validator1, BONDED_VALIDATOR_KEY1),
                (bonded_validator2, BONDED_VALIDATOR_KEY2),
                (joining_validator, JOINING_VALIDATOR_KEY)
            ]

            # propose block 7, 8
            propose_one_round(context,
                              leader=bootstrap_node,
                              followers=new_followers)

            #unbond validator 1
            # on block 9 and epoch on bootstrap
            unbond = bootstrap_node.deploy_contract_with_substitution(substitute_dict={},
                                                          rho_file_path="resources/wallets/unbond.rho",
                                                          private_key=BONDED_VALIDATOR_KEY1)

            for node, _ in new_followers:
                wait_for_node_sees_block(context, unbond, bonding_block_hash)

            new_followers_unbond = [
                (bonded_validator2, BONDED_VALIDATOR_KEY2),
                (joining_validator, JOINING_VALIDATOR_KEY)
            ]
            # propose block 10
            propose_simotanously(new_followers_unbond, contract_path)

            # propose block 11, 12
            propose_one_round(context,
                              leader=bootstrap_node,
                              followers=new_followers_unbond)

            # propose block 13, 14
            propose_one_round(context,
                              leader=bootstrap_node,
                              followers=new_followers_unbond)

            # propose block 15, 16
            propose_one_round(context,
                              leader=bootstrap_node,
                              followers=new_followers_unbond)

            # unbonded node can not propose
            with pytest.raises(RClientException):
                bonded_validator1.deploy('/opt/docker/examples/tut-hello.rho', BONDED_VALIDATOR_KEY1)
                bonded_validator1.propose()