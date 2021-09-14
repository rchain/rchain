from random import Random
import pytest
from docker.client import DockerClient

from rchain.crypto import PrivateKey
from rchain.client import RClientException
from rchain.pb.DeployServiceCommon_pb2 import BlockInfo
from . import conftest
from .common import (
    CommandLineOptions,
)
from .rnode import (
    bootstrap_connected_peer,
    ready_bootstrap_with_network,
)
from .wait import (
    wait_for_node_sees_block,
)

from .test_wallets import get_vault_balance

BOOTSTRAP_KEY = PrivateKey.from_hex("b2527b00340a83e302beae2a8daf6d654e8e57541acfa261cc1b5635eb16aa15")
VALIDATOR_KEY_1 = PrivateKey.from_hex("9a801debae8bb97fe54c99389cafa576c60612503348578125b65ab182ff5850")
VALIDATOR_KEY_2 = PrivateKey.from_hex("2bdedd2e4dd2e7b5f176b7a5bc155f10fafd3fbd9c03fb7556f2ffd22c786f8b")
VALIDATOR_KEY_3 = PrivateKey.from_hex("632a21e0176c4daed1ca78f08f98885f61d2050e0391e31eae59ff1a35ccca7f")


def get_total_cost_from_block(block_info: BlockInfo) -> int:
    """get the total costs in the block"""
    total_cost = 0
    for deploy in block_info.deploys:
        total_cost += deploy.cost
    return total_cost


def test_unbond_validator_and_reward(command_line_options: CommandLineOptions, random_generator: Random,
                                     docker_client: DockerClient) -> None:
    validator_1_initial_bonding_amount = 40000000
    validator_1_initial_wallet_amount = 20000000

    bonded_validator_map = {
        BOOTSTRAP_KEY: 20000000,
        VALIDATOR_KEY_1: validator_1_initial_bonding_amount,
        VALIDATOR_KEY_2: 20000000,
        VALIDATOR_KEY_3: 20000000,
    }

    wallets_file = {
        BOOTSTRAP_KEY: 30000000,
        VALIDATOR_KEY_1: validator_1_initial_wallet_amount,
        VALIDATOR_KEY_2: 20000000,
        VALIDATOR_KEY_3: 20000000,
    }

    total_bond_amount = sum(bonded_validator_map.values())
    # unbond a validator. set the epoch length to 3 and quarantine length to 2
    # every epoch change the network would re-pick the active validator and
    # the withdraw process is going to happen after (quarantine_length + epoch_length * (1 + blockNumber // epoch_length)) blocks
    # normally quarantine_length is 5x of epoch_length
    # but in order to decrease the time in running the test we choose 2x of the epoch_length
    epoch_length = 3
    quarantine_length = 6
    with conftest.testing_context(command_line_options, random_generator, docker_client,
                                  validator_bonds_dict=bonded_validator_map, bootstrap_key=BOOTSTRAP_KEY,
                                  wallets_dict=wallets_file) as context, \
            ready_bootstrap_with_network(context=context, synchrony_constraint_threshold=0, epoch_length=epoch_length,
                                         quarantine_length=quarantine_length) as bootstrap_node, \
            bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-1',
                                     private_key=VALIDATOR_KEY_1, epoch_length=epoch_length,
                                     quarantine_length=quarantine_length) as validator_1, \
            bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-2',
                                     private_key=VALIDATOR_KEY_2, epoch_length=epoch_length,
                                     quarantine_length=quarantine_length) as validator_2:
        # genesis block number is 0

        rewards_of_v1 = 0
        # block number 1
        validator_1.deploy('/opt/docker/examples/tut-hello.rho', VALIDATOR_KEY_3)
        b1 = validator_1.propose()
        block1_info = validator_1.get_block(b1)
        # rewards amount are distributed by the bonds amount
        rewards_of_v1 += get_total_cost_from_block(
            block1_info) * validator_1_initial_bonding_amount // total_bond_amount

        # block number 2
        # unbond in block number 2
        # withdraw should happen after
        # (quarantine_length + epoch_length ) = (6 + 3 * (1 + 2 // 3)) = 9 block number
        # so the withdraw happen after block number 9
        # we should see withdraw result in block number 10
        b2 = validator_1.deploy_contract_with_substitution(substitute_dict={},
                                                           rho_file_path="resources/wallets/unbond.rho",
                                                           private_key=VALIDATOR_KEY_1)
        block2_info = validator_1.get_block(b2)
        rewards_of_v1 += get_total_cost_from_block(
            block2_info) * validator_1_initial_bonding_amount // total_bond_amount

        # block number 3
        # close block happen after all deploys process
        validator_1.deploy('/opt/docker/examples/tut-hello.rho', VALIDATOR_KEY_3)
        b3 = validator_1.propose()
        block3_info = validator_1.get_block(b3)
        rewards_of_v1 += get_total_cost_from_block(
            block3_info) * validator_1_initial_bonding_amount // total_bond_amount

        # block number 4
        # validator_1 is no longer a active validator after block number 3
        # validator_1 should fail on proposing
        with pytest.raises(RClientException):
            validator_1.deploy('/opt/docker/examples/tut-hello.rho', VALIDATOR_KEY_3)
            validator_1.propose()

        wait_for_node_sees_block(context, bootstrap_node, b3)

        # block number 4
        # withdraw not happen yet
        validator_1_balance_before_bond_refund = get_vault_balance(context, bootstrap_node,
                                                                   VALIDATOR_KEY_1.get_public_key().get_rev_address(),
                                                                   VALIDATOR_KEY_2, 100000, 1)
        # the unbond process cost validator1 some phlos so the balance of validator1 is smaller than the initial wallet amount
        assert validator_1_balance_before_bond_refund < validator_1_initial_wallet_amount

        # block number 5
        bootstrap_node.deploy('/opt/docker/examples/tut-hello-again.rho', VALIDATOR_KEY_3)
        b5 = bootstrap_node.propose()

        wait_for_node_sees_block(context, validator_2, b5)

        # block number 6
        validator_1_balance_before_bond_refund = get_vault_balance(context, validator_2,
                                                                   VALIDATOR_KEY_1.get_public_key().get_rev_address(),
                                                                   VALIDATOR_KEY_2,
                                                                   100000, 1)
        assert validator_1_balance_before_bond_refund < 20000000

        # block number 7
        validator_1_balance_before_bond_refund = get_vault_balance(context, validator_2,
                                                                   VALIDATOR_KEY_1.get_public_key().get_rev_address(),
                                                                   VALIDATOR_KEY_2,
                                                                   100000, 1)
        assert validator_1_balance_before_bond_refund < 20000000

        # block number 8
        validator_2.deploy('/opt/docker/examples/tut-hello.rho', VALIDATOR_KEY_3)
        b8 = validator_2.propose()

        wait_for_node_sees_block(context, bootstrap_node, b8)

        # block number 9
        validator_1_balance_before_bond_refund = get_vault_balance(context, bootstrap_node,
                                                                   VALIDATOR_KEY_1.get_public_key().get_rev_address(),
                                                                   VALIDATOR_KEY_2, 100000, 1)
        assert validator_1_balance_before_bond_refund < validator_1_initial_wallet_amount

        # block number 10
        # withdraw happen in block number 9, result get in block 10
        # get the bonding amount and the reward amount
        # validator_1_balance_after_refund = get_vault_balance(context, bootstrap_node,
        #                                                      VALIDATOR_KEY_1.get_public_key().get_rev_address(),
        #                                                      VALIDATOR_KEY_2,
        #                                                      100000, 1)
        # assert validator_1_balance_after_refund == validator_1_balance_before_bond_refund + validator_1_initial_bonding_amount + rewards_of_v1
