from random import Random
import pytest
from docker.client import DockerClient

from rchain.crypto import PrivateKey
from . import conftest
from .common import (
    CommandLineOptions,
    NotAnActiveValidatorError
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
VALIDATOR_KEY_2 = PrivateKey.generate()
VALIDATOR_KEY_3 = PrivateKey.generate()


def test_unbond_validator(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    bonded_validator_map = {
        BOOTSTRAP_KEY: 20000000,
        VALIDATOR_KEY_1: 40000000,
        VALIDATOR_KEY_2: 20000000,
        VALIDATOR_KEY_3: 20000000,
    }

    wallets_file = {
        BOOTSTRAP_KEY : 30000000,
        VALIDATOR_KEY_1: 20000000,
        VALIDATOR_KEY_2: 20000000,
        VALIDATOR_KEY_3: 20000000,
    }

    # unbond a validator. set the epoch length to 3 and quarantine length to 2
    # every epoch change the network would re-pick the active validator and
    # the withdraw process is going to happen after (quarantine_length + epoch_length * (1 + blockNumber // epoch_length)) blocks
    # normally quarantine_length is 5x of epoch_length
    # but in order to decrease the time in running the test we choose 2x of the epoch_length
    epoch_length = 3
    quarantine_length = 6
    with conftest.testing_context(command_line_options, random_generator, docker_client, validator_bonds_dict=bonded_validator_map, bootstrap_key=BOOTSTRAP_KEY, wallets_dict=wallets_file) as context, \
        ready_bootstrap_with_network(context=context, synchrony_constraint_threshold=0, epoch_length=epoch_length, quarantine_length=quarantine_length) as bootstrap_node, \
        bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-1', private_key=VALIDATOR_KEY_1, epoch_length=epoch_length, quarantine_length=quarantine_length) as validator_1, \
        bootstrap_connected_peer(context=context, bootstrap=bootstrap_node, name='bonded-validator-2',
                                 private_key=VALIDATOR_KEY_2, epoch_length=epoch_length, quarantine_length=quarantine_length) as validator_2:
        # genesis block number is 0

        # block number 1
        validator_1.deploy('/opt/docker/examples/tut-hello.rho', VALIDATOR_KEY_3)
        validator_1.propose()

        # block number 2
        # unbond in block number 2
        # withdraw should happen after
        # (quarantine_length + epoch_length ) = (6 + 3 * (1 + 2 // 3)) = 9 block number
        # so the withdraw happen after block number 9
        # we should see withdraw result in block number 10
        validator_1.deploy_contract_with_substitution(substitute_dict={}, rho_file_path="resources/wallets/unbond.rho", private_key=VALIDATOR_KEY_1)

        # block number 3
        # close block happen after all deploys process
        validator_1.deploy('/opt/docker/examples/tut-hello.rho', VALIDATOR_KEY_3)
        b3 = validator_1.propose()

        # block number 4
        # validator_1 is no longer a active validator after block number 3
        # validator_1 should fail on proposing
        with pytest.raises(NotAnActiveValidatorError):
            validator_1.deploy('/opt/docker/examples/tut-hello.rho', VALIDATOR_KEY_3)
            validator_1.propose()

        wait_for_node_sees_block(context, bootstrap_node, b3)

        # block number 4
        # withdraw not happen yet
        validator_1_balance = get_vault_balance(context, bootstrap_node, VALIDATOR_KEY_1.get_public_key().get_rev_address(), VALIDATOR_KEY_2, 100000, 1)
        assert validator_1_balance < 20000000

        # block number 5
        bootstrap_node.deploy('/opt/docker/examples/tut-hello-again.rho', VALIDATOR_KEY_3)
        b5 = bootstrap_node.propose()

        wait_for_node_sees_block(context, validator_2, b5)

        # block number 6
        validator_1_balance = get_vault_balance(context, validator_2, VALIDATOR_KEY_1.get_public_key().get_rev_address(), VALIDATOR_KEY_2, 100000, 1)
        assert validator_1_balance < 20000000

        # block number 7
        validator_1_balance = get_vault_balance(context, validator_2, VALIDATOR_KEY_1.get_public_key().get_rev_address(), VALIDATOR_KEY_2, 100000, 1)
        assert validator_1_balance < 20000000

        # block number 8
        validator_2.deploy('/opt/docker/examples/tut-hello.rho', VALIDATOR_KEY_3)
        b8 = validator_2.propose()

        wait_for_node_sees_block(context, bootstrap_node, b8)

        # block number 9
        validator_1_balance = get_vault_balance(context, bootstrap_node, VALIDATOR_KEY_1.get_public_key().get_rev_address(), VALIDATOR_KEY_2, 100000, 1)
        assert validator_1_balance < 20000000

        # block number 10
        # withdraw happen in block number 9, result get in block 10
        validator_1_balance = get_vault_balance(context, bootstrap_node, VALIDATOR_KEY_1.get_public_key().get_rev_address(), VALIDATOR_KEY_2, 100000, 1)
        assert validator_1_balance > 50000000