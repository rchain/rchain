import contextlib
import os
import logging
import subprocess
import shutil
import tempfile
from random import Random
from typing import Generator

import pytest
from docker.client import DockerClient
from rchain.crypto import PrivateKey

from .common import CommandLineOptions
from .conftest import testing_context
from .rnode import (SynchronyConstraintError, docker_network,
                    started_bootstrap, started_peer, get_absolute_path_for_mounting)
from .test_wallets import get_vault_balance
from .wait import (wait_for_approved_block_received_handler_state,
                   wait_for_peers_count_at_least)

CEREMONY_MASTER_PRIVATE = PrivateKey.from_hex("80366db5fbb8dad7946f27037422715e4176dda41d582224db87b6c3b783d709")
VALIDATOR_A_PRIVATE = PrivateKey.from_hex("120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5")
VALIDATOR_B_PRIVATE = PrivateKey.from_hex("1f52d0bce0a92f5c79f2a88aae6d391ddf853e2eb8e688c5aa68002205f92dad")


@contextlib.contextmanager
def temp_rnode_data(mount_dir: str) -> Generator[str, None, None]:
    with tempfile.TemporaryDirectory() as temp_dir:
        logging.info("!!!{}".format(get_absolute_path_for_mounting("rnode_data", mount_dir)))
        a = subprocess.run(['ls', get_absolute_path_for_mounting("rnode_data", mount_dir)], check=True)
        logging.info("@@@{},{}".format(a.stdout, a.stderr))
        a = subprocess.run(['ls', mount_dir], check=True)
        logging.info("@@@{},{}".format(a.stdout, a.stderr))
        shutil.copytree(get_absolute_path_for_mounting("rnode_data", mount_dir), os.path.join(temp_dir, 'rnode_data'))
        yield temp_dir

def test_backward_compatible(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    """
    This test is for backward compatibility. It use the old version data of a rnode the create a existing network.
    If your pr failed on this test, it means your update make the old version rnode not working normally.

    Currently we don't release our mainnet so it is Ok to fail the test. Once your pr make the test failed , you
    should shout out loudly in discord and let everybody know your update is not backward compatible.
    """
    with  testing_context(command_line_options, random_generator, docker_client) as context, \
        temp_rnode_data(context.mount_dir) as temp_rnode, \
        docker_network(context, context.docker) as network, \
        started_bootstrap(context=context, network=network, extra_volumes=["{}/rnode_data/bootstrap:/var/lib/rnode".format(temp_rnode)]) as bootstrap_node, \
        started_peer(context=context, network=network, bootstrap=bootstrap_node, name='validator-a', private_key=VALIDATOR_A_PRIVATE, extra_volumes=["{}/rnode_data/validatorA:/var/lib/rnode".format(temp_rnode)], synchrony_constraint_threshold=0.33) as validator_a, \
        started_peer(context=context, network=network, bootstrap=bootstrap_node, name='validator-b', private_key=VALIDATOR_B_PRIVATE, extra_volumes=["{}/rnode_data/validatorB:/var/lib/rnode".format(temp_rnode)], synchrony_constraint_threshold=0.33) as validator_b:

        wait_for_approved_block_received_handler_state(context, bootstrap_node)
        wait_for_approved_block_received_handler_state(context, validator_a)
        wait_for_approved_block_received_handler_state(context, validator_b)
        wait_for_peers_count_at_least(context, bootstrap_node, 2)
        wait_for_peers_count_at_least(context, validator_a, 2)
        wait_for_peers_count_at_least(context, validator_b, 2)


        # why is here 28?
        # because the generated rnode data has been propose for 9 times each and plus one genesis block (3 * 9 + 1)
        # more detailed see the generated script "integration-tests/generate_rnode_data.py"
        assert bootstrap_node.get_blocks_count(40) == 22
        assert validator_a.get_blocks_count(40) == 22
        assert validator_b.get_blocks_count(40) == 22

        contract_path = '/opt/docker/examples/tut-hello.rho'

        with pytest.raises(SynchronyConstraintError):
            validator_b.deploy(contract_path, VALIDATOR_B_PRIVATE)
            validator_b.propose()

        assert get_vault_balance(context, bootstrap_node, CEREMONY_MASTER_PRIVATE.get_public_key().get_rev_address(), CEREMONY_MASTER_PRIVATE) == 1200
        assert get_vault_balance(context, validator_a, VALIDATOR_A_PRIVATE.get_public_key().get_rev_address(), VALIDATOR_A_PRIVATE) == 900
        assert get_vault_balance(context, validator_b, VALIDATOR_B_PRIVATE.get_public_key().get_rev_address(), VALIDATOR_B_PRIVATE) == 1900

        bootstrap_node.deploy(contract_path, VALIDATOR_B_PRIVATE)
        bootstrap_node.propose()

        validator_a.deploy(contract_path, VALIDATOR_B_PRIVATE)
        validator_a.propose()

        validator_b.deploy(contract_path, VALIDATOR_B_PRIVATE)
        validator_b.propose()
