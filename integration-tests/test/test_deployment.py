from random import Random
import pytest
from rchain.crypto import PrivateKey
from docker.client import DockerClient

from . import conftest
from .common import (
    CommandLineOptions,
)
from .rnode import (
    ready_bootstrap_with_network
)

BOOTSTRAP_NODE_KEYS = PrivateKey.from_hex("80366db5fbb8dad7946f27037422715e4176dda41d582224db87b6c3b783d709")
BONDED_VALIDATOR_KEY_1 = PrivateKey.from_hex("120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5")


def test_deploy_with_not_enough_phlo(command_line_options: CommandLineOptions, random_generator: Random,
                                     docker_client: DockerClient) -> None:
    genesis_vault = {
        BOOTSTRAP_NODE_KEYS: 500000000000,
        BONDED_VALIDATOR_KEY_1: 500000000000,
    }

    with conftest.testing_context(command_line_options, random_generator, docker_client,
                                  wallets_dict=genesis_vault) as context, \
            ready_bootstrap_with_network(context=context, synchrony_constraint_threshold=0) as bootstrap_node:
        contract = "/opt/docker/examples/tut-hello.rho"
        # deploy with not enough phlo
        bootstrap_node.deploy(contract, BOOTSTRAP_NODE_KEYS, phlo_limit=1000, phlo_price=1)
        block_hash = bootstrap_node.propose()
        block_info = bootstrap_node.get_block(block_hash)
        deploy = block_info.deploys[0]
        assert deploy.errored


@pytest.mark.xfail
def test_deploy_run_out_of_phlo(command_line_options: CommandLineOptions, docker_client: DockerClient) -> None:
    assert False
