import os
from random import Random

from docker.client import DockerClient

from .common import (
    CommandLineOptions,
)
from .conftest import testing_context
from .rnode import (
    docker_network_with_started_bootstrap,
    extract_validator_stake_from_deploy_cost_str,
    parse_show_block_output
)
from .wait import (
    wait_for_approved_block_received_handler_state,
)


CONTRACT_COST = {
    "dupe.rho": 105978,
    "hello_world_again.rho": 1632,
    "longfast.rho": 98445,
    "longslow.rho": 3851039,
    "shortfast.rho": 132,
    "shortslow.rho": 3785344,
    "stderr.rho": 126,
    "stderrAck.rho": 485,
    "stdout.rho": 126,
    "stdoutAck.rho": 485,
    "time.rho": 706,
    "tut-bytearray-methods.rho": 532,
    "tut-hash-functions.rho": 1368,
    "tut-hello-again.rho": 2782,
    "tut-hello.rho": 1947,
    "tut-lists-methods.rho": 648,
    "tut-maps-methods.rho": 2823,
    "tut-parens.rho": 399,
    "tut-philosophers.rho": 6111,
    "tut-prime.rho": 6789,
    "tut-rcon-or.rho": 2237,
    "tut-rcon.rho": 1884,
    "tut-registry.rho": 8356,
    "tut-sets-methods.rho": 2513,
    "tut-strings-methods.rho": 709,
    "tut-tuples-methods.rho": 213
}


def test_propose(command_line_options: CommandLineOptions, random_generator: Random, docker_client: DockerClient) -> None:
    with testing_context(command_line_options, random_generator, docker_client) as context:
        with docker_network_with_started_bootstrap(context=context) as bootstrap_node:
            wait_for_approved_block_received_handler_state(context, bootstrap_node)
            relative_paths = bootstrap_node.shell_out('sh', '-c', 'ls /opt/docker/examples/*.rho').splitlines()
            relative_path = random_generator.choice(relative_paths)
            full_path = os.path.join('/opt/docker/examples', relative_path)
            bootstrap_node.deploy(full_path, context.bootstrap_keypair.private_key)
            bootstrap_node.propose()
            block_hash = bootstrap_node.propose()
            output = bootstrap_node.show_block(block_hash)
            block_info = parse_show_block_output(output)
            cost = extract_validator_stake_from_deploy_cost_str(block_info['deployCost'])
            # FIXME now the user info is only the empty string. Fix it if user change to real user hash
            assert CONTRACT_COST[os.path.basename(full_path)] == cost['']