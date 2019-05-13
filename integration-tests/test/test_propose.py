import os
import shutil
from random import Random
from .rnode import (
    Node,
    extract_validator_stake_from_deploy_cost_str,
    parse_show_block_output,
)
from .common import KeyPair

DEPLOY_KEY = KeyPair(private_key='632a21e0176c4daed1ca78f08f98885f61d2050e0391e31eae59ff1a35ccca7f', public_key='15ab05a878ddce564865e591ff5927613b341e39572065dfeaa0b8b442b6084b')


def test_propose(started_standalone_bootstrap_node: Node, random_generator: Random) -> None:
    relative_paths = started_standalone_bootstrap_node.shell_out('sh', '-c', 'ls /opt/docker/examples/*.rho').splitlines()
    relative_path = random_generator.choice(relative_paths)
    full_path = os.path.join('/opt/docker/examples', relative_path)
    started_standalone_bootstrap_node.deploy(full_path, DEPLOY_KEY.private_key)
    started_standalone_bootstrap_node.propose()

FIX_COST_RHO_CONTRACTS = {
    "contract_1.rho": 33,
    "contract_2.rho": 69,
    "contract_3.rho": 76,
    "contract_4.rho": 83,
    "contract_5.rho": 1936,
}

def test_propose_cost(started_standalone_bootstrap_node: Node, random_generator: Random) -> None:
    rho_contract, contract_cost = random_generator.choice(list(FIX_COST_RHO_CONTRACTS.items()))
    shutil.copyfile(os.path.join('resources/cost', rho_contract), os.path.join(started_standalone_bootstrap_node.local_deploy_dir, rho_contract))
    container_contract_file_path = os.path.join(started_standalone_bootstrap_node.remote_deploy_dir, rho_contract)
    started_standalone_bootstrap_node.deploy(container_contract_file_path, DEPLOY_KEY.private_key)
    block_hash = started_standalone_bootstrap_node.propose()
    output = started_standalone_bootstrap_node.show_block(block_hash)
    block_info = parse_show_block_output(output)
    cost = extract_validator_stake_from_deploy_cost_str(block_info['deployCost'])
    assert contract_cost == cost[DEPLOY_KEY.public_key]
