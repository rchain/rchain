
from random import Random
import shutil
import os
import re
from typing import Optional
from docker.client import DockerClient

from .common import (
    CommandLineOptions,
    random_string,
)

from .conftest import (
    testing_context,
)
from .rnode import (
    Node,
    docker_network_with_started_bootstrap,
)
from .common import TestingContext
from . import conftest
from .wait import wait_for_log_match


ALICE_KEY = conftest.KeyPair(private_key='b2527b00340a83e302beae2a8daf6d654e8e57541acfa261cc1b5635eb16aa15', public_key='12a764e25517b99c35db937115f480a5067f6a809dd0c1f382ad2a9212a012c3')
BOB_KEY = conftest.KeyPair(private_key='9a801debae8bb97fe54c99389cafa576c60612503348578125b65ab182ff5850', public_key='155244935e80a72b4f3e83fb2fcfbb8d07c5bfdbc91e3b6c96371583139acad8')

def deploy_contract_with_substitution(node: Node, substitute_rules: str, contract_name: str, private_key: Optional[str]=None) -> str:
    local_contract_file_path = os.path.join('resources/wallets', contract_name)
    node_deploy_file_path = os.path.join(node.local_deploy_dir, contract_name)
    shutil.copyfile(local_contract_file_path, node_deploy_file_path)
    container_contract_file_path = '{}/{}'.format(node.remote_deploy_dir, contract_name)
    node.shell_out(
        'sed',
        '-i',
        '-e', substitute_rules,
        container_contract_file_path,
    )
    node.deploy(container_contract_file_path, private_key=private_key)
    block_hash = node.propose()
    os.remove(node_deploy_file_path)
    return block_hash

def create_genesis_vault(context: TestingContext, node: Node, rev_addr: str, init_vault_value: int) -> None:
    log_marker = random_string(context, 10)
    create_vault_success_pattern = re.compile("{} Genesis vault created!".format(log_marker))
    deploy_contract_with_substitution(node, "s/%REV_ADDR/{}/g; s/%INIT_VAULT_VALUE/{}/g; s/%LOG_MARKER/{}/g".format(rev_addr, init_vault_value, log_marker), "create_genesis_vault.rho")
    wait_for_log_match(context, node, create_vault_success_pattern)

def transfer_funds(context: TestingContext, node: Node, from_rev_addr: str, to_rev_addr: str, amount: int, private_key: str) -> bool:
    log_marker = random_string(context, 10)
    transfer_funds_result_pattern= re.compile('"{} (Successfully|Failing) reason: (?P<reason>[a-zA-Z0-9]*)"'.format(log_marker))
    deploy_contract_with_substitution(node, "s/%FROM/{}/g; s/%TO/{}/g; s/%AMOUNT/{}/g; s/%LOG_MARKER/{}/g".format(from_rev_addr, to_rev_addr, amount, log_marker), "transfer_funds.rho", private_key)
    wait_for_log_match(context, node, transfer_funds_result_pattern)
    transfer_result_match = transfer_funds_result_pattern.search(node.logs())
    reason = transfer_result_match.group('reason')  # type:ignore
    return reason == 'Nil'

def check_balance(context: TestingContext, node: Node, rev_addr: str) -> int:
    log_marker = random_string(context, 10)
    check_balance_pattern = re.compile('"{} Vault (?P<rev_addr>[a-zA-Z0-9]*) balance is (?P<balance>[0-9]*)"'.format(log_marker))
    deploy_contract_with_substitution(node, "s/%REV_ADDR/{}/g; s/%LOG_MARKER/{}/g".format(rev_addr, log_marker), "check_balance.rho")
    wait_for_log_match(context, node, check_balance_pattern)
    check_balance_match = check_balance_pattern.search(node.logs())
    return int(check_balance_match.group("balance"))  # type:ignore

def know_ones_rev_addr(context: TestingContext, node: Node, public_key: str) -> str:
    log_marker = random_string(context, 10)
    public_key_rev_addr_pattern = re.compile('"{} RevAddress for pubKey {} is (?P<rev_addr>[a-zA-Z0-9]*)"'.format(log_marker, public_key))
    deploy_contract_with_substitution(node, "s/%PUB_KEY/{}/g; s/%LOG_MARKER/{}/g".format(public_key, log_marker), "know_ones_revaddress.rho")
    wait_for_log_match(context, node, public_key_rev_addr_pattern)
    pattern_match = public_key_rev_addr_pattern.search(node.logs())
    return pattern_match.group("rev_addr")  # type:ignore


def test_alice_pay_bob(command_line_options: CommandLineOptions, docker_client: DockerClient, random_generator: Random) -> None:
    with testing_context(command_line_options, random_generator, docker_client) as context:
        with docker_network_with_started_bootstrap(context=context) as bootstrap_node:
            alice_init_vault_value = 5000
            transfer_amount = 100
            alice_rev_address = know_ones_rev_addr(context, bootstrap_node, ALICE_KEY.public_key)
            bob_rev_address = know_ones_rev_addr(context, bootstrap_node, BOB_KEY.public_key)
            create_genesis_vault(context, bootstrap_node, alice_rev_address, alice_init_vault_value)
            alice_balance = check_balance(context, bootstrap_node, alice_rev_address)
            bob_balance = check_balance(context, bootstrap_node, bob_rev_address)
            assert alice_balance == alice_init_vault_value
            assert bob_balance == 0

            assert transfer_funds(context, bootstrap_node, alice_rev_address, bob_rev_address, transfer_amount, ALICE_KEY.private_key)

            alice_balance = check_balance(context, bootstrap_node, alice_rev_address)
            assert alice_balance == alice_init_vault_value - transfer_amount

            bob_balance = check_balance(context, bootstrap_node, bob_rev_address)
            assert bob_balance == transfer_amount
