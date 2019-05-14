
from random import Random
import re
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
from .wait import (
    wait_for_log_match,
    wait_for_log_match_result,
)


ALICE_KEY = conftest.KeyPair(private_key='b2527b00340a83e302beae2a8daf6d654e8e57541acfa261cc1b5635eb16aa15', public_key='12a764e25517b99c35db937115f480a5067f6a809dd0c1f382ad2a9212a012c3')
BOB_KEY = conftest.KeyPair(private_key='9a801debae8bb97fe54c99389cafa576c60612503348578125b65ab182ff5850', public_key='155244935e80a72b4f3e83fb2fcfbb8d07c5bfdbc91e3b6c96371583139acad8')

def create_genesis_vault(context: TestingContext, node: Node, rev_addr: str, init_vault_value: int, private_key: str) -> None:
    log_marker = random_string(context, 10)
    create_vault_success_pattern = re.compile("{} Genesis vault created!".format(log_marker))
    node.deploy_contract_with_substitution(
        substitute_dict={"%REV_ADDR": rev_addr, "%INIT_VAULT_VALUE": str(init_vault_value), "%LOG_MARKER": log_marker},
        rho_file_path="resources/wallets/create_genesis_vault.rho",
        private_key=private_key
    )
    wait_for_log_match(context, node, create_vault_success_pattern)

def transfer_funds(context: TestingContext, node: Node, from_rev_addr: str, to_rev_addr: str, amount: int, private_key: str) -> bool:
    log_marker = random_string(context, 10)
    transfer_funds_result_pattern= re.compile('"{} (Successfully|Failing) reason: (?P<reason>[a-zA-Z0-9]*)"'.format(log_marker))
    node.deploy_contract_with_substitution(
        substitute_dict={"%FROM": from_rev_addr, "%TO": to_rev_addr, "%AMOUNT": str(amount), "%LOG_MARKER": log_marker},
        rho_file_path="resources/wallets/transfer_funds.rho",
        private_key=private_key
    )
    transfer_result_match = wait_for_log_match_result(context, node, transfer_funds_result_pattern)
    reason = transfer_result_match.group('reason')
    return reason == 'Nil'

def check_balance(context: TestingContext, node: Node, rev_addr: str, private_key: str) -> int:
    log_marker = random_string(context, 10)
    check_balance_pattern = re.compile('"{} Vault (?P<rev_addr>[a-zA-Z0-9]*) balance is (?P<balance>[0-9]*)"'.format(log_marker))
    node.deploy_contract_with_substitution(
        substitute_dict={"%REV_ADDR": rev_addr, "%LOG_MARKER": log_marker},
        rho_file_path="resources/wallets/check_balance.rho",
        private_key=private_key
    )
    check_balance_match = wait_for_log_match_result(context, node, check_balance_pattern)
    return int(check_balance_match.group("balance"))

def know_ones_rev_addr(context: TestingContext, node: Node, public_key: str, private_key: str) -> str:
    log_marker = random_string(context, 10)
    public_key_rev_addr_pattern = re.compile('"{} RevAddress for pubKey {} is (?P<rev_addr>[a-zA-Z0-9]*)"'.format(log_marker, public_key))
    node.deploy_contract_with_substitution(
        substitute_dict={"%PUB_KEY": public_key, "%LOG_MARKER": log_marker},
        rho_file_path="resources/wallets/know_ones_revaddress.rho",
        private_key=private_key
    )
    pattern_match = wait_for_log_match_result(context, node, public_key_rev_addr_pattern)
    return pattern_match.group("rev_addr")


def test_alice_pay_bob(command_line_options: CommandLineOptions, docker_client: DockerClient, random_generator: Random) -> None:
    with testing_context(command_line_options, random_generator, docker_client) as context:
        with docker_network_with_started_bootstrap(context=context) as bootstrap_node:
            alice_init_vault_value = 5000
            transfer_amount = 100
            alice_rev_address = know_ones_rev_addr(context, bootstrap_node, ALICE_KEY.public_key, ALICE_KEY.private_key)
            bob_rev_address = know_ones_rev_addr(context, bootstrap_node, BOB_KEY.public_key, BOB_KEY.private_key)
            create_genesis_vault(context, bootstrap_node, alice_rev_address, alice_init_vault_value, ALICE_KEY.private_key)
            alice_balance = check_balance(context, bootstrap_node, alice_rev_address, ALICE_KEY.private_key)
            bob_balance = check_balance(context, bootstrap_node, bob_rev_address, BOB_KEY.private_key)
            assert alice_balance == alice_init_vault_value
            assert bob_balance == 0

            assert transfer_funds(context, bootstrap_node, alice_rev_address, bob_rev_address, transfer_amount, ALICE_KEY.private_key)

            alice_balance = check_balance(context, bootstrap_node, alice_rev_address, ALICE_KEY.private_key)
            assert alice_balance == alice_init_vault_value - transfer_amount

            bob_balance = check_balance(context, bootstrap_node, bob_rev_address, BOB_KEY.private_key)
            assert bob_balance == transfer_amount

            assert transfer_funds(context, bootstrap_node, bob_rev_address, alice_rev_address, transfer_amount, BOB_KEY.private_key)

            alice_balance = check_balance(context, bootstrap_node, alice_rev_address, ALICE_KEY.private_key)
            assert alice_balance == 5000

            bob_balance = check_balance(context, bootstrap_node, bob_rev_address, BOB_KEY.private_key)
            assert bob_balance == 0
