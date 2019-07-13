from random import Random
import re
import pytest
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
)
from .common import (
    TestingContext,
    TransderFundsError,
)
from . import conftest
from .wait import (
    wait_for_log_match,
    wait_for_log_match_result,
)


ALICE_KEY = conftest.KeyPair(private_key='b2527b00340a83e302beae2a8daf6d654e8e57541acfa261cc1b5635eb16aa15', public_key='04967539c162c230d6bc41f49913cb0ea2cf16f8145a0e0b369ebd5e751f983774e94d890bd0720586993d6c01945f50e846fb07e84939576d1694be40c4a771a9')
BOB_KEY = conftest.KeyPair(private_key='9a801debae8bb97fe54c99389cafa576c60612503348578125b65ab182ff5850', public_key='04d31666bee8a39c1bdd079ec70dcfb7d61bc3001c8743d58edbfb6f63f7fde0d0df13ca126a245c3d2a1a9cf92385221421ee2990e75d2ce5c7480b3636b039bb')
CHARLIE_KEY = conftest.KeyPair(private_key='567ea426deaeb8233f134c3a266149fb196d6eea7d28b447dfefff92002cb400', public_key='04c225634d6b511ca76de8746058d4c31dd5a5457d68a0c3b8dfbb2a6d0bdc7d01e2e9831539f2a59cfc74582f96539c50aeac553898a989016ac29262a3c305de')

ALICE_GENESIS_VAULT_AMOUNT = 5000
CHARLIE_GENESIS_VAULT_AMOUNT = 5000


def create_genesis_vault(context: TestingContext, node: Node, rev_addr: str, init_vault_value: int, private_key: str) -> None:
    log_marker = random_string(context, 10)
    create_vault_success_pattern = re.compile("{} Genesis vault created!".format(log_marker))
    node.deploy_contract_with_substitution(
        substitute_dict={"%REV_ADDR": rev_addr, "%INIT_VAULT_VALUE": str(init_vault_value), "%LOG_MARKER": log_marker},
        rho_file_path="resources/wallets/create_genesis_vault.rho",
        private_key=private_key
    )
    wait_for_log_match(context, node, create_vault_success_pattern)


def transfer_funds(context: TestingContext, node: Node, from_rev_addr: str, to_rev_addr: str, amount: int, private_key: str) -> None:
    """
    Transfer rev from one vault to another vault.
    If the transfer is processed successfully, it would return None.
    If the transfer fail to be processed, it would raise "TransferFundsError".
    """
    log_marker = random_string(context, 10)
    transfer_funds_result_pattern = re.compile('"{} (Successfully|Failing) reason: (?P<reason>[a-zA-Z0-9 ]*)"'.format(log_marker))
    node.deploy_contract_with_substitution(
        substitute_dict={"%FROM": from_rev_addr, "%TO": to_rev_addr, "%AMOUNT": str(amount), "%LOG_MARKER": log_marker},
        rho_file_path="resources/wallets/transfer_funds.rho",
        private_key=private_key
    )
    transfer_result_match = wait_for_log_match_result(context, node, transfer_funds_result_pattern)
    reason = transfer_result_match.group('reason')
    if reason != "Nil":
        raise TransderFundsError(reason)


def get_vault_balance(context: TestingContext, node: Node, rev_addr: str, private_key: str) -> int:
    log_marker = random_string(context, 10)
    check_balance_pattern = re.compile('"{} Vault (?P<rev_addr>[a-zA-Z0-9]*) balance is (?P<balance>[0-9]*)"'.format(log_marker))
    node.deploy_contract_with_substitution(
        substitute_dict={"%REV_ADDR": rev_addr, "%LOG_MARKER": log_marker},
        rho_file_path="resources/wallets/get_vault_balance.rho",
        private_key=private_key
    )
    check_balance_match = wait_for_log_match_result(context, node, check_balance_pattern)
    return int(check_balance_match.group("balance"))


def get_rev_address_by_public_key(context: TestingContext, node: Node, public_key: str, private_key: str) -> str:
    log_marker = random_string(context, 10)
    public_key_rev_addr_pattern = re.compile('"{} RevAddress for pubKey {} is (?P<rev_addr>[a-zA-Z0-9]*)"'.format(log_marker, public_key))
    node.deploy_contract_with_substitution(
        substitute_dict={"%PUB_KEY": public_key, "%LOG_MARKER": log_marker},
        rho_file_path="resources/wallets/get_rev_address_by_public_key.rho",
        private_key=private_key
    )
    pattern_match = wait_for_log_match_result(context, node, public_key_rev_addr_pattern)
    return pattern_match.group("rev_addr")


def test_alice_pay_bob(started_standalone_bootstrap_node: Node, command_line_options: CommandLineOptions, docker_client: DockerClient, random_generator: Random) -> None:
    with testing_context(command_line_options, random_generator, docker_client) as context:
        transfer_amount = 100
        alice_rev_address = get_rev_address_by_public_key(context, started_standalone_bootstrap_node, ALICE_KEY.public_key, ALICE_KEY.private_key)
        bob_rev_address = get_rev_address_by_public_key(context, started_standalone_bootstrap_node, BOB_KEY.public_key, BOB_KEY.private_key)
        create_genesis_vault(context, started_standalone_bootstrap_node, alice_rev_address, ALICE_GENESIS_VAULT_AMOUNT, ALICE_KEY.private_key)
        alice_balance = get_vault_balance(context, started_standalone_bootstrap_node, alice_rev_address, ALICE_KEY.private_key)
        bob_balance = get_vault_balance(context, started_standalone_bootstrap_node, bob_rev_address, BOB_KEY.private_key)
        assert alice_balance == ALICE_GENESIS_VAULT_AMOUNT
        assert bob_balance == 0

        transfer_funds(context, started_standalone_bootstrap_node, alice_rev_address, bob_rev_address, transfer_amount, ALICE_KEY.private_key)

        alice_balance = get_vault_balance(context, started_standalone_bootstrap_node, alice_rev_address, ALICE_KEY.private_key)
        assert alice_balance == ALICE_GENESIS_VAULT_AMOUNT - transfer_amount

        bob_balance = get_vault_balance(context, started_standalone_bootstrap_node, bob_rev_address, BOB_KEY.private_key)
        assert bob_balance == transfer_amount

        transfer_funds(context, started_standalone_bootstrap_node, bob_rev_address, alice_rev_address, transfer_amount, BOB_KEY.private_key)

        alice_balance = get_vault_balance(context, started_standalone_bootstrap_node, alice_rev_address, ALICE_KEY.private_key)
        assert alice_balance == ALICE_GENESIS_VAULT_AMOUNT

        bob_balance = get_vault_balance(context, started_standalone_bootstrap_node, bob_rev_address, BOB_KEY.private_key)
        assert bob_balance == 0


def test_transfer_failed_with_invalid_key(started_standalone_bootstrap_node: Node, command_line_options: CommandLineOptions, docker_client: DockerClient, random_generator: Random) -> None:
    with testing_context(command_line_options, random_generator, docker_client) as context:
        alice_rev_address = get_rev_address_by_public_key(context, started_standalone_bootstrap_node, ALICE_KEY.public_key, ALICE_KEY.private_key)
        charlie_rev_address = get_rev_address_by_public_key(context, started_standalone_bootstrap_node, CHARLIE_KEY.public_key, CHARLIE_KEY.private_key)

        create_genesis_vault(context, started_standalone_bootstrap_node, charlie_rev_address, CHARLIE_GENESIS_VAULT_AMOUNT, CHARLIE_KEY.private_key)
        charlie_balance = get_vault_balance(context, started_standalone_bootstrap_node, charlie_rev_address, BOB_KEY.private_key)

        assert charlie_balance == CHARLIE_GENESIS_VAULT_AMOUNT

        with pytest.raises(TransderFundsError):
            transfer_funds(context, started_standalone_bootstrap_node, charlie_rev_address, alice_rev_address, 100, ALICE_KEY.private_key)
