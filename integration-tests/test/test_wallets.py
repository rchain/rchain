from random import Random
import re
import pytest
from docker.client import DockerClient
from rchain.crypto import PrivateKey

from .common import (
    CommandLineOptions,
    random_string,
)

from .conftest import (
    testing_context,
)
from .rnode import (
    Node,
    started_bootstrap,
    docker_network
)
from .common import (
    TestingContext,
    TransderFundsError,
)
from .wait import (
    wait_for_log_match_result,
)


ALICE_KEY = PrivateKey.from_hex("b2527b00340a83e302beae2a8daf6d654e8e57541acfa261cc1b5635eb16aa15")
BOB_KEY = PrivateKey.from_hex("9a801debae8bb97fe54c99389cafa576c60612503348578125b65ab182ff5850")
CHARLIE_KEY = PrivateKey.from_hex("567ea426deaeb8233f134c3a266149fb196d6eea7d28b447dfefff92002cb400")

ALICE_GENESIS_VAULT_AMOUNT = 5000
CHARLIE_GENESIS_VAULT_AMOUNT = 5000

def transfer_funds(context: TestingContext, node: Node, from_rev_addr: str, to_rev_addr: str, amount: int, private_key: PrivateKey) -> None:
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

def get_vault_balance(context: TestingContext, node: Node, rev_addr: str, private_key: PrivateKey) -> int:
    log_marker = random_string(context, 10)
    check_balance_pattern = re.compile('"{} Vault (?P<rev_addr>[a-zA-Z0-9]*) balance is (?P<balance>[0-9]*)"'.format(log_marker))
    node.deploy_contract_with_substitution(
        substitute_dict={"%REV_ADDR": rev_addr, "%LOG_MARKER": log_marker},
        rho_file_path="resources/wallets/get_vault_balance.rho",
        private_key=private_key
    )
    check_balance_match = wait_for_log_match_result(context, node, check_balance_pattern)
    return int(check_balance_match.group("balance"))

def test_alice_pay_bob(command_line_options: CommandLineOptions, docker_client: DockerClient, random_generator: Random) -> None:
    genesis_vault = {
        ALICE_KEY: ALICE_GENESIS_VAULT_AMOUNT
    }

    with testing_context(command_line_options, random_generator, docker_client, wallets_dict=genesis_vault) as context, \
            docker_network(context, context.docker) as network, \
            started_bootstrap(context=context, network=network) as bootstrap:

        transfer_amount = 100
        alice_rev_address = ALICE_KEY.get_public_key().get_rev_address()
        bob_rev_address = BOB_KEY.get_public_key().get_rev_address()
        alice_balance = get_vault_balance(context, bootstrap, alice_rev_address, ALICE_KEY)
        bob_balance = get_vault_balance(context, bootstrap, bob_rev_address, BOB_KEY)
        assert alice_balance == ALICE_GENESIS_VAULT_AMOUNT
        assert bob_balance == 0

        transfer_funds(context, bootstrap, alice_rev_address, bob_rev_address, transfer_amount, ALICE_KEY)

        alice_balance = get_vault_balance(context, bootstrap, alice_rev_address, ALICE_KEY)
        assert alice_balance == ALICE_GENESIS_VAULT_AMOUNT - transfer_amount

        bob_balance = get_vault_balance(context, bootstrap, bob_rev_address, BOB_KEY)
        assert bob_balance == transfer_amount

        transfer_funds(context, bootstrap, bob_rev_address, alice_rev_address, transfer_amount, BOB_KEY)

        alice_balance = get_vault_balance(context, bootstrap, alice_rev_address, ALICE_KEY)
        assert alice_balance == ALICE_GENESIS_VAULT_AMOUNT

        bob_balance = get_vault_balance(context, bootstrap, bob_rev_address, BOB_KEY)
        assert bob_balance == 0


def test_transfer_failed_with_invalid_key(command_line_options: CommandLineOptions, docker_client: DockerClient, random_generator: Random) -> None:
    genesis_vault = {
        CHARLIE_KEY: CHARLIE_GENESIS_VAULT_AMOUNT
    }
    with testing_context(command_line_options, random_generator, docker_client, wallets_dict=genesis_vault) as context, \
            docker_network(context, context.docker) as network, \
            started_bootstrap(context=context, network=network) as bootstrap:

        alice_rev_address = ALICE_KEY.get_public_key().get_rev_address()
        charlie_rev_address = CHARLIE_KEY.get_public_key().get_rev_address()

        charlie_balance = get_vault_balance(context, bootstrap, charlie_rev_address, BOB_KEY)

        assert charlie_balance == CHARLIE_GENESIS_VAULT_AMOUNT

        with pytest.raises(TransderFundsError):
            transfer_funds(context, bootstrap, charlie_rev_address, alice_rev_address, 100, ALICE_KEY)

