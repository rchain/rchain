from random import Random
import re
from typing import Pattern, Tuple
import pytest
from rchain.crypto import PrivateKey
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
    started_bootstrap_with_network
)
from .common import (
    TestingContext,
    TransderFundsError,
)
from .wait import (
    wait_for_log_match_result,
    wait_for_log_match_result_raise,
    wait_for_approved_block_received_handler_state,
    WaitTimeoutError
)


ALICE_KEY = PrivateKey.from_hex("b2527b00340a83e302beae2a8daf6d654e8e57541acfa261cc1b5635eb16aa15")
BOB_KEY = PrivateKey.from_hex("9a801debae8bb97fe54c99389cafa576c60612503348578125b65ab182ff5850")
CHARLIE_KEY = PrivateKey.from_hex("567ea426deaeb8233f134c3a266149fb196d6eea7d28b447dfefff92002cb400")


def wait_transfer_result(context: TestingContext, node: Node, transfer_funds_result_pattern: Pattern) -> None:
    transfer_result_match = wait_for_log_match_result_raise(context, node, transfer_funds_result_pattern)
    reason = transfer_result_match.group('reason')
    if reason != "Nil":
        raise TransderFundsError(reason)

def deploy_transfer(log_marker: str, node: Node, from_rev_addr: str, to_rev_addr: str, amount: int, private_key: PrivateKey, phlo_limit: int, phlo_price: int) -> str:
    return node.deploy_contract_with_substitution(
        substitute_dict={"%FROM": from_rev_addr, "%TO": to_rev_addr, "%AMOUNT": str(amount), "%LOG_MARKER": log_marker},
        rho_file_path="resources/wallets/transfer_funds.rho",
        private_key=private_key,
        phlo_limit=phlo_limit,
        phlo_price=phlo_price
    )
def deploy_transfer_from_pos_vault(log_marker: str, node: Node, to_rev_addr: str, amount: int, private_key: PrivateKey, phlo_limit: int, phlo_price: int) -> str:
    return node.deploy_contract_with_substitution(
        substitute_dict={"%TARGET_ADDR": to_rev_addr, "%AMOUNT": str(amount), "%LOG_MARKER": log_marker},
        rho_file_path="resources/wallets/transfer_from_pos_vault.rho",
        private_key=private_key,
        phlo_limit=phlo_limit,
        phlo_price=phlo_price
    )
def transfer_from_pos_vault(context: TestingContext, node: Node, to_rev_addr: str, amount: int, private_key: PrivateKey, phlo_limit: int, phlo_price: int) -> None:
    log_marker = random_string(context, 10)
    transfer_funds_result_pattern = re.compile('"{} (Successfully|Failing) reason: (?P<reason>[a-zA-Z0-9 ]*)"'.format(log_marker))
    deploy_transfer_from_pos_vault(log_marker, node, to_rev_addr, amount, private_key, phlo_limit, phlo_price)
    wait_transfer_result(context, node, transfer_funds_result_pattern)

def transfer_funds(context: TestingContext, node: Node, from_rev_addr: str, to_rev_addr: str, amount: int, private_key: PrivateKey, phlo_limit: int, phlo_price: int) -> None:
    """
    Transfer rev from one vault to another vault.
    If the transfer is processed successfully, it would return None.
    If the transfer fail to be processed, it would raise "TransferFundsError".
    """
    log_marker = random_string(context, 10)
    transfer_funds_result_pattern = re.compile('"{} (Successfully|Failing) reason: (?P<reason>[a-zA-Z0-9 ]*)"'.format(log_marker))
    deploy_transfer(log_marker, node, from_rev_addr, to_rev_addr, amount, private_key, phlo_limit, phlo_price)
    wait_transfer_result(context, node, transfer_funds_result_pattern)

def get_vault_balance(context: TestingContext, node: Node, rev_addr: str, private_key: PrivateKey, phlo_limit: int, phlo_price: int) -> Tuple[str, int]:
    log_marker = random_string(context, 10)
    check_balance_pattern = re.compile('"{} Vault (?P<rev_addr>[a-zA-Z0-9]*) balance is (?P<balance>[0-9]*)"'.format(log_marker))
    blockHash = node.deploy_contract_with_substitution(
        substitute_dict={"%REV_ADDR": rev_addr, "%LOG_MARKER": log_marker},
        rho_file_path="resources/wallets/get_vault_balance.rho",
        private_key=private_key,
        phlo_limit=phlo_limit,
        phlo_price=phlo_price
    )
    check_balance_match = wait_for_log_match_result(context, node, check_balance_pattern)
    return (blockHash, int(check_balance_match.group("balance")))


def test_transfer_from_pos_vault(command_line_options: CommandLineOptions, docker_client: DockerClient, random_generator: Random) -> None:
    pos_vault_private_key = PrivateKey.generate()
    genesis_vault = {
        pos_vault_private_key: 50000000,
        BOB_KEY: 0,
        ALICE_KEY: 50000000
    }

    pos_vault_pub_key = pos_vault_private_key.get_public_key().to_hex()
    with testing_context(command_line_options, random_generator, docker_client, wallets_dict=genesis_vault) as context, \
            started_bootstrap_with_network(context=context, pos_vault_pub_key=pos_vault_pub_key) as bootstrap:
        wait_for_approved_block_received_handler_state(context, bootstrap)
        transfer_amount = 100000

        # Check that money transfer from PoS vault with pos_vault_private_key finish successfully
        bob_rev_address = BOB_KEY.get_public_key().get_rev_address()
        transfer_from_pos_vault(context, bootstrap, bob_rev_address, transfer_amount, pos_vault_private_key, 1000000, 1)
        _, bob_balance = get_vault_balance(context, bootstrap, bob_rev_address, pos_vault_private_key, 1000000, 1)
        assert bob_balance == transfer_amount

        # Check that money transfer from PoS vault with other private key fails
        with pytest.raises(TransderFundsError) as e:
            transfer_from_pos_vault(context, bootstrap, bob_rev_address, transfer_amount, ALICE_KEY, 1000000, 1)
        assert e.value.reason == "You have not permission to transfer"

def test_alice_pay_bob(command_line_options: CommandLineOptions, docker_client: DockerClient, random_generator: Random) -> None:
    genesis_vault = {
        ALICE_KEY: 50000000
    }

    with testing_context(command_line_options, random_generator, docker_client, wallets_dict=genesis_vault) as context, \
            started_bootstrap_with_network(context=context) as bootstrap:
        wait_for_approved_block_received_handler_state(context, bootstrap)
        transfer_amount = 20000000
        alice_rev_address = ALICE_KEY.get_public_key().get_rev_address()
        bob_rev_address = BOB_KEY.get_public_key().get_rev_address()
        _, alice_balance = get_vault_balance(context, bootstrap, alice_rev_address, ALICE_KEY, 1000000, 1)
        _, bob_balance = get_vault_balance(context, bootstrap, bob_rev_address, ALICE_KEY, 1000000, 1)
        assert alice_balance == 50000000 - 1000000
        assert bob_balance == 0

        transfer_funds(context, bootstrap, alice_rev_address, bob_rev_address, transfer_amount, ALICE_KEY, 1000000, 1)

        _, alice_balance = get_vault_balance(context, bootstrap, alice_rev_address, ALICE_KEY, 1000000, 1)

        _, bob_balance = get_vault_balance(context, bootstrap, bob_rev_address, ALICE_KEY, 1000000, 1)
        assert bob_balance == transfer_amount

def test_transfer_failed_with_invalid_key(command_line_options: CommandLineOptions, docker_client: DockerClient, random_generator: Random) -> None:
    genesis_vault = {
        CHARLIE_KEY: 50000000,
        ALICE_KEY: 50000000
    }
    with testing_context(command_line_options, random_generator, docker_client, wallets_dict=genesis_vault) as context, \
            started_bootstrap_with_network(context=context) as bootstrap:
        wait_for_approved_block_received_handler_state(context, bootstrap)
        bob_rev_address = BOB_KEY.get_public_key().get_rev_address()
        charlie_rev_address = CHARLIE_KEY.get_public_key().get_rev_address()

        _, bob_balance = get_vault_balance(context, bootstrap, bob_rev_address, CHARLIE_KEY, 1000000, 1)
        assert bob_balance == 0

        with pytest.raises(TransderFundsError) as e:
            transfer_funds(context, bootstrap, charlie_rev_address, bob_rev_address, 100, ALICE_KEY, 1000000, 1)
        assert e.value.reason == "Invalid AuthKey"
        _, bob_balance = get_vault_balance(context, bootstrap, bob_rev_address, CHARLIE_KEY, 1000000, 1)
        assert bob_balance == 0


def test_transfer_failed_with_insufficient_funds(command_line_options: CommandLineOptions, docker_client: DockerClient, random_generator: Random) -> None:
    genesis_vault = {
        CHARLIE_KEY: 5000000,
        ALICE_KEY: 1000000
    }
    with testing_context(command_line_options, random_generator, docker_client, wallets_dict=genesis_vault) as context, \
            started_bootstrap_with_network(context=context) as bootstrap:
        wait_for_approved_block_received_handler_state(context, bootstrap)
        bob_rev_address = BOB_KEY.get_public_key().get_rev_address()
        alice_rev_address = ALICE_KEY.get_public_key().get_rev_address()

        _, bob_balance = get_vault_balance(context, bootstrap, bob_rev_address, CHARLIE_KEY, 1000000, 1)
        _, alice_balance = get_vault_balance(context, bootstrap, alice_rev_address, CHARLIE_KEY, 1000000, 1)
        assert bob_balance == 0
        assert alice_balance < 2000000

        with pytest.raises(TransderFundsError) as e:
            transfer_funds(context, bootstrap, alice_rev_address, bob_rev_address, 2000000, ALICE_KEY, 1000000, 1)
        assert e.value.reason == "Insufficient funds"
        _, bob_balance = get_vault_balance(context, bootstrap, bob_rev_address, CHARLIE_KEY, 1000000, 1)
        assert bob_balance == 0


def test_transfer_to_not_exist_vault(command_line_options: CommandLineOptions, docker_client: DockerClient, random_generator: Random) -> None:
    genesis_vault = {
        CHARLIE_KEY: 500000000,
        ALICE_KEY: 500000000
    }

    not_exist_vault = PrivateKey.generate()
    with testing_context(command_line_options, random_generator, docker_client, wallets_dict=genesis_vault) as context, \
            started_bootstrap_with_network(context=context) as bootstrap:

        transfer_amount = 2000000
        wait_for_approved_block_received_handler_state(context, bootstrap)
        alice_rev_address = ALICE_KEY.get_public_key().get_rev_address()
        no_exist_address = not_exist_vault.get_public_key().get_rev_address()

        _, alice_balance = get_vault_balance(context, bootstrap, alice_rev_address, CHARLIE_KEY, 1000000, 1)
        assert alice_balance == 500000000

        with pytest.raises(WaitTimeoutError):
            # transfer to a vault which does not exist in the genesis vault
            # the result can not be got because the vault is not created in the tuplespace
            log_marker = random_string(context, 10)
            transfer_funds_result_pattern = re.compile('"{} (Successfully|Failing) reason: (?P<reason>[a-zA-Z0-9 ]*)"'.format(log_marker))
            deploy_transfer(log_marker, bootstrap, alice_rev_address, no_exist_address, transfer_amount, ALICE_KEY, 1000000, 1)
            wait_transfer_result(context, bootstrap, transfer_funds_result_pattern)

        # the get_vault_balance contract would call the method `findOrCreate` to generate the not-exist vault
        # then the transfer above can get the continuation and transfer is done
        _, no_vault_balance = get_vault_balance(context, bootstrap, no_exist_address, CHARLIE_KEY, 1000000, 1)
        wait_transfer_result(context, bootstrap, transfer_funds_result_pattern)
        assert no_vault_balance == transfer_amount
