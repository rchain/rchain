import os
import shutil
import logging

import pytest
from delayed_assert import expect, assert_expectations

from rnode_testing.network import (
    start_network,
)
from rnode_testing.rnode import start_bootstrap
from rnode_testing.common import random_string
from rnode_testing.wait import (
    wait_for,
    string_contains,
    get_block,
    wait_for_approved_block_received_handler_state,
    wait_for_started_network,
    wait_for_converged_network,
    wait_for_approved_block_received,
)



@pytest.fixture(scope="module")
def star_network(system):
    with start_bootstrap(system.docker,
                         system.config.node_startup_timeout,
                         system.config.rnode_timeout,
                         system.validators_data,
                         mount_dir=system.config.mount_dir) as bootstrap_node:

        with start_network(system.config,
                           system.docker,
                           bootstrap_node,
                           system.validators_data,
                           [bootstrap_node.name]) as network:

            wait_for_started_network(system.config.node_startup_timeout, network)

            wait_for_converged_network(system.config.network_converge_timeout, network, 1)

            yield network


@pytest.fixture(scope="module")
def complete_network(system):
    with start_bootstrap(system.docker,
                         system.config.node_startup_timeout,
                         system.config.rnode_timeout,
                         system.validators_data,
                         mount_dir=system.config.mount_dir,
        ) as bootstrap_node:

        wait_for_approved_block_received_handler_state(bootstrap_node, system.config.node_startup_timeout)

        with start_network(system.config,
                           system.docker,
                           bootstrap_node,
                           system.validators_data) as network:

            wait_for_started_network(system.config.node_startup_timeout, network)

            wait_for_converged_network(system.config.network_converge_timeout, network, len(network.peers))

            wait_for_approved_block_received(network, system.config.node_startup_timeout)

            yield network


def test_metrics_api_socket(complete_network):
    for node  in complete_network.nodes:
        logging.info("Test metrics api socket for {}".format(node.name))
        exit_code, output = node.get_metrics()
        expect(exit_code == 0, "Could not get the metrics for node {node.name}")

    assert_expectations()


def test_node_logs_for_errors(complete_network):
    for node in complete_network.nodes:
        logging.info("Testing {} node logs for errors.".format(node.name))
        logs = node.logs()

        if "ERROR" in logs:
            for line in logs.splitlines():
                if "ERROR" in line:
                    logging.error("Error: {}".format(line))
            expect(not "ERROR" in line, "Node {name} error in log line: {line}".format(name=node.name, line=line))

    assert_expectations()


def test_node_logs_for_RuntimeException(complete_network):
    for node in complete_network.nodes:
        logging.info("Testing {} node logs for \"java RuntimeException\".".format(node.name))
        logs = node.logs()


        if "RuntimeException" in logs:
            for line in logs.splitlines():
                if "RuntimeException" in line:
                    logging.error("Error: {}".format(line))
            expect(not "RuntimeException" in line, "Node {name} error in log line: {line}".format(name=node.name, line=line))

    assert_expectations()


def deploy_block(node, expected_string, contract_name):
    local_contract_file_path = os.path.join('resources', contract_name)
    shutil.copyfile(local_contract_file_path, f"{node.local_deploy_dir}/{contract_name}")
    container_contract_file_path = '{}/{}'.format(node.remote_deploy_dir, contract_name)
    node.shell_out(
        'sed',
        '-i',
        '-e', 's/@placeholder@/{}/g'.format(expected_string),
        container_contract_file_path,
    )
    node.deploy(container_contract_file_path)
    block_hash = node.propose()
    return block_hash


def check_blocks(node, expected_string, network, config, block_hash):
    logging.info("Check all peer logs for blocks containing {}".format(expected_string))

    other_nodes = [n for n in network.nodes if n.container.name != node.container.name]

    for node in other_nodes:
        wait_for(
            string_contains(get_block(node, block_hash), expected_string),
            config.receive_timeout,
            "Container: {}: String {} NOT found in blocks added.".format(node.container.name, expected_string),
        )

        logging.info("Container: {}: SUCCESS!".format(node.container.name))


def mk_expected_string(node, random_token):
    return "<{name}:{random_token}>".format(name=node.container.name, random_token=random_token)


def casper_propose_and_deploy(config, network):
    """Deploy a contract and then checks if all the nodes have received the block
    containing the contract.
    """

    token_size = 20
    contract_name = 'contract.rho'
    for node in network.nodes:
        logging.info("Run test on node '{}'".format(node.name))

        random_token = random_string(token_size)

        expected_string = mk_expected_string(node, random_token)
        block_hash = deploy_block(node, expected_string, contract_name)

        expected_string = mk_expected_string(node, random_token)
        check_blocks(node, expected_string, network, config, block_hash)


def test_casper_propose_and_deploy(system, complete_network):
    casper_propose_and_deploy(system.config, complete_network)


def test_convergence(complete_network):
    # complete_network fixture does the job
    pass
