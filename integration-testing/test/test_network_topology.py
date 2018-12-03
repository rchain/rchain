import os
import shutil
import logging
import contextlib
from typing import (
    TYPE_CHECKING,
    Generator,
)

import pytest


import conftest
from rnode_testing.common import TestingContext, Network
from rnode_testing.rnode import (
    docker_network_with_started_bootstrap,
    create_peer_nodes,
)
from rnode_testing.common import random_string
from rnode_testing.wait import (
    wait_for_block_contains,
    wait_for_approved_block_received_handler_state,
    wait_for_started_network,
    wait_for_converged_network,
    wait_for_approved_block_received,
)

if TYPE_CHECKING:
    from rnode_testing.rnode import Node


@contextlib.contextmanager
def start_network(*, context: TestingContext, bootstrap: 'Node', allowed_peers=None) -> Generator[Network, None, None]:
    peers = create_peer_nodes(
        docker_client=context.docker,
        bootstrap=bootstrap,
        network=bootstrap.network,
        bonds_file=context.bonds_file,
        key_pairs=context.peers_keypairs,
        command_timeout=context.command_timeout,
        allowed_peers=allowed_peers,
    )

    try:
        yield Network(network=bootstrap.network, bootstrap=bootstrap, peers=peers)
    finally:
        for peer in peers:
            peer.cleanup()


@contextlib.contextmanager
def star_network(context: TestingContext) -> Generator[Network, None, None]:
    with docker_network_with_started_bootstrap(context) as bootstrap_node:
        with start_network(context=context, bootstrap=bootstrap_node, allowed_peers=[bootstrap_node.name]) as network:
            wait_for_started_network(context.node_startup_timeout, network)
            wait_for_converged_network(context.network_converge_timeout, network, 1)
            yield network


@contextlib.contextmanager
def complete_network(context: TestingContext) -> Generator[Network, None, None]:
    with docker_network_with_started_bootstrap(context) as bootstrap_node:
        wait_for_approved_block_received_handler_state(bootstrap_node, context.node_startup_timeout)
        with start_network(context=context, bootstrap=bootstrap_node) as network:
            wait_for_started_network(context.node_startup_timeout, network)
            wait_for_converged_network(context.network_converge_timeout, network, len(network.peers))
            wait_for_approved_block_received(network, context.node_startup_timeout)
            yield network


def test_metrics_api_socket(command_line_options_fixture, docker_client_fixture):
    with conftest.testing_context(command_line_options_fixture, docker_client_fixture) as context:
        with complete_network(context) as network:
            for node in network.nodes:
                exit_code, _ = node.get_metrics()
                assert exit_code == 0, "Could not get the metrics for node {node.name}"


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


def check_blocks(node, expected_string, network, context, block_hash):
    logging.info("Check all peer logs for blocks containing {}".format(expected_string))

    other_nodes = [n for n in network.nodes if n.container.name != node.container.name]

    for node in other_nodes:
        wait_for_block_contains(node, block_hash, expected_string, context.receive_timeout)


def mk_expected_string(node, random_token):
    return "<{name}:{random_token}>".format(name=node.container.name, random_token=random_token)


def casper_propose_and_deploy(context, network):
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
        check_blocks(node, expected_string, network, context, block_hash)


def test_casper_propose_and_deploy(command_line_options_fixture, docker_client_fixture):
    with conftest.testing_context(command_line_options_fixture, docker_client_fixture) as context:
        with complete_network(context) as network:
            casper_propose_and_deploy(context, network)


def test_convergence(command_line_options_fixture, docker_client_fixture):
    with conftest.testing_context(command_line_options_fixture, docker_client_fixture) as context:
        with complete_network(context) as network:
            pass
