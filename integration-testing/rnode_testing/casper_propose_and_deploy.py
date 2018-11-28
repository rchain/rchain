import os
import shutil
import logging

from rnode_testing.wait import wait_for, string_contains, get_block
from rnode_testing.util import log_box
from rnode_testing.random import random_string

import typing
if typing.TYPE_CHECKING:
    from rnode_testing.rnode import Node
    from conftest import TestConfig
    from rnode_testing.network import RChain

def deploy_block(node: "Node", expected_string: str, contract_name: str) -> str:
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


def check_blocks(node: "Node", expected_string: str, network: "RChain", config: "TestConfig", block_hash: str) -> None:
    logging.info(f"Check all peer logs for blocks containing {expected_string}")

    other_nodes = [n for n in network.nodes if n.container.name != node.container.name]

    for node in other_nodes:
        wait_for(
            string_contains(get_block(node, block_hash), expected_string),
            config.receive_timeout,
            f"Container: {node.container.name}: String {expected_string} NOT found in blocks added.",
        )

        logging.info(f"Container: {node.container.name}: SUCCESS!")


def mk_expected_string(node: "Node", random_token: str) -> str:
    return "<{name}:{random_token}>".format(name=node.container.name, random_token=random_token)


def run(config: "TestConfig", network: "RChain") -> None:
    """
    Deploy a contract and then checks if all the nodes have received the block containing the contract.
    """

    token_size = 20

    contract_name = 'contract.rho'

    for node in network.nodes:
        with log_box(logging.info, "Run test on node '{}'".format(node.name)):
            random_token = random_string(token_size)

            expected_string = mk_expected_string(node, random_token)
            block_hash = deploy_block(node, expected_string, contract_name)

            expected_string = mk_expected_string(node, random_token)
            check_blocks(node, expected_string, network, config, block_hash)
