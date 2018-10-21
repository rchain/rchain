import logging
from rnode_testing.wait import wait_for, string_contains, show_blocks
from rnode_testing.util import log_box
from rnode_testing.random import random_string
import rnode_testing.resources as resources
from shutil import copyfile
import os


def mk_expected_string(node, i, random_token):
    return f"<{node.container.name}:{i}:{random_token}>"


def deploy_block(i, node, expected_string, contract_path):
    logging.info(f"Expected string: {expected_string}")

    contract_name = os.path.basename(contract_path)

    copyfile(resources.get_resource_path(contract_path), f"{os.path.join(node.local_deploy_dir, contract_name)}")

    exit_code, output = node.exec_run(f"sed -i -e 's/@placeholder@/{expected_string}/g' {os.path.join(node.remote_deploy_dir, contract_name)}")
    logging.debug(f"Sed result: {exit_code}, output: {output}")

    exit_code, output = node.deploy_contract(contract_name)
    logging.debug(f"Deploy result: {exit_code}, output: {output}")

    logging.info("Propose to blockchain previously deployed smart contracts.")

    exit_code, output = node.propose_contract()
    logging.debug(f"Propose result: {exit_code}, output: {output}")


def check_blocks(blocks_num, node, expected_string, network, config):
    logging.info(f"Check all peer logs for blocks containing {expected_string}")

    other_nodes = [n
                    for n in network.nodes
                    if n.container.name != node.container.name]

    for node in other_nodes:
        wait_for(string_contains(show_blocks(node, blocks_num + 1), expected_string),  # i plus 1 genesis block
                    config.receive_timeout,
                    f"Container: {node.container.name}: String {expected_string} NOT found in blocks added.")

        logging.info(f"Container: {node.container.name}: block {i} : SUCCESS!")


def run(config, network):
    """
    Deploy a contract and then checks if all the nodes have received the block containing the contract.
    """

    token_size = 20

    contract_path = 'test/casper_propose_and_deploy/contract.rho'

    for node in network.nodes:
        with log_box(logging.info, f"Run test on node '{node.name}'"):
            random_token = random_string(token_size)

            for i in range(0, config.blocks):
                expected_string = mk_expected_string(node, i, random_token)
                deploy_block(i, node, expected_string, contract_path)

            for i in range(0, config.blocks):
                check_blocks(config.blocks, node, mk_expected_string(node, i, random_token), network, config)
