import logging
from rnode_testing.wait import wait_for, string_contains, show_blocks
from rnode_testing.util import log_box
from rnode_testing.random import random_string
import rnode_testing.resources as resources
from shutil import copyfile


def mk_expected_string(node, i, random_token):
    return "<{node.container.name}:{i}:{random_token}>".format(name=node.container.name, i=i, random_token=random_token)


def deploy_block(i, node, expected_string, contract_name):
    logging.info("Expected string: {expected_string}".format(expected_string=expected_string))

    copyfile(resources.file_path(contract_name, __name__), "{local_deploy_dir}/{contract_name}".format(local_deploy_dir=node.local_deploy_dir, contract_name=contract_name))

    exit_code, output = node.exec_run(
        "sed -i -e 's/@placeholder@/{expected_string}/g' {remote_deploy_dir}/{contract_name}".format(
            expected_string=expected_string, remote_deploy_dir=node.remote_deploy_dir,contract_name=contract_name))
    logging.debug("Sed result: {exit_code}, output: {output}".format(exit_code=exit_code, output=output))

    exit_code, output = node.deploy_contract(contract_name)
    logging.debug("Deploy result: {exit_code}, output: {output}".format(exit_code=exit_code, output=output))

    logging.info("Propose to blockchain previously deployed smart contracts.")

    exit_code, output = node.propose_contract()
    logging.debug("Propose result: {exit_code}, output: {output}".format(exit_code=exit_code, output=output))


def check_blocks(i, node, expected_string):
    logging.info("Check all peer logs for blocks containing {expected_string}".format(expected_string=expected_string))

    other_nodes = [n
                    for n in network.nodes
                    if n.container.name != node.container.name]

    for node in other_nodes:
        wait_for(string_contains(show_blocks(node), expected_string),
                    config.receive_timeout,
                    "Container: {name}: String {expected_string} NOT found in blocks added.".format(name=node.container.name, expected_string=expected_string))

        logging.info("Container: {name}: block {i} : SUCCESS!".format(name=node.container.name, i=i))


def run(config, network):
    """
    Deploy a contract and then checks if all the nodes have received the block containing the contract.
    """

    token_size = 20

    contract_name = 'contract.rho'

    for node in network.nodes:
        with log_box(logging.info, "Run test on node '{name}'".format(name=node.name)):
            random_token = random_string(token_size)

            for i in range(0, config.blocks):
                expected_string = mk_expected_string(node, i, random_token)
                deploy_block(i, node, expected_string, contract_name)

            for i in range(0, config.blocks):
                check_blocks(i, node, mk_expected_string(node, i, random_token))
