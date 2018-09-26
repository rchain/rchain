
import logging
from tools.wait import wait_for, string_contains, show_blocks
from tools.util import log_box
from tools.random import random_string
import tools.resources as resources
from shutil import copyfile


def run(config, network):
    """
    This test represents an integration test that deploys a contract and then checks
    if all the nodes have received the block containing the contract.
    """

    token_size = 20

    contract_name = 'contract.rho'

    def mk_expected_string(node, i, random_token) : return f"<{node.container.name}:{i}:{random_token}>"

    def deploy_block(i, node, expected_string):
        logging.info(f"Expected string: {expected_string}")

        copyfile(resources.file_path(contract_name, __name__), f"{node.local_deploy_dir}/{contract_name}")

        exit_code, output = node.exec_run(f"sed -i -e 's/@placeholder@/{expected_string}/g' {node.remote_deploy_dir}/{contract_name}")
        logging.debug(f"Sed result: {exit_code}, output: {output}")

        exit_code, output = node.deploy(contract_name)
        logging.debug(f"Deploy result: {exit_code}, output: {output}")

        logging.info("Propose to blockchain previously deployed smart contracts.")

        exit_code, output = node.propose()
        logging.debug(f"Propose result: {exit_code}, output: {output}")


    def check_blocks(i, node, expected_string):
        logging.info(f"Check all peer logs for blocks containing {expected_string}")

        other_nodes = [n
                       for n in network.nodes
                       if n.container.name != node.container.name]

        for node in other_nodes:
            wait_for( string_contains(show_blocks(node), expected_string),
                      config.receive_timeout,
                      f"Container: {node.container.name}: String {expected_string} NOT found in blocks added.")

            logging.info(f"Container: {node.container.name}: block {i} : SUCCESS!")

    for node in network.nodes:
        with log_box(logging.info, f"Run test on node '{node.name}'"):
            random_token = random_string(token_size)

            for i in range(0,config.blocks):
                deploy_block(i, node, mk_expected_string(node, i, random_token))

            for i in range(0, config.blocks):
                check_blocks(i, node, mk_expected_string(node, i, random_token))