from tools.random import random_string
import logging
import tools.resources as resources
from shutil import copyfile
from tools.wait import wait_for, string_contains, show_blocks
from tools.util import log_box
from tools.profiling import profile


@profile
def test_casper_propose_and_deploy(config, converged_network):
    """
    This test represents an integration test that deploys a contract and then checks
    if all the nodes have received the block containing the contract.
    """

    token_size = 20

    contract_name = 'contract.rho'

    for node in converged_network.nodes:
        with log_box(logging.info, f"Run test on node '{node.name}'"):
            expected_string = f"<{node.container.name}:{random_string(token_size)}>"


            logging.info(f"Expected string: {expected_string}")

            copyfile(resources.file_path(contract_name, __name__), f"{node.local_deploy_dir}/{contract_name}")

            exit_code, output = node.exec_run(f"sed -i -e 's/@placeholder@/{expected_string}/g' {node.remote_deploy_dir}/{contract_name}")
            logging.debug(f"Sed result: {exit_code}, output: {output}")

            exit_code, output = node.deploy(contract_name)
            logging.debug(f"Deploy result: {exit_code}, output: {output}")

            logging.info("Propose to blockchain previously deployed smart contracts.")

            exit_code, output = node.propose()
            logging.debug(f"Propose result: {exit_code}, output: {output}")


            logging.info(f"Check all peer logs for blocks containing {expected_string}")

            other_nodes = [n
                            for n in converged_network.nodes
                            if n.container.name != node.container.name]

            for node in other_nodes:
                wait_for( string_contains(show_blocks(node), expected_string),
                          config.receive_timeout,
                          f"Container: {node.container.name}: String {expected_string} NOT found in blocks added.")

                logging.info(f"Container: {node.container.name}: SUCCESS!")