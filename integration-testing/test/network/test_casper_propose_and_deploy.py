from tools.random import random_string
import re
import logging
from delayed_assert import expect, assert_expectations
import tools.resources as resources
from shutil import copyfile
from tools.wait import wait_for, string_matches, string_equals, node_logs, network_converged, node_blocks_received, node_blocks_added, find_first

def test_casper_propose_and_deploy(converged_network):
    """
    This test represents an integration test that deploys a contract and then checks
    if all the nodes have received the block containing the contract.
    """

    token_size = 20
    receive_timeout = 5

    contract_name = 'contract.rho'

    for node in converged_network.nodes:
        expected_string = f"<{node.container.name}:{random_string(token_size)}>"

        logging.info(f"Run test on container {node.container.name}. Expected string: {expected_string}")

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
            blocks_received_ids = wait_for(list_find_first(node_blocks_received(node), string_contains(expected_string)),
                                           f"Container: {node.container.name}: String {expected_string} NOT found in blocks added.")

            logging.info(f"Container: {node.container.name}: Received blocks found for {expected_string}: {blocks_received_ids}")

            assert len(blocks_received_ids) == 1, f"Too many blocks received: {blocks_received_ids}"

            block_id = blocks_received_ids[0]

            blocks_added = wait_for(list_find_first(node_blocks_added(node), string_equals(block_id)),
                                    f"Container: {node.container.name}: Added blocks not found for {blocks_received_ids}")

    assert_expectations()