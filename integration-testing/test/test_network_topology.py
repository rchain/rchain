import logging
import pytest
from delayed_assert import expect, assert_expectations
from rnode_testing.profiling import profile
import rnode_testing.casper_propose_and_deploy
from rnode_testing.network import (
    start_network,
    wait_for_started_network,
    wait_for_converged_network,
    wait_for_approved_block_received_handler_state,
    wait_for_approved_block_received,
)
from rnode_testing.rnode import start_bootstrap

from typing import TYPE_CHECKING, Iterator
if TYPE_CHECKING:
    from conftest import System
    from rnode_testing.network import RChain


@pytest.fixture(scope="module")
def star_network(system: "System") -> Iterator["RChain"]:
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
def complete_network(system: "System") -> Iterator["RChain"]:
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

@profile
def test_metrics_api_socket(complete_network: "RChain") -> None:
    for node  in complete_network.nodes:
        logging.info("Test metrics api socket for {}".format(node.name))
        exit_code, output = node.get_metrics()
        expect(exit_code == 0, "Could not get the metrics for node {node.name}")

    assert_expectations()


@profile
def test_node_logs_for_errors(complete_network: "RChain") -> None:
    for node in complete_network.nodes:
        logging.info("Testing {} node logs for errors.".format(node.name))
        logs = node.logs()

        if "ERROR" in logs:
            for line in logs.splitlines():
                if "ERROR" in line:
                    logging.error("Error: {}".format(line))
            expect(not "ERROR" in line, "Node {name} error in log line: {line}".format(name=node.name, line=line))

    assert_expectations()

@profile
def test_node_logs_for_RuntimeException(complete_network: "RChain") -> None:
    for node in complete_network.nodes:
        logging.info("Testing {} node logs for \"java RuntimeException\".".format(node.name))
        logs = node.logs()


        if "RuntimeException" in logs:
            for line in logs.splitlines():
                if "RuntimeException" in line:
                    logging.error("Error: {}".format(line))
            expect(not "RuntimeException" in line, "Node {name} error in log line: {line}".format(name=node.name, line=line))

    assert_expectations()

@profile
def test_casper_propose_and_deploy(system: "System", complete_network: "RChain") -> None:
    rnode_testing.casper_propose_and_deploy.run(system.config, complete_network)

def test_convergence(complete_network: "RChain") -> None:
    logging.info("Complete network converged successfully.")
