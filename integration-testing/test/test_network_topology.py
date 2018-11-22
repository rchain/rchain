import logging
import pytest
from delayed_assert import expect, assert_expectations
from rnode_testing.profiling import profile
import rnode_testing.casper_propose_and_deploy
from rnode_testing.network import start_network, wait_for_started_network, wait_for_converged_network
from rnode_testing.rnode import start_bootstrap

@pytest.fixture(scope="module")
def star_network(system):
    with start_bootstrap(system.docker,
                         system.config.node_startup_timeout,
                         system.config.rnode_timeout,
                         system.validators_data) as bootstrap_node:

        with start_network(system.config,
                           system.docker,
                           bootstrap_node,
                           system.validators_data,
                           [bootstrap_node.name]) as network:

            wait_for_started_network(system.config.node_startup_timeout, network)

            wait_for_converged_network(system.config.network_converge_timeout, network, 1)

            yield network

def test_convergence(star_network):
    logging.info("Star network converged successfully.")

@pytest.mark.skip(reason="This doesn't work since the show-blocks functionality was removed")
def test_casper_propose_and_deploy(star_network):
    rnode_testing.casper_propose_and_deploy.run(star_network)


@pytest.fixture(scope="module")
def complete_network(system):
    with start_bootstrap(system.docker,
                         system.config.node_startup_timeout,
                         system.config.rnode_timeout,
                         system.validators_data) as bootstrap_node:

        with start_network(system.config,
                           system.docker,
                           bootstrap_node,
                           system.validators_data) as network:

            wait_for_started_network(system.config.node_startup_timeout, network)

            wait_for_converged_network(system.config.network_converge_timeout, network, len(network.peers))

            yield network

@profile
def test_metrics_api_socket(complete_network):
    for node  in complete_network.nodes:
        logging.info("Test metrics api socket for {}".format(node.name))
        exit_code, output = node.get_metrics()
        expect(exit_code == 0, "Could not get the metrics for node {node.name}")

    assert_expectations()


@profile
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

@profile
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

@pytest.mark.skip(reason="This doesn't work since the show-blocks functionality was removed")
@profile
def test_casper_propose_and_deploy(config, complete_network):
    rnode_testing.casper_propose_and_deploy.run(config, complete_network)

def test_convergence(complete_network):
    logging.info("Complete network converged successfully.")
