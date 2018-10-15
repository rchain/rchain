import pytest
import rnode_testing.casper_propose_and_deploy
from rnode_testing.network import start_network, wait_for_started_network, wait_for_converged_network
from rnode_testing.rnode import start_bootstrap
import logging

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
