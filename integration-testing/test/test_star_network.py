import pytest
import test.casper_propose_and_deploy
from tools.network import start_network, wait_for_started_network, wait_for_converged_network
from tools.rnode import start_bootstrap
import logging

@pytest.fixture(scope="module")
def star_network(config, docker, validators_data):
    with start_bootstrap(docker,
                         config.node_startup_timeout,
                         config.rnode_timeout,
                         validators_data) as bootstrap_node:

        with start_network(config,
                           docker,
                           bootstrap_node,
                           validators_data,
                           [bootstrap_node.name]) as network:

            wait_for_started_network(config.node_startup_timeout, network)

            wait_for_converged_network(config.network_converge_timeout, network, 1)

            yield network

def test_convergence(star_network):
    logging.info("Star network converged successfully.")

@pytest.mark.skip(reason="This doesn't work since the show-blocks functionality was removed")
def test_casper_propose_and_deploy(star_network):
    test.casper_propose_and_deploy.run(star_network)