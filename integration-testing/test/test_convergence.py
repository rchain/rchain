import logging
from tools.fixture import parametrize
from fixtures.network import *

@parametrize.cartesian(network=[complete_network, star_network])
def test_network_convergence(network):
    logging.info("Network converged successfully")