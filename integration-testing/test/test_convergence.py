import logging
from tools.fixture import parametrize
from fixtures.network import *

@parametrize.cartesian(network=[converged_complete_network, converged_star_network])
def test_network_convergence(network):
    logging.info("Network converged successfully")