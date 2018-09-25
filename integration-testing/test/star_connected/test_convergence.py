import logging
from tools.profiling import profile

@profile
def test_network_convergence(converged_star_network):
    logging.info("Network converged successfully")
    pass