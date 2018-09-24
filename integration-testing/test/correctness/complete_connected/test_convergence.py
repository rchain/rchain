import logging
from tools.profiling import profile

@profile
def test_network_convergence(converged_complete_network):
    logging.info("Network converged successfully")
    pass