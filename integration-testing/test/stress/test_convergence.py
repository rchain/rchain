import logging
from tools.profiling import profile
from tools.fixture import parametrize

@profile
@parametrize.cartesian(test_id=range(1,10))
def test_network_convergence(converged_complete_network, test_id):
    logging.info(f"Test {test_id}: Network converged successfully")
    pass