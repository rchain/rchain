from tools.profiling import profile
from tests.casper_propose_and_deploy import casper_propose_and_deploy
from tools.fixture import parametrize
from fixtures.network import *

@profile
@parametrize.cartesian(network=[#converged_complete_network,
                                converged_star_network])
def test_casper_propose_and_deploy(config, network):
    casper_propose_and_deploy(config, network)