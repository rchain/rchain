from tools.profiling import profile
from tests.casper_propose_and_deploy import casper_propose_and_deploy

@profile
def test_casper_propose_and_deploy(config, converged_complete_network):
    casper_propose_and_deploy(config, converged_complete_network)