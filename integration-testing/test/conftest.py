from fixtures.common import *



def pytest_addoption(parser):
    parser.addoption(
        "--peer-count", action="store", default="2", help="number of peers in the network (excluding bootstrap node)"
    )
    parser.addoption(
        "--start-timeout", action="store", default="0", help="timeout in seconds for starting a node. Defaults to 30 + peer_count * 10"
    )
    parser.addoption(
        "--converge-timeout", action="store", default="0", help="timeout in seconds for network converge. Defaults to 200 + peer_count * 10"
    )
    parser.addoption(
        "--receive-timeout", action="store", default="0", help="timeout in seconds for receiving a message. Defaults to 10 + peer_count * 10"
    )
    parser.addoption(
        "--rnode-timeout", action="store", default="10", help="timeout in seconds for executing an rnode call (Examples: propose, show-logs etc.). Defaults to 10s"
    )