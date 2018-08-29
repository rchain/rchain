import collections

class RChain:
    def __init__(self, network, bootstrap, peers):
        self.network = network
        self.bootstrap = bootstrap
        self.peers = peers
        self.nodes = [bootstrap] + peers


Config = collections.namedtuple( "Config",
                                 [
                                     "peer_count",
                                     "node_startup_timeout",
                                     "network_converge_timeout",
                                     "receive_timeout",
                                     "rnode_timeout"
                                 ])


KeyPair = collections.namedtuple("KeyPair", ["private_key", "public_key"])