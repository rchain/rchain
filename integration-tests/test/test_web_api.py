from typing import Generator, Tuple, List
import pytest
from docker import DockerClient
from .rnode import Node
from .conftest import STANDALONE_KEY
from .utils import get_node_ip_of_network
from .http_client import HttpClient

SHARD_ID = ''

@pytest.fixture(scope="module")
def node_with_blocks(started_standalone_bootstrap_node: Node, docker_client: DockerClient) -> Generator[Tuple[Node, List[str], List[str]], None, None]:
    deploy_hash = []
    block_hash = []
    deploy_hash.append(started_standalone_bootstrap_node.deploy('/opt/docker/examples/tut-hello.rho', STANDALONE_KEY, 100000, 1, shard_id=SHARD_ID))
    block_hash.append(started_standalone_bootstrap_node.propose())
    deploy_hash.append(started_standalone_bootstrap_node.deploy('/opt/docker/examples/tut-hello.rho', STANDALONE_KEY, 100000, 1, shard_id=SHARD_ID))
    block_hash.append(started_standalone_bootstrap_node.propose())
    deploy_hash.append(started_standalone_bootstrap_node.deploy('/opt/docker/examples/tut-hello.rho', STANDALONE_KEY, 100000, 1, shard_id=SHARD_ID))
    block_hash.append(started_standalone_bootstrap_node.propose())
    with get_node_ip_of_network(docker_client, started_standalone_bootstrap_node.network):
        yield (started_standalone_bootstrap_node, deploy_hash, block_hash)


def test_web_api(node_with_blocks: Tuple[Node, List[str], List[str]]) -> None :
    node = node_with_blocks[0]
    deploy_hash = node_with_blocks[1]
    block_hash = node_with_blocks[2]
    ip = node.get_peer_node_ip(node.network)
    client = HttpClient(ip, 40403)

    status = client.status()
    assert status.version
    prepare_rep = client.prepare_deploy()
    assert prepare_rep.seq_number == 3
    prepare_rep_2 = client.prepare_deploy(STANDALONE_KEY.get_public_key().to_hex(), 1, 1)
    assert prepare_rep_2.seq_number == 3

    data_at_name = client.data_at_name(deploy_hash[0], 1, "UnforgDeploy")
    assert data_at_name.length == 0
    assert data_at_name.exprs == []

    last_finalized = client.last_finalized_block()
    assert "blockInfo" in last_finalized
    assert "deploys" in last_finalized

    block = client.get_block(block_hash[0])
    assert "blockInfo" in block
    assert "deploys" in block

    assert len(client.get_blocks(10)) == 4

    deploy_block = client.get_deploy(deploy_hash[0])
    assert "blockHash" in deploy_block
    assert "seqNum" in deploy_block

    ret = client.deploy("@2!(1)", 100000, 1, 5, STANDALONE_KEY, shard_id=SHARD_ID)
    assert ret is not None

