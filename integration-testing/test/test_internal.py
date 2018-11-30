"""Tests for the testing code itself."""

from rnode_testing.rnode import (
    extract_block_hash_from_propose_output,
    extract_block_count_from_show_blocks,
)


def test_blocks_count_from_show_blocks() -> None:
    show_blocks_output = '''
------------- block 0 ---------------
blockHash: "630c5372c67cc5400a9eb11459bb240226273a693bbb018df829a3119b26bbf0"
blockSize: "99746"
blockNumber: 0
version: 1
deployCount: 10
tupleSpaceHash: "f2fdac324a5fa86f58d3e8162ad5108d9bc75773311d32bb9bc36b74c632793a"
timestamp: 1
faultTolerance: 1.0
mainParentHash: ""
sender: ""

-----------------------------------------------------


count: 123

'''

    assert extract_block_count_from_show_blocks(show_blocks_output) == 123


def test_extract_block_hash_from_propose_output() -> None:
    response = "Response: Success! Block a91208047c... created and added.\n"
    assert extract_block_hash_from_propose_output(response) == "a91208047c"
