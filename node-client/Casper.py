'''RChain casper client

Usage:
  $ export FLASK_APP=Casper.py
  $ flask run

We assume the RChain node is running and that it is listening on port
5000. Double-check that you see this message in the logs:

  Server started, listening on 5000

There are 2 api provided in the flask app.

1. Navigate to http://localhost:5000/api/blocks

This api shows all the blocks in the chain.

The output should be something like:
{
    "blocks": [
        {
            "faultTolerance": "-0.8666666746139526",
            "hash": "8a50ce909d3605b43538fbe2da92289cb14549223c3dd726bef647535db5153b",
            "merkleroot": "00a956e671e3ba40d09cb68e8b22f4f5fdab68f68713d2046a6811ad13e6f521",
            "pool_info": {},
            "shardId": "rchain",
            "size": "40519",
            "time": "0",
            "tupleSpaceDump": "@{\"proofOfStake\"}!(.........",
            "txlength": "8"
        }
        ],
    "length": 1
}

2. Navigate to http://localhost:5000/api/block/<block_hash>

This api shows the specific block by the block_hash you offer.

The output should be something like:
{
    "faultTolerance": "-0.8666666746139526",
    "hash": "8a50ce909d3605b43538fbe2da92289cb14549223c3dd726bef647535db5153b",
    "merkleroot": "00a956e671e3ba40d09cb68e8b22f4f5fdab68f68713d2046a6811ad13e6f521",
    "pool_info": {},
    "shardId": "rchain",
    "size": "40519",
    "time": "0",
    "tupleSpaceDump": "@{\"proofOfStake\"}!({\"00f3bd4ebf3f2723e........",
    "tx": [],
    "txlength": "8"
}

Tested with rnode-0.6.4 Sep 13 2018.

'''

from __future__ import print_function

# cribbed from https://grpc.io/docs/tutorials/basic/python.html
import CasperMessage_pb2
import CasperMessage_pb2_grpc

from sys import argv, stdout
from grpc import insecure_channel
from flask import Flask
from flask import jsonify
import json
import math

def buildCasperCh(argv, stdout, insecure_channel,
                  host='127.0.0.1',
                  port=40401):
    channel = insecure_channel('%s:%s' % (host, port))
    return CasperMessage_pb2_grpc.DeployServiceStub(channel)

app = Flask(__name__)
casperCh = buildCasperCh(argv, stdout, insecure_channel)


rename = {"blockHash": "hash", "blockNumber": "height", "blockSize": "size",
          "timestamp": "time", "deployCount": "txlength", "mainParentHash": "previousblockhash",
          "tupleSpaceHash": "merkleroot"}

value_adjuster = {"time": lambda milliseconds : math.floor(milliseconds / 1000)}

def add_field_to_block_dict(block_dict, field):
    field_name = field[0].name
    renamed_field_name = rename.get(field_name) if rename.get(field_name) else field_name
    field_value = field[1]
    adjusted_field_value = str(value_adjuster[renamed_field_name](field_value)) if value_adjuster.get(
        renamed_field_name) else str(field_value)
    block_dict.update({renamed_field_name: adjusted_field_value})

@app.route("/api/block/<block_hash>")
def block(block_hash):
    req = CasperMessage_pb2.BlockQuery(hash=block_hash)
    output = casperCh.showBlock(req)
    block_dict = {"tx": [], "pool_info": {}}
    for field in output.blockInfo.ListFields():
        add_field_to_block_dict(block_dict, field)
    return jsonify(block_dict)

@app.route("/api/blocks")
def blocks():
    req = CasperMessage_pb2.google_dot_protobuf_dot_empty__pb2.Empty()
    output = casperCh.showBlocks(req)
    blocks = []
    for block in output:
        block_dict = {"pool_info": {}}
        for field in block.ListFields():
            add_field_to_block_dict(block_dict, field)
        blocks.append(block_dict)
    return jsonify({"blocks": blocks, "length": len(blocks)})