'''RChain casper client

Usage:
  $ export FLASK_APP=Casper.py
  $ flask run

  Navigate to http://localhost:5000/api/block/<block_hash>

We assume the RChain node is running and that it is listening on port
5000. Double-check that you see this message in the logs:

  Server started, listening on 50000

The output should be something like:
  {
    "blockHash": "9310ded826...",
    "blockNumber": "1",
    "parentsHashList": "['6b52036b26fcc8bc17140c8aad712f65e97d7b7d603727d5934bbbc3f893db9b']",
    "status": "Success",
    "tsDesc": "@{11}!(11) | for( x0 <= @{\"stdout\"} ) { Nil } | for( x0 <= @{\"stderr\"} ) { Nil } | for( x0, x1 <= @{\"stderrAck\"} ) { Nil } | for( x0, x1 <= @{\"stdoutAck\"} ) { Nil }"
  }

Tested with rnode-assembly-0.3.1.jar from commit 2424b43caae May 22 2018.

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
         port=50000):
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
    for block in output.blocks:
        block_dict = {"pool_info": {}}
        for field in block.ListFields():
            add_field_to_block_dict(block_dict, field)
        blocks.append(block_dict)
    return jsonify({"blocks": blocks, "length": output.length})