import re
from collections import defaultdict
from dataclasses import dataclass
from typing import List, Dict, Set

from .error import (
    UnexpectedDeployOutputFormatError,
    UnexpectedProposeOutputFormatError,
    UnexpectedShowBlocksOutputFormatError
)

@dataclass
class DeployInfo():
    deployer: str
    term: str
    timestamp: int
    sig: str
    sig_algorithm: str
    phlo_price: int
    phlo_limit: int
    valid_after_block_number: int
    cost: int
    error: str
    system_deploy_error: str

@dataclass  # pylint: disable=too-many-instance-attributes
class LightBlockInfo():
    block_hash: str
    sender: str
    seq_num: int
    sig: str
    sig_algorithm: str
    shard_id: str
    extra_bytes: str
    version: str
    timestamp: int
    header_extra_bytes: str
    parents: List[str]
    block_number: int
    pre_state_hash: str
    post_state_hash: str
    body_extra_bytes: str
    bonds: Dict[str, int]
    block_size: int
    deploy_count: int
    fault_tolerance: float

@dataclass
class BlockInfo(LightBlockInfo):
    deploys: List[DeployInfo]

bond_pattern = '''bonds {
    validator: "(?P<validator>[a-zA-Z0-9]*)"
    stake: (?P<stake>[0-9]+)
  }
'''

bond_pattern_2 = '''bonds {
  validator: "(?P<validator>[a-zA-Z0-9]*)"
  stake: (?P<stake>[0-9]+)
}
'''
deploy_pattern = r'''deploys {
  deployer: "(?P<deployer>[a-zA-Z0-9]*)"
  term: "(?P<term>.*)"
  timestamp: (?P<d_timestamp>[0-9]+)
  sig: "(?P<d_sig>[a-zA-Z0-9]*)"
  sigAlgorithm: "(?P<d_sigAlgorithm>[a-zA-Z0-9]*)"
  phloPrice: (?P<tiphloPricemestamp>[0-9]+)
  phloLimit: (?P<phloLimit>[0-9]+)
  validAfterBlockNumber: (?P<validAfterBlockNumber>[+-]?\d+)
  cost: (?P<cost>[0-9]+)
  errored: (?P<errored>[a-zA-Z0-9]*)
  systemDeployError: "(?P<systemDeployError>.*)"
}'''
parent_pattern = r'parentsHashList: "(?P<parent>[a-zA-Z0-9]*)"'

block_pattern = r'''blockInfo {
  blockHash: "(?P<blockHash>[a-zA-Z0-9]*)"
  sender: "(?P<sender>[a-zA-Z0-9]*)"
  seqNum: (?P<seqNum>[0-9]+)
  sig: "(?P<sig>[a-zA-Z0-9]*)"
  sigAlgorithm: "(?P<sigAlgorithm>[a-zA-Z0-9]*)"
  shardId: "(?P<shardId>[a-zA-Z0-9]*)"
  extraBytes: "(?P<extraBytes>[a-zA-Z0-9]*)"
  version: (?P<version>[0-9]+)
  timestamp: (?P<timestamp>[0-9]+)
  headerExtraBytes: "(?P<headerExtraBytes>[a-zA-Z0-9]*)"
(?P<parents>(  parentsHashList: "[a-zA-Z0-9]*"\n)*)  blockNumber: (?P<blockNumber>[0-9]+)
  preStateHash: "(?P<preStateHash>[a-zA-Z0-9]*)"
  postStateHash: "(?P<postStateHash>[a-zA-Z0-9]*)"
  bodyExtraBytes: "(?P<bodyExtraBytes>[a-zA-Z0-9]*)"
  (?P<bonds>(.*\n)+)  blockSize: "(?P<blockSize>[0-9]+)"
  deployCount: (?P<deployCount>[0-9]+)
  faultTolerance: (?P<faultTolerance>[+-]?\d+(?:\.\d+)?)
}
(?P<deploys>(.*\n)+)
'''

blocks_info_pattern = r'''blockHash: "(?P<blockHash>[a-zA-Z0-9]*)"
sender: "(?P<sender>[a-zA-Z0-9]*)"
seqNum: (?P<seqNum>[0-9]+)
sig: "(?P<sig>[a-zA-Z0-9]*)"
sigAlgorithm: "(?P<sigAlgorithm>[a-zA-Z0-9]*)"
shardId: "(?P<shardId>[a-zA-Z0-9]*)"
extraBytes: "(?P<extraBytes>[a-zA-Z0-9]*)"
version: (?P<version>[0-9]+)
timestamp: (?P<timestamp>[0-9]+)
headerExtraBytes: "(?P<headerExtraBytes>[a-zA-Z0-9]*)"
(?P<parents>.*?)blockNumber: (?P<blockNumber>[0-9]+)
preStateHash: "(?P<preStateHash>[a-zA-Z0-9]*)"
postStateHash: "(?P<postStateHash>[a-zA-Z0-9]*)"
bodyExtraBytes: "(?P<bodyExtraBytes>[a-zA-Z0-9]*)"
(?P<bonds>.*?)blockSize: "(?P<blockSize>[0-9]+)"
deployCount: (?P<deployCount>[0-9]+)
faultTolerance: (?P<faultTolerance>[+-]?\d+(?:\.\d+)?)
'''

def extract_block_count_from_show_blocks(show_blocks_output: str) -> int:
    block_count_pattern = re.compile("count: (?P<count>[0-9]+)")
    count_match = block_count_pattern.findall(show_blocks_output)
    if not count_match:
        raise UnexpectedShowBlocksOutputFormatError(show_blocks_output)
    result = int(count_match[0])
    return result


def parse_show_blocks_output(show_blocks_output: str) -> List[LightBlockInfo]:
    block_info_find = re.findall(blocks_info_pattern, show_blocks_output, re.DOTALL)
    if not block_info_find:
        raise UnexpectedShowBlocksOutputFormatError(show_blocks_output)
    result = []
    for block_info in block_info_find:
        parents = re.findall(parent_pattern, block_info[10])
        bonds = re.findall(bond_pattern_2, block_info[15])
        result.append(LightBlockInfo(
            block_hash= block_info[0],
            sender=block_info[1],
            seq_num = int(block_info[2]),
            sig= block_info[3],
            sig_algorithm= block_info[4],
            shard_id= block_info[5],
            extra_bytes= block_info[6],
            version= block_info[7],
            timestamp= int(block_info[8]),
            header_extra_bytes= block_info[9],
            parents= parents,
            block_number= int(block_info[11]),
            pre_state_hash= block_info[12],
            post_state_hash= block_info[13],
            body_extra_bytes= block_info[14],
            bonds= {bond[0]: int(bond[1]) for bond in bonds},
            block_size= int(block_info[16]),
            deploy_count= int(block_info[17]),
            fault_tolerance= float(block_info[18])
        ))
    return result


def parse_show_block_output(show_block_output: str) -> BlockInfo:
    block_info_match = re.match(block_pattern, show_block_output)
    parents_find = re.findall(parent_pattern, show_block_output)
    deploys_find = re.findall(deploy_pattern, show_block_output)
    bonds_find = re.findall(bond_pattern, show_block_output)
    if not bool(block_info_match or parents_find or deploys_find or bonds_find):
        raise UnexpectedShowBlocksOutputFormatError(show_block_output)
    assert block_info_match
    result = block_info_match.groupdict()
    deploys = [DeployInfo(
        deployer= d[0],
        term= d[1],
        timestamp= int(d[2]),
        sig= d[3],
        sig_algorithm= d[4],
        phlo_price= int(d[5]),
        phlo_limit= int(d[6]),
        valid_after_block_number= int(d[7]),
        cost=  int(d[8]),
        error= d[9],
        system_deploy_error= d[10]) for d in deploys_find]
    block_info = BlockInfo(
        block_hash= result['blockHash'],
        sender=result['sender'],
        seq_num = int(result['seqNum']),
        sig= result['sig'],
        sig_algorithm= result['sigAlgorithm'],
        shard_id= result['shardId'],
        extra_bytes= result['extraBytes'],
        version= result['version'],
        timestamp= int(result['timestamp']),
        header_extra_bytes= result['headerExtraBytes'],
        parents= parents_find,
        block_number= int(result['blockNumber']),
        pre_state_hash= result['preStateHash'],
        post_state_hash= result['postStateHash'],
        body_extra_bytes= result['bodyExtraBytes'],
        bonds= {bond[0]: int(bond[1]) for bond in bonds_find},
        block_size= int(result['blockSize']),
        deploy_count= int(result['deployCount']),
        fault_tolerance= float(result['faultTolerance']),
        deploys = deploys
    )
    return block_info

def extract_block_hash_from_propose_output(propose_output: str) -> str:
    """We're getting back something along the lines of:

    Response: Success! Block a91208047c... created and added.\n
    """
    match = re.match(r'Response: Success! Block ([0-9a-f]+) created and added.', propose_output.strip())
    if match is None:
        raise UnexpectedProposeOutputFormatError(propose_output)
    return match.group(1)

def parse_mvdag_str(mvdag_output: str) -> Dict[str, Set[str]]:
    dag_dict: Dict[str, Set[str]] = defaultdict(set)

    lines = mvdag_output.splitlines()
    for line in lines:
        parent_hash, child_hash = line.split(' ')
        dag_dict[parent_hash].add(child_hash)
    return dag_dict

def extract_deploy_id_from_deploy_output(deploy_output: str) -> str:
    match = re.match(r'Response: Success!\nDeployId is: ([0-9a-f]+)', deploy_output.strip())
    if match is None:
        raise UnexpectedDeployOutputFormatError(deploy_output)
    return match.group(1)