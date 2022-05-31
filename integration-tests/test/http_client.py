import time
from dataclasses import dataclass
from typing import Optional, List, Union, Dict
import requests
from rchain.crypto import PrivateKey
from rchain.pb.CasperMessage_pb2 import DeployDataProto
from rchain.util import sign_deploy_data

class HttpRequestException(Exception):
   def __init__(self, status_code: int, content: str):
       super().__init__()
       self.status_code = status_code
       self.content = content


@dataclass
class VersionInfo:
    api: str
    node: str


@dataclass
class ApiStatus:
    version: VersionInfo
    address: str
    network_id: str
    shard_id: str
    peers: int
    nodes: int


@dataclass
class DataResponse:
    exprs: List[Union[str, int]]
    length: int

def _check_reponse(response: requests.Response) -> None:
    if response.status_code != requests.codes.ok:  # pylint: disable=no-member
        raise HttpRequestException(response.status_code, response.text)

class HttpClient():
    def __init__(self, host: str, port: int):
        super().__init__()
        self.host = host
        self.port = port
        self.url = "http://{}:{}/api".format(host, port)


    def status(self) -> ApiStatus:
        status_url = self.url+'/status'
        rep = requests.get(status_url)
        _check_reponse(rep)
        message = rep.json()
        return ApiStatus(
            version=VersionInfo(
                api=message['version']['api'],
                node=message['version']['node']),
            address=message['address'],
            network_id=message['networkId'],
            shard_id=message['shardId'],
            peers=message['peers'],
            nodes=message['nodes'])


    def deploy(self, term: str, phlo_limit: int, phlo_price: int, valid_after_block_number: int, deployer: PrivateKey, shard_id: str = '') -> str:
        timestamp = int(time.time()* 1000)
        deploy_data = {
            "term": term,
            "timestamp": timestamp,
            "phloLimit": phlo_limit,
            "phloPrice": phlo_price,
            "validAfterBlockNumber": valid_after_block_number,
            "shardId": shard_id
        }
        deploy_proto = DeployDataProto(term=term, timestamp=timestamp, phloLimit=phlo_limit, phloPrice=phlo_price, validAfterBlockNumber=valid_after_block_number, shardId=shard_id)
        deploy_req = {
            "data": deploy_data,
            "deployer": deployer.get_public_key().to_hex(),
            "signature": sign_deploy_data(deployer, deploy_proto).hex(),
            "sigAlgorithm": "secp256k1"
        }
        deploy_url = self.url + '/deploy'
        rep = requests.post(deploy_url, json=deploy_req)
        _check_reponse(rep)
        return rep.text

    def data_at_name(self, name: str, depth: int, name_type: str) -> DataResponse:
        data_at_name_url = self.url + '/data-at-name'
        rep =requests.post(data_at_name_url, json={"name": {name_type: {"data": name}}, "depth": depth})
        _check_reponse(rep)
        message = rep.json()
        return DataResponse(exprs=message['exprs'], length=message['length'])

    def data_at_name_by_block_hash(self, name: str, data, block_hash: str, use_pre_state_hash: bool):
        data_at_name_by_block_hash_url = self.url + '/data-at-name-by-block-hash'
        rep = requests.post(data_at_name_by_block_hash_url,
                            json={"name": {name: {"data": data}}, "blockHash": block_hash,
                                  "usePreStateHash": use_pre_state_hash})
        _check_reponse(rep)
        return rep.json()

    def last_finalized_block(self) -> Dict:
        last_finalized_block_url = self.url + '/last-finalized-block'
        rep = requests.get(last_finalized_block_url)
        _check_reponse(rep)
        return rep.json()

    def get_block(self, block_hash: str) -> Dict:
        block_url = self.url + '/block/' + block_hash
        rep = requests.get(block_url)
        _check_reponse(rep)
        return rep.json()

    def get_blocks(self, depth: Optional[int]) -> List[Dict]:
        if depth:
            blocks_url = self.url + "/blocks/{}".format(depth)
        else:
            blocks_url = self.url + "/blocks"
        rep = requests.get(blocks_url)
        _check_reponse(rep)
        return rep.json()

    def get_deploy(self, deploy_id: str) -> Dict:
        deploy_url = self.url + "/deploy/{}".format(deploy_id)
        rep = requests.get(deploy_url)
        _check_reponse(rep)
        return rep.json()
