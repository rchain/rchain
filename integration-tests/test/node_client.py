import logging
from concurrent import futures
from contextlib import contextmanager
from queue import Queue, Empty
from typing import Generator, Iterator
from pathlib import Path
import grpc
from cryptography.hazmat.backends import default_backend
from cryptography.hazmat.backends.openssl.ec import _EllipticCurvePrivateKey
from cryptography.hazmat.primitives.serialization import load_pem_private_key
from eth_hash.auto import keccak
from rchain.pb import CasperMessage_pb2
from rchain.pb.CasperMessage_pb2 import BlockMessageProto as BlockMessage, BlockRequestProto as BlockRequest
from rchain.pb.routing_pb2 import (
    Chunk,
    Header,
    Node,
    Ack,
    Packet,
    Protocol,
    TLRequest,
    TLResponse
)
from rchain.pb.routing_pb2_grpc import (
    TransportLayerServicer,
    TransportLayerStub,
    add_TransportLayerServicer_to_server
)
from docker import DockerClient

from .utils import get_node_ip_of_network, get_free_tcp_port
from .rnode import Node as RNode
from .common import TestingContext

DEFAULT_TRANSPORT_SERVER_PORT = 40400
DEFAULT_NETWORK_ID = 'testnet'

logger = logging.getLogger("node_client")


class BlockNotFound(Exception):
    def __init__(self, block_hash: str, rnode: RNode):
        super().__init__()
        self.block_hash = block_hash
        self.rnode = rnode


def get_node_id_raw(key: _EllipticCurvePrivateKey) -> bytes:
    curve = key.public_key().public_numbers()
    pk_bytes = curve.x.to_bytes(32, 'big') + curve.y.to_bytes(32, 'big')
    return keccak(pk_bytes)[12:]


def get_node_id_str(key: _EllipticCurvePrivateKey) -> str:
    raw_id = get_node_id_raw(key)
    return raw_id.hex()


class TransportServer(TransportLayerServicer):
    def __init__(self, node: Node, networkId: str, return_queue: Queue):
        super().__init__()
        self.node = node
        self.header = Header(sender=self.node, networkId=networkId)
        self.return_queue = return_queue
        super().__init__()

    def Send(self, request: TLRequest, context: grpc.ServicerContext) -> TLResponse:
        return TLResponse(noResponse=Ack(header=self.header))

    def Stream(self, request_iterator: Iterator[Chunk], context: grpc.ServicerContext) -> TLResponse:
        message_cls = None
        data = b''
        for chunk in request_iterator:
            content_type = chunk.WhichOneof('content')
            if content_type == 'header':
                typeId = chunk.header.typeId
                message_cls = getattr(CasperMessage_pb2, "{}Proto".format(typeId))
            elif content_type == 'data':
                data = chunk.data.contentData
            else:
                raise NotImplementedError()
        assert message_cls is not None
        stream_message = message_cls()
        stream_message.ParseFromString(data)
        self.return_queue.put(stream_message)
        return TLResponse(ack=Ack(header=self.header))


class NodeClient:
    def __init__(self, node_pem_cert: bytes, node_pem_key: bytes, host: str, network_name: str, receive_timeout: int,
                 network_id: str = DEFAULT_NETWORK_ID):
        self.node_pem_cert = node_pem_cert
        self.node_pem_key = node_pem_key
        self.ec_key = load_pem_private_key(self.node_pem_key, password=None, backend=default_backend())
        self.network_id = network_id

        self.host = host
        self.tcp_port = 0
        self.udp_port = get_free_tcp_port()
        self.network_name = network_name  # docker network name
        self._receive_timeout = receive_timeout

        self.return_queue = Queue() # type: ignore

        self.server = self._start_transport_server()

    @property
    def node_pb(self) -> Node:
        node_id = get_node_id_raw(self.ec_key)
        return Node(id=node_id, host=self.host.encode('utf8'), tcp_port=self.tcp_port, udp_port=self.udp_port)

    @property
    def header_pb(self) -> Header:
        return Header(sender=self.node_pb, networkId=self.network_id)

    def get_peer_node_ip(self, rnode: RNode) -> str:
        return rnode.get_peer_node_ip(self.network_name)

    def _start_transport_server(self) -> grpc.Server:
        server_credential = grpc.ssl_server_credentials([(self.node_pem_key, self.node_pem_cert)])
        server = grpc.server(futures.ThreadPoolExecutor())
        add_TransportLayerServicer_to_server(TransportServer(self.node_pb, self.network_id, self.return_queue), server)
        self.tcp_port = server.add_secure_port("{}:0".format(self.host), server_credential)
        server.start()
        return server

    def block_request(self, block_hash: str, rnode: RNode) -> BlockMessage:
        block_request = BlockRequest(hash=bytes.fromhex(block_hash))
        request_msg_packet = Packet(typeId="BlockRequest", content=block_request.SerializeToString())
        protocol = Protocol(header=self.header_pb, packet=request_msg_packet)
        request = TLRequest(protocol=protocol)
        self.send_request(request, rnode)
        try:
            return self.return_queue.get(timeout=self._receive_timeout)
        except Empty as e:
            raise BlockNotFound(block_hash, rnode) from e

    def send_request(self, request: TLRequest, rnode: RNode) -> None:
        credential = grpc.ssl_channel_credentials(rnode.get_node_pem_cert(), self.node_pem_key, self.node_pem_cert)
        # only linux system can connect to the docker container through the container name
        rnode_ip = self.get_peer_node_ip(rnode)
        channel = grpc.secure_channel(
            "{}:{}".format(rnode_ip, DEFAULT_TRANSPORT_SERVER_PORT),
            credential,
            options=(('grpc.ssl_target_name_override',
                      get_node_id_str(load_pem_private_key(rnode.get_node_pem_key(), None, default_backend()))),)
        )
        try:
            stub = TransportLayerStub(channel)
            stub.Send(request)
        finally:
            channel.close()

    def send_block(self, block: BlockMessage, rnode: RNode) -> None:
        block_msg_packet = Packet(typeId="BlockMessage", content=block.SerializeToString())
        protocol = Protocol(header=self.header_pb, packet=block_msg_packet)
        request = TLRequest(protocol=protocol)
        self.send_request(request, rnode)

    def stop(self) -> None:
        self.server.stop(0)


@contextmanager
def node_protocol_client(network_name: str, docker_client: DockerClient, context: TestingContext) -> Generator[NodeClient, None, None]:
    cert = Path("resources/bootstrap_certificate/protocol.cert.pem").read_bytes()
    key = Path("resources/bootstrap_certificate/protocol.key.pem").read_bytes()

    with get_node_ip_of_network(docker_client, network_name) as node_ip:
        protocol_client = NodeClient(cert, key, node_ip, network_name, context.receive_timeout)
        try:
            yield protocol_client
        finally:
            protocol_client.stop()
