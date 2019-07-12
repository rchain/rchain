import base64
import logging
import socket
from concurrent import futures
from contextlib import contextmanager
from queue import Queue
from typing import Generator, Iterator
import os
import subprocess
from pathlib import Path
import grpc
from cryptography.hazmat.backends import default_backend
from cryptography.hazmat.backends.openssl.ec import _EllipticCurvePrivateKey
from cryptography.hazmat.primitives.serialization import load_pem_private_key
from docker import DockerClient
from eth_hash.auto import keccak
from rchain.pb import CasperMessage_pb2
from rchain.pb.CasperMessage_pb2 import BlockMessage, BlockRequest
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

from .rnode import Node as RNode

DEFAULT_TRANSPORT_SERVER_PORT = 40400
DEFAULT_NETWORK_ID = 'testnet'

logger = logging.getLogger("node_client")


def get_node_id_raw(key: _EllipticCurvePrivateKey) -> bytes:
    curve = key.public_key().public_numbers()
    pk_bytes = curve.x.to_bytes(32, 'big') + curve.y.to_bytes(32, 'big')
    return keccak(pk_bytes)[12:]


def get_node_id_str(key: _EllipticCurvePrivateKey) -> str:
    raw_id = get_node_id_raw(key)
    return base64.b16encode(raw_id).lower().decode('utf8')


def get_free_tcp_port() -> int:
    tcp = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
        tcp.bind(('', 0))
        _, port = tcp.getsockname()
    finally:
        tcp.close()
    return port

@contextmanager
def get_node_ip_of_network(docker_client: DockerClient, network_name: str) -> Generator[str, None, None]:
    """
    In a drone mode, the current pytest process is within a docker container.In order
    to create the connection with the node, we have to attach the current pytest container
    to the node network.

    In a local linux mode, the current pytest process is a normal system process which is
    running in the host machine. So as for the container, the gateway ip is the host
    machine ip.

    WARNING: For Windows and MacOS, you can not connect to the container from host
    without any other configuration. For more info, see https://github.com/docker/for-mac/issues/2670.
    """
    if os.environ['DRONE'] == 'true':
        current_container_id = get_current_container_id()
        current_container = docker_client.containers.get(current_container_id)
        network = docker_client.networks.get(network_name)
        try:
            network.connect(current_container)
            network.reload()
            container_network_config =network.attrs['Containers'][current_container.id]
            ip, _ = container_network_config['IPv4Address'].split('/')
            yield ip
        finally:
            network.disconnect(current_container)
    else:
        network_attr = docker_client.networks.get(network_name).attrs
        ipam_attr = network_attr.get("IPAM")
        assert ipam_attr is not None
        ip = ipam_attr['Config'][0]['Gateway']
        yield ip


def get_current_container_id() -> str:
    hostname = subprocess.run(['hostname'], stdout=subprocess.PIPE)
    return hostname.stdout.decode('utf8').strip("\n")


class TransportServer(TransportLayerServicer):
    def __init__(self, node: Node, networkId: str, return_queue: Queue):
        self.node = node
        self.header = Header(sender=self.node, networkId=networkId)
        self.return_queue = return_queue
        super(TransportServer, self).__init__()

    def Send(self, request: TLRequest, context: grpc.ServicerContext) -> TLResponse:
        return TLResponse(noResponse=Ack(header=self.header))

    def Stream(self, request_iterator: Iterator[Chunk], context: grpc.ServicerContext) -> TLResponse:
        message_cls = None
        data = b''
        for chunk in request_iterator:
            content_type = chunk.WhichOneof('content')
            if content_type == 'header':
                typeId = chunk.header.typeId
                message_cls = getattr(CasperMessage_pb2, typeId)
            elif content_type == 'data':
                data = chunk.data.contentData
            else:
                raise NotImplementedError()
        assert message_cls is not None
        stream_message = message_cls()
        stream_message.ParseFromString(data)
        self.return_queue.put(stream_message)
        return TLResponse(noResponse=Ack(header=self.header))


class NodeClient:
    def __init__(self, node_pem_cert: bytes, node_pem_key: bytes, host: str, network_name: str, network_id: str = DEFAULT_NETWORK_ID):
        self.node_pem_cert = node_pem_cert
        self.node_pem_key = node_pem_key
        self.ec_key = load_pem_private_key(self.node_pem_key, password=None, backend=default_backend())
        self.network_id = network_id

        self.host = host
        self.tcp_port = 0
        self.udp_port = get_free_tcp_port()
        self.network_name = network_name  # docker network name

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
        rnode.container.reload()
        network_config = rnode.container.attrs['NetworkSettings']['Networks'][self.network_name]
        assert network_config is not None
        return network_config['IPAddress']

    def _start_transport_server(self) -> grpc.Server:
        server_credential = grpc.ssl_server_credentials([(self.node_pem_key, self.node_pem_cert)])
        server = grpc.server(futures.ThreadPoolExecutor())
        add_TransportLayerServicer_to_server(TransportServer(self.node_pb, self.network_id, self.return_queue), server)
        self.tcp_port = server.add_secure_port(self.host, server_credential)
        server.start()
        return server

    def block_request(self, block_hash: str, rnode: RNode) -> BlockMessage:
        block_request = BlockRequest(base16Hash=block_hash, hash=base64.b16decode(block_hash, True))
        request_msg_packet = Packet(typeId="BlockRequest", content=block_request.SerializeToString())
        protocol = Protocol(header=self.header_pb, packet=request_msg_packet)
        request = TLRequest(protocol=protocol)
        self.send_request(request, rnode)
        return self.return_queue.get(timeout=20)

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
def node_protocol_client(network_name: str, docker_client: DockerClient) -> Generator[NodeClient, None, None]:
    cert = Path("resources/bootstrap_certificate/protocol.cert.pem").read_bytes()
    key = Path("resources/bootstrap_certificate/protocol.key.pem").read_bytes()

    with get_node_ip_of_network(docker_client, network_name) as node_ip:
        protocol_client = NodeClient(cert, key, node_ip, network_name)
        try:
            yield protocol_client
        finally:
            protocol_client.stop()
