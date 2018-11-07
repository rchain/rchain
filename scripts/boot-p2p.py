#!/usr/bin/env python3.6
# This is a simple script to boot a rchain p2p network using bootstrap/peer/bond named containers
# It deletes bootstrap/peer named containers before it creates them leaving other named containers
# This requires Python 3.6 to be installed for f-string. Install dependencies via pip
# python3.6 -m pip install docker ed25519
# Return code of 0 is success on test and 1 is fail.
# Example below shows how to boot network with 3 nodes, including bootstrap, and run specific test
# sudo ./boot-p2p.py -m 34360m -c 1 -p 3  --genesis --sigs 2 --bonds <bond_file_path> --wallet <wallet_file_path> --has-faucet  -i rchain-integration-testing:latest --remove

import argparse
import docker
import os
import time
import sys
import random
import collections

parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument("-c", "--cpuset-cpus",
                    dest='cpuset_cpus',
                    type=str,
                    default="0",
                    help="set docker cpuset-cpus for all nodes. Allows limiting execution in specific CPUs")

parser.add_argument("-i", "--image",
                    dest='image',
                    type=str,
                    default="coop.rchain/rnode:latest",
                    help="source repo for docker image")

parser.add_argument("-m", "--memory",
                    dest='memory',
                    type=str,
                    default="2048m",
                    help="set docker memory limit for all nodes")

parser.add_argument("-n", "--network",
                    dest='network',
                    type=str,
                    default="rchain.coop",
                    help="set docker network name")

parser.add_argument("-p", "--peers-amount",
                    dest='peer_amount',
                    type=int,
                    default="2",
                    help="set total amount of peers for network")

parser.add_argument("-r", "--remove",
                    action='store_true',
                    help="forcibly remove containers that start with bootstrap and peer associated to network name")

parser.add_argument("--bonds",
                    dest="bonds_file",
                    type=str,
                    help="set the bond file of the genesis process")

parser.add_argument("--wallet",
                    dest="wallet_file",
                    type=str,
                    help="set the wallet file of the genesis process")

parser.add_argument("--genesis",
                    dest="genesis",
                    action='store_true',
                    help="set if start with the genesis process")

parser.add_argument("--sigs",
                    dest="sigs",
                    type=int,
                    default=0,
                    help="set the required signatures , this equals the number of nodes bonded at genesis")

parser.add_argument("--has-faucet",
                    dest="has_faucet",
                    action="store_true",
                    default=False,
                    help="set if the chain support has-faucet")

# Print -h/help if no args
if len(sys.argv) == 1:
    parser.print_help(sys.stderr)
    sys.exit(1)

# Define globals
args = parser.parse_args()

if args.sigs > args.peer_amount:
    raise Exception('Sigs should be lower than peer amount')

read_only_peer_count = args.peer_amount - args.sigs

KeyPair = collections.namedtuple("KeyPair", ["private_key", "public_key"])

bonds_file = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'demo-bonds.txt')
keys_file = os.path.join(os.path.dirname(os.path.realpath(__file__)), "demo-validator-private-public-key-pairs.txt")
client = docker.from_env()
RNODE_CMD = '/opt/docker/bin/rnode'
# bonds_file = f'/tmp/bonds.{args.network}' alternate when dynamic bonds.txt creation/manpiulation file works
rnode_directory = "/var/lib/rnode"
container_bonds_file = f'{rnode_directory}/genesis/bonds.txt'
container_wallets_file = f'{rnode_directory}/genesis/wallets.txt'
peer_prefix_command = ['run', '--bootstrap',
                       f'rnode://cb74ba04085574e9f0102cc13d39f0c72219c5bb@bootstrap.{args.network}?protocol=40400&discovery=40404']

temp_bonds_file = f'/tmp/bonds_{random.randint(10000,99999)}'

bond_key_pairs = []
peer_key_pairs = []


def main():
    """Main program"""
    # Check is the docket network exist and create one if not.
    networks = [network.name for network in client.networks.list()]
    if args.network not in networks:
        client.networks.create(args.network)
    if args.remove:
        # only removes boostrap/peer.rchain.coop or .network nodes
        remove_resources_by_network(args.network)
    boot_p2p_network()


def remove_resources_by_network(args_network):
    """Remove bootstrap/peer resources in network name."""
    print(f"Removing bootstrap/peer named containers for docker network {args_network}")
    for container in client.containers.list(all=True, filters={"name": f"peer\d.{args.network}"}):
        container.remove(force=True, v=True)
    for container in client.containers.list(all=True, filters={"name": f"bootstrap.{args.network}"}):
        container.remove(force=True, v=True)
    for container in client.containers.list(all=True, filters={"name": f"bond\d.{args.network}"}):
        container.remove(force=True, v=True)
    # client.volumes.prune() # Removes unused volumes
    return 0


def boot_p2p_network():
    create_bootstrap_node()
    time.sleep(30)  # Give bootstrap node an early start
    print("Starting bonded nodes")
    create_bonded_nodes()
    print("Starting peer nodes.")
    create_peer_nodes()


def generate_validator_private_key():
    import ed25519
    signing_key, verifying_key = ed25519.create_keypair()
    encoded_private_key = signing_key.to_ascii(encoding="base16").decode('utf-8')
    encoded_public_key = verifying_key.to_ascii(encoding="base16").decode('utf-8')
    return KeyPair(private_key=encoded_private_key, public_key=encoded_public_key)


def modify_bonds_file():
    # In order to produce a bonds.txt which contains the key pair of the bonded node which is going to start,
    # the bonded nodes key pair should be put into the bonds file
    # So I have to add the key pairs to the bonds file.
    print('Modify bonds file and add bonds key pair to the bond file')
    if args.bonds_file:
        with open(args.bonds_file) as bond_f:
            content = bond_f.read()
    with open(temp_bonds_file, 'w') as f:
        if args.bonds_file:
            f.write(content)
        for i in range(args.sigs):
            key_pair = generate_validator_private_key()
            bond_key_pairs.append(key_pair)
            f.write(f'{key_pair.public_key} {random.randint(50,1000)}\n')


def create_bootstrap_node():
    """Create bootstrap node."""

    # Create key/cert pem files to be loaded into rnode volume
    bootstrap_node_demo_key = (
        "-----BEGIN PRIVATE KEY-----\n"
        "MIGTAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBHkwdwIBAQQgYcybGU15SCs2x+5I\n"
        "JHrzzBHZ0c7k2WwokG6yU754XKKgCgYIKoZIzj0DAQehRANCAAR/MkqpcKUE+NtM\n"
        "d8q7/IPih2vO6oMjm2ltSA2nSrueNd+jpLvxDQpRYScJBDyeylfB1VkPdpw9oqFQ\n"
        "Y5huc38x\n"
        "-----END PRIVATE KEY-----\n"
    )
    bootstrap_node_demo_cert = (
        "-----BEGIN CERTIFICATE-----\n"
        "MIIBXzCCAQKgAwIBAgIIU0qinJbBW5MwDAYIKoZIzj0EAwIFADAzMTEwLwYDVQQD\n"
        "EyhjYjc0YmEwNDA4NTU3NGU5ZjAxMDJjYzEzZDM5ZjBjNzIyMTljNWJiMB4XDTE4\n"
        "MDYxMjEzMzEyM1oXDTE5MDYxMjEzMzEyM1owMzExMC8GA1UEAxMoY2I3NGJhMDQw\n"
        "ODU1NzRlOWYwMTAyY2MxM2QzOWYwYzcyMjE5YzViYjBZMBMGByqGSM49AgEGCCqG\n"
        "SM49AwEHA0IABH8ySqlwpQT420x3yrv8g+KHa87qgyObaW1IDadKu54136Oku/EN\n"
        "ClFhJwkEPJ7KV8HVWQ92nD2ioVBjmG5zfzEwDAYIKoZIzj0EAwIFAANJADBGAiEA\n"
        "62Po1SVQyJB/2UeG5B9O1oTTlhYrLvLTWH24YiH4U4kCIQDrPa3Qop3yq83Egdq0\n"
        "VkEqI2rycmgp03DXsStJ7IGdBQ==\n"
        "-----END CERTIFICATE-----\n"
    )
    tmp_file_key = '/tmp/node.key.pem'
    tmp_file_cert = '/tmp/node.certificate.pem'
    with open(tmp_file_key, 'w') as f:
        f.write(bootstrap_node_demo_key)
    with open(tmp_file_cert, 'w') as f:
        f.write(bootstrap_node_demo_cert)


    print("Starting bootstrap node.")
    name = f"bootstrap.{args.network}"
    print(f"creating {name}")

    timestamp = int(time.time() * 1000)

    command = ['run', "--standalone",
               "--host", name,
               "--deploy-timestamp", f"{timestamp}",
               "--map-size", "17179869184",
               ]
    volume = {
        tmp_file_cert: {
            "bind": os.path.join(rnode_directory, 'node.certificate.pem'),
            "mode": 'rw'},
        tmp_file_key: {"bind": os.path.join(rnode_directory, 'node.key.pem'),
                            "mode": 'rw'}
    }

    modify_bonds_file()
    if args.bonds_file or args.sigs:
        command.extend(["--bonds-file", container_bonds_file])
        volume.update({temp_bonds_file: {"bind": container_bonds_file, "mode": 'rw'}})
    if args.wallet_file:
        command.extend(["--wallets-file", container_wallets_file])
        volume.update({args.wallet_file: {"bind": container_wallets_file, "mode": 'rw'}})
    if args.has_faucet:
        command.append("--has-faucet")
    container = client.containers.run(args.image,
                                      name=name,
                                      user='root',
                                      detach=True,
                                      cpuset_cpus=args.cpuset_cpus,
                                      mem_limit=args.memory,
                                      network=args.network,
                                      volumes=volume,
                                      command=command,
                                      hostname=name)
    return container


def create_peer_node(i, key_pair, name_prefix):
    name = f"{name_prefix}{i}.{args.network}"
    print(
        f"creating {name_prefix} node {name} with private key:{key_pair.private_key} and public key:{key_pair.public_key}")
    command = peer_prefix_command.copy()
    keys_command = ["--validator-private-key", key_pair.private_key,
                    "--validator-public-key", key_pair.public_key,
                    "--host", name]
    command.extend(keys_command)
    container = client.containers.run(args.image,
                                      name=name,
                                      user='root',
                                      detach=True,
                                      cpuset_cpus=args.cpuset_cpus,
                                      mem_limit=args.memory,
                                      network=args.network,
                                      command=command,
                                      hostname=name)
    return container


def create_bonded_nodes():
    print("Create and run bonded nodes to connect via bootstrap.")

    for i, key_pair in enumerate(bond_key_pairs):
        create_peer_node(i, key_pair, "bond")


def create_peer_nodes():
    """Create peer nodes."""
    print("Create and run peer nodes to connect via bootstrap.")

    for i in range(read_only_peer_count):
        key_pair = generate_validator_private_key()
        create_peer_node(i, key_pair, "peer")
    return 0


if __name__ == "__main__":
    main()
