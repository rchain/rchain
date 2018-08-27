#!/usr/bin/env python3.6
# This is a simple script to boot a rchain p2p network using bootstrap/peer named containers
# It deletes bootstrap/peer named containers before it creates them leaving other named containers
# This requires Python 3.6 to be installed for f-string. Install dependencies via pip
# python3.6 -m pip install docker
# Return code of 0 is success on test and 1 is fail.
# Example below shows how to boot network with 3 nodes, including bootstrap, and run specific test
# sudo ./scripts/boot-p2p.py -m 2048m -p 2 -c 0 -i rchain/rnode:dev
# Simple
# sudo ./scripts/boot-p2p.py -i rchain/rnode:dev
import subprocess
import argparse
import docker
import os
import tempfile
import re
import time
import sys
import random


parser = argparse.ArgumentParser(
             formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument("-b", "--boot",
                    action='store_true',
                    help="boot network by creating resources and starting services by network name")
parser.add_argument("--bootstrap-command",
                    dest='bootstrap_command',
                    type=str,
                    default="run --port 40400 --standalone",
                    help="bootstrap container run command")
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
                    default="1024m",
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
parser.add_argument("--rnode-directory",
                    dest='rnode_directory',
                    type=str,
                    default="/var/lib/rnode",
                    help="rnode container root directory on each container")
parser.add_argument("-r", "--remove",
                    action='store_true',
                    help="forcibly remove containers that start with bootstrap and peer associated to network name")
# Print -h/help if no args
if len(sys.argv)==1:
    parser.print_help(sys.stderr)
    sys.exit(1)


# Define globals
args = parser.parse_args()
client = docker.from_env()
RNODE_CMD = '/opt/docker/bin/rnode'
# bonds_file = f'/tmp/bonds.{args.network}' alternate when dynamic bonds.txt creation/manpiulation file works
bonds_file = os.path.dirname(os.path.realpath(__file__)) + '/demo-bonds.txt'
container_bonds_file = f'{args.rnode_directory}/genesis/bonds.txt'
peer_prefix_command=f'run --bootstrap rnode://cb74ba04085574e9f0102cc13d39f0c72219c5bb@bootstrap.{args.network}:40400'


def main():
    """Main program"""
    #if args.remove == True:
    #    # only removes boostrap/peer.rchain.coop or .network nodes
    remove_resources_by_network(args.network)
    boot_p2p_network()


def remove_resources_by_network(args_network):
    """Remove bootstrap/peer resources in network name."""
    print(f"Removing bootstrap/peer named containers for docker network {args_network}")
    for container in client.containers.list(all=True, filters={"name":f"peer\d.{args.network}"}):
            container.remove(force=True, v=True)
    for container in client.containers.list(all=True, filters={"name":f"bootstrap.{args.network}"}):
            container.remove(force=True, v=True)
    # client.volumes.prune() # Removes unused volumes
    return 0


def create_empty_bonds_file():

    # Create or zero out bonds file so it is empty and can be mounted as volumes by containers 
    try:
        with open(bonds_file, 'w+') as f:
            f.write("")
    except IOError as e:
        print(f"Failed to open or write to file {e}.")


def boot_p2p_network():
    try:
        create_bootstrap_node()
        time.sleep(20) # Give bootstrap node an early start
        print("Starting peer nodes.")
        create_peer_nodes()
        return 0
    except Exception as e:
        print(e)
        return 1


def generate_validator_private_key():
    ### Create --validator-private-key and --validator--public-key and add to bonds.txt # ed25519 eventually secp256k1 ###

    # # pynacl for libsodium
    # # import nacl # libsodium/ed25519 support
    # from nacl.public import PrivateKey, PublicKey
    # import nacl.encoding
    # import nacl.signing
    # private_key = PrivateKey.generate()
    # encoded_private_key = private_key.encode(encoder=nacl.encoding.Base16Encoder).decode('utf-8').lower()
    # encoded_public_key = private_key.public_key.encode(encoder=nacl.encoding.Base16Encoder).decode('utf-8').lower()
    # signing_key = nacl.signing.SigningKey.generate() 
    # verify_key = signing_key.verify_key
    # encoded_private_key = signing_key.encode(encoder=nacl.encoding.Base16Encoder).lower()
    # encoded_public_key = verify_key.encode(encoder=nacl.encoding.Base16Encoder).lower() 

    # import ed25519
    # signing_key, verifying_key = ed25519.create_keypair()
    # encoded_private_key = signing_key.to_ascii(encoding="base16").decode('utf-8')
    # encoded_public_key = verifying_key.to_ascii(encoding="base16").decode('utf-8')

    # import ecdsa # secp256k1 suppport

    # Using pre-generated validator key pairs by rnode. We do this because warning below  with python generated keys
    # WARN  coop.rchain.casper.Validate$ - CASPER: Ignoring block 2cb8fcc56e... because block creator 3641880481... has 0 weight
    f=open('scripts/demo-validator-private-public-key-pairs.txt')
    lines=f.readlines()
    line_number = random.randint(1,295)
    encoded_private_key = lines[line_number].split()[0]
    encoded_public_key = lines[line_number].split()[1]

    # print(f"Populating bonds file {bonds_file}")
    # bond_weight = random.randint(1,100)
    # line = f"{encoded_public_key} {bond_weight}"
    # print(line)
    # try:
    #     with open(bonds_file, 'a+') as f:
    #         f.write(f"{line}\n")
    # except IOError as e:
    #     print(f"Failed to open or write to file {e}.") 

    return encoded_private_key, encoded_public_key


def populate_bonds_file():
    print(f"Populating bonds file {bonds_file}")
    try:
        with open(bonds_file, 'a+') as f:
            for line in bonds:
                print(line)
                f.write(f"{line}\n")
    except IOError as e:
        print(f"Failed to open or write to file {e}.") 


def create_bootstrap_node():
    """Create bootstrap node."""

    validator_private_key, validator_public_key = generate_validator_private_key()

    # Create key/cert pem files to be loaded into rnode volume
    bootstrap_node_demo_key=(
        "-----BEGIN PRIVATE KEY-----\n"
        "MIGTAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBHkwdwIBAQQgYcybGU15SCs2x+5I\n"
        "JHrzzBHZ0c7k2WwokG6yU754XKKgCgYIKoZIzj0DAQehRANCAAR/MkqpcKUE+NtM\n"
        "d8q7/IPih2vO6oMjm2ltSA2nSrueNd+jpLvxDQpRYScJBDyeylfB1VkPdpw9oqFQ\n"
        "Y5huc38x\n"
        "-----END PRIVATE KEY-----\n"
    )
    bootstrap_node_demo_cert=(
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
    bootstrap_node = {}
    bootstrap_node['name'] = f"bootstrap.{args.network}"
    bootstrap_node['volume'] = client.volumes.create()
    print(f"creating {bootstrap_node['name']}")
    container = client.containers.run(args.image, \
        name=bootstrap_node['name'], \
        user='root', \
        detach=True, \
        cpuset_cpus=args.cpuset_cpus, \
        mem_limit=args.memory, \
        network=args.network, \
        volumes=[
                f"{bootstrap_node['volume'].name}:{args.rnode_directory}", \
                f"{bonds_file}:{container_bonds_file}", \
                f"{tmp_file_cert}:{args.rnode_directory}/node.certificate.pem", \
                f"{tmp_file_key}:{args.rnode_directory}/node.key.pem"
        ],
        # # Alternate volume mount
        # volumes={
        #         tmp_file_cert: {'bind': f'{args.rnode_directory}/node.certificate.pem', 'mode': 'rw'}, \
        #         tmp_file_key: {'bind': f'{args.rnode_directory}/node.key.pem', 'mode': 'rw'}, \
        #         bonds_file: {'bind': container_bonds_file, 'mode': 'rw'}, \
        #         bootstrap_node['volume'].name: {'bind': args.rnode_directory, 'mode': 'rw'} \
        # }, \
        command=f"{args.bootstrap_command} --validator-private-key {validator_private_key} --validator-public-key {validator_public_key} --host {bootstrap_node['name']}", \
        hostname=bootstrap_node['name'])
    return 0


def create_peer_nodes():
    """Create peer nodes."""
    print("Create and run peer nodes to connect via bootstrap.")

    for i in range(args.peer_amount):
        validator_private_key, validator_public_key = generate_validator_private_key()
        peer_node = {}
        peer_node[i] = {}
        peer_node[i]['name'] = f"peer{i}.{args.network}"
        peer_node[i]['volume'] = client.volumes.create()
        print(f"creating {peer_node[i]['name']}")
        container = client.containers.run(args.image, \
            name=peer_node[i]['name'], \
            user='root', \
            detach=True, \
            cpuset_cpus=args.cpuset_cpus, \
            mem_limit=args.memory, \
            network=args.network, \
            volumes=[
                f"{bonds_file}:{container_bonds_file}", \
                f"{peer_node[i]['volume'].name}:{args.rnode_directory}"
            ], \
            command=f"{peer_prefix_command} --validator-private-key {validator_private_key} --validator-public-key {validator_public_key} --host {peer_node[i]['name']}", \
            hostname=peer_node[i]['name'])
    return 0
      

if __name__ == "__main__":
    main()
