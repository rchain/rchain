#!/usr/bin/env python3.6
# This is a simple script to help with p2p network boot/testing.
# This requires Python 3.6 to be installed for fstring. Install dependencies via pip
# python3.6 -m pip install docker argparse pexpect requests
# Return code of 0 is success on test and 1 is fail.
from pexpect import replwrap
import subprocess
import argparse
import docker
import os
import tempfile
import requests
import re
import time
import sys
# from prometheus_client.parser import text_string_to_metric_families # optional for future


parser = argparse.ArgumentParser()
# parser.add_argument("-g", "--grpc-host", dest='grpc_host', default="peer0.rchain.coop", help="set grpc host")
parser.add_argument("-b", "--boot", action='store_true' , help="boot network by creating resources and starting services by network name")
parser.add_argument("-B", "--bootstrap-command", dest='bootstrap_command', type=str, default="--port 30304 --standalone --name 0f365f1016a54747b384b386b8e85352", help="bootstrap container run command")
parser.add_argument("-c", "--cpuset-cpus", dest='cpuset_cpus', type=str, default="0", help="set docker cpuset-cpus for nodes. Allows limiting execution in specific CPUs")
parser.add_argument("-D", "--deploy-demo", dest='deploy_demo', action='store_true', help="deploy casper demo")
parser.add_argument("-i", "--image", dest='image', type=str, default="coop.rchain/rnode:latest", help="source repo for docker image")
# parser.add_argument("-c", "--cpus", dest='cpus', type=int, default=1, help=".5 set docker cpus for repl client node")
parser.add_argument("-l", "--logs", action='store_true' , help="show all node logs")
parser.add_argument("-m", "--memory", dest='memory', type=str, default="1024m", help="1024m set docker memory for repl client node")
parser.add_argument("-n", "--network", dest='network', type=str, default="rchain.coop", help="set docker network name")
parser.add_argument("-t", "--run-tests", dest='run_tests', action='store_true' , help="only run tests")
parser.add_argument("-X", "--peer-command", dest='peer_command', type=str, default="--bootstrap rnode://0f365f1016a54747b384b386b8e85352@bootstrap.rchain.coop:30304", help="peer container run command")
parser.add_argument("-a", "--peers-amount", dest='peers_amount', type=int, default="2", help="set total amount of peers for network")
parser.add_argument("-p", "--prompt", dest='prompt', type=str, default="rholang $ ", help="set REPL prompt")
# parser.add_argument("-t", "--type", dest='type', type=str, default="docker", help="set vm/container type - docker/virtualbox")
parser.add_argument("-E", "--repl-commands", dest='repl_cmds', type=str, nargs='+', default=['5', '@"stdout"!("foo")', '@"listCh"!([1, 2, 3]) | for(@list <- @"listCh"){ match list { [a, b, c] => { @"stdout"!(a) } } }'], help="set repl commands to run as a list")
parser.add_argument("-L", "--repl-load-repetitions", dest='repl_load_repetitions', type=int, default=50, help="set repl load repetition peers_amount for loops")
parser.add_argument("-R", "--rnode-directory", dest='rnode_directory', type=str, default="/var/lib/rnode", help="container rnode mount point directory")
parser.add_argument("-r", "--remove", action='store_true' , help="forcibly remove all container resources associated to network name")
parser.add_argument("-s", "--skip-convergence_test", dest="skip_convergence_test", action='store_true' , help="skip network convergence test")
parser.add_argument("-T", "--tests", dest='tests', type=str, nargs='+', default=['eval', 'network_sockets', 'count', 'errors', 'repl'], help="run these tests")


# Define globals
args = parser.parse_args()
client = docker.from_env()
RNODE_CMD = 'java -Dfile.encoding=UTF8 -Djava.net.preferIPv4Stack=true -jar /rnode-assembly-0.3.1.jar'


def main():
    """Main program"""
    if args.run_tests == True:
        run_tests()
        return
    if args.logs == True:
        show_logs()
        return
    if args.remove == True:
        remove_resources_by_network(args.network)
        return
    if args.deploy_demo == True: # deploy casper demo
        deploy_demo()
        return
    if args.boot == True:
        remove_resources_by_network(args.network)
        boot_p2p_network()
        if not args.skip_convergence_test == True:
            for container in client.containers.list(all=True, filters={"name":f'bootstrap.{args.network}'}):
                check_network_convergence(container)
        time.sleep(10)
        deploy_demo()
        if args.tests:
            time.sleep(10)
            run_tests()
            return


def run_tests():
    notices ={} 
    notices['fail'] = []
    notices['pass'] = []
    # all_pass = True 
    if not client.containers.list(all=True, filters={"name":f".{args.network}"}): # return if empty
        return 

    for test in args.tests:
        if test == "network_sockets":
            for container in client.containers.list(all=True, filters={"name":f"peer\d.{args.network}"}):
                if test_network_sockets(container) == 0:
                    notices['pass'].append(f"{container.name}: Metrics API http/tcp/9095 is available.")
                else:
                    notices['fail'].append(f"{container.name}: Metrics API http/tcp/9095 is not available..")
        if test == "errors":
            for container in client.containers.list(all=True, filters={"name":f'peer\d.{args.network}'}):
                print(container.name)
                if test_node_logs_for_errors(container) == 0:
                    notices['pass'].append(f"{container.name}: No errors defined by \"ERROR\" in logs.")
                else:
                    notices['fail'].append(f"{container.name}: Errors found in node logs.")
        if test == "count":
            for container in client.containers.list(all=True, filters={"name":f"peer\d.{args.network}"}):
                if test_node_logs_for_correct_peers_count(container) == 0:
                    notices['pass'].append(f"{container.name}: Peers count correct in node logs.")
                else:
                    notices['fail'].append(f"{container.name}: Peers count incorrect in node logs.")
        if test == "eval":
            for container in client.containers.list(filters={"name":f"peer\d.{args.network}"}):
                if test_node_eval_of_rholang_files(container) == 0:
                    notices['pass'].append(f"{container.name}: Rholang evaluation of files performed correctly.")
                else:
                    notices['fail'].append(f"{container.name}: Rholang evaluation of files failed.")
        if test == "repl":
            # for container in client.containers.list(all=True, filters={"name":"peer\d\.rchain.coop"}): # this will enable all peers to be checked
            # for container in client.containers.list(all=True, filters={"name":f"{args.grpc_host}"}):
            for container in client.containers.list(all=True, filters={"name":f"peer0.{args.network}"}):
                if test_repl_load(container) == 0:
                    notices['pass'].append(f"{container.name}: REPL loader success!")
                else:
                    notices['fail'].append(f"{container.name}: REPL loader failure!")

    print("===========================================================")
    print("=================TEST SUMMARY RESULTS======================")
    if notices['pass']:
        for notice in notices['pass']:
            print(f"PASS: {notice}")
    if notices['fail']:
        for notice in notices['fail']:
            print(f"FAIL: {notice}")
        print('FAIL: Part or all of tests failed in one or more peer nodes.')
        raise Exception('FAIL: Part or all of tests failed in one or more peer nodes.')
    else:
        print("PASS ALL: All tests successfully passed")
    print("===========================================================")
    print("===========================================================")


def deploy_demo():
    cmd = f"{RNODE_CMD} --deploy-demo"
    for container in client.containers.list(all=True, filters={"name":f"peer\d.{args.network}"}):
        try:
            r = container.exec_run(cmd=cmd, detach=True)
        except Exception as e:
            print(e)

def test_node_eval_of_rholang_files(container):
    print(container.name)
    cmd = f"ls /usr/share/rnode/examples/*.rho"
    r = container.exec_run(['sh', '-c', cmd])
    for file_path in r.output.decode('utf-8').splitlines():
        print(file_path)
        eval_r = container.exec_run(['sh', '-c', f'{RNODE_CMD} --eval {file_path}'])
        #print(eval_r)
        for line in eval_r.output.decode('utf-8').splitlines():
            if 'ERROR' in line.upper():
                print(line)
                return 1 
    return 0 

def show_logs():
    for container in client.containers.list(all=True, filters={"name":f".{args.network}"}):
        print(f"Showing logs for {container.name}")
        r = container.logs().decode('utf-8')
        print(r)


def boot_p2p_network():
    try:
        client.networks.create(args.network, driver="bridge")
        print("Starting bootstrap node.")
        create_bootstrap_node()
        time.sleep(10) # give bootstrap node an early start
        print("Starting peer nodes.")
        create_peer_nodes()
        return 0
    except Exception as e:
        print(e)
        return 1


def var_to_docker_file(var, container_name, file_path):
    """Convert variabled to container file."""
    fd, path = tempfile.mkstemp()
    try:
        with os.fdopen(fd, 'w') as tmp:
            tmp.write(var)
    finally:
        subprocess.call(["docker", "cp", path, f"{container_name}:{file_path}"])
        os.remove(path)
    return 0



def check_network_convergence(container):
    peers_metric = ''
    peers_metric_expected = args.peers_amount
    timeout = 200
    count = 0

    while count < 200:
        cmd = f'curl -s {container.name}:9095'
        r = container.exec_run(cmd=cmd).output.decode('utf-8')
        print(r)
        print(f"checking {count} of {timeout} seconds")
        # peers_metric = int(float(re.search(r'^peers (.*)$', r, re.MULTILINE).group().rsplit(' ', 1)[1]))
        for line in r.splitlines():
            if line == f"peers {args.peers_amount}.0":
                print("Network converged.")
                return 0
        time.sleep(10)
        count += 10
    print("Timeout of {timeout} seconds reached ... exiting network convergence pre tests probe.")
    return 1 


def check_network_convergence_alternate(peers_metric_expected):
    """Alternate check for network convergence before running tests."""
    # Currently unused but is left in code example purposes
    print("Checking for network convergence. This could take a while. Max is 200 seconds.")
    for container in client.containers.list(all=True, filters={"name":f"scan.{args.network}"}):
        container.remove(force=True, v=True)

    scan_node = {}
    scan_node['name'] = f"scan.{args.network}"
    scan_node['volume'] = client.volumes.create()
    container = client.containers.run('alpine:latest', \
        name=scan_node['name'], \
        detach=True, \
        network=args.network, \
        volumes={scan_node['volume'].name: {'bind': '/app', 'mode': 'rw'}}, \
        hostname=scan_node['name'], tty=True)
    r = container.exec_run(cmd='apk update')
    r = container.exec_run(cmd='apk add python3')
    r = container.exec_run(cmd='pip3 install requests')

    # Generating a file out in code to run on docker container. An alternative way.
    var =("import tempfile\n"
          "import os\n"
          "import re\n"
          "import requests\n"
          "import time\n"
          "import subprocess\n"
          "peers_metric = ''\n"
          f"peers_metric_expected = {peers_metric_expected}\n"
          "timeout = 200\n"
          "count = 0\n"

          "while peers_metric != peers_metric_expected:\n"
          '    print(f"checking {count} of {timeout} seconds")\n'
          f"    metrics = requests.get('http://bootstrap.{args.network}:9095').content.decode('utf-8')\n"
              # peers_metric = float(re.findall(r'^peers (.*)$', metrics, re.MULTILINE)[0]) # alt get peers
          "    try:\n"
          "        peers_metric = int(float(re.search(r'^peers (.*)$', metrics, re.MULTILINE).group().rsplit(' ', 1)[1]))\n"
          "    except Exception as e:\n"
          "        pass\n"
          "    if count >= timeout:\n"
          '        print("timeout of {timeout} seconds reached ... exiting pre tests probe")\n'
          "        break\n"
          "    time.sleep(10)\n"
          "    count += 10\n")
    var_to_docker_file(var, f'scan.{args.network}', '/app/scan.py')
    r = container.exec_run(cmd='python3 /app/scan.py')
    print(r.output.decode("utf-8"))
    print("Network has converged.")
    return 0 


def remove_resources_by_network(args_network):
    """Remove resources by network name."""
    for container in client.containers.list(all=True, filters={"name":f".{args_network}"}):
            container.remove(force=True, v=True)

    for network in client.networks.list():
        if args_network == network.name:
            print(f"removing {network.name}")
            network.remove()
    # client.volumes.prune() # removes unused volumes
    return 0


def create_bootstrap_node():
    """Create bootstrap node."""
    bootstrap_node = {}
    bootstrap_node['name'] = f"bootstrap.{args.network}"
    bootstrap_node['volume'] = client.volumes.create()
    print(f"creating {bootstrap_node['name']}")
    container = client.containers.run(args.image, \
        name=bootstrap_node['name'], \
        detach=True, \
        cpuset_cpus=args.cpuset_cpus, \
        mem_limit=args.memory, \
        network=args.network, \
        volumes={bootstrap_node['volume'].name: {'bind': args.rnode_directory, 'mode': 'rw'}}, \
        command=args.bootstrap_command, \
        hostname=bootstrap_node['name'])

    # Add additional packages.
    container.exec_run(cmd='apk update')
    container.exec_run(cmd='apk add curl')
    container.exec_run(cmd='apk add nmap')
    container.exec_run(cmd='apk add python3')
    container.exec_run(cmd='pip3 install requests')
    return 0


def create_peer_nodes():
    """Create peer nodes."""
    for i in range(args.peers_amount):
        peer_node = {}
        peer_node[i] = {}
        peer_node[i]['name'] = f"peer{i}.{args.network}"
        peer_node[i]['volume'] = client.volumes.create()
        print(f"creating {peer_node[i]['name']}")
        container = client.containers.run(args.image, \
            name=peer_node[i]['name'], \
            detach=True, \
            cpuset_cpus=args.cpuset_cpus, \
            mem_limit=args.memory, \
            network=args.network, \
            volumes=[f"{peer_node[i]['volume'].name}:{args.rnode_directory}"], \
            command=args.peer_command, \
            hostname=peer_node[i]['name'])

        # Add additional packages.
        container.exec_run(cmd='apk update')
        container.exec_run(cmd='apk add curl')
        container.exec_run(cmd='apk add nmap')
        container.exec_run(cmd='apk add python3')
        container.exec_run(cmd='pip3 install requests')
    return 0
      

def test_network_sockets(container):
    print(f"Test metrics api socket for {container.name}")
    try:
        cmd = f"nmap -sS -n -p T:9095 -oG - {container.name}"
        r = container.exec_run(cmd=cmd).output.decode("utf-8")
        if "9095/open/tcp" in r:
            return 0 
        else:
            return 1 
    except Exception as e:
        print(e)
        return 1 


def test_repl_load(container):
    """Load REPL with commands."""
    for repl_container in client.containers.list(all=True, filters={"name":f"repl\d.{args.network}"}):
        print(f"removing {repl_container.name}")
        repl_container.remove(force=True, v=True)
    i = 0
    try:
        repl_node = {}
        repl_node[i] = {}
        repl_node[i]['name'] = f"repl{i}.{args.network}"
        repl_node[i]['volume'] = client.volumes.create()

        cmd = (f"sudo docker run --rm -it -v {repl_node[i]['volume'].name}:{args.rnode_directory} "
               f"--cpuset-cpus={args.cpuset_cpus} --memory={args.memory} --name {repl_node[i]['name']} "
               f"--network {args.network} {args.image} "
               f"--grpc-host {container.name} -r")
        print(f"docker repl cmd: {cmd}")
        repl_cmds = args.repl_cmds
        conn = replwrap.REPLWrapper(cmd, args.prompt, None)
        for i in range(args.repl_load_repetitions):
            for repl_cmd in repl_cmds:
                result = conn.run_command(repl_cmd)
                print(f"repetition: {i} output: {result}")
            i += 1 
        return 0 
    except Exception as e:
        print(e)
        return 1 


def test_node_logs_for_errors(container):
    print(f"Testing {container.name} node logs for errors.")
    r = container.logs().decode('utf-8')
    if not "ERROR" in r:
        print("PASS: No errors found in logs")
        retval = 0 
    else:
        print("FAIL: Errors matching ERROR found in logs")
        for line in r.splitlines():
            if "ERROR" in line:
                print(line)
                retval = 1 
    return retval 


def test_node_logs_for_correct_peers_count(container):
    retval = 1 
    print(f"Testing {container.name} node logs for correct peers count.")
    r = container.logs()
    for line in r.splitlines():
        line = line.decode('utf-8')
        if f"Peers: {args.peers_amount}." in line:
            print(line)
            retval = 0 
    return retval 


if __name__ == "__main__":
    main()
