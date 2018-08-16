
import logging
from fixtures import *
from delayed_assert import expect, assert_expectations

def test_metrics_api_socket(started_rchain_network):
    containers = [started_rchain_network.bootstrap] + started_rchain_network.peers

    for container  in containers:
        logging.info(f"Test metrics api socket for {container.name}")
        cmd = f"nmap -sS -n -p T:40403 -oG - {container.name}"
        r = container.exec_run(cmd=cmd, user='root').output.decode("utf-8")
        expect("40403/open/tcp" in r, f"Port 40403/tcp is not open in container {container}")

    assert_expectations()


def test_node_logs_for_errors(converged_network):
    containers = [converged_network.bootstrap] + converged_network.peers

    for container in containers:
        logging.info(f"Testing {container.name} node logs for errors.")
        logs = container.logs().decode('utf-8')

        if "ERROR" in logs:
            for line in logs.splitlines():
                if "ERROR" in line:
                    logging.error(f"Error: {line}")
            expect(not "ERROR" in line, f"Container {container.name} error in log line: {line}")

    assert_expectations()
