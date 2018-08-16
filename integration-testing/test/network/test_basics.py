
import logging
from delayed_assert import expect, assert_expectations

def containers(rchain):
    return [rchain.bootstrap] + rchain.peers

def test_metrics_api_socket(started_rchain_network):
    for container  in containers(started_rchain_network):
        logging.info(f"Test metrics api socket for {container.name}")
        cmd = f"nmap -sS -n -p T:40403 -oG - {container.name}"
        r = container.exec_run(cmd=cmd, user='root').output.decode("utf-8")
        expect("40403/open/tcp" in r, f"Port 40403/tcp is not open in container {container}")

    assert_expectations()


def test_node_logs_for_errors(converged_network):
    for container in containers(converged_network):
        logging.info(f"Testing {container.name} node logs for errors.")
        logs = container.logs().decode('utf-8')

        if "ERROR" in logs:
            for line in logs.splitlines():
                if "ERROR" in line:
                    logging.error(f"Error: {line}")
            expect(not "ERROR" in line, f"Container {container.name} error in log line: {line}")

    assert_expectations()

def test_node_logs_for_RuntimeException(converged_network):
    for container in containers(converged_network):
        logging.info(f"Testing {container.name} node logs for \"java RuntimeException\".")
        logs = container.logs().decode('utf-8')


        if "RuntimeException" in logs:
            for line in logs.splitlines():
                if "RuntimeException" in line:
                    logging.error(f"Error: {line}")
            expect(not "RuntimeException" in line, f"Container {container.name} error in log line: {line}")

    assert_expectations()