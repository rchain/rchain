import logging
import re

def wait_for(condition, timeout):
    """
    Waits for a condition to be fulfilled. It retries until the timeout expires.

    :param condition: the condition. Has to be a function 'Unit -> Boolean'
    :param timeout: the total time to wait
    :return: true  if the condition was met in the given timeout
    """
    import time

    logging.info(f"Waiting for condition `{condition.__doc__}`. Timeout={timeout}. Patience please!")

    elapsed = 0
    while elapsed < timeout:
        if condition():
            logging.info(f"Condition satisfied after {elapsed}s. Continue...")
            return True

        iteration_duration = max(1, int(0.15 * (timeout - elapsed))) # iteration duration is 15% of remaining timeout

        logging.info(f"Condition not fulfilled yet. Sleeping {iteration_duration}s...")

        time.sleep(iteration_duration)
        elapsed = elapsed + iteration_duration
    return False

# Predicates
# For each predicate please provide a nicely formatted __doc__ because it is used in wait_for to display a nice message
# Warning: The __doc__ has to be explicitly assigned as seen below if it's a formatted string, otherwise it will be None.

def container_logs(docker_container):
    def go(): return docker_container.logs().decode('utf-8')
    go.__doc__ = f"container_logs({docker_container.name})"
    return go

def contains(string_factory, regex_str, flags = 0):
    rx = re.compile(regex_str)

    def go(): return rx.search(string_factory(), flags)

    go.__doc__ = f"{string_factory.__doc__} contains regex '{regex_str}'"
    return go

def network_converged(bootstrap_container, expected_peers):
    rx = re.compile("^peers\s+(\d+).*", re.MULTILINE | re.DOTALL)

    def go():
        cmd = f'curl -s {bootstrap_container.name}:40403'

        r = bootstrap_container.exec_run(cmd=cmd).output.decode('utf-8')
        m = rx.search(r)

        peers = int(m[1]) if m else 0

        logging.info(f"Peers so far: {peers} Expected:{expected_peers}")

        return peers == expected_peers

    go.__doc__ = f"network converged({bootstrap_container.name} with {expected_peers} expected peers."

    return go
