import logging
import re
import pytest
import time
import collections

def wait_for(condition, timeout):
    """
    Waits for a condition to be fulfilled. It retries until the timeout expires.

    :param condition: the condition. Has to be a function 'Unit -> Boolean'
    :param timeout: the total time to wait
    :return: true  if the condition was met in the given timeout
    """

    __tracebackhide__ = True

    logging.info(f"Waiting for condition `{condition.__doc__}`. Timeout={timeout}. Patience please!")

    elapsed = 0
    while elapsed < timeout:
        try:
            value = condition()

            logging.info(f"Condition satisfied after {elapsed}s. Returning {value}")
            return value

        except ex:
            iteration_duration = max(1, int(0.15 * (timeout - elapsed))) # iteration duration is 15% of remaining timeout

            logging.info(f"Condition not fulfilled yet ({ex}). Sleeping {iteration_duration}s...")

            time.sleep(iteration_duration)
            elapsed = elapsed + iteration_duration

    pytest.fail(f"Timeout expired for `{condition.__doc__}`")

# Predicates
# For each predicate please provide a nicely formatted __doc__ because it is used in wait_for to display a nice message
# Warning: The __doc__ has to be explicitly assigned as seen below if it's a formatted string, otherwise it will be None.

def node_logs(node):
    def go(): return node.logs()
    go.__doc__ = f"container_logs({node.name})"
    return go

def string_matches(string_factory, regex_str, flags = 0):
    rx = re.compile(regex_str, flags)

    def go():
        s = string_factory()

        m = rx.search(s)
        if m:
            return m
        else:
            raise Exception(f"string doesn't contain regex {regex_str}")

    go.__doc__ = f"{string_factory.__doc__} search regex '{regex_str}'"
    return go

def string_equals(expected):
    def go(s): return s == expected

    go.__doc__ = f"string equals '{expected}'"
    return go

def string_contains(expected):
    def go(s): return expected in s

    go.__doc__ = f"string contains '{expected}'"
    return go

def network_converged(bootstrap_node, expected_peers):
    rx = re.compile("^peers\s+(\d+).*", re.MULTILINE | re.DOTALL)

    def go():
        cmd = f'curl -s {bootstrap_node.name}:40403'

        r = bootstrap_node.container.exec_run(cmd=cmd).output.decode('utf-8')
        m = rx.search(r)

        peers = int(m[1]) if m else 0

        if peers < expected_peers:
            raise Exception(f"Expected peers: {expected_peers}. Actual peers: {peers}")

    go.__doc__ = f"network {bootstrap_node.name} converged with {expected_peers} expected peers."

    return go

Block = collections.namedtuple("Block", ["id", "content"])

def block_content(block_factory):
    def go(): return block_factory().content
    go.__doc__ = f"block_content"
    return go

def node_blocks_received(node):
    def go():
        id_rx = "(.+?)"
        # received_block_rx = re.compile(f"^.* CASPER: Received Block #(\d+) \((.*?)\.\.\.\)(.*)$", re.MULTILINE | re.DOTALL)
        received_block_rx = re.compile(f"^.* CASPER: Received Block #\d+ \(({id_rx})\.\.\.\) -- Sender ID {id_rx}\.\.\. -- M Parent Hash {id_rx}\.\.\. -- Contents {id_rx}\.\.\.\.(.*)", re.MULTILINE | re.DOTALL)

        logs = node.log_lines()

        blocks = [Block( match[1], match[2])
                  for match in [received_block_rx.match(log)
                                for log in logs]
                  if match]

        return blocks

    go.__doc__ = f"node_blocks_received({node.name})"
    return go

def node_blocks_added(node):
    def go():
        id_rx = "(.+?)"
        added_block_rx = re.compile(f"^.*\s+CASPER: Added ({id_rx}).*", re.MULTILINE | re.DOTALL)
        logs = node.log_lines()

        block_ids = [ match[1]
                      for match in [ added_block_rx.match(log)
                                     for log in logs]
                      if match]

        return block_ids

    go.__doc__ = f"node_blocks_received({node.name})"
    return go

def find_first(list_factory, predicate):
    def go():
        lst = list_factory()
        found = [x for x in lst if predicate(x)]
        return found[0]

    go.__doc__ = f"`{list_factory.__doc__}` find `{predicate.__doc__}`"
    return go