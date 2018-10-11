import os
import pytest
from tools.rnode import start_bootstrap
import logging


def without_banner_and_prompt(input, output):
    banner_and_prompt = '\x1b[31m\n  ╦═╗┌─┐┬ ┬┌─┐┬┌┐┌  ╔╗╔┌─┐┌┬┐┌─┐  ╦═╗╔═╗╔═╗╦  \n  ╠╦╝│  ├─┤├─┤││││  ║║║│ │ ││├┤   ╠╦╝║╣ ╠═╝║  \n  ╩╚═└─┘┴ ┴┴ ┴┴┘└┘  ╝╚╝└─┘─┴┘└─┘  ╩╚═╚═╝╩  ╩═╝\n    \x1b[0m\n\x1b[32mrholang $ '
    assert output.startswith(banner_and_prompt)
    without_banner_and_prompt = output[len(banner_and_prompt):]
    colored_input = f'\x1b[0m{input}\n'
    assert without_banner_and_prompt.startswith(colored_input)
    return without_banner_and_prompt[len(colored_input):]


@pytest.fixture(scope="module")
def bootstrap_node(system):
    with start_bootstrap(system.docker,
                         system.config.node_startup_timeout,
                         system.config.rnode_timeout,
                         system.validators_data) as node:
        yield node


def test_repl(bootstrap_node):
    repl_commands = [
        '5',
        'new s(`rho:io:stdout`) in { s!("foo") }',
        '@"listCh"!([1, 2, 3]) | for(@list <- @"listCh"){ match list { [a, b, c] => { new s(`rho:io:stdout`) in { s!(a) } } } }',
    ]
    for repl_cmd in repl_commands:
        bootstrap_node.repl(repl_cmd)


def test_repl_detects_invalid_rholang(bootstrap_node):
    input = 'foo'
    output = bootstrap_node.repl(input)
    without_prologue = without_banner_and_prompt(input, output)
    assert without_prologue.startswith('\x1b[34mError: coop.rchain.rholang.interpreter.errors$TopLevelFreeVariablesNotAllowedError')
