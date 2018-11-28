import os
import pytest
import logging

from typing import TYPE_CHECKING
if TYPE_CHECKING:
    from rnode_testing.rnode import Node


def without_banner_and_prompt(input: str, output: str) -> str:
    banner_and_prompt = '\x1b[31m\n  ╦═╗┌─┐┬ ┬┌─┐┬┌┐┌  ╔╗╔┌─┐┌┬┐┌─┐  ╦═╗╔═╗╔═╗╦  \n  ╠╦╝│  ├─┤├─┤││││  ║║║│ │ ││├┤   ╠╦╝║╣ ╠═╝║  \n  ╩╚═└─┘┴ ┴┴ ┴┴┘└┘  ╝╚╝└─┘─┴┘└─┘  ╩╚═╚═╝╩  ╩═╝\n    \x1b[0m\n\x1b[32mrholang $ '
    assert output.startswith(banner_and_prompt)
    without_banner_and_prompt = output[len(banner_and_prompt):]
    colored_input = '\x1b[0m{}\n'.format(input)
    assert without_banner_and_prompt.startswith(colored_input)
    return without_banner_and_prompt[len(colored_input):]


def test_repl(bootstrap_node: "Node") -> None:
    repl_commands = [
        '5',
        'new s(`rho:io:stdout`) in { s!("foo") }',
        '@"listCh"!([1, 2, 3]) | for(@list <- @"listCh"){ match list { [a, b, c] => { new s(`rho:io:stdout`) in { s!(a) } } } }',
    ]
    for repl_cmd in repl_commands:
        bootstrap_node.repl(repl_cmd)


def test_repl_detects_invalid_rholang(bootstrap_node: "Node") -> None:
    input = 'foo'
    output = bootstrap_node.repl(input, stderr=False)
    without_prologue = without_banner_and_prompt(input, output)
    assert without_prologue.startswith('\x1b[34mError: coop.rchain.rholang.interpreter.errors$TopLevelFreeVariablesNotAllowedError')
