def test_repl(started_standalone_bootstrap_node):
    repl_commands = [
        '5',
        'new s(`rho:io:stdout`) in { s!("foo") }',
        '@"listCh"!([1, 2, 3]) | for(@list <- @"listCh"){ match list { [a, b, c] => { new s(`rho:io:stdout`) in { s!(a) } } } }',
    ]
    for repl_cmd in repl_commands:
        started_standalone_bootstrap_node.repl(repl_cmd)


def test_repl_detects_invalid_rholang(started_standalone_bootstrap_node):
    input = 'foo'
    output = started_standalone_bootstrap_node.repl(input, stderr=False)
    formatted_input = '{}\n'.format(input)
    without_input = output[len(formatted_input):]
    assert without_input.startswith('Error: coop.rchain.rholang.interpreter.errors$TopLevelFreeVariablesNotAllowedError')
