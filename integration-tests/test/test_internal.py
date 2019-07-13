"""Tests for the testing code itself."""

import random

from .common import (
    KeyPair,
)
from .rnode import (
    extract_block_hash_from_propose_output,
    extract_block_count_from_show_blocks,
    parse_show_blocks_key_value_line,
    parse_show_blocks_output,
    parse_show_block_output,
    extract_validator_stake_from_bonds_validator_str,
    extract_validator_stake_from_deploy_cost_str,
    parse_mvdag_str,
    extract_deploy_id_from_deploy_output,
)
from .conftest import (
    make_wallets_file_lines,
)


def test_blocks_count_from_show_blocks() -> None:
    show_blocks_output = '''
------------- block 0 ---------------
blockHash: "630c5372c67cc5400a9eb11459bb240226273a693bbb018df829a3119b26bbf0"
blockSize: "99746"
blockNumber: 0
version: 1
deployCount: 10
tupleSpaceHash: "f2fdac324a5fa86f58d3e8162ad5108d9bc75773311d32bb9bc36b74c632793a"
timestamp: 1
faultTolerance: 1.0
mainParentHash: ""
sender: ""

-----------------------------------------------------


count: 123

'''
    assert extract_block_count_from_show_blocks(show_blocks_output) == 123


def test_parse_show_blocks_key_value_line() -> None:
    assert parse_show_blocks_key_value_line('''blockHash: "cf42c994ff30189c35cbd007719c6bdb361b28c70ae88889a6e54b5431b8f7eb"''') == ('blockHash', 'cf42c994ff30189c35cbd007719c6bdb361b28c70ae88889a6e54b5431b8f7eb')
    assert parse_show_blocks_key_value_line('''blockSize: "111761"''') == ('blockSize', '111761')
    assert parse_show_blocks_key_value_line('''blockNumber: 0''') == ('blockNumber', '0')
    assert parse_show_blocks_key_value_line('''version: 1''') == ('version', '1')
    assert parse_show_blocks_key_value_line('''deployCount: 10''') == ('deployCount', '10')
    assert parse_show_blocks_key_value_line('''tupleSpaceHash: "dcd6e349d5b4ca45a11811808ad7757bdfb856b093e02c7e4a2930817f179cdb"''') == ('tupleSpaceHash', 'dcd6e349d5b4ca45a11811808ad7757bdfb856b093e02c7e4a2930817f179cdb')
    assert parse_show_blocks_key_value_line('''timestamp: 1''') == ('timestamp', '1')
    assert parse_show_blocks_key_value_line('''faultTolerance: -0.6666667''') == ('faultTolerance', '-0.6666667')
    assert parse_show_blocks_key_value_line('''mainParentHash: ""''') == ('mainParentHash', '')
    assert parse_show_blocks_key_value_line('''sender: ""''') == ('sender', '')


def test_parse_show_blocks_output() -> None:
    input = '''
------------- block 0 ---------------
blockHash: "cf42c994ff30189c35cbd007719c6bdb361b28c70ae88889a6e54b5431b8f7eb"
blockSize: "111761"
blockNumber: 0
version: 1
deployCount: 10
tupleSpaceHash: "dcd6e349d5b4ca45a11811808ad7757bdfb856b093e02c7e4a2930817f179cdb"
timestamp: 1
faultTolerance: -0.6666667
mainParentHash: ""
sender: ""

-----------------------------------------------------


count: 1

'''

    output = parse_show_blocks_output(input)
    assert len(output) == 1
    block = output[0]
    assert block['blockHash'] == 'cf42c994ff30189c35cbd007719c6bdb361b28c70ae88889a6e54b5431b8f7eb'
    assert block['blockSize'] == '111761'
    assert block['blockNumber'] == '0'
    assert block['version'] == '1'
    assert block['deployCount'] == '10'
    assert block['tupleSpaceHash'] == 'dcd6e349d5b4ca45a11811808ad7757bdfb856b093e02c7e4a2930817f179cdb'
    assert block['timestamp'] == '1'
    assert block['faultTolerance'] == '-0.6666667'
    assert block['mainParentHash'] == ''
    assert block['sender'] == ''


def test_parse_show_block_output() -> None:
    input = r'''status: "Success"
blockInfo {
  blockHash: "fd219ca26e55e671168fc907490a04a1a89651693544fabf4e6b74478ec2b337"
  blockSize: "115078"
  blockNumber: 0
  version: 1
  deployCount: 10
  tupleSpaceHash: "fc138f97493733c5cde8b8d4284326023e6ce07e6e858cdfe83fa70bcb5e9550"
  timestamp: 1
  faultTolerance: 0.14893617
  mainParentHash: ""
  sender: ""
  shardId: "rchain"
  bondsValidatorList: "02ab69930f74b931209df3ce54e3993674ab3e7c98f715608a5e74048b332821: 43"
  bondsValidatorList: "043c56051a613623cd024976427c073fe9c198ac2b98315a4baff9d333fbb42e: 15"
  bondsValidatorList: "1cd8bf79a2c1bd0afa160f6cdfeb8597257e48135c9bf5e4823f2875a1492c97: 2"
  bondsValidatorList: "23bb89653c1d43578ed421e655e7a0ed9f3ed2e7eab820ad7739277e380cafa3: 81"
  deployCost: "User: , Cost: 132 DeployData #1553171134886 -- new x in { x!(0) }\\n}"
  deployCost: "User: , Cost: 132 DeployData #1553171478932 -- new x in { x!(0) }\\n}"
}

'''

    output = parse_show_block_output(input)

    assert output['blockHash'] == 'fd219ca26e55e671168fc907490a04a1a89651693544fabf4e6b74478ec2b337'
    assert output['blockSize'] == '115078'
    assert output['blockNumber'] == '0'
    assert output['version'] == '1'
    assert output['deployCount'] == '10'
    assert output['tupleSpaceHash'] == 'fc138f97493733c5cde8b8d4284326023e6ce07e6e858cdfe83fa70bcb5e9550'
    assert output['timestamp'] == '1'
    assert output['faultTolerance'] == '0.14893617'
    assert output['mainParentHash'] == ''
    assert output['sender'] == ''
    assert output['shardId'] == 'rchain'
    assert output['bondsValidatorList'] == '02ab69930f74b931209df3ce54e3993674ab3e7c98f715608a5e74048b332821: 43#$043c56051a613623cd024976427c073fe9c198ac2b98315a4baff9d333fbb42e: 15#$1cd8bf79a2c1bd0afa160f6cdfeb8597257e48135c9bf5e4823f2875a1492c97: 2#$23bb89653c1d43578ed421e655e7a0ed9f3ed2e7eab820ad7739277e380cafa3: 81'
    assert output['deployCost'] == r'User: , Cost: 132 DeployData #1553171134886 -- new x in { x!(0) }\\n}#$User: , Cost: 132 DeployData #1553171478932 -- new x in { x!(0) }\\n}'


def test_extract_validator_stake_from_bonds_validator_str() -> None:
    input = r'''02ab69930f74b931209df3ce54e3993674ab3e7c98f715608a5e74048b332821: 43#$043c56051a613623cd024976427c073fe9c198ac2b98315a4baff9d333fbb42e: 15#$1cd8bf79a2c1bd0afa160f6cdfeb8597257e48135c9bf5e4823f2875a1492c97: 2#$23bb89653c1d43578ed421e655e7a0ed9f3ed2e7eab820ad7739277e380cafa3: 81'''
    validator_stake = extract_validator_stake_from_bonds_validator_str(input)

    assert validator_stake['02ab69930f74b931209df3ce54e3993674ab3e7c98f715608a5e74048b332821'] == 43
    assert validator_stake['043c56051a613623cd024976427c073fe9c198ac2b98315a4baff9d333fbb42e'] == 15
    assert validator_stake['1cd8bf79a2c1bd0afa160f6cdfeb8597257e48135c9bf5e4823f2875a1492c97'] == 2
    assert validator_stake['23bb89653c1d43578ed421e655e7a0ed9f3ed2e7eab820ad7739277e380cafa3'] == 81


def test_extract_block_hash_from_propose_output() -> None:
    response = "Response: Success! Block a91208047c... created and added.\n"
    assert extract_block_hash_from_propose_output(response) == "a91208047c"


def test_extract_validator_stake_from_deploy_cost_str() -> None:
    input = r'User: 23bb89653c1d43578ed421e655e7a0ed9f3ed2e7eab820ad7739277e380cafa3, Cost: 132 DeployData #1553171134886 -- new x in { x!(0) }\\n}#$User: 1cd8bf79a2c1bd0afa160f6cdfeb8597257e48135c9bf5e4823f2875a1492c97, Cost: 132 DeployData #1553171478932 -- new x in { x!(0) }\\n}'
    deploy_cost = extract_validator_stake_from_deploy_cost_str(input)
    assert deploy_cost['23bb89653c1d43578ed421e655e7a0ed9f3ed2e7eab820ad7739277e380cafa3'] == 132
    assert deploy_cost['1cd8bf79a2c1bd0afa160f6cdfeb8597257e48135c9bf5e4823f2875a1492c97'] == 132


def test_make_wallets_file_lines() -> None:
    random_generator = random.Random(1547120283)
    validator_keys = [
        KeyPair(private_key='80366db5fbb8dad7946f27037422715e4176dda41d582224db87b6c3b783d709', public_key='1cd8bf79a2c1bd0afa160f6cdfeb8597257e48135c9bf5e4823f2875a1492c97'),
        KeyPair(private_key='120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5', public_key='02ab69930f74b931209df3ce54e3993674ab3e7c98f715608a5e74048b332821'),
        KeyPair(private_key='1f52d0bce0a92f5c79f2a88aae6d391ddf853e2eb8e688c5aa68002205f92dad', public_key='043c56051a613623cd024976427c073fe9c198ac2b98315a4baff9d333fbb42e'),
    ]

    output = make_wallets_file_lines(random_generator, validator_keys)

    assert output == [
        '0x1cd8bf79a2c1bd0afa160f6cdfeb8597257e48135c9bf5e4823f2875a1492c97,40,0',
        '0x02ab69930f74b931209df3ce54e3993674ab3e7c98f715608a5e74048b332821,45,0',
        '0x043c56051a613623cd024976427c073fe9c198ac2b98315a4baff9d333fbb42e,26,0',
    ]


def test_parse_mvdag_str() -> None:
    input = """d5db034e82e10ee1037454a70737ac9e1a6f4900d28590776b5ccc5eef087312 a75e6ec04d42b3fa0a02160d0bd2d19cbe563016283f362eb114f19c0a2bbad7
a75e6ec04d42b3fa0a02160d0bd2d19cbe563016283f362eb114f19c0a2bbad7 9fa2d387275ff5019c26809e6d6b2ef6a250090892e3b9269fa303d19db15ee8
3851ce1c5f7a26b444c45edde5cff7fae20aa5b90aa6ce882f058c7834d748d6 9fa2d387275ff5019c26809e6d6b2ef6a250090892e3b9269fa303d19db15ee8
f591cea354b70a9c6b753d13d8912d7fd0219fd45b80f449a08431cb6b265ea2 9fa2d387275ff5019c26809e6d6b2ef6a250090892e3b9269fa303d19db15ee8
9fa2d387275ff5019c26809e6d6b2ef6a250090892e3b9269fa303d19db15ee8 b29aaeb2ae774bfa573c4e5e37bc84bbaa1616263fd83c820b0dd9a795a57907
879b1499c4bb5b8359559ab2a308ce76dd01ae1a3693f0edbdbf4a7126767d93 b29aaeb2ae774bfa573c4e5e37bc84bbaa1616263fd83c820b0dd9a795a57907
b52e9a808053703353a16ea85a4cda5820a2af115bad87b6cebfef03111f5541 b29aaeb2ae774bfa573c4e5e37bc84bbaa1616263fd83c820b0dd9a795a57907
b0880ca496258ebd0c8c36446ac7596681600e3ab90a9db44b464dd4767f5adf 9547694c620c3e78b39da3db3a2090aa863a0c1174686a4de105350f7d4e77f4"""

    dag = parse_mvdag_str(input)

    assert dag == {
        "d5db034e82e10ee1037454a70737ac9e1a6f4900d28590776b5ccc5eef087312": set(['a75e6ec04d42b3fa0a02160d0bd2d19cbe563016283f362eb114f19c0a2bbad7']),
        "a75e6ec04d42b3fa0a02160d0bd2d19cbe563016283f362eb114f19c0a2bbad7": set(['9fa2d387275ff5019c26809e6d6b2ef6a250090892e3b9269fa303d19db15ee8']),
        "3851ce1c5f7a26b444c45edde5cff7fae20aa5b90aa6ce882f058c7834d748d6": set(['9fa2d387275ff5019c26809e6d6b2ef6a250090892e3b9269fa303d19db15ee8']),
        "f591cea354b70a9c6b753d13d8912d7fd0219fd45b80f449a08431cb6b265ea2": set(['9fa2d387275ff5019c26809e6d6b2ef6a250090892e3b9269fa303d19db15ee8']),
        "9fa2d387275ff5019c26809e6d6b2ef6a250090892e3b9269fa303d19db15ee8": set(['b29aaeb2ae774bfa573c4e5e37bc84bbaa1616263fd83c820b0dd9a795a57907']),
        "879b1499c4bb5b8359559ab2a308ce76dd01ae1a3693f0edbdbf4a7126767d93": set(['b29aaeb2ae774bfa573c4e5e37bc84bbaa1616263fd83c820b0dd9a795a57907']),
        "b52e9a808053703353a16ea85a4cda5820a2af115bad87b6cebfef03111f5541": set(['b29aaeb2ae774bfa573c4e5e37bc84bbaa1616263fd83c820b0dd9a795a57907']),
        "b0880ca496258ebd0c8c36446ac7596681600e3ab90a9db44b464dd4767f5adf": set(['9547694c620c3e78b39da3db3a2090aa863a0c1174686a4de105350f7d4e77f4']),
    }


def test_parse_deploy_str() -> None:
    input = """Response: Success!
DeployId is: 3045022100970e70a1e00751df2c4bb3475b1eae8ca15f81711dcdd89136608b0bc3d144ea022038f935b2dc7de76c8543bf89a8515469fcab55fdabf0ce907f40d211fae438a5
"""

    deploy_id = extract_deploy_id_from_deploy_output(input)
    assert deploy_id == "3045022100970e70a1e00751df2c4bb3475b1eae8ca15f81711dcdd89136608b0bc3d144ea022038f935b2dc7de76c8543bf89a8515469fcab55fdabf0ce907f40d211fae438a5"
