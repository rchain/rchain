"""Tests for the testing code itself."""

from rchain.crypto import PrivateKey

from .conftest import (
    make_wallets_file_lines,
)

from .utils import(
    extract_block_hash_from_propose_output,
    extract_block_count_from_show_blocks,
    parse_show_blocks_output,
    parse_show_block_output,
    parse_mvdag_str,
    extract_deploy_id_from_deploy_output,
)


def test_blocks_count_from_show_blocks() -> None:
    show_blocks_output = '''------------- block 0 ---------------
blockHash: "1b69a62e5d4d57173efd918d828f3308f801a0867a22fc942b4a4775ae896958"
sender: ""
seqNum: 0
sig: ""
sigAlgorithm: ""
shardId: "rchain"
extraBytes: ""
version: 1
timestamp: 1575005928241
headerExtraBytes: ""
blockNumber: 0
preStateHash: "6284b05545513fead17c469aeb6baa2a11ed5a86eeda57accaa3bb95d60d5250"
postStateHash: "de7e15efcdfd0018497bcb40104afc863613619c0e47d5b2bf18c0c6d9e53865"
bodyExtraBytes: ""
bonds {
  validator: "04ab4c08f1986bb40c57d6aa24a650a4122bd6afb6b77990a1447230fc428cefd1d8d51b75812e549e0e4f2289c8fea6389b1d26ce71a7204782d92ea6c9862a35"
  stake: 15
}
bonds {
  validator: "04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519"
  stake: 20
}
bonds {
  validator: "0444f16eee91c879a70a2d53e90b329670580395c8639ffef3f39ef74bdd9364279f877cd3d7cca806c815bd6fc568bf2fc0695a9c2cd6ac3d36fc1f4864243efb"
  stake: 60
}
blockSize: "192205"
deployCount: 11
faultTolerance: 0.2631579
justifications {
  validator: "04126107bc353c73e044fb21a5085aeafeecd69895fc05ec5033764a586bf044ddb19da5140a00912d892bfe8e10aa34eb7f9a68308646c3ac8804096ba605c2d2"
  latestBlockHash: "2729db9efe943668950a7b8ad9197aff1e172c3396a379b705887f83c07c1607"
}
justifications {
  validator: "0412ce31a3c3cbf9c69c098e593568c476a6bf7efdf9f7579c80e5328af05db7693b077d04fabbed28bb4e2d28aaba4ee50af6eddfab957c9c3c16d629c9d6aac3"
  latestBlockHash: "2729db9efe943668950a7b8ad9197aff1e172c3396a379b705887f83c07c1607"
}
justifications {
  validator: "04f42348554ab10387739d6f709ddba0eb9b80792f57ed68a1c9341635c0777590e9dbdd316c57cff51587f2f320e30605e6641e042f030b83aaaa3a3268a00fb0"
  latestBlockHash: "2729db9efe943668950a7b8ad9197aff1e172c3396a379b705887f83c07c1607"
}

-----------------------------------------------------


count: 22


'''
    assert extract_block_count_from_show_blocks(show_blocks_output) == 22


def test_parse_show_blocks_output() -> None:
    input = '''
------------- block 1 ---------------
blockHash: "91979d8509e6ff886d54475e7519f23631205957cb3396bb9d1e0371aa01b02a"
sender: "0444f16eee91c879a70a2d53e90b329670580395c8639ffef3f39ef74bdd9364279f877cd3d7cca806c815bd6fc568bf2fc0695a9c2cd6ac3d36fc1f4864243efb"
seqNum: 1
sig: "304502210086fcc0e8e0cb391275196711f11705cddf6724498965b68a34705d3631290bed022012c661bc2102c61443ed7649dbdcf76aa35780153f90210ddf69a708467c5bbf"
sigAlgorithm: "secp256k1"
shardId: "rchain"
extraBytes: ""
version: 1
timestamp: 1575009346798
headerExtraBytes: ""
parentsHashList: "2a7f8806968fb93f9a74e52502f5d7ac8f84c6a6bc303f692cb1b9e63bdca36c"
blockNumber: 1
preStateHash: "de7e15efcdfd0018497bcb40104afc863613619c0e47d5b2bf18c0c6d9e53865"
postStateHash: "ce921313bee2afe2f20818931f0580b2fa86594eb8eaba3ff2bc3686d703e8b5"
bodyExtraBytes: ""
bonds {
  validator: "0444f16eee91c879a70a2d53e90b329670580395c8639ffef3f39ef74bdd9364279f877cd3d7cca806c815bd6fc568bf2fc0695a9c2cd6ac3d36fc1f4864243efb"
  stake: 60
}
bonds {
  validator: "04ab4c08f1986bb40c57d6aa24a650a4122bd6afb6b77990a1447230fc428cefd1d8d51b75812e549e0e4f2289c8fea6389b1d26ce71a7204782d92ea6c9862a35"
  stake: 15
}
bonds {
  validator: "04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519"
  stake: 20
}
blockSize: "20939"
deployCount: 1
faultTolerance: 0.57894737
justifications {
  validator: "04126107bc353c73e044fb21a5085aeafeecd69895fc05ec5033764a586bf044ddb19da5140a00912d892bfe8e10aa34eb7f9a68308646c3ac8804096ba605c2d2"
  latestBlockHash: "2729db9efe943668950a7b8ad9197aff1e172c3396a379b705887f83c07c1607"
}
justifications {
  validator: "0412ce31a3c3cbf9c69c098e593568c476a6bf7efdf9f7579c80e5328af05db7693b077d04fabbed28bb4e2d28aaba4ee50af6eddfab957c9c3c16d629c9d6aac3"
  latestBlockHash: "2729db9efe943668950a7b8ad9197aff1e172c3396a379b705887f83c07c1607"
}
justifications {
  validator: "04f42348554ab10387739d6f709ddba0eb9b80792f57ed68a1c9341635c0777590e9dbdd316c57cff51587f2f320e30605e6641e042f030b83aaaa3a3268a00fb0"
  latestBlockHash: "2729db9efe943668950a7b8ad9197aff1e172c3396a379b705887f83c07c1607"
}
-----------------------------------------------------


------------- block 0 ---------------
blockHash: "2a7f8806968fb93f9a74e52502f5d7ac8f84c6a6bc303f692cb1b9e63bdca36c"
sender: ""
seqNum: 0
sig: ""
sigAlgorithm: ""
shardId: "rchain"
extraBytes: ""
version: 1
timestamp: 1575008703176
headerExtraBytes: ""
blockNumber: 0
preStateHash: "6284b05545513fead17c469aeb6baa2a11ed5a86eeda57accaa3bb95d60d5250"
postStateHash: "de7e15efcdfd0018497bcb40104afc863613619c0e47d5b2bf18c0c6d9e53865"
bodyExtraBytes: ""
bonds {
  validator: "04ab4c08f1986bb40c57d6aa24a650a4122bd6afb6b77990a1447230fc428cefd1d8d51b75812e549e0e4f2289c8fea6389b1d26ce71a7204782d92ea6c9862a35"
  stake: 15
}
bonds {
  validator: "04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519"
  stake: 20
}
bonds {
  validator: "0444f16eee91c879a70a2d53e90b329670580395c8639ffef3f39ef74bdd9364279f877cd3d7cca806c815bd6fc568bf2fc0695a9c2cd6ac3d36fc1f4864243efb"
  stake: 60
}
blockSize: "192205"
deployCount: 11
faultTolerance: 1.0
justifications {
  validator: "04126107bc353c73e044fb21a5085aeafeecd69895fc05ec5033764a586bf044ddb19da5140a00912d892bfe8e10aa34eb7f9a68308646c3ac8804096ba605c2d2"
  latestBlockHash: "2729db9efe943668950a7b8ad9197aff1e172c3396a379b705887f83c07c1607"
}
justifications {
  validator: "0412ce31a3c3cbf9c69c098e593568c476a6bf7efdf9f7579c80e5328af05db7693b077d04fabbed28bb4e2d28aaba4ee50af6eddfab957c9c3c16d629c9d6aac3"
  latestBlockHash: "2729db9efe943668950a7b8ad9197aff1e172c3396a379b705887f83c07c1607"
}
justifications {
  validator: "04f42348554ab10387739d6f709ddba0eb9b80792f57ed68a1c9341635c0777590e9dbdd316c57cff51587f2f320e30605e6641e042f030b83aaaa3a3268a00fb0"
  latestBlockHash: "2729db9efe943668950a7b8ad9197aff1e172c3396a379b705887f83c07c1607"
}
-----------------------------------------------------


count: 2


'''

    blocks = parse_show_blocks_output(input)
    assert len(blocks) == 2
    block1 = blocks[0]
    assert block1.block_hash == '91979d8509e6ff886d54475e7519f23631205957cb3396bb9d1e0371aa01b02a'
    assert block1.sender == '0444f16eee91c879a70a2d53e90b329670580395c8639ffef3f39ef74bdd9364279f877cd3d7cca806c815bd6fc568bf2fc0695a9c2cd6ac3d36fc1f4864243efb'
    assert block1.seq_num == 1
    assert block1.sig == '304502210086fcc0e8e0cb391275196711f11705cddf6724498965b68a34705d3631290bed022012c661bc2102c61443ed7649dbdcf76aa35780153f90210ddf69a708467c5bbf'
    assert block1.sig_algorithm == 'secp256k1'
    assert block1.shard_id == 'rchain'
    assert block1.extra_bytes == ''
    assert block1.version == '1'
    assert block1.timestamp == 1575009346798
    assert block1.header_extra_bytes == ''
    assert block1.parents == ['2a7f8806968fb93f9a74e52502f5d7ac8f84c6a6bc303f692cb1b9e63bdca36c']
    assert block1.block_number == 1
    assert block1.pre_state_hash == 'de7e15efcdfd0018497bcb40104afc863613619c0e47d5b2bf18c0c6d9e53865'
    assert block1.post_state_hash == 'ce921313bee2afe2f20818931f0580b2fa86594eb8eaba3ff2bc3686d703e8b5'
    assert block1.body_extra_bytes == ''
    assert block1.block_size == 20939
    assert block1.deploy_count == 1
    assert block1.fault_tolerance == 0.57894737
    assert block1.bonds == {
        '0444f16eee91c879a70a2d53e90b329670580395c8639ffef3f39ef74bdd9364279f877cd3d7cca806c815bd6fc568bf2fc0695a9c2cd6ac3d36fc1f4864243efb': 60,
        '04ab4c08f1986bb40c57d6aa24a650a4122bd6afb6b77990a1447230fc428cefd1d8d51b75812e549e0e4f2289c8fea6389b1d26ce71a7204782d92ea6c9862a35': 15,
        '04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519': 20
        }

    block2 = blocks[1]
    assert block2.block_hash == '2a7f8806968fb93f9a74e52502f5d7ac8f84c6a6bc303f692cb1b9e63bdca36c'
    assert block2.sender == ''
    assert block2.seq_num == 0
    assert block2.sig == ''
    assert block2.sig_algorithm == ''
    assert block2.shard_id == 'rchain'
    assert block2.extra_bytes == ''
    assert block2.version == '1'
    assert block2.timestamp == 1575008703176
    assert block2.header_extra_bytes == ''
    assert block2.parents == []
    assert block2.block_number == 0
    assert block2.pre_state_hash == '6284b05545513fead17c469aeb6baa2a11ed5a86eeda57accaa3bb95d60d5250'
    assert block2.post_state_hash == 'de7e15efcdfd0018497bcb40104afc863613619c0e47d5b2bf18c0c6d9e53865'
    assert block2.body_extra_bytes == ''
    assert block2.block_size == 192205
    assert block2.deploy_count == 11
    assert block2.fault_tolerance == 1.0
    assert block2.bonds == {
        '0444f16eee91c879a70a2d53e90b329670580395c8639ffef3f39ef74bdd9364279f877cd3d7cca806c815bd6fc568bf2fc0695a9c2cd6ac3d36fc1f4864243efb': 60,
        '04ab4c08f1986bb40c57d6aa24a650a4122bd6afb6b77990a1447230fc428cefd1d8d51b75812e549e0e4f2289c8fea6389b1d26ce71a7204782d92ea6c9862a35': 15,
        '04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519': 20
        }


def test_parse_show_block_output() -> None:
    input = r'''blockInfo {
  blockHash: "b3e8560f42451ee20f62c3d3bf52d00aa12131876bdf4fb2ddb6ac80937edbaf"
  sender: "04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519"
  seqNum: 1
  sig: "304502210086be15066503ab4cd0707f618e90eb65036eed374aa8f46c789939b93cc280c702201987cca65b56517e38629718063cb4e17f7dc07a35593ff27a19e58838b22fe8"
  sigAlgorithm: "secp256k1"
  shardId: "rchain"
  extraBytes: ""
  version: 1
  timestamp: 1574992953104
  headerExtraBytes: ""
  parentsHashList: "a0e9b7870112390da059ecf1d23636efb672a5e23aacb8ac9ade5cbd60ea394b"
  parentsHashList: "a0e9b7870112390da059ecf1d23636efb672a5e23aacb8ac9ade5cbd60ea394c"
  blockNumber: 1
  preStateHash: "d602762105b18cbb30747979d860657f7dd3919791bdc5db237ece9c607933a8"
  postStateHash: "62bfc991fdc775b92252548fe06ddecdff2be024120d149a9596b3f334d798f1"
  bodyExtraBytes: ""
  bonds {
    validator: "0444f16eee91c879a70a2d53e90b329670580395c8639ffef3f39ef74bdd9364279f877cd3d7cca806c815bd6fc568bf2fc0695a9c2cd6ac3d36fc1f4864243efb"
    stake: 79
  }
  bonds {
    validator: "04ab4c08f1986bb40c57d6aa24a650a4122bd6afb6b77990a1447230fc428cefd1d8d51b75812e549e0e4f2289c8fea6389b1d26ce71a7204782d92ea6c9862a35"
    stake: 52
  }
  bonds {
    validator: "04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519"
    stake: 56
  }
  blockSize: "20939"
  deployCount: 1
  faultTolerance: -1.0
  justifications {
    validator: "04126107bc353c73e044fb21a5085aeafeecd69895fc05ec5033764a586bf044ddb19da5140a00912d892bfe8e10aa34eb7f9a68308646c3ac8804096ba605c2d2"
    latestBlockHash: "3546358e6b5675b0e85f30edb55f837d851aa7446ee4f3cc4ada0e7b9d8d0bd7"
  }
  justifications {
    validator: "0412ce31a3c3cbf9c69c098e593568c476a6bf7efdf9f7579c80e5328af05db7693b077d04fabbed28bb4e2d28aaba4ee50af6eddfab957c9c3c16d629c9d6aac3"
    latestBlockHash: "3546358e6b5675b0e85f30edb55f837d851aa7446ee4f3cc4ada0e7b9d8d0bd7"
  }
  justifications {
    validator: "04f42348554ab10387739d6f709ddba0eb9b80792f57ed68a1c9341635c0777590e9dbdd316c57cff51587f2f320e30605e6641e042f030b83aaaa3a3268a00fb0"
    latestBlockHash: "3546358e6b5675b0e85f30edb55f837d851aa7446ee4f3cc4ada0e7b9d8d0bd7"
  }
}
deploys {
  deployer: "04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519"
  term: "@0!(2)"
  timestamp: 1574992934035
  sig: "3045022100994d74bfdb230d2af95d2090e3d9cd9020eb9c70224c2285585ea0f9a3aa406c022017ed317fe3e721626a9a1dea1804b0aeea5c32b627495dbbc8458e07ae5c1605"
  sigAlgorithm: "secp256k1"
  phloPrice: 1
  phloLimit: 100000
  validAfterBlockNumber: -1
  cost: 0
  errored: false
  systemDeployError: "Deploy payment failed: Insufficient funds"
}

'''
    block = parse_show_block_output(input)
    assert block.block_hash == 'b3e8560f42451ee20f62c3d3bf52d00aa12131876bdf4fb2ddb6ac80937edbaf'
    assert block.sender == '04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519'
    assert block.seq_num == 1
    assert block.sig == '304502210086be15066503ab4cd0707f618e90eb65036eed374aa8f46c789939b93cc280c702201987cca65b56517e38629718063cb4e17f7dc07a35593ff27a19e58838b22fe8'
    assert block.sig_algorithm == 'secp256k1'
    assert block.shard_id == 'rchain'
    assert block.extra_bytes == ''
    assert block.version == '1'
    assert block.timestamp == 1574992953104
    assert block.header_extra_bytes == ''
    assert block.block_number == 1
    assert block.pre_state_hash == 'd602762105b18cbb30747979d860657f7dd3919791bdc5db237ece9c607933a8'
    assert block.post_state_hash == '62bfc991fdc775b92252548fe06ddecdff2be024120d149a9596b3f334d798f1'
    assert block.body_extra_bytes == ''
    assert block.block_size == 20939
    assert block.deploy_count == 1
    assert block.fault_tolerance == -1.0
    assert block.parents == ['a0e9b7870112390da059ecf1d23636efb672a5e23aacb8ac9ade5cbd60ea394b', 'a0e9b7870112390da059ecf1d23636efb672a5e23aacb8ac9ade5cbd60ea394c']
    assert block.bonds == {
        '0444f16eee91c879a70a2d53e90b329670580395c8639ffef3f39ef74bdd9364279f877cd3d7cca806c815bd6fc568bf2fc0695a9c2cd6ac3d36fc1f4864243efb': 79,
        '04ab4c08f1986bb40c57d6aa24a650a4122bd6afb6b77990a1447230fc428cefd1d8d51b75812e549e0e4f2289c8fea6389b1d26ce71a7204782d92ea6c9862a35': 52,
        '04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519': 56
        }

    deploy = block.deploys[0]
    assert deploy.deployer == '04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519'
    assert deploy.term == "@0!(2)"
    assert deploy.timestamp == 1574992934035
    assert deploy.sig == '3045022100994d74bfdb230d2af95d2090e3d9cd9020eb9c70224c2285585ea0f9a3aa406c022017ed317fe3e721626a9a1dea1804b0aeea5c32b627495dbbc8458e07ae5c1605'
    assert deploy.sig_algorithm == 'secp256k1'
    assert deploy.phlo_price == 1
    assert deploy.phlo_limit == 100000
    assert deploy.valid_after_block_number == -1
    assert deploy.cost == 0
    assert deploy.error == 'false'
    assert deploy.system_deploy_error == 'Deploy payment failed: Insufficient funds'

def test_parse_show_block_output_without_parents() -> None:
    input = r'''blockInfo {
  blockHash: "b3e8560f42451ee20f62c3d3bf52d00aa12131876bdf4fb2ddb6ac80937edbaf"
  sender: "04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519"
  seqNum: 1
  sig: "304502210086be15066503ab4cd0707f618e90eb65036eed374aa8f46c789939b93cc280c702201987cca65b56517e38629718063cb4e17f7dc07a35593ff27a19e58838b22fe8"
  sigAlgorithm: "secp256k1"
  shardId: "rchain"
  extraBytes: ""
  version: 1
  timestamp: 1574992953104
  headerExtraBytes: ""
  blockNumber: 1
  preStateHash: "d602762105b18cbb30747979d860657f7dd3919791bdc5db237ece9c607933a8"
  postStateHash: "62bfc991fdc775b92252548fe06ddecdff2be024120d149a9596b3f334d798f1"
  bodyExtraBytes: ""
  bonds {
    validator: "0444f16eee91c879a70a2d53e90b329670580395c8639ffef3f39ef74bdd9364279f877cd3d7cca806c815bd6fc568bf2fc0695a9c2cd6ac3d36fc1f4864243efb"
    stake: 79
  }
  bonds {
    validator: "04ab4c08f1986bb40c57d6aa24a650a4122bd6afb6b77990a1447230fc428cefd1d8d51b75812e549e0e4f2289c8fea6389b1d26ce71a7204782d92ea6c9862a35"
    stake: 52
  }
  bonds {
    validator: "04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519"
    stake: 56
  }
  blockSize: "20939"
  deployCount: 1
  faultTolerance: -1.0
  justifications {
    validator: "04126107bc353c73e044fb21a5085aeafeecd69895fc05ec5033764a586bf044ddb19da5140a00912d892bfe8e10aa34eb7f9a68308646c3ac8804096ba605c2d2"
    latestBlockHash: "3546358e6b5675b0e85f30edb55f837d851aa7446ee4f3cc4ada0e7b9d8d0bd7"
  }
  justifications {
    validator: "0412ce31a3c3cbf9c69c098e593568c476a6bf7efdf9f7579c80e5328af05db7693b077d04fabbed28bb4e2d28aaba4ee50af6eddfab957c9c3c16d629c9d6aac3"
    latestBlockHash: "3546358e6b5675b0e85f30edb55f837d851aa7446ee4f3cc4ada0e7b9d8d0bd7"
  }
  justifications {
    validator: "04f42348554ab10387739d6f709ddba0eb9b80792f57ed68a1c9341635c0777590e9dbdd316c57cff51587f2f320e30605e6641e042f030b83aaaa3a3268a00fb0"
    latestBlockHash: "3546358e6b5675b0e85f30edb55f837d851aa7446ee4f3cc4ada0e7b9d8d0bd7"
  }
}
deploys {
  deployer: "04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519"
  term: "@0!(2)"
  timestamp: 1574992934035
  sig: "3045022100994d74bfdb230d2af95d2090e3d9cd9020eb9c70224c2285585ea0f9a3aa406c022017ed317fe3e721626a9a1dea1804b0aeea5c32b627495dbbc8458e07ae5c1605"
  sigAlgorithm: "secp256k1"
  phloPrice: 1
  phloLimit: 100000
  validAfterBlockNumber: -1
  cost: 0
  errored: false
  systemDeployError: "Deploy payment failed: Insufficient funds"
}

'''
    block = parse_show_block_output(input)
    assert block.block_hash == 'b3e8560f42451ee20f62c3d3bf52d00aa12131876bdf4fb2ddb6ac80937edbaf'
    assert block.sender == '04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519'
    assert block.seq_num == 1
    assert block.sig == '304502210086be15066503ab4cd0707f618e90eb65036eed374aa8f46c789939b93cc280c702201987cca65b56517e38629718063cb4e17f7dc07a35593ff27a19e58838b22fe8'
    assert block.sig_algorithm == 'secp256k1'
    assert block.shard_id == 'rchain'
    assert block.extra_bytes == ''
    assert block.version == '1'
    assert block.timestamp == 1574992953104
    assert block.header_extra_bytes == ''
    assert block.block_number == 1
    assert block.pre_state_hash == 'd602762105b18cbb30747979d860657f7dd3919791bdc5db237ece9c607933a8'
    assert block.post_state_hash == '62bfc991fdc775b92252548fe06ddecdff2be024120d149a9596b3f334d798f1'
    assert block.body_extra_bytes == ''
    assert block.block_size == 20939
    assert block.deploy_count == 1
    assert block.fault_tolerance == -1.0
    assert block.parents == []
    assert block.bonds == {
        '0444f16eee91c879a70a2d53e90b329670580395c8639ffef3f39ef74bdd9364279f877cd3d7cca806c815bd6fc568bf2fc0695a9c2cd6ac3d36fc1f4864243efb': 79,
        '04ab4c08f1986bb40c57d6aa24a650a4122bd6afb6b77990a1447230fc428cefd1d8d51b75812e549e0e4f2289c8fea6389b1d26ce71a7204782d92ea6c9862a35': 52,
        '04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519': 56
        }

    deploy = block.deploys[0]
    assert deploy.deployer == '04ac75929e588b030989d216043d2c98117d50d863c4f6b7115d737509f2df848d7fec7ccae9a7c5a45ad94d151ec4372ab552dd8c27ae9ed09f085377ebee0519'
    assert deploy.term == "@0!(2)"
    assert deploy.timestamp == 1574992934035
    assert deploy.sig == '3045022100994d74bfdb230d2af95d2090e3d9cd9020eb9c70224c2285585ea0f9a3aa406c022017ed317fe3e721626a9a1dea1804b0aeea5c32b627495dbbc8458e07ae5c1605'
    assert deploy.sig_algorithm == 'secp256k1'
    assert deploy.phlo_price == 1
    assert deploy.phlo_limit == 100000
    assert deploy.valid_after_block_number == -1
    assert deploy.cost == 0
    assert deploy.error == 'false'
    assert deploy.system_deploy_error == 'Deploy payment failed: Insufficient funds'

def test_extract_block_hash_from_propose_output() -> None:
    response = "Response: Success! Block a91208047c created and added.\n"
    assert extract_block_hash_from_propose_output(response) == "a91208047c"


def test_make_wallets_file_lines() -> None:
    wallets_map = {
        PrivateKey.from_hex("80366db5fbb8dad7946f27037422715e4176dda41d582224db87b6c3b783d709"): 40,
        PrivateKey.from_hex("120d42175739387af0264921bb117e4c4c05fbe2ce5410031e8b158c6e414bb5"): 45,
        PrivateKey.from_hex("1f52d0bce0a92f5c79f2a88aae6d391ddf853e2eb8e688c5aa68002205f92dad"): 26
    }

    output = make_wallets_file_lines(wallets_map)

    assert output == [
        '26218db6e5a2eed1901f72cea58fda7ef1f602c6,40,0',
        '42c828c183163cb50f6ad5207a10899b59aae91c,45,0',
        '2a11fd494610330f3b522562f7204670f8928133,26,0',
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
