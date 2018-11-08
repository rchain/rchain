"""
First approximation:
1) Bootstrap node with a bonds file in the genesis block for at least one bonded validator
2) Bonded validator joins the network and proposes to create a block chain length 10
3) Unbounded validator joins and goes through the process to bond.
4) Validators create a blockchain length 20
5) Unbounded validator joins and attempts to catch the state (20 blocks)
"""


"""
Second approximation:
1) Create boostrap node
2) Create validator B bonded with the bootstrap node
3) B executes propose 10 times in a row
4) Create new validator U
5) Make U bonded
6) Execute 10 propose operations
7) Create new validator N and wait until it catches up
"""


def test_heterogenous_validators(bootstrap_node):
    bonded_validator_key = generate_validator_key()
    bonds_file = make_bonds_file(bonded_validator_key)
    bootstrap_node = create_bootstrap_node(bonds_file)
    bonded_validator_node = create_peer()
    for _ in range(10):
        bonded_validator_node.deploy()
        bonded_validator_node.propose()
    joining_validator = create_peer()
    bond_validator(bootstrap_node, joining_validator)
    for _ in range(10):
        joining_validator.deploy()
        last_block_id = joining_validator.propose()
    unbonded_validator = create_peer()
    unbonded_validator.wait_until_receives(last_block_id)
