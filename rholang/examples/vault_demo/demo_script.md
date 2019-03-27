# SETUP

    #execute from the `examples` directory
    . keys.env
    ./propose.sh $GENESIS_PRV vault_demo/0.create_genesis_vault.rho
    ./propose.sh $GENESIS_PRV vault_demo/0.transfer_from_genesis.rho "-e s/%REV_ADDR/$ALICE_REV/"
    ./propose.sh $GENESIS_PRV vault_demo/1.create_vault.rho "-e s/%PUB_KEY/$ALICE_PUB/"

# DEMO START

## Access your own vault

Here's how you'd access your vault once you're on-chain:

    ./propose.sh $ALICE_PRV vault_demo/2.access_your_own_vault.rho

## Transfer to a RevAddress

Here's how you'd transfer funds to any other RevVault.
The only things needed are access to your RevVault, and the target RevAddress.

    ./propose.sh $ALICE_PRV vault_demo/3.transfer_funds.rho "-e s/%REV_ADDR/$BOB_REV/"
    ./propose.sh $ALICE_PRV vault_demo/2.access_your_own_vault.rho

Notice that an exchange would do the transfer exactly the same way.

Because the "transfer" method takes a RevAddress (and not a RevVault),
transfers between different "kinds", or security schemes of RevVaults are possible.

For now, we only provide a simple Lockbox that only grants access to its designated user.
By "user" here we mean the deployer of the contract that uses the Lockbox and its underlying RevVault.
    
Notice that the transfer was initiated, but hasn't finished yet. Have burnt the REV?     

## Onboard a friend on the platform

*This is subject to change, it might be that the platform will perform this step automatically during users first deploy.* 

    ./propose.sh $ALICE_PRV vault_demo/1.create_vault.rho "-e s/%PUB_KEY/$BOB_PUB/"

Notice the transfer that was initiated previously has now been finished.
Let's become Bob and check the balance. 

    ./propose.sh $BOB_PRV vault_demo/2.access_your_own_vault.rho

## Transfer back to the genesis vault

    #check balances
    ./propose.sh $BOB_PRV vault_demo/2.access_your_own_vault.rho
    #FIXME unify genesis vault access - use 'access vault' script here
    ./propose.sh $GENESIS_PRV vault_demo/0.create_genesis_vault.rho

    #transfer
    ./propose.sh $BOB_PRV vault_demo/3.transfer_funds.rho "-e s/%REV_ADDR/$GENESIS_REV/"

    #check balances
    ./propose.sh $BOB_PRV vault_demo/2.access_your_own_vault.rho
    #FIXME unify genesis vault access - use 'access vault' script here
    ./propose.sh $GENESIS_PRV vault_demo/0.create_genesis_vault.rho

## Attempt a transfer despite insufficient funds

    ./propose.sh $ALICE_PRV vault_demo/3.transfer_funds.rho "-e s/%REV_ADDR/$BOB_REV/"

## Attempt a transfer despite invalid RevAddress

    ./propose.sh $ALICE_PRV vault_demo/3.transfer_funds.rho "-e s/%REV_ADDR/lala/"

Notice the platform only checks whether the address is syntactically correct. A typo means the funds are lost.
