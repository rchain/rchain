# SETUP

    #execute from the `examples` directory
    . keys.env
    ./propose.sh $GENESIS_PRV vault_demo/0.create_genesis_vault.rho "-e s/%REV_ADDR/$GENESIS_REV/"

# DEMO START

## Access your own vault

Here's how you'd access your vault once you're on-chain:

    ./propose.sh $ALICE_PRV vault_demo/2.check_balance.rho "-e s/%REV_ADDR/$ALICE_REV/"
    
## Know your RevAddress

    ./propose.sh $ALICE_PRV vault_demo/1.know_ones_revaddress.rho "-e s/%PUB_KEY/$ALICE_PUB/"

Notice that this doesn't need to be executed using Alice's private key.

## Transfer to a RevAddress

Here's how you'd transfer funds to any other RevVault.
The only things needed are access to your RevVault, and the target RevAddress.

*In this example we're deploying as a Testnet operator, but it works exactly the same for a normal user.*

    ./propose.sh $GENESIS_PRV vault_demo/3.transfer_funds.rho "-e s/%FROM/$GENESIS_REV/ -e s/%TO/$ALICE_REV/"
    ./propose.sh $ALICE_PRV vault_demo/2.check_balance.rho "-e s/%REV_ADDR/$ALICE_REV/"

Notice that anybody can query the balance of any RevVault.

    ./propose.sh $GENESIS_PRV vault_demo/2.check_balance.rho "-e s/%REV_ADDR/$ALICE_REV/"

An exchange would do the transfer exactly the same way.
That's also exactly what the Testnet operators are going to do to distribute initial funds.

Because the "transfer" method takes a RevAddress (and not a RevVault),
transfers between different "kinds", or security schemes of RevVaults are possible.

For now, we only provide a simple RevVault that only grants access to its designated user.
By "user" here we mean the deployer of the contract that uses the RevVault and its corresponding AuthKey.

## Onboard a friend on the platform

    ./propose.sh $ALICE_PRV vault_demo/3.transfer_funds.rho "-e s/%FROM/$ALICE_REV/ -e s/%TO/$BOB_REV/"
    ./propose.sh $ALICE_PRV vault_demo/2.check_balance.rho "-e s/%REV_ADDR/$ALICE_REV/"

Notice the transfer hasn't been finished yet. Still, funds have been deducted from Alice's vault.

Let's execute as Bob and check his balance:

    ./propose.sh $BOB_PRV vault_demo/2.check_balance.rho "-e s/%REV_ADDR/$BOB_REV/"

Notice the transfer that was initiated previously has now been finished.

## Transfer back to the genesis vault

    #check balances
    ./propose.sh $BOB_PRV vault_demo/2.check_balance.rho "-e s/%REV_ADDR/$BOB_REV/"
    ./propose.sh $BOB_PRV vault_demo/2.check_balance.rho "-e s/%REV_ADDR/$GENESIS_REV/"

    #transfer
    ./propose.sh $BOB_PRV vault_demo/3.transfer_funds.rho "-e s/%FROM/$BOB_REV/ -e s/%TO/$GENESIS_REV/"

    #check balances
    ./propose.sh $BOB_PRV vault_demo/2.check_balance.rho "-e s/%REV_ADDR/$BOB_REV/"
    ./propose.sh $BOB_PRV vault_demo/2.check_balance.rho "-e s/%REV_ADDR/$GENESIS_REV/"

## Attempt a transfer despite insufficient funds

    ./propose.sh $ALICE_PRV vault_demo/3.transfer_funds.rho "-e s/%FROM/$ALICE_REV/ -e s/%TO/$BOB_REV/"

## Attempt a transfer despite invalid RevAddress

    ./propose.sh $ALICE_PRV vault_demo/3.transfer_funds.rho "-e s/%FROM/$ALICE_REV/ -e s/%TO/lala/"

Notice the platform only checks whether the address is syntactically correct. A typo means the funds are lost.
