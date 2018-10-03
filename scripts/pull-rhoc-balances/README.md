## rrc

Generate a balance report for the RHOC REV conversion. Outputs a list of all
keys along with RHOC balance (in wei) and a 0|1 flag to indicate whether or not
there is a contract at that address.

### Setup

Clone the repo and run npm install from the project root:

```bash
$ git clone https://github.com/rchain/rchain.git
$ cd rchain/scripts/pull-rhoc-balances
$ npm install
```

### Generate a RHOC balance report

If you're not running the report for the first time you'll need to remove the
previous version.

```bash
$ rm balances.csv
```

Then run the balances script. Specify options by setting shell variables:

```bash
BLOCK=<block height>
ETH_WS=<websockets provider>
```

Eth provider defaults to Infura so running a local node is optional.

```bash
$ BLOCK=6350512 npm run balances
```

### Improvements

* Timestamp balances.csv output so that it can be included in version control.
