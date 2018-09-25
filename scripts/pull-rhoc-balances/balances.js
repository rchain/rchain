/// balances.js -- RHOC balance reporting

// Copyright (C) 2018 dc <dc@dapp.org>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the Apache License as published by the Apache
// Software Foundation, either version 2 of the License, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// Apache License, Version 2 for more details.
//
// You should have received a copy of the Apache License, Version 2
// along with this program. If not, see <https://www.apache.org/licenses/>.

const R    = require('ramda');
const fs   = require('fs');
const Web3 = require('web3');
const bb   = require('bluebird');

const web3 = new Web3(process.env.ETH_WS || 'wss://mainnet.infura.io/_ws');
const rhoc = new web3.eth.Contract(require('./abi.json'), "0x168296bb09e24a88805cb9c33356536b980d3fc5");
const keys = new Set();

const fromBlock = 3383352;
const toBlock   = process.env.BLOCK || 6350512;
const batch     = 500

// Block range increments
const blockRange = () => R.map(
  n => fromBlock + batch * n,
  R.range(0, (1 + (toBlock - fromBlock) / batch) >>> 0)
);

const blockPairs = R.aperture(2, blockRange());

const writeStream = fs.createWriteStream('balances.csv');

// Parse all transfer events between a pair of blocks
const parseTransfers = (pair) => {
  return rhoc.getPastEvents('Transfer', { fromBlock: pair[0], toBlock: pair[1] })
    .then(transfers => {
      bb.map(transfers, (t) => {
        return [t.returnValues.from, t.returnValues.to].forEach(addKey)
      }, {concurrency: 5})
    })
    .catch(e => error(e, pair));
}

// Only write unique keys
const addKey = (key) => { if (!keys.has(key)) { keys.add(key); write(key); } }

// Retrieve key state and append to file
const write = (key) => {
  return rhocBalance(key).then(bal => {
    return isContract(key).then(c => {
      console.log('Key:', key, 'Balance:', bal, 'Contract:', c);
      if(bal > 0) writeStream.write(key+','+bal+','+c+'\n');
    })
  })
  .catch(e => error(e, key));
}

// Batch the retrieval of transfer events in order to
// be able to run against infura.io
bb.map(blockPairs, (pair) => {
    console.log('Blocks:', pair, 'Keys:', keys.size);
    return parseTransfers(pair);
}, {concurrency: 5})
.then(() => {
  console.log('Completed')
})
.finally(() => {
  writeStream.end();
  process.exit();
});

// Return the RHOC balance for a given key
const rhocBalance = (key) => {
  return rhoc.methods.balanceOf(key).call({}, toBlock)
    .then(balance => { return balance })
}

// Return 1 if the key is a contract 0 if not
const isContract = (key) => {
  return web3.eth.getCode(key).then(code => {
    return (code == '0x') ? 0 : 1
  })
}

const error = (e, x) => {
  console.log('Error:', x);
  console.log(e);
  process.exit();
}
