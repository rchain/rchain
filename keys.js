const R    = require('ramda');
const fs   = require('fs');
const Web3 = require('web3');

const web3 = new Web3('wss://mainnet.infura.io/_ws');
const rhoc = new web3.eth.Contract(require('./abi.json'), "0x168296bb09e24a88805cb9c33356536b980d3fc5");

const fromBlock = 3383352;
const toBlock   = process.env.BLOCK || 5866762;
const batch     = 500

const rangeStep = (start, step, stop) => R.map(
  n => start + step * n,
  R.range(0, (1 + (stop - start) / step) >>> 0)
);

const pairs     = R.aperture(2);
const blocks    = pairs(rangeStep(fromBlock, batch, toBlock));

const writeStream = fs.createWriteStream('keys.txt');

const getTransfers = (pair) => {
  return rhoc.getPastEvents('Transfer', { fromBlock: pair[0], toBlock: pair[1] })
    .then(transfers => transfers.forEach(t => append(t)))
    .catch(e => console.log(e));
}

const append = (transfer) => {
  const keys = transfer.returnValues.from+"\n"+transfer.returnValues.to
  writeStream.write(keys);
  console.log(keys);
}

R.forEach(getTransfers, blocks);
