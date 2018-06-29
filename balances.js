const R        = require('ramda');
const fs       = require('fs');
const Web3     = require('web3');

const web3 = new Web3('wss://mainnet.infura.io/_ws');
const rhoc = new web3.eth.Contract(require('./abi.json'), "0x168296bb09e24a88805cb9c33356536b980d3fc5");
const keys = new Set();

const toBlock   = process.env.BLOCK || 5866762;

const writeStream = fs.createWriteStream('balances.csv');

const keyReader = require('readline').createInterface({
  input: fs.createReadStream('keys.txt'),
  removeHistoryDuplicates: true,
  terminal: false
})

const rhocBalance = (key) => {
  return rhoc.methods.balanceOf(key).call({}, toBlock)
    .then(balance => { return balance })
}

const isContract = (key) => {
  return web3.eth.getCode(key).then(code => {
    return (code == '0x') ? 0 : 1
  })
}

const append = (key) => {
  return rhocBalance(key).then(bal => {
    return isContract(key).then(c => {
      console.log(keys.size, key, bal, c);
      writeStream.write(key+','+bal+','+c+'\n');
      keys.delete(key);
    })
    .catch(e => {
      console.log('Error:', key);
      console.log(e);
      process.exit();
    });
  })
}

keyReader.on('line', (key) => {
  keys.add(key);
  console.log(key);
})
keyReader.input.on('end', () => {
  require('bluebird').map(keys, (key) => {
    return append(key);
  }, {concurrency: 5})
  .then(() => console.log('Completed', keys.size))
  .finally(() => process.exit());
});
