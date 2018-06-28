const R    = require('ramda');
const fs   = require('fs');
const Web3 = require('web3');

const web3 = new Web3('wss://mainnet.infura.io/_ws');
const rhoc = new web3.eth.Contract(require('./abi.json'), "0x168296bb09e24a88805cb9c33356536b980d3fc5");

const rhocBalance = (key) => {
  rhoc.methods.balanceOf(key).call({}, toBlock)
    .then(bal => console.log(key, bal))
}
