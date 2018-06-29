```
# Setup
npm install

# Generate a rhoc balance report
rm balances.csv
npm run balances

# Options:
BLOCK=<blockheight>
ETH_WS=<websockets provider> (defaults to infura)

# Example
BLOCK=5866762 npm run balances
```
