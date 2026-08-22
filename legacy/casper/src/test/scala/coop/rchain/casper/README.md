# Casper unit tests

To parallelize execution of tests with CI, tests were grouped into folders.

This enables to run tests in specific folder with wildcard e.g.  
`sbt 'casper/test:testOnly coop.rchain.casper.addblock.*'`.

 Main tests for multi parent casper are in these 3 folders: `addblock`, `batch1` and `batch2`.  
 The idea is to equalize the duration per group (folder). Adding a block takes long enough to be alone in one folder. 
