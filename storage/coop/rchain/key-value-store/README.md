# KeyValueStore

KeyValueStore is an exploratory programming project to demonstrate the application of Prolog unification to query processing.

Each key and query is a recursive tree where each node is a TermTree.  A TermTree can be:
* a predicate with parameters, where each parameter is  TermTree
* an atom, which is either a constant or numeral, both of which are represented by strings

The database is represented by a scala.collection.mutable.SortedMap.

# Configuration

Clone the repository and run the program with sbt.

# Usage

In sbt, run the program in one of three ways:

1.  no arguments : interactive test harness with empty store
2.  < store file path > : read file into store and launch interactive test harness
3.  test < store file path > : read file into store and then run corresponding test

There are three stores in the stores directory:

1.  storeFlat.txt
2.  storeNested.txt
3.  storeRecursive.txt


