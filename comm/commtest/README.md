# rchain-comm-doodles
Communication doodles for rchain.

`coop.rchain.comm.CommTest` is a simple application that attempts to
keep _n_ copies of a database in sync. There is a rudimentary
"discovery" protocol, whereby a new node can join the mesh, receive
the current state of the store, and maintain future mutations. There
is no arbitration or concensus, so order may differ among the nodes.
 
Each node also runs an HTTP server, which allows interaction with the
store (setting values, querying the store, and so on).

For each copy of this you want to run, decide on
 1. a communication port and
 1. an HTTP port

### Example Invocation

#### Running the things

Suppose I want to run on one physical machine three nodes A, B, and C,
listening on ports 33333, 33334, and 33335 (respectively). Give them
each an HTTP server, with HTTP ports 8883, 8884, and 8885. The order
in which these nodes are started doesn't matter, so:

##### Run Node A
```
$ sbt "run --listen *:33333 --http-port 8883"
```
and maybe set a value:
```
$ curl 'localhost:8883/set/A(x)/foo'
```

##### Run Node B, attaching it to A
```
$ sbt "run --home localhost:33333 --listen *:33334 --http-port 8884"
```
and dump its current state to verify:
```
$ curl 'localhost:8884/dump'
1. A(x) -> [foo]
```

##### Node C attaches to either A or B; here we chose B:
```
$ sbt "run --home localhost:33334 --listen *:33335 --http-port 8885"
```
and check its state as well
```
$ curl 'localhost:8885/dump'
1. A(x) -> [foo]
```

#### Interacting with the things
Griff's KV API is attached (loosely) to the HTTP endpoint, so you can issue HTTP requests of the following forms:

##### GET `/get/:key`
Return a JSON value that is the result of the query in `:key`.

##### GET `/dump`
Return some text which is the debugging print output of the store.

##### GET `/set/:key/:value`
Add the kv pair `(:key, :value)` to the store.

##### POST `/set`
Same as the GET version, but JSON of the form `{"key": :key, "value": :value}` may be in the POST body. This is useful if either `:key` or `:value` is complicated enough to make the command line irritating.

##### GET `/peers`
Returns the current list of peers known to the receiver.

Here's an example interaction with the three nodes A, B, and C above (with debugging output shortened in cases):
Add the pair `(A(x), foo)` to the store at A
```
$ curl localhost:8883/'set/A(x)/foo'
```
and fetch it from B
```
curl 'localhost:8884/get/A(x)'
["A(x)",["{x} -> [foo]"]]
```
Looks good. Let's set a bunch of values willy-nilly, which I have a script for (these are Griff's test stores):
```
ports="8883\n8884\n8885"
curl localhost:$(echo $ports | shuf -n 1)/set -d '{"key":"A(1)","value":"one"}'
curl localhost:$(echo $ports | shuf -n 1)/set -d '{"key":"A( 2  )","value":"two"}'
curl localhost:$(echo $ports | shuf -n 1)/set -d '{"key":"A(3, 4 ,5)","value":"three"}'
...
```
and then get them from everywhere:
```
$ for p in 8883 8884 8885; do curl -s localhost:${p}/dump > /tmp/dump.${p}; echo $p; done
8883
8884
8885
```
and see whether they agree
```
$ ls -l /tmp/dump.*
-rw-r--r-- 1 kirkwood kirkwood 647 Aug  9 12:45 /tmp/dump.8883
-rw-r--r-- 1 kirkwood kirkwood 647 Aug  9 12:45 /tmp/dump.8884
-rw-r--r-- 1 kirkwood kirkwood 647 Aug  9 12:45 /tmp/dump.8885
$ diff3 /tmp/dump.*
<no output>
```
### Strawman Bootstrapping Protocol
Bootstrapping a new node requires a "home" node to contact for
initialization. Any node may serve this purpose, and the bootstrapping
node joins that node's network. The protocol is simple for a new node
N and a home node O:

1. N sends `Hello` to O causing O to add N to its list of peers
1. N sends `GetPeers` to O
1. O sends `Peers` (a list of O's peers) back to N causing N to use as peers all of O's peers
1. N sends `Hello` to each new peer received from O; N is a member of the network now
1. N sends `GetBlocks` to O
1. O sends `Blocks` (all known blocks) to N

Since these are essentially unsorted blocks, there are still
races. The obvious one: a mutation observed at a node Q in the network
other than O or N after N sends `Hello` to Q but before that message
is processed by Q will not be registered by N unless it also is
observed at O before the `Blocks` response from O to N is built.

