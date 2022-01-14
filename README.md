# Revdefine RNode

Revdefine RNode is just a special RNode which does not only provide all the node function RNode has but also the
additional api which Revdefine explorer needs.

# PreRequirement

1. install mongo-cli -> https://docs.mongodb.com/mongocli/stable/install/

## How to get the node

### 1. compile by yourself

Follow original RChain compile guidance
and [install the prerequisites](https://github.com/rchain/rchain/blob/dev/DEVELOPER.md#prerequisites).

After installing the prerequisites. Run

```bash
sbt revdefine/docker:publishLocal
```

If memeory is not enough, remember to config the max memory in compile.

```bash
_JAVA_OPTIONS="-Xms2G -Xmx4G" sbt revdefine/docker:publishLocal
```

After the compilation is done, you can find the binary in `$root/revdefine/target/docker/stage/opt/docker/bin/revdefine`
or the docker images.

### 2. use the self-pushed docker image

```bash
docker pull zsluedem/revdefine:revdefine-0.1.4
```

## How to run the node

The revdefine is fully compatible with RChain RNode which means all the configurations are accepted. However, you need
to provide a little more configurations which are necessary for Revdefine RNode.

### Run a mongo instance from docker

Before starting the Revdefine RNode, you have to start a [MongoDB](https://www.mongodb.com/) instance (or cluster)
because Revdefine RNode needs an indexed database for data.

Revdefine needs the [replication function of MongoDb](https://docs.mongodb.com/manual/replication/). I am going to
demonstrate a single-MongoDB-instance replica set.If you want to start a real MongoDB replica set, please refer to the
[official MongoDB documents](https://docs.mongodb.com/manual/administration/replica-set-deployment/).

#### Setup tutorial

The setup tutorial is going to demonstrate how to start the revdefin RNode with mongo in docker-compose file.

#### 1. prepare docker-compose file

```yaml
version: "2.3"
services:
  rnode:
    image: zsluedem/revdefine:revdefine-0.1.4
    user: root
    networks:
      - rchain-net
    container_name: rnode
    environment:
      - MONGO_URI=mongodb://define-mongo:27017   # use the mongo container name as host name 
    ports:
      - "40403:40403"
      - "40401:40401"
      - "40400:40400"
      - "40404:40404"
      - "40406:40406"  # revdefine use 40406 port
    command:
      [
          "-XX:MaxRAMPercentage=65",
          "-XX:MaxDirectMemorySize=1G",
          "-XX:+ExitOnOutOfMemoryError",
          "-XX:+HeapDumpOnOutOfMemoryError",
          "-XX:HeapDumpPath=/var/lib/rnode/heap.hprof",
          "run",
          "--bootstrap=rnode://ef34d5e196c8292cfe0da69d7df2e9ae6cea0be2@node0.root-shard.mainnet.rchain.coop?protocol=40400&discovery=40404",
          "--network-id=mainnet1",
          "--shard-name=root1",
          "--fault-tolerance-threshold=-1",
          "--api-enable-reporting",
          "--finalization-rate=1",
          "--max-number-of-parents=1"
      ]
    volumes:
      - /rchain/rnode:/var/lib/rnode
  mongo:
    image: mongo
    user: root
    networks:
      - rchain-net
    container_name: define-mongo  # mongo host name in docker network
    ports:
      - "127.0.0.1:27017:27017"
    volumes:
      - /rchain/rnode/revdefine/mongo:/data/db  # mongDB db file
      - /rchain/rnode/revdefine/mongod.conf:/etc/mongo/mongod.conf  # mongoDB configuration
    command:
      [ "--config", "/etc/mongo/mongod.conf" ]

networks:
  rchain-net:
```

what's in `mongod.conf`.
And [for more information on mongo configuration](https://docs.mongodb.com/manual/reference/configuration-options/).

```conf
replication:
   replSetName: rs0
storage:
   wiredTiger:
     engineConfig:
       cacheSizeGB: 1.5
```

**NOTE!!!!!**

1. `replSetName: rs0` is very important, and it would be used in replica setup.
2. MongoDB host name `define-mongo` is also important, and it would be used in replica setup.
3. Revdefine RNode needs to config target **mongoDB host** by env.

#### 1. start a MongoDB instance

```bash
docker-compose up -d mongo
```

**NOTE: The `replSetName: rs0` is very important on configuring replica set.

#### 2. Setup replica set(This step is only needed the first time you start the replica set.)

```bash
$ mongo            # start a mongo client in a terminal
> replica_config = {_id: "rs0", version: 1, term:1,members: [{ _id: 0, host : "define-mongo:27017"}]}
> rs.initiate(replica_config)
```

#### 3. Initialize the mongo database

The node need to initial transaction data to proceed. Currently there are two options for initializing the database->
mainnet and other networks. Because mainnet had hard fork , it requires some special process for that initializing.

#### A. Initialize mainnet

The mainnet initialization needs

1. before-hard-fork1-transaction-history-file
2. a rnode observer database which enable transaction database and got all the after-hard-fork2 transactions.

The docker script for initialization is below:

```yaml
version: "2.3"
services:
  rnode:
    image: zsluedem/revdefine:revdefine-0.1.4
    user: root
    container_name: rnode
    environment:
      - MONGO_URI=mongodb://define-mongo:27017   # use the mongo container name as host name and the mongo container needs to be up
    entrypoint: /opt/docker/bin/init-mainnet
    command:
      [
          "--data-dir",
          "/var/lib/rnode",
          "--target-block-hash",
          "TARGET_BLOCK_HASH",
          "--transaction-file",
          "/var/lib/transactions.txt",
          "--genesis-bonds-file",
          "/var/lib/bonds.txt",
      ]
    volumes:
      - DATA_DIR_PATH:/var/lib/rnode
      - TRANSACTIONS_FILE:/var/lib/transactions.txt
```

#### B. Initialize custom network

The custom network initialization needs

1. genesis wallet file
2. genesis bonds file
3. genesis blockHash
4. a observer node database

The docker script for initialization is below:

```yaml
version: "2.3"
services:
  rnode:
    image: zsluedem/revdefine:revdefine-0.1.4
    user: root
    container_name: rnode
    environment:
      - MONGO_URI=mongodb://define-mongo:27017   # use the mongo container name as host name and the mongo container needs to be up
    entrypoint: /opt/docker/bin/init
    command:
      [
          "--data-dir",
          "/var/lib/rnode",
          "--genesis-block-hash",
          "GENESIS_BLOCK_HASH",
          "--genesis-wallet-file",
          "/var/lib/wallets.txt",
          "--genesis-bonds-file",
          "/var/lib/bonds.txt",
      ]
    volumes:
      - DATA_DIR_PATH:/var/lib/rnode
      - WALLET_FILE:/var/lib/wallets.txt
      - BONDS_FILE:/var/lib/bonds.txt
```

#### 4. start Revdefine RNode

```bash
docker-compose up -d rnode
```

### Difference between Revdefine RNode and normal RNode

1. Revdefine define a set of totally new api on port 40406 and can be used by the explorer while Revdefine RNode hold
   the same api as normal RNode on other ports.
2. Revdefine would create an additional folder -> `revdefine` in rnode data folder.
3. Revdefine is fully compatible with RNode which means you can stop Revdefine RNode and start normal RNode on the same
   data file.

### Revdefine RNode additional configuration

1. MongoDB configuration which should be defined in environments.
2. Mainnet before hard-fork1 compatible deployID index file can be put in `rnode/revdefine/deployId.txt`. With that and
   old block store, you can look for old deploy before hard-fork1.