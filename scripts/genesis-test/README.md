RChain Genesis test
===================

Scripts to setup and test genesis in Docker containers.

## Dependencies

	pip install --user -r requirements.txt

## Usage

1. Generate a random *setup*:
    ```
    ./generate-setup --validator-count 2 --required-sigs 2 --wallet-count 5 > setup-small.yaml
    ```
   Pass `--help` switch to learn about more options. The setup generated in last step may look like:
    ```
    deploy_timestamp: 1
    required_sigs: 2
    approval_interval: 10s
    approval_duration: 2min
    bootstrap:
      name: bootstrap-c4e795
      addr: c4e7955edfbd4e5c60dab3ab80da19a5e2de3a03
      sk_pem: |
        -----BEGIN PRIVATE KEY-----
        MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgp/bSuhg8ngnztA4l
        FusXhJ5oY9hRJueLHzcQA8h8zw6hRANCAATrm/0Mrg53r4EbCJohBjO2EPISL2G1
        u3nfSSxculX/IpqBqAgnRrZzGRMraCxBphgqw4bcA6/bTyx7tTEjeZJx
        -----END PRIVATE KEY-----
    validators:
    - name: validator-9ea830
      addr: 9ea83092d899381893be3c26d05d5577dcb1892e
      sk_pem: |
        -----BEGIN PRIVATE KEY-----
        MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgOJKWFAefClvP2QW1
        WQQlMSf2Cv5kD+1ekXFQTp/4ky2hRANCAARvo/p8V5tagauO410BlZhbXQXAp/d2
        8caGTSV3o3oKxJ7NeWJZ2eia4eH3cA8pDg07kaSWl7GG58CkfmaFlsxo
        -----END PRIVATE KEY-----
      wallet:
        sk: 111a0ab826f13de289323bfd95431a241601b278b9dc84bd74847675d820fc00
        pk: 56eccd84e5b1bef290045052bd0a95725c46e3b16f7070911985e1206cdb8ebc
        balance: 73
    - name: validator-9a0edb
      addr: 9a0edbdd20d4d73afd3842af53993ba198d0e15e
      sk_pem: |
        -----BEGIN PRIVATE KEY-----
        MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgxw3amMRRe6bTvZhu
        mifsCbbmpsft5hZvZJzcd70bdG+hRANCAASXZbpC0g5rTBEJwe/okC0m1cB/+sh8
        /tcF3U/p5LUVWozihRtjkkb5yvkJ37nGr1CWS9M7F/dUBOMdrsst7JUa
        -----END PRIVATE KEY-----
      wallet:
        sk: c81d52e385339914cf813b936712b54eb213556fff8ad2f816b25efa61e3fd38
        pk: 62acbefa2b9c6dc44832ab286ee1f4c4c0ecce63c26650af0bbbb6ed443f6a96
        balance: 74
    wallets:
    - sk: fd32c585115b84da0a8a8c5fa3558b71fa8f0a89b2421f473a05c945aec70ac5
      pk: 897bedbb525bb661422e3cfed5ef9b0799fa423674071aedd270308c433eeef9
      balance: 89
    - sk: 42e5ce9754e964375e24ee3f3dc7188d73e432ac84b7b6f79c6fc1b855c7bab1
      pk: 2f6e3dc8a8f03ca2b4ab1d278c797365274fa1e362658bd67a0983832ba20c60
      balance: 60
    - sk: be5b92e29c310ce92f879e395310bb9e3b91ace09606937e39ba784da6e0ad21
      pk: 06afce74f434cb70ea73483cecb4b1e326656e2b43f46a4238efc031096acba3
      balance: 69
    - sk: 694781c50c44b08775cc2c79d8c7bda921fd199b80c52e32d353e8581d1cc528
      pk: ad878b567154a16d148ce3e6e33bfb88fe252c55e2b4cf12d4d9e11a11c2c9d5
      balance: 75
    - sk: a712c13cee59e17192c6a01de267735e78ba736cc866c020d5eca15ce9a4fcf9
      pk: b1baf2c297cb5986888b4c26c1cd187794f11e948feace3c4a200418388d1d8c
      balance: 53
    ```

2. Deploy a setup
    ```
    % ./deploy-setup setup-small.yaml 
    Context initialized in /tmp/rchain-genesis-setup_a0brhxy6
    % docker ps
    CONTAINER ID        IMAGE                      COMMAND                  CREATED             STATUS              PORTS               NAMES
    4ed77bf6abef        coop.rchain/rnode:latest   "bin/rnode --profile…"   3 seconds ago       Up 3 seconds                            validator-9a0edb
    34a8beacdcce        coop.rchain/rnode:latest   "bin/rnode --profile…"   4 seconds ago       Up 3 seconds                            validator-9ea830
    e631421a6b3b        coop.rchain/rnode:latest   "bin/rnode --profile…"   4 seconds ago       Up 4 seconds                            bootstrap-c4e795
    % tree /tmp/rchain-genesis-setup_a0brhxy6
    /tmp/rchain-genesis-setup_a0brhxy6
    ├── bonds.txt
    ├── bootstrap-c4e795
    │   ├── casper-block-store
    │   │   ├── data.mdb
    │   │   └── lock.mdb
    │   ├── node.certificate.pem
    │   ├── node.key.pem
    │   ├── rnode.log
    │   └── rspace
    │       ├── casper
    │       │   ├── data.mdb
    │       │   └── lock.mdb
    │       ├── data.mdb
    │       └── lock.mdb
    ├── validator-9a0edb
    │   ├── casper-block-store
    │   │   ├── data.mdb
    │   │   └── lock.mdb
    │   ├── node.certificate.pem
    │   ├── node.key.pem
    │   ├── rnode.log
    │   └── rspace
    │       ├── casper
    │       │   ├── data.mdb
    │       │   └── lock.mdb
    │       ├── data.mdb
    │       └── lock.mdb
    ├── validator-9ea830
    │   ├── casper-block-store
    │   │   ├── data.mdb
    │   │   └── lock.mdb
    │   ├── node.certificate.pem
    │   ├── node.key.pem
    │   ├── rnode.log
    │   └── rspace
    │       ├── casper
    │       │   ├── data.mdb
    │       │   └── lock.mdb
    │       ├── data.mdb
    │       └── lock.mdb
    └── wallets.txt

    12 directories, 29 files
    % docker logs -f bootstrap-c4e795
    ```
