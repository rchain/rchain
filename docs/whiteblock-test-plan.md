# RChain Test Plan

## Overview

The network has N validators plus one bootstrap node.

## Test Procedure

Per test case:
1. Build network
2. Provision nodes
3. Configure network conditions between nodes according to specified test case
4. Configure actions and behavior between nodes according to specified test case
5. Output performance data in CSV format
6. Aggregate data, parse, & present data
8. Push data to appropriate repo
9. Reset environment

## Test Environment

The proposed testing initiatives will be conducted within the Whiteblock test
lab on a group of 6 rack mounted chassis servers.

Should testing initiatives require additional computational resources or
demand a higher overhead than can be adequately provided within the proposed
environment, the Whiteblock cloud-based solution can be used. However, in the
interest of cost-effectiveness, the current on-premises lab setup can
adequately provide the current scope of work. 

## System Specifications

| Component   | Value                                          |
|-------------|------------------------------------------------|
| Model Name  | PowerEdge R720                                 |
| Vendor      | Dell                                           |
| CPUs        | (2) Intel Xeon CPU E5-2667 v2 @ 3.30GHz        |
| CPU Max MHz | 4000.0000                                      |
| NIC         | 82599ES 10-Gigabit SFI/SFP+ Network Connection |
| RAM         | 256GB DDR4                                     |


## Performance Metrics

Time measurements are expressed in terms of the time passed on the node
coordinating the tests.  Assuming the coordinating node's clock hasn't been tempered with.

| Value			    | Description | 
| ------------------------- | -------- | 
| Discovery Time	    | The length of time from a node startup to node become aware of its first peer | 
| Block Propagation Time    | The length of time it takes for a message, once broadcast, to be received by a majority (99%) of peers within the network |
| Consensus Time	    | The length of time from a finish of a propose operation, to all the network nodes reporting the same tip hash | 
| Node Join Time	    | The length of time from a node startup to adding first block to its DAG | 
| *.comm.consume	    | Counter available at `curl -s http://localhost:40403/metrics` |
| *.comm.produce	    | Counter available at `curl -s http://localhost:40403/metrics` |
| *.consume		    | Timespan available at `curl -s http://localhost:40403/metrics` |
| *.produce		    | Timespan available at `curl -s http://localhost:40403/metrics` |
| *.install		    | Timespan available at `curl -s http://localhost:40403/metrics` |

NOTE: The `/metrics` above are available only once they have been reported at
least once *AND* `prometheus = true` in `rnode.toml`!

## Performance Tests

The following tables define each test series within this test phase. A test
series focuses on observing and documenting the effects of certain conditions
on performance. Each test series is comprised of three separate test cases
which define the variable to be tested. 

### Series 1: Number of Validators

| Variable         | Test Case A | Test Case B | Test Case C |
|------------------|------------:|------------:|------------:|
| Validators       | 300         | 600         | 1000        |
| Static Nodes     | 600         | 600         | 600         |
| Contract         | dupe.rho    | dupe.rho    | dupe.rho    |
| Bandwidth        | 5Mb         | 5Mb         | 5Mb         |
| Propagation Time | 8s          | 8s          | 8s          |


### Series 2: Number of Static Nodes

| Variable        | Test Case A | Test Case B | Test Case C |
|-----------------|------------:|------------:|------------:|
| Validators      | 600         | 600         | 600         |
| Static Nodes    | 300         | 600         | 1000        |
| Contract        | dupe.rho    | dupe.rho    | dupe.rho    |
| Bandwidth       | 5Mb         | 5Mb         | 5Mb         |
| Network Latency | 0ms         | 0ms         | 0ms         |
| Packet Loss     | 0%          | 0%          | 0%          |


### Series 3: Contract

| Variable        | Test Case A | Test Case B   | Test Case C   |
|-----------------|------------:|--------------:|--------------:|
| Validators      | 1000        | 1000          | 1000          |
| Static Nodes    | 1000        | 1000          | 1000          |
| Contract        | dupe.rho    | shortfast.rho | shortslow.rho |
| Bandwidth       | 5Mb         | 5Mb           | 5Mb           |
| Network Latency | 0ms         | 0ms           | 0ms           |
| Packet Loss     | 0.01%       | 0.5%          | 1.0%          |


### Series 4: Bandwidth

| Variable        | Test Case A | Test Case B | Test Case C |
|-----------------|------------:|------------:|------------:|
| Validators      | 1000        | 1000        | 1000        |
| Static Nodes    | 1000        | 1000        | 1000        |
| Contract        | dupe.rho    | dupe.rho    | dupe.rho    |
| Bandwidth       | 5Mb         | 500Mb       | 1G          |
| Network Latency | 0ms         | 0ms         | 0ms         |
| Packet Loss     | 0%          | 0%          | 0%          |


### Series 5: Network Latency

| Variable        | Test Case A | Test Case B | Test Case C |
|-----------------|------------:|------------:|------------:|
| Validators      | 1000        | 1000        | 1000        |
| Static Nodes    | 1000        | 1000        | 1000        |
| Contract        | dupe.rho    | dupe.rho    | dupe.rho    |
| Bandwidth       | 5Mb         | 5Mb         | 5Mb         |
| Network Latency | 25ms        | 50ms        | 100ms       |
| Packet Loss     | 0%          | 0%          | 0%          |


### Series 6: Packet Loss

| Variable        | Test Case A | Test Case B | Test Case C |
|-----------------|------------:|------------:|------------:|
| Validators      | 1000        | 1000        | 1000        |
| Static Nodes    | 1000        | 1000        | 1000        |
| Contract        | dupe.rho    | dupe.rho    | dupe.rho    |
| Bandwidth       | 5Mb         | 5Mb         | 5Mb         |
| Network Latency | 0ms         | 0ms         | 0ms         |
| Packet Loss     | 0.01%       | 0.5%        | 1.0%        |


### Series 7: Stress Test

| Variable        | Test Case A | Test Case B | Test Case C |
|-----------------|------------:|------------:|------------:|
| Validators      | 1000        | 500         | 300         |
| Static Nodes    | 2000        | 2000        | 2000        |
| Contract        | dupe.rho    | dupe.rho    | dupe.rho    |
| Bandwidth       | 5Mb         | 5Mb         | 5Mb         |
| Network Latency | 50ms        | 50ms        | 50ms        |
| Packet Loss     | 0.01%       | 0.01%       | 0.01%       |


## Future plans

 * Test transfers (requires working conflict resolution)
