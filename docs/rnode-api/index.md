# Protocol Documentation
<a name="top"/>

## Table of Contents

- [CasperMessage.proto](#CasperMessage.proto)
    - [ApprovedBlock](#coop.rchain.casper.protocol.ApprovedBlock)
    - [ApprovedBlockCandidate](#coop.rchain.casper.protocol.ApprovedBlockCandidate)
    - [ApprovedBlockRequest](#coop.rchain.casper.protocol.ApprovedBlockRequest)
    - [BlockApproval](#coop.rchain.casper.protocol.BlockApproval)
    - [BlockInfo](#coop.rchain.casper.protocol.BlockInfo)
    - [LightBlockInfo](#coop.rchain.casper.protocol.LightBlockInfo)
    - [BlockMessage](#coop.rchain.casper.protocol.BlockMessage)
    - [BlockQuery](#coop.rchain.casper.protocol.BlockQuery)
    - [BlockQueryResponse](#coop.rchain.casper.protocol.BlockQueryResponse)
    - [BlockRequest](#coop.rchain.casper.protocol.BlockRequest)
    - [BlocksQuery](#coop.rchain.casper.protocol.BlocksQuery)
    - [Body](#coop.rchain.casper.protocol.Body)
    - [Bond](#coop.rchain.casper.protocol.Bond)
    - [CommEvent](#coop.rchain.casper.protocol.CommEvent)
    - [ConsumeEvent](#coop.rchain.casper.protocol.ConsumeEvent)
    - [ContinuationAtNameQuery](#coop.rchain.casper.protocol.ContinuationAtNameQuery)
    - [ContinuationsWithBlockInfo](#coop.rchain.casper.protocol.ContinuationsWithBlockInfo)
    - [DataAtNameQuery](#coop.rchain.casper.protocol.DataAtNameQuery)
    - [DataWithBlockInfo](#coop.rchain.casper.protocol.DataWithBlockInfo)
    - [Deploy](#coop.rchain.casper.protocol.Deploy)
    - [DeployData](#coop.rchain.casper.protocol.DeployData)
    - [DeployServiceResponse](#coop.rchain.casper.protocol.DeployServiceResponse)
    - [Event](#coop.rchain.casper.protocol.Event)
    - [FindDeployInBlockQuery](#coop.rchain.casper.protocol.FindDeployInBlockQuery)
    - [Header](#coop.rchain.casper.protocol.Header)
    - [Justification](#coop.rchain.casper.protocol.Justification)
    - [ListeningNameContinuationResponse](#coop.rchain.casper.protocol.ListeningNameContinuationResponse)
    - [ListeningNameDataResponse](#coop.rchain.casper.protocol.ListeningNameDataResponse)
    - [MaybeBlockMessage](#coop.rchain.casper.protocol.MaybeBlockMessage)
    - [NoApprovedBlockAvailable](#coop.rchain.casper.protocol.NoApprovedBlockAvailable)
    - [ProcessedDeploy](#coop.rchain.casper.protocol.ProcessedDeploy)
    - [ProduceEvent](#coop.rchain.casper.protocol.ProduceEvent)
    - [RChainState](#coop.rchain.casper.protocol.RChainState)
    - [Signature](#coop.rchain.casper.protocol.Signature)
    - [UnapprovedBlock](#coop.rchain.casper.protocol.UnapprovedBlock)
    - [WaitingContinuationInfo](#coop.rchain.casper.protocol.WaitingContinuationInfo)



    - [DeployService](#coop.rchain.casper.protocol.DeployService)


- [RhoTypes.proto](#RhoTypes.proto)
    - [BindPattern](#.BindPattern)
    - [Bundle](#.Bundle)
    - [Connective](#.Connective)
    - [ConnectiveBody](#.ConnectiveBody)
    - [EAnd](#.EAnd)
    - [EDiv](#.EDiv)
    - [EEq](#.EEq)
    - [EGt](#.EGt)
    - [EGte](#.EGte)
    - [EList](#.EList)
    - [ELt](#.ELt)
    - [ELte](#.ELte)
    - [EMap](#.EMap)
    - [EMatches](#.EMatches)
    - [EMethod](#.EMethod)
    - [EMinus](#.EMinus)
    - [EMinusMinus](#.EMinusMinus)
    - [EMult](#.EMult)
    - [ENeg](#.ENeg)
    - [ENeq](#.ENeq)
    - [ENot](#.ENot)
    - [EOr](#.EOr)
    - [EPercentPercent](#.EPercentPercent)
    - [EPlus](#.EPlus)
    - [EPlusPlus](#.EPlusPlus)
    - [ESet](#.ESet)
    - [ETuple](#.ETuple)
    - [EVar](#.EVar)
    - [Expr](#.Expr)
    - [GPrivate](#.GPrivate)
    - [KeyValuePair](#.KeyValuePair)
    - [ListBindPatterns](#.ListBindPatterns)
    - [ListParWithRandom](#.ListParWithRandom)
    - [Match](#.Match)
    - [MatchCase](#.MatchCase)
    - [New](#.New)
    - [PCost](#.PCost)
    - [Par](#.Par)
    - [ParWithRandom](#.ParWithRandom)
    - [Receive](#.Receive)
    - [ReceiveBind](#.ReceiveBind)
    - [Send](#.Send)
    - [TaggedContinuation](#.TaggedContinuation)
    - [Var](#.Var)
    - [Var.WildcardMsg](#.Var.WildcardMsg)
    - [VarRef](#.VarRef)





- [diagnostics.proto](#diagnostics.proto)
    - [GarbageCollector](#coop.rchain.node.model.GarbageCollector)
    - [GarbageCollectors](#coop.rchain.node.model.GarbageCollectors)
    - [Memory](#coop.rchain.node.model.Memory)
    - [MemoryPool](#coop.rchain.node.model.MemoryPool)
    - [MemoryPools](#coop.rchain.node.model.MemoryPools)
    - [MemoryUsage](#coop.rchain.node.model.MemoryUsage)
    - [NodeCoreMetrics](#coop.rchain.node.model.NodeCoreMetrics)
    - [Peer](#coop.rchain.node.model.Peer)
    - [Peers](#coop.rchain.node.model.Peers)
    - [ProcessCpu](#coop.rchain.node.model.ProcessCpu)
    - [Threads](#coop.rchain.node.model.Threads)



    - [Diagnostics](#coop.rchain.node.model.Diagnostics)


- [Scalar Value Types](#scalar-value-types)



<a name="CasperMessage.proto"/>
<p align="right"><a href="#top">Top</a></p>

## CasperMessage.proto
The main API is `DeployService`.


<a name="coop.rchain.casper.protocol.ApprovedBlock"/>

### ApprovedBlock



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| candidate | [ApprovedBlockCandidate](#coop.rchain.casper.protocol.ApprovedBlockCandidate) |  |  |
| sigs | [Signature](#coop.rchain.casper.protocol.Signature) | repeated |  |






<a name="coop.rchain.casper.protocol.ApprovedBlockCandidate"/>

### ApprovedBlockCandidate
---------- Signing Protocol ---------


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| block | [BlockMessage](#coop.rchain.casper.protocol.BlockMessage) |  |  |
| requiredSigs | [int32](#int32) |  |  |






<a name="coop.rchain.casper.protocol.ApprovedBlockRequest"/>

### ApprovedBlockRequest



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| identifier | [string](#string) |  |  |






<a name="coop.rchain.casper.protocol.BlockApproval"/>

### BlockApproval



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| candidate | [ApprovedBlockCandidate](#coop.rchain.casper.protocol.ApprovedBlockCandidate) |  |  |
| sig | [Signature](#coop.rchain.casper.protocol.Signature) |  |  |






<a name="coop.rchain.casper.protocol.BlockInfo"/>

### BlockInfo
For node clients, see BlockMessage for actual Casper protocol Block representation


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| blockHash | [string](#string) |  |  |
| blockSize | [string](#string) |  |  |
| blockNumber | [int64](#int64) |  |  |
| version | [int64](#int64) |  |  |
| deployCount | [int32](#int32) |  |  |
| tupleSpaceHash | [string](#string) |  | Same as postStateHash of BlockMessage |
| tupleSpaceDump | [string](#string) |  |  |
| timestamp | [int64](#int64) |  |  |
| faultTolerance | [float](#float) |  |  |
| mainParentHash | [string](#string) |  |  |
| parentsHashList | [string](#string) | repeated |  |
| sender | [string](#string) |  |  |
| shardId | [string](#string) |  |  |






<a name="coop.rchain.casper.protocol.LightBlockInfo"/>

### LightBlockInfo



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| blockHash | [string](#string) |  |  |
| blockSize | [string](#string) |  |  |
| blockNumber | [int64](#int64) |  |  |
| version | [int64](#int64) |  |  |
| deployCount | [int32](#int32) |  |  |
| tupleSpaceHash | [string](#string) |  | Same as postStateHash of BlockMessage |
| timestamp | [int64](#int64) |  |  |
| faultTolerance | [float](#float) |  |  |
| mainParentHash | [string](#string) |  |  |
| parentsHashList | [string](#string) | repeated |  |
| sender | [string](#string) |  |  |






<a name="coop.rchain.casper.protocol.BlockMessage"/>

### BlockMessage
--------- Core Protocol  --------


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| blockHash | [bytes](#bytes) |  | obtained by hashing the information in the header |
| header | [Header](#coop.rchain.casper.protocol.Header) |  |  |
| body | [Body](#coop.rchain.casper.protocol.Body) |  |  |
| justifications | [Justification](#coop.rchain.casper.protocol.Justification) | repeated | map of all validators to latest blocks based on current view |
| sender | [bytes](#bytes) |  | public key of the validator that created the block |
| seqNum | [int32](#int32) |  | number of blocks created by the validator |
| sig | [bytes](#bytes) |  | signature generated by signing `hash(hash(justification) concat blockHash)`. |
| sigAlgorithm | [string](#string) |  | name of the algorithm used to sign |
| shardId | [string](#string) |  | identifier of the shard where the block was created |
| extraBytes | [bytes](#bytes) |  |  |






<a name="coop.rchain.casper.protocol.BlockQuery"/>

### BlockQuery



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| hash | [string](#string) |  |  |






<a name="coop.rchain.casper.protocol.BlockQueryResponse"/>

### BlockQueryResponse



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| status | [string](#string) |  |  |
| blockInfo | [BlockInfo](#coop.rchain.casper.protocol.BlockInfo) |  |  |






<a name="coop.rchain.casper.protocol.BlockRequest"/>

### BlockRequest



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| base16Hash | [string](#string) |  | Fields are redundant to allow for validity check. Such a check can definitively distinguish this from other message types with similar serializations. |
| hash | [bytes](#bytes) |  |  |






<a name="coop.rchain.casper.protocol.BlocksQuery"/>

### BlocksQuery



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| depth | [int32](#int32) |  |  |






<a name="coop.rchain.casper.protocol.Body"/>

### Body



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| state | [RChainState](#coop.rchain.casper.protocol.RChainState) |  |  |
| deploys | [ProcessedDeploy](#coop.rchain.casper.protocol.ProcessedDeploy) | repeated |  |
| extraBytes | [bytes](#bytes) |  |  |






<a name="coop.rchain.casper.protocol.Bond"/>

### Bond



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| validator | [bytes](#bytes) |  |  |
| stake | [int64](#int64) |  |  |






<a name="coop.rchain.casper.protocol.CommEvent"/>

### CommEvent



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| consume | [ConsumeEvent](#coop.rchain.casper.protocol.ConsumeEvent) |  |  |
| produces | [ProduceEvent](#coop.rchain.casper.protocol.ProduceEvent) | repeated |  |






<a name="coop.rchain.casper.protocol.ConsumeEvent"/>

### ConsumeEvent



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| channelsHashes | [bytes](#bytes) | repeated |  |
| hash | [bytes](#bytes) |  |  |
| sequenceNumber | [int32](#int32) |  |  |






<a name="coop.rchain.casper.protocol.ContinuationAtNameQuery"/>

### ContinuationAtNameQuery



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| depth | [int32](#int32) |  |  |
| names | [Par](#Par) | repeated |  |






<a name="coop.rchain.casper.protocol.ContinuationsWithBlockInfo"/>

### ContinuationsWithBlockInfo



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| postBlockContinuations | [WaitingContinuationInfo](#coop.rchain.casper.protocol.WaitingContinuationInfo) | repeated |  |
| block | [LightBlockInfo](#coop.rchain.casper.protocol.LightBlockInfo) |  |  |






<a name="coop.rchain.casper.protocol.DataAtNameQuery"/>

### DataAtNameQuery



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| depth | [int32](#int32) |  |  |
| name | [Par](#Par) |  |  |






<a name="coop.rchain.casper.protocol.DataWithBlockInfo"/>

### DataWithBlockInfo



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| postBlockData | [Par](#Par) | repeated |  |
| block | [LightBlockInfo](#coop.rchain.casper.protocol.LightBlockInfo) |  |  |






<a name="coop.rchain.casper.protocol.Deploy"/>

### Deploy



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| term | [Par](#Par) |  | rholang term to deploy |
| raw | [DeployData](#coop.rchain.casper.protocol.DeployData) |  |  |






<a name="coop.rchain.casper.protocol.DeployData"/>

### DeployData
Note: deploys are uniquely keyed by `user`, `timestamp`.

**TODO**: details of signatures and payment. See RHOL-781


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| user | [bytes](#bytes) |  | public key |
| term | [string](#string) |  | rholang source code to deploy (will be parsed into `Par`) |
| timestamp | [int64](#int64) |  | millisecond timestamp |
| sig | [bytes](#bytes) |  | signature of (hash(term) &#43; timestamp) using private key |
| sigAlgorithm | [string](#string) |  | name of the algorithm used to sign |
| from | [string](#string) |  | wallet address which will be used to pay for the deployment |
| phloPrice | [int64](#int64) |  | phlo price |
| phloLimit | [int64](#int64) |  | phlo limit for the deployment |
| nonce | [int32](#int32) |  | nonce for transaction made against `from` wallet |






<a name="coop.rchain.casper.protocol.DeployServiceResponse"/>

### DeployServiceResponse



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| success | [bool](#bool) |  |  |
| message | [string](#string) |  |  |






<a name="coop.rchain.casper.protocol.Event"/>

### Event



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| produce | [ProduceEvent](#coop.rchain.casper.protocol.ProduceEvent) |  |  |
| consume | [ConsumeEvent](#coop.rchain.casper.protocol.ConsumeEvent) |  |  |
| comm | [CommEvent](#coop.rchain.casper.protocol.CommEvent) |  |  |






<a name="coop.rchain.casper.protocol.FindDeployInBlockQuery"/>

### FindDeployInBlockQuery



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| user | [bytes](#bytes) |  |  |
| timestamp | [int64](#int64) |  |  |






<a name="coop.rchain.casper.protocol.Header"/>

### Header



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| parentsHashList | [bytes](#bytes) | repeated | list of parent block hashes |
| postStateHash | [bytes](#bytes) |  |  |
| deploysHash | [bytes](#bytes) |  |  |
| timestamp | [int64](#int64) |  |  |
| version | [int64](#int64) |  |  |
| deployCount | [int32](#int32) |  |  |
| extraBytes | [bytes](#bytes) |  |  |






<a name="coop.rchain.casper.protocol.Justification"/>

### Justification



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| validator | [bytes](#bytes) |  |  |
| latestBlockHash | [bytes](#bytes) |  |  |






<a name="coop.rchain.casper.protocol.ListeningNameContinuationResponse"/>

### ListeningNameContinuationResponse



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| status | [string](#string) |  |  |
| blockResults | [ContinuationsWithBlockInfo](#coop.rchain.casper.protocol.ContinuationsWithBlockInfo) | repeated |  |
| length | [int32](#int32) |  |  |






<a name="coop.rchain.casper.protocol.ListeningNameDataResponse"/>

### ListeningNameDataResponse



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| status | [string](#string) |  |  |
| blockResults | [DataWithBlockInfo](#coop.rchain.casper.protocol.DataWithBlockInfo) | repeated |  |
| length | [int32](#int32) |  |  |






<a name="coop.rchain.casper.protocol.MaybeBlockMessage"/>

### MaybeBlockMessage



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| block | [BlockMessage](#coop.rchain.casper.protocol.BlockMessage) |  |  |






<a name="coop.rchain.casper.protocol.NoApprovedBlockAvailable"/>

### NoApprovedBlockAvailable



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| identifier | [string](#string) |  |  |
| nodeIdentifer | [string](#string) |  |  |

  




<a name="coop.rchain.casper.protocol.ProcessedDeploy"/>

### ProcessedDeploy



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| deploy | [Deploy](#coop.rchain.casper.protocol.Deploy) |  |  |
| cost | [PCost](#PCost) |  |  |
| log | [Event](#coop.rchain.casper.protocol.Event) | repeated | the new terms and comm. rule reductions from this deploy |
| errored | [bool](#bool) |  | true if deploy encountered a user error |






<a name="coop.rchain.casper.protocol.ProduceEvent"/>

### ProduceEvent



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| channelsHash | [bytes](#bytes) |  |  |
| hash | [bytes](#bytes) |  |  |
| sequenceNumber | [int32](#int32) |  |  |






<a name="coop.rchain.casper.protocol.RChainState"/>

### RChainState



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| preStateHash | [bytes](#bytes) |  | hash of the tuplespace contents before new deploys |
| postStateHash | [bytes](#bytes) |  | hash of the tuplespace contents after new deploys |
| bonds | [Bond](#coop.rchain.casper.protocol.Bond) | repeated | Internals of what will be the &#34;blessed&#34; PoS contract (which will be part of the tuplespace in the real implementation). |
| blockNumber | [int64](#int64) |  |  |






<a name="coop.rchain.casper.protocol.Signature"/>

### Signature



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| publicKey | [bytes](#bytes) |  |  |
| algorithm | [string](#string) |  |  |
| sig | [bytes](#bytes) |  |  |






<a name="coop.rchain.casper.protocol.UnapprovedBlock"/>

### UnapprovedBlock



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| candidate | [ApprovedBlockCandidate](#coop.rchain.casper.protocol.ApprovedBlockCandidate) |  |  |
| timestamp | [int64](#int64) |  |  |
| duration | [int64](#int64) |  |  |






<a name="coop.rchain.casper.protocol.WaitingContinuationInfo"/>

### WaitingContinuationInfo



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| postBlockPatterns | [BindPattern](#BindPattern) | repeated |  |
| postBlockContinuation | [Par](#Par) |  |  |












<a name="coop.rchain.casper.protocol.DeployService"/>

### DeployService
Use `DoDeploy` to queue deployments of Rholang code and then
`createBlock` to make a new block with the results of running them
all.

To get results back, use `listenForDataAtName`.

| Method Name | Request Type | Response Type | Description |
| ----------- | ------------ | ------------- | ------------|
| DoDeploy | [DeployData](#coop.rchain.casper.protocol.DeployData) | [DeployServiceResponse](#coop.rchain.casper.protocol.DeployData) | Queue deployment of Rholang code (or fail to parse). |
| createBlock | [.google.protobuf.Empty](#google.protobuf.Empty) | [DeployServiceResponse](#google.protobuf.Empty) | Add a block including all pending deploys. |
| getBlock | [BlockQuery](#coop.rchain.casper.protocol.BlockQuery) | [BlockQueryResponse](#coop.rchain.casper.protocol.BlockQuery) | Get details about a particular block. |
| showMainChain | [BlocksQuery](#coop.rchain.casper.protocol.BlocksQuery) | [LightBlockInfo](#coop.rchain.casper.protocol.BlocksQuery) |  |
| getBlocks | [BlocksQuery](#coop.rchain.casper.protocol.BlocksQuery) | [LightBlockInfo](#coop.rchain.casper.protocol.BlocksQuery) | Get a summary of blocks on the blockchain. |
| listenForDataAtName | [DataAtNameQuery](#coop.rchain.casper.protocol.DataAtNameQuery) | [ListeningNameDataResponse](#coop.rchain.casper.protocol.DataAtNameQuery) | Find data sent to a name. |
| listenForContinuationAtName | [ContinuationAtNameQuery](#coop.rchain.casper.protocol.ContinuationAtNameQuery) | [ListeningNameContinuationResponse](#coop.rchain.casper.protocol.ContinuationAtNameQuery) | Find processes receiving on a name. |
| findBlockWithDeploy | [FindDeployInBlockQuery](#coop.rchain.casper.protocol.FindDeployInBlockQuery) | [BlockQueryResponse](#coop.rchain.casper.protocol.FindDeployInBlockQuery) | Find block from a deploy. |





<a name="RhoTypes.proto"/>
<p align="right"><a href="#top">Top</a></p>

## RhoTypes.proto
Rholang Term Structure

The top level is `Par`.


<a name=".BindPattern"/>

### BindPattern



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| patterns | [Par](#Par) | repeated |  |
| remainder | [Var](#Var) |  |  |
| freeCount | [int32](#int32) |  |  |






<a name=".Bundle"/>

### Bundle
Nothing can be received from a (quoted) bundle with `readFlag = false`.
Likeise nothing can be sent to a (quoted) bundle with `writeFlag = false`.

If both flags are set to false, bundle allows only for equivalance check.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| body | [Par](#Par) |  |  |
| writeFlag | [bool](#bool) |  | flag indicating whether bundle is writeable |
| readFlag | [bool](#bool) |  | flag indicating whether bundle is readable |






<a name=".Connective"/>

### Connective



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| conn_and_body | [ConnectiveBody](#ConnectiveBody) |  |  |
| conn_or_body | [ConnectiveBody](#ConnectiveBody) |  |  |
| conn_not_body | [Par](#Par) |  |  |
| var_ref_body | [VarRef](#VarRef) |  |  |
| conn_bool | [bool](#bool) |  |  |
| conn_int | [bool](#bool) |  |  |
| conn_string | [bool](#bool) |  |  |
| conn_uri | [bool](#bool) |  |  |
| conn_byte_array | [bool](#bool) |  |  |






<a name=".ConnectiveBody"/>

### ConnectiveBody



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ps | [Par](#Par) | repeated |  |






<a name=".EAnd"/>

### EAnd



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| p1 | [Par](#Par) |  |  |
| p2 | [Par](#Par) |  |  |






<a name=".EDiv"/>

### EDiv



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| p1 | [Par](#Par) |  |  |
| p2 | [Par](#Par) |  |  |






<a name=".EEq"/>

### EEq



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| p1 | [Par](#Par) |  |  |
| p2 | [Par](#Par) |  |  |






<a name=".EGt"/>

### EGt



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| p1 | [Par](#Par) |  |  |
| p2 | [Par](#Par) |  |  |






<a name=".EGte"/>

### EGte



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| p1 | [Par](#Par) |  |  |
| p2 | [Par](#Par) |  |  |






<a name=".EList"/>

### EList



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ps | [Par](#Par) | repeated |  |
| locallyFree | [bytes](#bytes) |  |  |
| connective_used | [bool](#bool) |  |  |
| remainder | [Var](#Var) |  |  |






<a name=".ELt"/>

### ELt



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| p1 | [Par](#Par) |  |  |
| p2 | [Par](#Par) |  |  |






<a name=".ELte"/>

### ELte



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| p1 | [Par](#Par) |  |  |
| p2 | [Par](#Par) |  |  |






<a name=".EMap"/>

### EMap



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| kvs | [KeyValuePair](#KeyValuePair) | repeated |  |
| locallyFree | [bytes](#bytes) |  |  |
| connective_used | [bool](#bool) |  |  |
| remainder | [Var](#Var) |  |  |






<a name=".EMatches"/>

### EMatches



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| target | [Par](#Par) |  |  |
| pattern | [Par](#Par) |  |  |






<a name=".EMethod"/>

### EMethod
`target.method(arguments)`


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| methodName | [string](#string) |  |  |
| target | [Par](#Par) |  |  |
| arguments | [Par](#Par) | repeated |  |
| locallyFree | [bytes](#bytes) |  |  |
| connective_used | [bool](#bool) |  |  |






<a name=".EMinus"/>

### EMinus



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| p1 | [Par](#Par) |  |  |
| p2 | [Par](#Par) |  |  |






<a name=".EMinusMinus"/>

### EMinusMinus
Set difference


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| p1 | [Par](#Par) |  |  |
| p2 | [Par](#Par) |  |  |






<a name=".EMult"/>

### EMult



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| p1 | [Par](#Par) |  |  |
| p2 | [Par](#Par) |  |  |






<a name=".ENeg"/>

### ENeg



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| p | [Par](#Par) |  |  |






<a name=".ENeq"/>

### ENeq



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| p1 | [Par](#Par) |  |  |
| p2 | [Par](#Par) |  |  |






<a name=".ENot"/>

### ENot



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| p | [Par](#Par) |  |  |






<a name=".EOr"/>

### EOr



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| p1 | [Par](#Par) |  |  |
| p2 | [Par](#Par) |  |  |






<a name=".EPercentPercent"/>

### EPercentPercent
String interpolation

`&#34;Hello, {name}&#34; %% {&#34;name&#34;: &#34;Bob&#34;}` denotes `&#34;Hello, Bob&#34;`


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| p1 | [Par](#Par) |  |  |
| p2 | [Par](#Par) |  |  |






<a name=".EPlus"/>

### EPlus



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| p1 | [Par](#Par) |  |  |
| p2 | [Par](#Par) |  |  |






<a name=".EPlusPlus"/>

### EPlusPlus
Concatenation


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| p1 | [Par](#Par) |  |  |
| p2 | [Par](#Par) |  |  |






<a name=".ESet"/>

### ESet



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ps | [Par](#Par) | repeated |  |
| locallyFree | [bytes](#bytes) |  |  |
| connective_used | [bool](#bool) |  |  |
| remainder | [Var](#Var) |  |  |






<a name=".ETuple"/>

### ETuple



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| ps | [Par](#Par) | repeated |  |
| locallyFree | [bytes](#bytes) |  |  |
| connective_used | [bool](#bool) |  |  |






<a name=".EVar"/>

### EVar
A variable used as a var should be bound in a process context, not a name
context. For example:
`for (@x &lt;- c1; @y &lt;- c2) { z!(x &#43; y) }` is fine, but
`for (x &lt;- c1; y &lt;- c2) { z!(x &#43; y) }` should raise an error.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| v | [Var](#Var) |  |  |






<a name=".Expr"/>

### Expr
Any process may be an operand to an expression.
Only processes equivalent to a ground process of compatible type will reduce.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| g_bool | [bool](#bool) |  |  |
| g_int | [sint64](#sint64) |  |  |
| g_string | [string](#string) |  |  |
| g_uri | [string](#string) |  |  |
| g_byte_array | [bytes](#bytes) |  |  |
| e_not_body | [ENot](#ENot) |  |  |
| e_neg_body | [ENeg](#ENeg) |  |  |
| e_mult_body | [EMult](#EMult) |  |  |
| e_div_body | [EDiv](#EDiv) |  |  |
| e_plus_body | [EPlus](#EPlus) |  |  |
| e_minus_body | [EMinus](#EMinus) |  |  |
| e_lt_body | [ELt](#ELt) |  |  |
| e_lte_body | [ELte](#ELte) |  |  |
| e_gt_body | [EGt](#EGt) |  |  |
| e_gte_body | [EGte](#EGte) |  |  |
| e_eq_body | [EEq](#EEq) |  |  |
| e_neq_body | [ENeq](#ENeq) |  |  |
| e_and_body | [EAnd](#EAnd) |  |  |
| e_or_body | [EOr](#EOr) |  |  |
| e_var_body | [EVar](#EVar) |  |  |
| e_list_body | [EList](#EList) |  |  |
| e_tuple_body | [ETuple](#ETuple) |  |  |
| e_set_body | [ESet](#ESet) |  |  |
| e_map_body | [EMap](#EMap) |  |  |
| e_method_body | [EMethod](#EMethod) |  |  |
| e_matches_body | [EMatches](#EMatches) |  |  |
| e_percent_percent_body | [EPercentPercent](#EPercentPercent) |  | string interpolation |
| e_plus_plus_body | [EPlusPlus](#EPlusPlus) |  | concatenation |
| e_minus_minus_body | [EMinusMinus](#EMinusMinus) |  | set difference |






<a name=".GPrivate"/>

### GPrivate
Unforgeable names resulting from `new x { ... }`
These should only occur as the program is being evaluated. There is no way in
the grammar to construct them.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| id | [bytes](#bytes) |  |  |






<a name=".KeyValuePair"/>

### KeyValuePair



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| key | [Par](#Par) |  |  |
| value | [Par](#Par) |  |  |






<a name=".ListBindPatterns"/>

### ListBindPatterns



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| patterns | [BindPattern](#BindPattern) | repeated |  |






<a name=".ListParWithRandom"/>

### ListParWithRandom



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| pars | [Par](#Par) | repeated |  |
| randomState | [bytes](#bytes) |  |  |


<a name=".Match"/>

### Match



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| target | [Par](#Par) |  |  |
| cases | [MatchCase](#MatchCase) | repeated |  |
| locallyFree | [bytes](#bytes) |  |  |
| connective_used | [bool](#bool) |  |  |






<a name=".MatchCase"/>

### MatchCase



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| pattern | [Par](#Par) |  |  |
| source | [Par](#Par) |  |  |
| freeCount | [int32](#int32) |  |  |






<a name=".New"/>

### New
Number of variables bound in the new statement.
For normalized form, p should not contain solely another new.
Also for normalized form, the first use should be level&#43;0, next use level&#43;1
up to level&#43;count for the last used variable.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| bindCount | [sint32](#sint32) |  | Includes any uris listed below. This makes it easier to substitute or walk a term. |
| p | [Par](#Par) |  |  |
| uri | [string](#string) | repeated | For normalization, uri-referenced variables come at the end, and in lexicographical order. |
| locallyFree | [bytes](#bytes) |  |  |






<a name=".PCost"/>

### PCost
A measure of processing cost: a number of iterations and the sum
cost of the iterations.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| iterations | [int32](#int32) |  |  |
| cost | [uint64](#uint64) |  |  |






<a name=".Par"/>

### Par
Rholang process

For example, `@0!(1) | @2!(3) | for(x &lt;- @0) { Nil }` has two sends
and one receive.

The Nil process is a `Par` with no sends, receives, etc.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| sends | [Send](#Send) | repeated |  |
| receives | [Receive](#Receive) | repeated |  |
| news | [New](#New) | repeated |  |
| exprs | [Expr](#Expr) | repeated |  |
| matches | [Match](#Match) | repeated |  |
| ids | [GPrivate](#GPrivate) | repeated | unforgeable names |
| bundles | [Bundle](#Bundle) | repeated |  |
| connectives | [Connective](#Connective) | repeated |  |
| locallyFree | [bytes](#bytes) |  |  |
| connective_used | [bool](#bool) |  |  |






<a name=".ParWithRandom"/>

### ParWithRandom
Rholang code along with the state of a split random number
generator for generating new unforgeable names.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| body | [Par](#Par) |  |  |
| randomState | [bytes](#bytes) |  |  |






<a name=".Receive"/>

### Receive
A receive is written `for(binds) { body }`
i.e. `for(patterns &lt;- source) { body }`
or for a persistent recieve: `for(patterns &lt;= source) { body }`.

It&#39;s an error for free Variable to occur more than once in a pattern.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| binds | [ReceiveBind](#ReceiveBind) | repeated |  |
| body | [Par](#Par) |  |  |
| persistent | [bool](#bool) |  |  |
| bindCount | [int32](#int32) |  |  |
| locallyFree | [bytes](#bytes) |  |  |
| connective_used | [bool](#bool) |  |  |






<a name=".ReceiveBind"/>

### ReceiveBind



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| patterns | [Par](#Par) | repeated |  |
| source | [Par](#Par) |  |  |
| remainder | [Var](#Var) |  |  |
| freeCount | [int32](#int32) |  |  |






<a name=".Send"/>

### Send
A send is written `chan!(data)` or `chan!!(data)` for a persistent send.

Upon send, all free variables in data are substituted with their values.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| chan | [Par](#Par) |  |  |
| data | [Par](#Par) | repeated |  |
| persistent | [bool](#bool) |  |  |
| locallyFree | [bytes](#bytes) |  |  |
| connective_used | [bool](#bool) |  |  |






<a name=".TaggedContinuation"/>

### TaggedContinuation
Either rholang code or code built in to the interpreter.


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| par_body | [ParWithRandom](#ParWithRandom) |  |  |
| scala_body_ref | [int64](#int64) |  |  |






<a name=".Var"/>

### Var
While we use vars in both positions, when producing the normalized
representation we need a discipline to track whether a var is a name or a
process.
These are DeBruijn levels


| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| bound_var | [sint32](#sint32) |  |  |
| free_var | [sint32](#sint32) |  |  |
| wildcard | [Var.WildcardMsg](#Var.WildcardMsg) |  |  |






<a name=".Var.WildcardMsg"/>

### Var.WildcardMsg







<a name=".VarRef"/>

### VarRef



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| index | [sint32](#sint32) |  |  |
| depth | [sint32](#sint32) |  |  |















<a name="diagnostics.proto"/>
<p align="right"><a href="#top">Top</a></p>

## diagnostics.proto



<a name="coop.rchain.node.model.GarbageCollector"/>

### GarbageCollector



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| name | [string](#string) |  | name representing this memory manager |
| totalCollections | [int64](#int64) |  | total number of collections that have occurred |
| totalCollectionTime | [int64](#int64) |  | accumulated collection time in ms |
| startTime | [google.protobuf.Int64Value](#google.protobuf.Int64Value) |  | start time of last GC since the JVM was started in ms |
| endTime | [google.protobuf.Int64Value](#google.protobuf.Int64Value) |  | end time of last GC since the JVM was started |
| duration | [google.protobuf.Int64Value](#google.protobuf.Int64Value) |  | elapsed time of last GC in ms |






<a name="coop.rchain.node.model.GarbageCollectors"/>

### GarbageCollectors



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| garbageCollectors | [GarbageCollector](#coop.rchain.node.model.GarbageCollector) | repeated |  |






<a name="coop.rchain.node.model.Memory"/>

### Memory



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| committed | [int64](#int64) |  | amount of memory committed in bytes |
| init | [int64](#int64) |  | amount of memory that the JVM initially requested in bytes |
| max | [google.protobuf.Int64Value](#google.protobuf.Int64Value) |  | maximum amount of memory possible in bytes |
| used | [int64](#int64) |  | amount of used memory in bytes |






<a name="coop.rchain.node.model.MemoryPool"/>

### MemoryPool



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| name | [string](#string) |  | name representing this memory pool |
| poolType | [string](#string) |  | type of this memory pool |
| usage | [Memory](#coop.rchain.node.model.Memory) |  | memory pool usage |
| peakUsage | [Memory](#coop.rchain.node.model.Memory) |  | peak memory usage |






<a name="coop.rchain.node.model.MemoryPools"/>

### MemoryPools



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| memoryPools | [MemoryPool](#coop.rchain.node.model.MemoryPool) | repeated |  |






<a name="coop.rchain.node.model.MemoryUsage"/>

### MemoryUsage



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| heap | [Memory](#coop.rchain.node.model.Memory) |  | memory currently used by the Java heap for object allocation |
| nonHeap | [Memory](#coop.rchain.node.model.Memory) |  | current non-heap memory usage of the JVM |






<a name="coop.rchain.node.model.NodeCoreMetrics"/>

### NodeCoreMetrics



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| pingReceiverCount | [int64](#int64) |  |  |
| lookupReceiverCount | [int64](#int64) |  |  |
| disconnectReceiverCount | [int64](#int64) |  |  |
| connects | [int64](#int64) |  |  |
| p2pEncryptionHandshakeReceiverCount | [int64](#int64) |  |  |
| p2pProtocolHandshakeReceiverCount | [int64](#int64) |  |  |
| peers | [int64](#int64) |  |  |
| from | [int64](#int64) |  |  |
| to | [int64](#int64) |  |  |






<a name="coop.rchain.node.model.Peer"/>

### Peer



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| host | [string](#string) |  |  |
| port | [int32](#int32) |  |  |
| key | [bytes](#bytes) |  |  |






<a name="coop.rchain.node.model.Peers"/>

### Peers



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| peers | [Peer](#coop.rchain.node.model.Peer) | repeated |  |






<a name="coop.rchain.node.model.ProcessCpu"/>

### ProcessCpu



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| load | [google.protobuf.DoubleValue](#google.protobuf.DoubleValue) |  | amount of CPU load, as a value between 0.0 and 1.0 |
| time | [google.protobuf.Int64Value](#google.protobuf.Int64Value) |  | CPU time used by the process on which the JVM is running in nanoseconds |






<a name="coop.rchain.node.model.Threads"/>

### Threads



| Field | Type | Label | Description |
| ----- | ---- | ----- | ----------- |
| threadCount | [int32](#int32) |  | current number of live threads including both daemon and non-daemon threads |
| daemonThreadCount | [int32](#int32) |  | current number of live daemon threads. |
| peakThreadCount | [int32](#int32) |  | peak live thread count since the JVM started |
| totalStartedThreadCount | [int64](#int64) |  | total number of threads created and also started since the JVM started |












<a name="coop.rchain.node.model.Diagnostics"/>

### Diagnostics


| Method Name | Request Type | Response Type | Description |
| ----------- | ------------ | ------------- | ------------|
| ListPeers | [.google.protobuf.Empty](#google.protobuf.Empty) | [Peers](#google.protobuf.Empty) |  |
| ListDiscoveredPeers | [.google.protobuf.Empty](#google.protobuf.Empty) | [Peers](#google.protobuf.Empty) |  |
| GetProcessCpu | [.google.protobuf.Empty](#google.protobuf.Empty) | [ProcessCpu](#google.protobuf.Empty) |  |
| GetMemoryUsage | [.google.protobuf.Empty](#google.protobuf.Empty) | [MemoryUsage](#google.protobuf.Empty) |  |
| GetGarbageCollectors | [.google.protobuf.Empty](#google.protobuf.Empty) | [GarbageCollectors](#google.protobuf.Empty) |  |
| GetMemoryPools | [.google.protobuf.Empty](#google.protobuf.Empty) | [MemoryPools](#google.protobuf.Empty) |  |
| GetThreads | [.google.protobuf.Empty](#google.protobuf.Empty) | [Threads](#google.protobuf.Empty) |  |
| GetNodeCoreMetrics | [.google.protobuf.Empty](#google.protobuf.Empty) | [NodeCoreMetrics](#google.protobuf.Empty) |  |





## Scalar Value Types

| .proto Type | Notes | C++ Type | Java Type | Python Type |
| ----------- | ----- | -------- | --------- | ----------- |
| <a name="double" /> double |  | double | double | float |
| <a name="float" /> float |  | float | float | float |
| <a name="int32" /> int32 | Uses variable-length encoding. Inefficient for encoding negative numbers – if your field is likely to have negative values, use sint32 instead. | int32 | int | int |
| <a name="int64" /> int64 | Uses variable-length encoding. Inefficient for encoding negative numbers – if your field is likely to have negative values, use sint64 instead. | int64 | long | int/long |
| <a name="uint32" /> uint32 | Uses variable-length encoding. | uint32 | int | int/long |
| <a name="uint64" /> uint64 | Uses variable-length encoding. | uint64 | long | int/long |
| <a name="sint32" /> sint32 | Uses variable-length encoding. Signed int value. These more efficiently encode negative numbers than regular int32s. | int32 | int | int |
| <a name="sint64" /> sint64 | Uses variable-length encoding. Signed int value. These more efficiently encode negative numbers than regular int64s. | int64 | long | int/long |
| <a name="fixed32" /> fixed32 | Always four bytes. More efficient than uint32 if values are often greater than 2^28. | uint32 | int | int |
| <a name="fixed64" /> fixed64 | Always eight bytes. More efficient than uint64 if values are often greater than 2^56. | uint64 | long | int/long |
| <a name="sfixed32" /> sfixed32 | Always four bytes. | int32 | int | int |
| <a name="sfixed64" /> sfixed64 | Always eight bytes. | int64 | long | int/long |
| <a name="bool" /> bool |  | bool | boolean | boolean |
| <a name="string" /> string | A string must always contain UTF-8 encoded or 7-bit ASCII text. | string | String | str/unicode |
| <a name="bytes" /> bytes | May contain any arbitrary sequence of bytes. | string | ByteString | str |
