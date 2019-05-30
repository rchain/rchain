---------------------------- MODULE casper -----------------------------
EXTENDS Integers, Sequences, util
CONSTANT Nodes
VARIABLES
nodes,
nStatus,
nInMsg,
nOutStreamMsg,
nOutMsg

vars == << nodes, nStatus, nInMsg, nOutStreamMsg, nOutMsg >>

-----------------------------------------------------------------------------

Pack(_msg, _from, _to) == [msg |-> _msg, from |-> _from, to |-> _to]

StreamMsg(_msg, _from, _to) ==
  nOutStreamMsg' = [nOutStreamMsg EXCEPT ![_from] = Append(nOutStreamMsg[_from], Pack(_msg, _from, _to))]

BroadcastStream(_msg, _from, _tos) ==
  LET CreatePacket(_to) == Pack(_msg, _from, _to)
  IN nOutStreamMsg' = [nOutStreamMsg EXCEPT ![_from] = nOutStreamMsg[_from] \o MapSeq(CreatePacket, SetToSeq(_tos))]

TransferStreamMsg(msgType) == \E n \in nodes :
  /\ nOutStreamMsg[n] # << >>
  /\ Head(nOutStreamMsg[n]).msg.type = msgType
  /\ nOutStreamMsg' = [nOutStreamMsg EXCEPT ![n] = Tail(nOutStreamMsg[n])]
  /\ nInMsg' = [nInMsg EXCEPT ![Head(nOutStreamMsg[n]).to] = Append(nInMsg[Head(nOutStreamMsg[n]).to], Head(nOutStreamMsg[n]))]
  /\ UNCHANGED << nodes, nStatus, nOutMsg >>

LooseStreamMsg == \E n \in nodes :
  /\ nOutStreamMsg[n] # << >>
  /\ nOutStreamMsg' = [nOutStreamMsg EXCEPT ![n] = Tail(nOutStreamMsg[n])]
  /\ UNCHANGED << nodes, nStatus, nInMsg, nOutMsg >>

SendingStatus == {"init", "success", "failed"}

SentMsgStatus(_status, _packet) ==
  [status |-> _status, packet |-> _packet]

SendMsg(_msg, _from, _to) ==
  nOutMsg' = [nOutMsg EXCEPT ![_from] = Some(SentMsgStatus("init", Pack(_msg, _from, _to)))]

TransferMsg == \E n \in nodes :
  /\ nOutMsg[n] # None
  /\ nOutMsg[n].some.status = "init"
  /\ nOutMsg' = [nOutMsg EXCEPT ![n].some.status = "success"]
  /\ nInMsg' = [nInMsg EXCEPT ![nOutMsg[n].some.packet.to] = Append(nInMsg[nOutMsg[n].some.packet.to], nOutMsg[n].some.packet)]
  /\ UNCHANGED << nodes, nStatus, nOutStreamMsg >>

LooseMsg == \E n \in nodes :
  /\ nOutMsg[n] # None
  /\ nOutMsg[n].some.status = "init"
  /\ nOutMsg' = [nOutMsg EXCEPT ![n].some.status = "failed"]
  /\ UNCHANGED << nodes, nStatus, nInMsg, nOutStreamMsg >>
 
-----------------------------------------------------------------------------

NextInMsgIs(n, msgType) ==
  /\ Len(nInMsg[n]) > 0
  /\ Head(nInMsg[n]).msg.type = msgType

PopNextInMsg(n) ==
  nInMsg' = [nInMsg EXCEPT ![n] = Tail(nInMsg[n])]

Handling(n, status, msgType) ==
  /\ nStatus[n] = status
  /\ NextInMsgIs(n, msgType)
  /\ PopNextInMsg(n)

DoNotHandle(status, msgType) == \E n \in nodes :
  /\ Handling(n, status, msgType)
  /\ UNCHANGED << nodes, nOutMsg, nOutStreamMsg, nStatus >>

Sender(n) ==
  Head(nInMsg[n]).from

TransitionTo(n, status) ==
  nStatus' = [nStatus EXCEPT ![n] = status]

-----------------------------------------------------------------------------

MessageType == {"UnapprovedBlock", "BlockApproval", "ApprovedBlockRequest", "ApprovedBlock"}
PossibleStatuses == {"new", "init", "running", "genesis_validator", "ceremony_master"}

Messages == [type : MessageType]
Packets == [msg : Messages, from : nodes, to : nodes]

NewMessage(_type) == [type |-> _type]
NewUnapprovedBlock == NewMessage("UnapprovedBlock")
NewBlockApproval == NewMessage("BlockApproval")
NewApprovedBlockRequest == NewMessage("ApprovedBlockRequest")
NewApprovedBlock == NewMessage("ApprovedBlock")

-----------------------------------------------------------------------------

HoldsMessageType(msgQueue) == SeqToSet(msgQueue) \subseteq Packets

TypeOK ==
  /\ \A n \in nodes : nStatus[n] \in PossibleStatuses
  /\ \A n \in nodes : HoldsMessageType(nInMsg[n]) 
  /\ \A n \in nodes : HoldsMessageType(nOutStreamMsg[n])
  /\ \A n \in nodes : nOutMsg[n] \in Option([status : SendingStatus, packet : Packets])

-----------------------------------------------------------------------------

Bootstrap(n) ==
  LET node == (CHOOSE cn \in Nodes : cn.id = n)
  IN node.b

LaunchFromNewToInit == \E n \in nodes :
  /\ nStatus[n] = "new"
  /\ nOutMsg[n] = None
  /\ TransitionTo(n, "init")  
  /\ SendMsg(NewApprovedBlockRequest, n, Bootstrap(n))
  /\ UNCHANGED << nodes, nInMsg, nOutStreamMsg >>

SuccessFromNewToInit == \E n \in nodes :
  /\ nStatus[n] = "init"
  /\ nOutMsg[n] = Some(SentMsgStatus("success", Pack(NewApprovedBlockRequest, n, Bootstrap(n))))
  /\ nOutMsg' = [nOutMsg EXCEPT ![n] = None]
  /\ UNCHANGED << nodes, nInMsg, nOutStreamMsg, nStatus >>

FailedFromNewToInit == \E n \in nodes :
  /\ nStatus[n] = "init"
  /\ nOutMsg[n] = Some(SentMsgStatus("failed", Pack(NewApprovedBlockRequest, n, Bootstrap(n))))
  /\ SendMsg(NewApprovedBlockRequest, n, Bootstrap(n))  
  /\ UNCHANGED << nodes, nInMsg, nOutStreamMsg, nStatus >>

ResendWhileInit == \E n \in nodes :
  /\ nStatus[n] = "init"
  /\ nOutMsg[n] = None
  /\ ~(Pack(NewApprovedBlockRequest, n, Bootstrap(n)) \in SeqToSet(nInMsg[Bootstrap(n)]))
  /\ ~(Pack(NewApprovedBlock, Bootstrap(n), n) \in SeqToSet(nOutStreamMsg[Bootstrap(n)]))
  /\ SendMsg(NewApprovedBlockRequest, n, Bootstrap(n))
  /\ UNCHANGED << nodes, nInMsg, nOutStreamMsg, nStatus >>

FromNewToInit ==
  \/ LaunchFromNewToInit
  \/ SuccessFromNewToInit
  \/ FailedFromNewToInit
  \/ ResendWhileInit

CMBroadcastUnapprovedBlock == \E n \in nodes :
  /\ nStatus[n] = "ceremony_master"
  /\ ~(\E p \in SeqToSet(nOutStreamMsg[n]) : p.msg.type = NewUnapprovedBlock.type)
  /\ ~(\E p \in SeqToSet(nInMsg[n]) : p.msg.type = NewUnapprovedBlock.type)
  /\ BroadcastStream(NewUnapprovedBlock, n, nodes \ {n})
  /\ UNCHANGED << nodes, nStatus, nInMsg, nOutMsg >>

LaunchCeremonyMaster ==
  \/ CMBroadcastUnapprovedBlock

(***************************************************************************)
(*  Ceremony Master message handling                                     *)
(***************************************************************************)

CeremonyMasterHandlesBlockApproval == \E n \in nodes :
  /\ Handling(n, "ceremony_master", "BlockApproval")
  /\ TransitionTo(n, "running")
  /\ BroadcastStream(NewApprovedBlock, n, nodes \ {n})
  /\ UNCHANGED << nodes, nOutMsg>>

CeremonyMasterHandlesApprovedBlock        == DoNotHandle("ceremony_master", "ApprovedBlock")
CeremonyMasterHandlesApprovedBlockRequest == DoNotHandle("ceremony_master", "ApprovedBlockRequest")
CeremonyMasterHandlesUnapprovedBlock      == DoNotHandle("ceremony_master", "UnapprovedBlock")

(***************************************************************************)
(*  Genesis Validator message handling                                     *)
(***************************************************************************)

GenesisValidatorHandlesUnapprovedBlock == \E n \in nodes :
  /\ Handling(n, "genesis_validator", "UnapprovedBlock")
  /\ TransitionTo(n, "init")
  /\ StreamMsg(NewBlockApproval, n, Sender(n))
  /\ UNCHANGED << nodes, nOutMsg >>

GenesisValidatorHandlesApprovedBlock        == DoNotHandle("genesis_validator", "ApprovedBlock")
GenesisValidatorHandlesApprovedBlockRequest == DoNotHandle("genesis_validator", "ApprovedBlockRequest")
GenesisValidatorHandlesBlockApproval        == DoNotHandle("genesis_validator", "BlockApproval")

(***************************************************************************)
(*  Initializing message handling                                          *)
(***************************************************************************)

InitHandlesApprovedBlock == \E n \in nodes :
  /\ Handling(n, "init", "ApprovedBlock")
  /\ TransitionTo(n, "running")
  /\ UNCHANGED << nodes, nOutMsg, nOutStreamMsg >>

InitHandlesApprovedBlockRequest == DoNotHandle("init", "ApprovedBlockRequest")
InitHandlesUnapprovedBlock      == DoNotHandle("init", "UnapprovedBlock")
InitHandlesBlockApproval        == DoNotHandle("init", "BlockApproval")

(***************************************************************************)
(*  Running message handling                                               *)
(***************************************************************************)
RunningHandlesApprovedBlockRequest == \E n \in nodes :
  /\ Handling(n, "running", "ApprovedBlockRequest")
  /\ StreamMsg(NewApprovedBlock, n, Sender(n))
  /\ UNCHANGED << nodes, nOutMsg, nStatus >>

RunningHandlesApprovedBlock   == DoNotHandle("running", "ApprovedBlock")
RunningHandlesUnapprovedBlock == DoNotHandle("running", "UnapprovedBlock")
RunningHandlesBlockApproval == DoNotHandle("running", "BlockApproval")

-----------------------------------------------------------------------------

EventuallyAllNodesAreRunning ==
  <>[](\A n \in nodes : nStatus[n] = "running")

-----------------------------------------------------------------------------

InitNodeStatus(n) ==
  (CHOOSE cn \in Nodes : cn.id = n).status

InitNodeState(n) ==
  LET node == (CHOOSE cn \in Nodes : cn.id = n)
  IN [bootstrap |-> n.bootstrap]

Setup ==
  /\ nodes = {n.id : n \in Nodes}
  /\ nStatus = [n \in nodes |-> InitNodeStatus(n)]
  /\ nInMsg = [n \in nodes |-> << >>]
  /\ nOutStreamMsg = [n \in nodes |-> << >>]
  /\ nOutMsg = [n \in nodes |-> None]

Next ==
  \/ \E mt \in MessageType : TransferStreamMsg(mt)
  \/ TransferMsg  
  \/ LooseStreamMsg
  \/ LooseMsg
  \/ FromNewToInit
  \/ LaunchCeremonyMaster
  \/ CeremonyMasterHandlesApprovedBlock
  \/ CeremonyMasterHandlesApprovedBlockRequest
  \/ CeremonyMasterHandlesUnapprovedBlock
  \/ CeremonyMasterHandlesBlockApproval
  \/ GenesisValidatorHandlesApprovedBlock
  \/ GenesisValidatorHandlesApprovedBlockRequest
  \/ GenesisValidatorHandlesUnapprovedBlock
  \/ GenesisValidatorHandlesBlockApproval
  \/ InitHandlesApprovedBlock
  \/ InitHandlesApprovedBlockRequest
  \/ InitHandlesUnapprovedBlock
  \/ InitHandlesBlockApproval  
  \/ RunningHandlesApprovedBlock
  \/ RunningHandlesApprovedBlockRequest
  \/ RunningHandlesUnapprovedBlock
  \/ RunningHandlesBlockApproval  

Spec ==
   /\ Setup /\ []([Next]_vars)
   /\ SF_vars(TransferMsg)
   /\ \A mt \in MessageType : SF_vars(TransferStreamMsg(mt))
   /\ WF_vars(FromNewToInit)
   /\ WF_vars(LaunchCeremonyMaster)
   /\ WF_vars(CeremonyMasterHandlesApprovedBlock)
   /\ WF_vars(CeremonyMasterHandlesApprovedBlockRequest)
   /\ WF_vars(CeremonyMasterHandlesUnapprovedBlock)
   /\ WF_vars(CeremonyMasterHandlesBlockApproval)
   /\ WF_vars(GenesisValidatorHandlesApprovedBlock)
   /\ WF_vars(GenesisValidatorHandlesApprovedBlockRequest)
   /\ WF_vars(GenesisValidatorHandlesUnapprovedBlock)
   /\ WF_vars(GenesisValidatorHandlesBlockApproval)
   /\ WF_vars(InitHandlesApprovedBlock)
   /\ WF_vars(InitHandlesApprovedBlockRequest)
   /\ WF_vars(InitHandlesUnapprovedBlock)
   /\ WF_vars(InitHandlesBlockApproval)  
   /\ WF_vars(RunningHandlesApprovedBlock)
   /\ WF_vars(RunningHandlesApprovedBlockRequest)
   /\ WF_vars(RunningHandlesUnapprovedBlock)
   /\ WF_vars(RunningHandlesBlockApproval)  

=============================================================================