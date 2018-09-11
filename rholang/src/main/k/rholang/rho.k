// Rholang K-framework

require "rho-syntax.k"
require "processes-semantics.k"
require "sending-receiving.k"
require "persistent-sending-receiving.k"
require "match-process.k"
require "matching-function.k"
require "specific-matching-rules.k"
require "matching-with-par.k"
require "free.k"
require "program-restrictions.k"

module RHO
imports RHO-SYNTAX
imports PROCESSES-SEMANTICS
imports SENDING-RECEIVING
imports PERSISTENT-SENDING-RECEIVING
imports MATCH-PROCESS
imports MATCHING-FUNCTION
imports SPECIFIC-MATCHING-RULES
imports MATCHING-WITH-PAR
imports FREE
imports PROGRAM-RESTRICTIONS


// Configuration
configuration <T color="yellow">
                <thread multiplicity="*" color="purple">
                  // When we initiate our program, fvInitial( ) checks for free variables
                  <k color="red"> fvInitial( $PGM:Proc ) </k>
                  <in multiplicity="*" color="blue"> .K </in>
                  <out multiplicity="*" color="yellow"> .K </out>
                  <InData multiplicity="*"> .K </InData>
                  <OutData multiplicity="*"> .K </OutData>
                  <subst multiplicity="*" color="orange"> .K </subst>
                  // to match patterns with parallel processes in them
                  <ParMatch multiplicity="*">
                    <id> .K </id>
                    <to multiplicity="*"> .K </to>
                    <from multiplicity="*"> .K </from>
                    <MatchFromCounter> 0 </MatchFromCounter>
                    <MatchToCounter> 0 </MatchToCounter>
                    <ListofAllToCells> .List </ListofAllToCells>
                    <BindToVars> Nil </BindToVars>
                    <Matched> .K </Matched>
                  </ParMatch>
                  // to check patterns with a logical "or" in them
                  <OrCheck multiplicity="*"> .K </OrCheck>
                  // to keep track of free variables
                  <FreeVars multiplicity="*">
                    <node multiplicity="*"> .K </node>
                    <bnode multiplicity="*"> .K </bnode>
                    <ornode multiplicity="*"> .K </ornode>
                    <NodeCounter> 0 </NodeCounter>
                  </FreeVars>
                </thread>
                <GlobalListofOutIds color="orange"> .List </GlobalListofOutIds>
                <GlobalListofInIds color="purple"> .List </GlobalListofInIds>
                <NewIntGenerator color="purple"> 0 </NewIntGenerator>
                <ParMatchCounter> 0 </ParMatchCounter>
                <StdOut color="Orchid" stream="stdout"> .List </StdOut>
              </T>


endmodule
