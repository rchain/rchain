module CONFIGURATION
imports RHO-SYNTAX

// Configuration
configuration <T color="yellow">
                <thread multiplicity="*" color="purple">
                  // When we initiate our program, fvInitial( ) checks for free variables
                  <k multiplicity="*" color="red"> fvInitial( $PGM:Proc ) </k>
                  <In multiplicity="*" color="blue"> .K </In>
                  <Out multiplicity="*" color="yellow"> .K </Out>
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
                    <TotalToCells> 0 </TotalToCells>
                    <SetofAllToCells> .Set </SetofAllToCells>
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
                <GlobalSetofOutIds color="orange"> .Set </GlobalSetofOutIds>
                <GlobalSetofInIds color="purple"> .Set </GlobalSetofInIds>
                <ParMatchCounter> 0 </ParMatchCounter>
                <StdOut color="Orchid" stream="stdout"> .List </StdOut>
              </T>

endmodule
