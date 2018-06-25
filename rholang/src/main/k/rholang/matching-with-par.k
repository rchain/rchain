module MATCHING-WITH-PAR
imports SPECIFIC-MATCHING-RULES 
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //
/// PAR MATCHING ALGORITHM
// ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* //

// When matching the pattern "par" this process becomes a little more complicated
syntax PatternMatchingCells ::= "[" String "][" HigherProc "][" HigherProc "][" HigherProc "][" Int "][" HigherProc "]{" List "}"
          | "[" HigherProc "][" HigherProc "][" HigherProc "][" HigherProc "][" HigherProc "]"
          // For variable matching at the end
          | "[" HigherProc "][" HigherProc "][" HigherProc "][" HigherProc "][" HigherProc "]{" HigherProc "}"

// If we get a pattern or a process sent that has a "|" in it, we pause computation and invoke the par match.
// The processes sent go in the "from" cells; the patterns go in the "to" cells.
// The intuition for this is that we match from the process sent to the pattern.
// Only the "to" cells can contain free variables.
rule <thread> ...
         <k> ... matchstdform[S:String]( "par"[P1:HigherProc][P2:HigherProc] ; "par"[Q1:HigherProc][Q2:HigherProc] ; true)
                 => #(PARPAUSE)[S,I +Int 1] ... </k>
         (.Bag =>
         <ParMatch> ...
           // We need each ParMatch cell to have an ID, since it's possible to nest ParMatches inside of ParMatches
           // with correct syntax. We assign the ID in such a way that the ParMatchCounter is the same as the ParMatch ID,
           // and not one off.
           <id> I +Int 1 </id>
           <from> fromparmatchstdform(P1 | P2) </from>
           <to> toparmatchstdform(Q1 | Q2) </to>
         ... </ParMatch>)
   ... </thread>
   <ParMatchCounter> I:Int => I +Int 1 </ParMatchCounter>

// In order to systematically check for a match we need these in some kind of a standard
// form. We first need to do this. We give semantics for a "par", but the whole point of
// this is to break up the "par". Right after this we'll give rules for breaking up the "par"

// The "true" "false" bracketed part of this standard form is just to make sure that we separate
// all the "par"s before checking everything. This ensures we don't miss anything.

// First the "from" cells
// The standard form is [ process sort ][ process ][ id of the process to which it matches ][ id ][ active/inactive ]{ list of "to" cells }
// We start with negative values in the matching ID, as well as a nonsensical list. These will
// get updated as the matching procedes.
// We include process sort (the string in the first bracketed part) there for both a natural sorting to speed up computation (as rholang does)
// as well as to watch for any remaining "par"s that might mess things up.
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform(P1:HigherProc | P2:HigherProc)
                  => ["par"][true][P1 | P2][ -1 ][ -1 ]["inactive"]{ListItem(-1)}  </from> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform(for(C:Chan <- D:Chan){P:HigherProc})
                  => ["listen"][false][for(C <- D){P}][ -1 ][ I ]["inactive"]{ListItem(-1)}  </from>
                  <MatchFromCounter> I => I +Int 1 </MatchFromCounter> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform(for(C:Chan <= D:Chan){P:HigherProc})
                  => ["persistentlisten"][false][for(C <= D){P}][ -1 ][ I ]["inactive"]{ListItem(-1)}  </from>
                  <MatchFromCounter> I => I +Int 1 </MatchFromCounter> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform(contract D:Chan(C:Chan) = { P:HigherProc })
                  => ["persistentlisten"][false][for(C <= D){P}][ -1 ][ I ]["inactive"]{ListItem(-1)}  </from>
                  <MatchFromCounter> I => I +Int 1 </MatchFromCounter> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform(new X:Ids in { P:HigherProc })
                  => ["new"][false][new X in { P }][ -1 ][ I ]["inactive"]{ListItem(-1)}  </from>
                  <MatchFromCounter> I => I +Int 1 </MatchFromCounter> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform(C:Chan!(P:HigherProc))
                  => ["send"][false][C!(P)][ -1 ][ I ]["inactive"]{ListItem(-1)}  </from>
                  <MatchFromCounter> I => I +Int 1 </MatchFromCounter> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform(C:Chan!!(P:HigherProc))
                  => ["persistentsend"][false][C!!(P)][ -1 ][ I ]["inactive"]{ListItem(-1)}  </from>
                  <MatchFromCounter> I => I +Int 1 </MatchFromCounter> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform(match P:HigherProcOrChan { H:HigherMatchCases })
                  => ["match"][false][match P { H }][ -1 ][ I ]["inactive"]{ListItem(-1)}  </from>
                  <MatchFromCounter> I => I +Int 1 </MatchFromCounter> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform( Nil )
                  => ["Nil"][false][ Nil ][ -1 ][ I ]["inactive"]{ListItem(-1)}  </from>
                  <MatchFromCounter> I => I +Int 1 </MatchFromCounter> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> fromparmatchstdform(E:Exp)
                  => ["Exp"][false][E][ -1 ][ I ]["inactive"]{ListItem(-1)}  </from>
                  <MatchFromCounter> I => I +Int 1 </MatchFromCounter> ... </ParMatch> ... </thread>

// Then the "to" cells
// The standard form is [ process sort ][ process ][ id number ][ taken/available ]
// During matching, only the taken/available gets updated
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(P1:HigherProc | P2:HigherProc)
                  => ["par"][true][P1 | P2][ -1 ]["available"]  </to> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(for(C:Chan <- D:Chan){P:HigherProc})
                  => ["listen"][false][for(C <- D){P}][ I ]["available"]  </to>
                  <MatchToCounter> I => I +Int 1 </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem(I) ... </ListofAllToCells> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(for(C:Chan <= D:Chan){P:HigherProc})
                  => ["persistentlisten"][false][for(C <= D){P}][ I ]["available"]  </to>
                  <MatchToCounter> I => I +Int 1 </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem(I) ... </ListofAllToCells> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(contract D:Chan(C:Chan) = { P:HigherProc })
                  => ["persistentlisten"][false][for(C <= D){P}][ I ]["available"]  </to>
                  <MatchToCounter> I => I +Int 1 </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem(I) ... </ListofAllToCells> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(new X:Id in { P:HigherProc })
                  => ["new"][false][new X in { P }][ I ]["available"]  </to>
                  <MatchToCounter> I => I +Int 1 </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem(I) ... </ListofAllToCells> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(C:Chan!(P:HigherProc))
                  => ["send"][false][C!(P)][ I ]["available"]  </to>
                  <MatchToCounter> I => I +Int 1 </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem(I) ... </ListofAllToCells> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(C:Chan!(P:HigherProc))
                  => ["persistentsend"][false][C!!(P)][ I ]["available"]  </to>
                  <MatchToCounter> I => I +Int 1 </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem(I) ... </ListofAllToCells> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(match P:HigherProcOrChan { H:HigherMatchCases })
                  => ["match"][false][ match P { H } ][ I ]["available"]  </to>
                  <MatchToCounter> I => I +Int 1 </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem(I) ... </ListofAllToCells> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform( Nil )
                  => ["Nil"][false][ Nil ][ I ]["available"]  </to>
                  <MatchToCounter> I => I +Int 1 </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem(I) ... </ListofAllToCells> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(E:Exp)
                  => ["Exp"][false][E][ I ]["available"]  </to>
                  <MatchToCounter> I => I +Int 1 </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem(I) ... </ListofAllToCells> ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <to> toparmatchstdform(X:Id)
                  => ["Id"][false][X][ I ]["variable"]  </to>
                  <MatchToCounter> I </MatchToCounter>
                  <ListofAllToCells> ... .List => ListItem("var") ... </ListofAllToCells> ... </ParMatch> ... </thread>


// Delete empty "to" and "from" cells
rule <thread> ... <ParMatch> ... <to> .K </to> => . ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <from> .K </from> => . ... </ParMatch> ... </thread>

// Now to break up the "par"s
// first the "from"
rule <thread> ... <ParMatch> ... <from> ["par"][true][P1:HigherProc | P2:HigherProc][ -1 ][ -1 ]["inactive"]{ListItem(-1)}
                  => fromparmatchstdform(P1) </from>
                  (.Bag => <from> fromparmatchstdform(P2) </from> ) ... </ParMatch> ... </thread>

// now the "to"
rule <thread> ... <ParMatch> ... <to> ["par"][true][P1:HigherProc | P2:HigherProc][ -1 ]["available"]
                  => toparmatchstdform(P1) </to>
                  (.Bag => <to> toparmatchstdform(P2) </to> ) ... </ParMatch> ... </thread>

// Now that the pars are separated, we can start systematically trying to find a match.
// Note: because of possible nondeterministic behavior, we have to make sure we don't
// accidentally match before all the "par"s are done separating. This is taken care of by the
// tags on each std form statement: true of it's still a "par", false otherwise. We only proceed if
// it's false.

// Let the par matching games begin
// We change this "from" cell's id to -1 so that we don't start the game over and over again
rule <thread> ... <ParMatch> ... <from> [S:String][false][ P:HigherProc ][ -1 ][ (0 => -1) ][ ("inactive" => "active") ]{ (_ => L) } </from>
                                 <ListofAllToCells> L:List </ListofAllToCells> ... </ParMatch> ... </thread>

// General step (for matching with non-variables)
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ (-1 => I) ][ J:Int ][ "active" ]{ ListHead:List (ListItem(I) => .) ListTail:List } </from>
                                 <to> [S2:String][false][P2:HigherProc][I:Int]["available"] </to>
                                 <id> ID:Int </id> ... </ParMatch>
                  // ERROR!! YOU NEED TO MAKE A "TFSTDMATCH" FOR EACH KIND OF MATCHING YOU WANT!!!
                  <k> #(PARPAUSE)[S:String,ID] => match["TF" +String S]( P1 ; P2 ) ~> #(PARPAUSE)[S,ID] ... </k> ... </thread>

// You can't try to match with a process that's already taken
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ J1:Int ][ J2:Int ][ "active" ]{ ListHead:List (ListItem(I) => .) ListTail:List } </from>
                                 <to> [S2:String][false][P2:HigherProcNoVars][I]["taken"] </to> ... </ParMatch> ... </thread>

// There's been a match!
// We change the current from cell from active to inactive, reserve the to cell it matched with, and move up to the next from cell
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ I ][ J:Int ][ ("active" => "inactive") ]{ L1 } </from>
                                 <to> [S2:String][false][P2:HigherProc][I][("available" => "taken")] </to>
                                 <id> ID </id>
                                 // We have just taken one of the "to" cells, so we keep track of that in the counter
                                 <MatchToCounter> I2:Int => I2 +Int -1 </MatchToCounter>
                                 // We grab the next "from" cell up
                                 <from> [S3:String][false][ P3:HigherProc ][ M:Int ][ J2:Int ][ ("inactive" => "active") ]{ _ => L2 } </from>
                                 <ListofAllToCells> L2:List </ListofAllToCells> ... </ParMatch>
                  // There's been a match because there is nothing before the #(PARPAUSE) in the k-cell
                  // This means that the "match["TFSTDMATCH"]" function was successful, and left nothing.
                  <k> #(PARPAUSE)[S:String,ID:Int] ... </k> ... </thread>
                  requires (J2 =Int J +Int 1)

// We have to do the same rule, but in the case we matched on level 0 (since we changed 0 to -1)
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ I ][ -1 ][ ("active" => "inactive") ]{ L1 } </from>
                                 <to> [S2:String][false][P2:HigherProc][I][("available" => "taken")] </to>
                                 <id> ID </id>
                                 // We have just taken one of the "to" cells, so we keep track of that in the counter
                                 <MatchToCounter> I2:Int => I2 +Int -1 </MatchToCounter>
                                 // We grab the next "from" cell up
                                 <from> [S3:String][false][ P3:HigherProc ][ M:Int ][ 1 ][ ("inactive" => "active") ]{ _ => L2 } </from>
                                 <ListofAllToCells> L2:List </ListofAllToCells> ... </ParMatch>
                  // There's been a match because there is nothing before the #(PARPAUSE) in the k-cell
                  // This means that the "match["TFSTDMATCH"]" function was successful, and left nothing.
                  <k> #(PARPAUSE)[S:String,ID:Int] ... </k> ... </thread>

// Finally, we do the same rule, but in the case that we've matched on the last "from" cell
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ I ][ J1:Int ][ ("active" => "done") ]{ L1 } </from>
                                 <to> [S2:String][false][P2:HigherProc][I][("available" => "taken")] </to>
                                 <id> ID </id>
                                 // We have just taken one of the "to" cells, so we keep track of that in the counter
                                 <MatchToCounter> I2:Int => I2 +Int -1 </MatchToCounter>
                                 // We're at the top
                                 <MatchFromCounter> J2:Int </MatchFromCounter> ... </ParMatch>
                  // There's been a match because there is nothing before the #(PARPAUSE) in the k-cell
                  // This means that the "match["TFSTDMATCH"]" function was successful, and left nothing.
                  <k> #(PARPAUSE)[S:String,ID:Int] ... </k> ... </thread>
                  requires (J2 =Int J1 +Int 1)

// There's been a match! (with a variable)
// General step for binding to variables
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ (-1 => "var") ][ J:Int ][ ("active" => "inactive") ]{ ListHead:List (ListItem("var") => .) ListTail:List } </from>
                    <from> [S2:String][false][ P2:HigherProc ][ I2:Int ][ J2:Int ][ ("inactive" => "active") ]{ _ => L2:List } </from>
                    <ListofAllToCells> L2:List </ListofAllToCells>
                  ... </ParMatch> ... </thread>
                  requires (J2 =Int J +Int 1)

// Same step, but if we are at the 0 level
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ (-1 => "var") ][ -1 ][ ("active" => "inactive") ]{ ListHead:List (ListItem("var") => .) ListTail:List } </from>
                    <from> [S2:String][false][ P2:HigherProc ][ I2:Int ][ 1 ][ ("inactive" => "active") ]{ _ => L2:List } </from>
                    <ListofAllToCells> L2:List </ListofAllToCells>
                  ... </ParMatch> ... </thread>

// Same step, but if we're at the last "from" cell
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ (-1 => "var") ][ J:Int ][ ("active" => "done") ]{ ListHead:List (ListItem("var") => .) ListTail:List } </from>
                    <MatchFromCounter> J2:Int </MatchFromCounter>
                  ... </ParMatch> ... </thread>
                  requires (J2 =Int J +Int 1)

// The match failed, so we try another one (we redo the general step)
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ (I => -1) ][ J:Int ][ "active" ]{ L1 } </from>
                                 <to> [S2:String][false][P2:HigherProc][I:Int]["available"] </to>
                                 <id> ID:Int </id> ... </ParMatch>
                  <k> ... (#(MATCHFAIL)[ TF:String ] ~> M:K => .) ~> #(PARPAUSE)[S:String,ID] ... </k> ... </thread>
                  <ParMatchCounter> ID </ParMatchCounter>
                  // WATCH OUT FOR ME. This could cause future problems
                  requires (TF ==String "TFSTDMATCH") orBool (TF ==String "TFCHANMATCH")


// There are no more options for possible matches (go back down)
// This is for a non-variable
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ -1 ][ J:Int ][ ("active" => "inactive") ]{ .List } </from>
                                 // Go down one; you can't use what you had before
                                 <from> [R1:String][false][ Q1:HigherProc ][ (I2 => -1) ][ J2:Int ][ ("inactive" => "active") ]{ L } </from>
                                 <to> [R2:String][false][Q2:HigherProc][I2:Int][("taken" => "available")] </to>
                                 // We have to return one of the "to" cells to move forward
                                 <MatchToCounter> C => C +Int 1 </MatchToCounter> ... </ParMatch> ... </thread>
                                 requires (J2 =Int J +Int -1)

// We have to do the same rule, but in the case that we were on the first level and we have to go back down to 0
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ -1 ][ 1 ][ ("active" => "inactive") ]{ .List } </from>
                                 // Go down one; you can't use what you had before
                                 <from> [R1:String][false][ Q1:HigherProc ][ (I2 => -1) ][ -1 ][ ("inactive" => "active") ]{ L } </from>
                                 <to> [R2:String][false][Q2:HigherProc][I2:Int][("taken" => "available")] </to>
                                 // We have to return one of the "to" cells to move forward
                                 <MatchToCounter> C => C +Int 1 </MatchToCounter> ... </ParMatch> ... </thread>

// Going back down where the thing below matched to a variable (essential duplicate of rules above)
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ (I1 => -1) ][ J:Int ][ ("active" => "inactive") ]{ .List } </from>
                                 // Go down one; you can't use what you had before
                                 <from> [R1:String][false][ Q1:HigherProc ][ ("var" => -1) ][ J2:Int ][ ("inactive" => "active") ]{ L } </from>
                                 ... </ParMatch> ... </thread>
                                 requires (J2 =Int J +Int -1)

rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ (I1 => -1) ][ 1 ][ ("active" => "inactive") ]{ .List } </from>
                                 // Go down one; you can't use what you had before
                                 <from> [R1:String][false][ Q1:HigherProc ][ ("var" => -1) ][ -1 ][ ("inactive" => "active") ]{ L } </from>
                                 ... </ParMatch> ... </thread>

// There are no more options on the 0 level, so the match fails and we rewrite #(PARPAUSE) as #(MATCHFAIL)
// (a) We first write these rules for nested ParMatches.
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ -1 ][ -2 ][ "active" ]{ .List } => #(MATCHFAIL)[ "TFSTDMATCH" ] </from>
                    <id> ID:Int </id> ... </ParMatch>
                   <k> #(PARPAUSE)[S:String,ID] => #(MATCHFAIL)["TF" +String S] ... </k> ... </thread>
                   <ParMatchCounter> ID => ID +Int -1 </ParMatchCounter>
                   requires ID >Int 1

rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ -1 ][ -1 ][ "active" ]{ .List } => #(MATCHFAIL)[ "TFSTDMATCH" ] </from>
                    <id> ID:Int </id> ... </ParMatch>
                   <k> #(PARPAUSE)[S:String,ID] => #(MATCHFAIL)["TF" +String S] ... </k> ... </thread>
                   <ParMatchCounter> ID => ID +Int -1 </ParMatchCounter>
                   requires ID >Int 1

// Cleanup
rule <thread> ... <ParMatch> ... <from> #(MATCHFAIL)[ "TFSTDMATCH" ] </from> <id> ID </id> ... </ParMatch> => . ... </thread>
     requires ID >Int 1

// (b) Now the rules for the case that we're at the base ParMatch. The difference is that we throw an
//     error matching the original request, instead of a "TFSTDMATCH" matchfail.
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ -1 ][ -2 ][ "active" ]{ .List } => #(MATCHFAIL)[ "TFSTDMATCH" ] </from>
                    <id> ID:Int </id> ... </ParMatch>
                   <k> #(PARPAUSE)[S:String,ID] => #(MATCHFAIL)[S] ... </k> ... </thread>
                   <ParMatchCounter> ID => ID +Int -1 </ParMatchCounter>
                   requires ID =Int 1

rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ -1 ][ -1 ][ "active" ]{ .List } => #(MATCHFAIL)[ "TFSTDMATCH" ] </from>
                    <id> ID:Int </id> ... </ParMatch>
                   <k> #(PARPAUSE)[S:String,ID] => #(MATCHFAIL)[S] ... </k> ... </thread>
                   <ParMatchCounter> ID => ID +Int -1 </ParMatchCounter>
                   requires ID =Int 1

// Cleanup
rule <thread> ... <ParMatch> ... <from> #(MATCHFAIL)[ "TFSTDMATCH" ] </from> <id> ID </id> ... </ParMatch> => . ... </thread>
     requires ID =Int 1


// We've gotten to the end but we need to make sure there
// aren't any "to" cells which have been neglected.
// This can happen by matching too many processes to variables
// If this happens, we have to go back down and try again,
// potentially failing the whole match.

// First we see if the MatchToCounter is 0. If it is, that means we
// didn't neglect any "to" cells. If not, then we need to try again
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ E:Exp ][ J:Int ][ "done" ]{ L1:List } </from>
                    <MatchFromCounter> J1:Int </MatchFromCounter>
                    <MatchToCounter> J2:Int </MatchToCounter>
                    <Matched> .K => (J2 =Int 0) </Matched>
                    <id> ID:Int </id>
                  ... </ParMatch> ... </thread>
                  <ParMatchCounter> ID </ParMatchCounter>
                  requires (J +Int 1 =Int J1)

// If there are leftover "to" cells, we need to go back down
// If the last thing we matched to was a variable, then we just go back down and try again
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ ("var" => -1) ][ J:Int ][ ("done" => "active") ]{ L1:List } </from>
                    <MatchFromCounter> J2:Int </MatchFromCounter>
                    <Matched> false => . </Matched>
                    <id> ID:Int </id>
                  ... </ParMatch> ... </thread>
                  <ParMatchCounter> ID </ParMatchCounter>
                  requires (J2 =Int J +Int 1)

// If the last thing we matched to was not a variable, we need to go back down
// and update the thing we were matched to to say "available"
rule <thread> ... <ParMatch> ...
                    <from> [S1:String][false][ P1:HigherProc ][ (I:Int => -1) ][ J:Int ][ ("done" => "active") ]{ L1:List } </from>
                    <to> [S2:String][false][P2:HigherProc][I][("taken" => "available")] </to>
                    <MatchFromCounter> J2:Int </MatchFromCounter>
                    // We need to put back the "to" cell
                    <MatchToCounter> I2:Int => I2 +Int -1 </MatchToCounter>
                    <Matched> false => . </Matched>
                  ... </ParMatch> ... </thread>
                  requires (J2 =Int J +Int 1)

// We've matched everything, this match as a whole is a success!
// (We may have extra variables left over, which we bind to "Nil")
rule <thread> ... <ParMatch> ... <from> [S1:String][false][ P1:HigherProc ][ I ][ J:Int ][ ("done" => "inactive") ]{ L1 } </from>
                                 // So we matched
                                 <Matched> true </Matched>
                                 // Work on the furthest ParMatch invoked
                                 <id> ID </id> ... </ParMatch>
                   <k> #(PARPAUSE)[S:String,ID:Int] ... </k> ... </thread>
                   <ParMatchCounter> ID </ParMatchCounter>

// What happens when "Matched" is true
rule <thread> ... <ParMatch> ...
                            <from> [S1:String][false][ P1:HigherProc ][ I ][ J:Int ][ "inactive" ]{ L1 } => . </from>
                            <to> [S2:String][false][P2:HigherProc][I:Int]["taken"] => . </to>
                            <Matched> true </Matched>
                            <MatchToCounter> I1 => I1 +Int 1 </MatchToCounter>
                            <MatchFromCounter> I2 => I2 +Int -1 </MatchFromCounter>
                            <id> ID </id>
                            ... </ParMatch>
                  <k> #(PARPAUSE)[S:String,ID:Int] => #(PARPAUSE)[S,ID] ~> match[S](P1 ; P2) ... </k>  ... </thread>
                  <ParMatchCounter> ID </ParMatchCounter>

rule <thread> ... <ParMatch> ...
                    <to> [S:String][false][P:HigherProc][I]["variable"]
                     =>  [S:String][false][P:HigherProc][I]["variable"]{ Nil } </to>
                    <Matched> true </Matched>
                    <id> ID:Int </id> ... </ParMatch> ... </thread>
                    <ParMatchCounter> ID </ParMatchCounter>

// Now we (nondetermistically) assign processes which are to be bound to vars
// by grouping them.
rule <thread> ... <ParMatch> ...
                            <from> [S1:String][false][ P1:HigherProc ]["var"][ J:Int ][ "inactive" ]{ L1 } => . </from>
                            <to> [S2:String][false][P2:HigherProc][I]["variable"]{ (P:HigherProc => P | P1) } </to>
                            <Matched> true </Matched>
                            <MatchToCounter> I1 => I1 +Int 1 </MatchToCounter>
                            <MatchFromCounter> I2 => I2 +Int -1 </MatchFromCounter>
                            <id> ID:Int </id>
                            ... </ParMatch>  ... </thread>
                            <ParMatchCounter> ID </ParMatchCounter>

// Once that is done, we send all the vars to be matched; if they got no processes
// they get the "Nil" process
rule <thread> ... <ParMatch> ...
                            <to> [S1:String][false][P1:Id][I]["variable"]{ P2:HigherProc } => . </to>
                            <Matched> true </Matched>
                            <MatchFromCounter> 0 </MatchFromCounter>
                            <id> ID </id>
                            ... </ParMatch>
                  <k> #(PARPAUSE)[S:String,ID:Int] => #(PARPAUSE)[S,ID] ~> match["STDMATCH"](P2 ; P1) ... </k>  ... </thread>
                  <ParMatchCounter> ID </ParMatchCounter>

// Once this has exhausted, clear the contents of the the "ParMatch" cell,
// unpause the calculation and move forward
rule <thread> ... <ParMatch>
                      <id> ID:Int => . </id>
                      <MatchFromCounter> I2:Int => . </MatchFromCounter>
                      <MatchToCounter> I1:Int => . </MatchToCounter>
                      <ListofAllToCells> L1:List => . </ListofAllToCells>
                      <Wildcards> B2:Bool => . </Wildcards>
                      <Matched> true => . </Matched>
                      <BindToVars> Nil => . </BindToVars>
                  </ParMatch>
                  // Continue forward on the calculation! Hooray!!
                  <k> #(PARPAUSE)[S:String,ID] => . ... </k>
                  ... </thread>
                  <ParMatchCounter> ID => ID +Int -1 </ParMatchCounter>


// Empty cells get deleted
rule <thread> ... <ParMatch> ... <MatchFromCounter> .K </MatchFromCounter> => . ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <MatchToCounter> .K </MatchToCounter> => . ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <ListofAllToCells> .List </ListofAllToCells> => . ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <Wildcards> .K </Wildcards> => . ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <BindToVars> .K </BindToVars> => . ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> ... <id> .K </id> => . ... </ParMatch> ... </thread>
rule <thread> ... <ParMatch> <Matched> .K </Matched> </ParMatch> => . ... </thread>

endmodule
