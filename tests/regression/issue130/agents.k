require /modules/seq

module AGENTS imports SEQ
  configuration
      <agent multiplicity="*"> 
          <me>0</me>
          <parent>-1</parent>
          <control> <k color="green"> .K </k> </control>
      </agent> 
      <br/>
      <nextAgent>1</nextAgent>
      <world>SetItem(0)</world>
      <barrier>true</barrier> <waiting>.Set</waiting> 
      <br/>
      <messages> <message multiplicity="*">
          <from>.K</from> <to>.Set</to> <body>.K</body>
      </message> </messages>
      
  syntax K ::= newAgent K
             | me
             | parent
             | receive
             | receiveFrom K     [strict]
             | send K to K       [strict]
             | sendSynch K to K  [strict]
             | barrier        
             | broadcast K       [strict]
             | haltAgent
             | Int
             | Bool

--- Agents
  rule [agent-creation] : <agent>...  <k>newAgent S:K => N2:Nat ...</k> <me>N1:Nat</me> 
        ...</agent> <br/> <world>... . => SetItem(N2) ...</world> <br/>
       (. => <agent>... <me>N2</me><br/><parent>N1</parent><br/><k>S</k> ...</agent>) 
       <br/><nextAgent>N2 => N2 +Int 1</nextAgent> 
end module
