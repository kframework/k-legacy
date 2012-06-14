--- $K_BASE/core/kompile -lint js

in js-syntax
in js-configuration
in js-basic-semantics
in js-semantics
--- in ../json/json
--- in js-json-ast-ids
--- in json2prefix
in js-programs

--- K				4 ~> 5 ~> 6
--- List{K}			3 ,, 5 ,, 6
--- List			ListItem(3) ListItem(5) ListItem(5)
--- Set				SetItem(1) SetItem(1) SetItem(1)
--- Bag				BagItem(1) BagItem(1) BagItem(1)
--- Map 	|->
--- Bag		<bsg> </bsg>  -- config style bag

kmod JS-INIT-JS is including JS-SEMANTICS
	syntax K ::= 
			initJS
		|	initGlobalLexEnv( K ) [strict]  --- Ask Traian
		|	initUndefined

	rule [initJS]: 
		<k> initJS => initGlobalLexEnv(newObj) ~> initUndefined ...</k>
		[structural]

	rule [initGlobalLexEnv]:
		<k> initGlobalLexEnv(Obj:V) => . ...</k> 
		<lexEnv> .None => $env(.None, Obj) </lexEnv> 
		<globalEnv> .None => Obj </globalEnv>
		[structural]
	
	rule [initUndefined]:
		<k> initUndefined => setPropertyValue(Obj, "undefined", $undefined) ~> discard ...</k>
		<globalEnv> Obj:V </globalEnv>
		[structural]

endkm


kmod JS is including JS-INIT-JS + JS-PROGRAMS
	syntax Bag ::= run( KLabel )
	syntax Bag ::= go( K )
	
	rule go(Program:K) => <T>... <k> initJS ~> Program </k> ...</T> 
	
	rule [run-program]:
		run (L:KLabel) => <T>... <k> initJS ~> L(.List{K}) </k> ...</T>  
		[structural]
	
	rule [final-output]: 
		<k> Result:V => . </k> 
		<out> _ => stringValue(toString(Result)) </out>
	
	  --- rule <T>... <threads> .Bag </threads> <out> S:String </out> ...</T> 
	  ---   => <result> S </result>
	
	
  	--- macro pgm = [['program1]]
endkm