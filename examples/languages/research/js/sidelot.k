change $u -> $undefined

kmod JS-TESTING is including JS-UTILITY
	syntax Bool ::= isTruthy( R ) 
		| isNumeric( R ) | isNumber( R )
		| isNaN( R ) | isEitherNaN( R , R )

	rule isTruthy($undefined) 			=> false		 [structural]
	rule isTruthy($null) 				=> false		 [structural]
	rule isTruthy($b(B:Bool)) 			=> B			 [structural]
	rule isTruthy($zero(_)) 			=> false		 [structural]
	rule isTruthy($n(_)) 				=> true			 [structural]
	rule isTruthy($infinity(_)) 		=> true			 [structural]
	rule isTruthy($nan) 				=> false		 [structural]
	rule isTruthy($s(S_)) 				=> S =/=K ""  [structural]
	rule isTruthy($obj(_)) 				=> true			 [structural]

	rule isNumeric(Numeric:JSNumeric)		=> true  [structural]
	rule isNumeric(NonNumeric:JSNonNumeric)	=> false [structural]

	rule isNumber(Num:JSNumber)				=> true  [structural]
	rule isNumber($infinity(SignValue))		=> false [structural]
	rule isNumber($nan)						=> false [structural]
	rule isNumber(NonNumeric:JSNonNumeric)	=> false [structural]
	
	rule isNaN($nan)						=> true  [structural]
	rule isNaN(NonNaN:JSNonNaN)				=> false [structural]
	
	--- rule isEitherNaN($nan, Value:JSValue)			=> true  [structural]
	--- rule isEitherNaN(Value, $nan)					=> true  [structural]
	--- rule isEitherNaN(NN1:JSNonNaN, NN2:JSNonNaN)	=> false [structural]
	
endkm



	
	
	
endkm

kmod JS-COMPARISON is including JS-UNARY + JS-SYNTAX + PL-BUILTINS

	--- ECMA-262 11.8.1-4
	rule $lt(Left:JSValue, Right:JSValue) 	
		=> comparison(primLessThan(toPrimitive(Left, "N"), toPrimitive(Right, "N")), true)
	rule $gt(Left, Right) 	
		=> comparison(primLessThan(toPrimitive(Right, "N"), toPrimitive(Left, "N")), true)
	rule $lte(Left, Right) 	
		=> comparison(primLessThan(toPrimitive(Right, "N"), toPrimitive(Left, "N")), false)
	rule $gte(Left, Right) 
		=> comparison(primLessThan(toPrimitive(Left, "N"), toPrimitive(Right, "N")), false)


	--- ECMA-262 11.8.5
	syntax JSBoolean ::= comparison( JSBooleanPlus , Bool )
	syntax JSBooleanPlus ::= primLessThan( JSValue , JSValue ) 
			| lessThan( JSNumeric , JSNumeric )
	
	rule comparison($undefined, B:Bool)	=> $b(false)	[structural]
	rule comparison($b(B), true)			=> $b(B)		[structural]
	rule comparison($b(B), false)			=> not($b(B))	[structural]
	
	rule primLessThan($s(S1:String), $s(S2:String)) 	=> $b(_<String_(S1, S2)) [structural]
	rule primLessThan(P1:JSPrimitive, NS2:JSNonString) 	=> lessThan(toNumeric(P1), toNumeric(NS2)) [structural]
	rule primLessThan(NS1:JSNonString, P2:JSPrimitive) 	=> lessThan(toNumeric(NS1), toNumeric(P2)) [structural]
	
	rule lessThan($nan, Numeric:JSNumeric) 				=> $undefined [structural]
	rule lessThan(Numeric:JSNumeric, $nan) 				=> $undefined [structural]
	rule lessThan($infinity(Sign:Int), Q:JSQuantity) 	=> $b(_<Int_(Sign, 0)) [structural]
	rule lessThan(Num1:JSNumber, $infinity(Sign)) 		=> $b(_<Int_(0, Sign)) [structural]
	rule lessThan(Num1, Num2:JSNumber) 		=> $b(_<Float_(floatValue(Num1), floatValue(Num2))) [structural]
		
endkm


kmod JS-IDENTITY is including JS-UNARY + JS-SYNTAX
	syntax JSBoolean ::= identical( JSNonNaN , JSNonNaN )
	
	--- ECMA-262 11.9.4-6
	
	rule $notIdentical(Left:JSValue, Right:JSValue) => not($identical(Left, Right))
	
	rule $identical($nan, Right) 						=> $b(false)
	rule $identical(Left, $nan) 						=> $b(false)
	rule $identical(Value1:JSNonNaN, Value2:JSNonNaN) 	=> identical(Value1, Value2)

	--- rule $identical(Left, Right) => $b(false) when isEitherNaN(Left, Right)
	--- rule $identical(Left, Right) => identical(Left, Right)
	
	rule identical(Num1:JSNumber, Num2:JSNumber) 
			=> $b(floatValue(Num1) ==K floatValue(Num2)) [structural]
	rule identical(Other1:JSNonNumber, Other2:JSNonNumber) 
			=> $b(Other1 ==K Other2) [structural]
	
endkm

--- kmod JS-EQUALITY is including JS-UNARY + JS-SYNTAX + JS-IDENTITY 
--- 	syntax JSBoolean ::= equal( JSValue , JSValue )
--- 	
--- 	--- ECMA-262 11.9.1-3
--- 	rule $notEqual(Left:JSValue, Right:JSValue) => not($equal(Left, Right))
--- 	
--- 	rule $equal($nan, Right) 	=> $b(false)
--- 	rule $equal(Left, $nan) 	=> $b(false)
--- 	rule $equal(Left, Right) 	=> $b(true)  when identical(Left, Right)
--- 	rule $equal(Left, Right) 	=> equal(Left, Right) [owise]
--- 	
--- 	macro equal($null, $undefined)			= $b(true)
--- 	macro equal($undefined, $null)			= $b(true)
--- 	
--- 	macro equal(N1:JSNumeric, S2:String)	= $equal(N1, toNumeric(S2))
--- 	macro equal(S1:String, N2:JSNumeric)	= $equal(toNumeric(S1), N2)
--- 	macro equal(B1:JSBoolean, V2)				= $equal(toNumeric(B1), V2)
--- 	macro equal(V1, B2:JSBoolean)				= $equal(V1, toNumeric(B2))
--- 	macro equal(N1, O2:JSObject)			= $equal(N1, toPrimitive(O2, "N"))
--- 	macro equal(O1:JSObject, N2)			= $equal(toPrimitive(O1, "N"), N2)
--- 	macro equal(S1, O2)						= $equal(S1, toPrimitive(O2, "N"))
--- 	macro equal(O1, S2)						= $equal(toPrimitive(O1, "N"), S2)
--- 	macro equal(V1, V2)						= $b(false)
--- endkm

kmod JS-LOGIC is including JS-SYNTAX + JS-CONVERSIONS
	
	--- ECMA-262 11.11
	rule $and(Val:JSValue, Exp)	=> #if isTruthy(Val) #then Exp #else Val #fi
	rule $or(Val, Exp:JSValue) 	=> #if isTruthy(Val) #then Val #else Exp #fi
	
endkm

kmod JS-CONDITIONAL is including JS-SYNTAX + JS-CONVERSIONS
	syntax K ::= if( K , K , K ) [strict(1)]

	rule [if-true]:
		<k> if(true, TrueAction:K, _) => TrueAction ...</k>
		[structural]
	rule [if-false]:
		<k> if(false, _, FalseAction:K) => FalseAction ...</k>
		[structural]
	
	--- ECMA-262 11.11
	rule [ternary]:
		<k> $ternary(Val:JSValue, TrueAction:K , FalseAction:K)
			=> if(isTruthy(Val), TrueAction, FalseAction) ...</k>
		---	  => #if isTruthy(Val) #then TrueExp #else FalseExp #fi

	rule [ifElse]:
		<k> $ifElse(Val:JSValue, TrueAction:K , FalseAction:K) 
			=> $ternary(Val, TrueAction, 
						#if FalseAction ==K .none #then $undefined #else FalseAction #fi) ...</k>

endkm
--- 
--- kmod JS-VAR is including JS-SYNTAX + JS-CONVERSIONS
--- 
--- $varDeclarations ( List{K} ) [strict]
--- 
--- macro o(type : "VAR" children : a(Exps) Extra) = $varDeclarations(Exps)
--- 
--- var count = elements.length;
--- var index, element, separator;
--- var accumulator = accumulator__ || [];
--- 
--- separator_ = separator_ || ',';
--- 
--- program(
--- 	varDeclarations(id("count")),
--- 	varDeclarations(id("index"),id("element"),id("separator")),
--- 	varDeclarations(id("accumulator")),
--- 	exp(assignment(id("separator_"),.undefined,or(id("separator_"),s(","))))) .


---  rule <k>X:Id => I:Int<_/k> <state_>X|->I<_/state> 
---  rule I1:Int + I2:Int => I1 +Int I2


kmod JS-BASIC-SEMANTICS is 
    including 	  JS-CONVERSIONS 	+ JS-TYPEOF	+ JS-UNARY	+ JS-ARITHMETIC	+ JS-COMPARISON	+ JS-LOGIC	+ JS-CONDITIONAL
	--- + JS-IDENTITY
	--- + JS-EQUALITY
endkm



----================

kmod JS-INIT-JS is including JS-SEMANTICS
	syntax K ::= 
			initJS
		|	initGlobalLexEnv( K ) [strict]  --- Ask Traian

	rule [initJS]: 
		<k> initJS => initGlobalLexEnv(newObj) ~> initUndefined <_/k>
		[structural]

	rule [initGlobalLexEnv]:
		<k> initGlobalLexEnv(Obj:K) => . <_/k> 
		<lexEnv> .none => $env(.Env, Obj) </lexEnv> 
		<globalEnv> .none => Obj </globalEnv>
		[structural]
	
	rule [initUndefined]:
		initUndefined => 
		
		
		setPropertyValue(
			#if Obj ==K .Target #then GlobalObj #else Obj #fi, 
			Name, Value) ...</k>
	<globalEnv> GlobalObj:V </globalEnv>
	
endkm
