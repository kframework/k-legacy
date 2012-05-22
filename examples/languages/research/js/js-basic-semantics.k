kmod JS-UTILITY	is including JS-SYNTAX
	syntax Bool ::= isKeyPresent( Map , K ) | isKeyAbsent( Map , K )
	syntax K ::= discard
	syntax K ::= replaceResult( K )
	
	rule [isKeyPresent]:
		isKeyPresent(M:Map, K:K) => $hasMapping(M, K) 
		[structural]

	rule [isKeyAbsent]:
		isKeyAbsent(M:Map, K:K) => notBool($hasMapping(M, K)) 
		[structural]
		
	rule [discard]:
	 	<k> Previous:V ~> discard => . ...</k>
	
	rule [replaceResult]:
		<k> Previous:V ~> replaceResult(Value:V) => Value ...</k>
		
endkm

kmod JS-TYPE is including JS-UTILITY
	syntax String ::= jsType( V )
	
	rule jsType($u)		 			=> "undefined"	[structural anywhere]
	rule jsType($null) 				=> "null"		[structural anywhere]
	rule jsType($b(_)) 				=> "boolean"	[structural anywhere]
	rule jsType($z(_)) 				=> "finite"		[structural anywhere]
	rule jsType($f(_)) 				=> "finite"		[structural anywhere]
	rule jsType($inf(_)) 			=> "infinite"	[structural anywhere]
	rule jsType($nan) 				=> "nan"		[structural anywhere]
	rule jsType($s(_)) 				=> "string"		[structural anywhere]
	rule jsType($obj(_)) 			=> "object"		[structural anywhere]
	rule jsType($binding(_,_))		=> "binding"	[structural anywhere]
	--- rule jsType($function(_,_,_,_))	=> "function"	[structural anywhere]
endkm

kmod JS-TYPEOF is including JS-TYPE
	syntax String ::= typeof( V )
	
	rule typeof($u) 				=> "undefined"	[structural]
	rule typeof($null) 				=> "object"		[structural]
	rule typeof($b(_)) 				=> "boolean"	[structural]
	rule typeof($z(_)) 				=> "number"		[structural]
	rule typeof($f(_)) 				=> "number"		[structural]
	rule typeof($inf(_)) 			=> "number"		[structural]
	rule typeof($nan) 				=> "number"		[structural]
	rule typeof($s(_)) 				=> "string"		[structural]
	rule typeof($obj(_)) 			=> "object"		[structural]
	
	--- ECMA-262 11.4.3
	rule [typeof]: $typeof(Value:V) => $s(typeof(Value))
	
	

	--- rule $typeof($null) 	=> $s("object")
	--- rule $typeof(Value:V) 	=> $s(jsType(Value)) when jsType(Value) =/=K "null"

	---	rule [typeof]: $typeof(Value:V)		=> $s(jsTypeNameOf(jsType(V)))
	--- rule [jsTypeNameOf]: jsTypeNameOf(Name:String)	=> 
	--- 	#if Name ==K "numeric" 
	--- 		#then "number" 
	--- 		#else #if Name ==K "null" 
	--- 			#then "object" 
	--- 			#else Name
	--- 		#fi
	--- 	#fi 	[structural]
endkm


kmod JS-TESTING is including JS-TYPEOF
	syntax Bool ::= isTruthy( V ) 
	syntax Bool ::= isFinite( V ) 
	syntax Bool ::= notNan( V ) 
	syntax Bool ::= isNumber( V ) 
	syntax Bool ::= notNumber( V ) 
	syntax Bool ::= isString( V ) 
	syntax Bool ::= notString( V ) 
	syntax Bool ::= isBinding( V ) 
	syntax Bool ::= notBinding( V ) 
	
	
	rule isTruthy($u) 					=> false		 [structural anywhere]
	rule isTruthy($null) 				=> false		 [structural anywhere]
	rule isTruthy($b(B:Bool)) 			=> B			 [structural anywhere]
	rule isTruthy($z(_)) 				=> false		 [structural anywhere]
	rule isTruthy($f(_)) 				=> true			 [structural anywhere]
	rule isTruthy($inf(_)) 				=> true			 [structural anywhere]
	rule isTruthy($nan) 				=> false		 [structural anywhere]
	rule isTruthy($s(S:String)) 		=> S =/=K ""	 [structural anywhere]
	rule isTruthy($obj(_)) 				=> true			 [structural anywhere]
	--- rule jsType($function(_,_,_,_))
	
	rule [isFinite]: isFinite(Value:V) 		=> jsType(Value) ==K "finite"	[structural anywhere]
	rule [notNan]: notNan(Value:V) 			=> jsType(Value) =/=K "nan"		[structural anywhere]
	
	rule [isNumber]: isNumber(Value:V) 		=> typeof(Value) ==K "number"	[structural anywhere]
	rule [notNumber]: notNumber(Value:V) 	=> typeof(Value) =/=K "number"	[structural anywhere]
	
	rule [isString]: notString(Value:V) 	=>	jsType(Value) ==K "string"	[structural anywhere]
	rule [notString]: notString(Value:V) 	=>	jsType(Value) =/=K "string"	[structural anywhere]
	
	rule [isBinding]: isBinding(Value:V)	=>	jsType(Value) ==K "binding"	[structural anywhere]
	rule [notBinding]: notBinding(Value:V)	=>	jsType(Value) =/=K "binding" [structural anywhere]
endkm

kmod JS-CONVERSIONS is including JS-TESTING
	syntax V ::= toPrimitive( K , String ) 
	syntax V ::= toBoolean( K )
	syntax V ::= toNumber( K )
	syntax V ::= toString( K )
	syntax V ::= toObject( K )
	syntax V ::= asObject ( K )
	syntax V ::= checkObjectCoercible( V )
	
	--- ECMA-262 8.12.8
---	rule defaultValue(Obj:JSObject, Hint:String) --- Not yet implemented!!!
	
	--- ECMA-262 9.1
	rule toPrimitive(Prim:V, Hint:String)	=> Prim 						[structural]
---	rule toPrimitive(Obj:JSObject, Hint)			=> defaultValue(Obj, Hint) [structural]
	
	--- ECMA-262 9.2
	rule toBoolean(Value:V) 		=> $b(isTruthy(Value)) 					[structural]
	
	--- ECMA-262 9.3
	rule toNumber(Value:V)			=> Value 		when isNumber(Value) 	[structural]
	rule toNumber($u) 				=> $nan									[structural]
	rule toNumber($null) 			=> $n(0)								[structural]
	rule toNumber($b(true)) 		=> $n(1)								[structural]
	rule toNumber($b(false)) 		=> $n(0)								[structural]
	rule toNumber($b(false)) 		=> $n(0)								[structural]
	rule toNumber($s(S:String)) 	=>  #if S ==K "" #then $n(0) #else 
										#if S ==K "Infinity" #then $inf(1) #else
										#if S ==K "-Infinity" #then $inf(-1) #else
										$n(String2Float(S)) #fi #fi #fi		[structural]
	--- TODOS: need to handle non-numeric strings !!!
	--- rule toNumber($obj(Loc:Int))	=> toNumber(toPrimitive($obj(Loc), "N")) [structural]
	--- NOTE: some objects (such as arrays & numbers) map to $n(0) or $n(n)  !!!
	
	--- ECMA-262 9.8
	rule toString($u)		 				=> $s("undefined")			[structural]
	rule toString($null) 					=> $s("null")				[structural]
	rule toString($b(true)) 				=> $s("true")				[structural]
	rule toString($b(false)) 				=> $s("false")				[structural]
	rule toString($nan) 					=> $s("NaN")				[structural]
	rule toString($f(F:Float)) 				=> $s(Float2String(F))		[structural]
	rule toString($inf(1)) 					=> $s("Infinity")			[structural]
	rule toString($inf(-1)) 				=> $s("-Infinity")			[structural]
	rule toString($z(_))					=> $s("0")					[structural]
	rule toString($s(S:String)) 			=> $s(S)					[structural]
---	rule toString(Obj:JSObject)				=> $s("[object Object]")	[structural]
	---	rule toString($s(S)) 			=> $invoke($staticAccess(newString(S),$s("toString")),$comma(.List{JS}))
	
	rule toObject(Value:V)	=> asObject(checkObjectCoercible(Value))
	
	--- rule asObject($b(B:Bool))		=>
	--- rule asObject(Num:V)			=> 		when isNumber(Num)
	--- rule asObject($s(S:String))	=>	
	rule asObject($obj(Loc:Int)) 	=> $obj(Loc)

	rule checkObjectCoercible($u)				=> $throw($TypeError("Cannot be undefined!"))
	rule checkObjectCoercible($null)			=> $throw($TypeError("Cannot be null!"))
	rule checkObjectCoercible($b(B:Bool))		=> $b(B)
	rule checkObjectCoercible(Num:V)			=> Num		when isNumber(Num)
	rule checkObjectCoercible($s(S:String))		=> $s(S)
	rule checkObjectCoercible($obj(Loc:Int)) 	=> $obj(Loc)
	
endkm 

kmod JS-LOGIC is including JS-CONVERSIONS
	--- TODO: write this in terms of using 'if' below.
	
	--- ECMA-262 11.11
	rule [and]: $and(Value:V, Exp:K)	=> #if isTruthy(Value) #then Exp #else Value #fi
	rule [or]:  $or(Value:V, Exp:K) 	=> #if isTruthy(Value) #then Value #else Exp #fi
	
endkm

kmod JS-CONDITIONAL is including JS-LOGIC
	--- TODO: add getValue calls in the right places
	
	syntax K ::= if( K , K , K ) [strict(1)]

	rule [if-true]:  <k> if(true, TrueAction:K, _) 		=> TrueAction ...</k>	[structural]
	rule [if-false]: <k> if(false, _, FalseAction:K) 	=> FalseAction ...</k>	[structural]
	
	--- ECMA-262 11.11
	rule [ternary]:
		<k> $ternary(Condition:V, TrueAction:K , FalseAction:K)
			=> if(isTruthy(Condition), TrueAction, FalseAction) ...</k>
		---	  => if isTruthy(Condition) then TrueExp else FalseExp fi

	rule [ifElse]:
		<k> $ifElse(Condition:V, TrueAction:K , FalseAction:K) 
			=> $ternary(Condition, TrueAction, 
						#if FalseAction ==K .None #then $u #else FalseAction #fi) ...</k>


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

kmod JS-ENV-REC is including JS-CONDITIONAL
	--- TO DOS: Add get set property methods.
		
	syntax K ::= 
			hasRecProperty( K , String ) 
		|	hasNoRecProperty( K , String ) 
---	syntax JSValue ::= 
	syntax K ::= 
			getRecValueIfAbsent( K , String , K ) 
		|	getEnsuredRecValue( K , String ) 
		|	copyRecWithValue( K , String , K ) 
		|	copyRecWithoutProperty( K , String ) 
	
	rule [hasRecProperty]:
		hasRecProperty($rec(Properties:Map), Name:String) 
		=> isKeyPresent(Properties, Name)  
		[structural anywhere]

	rule [hasNoRecProperty]:
		hasNoRecProperty($rec(Properties:Map), Name:String) 
		=> isKeyAbsent(Properties, Name)  
		[structural anywhere]

	rule [getRecValueIfAbsent-absent]:
		getRecValueIfAbsent($rec(Properties:Map), Name:String, AbsentAction:K) 
		=> AbsentAction
		when isKeyAbsent(Properties, Name)  
		[structural anywhere]

	rule [getRecValueIfAbsent-present]:
		getRecValueIfAbsent($rec(Name |-> Value:V Extra:Map), Name:String, _) 
		=> Value  
		[structural anywhere]


	rule [getEnsuredRecValue]:
		getEnsuredRecValue($rec(Properties:Map), Name:String) 
		=> Properties(Name)  
		[structural anywhere]

	rule [copyRecWithValue]:
		copyRecWithValue($rec(Properties:Map), Name:String, Value:V) 
		=> $rec(Properties[Value / Name])  
		[structural anywhere]
	
	rule [copyRecWithoutProperty-absent]:
		copyRecWithoutProperty(Rec:V, Name:String) => Rec
		when hasNoRecProperty(Rec, Name)  
		[structural anywhere]

	rule [copyRecWithoutProperty-present]:
		copyRecWithoutProperty($rec(Name |-> _ Extra:Map), Name:String) 
		=> $rec(Extra)
		[structural anywhere]
endkm

kmod JS-OBJ is including JS-ENV-REC + JS-CONFIGURATION
	syntax K ::= 
			hasProperty( K , String )		--- [strict]
		|	hasNoProperty( K , String )	--- [strict]
	syntax K ::= 
			newObj
		|	allocNewRec
		|	getEnsuredPropertyValue( K , String )
		|	getPropertyValueIfAbsent( K , String , K )
		|	getPropertyValue( K , String )
		|	setPropertyValue( K , String , K )
		|	deleteProperty( K , String )
	
	rule [newObj]: newObj => allocNewRec  [structural]
		
	rule [allocNewRec]:
		<k> allocNewRec => $obj(Loc) ...</k>
		<nextLoc> Loc:Int => Loc +Int 1 </nextLoc>  
		<heap> Heap:Map => Heap[$rec(.Map)/Loc]  </heap>
		[structural]
	
	--- ECMA-262 10.2.1.x.1 HasBinding(N)
	--- Determine if an environment record has a binding for an identifier. 
	--- Return true if it does and false if it does not. 
	--- The String value N is the text of the identifier.
	rule [hasProperty]:
		<k> hasProperty($obj(Loc:Int), Name:String) => hasRecProperty(Heap(Loc), Name) ...</k>
		<heap> Heap:Map </heap>  
		[structural]

	rule [hasNoProperty]:
		<k> hasNoProperty($obj(Loc:Int), Name:String) => hasNoRecProperty(Heap(Loc), Name) ...</k>
		<heap> Heap:Map </heap>  
		[structural]

	rule [getEnsuredPropertyValue]:
		<k> getEnsuredPropertyValue($obj(Loc:Int), Name:String) 
			=> getEnsuredRecValue(Heap(Loc), Name) ...</k>
		<heap> Heap:Map </heap>  
		[structural]
	
	rule [getPropertyValueIfAbsent]:
		<k> getPropertyValueIfAbsent($obj(Loc:Int), Name:String, AbsentAction:K) 
			=> getRecValueIfAbsent(Heap(Loc), Name, AbsentAction) ...</k>
		<heap> Heap:Map </heap>  
		[structural]
	
	--- ECMA-262 10.2.1.x.4 GetBindingValue(N,S)
	--- Returns the value of an already existing binding from an environment record. 
	--- The String value N is the text of the bound name. S is used to identify strict mode 
	--- references. If S is true and the binding does not exist or is uninitialized throw a 
	--- ReferenceError exception.
	rule [getPropertyValue]:
		getPropertyValue(Ref:V, Name:String) 
		=> getPropertyValueIfAbsent(Ref, Name, $u)
		[structural]
	
	--- ECMA-262 10.2.1.x.2 CreateMutableBinding(N, D)
	--- Create a new mutable binding in an environment record. 
	--- The String value N is the text of the bound name. 
	--- If the optional Boolean argument D is true the binding is may be subsequently deleted.
	
	--- ECMA-262 10.2.1.x.3 SetMutableBinding(N,V, S)
	--- Set the value of an already existing mutable binding in an environment record. 
	--- The String value N is the text of the bound name. V is the value for the binding and 
	--- may be a value of any ECMAScript language type. S is a Boolean flag. 
	--- If S is true and the binding cannot be set throw a TypeError exception. 
	---- S is used to identify strict mode references.
	rule [setPropertyValue]:
		<k> setPropertyValue($obj(Loc:Int), Name:String, Value:V) => Value ...</k>
		<heap>... Loc |-> (Rec:V => copyRecWithValue(Rec, Name, Value)) ...</heap>  
		[structural]
	
	--- ECMA-262 10.2.1.x.5 DeleteBinding(N)
	--- Delete a binding from an environment record. 
	--- The String value N is the text of the bound name If a binding for N exists, 
	--- remove the binding and return true. If the binding exists but cannot be removed return 
	--- false. If the binding does not exist return true.
	rule [deleteProperty]:
		<k> deleteProperty($obj(Loc:Int), Name:String) => . ...</k> 
		<heap>... Loc |-> (Rec:V => copyRecWithoutProperty(Rec, Name)) ...</heap>  
		[structural]

	--- TODOS: add Strict
	--- TODOS: add property descs (immutability, configurability, enumerability)
	--- TODOS: enable property key to be other than strings

	--- ECMA-262 10.2.1.x.6 ImplicitThisValue()
	--- Returns the value to use as the this value on calls to function objects that are obtained as binding values from this environment record.
	--- 
	--- ECMA-262 10.2.1.1.7 CreateImmutableBinding(N)
	--- Create a new but uninitialized immutable binding in an environment record. The String value N is the text of the bound name.
	--- ECMA-262 10.2.1.1.8 InitializeImmutableBinding(N,V)
	--- Set the value of an already existing but uninitialized immutable binding in an environment record. The String value N is the text of the bound name. V is the value for the binding and is a value of any ECMAScript language type.

endkm

kmod JS-IDENTIFIER-RESOLUTION is including JS-OBJ
	syntax K ::= resolveIdentifier( K , String )
	syntax K ::= getTarget( V )
	syntax K ::= getPropertyName( V )
	
		
	--- ECMA-262 10.2.2.1	GetIdentifierReference (lex, name, strict)
	--- GetPropertyReference
	rule [resolveIdentifier-unresolved]:
		<k> resolveIdentifier(.None, Name:String) => $binding(.None, Name) ...</k>
		[structural]
		
	rule [resolveIdentifier]:
		<k> resolveIdentifier($env(Outer:K, Obj:V), Name:String) 
			=> if(hasProperty(Obj, Name), $binding(Obj, Name), 
					resolveIdentifier(Outer, Name)) ...</k>
		[structural]
	
	--- ECMA-262 8.7
	rule [getTarget]:
		getTarget($binding(Target:V, _)) => Target
		[structural]
	
	rule [getPropertyName]:
		getPropertyName($binding(_, Name:String)) => Name
		[structural]

	--- ECMA-262 10.3.1  Identifier Resolution
	rule [identifier-resolution]:
		<k> $id(Name:String) => resolveIdentifier(Env, Name) ...</k>
		<lexEnv> Env:K </lexEnv>
	
endkm

kmod JS-VALUE-ACCESS is including JS-IDENTIFIER-RESOLUTION	
	syntax V ::= 
			getValue( K )		[strict]
		|	putValue( K , K )	[strict]
	
	
	
	
	--- ECMA-262 8.7.1
	rule [getValue-value]: 
		getValue(Value:V) => Value	when notBinding(Value)							[structural]
		
	rule [getValue-unresolved]: 
		getValue($binding(.None, Name:String)) 
		=> $throw($ReferenceError("Unresolvable property reference: " +String Name +String "!"))  
		[structural]
		
	rule [getValue-identifier]:
		getValue($binding(Obj:V, Name:String)) => getPropertyValue(Obj, Name)		[structural]



	--- ECMA-262 8.7.2
	rule [putValue-error]:
		<k> putValue(Value:V, Name:String) 
			=> $throw($ReferenceError(
				"Cannot assign property named -> " +String Name +String " to value!")) ...</k>
		when notBinding(Value)
		[structural]

	rule [putValue]:
		<k> putValue($binding(Obj:V, Name:String), Value:V) 
			=> setPropertyValue(
				#if Obj ==K .None #then GlobalObj #else Obj #fi, 
				Name, Value) ...</k>
		<globalEnv> GlobalObj:V </globalEnv> 
		[structural]

	--- rule [putValue-primitive-base]:
	--- 	<k> putValue($binding(Target:JSPrimitiveTarget, Name:String), Value:JSValue) 
	--- 		=> setPropertyValue(Obj, Name, Value) ...</k>
	--- 	<lexEnv> $env(_, Obj:JSRef) </lexEnv> 
	--- 	[structural]
endkm


kmod JS-BASIC-SEMANTICS is 
    including JS-VALUE-ACCESS
	--- + JS-IDENTITY
	--- + JS-EQUALITY
endkm
