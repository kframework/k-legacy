kmod JSON-TO-PREFIX is including JS-UTILITY + JSON + JS-JSON-AST-IDS
	
	syntax K ::= j2p( K ) --- json2prefix( K )
	syntax K ::= buildEachPrefix( List{K} , List{K} )

	--- macro undefined = .none
	rule j2p( null ) => .None														[structural]
	
	rule j2p( a(Remaining:List{K}) ) => buildEachPrefix( Remaining, .List{K} )
	rule buildEachPrefix( .List{K} , Complete:List{K} ) => Complete					[structural]
	rule buildEachPrefix( (Element:K ,, Remaining:List{K}) , Complete:List{K} ) 
		=> buildEachPrefix( Remaining , (Complete ,, j2p(Element)) )				[structural]
		
	rule j2p( o(type |-> "NUMBER" value |-> Num:MaudeNumber M:Map) ) => $n(Num) 	[structural]
	rule j2p( o(type |-> "STRING" value |-> S:String M:Map) ) 	=> $s(S)			[structural]
	rule j2p( o(type |-> "REGEXP" value |-> S:String M:Map) ) 	=> $regex(S)		[structural]
	rule j2p( o(type |-> "TRUE"  M:Map) ) 						=> $b(true)			[structural]
	rule j2p( o(type |-> "FALSE" M:Map) ) 						=> $b(false)		[structural]
	rule j2p( o(type |-> "NULL" M:Map) ) 						=> $null			[structural]
	rule j2p( o(type |-> "THIS" M:Map) ) 						=> $this			[structural]


	--- rule j2p(Json:K) => $n(Json(value)) 	when isNodeType(Json, "NUMBER")	[structural]
	--- rule j2p(Json:K) => $s(Json(value)) 	when isNodeType(Json, "STRING")	[structural]
	--- rule j2p(Json:K) => $regex(Json(value)) when isNodeType(Json, "REGEXP")	[structural]
	--- rule j2p(Json:K) => $b(true) 			when isNodeType(Json, "TRUE")		[structural]
	--- rule j2p(Json:K) => $b(false) 			when isNodeType(Json, "FALSE")	[structural]
	--- rule j2p(Json:K) => $null 				when isNodeType(Json, "NULL")		[structural]
	--- rule j2p(Json:K) => $this 				when isNodeType(Json, "THIS")		[structural]
	--- 
	--- rule j2p(Json:K) => $initNewId( Json(value), j2p(Json(initializer)) ) 	
	--- 	when isNodeType(Json, "IDENTIFIER") andBool isKeyPresent(Json, initializer)	[structural]
	--- 
	--- rule j2p(Json:K) => $id( Json(value) )
	--- 	when isNodeType(Json, "IDENTIFIER") andBool isKeyAbsent(Json, initializer)	[structural]


	rule j2p( o(type |-> "IDENTIFIER" value |-> Name:String Properties:Map) ) 
		=> #if isKeyAbsent(Properties, initializer) #then $id( Name )
			#else $initNewId( Name, j2p(Properties(initializer)) ) #fi 				[structural]

	rule j2p( o(type |-> "DOT" children |-> a(Left:K ,, Right:K) M:Map) ) 
		=> $staticAccess( j2p(Left) , j2p(Right) )									[structural]

	rule j2p( o(type |-> "INDEX" children |-> a(Left:K ,, Right:K) M:Map) )
		=> $dynamicAccess( j2p(Left) , j2p(Right) )									[structural]
	
	rule j2p( o(type |-> "CALL" children |-> a(FunctionExp:K ,, ArgList:K) M:Map) )
		=> $invocation( j2p(FunctionExp), j2p(ArgList) )							[structural]

---	rule o(type |-> "NEW" children |-> a(Exp) M:Map) = $new(Exp)
	rule j2p( o(type |-> "NEW_WITH_ARGS" children |-> Exps:K M:Map) )
		=> $new( j2p(Exps) )														[structural]
	
	rule j2p( o(type |-> "INCREMENT" children |-> Exp:K Properties:Map) ) 
		=> #if isKeyAbsent(Properties, postfix) #then $inc( j2p(Exp), false )
			#else $inc( j2p(Exp), Properties(postfix) ) #fi							[structural]
	
	rule j2p( o(type |-> "DECREMENT" children |-> Exp:K Properties:Map) ) 
		=> #if isKeyAbsent(Properties, postfix) #then $dec( j2p(Exp), false )
			#else $dec( j2p(Exp), Properties(postfix) ) #fi							[structural]

	rule j2p( o(type |-> "DELETE" children |-> Exp:K M:Map) ) 
		=> $delete( j2p(Exp) )														[structural]
	
	rule j2p( o(type |-> "TYPEOF" children |-> Exp:K M:Map) ) 
		=> $typeof( j2p(Exp) )														[structural]
	rule j2p( o(type |-> "NOT" children |-> Exp:K M:Map) ) 
		=> $not( j2p(Exp) )															[structural]
	rule j2p( o(type |-> "UNARY_MINUS" children |-> Exp:K M:Map) ) 
		=> $neg( j2p(Exp) )															[structural]
	rule j2p( o(type |-> "UNARY_PLUS" children |-> Exp:K M:Map) ) 
		=> $plus( j2p(Exp) )														[structural]
	
	rule j2p( o(type |-> "MUL" children |-> a(Left:K ,, Right:K) M:Map) ) 
		=> $mul( j2p(Left) , j2p(Right) )											[structural]
	rule j2p( o(type |-> "DIV" children |-> a(Left:K ,, Right:K) M:Map) ) 
		=> $div( j2p(Left) , j2p(Right) )											[structural]
	rule j2p( o(type |-> "MOD" children |-> a(Left:K ,, Right:K) M:Map) ) 
		=> $mod( j2p(Left) , j2p(Right) )											[structural]
	
	rule j2p( o(type |-> "PLUS" children |-> a(Left:K ,, Right:K) M:Map) )
		=> $add( j2p(Left) , j2p(Right) )											[structural]
	rule j2p( o(type |-> "MINUS" children |-> a(Left:K ,, Right:K) M:Map) )
		=> $sub( j2p(Left) , j2p(Right) )											[structural]
	
	rule j2p( o(type |-> "LT" children |-> a(Left:K ,, Right:K) M:Map) )
		=> $lt( j2p(Left) , j2p(Right) )											[structural]
	rule j2p( o(type |-> "LE" children |-> a(Left:K ,, Right:K) M:Map) ) 
		=> $lte( j2p(Left) , j2p(Right) )											[structural]
	rule j2p( o(type |-> "GT" children |-> a(Left:K ,, Right:K) M:Map) ) 
		=> $gt( j2p(Left) , j2p(Right) )											[structural]
	rule j2p( o(type |-> "GE" children |-> a(Left:K ,, Right:K) M:Map) ) 
		=> $gte( j2p(Left) , j2p(Right) )											[structural]
	rule j2p( o(type |-> "IN" children |-> a(Left:K ,, Right:K) M:Map) ) 
		=> $in( j2p(Left) , j2p(Right) )											[structural]
	rule j2p( o(type |-> "INSTANCEOF" children |-> a(Left:K ,, Right:K) M:Map) ) 
		=> $instanceof( j2p(Left) , j2p(Right) )									[structural]
		
	rule j2p( o(type |-> "EQ" children |-> a(Left:K ,, Right:K) M:Map) ) 
		=> $sameAs( j2p(Left) , j2p(Right) )										[structural]
	rule j2p( o(type |-> "NE" children |-> a(Left:K ,, Right:K) M:Map) ) 
		=> $notSameAs( j2p(Left) , j2p(Right) )										[structural]
	rule j2p( o(type |-> "STRICT_EQ" children |-> a(Left:K ,, Right:K) M:Map) ) 
		=> $identical( j2p(Left) , j2p(Right) )										[structural]
	rule j2p( o(type |-> "STRICT_NE" children |-> a(Left:K ,, Right:K) M:Map) ) 
		=> $notIdentical( j2p(Left) , j2p(Right) )									[structural]
	
	rule j2p( o(type |-> "AND" children |-> a(Left:K ,, Right:K) M:Map) ) 
		=> $and( j2p(Left) , j2p(Right) )											[structural]
	
	rule j2p( o(type |-> "OR" children |-> a(Left:K ,, Right:K) M:Map) ) 
		=> $or( j2p(Left) , j2p(Right) )											[structural]
	
	rule j2p( o(type |-> "HOOK" children |-> a(Left:K ,, Middle:K ,, Right:K) M:Map) ) 
		=> $ternary( j2p(Left) , j2p(Middle) , j2p(Right) )							[structural]
	
	rule j2p( o(type |-> "ASSIGN" assignOp |-> null 
						 children |-> a(Left:K ,, Right:K) M:Map) )
		=> $assignment( j2p(Left) , j2p(Right) )									[structural]
	rule j2p( o(type |-> "ASSIGN" assignOp |-> OpCode:Nat 
						 children |-> a(Left:K ,, Right:K) M:Map) )
		=> $compoundAssignment( j2p(Left) , OpCode , j2p(Right) )					[structural]
	
	rule j2p( o(type |-> "ARRAY_INIT" children |-> Children:K M:Map) ) 
		=> $arrayLit( j2p(Children) )												[structural]
	rule j2p( o(type |-> "OBJECT_INIT" children |-> Children:K M:Map) ) 
		=> $objectLit( j2p(Children) )												[structural]
	rule j2p( o(type |-> "PROPERTY_INIT" children |-> a(Left:K ,, Right:K) M:Map) ) 
		=> $propertyLit( j2p(Left) , j2p(Right) )									[structural]
		
	rule j2p( o(type |-> "RETURN" value |-> Exp:K M:Map) ) 
		=> $return( j2p(Exp) )														[structural]
	rule j2p( o(type |-> "BREAK" label |-> Name:String Properties:Map) ) 
		=>  #if isKeyAbsent(Properties, label) #then $break(.None) 
			#else $break(Properties(label)) #fi										[structural]
	rule j2p( o(type |-> "CONTINUE" label |-> Name:String Properties:Map) ) 
		=>  #if isKeyAbsent(Properties, label) #then $continue(.None) 
			#else $continue(Properties(label)) #fi									[structural]
	rule j2p( o(type |-> "THROW" exception |-> Exp:K M:Map) ) 
		=> $throw( j2p(Exp) )														[structural]
	
	rule j2p( o(type |-> "BLOCK" children |-> Children:K M:Map) ) 
		=> $block( j2p(Children) )													[structural]
	
	rule j2p( o(type |-> "SEMICOLON" expression |-> Exp:K M:Map) ) 
		=> $exp( j2p(Exp) )															[structural]
		
	rule j2p( o(type |-> "LIST" children |-> Children:K M:Map) ) 
		=> $comma( j2p(Children) )													[structural]
	rule j2p( o(type |-> "COMMA" children |-> Children:K M:Map) ) 
		=> $comma( j2p(Children) )													[structural]
	
	rule j2p( o(type |-> "IF" condition |-> Condition:K 
						 thenPart |-> TrueStmt:K elsePart |-> FalseStmt:K M:Map) )
		=> $ifElse( j2p(Condition) , j2p(TrueStmt) , j2p(FalseStmt) )				[structural]
	rule j2p( o(type |-> "TRY" tryBlock |-> Block:K catchClauses |-> Clauses:K Properties:Map) )
		=> $try( j2p(Block) , j2p(Clauses) ,
				#if isKeyAbsent(Properties, finallyBlock) #then .None 
				#else j2p(Properties(finallyBlock)) #fi )								[structural]

	rule j2p( o(type |-> "CATCH" varName |-> Exp:K block |-> Stmt:K M:Map) ) 
		=> $catch( j2p(Exp) , j2p(Stmt) )											[structural]
	
	rule j2p( o(type |-> "CASE" caseLabel |-> Exp:K statements |-> Stmt:K M:Map) ) 
		=> $case( j2p(Exp) , j2p(Stmt) )											[structural]
	rule j2p( o(type |-> "DEFAULT" statements |-> Stmt:K M:Map) ) 
		=> $default( j2p(Stmt) )													[structural]
	
	rule j2p( o(type |-> "SWITCH" discriminant |-> Exp:K cases |-> Cases:K 
						 defaultIndex |-> I:Int M:Map) )
		=> $switch( j2p(Exp) , j2p(Cases) , I)										[structural]
	
	rule j2p( o(type |-> "FOR" setup |-> Setup:K condition |-> Condition:K 
						 update |-> Exp:K body |-> Stmt:K M:Map) )
		=> $for( j2p(Setup) , j2p(Condition) , j2p(Exp), j2p(Stmt) )				[structural]
	rule j2p( o(type |-> "FOR_IN" iterator |-> Left:K object |-> Right:K 
						 body |-> Stmt:K M:Map) )
		=> $forIn( j2p(Left) , j2p(Right) , j2p(Stmt) )								[structural]
	
	rule j2p( o(type |-> "WHILE" condition |-> Condition:K body |-> Stmt:K M:Map) ) 
		=> $while( j2p(Condition) , j2p(Stmt) )										[structural]
	rule j2p( o(type |-> "DO" condition |-> Condition:K body |-> Stmt:K M:Map) ) 
		=> $do( j2p(Condition) , j2p(Stmt) )										[structural]
	
	rule j2p( o(type |-> "LABEL" label |-> Name:String statement |-> Stmt:K M:Map) ) 
		=> $label(Name , j2p(Stmt) )												[structural]
	
	rule j2p( o(type |-> "VAR" children |-> Children:K M:Map) ) 
		=> $varDeclarations( j2p(Children) )										[structural]
	
	rule j2p( o(type |-> "FUNCTION" functionForm |-> N:Nat 
						 params |-> a(Params:List{K}) body |-> Stmt:K Properties:Map) )
		=> #if isKeyAbsent(Properties, name) #then $function( N , "" , Params , j2p(Stmt) )
			#else $function( N , Properties(name) , Params , j2p(Stmt) ) #fi			[structural]
	
	rule j2p( o(type |-> "SCRIPT" children |-> Children:K 
						 varDecls |-> VarDecls:K funDecls |-> FunDecls:K M:Map) ) 
		=> $program( j2p(VarDecls), j2p(FunDecls), j2p(Children) )					[structural]
	
endkm

