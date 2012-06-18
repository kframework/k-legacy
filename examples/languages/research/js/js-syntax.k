kmod JS-BASIS is 
	---- including PL-BUILTINS
	syntax V ::= Int | Float | Bool | String | Id
	syntax KResult ::= V
endkm

kmod JS-NUMERICS is including JS-BASIS
	--- JS has both positive and negative infinities and zeroes.
	syntax MaudeNumber ::= Int | Float
	syntax K ::= $n( MaudeNumber )
	syntax V ::= $f( Float )
	syntax V ::= $z( Int )
	syntax V ::= $inf( Int )
	syntax V ::= $nan
	
	syntax Int ::= sign( V )
	syntax Float ::= finiteValue( V )
	
	rule [sign-zero]: 		sign($z(Sign:Int)) 		=> Sign 		[structural anywhere]
	rule [sign-infinity]: 	sign($inf(Sign:Int))	=> Sign 		[structural anywhere]
	rule [sign-finite]: 	sign($f(F:Float)) => 
							#if F >=Float 0.0 #then 1 #else -1 #fi	[structural anywhere]
	
	--- rule [finiteValue-int]:		I:Int				 => Int2Float(I) [structural anywhere]
	--- rule [finiteValue-float]:	F:Float				 => F 			[structural anywhere]
	rule [finiteValue-finite]: 	finiteValue($f(F:Float)) => F 			[structural anywhere]
	rule [finiteValue-zero]: 	finiteValue($z(_)) 		 => 0.0 		[structural anywhere]
 
	rule [n-int-to-n-float]: 			$n(I:Int) => $n(Int2Float(I)) 	[structural anywhere]
	rule [n-float-zero-to-zero]:		$n(0.0) => $z(1)				[structural anywhere]
---	rule [neg-z-to-zero]:				$n(-0.0) => $z(-1)				[structural anywhere]
	rule [n-float-nonzero-to-finite]: 	$n(F:Float) => 
										$f(F) when F =/=K 0.0			[structural anywhere]
	
	--- TO DO !!!
	---	macro $n(-0.0) 		= $zero(-1)
	--- macro $n(Infinity) 	= $infinity(1)
	--- macro $n(-Infinity) = $infinity(-1)
endkm

kmod JS-PRIMITIVES is including JS-NUMERICS
	syntax V ::= $undefined
	syntax V ::= $null
	
	syntax V ::= $b( Bool )
	syntax V ::= $s( String )

	syntax Bool ::= boolValue( V )
	syntax String ::= stringValue( V )
	
	rule [boolValue]:	boolValue($b(B:Bool)) 		=> B	[structural anywhere]
	rule [stringValue]:	stringValue($s(S:String))	=> S	[structural anywhere]
endkm

kmod JS-SYNTAX is including JS-PRIMITIVES + K
	--- ECMA-262 8.7
	syntax V ::= .None
	syntax V ::= $binding( V , String )
	
	syntax V ::= $regex( String ) --- $o( Map ) --- | $function!!!
	syntax V ::= $ReferenceError( String )
	syntax V ::= $TypeError( String )
	
	syntax V ::= .Rec | $rec( Map )
	syntax V ::= $obj( Int )
	
	syntax K ::= $env( K , K )
	syntax K ::= $id ( String )
	
	syntax List{K} ::= .Empty
	
	macro .Empty = .List{K}  --- Maybe make this a rule !!!
	
	
	syntax K ::= 
		  $this
		| $nop
		| $initNewId ( String , K )				--- String = InitExp 
		| $staticAccess ( K , K ) [seqstrict] 		--- Exp . Name
		| $dynamicAccess ( K , K ) [seqstrict] 	--- Exp [ Exp ]
		| $invoke ( K , K ) [seqstrict] 		--- Exp ( CommaList )
		
		| $new ( List{K} ) [seqstrict]				--- new InvocationExp
		
		| $inc ( K , Bool ) [strict]			--- ++exp OR exp++
		| $dec ( K , Bool ) [strict]			--- --exp OR exp--
		
		| $delete ( K ) [strict]				--- delete TargetExp
		
		| $typeof ( K ) [strict]				--- typeof Exp
		| $not ( K ) [strict]					--- ! Exp
		| $neg ( K ) [strict] 					--- - Exp
		| $plus ( K ) [strict] 					--- + Exp
		
		| $mul ( K , K ) [seqstrict] 				--- Exp * Exp
		| $div ( K , K ) [seqstrict] 				--- Exp / Exp
		| $mod ( K , K ) [seqstrict] 				--- Exp % Exp
		
		| $add ( K , K ) [seqstrict]				--- Exp + Exp
		| $sub ( K , K ) [seqstrict] 				--- Exp - Exp
		
		| $lt ( K , K ) [seqstrict] 				--- Exp < Exp
		| $lte ( K , K ) [seqstrict] 				--- Exp <= Exp
		| $gt ( K , K ) [seqstrict] 				--- Exp > Exp
		| $gte ( K , K ) [seqstrict] 				--- Exp >= Exp
		| $in ( K , K ) [seqstrict]				--- Exp in Exp
		| $instanceof ( K , K ) [seqstrict] 		--- Exp instanceof Exp
		
		| $equal ( K , K ) [seqstrict] 			--- Exp == Exp
		| $notEqual ( K , K ) [seqstrict] 		--- Exp != Exp
		| $identical ( K , K ) [seqstrict] 		--- Exp === Exp
		| $notIdentical ( K , K ) [seqstrict] 		--- Exp !== Exp
		
		| $and ( K , K ) [strict(1)] 			--- Exp && Exp
		
		| $or ( K , K ) [strict(1)] 			--- Exp || Exp
		
		| $ternary ( K , K , K ) [strict(1)]	--- Exp ? Exp : Exp
		
		
		| $assignment ( K , K ) [seqstrict]		--- TargetExp Exp
		| $compoundAssignment ( K , Nat , K ) [seqstrict]	--- TargetExp AssignmentOp Exp
		| $arrayLit ( List{K} )						--- [ CommaList{Exp} ]
		| $objectLit ( List{K} )					--- { CommaList{Property} }
		| $propertyLit ( K , K )					--- PropertyName : Exp
		
		| $return ( K ) [strict]				--- return Exp | .
		| $break ( K ) [strict] 				--- break Label | .
		| $continue ( K ) [strict] 				--- continue Label | .
		| $throw ( K ) [strict]					--- throw Exp
		| $label ( String , K ) 
		
		| $exp ( K ) [strict]					--- exp
		
		| $stmts ( List{K} ) 					--- List{Stmt}
		| $block ( List{K} )
		| $comma ( List{K} )
		| $list ( List{K} )						--- for invocation parameters
		| $ifElse ( K , K , K ) [strict(1)]		--- if (Exp) Block Block
		| $try ( K , List{K} , K ) [seqstrict]		--- 
		| $catch ( K , K )
		
		| $case ( K , K ) [strict(1)]	 		--- case Exp : Stmt
		| $default ( K ) 						--- default : Stmt
		
		| $switch ( K , List{K} , Int ) [strict(1)]	
			--- switch (Exp) { List{Case} ; DefaultCase }
		| $for ( K , K , K , K ) [strict(1)]		
			--- for ( Inits ; Cond ; Incrs ) Block
		| $forIn ( K , K , K ) [strict(2)]
			--- for ( Name in Exp ) Block
		| $while ( K , K )
			--- while ( Exp ) Block
		| $do ( K , K )
			--- do Block while ( Exp )
			
		| $labelledStmt ( K , K )	
			--- Label : Stmt
		
		| $varDeclarations ( List{K} )
		
		| $function ( Nat , String , List{K} , List{K} )
			--- function ( Parameters ) { Statements }
			--- function Name ( Parameters ) { Statements }

		| $program ( List{K} , List{K} , List{K} )
			--- List{VarDeclaration} List{Function} List{ExecutionStmt} 
endkm
