kmod JS-PROGRAMS is including JS-SEMANTICS   --- + JSON-TO-PREFIX
	syntax K ::= program1 | program2 | program3 | program4 
		| programEx1 | programEx3 | programEx4 | programEx5 

---	macro program1 = $mul($add($n(2), $n(3)), $n(4))
	

	macro program1 = 
		$program(
			.List{K},
			.List{K},
			$assignment($id("x"),$n(1)))
	
	--- var x, y, z;
	--- x = 1;			
	macro program2 = 
		$program(
			$id("x"),,$id("y"),,$id("z"),
			.List{K},
			$varDeclarations($id("x"),,$id("y"),,$id("z")),,
			$exp($assignment($id("x"),$n(1))))

	--- v = 234;
	--- first = if (false) {}
	--- second = if (111) {x = undefined;} else {null, undefined;}
	--- third = if (0) {x = second;} else {first, v} 
	macro program3 =
		$program(
			.List{K},
			.List{K},
			$assignment($id("v"),$n(234)),,
			$assignment($id("first"),$ifElse($b(false),$block(.List{K}),.None)),,
			$assignment($id("second"),$ifElse(
				$n(111),
				$block($exp($assignment($id("x"),$id("undefined")))),
				$block($exp($comma($null,,$id("undefined")))))),,
			$assignment($id("third"),$ifElse(
				$n(0),
				$block($exp($assignment($id("x"),$id("second")))),
				$block($exp($comma($id("first"),,$id("v")))))))	
	
	--- var count = elements.length;
	--- var index, element, separator;
	--- var accumulator = accumulator__ || [];
	--- 
	--- separator_ = separator_ || ',';
	
	macro programEx1 =
		$program(
			$initNewId("count",$staticAccess($id("elements"),$id("length"))),,
				$id("index"),,$id("element"),,$id("separator"),,
				$initNewId("accumulator",$or($id("accumulator__"),$arrayLit(.List{K}))),
			.List{K},
			$varDeclarations($initNewId("count",$staticAccess($id("elements"),$id("length")))),,
			$varDeclarations($id("index"),,$id("element"),,$id("separator")),,
			$varDeclarations(
				$initNewId("accumulator",$or($id("accumulator__"),$arrayLit(.List{K})))),,
			$exp($assignment($id("separator_"),$or($id("separator_"),$s(","))))) 



	
endkm

