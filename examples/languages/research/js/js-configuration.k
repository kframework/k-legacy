kmod JS-CONFIGURATION is including JS-SYNTAX
	syntax K ::= startJS ( K )
	
	configuration <T> 
		<k>		startJS($PGM:K)		</k> 
		<lexEnv>	.None		</lexEnv>
		<globalEnv>	.None		</globalEnv>
		<heap>		.Map		</heap> 
		<nextLoc>	0			</nextLoc> 
		<fstack>	.List		</fstack>
		--- <in>      .List		</in> 
		--- <out>     "void"	</out> 
		<in stream="stdin">      .List	</in> 
		<out stream="stdout">    .List	</out>
	</T>
	--- <result> "" </result>
endkm