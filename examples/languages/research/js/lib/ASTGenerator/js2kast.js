/*global AnExpectedGlobal: true, AGlobalNotToOverwritten: false,
		jasmine:true, netscape: true, FileIO: true, DirIO: true, 
		JSAST: true, Narcissus: true */
/*jshint smarttabs:true */

JSAST = (function () {
	"use strict";

	/*jshint bitwise:true, curly:true, eqeqeq:true, forin:true, latedef:true, 
	  newcap:true, noarg:true, nonew:true, noempty:true, regexp:true, undef:true, 
	  globalstrict:true, plusplus:false, expr:true, lastsemic:true,
	  browser:true, jquery:true, maxerr:50, 
	  immed:true, validthis:true, newcap:false
	*/
	// trailing:false

	
	var WORKING_DIR = "/Users/m3rabb/Dev/Maude/k2/examples/languages/research/js/",
		_Parser = Narcissus.parser,
		_Node = _Parser.Node,
		_definitions = Narcissus.definitions,
		_operatorSymbolsArray = _definitions.tokens,
		_operatorNamesDictionary = _definitions.opTypeNames,
		_processors;


	function _tokenNameAt(index) {
        var symbol = _operatorSymbolsArray[index];
		// if (symbol.toUpperCase === undefined) debugger;
		return _operatorNamesDictionary[symbol] || symbol.toUpperCase();
    }
			
	function _createProcessors() {
		function _createNamedParameterBuilder(_PropertyNames) {
			return function buildFromNamedProperties(_Node) {
				return _PropertyNames.map(function (propertyName) {return _Node[propertyName];});
			};
		}
		
		// function _emptyParamsBuilder(node) {return [];};
		
		function _createFlattenedParameterBuilder(_PropertyName) {
			return function buildFlattenedFromProperty(node) {
				return node[_PropertyName];
			};
		}

		function _shouldFlatten(spec) {return spec[spec.length - 1] === "*";}

		function _createProcessor(_PrefixName, _ParameterBuilder) {
			return function processor(node) {
				return this.process(_PrefixName, _ParameterBuilder(node));
			};
		}

		function _createProcessorFrom(spec) {
			var propertyNames, prefixName, parameterBuilder;
			propertyNames = spec.match(/[\$\()\w]+/ig);
			prefixName = propertyNames.shift();
			parameterBuilder = _shouldFlatten(spec) ? 
				_createFlattenedParameterBuilder(propertyNames.pop()) : 
				_createNamedParameterBuilder(propertyNames);
			return _createProcessor(prefixName, parameterBuilder);
		}
		
		
		var assignProcessor = function processor(node) {
			var operatorId, params, prefixName;
			operatorId = node.assignOp;
			params = node.children;
			prefixName = '$assignment';
			if (operatorId !== null) { 
				prefixName = '$compoundAssignment';
				params.splice(1, 0, _operatorSymbolsArray[operatorId]);
			}
			return this.process(prefixName, params);
		};

		var identifierProcessor = function processor(node) {
			var name = node.value,
				params = [name],
				initializer = node.initializer,
				prefixName = '$id';
			if (initializer) {
				prefixName = '$initNewId';
				params.push(initializer);
			}
			return this.process(prefixName, params);
		};
		
		var booleanProcessor = function processor(node) {
			return this.process("$b", [node.value === "true"]);
		};
				
		var incrementProcessor = function processor(node, typeName_) {
			var prefix, target, isPostfix;
			prefix = (typeName_ = "INCREMENT") ? "$inc" : "$dec";
			target = node.children[0];
			isPostfix = !!node.postfix;
			return this.process(prefix, [target, isPostfix]);
		};
		
		var processors = {
			'NUMBER'		: '$n                     value',
			'STRING'		: '$s                     value',
			'REGEXP'		: '$regex                 value',
			'TRUE'			: booleanProcessor,
			'FALSE'			: booleanProcessor,
			'NULL'			: '$null',
			'THIS'			: '$this',
			'IDENTIFIER'	: identifierProcessor,
			'DOT'			: '$staticAccess          children*',
			'INDEX'			: '$dynamicAccess         children*',
			'CALL'			: '$invoke                children*',
			// 'NEW'	: '$new                   children',
			'NEW_WITH_ARGS'	: '$new                   children',
			'INCREMENT'		: incrementProcessor,
			'DECREMENT'		: incrementProcessor,
			'DELETE'		: '$delete                children*',
			'TYPEOF'		: '$typeof                children*',
			'NOT'			: '$not                   children*',
			'UNARY_MINUS'	: '$neg                   children*',
			'UNARY_PLUS'	: '$plus                  children*',
			'MUL'			: '$mul                   children*',
			'DIV'			: '$div                   children*',
			'MOD'			: '$mod                   children*',	
			'PLUS'			: '$add                   children*',
			'MINUS'			: '$sub                   children*',
			'LT'			: '$lt                    children*',
			'LE'			: '$lte                   children*',
			'GT'			: '$gt                    children*',
			'GE'			: '$gte                   children*',
			'IN'			: '$in                    children*',
			'INSTANCEOF'	: '$instanceof            children*',
			'EQ'			: '$equal                 children*',
			'NE'			: '$notEqual              children*',
			'STRICT_EQ'		: '$identical             children*',
			'STRICT_NE'		: '$notIdentical          children*',
			'AND'			: '$and                   children*',
			'OR'			: '$or                    children*',
			'HOOK'			: '$ternary               children*',
			'ASSIGN'		: assignProcessor,
			'ARRAY_INIT'	: '$arrayLit              children',
			'OBJECT_INIT'	: '$objectLit             children',
			'PROPERTY_INIT'	: '$propertyLit           children*',
			'RETURN'		: '$return                value',
			'BREAK'			: '$break                 label',
			'CONTINUE'		: '$continue              label',
			'THROW'			: '$throw                 exception',
			'BLOCK'			: '$block                 children',
			'SEMICOLON'		: '$exp                   expression',
			'LIST'			: '$list                  children',
			'COMMA'			: '$comma                 children',
			'IF'			: '$ifElse                condition thenPart elsePart',
			'TRY'			: '$try                   tryBlock catchClauses finallyBlock',
			'CATCH'			: '$catch                 varName block',
			'CASE'			: '$case                  caseLabel statements',
			'DEFAULT'		: '$default               statements',
			'SWITCH'		: '$switch                discriminant cases defaultIndex',
			'FOR'			: '$for                   setup condition update body',
			'FOR_IN'		: '$forIn                 iterator object body',
			'WHILE'			: '$while                 condition body',
			'DO'			: '$do                    condition body',
			'LABEL'			: '$label                 label statement',
			'VAR'			: '$varDeclarations       children',
			'FUNCTION'		: '$function              functionForm name params body', //params
			'SCRIPT'		: '$program               varDecls funDecls children'
		};
		
		for (var name in processors) {
			if (processors.hasOwnProperty(name)) {
				var spec = processors[name];
				if (typeof spec === "string") {
					processors[name] = _createProcessorFrom(spec);
				}
			}
		}
		return processors;
	}
	
	_processors = _createProcessors();
	
	// Object.prototype.acceptVisitor = function default_acceptVisitor(visitor) {
	// 	return visitor.processPrimitive(this);
	// };
	// 
	// String.prototype.acceptVisitor = function string_acceptVisitor(visitor) {
	// 	return visitor.processString(this);
	// };
	// 
	// RegExp.prototype.acceptVisitor = function regexp_acceptVisitor(visitor) {
	// 	return visitor.processRegExp(this);
	// };
	// 
	// _Node.prototype.acceptVisitor = function node_acceptVisitor(visitor) {
	// 	return visitor.processNode(this);
	// };
	// 
	// Array.prototype.acceptVisitor = function array_acceptVisitor(visitor) {
	// 	return visitor.processArray(this);
	// };



	function ASTVisitor() {
		this._substrings = [];
		this._indentLevel = 0;
	}
	
	ASTVisitor.prototype.emit = function emit() {return this._substrings.join("");};

	ASTVisitor.prototype.nextPut = function nextPut(string_) {
		var index, count;
		count = arguments.length;
		for (index = 0; index < count; ++index) {
			this._substrings.push(arguments[index]);
		}
		return this;
	};
		
	ASTVisitor.prototype.backup = function backup() {
		--this._substrings.length;
		return this;
	};

	var _Tabs = "";
	var _RETURN = "\n";
	
	ASTVisitor.prototype.nextLine = function nextLine() {
		return this.nextPut(_RETURN, _Tabs.substr(0, this._indentLevel));
	};

	ASTVisitor.prototype.indent = function indent() {
		var count = ++this._indentLevel;
		if (count > _Tabs.length) {_Tabs += "\t";}
		return this;
	};

	ASTVisitor.prototype.outdent = function outdent() {
		--this._indentLevel;
		return this;
	};



	ASTVisitor.prototype.processElement = function processElement(element) {
		// element.acceptVisitor(this);  
		// Having a problem with Object method being cal for _Nodes, so using the following hack
		if (element instanceof _Node) {return this.processNode(element);}
		if (Array.isArray(element)) {return this.processArray(element);}
		if (typeof element === 'string') {return this.processString(element);}
		if (element instanceof RegExp) {return this.processRegExp(element);}
		return this.processPrimitive(element);		
	};

	ASTVisitor.prototype.processPrimitive = function processPrimitive(prim) {
		return this.nextPut("# " + prim, "(.List{K})");
	};

	ASTVisitor.prototype.processString = function processString(string) {
		return this.nextPut("# ", string.quote(), "(.List{K})");
	};

	ASTVisitor.prototype.processRegExp = function processRegExp(regexp) {
		return this.nextPut("# ", regexp.toString().quote(), "(.List{K})");
	};

	ASTVisitor.prototype.processArray = function processArray(list) {
		this.nextPut('kList("List`{K`}2K_")');
		return (list.length === 0) ? 
			this.nextPut("(.List`{K`})") : 
			this.processElements(list, ',,');
	};

	ASTVisitor.prototype.processElements = function processElements(elements, delimiter) {
		var count, index;
		count = elements.length;
		this.nextPut("(");
		if (count > 0) {
			if (count === 1) {
				this.processElement(elements[0]);
			} else {
				this.indent().nextLine().processElement(elements[0]);
				index = 1;
				do {
					this.nextPut(delimiter).nextLine().processElement(elements[index++]);
				} while (index < count);
				this.outdent().nextLine();
			}
		}
		return this.nextPut(")");
	};



	ASTVisitor.prototype.processPrefix = function processPrefix(prefixName, params) {
		var count = params.length;
		this.nextPut("'", prefixName);
		if (count) {
			this.nextPut("`(", "_");
			while (--count > 0) {this.nextPut("`,_");}
			this.nextPut("`)");
		}
	};
		
	ASTVisitor.prototype.processNode = function processNode(node) {
		var typeName = _tokenNameAt(node.type);
		// this.updateIndent(node.lineno);
		return _processors[typeName].call(this, node, typeName);
		// this.outdent();
	};
	
	ASTVisitor.prototype.process = function process(prefixName, params) {
		this.processPrefix(prefixName, params);
		return (params.length === 0) ? 
			this.nextPut("(.List{K})") : 
			this.processElements(params, ',,');
	};

	function _js2kast(astNode) {
		var prefixVisitor = new ASTVisitor();
		prefixVisitor.processNode(astNode);
		return prefixVisitor.emit();
	}

	function _asKast(fileNameWithExt) {
		var dirName = WORKING_DIR + "examples/",
			fileName = fileNameWithExt.replace(/\.js$/, ""),
			path = dirName + fileName,
			sourceFile, source, ast, kast, kastFile, resultFlag;

		netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect");		
		if (fileNameWithExt.search(/\.js$/) === -1) {return "ERROR: Must be a .js file!";}
		sourceFile = FileIO.open(path + ".js");
		if (! sourceFile.exists()) {return "ERROR " + fileNameWithExt + " doesn't exist!";}
		
		source = FileIO.read(sourceFile);
		ast = _Parser.parse(source);
		kast = _js2kast(ast);
		
		kastFile = FileIO.open(path + ".kast");
		resultFlag = FileIO.write(kastFile, kast);
		
		return resultFlag;
	}
	
	return {
		asKast : _asKast
	};
})();


