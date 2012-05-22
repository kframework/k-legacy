/*jshint bitwise: true, curly: true, eqeqeq: true, forin: true, immed: false, laxbreak: false, 
		 newcap: true, noarg: true, nonew: true, nomen: false, onevar: true, plusplus: false, 
		 undef: true, sub: false, strict: true, white: false, passfail: false  */

/*global AnExpectedGlobal: true, AGlobalNotToOverwritten: false,
		 netscape: true, FileIO: true, DirIO: true, parse: true,
		JSAST: true, Narcissus: true */


JSAST = (function () {
	"use strict";
	
	var WORKING_DIR = "/Users/m3rabb/Dev/Maude/k2/examples/languages/research/js/",
		_Parser = Narcissus.parser,
		_Node = _Parser.Node,
		_definitions = Narcissus.definitions,
		_operatorSymbolsArray = _definitions.tokens,
		_operatorNamesDictionary = _definitions.opTypeNames,
		_indentLevel = 0, 
		_indentations = [""],
		_converters;


	function _tokenNameAt(index) {
        var symbol = _operatorSymbolsArray[index]
		// if (symbol.toUpperCase === undefined) debugger;
		return _operatorNamesDictionary[symbol] || symbol.toUpperCase();
    }
			
	function _createConverters() {
		function _createNamedParameterBuilder(_propertyNames) {
			return function buildParamsFromNamedProperties(_node) {
				return _propertyNames.map(function (propertyName) {return _node[propertyName];});
			};
		}
		
		function _emptyParamsBuilder(node) {return [];};
		
		function _createFixedArrayParameterBuilder(_fixedArrayPropertyName) {
			return function buildFixedArrayParamsFromNamedProperty(node) {
				return node[_fixedArrayPropertyName];
			};
		}

		function _hasArrayedParameter(spec) {
			return spec[spec.length - 1] === "*";
		}

		function _createConverter(_prefixName, _parameterBuilder) {
			return function converter(node) {
				this.processParts(_prefixName, _parameterBuilder(node));
			};
		}

		function _createConverterFrom(spec) {
			var propertyNames = spec.match(/[\$\(\())\w]+/ig),
				prefixName = propertyNames.shift(),
				parameterBuilder = _hasArrayedParameter(spec) ? 
					_createFixedArrayParameterBuilder(propertyNames.pop()) : 
					(propertyNames.length ? 
						_createNamedParameterBuilder(propertyNames) : 
						_emptyParamsBuilder);
			return _createConverter(prefixName, parameterBuilder);
		}
		
		
		var assignConverter = function converter(node) {
			var operatorId = node.assignOp,
				params = node.children,
				prefixName = '$assignment';
			if (operatorId !== null) { 
				prefixName = '$compoundAssignment';
				params.splice(1, 0, _operatorSymbolsArray[operatorId]);
			}
			this.processParts(prefixName, params);
		};

		var identifierConverter = function converter(node) {
			var name = node.value,
				params = [name],
				initializer = node.initializer,
				prefixName = '$id';
			if (initializer) {
				prefixName = '$initNewId';
				params.push(initializer);
			}
			this.processParts(prefixName, params);
		};
		
		var converters = {
			'NUMBER'		: '$n                     value',
			'STRING'		: '$s                     value',
			'REGEXP'		: '$regex                 value',
			'TRUE'			: '$b(true)',
			'FALSE'			: '$b(false)',
			'NULL'			: '$null',
			'THIS'			: '$this',
			'IDENTIFIER'	: identifierConverter,
			'DOT'			: '$staticAccess          children*',
			'INDEX'			: '$dynamicAccess         children*',
			'CALL'			: '$invoke                children*',
			// 'NEW'	: '$new                   children',
			'NEW_WITH_ARGS'	: '$new                   children',
			'INCREMENT'		: '$inc                   children postfix',
			'DECREMENT'		: '$dec                   children postfix',
			'DELETE'		: '$delete                children',
			'TYPEOF'		: '$typeof                children',
			'NOT'			: '$not                   children',
			'UNARY_MINUS'	: '$neg                   children',
			'UNARY_PLUS'	: '$plus                  children',
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
			'ASSIGN'		: assignConverter,
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
			'FUNCTION'		: '$function              functionForm name params body', //params*
			'SCRIPT'		: '$program               varDecls funDecls children'
		};
		
		for (var name in converters) {
			var spec = converters[name];
			if (typeof spec === 'string') {
				converters[name] = _createConverterFrom(spec);
			}
		}
		return converters;
	}
	
	_converters = _createConverters();
	
	Object.prototype.acceptVisitor = function default_acceptVisitor(visitor) {
		return visitor.processPrimitive(this);
	};
	
	String.prototype.acceptVisitor = function string_acceptVisitor(visitor) {
		return visitor.processString(this);
	};

	RegExp.prototype.acceptVisitor = function regexp_acceptVisitor(visitor) {
		return visitor.processRegExp(this);
	};

	_Node.prototype.acceptVisitor = function node_acceptVisitor(visitor) {
		return visitor.processNode(this);
	};

	Array.prototype.acceptVisitor = function array_acceptVisitor(visitor) {
		return visitor.processArray(this);
	};



	function PrefixVisitor() {
		this._substrings = [];
	}
	
	PrefixVisitor.prototype.emit = function emit() {return this._substrings.join("");};

	PrefixVisitor.prototype.nextPut = function nextPut(string) {this._substrings.push(string);};
		
	PrefixVisitor.prototype.backup = function backup() {--this._substrings.length;};

	PrefixVisitor.prototype.processNode = function processNode(node) {
		var typeName = _tokenNameAt(node.type);
		// node.type = typeName;
		// this.updateIndent(node.lineno);
		_converters[typeName].call(this, node);
		// this.outdent();
	};
	
	PrefixVisitor.prototype.processPrimitive = function processPrimitive(prim) {
		this.nextPut("" + prim);
	};

	PrefixVisitor.prototype.processString = function processString(string) {
		this.nextPut(string.quote());
	};

	PrefixVisitor.prototype.processRegExp = function processRegExp(regexp) {
		this.nextPut(regexp.toString().quote());
	};

	PrefixVisitor.prototype.processArray = function processArray(array) {
		array.length ? this.processElements(array, ',,') : this.nextPut('.Empty');
	};

	PrefixVisitor.prototype.processElement = function processElement(element) {
		// element.acceptVisitor(this);  
		// Having a problem with Object method being cal for _Nodes, so using the following hack
		if (element instanceof _Node) {return this.processNode(element);}
		if (Array.isArray(element)) {return this.processArray(element);}
		if (typeof element === 'string') {return this.processString(element);}
		if (element instanceof RegExp) {return this.processRegExp(element);}
		return this.processPrimitive(element);		
	};

	PrefixVisitor.prototype.processElements = function processElements(elements, delimiter) {
		elements.forEach(function (element) {
			this.processElement(element);
			this.nextPut(delimiter);
		}, this);
		this.backup();
	};
	
	PrefixVisitor.prototype.processParts = function processParts(prefixName, params) {
		this.nextPut(prefixName);
		if (params.length) {
			this.nextPut('(');
			this.processElements(params, ',');
			this.nextPut(')');
		}
	};

	function _ast2prefix(astNode) {
		var prefixVisitor = new PrefixVisitor();
		prefixVisitor.processNode(astNode);
		return prefixVisitor.emit();
	}

	function _asPrefix(fileNameWithExt) {
		var dirName = WORKING_DIR + "examples/",
			fileName = fileNameWithExt.replace(/\.js$/, ""),
			path = dirName + fileName,
			sourceFile, source, ast, prefixed, prefixedFile, resultFlag;

		netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect");		
		if (fileNameWithExt.search(/\.js$/) === -1) {return "ERROR: Must be a .js file!";}
		sourceFile = FileIO.open(path + ".js");
		if (! sourceFile.exists()) {return "ERROR " + fileNameWithExt + " doesn't exist!";}
		
		source = FileIO.read(sourceFile);
		ast = _Parser.parse(source);
		prefixed = _ast2prefix(ast);
		
		prefixedFile = FileIO.open(path + ".m");
		resultFlag = FileIO.write(prefixedFile, prefixed);
		
		return resultFlag;
	}
	
	return {
		asPrefix : _asPrefix
	};
})();


