/*jshint bitwise: true, curly: true, eqeqeq: true, forin: true, immed: false, laxbreak: false, 
		 newcap: true, noarg: true, nonew: true, nomen: false, onevar: true, plusplus: false, 
		 undef: true, sub: false, strict: true, white: false, passfail: false  */

/*global AnExpectedGlobal: true, AGlobalNotToOverwritten: false,
		 netscape: true, FileIO: true, DirIO: true, parse: true,
		JSAST: true, Narcissus: true */


JSAST = (function () {
	"use strict";
	
	var _asASTString, _outputElement, _outputNode, // Resolves co-recursive forward refs
		INDENTATION = "\t",
		WORKING_DIR = "/Users/m3rabb/Dev/Maude/k-framework/examples/inProgress/js/",
		IDS_FILE_NAME = "js-json-ast-ids.k",
		PARSER = Narcissus.parser,
		NODE = PARSER.Node,
		DEFINITIONS = Narcissus.definitions,
		TOKENS = DEFINITIONS.tokens,
		OP_TYPE_NAMES = DEFINITIONS.opTypeNames,
		EXCLUDED_PROPERTIES = 
			['type', 'target', 'tokenizer', 'exports', 'labels',
			 'modAssns', 'modDecls', 'modDefns', 'modLoads', 
			 'start', 'end'],
		IDS_FILE_PREFIX = "kmod JS-JSON-AST-IDS\n" + 
			"    is including PL-INT + PL-FLOAT + PL-STRING + PL-ID\n\n" +
			"    syntax JSJsonAstIds ::= \n\t\t",
		IDS_FILE_SUFFIX = "\nendkm\n",
		IDS_FILE_PREFIX_LENGTH = IDS_FILE_PREFIX.length,
		IDS_FILE_SUFFIX_LENGTH = IDS_FILE_SUFFIX.length,
		_indentLevel = 0, _indentations = [""],
		_allIds = {},
		_collectIds;
	
	function _addIds(ids) {
		ids.forEach(function (id) {_allIds[id] = true;});
	}
	
	function _includeIds() {
		return Object.keys(_allIds).sort();
	}
	
	function _indent(startLevel_) {
		var indent;
		_indentLevel = (startLevel_ === undefined) ? _indentLevel + 1 : startLevel_;
		indent = _indentations[_indentLevel];
		if (indent === undefined) {
			indent = _indentations[_indentLevel] = INDENTATION.repeat(_indentLevel);
		}
		return indent;
	}

	function _outdent() {
		return _indentations[--_indentLevel];
	}
	
	function _isAllowedProperty(propertyName) {
		return EXCLUDED_PROPERTIES.indexOf(propertyName) === -1;
	}
	
	function _tokenName(string) {
        var token = TOKENS[string];
        return (/^\W/).test(token) ? OP_TYPE_NAMES[token] : token.toUpperCase();
    }
   
	function _outputArray(elements) {
		var output, indent;
		if (elements.length === 0) {return "[]";}
		output = "[";
		indent = _indent();
		elements.forEach(function (element) {
			output += "\n" + indent + _outputElement(element) + ",";
		});
		output = output.slice(0, -1) + "\n" + _outdent() + "]";
		return output;
	}
	
	_outputElement = function _outputElement(element) {
		if (element instanceof NODE) {return _outputNode(element);}
		if (Array.isArray(element)) {return _outputArray(element);}
		if (typeof element === 'string') {return element.quote();}
		if (element instanceof RegExp) {return element.toString().quote();}
		return "" + element;
	};
		
	_outputNode = function _outputNode(node) {
		var keys = Object.keys(node),
			ids = keys.filter(_isAllowedProperty),
			indent = _indent(),
			output = "{\n",
			typeName = _tokenName(node.type).quote();
			
		if (_collectIds) {_addIds(ids);}
		output += indent + "type : " + typeName;
		ids.sort();
		ids.forEach(function (id) {
			output += ",\n" + indent + id + " : ";
			output += _outputElement(node[id]);
		});
		output += "\n" + _outdent() + "}";
		return output;
    };

	NODE.prototype.toString = function () {
		return _indent(0) + _outputNode(this);
	};
	
	function _updateIds() {
		var path = WORKING_DIR + IDS_FILE_NAME, 
			idsFile = FileIO.open(path),
			delimiter = " |\n\t\t",
			input, ids, output;
		if (! idsFile.exists()) {return "ERROR: " + IDS_FILE_NAME + " doesn't exist!";}
		
		input = FileIO.read(idsFile);
		input = input.slice(IDS_FILE_PREFIX_LENGTH, -IDS_FILE_SUFFIX_LENGTH);
		ids = input.split(delimiter);
		_addIds(ids);
		ids = _includeIds();
		output = IDS_FILE_PREFIX + ids.join(delimiter) + IDS_FILE_SUFFIX;
		return FileIO.write(idsFile, output);
	}
	
	function _generate(fileNameWithExt, collectIds_) {
		var dirName = WORKING_DIR + "examples/",
			fileName = fileNameWithExt.replace(/\.js$/, ""),
			path = dirName + fileName,
			// isStrictMode = isStrictMode_ || false,
			sourceFile, source, ast, json, jsonFile, maude, maudeFile, resultFlag;

		netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect");		
		if (fileNameWithExt.search(/\.js$/) === -1) {return "ERROR: Must be a .js file!";}
		sourceFile = FileIO.open(path + ".js");
		if (! sourceFile.exists()) {return "ERROR " + fileNameWithExt + " doesn't exist!";}
		
		_collectIds = collectIds_ || false;
		source = FileIO.read(sourceFile);
		ast = PARSER.parse(source);
		json = ast.toString();
		
		jsonFile = FileIO.open(path + ".json");
		resultFlag = FileIO.write(jsonFile, json);
		if (_collectIds) {resultFlag = resultFlag && _updateIds();}
		
		// maude = "red \n" + json + "\n .\n"
		// maudeFile = FileIO.open(path + ".maude");
		// resultFlag = resultFlag && FileIO.write(maudeFile, maude);
		return resultFlag;
	}
	
	return {
		generate : _generate
	};
})();
	