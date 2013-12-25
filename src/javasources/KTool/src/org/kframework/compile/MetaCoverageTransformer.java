package org.kframework.compile;

import java.util.HashMap;
import java.util.Map;

import org.kframework.kil.ASTNode;
import org.kframework.kil.Rule;
import org.kframework.kil.loader.Context;
import org.kframework.kil.visitors.BasicTransformer;
import org.kframework.kil.visitors.exceptions.TransformerException;

/*
 * This class creates a mapping between each rule and a unique identifier
 */
public class MetaCoverageTransformer extends BasicTransformer {

	public static Map<Rule, Long> RuleToId = new HashMap<>();
	static long i = 0;

	public MetaCoverageTransformer(String name, Context context) {
		super(name, context);
	}

	@Override
	public ASTNode transform(Rule node) throws TransformerException {
		RuleToId.put(node, i++);
		return node;

	}

}
