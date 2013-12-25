package org.kframework.compile;

import java.util.Map.Entry;

import org.kframework.kil.ASTNode;
import org.kframework.kil.Bag;
import org.kframework.kil.Cell;
import org.kframework.kil.Configuration;
import org.kframework.kil.IntBuiltin;
import org.kframework.kil.KApp;
import org.kframework.kil.KList;
import org.kframework.kil.List;
import org.kframework.kil.ListItem;
import org.kframework.kil.Map;
import org.kframework.kil.MapItem;
import org.kframework.kil.Rewrite;
import org.kframework.kil.Rule;
import org.kframework.kil.Sentence;
import org.kframework.kil.StringBuiltin;
import org.kframework.kil.Term;
import org.kframework.kil.loader.Context;
import org.kframework.kil.visitors.BasicTransformer;
import org.kframework.kil.visitors.exceptions.TransformerException;

public class CoverageTransformer extends BasicTransformer {

	private static final String TRACE_CELL_TAG = "trace";
	private static final String META_CELL_TAG = "meta";
	private final String CONTAINER_CELL;
	
	public CoverageTransformer(String name, Context context, String cellName) {
		super(name,context);
		this.CONTAINER_CELL=cellName;		
	}
	
	public CoverageTransformer(String name, Context context) {
		this(name, context, "coverage-info");
	}

	@Override
	public ASTNode transform(Rule node) throws TransformerException {
		Rule r = node.shallowCopy();		
		// coverage only works for non-function rules.
		// stdout, stdin, and anywhere also needs to be checked.
		if (!r.containsAttribute("function")
				&& !r.containsAttribute("anywhere")
				&& !r.containsAttribute("stdout")
				&& !r.containsAttribute("stdin")) {
			Cell c = makeNewCoverageRewriteCell(MetaCoverageTransformer.RuleToId
					.get(node));
			Bag body = boxTermToBag(r.getBody());
			body.add(c);
			r.setBody(body);
			return r;

		}
		return transform((Sentence) r);
	}

	@Override
	public ASTNode transform(Configuration node) {
		Configuration config = node.shallowCopy();
		
		Cell traceCell = createCell(TRACE_CELL_TAG, List.EMPTY);
		Cell metaCell = createCell(META_CELL_TAG, createMetaMap());

		//create cell that handles tracing
		Bag b = new Bag();
		b.add(traceCell);
		b.add(metaCell);
		Cell container = createCell(this.CONTAINER_CELL, b);

		//add cell to current configuration
		Term body = config.getBody();
		body = boxTermToBag(body);
		((Bag)body).add(container);
		config.setBody(body);
		
		return config;
	}
	
	private Bag boxTermToBag(Term body) {	     		
		if (!(body instanceof Bag)) {
			Bag b = new Bag();
			b.add(body);		
			body = b;
		}		
		return (Bag)body;
	}

	private Cell makeNewCoverageRewriteCell(Long long1) {		
		KApp uid = IntBuiltin.kAppOf(long1);
		Rewrite r = new Rewrite(List.EMPTY, new ListItem(uid), context);
		Cell coverage = createCell(TRACE_CELL_TAG, r);
		coverage.setEllipses(Cell.Ellipses.LEFT);
		return coverage;
	}
       
	private Cell createCell(String tag, Term content) {
		Cell coverageCell = new Cell();
		coverageCell.setLabel(tag);
		coverageCell.setContents(content);
		return coverageCell;
	}

	private Map createMetaMap() {
		Map map = new Map();

		for (Entry<Rule, Long> entry : MetaCoverageTransformer.RuleToId
				.entrySet()) {
			Long id = entry.getValue();
			IntBuiltin key = IntBuiltin.of(id);
			KApp idK = new KApp(key, new KList());
			StringBuiltin val = StringBuiltin.of(entry.getKey().getAttributes()
					.toString());
			KApp valK = new KApp(val, new KList());
			MapItem item = new MapItem(idK, valK);
			map.add(item);
		}
		return map;
	}	
}
