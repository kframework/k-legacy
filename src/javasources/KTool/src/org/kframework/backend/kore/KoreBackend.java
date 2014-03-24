package org.kframework.backend.kore;

import org.kframework.backend.BasicBackend;
import org.kframework.compile.checks.CheckConfigurationCells;
import org.kframework.compile.checks.CheckRewrite;
import org.kframework.compile.checks.CheckVariables;
import org.kframework.compile.transformers.AddEmptyLists;
import org.kframework.compile.transformers.FlattenTerms;
import org.kframework.compile.transformers.RemoveBrackets;
import org.kframework.compile.transformers.RemoveSyntacticCasts;
import org.kframework.compile.utils.CheckVisitorStep;
import org.kframework.compile.utils.CompilerStepDone;
import org.kframework.compile.utils.CompilerSteps;
import org.kframework.compile.utils.MetaK;
import org.kframework.kil.*;
import org.kframework.kil.visitors.BasicVisitor;
import org.kframework.kil.loader.Context;
import org.kframework.main.FirstStep;
import org.kframework.utils.Stopwatch;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

public class KoreBackend extends BasicBackend {
  public KoreBackend(Stopwatch sw, Context context) {
    super(sw, context);
  }

  @Override
  public void run(Definition toKore) throws IOException {
      
      try {
        toKore = this.getCompilationSteps().compile(toKore, this.getDefaultStep());
    } catch (CompilerStepDone e) {
        // TODO Auto-generated catch block
        toKore = (Definition) e.getResult();
    }
      KilTransformer trans = new KilTransformer(context);
      HashMap<String,PrintWriter> fileTable = new HashMap<String,PrintWriter>();
      for(int i = 0; i < toKore.getItems().size(); ++i){
          
          if(!fileTable.containsKey(((toKore.getItems().get(i)).getFilename()))){
              
              fileTable.put((toKore.getItems().get(i)).getFilename(), 
                      new PrintWriter(((toKore.getItems().get(i)).getFilename().substring(0, 
                              (toKore.getItems().get(i)).getFilename().length()-2))+".kore"));
              }
      }
      
      for(int i = 0; i < toKore.getItems().size(); ++i){
    		  
          fileTable.get((toKore.getItems().get(i)).getFilename()).println(trans.kilToKore(((toKore.getItems().get(i)))));
      }
      
      ArrayList<PrintWriter> toClosedFiles = new ArrayList<PrintWriter>(fileTable.values());
      
      for(int i = 0; i < toClosedFiles.size(); ++i){
          
          toClosedFiles.get(i).close();
      }
  }

  @Override
  public String getDefaultStep() {
      return "LastStep";
  }
  
  @Override
    public CompilerSteps<Definition> getCompilationSteps() {
        CompilerSteps<Definition> steps = new CompilerSteps<Definition>(context);
        steps.add(new FirstStep(this, context));
        steps.add(new CheckVisitorStep<Definition>(new CheckConfigurationCells(context), context));
        steps.add(new RemoveBrackets(context));
        steps.add(new AddEmptyLists(context));
        steps.add(new RemoveSyntacticCasts(context));
//        steps.add(new EnforceInferredSorts(context));
        steps.add(new CheckVisitorStep<Definition>(new CheckVariables(context), context));
        steps.add(new CheckVisitorStep<Definition>(new CheckRewrite(context), context));
        steps.add(new FlattenTerms(context));
        //steps.add(new StrictnessToContexts(context));
        //steps.add(new FreezeUserFreezers(context));
        //steps.add(new ContextsToHeating(context));
        //steps.add(new AddSupercoolDefinition(context));
        //steps.add(new AddHeatingConditions(context));
        //steps.add(new AddSuperheatRules(context));
        //steps.add(new DesugarStreams(context, false));
        //steps.add(new ResolveFunctions(context));
        //steps.add(new AddKCell(context));
        //steps.add(new AddStreamCells(context));
        //steps.add(new AddSymbolicK(context));
        //steps.add(new AddSemanticEquality(context));
        // steps.add(new ResolveFresh());
        //steps.add(new FreshCondToFreshVar(context));
        //steps.add(new ResolveFreshVarMOS(context));
        //steps.add(new AddTopCellConfig(context));
        //if (GlobalSettings.addTopCell) {
         //   steps.add(new AddTopCellRules(context));
        //}
        //steps.add(new ResolveBinder(context));
        //steps.add(new ResolveAnonymousVariables(context));
        //steps.add(new ResolveBlockingInput(context, false));
        //steps.add(new AddK2SMTLib(context));
        //steps.add(new AddPredicates(context));
        //steps.add(new ResolveSyntaxPredicates(context));
        //steps.add(new ResolveBuiltins(context));
        //steps.add(new ResolveListOfK(context));
        //steps.add(new FlattenSyntax(context));
        //steps.add(new LastStep(this, context));
        return steps;
    }
}

class KoreFilter extends BasicVisitor {
    
    protected StringBuilder indenter;

    public KoreFilter(Context context) {
      super(context);
      indenter = new StringBuilder();
    }
    
    public String getResult() {
        String a = indenter.toString();
        this.clear();
        return a;
    }
    
    public void clear(){
        
        indenter = new StringBuilder();
    }

    @Override
    public String getName() {
        return "KoreFilter";
    }
    
    @Override
    public void visit(Ambiguity node) {

        indenter.append("amb(");
        for (int i = 0; i < node.getContents().size() ; ++i){
            Term term=node.getContents().get(i);
            if (term != null){
                term.accept(this);
                if(i!=node.getContents().size()-1){
                    indenter.append(",");
                }
            }
        }
        indenter.append(")");
    }
    
    @Override
    public void visit(Attribute node) {
    	
    	if(node.getValue().equals("")){
    		indenter.append(" "+node.getKey());
    	} else {
    		indenter.append(" "+node.getKey()+"("+node.getValue()+")");
    	}
    }
    
    @Override
    public void visit(Attributes node) {
        
    	boolean containKore=false;
        if(node.isEmpty()){
            return;
        }
        indenter.append("[");
        for (int i = 0; i < node.getContents().size() ; ++i){
            Attribute term=node.getContents().get(i);
            if(term.getKey().equals("kore")){
            	containKore=true;
            }
            term.accept(this);
            if(i!=node.getContents().size()-1){
                indenter.append(", ");
            }
        }
        
        if(!containKore){
        	indenter.append(", kore");
        }
        indenter.append("]");
    }
    
    @Override
    public void visit(BackendTerm node) {
        indenter.append(node.getValue());
    }
    
    @Override
    public void visit(Collection node) {
        
        if(node.getContents().size()==0){
            indenter.append("."+node.getSort());
            return;
        }
        
        for(int i = 0;i < node.getContents().size(); ++i){
            Term term = node.getContents().get(i);
            term.accept(this);
        }
    }
    
    @Override
    public void visit(BagItem node) {
        node.getItem().accept(this);
    }
    
    @Override
    public void visit(Token node) {
        indenter.append("#token(\"" + node.tokenSort() + "\", \"" + node.value() + "\")");
    }
    
    @Override
    public void visit(Bracket node) {
        indenter.append("(");
        node.getContent().accept(this);
        indenter.append(")");
    }
    
    @Override
    public void visit(Cast node) {
        indenter.append("(");
        node.getContent().accept(this);
        indenter.append(" :");
        if (node.isSyntactic()) {
            indenter.append(":");
        }
        indenter.append(node.getSort());
        indenter.append(")");
    }
    
    @Override
    public void visit(Cell cell) {

        String attributes = "";
        for (Entry<String, String> entry : cell.getCellAttributes().entrySet()) {
            if (entry.getKey() != "ellipses") {
                attributes += " " + entry.getKey() + "=\"" + entry.getValue() + "\"";
            }
        }

        indenter.append("<" + cell.getLabel() + attributes + ">");
        if (cell.hasLeftEllipsis()) {
            indenter.append("... ");
        } else {
            indenter.append(" ");
        }
        cell.getContents().accept(this);
        
        if (cell.hasRightEllipsis()) {
            indenter.append(" ...");
        } else {
            indenter.append(" ");
        }
        
        indenter.append("</" + cell.getLabel() + ">");
    }
    
    @Override
    public void visit(Configuration node) {
        indenter.append("  configuration ");
        node.getBody().accept(this) ;
        indenter.append(" ");
        node.getAttributes().accept(this);
        indenter.append('\n');
    }
    
    @Override
    public void visit(org.kframework.kil.Context node) {
        indenter.append("  context ");
        node.getBody().accept(this);
        indenter.append(" ");
        node.getAttributes().accept(this);
        indenter.append('\n');
    }
    
    public void visit(DataStructureSort node) {
        indenter.append(node.name());
    }
    
    @Override
    public void visit(Definition node) {
        for (DefinitionItem di : node.getItems()) {
            di.accept(this);
        }
    }
    
    @Override
    public void visit(Freezer node) {
        indenter.append("#freezer ");
        node.getTerm().accept(this);
        indenter.append("(.KList)");
    }
    
    @Override
    public void visit(FreezerHole hole) {
        indenter.append("HOLE(" + hole.getIndex() + ")");
    }
    
    @Override
    public void visit(Hole hole) {
        indenter.append("HOLE");
    }

    @Override
    public void visit(Import node) {
        indenter.append("  imports " +node.getName());
        indenter.append('\n');
    }
  
    private void visitList(List<? extends ASTNode> nodes, String sep, String empty) {
    	boolean isInKList=false;
    	boolean needPren=false;
    	if(empty.equals(".KList")){
    		isInKList=true;
    	}
    	
        if (nodes.size() == 0) { this.indenter.append(empty); }
        else {
          for (int i = 0; i < nodes.size(); i++) {
        	if(isInKList && (nodes.get(i) instanceof KList)){
        		indenter.append(" [ ");
        		needPren=true;
        	}
            nodes.get(i).accept(this);
            if(needPren){
            	indenter.append(" ] ");
            }
            if (i != (nodes.size() - 1)) { indenter.append(sep); }
            needPren=false;
          }
        }
      }

          @Override
        public void visit(KSequence node) { 
            visitList(node.getContents(), " ~> ", ".K");
        }

          @Override
        public void visit(KList node) {
              visitList(node.getContents(), " , ", ".KList");
        }

          @Override
        public void visit(BoolBuiltin node) {
              this.indenter.append(node.value()); // TODO: true() vs #"true"()
        }

        @Override
        public void visit(IntBuiltin node) {
            this.indenter.append(node.value()); // TODO: true() vs #"true"()
        }

        @Override
        public void visit(StringBuiltin node) {
            this.indenter.append(node.value());
        }
        
        @Override
        public void visit(KApp node) {
              node.getLabel().accept(this);
              this.indenter.append("(");
              node.getChild().accept(this);
              this.indenter.append(")");
              this.indenter.append(":"+node.getSort());
        }

        @Override
        public void visit(KLabelConstant node) {
            this.indenter.append(node.getLabel().replaceAll("\\(", "`(").replaceAll("\\)", "`)")); // TODO: escape the label
        }
        
        @Override
        public void visit(KInjectedLabel kInjectedLabel) {
            Term term = kInjectedLabel.getTerm();
            if (MetaK.isKSort(term.getSort())) {
                indenter.append(" 2KLabel ");
                indenter.append(KInjectedLabel.getInjectedSort(term.getSort()));
            } else {
                indenter.append("# ");
            }
            indenter.append("(");
            term.accept(this);
            indenter.append(")");
        }
        
        @Override
        public void visit(Lexical node) {
            this.indenter.append("Token{"+node.getLexicalRule()+"}");
        }
  
        @Override
        public void visit(ListTerminator node) {
            this.indenter.append(node.toString());
        }
        
        @Override
        public void visit(LiterateModuleComment node) {
        	
        	if(node.getValue().contains("/*") && node.getValue().contains("*/")){
        		indenter.append("// "+node.getType()+node.getValue());
                indenter.append('\n');
        	} else {
            	indenter.append("/* "+node.getType()+node.getValue()+" */");
                indenter.append('\n');
        	}
        }
        
        @Override
        public void visit(LiterateDefinitionComment node) {
        	if(node.getValue().contains("/*") && node.getValue().contains("*/")){
        		indenter.append("// "+node.getType()+node.getValue());
                indenter.append('\n');
        	} else {
            	indenter.append("/* "+node.getType()+node.getValue()+" */");
                indenter.append('\n');
        	}
        }
          
        @Override
        public void visit(Module mod) {
            indenter.append("module " + mod.getName() + "\n");
            for (ModuleItem i : mod.getItems()){
                
                i.accept(this);
            }
            indenter.append("\nendmodule");
        }

        @Override
        public void visit(ParseError node) {
            indenter.append("Parse error: " + node.getMessage());
        }
        
        @Override
        public void visit(Production node) {
            for (ProductionItem i : node.getItems()){
                
                i.accept(this);
                indenter.append(" ");
            }
            node.getAttributes().accept(this);
        }
        
        @Override
        public void visit(PriorityBlock node) {
            
            if (node.getAssoc() != null && !node.getAssoc().equals("")){
                indenter.append(node.getAssoc()+": ");
            }
            
            for (int i = 0; i < node.getProductions().size(); ++i){
                Production production = node.getProductions().get(i);
                if(!production.getItems().isEmpty()){
                	production.accept(this);
                    if(i!=node.getProductions().size()-1){
                        indenter.append("\n     | ");
                    }
                }
            }
        }
        
        @Override
        public void visit(PriorityBlockExtended node) {
            
            for (int i = 0; i < node.getProductions().size(); ++i){
                KLabelConstant production = node.getProductions().get(i);
                production.accept(this);
                if(i!=node.getProductions().size()-1){
                    indenter.append(" ");
                }
            }
        }
        
        @Override
        public void visit(PriorityExtended node) {
            
            indenter.append("  syntax priorities " );
            for (int i = 0; i < node.getPriorityBlocks().size(); ++i){
                PriorityBlockExtended production = node.getPriorityBlocks().get(i);
                production.accept(this);
                if(i!=node.getPriorityBlocks().size()-1){
                    indenter.append("\n     > ");
                }
            }
            indenter.append('\n');
        }
        
        @Override
        public void visit(PriorityExtendedAssoc node) {
            
        	if(!node.getTags().isEmpty()){
        		indenter.append("  syntax "+node.getAssoc()+" " );
                for (int i = 0; i < node.getTags().size(); ++i){
                    KLabelConstant production = node.getTags().get(i);
                    production.accept(this);
                    if(i!=node.getTags().size()-1){
                        indenter.append(" ");
                    }
                }
                indenter.append('\n');
        	}
        }
        
        @Override
        public void visit(Require node) {
            
            indenter.append(node.toString());
            indenter.append('\n');
        }
        
        @Override
        public void visit(Restrictions node) {
            indenter.append("  syntax ");
            if(node.getSort()!=null){
                node.getSort().accept(this);
            } else {
                node.getTerminal().accept(this);
            }
            indenter.append(" -/- " + node.getPattern());
            indenter.append('\n');
        }
        
        @Override
        public void visit(Rewrite rewrite) {
        	indenter.append("{ ");
        	if(rewrite.getLeft().getSort().equals(KSorts.KLABEL)){
        		KApp result = (new KApp(rewrite.getLeft(),KList.EMPTY));
        		result.accept(this);
        	} else {
        		rewrite.getLeft().accept(this);
        	}
            indenter.append(" => ");
        	if(rewrite.getRight().getSort().equals(KSorts.KLABEL)){
        		KApp result = (new KApp(rewrite.getRight(),KList.EMPTY));
        		result.accept(this);
        	} else {
        		rewrite.getRight().accept(this);
        	}
            indenter.append(" }");
            indenter.append('\n');
        }
        
        @Override
        public void visit(Rule node) {
            indenter.append("  rule ");

            if (node.getLabel() != null && !node.getLabel().equals(""))
                indenter.append("[" + node.getLabel() + "]: ");

            node.getBody().accept(this);
            indenter.append(" ");
            
            if (node.getRequires() != null) {
                indenter.append("requires ");
                node.getRequires().accept(this);
                indenter.append(" ");
            }
            if (node.getEnsures() != null) {
                indenter.append("ensures ");
                node.getEnsures().accept(this);
                indenter.append(" ");
            }
            node.getAttributes().accept(this);
            indenter.append('\n');
        }
        
        @Override
        public void visit(Sentence node) {

            if (node.getLabel() != null && !node.getLabel().equals(""))
                indenter.append("[" + node.getLabel() + "]: ");

            node.getBody().accept(this);
            indenter.append(" ");
            
            if (node.getRequires() != null) {
                indenter.append("requires ");
                node.getRequires().accept(this);
                indenter.append(" ");
            }
            if (node.getEnsures() != null) {
                indenter.append("ensures ");
                node.getEnsures().accept(this);
                indenter.append(" ");
            }
            node.getAttributes().accept(this);
            indenter.append('\n');
        }
        
        @Override
        public void visit(Sort node) {
            indenter.append(node.toString());
        }

        @Override
        public void visit(StringSentence node) {
            indenter.append(node.toString());
        }
        
        @Override
        public void visit(Syntax node) {
            
        	boolean shouldPrint=false;
        	if(node.getPriorityBlocks().isEmpty()){
        		return;
        	}
        	
            for (int i = 0; i < node.getPriorityBlocks().size(); ++i){
                PriorityBlock production = node.getPriorityBlocks().get(i);
                if(!production.getProductions().isEmpty()){
                	shouldPrint=true;
                	break;
                }
            }
            
            if(!shouldPrint){
            	return;
            }
        	
            indenter.append("  syntax ");
            node.getSort().accept(this);
            indenter.append(" ::=");
            for (int i = 0; i < node.getPriorityBlocks().size(); ++i){
                PriorityBlock production = node.getPriorityBlocks().get(i);
                if(!production.getProductions().isEmpty()){
                	production.accept(this);
                    if(i!=node.getPriorityBlocks().size()-1){
                        indenter.append("\n     > ");
                    }
                }
            }
            indenter.append('\n');
        }
        
        @Override
        public void visit(TermComment node) {
            indenter.append(node.toString());
            indenter.append('\n');
        }
  
        @Override
        public void visit(Terminal node) {
            indenter.append(node.toString());
        }
        
        @Override
        public void visit(UserList node) {
            indenter.append(node.toString());
        }

        @Override
        public void visit(Variable node) {
            this.indenter.append(node.getName() + ":" + node.getSort());
        }
        
        @Override
        public void visit(TermCons node){
        	
        	KApp result = (new KApp(new KLabelConstant(node.getProduction().getKLabel()),new KList(node.getContents())));
        	result.setSort(node.getSort());
        	result.accept(this);

        }
}
