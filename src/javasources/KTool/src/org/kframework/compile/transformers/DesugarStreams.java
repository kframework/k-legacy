// Copyright (c) 2012-2014 K Team. All Rights Reserved.
package org.kframework.compile.transformers;

import org.kframework.kil.*;
import org.kframework.kil.visitors.CopyOnWriteTransformer;
import org.kframework.utils.errorsystem.KException;
import org.kframework.utils.errorsystem.KException.ExceptionType;
import org.kframework.utils.errorsystem.KException.KExceptionGroup;
import org.kframework.utils.general.GlobalSettings;

import java.util.ArrayList;


public class DesugarStreams extends CopyOnWriteTransformer {
    
    ArrayList<String> channels = new ArrayList<String>();

    public DesugarStreams(org.kframework.kil.loader.Context context) {
        super("Desugar streams", context);
        
        channels.add("stdin");
        channels.add("stdout");
    }
    
    @Override
    public ASTNode visit(Cell node, Void _)  {
        ASTNode result = super.visit(node, _);
        if (!(result instanceof Cell)) {
            GlobalSettings.kem.register(new KException(ExceptionType.ERROR, 
                    KExceptionGroup.INTERNAL, 
                    "Expecting Cell, but got " + result.getClass() + " in Streams Desugarer.", 
                    getName(), result.getFilename(), result.getLocation()));
        }
        Cell cell = (Cell) result;
        String stream = cell.getCellAttributes().get("stream");
        if (null == stream) return cell;
        if (result == node) {
            node = node.shallowCopy();
        } else node = cell;
        node.setContents(makeStreamList(stream, node));
        return node;
    }
    
    private Term makeStreamList(String stream, Cell node) {
        Term contents = node.getContents();
        
        Term addAtBeginning = null;
        Term addAtEnd = null;
        java.util.List<Term> items = new ArrayList<Term>();
        if ("stdin".equals(stream)) {
//            eq evalCleanConf(T, "stdin") = mkCollection(List, (T, ioBuffer(stdinVariable), noIOVariable, stdinStream)) .
            addAtBeginning = contents;
//            syntax List ::= "#buffer" "(" K ")"           [cons(List1IOBufferSyn)]
            //change TermCons here to KApp(KList) format by using the label of Stream1IOBufferSyn
            KList bufferKList = new KList();
            KApp bufferKApp = new KApp(KLabelConstant.of(context.conses.get("Stream1IOBufferSyn").getKLabel(), context),bufferKList);
            bufferKList.getContents().add(new Variable("$stdin", "String"));// eq stdinVariable = mkVariable('$stdin,K) .
            items.add(newListItem(bufferKApp));
            
            items.add(new Variable("$noIO", ("List")));//          eq noIOVariable = mkVariable('$noIO,List) .
            
//            syntax List ::= "#istream" "(" Int ")"        [cons(List1InputStreamSyn)]
          //change TermCons here to KApp(KList) format by using the label of Stream1InputStreamSyn
            KList stdinStreamKList = new KList();
            KApp stdinStreamKApp = new KApp(KLabelConstant.of(context.conses.get("Stream1InputStreamSyn").getKLabel(), context),stdinStreamKList);
            stdinStreamKList.getContents().add(IntBuiltin.ZERO);
            items.add(newListItem(stdinStreamKApp));
        }
        if ("stdout".equals(stream)) {
//            eq evalCleanConf(T, "stdout") = mkCollection(List, (stdoutStream, noIOVariable, ioBuffer(nilK),T)) .
//            | "#ostream" "(" Int ")"        [cons(List1OutputStreamSyn)]
            //change TermCons here to KApp(KList) format by using the label of Stream1OutputStreamSyn
            KList stdoutStreamKList = new KList();
            KApp stdoutStreamKApp =  new KApp(KLabelConstant.of(context.conses.get("Stream1OutputStreamSyn").getKLabel(), context),stdoutStreamKList);
            stdoutStreamKList.getContents().add(IntBuiltin.ONE);
            items.add(newListItem(stdoutStreamKApp));
            
            items.add(new Variable("$noIO", ("List")));//          eq noIOVariable = mkVariable('$noIO,List) .

//            syntax List ::= "#buffer" "(" K ")"           [cons(List1IOBufferSyn)]
          //change TermCons here to KApp(KList) format by using the label of Stream1IOBufferSyn
            KList bufferKList = new KList();
            KApp bufferKApp = new KApp(KLabelConstant.of(context.conses.get("Stream1IOBufferSyn").getKLabel(), context),bufferKList);
            bufferKList.getContents().add(StringBuiltin.EMPTY);// eq stdinVariable = mkVariable('$stdin,K) .
            items.add(newListItem(bufferKApp));

            addAtEnd = contents;
        }
        if(channels.indexOf(stream) == -1){
            GlobalSettings.kem.register(new KException(ExceptionType.ERROR, 
                    KExceptionGroup.INTERNAL, 
                    "Make sure you give the correct stream names: " + channels.toString(), 
                    getName(), node.getFilename(), node.getLocation()));
        }
        DataStructureSort myList = context.dataStructureListSortOf(DataStructureSort.DEFAULT_LIST_SORT);
        Term newItems = DataStructureSort.listOf(context, items.toArray(new Term[] {}));
        if (addAtBeginning != null) {
            newItems = KApp.of(KLabelConstant.of(myList.constructorLabel(), context), addAtBeginning, newItems);
        }
        if (addAtEnd != null) {
            newItems = KApp.of(KLabelConstant.of(myList.constructorLabel(), context), newItems, addAtEnd);
        }
        return newItems;
    }

    private Term newListItem(Term element) {
        DataStructureSort myList = context.dataStructureListSortOf(DataStructureSort.DEFAULT_LIST_SORT);
        return KApp.of(KLabelConstant.of(myList.elementLabel(), context), element);
    }        

    @Override
    public ASTNode visit(org.kframework.kil.Context node, Void _) {
        return node;
    }
    
    @Override
    public ASTNode visit(Rule node, Void _) {
        return node;
    }
    
    @Override
    public ASTNode visit(Syntax node, Void _) {
        return node;
    }
    
}
