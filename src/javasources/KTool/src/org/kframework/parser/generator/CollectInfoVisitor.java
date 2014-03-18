package org.kframework.parser.generator;

import org.kframework.kil.KSorts;
import org.kframework.kil.Production;
import org.kframework.kil.Terminal;
import org.kframework.kil.UserList;
import org.kframework.kil.loader.Context;
import org.kframework.kil.visitors.BasicVisitor;

import java.util.HashSet;
import java.util.Set;

public class CollectInfoVisitor extends BasicVisitor {
    public CollectInfoVisitor(Context context) {
        super(context);
        lexerSorts.add(KSorts.KLABEL);
    }

    public Set<String> terminals = new HashSet<String>();
    public Set<String> lexerSorts = new HashSet<String>();

    private void addTerminal(String terminal) {
        if (terminal.equals(""))
            return;
        terminals.add(terminal);
    }

    public void visit(Production p) {
        if (p.isLexical() || p.getSort().startsWith("#"))
            lexerSorts.add(p.getSort());
        super.visit(p);
    }

    public void visit(Terminal t) {
        addTerminal(t.getTerminal());
    }

    public void visit(UserList ul) {
        addTerminal(ul.getSeparator());
    }
}
