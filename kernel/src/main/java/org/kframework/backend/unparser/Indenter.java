// Copyright (c) 2012-2015 K Team. All Rights Reserved.
package org.kframework.backend.unparser;

import java.io.IOException;
import java.io.Writer;

import org.kframework.utils.errorsystem.KExceptionManager;

class Indenter {
    String endl = System.getProperty("line.separator");
    protected java.util.Stack<Integer> indents;

    private final java.lang.StringBuilder stringBuilder;
    private final Writer out;

    protected boolean atBOL = true;
    protected IndentationOptions indentationOptions;
    private int lineNo;
    private int colNo;


    public Indenter() {
        this(new IndentationOptions());
    }

    public Indenter(IndentationOptions indentationOptions) {
        indents = new java.util.Stack<Integer>();
        stringBuilder = new java.lang.StringBuilder();
        this.out = null;
        lineNo = 1;
        colNo = 1;
        this.indentationOptions = indentationOptions;
    }

    public Indenter(Writer out) {
        this(out, new IndentationOptions());
    }

    public Indenter(Writer out, IndentationOptions indentationOptions) {
        indents = new java.util.Stack<Integer>();
        this.out = out;
        this.stringBuilder = null;
        lineNo = 1;
        colNo = 1;
        this.indentationOptions = indentationOptions;
    }

    private int indentSize() {
        int size = 0;
        for (Integer i : indents) {
            size += i;
        }
        return size;
    }

    public void setWidth(int newWidth) {
        indentationOptions.setWidth(newWidth);
    }

    public int getWidth() {
        return indentationOptions.getWidth();
    }

    public int getAuxTabSize() {
        return indentationOptions.getAuxTabSize();
    }

    private void append(String string) {
        if (stringBuilder != null) {
            stringBuilder.append(string);
        } else {
            try {
                out.append(string);
            } catch (IOException e) {
                KExceptionManager.criticalError("Error writing to stream underlying unparser", e);
            }
        }
    }

    public void write(String string) {
        if (string.isEmpty()) {
            return;
        }
//        System.err.println("@" + string + "@"); // for debugging
        if (atBOL) {
            for (int i = 0; i < indentSize(); ++i) {
                append(" ");
                colNo++;
            }
        }
        if (getWidth() >= 0 && colNo - 1 + endl.length() + string.length() > getWidth()) {
            append(endl);
            lineNo++;
            colNo = 1;
            for (int i = 0; i < indentSize() + getAuxTabSize(); ++i) {
                append(" ");
                colNo++;
            }
        }
        append(string);
        colNo += string.length();
        atBOL = false;
    }

    public void endLine() {
        atBOL = true;
        append(endl);
        lineNo++;
        colNo = 1;
    }

    public void indentToCurrent() {
        indents.push(colNo - 1 - indentSize());
    }

    public void indent(int size) {
        indents.push(indentationOptions.getTabSize() * size);
    }

    public void unindent() {
        indents.pop();
    }

    public int getLineNo() {
        return lineNo;
    }

    public int getColNo() {
        return colNo;
    }

    public int length() {
        return stringBuilder.length();
    }

    public String substring(int startIdx) {
        return stringBuilder.substring(startIdx);
    }

    public String substring(int startIdx, int endIdx) {
        return stringBuilder.substring(startIdx, endIdx);
    }
}
