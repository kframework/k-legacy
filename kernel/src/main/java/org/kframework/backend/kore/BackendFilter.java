package org.kframework.backend.kore;

import org.kframework.kil.visitors.NonCachingVisitor;
import org.kframework.kompile.KompileOptions;

/**
 * Created by Edgar Pek on 9/23/15.
 */
public class BackendFilter extends NonCachingVisitor {
    protected StringBuilder result;
    protected KompileOptions options;

    public BackendFilter(KompileOptions options) {
        this.options = options;
        this.result  = new java.lang.StringBuilder();
    }

    /**
     * @return The result of backend visitor (Latex / HTML)
     */
    public StringBuilder getResult() { return this.result; }

}
