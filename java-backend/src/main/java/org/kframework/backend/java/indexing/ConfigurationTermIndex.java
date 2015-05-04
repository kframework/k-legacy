// Copyright (c) 2014-2015 K Team. All Rights Reserved.
package org.kframework.backend.java.indexing;

import java.util.List;
import org.kframework.backend.java.kil.CellLabel;
import org.kframework.backend.java.kil.Term;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ListMultimap;

/**
 * Indexing information retrieved from a {@link Term} representing the entire
 * configuration to rewrite. In particular, it currently only retrieves indexing
 * information from {@code k} cells and {@code IO stream} cells.
 *
 * @author YilongL
 *
 */
public class ConfigurationTermIndex {
    private final ListMultimap<CellLabel, IndexingPair> cellIndexingPairs;
    private final List<IndexingPair> instreamIndexingPairs;
    private final List<IndexingPair> outstreamIndexingPairs;
    private final int maxInputBufLen;
    private final int maxOutputBufLen;

    public ConfigurationTermIndex(ListMultimap<CellLabel, IndexingPair> normalIndexingPairs,
            List<IndexingPair> instreamIndexingPairs,
            List<IndexingPair> outstreamIndexingPairs, int maxInputBufLen,
            int maxOutputBufLen) {
        this.cellIndexingPairs = ImmutableListMultimap.copyOf(normalIndexingPairs);
        this.instreamIndexingPairs = ImmutableList.copyOf(instreamIndexingPairs);
        this.outstreamIndexingPairs = ImmutableList.copyOf(outstreamIndexingPairs);
        this.maxInputBufLen = maxInputBufLen;
        this.maxOutputBufLen = maxOutputBufLen;
    }

    public ListMultimap<CellLabel, IndexingPair>  getCellIndexingPairs() {
        return cellIndexingPairs;
    }

    public List<IndexingPair> getInstreamIndexingPairs() {
        return instreamIndexingPairs;
    }

    public List<IndexingPair> getOutstreamIndexingPairs() {
        return outstreamIndexingPairs;
    }

    public int maxInputBufLen() {
        return maxInputBufLen;
    }

    public int maxOutputBufLen() {
        return maxOutputBufLen;
    }
}
