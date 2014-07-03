// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.parser.concrete.disambiguate;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * A mapping of the form:
 * VarName  1-*  location  1-*  sort constraint
 *
 * Two level mapping from a variable name, to locations and then to the sort
 * constraint for each location information.
 *
 */
public class VarHashMap extends HashMap<String, Map<String, Set<String>>> {

    private static final long serialVersionUID = 1L;

    public int hashCode() {
        int code = 0;
        for (Map.Entry<String, Map<String, Set<String>>> entry : this.entrySet()) {
            code += entry.getKey().hashCode();
            for (Map.Entry<String, Set<String>> locSet : entry.getValue().entrySet()) {
                code += locSet.getKey().hashCode();
                for (String sort : locSet.getValue())
                    code += sort.hashCode();
            }
        }
        return code;
    }

    public boolean equals(Object obj) {
        if (obj == null)
            return false;
        if (obj == this)
            return true;
        if (obj instanceof VarHashMap) {
            VarHashMap that = (VarHashMap) obj;
            if (this.size() != that.size())
                return false;
            for (Map.Entry<String, Map<String, Set<String>>> thisNameEntry : this.entrySet()) {
                if (!that.containsKey(thisNameEntry.getKey()))
                    return false;
                Map<String, Set<String>> thisLoc = thisNameEntry.getValue();
                Map<String, Set<String>> thatLoc = that.get(thisNameEntry.getKey());
                if (thisLoc.size() != thatLoc.size())
                    return false;

                for (Map.Entry<String, Set<String>> thisSortsEntry : thisLoc.entrySet()) {
                    if (!thatLoc.containsKey(thisSortsEntry.getKey()))
                        return false;
                    Set<String> thisSorts = thisSortsEntry.getValue();
                    Set<String> thatSorts = thatLoc.get(thisSortsEntry.getKey());
                    if (thisSorts.size() != thatSorts.size())
                        return false;
                    for (String sort : thisSorts)
                        if (!thatSorts.contains(sort))
                            return false;
                }
            }
        } else
            return false;
        return true;
    }

    /**
     * For the given variable name and location, adds the sort into a set. If the varName doesn't exist, it creates the set.
     * @param varName
     * @param sort
     */
    public void add(String varName, String location, String sort) {
        Map<String, Set<String>> locs;
        if (this.containsKey(varName)) {
            locs = this.get(varName);
        } else {
            locs = new HashMap<>();
            this.put(varName, locs);
        }
        Set<String> sorts;
        if (locs.containsKey(location)) {
            sorts = locs.get(location);
        } else {
            sorts = new HashSet<>();
            locs.put(location, sorts);
        }
        sorts.add(sort);
    }

    public Set<String> getAll(String name) {
        Set<String> all = new HashSet<>();
        for (Set<String> set : this.get(name).values()) {
            all.addAll(set);
        }
        return all;
    }

    /**
     * For the given varName, adds all the values to the map of sets.
     * If the varName doesn't exist, it creates the map.
     * If the location doesn't exist, it creates the set.
     *
     * @return this
     */
    public VarHashMap addAll(VarHashMap that) {
        for (Map.Entry<String, Map<String, Set<String>>> thatNameEntry : that.entrySet()) {
            Map<String, Set<String>> locs;
            if (this.containsKey(thatNameEntry.getKey()))
                locs = this.get(thatNameEntry.getKey());
            else {
                locs = new HashMap<>();
                this.put(thatNameEntry.getKey(), locs);
            }
            for (Map.Entry<String, Set<String>> thatSortsEntry : thatNameEntry.getValue().entrySet()) {
                Set<String> thisSorts;
                if (locs.containsKey(thatSortsEntry.getKey())) {
                    thisSorts = locs.get(thatSortsEntry.getKey());
                } else {
                    thisSorts = new HashSet<>();
                    locs.put(thatSortsEntry.getKey(), thisSorts);
                }
                thisSorts.addAll(thatSortsEntry.getValue());
            }
        }
        return this;
    }

    public VarHashMap addAll(String varName, String location, Set<String> sorts) {
        Map<String, Set<String>> locs;
        if (this.containsKey(varName)) {
            locs = this.get(varName);
        } else {
            locs = new HashMap<>();
            this.put(varName, locs);
        }
        Set<String> sorts1;
        if (locs.containsKey(location)) {
            sorts1 = locs.get(location);
        } else {
            sorts1 = new HashSet<>();
            locs.put(location, sorts1);
        }
        sorts1.addAll(sorts);
        return this;
    }

    public VarHashMap deepClone() {
        VarHashMap that = new VarHashMap();
        for (Map.Entry<String, Map<String, Set<String>>> thatNameEntry : that.entrySet()) {
            Map<String, Set<String>> locs = this.get(thatNameEntry.getKey());
            for (Map.Entry<String, Set<String>> thatSortsEntry : thatNameEntry.getValue().entrySet()) {
                Set<String> thisSorts = locs.get(thatSortsEntry.getKey());
                thisSorts.addAll(thatSortsEntry.getValue());
            }
        }
        return that;
    }
}
