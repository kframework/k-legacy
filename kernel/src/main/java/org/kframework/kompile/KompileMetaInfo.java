// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.kompile;

import com.google.gson.Gson;
import org.kframework.builtin.Sorts;
import org.kframework.kore.Sort;

import java.util.HashMap;
import java.util.Map;

public class KompileMetaInfo {
    public final String mainSyntaxModuleName;
    public final Map<String, String> configVarDefaultSort = new HashMap<>();
    public final String programStartSymbol;


    public KompileMetaInfo(String moduleName, Map<String, Sort> configVarDefaultSort) {
        this.mainSyntaxModuleName = moduleName;
        for(String key: configVarDefaultSort.keySet()){
            String sort = configVarDefaultSort.get(key).name();
            this.configVarDefaultSort.put(key, sort);
        }

        this.programStartSymbol = this.configVarDefaultSort.getOrDefault("$PGM", Sorts.K().name());
    }

    public String serialize() {
        Gson gson = new Gson();
        return gson.toJson(this);
    }

    public static KompileMetaInfo deserialize(String json) {
        Gson gson = new Gson();
        KompileMetaInfo info = gson.fromJson(json, KompileMetaInfo.class);
        return info;
    }
}
