// Copyright (c) 2015 K Team. All Rights Reserved.
package org.kframework.krun;

import java.io.InputStream;
import java.io.InputStreamReader;

import org.kframework.kil.Attributes;
import org.kframework.transformation.Transformation;
import org.kframework.utils.file.FileUtil;

public class ReadInputStream implements Transformation<InputStream, String> {

    @Override
    public String run(InputStream p, Attributes attrs) {
        return FileUtil.read(new InputStreamReader(p));
    }

    @Override
    public String getName() {
        return "";
    }

}
