// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework.kast;

import org.kframework.attributes.Source;
import org.kframework.kompile.ParserGenerator;
import org.kframework.kore.K;
import org.kframework.kore.Sort;
import org.kframework.minikore.converters.MiniToKore;
import org.kframework.minikore.interfaces.pattern;
import org.kframework.parser.ParseResult;
import org.kframework.parser.UserParser;
import org.kframework.utils.BinaryLoader;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class Kast {

    private transient Map<String, UserParser> cachedParsers = new HashMap<>();
    private ParserGenerator generator = null;
    private FileUtil files;

    public Kast(FileUtil files){
        this.files = files;
    }

    public static K parseWithModuleParser(String toParse, Source source, Sort startSymbol, String moduleName,
                                          FileUtil files, KExceptionManager kem) {
        return parseWithModuleParser(toParse, source.source(), startSymbol.name(), moduleName, files, kem);
    }

    // This function is used for one-time parsing. If the corresponding parser object exists in the extra directory, the
    // function deserialize the object and use it to parse the text. If the parser object does not exist, the function
    // will load the parser generator to generate one and then parse the text.
    public static K parseWithModuleParser(String toParse, String source, String startSymbol, String moduleName,
                                          FileUtil files, KExceptionManager kem) {
        String modulePath = files.moduleDerivedParserPath(moduleName);
        BinaryLoader loader = new BinaryLoader(kem);
        UserParser parser;
        File moduleFile = files.resolveKompiled(modulePath);
        if(moduleFile.exists()) {
            parser = loader.loadOrDie(UserParser.class, files.resolveKompiled(modulePath));
        } else {
            ParserGenerator generator;
            try {
                generator = loader.loadOrDie(ParserGenerator.class, files.resolveKompiled(FileUtil.PARSER_GENERATOR_BIN));
            } catch (KEMException e) {
                throw KEMException.innerParserError("Parser Generator not found.");
            }
            parser = generator.getParser(moduleName, kem);
            loader.saveOrDie(moduleFile, parser);
        }

        ParseResult result = parser.parse(toParse, source, startSymbol);
        pattern.Pattern ast = result.ast;
        kem.addAllKException(result.warnings.stream().map(e->e.getKException()).collect(Collectors.toSet()));
        return MiniToKore.apply(ast);
    }

    // This function is used for parsing the texts many times during the execution. The parser will be cached in order to be
    // reused later. This function will not load the parser object from the directory.
    public K parseWithUserParserAndCache(String toParse, String source, String startSymbol, String moduleName,
                                         KExceptionManager kem) {
        if(cachedParsers == null) {
            cachedParsers = new HashMap<>();
        }
        UserParser parser;
        parser = cachedParsers.get(moduleName);
        if(parser == null) {
            if(this.generator == null) {
                try {
                    BinaryLoader loader = new BinaryLoader(kem);
                    this.generator = loader.loadOrDie(ParserGenerator.class, files.resolveKompiled(FileUtil.PARSER_GENERATOR_BIN));
                }catch (KEMException e) {
                    throw KEMException.innerParserError("Parser Generator can not be deserialized.");
                }
            }
            parser = this.generator.getParser(moduleName, kem);
            cachedParsers.put(moduleName, parser);
        }

        ParseResult result = parser.parse(toParse, source, startSymbol);
        pattern.Pattern ast = result.ast;
        kem.addAllKException(result.warnings.stream().map(e->e.getKException()).collect(Collectors.toSet()));
        return MiniToKore.apply(ast);
    }

}
