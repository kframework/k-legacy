package org.kframework.kast;

import org.kframework.attributes.Source;
import org.kframework.kore.K;
import org.kframework.kore.Sort;
import org.kframework.minikore.MiniKore;
import org.kframework.minikore.MiniToKore;
import org.kframework.parser.ParseResult;
import org.kframework.parser.UserParser;
import org.kframework.utils.BinaryLoader;
import org.kframework.utils.errorsystem.KEMException;
import org.kframework.utils.errorsystem.KExceptionManager;
import org.kframework.utils.file.FileUtil;

import java.util.stream.Collectors;

public class Kast {

    public static K parse(String toParse, Source source, Sort startSymbol, String moduleName, FileUtil files, KExceptionManager kem) {
        return parse(toParse, source.source(), startSymbol.name(), moduleName, files, kem);
    }

    public static K parse(String toParse, String source, String startSymbol, String moduleName, FileUtil files, KExceptionManager kem) {
        String modulePath = files.moduleDerivedParserPath(moduleName);
        BinaryLoader loader = new BinaryLoader(kem);
        UserParser parser;
        try {
            parser = loader.loadOrDie(UserParser.class, files.resolveKompiled(modulePath));
        }catch (KEMException e) {
            throw KEMException.innerParserError("Module " + moduleName + " not found.");
        }
        ParseResult result = parser.parse(toParse, source, startSymbol);
        MiniKore.Pattern ast = result.ast;
        kem.addAllKException(result.warnings.stream().map(e->e.getKException()).collect(Collectors.toSet()));
        return MiniToKore.apply(ast);
    }
}
