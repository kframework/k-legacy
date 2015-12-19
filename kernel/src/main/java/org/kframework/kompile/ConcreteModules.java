package org.kframework.kompile;

import org.apache.commons.io.FilenameUtils;
import org.kframework.definition.Configuration;
import org.kframework.definition.Module;
import scala.Option;
import scala.collection.immutable.Set;

import java.io.Serializable;
import java.util.Optional;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * Created by Edgar Pek on 9/23/15.
 */
public class ConcreteModules implements Serializable {
    private final String definitionSource;
    private final TreeMap<String, ConcreteSentences> modulesMap;

    public ConcreteModules(String definitionSource, Set<Module> modules) {
        this.definitionSource = definitionSource;
        java.util.Set<Module> topLevelModules = this.getTopLevelModules(modules);
        this.modulesMap = new TreeMap<>();
    }

    public ConcreteSentences getConcreteSentences(String moduleName) {
        assert this.modulesMap.containsKey(moduleName);
        return this.modulesMap.get(moduleName);
    }

    public TreeSet<String> getConcreteModules() {
        return new TreeSet<String>(modulesMap.keySet());
    }

    private Optional<Set<Configuration>>
    getOptionalConfigurationSet(String moduleName, TreeMap<String, Set<Configuration>> moduleToConfiguration) {
        return moduleToConfiguration.containsKey(moduleName) ? Optional.of(moduleToConfiguration.get(moduleName))
                                                             : Optional.empty();
    }

    private java.util.Set<Module> getTopLevelModules(Set<Module> modules) {
        java.util.Set<Module> retSet = scala.collection.JavaConversions.setAsJavaSet(modules);
        return retSet.stream()
                .filter(m -> getSourceFileName(m).contentEquals(FilenameUtils.getName(this.definitionSource)))
                .collect(Collectors.toSet());
    }

    private String getSourceFileName(Module m) {
        Option<String> someSourceName = m.att().get("Source");
        assert(someSourceName.isDefined());
        return FilenameUtils.getName(someSourceName.get());
    }

}
