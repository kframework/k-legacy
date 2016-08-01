// Copyright (c) 2015-2016 K Team. All Rights Reserved.
package org.kframework;

import org.kframework.definition.DefinitionTransformer;
import org.kframework.definition.Module;
import org.kframework.definition.ModuleTransformer;
import org.kframework.kompile.ResolveConfig;
import org.kframework.parser.concrete2kore.generator.RuleGrammarGenerator;
import org.kframework.definition.Definition;

/**
 * Contains a curated list of compiler passes.
 * Please only add short code constructing the passes here, not full implementations.
 *
 * The passes are methods with one of the following signatures:
 * Definition -> Definition
 * Module -> Module
 * Definition x Module -> Module // when only changing a module, but require information from the entire definition for the change
 * Sentence -> Sentence
 * Module x Sentence-> Sentence // when only changing a sentence, but require information from the entire module for the change
 * Definition x Sentence -> Sentence // when only changing a sentence, but require information from the entire defintion for the change
 */

@API
public class Kompiler {
    /**
     * Generates the definition containing the modules appropriate for generating rule parsers.
     */
    public static Definition toRuleParser(Definition d) {
        return DefinitionTransformer.fromHybrid((Module m) -> RuleGrammarGenerator.getRuleGrammar(m, s -> d.getModule(s).get()), "toRuleParser").apply(d);
    }

    public static Definition configurationSentencesToSyntaxAndRules(Definition d) {
        ResolveConfig resolveConfig = new ResolveConfig(d, true);
        return DefinitionTransformer.from(resolveConfig, "toRuleParser").apply(d);
    }

    /**
     * Generates the module appropriate for generating the parser of a partial configuration,
     * with the exact cell labels not known apriori.
     */
    public static Definition toGenericAbstractConfigurationParser(Definition d) {
        return DefinitionTransformer.fromHybrid((Module m) -> RuleGrammarGenerator.getRuleGrammar(m, s -> d.getModule(s).get()), "toGenericAbstractConfigurationParser").apply(d);
    }

}
