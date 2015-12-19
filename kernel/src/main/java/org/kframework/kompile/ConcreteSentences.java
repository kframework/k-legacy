package org.kframework.kompile;

import org.kframework.attributes.Location;
import org.kframework.definition.Configuration;
import org.kframework.definition.Sentence;

import java.io.Serializable;
import java.util.*;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import scala.Option;
import scala.collection.immutable.Set;

/**
 * Created by Edgar Pek on 9/23/15.
 */
public class ConcreteSentences implements Serializable, Iterable<Sentence>{
    private TreeSet<Sentence> sentences;

    public ConcreteSentences(Set<Sentence> sentenceSet, Optional<Set<Configuration>> configurationSetOptional) {
        this.sentences = new TreeSet<Sentence>(new SortByLocation());
        java.util.Set<Sentence> javaSentenceSet = scala.collection.JavaConversions.setAsJavaSet(sentenceSet);
        for(Sentence s : javaSentenceSet) {
            if (s.att().attMap().contains("Location")
                    || s.att().attMap().contains("org.kframework.attributes.Location")) {
                this.sentences.add(s);
            }
            //else {
            //  (Edgar Pek) What to do with the sentence w/o location, is there some loss of information?
            //    System.out.println("[DEBUG] Sentences w/o location: " + s.toString());
            //}

        }
        if (configurationSetOptional.isPresent()) {
            Set<Configuration> configurationSet = configurationSetOptional.get();
            java.util.Set<Configuration> javaConfigurationSet
                    = scala.collection.JavaConversions.setAsJavaSet(configurationSet);

            javaConfigurationSet.stream().forEach(c -> replaceWithConfig(c));

        }
    }

    private void replaceWithConfig(Configuration c) {
        Supplier<TreeSet<Sentence>> supplier =() -> new TreeSet<Sentence>(new SortByLocation());
        this.sentences = this.sentences
                        .stream()
                        .filter(sentence -> !haveEqualLocations(getSentenceLocation(sentence),
                                                                getSentenceLocation(c)))
                        .collect(Collectors.toCollection(supplier));
        this.sentences.add(c);
    }

    private class SortByLocation implements Comparator<Sentence>, Serializable {
        @Override
        public int compare(Sentence s1, Sentence s2) {
            // TODO: test compareTo of locations
            return getSentenceLocation(s1).compareTo(getSentenceLocation(s2));
        }
    }

    private boolean haveEqualLocations(Location x, Location y) {
        return  x.startLine() == y.startLine()
                && x.startColumn() == y.startColumn()
                && x.endLine() == y.endLine()
                && x.endColumn() == y.endColumn();

    }
    private Location getSentenceLocation(Sentence s) {
        Option<Location> someLoc = s.att().get("Location");
        Option<Location> someLocAtt = s.att().get("org.kframework.attributes.Location");
        assert(someLoc.isDefined() || someLocAtt.isDefined());
        if (someLoc.isDefined()) return someLoc.get();
        else                     return someLocAtt.get();
    }
    @Override
    public Iterator<Sentence> iterator() {
        return sentences.iterator();
    }
}
