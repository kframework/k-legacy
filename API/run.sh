#!/bin/bash -x

java -Djava.awt.headless=true -Djava.library.path=$(dirname "$0")/../k-distribution/target/release/k/lib/native/osx -Xms64m -Xmx4096m -ea -cp "$(dirname "$0")/target/*:$(dirname "$0")/../k-distribution/target/release/k/lib/java/*" org.kframework.KRunAPI "$@"
