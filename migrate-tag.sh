#!/bin/bash
REF=$1
BRANCH=${2:-master}
REFTREE=$(git rev-parse "$REF":)
if [[ $(git merge-base -a "$BRANCH" "$REF" | wc -w) != 1 ]]; then
  echo no unique merge-base for $REF on branch $BRANCH
  exit 1
fi
MERGE=$(git merge-base "$BRANCH" "$REF")
MERGETREE=$(git rev-parse "$MERGE":)
if [[ $REFTREE = $MERGETREE ]]; then
  BASE=$(basename "$REF")
  echo Tagging treesame merge-base of $REF as $BASE
  TAG=$($(dirname $0)/conv-tag.sh "$REF" $BASE $MERGE | git mktag)
  git update-ref refs/tags/$BASE $TAG ''
else
  BASE=$(basename "$REF")
  echo Tagging $REF as $BASE and its non-treesame merge-base as lca/$BASE
  git tag $BASE $REF
  git tag lca/$BASE $MERGE
fi
