#!/bin/sh
# Produce git-mktag input for
# a tag named $2 of commit $3,
# taking s/author/tagger/ and message from commit $1
COMMIT=$1
TAG=$2
TARGET=$3
echo object $(git rev-parse "$TARGET")
echo type commit
echo tag "$TAG"
git cat-file commit "$COMMIT" | sed -ne '/^$/,$p; 0,/^$/{s/^author/tagger/;T;p}'
