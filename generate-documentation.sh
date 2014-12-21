#!/bin/bash

# Generates the documentation and puts it in the gh-pages branch
# Taken from https://github.com/jspahrsummers/documentalist/blob/master/generate-pages and modified.

# can run this script in git hooks, can use
# "`git branch | grep '\*' | cut -f2 -d' '`" == "master"
# to check that the branch is set to master.

# i use this in my hook:

#BRANCH=`git branch | grep '\*' | cut -f2 -d' '`
#
#if [ $BRANCH == "master" ]
#then
#    echo "Generating Haddock Documentation in gh-pages"
#    ./generate-documentation.sh
#    git checkout master
#fi


set -x
set -o

cabal-dev configure --disable-tests

INTDOCDIR=`mktemp -d -t pages.XXXXXX`
PUBDOCDIR=`mktemp -d -t pages.XXXXXX`

cabal-dev haddock --hyperlink-source --internal --haddock-options=-o$INTDOCDIR
cabal-dev haddock --hyperlink-source  --haddock-options=-o$PUBDOCDIR

HEAD=`git rev-parse HEAD`

git checkout gh-pages
git rm -rf --ignore-unmatch .

echo "cabal-dev/" > .gitignore

mkdir internal
mkdir public

mv $INTDOCDIR/* internal/
mv $PUBDOCDIR/* public/

rm -rf dist/

git add -A

git commit -m "Generated documentation from $HEAD"
