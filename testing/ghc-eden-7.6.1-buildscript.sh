#!/bin/sh -e

# To build Eden-7.6.1, do the following:

# 1) download and unpack a source snapshot from the git repository:

wget -O ghc-eden-2b3adc0.tar.gz --post-data="p=ghc-eden.git;a=snapshot;h=2b3adc000eb58b1d5942e01ce5a027b8b9f0ed36;sf=tgz" http://james.mathematik.uni-marburg.de:8080/gitweb/

tar xzf ghc-eden-2b3adc0.tar.gz
mv ghc-eden-2b3adc0 ghc-eden-7.6.1

# 2) check out the Eden modules repository, branch "ghc-7.6":

cd ghc-eden-7.6.1/libraries
git clone -b ghc-7.6 git://james.mathematik.uni-marburg.de/edenmodules.git

# 3) check out other (incl. extra) libraries, in branch 7.6

cd ..
mkdir .git
./sync-all -r http://darcs.haskell.org --extra get -b ghc-7.6 || \
    (if test (-d 'libraries/xhtml') && test "!( -d libraries/parallel)" ]; then
      echo "probably git refuses to check out master instead of a branch";
      echo "Attempting to recover...";
      ./sync-all -r http://darcs.haskell.org --extra get;
     fi)

# 4) configure and build Eden

perl boot
./configure --prefix=`pwd`
make

# 5) ask the compiler for its version

./inplace/bin/ghc-stage2 --version

# the reply should be 
# "The Parallel Haskell Compilation System, version 7.6.1"

# 6) compile and run your favourite Eden program

# if your favourite Eden program needs the skeleton library, install it into
# the newly built compiler:

# 6a) check out the repository

cd libraries
git clone git://james.mathematik.uni-marburg.de/edenskel.git

# 6b) configure and build the package with the inplace compiler

cd edenskel
../../inplace/bin/ghc-stage2 Setup.hs
./Setup configure --with-compiler="../../inplace/bin/ghc-stage2" --global 
./Setup build
./Setup register --inplace

# continue at step 6.

./inplace/bin/ghc-stage2 --interactive <<EOF
1+1
1+1.0
:m Control.Parallel.Eden
:browse
:q
EOF
