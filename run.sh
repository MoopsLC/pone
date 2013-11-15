#!/usr/bin/env bash
#path to pone_src/
test_dir="pone_src/"
#cabal configure --enable-tests > /dev/null
cabal build > /dev/null &&\
./dist/build/Pone/Pone.exe "$test_dir"
