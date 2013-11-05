#!/usr/bin/env bash

cabal configure --enable-tests > /dev/null &&\
cabal build > /dev/null &&\
./dist/build/Pone/Pone.exe
