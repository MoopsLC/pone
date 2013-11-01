#!/usr/bin/env bash
cabal configure > /dev/null && cabal build > /dev/null && ./dist/build/Pone/Pone.exe
