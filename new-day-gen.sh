#!/usr/bin/env bash

d=day-${1}

echo -e "module Main where\n\nmain = print \"${d}\"" > ./solutions/${d}.hs
echo > ./inputs/${d}.example
echo > ./inputs/${d}.input
echo -e "  ${d}:\n    main: ${d}.hs" >> package.yaml 