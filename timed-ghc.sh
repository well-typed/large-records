#!/bin/bash

if [[ "$*" == *-no-link* ]]
then
  /usr/bin/time -f "%e\t%M" -a -o memory.log -- ghc "$@"
else
  ghc "$@"
fi


