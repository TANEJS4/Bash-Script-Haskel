#!/bin/bash
# part1
local=(git rev-parse master)  #gives an output
remote=(git rev-parse remote/master)  #gives an output
if [ $local == $remote ] 
then
    echo "Local Repository is up to date with Remote Repository"
else
    echo "Local Repository is not up to date with Remote Repository"
fi


a1=(git fetch origin)

#part 2

git diff $a1 >> changes.log


#part 3

grep -r --exclude=todo.log "#TODO" $1 >> todo.log 


#part 4


ghc -fno-code *.hs $1 &>> error.log  

#part 5
