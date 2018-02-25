#!/bin/bash
# part1
function checkstatus()
{
git fetch origin 
echo $(git status)
}

a1=(git fetch origin)

#part 2
function redirectChanges()
{
git diff $a1 >> changes.log

}
#part 3
function todo()
{
grep -r --exclude=todo.log "#TODO" $1 >> todo.log 

}
#part 4
function errors()
{

ghc -fno-code *.hs $1 &>> error.log  
}
#part 5
function feature()
{
while true
do
echo "Enter yes / no "
read n
if [ $n = "yes" ] ; then
        git pull
        git status
        git diff HEAD
        break
elseif [ $n = "no" ]
        break
else
    	echo "Please enter yes / no ONLY"
        read phew
fi
done
}
