#!/bin/bash

while true
do
echo "Enter yes / no "
read n
if [ $n = "yes" ] ; then
	git pull
	
	break
elseif [ $n = "no" ]
	break
else
	echo "Pl enter yes / no ONLY ... Press enter to continue"
	read phew
fi
done
