#!/bin/bash
ERRFILE=errors.log

echo
directories=$(find tests -mindepth 1 -maxdepth 1 -type d | sed 's|tests/||' | grep -v ^_ | sort)
rm -f $ERRFILE
for dir in $directories; do
	files=$(find tests/${dir} -name "*.cc")
	./minicc test ${dir} ${files} 2>> $ERRFILE 
done
errsize=$(wc -c $ERRFILE | cut -d' ' -f1)
if [ $errsize -ne 0 ]; then
	echo
	echo "Errors written to '$ERRFILE'"
fi