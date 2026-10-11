#!/bin/bash

# Ensure the binary is the latest
make -s

# List all directories within './tests'
DIRS=$(find ./tests -mindepth 1 -maxdepth 1 -type d)

# Run tests in all directories
for dir in $DIRS; do
	echo -n $dir": "
	if [ -f $dir/test.sh ]; then
		(cd $dir; bash ./test.sh)
		if [ ! $? -eq 0 ]; then
			echo "Error executing '$dir/test.sh'"
		fi
	fi
	echo
done
