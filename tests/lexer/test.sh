#!/bin/bash

function run_test() {
	# All tests are a pair of files `A.cc`-`A.out`
	filename=$1
	STEM=${filename%.cc}
	expected=$STEM.out

	# Run with output to a temporary file
	output=$(mktemp /tmp/minicc-output.XXXXXX)
	errors=$(mktemp /tmp/minicc-errors.XXXXXX)

	# Remove the prefix `./` from `find`
	clean_filename=${filename#./}

	# Run `minicc` with the test
	../../minicc lex $clean_filename > $output 2> $errors

	# Remove the .diff and .err files to avoid confusion
	rm -f $STEM.diff $STEM.err

	# Look for execution errors
	if [ $? != 0 ] || [ -s $errors ]; then
		echo -n "E"
		cp $errors $STEM.err
	elif [ ! -f $expected ]; then
		# If the .out file does not exist, we will assume
		# that the first run stores it from the execution
		echo -n "!"
		cp $output $expected
	else
		# If execution was ok and the $expected exists, look at the diff
		DIFF=$(diff $output $expected)
		if [ -z "$DIFF" ]; then
			echo -n "." # PASS
		else
			echo -n "X" # FAIL
			echo "$DIFF" > $STEM.diff
		fi
	fi

	# Remove temporary file
	rm -f $output $errors
}

for file in $(find . -name "*.cc" -type f); do
	run_test $file
done
