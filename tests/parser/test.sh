#!/bin/bash

function run_test() {
	# All tests are a pair of files `A.cc`-`A.out`
	filename_cc=$1
	filename=${filename_cc%.cc}

	# Run with output to a temporary file
	output=$(mktemp /tmp/minicc-output.XXXXXX)
	errors=$(mktemp /tmp/minicc-errors.XXXXXX)

	# Remove the prefix `./` from `find`
	clean_filename_cc=${filename_cc#./}

	# Run `minicc` with the test
	../../minicc parse $clean_filename_cc > $output 2> $errors
	RETURN_CODE=$?

	RESULT="."

	# Execution error: we expect the error output to be non-empty.
	if [ $RETURN_CODE != 0 ]; then
		if [ -f $filename.err ]; then
			# 1. Compare both, keep .diff if non-empty
			diff $errors $filename.err > $filename.err.diff
			if [ -s "$filename.err.diff" ]; then
				RESULT="X" # FAIL (should give the exact error in $filename.err)
			else
				RESULT="." # PASS
				rm -f $filename.err.diff
			fi
		elif [ -f $errors ]; then
			# 2. If there isn't an .out file, create the .err file (assume it is the first run),
			if [ ! -f $filename.out ]; then
				RESULT="e"
				cp $errors $filename.err
			else
				RESULT="Y"
				cp $errors $filename.err.diff
			fi
		else
			# This could mean the program is not reporting properly
			echo "Warning: the error output is empty and the return code is != 0 ($?)."
		fi
	fi

	# Now look at the output
	if [ -f $filename.out ]; then
		# 1. Compare both, keep .diff if non-empty
		diff $output $filename.out > $filename.out.diff
		if [ -s "$filename.out.diff" ]; then
			RESULT="X" # FAIL (should give the exact error in $filename.out)
		else
			RESULT="." # PASS
			rm -f $filename.out.diff
		fi
	elif [ -f $output ]; then
		# 2. Create the .out file (assume it is the first run), only if the RETURN_CODE was OK.
		if [ $RETURN_CODE = 0 ]; then
			RESULT="o"
			cp $output $filename.out
		fi
	fi

	# Show result
	echo -n $RESULT

	# Remove temporary file
	rm -f $output $errors
}

for file in $(find . -name "*.cc" -type f); do
	run_test $file
done
