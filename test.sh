#!/bin/bash
directories=$(find tests -mindepth 1 -maxdepth 1 -type d | sed 's|tests/||' | sort)
for dir in $directories; do
	files=$(find tests/${dir} -name "*.cc")
	./minicc test ${dir} ${files}
done
