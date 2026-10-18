#!/bin/bash

IFS=   # <-- CRITICAL since it affects 'read', which will not use _spaces_ as a separator
cat config | while read line; do
	level=$(echo $line | tr -cd '[:space:]' | wc -c)
	conf=$(echo $line | tr -d '[:space:]')
	echo $level $conf
done
