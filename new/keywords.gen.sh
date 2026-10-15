#!/bin/bash
for keyword in $(cat keywords); do
	KEYWORD=$(echo $keyword | tr a-z A-Z)
	echo "KEYWORD($KEYWORD, $keyword)"
done > keywords.inc

# `keywords` is (supposed to be) **in lexicographical order**
(
	KEYWORDS=$(cat keywords)
	CURR=''
	for keyword in $KEYWORDS; do
		first=$(echo $keyword | cut -c1-1)
		KEYWORD=$(echo $keyword | tr a-z A-Z)

		if [ "$first" = "$CURR" ]; then
			echo "    } else if (ident == \"$keyword\") {"
			echo "        tok = $KEYWORD;"
		else
			if ! [ -z "$CURR" ]; then
				echo "    }"
				echo "    break;"
				echo "}"
			fi
			echo "case '$first': {"
			echo "    if (ident == \"$keyword\") {"
			echo "        tok = $KEYWORD;"
			CURR="$first"
		fi
	done
	echo "    }"
	echo "    break;"
	echo "}"
) > keyword_switch.inc
