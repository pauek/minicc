#!/bin/bash
sed -rn '0,/\[\[(out|in|err)\]\]/p' $1 | head -n -1