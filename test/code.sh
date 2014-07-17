#!/bin/bash
sed -n '0,/[[out]]/p' $1 | head -n -1