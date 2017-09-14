#!/bin/bash
cm c lexer.c.lisp > lexer.c
gcc -o lexer lexer.c