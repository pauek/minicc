#!/bin/bash
cm c++ lexer.c.lisp > lexer.cc
g++ -o lexer lexer.cc