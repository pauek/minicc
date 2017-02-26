#!/bin/bash
clang++ -DDEBUG --std=c++11 -o minicc main.cc atom.cc lexer.cc file.cc debug.cc
# clang++ -O3 --std=c++11 -o minicc main.cc atom.cc lexer.cc file.cc
