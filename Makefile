CXX=clang++
OBJECTS=main.o input.o parser.o ast.o prettyprint.o test_parser.o
CXXFLAGS=-std=c++11 -g3

all: minicc

minicc: $(OBJECTS)
	clang++ -o minicc $(OBJECTS)

input.o: input.hh
ast.o: ast.hh
parser.o: parser.hh ast.hh input.hh
test_parser.o: parser.hh prettyprint.hh
prettyprint.o: prettyprint.hh ast.hh
main.o: parser.hh ast.hh input.hh

clean:
	rm -f minicc $(OBJECTS)
