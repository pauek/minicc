CXX=clang++
OBJECTS=main.o input.o parser.o ast.o prettyprint.o
CXXFLAGS=-std=c++11

all: minicc

minicc: $(OBJECTS)
	clang++ -o minicc $(OBJECTS)

input.o: input.hh
ast.o: ast.hh
parser.o: parser.hh ast.hh input.hh
prettyprint.o: prettyprint.hh ast.hh
main.o: parser.hh ast.hh input.hh

clean:
	rm -f minicc $(OBJECTS)
