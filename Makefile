CXX=clang++
OBJECTS=main.o input.o parser.o ast.o prettyprinter.o astprinter.o test.o
CXXFLAGS=-std=c++11 -g3

all: minicc

minicc: $(OBJECTS)
	clang++ -o minicc $(OBJECTS)

input.o: input.hh
ast.o: ast.hh
parser.o: parser.hh ast.hh input.hh
astprinter.o: astprinter.hh ast.hh
prettyprinter.o: prettyprinter.hh ast.hh
test.o: parser.hh prettyprinter.hh astprinter.hh
main.o: parser.hh ast.hh input.hh

clean:
	rm -f minicc $(OBJECTS)
