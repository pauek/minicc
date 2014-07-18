CXX=clang++
OBJECTS=main.o input.o parser.o ast.o prettyprinter.o astprinter.o test.o
CXXFLAGS=-std=c++11

all: minicc

debug: CXXFLAGS += -g
debug: minicc

release: CXXFLAGS += -O3
release: minicc

minicc: $(OBJECTS)
	clang++ -o minicc $(OBJECTS)

input.o: input.hh
ast.o: ast.hh
parser.o: parser.hh ast.hh ast.o input.hh input.o
astprinter.o: astprinter.hh ast.hh ast.o
prettyprinter.o: prettyprinter.hh ast.hh ast.o
test.o: parser.hh prettyprinter.hh prettyprinter.o astprinter.hh astprinter.o
main.o: parser.hh ast.hh input.hh

clean:
	rm -f minicc $(OBJECTS)
