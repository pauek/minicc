CXX=clang++
OBJECTS=main.o input.o parser.o prettyprint.o

all: minicc

minicc: $(OBJECTS)
	clang++ -o minicc $(OBJECTS)

input.o: input.hh
parser.o: parser.hh ast.hh input.hh
prettyprint.o: prettyprint.hh ast.hh
main.o: parser.hh ast.hh input.hh

clean:
	rm -f minicc $(OBJECTS)
