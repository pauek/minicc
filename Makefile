CXX=clang++
OBJECTS=main.o input.o parser.o ast.o prettypr.o astpr.o walker.o test.o token.o
CXXFLAGS=-std=c++11

all: minicc

debug: CXXFLAGS += -g
debug: minicc

release: CXXFLAGS += -O3
release: minicc

minicc: $(OBJECTS)
	clang++ -o minicc $(OBJECTS)

token.o:    token.hh
ast.o:      ast.hh input.hh token.hh
input.o:    ast.hh input.hh token.hh
parser.o:   ast.hh input.hh token.hh parser.hh
astpr.o:    ast.hh astpr.hh
prettypr.o: ast.hh prettypr.hh
walker.o:   ast.hh walker.hh
test.o:     ast.hh input.hh token.hh parser.hh astpr.hh prettypr.hh
main.o:     ast.hh input.hh token.hh parser.hh astpr.hh prettypr.hh walker.hh test.hh

clean:
	rm -f minicc $(OBJECTS)
