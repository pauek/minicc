CXX=clang++
OBJECTS=main.o input.o parser.o ast.o prettypr.o astpr.o test.o
CXXFLAGS=-std=c++11

all: minicc

debug: CXXFLAGS += -g
debug: minicc

release: CXXFLAGS += -O3
release: minicc

minicc: $(OBJECTS)
	clang++ -o minicc $(OBJECTS)

ast.o:      ast.hh input.hh
input.o:    ast.hh input.hh
parser.o:   ast.hh input.hh parser.hh
astpr.o:    ast.hh astpr.hh
prettypr.o: ast.hh prettypr.hh
test.o:     ast.hh input.hh parser.hh astpr.hh prettypr.hh
main.o:     ast.hh input.hh parser.hh astpr.hh prettypr.hh test.hh

clean:
	rm -f minicc $(OBJECTS)
