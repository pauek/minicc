CXX=clang++

OBJECTS=main.o test.o input.o parser.o ast.o token.o value.o \
   prettypr.o astpr.o interpreter.o walker.o

CXXFLAGS=-std=c++11

all: minicc

debug: CXXFLAGS += -g3 -O0
debug: minicc

release: CXXFLAGS += -O3
release: minicc

minicc: $(OBJECTS)
	$(CXX) -o minicc $(OBJECTS)

token.o:       token.hh
ast.o:         ast.hh input.hh token.hh
input.o:       ast.hh input.hh token.hh
parser.o:      ast.hh input.hh token.hh parser.hh
astpr.o:       ast.hh astpr.hh
prettypr.o:    ast.hh prettypr.hh
interpreter.o: ast.hh value.hh interpreter.hh
value.o:       value.hh
walker.o:      ast.hh walker.hh
test.o:        ast.hh input.hh token.hh value.hh parser.hh astpr.hh prettypr.hh interpreter.hh
main.o:        ast.hh input.hh token.hh value.hh parser.hh astpr.hh prettypr.hh interpreter.hh walker.hh test.hh

clean:
	rm -f minicc $(OBJECTS)
