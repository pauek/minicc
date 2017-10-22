ifndef CXX
	CXX=clang++ # TODO: Fix bug compiling with G++!!
endif

OBJECTS=main.o test.o lexer.o parser.o ast.o token.o value.o types.o \
	prettypr.o prettypr2.o astpr.o \
	interpreter.o stepper.o walker.o translator.o semantic.o

HEADERS=$(wildcard *.hh)

SRCS=$(OBJECTS:.o=.cc)

CXXFLAGS=-std=c++11

all: minicc

depend: .depend

.depend: $(SRCS) $(HEADERS)
	@rm -f ./.depend
	@echo "Computing dependencies..."
	@$(CC) $(CXXFLAGS) -MM $^>>./.depend;

-include .depend

debug: CXXFLAGS += -g3 -O0
debug: minicc

release: CXXFLAGS += -O3
release: minicc

minicc: .depend $(OBJECTS)
	$(CXX) -o minicc $(OBJECTS)

clean:
	rm -f .depend minicc $(OBJECTS)
