CXX=clang++

SOURCES=gen.cc ../utils.cc
HEADERS=utils.hh

OBJECTS=$(patsubst %.cc,%.o,$(SOURCES))

CXXFLAGS=-std=c++20 -g3

all: gen

gen: $(OBJECTS)
	$(CXX) $(CXXFLAGS) -o gen $(OBJECTS)

generate:
	@./gen

clean:
	rm -f .depend gen $(OBJECTS)

.PHONY: clean generate
