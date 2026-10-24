CXX=clang++

SOURCES=gen.cc config-line.cc ../utils.cc
HEADERS=utils.hh

OBJECTS=$(patsubst %.cc,%.o,$(SOURCES))

CXXFLAGS=-std=c++20 -g3

all: gen

gen: $(OBJECTS)
	$(CXX) $(CXXFLAGS) -o gen $(OBJECTS)

.PHONY: clean run

run:
	@./gen

clean:
	@rm -f gen $(OBJECTS)
