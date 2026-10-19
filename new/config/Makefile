CXX=clang++

SOURCES=$(wildcard *.cc)
HEADERS=$(wildcard *.hh)
OBJECTS=$(patsubst %.cc,%.o,$(SOURCES))

CXXFLAGS=-std=c++20 -g3

all: gen

.depend: $(SOURCES) $(HEADERS)
	@rm -f .depend
	@$(CC) $(CXXFLAGS) -MM $^ >> .depend;

-include .depend

gen: .depend $(OBJECTS)
	$(CXX) $(CXXFLAGS) -o gen $(OBJECTS)

clean:
	@rm -f .depend gen $(OBJECTS)

format:
	@clang-format -i $(SOURCES) $(HEADERS)

.PHONY: format
