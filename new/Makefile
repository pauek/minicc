CXX=clang++
# CXX=g++

SOURCES=$(wildcard *.cc)
HEADERS=$(wildcard *.hh)
OBJECTS=$(patsubst %.cc,%.o,$(SOURCES))

CXXFLAGS=-std=c++20

all: debug

.depend: $(SOURCES) $(HEADERS)
	@rm -f .depend
	@$(CC) $(CXXFLAGS) -MM $^ >> .depend;

-include .depend

debug: CXXFLAGS += -g3 -O0
debug: minicc

release: CXXFLAGS += -O3
release: minicc

minicc: .depend $(OBJECTS)
	$(CXX) -o minicc $(OBJECTS)

clean:
	@rm -f .depend minicc tester $(OBJECTS)

format:
	@clang-format -i $(SOURCES) $(HEADERS)

test: minicc
	@./test.sh
