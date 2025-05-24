CXX=clang++-19
# CXX=g++
CXXFLAGS=-std=c++20 -fxray-instrument

SOURCES=$(wildcard *.cc)
HEADERS=$(wildcard *.hh)
OBJECTS=$(patsubst %.cc,%.o,$(SOURCES))

all: debug


.dependencies: $(SOURCES) $(HEADERS)
	@echo -n "Computing dependencies... "
	@$(CXX) $(CXXFLAGS) -MM $^ > .dependencies
	@echo done.

-include .dependencies

src/version.hh: Makefile
	VERSION := $(shell date +%s)
	@echo "#define VERSION $(VERSION)" > src/version.hh
	@echo "New version ($(VERSION))"

debug: CXXFLAGS += -g3 -O0
debug: minicc

release: CXXFLAGS += -O3 -DNDEBUG
release: minicc

minicc: .dependencies $(OBJECTS)
	$(CXX) -o minicc $(CXXFLAGS) $(OBJECTS)

clean:
	@rm -f .dependencies minicc $(OBJECTS)

format:
	@clang-format -i $(SOURCES) $(HEADERS)

.PHONY: clean format 