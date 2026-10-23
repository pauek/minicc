CXX=clang++ # CXX=g++
MAKEFLAGS += --no-print-directory

SOURCES=$(wildcard *.cc)
HEADERS=$(wildcard *.hh)
OBJECTS=$(patsubst %.cc,%.o,$(SOURCES))

CXXFLAGS=-std=c++20

all: debug

.depend: $(SOURCES) $(HEADERS)
	@rm -f .depend
	@$(CC) $(CXXFLAGS) -MM $^ >> .depend;

-include .depend

# This dependencies are _merged_ (https://stackoverflow.com/questions/29534583/add-dependencies-to-existing-make-targets)
lexer.cc: keywords.inc tokens.inc lexer.hh tokens.hh
config.cc: config.levels.cc.inc config.types.cc.inc config.hh

debug: CXXFLAGS += -g3 -O0 -Wall -Wpedantic
debug: minicc

release: CXXFLAGS += -O3
release: minicc

minicc: .depend $(OBJECTS)
	$(CXX) -o minicc $(OBJECTS)

format:
	@clang-format -i $(SOURCES) $(HEADERS)

.PHONY: install format clean

clean:
	@rm -f .depend minicc $(OBJECTS) keywords.inc keyword_switch.inc config.levels.cc.inc config.types.cc.inc

test: minicc
	@./test.sh

install: minicc
	cp minicc $(HOME)/.local/bin

# Generators
# '&:' means that only one invocation is necessary to produce both files!
keywords.inc keyword_switch.inc &: keywords.gen.sh keywords
	@bash keywords.gen.sh
config.levels.cc.inc config.types.cc.inc &: config/gen.cc config.options
	@make -C ./config run
