CXX=clang++ # CXX=g++
MAKEFLAGS += --no-print-directory

SOURCES=$(wildcard *.cc)
SOURCES += config/config-line.cc
SOURCES += $(wildcard commands/*.cc)
HEADERS=$(wildcard *.hh)
HEADERS += $(wildcard commands/*.hh)
OBJECTS=$(patsubst %.cc,%.o,$(SOURCES))

CXXFLAGS=-std=c++20

all: debug

#

.PHONY: install format clean

format:
	@clang-format -i $(SOURCES) $(HEADERS)

clean:
	@rm -f minicc $(OBJECTS) generated/*.*

install: minicc
	cp minicc $(HOME)/.local/bin


# Generators

# '&:' means that only one invocation is necessary to produce both files!
generated/keywords.inc generated/keyword_switch.inc &: keywords.gen.sh keywords
	@bash keywords.gen.sh

generated/config-levels.cc generated/config-types.hh &: config/gen.cc config.options
	@make -C ./config run

# Dependencies

.depend: $(SOURCES) $(HEADERS) generated/keywords.inc generated/keyword_switch.inc generated/config-levels.cc generated/config-types.hh
	@$(CXX) $(CXXFLAGS) -MM $(SOURCES) $(HEADERS) > .depend;

-include .depend
config.cc: config/config-line.cc

debug: CXXFLAGS += -g3 -O0 -Wall -Wpedantic
debug: minicc

release: CXXFLAGS += -O3
release: minicc



minicc: .depend $(OBJECTS)
	$(CXX) -o minicc $(OBJECTS)

test: minicc
	@./test.sh
