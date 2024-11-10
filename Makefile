CXX=clang++
# CXX=g++

SOURCES=$(wildcard src/*.cc)
HEADERS=$(wildcard src/*.hh)
OBJECTS=$(patsubst src/%.cc,src/%.o,$(SOURCES))

CXXFLAGS=-std=c++20

all: debug

./src/.depend: $(SOURCES) $(HEADERS)
	@rm -f ./src/.depend
	@$(CC) $(CXXFLAGS) -MM $^ >> ./src/.depend;

-include ./src/.depend

debug: CXXFLAGS += -g3 -O0
debug: minicc

release: CXXFLAGS += -O3
release: minicc

minicc: ./src/.depend $(OBJECTS)
	$(CXX) -o minicc $(OBJECTS)

clean:
	@rm -f ./src/.depend minicc tester $(OBJECTS)

format:
	@clang-format -i $(SOURCES) $(HEADERS)

test: minicc
	@./test.sh
