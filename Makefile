# CXX=clang++
CXX=g++

SOURCES=$(wildcard src/*.cc)
HEADERS=$(wildcard src/*.hh)
OBJECTS=$(patsubst src/%.cc,src/%.o,$(SOURCES))

CXXFLAGS=-std=c++17

all: debug

depend: .depend

src/.depend: $(SOURCES) $(HEADERS)
	@rm -f ./src/.depend
	@echo "Computing dependencies..."
	@$(CC) $(CXXFLAGS) -MM $^ >> ./src/.depend;

-include ./src/.depend

debug: CXXFLAGS += -g3 -O0
debug: minicc

release: CXXFLAGS += -O3
release: minicc

minicc: ./src/.depend $(OBJECTS)
	$(CXX) -o minicc $(OBJECTS)

clean:
	rm -f ./src/.depend minicc tester $(OBJECTS)

test: minicc
	@./test.sh
