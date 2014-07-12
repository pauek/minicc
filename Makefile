CXX=clang++
OBJECTS=main.o input.o parser.o

minicc: $(OBJECTS)
	clang++ -o minicc $(OBJECTS)

all: minicc

clean:
	rm -f minicc $(OBJECTS)
