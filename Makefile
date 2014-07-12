OBJECTS=main.o input.o

minicc: $(OBJECTS)
	clang++ -o minicc $(OBJECTS)

all: minicc

