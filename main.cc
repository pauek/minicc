
#include <stdlib.h>
#include <stdio.h>
#include "minicc.hh"

int main() {
	Atom *a1 = atom_get("hola", 4);
	Atom *a2 = atom_get("blabla", 6);
	Atom *a3 = atom_get("hola", 4);
	Atom *a4 = atom_get("jiji", 4);
	Atom *a5 = atom_get("jiji", 4);
	printf("%p\n", a1);
	printf("%p\n", a2);
	printf("%p\n", a3);
	printf("%p\n", a4);
	printf("%p\n", a5);
}