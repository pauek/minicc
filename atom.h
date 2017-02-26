/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(DECLARATION)


#include <assert.h>
#include <string.h> // strncmp
#include <stdlib.h> // malloc

#define ATOM_NUM_NODES (1<<12) // 4096 -- This has to be a power of two!

struct Atom {
   size_t      len;
   const char *str;
};

struct Atom__Node {
   Atom       atom;
   Atom__Node *prev;
};

extern Atom__Node *nodes[ATOM_NUM_NODES];

#define TOKEN(name, str, len) extern Atom *atom_##name;
#include "tokens.inc"
#undef TOKEN

void  atom_init();
Atom *atom_get(const char *str, size_t len);


#endif // DECLARATION
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(IMPLEMENTATION)


Atom__Node *nodes[ATOM_NUM_NODES] = {}; // important to fill with zeros!

static uint32_t hash(const char *p, size_t len) {
    // FNV hash
    uint32_t r = 2166136261;
    for (uint32_t i = 0; i < len; i++) {
        r ^= *p++;
        r *= 16777619;
    }
    return r;
}

// Token atoms
#define TOKEN(name, str, len) Atom *atom_##name;
#include "tokens.inc"
#undef TOKEN

// Register an atom for each token
void atom_init() {
#define TOKEN(name, str, len) atom_##name = atom_get(str, len);
#include "tokens.inc"
#undef TOKEN
}

Atom *atom_get(const char *str, size_t len) {
	uint32_t mask = ATOM_NUM_NODES-1;
	uint32_t idx  = hash(str, len) & mask;
	Atom__Node *n;
	for (n = nodes[idx]; n; n = n->prev) {
		if (n->atom.len == len && 0 == strncmp(n->atom.str, str, len)) {
			return &n->atom;
		}
	}
	n = (Atom__Node *)malloc(sizeof(Atom__Node));
	n->atom.str = str;
	n->atom.len = len;
	n->prev = nodes[idx];
	nodes[idx] = n;
	return &n->atom;
}


#endif // IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////////////////////
