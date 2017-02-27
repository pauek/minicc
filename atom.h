/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(DECLARATION)


#include <assert.h>
#include <string.h> // strncmp
#include <stdlib.h> // malloc

#define ATOM_NUM_NODES (1<<12) // 4096 -- This has to be a power of two!

namespace atom {

struct Atom {
   size_t      len;
   const char *str;
};

struct Node {
   Atom  atom;
   Node *prev;
};

extern Node *nodes[ATOM_NUM_NODES];

#define TOKEN(name, str, len) extern Atom *_##name##_;
#include "tokens.inc"
#undef TOKEN

void  init();
Atom *get(const char *str, size_t len);
Atom *get(const char *str);

} // namespace atom

#endif // DECLARATION
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
#if defined(IMPLEMENTATION)


namespace atom {

Node *nodes[ATOM_NUM_NODES] = {}; // important to fill with zeros!

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
#define TOKEN(name, str, len) Atom *_##name##_;
#include "tokens.inc"
#undef TOKEN

// Register an atom for each token
void init() {
#define TOKEN(name, str, len) _##name##_ = get(str, len);
#include "tokens.inc"
#undef TOKEN
}

Atom *get(const char *str) {
   return get(str, strlen(str));
}

Atom *get(const char *str, size_t len) {
	uint32_t mask = ATOM_NUM_NODES-1;
	uint32_t idx  = hash(str, len) & mask;
	Node *n;
	for (n = nodes[idx]; n; n = n->prev) {
		if (n->atom.len == len && 0 == strncmp(n->atom.str, str, len)) {
			return &n->atom;
		}
	}
	n = (Node *)malloc(sizeof(Node));
	n->atom.str = str;
	n->atom.len = len;
	n->prev = nodes[idx];
	nodes[idx] = n;
	return &n->atom;
}

} // namespace atom


#endif // IMPLEMENTATION
/////////////////////////////////////////////////////////////////////////////////////////////
