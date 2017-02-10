
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include "minicc.hh"

#define NUM_NODES (1<<11) // 2048 -- This has to be a power of two!

struct Node {
	Atom  atom;
	Node *prev;
};

static Node *nodes[NUM_NODES] = {}; // important to fill with zeros!

static uint32_t hash(const char *p, size_t len) {
    // FNV hash
    uint32_t r = 2166136261;
    for (uint32_t i = 0; i < len; i++) {
        r ^= *p++;
        r *= 16777619;
    }
    return r;
}

Atom *atom_get(const char *str, size_t len) {
	uint32_t mask = NUM_NODES-1;
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

