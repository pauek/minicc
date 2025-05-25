/**
 * What to instrument?
 * - Assignments.
 * - Additions: adds, subtracts, increments, decrements.
 * - Multiplications: multiplications, divisions, modulus.
 * - Reads and writes of each value.
 * - Value copies (in passing vectors, etc.).
 * - Loop iterations.
 * - Memory allocations.
 * - Function calls.
 * - Calls to specific library functions.
 *
 */

#ifndef INSTRUMENTER_HH
#define INSTRUMENTER_HH

#include "ast.hh"

void instrument(AstNode *program);

#endif