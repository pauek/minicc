#a
#a b c
#include "ab"
#include <ab>
#a \
  b c

int f() {}

#ifndef BUBI
#define BUBI "yay!"

#another macro

int g() {}

#pragma whatever 1 2 3 4 'a' "b"

#pragma whatever \
	1 2 3 4 \
	'a' "b"

#endif
