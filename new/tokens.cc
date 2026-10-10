#include "tokens.hh"

std::string token_name(TokenID id) {
	switch (id) {
// Include the token file and expand it into many cases
#define TOKEN(id, name) \
	case id: {          \
		return name;    \
	}
#include "tokens.inc"
#undef TOKEN
//
		default: {
			return "<unknown>";
		}
	}
}
