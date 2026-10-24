#ifndef HL_TOKENS_HH
#define HL_TOKENS_HH

#include <cstdint>
#include <vector>

#include "tokens.hh"

enum HLTokenID {
#define HLTOKEN(id) id,
#include "hl-tokens.inc"
#undef HLTOKEN
};

struct HLTokens {
    const Tokens& tokens_;

    std::vector<HLTokenID> ids;
    std::vector<uint32_t>  start_pos, end_pos;

    HLTokens(const Tokens& tokens) : tokens_(tokens) {}

    size_t size() const { return tokens_.ids.size(); }

    // void dump(std::ostream& out) const;
};

#endif
