#include <iostream>
using namespace std;

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

void Tokens::dump(ostream& out) const {
    for (size_t i = 0; i < ids.size(); i++) {
        auto pos = token_line_col(i);
        out << filename_ << ':' << pos.lin << ':' << pos.col << ": ";

        const auto code = ids[i];
        switch (code) {
            case IDENT:
            case KEYWORD:
            case STR_LITERAL:
            case CHAR_LITERAL:
            case INT_LITERAL:
            case MACRO:
            case LINE_COMMENT:
            case GLOBAL_FILENAME:
            case LOCAL_FILENAME:
            case MULTILINE_COMMENT: {
                out << name(i) << " \'" << str(i) << '\'';
                break;
            }
            case ESC:
            case ENDL: {
                out << name(i);
                break;
            }
            default: {
                out << '\'' << name(i) << '\'';
            }
        }
        out << std::endl;
    }
}
