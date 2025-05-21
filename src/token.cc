
#include "token.hh"
using namespace std;

bool Token::is_type_spec() const {
    switch (type) {
        case Token::Void:
        case Token::Int:
        case Token::Short:
        case Token::Long:
        case Token::Bool:
        case Token::Char:
        case Token::Float:
        case Token::Double:
        case Token::String:
        case Token::Signed:
        case Token::Unsigned:
        case Token::Const:
        case Token::Volatile:
        case Token::Auto:
        case Token::Register:
        case Token::Static:
        case Token::Extern:
        case Token::Mutable:
        case Token::Inline:
        case Token::Virtual:
        case Token::Explicit:
            return true;
        default:
            return false;
    }
    return false;
}

bool Token::is_operator() const {
    switch (type) {
        case Token::Comma:
        case Token::QMark:
        case Token::BarBar:
        case Token::AmpAmp:
        case Token::Eq:
        case Token::StarEq:
        case Token::MinusEq:
        case Token::PlusEq:
        case Token::SlashEq:
        case Token::DivEq:
        case Token::BarEq:
        case Token::AmpEq:
        case Token::XorEq:
        case Token::LShiftEq:
        case Token::RShiftEq:
        case Token::Or:
        case Token::And:
        case Token::Not:
        case Token::EqEq:
        case Token::ExclEq:
        case Token::LT:
        case Token::GT:
        case Token::GE:
        case Token::LE:
        case Token::LShift:
        case Token::RShift:
        case Token::Dot:
        case Token::Arrow:
        case Token::Excl:
        case Token::Amp:
        case Token::Bar:
        case Token::Xor:
        case Token::Plus:
        case Token::Minus:
        case Token::Star:
        case Token::Slash:
        case Token::Div:
        case Token::PlusPlus:
        case Token::MinusMinus:
            return true;
        default:
            return false;
    }
    return false;
}

bool Token::is_basic_type() const {
    switch (type) {
        case Token::Void:
        case Token::Int:
        case Token::Short:
        case Token::Long:
        case Token::Bool:
        case Token::Char:
        case Token::Float:
        case Token::Double:
        case Token::String:
            return true;
        default:
            return false;
    }
    return false;
}

bool Token::is_type_qual() const {
    switch (type) {
        case Token::Const:
        case Token::Long:
        case Token::Volatile:
        case Token::Auto:
        case Token::Register:
        case Token::Static:
        case Token::Extern:
        case Token::Mutable:
            return true;
        default:
            return false;
    }
    return false;
}
