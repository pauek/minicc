#include <cassert>
#include <iostream>
using namespace std;

#include "ast2.hh"

typedef Variant<3, 15> Expr;

struct Include : Type<Include, 0> {
    string filename;
    bool   global;
};

struct Macro : Type<Macro, 1> {
    string macro;
};

struct Using : Type<Using, 2> {
    string namespc;
};

struct BoolLiteral : Type<BoolLiteral, 3> {
    bool value;
};

struct IntLiteral : Type<IntLiteral, 4> {
    int value;
};

struct DoubleLiteral : Type<DoubleLiteral, 5> {
    double value;
};

struct CharLiteral : Type<CharLiteral, 6> {
    char value;
};

struct StringLiteral : Type<StringLiteral, 6> {
    string value;
};

typedef Variant<3, 6> Literal;

struct Identifier : Type<Identifier, 7> {
    string name;
};

struct TypeSpecifier : Type<TypeSpecifier, 8> {
    enum Qualifier {
        Const = 0b000001,
        Volatile = 0b000010,
        Mutable = 0b000100,
        Register = 0b001000,
        Auto = 0b010000,
        Extern = 0b100000,
    };

    bool            reference = false;
    int8_t          bqual;
    Ref<Identifier> ident;
};

struct UnaryExpr_ {
    Expr expr;
};

struct SignExpr : UnaryExpr_, Type<SignExpr, 11> {
    bool negative;
};

struct IncrExpr : UnaryExpr_, Type<SignExpr, 12> {
    bool negative;
    bool pre;
};

struct NotExpr : UnaryExpr_, Type<NotExpr, 13> {};

typedef Variant<11, 13> UnaryExpr;

struct BinaryExpr : Type<BinaryExpr, 14> {
    enum Kind {
        Unknown,
        Multiplicative,
        Additive,
        Shift,
        Relational,
        Equality,
        BitAnd,
        BitXor,
        BitOr,
        LogicalAnd,
        LogicalOr,
        Conditional,
        Eq,
        Comma,
        Infinite
    };

    Kind   kind;
    string op;
    Expr   left, right;
};

struct IndexExpr : Type<IndexExpr, 15> {
    Expr base, index;
};

///

class Printer {
    ostream& out_;

   public:
    Printer(ostream& out) : out_(out) {}

    void print(const IndexExpr& expr) {
        print(expr.base);
        out_ << "[";
        print(expr.index);
        out_ << "]";
    }

    void print(const BinaryExpr& expr) {
        print(expr.left);
        out_ << ' ' << expr.op << ' ';
        print(expr.right);
    }

    void print(const Expr& expr) {
        switch (expr.type_id) {
            case IntLiteral::type_id: {
                out_ << expr.as<IntLiteral>().value;
                break;
            }
            case BoolLiteral::type_id: {
                out_ << expr.as<BoolLiteral>().value;
                break;
            }
            case Identifier::type_id: {
                out_ << expr.as<Identifier>().name;
                break;
            }
            case IndexExpr::type_id: {
                print(expr.as<IndexExpr>());
                break;
            }
            case BinaryExpr::type_id: {
                print(expr.as<BinaryExpr>());
                break;
            }
            default:
                assert(false);
        }
    }

    void print(const Include& include) { out_ << "#include<" << include.filename << ">" << endl; }
};

int main() {
    auto ie = IndexExpr::make({
        .base = Expr(Identifier::make({.name = "a"})),
        .index = Expr(IntLiteral::make({.value = 1})),
    });

    auto e = Include::make({.filename = "iostream"});

    auto pr = Printer(cout);
    pr.print(ie.get());
    pr.print(e.get());
}
