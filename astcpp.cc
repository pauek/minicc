#include "astcpp.hh"
#include "tokens.hh"
#include "datatype.hh"

TypeSpec::Qualifier TypeSpec::qualifier_from_str(std::string name) {
    if (name == "auto") {
        return Auto;
    } else if (name == "const") {
        return Const;
    } else {
        assert(false);
    }
}

string TypeSpec::qualifier_to_str(Qualifier qual) {
    switch (qual) {
        case Auto:
            return "auto";
        case Const:
            return "const";
        default:
            assert(false);
    }
}

BinaryExpr::Level BinaryExpr::op_level(Operator op) {
    switch (op) {
        case Mul:
        case Div:
        case Mod:
            return Multiplicative;
        case Add:
        case Sub:
            return Additive;
        case GT:
        case LT:
        case GE:
        case LE:
            return Relational;
        case Eq:
        case NotEq:
            return Equality;
        case Or:
            return LogicalOr;
        case And:
            return LogicalAnd;
        case None:
            assert(false);
            return Zero;
    }
}

string BinaryExpr::operator_to_str(Operator op) {
    switch (op) {
        case Add:
            return "+";
        case Sub:
            return "-";
        case Mul:
            return "*";
        case Div:
            return "/";
        case Mod:
            return "%";
        case And:
            return "&&";
        case Or:
            return "||";
        case Eq:
            return "==";
        case NotEq:
            return "=";
        case GT:
            return ">";
        case LT:
            return "<";
        case GE:
            return ">=";
        case LE:
            return "<=";
        default:
            assert(false);
    }
}

bool AssignExpr::is_assign_op(TypeID id) {
    switch (id) {
        case EQ:
        case PLUSEQ:
        case MINUSEQ:
        case STAREQ:
        case DIVEQ:
        case MODEQ:
            return true;
        default:
            return false;
    }
}

AssignExpr::Operator AssignExpr::token_to_op(TypeID id) {
    switch (id) {
        case EQ: {
            return Normal;
        }
        case PLUSEQ: {
            return Add;
        }
        case MINUSEQ: {
            return Sub;
        }
        case STAREQ: {
            return Mul;
        }
        case DIVEQ: {
            return Div;
        }
        case MODEQ: {
            return Mod;
        }
        default:
            return Error;
    }
}

string AssignExpr::op_to_str(Operator op) {
    switch (op) {
        case Normal:
            return "=";
        case Add:
            return "+=";
        case Sub:
            return "-=";
        case Mul:
            return "*=";
        case Div:
            return "/=";
        case Mod:
            return "%=";
        default:
            assert(false);
    }
}

bool IncrExpr::is_incr_op(TypeID id) {
    switch (id) {
        case PLUSPLUS:
        case MINUSMINUS:
            return true;
        default:
            return false;
    }
}

IncrExpr::Operator IncrExpr::token_to_op(TypeID id) {
    switch (id) {
        case PLUSPLUS:
            return Add;
        case MINUSMINUS:
            return Sub;
        default:
            assert(false);
            return Error;
    }
}

string IncrExpr::op_to_str(Operator op) {
    switch (op) {
        case Add:
            return "++";
        case Sub:
            return "--";
        default:
            assert(false);
            return "";
    }
}
