#include "printer.hh"

void Printer::print(const IndexExpr& e) {
    print(e.base);
    out_ << "[";
    print(e.index);
    out_ << "]";
}

void Printer::print(const BinaryExpr& e) {
    print(e.left);
    out_ << ' ';
    switch (e.op) {
        case BinaryExpr::Add:
            out_ << '+';
            break;
        case BinaryExpr::Sub:
            out_ << '-';
            break;
        case BinaryExpr::Mul:
            out_ << '*';
            break;
        case BinaryExpr::Div:
            out_ << '/';
            break;
        case BinaryExpr::Mod:
            out_ << '%';
            break;
        case BinaryExpr::And:
            out_ << "&&";
            break;
        case BinaryExpr::Eq:
            out_ << "==";
            break;
        case BinaryExpr::NotEq:
            out_ << "!=";
            break;
        case BinaryExpr::GT:
            out_ << '>';
            break;
        case BinaryExpr::LT:
            out_ << '<';
            break;
        case BinaryExpr::GE:
            out_ << ">=";
            break;
        case BinaryExpr::LE:
            out_ << "<=";
            break;
        case BinaryExpr::Or:
            out_ << "||";
            break;
    }
    out_ << ' ';
    print(e.right);
}

void Printer::print(const CallExpr& e) {
    print(e.func);
    out_ << "(";
    if (not e.args.empty()) {
        print(e.args[0]);
        for (size_t i = 1; i < e.args.size(); i++) {
            out_ << ", ";
            print(e.args[i]);
        }
    }
    out_ << ")";
}

void Printer::print(const FieldExpr& e) {
    print(e.expr);
    out_ << "." << e.field;
}

void Printer::print(const Expr& e) {
    switch (e.type_id) {
        case IntLiteral::type_id: {
            out_ << e.as<IntLiteral>().value;
            break;
        }
        case BoolLiteral::type_id: {
            out_ << e.as<BoolLiteral>().value;
            break;
        }
        case Identifier::type_id: {
            out_ << e.as<Identifier>().name;
            break;
        }
        case IndexExpr::type_id: {
            print(e.as<IndexExpr>());
            break;
        }
        case BinaryExpr::type_id: {
            print(e.as<BinaryExpr>());
            break;
        }
        default:
            assert(false);
    }
}

void Printer::print(const Include& include) {
    out_ << "#include ";
    out_ << "<" << include.filename << ">" << endl;
}

void Printer::print(const Macro& macro) {
    out_ << '#' << macro.name << endl;
}

void Printer::print(const Using& usin) {
    out_ << "using namespace " << usin.namespc << ";" << endl;
}

void Printer::print(const Program& P) {
    for (auto decl : P.declarations) {
        switch (decl.type_id) {
            case Include::type_id: {
                print(decl.as<Include>());
                break;
            }
            case Macro::type_id: {
                print(decl.as<Macro>());
                break;
            }
            case Using::type_id: {
                print(decl.as<Using>());
                break;
            }
        }
    }
}
