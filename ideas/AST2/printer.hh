#ifndef PRINTER_HH
#define PRINTER_HH

#include <iostream>
#include "astcpp.hh"

class Printer {
    std::ostream& out_;

   public:
    Printer(std::ostream& out) : out_(out) {}

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

#endif
