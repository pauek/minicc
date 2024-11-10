#ifndef TEST_HH
#define TEST_HH

#include <sstream>
#include "ast.hh"
#include "commands.hh"
#include "parser.hh"
#include "types.hh"
#include "walker.hh"

void parse_test_file(
    std::string  filename,
    std::string& code,
    std::string& in,
    std::string& out,
    std::string& err
);

void compare_result(
    std::string filename,
    std::string sout,
    std::string serr,
    std::string out,
    std::string err
);

typedef void (*TestFunc)(AstNode *, std::istream&, std::ostream&);

template <TestFunc func>
int test(Args& args) {
    while (!args.empty()) {
        std::string filename = args.shift();
        std::string code, in, out, err;
        parse_test_file(filename, code, in, out, err);
        std::istringstream scode(code), sin(in);
        std::ostringstream sout, serr;
        try {
            AstNode *program = Parser(&scode, &serr).parse();
            func(program, sin, sout);
            for (Error *e : collect_errors(program)) {
                serr << filename << "[" << e->span << "]: " << e->msg << std::endl;
            }
        } catch (ParseError& e) {
            serr << filename << "[" << e.pos << "]: " << e.msg << std::endl;
        } catch (TypeError& e) {
            serr << filename << "[" << e.span << "]: " << e.msg << std::endl;
        } catch (EvalError& e) {
            serr << filename << "[" << e.span << "]: " << e.msg << std::endl;
        } catch (Error *e) {
            serr << filename << "[" << e->span << "]: " << e->msg << std::endl;
        }
        compare_result(filename, sout.str(), serr.str(), out, err);
    }
    return 0;
}

int cmd_test(Args& args);

#endif