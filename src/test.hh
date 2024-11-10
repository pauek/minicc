#ifndef TEST_HH
#define TEST_HH
#include <sstream>
#include "commands.hh"
#include "walker.hh"
void parse_test_file(string filename, string& code, string& in, string& out, string& err);
void compare_result(string filename, string sout, string serr, string out, string err);
typedef void (*TestFunc)(Ast *, std::istream&, std::ostream&);

template <TestFunc func>
int test(Args& args) {
    while (!args.empty()) {
        std::string filename = args.shift();
        std::string code, in, out, err;
        parse_test_file(filename, code, in, out, err);
        std::istringstream scode(code), sin(in);
        std::ostringstream sout, serr;
        try {
            Ast *program = Parser(&scode, &serr).parse();
            func(program, sin, sout);
            for (Error *e : collect_errors(program)) {
                serr << filename << "[" << e->span << "]: " << e->msg << endl;
            }
        } catch (ParseError& e) {
            serr << filename << "[" << e.pos << "]: " << e.msg << endl;
        } catch (TypeError& e) {
            serr << filename << "[" << e.span << "]: " << e.msg << endl;
        } catch (EvalError& e) {
            serr << filename << "[" << e.span << "]: " << e.msg << endl;
        } catch (Error *e) {
            serr << filename << "[" << e->span << "]: " << e->msg << endl;
        }
        compare_result(filename, sout.str(), serr.str(), out, err);
    }
    return 0;
}

int cmd_test(Args& args);
#endif