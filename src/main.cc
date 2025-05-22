
#include <fstream>
#include <iostream>
#include <map>
using namespace std;
#include "astpr.hh"
#include "commands.hh"
#include "interpreter.hh"
#include "parser.hh"
#include "pprint.hh"
#include "semantic.hh"
#include "stepper.hh"
#include "test.hh"
#include "translator.hh"
#include "vm.hh"
#include "walker.hh"
#include "instrumenter.hh"

int cmd_vm(Args& args) {
    using namespace vm;
    VM vm;
    vm.test();
    return 0;
}

int cmd_tokenize(Args& args) {
    if (args.empty()) {
        cout << "usage: minicc tok <filename>" << endl;
        exit(1);
    }
    string   filename = args.shift();
    ifstream codefile(filename);
    Lexer    L(&codefile);
    L.next();
    L.skip();
    while (!L.end()) {
        Token tok = L.read_token();
        cout << tok.pos << ' ' << L.substr(tok) << endl;
        L.skip();
    }
    return 0;
}

int cmd_ast(Args& args) {
    if (args.empty()) {
        cout << "usage: minicc ast <filename>" << endl;
        exit(1);
    }
    string   filename = args.shift();
    ifstream codefile(filename);
    Parser   P(&codefile);
    AstNode *program = P.parse();
    ast_print(program);
    return 0;
}

int cmd_canparse(Args& args) {
    if (args.empty()) {
        cout << "usage: minicc ast <filename>" << endl;
        exit(1);
    }
    string filename = args.shift();
    try {
        ifstream codefile(filename);
        Parser   P(&codefile);
        AstNode *program = P.parse();
        if (!has_errors(program)) {
            return 0;
        }
        for (Error *e : collect_errors(program)) {
            cerr << filename << ":" << e->span.begin << ": " << e->msg << endl;
        }
        return 127;
    } catch (ParseError& e) {
        cerr << "ParseError" << endl << filename << ':' << e.pos << ": " << e.msg << endl;
        return 127;
    } catch (std::out_of_range& e) {
        cerr << filename << ": Out of Range: " << e.what() << endl;
        return 127;
    }
}

int cmd_prettyprint(Args& args) {
    if (args.empty()) {
        cout << "usage: minicc pprint <filename>" << endl;
        exit(1);
    }
    string filename = args.shift();
    try {
        AstNode *program = parse_file(filename);
        pretty_print(program);
    } catch (Error *e) {
        cerr << _T("Pretty Print Error")
             << ": " << e->msg << endl;
        return 1;
    }
    return 0;
}

void _analyze_semantics(AstNode *program, string filename) {
    analyze_semantics(program);
    vector<Error *> errors = collect_errors(program);
    for (Error *e : errors) {
        cerr << filename << ":" << e->span.begin << ": " << e->msg << endl;
    }
    if (errors.size() > 0) {
        exit(1);
    }
}

int cmd_eval(Args& args) {
    if (args.empty()) {
        cout << "usage: minicc eval <filename>" << endl;
        exit(1);
    }
    try {
        string   filename = args.shift();
        ifstream codefile(filename);
        Parser   P(&codefile);
        AstNode *program = P.parse();
        _analyze_semantics(program, filename);
        eval(program, cin, cout);
        if (!has_errors(program)) {
            return 0;
        }
        for (Error *e : collect_errors(program)) {
            cerr << e->msg << endl;
        }
        return 1;
    } catch (Error *e) {
        cerr << _T("Execution Error") << ": " << e->msg << endl;
        return 1;
    }
}

int cmd_step(Args& args) {
    if (args.empty()) {
        cout << "usage: minicc step <filename>" << endl;
        exit(1);
    }
    try {
        string   filename = args.shift();
        ifstream codefile(filename);
        Parser   P(&codefile);
        AstNode *program = P.parse();
        _analyze_semantics(program, filename);
        Stepper S;
        S.Step(program);
        while (!S.finished()) {
            cout << S.span() << ": " << P.lexer().substr(S.span()) << endl;
            cout << S.status() << endl;
            string out = S.output();
            if (out != "") {
                cout << "OUTPUT: \"" << out << "\"" << endl;
            }
            cout << endl;
            if (!S.step()) {
                throw S.error();
            }
        }
        return 0;
    } catch (Error *e) {
        cerr << _T("Execution Error")
             << ": " << e->msg << endl;
        return 1;
    }
}

int cmd_instrument(Args& args) {
    string   filename = args.shift();
    ifstream codefile(filename);
    Parser   P(&codefile);
    AstNode *program = P.parse();
    Instrumenter().instrument(program);
    pretty_print(program);
    return 0;
}

vector<Command> commands = {
    {"tok",        cmd_tokenize,    "Tokenize a program"                            },
    {"ast",        cmd_ast,         "Show the AST of a program"                     },
    {"canparse",   cmd_canparse,    "Parse a program an return 0 if it was possible"},
    {"instrument", cmd_instrument,  "Instrument program"                            },
    {"pprint",     cmd_prettyprint, "Pretty print a program"                        },
    {"eval",       cmd_eval,        "Evaluate a program"                            },
    {"step",       cmd_step,        "Evaluate a program step by step"               },
    {"vm",         cmd_vm,          "TODO: virtual machine"                         },
    {"test",       cmd_test,        "Test MiniCC"                                   },
};

int main(int argc, char *argv[]) {
    Args args(argc, argv);
    if (args.empty()) {
        help(commands);
    }
    string cmd = args.shift();
    auto   pcmd = find_command(cmd, commands);
    if (pcmd == nullptr) {
        cerr << "error: No se conoce el comando '" << cmd << "'" << endl;
        return 1;
    }
    return (*pcmd->func)(args);
}
