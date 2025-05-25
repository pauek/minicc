#include "test.hh"
#include <fstream>
#include <iostream>
#include <map>
#include "astpr.hh"
#include "commands.hh"
#include "interpreter.hh"
#include "parser.hh"
#include "pprint.hh"
#include "semantic.hh"
#include "stepper.hh"
#include "translator.hh"
#include "vm.hh"
#include "walker.hh"
using namespace std;

// Detect lines like:
//
//   [[out]]-------------------------
//
string test_separator(string line) {
    int p1 = line.find("[[");
    if (p1 != 0) {
        return "";
    }
    int p2 = line.find("]]");
    if (p2 == string::npos) {
        return "";
    }
    int    len = line.size() - (p2 + 2);
    string dashes(len, '-');
    if (line.substr(p2 + 2) != dashes) {
        return "";
    }
    return line.substr(2, p2 - 2);
}

string visible_spaces(string output, string compare = "") {
    string res;
    bool   first_error_shown = false;
    for (int i = 0; i < output.size(); i++) {
        char c = output[i];
        if (!first_error_shown and compare != "" and output[i] != compare[i]) {
            res += "\x1b[31;1m>";
        }
        switch (c) {
            case ' ':
                res += '_';
                break;
            case '\n':
                res += "[endl]";
                break;
            case '\t':
                res += "\\t  ";
                break;
            default:
                res += c;
        }
        if (!first_error_shown and compare != "" and output[i] != compare[i]) {
            res += "<\x1b[0m";
            first_error_shown = true;
        }
        if (c == '\n') {
            res += '\n';
        }
    }
    return res;
}

void parse_test_file(string filename, string& code, string& in, string& out, string& err) {
    ifstream F(filename);
    string   line;
    string  *acum = &code;
    while (getline(F, line)) {
        string label = test_separator(line);
        if (label == "") {
            if (line.size() > 0) {
                *acum += line.substr(0, line.size() - 1);
                if (line.back() != '\\') {
                    *acum += line.back();
                    *acum += '\n';
                }
            } else {
                *acum += '\n';
            }
        } else if (label == "out") {
            acum = &out;
        } else if (label == "in") {
            acum = &in;
        } else if (label == "err") {
            acum = &err;
        }
    }
}

void compare_result(string filename, string sout, string serr, string out, string err) {
    char   res = '.';
    string sep((82 - filename.size()) / 2, '-');
    string header = sep + " " + filename + " " + sep + "\n";
    if (sout != out) {
        cerr << header << "[out]:" << endl;
        header = "";
        cerr << "target  \"\"\"" << visible_spaces(out, sout) << "\"\"\"" << endl;
        cerr << "current \"\"\"" << visible_spaces(sout, out) << "\"\"\"" << endl;
        cerr << endl;
        res = 'x';
    }
    if (serr != err) {
        cerr << header << "[err]:" << endl;
        cerr << "target  \"\"\"" << visible_spaces(err, serr) << "\"\"\"" << endl;
        cerr << "current \"\"\"" << visible_spaces(serr, err) << "\"\"\"" << endl;
        cerr << endl;
        res = 'x';
    }
    cout << res << flush;
}

void _parse(AstNode *program, istream& Sin, ostream& Sout) {}

void _ast(AstNode *program, istream& Sin, ostream& Sout) {
    ast_print(program, Sout);
}

void _semantic(AstNode *program, istream& Sin, ostream& Sout) {
    analyze_semantics(program);
}

void _print(AstNode *program, istream& Sin, ostream& Sout) {
    pprint(program, Sout);
}

void _eval(AstNode *program, istream& Sin, ostream& Sout) {
    eval(program, Sin, Sout);
}

void _stepper(AstNode *program, istream& Sin, ostream& Sout) {
    ostringstream Saux;
    Stepper       S(&Sin, &Saux);
    S.Step(program);
    while (!S.finished()) {
        Sout << S.span() << ": " << /* FIXME: P.lexer().substr(S.span()) <<*/ endl;
        Sout << S.status() << endl;
        string output = S.output();
        if (output != "") {
            Sout << "OUTPUT: \"" << output << '"' << endl;
        }
        Sout << endl;
        if (!S.step()) {
            throw S.error();
        }
    }
}

vector<Command> test_commands = {
    {"parser",      test<_parse>,    "Test the parser"           },
    {"ast",         test<_ast>,      "Test the AST"              },
    {"print",       test<_print>,    "Test the pretty printer"   },
    {"semantic",    test<_semantic>, "Test the semantic analyzer"},
    {"interpreter", test<_eval>,     "Test the interpreter"      },
    {"stepper",     test<_stepper>,  "Test the stepper"          },
};

int cmd_test(Args& args) {
    if (args.empty()) {
        help(test_commands);
    }
    string cmd = args.shift();
    auto   pcmd = find_command(cmd, test_commands);
    if (pcmd == nullptr) {
        cerr << "error: No se conoce el comando '" << cmd << "'" << endl;
        return 1;
    }
    cout << cmd << " ";
    (*pcmd->func)(args);
    cout << endl;
    return 0;
}
