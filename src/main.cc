
#include <fstream>
#include <iostream>
#include <map>
using namespace std;

#include "astpr.hh"
#include "commands.hh"
#include "interpreter.hh"
#include "parser.hh"
#include "prettypr.hh"
#include "semantic.hh"
#include "stepper.hh"
#include "test.hh"
#include "translator.hh"
#include "vm.hh"
#include "walker.hh"

int test_vm(Args& args) {
	using namespace vm;
	VM vm;
	vm.test();
	return 0;
}

int tokenize(Args& args) {
	string	 filename = args.shift();
	ifstream codefile(filename);
	Lexer	 L(&codefile);
	L.next();
	L.skip();
	while (!L.end()) {
		Token tok = L.read_token();
		cout << tok.pos << ' ' << L.substr(tok) << endl;
		L.skip();
	}
	return 0;
}

int show_ast(Args& args) {
	string	 filename = args.shift();
	ifstream codefile(filename);
	Parser	 P(&codefile);
	Ast		*program = P.parse();
	AstPrint(program);
	return 0;
}

int prettyprint(Args& args) {
	string filename = args.shift();
	try {
		ifstream codefile(filename);
		Parser	 P(&codefile);
		Ast		*program = P.parse();
		PrettyPrint(program);
	} catch (Error *e) {
		cerr << _T("Pretty Print Error")
			 << ": " << e->msg << endl;
		return 1;
	}
	return 0;
}

int semantic_analysis(Ast *program, string filename) {
	AnalyzeSemantics(program);
	vector<Error *> ve;
	collect_errors(program, ve);
	for (Error *e : ve) {
		cerr << filename << ":" << e->span.begin << ": " << e->msg << endl;
	}
	return ve.size();
}

Ast *parse_and_analyze(string filename) {
	ifstream codefile(filename);
	Parser	 P(&codefile);
	Ast		*program = P.parse();

	int nerrors = semantic_analysis(program, filename);
	if (nerrors > 0) {
		exit(1);
	}
	return program;
}

int interpret(Args& args) {
	string filename = args.shift();
	try {
		Ast *program = parse_and_analyze(filename);
		eval(program, cin, cout);
		vector<Error *> ve;
		collect_errors(program, ve);
		for (Error *e : ve) {
			cerr << e->msg << endl;
		}
		return (ve.empty() ? 0 : 1);
	} catch (Error *e) {
		cerr << _T("Execution Error")
			 << ": " << e->msg << endl;
		return 1;
	}
	return 0;
}

int step(Args& args) {
	string filename = args.shift();

	try {
		ifstream codefile(filename);
		Parser	 P(&codefile);
		Ast		*program = P.parse();

		int nerrors = semantic_analysis(program, filename);
		if (nerrors > 0) {
			exit(1);
		}
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

vector<Command> commands = {
	{"tok", tokenize, "Tokenize a program"},
	{"ast", show_ast, "Show the AST of a program"},
	{"pprint", prettyprint, "Pretty print a program"},
	{"eval", interpret, "Evaluate a program"},
	{"step", step, "Evaluate a program step by step"},
	{"vm", test_vm, "Use the virtual machine (TODO)"},
	{"test", test_command, "Test MiniCC"},
};

int main(int argc, char *argv[]) {
	Translator::translator.set_language("es");
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
